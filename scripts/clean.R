################################### Project: analyses for ISAEC Krembil abstracts #####################################
# date: 2016-03-15
# written by: Kala Sundararajan
# BL and 6 month data from ISAEC clinical database, and consent status from master list


########################### clean.R: create indicators, merge and subset data, clean variables ###############################

# create baseline indicator
BL$received_BL <- 1

# create 6m indicator
x6m$received_6m <- 1

# remove variable prefixes
names(BL) <- sub("X31_", "", names(BL))
names(x6m) <- sub("X31_", "", names(x6m))

# select variables to keep from baseline
BL <- BL[,c(grep("SUBJECT_ID", names(BL)):grep("GENDER", names(BL)),
            grep("COLLECTION_CENTER", names(BL)),
            grep("BASELINE_DOC$", names(BL)),
            grep("BASELINE_MSTATUS", names(BL)):grep("received_BL", names(BL))
          )]

# remove extra DOC from BL 
BL <- BL[,!grepl("DOC.", names(BL))]

# select variables to keep from 6 month
x6m <- x6m[,c(grep("SUBJECT_ID", names(x6m)),
              grep("6MONTH_DOC$", names(x6m)),
              grep("6MONTH_PAINLOCATION", names(x6m)):grep("received_6m", names(x6m))
            )]

# remove extra DOC from 6 month
x6m <- x6m[,!grepl("DOC.", names(x6m))]

# clean consent statuses
consent$Consent[grepl("neligib", consent$Consent)] <- "Ineligible"
consent$Consent[grepl("ithdraw", consent$Consent)] <- "Withdrawal"
consent$Consent[grepl("0", consent$Consent)] <- "Did not consent"
consent$Consent[grepl("1", consent$Consent)] <- "Consented"

# merge baseline and 6 month
ISAEC <- merge(BL, x6m, by="SUBJECT_ID", all.x=T, all.y=T)

# merge consent status
ISAEC <- merge(ISAEC, consent, by.x="SUBJECT_ID", by.y="ID", all.x=T, all.y=F)

# change NA to 0 on received indicators
ISAEC$received_BL[is.na(ISAEC$received_BL)] <- 0
ISAEC$received_6m[is.na(ISAEC$received_6m)] <- 0

# change blanks to NA
ISAEC[ISAEC==""] <- NA

# format dates
datevars <- c("DOB", "DOE", grep("_DOC", names(ISAEC), value=TRUE))
ISAEC[,datevars] <- lapply(ISAEC[,datevars], ymd)

# fix incorrect DOE
ISAEC$DOE[ISAEC$SUBJECT_ID==4357] <- ymd(20160112)

# replace DOE with BASELINE_DOC if missing
ISAEC$DOE <- apply(ISAEC[,c("DOE", "BASELINE_DOC")], 1, function(x) ifelse(is.na(x[1]), x[2], x[1]))
ISAEC$DOE <- ymd(ISAEC$DOE)

# calculate age
ISAEC$AGE = round(interval(ISAEC$DOB, ISAEC$DOE) / dyears(1))
ISAEC$AGE <- ifelse(ISAEC$AGE < 15, NA, ISAEC$AGE)

# clean ODI score scores
ODI_scores <- c("BASELINE_TOTAL", "6MONTH_TOTAL")
ISAEC[,ODI_scores] <- lapply(ISAEC[,ODI_scores], function(x) {
  x <- sub("%", "", x)
  return(as.numeric(as.character(x)))
})

# calculate self-efficacy scores
effic_vars_BL <- c("BASELINE_MANAGEFATIGUE", "BASELINE_MANAGEPAIN", "BASELINE_MANAGEEMODISTRESS", "BASELINE_MANAGEOTHERS_S", 
                   "BASELINE_MANAGETASKS","BASELINE_MANAGETHINGS")
effic_vars_6m <- c("6MONTH_MANAGEFATIGUE", "6MONTH_MANAGEPAIN", "6MONTH_MANAGEEMODISTRESS", "6MONTH_MANAGEOTHERS_S", 
                   "6MONTH_MANAGETASKS","6MONTH_MANAGETHINGS")

ISAEC$BASELINE_SELF_EFFICACY_SCORE <- apply(ISAEC[,effic_vars_BL], 1, score_effic)
# remove baseline score if intake before june 24 2013
ISAEC$BASELINE_SELF_EFFICACY_SCORE[ISAEC$DOE <= ymd(20130623)] <- NA 

ISAEC$'6MONTH_SELF_EFFICACY_SCORE' <- apply(ISAEC[,effic_vars_6m], 1, score_effic)

# calculate resilience scores
ISAEC$BASELINE_RESILIENCE <- apply(ISAEC[,grep("BASELINE_CD_R", names(ISAEC))], 1, score_resil)
ISAEC$'6MONTH_RESILIENCE' <- apply(ISAEC[,grep("6MONTH_CD_R", names(ISAEC))], 1, score_resil)

# fix start back scores
BL_scores <- apply(ISAEC[,grep("BASELINE_START\\d*$", names(ISAEC))], 1, score_start)
BL_scores <- data.frame(do.call(rbind, lapply(BL_scores, rbind)))
x6m_scores <- apply(ISAEC[,grep("6MONTH_START\\d*$", names(ISAEC))], 1, score_start)
x6m_scores <- data.frame(do.call(rbind, lapply(x6m_scores, rbind)))

ISAEC$BASELINE_RESULT <- unlist(BL_scores$risk)
ISAEC$BASELINE_OVERALL <- unlist(BL_scores$overall)
ISAEC$BASELINE_DISTRESS <- unlist(BL_scores$psych)

ISAEC$'6MONTH_RESULT' <- unlist(x6m_scores$risk)
ISAEC$'6MONTH_OVERALL' <- unlist(x6m_scores$overall)
ISAEC$'6MONTH_DISTRESS' <- unlist(x6m_scores$psych)

### reorder variables ###
ISAEC <- ISAEC[,c(grep("^SUBJECT_ID$", names(ISAEC)):grep("^COLLECTION_CENTER$", names(ISAEC)), 
                 grep("^Consent$", names(ISAEC)), 
                 grep("^received_BL$", names(ISAEC)), 
                 grep("^received_6m$", names(ISAEC)),
                 grep("^AGE$", names(ISAEC)), 
                 grep("^BASELINE_", names(ISAEC)), 
                 grep("^6MONTH_", names(ISAEC))
              )]


write.csv(ISAEC, file="processed_data/ISAEC.csv", na="", row.names=F)
save(ISAEC, file="processed_data/ISAEC.Rdata")

# exclude if no intake or not consented
ISAEC_cons <- subset(ISAEC, ISAEC$received_BL==1 & ISAEC$Consent=="Consented")
write.csv(ISAEC_cons, file="processed_data/ISAEC_cons.csv", na="", row.names=F)
save(ISAEC_cons, file="processed_data/ISAEC_cons.Rdata")

# create dataset for 6-month eligible patients
ISAEC_cons <- subset(ISAEC_cons, !is.na(ISAEC_cons$DOE))

ISAEC_cons_6m <- subset(ISAEC_cons, ISAEC_cons$DOE <= ymd(20150914))
write.csv(ISAEC_cons_6m, file="processed_data/ISAEC_cons_6m.csv", na="", row.names=F)
save(ISAEC_cons_6m, file="processed_data/ISAEC_cons_6m.Rdata")

# create dataset for 6-month eligible patients with intakes after oct 1, 2014
ISAEC_cons_recent <- subset(ISAEC_cons_6m, ISAEC_cons_6m$DOE >= ymd(20141001))
write.csv(ISAEC_cons_recent, file="processed_data/ISAEC_cons_recent.csv", na="", row.names=F)
save(ISAEC_cons_recent, file="processed_data/ISAEC_cons_recent.Rdata")


#### recode or create new baseline variables ####
# marital status -  1: married or common-law
#                   2: single (never married)
#                   3: widowed/separated/divorced
#                   4: widowed
ISAEC$MARITAL[ISAEC$BASELINE_MSTATUS==1 | ISAEC$BASELINE_MSTATUS==2] <- 1
ISAEC$MARITAL[ISAEC$BASELINE_MSTATUS==3] <- 2
ISAEC$MARITAL[ISAEC$BASELINE_MSTATUS==4 | ISAEC$BASELINE_MSTATUS==5 | ISAEC$BASELINE_MSTATUS==6] <- 3

# non-white ethnicity - 0 = white, 1 = nonwhite
ISAEC$NONWHITE[ISAEC$BASELINE_ETHNICITY == 1] <- 0
ISAEC$NONWHITE[ISAEC$BASELINE_ETHNICITY != 1] <- 1

# pain location
ISAEC$BPLOC <- ISAEC$BASELINE_BPLOCATION

# pain stops ever
ISAEC$BPSTOP <- ISAEC$BASELINE_BPSTOP

# back pain started with injury and/or accident: 0=other (including NA), 1 = injury/accident
ISAEC$BPSTART <- 0
ISAEC$BPSTART[grepl("[1|2]",ISAEC$BASELINE_BPSTART)] <- 1

# previous history of back pain and surgery history: 0 = no/NA, 1 = yes
ISAEC$BPHX <- 0
ISAEC$BPHX[ISAEC$BASELINE_BPHX==1] <- 1

ISAEC$BPSX <- 0
ISAEC$BPSX[ISAEC$BASELINE_BPSX==1] <- 1

# LBP duration and leg pain duration: same categorization, but recode '88' and NA to 6
ISAEC$BPDURATION <- ISAEC$BASELINE_LBPEPISODE
ISAEC$BPDURATION[ISAEC$BPDURATION==88 | is.na(ISAEC$BPDURATION)] <- 6

ISAEC$LPDURATION <- ISAEC$BASELINE_LEGPEPISODE
ISAEC$LPDURATION[ISAEC$LPDURATION==88 | is.na(ISAEC$LPDURATION)] <- 6

# LBP claim: 0 = no claim or missing, 1 = some claim (1, 2, or 3)
ISAEC$CLAIM <- 0
ISAEC$CLAIM[grepl("[1|2|3]", ISAEC$BASELINE_BPCLAIM)] <- 1

# non-radicular pain location: 0 = only radicular, 1 = multi-site or non-anatomical
ISAEC$NONRADIC <- 0
ISAEC$NONRADIC[grepl("[2|3]", ISAEC$BASELINE_PAINLOCATION)] <- 1

# pain change perception
ISAEC$PAINPERC <- ISAEC$BASELINE_PAINQ

# bowel/bladder changes, leg weakness, leg numbness: 0 = none or missing, 1 = yes
ISAEC$BOWELCHANGE <- 0
ISAEC$BOWELCHANGE[ISAEC$BASELINE_BOWELFX == 1] <- 1

ISAEC$LEGWEAK <- 0
ISAEC$LEGWEAK[ISAEC$BASELINE_LEGWEAKNESS == 1] <- 1

ISAEC$LEGNUMB <- 0
ISAEC$LEGNUMB[ISAEC$BASELINE_LEGNUMBNESS == 1] <- 1

# medications: 0 = no or missing, 1 = yes
ISAEC$MEDTYL <- ISAEC$MEDNSAID <- ISAEC$MEDTYL34 <- ISAEC$MEDPERC <- ISAEC$MEDOXY <- ISAEC$MEDHYD <- 0
ISAEC$MEDTYL[ISAEC$BASELINE_PAINMEDTYLENOL == 1 | !is.na(ISAEC$BASELINE_PAINMEDTYLENOL_DOSE)] <- 1
ISAEC$MEDNSAID[ISAEC$BASELINE_PAINMEDNSAID == 1 | !is.na(ISAEC$BASELINE_PAINMEDNSAID_DOSE)] <- 1
ISAEC$MEDTYL34[ISAEC$BASELINE_PAINMEDTYLENOL34 == 1 | !is.na(ISAEC$BASELINE_PAINMEDTYLENOL34_DOSE)] <- 1
ISAEC$MEDPERC[ISAEC$BASELINE_PAINMEDPERCOCET == 1 | !is.na(ISAEC$BASELINE_PAINMEDPERCOCET_DOSE)] <- 1
ISAEC$MEDOXY[ISAEC$BASELINE_PAINMEDOXYMOR == 1 | !is.na(ISAEC$BASELINE_PAINMEDOXYMOR_DOSE)] <- 1
ISAEC$MEDHYD[ISAEC$BASELINE_PAINMEDHYDDIL == 1 | !is.na(ISAEC$BASELINE_PAINMEDHYDDIL_DOSE)] <- 1


# opioid indicator: 0 = no opioids reported, 1= 1 or more opioids
ISAEC$MEDOPI <- 0
ISAEC$MEDOPI[ISAEC$MEDTYL34==1 | ISAEC$MEDPERC==1 | ISAEC$MEDOXY==1 | ISAEC$MEDHYD==1] <- 1

# employment status: recode missing as 'other'
ISAEC$WORKSTAT <- ISAEC$BASELINE_WORKSTATUS
ISAEC$WORKSTAT[is.na(ISAEC$WORKSTAT)] <- 7

# work satisfaction: 1 = satisfied, 2 = not satisfied, 3 = NA
ISAEC$WORKSATIS[ISAEC$BASELINE_WORKSATIS==0] <- 2
ISAEC$WORKSATIS[ISAEC$BASELINE_WORKSATIS==1] <- 1
ISAEC$WORKSATIS[is.na(ISAEC$BASELINE_WORKSATIS)] <- 3

# work physically/mentally demanding: 1 = yes, 2 = no, 3 = NA
ISAEC$WORKPHYS[ISAEC$BASELINE_WORKPHYS==1] <- 1
ISAEC$WORKPHYS[ISAEC$BASELINE_WORKPHYS==0] <- 2
ISAEC$WORKPHYS[is.na(ISAEC$BASELINE_WORKPHYS)] <- 3

ISAEC$WORKMENT[ISAEC$BASELINE_WORKMENT==1] <- 1
ISAEC$WORKMENT[ISAEC$BASELINE_WORKMENT==0] <- 2
ISAEC$WORKMENT[is.na(ISAEC$BASELINE_WORKMENT)] <- 3

# underemployed: 1=mod. duties, not employed, or disability benefit, 2=other
ISAEC$WORK_UNDEREMP <- ifelse(ISAEC$WORKSTAT==2 | ISAEC$WORKSTAT==5 | ISAEC$WORKSTAT==7, 1, 0)

# frequent exercise: 1 = twice or more per week, 0 = other or missing
ISAEC$FREQ_EXERC <- 0
ISAEC$FREQ_EXERC[ISAEC$BASELINE_EXERCISE==4] <- 1

# treatments: 1 = yes, 0 = no or NA
ISAEC$TXCHIRO <- ISAEC$TXPHYSIO <- ISAEC$TXMASSAGE <- ISAEC$TXACU <- ISAEC$TXSURG <- ISAEC$TXCONSULT <- 0
ISAEC$TXCHIRO[ISAEC$BASELINE_V2_TXCHIRO==1] <- 1
ISAEC$TXPHYSIO[ISAEC$BASELINE_V2_TXPHYSIO==1] <- 1
ISAEC$TXMASSAGE[ISAEC$BASELINE_V2_TXMASSAGE==1] <- 1
ISAEC$TXACU[ISAEC$BASELINE_V2_TXACUP==1] <- 1
ISAEC$TXSURG[ISAEC$BASELINE_V2_TXSX==1] <- 1
ISAEC$TXCONSULT[ISAEC$BASELINE_V2_TXCONSULT==1] <- 1

# investigations: 1 = yes, 0 = no or NA
ISAEC$INVXRAY <- ISAEC$INVCT <- ISAEC$INVMRI <- ISAEC$INVBONESCAN <- ISAEC$INVEMG <- 0
ISAEC$INVXRAY[ISAEC$BASELINE_V2_XRAY==1] <- 1
ISAEC$INVCT[ISAEC$BASELINE_V2_CTSCAN==1] <- 1
ISAEC$INVMRI[ISAEC$BASELINE_V2_MRI==1] <- 1
ISAEC$INVBONESCAN[ISAEC$BASELINE_V2_BONESCAN==1] <- 1
ISAEC$INVEMG[ISAEC$BASELINE_V2_EMG_NERVECONDUCTION==1] <- 1

# comorbidities: 1 = yes, 0 = no or NA
ISAEC$CM_HCHOL <- ISAEC$CM_HBP <- ISAEC$CM_STROKE <- ISAEC$CM_CAD <- ISAEC$CM_AST <- ISAEC$CM_PVD <- ISAEC$CM_DIAB <- ISAEC$CM_KIDN <- 
  ISAEC$CM_LIV <- ISAEC$CM_ULC <- ISAEC$CM_ANEM <- ISAEC$CM_CANC <- ISAEC$CM_DEP <- ISAEC$CM_PSYC <- ISAEC$CM_OA <- ISAEC$CM_RA <- 0

ISAEC$CM_HCHOL[ISAEC$BASELINE_V2_HCHOLPROB==1] <- 1
ISAEC$CM_HBP[ISAEC$BASELINE_V2_HBPPROB==1] <- 1
ISAEC$CM_STROKE[ISAEC$BASELINE_V2_STROKEPROB==1] <- 1
ISAEC$CM_CAD[ISAEC$BASELINE_V2_CADPROB==1] <- 1
ISAEC$CM_AST[ISAEC$BASELINE_V2_ASTCOPDPROB==1] <- 1
ISAEC$CM_PVD[ISAEC$BASELINE_V2_PVDPROB==1] <- 1
ISAEC$CM_DIAB[ISAEC$BASELINE_V2_DMPROB==1] <- 1
ISAEC$CM_KIDN[ISAEC$BASELINE_V2_KIDNEYDZPROB==1] <- 1
ISAEC$CM_LIV[ISAEC$BASELINE_V2_LIVERDZPROB==1] <- 1
ISAEC$CM_ULC[ISAEC$BASELINE_V2_STOMACHDZPROB==1] <- 1
ISAEC$CM_ANEM[ISAEC$BASELINE_V2_BLDDZPROB==1] <- 1
ISAEC$CM_CANC[ISAEC$BASELINE_V2_CAPROB==1] <- 1
ISAEC$CM_DEP[ISAEC$BASELINE_V2_DEPPROB==1] <- 1
ISAEC$CM_PSYC[ISAEC$BASELINE_V2_PSYCHPROB==1] <- 1
ISAEC$CM_OA[ISAEC$BASELINE_V2_OAPROB==1] <- 1
ISAEC$CM_RA[ISAEC$BASELINE_V2_RAPROB==1] <- 1

# comorbidity count (excluding 'other')
cm_vars <- c("CM_RA","CM_OA","CM_PSYC","CM_DEP","CM_CANC","CM_ANEM","CM_ULC","CM_LIV","CM_KIDN","CM_DIAB","CM_PVD","CM_AST","CM_CAD","CM_STROKE",
             "CM_HBP","CM_HCHOL")
ISAEC$CMCOUNT <- apply(ISAEC[,cm_vars], 1, sum)
ISAEC$CM_ANY <- ifelse(ISAEC$CMCOUNT>0, 1, 0)

# smoking: 1 = yes, 2 = no, 3 = quit
ISAEC$SMOKETYPE[ISAEC$BASELINE_SMOKINGHX == 1] <- 1
ISAEC$SMOKETYPE[ISAEC$BASELINE_SMOKINGHX == 0] <- 2
ISAEC$SMOKETYPE[ISAEC$BASELINE_SMOKINGQUIT == 1 & ISAEC$BASELINE_SMOKINGHX != 1] <- 3

# expectations: recode --> NA = 6 (not applicable)
expc_vars <- c("BASELINE_EXPECTRELIEF","BASELINE_EXPECTACT","BASELINE_EXPECTSLEEP","BASELINE_EXPECTWORK","BASELINE_EXPECTREC","BASELINE_EXPECTPREV")
for (v in expc_vars) {
  varname <- sub("BASELINE_","",v)
  ISAEC[,varname] <- ISAEC[,v]
  ISAEC[,varname][is.na(ISAEC[,varname])] <- 6
}

# 3-level expectations: recode --> 1=not at all to somewhat likely, 2= very likely or extremely likely, 3= not applicable
for (v in expc_vars) {
  varname <- sub("BASELINE_","",v)
  hvarname <- paste0(varname, "3")
  ISAEC[,hvarname] <- ifelse(ISAEC[,varname] <= 3, 1,
                             ifelse(ISAEC[,varname] <= 5, 2, 3))
}

#### save dataset
# exclude no consent or no intake
ISAEC_cons <- subset(ISAEC, ISAEC$Consent=="Consented" & ISAEC$received_BL==1)
write.csv(ISAEC_cons, file="processed_data/ISAEC_cons_newvars.csv", na="", row.names=F)
save(ISAEC_cons, file="processed_data/ISAEC_cons_newvars.Rdata")
