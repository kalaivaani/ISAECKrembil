################################### Project: analyses for ISAEC Krembil abstracts #####################################
# date: 2016-03-15
# written by: Kala Sundararajan
# BL and 6 month data from ISAEC clinical database, and consent status from master list


########################### analysis_1.R: basic metrics ###############################
load(file="processed_data/ISAEC_cons_newvars.Rdata")

# rename ISAEC_cons
ISAEC <- ISAEC_cons

# find most recent intake with 6m received
cutoffdate <- max(subset(ISAEC, ISAEC$received_6m==1)$DOE, na.rm=TRUE)
firstdate <- min(ISAEC$DOE, na.rm=TRUE)

# 6 month followup rate monthly from inception to cut off date
months <- seq(firstdate, cutoffdate, "months")
names(months) <- months

x6m_rates <- lapply(months, function(m, cut=cutoffdate, dt=ISAEC) {
  relevant_data <- subset(dt, dt$DOE >= m & dt$DOE <= cut)
  n_intakes <- nrow(relevant_data)
  n_6m <- nrow(subset(relevant_data, relevant_data$received_6m==1))
  return(c(Intakes=n_intakes, '6 month form'=n_6m, Rate=round((n_6m/n_intakes)*100, 2)))
})
x6m_rates <- data.frame(do.call(rbind, x6m_rates))
x6m_rates$startdate <- months

write.csv(x6m_rates, file="output/6m_rates.csv", na="")

x6m_graph <- ggplot(data=x6m_rates, aes(x=startdate, y=Rate)) + geom_point() + geom_line() + 
  ggtitle("Monthly 6 month follow up rate for consented ISAEC patients\nIntakes up to Sept 2015") +
  theme_grey()

# View(sapply(ISAEC, function(x) paste(class(x), collapse=",")))

# variable lists
contvars <- c("AGE", "BASELINE_BPREST", "BASELINE_BPACTIVITY", "BASELINE_LEGPREST", 
              "BASELINE_LEGPACTIVITY", "BASELINE_TOTAL", "BASELINE_OVERALL", "BASELINE_DISTRESS", "BASELINE_SELF_EFFICACY_SCORE", 
              "BASELINE_RESILIENCE", "CMCOUNT")

demo_catvars <- c("GENDER", "COLLECTION_CENTER", "MARITAL", "NONWHITE","SMOKETYPE","FREQ_EXERC")

pain_catvars <- c("BPLOC","BPSTOP","BPSTART","BPHX","BPSX","BPDURATION","LPDURATION","CLAIM","NONRADIC","PAINPERC","BOWELCHANGE","LEGWEAK","LEGNUMB",
                  "BASELINE_RESULT")

work_vars <- c("WORKSTAT", "WORK_UNDEREMP", "WORKSATIS", "WORKPHYS","WORKMENT")

med_catvars <- c("MEDHYD","MEDOXY","MEDPERC","MEDTYL34","MEDNSAID","MEDTYL", "MEDOPI")

txinv_catvars <- c("TXCONSULT","TXSURG","TXACU","TXMASSAGE","TXPHYSIO","TXCHIRO","INVEMG","INVBONESCAN","INVMRI","INVCT","INVXRAY")

cm_catvars <- c("CM_RA","CM_OA","CM_PSYC","CM_DEP","CM_CANC","CM_ANEM","CM_ULC","CM_LIV","CM_KIDN","CM_DIAB","CM_PVD","CM_AST","CM_CAD","CM_STROKE",
                "CM_HBP","CM_HCHOL","CM_ANY")

expt_catvars <- c("EXPECTRELIEF","EXPECTACT","EXPECTSLEEP","EXPECTWORK","EXPECTREC","EXPECTPREV")
expt3_catvars <- c("EXPECTRELIEF3","EXPECTACT3","EXPECTSLEEP3","EXPECTWORK3","EXPECTREC3","EXPECTPREV3")

all_catvars <- c(demo_catvars, pain_catvars, work_vars, med_catvars, txinv_catvars, cm_catvars, expt_catvars, expt3_catvars)

# descriptive statistics
desc_cont <- MeanSD(ISAEC, contvars)
write.csv(desc_cont, file="output/desc_means.csv")

desc_freq <- MakeTable(ISAEC, all_catvars)
write.csv(desc_freq, file="output/desc_freq.csv")
