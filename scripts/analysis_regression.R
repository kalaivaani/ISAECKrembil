################################### Project: analyses for ISAEC Krembil abstracts #####################################
# date: 2016-03-17
# written by: Kala Sundararajan
# BL and 6 month data from ISAEC clinical database, and consent status from master list


########################### analysis_regression.R: self-efficacy vs baseline characteristics ###############################

load(file="processed_data/ISAEC_cons_newvars.Rdata")
ISAEC <- ISAEC_cons

# restrict sample to ones with self-efficacy scores
ISAEC_SE <- subset(ISAEC, !is.na(ISAEC$BASELINE_SELF_EFFICACY_SCORE))

analysis_vars <- c("AGE","BASELINE_BPREST", "BASELINE_BPACTIVITY", "BASELINE_LEGPREST", "BASELINE_LEGPACTIVITY",
                   "BASELINE_TOTAL", "CMCOUNT","GENDER", "SMOKETYPE", "FREQ_EXERC", "BPLOC", "BPSTOP", "BPHX", 
                   "BPDURATION", "LPDURATION", "NONRADIC", "BOWELCHANGE", "LEGWEAK", "LEGNUMB", "MEDOPI",
                   "BASELINE_RESILIENCE", "BASELINE_RESULT", "CLAIM", "WORK_UNDEREMP", "WORKSATIS", 
                   "WORKPHYS", "WORKMENT", grep("^EXPECT.*3$", names(ISAEC), value=TRUE), "COLLECTION_CENTER")

# restrict to complete cases
ISAEC_SEreg <- na.omit(ISAEC_SE[,c("BASELINE_SELF_EFFICACY_SCORE",analysis_vars)])

# complete cases without pain ratings
nprs_vars <- names(ISAEC_SEreg) %in% c("BASELINE_BPREST", "BASELINE_BPACTIVITY", "BASELINE_LEGPREST", "BASELINE_LEGPACTIVITY")
ISAEC_SEreg_rmNPRS <- na.omit(ISAEC_SEreg[,!nprs_vars])

# full model, including NPRS scores
SE_fmNPRS <- with(ISAEC_SEreg, lm(BASELINE_SELF_EFFICACY_SCORE ~ GENDER + AGE + COLLECTION_CENTER + 
                                       factor(BPDURATION) + factor(LPDURATION) +
                                       BASELINE_BPREST + BASELINE_BPACTIVITY + BASELINE_LEGPREST + 
                                       BASELINE_LEGPACTIVITY + BASELINE_TOTAL + relevel(factor(BASELINE_RESULT), ref="Low risk") +
                                       factor(SMOKETYPE) + BOWELCHANGE + LEGWEAK + LEGNUMB + 
                                       WORK_UNDEREMP + MEDOPI + factor(EXPECTRELIEF3) + factor(EXPECTACT3) + 
                                       factor(EXPECTSLEEP3) + factor(EXPECTWORK3) + factor(EXPECTREC3) + 
                                       factor(EXPECTPREV3)))

# model without NPRS scores
SE_fm_exNPRS <- with(ISAEC_SEreg, lm(BASELINE_SELF_EFFICACY_SCORE ~ GENDER + AGE + COLLECTION_CENTER + 
                                          factor(BPDURATION) + factor(LPDURATION) +
                                          BASELINE_TOTAL + relevel(factor(BASELINE_RESULT), ref="Low risk") +
                                          factor(MARITAL) + factor(SMOKETYPE) + BOWELCHANGE + LEGWEAK + LEGNUMB + 
                                          WORK_UNDEREMP + MEDOPI + factor(EXPECTRELIEF3) + factor(EXPECTACT3) + 
                                          factor(EXPECTSLEEP3) + factor(EXPECTWORK3) + factor(EXPECTREC3) + 
                                          factor(EXPECTPREV3)))

lapply(list(withNPRS = SE_fmNPRS, noNPRS = SE_fm_exNPRS), summary)
step_SEmodel <- stepAIC(SE_fmNPRS, direction="both")


############## clinical characteristics ##################

clin_vars_cont <- c("AGE","BASELINE_BPREST", "BASELINE_BPACTIVITY", "BASELINE_LEGPREST", "BASELINE_LEGPACTIVITY",
               "BASELINE_TOTAL", "CMCOUNT")
clin_vars_cat <- c("GENDER", "SMOKETYPE", "FREQ_EXERC", "BPLOC", "BPSTOP", "BPHX", "BPDURATION", "LPDURATION", 
                   "NONRADIC", "BOWELCHANGE", "LEGWEAK", "LEGNUMB", "MEDOPI")

clin_formula <- paste0("BASELINE_SELF_EFFICACY_SCORE ~ ", 
                          paste(clin_vars_cont, collapse=" + "), " + ", 
                          paste(paste0("factor(", clin_vars_cat, ")"), collapse=" + ")
                      )

SE_clin <- lm(as.formula(clin_formula), data=ISAEC_SEreg)
SE_clin_output <- LMOut(SE_clin)

#### non-clinical characteristics ####

nonclin_vars_cont <- c()
nonclin_vars_cat <- c("BASELINE_RESULT", "CLAIM", "WORK_UNDEREMP", "WORKSATIS", "WORKPHYS",
                      "WORKMENT", grep("^EXPECT.*3$", names(ISAEC), value=TRUE))

full_formula <- paste0(clin_formula, " + ", 
                       paste(nonclin_vars_cont, collapse=" + "), " + ",
                       paste(paste0("factor(", nonclin_vars_cat, ")"), collapse=" + "))

SE_full <- lm(as.formula(full_formula), data=ISAEC_SEreg)
SE_full_output <- LMOut(SE_full)

#### step-wise models ####
SE_clin_step <- stepAIC(SE_clin, direction="both")
SE_full_step <- stepAIC(SE_full, direction="both")


clin_model_outputs <- merge(LMOut(SE_clin), LMOut(SE_clin_step), by="row.names", all=TRUE)
full_model_outputs <- merge(LMOut(SE_full), LMOut(SE_full_step), by="row.names", all=TRUE)
all_model_outputs <- merge(clin_model_outputs, full_model_outputs, by="Row.names", all=TRUE)
all_model_outputs <- all_model_outputs[match(row.names(LMOut(SE_full)),all_model_outputs$Row.names),]
write.csv(all_model_outputs, file="output/reg_models.csv", na="", row.names=FALSE)


fit_stats <- do.call(rbind, lapply(list(clin=SE_clin, clin.step=SE_clin_step, full=SE_full, full.step=SE_full_step), 
                                   reg_chars))
write.csv(fit_stats, file="output/reg_fit_stats.csv")
