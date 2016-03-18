################################### Project: analyses for ISAEC Krembil abstracts #####################################
# date: 2016-03-17
# written by: Kala Sundararajan
# BL and 6 month data from ISAEC clinical database, and consent status from master list


########################### analysis_regression.R: self-efficacy vs baseline characteristics ###############################

load(file="processed_data/ISAEC_cons_newvars.Rdata")
ISAEC <- ISAEC_cons

# restrict sample to ones with self-efficacy scores
ISAEC_SE <- subset(ISAEC, !is.na(ISAEC$BASELINE_SELF_EFFICACY_SCORE))

analysis_vars <- c("GENDER", "AGE","COLLECTION_CENTER", "BPDURATION", "LPDURATION",
                   "BASELINE_BPREST", "BASELINE_BPACTIVITY", "BASELINE_LEGPREST", 
                   "BASELINE_LEGPACTIVITY", "BASELINE_TOTAL", "BASELINE_RESULT", "BASELINE_RESILIENCE", "MARITAL","SMOKETYPE",
                   "FREQ_EXERC", "BPLOC","BPSTOP","BPHX","BPDURATION","LPDURATION","CLAIM","NONRADIC","PAINPERC",
                   "BOWELCHANGE","LEGWEAK","LEGNUMB","WORK_UNDEREMP","MEDOPI","EXPECTRELIEF3",
                   "EXPECTACT3","EXPECTSLEEP3","EXPECTWORK3","EXPECTREC3","EXPECTPREV3")

# restrict to complete cases
ISAEC_SEreg <- ISAEC_SE[,c("BASELINE_SELF_EFFICACY_SCORE",analysis_vars)]
ISAEC_SEreg.nm <- na.omit(ISAEC_SEreg)

# complete cases without pain ratings
nprs_vars <- names(ISAEC_SEreg) %in% c("BASELINE_BPREST", "BASELINE_BPACTIVITY", "BASELINE_LEGPREST", "BASELINE_LEGPACTIVITY")
ISAEC_SEreg_rmNPRS <- ISAEC_SEreg[,!nprs_vars]
ISAEC_SEreg_rmNPRS.nm <- na.omit(ISAEC_SEreg_rmNPRS)

# full model, including NPRS scores
SE_fmNPRS <- with(ISAEC_SEreg.nm, lm(BASELINE_SELF_EFFICACY_SCORE ~ GENDER + AGE + COLLECTION_CENTER + 
                                       factor(BPDURATION) + factor(LPDURATION) +
                                       BASELINE_BPREST + BASELINE_BPACTIVITY + BASELINE_LEGPREST + 
                                       BASELINE_LEGPACTIVITY + BASELINE_TOTAL + BASELINE_RESULT +
                                       factor(MARITAL) + factor(SMOKETYPE) + BOWELCHANGE + LEGWEAK + LEGNUMB + 
                                       WORK_UNDEREMP + MEDOPI + factor(EXPECTRELIEF3) + factor(EXPECTACT3) + 
                                       factor(EXPECTSLEEP3) + factor(EXPECTWORK3) + factor(EXPECTREC3) + 
                                       factor(EXPECTPREV3)))

# model without NPRS scores
SE_fm_exNPRS <- with(ISAEC_SEreg.nm, lm(BASELINE_SELF_EFFICACY_SCORE ~ GENDER + AGE + COLLECTION_CENTER + 
                                          factor(BPDURATION) + factor(LPDURATION) +
                                          BASELINE_TOTAL + BASELINE_RESULT +
                                          factor(MARITAL) + factor(SMOKETYPE) + BOWELCHANGE + LEGWEAK + LEGNUMB + 
                                          WORK_UNDEREMP + MEDOPI + factor(EXPECTRELIEF3) + factor(EXPECTACT3) + 
                                          factor(EXPECTSLEEP3) + factor(EXPECTWORK3) + factor(EXPECTREC3) + 
                                          factor(EXPECTPREV3)))

lapply(list(withNPRS = SE_fmNPRS, noNPRS = SE_fm_exNPRS), summary)
