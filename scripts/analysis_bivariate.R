################################### Project: analyses for ISAEC Krembil abstracts #####################################
# date: 2016-03-17
# written by: Kala Sundararajan
# BL and 6 month data from ISAEC clinical database, and consent status from master list


########################### analysis_bivariate.R: self-efficacy vs baseline characteristics ###############################
load(file="processed_data/ISAEC_cons_newvars.Rdata")
ISAEC <- ISAEC_cons

# restrict sample to ones with self-efficacy scores
ISAEC_SE <- subset(ISAEC, !is.na(ISAEC$BASELINE_SELF_EFFICACY_SCORE))

# variable lists
contvars <- c("AGE", "BASELINE_BPREST", "BASELINE_BPACTIVITY", "BASELINE_LEGPREST", 
              "BASELINE_LEGPACTIVITY", "BASELINE_TOTAL", "BASELINE_OVERALL", "BASELINE_DISTRESS", 
              "BASELINE_RESILIENCE", "CMCOUNT")
names(contvars) = contvars

demo_catvars <- c("GENDER", "COLLECTION_CENTER", "MARITAL", "NONWHITE","SMOKETYPE","FREQ_EXERC")
names(demo_catvars) <- demo_catvars

pain_catvars <- c("BPLOC","BPSTOP","BPSTART","BPHX","BPSX","BPDURATION","LPDURATION","CLAIM","NONRADIC","PAINPERC","BOWELCHANGE","LEGWEAK","LEGNUMB",
                  "BASELINE_RESULT")
names(pain_catvars) <- pain_catvars

work_catvars <- c("WORK_UNDEREMP", "WORKSATIS", "WORKPHYS","WORKMENT")
names(work_catvars) <- work_catvars

med_catvars <- c("MEDHYD","MEDOXY","MEDPERC","MEDTYL34","MEDNSAID","MEDTYL","MEDOPI")
names(med_catvars) <- med_catvars

txinv_catvars <- c("TXCONSULT","TXSURG","TXACU","TXMASSAGE","TXPHYSIO","TXCHIRO","INVEMG","INVBONESCAN","INVMRI","INVCT","INVXRAY")
names(txinv_catvars) <- txinv_catvars

cm_catvars <- c("CM_RA","CM_OA","CM_PSYC","CM_DEP","CM_CANC","CM_ANEM","CM_ULC","CM_LIV","CM_KIDN","CM_DIAB","CM_PVD","CM_AST","CM_CAD","CM_STROKE",
                "CM_HBP","CM_HCHOL","CM_ANY")
names(cm_catvars) <- cm_catvars

expt_catvars <- c("EXPECTRELIEF3","EXPECTACT3","EXPECTSLEEP3","EXPECTWORK3","EXPECTREC3","EXPECTPREV3")
names(expt_catvars) <- expt_catvars

all_catvars <- c(demo_catvars, pain_catvars, work_catvars, med_catvars, txinv_catvars, cm_catvars, expt_catvars)


# SE vs continuous variables
SE_cont_bivar <- lapply(contvars, function(var) {
  lm(ISAEC_SE$BASELINE_SELF_EFFICACY_SCORE ~ ISAEC_SE[,var])
})
coefs_pvals <- lapply(SE_cont_bivar, function(x) c(coef=summary(x)$coefficients[2,1], p=summary(x)$coefficients[2,4],r.sq=summary(x)$r.squared,n=nobs(x)))
coef_table <- do.call(rbind, coefs_pvals)

write.csv(coef_table, file="output/SE_bivariate_regression.csv")


# SE vs categorical variables
SE_cat_means <- lapply(all_catvars, function(var, dat=ISAEC_SE) {
  meansd <- tapply(dat$BASELINE_SELF_EFFICACY_SCORE, dat[,var], function(x) c(mean=round(mean(x, na.rm=TRUE),2), sd=round(sd(x, na.rm=TRUE),2), n=length(x)))
  names(meansd) <- paste(var, names(meansd), sep=":")
  return(do.call(rbind, meansd))
})
SE_cat_means_table <- do.call(rbind, SE_cat_means)
write.csv(SE_cat_means_table, file="output/SE_cat_means.csv")

# anova models
SE_cat_anova <- lapply(all_catvars, function(var, dat=ISAEC_SE) {
  aov(dat$BASELINE_SELF_EFFICACY_SCORE ~ factor(dat[,var]))
})

SE_cat_rsq_p <- lapply(SE_cat_anova, function(mod) {
  sumsq <- summary(mod)[[1]][,2]
  rsq <- sumsq[1] / sum(sumsq)
  pval <- summary(mod)[[1]][1,5]
  return(c(R.sq = rsq, p=pval))
})
SE_cat_rsq_p <- do.call(rbind, SE_cat_rsq_p)
write.csv(SE_cat_rsq_p, file="output/SE_cat_models.csv")

