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

# set seed
set.seed(38947)

# model
predictors <- FLXMRglm(~ factor(BPDURATION) + factor(LPDURATION) + CLAIM + BPSTOP + 
                         BASELINE_BPREST + BASELINE_BPACTIVITY + BASELINE_LEGPREST + 
                         BASELINE_LEGPACTIVITY + BASELINE_TOTAL + factor(BASELINE_RESULT)
                         BOWELCHANGE + LEGWEAK + LEGNUMB + MEDOPI + factor(EXPECTRELIEF3) + factor(EXPECTACT3) + 
                         factor(EXPECTSLEEP3) + factor(EXPECTWORK3) + factor(EXPECTREC3) + 
                         factor(EXPECTPREV3) + GENDER + AGE + COLLECTION_CENTER + factor(MARITAL) + factor(SMOKETYPE) + 
                         WORK_UNDEREMP)

clin_predictors <- FLXMRglm(~ factor(BPDURATION) + factor(LPDURATION) + BPSTOP + 
                              BASELINE_BPREST + BASELINE_BPACTIVITY + BASELINE_LEGPREST + 
                              BASELINE_LEGPACTIVITY +
                              BOWELCHANGE + LEGWEAK + LEGNUMB + MEDOPI + GENDER + AGE + factor(SMOKETYPE))
LC_SEmodel <- stepFlexmix(BASELINE_SELF_EFFICACY_SCORE~factor(BPDURATION) + factor(LPDURATION) + BPSTOP + 
                            BASELINE_BPREST + BASELINE_BPACTIVITY + BASELINE_LEGPREST + 
                            BASELINE_LEGPACTIVITY + BOWELCHANGE + LEGWEAK + LEGNUMB + MEDOPI + GENDER + 
                            AGE + factor(SMOKETYPE), data=ISAEC_SEreg.nm, k=1:5)

plot(LC_SEmodel, what="BIC")
best_SEmodel <- getModel(LC_SEmodel, which="BIC")
parameters(best_SEmodel)
ISAEC_SEreg.nm$cluster <- best_SEmodel@cluster
cluster_SEstats <- with(ISAEC_SEreg.nm, tapply(BASELINE_SELF_EFFICACY_SCORE, cluster, function(x) {
  c(mean=mean(x), sd=sd(x), count=length(x))
}))

ggplot(ISAEC_SEreg.nm, aes(y=BASELINE_SELF_EFFICACY_SCORE, x=factor(cluster))) + geom_boxplot()
ggplot(ISAEC_SEreg.nm, aes(x=BASELINE_SELF_EFFICACY_SCORE, color=factor(cluster))) + geom_density()

ggplot(ISAEC_SEreg.nm, aes(x=factor(MEDOPI), y=BASELINE_SELF_EFFICACY_SCORE)) + geom_boxplot() + facet_grid(.~cluster)


ggplot(ISAEC_SEreg.nm, aes(x=factor(LPDURATION), y=BASELINE_SELF_EFFICACY_SCORE)) + geom_boxplot() + facet_grid(.~cluster)

stepLC_SEmodel <- stepFlexmix(BASELINE_SELF_EFFICACY_SCORE ~ GENDER + AGE + 
                        factor(BPDURATION) + factor(LPDURATION) +
                        BASELINE_BPREST + BASELINE_BPACTIVITY + BASELINE_LEGPREST + 
                        BASELINE_LEGPACTIVITY + BASELINE_TOTAL + 
                        factor(MARITAL) + factor(SMOKETYPE) + BOWELCHANGE + LEGWEAK + LEGNUMB + 
                        WORK_UNDEREMP + MEDOPI, data=ISAEC_SEreg.nm, k=1:5, nrep=5)

plot(stepLC_SEmodel, what="BIC")
best_model <- getModel(stepLC_SEmodel, which="BIC")
parameters(best_model)

summary(LC_SEmodel)

coef_LC_SEmodel <- refit(LC_SEmodel)
View(LC_SEmodel@cluster)

ISAEC_SEreg.nm$cl2 <- LC_SEmodel@cluster
