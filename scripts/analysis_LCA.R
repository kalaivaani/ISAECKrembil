################################### Project: analyses for ISAEC Krembil abstracts #####################################
# date: 2016-03-23
# written by: Kala Sundararajan
# BL and 6 month data from ISAEC clinical database, and consent status from master list


########################### analysis_LCA.R: self-efficacy vs baseline characteristics ###############################

# set seed
set.seed(38947)

load(file="processed_data/ISAEC_cons_newvars.Rdata")
ISAEC <- ISAEC_cons

# restrict sample to ones with self-efficacy scores
ISAEC_SE <- subset(ISAEC, !is.na(ISAEC$BASELINE_SELF_EFFICACY_SCORE))


clin_vars_cont <- c("AGE","BASELINE_BPREST", "BASELINE_BPACTIVITY", "BASELINE_LEGPREST", "BASELINE_LEGPACTIVITY",
                    "BASELINE_TOTAL", "CMCOUNT")
clin_vars_cat <- c("GENDER", "SMOKETYPE", "FREQ_EXERC", "BPLOC", "BPSTOP", "BPHX", "BPDURATION", "LPDURATION", 
                   "NONRADIC", "BOWELCHANGE", "LEGWEAK", "LEGNUMB", "MEDOPI")
nonclin_vars_cont <- c()
nonclin_vars_cat <- c("BASELINE_RESULT", "CLAIM", "WORK_UNDEREMP", "WORKSATIS", "WORKPHYS",
                      "WORKMENT", grep("^EXPECT.*3$", names(ISAEC), value=TRUE))

analysis_vars <- c(clin_vars_cont, clin_vars_cat, nonclin_vars_cont, nonclin_vars_cat)

# restrict to complete cases
ISAEC_SE <- na.omit(ISAEC_SE[,c("BASELINE_SELF_EFFICACY_SCORE",analysis_vars)])


# create formulas
clin_formula <- paste0("BASELINE_SELF_EFFICACY_SCORE ~ ", 
                       paste(clin_vars_cont, collapse=" + "), " + ", 
                       paste(paste0("factor(", clin_vars_cat, ")"), collapse=" + ")
                        )

full_formula <- paste0(clin_formula, " + ", 
                       paste(nonclin_vars_cont, collapse=" + "), " + ",
                       paste(paste0("factor(", nonclin_vars_cat, ")"), collapse=" + "))
# remove pain location
clin_formula_nopainloc <- sub("factor\\(BPLOC\\) \\+ ", "", clin_formula)
full_formula_nopainloc <- sub("factor\\(BPLOC\\) \\+ ", "", clin_formula)


############# LCR models ##################
SE_clusters <- stepFlexmix(BASELINE_SELF_EFFICACY_SCORE ~ 1, data=ISAEC_SE, k=1:5)
plot(SE_clusters, what="BIC")
best_SEmodel <- getModel(SE_clusters, which="BIC")
summary(best_SEmodel)
parameters(best_SEmodel)
ISAEC_SE$cluster <- best_SEmodel@cluster
cluster_SEstats <- with(ISAEC_SE, tapply(BASELINE_SELF_EFFICACY_SCORE, cluster, function(x) {
  c(mean=mean(x), sd=sd(x), count=length(x))
}))
ggplot(ISAEC_SE, aes(x=BASELINE_SELF_EFFICACY_SCORE)) +
  geom_histogram(aes(fill=factor(cluster)), alpha=0.5, position="identity", bins=50) +
  ggtitle("Two latent components of baseline self-efficacy\nin ISAEC patients") +
  scale_fill_discrete(name="Cluster") +
  theme_bw(base_size=20)



SE_clusters_bploc <- stepFlexmix(BASELINE_SELF_EFFICACY_SCORE ~ 1, concomitant=FLXPmultinom(~BPLOC), data=ISAEC_SE, k=1:5)
plot(SE_clusters_bploc, what="BIC")


clin_step_model <- stepFlexmix(as.formula(clin_formula), data=ISAEC_SE, k=1:5)
plot(clin_step_model, what="BIC")
best_SEmodel <- getModel(clin_step_model, which="BIC")
parameters(best_SEmodel)

clin_step_model_painloc <- stepFlexmix(as.formula(clin_formula), concomitant = FLXPmultinom(~BPLOC), data=ISAEC_SE, k=1:5)
plot(clin_step_model_painloc, what="BIC")
