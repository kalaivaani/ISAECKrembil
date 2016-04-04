################################### Project: analyses for ISAEC Krembil abstracts #####################################
# date: 2016-03-24
# written by: Kala Sundararajan
# BL and 6 month data from ISAEC clinical database, and consent status from master list


########################### analysis_randomforest.R: self-efficacy vs baseline characteristics ###############################

# set seed
set.seed(38947)

# load data
load(file="processed_data/ISAEC_cons_newvars.Rdata")
ISAEC <- ISAEC_cons

# restrict sample to ones with self-efficacy scores
ISAEC_SE <- subset(ISAEC, !is.na(ISAEC$BASELINE_SELF_EFFICACY_SCORE))

# variable lists 
clin_vars_cont <- c("AGE","BASELINE_BPREST", "BASELINE_BPACTIVITY", "BASELINE_LEGPREST", "BASELINE_LEGPACTIVITY",
                    "BASELINE_TOTAL", "CMCOUNT")
clin_vars_cat <- c("GENDER", "SMOKETYPE", "FREQ_EXERC", "BPLOC", "BPSTOP", "BPHX", "BPDURATION", "LPDURATION", 
                   "NONRADIC", "BOWELCHANGE", "LEGWEAK", "LEGNUMB", "MEDOPI")
nonclin_vars_cont <- c()
nonclin_vars_cat <- c("BASELINE_RESULT", "CLAIM", "WORK_UNDEREMP", "WORKSATIS", "WORKPHYS",
                      "WORKMENT", grep("^EXPECT.*3$", names(ISAEC), value=TRUE))

analysis_vars <- c(clin_vars_cont, clin_vars_cat, nonclin_vars_cont, nonclin_vars_cat)

# create dataset without missing values
ISAEC_SE <- na.omit(ISAEC_SE[,c("SUBJECT_ID", "BASELINE_SELF_EFFICACY_SCORE", analysis_vars)])

# create formulae
clin_formula <- paste0("BASELINE_SELF_EFFICACY_SCORE ~ ", 
                       paste(clin_vars_cont, collapse=" + "), " + ", 
                       paste(paste0("as.factor(", clin_vars_cat, ")"), collapse=" + ")
)

full_formula <- paste0(clin_formula, " + ", 
                       paste(nonclin_vars_cont, collapse=" + "), " + ",
                       paste(paste0("as.factor(", nonclin_vars_cat, ")"), collapse=" + "))

full_formula_nofactor <- gsub("factor\\(", "", full_formula)
full_formula_nofactor <- gsub("\\)", "", full_formula_nofactor)

# convert factor variables
catvars <- c(clin_vars_cat, nonclin_vars_cat)
ISAEC_SE[,catvars] <- lapply(ISAEC_SE[,catvars], function(var) {
  var <- as.factor(var)
  return(var)
})

# random forest model of baseline self-efficacy
SE_RFmodel <- randomForest(ISAEC_SE[,analysis_vars], y=ISAEC_SE$BASELINE_SELF_EFFICACY_SCORE, importance=TRUE)
varImpPlot(SE_RFmodel)
print(SE_RFmodel)

ISAEC_SE$predictSE <- predict(SE_RFmodel)
ggplot(ISAEC_SE, aes(x=BASELINE_SELF_EFFICACY_SCORE, y=predictSE)) + geom_point(position="jitter") + geom_smooth(method=lm) 
