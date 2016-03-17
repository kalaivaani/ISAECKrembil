################################### Project: analyses for ISAEC Krembil abstracts #####################################
# date: 2016-03-15
# written by: Kala Sundararajan
# BL and 6 month data from ISAEC clinical database, and consent status from master list


########################### functions.R ###############################

#### score self-efficacy ####
score_effic <- function(responses) {
  count <- sum(!is.na(responses))
  score <- ifelse(count >= 4, mean(responses, na.rm=TRUE), NA)
  return(score)
}

#### score resilience ####
score_resil <- function(responses) {
  count <- sum(!is.na(responses))
  score <- ifelse(count==2, sum(responses), NA)
  return(score)
}

#### score start back ####
score_start <- function(responses, thresholds=c(2, 1, 5, 5, 7, 3, 6, 7, 3)) {
  score <- NULL
  for (i in 1:9) {
    score[i] <- ifelse(responses[i] >= thresholds[i], 1, 0)
  }
  TotalScore <- sum(score[1:9])
  PsychScore <- sum(score[5:9])
  
  risk <- ifelse(TotalScore <= 3, "Low risk",
                 ifelse(PsychScore <= 3, "Medium risk", "High risk"))
  
  count <- sum(!is.na(responses))
  
  TotalScore <- ifelse(count==9, TotalScore, NA)
  PsychScore <- ifelse(count==9, PsychScore, NA)
  risk <- ifelse(count==9, risk, NA)
  
  output <- list(risk=risk, overall=TotalScore, psych=PsychScore)
  return(output)
}


