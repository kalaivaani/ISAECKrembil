################################### Project: analyses for ISAEC Krembil abstracts #####################################
# date: 2016-03-15
# written by: Kala Sundararajan
# BL and 6 month data from ISAEC clinical database, and consent status from master list


########################### load.R: load data ###############################

# baseline data
BL <- read.csv("raw_data/ISAEC_BL_20160314.csv", stringsAsFactors=FALSE)

# 6 month data
x6m <- read.csv("raw_data/ISAEC_6m_20160314.csv", stringsAsFactors=FALSE)

# consent status
consent <- read.csv("raw_data/ISAEC_consent_20160315.csv", stringsAsFactors=FALSE)