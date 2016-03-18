################################### Project: analyses for ISAEC Krembil abstracts #####################################
# date: 2016-03-15
# written by: Kala Sundararajan
# BL and 6 month data from ISAEC clinical database, and consent status from master list


########################### do.R: load, clean/merge, and analyse data ###############################

rm(list = ls())
setwd("Z:/Documents/work/isaec/Krembil abstracts")
library(plyr)
library(lubridate)
library(ggplot2)
library(MASS)
library(flexmix)

#### load functions ####
source("Y:/LEAP/24. LEAP OA Data Quality/scripts/functions.R")
source("scripts/functions.R")

#### load data ####
source("scripts/load.R")

#### clean data ####
source("scripts/clean.R")

#### analysis ####
source("scripts/analysis_1.R")           # basic stats
source("scripts/analysis_bivariate.R")   # bivariate stats (self-efficacy)
source("scripts/analysis_regression.R")  # simple regression model
source("scripts/analysis_LCA.R")         # latent class analysis