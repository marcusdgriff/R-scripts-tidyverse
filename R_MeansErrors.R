## Griffiths M (2022)
## Means & Error data processing

################
## User setup ## -------------------------------------------------------------------------------------------------------
################
# 1) Set working directory to where input files are
# 2) Install or load following packages
library(tidyverse) ## includes packages ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats

## **optional** enter experiment parameters ----------------------------------------------------------------------------
expname <- "NAM"
expnumber <- "001"

## import dataframes ---------------------------------------------------------------------------------------------------
fulldata <- read_csv(paste0("YOUR_DATA.csv"), na = c("NA", "na", "n.a.", ""))

names(fulldata)
head(fulldata)
summary(fulldata)
unique(fulldata$Geno)

##############################
## Dataframe pre-processing ## -----------------------------------------------------------------------------------------
##############################

## Remove unnecessary columns, only leave Factors and variables --------------------------------------------------------
dat <- fulldata %>% ungroup() %>% select(-Experiment
                                         ,-Date
                                         ,-Plant
                                         )

## **optional** if you have treatments then rename ---------------------------------------------------------------------
dat <- rename(dat, Treatment = YOUR_TREATMENT_NAME1)
# dat <- rename(dat, Treatment2 = YOUR_TREATMENT_NAME2)
# dat <- rename(dat, Treatment3 = YOUR_TREATMENT_NAME3)

## define first variables column ---------------------------------------------------------------------------------------
names(dat)
names(dat[3:length(dat)])
varcol <- 3:length(dat)

###############################
## Mean & Error calculations ## ----------------------------------------------------------------------------------------
###############################

## define standard error function --------------------------------------------------------------------------------------
stderror <- function(x) sd(na.omit(x))/sqrt(length(na.omit(x)))

## calculate means & errors --------------------------------------------------------------------------------------------
dat_mean <- dat %>% group_by(Geno
                             ,Treatment
                             #,Treatment2
                             #,Treatment3
                             ) %>% summarize_at(colnames(.)[varcol], mean, na.rm = TRUE)

dat_se <- dat %>% group_by(Geno
                           ,Treatment
                           #,Treatment2
                           #,Treatment3
                           ) %>% summarize_at(colnames(.)[varcol], stderror)

##  **optional** if unused columns not removed earlier then redefine length of dataframe  ------------------------------
# names(dat_mean)
# names(dat_mean[3:length(dat_mean)])
# varcol <- 3:length(dat_mean)

## add means & errors to column names & merge  -------------------------------------------------------------------------
names(dat_mean)[varcol] <- paste0(names(dat_mean)[varcol],"_mean")
names(dat_se)[varcol] <- paste0(names(dat_se)[varcol],"_se")
dat_meanErrors <- merge(dat_mean
                        ,dat_se
                        ,by=c("Geno"
                              ,"Treatment"
                              #,"Treatment2"
                              #,"Treatment3"
                              ))

rm(dat
   ,dat_mean
   ,dat_meanErrors
   ,dat_se
   ,varcol
   )

## save mean & errors dataframe  ---------------------------------------------------------------------------------------
dir.create(paste0(expname,expnumber,"_dataprocessing"),showWarnings = TRUE)
write_csv(dat_meanErrors, paste0(expname,expnumber,"_dataprocessing/",expname,expnumber,"_MeansErrors.csv"))
