## Griffiths M (2022) Useful R Tidyverse commands for data processing

###########
## Setup ## ------------------------------------------------------------------------------------------------------------
###########

## install & load following packages -----------------------------------------------------------------------------------
library(tidyverse) #includes packages ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(readxl)    #import excel sheets

## enter experiment parameters for filenames ---------------------------------------------------------------------------
expname <- "EXPNAME"
expnumber <- "1"

## set working directory -----------------------------------------------------------------------------------------------
getwd()
setwd("C:/Users/USERNAME/FILEPATH") #PC
setwd("C:\\Users\\USERNAME\\FILEPATH") #PC check***
setwd("/Users/USERNAME/FILEPATH") #macOS

## create folder in working directory ----------------------------------------------------------------------------------
dir.create("output",showWarnings = TRUE)

########################
## Import  dataframes ## -----------------------------------------------------------------------------------------------
########################

## import dataframes ---------------------------------------------------------------------------------------------------
dat <- read_csv(file=paste("FILENAME.csv"), na = c("NA", "na", "n.a.", ""))
dat <- read_csv(file=paste(expname,expnumber,"_FILENAME.csv", sep=""), na = c("NA", "na", "n.a.", ""))
dat <- read.delim("clipboard")

## list files in directory ---------------------------------------------------------------------------------------------
file_list <- list.files(path = ".", pattern=".csv") %>% as.data.frame
write_csv(file_list, paste("file_list.csv"))

########################
## Explore dataframes ## -----------------------------------------------------------------------------------------------
########################

## view column types ---------------------------------------------------------------------------------------------------
str(dat)

## view columns & column numbers ---------------------------------------------------------------------------------------
colnames(dat)

## check if dataframes are the same / identical ------------------------------------------------------------------------
test <- (dat == dat1)
which(test == FALSE)

## find difference between observations from two arrays (assumes columns are the same) ---------------------------------
setdiff(LargeDat1, SmallDat1)

## list number of unique / distinct rows -------------------------------------------------------------------------------
test <- distinct(dat1, COLUMNNAME, .keep_all = FALSE)

########################
## Dataframe curation ## -----------------------------------------------------------------------------------------------
########################

## rename a column -----------------------------------------------------------------------------------------------------
dat <- dat %>% rename(NEWNAME = OLDNAME)

## rename all columns containing ---------------------------------------------------------------------------------------
names(dat) <- str_replace_all(names(dat)
                              ,pattern = "_mean"
                              ,replacement = ""
                              )

## find and replace na's & infinite values in dataframe ----------------------------------------------------------------
any(is.na(dat))
dat <- dat %>% replace(is.na(.), 0)
dat <- dat %>% mutate_all(function(x) ifelse(is.infinite(x), 0, x))

## change column data type ---------------------------------------------------------------------------------------------
dat$Geno <- dat$Geno %>% as_factor()
dat <- dat %>% mutate(date = as_Date(date)) ###!check

############################
## Logical operators in R ## -------------------------------------------------------------------------------------------
############################
<         #less than
<=        #less than or equal to
>         #greater than
>=        #greater than or equal to
==        #exactly equal to
!=        #not equal to
!x        #Not x
x | y     #x OR y
x & y     #x AND y
isTRUE(x) #test if X is TRUE

x && y    #logical only

#######################
## Selecting columns ## ---------------------------------------------------------------------------------------------------
#######################

## selecting columns by name -------------------------------------------------------------------------------------------
dat <- dat %>% select(COLUMNNAME1
                      ,COLUMNNAME2
                      ,COLUMNNAME3
                      )

## drop columns by name ------------------------------------------------------------------------------------------------
dat <- dat %>% select(-COLUMNNAME4
                      ,-COLUMNNAME5
                      ,-COLUMNNAME6
                      )

## select column numbers 3,4,5 -----------------------------------------------------------------------------------------
dat <- dat %>% select(matches(c(3,4,5)))

## select all column numbers except 3,4,5 ------------------------------------------------------------------------------
dat <- dat %>% select(-c(3,4,5))

## select all column numbers except 3,4,5 ------------------------------------------------------------------------------
dat <- dat %>% select(c(3,4,5,"COLUMNNAME"))

## select all from column5 ---------------------------------------------------------------------------------------------
dat <- dat %>% select(5:ncol(dat))

## select every column -------------------------------------------------------------------------------------------------
dat <- dat %>% select(everything())

## select column names by starting text --------------------------------------------------------------------------------
dat <- dat %>% select(starts_with("Root"))
dat <- dat %>% select(-starts_with("Root"))

## select column names by ending text ----------------------------------------------------------------------------------
dat <- dat %>% select(ends_with("Width"))
dat <- dat %>% select(-ends_with("Width"))

## select column names by containing text ------------------------------------------------------------------------------
dat <- dat %>% select(contains("_se"))
dat <- dat %>% select(-contains("_se"))

## select columns by type ----------------------------------------------------------------------------------------------
dat <- dat %>% select_if(is.factor)
dat <- dat %>% select_if(is.numeric)

##########################
## Reorganising columns ## ---------------------------------------------------------------------------------------------
##########################

## move column to front ------------------------------------------------------------------------------------------------
dat <- dat %>% select(Geno, everything())  

## move column to back -------------------------------------------------------------------------------------------------
dat <- dat %>% select(-Geno, Geno)          

####################
## Selecting rows ## ---------------------------------------------------------------------------------------------------
####################

dat <- slice(dat, -1,-2)  #remove rows 1,2

#################################
## Convert rownames to columns ## --------------------------------------------------------------------------------------
#################################

has_rownames(dat)
dat <- as_tibble(rownames_to_column(dat, var = "Geno"))

#########################
## Concatenate / Split ## ----------------------------------------------------------------------------------------------
#########################
dat <- dat %>% separate(COLUMNNAME
                        ,into = c("COLUMN1", "COLUMN2", "COLUMN3")
                        ,sep = '_'
                        )

dat <- dat %>% unite(COLUMNNAME
                     ,"COLUMN1", "COLUMN2", "COLUMN3"
                     ,sep = "_"
                     ,remove = FALSE
                     )

####################
## Filtering data ## ---------------------------------------------------------------------------------------------------
####################

## filter function -----------------------------------------------------------------------------------------------------

filtereddata <- dat %>% filter(VARIABLE==value)

## examples of filtering, see logical operators section above ----------------------------------------------------------
filtereddata <- dat %>% filter(Timepoint==0 | Timepoint==finaltimepoint)
filtereddata <- dat %>% filter(Timepoint>=100 & Timepoint<=150)
filtereddata <- dat %>% filter(Depriv=="4" & Induct!=150)
filtereddata <- dat %>% filter(GeneID=="DRO1" | GeneID=="DRO2")
filtereddata <- dat %>% filter(is.na(Induct))
filtereddata <- dat %>% filter(str_detect(COLUMNNAME, pattern="nitra"), ignore.case = TRUE)

###!check
# filtereddata <- dat %>% filter(grepl("nitrate", VARIABLE))
# filtereddata <- dat %>% filter(grepl("nitrate | phosphate", VARIABLE))

filtereddata <- dat %>% drop_na()
filtereddata <- dat %>% drop_na(COLUMN1)

vars <- c("COLUMN1","COLUMN2")
filtereddata <- dat %>% drop_na(any_of(vars))

filtereddata <- dat %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

#####################
## Reordering data ## --------------------------------------------------------------------------------------------------
#####################

## arrange column order	------------------------------------------------------------------------------------------------
dat <- dat %>% select(COLUMNNAME1, COLUMNNAME2, everything())

## arrange column order with COLUMN1 to the back -----------------------------------------------------------------------
dat <- dat %>% select(-COLUMNNAME1, COLUMNNAME2, everything())

## arrange by ascending order values -----------------------------------------------------------------------------------
dat <- dat %>% arrange(COLUMNNAME1)
dat <- dat %>% arrange(COLUMNNAME1, COLUMNNAME2, COLUMNNAME3)

## arrange by descending order values ----------------------------------------------------------------------------------
dat <- dat %>% arrange(desc(COLUMNNAME1))
dat <- dat %>% arrange(desc(COLUMNNAME1, COLUMNNAME2, COLUMNNAME3))

## descending,ascending,descending -------------------------------------------------------------------------------------
dat <- dat %>% arrange(desc(COLUMNNAME1), COLUMNNAME2, desc(COLUMNNAME3))

#################################
## Merge / join / combine data ## --------------------------------------------------------------------------------------
#################################

## join matching values, retain all matches, not rows, produces NAs when values missing --------------------------------
dat <- left_join(dat1, dat2...)

## join matching values, retain matches only ---------------------------------------------------------------------------
dat <- inner_join(dat1, dat2...)

## join matching values, retain all matches, retain all rows, produces NAs when values missing -------------------------
dat <- full_join(dat1, dat2...)
dat <- full_join(dat1, dat2, by = c("PlantID", "Treatment"))

## merge rows from two tables ------------------------------------------------------------------------------------------
dat <- bind_rows(dat1, dat2...)

###!check
# ## rows that appear in both x and y ------------------------------------------------------------------------------------
# intersect(x, y, ...)
# 
# ## rows that appear in x but not y. ------------------------------------------------------------------------------------
# setdiff(x, y, ...)
# 
## rows that appear in x or y (Duplicates removed). union_all() retains duplicates.-------------------------------------
# union(x, y, ...)

#########################
## Mutating / new data ## ----------------------------------------------------------------------------------------------
#########################

dat <- dat %>% mutate(NEWVARIABLE = (VARIABLE1-VARIABLE2))

dat <- dat %>% mutate(NEWVARIABLE = case_when(COLUMNNAME1=="B73" ~ "Control"
                                             ,COLUMNNAME1=="M37W" | Geno=="Mo18W" | Geno=="Tx303" ~ "Mixed"
                                             ))

################################
## Percentages with mean data ## ---------------------------------------------------------------------------------------
################################
## Percentage Change, quantify the change from one number to another and express the change as an increase or decrease -
V1 <- dat[10, "VARIABLE1"] #starting value
V2 <- dat[15, "VARIABLE1"] #final value
PercentageChange <- ((V2-V1)/V1)*100
PercentageChange

#Percentage Difference, find the difference in percentage between two numbers ------------------------------------------
V1 <- dat[10, "VARIABLE1"] #value 1
V2 <- dat[15, "VARIABLE1"] #value 2
PercentageDifference <- ((V1-V2)/((V1+V2)/2))*100
PercentageDifference

#############################
## Save & export dataframe ## ------------------------------------------------------------------------------------------
#############################
write_csv(dat, file="output/FILENAME.csv")
write_csv(dat, paste("FILENAME.csv"))
write_csv(dat, paste0("output/",expname,"_FILENAME.csv"))