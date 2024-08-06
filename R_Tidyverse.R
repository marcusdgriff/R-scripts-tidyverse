## Griffiths M (2024) Useful R Tidyverse commands for data processing

###########
## Setup ## --------------------------------------------------------------------
###########

## install & load following packages -------------------------------------------
library(tidyverse) #includes packages ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(readxl)    #import excel sheets
library(janitor)

## enter experiment parameters for filenames -----------------------------------
expname <- "EXPNAME1"

## set working directory, or use "Session > Set working dir > Choose dir" ------
getwd()
setwd("C:/Users/USERNAME/FILEPATH") #PC
setwd("C:\\Users\\USERNAME\\FILEPATH") #PC check***
setwd("/Users/USERNAME/FILEPATH") #macOS

## create folder in working directory ------------------------------------------
dir.create("output",showWarnings = TRUE)

#######################
## Import dataframes ## --------------------------------------------------------
#######################

## import a single dataframe from CSV file -------------------------------------
dat <- read_csv(paste0("FILENAME.csv"), na = c("NA", "na", "n.a.", ""))
dat <- read_csv(paste0(expname,"_FILENAME.csv"), na = c("NA", "na", "n.a.", ""))
dat <- read_csv(file=paste(expname,"_FILENAME.csv", sep=""), na = c("NA", "na", "n.a.", ""))

## import a single dataframe from clipboard ------------------------------------
dat <- read.delim("clipboard")

# import multiple dataframes from CSV files and bind them together with a filename column
dat <- list.files(path = ".", pattern = ".csv") %>% 
  map_df(~ read_csv(.x) %>% mutate(filename = .x)) %>%
  mutate(filename = str_remove(filename, "\\.csv$"))

## list files in directory -----------------------------------------------------
file_list <- list.files(path = ".", pattern=".csv") %>% as.data.frame
write_csv(file_list, paste("file_list.csv"))

########################
## Explore dataframes ## -------------------------------------------------------
########################

## view column types -----------------------------------------------------------
str(dat)

## view columns & numbers ------------------------------------------------------
colnames(dat)

## check if two dataframes are the same ----------------------------------------


## find difference between observations from two arrays ------------------------
setdiff(LargeDat1, SmallDat1)

## list number of unique/distinct rows -----------------------------------------
distinct_count <- distinct(dat, COLUMNNAME, .keep_all = FALSE)
distinct_count

############################
## Dataframe manipulation ## ---------------------------------------------------
############################

## rename a column -------------------------------------------------------------
dat <- dat %>% rename(NEWNAME = OLDNAME)

## rename all column names containing string -----------------------------------
names(dat) <- str_replace_all(names(dat)
                              ,pattern = "_mean"
                              ,replacement = ""
                              )

## column column names using janitor package  ----------------------------------
dat <- dat %>% clean_names()
dat <- dat %>% clean_names(case = "big_camel")
dat <- dat %>% clean_names(case = "lower_camel")
dat <- dat %>% clean_names(case = "sentence")

names(dat) <- make_clean_names(names(dat), case = "big_camel")
names(dat) <- make_clean_names(names(dat), case = "sentence")
names(dat) <- make_clean_names(names(dat))

## rename all row names in a column containing string --------------------------
dat <- dat %>% mutate(
  COLUMNNAME = str_replace(
    COLUMNNAME
    ,pattern = "-"
    ,replacement = ""
    ))

## replace NA's and infinite values in dataframe -------------------------------
any(is.na(dat))
dat <- dat %>% replace(is.na(.), 0)
dat <- dat %>% mutate_all(function(x) ifelse(is.infinite(x), 0, x))

###!check
# dat <- dat %>% replace_na(list(value = 0))
# dat <- dat %>% mutate_all(~ ifelse(is.infinite(.), 0, .))

## set number of decimal places ------------------------------------------------
dat$COLUMNNAME <- format(dat$COLUMNNAME, digits = 3)

## change column data type -----------------------------------------------------
dat$Geno <- dat$Geno %>% as_factor()
dat$TotalRootLength <- dat$TotalRootLength %>% as_numeric()
dat <- dat %>% mutate(date = as.Date(date))

############################
## Logical operators in R ## ---------------------------------------------------
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
## Selecting columns ## --------------------------------------------------------
#######################

## select or drop columns by name ----------------------------------------------
dat <- dat %>% select(COLUMNNAME1
                      ,COLUMNNAME2
                      ,COLUMNNAME3
                      )

dat <- dat %>% select(-COLUMNNAME4
                      ,-COLUMNNAME5
                      ,-COLUMNNAME6
                      )

## select or drop by column numbers --------------------------------------------
dat <- dat %>% select(matches(c(3,4,5)))
dat <- dat %>% select(-c(3,4,5))

## select column numbers & name ------------------------------------------------
dat <- dat %>% select(c(3,4,5,"COLUMNNAME"))

## select all columns from column5 ---------------------------------------------
dat <- dat %>% select(5:ncol(dat))

## select every column ---------------------------------------------------------
dat <- dat %>% select(everything())

## select or drop column names by starting text --------------------------------
dat <- dat %>% select(starts_with("Root"))
dat <- dat %>% select(-starts_with("Root"))

## select or drop column names by ending text ----------------------------------
dat <- dat %>% select(ends_with("Width"))
dat <- dat %>% select(-ends_with("Width"))

## select or drop column names by containing text ------------------------------
dat <- dat %>% select(contains("_mean"))
dat <- dat %>% select(-contains("_mean"))

## select columns by type ------------------------------------------------------
dat <- dat %>% select_if(is.factor)
dat <- dat %>% select_if(is.numeric)

##########################
## Reorganising columns ## -----------------------------------------------------
##########################

## move column to front --------------------------------------------------------
dat <- dat %>% select(Geno, everything())  

## move column to back ---------------------------------------------------------
dat <- dat %>% select(-Geno, Geno)          

####################
## Selecting rows ## -----------------------------------------------------------
####################

## remove rows 1,2
dat <- slice(dat, -1,-2) 

#################################
## Convert rownames to columns ## ----------------------------------------------
#################################
has_rownames(dat)
dat <- as_tibble(rownames_to_column(dat, var = "Geno"))

#########################
## Concatenate / Split ## ------------------------------------------------------
#########################

## split a string into multiple columns e.g. WT_Drought_rep1 --> WT, Drought, rep1
dat <- dat %>% separate(COLUMNNAME
                        ,into = c("COLUMN1", "COLUMN2", "COLUMN3")
                        ,sep = '_'
                        )

## unite multiple columns into a string e.g. WT, Drought, rep1 --> WT_Drought_rep1
dat <- dat %>% unite(COLUMNNAME
                     ,"COLUMN1", "COLUMN2", "COLUMN3"
                     ,sep = "_"
                     ,remove = FALSE
                     )

#####################################
## Make dataframe into Tidy format ## ------------------------------------------
######################################
dat_tidy <- dat %>% 
  gather("sample", "COLUMNNAME", c(sample1, sample2, sample3))

################################
## Make Long / Wide dataframe ## -----------------------------------------------
################################
dat_wide <- dat %>% pivot_wider(
  names_from = Date
  ,names_prefix = "COLUMNNAME_"
  ,values_from = COLUMN1
  )

dat_long <- dat %>% pivot_longer(
  cols = 3:4
  ,names_to ="RootClass"
  ,values_to = "RootLength_mm"
  ,values_drop_na = FALSE
  )

#################################
## Dataframe filtering/sorting ## ----------------------------------------------
#################################

## filter function -------------------------------------------------------------
filtereddata <- dat %>% filter(VARIABLE==value)

## examples of filtering, see logical operators section above ------------------
filtereddata <- dat %>% filter(Timepoint==0 | Timepoint==finaltimepoint)
filtereddata <- dat %>% filter(Timepoint>=100 & Timepoint<=150)
filtereddata <- dat %>% filter(Depriv=="4" & Induct!=150)
filtereddata <- dat %>% filter(GeneID=="DRO1" | GeneID=="DRO2")
filtereddata <- dat %>% filter(is.na(Induct))
filtereddata <- dat %>% filter(!is.na(Induct))
filtereddata <- dat %>% filter(str_detect(COLUMNNAME, pattern="nitra"), ignore.case = TRUE)

###!check
# filtereddata <- dat %>% filter(grepl("nitrate", VARIABLE))
# filtereddata <- dat %>% filter(grepl("nitrate | phosphate", VARIABLE))

filtereddata <- dat %>% drop_na()
filtereddata <- dat %>% drop_na(COLUMN1)

vars <- c("COLUMN1","COLUMN2")
filtereddata <- dat %>% drop_na(any_of(vars))

filtereddata <- dat %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

## arrange column order	--------------------------------------------------------
dat <- dat %>% select(COLUMNNAME1, COLUMNNAME2, everything())

## arrange column order with COLUMN1 to the back -------------------------------
dat <- dat %>% select(-COLUMNNAME1, COLUMNNAME2, everything())

## arrange/sort rows by ascending order values ---------------------------------
dat <- dat %>% arrange(COLUMNNAME1)
dat <- dat %>% arrange(COLUMNNAME1, COLUMNNAME2, COLUMNNAME3)

## arrange/sort rows by descending order values --------------------------------
dat <- dat %>% arrange(desc(COLUMNNAME1))
dat <- dat %>% arrange(desc(COLUMNNAME1, COLUMNNAME2, COLUMNNAME3))

## descending,ascending,descending ---------------------------------------------
dat <- dat %>% arrange(desc(COLUMNNAME1), COLUMNNAME2, desc(COLUMNNAME3))

##########################################
## Merging/joining/combining dataframes ## -------------------------------------
##########################################

## join matching values, retain all matches, not rows, produces NAs when values missing 
dat <- left_join(dat1, dat2, by = "COMMON_COLUMN")

## join matching values, retain matches only -----------------------------------
dat <- inner_join(dat1, dat2, by = "COMMON_COLUMN")

## join matching values, retain all matches, retain all rows, produces NAs for missing values
dat <- full_join(dat1, dat2, by = "COMMON_COLUMN")
dat <- full_join(dat1, dat2, by = c("PlantID", "Treatment"))

## merge rows from two tables --------------------------------------------------
dat <- bind_rows(dat1, dat2...)

## merge columns from two tables -----------------------------------------------
dat <- bind_cols(dat1, dat2...)

###!check
# ## rows that appear in both x and y ------------------------------------------
# intersect(x, y, ...)
# 
# ## rows that appear in x but not y. ------------------------------------------
# setdiff(x, y, ...)
# 
## rows that appear in x or y (Duplicates removed). union_all() retains duplicates
# union(x, y, ...)

##########################
## Mutating new columns ## -----------------------------------------------------
##########################

## create a new variable based on existing variables ---------------------------
dat <- dat %>% mutate(NEWVARIABLE = (VARIABLE1-VARIABLE2))

## create a new variable based on conditions using case_when() -----------------
dat <- dat %>% mutate(NEWVARIABLE = case_when(
  COLUMNNAME1=="B73" ~ "Control"
  ,COLUMNNAME1 %in% c("M37W", "Mo18W", "Tx303") ~ "Mixed"
  ))

################################
## Percentages with mean data ## -----------------------------------------------
################################

## percentage change, quantify the change from one number to another and express the change as an increase or decrease
V1 <- dat[10, "VARIABLE1"] #starting value
V2 <- dat[15, "VARIABLE1"] #final value
PercentageChange <- ((V2-V1)/V1)*100
PercentageChange

## percentage difference, find the difference in percentage between two numbers
V1 <- dat[10, "VARIABLE1"] #value 1
V2 <- dat[15, "VARIABLE1"] #value 2
PercentageDifference <- ((V1-V2)/((V1+V2)/2))*100
PercentageDifference

##########################
## Exporting dataframes ## -----------------------------------------------------
##########################

# save dataframe to CSV file
write_csv(dat, file="output/FILENAME.csv")
write_csv(dat, paste("FILENAME.csv"))
write_csv(dat, paste0("output/",expname,"_FILENAME.csv"))