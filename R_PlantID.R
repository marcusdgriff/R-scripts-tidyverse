## Griffiths M (2020) PlantID system for R statistics

library(tidyverse) #ggplot2, purrr, tibble, dplyr, tidyr, stringr, readr, forcats 

Plantdata <- read_csv(file=paste("FILENAME.csv"), na = c("NA", "na", "n.a.", ""))

##If you have multiple dataframes for the same plants combine them together by the PlantID
Plantdata <- Rootweightdata %>% 
  full_join(Rootrespirationdata, by = "PlantID") %>% 
  full_join(Rootlengthdata, by = "PlantID") %>% 
  full_join(Shootweightdata, by = "PlantID") %>% 
  full_join(Shootimagedata, by = "PlantID") %>% 
  full_join(ShootNcontentdata, by = "PlantID") 

##Seperate out the tag command. From here you can group and separate by genotype & treatment, make figures etc. 
Plantdata <- Plantdata %>% 
  separate(Plantdata, PlantID,
           into = c("Date", "Experiment", "Geno", "Plant", "Treatment", "Rep"),
           sep = '_') %>%
  separate(Plantdata, `Plant`, 
           into = c("Plant1","Plant"), 
           sep = 'p') %>%
  separate(Plantdata, `Treatment`,
           into = c("Treatment1","Treatment"),
           sep = 't')  %>% 
  separate(Plantdata, `Rep`,
           into = c("Rep1","Rep"),
           sep = 'r')

name <- c("Plant1", "Treatment1", "Rep1") 
Plantdata <- Plantdata %>% select(-one_of(name)) 
rm(name) 