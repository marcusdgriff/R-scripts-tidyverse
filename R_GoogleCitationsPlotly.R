## Griffiths M (2022)
## Citation plot from Google Scholar profile using R and plotly

################
## User setup ## -------------------------------------------------------------------------------------------------------
################

## install or load following packages ----------------------------------------------------------------------------------
library("scholar")
library("plotly")
library("tidyverse")

##############################
## Define scholar dataframe ## -----------------------------------------------------------------------------------------
##############################

## add google scholar id part of profile URL --------------------------------------------------------------------------
id <- "X--dqYYAAAAJ&hl=en&oi=ao"

scholar <- get_profile(id)

scholar$name
scholar$affiliation
scholar$h_index
scholar$i10_index

## create citation_history dataframe -----------------------------------------------------------------------------------
citation_history <- get_citation_history(id)
citation_history

#####################################
## Create figure with scholar data ## ----------------------------------------------------------------------------------
#####################################

## preview ggplot bar chart --------------------------------------------------------------------------------------------
p <- ggplot(data=citation_history, aes(x=year, y=cites)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )  +
  xlab(bquote("Year")) + 
  ylab(bquote("Citations"))
p

###########################
## Upload plot to plotly ## --------------------------------------------------------------------------------------------
###########################

## create ggplot bar chart ready for plotly upload ---------------------------------------------------------------------
p <- ggplotly(p)
p

## Upload to plotly, replace USERNAME and APIKEY with your plotly details ----------------------------------------------
Sys.setenv("plotly_username"="USERNAME")
Sys.setenv("plotly_api_key"="APIKEY")

api_create(p, filename = "googlecitations")