## Griffiths M (2024) Citation plot from Google Scholar profile using R and plotly

################
## User setup ## ---------------------------------------------------------------
################

library("scholar")
library("plotly")
library("tidyverse")
library("scales")

##############################
## Define scholar dataframe ## -------------------------------------------------
##############################

# Find and replace the google scholar id part of profile URL -------------------
id <- "X--dqYYAAAAJ&hl=en&oi=ao"

scholar <- get_profile(id)

scholar$name
scholar$affiliation
scholar$h_index
scholar$i10_index

## create citation_history dataframe -------------------------------------------
citation_history <- get_citation_history(id)
citation_history

#####################################
## Plot figure with scholar data ## --------------------------------------------
#####################################

# Plot barchart citations per year ---------------------------------------------
p <- ggplot(data=citation_history, aes(x=year, y=cites)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  )  +
  xlab(bquote("Year")) + 
  ylab(bquote("Citations")) +
  # scale_x_continuous(breaks = 2016:2024)
  # scale_x_continuous(labels = scales::number_format(accuracy = 1))
  scale_x_continuous(breaks = scales::breaks_width(2))

p <- ggplotly(p)

p

###########################
## Upload plot to plotly ## ----------------------------------------------------
###########################

# Replace USERNAME and APIKEY with your plotly details -------------------------
Sys.setenv("plotly_username"="mdgriffiths")
Sys.setenv("plotly_api_key"="7ILBBxStrQOBjjb19Vs0")

# Upload plot to plotly --------------------------------------------------------
api_create(p, filename = "googlecitations")
