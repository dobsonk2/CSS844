# TITLE:          Potato hyperspec + disease dataframe merging & creating indices
# AUTHORS:        Kara Dobson
# DATA INPUT:     Data imported as csv files from shared group 4 folder
# DATA OUTPUT:    Cleaned up dataframe w/ both hyperspec and disease ratings + indices
# PROJECT:        CSS 844
# DATE:           March 2022

# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse)

# Get data
# File path set to OneDrive on my computer
disease <- read.csv("/Users/karachristinad/Library/CloudStorage/OneDrive-MichiganStateUniversity/CSS 844/Module 2/Group4/2020 Potato Late Blight Trial RAUDPC 20220224.csv",skip=4)
hyperspec <- read.csv("/Users/karachristinad/Library/CloudStorage/OneDrive-MichiganStateUniversity/CSS 844/Module 2/Group4/plotwise_values_new.csv")

# Cleaning up column names in the hyperspec data to match the disease data for merging
# remove certain columns we don't need
remove_col <- function(df,name){
        vec <- which(names(df) %in% name)
        df = df[,-vec]
        return(df)
}
hyperspec2 <- remove_col(hyperspec, name=c("fid", "id"))

# changing all columns to lowercase
names(hyperspec2) <- tolower(names(hyperspec2))
names(disease) <- tolower(names(disease))

# changing column names in hyperspec to match names in disease
names(hyperspec2)[names(hyperspec2)=="range"] <- "tier"
names(hyperspec2)[names(hyperspec2)=="plotnumber"] <- "plot"

# removing spreader for now - going to go back and average at the row level to match disease
hyperspec3 <- hyperspec2[!(hyperspec2$line=="AtlanticSpreader"),]
disease2 <- disease[!(disease$line=="Atlantic Spreader"),]

# making the columns the same data type
disease2$tier <- as.integer(disease2$tier)
disease2$block <- as.integer(disease2$block)
disease2$plot <- as.integer(disease2$plot)

# merging the two dataframes into one
merged <- left_join(disease2, hyperspec3, by = c("row", "tier", "block", "line", "rep", "plot"))

# calculating indices
merged2 <- merged %>%
        mutate(disease_index = (x63_mean-x137_mean/x63_mean+x137_mean)-(0.5*x139_mean)) %>%
        mutate(red_edge = x160_mean/x142_mean) %>%
        mutate(chloro_a = x183_mean/x126_mean) %>%
        mutate(chloro_b = x183_mean/x115_mean)

x <- merged2[,1427, drop=FALSE] # selecting the disease index column to check it
x2 <- merged2[,1428, drop=FALSE] # selecting the red edge column to check it
x3 <- merged2[,1429, drop=FALSE] # selecting the chlorophyll a column to check it
x4 <- merged2[,1430, drop=FALSE] # selecting the chlorophyll b column to check it

# save final csv
write.csv(merged2, "potato_merged_new.csv", row.names=F)

          