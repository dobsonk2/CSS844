# TITLE:          Hyperspec disease plot
# AUTHORS:        Kara Dobson
# DATA INPUT:     Data imported as csv files from shared group 4 folder
# DATA OUTPUT:    Figure of hyperspec data between diseased and non diseased plants
# PROJECT:        CSS 844
# DATE:           March 2022

# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse)
library(plotrix)

# Get data
# File path set to OneDrive on my computer
spec <- read.csv("/Users/karachristinad/Library/CloudStorage/OneDrive-MichiganStateUniversity/CSS 844/Module 2/Group4/potato_merged_new.csv")

# setting the "." to NA
spec[spec == "."] <- NA
str(spec)

# changing the needed column to numeric
spec$eb._dpi_47 <- as.integer(spec$eb._dpi_47)

# classifying plants as "disease" or "no_disease"
# picking a cutoff of >=20 for disease, <20 for no disease
spec$disease = NA
spec$disease[spec$eb._dpi_47 >= 20] = "disease"
spec$disease[spec$eb._dpi_47 < 20] = "no_disease"
x <- spec[,1431, drop=FALSE] # checking the disease column

# removing all columns but the mean hyperspec and disease
spec2 <- spec[, -c(1:56)]
spec3 <- spec2[, -c(275:1374)]

# adding unique ID column for each plant
spec3$ID<-1:nrow(spec)
spec3$ID <- as.character(spec3$ID)

# transforming data from wide to long
spec_transform <- spec3 %>%
        gather(key="wavelength", value="value", -disease, -ID)
spec_transform<- spec_transform[complete.cases(spec_transform), ]

# fixing wavelength names
spec_transform <- spec_transform %>%
        mutate(wavelength2 = str_extract(wavelength, '\\d+'))
spec_transform$wavelength2 <- as.numeric(spec_transform$wavelength2)

# taking the average value of each wavelength for disease and no disease plots
spec_avg <-spec_transform %>%
        group_by(disease, wavelength2) %>%
        summarize(avg = mean(value, na.rm = TRUE),
                  se = std.error(value, na.rm = TRUE))

# plot
# all plots
ggplot(spec_transform, aes(x = wavelength2, y = value, group = ID, color=disease)) +
        geom_line(size = 1) +
        theme_bw(14)

# averaged by disease
png("hyperspec_disease.png", units="in", width=8, height=6, res=300)
ggplot(spec_avg, aes(x = wavelength2, y = avg, group = disease, color=disease)) +
        geom_line(size = 1) +
        labs(y="Value",x="Layer") +
        theme_bw(14)
dev.off()
