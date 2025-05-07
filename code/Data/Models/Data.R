# Loading packages
# setwd("C:/Users/MMXF/OneDrive - Novo Nordisk/8. Semester/P8/code")
set.seed(123)
library(tidyverse)
library(mrgsolve)
library(nlmixr2)
library(xpose.nlmixr2)
library(ggplot2)

# Loading data
readData <- readRDS("Data/SD1_mrgsims.rds")
dataframe <- as.data.frame(readData)

# Making a set with only the true values
truedata <- dataframe %>%
                      select(-DV) %>%
                      rename(DV = CP)

# Subset of the dataset
subsetTrue <- truedata[truedata$time %in% c(0, 6, 12, 24, 48, 72, 96, 120, 144, 168, 336, 504, 672, 840), ] %>%
              mutate(EVID = ifelse(time == 0 & ADMSC != 0, 101, 0)) %>%
              mutate(AMT = ifelse(time == 0 & ADMSC != 0, ADMSC, 0)) %>%
              mutate(CMT = ifelse(time == 0 & ADMSC != 0, 1, 2)) %>%
              filter(!(time == 0 & CMT == 2))

# Making a set with only the values with noise
noiseData <- dataframe %>%
                      select(-CP)

# Subset of the dataset
subsetNoise <- noiseData[noiseData$time %in% c(0, 6, 12, 24, 48, 72, 96, 120, 144, 168, 336, 504, 672, 840), ] %>%
            filter(ADMSC > 0) %>%
              mutate(EVID = ifelse(time == 0 & ADMSC != 0, 101, 0)) %>%
              mutate(AMT = ifelse(time == 0 & ADMSC != 0, ADMSC, 0)) %>%
              mutate(CMT = ifelse(time == 0 & ADMSC != 0, 1, 2)) %>%
              filter(!(time == 0 & CMT == 2))

# Removing the variables that are no longer of use
rm(truedata, readData, noiseData, dataframe)