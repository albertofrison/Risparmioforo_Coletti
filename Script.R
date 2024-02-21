################################################################################
# This is the R implementation of the Risparmioforo Python project from Prof. Paolo Coletti
# YT Channel: https://www.youtube.com/@PaoloColetti
# https://colab.research.google.com/drive/1hQXxDTKuccacVeHPs79fDPVw2HjOqHgB?usp=sharing#scrollTo=pe5BeLMCLaIV
# Made with ♥︎ by Alberto Frison
# Created on February 2023
################################################################################

################################################################################
#
# LOAD LIBRARIES AND CLEANING ENVIRONMENT
#
################################################################################
#-------------------------------------------------------------------------------
# LOAD LIBRARIES
library(tidyverse)

#-------------------------------------------------------------------------------
# CLEANING ENVIRONMENT
rm (list = ls())


################################################################################
#
# LOAD DATA
#
################################################################################


msci.usa <- read.csv ("https://raw.githubusercontent.com/paolocole/Stock-Indexes-Historical-Data/main/NET/EUR/LARGE_AND_MID_CAP/COUNTRIES/MSCI%20USA.csv", sep =",")
msci.europe <- read.csv("https://raw.githubusercontent.com/paolocole/Stock-Indexes-Historical-Data/main/NET/EUR/LARGE_AND_MID_CAP/REGIONS/MSCI%20EUROPE.csv",sep =",")
msci.emerging.asia <-  read.csv ("https://raw.githubusercontent.com/paolocole/Stock-Indexes-Historical-Data/main/NET/EUR/LARGE_AND_MID_CAP/REGIONS/MSCI%20EM%20ASIA.csv", sep = ",")
msci.world <- read.csv ("https://raw.githubusercontent.com/paolocole/Stock-Indexes-Historical-Data/main/NET/EUR/LARGE_AND_MID_CAP/REGIONS/MSCI%20WORLD.csv", sep = ",")
gold.spot <- read.csv ("https://raw.githubusercontent.com/AxelFooley/Market-Indexes-Historical-Data/main/Gold-Spot.csv", sep = ",")


# MSCI USA
msci.usa <- msci.usa %>%
    mutate (pct_change = (USA - lag (USA, 12))/lag (USA, 12))
summary(msci.usa)

# MSCI EUROPE
msci.europe <- msci.europe %>%
  mutate (pct_change = (EUROPE - lag (EUROPE, 12))/lag (EUROPE, 12))

summary(msci.europe)

# MSCI EMERGING ASIA
msci.emerging.asia <- msci.emerging.asia %>%
  mutate (pct_change = (EM.ASIA - lag (EM.ASIA, 12))/lag (EM.ASIA, 12))

summary(msci.emerging.asia)

# MSCI WORLD
msci.world <- msci.world %>%
  mutate (pct_change = (WORLD - lag (WORLD, 12))/lag (WORLD, 12))

summary(msci.world)

# GOLD SPOT
gold.spot <- gold.spot %>%
  mutate (pct_change = (Gold.spot.price - lag (Gold.spot.price, 12))/lag (Gold.spot.price, 12))

summary(gold.spot)
