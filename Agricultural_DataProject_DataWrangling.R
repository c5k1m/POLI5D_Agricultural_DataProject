# Name: Christian Kim
# PID: A16132108
# ECON 5 / POLI 5D Final Project Part 1 (Cleaning Raw Data)

rm(list = ls())

library(dplyr)

GDP <- read.csv("/Users/christiankim/Desktop/GDP_by_Country.csv")
Population <- read.csv("/Users/christiankim/Desktop/Population_by_Country.csv")
Land <- read.csv("/Users/christiankim/Desktop/Land_by_Country.csv")

# Below, I merged the GDP and Land datasets together, using the GDP dataset as the master
# dataset. I also merged the Population and Land datasets together, using the Population dataset
# as the master dataset.
GDP_Land <- GDP %>%
  left_join(Land, by = "Country.Name")

Population_Land <- Population %>%
  left_join(Land, by = "Country.Name")

# Below, I renamed all of the variables in the GDP_Land dataset for an easier read.
GDP_Land <- GDP_Land %>% rename(GDP_2006 = X2006.x)
GDP_Land <- GDP_Land %>% rename(GDP_2016 = X2016.x)
GDP_Land <- GDP_Land %>% rename(Land_2006 = X2006.y)
GDP_Land <- GDP_Land %>% rename(Land_2016 = X2016.y)

# Below, I renamed all the variables in the Population_Land dataset for an easier read.
Population_Land <- Population_Land %>% rename(Population_2006 = X2006.x)
Population_Land <- Population_Land %>% rename(Population_2016 = X2016.x)
Population_Land <- Population_Land %>% rename(Land_2006 = X2006.y)
Population_Land <- Population_Land %>% rename(Land_2016 = X2016.y)

# Below, I created two new variables, "Logarithmic_GDP_2006" and "Logarithmic_GDP_2016," which change GDP_2006 and GDP_2016 into
# logarithmic form since some figures were in the millions and some in the trillions; leaving them in regular absolute form
# would have caused HUGE disparities in the graphs.
GDP_Land$Logarithmic_GDP_2006 <- log(GDP_Land$GDP_2006)
GDP_Land$Logarithmic_GDP_2016 <- log(GDP_Land$GDP_2016)

summary(GDP_Land$GDP_2006)
summary(GDP_Land$GDP_2016)
summary(GDP_Land$Log_GDP_2006)
summary(GDP_Land$Log_GDP_2016)


# Below, I used the same approach as above. I created "Logarithmic_Population_2006" and "Logarithmic_Population_2016" as logarithmic
# values for Population_2006 and Population_2016.
Population_Land$Logarithmic_Population_2006 <- log(Population_Land$Population_2006)
Population_Land$Logarithmic_Population_2016 <- log(Population_Land$Population_2016)

summary(Population_Land$Population_2006)
summary(Population_Land$Population_2016)
summary(Population_Land$Logarithmic_Population_2006)
summary(Population_Land$Logarithmic_Population_2016)
