# Name: Christian Kim
# PID: A16132108
# ECON 5 / POLI 5D Final Project Part 2 (Statistics, Plots, and Linear Regressions)

# Everything on this script includes all code needed to generate results, analyze data, and
# ultimately answer the question.

rm(list=ls())

# Below, I am using the "stargazer" package and function to generate tables for
# all the summary statistics regarding each variable in each dataset. The first "stargazer" command creates a
# latex table for summary statistics for GDP and Land in 2006 and 2016. The second "stargazer"
# creates a latex table for summary statistics for population in 2006 and 2016.
install.packages("stargazer")
library(stargazer)

stargazer(GDP_Land,
          type = "latex", title = "GDP_Land Statistics", style = "default",
          summary = NULL,
          median = TRUE)

stargazer(Population_Land,
          type = "latex", title = "Population Statistics", style = "default",
          summary = NULL, 
          median = TRUE)


# Below, I am using the "fivenum" command, which shows five different statistical figures 
# (minimum, 25th percentile, median, 75th percentile, maximum)
fivenum(GDP_Land$GDP_2006)
fivenum(GDP_Land$GDP_2016)

fivenum(GDP_Land$Logarithmic_GDP_2006)
fivenum(GDP_Land$Logarithmic_GDP_2016)

# Below, I computed the mean and standard deviation of GDP and Land in 2006 and 2016 since the "fivenum" command did not comput
# mean and standard deviation.
mean(GDP_Land$GDP_2006, na.rm = TRUE)
mean(GDP_Land$GDP_2016, na.rm = TRUE)
mean(GDP_Land$Land_2006, na.rm = TRUE)
mean(GDP_Land$Land_2016, na.rm = TRUE)

sd(GDP_Land$GDP_2006, na.rm = TRUE)
sd(GDP_Land$GDP_2016, na.rm = TRUE)
sd(GDP_Land$Land_2006, na.rm = TRUE)
sd(GDP_Land$Land_2016, na.rm = TRUE)


# Below, I wanted to get an idea of which countries had higher GDP levels in one year compared to another year. Observing decreases
# and increases in GDP between 2006 and 2016 would give me an idea of what I could possibly expect with my regressional analysis.
print(GDP_Land$Country.Name[GDP_Land$GDP_2006 > GDP_Land$GDP_2016])
print(GDP_Land$Country.Name[GDP_Land$GDP_2006 < GDP_Land$GDP_2016])


# Below, I created two histograms using the ggplot2 package. I wanted to see how many countries had a certain percentage of
# agricultural land prior to creating scatterplots and performing linear regressions. THe first histogram shows the frequency
# of countries at a certain percentage in 2006, while the second histogram shows the frequency for 2016.
library(ggplot2)
Hist1 <- ggplot(GDP_Land, aes(x=Land_2006)) + geom_histogram(color= "black", fill = "white")
Hist1 + ggtitle("Figure 1: Frequency of Countries having a Certain Amount of Land in 2006") + xlab("Amount of Arable Agricultural Land (%)") +
  ylab("Number of Countries") + stat_bin(geom="text", aes(label=..count..), vjust=-1) +
  geom_vline(aes(xintercept = mean(GDP_Land$Land_2006, na.rm = TRUE), color = 'Mean'), show.legend = TRUE, size = 2) + 
  geom_vline(aes(xintercept = median(GDP_Land$Land_2006, na.rm = TRUE), color = 'Median'), show.legend = TRUE, size = 1)

Hist2 <- ggplot(GDP_Land, aes(x=Land_2016)) + geom_histogram(color= "black", fill = "white")
Hist2 + ggtitle("Figure 2: Frequency of Countries having a Certain Amount of Land in 2016") + xlab("Amount of Arable Agricultural Land (%)") +
  ylab("Number of Countries") + stat_bin(geom="text", aes(label=..count..), vjust=-1) +
  geom_vline(aes(xintercept = mean(GDP_Land$Land_2016, na.rm = TRUE), color = 'Mean'), show.legend = TRUE, size = 1) + 
  geom_vline(aes(xintercept = median(GDP_Land$Land_2016, na.rm = TRUE), color = 'Median'), show.legend = TRUE, size = 1)


# Below is the ggplot2 plot for 2006 and, separately, 2016 to illustrate a country's GDP 
# level in relation to the amount of arable agricultural land available. Plot1 is for 2006, 
# and Plot2 is for 2016.
Plot1 <- ggplot(data = GDP_Land,
                aes(x = Logarithmic_GDP_2006, y = Land_2006, color = Land_2006))
Plot1 + geom_point() + scale_color_gradient(low = "red", high= "green") + 
  geom_smooth(method = lm, fullrange=TRUE, se = FALSE) +
  ggtitle("Figure 3: Amount of Arable Agricultural Land by GDP in 2006") + xlab("GDP in 2006 (Logarithmic Value)") +
  ylab("Amount of Arable Land Available (%)")

Plot2 <- ggplot(data = GDP_Land,
                aes(x = Logarithmic_GDP_2016, y = Land_2016, color = Land_2016))
Plot2 + geom_point() + scale_color_gradient(low = "red", high= "green") + 
  geom_smooth(method = lm, fullrange=TRUE, se = FALSE) +
  ggtitle("Figure 4: Amount of Arable Agricultural Land by GDP in 2016") + xlab("GDP in 2016 (Logarithmic Value)") +
  ylab("Amount of Arable Land Available (%)")


# Below, is the ggplot2 plot for 2006 and, separately, 2016 to illustrate a country's total population, the confounding variable, 
#  in relation to the amount of arable agricultural land available. Plot3 is for 2006, and Plot4 is for 2016.
Plot3 <- ggplot(data = Population_Land,
                aes(x = Logarithmic_Population_2006, y = Land_2006, color = Land_2006))
Plot3 + geom_point() + scale_color_gradient(low = "red", high= "green") + geom_smooth(method = lm, fullrange=TRUE, se = FALSE) +
  ggtitle("Figure 7: Amount of Land in Countries by Population in 2006") + xlab("Population in 2006 (Logarithmic Value)") +
  ylab("Amount of Arable Land Available (%)")

Plot4 <- ggplot(data = Population_Land,
                aes(x = Logarithmic_Population_2016, y = Land_2016, color = Land_2016))
Plot4 + geom_point() + scale_color_gradient(low = "red", high= "green") + geom_smooth(method = lm, fullrange=TRUE, se = FALSE) +
  ggtitle("Figure 8: Amount of Land in Countries by Population in 2016") + xlab("Population in 2016 (Logarithmic Value)") +
  ylab("Amount of Arable Land Available (%)")


# Below is the linear regressions for assessing GDP and Population in relation to land. The first two linear regressions are dedicated
# to the GDP in Logarithmic form (which was already done in the first R script), with "fit_2006" being the linear regression between
# land and GDP for 2006 and "fit_2016" for 2016. The last two linear regressions are for the confounding variable, total population,
# in logarithmic form, where "fit_Population_2006" is the linear regression for 2006 and "fit_Population_2016" for 2016.
fit_2006 <- lm(Land_2006 ~ Logarithmic_GDP_2006, data = GDP_Land)
summary(fit_2006)

fit_2016 <- lm(Land_2016 ~ Logarithmic_GDP_2016, data = GDP_Land)
summary(fit_2016)

fit_Population_2006 <- lm(Land_2006 ~ Logarithmic_Population_2006, data = Population_Land)
summary(fit_Population_2006)

fit_Population_2016 <- lm(Land_2016 ~ Logarithmic_Population_2016, data = Population_Land)
summary(fit_Population_2016)


# Below is the tables for the linear regressions above by using the "stargazer" command once again. The first two "stargazer"
# functions observe the linear regressions between GDP and land, with the first for 2006 and second for 2016. The last two
# "stargazer" commands observe the linear regressions between population and land, with the first for 2006 and second for 2016.
stargazer(fit_2006, type = "latex", title = "Regressional Analysis for 2006", style = "default",
          align = TRUE)

stargazer(fit_2016, type = "latex", title = "Regressional Analysis for 2016", style = "default",
          align = TRUE)

stargazer(fit_Population_2006, type = "latex", title = "Regressional Analysis for 2006", style = "default",
          align = TRUE)

stargazer(fit_Population_2016, type = "latex", title = "Regressional Analysis for 2016", style = "default",
          align = TRUE)


