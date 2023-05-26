# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

library(sandwich)
library(stargazer)
library(ggplot2)
library(lmtest)

# Reading in the panel data

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')

# Subsetting the data based on real interest rate

cement <- cement[which(abs(cement$Real.Interest.Rate) < 50),]

# Adding a Differenced variable

cement <- cement[which(cement$Cement > 0 & cement$Lagged.Cement > 0),]
cement$Differenced <- 100 * (cement$Cement - cement$Lagged.Cement) / cement$Lagged.Cement

# Removing outliers since this is percentage based

lower.cutoff <- quantile(cement$Differenced, c(.005), na.rm = TRUE)
upper.cutoff <- quantile(cement$Differenced, c(.995), na.rm = TRUE)
cement <- cement[which(cement$Differenced <= upper.cutoff & cement$Differenced >= lower.cutoff),]

# Difference-in-differences models

cement$Post2005 <- as.numeric(cement$Year > 2005)
cement$Post2006 <- as.numeric(cement$Year > 2006)
cement$Post2007 <- as.numeric(cement$Year > 2007)
cement$Post2008 <- as.numeric(cement$Year > 2008)

rar1 <- lm(Differenced ~ log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.Rat*Post2005 + factor(Year) + factor(Country), data = cement)

rar2 <- lm(Differenced ~ log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.Rat*Post2006 + factor(Year) + factor(Country), data = cement)

rar3 <- lm(Differenced ~ log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.Rat*Post2007 + factor(Year) + factor(Country), data = cement)

rar4 <- lm(Differenced ~ log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.Rat*Post2008 + factor(Year) + factor(Country), data = cement)

rar1x <- coeftest(rar1, vcov = vcovCL, cluster = ~Kyoto.Rat)
rar2x <- coeftest(rar2, vcov = vcovCL, cluster = ~Kyoto.Rat)
rar3x <- coeftest(rar3, vcov = vcovCL, cluster = ~Kyoto.Rat)
rar4x <- coeftest(rar4, vcov = vcovCL, cluster = ~Kyoto.Rat)

stargazer(rar1x, rar2x, rar3x, rar4x, type = 'text')

write.csv(stargazer(rar1x, rar2x, rar3x, rar4x, type = 'text'), paste(directory, 'cement_differenced_regression_results_DID.txt'), row.names = FALSE)

write.csv(stargazer(rar1x, rar2x, rar3x, rar4x), paste(directory, 'cement_differenced_regression_results_DID_tex.txt'), row.names = FALSE)

