# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on CO2 emissions from cement manufacturing

# Loading libraries

library(stargazer)
library(ggplot2)


# Reading in the panel data

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')

# Removing outliers w.r.t. real interest rate for consistency

cement <- cement[which(abs(cement$Real.Interest.Rate) < 50),]

# Adding a Differenced variable

cement <- cement[which(cement$Cement.Emissions > 0 & cement$Lagged.Cement.Emissions > 0),]
cement$Differenced <- 100 * (cement$Cement.Emissions - cement$Lagged.Cement.Emissions) / cement$Lagged.Cement.Emissions

# Removing outliers since this is percentage based

lower.cutoff <- quantile(cement$Differenced, c(.005), na.rm = TRUE)
upper.cutoff <- quantile(cement$Differenced, c(.995), na.rm = TRUE)
cement <- cement[which(cement$Differenced <= upper.cutoff & cement$Differenced >= lower.cutoff),]

# Running autoregressive models

ear1 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D, data = cement)

ear3 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading, data = cement)

ear5 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ear7 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ear9 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading, data = cement)

ear11 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + KP, data = cement)

ear13 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear15 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear17 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear19 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear21 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear23 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear25 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear27 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

# Viewing results

stargazer(ear1, ear3, type = 'text') # No KP effects
stargazer(ear5, ear7, type = 'text') # KP effects by phase
stargazer(ear9, ear11, type = 'text') # General KP effects
stargazer(ear13, ear15, type = 'text') # KP effects by phase with 5YPs
stargazer(ear17, ear19, type = 'text') # General KP effects with 5YPs
stargazer(ear21, ear23, type = 'text') # KP effects by phase with 5YPs with year FE
stargazer(ear25, ear27, type = 'text') # General KP effects with 5YPs with year FE

# Writing results to file

write.csv(stargazer(ear1, ear3, ear5, ear7, ear13, ear15, ear21, ear23, type = 'text'),
          paste(directory, 'emissions_differenced_regression_results_phases.txt'), row.names = FALSE)
write.csv(stargazer(ear1, ear3, ear5, ear7, ear13, ear15, ear21, ear23),
          paste(directory, 'emissions_differenced_regression_results_phases_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear1, ear3, ear9, ear11, ear17, ear19, ear25, ear27, type = 'text'),
          paste(directory, 'emissions_differenced_regression_results_all.txt'), row.names = FALSE)
write.csv(stargazer(ear1, ear3, ear9, ear11, ear17, ear19, ear25, ear27),
          paste(directory, 'emissions_differenced_regression_results_all_tex.txt'), row.names = FALSE)

