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

ear1 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D, data = cement)

ear2 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C), data = cement)

ear3 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading, data = cement)

ear4 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C), data = cement)

ear5 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ear6 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I
           + Kyoto.I.Rat.Phase.II + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ear7 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ear8 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ear9 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading, data = cement)

ear10 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + KP + KP*Emissions.Trading, data = cement)

ear11 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + KP, data = cement)

ear12 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + KP, data = cement)

ear13 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear14 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I
            + Kyoto.I.Rat.Phase.II + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear15 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear16 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear17 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear18 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + KP + KP*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear19 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear20 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + KP
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear21 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear22 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I
            + Kyoto.I.Rat.Phase.II + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear23 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear24 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear25 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear26 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + KP + KP*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear27 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear28 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + KP
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

# Viewing results

stargazer(ear1, ear2, ear3, ear4, type = 'text') # No KP effects
stargazer(ear5, ear6, ear7, ear8, type = 'text') # KP effects by phase
stargazer(ear9, ear10, ear11, ear12, type = 'text') # General KP effects
stargazer(ear13, ear14, ear15, ear16, type = 'text') # KP effects by phase with 5YPs
stargazer(ear17, ear18, ear19, ear20, type = 'text') # General KP effects with 5YPs
stargazer(ear21, ear22, ear23, ear24, type = 'text') # KP effects by phase with 5YPs with year FE
stargazer(ear25, ear26, ear27, ear28, type = 'text') # General KP effects with 5YPs with year FE

# Writing results to file

write.csv(stargazer(ear1, ear2, ear3, ear4, type = 'text'),
          paste(directory, 'emissions_differenced_regression_results_1_no_KP.txt'), row.names = FALSE)
write.csv(stargazer(ear1, ear2, ear3, ear4),
          paste(directory, 'emissions_differenced_regression_results_1_no_KP_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear5, ear6, ear7, ear8, type = 'text'),
          paste(directory, 'emissions_differenced_regression_results_2_KP_effects_by_phase.txt'), row.names = FALSE)
write.csv(stargazer(ear5, ear6, ear7, ear8),
          paste(directory, 'emissions_differenced_regression_results_2_KP_effects_by_phase_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear9, ear10, ear11, ear12, type = 'text'),
          paste(directory, 'emissions_differenced_regression_results_3_KP_general.txt'), row.names = FALSE)
write.csv(stargazer(ear9, ear10, ear11, ear12),
          paste(directory, 'emissions_differenced_regression_results_3_KP_general_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear13, ear14, ear15, ear16, type = 'text'),
          paste(directory, 'emissions_differenced_regression_results_4_KP_effects_by_phase_5YP.txt'), row.names = FALSE)
write.csv(stargazer(ear13, ear14, ear15, ear16),
          paste(directory, 'emissions_differenced_regression_results_4_KP_effects_by_phase_5YP_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear17, ear18, ear19, ear20, type = 'text'),
          paste(directory, 'emissions_differenced_regression_results_5_KP_effects_by_phase_5YP.txt'), row.names = FALSE)
write.csv(stargazer(ear17, ear18, ear19, ear20),
          paste(directory, 'emissions_differenced_regression_results_5_KP_effects_by_phase_5YP_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear21, ear22, ear23, ear24, type = 'text'),
          paste(directory, 'emissions_differenced_regression_results_6_KP_general_5YP.txt'), row.names = FALSE)
write.csv(stargazer(ear21, ear22, ear23, ear24),
          paste(directory, 'emissions_differenced_regression_results_6_KP_general_5YP_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear25, ear26, ear27, ear28, type = 'text'),
          paste(directory, 'emissions_differenced_regression_results_7_KP_general_5YP_FE.txt'), row.names = FALSE)
write.csv(stargazer(ear25, ear26, ear27, ear28),
          paste(directory, 'emissions_differenced_regression_results_7_KP_general_5YP_FE_tex.txt'), row.names = FALSE)

