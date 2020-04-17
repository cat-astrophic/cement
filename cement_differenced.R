# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

library(stargazer)
library(ggplot2)

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

# Running autoregressive models

ar1 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Lagged.R.D, data = cement)

ar2 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C), data = cement)

ar3 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading, data = cement)

ar4 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C), data = cement)

ar5 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
          + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ar6 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I
          + Kyoto.I.Rat.Phase.II + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ar7 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ar8 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ar9 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading, data = cement)

ar10 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + KP + KP*Emissions.Trading, data = cement)

ar11 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + KP, data = cement)

ar12 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + KP, data = cement)

ar13 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar14 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I
           + Kyoto.I.Rat.Phase.II + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar15 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar16 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar17 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar18 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + KP + KP*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar19 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar20 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + KP
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar21 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar22 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I
           + Kyoto.I.Rat.Phase.II + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar23 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar24 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar25 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar26 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + KP + KP*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar27 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar28 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + KP
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

# Viewing results

stargazer(ar1, ar2, ar3, ar4, type = 'text') # No KP effects
stargazer(ar5, ar6, ar7, ar8, type = 'text') # KP effects by phase
stargazer(ar9, ar10, ar11, ar12, type = 'text') # General KP effects
stargazer(ar13, ar14, ar15, ar16, type = 'text') # KP effects by phase with 5YPs
stargazer(ar17, ar18, ar19, ar20, type = 'text') # General KP effects with 5YPs
stargazer(ar21, ar22, ar23, ar24, type = 'text') # KP effects by phase with 5YPs with year FE
stargazer(ar25, ar26, ar27, ar28, type = 'text') # General KP effects with 5YPs with year FE

# Writing results to file

write.csv(stargazer(ar1, ar2, ar3, ar4, type = 'text'),
          paste(directory, 'cement_Differenced_regression_results_1_no_KP.txt'), row.names = FALSE)
write.csv(stargazer(ar1, ar2, ar3, ar4),
          paste(directory, 'cement_Differenced_regression_results_1_no_KP_tex.txt'), row.names = FALSE)
write.csv(stargazer(ar5, ar6, ar7, ar8, type = 'text'),
          paste(directory, 'cement_Differenced_regression_results_2_KP_effects_by_phase.txt'), row.names = FALSE)
write.csv(stargazer(ar5, ar6, ar7, ar8),
          paste(directory, 'cement_Differenced_regression_results_2_KP_effects_by_phase_tex.txt'), row.names = FALSE)
write.csv(stargazer(ar9, ar10, ar11, ar12, type = 'text'),
          paste(directory, 'cement_Differenced_regression_results_3_KP_general.txt'), row.names = FALSE)
write.csv(stargazer(ar9, ar10, ar11, ar12),
          paste(directory, 'cement_Differenced_regression_results_3_KP_general_tex.txt'), row.names = FALSE)
write.csv(stargazer(ar13, ar14, ar15, ar16, type = 'text'),
          paste(directory, 'cement_Differenced_regression_results_4_KP_effects_by_phase_5YP.txt'), row.names = FALSE)
write.csv(stargazer(ar13, ar14, ar15, ar16),
          paste(directory, 'cement_Differenced_regression_results_4_KP_effects_by_phase_5YP_tex.txt'), row.names = FALSE)
write.csv(stargazer(ar17, ar18, ar19, ar20, type = 'text'),
          paste(directory, 'cement_Differenced_regression_results_5_KP_effects_by_phase_5YP.txt'), row.names = FALSE)
write.csv(stargazer(ar17, ar18, ar19, ar20),
          paste(directory, 'cement_Differenced_regression_results_5_KP_effects_by_phase_5YP_tex.txt'), row.names = FALSE)
write.csv(stargazer(ar21, ar22, ar23, ar24, type = 'text'),
          paste(directory, 'cement_Differenced_regression_results_6_KP_general_5YP.txt'), row.names = FALSE)
write.csv(stargazer(ar21, ar22, ar23, ar24),
          paste(directory, 'cement_Differenced_regression_results_6_KP_general_5YP_tex.txt'), row.names = FALSE)
write.csv(stargazer(ar25, ar26, ar27, ar28, type = 'text'),
          paste(directory, 'cement_Differenced_regression_results_7_KP_general_5YP_FE.txt'), row.names = FALSE)
write.csv(stargazer(ar25, ar26, ar27, ar28),
          paste(directory, 'cement_Differenced_regression_results_7_KP_general_5YP_FE_tex.txt'), row.names = FALSE)

