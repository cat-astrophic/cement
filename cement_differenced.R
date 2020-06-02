# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

library(sandwich)
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

ar1 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
          + Forest.Rents + Tariff.Rate + Lagged.R.D, data = cement)

ar3 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading, data = cement)

ar5 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
          + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ar7 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ar9 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading, data = cement)

ar11 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + KP, data = cement)

ar13 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar15 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar17 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar19 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar21 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar23 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar25 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar27 <- lm(Differenced ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

# Calculating robust standard errors

cov1 <- vcovHC(ar1, type = 'HC1')
rse1 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar3, type = 'HC1')
rse3 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar5, type = 'HC1')
rse5 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar7, type = 'HC1')
rse7 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar9, type = 'HC1')
rse9 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar11, type = 'HC1')
rse11 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar13, type = 'HC1')
rse13 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar15, type = 'HC1')
rse15 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar17, type = 'HC1')
rse17 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar19, type = 'HC1')
rse19 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar21, type = 'HC1')
rse21 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar23, type = 'HC1')
rse23 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar25, type = 'HC1')
rse25 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar27, type = 'HC1')
rse27 <- sqrt(diag(cov1))

# Viewing results

stargazer(ar1, ar3, type = 'text', se = list(rse1, rse3)) # No KP effects
stargazer(ar5, ar7, type = 'text', se = list(rse5, rse7)) # KP effects by phase
stargazer(ar9, ar11, type = 'text', se = list(rse9, rse11)) # General KP effects
stargazer(ar13, ar15, type = 'text', se = list(rse13, rse15)) # KP effects by phase with 5YPs
stargazer(ar17, ar19, type = 'text', se = list(rse17, rse19)) # General KP effects with 5YPs
stargazer(ar21, ar23, type = 'text', se = list(rse21, rse23)) # KP effects by phase with 5YPs with year FE
stargazer(ar25, ar27, type = 'text', se = list(rse25, rse27)) # General KP effects with 5YPs with year FE

# Writing results to file

write.csv(stargazer(ar1, ar3, ar5, ar7, ar13, ar15, ar21, ar23, type = 'text',
                    se = list(rse1, rse3, rse5, rse7, rse13, rse15, rse21, rse23)),
          paste(directory, 'cement_Differenced_regression_results_phases.txt'), row.names = FALSE)
write.csv(stargazer(ar1, ar3, ar5, ar7, ar13, ar15, ar21, ar23,
                    se = list(rse1, rse3, rse5, rse7, rse13, rse15, rse21, rse23)),
          paste(directory, 'cement_Differenced_regression_results_phases_tex.txt'), row.names = FALSE)
write.csv(stargazer(ar1, ar3, ar9, ar11, ar17, ar19, ar25, ar27, type = 'text',
                    se = list(rse1, rse3, rse9, rse11, rse17, rse19, rse25, rse27)),
          paste(directory, 'cement_Differenced_regression_results_all.txt'), row.names = FALSE)
write.csv(stargazer(ar1, ar3, ar9, ar11, ar17, ar19, ar25, ar27,
                    se = list(rse1, rse3, rse9, rse11, rse17, rse19, rse25, rse27)),
          paste(directory, 'cement_Differenced_regression_results_all_tex.txt'), row.names = FALSE)

