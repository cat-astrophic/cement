# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

library(sandwich)
library(stargazer)
library(ggplot2)

# Reading in the panel data

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')

# Subsetting the data in the same fashion as the previous two scripts

cement <- cement[which(abs(cement$Real.Interest.Rate) < 50),]

# Plotting data

hist(cement$Intensity)
plot(cement$Year,cement$Intensity)

# We see that there are some extreme outliers in the above plot so we remove them -- anything above 99.5 percentile or below 0.5 percentile

upper <- quantile(cement$Intensity, c(.995), na.rm = TRUE)
lower <- quantile(cement$Intensity, c(.005), na.rm = TRUE)
cement <- cement[which(cement$Intensity <= upper & cement$Intensity >= lower),]

# Plotting data

hist(cement$Intensity)
plot(cement$Year,cement$Intensity)

# Running Carbon intensity regressions for cement manufacturing

i1 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
         + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
         + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading, data = cement)

i2 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
         + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
         + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading, data = cement)

i3 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita)*Kyoto.I.Rat.Phase.I + log(GDP.per.capita)*Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
         + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
         + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading, data = cement)

i4 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
         + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
         + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year), data = cement)

i5 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
         + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
         + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year), data = cement)

i6 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita)*Kyoto.I.Rat.Phase.I + log(GDP.per.capita)*Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
         + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
         + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year), data = cement)

ii1 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
          + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
          + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading, data = cement)

ii2 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
          + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
          + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading, data = cement)

ii3 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
          + Urban.Population + Lagged.R.D*Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
          + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading, data = cement)

ii4 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
          + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
          + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year), data = cement)

ii5 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
          + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
          + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year), data = cement)

ii6 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
          + Urban.Population + Lagged.R.D*Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
          + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year), data = cement)

iii1 <- lm(Intensity ~ log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
           + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
           + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading, data = cement)

iii2 <- lm(Intensity ~ log(GDP.per.capita) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
           + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
           + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading, data = cement)

iii3 <- lm(Intensity ~ log(GDP.per.capita)*Kyoto.I.Rat.Phase.I + log(GDP.per.capita)*Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
           + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
           + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading, data = cement)

iii4 <- lm(Intensity ~ log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
           + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
           + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year), data = cement)

iii5 <- lm(Intensity ~ log(GDP.per.capita) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
           + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
           + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year), data = cement)

iii6 <- lm(Intensity ~ log(GDP.per.capita)*Kyoto.I.Rat.Phase.I + log(GDP.per.capita)*Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
           + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
           + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year), data = cement)

# Calculating robust standard errors

cov1 <- vcovHC(i1, type = 'HC1')
rsei1 <- sqrt(diag(cov1))

cov1 <- vcovHC(i2, type = 'HC1')
rsei2 <- sqrt(diag(cov1))

cov1 <- vcovHC(i3, type = 'HC1')
rsei3 <- sqrt(diag(cov1))

cov1 <- vcovHC(i4, type = 'HC1')
rsei4 <- sqrt(diag(cov1))

cov1 <- vcovHC(i5, type = 'HC1')
rsei5 <- sqrt(diag(cov1))

cov1 <- vcovHC(i6, type = 'HC1')
rsei6 <- sqrt(diag(cov1))

cov1 <- vcovHC(ii1, type = 'HC1')
rseii1 <- sqrt(diag(cov1))

cov1 <- vcovHC(ii2, type = 'HC1')
rseii2 <- sqrt(diag(cov1))

cov1 <- vcovHC(ii3, type = 'HC1')
rseii3 <- sqrt(diag(cov1))

cov1 <- vcovHC(ii4, type = 'HC1')
rseii4 <- sqrt(diag(cov1))

cov1 <- vcovHC(ii5, type = 'HC1')
rseii5 <- sqrt(diag(cov1))

cov1 <- vcovHC(ii6, type = 'HC1')
rseii6 <- sqrt(diag(cov1))

cov1 <- vcovHC(iii1, type = 'HC1')
rseiii1 <- sqrt(diag(cov1))

cov1 <- vcovHC(iii2, type = 'HC1')
rseiii2 <- sqrt(diag(cov1))

cov1 <- vcovHC(iii3, type = 'HC1')
rseiii3 <- sqrt(diag(cov1))

cov1 <- vcovHC(iii4, type = 'HC1')
rseiii4 <- sqrt(diag(cov1))

cov1 <- vcovHC(iii5, type = 'HC1')
rseiii5 <- sqrt(diag(cov1))

cov1 <- vcovHC(iii6, type = 'HC1')
rseiii6 <- sqrt(diag(cov1))

# Viewing results

stargazer(i1, i2, i3, i4, i5, i6, type = 'text', se = list(rsei1, rsei2, rsei3, rsei4, rsei5, rsei6))
stargazer(ii1, ii2, ii3, ii4, ii5, ii6, type = 'text', se = list(rseii1, rseii2, rseii3, rseii4, rseii5, rseii6))
stargazer(iii1, iii2, iii3, iii4, iii5, iii6, type = 'text', se = list(rseiii1, rseiii2, rseiii3, rseiii4, rseiii5, rseiii6))

# Running additional robustness checks by adding country level fixed effects to the original models

j1 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
         + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
         + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Country), data = cement)

j2 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
         + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
         + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Country), data = cement)

j3 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita)*Kyoto.I.Rat.Phase.I + log(GDP.per.capita)*Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
         + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
         + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Country), data = cement)

j4 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
         + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
         + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year) + factor(Country), data = cement)

j5 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
         + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
         + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year) + factor(Country), data = cement)

j6 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita)*Kyoto.I.Rat.Phase.I + log(GDP.per.capita)*Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
         + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
         + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year) + factor(Country), data = cement)

jj1 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
          + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
          + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Country), data = cement)

jj2 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
          + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
          + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Country), data = cement)

jj3 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
          + Urban.Population + Lagged.R.D*Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
          + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Country), data = cement)

jj4 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
          + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
          + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year) + factor(Country), data = cement)

jj5 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
          + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
          + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year) + factor(Country), data = cement)

jj6 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
          + Urban.Population + Lagged.R.D*Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
          + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year) + factor(Country), data = cement)

jjj1 <- lm(Intensity ~ log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
           + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
           + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Country), data = cement)

jjj2 <- lm(Intensity ~ log(GDP.per.capita) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
           + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
           + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Country), data = cement)

jjj3 <- lm(Intensity ~ log(GDP.per.capita)*Kyoto.I.Rat.Phase.I + log(GDP.per.capita)*Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
           + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
           + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Country), data = cement)

jjj4 <- lm(Intensity ~ log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
           + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
           + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year) + factor(Country), data = cement)

jjj5 <- lm(Intensity ~ log(GDP.per.capita) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
           + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
           + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year) + factor(Country), data = cement)

jjj6 <- lm(Intensity ~ log(GDP.per.capita)*Kyoto.I.Rat.Phase.I + log(GDP.per.capita)*Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + log(Land.Area) + CO2.Change
           + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
           + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year) + factor(Country), data = cement)

# Calculating robust standard errors

cov1 <- vcovHC(j1, type = 'HC1')
rsej1 <- sqrt(diag(cov1))

cov1 <- vcovHC(j2, type = 'HC1')
rsej2 <- sqrt(diag(cov1))

cov1 <- vcovHC(j3, type = 'HC1')
rsej3 <- sqrt(diag(cov1))

cov1 <- vcovHC(j4, type = 'HC1')
rsej4 <- sqrt(diag(cov1))

cov1 <- vcovHC(j5, type = 'HC1')
rsej5 <- sqrt(diag(cov1))

cov1 <- vcovHC(j6, type = 'HC1')
rsej6 <- sqrt(diag(cov1))

cov1 <- vcovHC(jj1, type = 'HC1')
rsejj1 <- sqrt(diag(cov1))

cov1 <- vcovHC(jj2, type = 'HC1')
rsejj2 <- sqrt(diag(cov1))

cov1 <- vcovHC(jj3, type = 'HC1')
rsejj3 <- sqrt(diag(cov1))

cov1 <- vcovHC(jj4, type = 'HC1')
rsejj4 <- sqrt(diag(cov1))

cov1 <- vcovHC(jj5, type = 'HC1')
rsejj5 <- sqrt(diag(cov1))

cov1 <- vcovHC(jj6, type = 'HC1')
rsejj6 <- sqrt(diag(cov1))

cov1 <- vcovHC(jjj1, type = 'HC1')
rsejjj1 <- sqrt(diag(cov1))

cov1 <- vcovHC(jjj2, type = 'HC1')
rsejjj2 <- sqrt(diag(cov1))

cov1 <- vcovHC(jjj3, type = 'HC1')
rsejjj3 <- sqrt(diag(cov1))

cov1 <- vcovHC(jjj4, type = 'HC1')
rsejjj4 <- sqrt(diag(cov1))

cov1 <- vcovHC(jjj5, type = 'HC1')
rsejjj5 <- sqrt(diag(cov1))

cov1 <- vcovHC(jjj6, type = 'HC1')
rsejjj6 <- sqrt(diag(cov1))

# Viewing results

stargazer(j1, j2, j3, j4, j5, j6, type = 'text', se = list(rsej1, rsej2, rsej3, rsej4, rsej5, rsej6))
stargazer(jj1, jj2, jj3, jj4, jj5, jj6, type = 'text', se = list(rsejj1, rsejj2, rsejj3, rsejj4, rsejj5, rsejj6))
stargazer(jjj1, jjj2, jjj3, jjj4, jjj5, jjj6, type = 'text', se = list(rsejjj1, rsejjj2, rsejjj3, rsejjj4, rsejjj5, rsejjj6))

# Writing main results to file

write.csv(stargazer(i1, i2, i3, i4, i5, i6, type = 'text',
                    se = list(rsei1, rsei2, rsei3, rsei4, rsei5, rsei6)),
          paste(directory, 'intensity_regression_results.txt'), row.names = FALSE)
write.csv(stargazer(i1, i2, i3, i4, i5, i6, se = list(rsei1, rsei2, rsei3, rsei4, rsei5, rsei6)),
          paste(directory, 'intensity_regression_results_tex.txt'), row.names = FALSE)
write.csv(stargazer(ii1, ii2, ii3, ii4, ii5, ii6, type = 'text',
                    se = list(rseii1, rseii2, rseii3, rseii4, rseii5, rseii6)),
          paste(directory, 'intensity_regression_results_2.txt'), row.names = FALSE)
write.csv(stargazer(ii1, ii2, ii3, ii4, ii5, ii6, se = list(rseii1, rseii2, rseii3, rseii4, rseii5, rseii6)),
          paste(directory, 'intensity_regression_results_2_tex.txt'), row.names = FALSE)
write.csv(stargazer(iii1, iii2, iii3, iii4, iii5, iii6, type = 'text',
                    se = list(rseiii1, rseiii2, rseiii3, rseiii4, rseiii5, rseiii6)),
          paste(directory, 'intensity_regression_results_3.txt'), row.names = FALSE)
write.csv(stargazer(iii1, iii2, iii3, iii4, iii5, iii6, se = list(rseiii1, rseiii2, rseiii3, rseiii4, rseiii5, rseiii6)),
          paste(directory, 'intensity_regression_results_tex_3_tex.txt'), row.names = FALSE)

# Creating figures for the paper

# Data prep -- aggregate time series for C intensities

yrs <- c()
c_int <- c()
china_int <- c()
us_int <- c()

for (i in 1998:2014) {
  
  d <- cement[which(cement$Year == i),]
  d <- d[which(d$Intensity > 0),]
  d <- d[which(d$Cement > 0),]
  c_int[[i-1997]] <- sum(d$Intensity * d$Cement) / sum(d$Cement)
  china_int[[i-1997]] <- d[which(d$Country == 'China'),59] 
  us_int[[i-1997]] <- d[which(d$Country == 'United States'),59]
  yrs[[i-1997]] <- i
  
}

Cint.df <- data.frame(Year = c(yrs), World = c(c_int), US = c(us_int), China = c(china_int))

# First plot

ggplot(data = Cint.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('CO2 Emissions from Global Cement Production by Year') +
  ylab('CO2 Emissions (million metric tons of C)') +
  geom_line(aes(y = World, col = 'Global Average'), size = 2, alpha = 1) +
  geom_line(aes(y = US, col = 'USA'), size = 2, alpha = 1) +
  geom_line(aes(y = China, col = 'China'), size = 2, alpha = 1) +
  theme(legend.position = c(0.2,0.2), plot.title = element_text(hjust = 0.5)) +
  ylim(0.03695*3.6667,0.03715*3.6667) + theme(legend.title = element_blank())

dev.copy(png, paste(directory, 'cementensity_fig_1.png'))
dev.off()

# Second plot -- first plot with markers for Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II

ggplot(data = Cint.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('Carbon Intensity for Global Cement Production by Year\nwith Kyoto Protocol Indicators') +
  ylab('Carbon Intensity (kg of C per kg of Cement)') +
  geom_line(aes(y = World, col = 'Global Average'), size = 2, alpha = 1) +
  geom_line(aes(y = US, col = 'USA'), size = 2, alpha = 1) +
  geom_line(aes(y = China, col = 'China'), size = 2, alpha = 1) +
  theme(legend.position = c(0.2,0.2), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  ylim(0.03695*3.6667,0.03715*3.6667) + theme(legend.title = element_blank()) +
  geom_vline(xintercept = 2008) + geom_vline(xintercept = 2013)

dev.copy(png, paste(directory, 'introfig.png'))
dev.off()

# Third plot -- carbon intensities 10 years prior to Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II

ggplot(data = Cint.df[which(Cint.df$Year < 2008),], aes(x = Year, y = value, color = variable)) +
  ggtitle('Carbon Intensity for Global Cement Production by Year') +
  ylab('Carbon Intensity (kg of C per kg of Cement)') +
  geom_line(aes(y = World, col = 'Global Average'), size = 2, alpha = 1) +
  geom_line(aes(y = US, col = 'USA'), size = 2, alpha = 1) +
  geom_line(aes(y = China, col = 'China'), size = 2, alpha = 1) +
  theme(legend.position = c(0.2,0.2), plot.title = element_text(hjust = 0.5)) +
  ylim(0.03695*3.6667,0.03715*3.6667) + theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))

dev.copy(png, paste(directory, 'cementensity_fig_2.png'))
dev.off()

# Fourth plot -- visualizing the relationship between income and carbon intensities -- High income countries have GDP per capita >= 40,000

cement <- cement[which(cement$Cement > 0 & cement$Intensity > 0),]
high <- cement[which(cement$GDP.per.capita >= 30000),]
low <- cement[which(cement$GDP.per.capita > 0 & cement$GDP.per.capita < 30000),]

hi <- c()
li <- c()
yrs <- c()

for (yr in 1990:2014) {
  
  h <- high[which(high$Year == yr),]
  l <- low[which(low$Year == yr),]
  
  hc <- sum(h$Cement)
  lc <- sum(l$Cement)
  
  hi[[yr-1989]] <- sum(h$Intensity * h$Cement) / hc
  li[[yr-1989]] <- sum(l$Intensity * l$Cement) / lc
  yrs[[yr-1989]] <- yr
  
}

Cint.df <- data.frame(Year = c(yrs), High = c(hi), Low = c(li))

ggplot(data = Cint.df[which(Cint.df$Year < 2008),], aes(x = Year, y = value)) +
  ggtitle('Relationship Between GDP per capita and Carbon Intensity\nPrior to the Kyoto Protocol') +
  ylab('Carbon Intensity (kg of C per kg of Cement)') +
  geom_line(aes(y = High, col = 'High Income Nations'), size = 2, alpha = 1) +
  geom_line(aes(y = Low, col = 'Low Income Nations'), size = 2, alpha = 1) +
  theme(legend.position = c(0.19,0.81), legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

dev.copy(png, paste(directory, 'cementensity_fig_3.png'))
dev.off()

# Fifth plot -- an extension of the fourth plot

ggplot(data = Cint.df[which(Cint.df$Year < 2012),], aes(x = Year, y = value)) +
  ggtitle('Relationship Between GDP per capita and Carbon Intensity') +
  ylab('Carbon Intensity (kg of C per kg of Cement)') +
  geom_line(aes(y = High, col = 'High Income Nations'), size = 2, alpha = 1) +
  geom_line(aes(y = Low, col = 'Low Income Nations'), size = 2, alpha = 1) +
  theme(legend.position = c(0.2075,0.425), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 2008)

dev.copy(png, paste(directory, 'cementensity_fig_4.png'))
dev.off()

# Sixth plot -- a baseline for the simulation plot

ggplot(data = Cint.df[which(Cint.df$Year < 2008 & Cint.df$Year > 1997),], aes(x = Year, y = value)) +
  ggtitle('Relationship Between GDP per capita and Carbon Intensity\nPrior to the Kyoto Protocol') +
  ylab('Carbon Intensity (kg of C per kg of Cement)') +
  geom_line(aes(y = High, col = 'High Income Nations'), size = 2, alpha = 1) +
  geom_line(aes(y = Low, col = 'Low Income Nations'), size = 2, alpha = 1) +
  theme(legend.position = c(0.19,0.81), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))

dev.copy(png, paste(directory, 'simulation_baseline.png'))
dev.off()

