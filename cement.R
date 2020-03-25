# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

library(stargazer)
library(plotly)

# Declaring the filepath for the data file

filepath <- 'C:/Users/User/Documents/Data/cement/cement.csv'

# Reading in the data

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')

# Visualizing total world production as a time series

TOT <- c()
c_sums <- c()

for (i in 1990:2016) {
  
  d <- cement[which(cement$Year == i),]
  c_sums[[i-1989]] <- sum(d$Cement, na.rm = T) / 1000000
  TOT[[i-1989]] <- i
  
}

plot(TOT, c_sums, main = 'Global Cement Production by Year', xlab = 'Year', ylab = 'Cement Production (billion metric tons)',
     ylim = c(0,4.5), col = 'black', type = 'l', lwd = 2, yaxt= 'n', xaxt = 'n')
axis(1, xaxp = c(1990, 2016, 13), las = 2)
axis(2, yaxp = c(0, 4.5, 9), las = 2)
abline(v = 2008)
abline(v = 2013)

# Visualizing relationships between the data

par(mfrow = c(2,2))
plot(cement$GDP.per.capita, log(cement$Cement))
plot(cement$GDP.per.capita.Growth.Rate, log(cement$Cement))
plot(cement$GDP.Growth.Rate, log(cement$Cement))
plot(log(cement$Population),log(cement$Cement))

par(mfrow = c(2,2))
plot(cement$Population.Growth.Rate, log(cement$Cement))
plot(cement$CO2.Intensity, log(cement$Cement))
plot(log(cement$CO2.Emissions), log(cement$Cement))
plot(cement$CO2.Emissions.per.capita, log(cement$Cement))

par(mfrow = c(2,2))
plot(cement$CO2.Emissions.from.M.C, log(cement$Cement))
plot(cement$Land.Area, log(cement$Cement))
plot(cement$Ores.and.Metals.Exports, log(cement$Cement))
plot(cement$Ores.and.Metals.Imports, log(cement$Cement))

par(mfrow = c(2,2))
plot(cement$Real.Interest.Rate, log(cement$Cement))
plot(cement$Urban.Population, log(cement$Cement))
plot(cement$Urban.Population.Growth, log(cement$Cement))

# Based on the above we want to remove all instances of the real interest rate being large enough to indicate hyperinflation

cement <- cement[which(abs(cement$Real.Interest.Rate) < 50),]

# Running OLS models

mod1 <- lm(log(Cement) ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) 
           + Forest.Rents + Tariff.Rate, data = cement)

mod2 <- lm(log(Cement) ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + log(Lagged.CO2.Emissions.from.M.C), data = cement)

mod3 <- lm(log(Cement) ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Emissions.Trading, data = cement)

mod4 <- lm(log(Cement) ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C), data = cement)

mod5 <- lm(log(Cement) ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

mod6 <- lm(log(Cement) ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I
           + Kyoto.I.Rat.Phase.II + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

mod7 <- lm(log(Cement) ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

mod8 <- lm(log(Cement) ~ log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

# Running autoregressive models

ar1 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate, data = cement)

ar2 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + log(Lagged.CO2.Emissions.from.M.C), data = cement)

ar3 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Emissions.Trading, data = cement)

ar4 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C), data = cement)

ar5 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
          + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ar6 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I
          + Kyoto.I.Rat.Phase.II + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ar7 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ar8 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

# Viewing results - OLS

stargazer(mod1,mod2,mod3,mod4,type='text')
stargazer(mod5,mod6,mod7,mod8,type='text')

# Viewing results - autoregressie

stargazer(ar1,ar2,ar3,ar4,type='text')
stargazer(ar5,ar6,ar7,ar8,type='text')

