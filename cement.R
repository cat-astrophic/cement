# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

library(stargazer)
library(ggplot2)

# Declaring the filepath for the data file

filepath <- 'C:/Users/User/Documents/Data/cement/cement.csv'

# Reading in the panel data

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')

# Visualizing total world production as a time series

yrs <- c()
c_sums <- c()

for (i in 1990:2016) {
  
  d <- cement[which(cement$Year == i),]
  c_sums[[i-1989]] <- sum(d$Cement, na.rm = T) / 1000000
  yrs[[i-1989]] <- i
  
}

all.df = data.frame(Year = c(yrs), C = c(c_sums))

# First plot - Global cement production

ggplot(data = all.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('Global Cement Production by Year') +
  ylab('Cement Production (billion metric tons)') +
  geom_line(aes(y = C , col = 'Cement'), size = 2, alpha = 1) +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0,4.5) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14))

# Second Plot - Global cement production with Kyoto Protocol phases indicated

ggplot(data = all.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('Global Cement Production by Year with Kyoto Protocol Indicators') +
  ylab('Cement Production (billion metric tons)') +
  geom_line(aes(y = C , col = 'Cement'), size = 2, alpha = 1) +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0,4.5) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +
  geom_vline(xintercept = 2008) +
  geom_vline(xintercept = 2013)
  
# Third plot - Global cement production with five year plans indicated

ggplot(data = all.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('Global Cement Production by Year with 5 Year Plan Indicators') +
  ylab('Cement Production (billion metric tons)') +
  geom_line(aes(y = C , col = 'Cement'), size = 2, alpha = 1) +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0,4.5) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +
  geom_vline(xintercept = 1990) +
  geom_vline(xintercept = 1995) +
  geom_vline(xintercept = 2000) +
  geom_vline(xintercept = 2005) +
  geom_vline(xintercept = 2010) +
  geom_vline(xintercept = 2015)

# Subsetting cement data into China v rest-of-world data

china <- cement[which(cement$Country == 'China'),]
others <- cement[which(cement$Country != 'China'),]
c2_sums <- c() # Chinese production
c3_sums <- c() # rest-of-world production

for (i in 1990:2016) {
  
  d2 <- china[which(china$Year == i),]
  d3 <- others[which(others$Year == i),]
  c2_sums[[i-1989]] <- sum(d2$Cement, na.rm = T) / 1000000
  c3_sums[[i-1989]] <- sum(d3$Cement, na.rm = T) / 1000000
  
}

gtemp.df = data.frame(Year = c(yrs), C = c(c2_sums), ROW = c(c3_sums))

# Fourth Plot - China v rest-of-world cement production

ggplot(data = gtemp.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('Cement Production by Year: China v. Rest of World') +
  ylab('Cement Production (billion metric tons)') +
  geom_line(aes(y = C , col = 'China'), size = 2, alpha = 1) +
  geom_line(aes(y = ROW, col = 'Rest of World'),  size = 2, alpha = 1) +
  theme(legend.position = c(.15,.85), plot.title = element_text(hjust = 0.5)) +
  ylim(0,3) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +
  theme(legend.title = element_blank())

# Fifth Plot - China v rest-of-world cement production with Kyoto Protocol phases indicated

ggplot(data = gtemp.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('Cement Production by Year: China v. Rest of World') +
  ylab('Cement Production (billion metric tons)') +
  geom_line(aes(y = C , col = 'China'), size = 2, alpha = 1) +
  geom_line(aes(y = ROW, col = 'Rest of World'),  size = 2, alpha = 1) +
  theme(legend.position = c(.15,.85), plot.title = element_text(hjust = 0.5)) +
  ylim(0,3) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 2008) + 
  geom_vline(xintercept = 2012)

# Sixth Plot - China v rest-of-world cement production with five year plans indicated

ggplot(data = gtemp.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('Cement Production by Year: China v. Rest of World') +
  ylab('Cement Production (billion metric tons)') +
  geom_line(aes(y = C , col = 'China'), size = 2, alpha = 1) +
  geom_line(aes(y = ROW, col = 'Rest of World'),  size = 2, alpha = 1) +
  theme(legend.position = c(.15,.85), plot.title = element_text(hjust = 0.5)) +
  ylim(0,3) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 1990) +
  geom_vline(xintercept = 1995) + 
  geom_vline(xintercept = 2000) +
  geom_vline(xintercept = 2005) +
  geom_vline(xintercept = 2010) +
  geom_vline(xintercept = 2015)

# Seventh Plot - Chinese cement production

ggplot(data = gtemp.df, aes(x = Year, y = value)) + 
  ggtitle('Chinese Cement Production by Year') +
  ylab('Cement Production (billion metric tons)') +
  geom_line(aes(y = C , col = 'China'), size = 2, alpha = 1) +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0,3) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14))

# Eighth Plot - chinese cement production with five year plans indicated

ggplot(data = gtemp.df, aes(x = Year, y = value)) + 
  ggtitle('Chinese Cement Production by Year') +
  ylab('Cement Production (billion metric tons)') +
  geom_line(aes(y = C , col = 'China'), size = 2, alpha = 1) +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0,3) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +
  geom_vline(xintercept = 1990) +
  geom_vline(xintercept = 1995) + 
  geom_vline(xintercept = 2000) +
  geom_vline(xintercept = 2005) +
  geom_vline(xintercept = 2010) +
  geom_vline(xintercept = 2015)

# Ninth Plot - All 3 cement production time series

df <- data.frame(Year = c(yrs), W = c(c_sums), C = c(c2_sums), ROW = c(c3_sums))

ggplot(data = df, aes(x = Year, y = value, color = variable)) + 
  ggtitle('Comparison of Cement Production') +
  ylab('Cement Production (billion metric tons)') +
  geom_line(aes(y = C , col = 'China'), size = 2, alpha = 1) +
  geom_line(aes(y = W , col = 'World'), size = 2, alpha = 1) +
  geom_line(aes(y = ROW , col = 'Rest of World'), size = 2, alpha = 1) +
  theme(legend.position = c(.15,85), plot.title = element_text(hjust = 0.5)) +
  ylim(0,4.5) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +
  theme(legend.title = element_blank())

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

ar9 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
         + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
         + Forest.Rents + Tariff.Rate + Emissions.Trading + KP + KP*Emissions.Trading, data = cement)

ar10 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + KP + KP*Emissions.Trading, data = cement)

ar11 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + KP, data = cement)

ar12 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + log(Lagged.CO2.Emissions.from.M.C) + KP, data = cement)

ar13 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar14 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I
           + Kyoto.I.Rat.Phase.II + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar15 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar16 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar17 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Emissions.Trading + KP + KP*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar18 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + KP + KP*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar19 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar20 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + log(Lagged.CO2.Emissions.from.M.C) + KP
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar21 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar22 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I
           + Kyoto.I.Rat.Phase.II + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar23 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar24 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar25 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Emissions.Trading + KP + KP*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar26 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + KP + KP*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar27 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar28 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + log(Lagged.CO2.Emissions.from.M.C) + KP
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

# Viewing results

stargazer(ar1, ar2, ar3, ar4, type = 'text') # No KP effects
stargazer(ar5, ar6, ar7, ar8, type = 'text') # KP effects by phase
stargazer(ar9, ar10, ar11, ar12, type = 'text') # General KP effects
stargazer(ar13, ar14, ar15, ar16, type = 'text') # KP effects by phase with 5YPs
stargazer(ar17, ar18, ar19, ar20, type = 'text') # General KP effects with 5YPs
stargazer(ar21, ar22, ar23, ar24, type = 'text') # KP effects by phase with 5YPs with year FE
stargazer(ar25, ar26, ar27, ar28, type = 'text') # General KP effects with 5YPs with year FE

