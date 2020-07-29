# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

library(sandwich)
library(stargazer)
library(ggplot2)

# Reading in the panel data

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')

# Visualizing total world production as a time series

yrs <- c()
c_sums <- c()

for (i in 1990:2016) {
  
  d <- cement[which(cement$Year == i),]
  c_sums[[i-1989]] <- sum(d$Cement, na.rm = TRUE) / 1000000
  yrs[[i-1989]] <- i
  
}


c_sums1 <- c_sums*1000
c_sums2 <- c_sums*1000
all.df = data.frame(Year = c(yrs), a1 = c(c_sums1), a2 = c(c_sums2), C = c(c_sums))

# First plot - Global cement production

ggplot(data = all.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('Global Cement Production by Year') +
  ylab('Cement Production (billion metric tons)') +
  geom_line(aes(y = C , col = 'Cement'), size = 2, alpha = 1) +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0,4.5) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14))

dev.copy(png, paste(directory, 'cement_fig_1.png'))
dev.off()

# Second Plot - Global cement production with Kyoto Protocol phases indicated

ggplot(data = all.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('Global Cement Production by Year with Kyoto Protocol Indicators') +
  ylab('Cement Production (billion metric tons)') +
  geom_line(aes(y = a1 , col = 'a1'), size = 2, alpha = 1) +
  geom_line(aes(y = a2 , col = 'a2'), size = 2, alpha = 1) +
  geom_line(aes(y = C , col = 'Cement'), size = 2, alpha = 1) +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0,4.5) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +
  geom_vline(xintercept = 2008) +
  geom_vline(xintercept = 2013)

dev.copy(png, paste(directory, 'cement_fig_2.png'))
dev.off()

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

dev.copy(png, paste(directory, 'cement_fig_3.png'))
dev.off()

# Subsetting cement data into China v rest-of-world data

china <- cement[which(cement$Country == 'China'),]
others <- cement[which(cement$Country != 'China'),]
c2_sums <- c() # Chinese production
c3_sums <- c() # rest-of-world production

for (i in 1990:2016) {
  
  d2 <- china[which(china$Year == i),]
  d3 <- others[which(others$Year == i),]
  c2_sums[[i-1989]] <- sum(d2$Cement, na.rm = TRUE) / 1000000
  c3_sums[[i-1989]] <- sum(d3$Cement, na.rm = TRUE) / 1000000
  
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

dev.copy(png, paste(directory, 'cement_fig_4.png'))
dev.off()

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

dev.copy(png, paste(directory, 'cement_fig_5.png'))
dev.off()

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

dev.copy(png, paste(directory, 'cement_fig_6.png'))
dev.off()

# Seventh Plot - Chinese cement production

ggplot(data = gtemp.df, aes(x = Year, y = value)) + 
  ggtitle('Chinese Cement Production by Year') +
  ylab('Cement Production (billion metric tons)') +
  geom_line(aes(y = C , col = 'China'), size = 2, alpha = 1) +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0,3) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14))

dev.copy(png, paste(directory, 'cement_fig_7.png'))
dev.off()

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

dev.copy(png, paste(directory, 'cement_fig_8.png'))
dev.off()

# Ninth Plot - All 3 cement production time series

df <- data.frame(Year = c(yrs), W = c(c_sums), C = c(c2_sums), ROW = c(c3_sums))

ggplot(data = df, aes(x = Year, y = value, color = variable)) + 
  ggtitle('Comparison of Cement Production') +
  ylab('Cement Production (billion metric tons)') +
  geom_line(aes(y = W , col = 'World'), size = 2, alpha = 1) +
  geom_line(aes(y = C , col = 'China'), size = 2, alpha = 1) +
  geom_line(aes(y = ROW , col = 'Rest of World'), size = 2, alpha = 1) +
  theme(legend.position = c(.15,.85), plot.title = element_text(hjust = 0.5)) +
  ylim(0,4.5) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 2008) +
  geom_vline(xintercept = 2013)

dev.copy(png, paste(directory, 'cement_fig_9.png'))
dev.off()

# Visualizing relationships between the data

par(mfrow = c(2,2))
plot(cement$GDP.per.capita, log(cement$Cement))
plot(cement$GDP.per.capita.Growth.Rate, log(cement$Cement))
plot(cement$GDP.Growth.Rate, log(cement$Cement))
plot(log(cement$Population),log(cement$Cement))

par(mfrow = c(2,2))
plot(cement$Population.Growth.Rate, log(cement$Cement))
plot(log(cement$CO2.Emissions), log(cement$Cement))
plot(cement$CO2.Emissions.per.capita, log(cement$Cement))

par(mfrow = c(2,2))
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

ar1 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
          + Forest.Rents + Tariff.Rate + Lagged.R.D, data = cement)

ar3 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading, data = cement)

ar5 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
          + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ar7 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ar9 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + KP, data = cement)

ar11 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
         + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
         + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading, data = cement)

ar13 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ar15 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ar17 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year), data = cement)

ar19 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year), data = cement)

ar21 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading + factor(Year), data = cement)

ar23 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + factor(Year), data = cement)

ar25 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + factor(Year), data = cement)

ar27 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading + factor(Year), data = cement)

ar29 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Country), data = cement)

ar31 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Country), data = cement)

ar33 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + factor(Country), data = cement)

ar35 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading + factor(Country), data = cement)

ar37 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year) + factor(Country), data = cement)

ar39 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year) + factor(Country), data = cement)

ar41 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + factor(Year) + factor(Country), data = cement)

ar43 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading + factor(Year) + factor(Country), data = cement)

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

cov1 <- vcovHC(ar29, type = 'HC1')
rse29 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar31, type = 'HC1')
rse31 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar33, type = 'HC1')
rse33 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar35, type = 'HC1')
rse35 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar37, type = 'HC1')
rse37 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar39, type = 'HC1')
rse39 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar41, type = 'HC1')
rse41 <- sqrt(diag(cov1))

cov1 <- vcovHC(ar43, type = 'HC1')
rse43 <- sqrt(diag(cov1))

# Viewing results

stargazer(ar1, ar3, type = 'text', se = list(rse1, rse3)) # No KP effects
stargazer(ar5, ar7, type = 'text', se = list(rse5, rse7)) # KP effects by phase
stargazer(ar9, ar11, type = 'text', se = list(rse9, rse11)) # General KP effects
stargazer(ar13, ar15, type = 'text', se = list(rse13, rse15)) # KP effects by phase
stargazer(ar17, ar19, type = 'text', se = list(rse17, rse19)) # No KP effects with year FE
stargazer(ar21, ar23, type = 'text', se = list(rse21, rse23)) # KP effects by phase with year FE
stargazer(ar25, ar27, type = 'text', se = list(rse25, rse27)) # General KP effects with year FE
stargazer(ar29, ar31, type = 'text', se = list(rse29, rse31)) # No KP effects with country FE
stargazer(ar33, ar35, type = 'text', se = list(rse33, rse35)) # General KP effects with country FE
stargazer(ar37, ar39, type = 'text', se = list(rse37, rse39)) # No KP effects with year and country FE
stargazer(ar41, ar43, type = 'text', se = list(rse41, rse43)) # General KP effects with year and country FE

# Writing results to file

write.csv(stargazer(ar1, ar3, ar5, ar7, ar13, ar15, ar21, ar23, type = 'text',
                    se = list(rse1, rse3, rse5, rse7, rse13, rse15, rse21, rse23)),
          paste(directory, 'cement_regression_results_phases.txt'), row.names = FALSE)
write.csv(stargazer(ar1, ar3, ar5, ar7, ar13, ar15, ar21, ar23,
                    se = list(rse1, rse3, rse5, rse7, rse13, rse15, rse21, rse23)),
          paste(directory, 'cement_regression_results_phases_tex.txt'), row.names = FALSE)
write.csv(stargazer(ar1, ar3, ar9, ar11, ar17, ar19, ar25, ar27, type = 'text',
                    se = list(rse1, rse3, rse9, rse11, rse17, rse19, rse25, rse27)),
          paste(directory, 'cement_regression_results_all.txt'), row.names = FALSE)
write.csv(stargazer(ar1, ar3, ar9, ar11, ar17, ar19, ar25, ar27,
                    se = list(rse1, rse3, rse9, rse11, rse17, rse19, rse25, rse27)),
          paste(directory, 'cement_regression_results_all_tex.txt'), row.names = FALSE)
write.csv(stargazer(ar29, ar31, ar33, ar35, ar37, ar39, ar41, ar43, type = 'text',
                    se = list(rse29, rse31, rse33, rse35, rse37, rse39, rse41, rse43)),
          paste(directory, 'cement_regression_results_all_robcheck.txt'), row.names = FALSE)
write.csv(stargazer(ar29, ar31, ar33, ar35, ar37, ar39, ar41, ar43,
                    se = list(rse29, rse31, rse33, rse35, rse37, rse39, rse41, rse43)),
          paste(directory, 'cement_regression_results_all_robcheck_tex.txt'), row.names = FALSE)

