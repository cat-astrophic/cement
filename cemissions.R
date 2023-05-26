# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on CO2 emissions from cement manufacturing

# Loading libraries

library(sandwich)
library(stargazer)
library(ggplot2)
library(lmtest)
library(AER)
library(coeftest)

# Reading in the panel data

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')

# Visualizing total world production as a time series

yrs <- c()
c_sums <- c()

for (i in 1990:2014) {
  
  d <- cement[which(cement$Year == i),]
  c_sums[[i-1989]] <- sum(d$Cement.Emissions, na.rm = TRUE) / 1000
  yrs[[i-1989]] <- i
  
}

all.df = data.frame(Year = c(yrs), C = c(c_sums))

# First plot - Global CO2 emissions from cement production

ggplot(data = all.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('CO2 Emissions from Global Cement Production by Year') +
  ylab('CO2 Emissions (million metric tons of C)') +
  geom_line(aes(y = C , col = 'Cement'), size = 2, alpha = 1) +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0,600) + scale_x_continuous(breaks = scales::pretty_breaks(n = 12))

dev.copy(png, paste(directory, 'cemissions_fig_1.png'))
dev.off()

# Second Plot - Global CO2 emissions from cement production with Kyoto Protocol phases indicated

ggplot(data = all.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('CO2 Emissions from Global Cement Production by Year\nwith Kyoto Protocol Indicators') +
  ylab('CO2 Emissions (million metric tons of C)') +
  geom_line(aes(y = C , col = 'Cement'), size = 2, alpha = 1) +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0,600) + scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  geom_vline(xintercept = 2008) +
  geom_vline(xintercept = 2013)

dev.copy(png, paste(directory, 'cemissions_fig_2.png'))
dev.off()

# Subsetting CO2 emissions from cement data into China v rest-of-world data

china <- cement[which(cement$Country == 'China'),]
others <- cement[which(cement$Country != 'China'),]
c2_sums <- c() # Chinese production
c3_sums <- c() # rest-of-world production

for (i in 1990:2014) {
  
  d2 <- china[which(china$Year == i),]
  d3 <- others[which(others$Year == i),]
  c2_sums[[i-1989]] <- sum(d2$Cement.Emissions, na.rm = TRUE) / 1000
  c3_sums[[i-1989]] <- sum(d3$Cement.Emissions, na.rm = TRUE) / 1000
  
}

gtemp.df = data.frame(Year = c(yrs), C = c(c2_sums), ROW = c(c3_sums), W = c(c_sums))

# Third Plot - China v rest-of-world CO2 emissions from cement production

ggplot(data = gtemp.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('CO2 Emissions from Cement Production by Year:\nChina v. Rest of World') +
  ylab('CO2 Emissions (million metric tons)') +
  geom_line(aes(y = W, col = 'World'), size = 2, alpha = 1) +
  geom_line(aes(y = C, col = 'China'), size = 2, alpha = 1) +
  geom_line(aes(y = ROW, col = 'Rest of World'),  size = 2, alpha = 1) +
  theme(legend.position = c(.15,.85), plot.title = element_text(hjust = 0.5)) +
  ylim(0,650) + scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 2008) +
  geom_vline(xintercept = 2013)

dev.copy(png, paste(directory, 'cemissions_fig_3.png'))
dev.off()

# Fourth Plot - China v rest-of-world CO2 emissions from cement production with Kyoto Protocol phases indicated

ggplot(data = gtemp.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('CO2 Emissions from Cement Production by Year:\nChina v. Rest of World') +
  ylab('CO2 Emissions (million metric tons)') +
  geom_line(aes(y = C , col = 'China'), size = 2, alpha = 1) +
  geom_line(aes(y = ROW, col = 'Rest of World'),  size = 2, alpha = 1) +
  theme(legend.position = c(.15,.85), plot.title = element_text(hjust = 0.5)) +
  ylim(0,350) + scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 2008) + 
  geom_vline(xintercept = 2013)

dev.copy(png, paste(directory, 'cemissions_fig_4.png'))
dev.off()

# Fifth Plot - Chinese CO2 emissions from cement production

ggplot(data = gtemp.df, aes(x = Year, y = value)) + 
  ggtitle('CO3 Emissions from Chinese Cement Production by Year') +
  ylab('CO2 Emissions (million metric tons)') +
  geom_line(aes(y = C , col = 'China'), size = 2, alpha = 1) +
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5)) +
  ylim(0,350) + scale_x_continuous(breaks = scales::pretty_breaks(n = 12))

dev.copy(png, paste(directory, 'cemissions_fig_5.png'))
dev.off()

# Sixth Plot - All 3 CO2 emissions from cement production time series

df <- data.frame(Year = c(yrs), W = c(c_sums), C = c(c2_sums), ROW = c(c3_sums))

ggplot(data = df, aes(x = Year, y = value, color = variable)) + 
  ggtitle('Comparison of CO2 Emissions from Cement Production') +
  ylab('CO2 Emissions (million metric tons)') +
  geom_line(aes(y = C , col = 'China'), size = 2, alpha = 1) +
  geom_line(aes(y = W , col = 'World'), size = 2, alpha = 1) +
  geom_line(aes(y = ROW , col = 'Rest of World'), size = 2, alpha = 1) +
  theme(legend.position = c(.15,85), plot.title = element_text(hjust = 0.5)) +
  ylim(0,600) + scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  theme(legend.title = element_blank())

dev.copy(png, paste(directory, 'cemissions_fig_6.png'))
dev.off()

# Visualizing relationships between the data

par(mfrow = c(2,2))
plot(cement$GDP.per.capita, log(cement$Cement.Emissions))
plot(cement$GDP.per.capita.Growth.Rate, log(cement$Cement.Emissions))
plot(cement$GDP.Growth.Rate, log(cement$Cement.Emissions))
plot(log(cement$Population),log(cement$Cement.Emissions))

par(mfrow = c(2,2))
plot(cement$Population.Growth.Rate, log(cement$Cement.Emissions))
plot(log(cement$CO2.Emissions), log(cement$Cement.Emissions))
plot(cement$CO2.Emissions.per.capita, log(cement$Cement.Emissions))

par(mfrow = c(2,2))
plot(cement$Land.Area, log(cement$Cement.Emissions))
plot(cement$Ores.and.Metals.Exports, log(cement$Cement.Emissions))
plot(cement$Ores.and.Metals.Imports, log(cement$Cement.Emissions))

par(mfrow = c(2,2))
plot(cement$Real.Interest.Rate, log(cement$Cement.Emissions))
plot(cement$Urban.Population, log(cement$Cement.Emissions))
plot(cement$Urban.Population.Growth, log(cement$Cement.Emissions))

# Based on the above we want to remove all instances of the real interest rate being large enough to indicate hyperinflation

cement <- cement[which(abs(cement$Real.Interest.Rate) < 50),]

# Difference-in-differences models

cement$Post2005 <- as.numeric(cement$Year > 2005)
cement$Post2006 <- as.numeric(cement$Year > 2006)
cement$Post2007 <- as.numeric(cement$Year > 2007)
cement$Post2008 <- as.numeric(cement$Year > 2008)

rar1 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.Rat*Post2005 + factor(Year) + factor(Country), data = cement)

rar2 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.Rat*Post2006 + factor(Year) + factor(Country), data = cement)

rar3 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.Rat*Post2007 + factor(Year) + factor(Country), data = cement)

rar4 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.Rat*Post2008 + factor(Year) + factor(Country), data = cement)

rar1x <- coeftest(rar1, vcov = vcovCL, cluster = ~Kyoto.Rat)
rar2x <- coeftest(rar2, vcov = vcovCL, cluster = ~Kyoto.Rat)
rar3x <- coeftest(rar3, vcov = vcovCL, cluster = ~Kyoto.Rat)
rar4x <- coeftest(rar4, vcov = vcovCL, cluster = ~Kyoto.Rat)

stargazer(rar1x, rar2x, rar3x, rar4x, type = 'text', omit = c('Year', 'Country'))

write.csv(stargazer(rar1x, rar2x, rar3x, rar4x, type = 'text'), paste(directory, 'emissions_regression_results_DID.txt'), row.names = FALSE)

write.csv(stargazer(rar1x, rar2x, rar3x, rar4x), paste(directory, 'emissions_regression_results_DID_tex.txt'), row.names = FALSE)

# Double interaction model

xrar1 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
            + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.Rat*Post2005 + Kyoto.Rat*Emissions.Trading + factor(Year) + factor(Country), data = cement)

xrar2 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
            + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.Rat*Post2006 + Kyoto.Rat*Emissions.Trading + factor(Year) + factor(Country), data = cement)

xrar3 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
            + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.Rat*Post2007 + Kyoto.Rat*Emissions.Trading + factor(Year) + factor(Country), data = cement)

xrar4 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
            + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.Rat*Post2008 + Kyoto.Rat*Emissions.Trading + factor(Year) + factor(Country), data = cement)

xrar1x <- coeftest(xrar1, vcov = vcovCL, cluster = ~Kyoto.Rat)
xrar2x <- coeftest(xrar2, vcov = vcovCL, cluster = ~Kyoto.Rat)
xrar3x <- coeftest(xrar3, vcov = vcovCL, cluster = ~Kyoto.Rat)
xrar4x <- coeftest(xrar4, vcov = vcovCL, cluster = ~Kyoto.Rat)

stargazer(xrar1x, xrar2x, xrar3x, xrar4x, type = 'text', omit = c('Year', 'Country'))

write.csv(stargazer(xrar1x, xrar2x, xrar3x, xrar4x, type = 'text'), paste(directory, 'emissions_regression_results_DID_2.txt'), row.names = FALSE)

write.csv(stargazer(xrar1x, xrar2x, xrar3x, xrar4x), paste(directory, 'emissions_regression_results_DID_2_tex.txt'), row.names = FALSE)

# Repeat with IV using ICC membership

ixrar1 <- ivreg(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
                + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
                + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.Rat*Post2005 + Kyoto.Rat*Emissions.Trading + factor(Year) + factor(Country) | . - Kyoto.Rat + ICC, data = cement)

ixrar2 <- ivreg(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
                + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
                + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.Rat*Post2006 + Kyoto.Rat*Emissions.Trading + factor(Year) + factor(Country) | . - Kyoto.Rat + ICC, data = cement)

ixrar3 <- ivreg(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
                + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
                + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.Rat*Post2007 + Kyoto.Rat*Emissions.Trading + factor(Year) + factor(Country) | . - Kyoto.Rat + ICC, data = cement)

ixrar4 <- ivreg(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
                + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
                + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.Rat*Post2008 + Kyoto.Rat*Emissions.Trading + factor(Year) + factor(Country) | . - Kyoto.Rat + ICC, data = cement)

ixrar1x <- coeftest(ixrar1, vcov = vcovCL, cluster = ~Kyoto.Rat)
ixrar2x <- coeftest(ixrar2, vcov = vcovCL, cluster = ~Kyoto.Rat)
ixrar3x <- coeftest(ixrar3, vcov = vcovCL, cluster = ~Kyoto.Rat)
ixrar4x <- coeftest(ixrar4, vcov = vcovCL, cluster = ~Kyoto.Rat)

stargazer(ixrar1x, ixrar2x, ixrar3x, ixrar4x, type = 'text', omit = c('Year', 'Country'))

write.csv(stargazer(ixrar1x, ixrar2x, ixrar3x, ixrar4x, type = 'text'), paste(directory, 'emissions_regression_results_DID_IV.txt'), row.names = FALSE)

write.csv(stargazer(ixrar1x, ixrar2x, ixrar3x, ixrar4x), paste(directory, 'emissions_regression_results_DID_IV_tex.txt'), row.names = FALSE)

# Check the first stage F-stat

c.cols <- c('Cement.Emissions', 'Lagged.Cement.Emissions', 'GDP.per.capita', 'Population', 'Real.Interest.Rate', 'Land.Area', 'CO2.Change',
            'Renewable.Electricity.Output', 'Ores.and.Metals.Imports', 'Ores.and.Metals.Exports', 'Polity.Index',
            'Forest.Rents', 'Tariff.Rate', 'Lagged.R.D', 'Kyoto.Rat', 'Emissions.Trading', 'ICC')

cementx <- cement[,which(colnames(cement) %in% c.cols)]
cementx <- cementx[which(complete.cases(cementx)),]

oof <- lm(Kyoto.Rat ~ ICC, data = cementx)
stargazer(oof, type = 'text')

# Repeat with IV using ICC membership with fewer controls to include more low income nations

ixrar1 <- ivreg(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
                + Polity.Index + Kyoto.Rat*Post2005 + Kyoto.Rat*Emissions.Trading + factor(Year) + factor(Country) | . - Kyoto.Rat + ICC, data = cement)

ixrar2 <- ivreg(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
                + Polity.Index + Kyoto.Rat*Post2006 + Kyoto.Rat*Emissions.Trading + factor(Year) + factor(Country) | . - Kyoto.Rat + ICC, data = cement)

ixrar3 <- ivreg(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
                + Polity.Index + Kyoto.Rat*Post2007 + Kyoto.Rat*Emissions.Trading + factor(Year) + factor(Country) | . - Kyoto.Rat + ICC, data = cement)

ixrar4 <- ivreg(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + CO2.Change
                + Polity.Index + Kyoto.Rat*Post2008 + Kyoto.Rat*Emissions.Trading + factor(Year) + factor(Country) | . - Kyoto.Rat + ICC, data = cement)

ixrar1x <- coeftest(ixrar1, vcov = vcovCL, cluster = ~Country)
ixrar2x <- coeftest(ixrar2, vcov = vcovCL, cluster = ~Country)
ixrar3x <- coeftest(ixrar3, vcov = vcovCL, cluster = ~Country)
ixrar4x <- coeftest(ixrar4, vcov = vcovCL, cluster = ~Country)

stargazer(ixrar1x, ixrar2x, ixrar3x, ixrar4x, type = 'text', omit = c('Year', 'Country'))
stargazer(ixrar1, ixrar2, ixrar3, ixrar4, type = 'text', omit = c('Year', 'Country'))

c.cols <- c('Cement.Emissions', 'Lagged.Cement.Emissions', 'GDP.per.capita', 'Population', 'Real.Interest.Rate',
            'Land.Area', 'CO2.Change', 'Polity.Index','Kyoto.Rat', 'Emissions.Trading', 'ICC', 'Country')

cementx <- cement[,which(colnames(cement) %in% c.cols)]
cementx <- cementx[which(complete.cases(cementx)),]

oof <- lm(Kyoto.Rat ~ ICC, data = cementx)
stargazer(oof, type = 'text')

