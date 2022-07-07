# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

library(sandwich)
library(stargazer)
library(ggplot2)
library(lmtest)

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

# Difference-in-differences models

cement$Post2004 <- as.numeric(cement$Year > 2004)
cement$Post2005 <- as.numeric(cement$Year > 2005)
cement$Post2006 <- as.numeric(cement$Year > 2006)
cement$Post2007 <- as.numeric(cement$Year > 2007)

rar1 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.Rat*Post2004 + factor(Year), data = cement)

rar2 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.Rat*Post2005 + factor(Year), data = cement)

rar3 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.Rat*Post2006 + factor(Year), data = cement)

rar4 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.Rat*Post2007 + factor(Year), data = cement)

rar1x <- coeftest(rar1, vcov = vcovCL, cluster = ~Country)
rar2x <- coeftest(rar2, vcov = vcovCL, cluster = ~Country)
rar3x <- coeftest(rar3, vcov = vcovCL, cluster = ~Country)
rar4x <- coeftest(rar4, vcov = vcovCL, cluster = ~Country)

stargazer(rar1x, rar2x, rar3x, rar4x, type = 'text')

write.csv(stargazer(rar1x, rar2x, rar3x, rar4x, type = 'text'), paste(directory, 'cement_regression_results_DID.txt'), row.names = FALSE)

write.csv(stargazer(rar1x, rar2x, rar3x, rar4x), paste(directory, 'cement_regression_results_DID_tex.txt'), row.names = FALSE)

# Double interaction model

xrar1 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.Rat*Post2004 + Kyoto.Rat*Emissions.Trading + factor(Year), data = cement)

xrar2 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.Rat*Post2005 + Kyoto.Rat*Emissions.Trading + factor(Year), data = cement)

xrar3 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.Rat*Post2006 + Kyoto.Rat*Emissions.Trading + factor(Year), data = cement)

xrar4 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.Rat*Post2007 + Kyoto.Rat*Emissions.Trading + factor(Year), data = cement)

xrar1x <- coeftest(xrar1, vcov = vcovCL, cluster = ~Country)
xrar2x <- coeftest(xrar2, vcov = vcovCL, cluster = ~Country)
xrar3x <- coeftest(xrar3, vcov = vcovCL, cluster = ~Country)
xrar4x <- coeftest(xrar4, vcov = vcovCL, cluster = ~Country)

stargazer(xrar1x, xrar2x, xrar3x, xrar4x, type = 'text')

write.csv(stargazer(xrar1x, xrar2x, xrar3x, xrar4x, type = 'text'), paste(directory, 'cement_regression_results_DID_2.txt'), row.names = FALSE)

write.csv(stargazer(xrar1x, xrar2x, xrar3x, xrar4x), paste(directory, 'cement_regression_results_DID_2_tex.txt'), row.names = FALSE)

