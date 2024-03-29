# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

library(sandwich)
library(stargazer)
library(ggplot2)
library(lmtest)

# Reading in the panel data

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')

# Subsetting the data in the same fashion as the previous two scripts

cement <- cement[which(abs(cement$Real.Interest.Rate) < 50),]

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
  china_int[[i-1997]] <- d[which(d$Country == 'China'),63]
  us_int[[i-1997]] <- d[which(d$Country == 'United States'),63]
  yrs[[i-1997]] <- i
  
}

Cint.df <- as.data.frame(cbind(unlist(yrs), unlist(c_int), unlist(us_int), unlist(china_int)))
names(Cint.df) <- c('Year', 'World', 'US', 'China')

# First plot

ggplot(data = Cint.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('CO2 Emissions from Global Cement Production by Year') +
  ylab('CO2 Emissions (million metric tons of C)') +
  geom_line(aes(y = World, col = 'Global Average'), size = 2, alpha = 1) +
  geom_line(aes(y = US, col = 'U.S.'), size = 2, alpha = 1) +
  geom_line(aes(y = China, col = 'China'), size = 2, alpha = 1) +
  theme(legend.position = c(0.2,0.2), plot.title = element_text(hjust = 0.5)) +
  ylim(0.03695*3.6667,0.03715*3.6667) + theme(legend.title = element_blank())

dev.copy(png, paste(directory, 'cementensity_fig_1.png'))
dev.off()

# Second plot -- first plot with markers for Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II

ggplot(data = Cint.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('Carbon Intensity for Global Cement Production by Year') +
  ylab('Carbon Intensity (kg of C per kg of Cement)') +
  geom_line(aes(y = World, col = 'Global Average'), size = 2, alpha = 1) +
  geom_line(aes(y = US, col = 'U.S.'), size = 2, alpha = 1) +
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
  geom_line(aes(y = US, col = 'U.S.'), size = 2, alpha = 1) +
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

Cint.df <- as.data.frame(cbind(unlist(yrs), unlist(hi), unlist(li)))
colnames(Cint.df) <- c('Year', 'High', 'Low')

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

# Seventh plot -- intensities over time by treated status

treated.int <- c()
control.int <- c()
years.int <- c()

for (i in 1990:2014) {
  
  tmp <- cement[which(cement$Year == i),]
  tmp.t <- tmp[which(tmp$Kyoto.Rat == 1),]
  tmp.c <- tmp[which(tmp$Kyoto.Rat == 0),]
  treated.int <- c(treated.int, sum(tmp.t$Intensity, na.rm = TRUE) / dim(tmp.t)[1])
  control.int <- c(control.int, sum(tmp.c$Intensity, na.rm = TRUE) / dim(tmp.c)[1])
  years.int <- c(years.int, i)
  
}

intdf <- data.frame(Year = c(years.int), KP = c(treated.int), C = c(control.int))

ggplot(data = intdf, aes(x = Year, y = value, color = variable)) + 
  ggtitle('Carbon Intensity of Cement Production by Kyoto Protocol Status') +
  ylab('Carbon Intensity of Cement Production') +
  geom_line(aes(y = KP , col = 'Kyoto Protocol'), size = 2, alpha = 1) +
  geom_line(aes(y = C , col = 'Non-KP'), size = 2, alpha = 1) +
  theme(legend.position = c(.15,.85), plot.title = element_text(hjust = 0.5)) +
  ylim(0.130,0.180) + scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept = 2008) +
  geom_vline(xintercept = 2013)

# We remove anything above 99.5 percentile or below 0.5 percentile as outliers for regressions

upper <- quantile(cement$Intensity, c(.995), na.rm = TRUE)
lower <- quantile(cement$Intensity, c(.005), na.rm = TRUE)
cement <- cement[which(cement$Intensity <= upper & cement$Intensity >= lower),]

# Plotting data

hist(cement$Intensity)
plot(cement$Year,cement$Intensity)

# Running Carbon intensity regressions for cement manufacturing

j1 <- ivreg(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + CO2.Change
            + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
            + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year) + factor(Country), data = cement)

j2 <- ivreg(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + CO2.Change
            + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
            + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year) + factor(Country), data = cement)

j3 <- ivreg(Intensity ~ Lagged.Intensity + log(GDP.per.capita)*Kyoto.I.Rat.Phase.I + log(GDP.per.capita)*Kyoto.I.Rat.Phase.II + I(log(GDP.per.capita)^2) + log(Population) + CO2.Change
            + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
            + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year) + factor(Country), data = cement)

j4 <- ivreg(Intensity ~ Lagged.Intensity + log(GDP.per.capita)*Kyoto.Rat + I(log(GDP.per.capita)^2) + log(Population) + CO2.Change
            + Urban.Population + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output + log(Ores.and.Metals.Exports)
            + log(Ores.and.Metals.Imports) + Polity.Index + Forest.Rents + Tariff.Rate + Emissions.Trading + factor(Year) + factor(Country), data = cement)

# Calculating robust standard errors

j1x <- coeftest(j1, vcov = vcovCL, cluster = ~Kyoto.I.Rat.Phase.I)
j2x <- coeftest(j2, vcov = vcovCL, cluster = ~Kyoto.I.Rat.Phase.I)
j3x <- coeftest(j3, vcov = vcovCL, cluster = ~Kyoto.I.Rat.Phase.I)
j4x <- coeftest(j4, vcov = vcovCL, cluster = ~Kyoto.I.Rat.Phase.I)

# Viewing results

stargazer(j1x, j2x, j3x, j4x, type = 'text', omit = c('Country'))

# Writing main results to file

write.csv(stargazer(j1x, j2x, j3x, j4x, type = 'text'),
          paste(directory, 'intensity_regression_results.txt'), row.names = FALSE)
write.csv(stargazer(j1x, j2x, j3x, j4x),
          paste(directory, 'intensity_regression_results_tex.txt'), row.names = FALSE)
