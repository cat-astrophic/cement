# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

library(stargazer)
library(ggplot2)

# Reading in the panel data

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')

# Subsetting the data in the same fashion as the previous two scripts

cement <- cement[which(abs(cement$Real.Interest.Rate) < 50),]

# Plotting data

hist(cement$Intensity)
plot(cement$Year,cement$Intensity)

# We see that there are some extreme outliers in the above plot so we remove them -- anything above 99.5 percentile

cutoff <- quantile(cement$Intensity, c(.995), na.rm = TRUE)
cement <- cement[which(cement$Intensity <= cutoff),]

# Plotting data

hist(cement$Intensity)
plot(cement$Year,cement$Intensity)

# Running Carbon intensity regressions for cement manufacturing

i1 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output, data = cement)

i2 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + KP + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output, data = cement)

i3 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita)*KP + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output, data = cement)

i4 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output
         + factor(Year), data = cement)

i5 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + KP + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output
         + factor(Year), data = cement)

i6 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita)*KP + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output
         + factor(Year), data = cement)

ii1 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output, data = cement)

ii2 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + KP + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output, data = cement)

ii3 <- lm(Intensity ~ Lagged.Intensity*KP + log(GDP.per.capita) + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output, data = cement)

ii4 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output
         + factor(Year), data = cement)

ii5 <- lm(Intensity ~ Lagged.Intensity + log(GDP.per.capita) + KP + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output
          + factor(Year), data = cement)

ii6 <- lm(Intensity ~ Lagged.Intensity*KP + log(GDP.per.capita) + Lagged.R.D + Real.Interest.Rate + Renewable.Electricity.Output
          + factor(Year), data = cement)

# Viewing results

stargazer(i1, i2, i3, i4, i5, i6, type = 'text')
stargazer(ii1, ii2, ii3, ii4, ii5, ii6, type = 'text')

# Writing results to file

write.csv(stargazer(i1, i2, i3, i4, i5, i6, type = 'text'),
          paste(directory, 'intensity_regression_results.txt'), row.names = FALSE)
write.csv(stargazer(i1, i2, i3, i4, i5, i6),
          paste(directory, 'intensity_regression_results_tex.txt'), row.names = FALSE)
write.csv(stargazer(ii1, ii2, ii3, ii4, ii5, ii6, type = 'text'),
          paste(directory, 'intensity_regression_results_2.txt'), row.names = FALSE)
write.csv(stargazer(ii1, ii2, ii3, ii4, ii5, ii6),
          paste(directory, 'intensity_regression_results_2_tex.txt'), row.names = FALSE)

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
  china_int[[i-1997]] <- d[which(d$Country == 'China'),58] 
  us_int[[i-1997]] <- d[which(d$Country == 'United States'),58]
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

# Second plot -- first plot with markers for KP

ggplot(data = Cint.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('Carbon Intensity for Global Cement Production by Year\nwith Kyoto Protocol Indicators') +
  ylab('Carbon Intensity (kg of C per kg of Cement)') +
  geom_line(aes(y = World, col = 'Global Average'), size = 2, alpha = 1) +
  geom_line(aes(y = US, col = 'USA'), size = 2, alpha = 1) +
  geom_line(aes(y = China, col = 'China'), size = 2, alpha = 1) +
  theme(legend.position = c(0.2,0.2), plot.title = element_text(hjust = 0.5)) +
  ylim(0.03695*3.6667,0.03715*3.6667) + theme(legend.title = element_blank()) +
  geom_vline(xintercept = 2008) + geom_vline(xintercept = 2013)

dev.copy(png, paste(directory, 'cementensity_fig_2.png'))
dev.off()

# Third plot -- carbon intensities 10 years prior to KP

ggplot(data = Cint.df[which(Cint.df$Year < 2008),], aes(x = Year, y = value, color = variable)) +
  ggtitle('Carbon Intensity for Global Cement Production by Year') +
  ylab('Carbon Intensity (kg of C per kg of Cement)') +
  geom_line(aes(y = World, col = 'Global Average'), size = 2, alpha = 1) +
  geom_line(aes(y = US, col = 'USA'), size = 2, alpha = 1) +
  geom_line(aes(y = China, col = 'China'), size = 2, alpha = 1) +
  theme(legend.position = c(0.2,0.2), plot.title = element_text(hjust = 0.5)) +
  ylim(0.03695*3.6667,0.03715*3.6667) + theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))

dev.copy(png, paste(directory, 'introfig.png'))
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

