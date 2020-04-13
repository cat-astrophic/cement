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

cutoff <- quantile(xxx, c(.995), na.rm = TRUE)
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

# Viewing results

stargazer(i1, i2, i3, i4, i5, i6, type = 'text')

# Writing results to file

write.csv(stargazer(i1, i2, i3, i4, i5, i6, type = 'text'),
          paste(directory, 'intensity_regression_results.txt'), row.names = FALSE)
write.csv(stargazer(i1, i2, i3, i4, i5, i6),
          paste(directory, 'intensity_regression_results_tex.txt'), row.names = FALSE)

# Creating figures for the paper

# Data prep -- aggregate time series for C intensities

yrs <- c()
c_int <- c()

for (i in 1990:2014) {
  
  d <- cement[which(cement$Year == i),]
  d <- d[which(d$Intensity > 0),]
  d <- d[which(d$Cement > 0),]
  c_int[[i-1989]] <- sum(d$Intensity * d$Cement) / sum(d$Cement)
  yrs[[i-1989]] <- i
  
}

US <- cement[which(cement$Country == 'United States'),]
us_int <- US$Intensity
China <- cement[which(cement$Country == 'China'),]
china_int <- China$Intensity
Cint.df <- data.frame(Year = c(yrs), World = c(c_int), US = c(us_int), China = c(china_int))

# First plot

ggplot(data = Cint.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('CO2 Emissions from Global Cement Production by Year') +
  ylab('CO2 Emissions (million metric tons of C)') +
  geom_line(aes(y = World, col = 'Global Average'), size = 2, alpha = 1) +
  geom_line(aes(y = US, col = 'USA'), size = 2, alpha = 1) +
  geom_line(aes(y = China, col = 'China'), size = 2, alpha = 1) +
  theme(legend.position = c(0.2,0.2), plot.title = element_text(hjust = 0.5)) +
  ylim(0.03695,0.03715) + theme(legend.title = element_blank())

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
  ylim(0.03695,0.03715) + theme(legend.title = element_blank()) +
  geom_vline(xintercept = 2008) + geom_vline(xintercept = 2013)

dev.copy(png, paste(directory, 'cementensity_fig_2.png'))
dev.off()

