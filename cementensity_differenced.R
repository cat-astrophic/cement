# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

library(sandwich)
library(stargazer)
library(ggplot2)
library(lmtest)

# Reading in the panel data

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')

# Subsetting the data in the same fashion as the previous scripts

cement <- cement[which(abs(cement$Real.Interest.Rate) < 50),]
cement <- cement[which(cement$Intensity > 0 & cement$Lagged.Intensity > 0),]
cement$Differenced <- 100 * (cement$Intensity - cement$Lagged.Intensity) / cement$Lagged.Intensity

# Removing outliers since this is percentage based

lower.cutoff <- quantile(cement$Differenced, c(.005), na.rm = TRUE)
upper.cutoff <- quantile(cement$Differenced, c(.995), na.rm = TRUE)
cement <- cement[which(cement$Differenced <= upper.cutoff & cement$Differenced >= lower.cutoff),]

# Plotting data

hist(cement$Differenced)
plot(cement$Year,cement$Differenced)

# Post variables

cement$Post2004 <- as.numeric(cement$Year > 2004)
cement$Post2005 <- as.numeric(cement$Year > 2005)
cement$Post2006 <- as.numeric(cement$Year > 2006)
cement$Post2007 <- as.numeric(cement$Year > 2007)

# Running Carbon intensity regressions for cement manufacturing

i1 <- lm(Differenced ~ log(GDP.per.capita) + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output, data = cement)

i2 <- lm(Differenced ~ log(GDP.per.capita) + KP + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output, data = cement)

i3 <- lm(Differenced ~ log(GDP.per.capita)*KP + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output, data = cement)

i4 <- lm(Differenced ~ log(GDP.per.capita) + Kyoto.Rat*Post2007 + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output, data = cement)

i5 <- lm(Differenced ~ log(GDP.per.capita) + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output
         + factor(Year), data = cement)

i6 <- lm(Differenced ~ log(GDP.per.capita) + KP + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output
         + factor(Year), data = cement)

i7 <- lm(Differenced ~ log(GDP.per.capita)*KP + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output
         + factor(Year), data = cement)

i8 <- lm(Differenced ~ log(GDP.per.capita) + Kyoto.Rat*Post2007 + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output
         + factor(Year), data = cement)

# Calculating robust standard errors

i1x <- coeftest(i1, vcov = vcovCL, cluster = ~Country)
i2x <- coeftest(i2, vcov = vcovCL, cluster = ~Country)
i3x <- coeftest(i3, vcov = vcovCL, cluster = ~Country)
i4x <- coeftest(i4, vcov = vcovCL, cluster = ~Country)
i5x <- coeftest(i5, vcov = vcovCL, cluster = ~Country)
i6x <- coeftest(i6, vcov = vcovCL, cluster = ~Country)
i7x <- coeftest(i7, vcov = vcovCL, cluster = ~Country)
i8x <- coeftest(i8, vcov = vcovCL, cluster = ~Country)

# Viewing results

stargazer(i1x, i2x, i3x, i4x, i5x, i6x, i7x, i8x, type = 'text')

# Writing results to file

write.csv(stargazer(i1x, i2x, i3x, i4x, i5x, i6x, i7x, i8x, type = 'text'),
          paste(directory, 'intensity_differenced_regression_results.txt'), row.names = FALSE)
write.csv(stargazer(i1x, i2x, i3x, i4x, i5x, i6x, i7x, i8x),
          paste(directory, 'intensity_differenced_regression_results_tex.txt'), row.names = FALSE)

