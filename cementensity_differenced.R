# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

library(stargazer)
library(ggplot2)

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

# Running Carbon intensity regressions for cement manufacturing

i1 <- lm(Differenced ~ log(GDP.per.capita) + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output, data = cement)

i2 <- lm(Differenced ~ log(GDP.per.capita) + KP + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output, data = cement)

i3 <- lm(Differenced ~ log(GDP.per.capita)*KP + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output, data = cement)

i4 <- lm(Differenced ~ log(GDP.per.capita) + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output
         + factor(Year), data = cement)

i5 <- lm(Differenced ~ log(GDP.per.capita) + KP + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output
         + factor(Year), data = cement)

i6 <- lm(Differenced ~ log(GDP.per.capita)*KP + Lagged.R.D + Emissions.Trading + Real.Interest.Rate + Renewable.Electricity.Output
         + factor(Year), data = cement)

# Viewing results

stargazer(i1, i2, i3, i4, i5, i6, type = 'text')

# Writing results to file

write.csv(stargazer(i1, i2, i3, i4, i5, i6, type = 'text'),
          paste(directory, 'intensity_differenced_regression_results.txt'), row.names = FALSE)
write.csv(stargazer(i1, i2, i3, i4, i5, i6),
          paste(directory, 'intensity_differenced_regression_results_tex.txt'), row.names = FALSE)

