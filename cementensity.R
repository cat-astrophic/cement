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

