# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

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

ar1 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Lagged.R.D, data = cement)

ar3 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading, data = cement)

ar5 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
          + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ar7 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
          + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
          + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ar9 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
         + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
         + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading, data = cement)

ar11 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + KP, data = cement)

ar13 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar15 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar17 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar19 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ar21 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar23 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar25 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading
           + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ar27 <- lm(log(Cement) ~ log(Lagged.Cement) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

# Viewing results

stargazer(ar1, ar3, type = 'text') # No KP effects
stargazer(ar5, ar7, type = 'text') # KP effects by phase
stargazer(ar9, ar11, type = 'text') # General KP effects
stargazer(ar13, ar15, type = 'text') # KP effects by phase with 5YPs
stargazer(ar17, ar19, type = 'text') # General KP effects with 5YPs
stargazer(ar21, ar23, type = 'text') # KP effects by phase with 5YPs with year FE
stargazer(ar25, ar27, type = 'text') # General KP effects with 5YPs with year FE

# Writing results to file

write.csv(stargazer(ar1, ar3, ar5, ar7, ar13, ar15, ar21, ar23, type = 'text'),
          paste(directory, 'cement_regression_results_phases.txt'), row.names = FALSE)
write.csv(stargazer(ar1, ar3, ar5, ar7, ar13, ar15, ar21, ar23),
          paste(directory, 'cement_regression_results_phases_tex.txt'), row.names = FALSE)
write.csv(stargazer(ar1, ar3, ar9, ar11, ar17, ar19, ar25, ar27, type = 'text'),
          paste(directory, 'cement_regression_results_all.txt'), row.names = FALSE)
write.csv(stargazer(ar1, ar3, ar9, ar11, ar17, ar19, ar25, ar27),
          paste(directory, 'cement_regression_results_all_tex.txt'), row.names = FALSE)

