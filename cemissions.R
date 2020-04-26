# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on CO2 emissions from cement manufacturing

# Loading libraries

library(stargazer)
library(ggplot2)


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
plot(cement$CO2.Emissions.from.M.C, log(cement$Cement.Emissions))

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

# Running autoregressive models

ear1 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D, data = cement)

ear2 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C), data = cement)

ear3 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading, data = cement)

ear4 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C), data = cement)

ear5 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ear6 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I
           + Kyoto.I.Rat.Phase.II + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ear7 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ear8 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ear9 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading, data = cement)

ear10 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + KP + KP*Emissions.Trading, data = cement)

ear11 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + KP, data = cement)

ear12 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + KP, data = cement)

ear13 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear14 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I
            + Kyoto.I.Rat.Phase.II + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear15 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear16 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear17 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear18 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + KP + KP*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear19 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear20 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + KP
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13, data = cement)

ear21 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear22 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I
            + Kyoto.I.Rat.Phase.II + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear23 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear24 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear25 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear26 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + log(Lagged.CO2.Emissions.from.M.C) + KP + KP*Emissions.Trading
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear27 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

ear28 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports)
            + Forest.Rents + Tariff.Rate + Lagged.R.D + log(Lagged.CO2.Emissions.from.M.C) + KP
            + FYP8 + FYP9 + FYP10 + FYP12 + FYP13 + FYP13 + factor(Year), data = cement)

# Viewing results

stargazer(ear1, ear2, ear3, ear4, type = 'text') # No KP effects
stargazer(ear5, ear6, ear7, ear8, type = 'text') # KP effects by phase
stargazer(ear9, ear10, ear11, ear12, type = 'text') # General KP effects
stargazer(ear13, ear14, ear15, ear16, type = 'text') # KP effects by phase with 5YPs
stargazer(ear17, ear18, ear19, ear20, type = 'text') # General KP effects with 5YPs
stargazer(ear21, ear22, ear23, ear24, type = 'text') # KP effects by phase with 5YPs with year FE
stargazer(ear25, ear26, ear27, ear28, type = 'text') # General KP effects with 5YPs with year FE

# Writing results to file

write.csv(stargazer(ear1, ear2, ear3, ear4, type = 'text'),
          paste(directory, 'emissions_regression_results_1_no_KP.txt'), row.names = FALSE)
write.csv(stargazer(ear1, ear2, ear3, ear4),
          paste(directory, 'emissions_regression_results_1_no_KP_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear5, ear6, ear7, ear8, type = 'text'),
          paste(directory, 'emissions_regression_results_2_KP_effects_by_phase.txt'), row.names = FALSE)
write.csv(stargazer(ear5, ear6, ear7, ear8),
          paste(directory, 'emissions_regression_results_2_KP_effects_by_phase_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear9, ear10, ear11, ear12, type = 'text'),
          paste(directory, 'emissions_regression_results_3_KP_general.txt'), row.names = FALSE)
write.csv(stargazer(ear9, ear10, ear11, ear12),
          paste(directory, 'emissions_regression_results_3_KP_general_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear13, ear14, ear15, ear16, type = 'text'),
          paste(directory, 'emissions_regression_results_4_KP_effects_by_phase_5YP.txt'), row.names = FALSE)
write.csv(stargazer(ear13, ear14, ear15, ear16),
          paste(directory, 'emissions_regression_results_4_KP_effects_by_phase_5YP_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear17, ear18, ear19, ear20, type = 'text'),
          paste(directory, 'emissions_regression_results_5_KP_effects_by_phase_5YP.txt'), row.names = FALSE)
write.csv(stargazer(ear17, ear18, ear19, ear20),
          paste(directory, 'emissions_regression_results_5_KP_effects_by_phase_5YP_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear21, ear22, ear23, ear24, type = 'text'),
          paste(directory, 'emissions_regression_results_6_KP_general_5YP.txt'), row.names = FALSE)
write.csv(stargazer(ear21, ear22, ear23, ear24),
          paste(directory, 'emissions_regression_results_6_KP_general_5YP_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear25, ear26, ear27, ear28, type = 'text'),
          paste(directory, 'emissions_regression_results_7_KP_general_5YP_FE.txt'), row.names = FALSE)
write.csv(stargazer(ear25, ear26, ear27, ear28),
          paste(directory, 'emissions_regression_results_7_KP_general_5YP_FE_tex.txt'), row.names = FALSE)

