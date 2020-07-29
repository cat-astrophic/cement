# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on CO2 emissions from cement manufacturing

# Loading libraries

library(sandwich)
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

ear1 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D, data = cement)

ear3 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading, data = cement)

ear5 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
           + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ear7 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ear9 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + KP, data = cement)

ear11 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
           + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
           + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading, data = cement)

ear13 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading, data = cement)

ear15 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II, data = cement)

ear17 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year), data = cement)

ear19 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + factor(Year), data = cement)

ear21 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II
            + Kyoto.I.Rat.Phase.I*Emissions.Trading + Kyoto.I.Rat.Phase.II*Emissions.Trading + factor(Year), data = cement)

ear23 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Kyoto.I.Rat.Phase.I + Kyoto.I.Rat.Phase.II + factor(Year), data = cement)

ear25 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + factor(Year), data = cement)

ear27 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading + factor(Year), data = cement)

ear29 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Country), data = cement)

ear31 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + factor(Country), data = cement)

ear33 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + factor (Country), data = cement)

ear35 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading + factor(Country), data = cement)

ear37 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year) + factor(Country), data = cement)

ear39 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + factor(Year) + factor(Country), data = cement)

ear41 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + KP + factor(Year) + factor (Country), data = cement)

ear43 <- lm(log(Cement.Emissions) ~ log(Lagged.Cement.Emissions) + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area) + CO2.Change
            + Urban.Population + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D + Emissions.Trading + KP + KP*Emissions.Trading + factor(Year) + factor(Country), data = cement)

# Calculating robust standard errors

cov1 <- vcovHC(ear1, type = 'HC1')
rse1 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear3, type = 'HC1')
rse3 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear5, type = 'HC1')
rse5 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear7, type = 'HC1')
rse7 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear9, type = 'HC1')
rse9 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear11, type = 'HC1')
rse11 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear13, type = 'HC1')
rse13 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear15, type = 'HC1')
rse15 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear17, type = 'HC1')
rse17 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear19, type = 'HC1')
rse19 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear21, type = 'HC1')
rse21 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear23, type = 'HC1')
rse23 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear25, type = 'HC1')
rse25 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear27, type = 'HC1')
rse27 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear29, type = 'HC1')
rse29 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear31, type = 'HC1')
rse31 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear33, type = 'HC1')
rse33 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear35, type = 'HC1')
rse35 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear37, type = 'HC1')
rse37 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear39, type = 'HC1')
rse39 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear41, type = 'HC1')
rse41 <- sqrt(diag(cov1))

cov1 <- vcovHC(ear43, type = 'HC1')
rse43 <- sqrt(diag(cov1))

# Viewing results

stargazer(ear1, ear3, type = 'text', se = list(rse1, rse3)) # No KP effects
stargazer(ear5, ear7, type = 'text', se = list(rse5, rse7)) # KP effects by phase
stargazer(ear9, ear11, type = 'text', se = list(rse9, rse11)) # General KP effects
stargazer(ear13, ear15, type = 'text', se = list(rse13, rse15)) # KP effects by phase
stargazer(ear17, ear19, type = 'text', se = list(rse17, rse19)) # No KP effects with year FE
stargazer(ear21, ear23, type = 'text', se = list(rse21, rse23)) # KP effects by phase with year FE
stargazer(ear25, ear27, type = 'text', se = list(rse25, rse27)) # General KP effects with year FE
stargazer(ear29, ear31, type = 'text', se = list(rse29, rse31)) # No KP effects with country FE
stargazer(ear33, ear35, type = 'text', se = list(rse33, rse35)) # General KP effects with country FE
stargazer(ear37, ear39, type = 'text', se = list(rse37, rse39)) # No KP effects with year and country FE
stargazer(ear41, ear43, type = 'text', se = list(rse41, rse43)) # General KP effects with year and country FE

# Writing results to file

write.csv(stargazer(ear1, ear3, ear5, ear7, ear13, ear15, ear21, ear23, type = 'text',
                    se = list(rse1, rse3, rse5, rse7, rse13, rse15, rse21, rse23)),
          paste(directory, 'emissions_regression_results_phases.txt'), row.names = FALSE)
write.csv(stargazer(ear1, ear3, ear5, ear7, ear13, ear15, ear21, ear23,
                    se = list(rse1, rse3, rse5, rse7, rse13, rse15, rse21, rse23)),
          paste(directory, 'emissions_regression_results_phases_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear1, ear3, ear9, ear11, ear17, ear19, ear25, ear27, type = 'text',
                    se = list(rse1, rse3, rse9, rse11, rse17, rse19, rse25, rse27)),
          paste(directory, 'emissions_regression_results_all.txt'), row.names = FALSE)
write.csv(stargazer(ear1, ear3, ear9, ear11, ear17, ear19, ear25, ear27,
                    se = list(rse1, rse3, rse9, rse11, rse17, rse19, rse25, rse27)),
          paste(directory, 'emissions_regression_results_all_tex.txt'), row.names = FALSE)
write.csv(stargazer(ear29, ear31, ear33, ear35, ear37, ear39, ear41, ear43, type = 'text',
                    se = list(rse29, rse31, rse33, rse35, rse37, rse39, rse41, rse43)),
          paste(directory, 'emissions_regression_results_all_robcheck.txt'), row.names = FALSE)
write.csv(stargazer(ear29, ear31, ear33, ear35, ear37, ear39, ear41, ear43,
                    se = list(rse29, rse31, rse33, rse35, rse37, rse39, rse41, rse43)),
          paste(directory, 'emissions_regression_results_all_robcheck_tex.txt'), row.names = FALSE)

