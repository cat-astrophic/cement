# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing (CARBON LEAKGE)

# NOTE: For the IV regressions, the instrument was ICC membership via 

# Loading libraries

library(sandwich)
library(stargazer)
library(ggplot2)
library(AER)

# Reading in the panel data

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')

# Reading in the net imports/exports data for each year

tradedata <- data.frame()

for (y in 1996:2019) {
  
  if (y != 2013) {
    
    td <- read.csv(paste(tradedir, 'Net-Trade-', y, '.csv', sep = ''), fileEncoding = 'UTF-8-BOM')
    yr <- rep(y,dim(td)[1])
    td$Year <- yr
    tradedata <- rbind(tradedata, td)
    
  }
  
}

# Matching name formats between data sets

reflist <- c('Democratic Republic of the Congo', 'Republic of the Congo',
             'Brunei', 'Hong Kong', 'South Korea', 'Macau', 'Burma',
             'North Korea', 'Czechia', 'Slovakia')

updatelist <- c('Congo, Dem. Rep.', 'Congo, Rep.', 'Brunei Darussalam',
                'Hong Kong SAR, China', 'Korea, South', 'Macao SAR, China',
                'Myanmar', 'Korea, North', 'Czech Republic', 'Slovak Republic')

for (i in 1:length(reflist)) {
  
  tradedata[tradedata == reflist[i]] <- updatelist[i]
  
}

# Creating an ID column for each data frame

tradedata$concat <- paste(tradedata$Country, tradedata$Year, sep = '')
cement$concat <- paste(cement$Country, cement$Year, sep = '')

# Merging the data

cement <- merge.data.frame(cement, tradedata, by.x = 'concat', by.y = 'concat')

# Creating the post variables

# First set for general KP with variation in the initial year

cement$post2004 <- as.numeric(cement$Year.x > 2004)
cement$post2005 <- as.numeric(cement$Year.x > 2005)
cement$post2006 <- as.numeric(cement$Year.x > 2006)
cement$post2007 <- as.numeric(cement$Year.x > 2007)

# Phase I indicator

cement$post.I <- as.numeric(cement$Year.x > 2007)

# Phase II indicator

cement$post.II <- as.numeric(cement$Year.x > 2011)

# Creating the price data

prices <- c(70,74.5,75,78,77.5,79,77,76,85,84,98,102,107,100,92,91,95,91,98.5,105.5,111,113,126.5,123.5,124)
years <- 1996:2020
values <- c()

for (i in 1:dim(cement)[1]) {
  
  values <- c(values,prices[which(years %in% cement$Year.x[i])])
  
}

cement$Price <- values

# Computing net export quantities

cement$NX.Cement <- (cement$Trade.Value.Delta / cement$Price)/1000

# Computing total emissions from cement consumption

cement$Footprint <- (-1*cement$NX.Cement * cement$Intensity) + cement$Cement.Emissions # includes a unit conversion

# Calculating total cement consumption

cement$Consumption <- cement$Cement + cement$NX.Cement

# Subset to remove extreme emissions intensities

cement <- cement[which(cement$Intensity < 1),]

# Plotting a quick comparison between Cement Carbon Emissions and Cement Carbon Footprint

par(mfrow = c(2,1))
plot(cement$Year.x, cement$Footprint, col = 'black')
plot(cement$Year.x, cement$Cement.Emissions, col = 'blue')

# Creating the dependent variables (ln footprint, -nx)

cement$Ln.Footprint <- log(cement$Footprint)
cement$Net.Imports <- -1*cement$NX.Cement
cement$Ln.Net.Imports <- log(cement$Net.Imports)
cement$Ln.Cement <- log(cement$Cement)
cement$Ln.Cemissions <- log(cement$Cement.Emissions)

# Running regressions for footprint

lmod1 <- lm(Ln.Footprint ~ log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D, data = cement)

lmod2 <- lm(Ln.Footprint ~ Kyoto.Rat*post2007 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D, data = cement)

lmod3 <- ivreg(Ln.Footprint ~ Kyoto.Rat*post2007 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
               + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
               + Forest.Rents + Tariff.Rate + Lagged.R.D | . - Kyoto.Rat + ICC, data = cement)

lmod11 <- lm(Ln.Footprint ~ log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
             + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
             + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x), data = cement)

lmod12 <- lm(Ln.Footprint ~ Kyoto.Rat*post2007 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
             + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
             + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x), data = cement)

lmod13 <- ivreg(Ln.Footprint ~ Kyoto.Rat*post2007 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
                + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
                + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x) | . - Kyoto.Rat + ICC, data = cement)

# Computing heteroskedasticity robust standard errors

c1 <- vcovHC(lmod1, type = 'HC1')
rse1 <- sqrt(diag(c1))

c2 <- vcovHC(lmod2, type = 'HC1')
rse2 <- sqrt(diag(c2))

c3 <- vcovHC(lmod3, type = 'HC1')
rse3 <- sqrt(diag(c3))

c11 <- vcovHC(lmod11, type = 'HC1')
rse11 <- sqrt(diag(c11))

c12 <- vcovHC(lmod12, type = 'HC1')
rse12 <- sqrt(diag(c12))

c13 <- vcovHC(lmod13, type = 'HC1')
rse13 <- sqrt(diag(c13))

# Viewing the results and writing them to file

stargazer(lmod1, lmod2, lmod3, lmod11, lmod12, lmod13, type = 'text',
          se = list(rse1, rse2, rse3, rse11, rse12, rse13), omit.stat = c('f'))

write.csv(stargazer(lmod1, lmod2, lmod11, lmod12, type = 'text',
                    se = list(rse1, rse2, rse11, rse12)),
          paste(directory, 'leakage_footprint_regression_results.txt'), row.names = FALSE)

write.csv(stargazer(lmod1, lmod2, lmod11, lmod12, se = list(rse1, rse2, rse11, rse12)),
          paste(directory, 'leakage_footprint_regression_results_tex.txt'), row.names = FALSE)

# Running regressions for net imports

l2mod1 <- lm(Net.Imports ~ log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D, data = cement)

l2mod2 <- lm(Net.Imports ~ Kyoto.Rat*post2007 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
            + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
            + Forest.Rents + Tariff.Rate + Lagged.R.D, data = cement)

l2mod3 <- ivreg(Net.Imports ~ Kyoto.Rat*post2007 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
               + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
               + Forest.Rents + Tariff.Rate + Lagged.R.D | . - Kyoto.Rat + ICC, data = cement)

l2mod11 <- lm(Net.Imports ~ log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
             + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
             + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x), data = cement)

l2mod12 <- lm(Net.Imports ~ Kyoto.Rat*post2007 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
             + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
             + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x), data = cement)

l2mod13 <- ivreg(Net.Imports ~ Kyoto.Rat*post2007 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
                + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
                + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x) | . - Kyoto.Rat + ICC, data = cement)

# Computing heteroskedasticity robust standard errors

lc1 <- vcovHC(l2mod1, type = 'HC1')
lrse1 <- sqrt(diag(lc1))

lc2 <- vcovHC(l2mod2, type = 'HC1')
lrse2 <- sqrt(diag(lc2))

lc3 <- vcovHC(l2mod3, type = 'HC1')
lrse3 <- sqrt(diag(lc3))

lc11 <- vcovHC(l2mod11, type = 'HC1')
lrse11 <- sqrt(diag(lc11))

lc12 <- vcovHC(l2mod12, type = 'HC1')
lrse12 <- sqrt(diag(lc12))

lc13 <- vcovHC(l2mod13, type = 'HC1')
lrse13 <- sqrt(diag(lc13))

# Viewing the results and writing them to file

stargazer(l2mod1, l2mod2, l2mod3, l2mod11, l2mod12, l2mod13, type = 'text',
          se = list(lrse1, lrse2, lrse3, lrse11, lrse12, lrse13), omit.stat = c('f'))

write.csv(stargazer(l2mod1, l2mod2, l2mod11, l2mod12, type = 'text',
                    se = list(lrse1, lrse2, lrse11, lrse12)),
          paste(directory, 'leakage_net_imports_regression_results.txt'), row.names = FALSE)

write.csv(stargazer(l2mod1, l2mod2, l2mod11, l2mod12, se = list(lrse1, lrse2, lrse11, lrse12)),
          paste(directory, 'leakage_net_imports_regression_results_tex.txt'), row.names = FALSE)

# Creating plots

cement$KPplot <- cement$Kyoto.Rat * cement$Footprint
cement$NKPplot <- (1 - cement$Kyoto.Rat) * cement$Footprint

yrs <- c()
kpdat <- c()
nkpdat <- c()

for (i in 1996:2014) {
  
  if (i != 2013) {
    
    d <- cement[which(cement$Year.x == i),]
    kpdat[i-1995] <- sum(d$KPplot, na.rm = TRUE)
    nkpdat[i-1995] <-sum(d$NKPplot, na.rm = TRUE)
    yrs[i-1995] <- i
    
  }
  
}

kp.df <- data.frame(Year = c(yrs), KP = c(kpdat), Non.KP = c(nkpdat))

# First plot

ggplot(data = kp.df, aes(x = Year, y = value, color = variable)) +
  ggtitle('Carbon Footprint for Cement') +
  ylab('Emissions') +
  geom_line(aes(y = kpdat, col = 'KP'), size = 2, alpha = 1) +
  geom_line(aes(y = nkpdat, col = 'Non-KP'), size = 2, alpha = 1) +
  geom_vline(xintercept = 2007) + geom_vline(xintercept = 2011) +
  theme(legend.position = c(0.18,0.79), plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank())

dev.copy(png, paste(directory, 'NX.png'))
dev.off()

# Computing the net effects of the carbon leakage

# Coefficients for increase in net cement imports due to KP (with a conversion to tons of cement)

mu <- 1043 * 1000

# Coefficient for reduction in cement derived emissions due to KP

eta <- .10

# Mean value of cement derived emissions post KP for KP nations

kpdatums <- cement[which(cement$Kyoto.Rat == 1),]
kpdatums <- kpdatums[which(kpdatums$post2007 == 1),]
omicron <- mean(kpdatums$Cement.Emissions) * 1000 # This converts to tons of emissions

# Computing the mean carbon intensities from cement production during the KP for KP nations

iota <- sum(kpdatums$Intensity * kpdatums$Cement) / sum(kpdatums$Cement)

# Computing foregone emissions from cement manufacturing due to the KP

emissions.decrease <- eta * omicron

# Computing the net impact on cement manufacturing derived emissions due to the KP

sigma <- emissions.decrease - (mu*iota)

# Social cost of carbon parameters from Nordhaus (2017) for year 2020

ssc.low <- 22.6
ssc.high <- 200

# Computing the potential gains due to the KP

W.low <- ssc.low * sigma
W.high <- ssc.high * sigma

# Repeating if carbon leakage is ignored

W.low.noCL <- ssc.low * emissions.decrease
W.high.noCL <- ssc.high * emissions.decrease

