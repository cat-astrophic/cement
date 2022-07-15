# This script performs the data analysis for a paper on the effect of the Kyoto Protocol on cement manufacturing (CARBON LEAKGE)

# NOTE: For the IV regressions, the instrument was ICC membership via 

# Loading libraries

library(sandwich)
library(stargazer)
library(ggplot2)
library(AER)
library(plotly)

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

cement$post2005 <- as.numeric(cement$Year.x > 2005)
cement$post2006 <- as.numeric(cement$Year.x > 2006)
cement$post2007 <- as.numeric(cement$Year.x > 2007)
cement$post2008 <- as.numeric(cement$Year.x > 2008)

# Phase I indicator

cement$post.I <- as.numeric(cement$Year.x > 2008)
# Phase II indicator

cement$post.II <- as.numeric(cement$Year.x > 2012)

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

lmod1 <- ivreg(Ln.Footprint ~ Kyoto.Rat*post2005 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
                + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
                + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x) + factor(Country.x) | . - Kyoto.Rat + ICC, data = cement)

lmod2 <- ivreg(Ln.Footprint ~ Kyoto.Rat*post2006 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
                + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
                + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x) + factor(Country.x) | . - Kyoto.Rat + ICC, data = cement)

lmod3 <- ivreg(Ln.Footprint ~ Kyoto.Rat*post2007 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
                + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
                + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x) + factor(Country.x) | . - Kyoto.Rat + ICC, data = cement)

lmod4 <- ivreg(Ln.Footprint ~ Kyoto.Rat*post2008 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
                + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
                + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x) + factor(Country.x) | . - Kyoto.Rat + ICC, data = cement)

# Computing heteroskedasticity robust standard errors

lmod1x <- coeftest(lmod1, vcov = vcovCL, cluster = ~Country.x)
lmod2x <- coeftest(lmod2, vcov = vcovCL, cluster = ~Country.x)
lmod3x <- coeftest(lmod3, vcov = vcovCL, cluster = ~Country.x)
lmod4x <- coeftest(lmod4, vcov = vcovCL, cluster = ~Country.x)

# Viewing the results and writing them to file

stargazer(lmod1x, lmod2x, lmod3x, lmod4x, type = 'text', omit = c('Year.x', 'Country.x'))

write.csv(stargazer(lmod1x, lmod2x, lmod3x, lmod4x, type = 'text'),
          paste(directory, 'leakage_footprint_regression_results.txt'), row.names = FALSE)

write.csv(stargazer(lmod1x, lmod2x, lmod3x, lmod4x),
          paste(directory, 'leakage_footprint_regression_results_tex.txt'), row.names = FALSE)

# Running regressions for net imports

cement$Net.Imports2 <- cement$Net.Imports / 1000

l2mod1 <- ivreg(Net.Imports2 ~ Kyoto.Rat*post2005 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
               + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
               + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x) + factor(Country.x) | . - Kyoto.Rat + ICC, data = cement)

l2mod2 <- ivreg(Net.Imports2 ~ Kyoto.Rat*post2006 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
               + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
               + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x) + factor(Country.x) | . - Kyoto.Rat + ICC, data = cement)

l2mod3 <- ivreg(Net.Imports2 ~ Kyoto.Rat*post2007 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
               + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
               + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x) + factor(Country.x) | . - Kyoto.Rat + ICC, data = cement)

l2mod4 <- ivreg(Net.Imports2 ~ Kyoto.Rat*post2008 + log(GDP.per.capita) + I(log(GDP.per.capita)^2) + log(Population) + Real.Interest.Rate + log(Land.Area)
               + Renewable.Electricity.Output + log(Ores.and.Metals.Imports) + log(Ores.and.Metals.Exports) + Polity.Index
               + Forest.Rents + Tariff.Rate + Lagged.R.D + factor(Year.x) + factor(Country.x) | . - Kyoto.Rat + ICC, data = cement)

# Computing heteroskedasticity robust standard errors

l2mod1x <- coeftest(l2mod1, vcov = vcovCL, cluster = ~Country.x)
l2mod2x <- coeftest(l2mod2, vcov = vcovCL, cluster = ~Country.x)
l2mod3x <- coeftest(l2mod3, vcov = vcovCL, cluster = ~Country.x)
l2mod4x <- coeftest(l2mod4, vcov = vcovCL, cluster = ~Country.x)

# Viewing the results and writing them to file

stargazer(l2mod1x, l2mod2x, l2mod3x, l2mod4x, type = 'text', omit = c('Year.x', 'Country.x'))

write.csv(stargazer(l2mod1x, l2mod2x, l2mod3x, l2mod4x, type = 'text'),
          paste(directory, 'leakage_net_imports_regression_results.txt'), row.names = FALSE)

write.csv(stargazer(l2mod1x, l2mod2x, l2mod3x, l2mod4x),
          paste(directory, 'leakage_net_imports_regression_results_tex.txt'), row.names = FALSE)

# Joint results output

write.csv(stargazer(l2mod1x, l2mod2x, l2mod3x, l2mod4x, lmod1x, lmod2x, lmod3x, lmod4x, type = 'text'),
          paste(directory, 'leakage_all_regression_results.txt'), row.names = FALSE)

write.csv(stargazer(l2mod1x, l2mod2x, l2mod3x, l2mod4x, lmod1x, lmod2x, lmod3x, lmod4x),
          paste(directory, 'leakage_all_regression_results_tex.txt'), row.names = FALSE)

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

# Coefficients for increase in net cement imports due to KP

mu <- 0.910 * 1000

# Coefficient for reduction in cement derived emissions due to KP

eta <- .05

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

# Mean annual national values for KP nations

W.low.1 <- ssc.low * sigma
W.high.1 <- ssc.high * sigma

# Extending this over time

W.low <- W.low.1 * 39 * 13
W.high <- W.high.1 * 39 * 13

# Repeating if carbon leakage is ignored

W.low.noCL.1 <- ssc.low * emissions.decrease
W.high.noCL.1 <- ssc.high * emissions.decrease

W.low.noCL <- W.low.noCL.1 * 39 * 13
W.high.noCL <- W.high.noCL.1 * 39 * 13

# Creating some choropleths for the paper with plotly

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
names(df) <- c('Country', 'Junk', 'Code')

list1 <- c('Brunei Darussalam', 'Congo, Dem. Rep.', 'Hong Kong SAR, China', 'North Macedonia', 'Slovak Republic')
list2 <- c('Brunei', 'Congo, Democratic Republic of the', 'Hong Kong', 'Macedonia', 'Slovakia')

for (i in 1:5) {
  
  idx <- as.numeric(rownames(df[match(list2[i],df$Country),]))
  df$Country[idx] <- list1[i]
  
}

mapdat96 <- subset(cement, Year == 1996)
mapdat96 <- merge(mapdat96, df, by = 'Country')
mapdat96$Cement <- log(mapdat96$Cement)

mapdat16 <- subset(cement, Year == 2016)
mapdat16 <- merge(mapdat16, df, by = 'Country')
mapdat16$Cement <- log(mapdat16$Cement)

tmapdat96 <- subset(tradedata, Year == 1996)
tmapdat96 <- merge(tmapdat96, df, by = 'Country')
tmapdat96$Net.Imports <- -1*log(abs(tmapdat96$Trade.Value.Delta)) * (abs(tmapdat96$Trade.Value.Delta) / tmapdat96$Trade.Value.Delta)

tmapdat16 <- subset(tradedata, Year == 2016)
tmapdat16 <- merge(tmapdat16, df, by = 'Country')
tmapdat16$Net.Imports <- -1*log(abs(tmapdat16$Trade.Value.Delta)) * (abs(tmapdat16$Trade.Value.Delta) / tmapdat16$Trade.Value.Delta)

# Specify map projection/options

l <- list(color = toRGB('grey'), width = 0.5)

geog <- list(showframe = FALSE,
             showcoastlines = FALSE,
             projection = list(type = 'Mercator'))

# Create the choropleth maps for cement production

init_cmap <- plot_geo(mapdat96) %>%
  add_trace(z = ~Cement, color = ~Cement, colors = 'Blues',
            text = ~Country, locations = ~Code, marker = list(line = l)) %>%
  #colorbar(title = '', limits = c(0,max(mapdat96$Cement))) %>%
  hide_colorbar() %>%
  layout(title = 'Initial (1996) Cement Production',
         geo = geog)

fin_cmap <- plot_geo(mapdat16) %>%
  add_trace(z = ~Cement, color = ~Cement, colors = 'Blues',
            text = ~Country, locations = ~Code, marker = list(line = l)) %>%
  #colorbar(title = '', limits = c(0,max(mapdat16$Cement))) %>%
  hide_colorbar() %>%
  layout(title = 'Final (2016) Cement Production',
         geo = geog)

# Create the choropleth maps for net cement imports

init_impmap <- plot_geo(tmapdat96) %>%
  add_trace(z = ~Net.Imports, color = ~Net.Imports, colors = c('Blue', 'Red'),
            text = ~Country, locations = ~Code, marker = list(line = l)) %>%
  colorbar(title = '', limits = c(min(tmapdat96$Net.Imports)),max(tmapdat96$Net.Imports)) %>%
  #hide_colorbar() %>%
  layout(title = 'Initial (1996) Net Cement Imports',
         geo = geog)

fin_impmap <- plot_geo(tmapdat16) %>%
  add_trace(z = ~Net.Imports, color = ~Net.Imports, colors = c('Blue', 'Red'),
            text = ~Country, locations = ~Code, marker = list(line = l)) %>%
  colorbar(title = '', limits = c(min(tmapdat16$Net.Imports)),max(tmapdat16$Net.Imports)) %>%
  #hide_colorbar() %>%
  layout(title = 'Final (2016) Net Cement Imports',
         geo = geog)

