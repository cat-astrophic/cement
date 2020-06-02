# This script performs the carbon intensity simualtion portion of a paper on the effect of the Kyoto Protocol on cement manufacturing

# Loading libraries

library(sandwich)
library(stargazer)
library(ggplot2)

# Reading in the panel data

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')

# Subset data to pre-KP

cement <- cement[which(abs(cement$Real.Interest.Rate) < 50),]
cement <- cement[which(cement$Year < 2008),]

# Get count of low income nations by year

lows <- c()
low.inc <- cement[which(cement$GDP.per.capita < 30000),]
low.inc <- low.inc[complete.cases(low.inc[, 59]),]

for (yr in 1990:2007) {
  
  lows <- rbind(lows, dim(unique(low.inc[which(low.inc$Year == yr),]))[1])
  
}

# We see that we nearly triple the number of low income nations over this time period

# Create new dataframe with linearly extrapolated carbon intensities

nats <- unique(low.inc$Country)

sim.nats <- c()
sim.yrs <- c()
sim.vals <- c()
sim.gdp <- c()
sim.cem <- c()

for (nat in nats) {
  
  temp.df <- cement[which(cement$Country == nat),]

  mod <- lm(temp.df$Intensity ~ temp.df$Year)
  income <- lm(temp.df$GDP.per.capita ~ temp.df$Year)
  ccc <- lm(temp.df$Cement ~ temp.df$Year)
  
  for (yr in 1990:2007) {
      
    intensity <- max(0,summary(mod)$coefficients[1,1] + summary(mod)$coefficients[2,1]*yr)
    gdp <- max(0,summary(income)$coefficients[1,1]  + summary(income)$coefficients[2,1]*yr)
    cem <- max(0,summary(ccc)$coefficients[1,1] + summary(ccc)$coefficients[2,1]*yr)
               
    sim.nats <- rbind(sim.nats,nat)
    sim.yrs <- rbind(sim.yrs,yr)
    sim.vals <- rbind(sim.vals,intensity)
    sim.gdp <- rbind(sim.gdp,gdp)
    sim.cem <- rbind(sim.cem,cem)
    
  }
    
}

sim.df <- data.frame(Country = c(sim.nats), Year = c(sim.yrs), Intensity = c(sim.vals), Cement = c(sim.cem), GDP = c(sim.gdp))

# Remove instances where simulated gdp exceeds the threshold

sim.df <- sim.df[which(sim.df$GDP < 30000),]

# Recreating the high income baseline from the previous script

cement <- read.csv(filepath, fileEncoding = 'UTF-8-BOM')

# Subsetting the data in the same fashion as the previous two scripts

cement <- cement[which(abs(cement$Real.Interest.Rate) < 50),]

# We see that there are some extreme outliers in the above plot so we remove them -- anything above 99.5 percentile or below 0.5 percentile

upper <- quantile(cement$Intensity, c(.995), na.rm = TRUE)
lower <- quantile(cement$Intensity, c(.005), na.rm = TRUE)
cement <- cement[which(cement$Intensity <= upper & cement$Intensity >= lower),]
cement <- cement[which(cement$Cement > 0 & cement$Intensity > 0),]
high <- cement[which(cement$GDP.per.capita >= 30000),]

# Repeat for simulated data

upper <- quantile(sim.df$Intensity, c(.995), na.rm = TRUE)
lower <- quantile(sim.df$Intensity, c(.005), na.rm = TRUE)
sim.df <- sim.df[which(sim.df$Intensity <= upper & sim.df$Intensity >= lower),]
sim.df <- sim.df[which(sim.df$Cement > 0 & sim.df$Intensity > 0),]

hi <- c()
li <- c()
yrs <- c()

for (yr in 1990:2007) {
  
  h <- high[which(high$Year == yr),]
  l <- sim.df[which(sim.df$Year == yr),]
  hc <- sum(h$Cement)
  lc <- sum(l$Cement)

  hi[[yr-1989]] <- sum(h$Intensity * h$Cement) / hc
  li[[yr-1989]] <- sum(l$Intensity * l$Cement) / lc
  yrs[[yr-1989]] <- yr
  
}

Cint.df <- data.frame(Year = c(yrs), High = c(hi), Low = c(li))

ggplot(data = Cint.df[which(Cint.df$Year > 1997 & Cint.df$Year < 2008),], aes(x = Year, y = value)) +
  ggtitle('Relationship Between GDP per capita and Carbon Intensity\nPrior to the Kyoto Protocol (Simulated Data)') +
  ylab('Carbon Intensity (kg of C per kg of Cement)') +
  geom_line(aes(y = High, col = 'High Income Nations'), size = 2, alpha = 1) +
  geom_line(aes(y = Low, col = 'Low Income Nations'), size = 2, alpha = 1) +
  theme(legend.position = c(0.5,0.52), legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9))

dev.copy(png, paste(directory, 'simulation_fig.png'))
dev.off()

