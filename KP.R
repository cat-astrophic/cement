# Main script for project studying the effect of the Kyoto Protocol on cement manufacturing

# Reading in the data -- make sure that the filepath below matches where you store the data

filepath <- 'C:/Users/User/Documents/Data/cement/cement.csv'

# Declaring a directory to write results to

directory <- 'C:/Users/User/Documents/Data/cement/'

# This is where the raw net trade data files are stored

tradedir <- 'C:/Users/User/Documents/Data/cement/trade/'

# Running the three scripts -- ensure that the directory is correct

source('C:/Users/User/Documents/cement.R')
source('C:/Users/User/Documents/cement_differenced.R')
source('C:/Users/User/Documents/cemissions.R')
source('C:/Users/User/Documents/cemissions_differenced.R')
source('C:/Users/User/Documents/cementensity.R')
source('C:/Users/User/Documents/cementensity_differenced.R')
source('C:/Users/User/Documents/cementensity_simulation.R')
source('C:/Users/User/Documents/cement_leakage.R')

