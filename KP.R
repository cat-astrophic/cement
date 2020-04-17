# Main script for project studying the effect of the Kyoto Protocol on cement manufaction

# Reading in the data -- make sure that the filepath below matches where you store the data

filepath <- 'C:/Users/M535040/Documents/Data/cement/cement.csv'

# Declaring a directory to write results to

directory <- 'C:/Users/M535040/Documents/Data/cement/'

# Running the three scripts -- ensure that the directory is correct

source('C:/Users/M535040/Documents/cement.R')
source('C:/Users/M535040/Documents/cement_differenced.R')
source('C:/Users/M535040/Documents/cemissions.R')
source('C:/Users/M535040/Documents/cemissions_differenced.R')
source('C:/Users/M535040/Documents/cementensity.R')
source('C:/Users/M535040/Documents/cementensity_differenced.R')

