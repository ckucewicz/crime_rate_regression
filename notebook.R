# installing necessary packages
install.packages('dplyr')
library(dplyr)

# reading in data file
data <- read.csv('data/crime_rate_regression_data.csv')

data_2000_to_present <- data[data$year >=2000, ]

# Converting all blank values to NA
data_2000_to_present[data_2000_to_present == ""] <- NA

# removing commas from population values
data_2000_to_present$population <- as.numeric(gsub(",", "", data_2000_to_present$population))


# for interpolating population for missing years

# (x,y) need to return rows where population is known
known_data <- data_2000_to_present[!is.na(data_2000_to_present$population), c('year', 'population')]

# interpolating population counts
interp <- approx(
  x = known_data$year,
  y = known_data$population,
  xout = data_2000_to_present$year,
  method = "linear"
)
data_2000_to_present$population_interpolated <- interp$y
