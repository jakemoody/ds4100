###############################
#
# Assignment 3 
# Jake Moody 
#
################################

# Load Packages
require(lubridate) # for date manipulation 
require(dplyr) # for data manipulation
require(stringr) # for string manipulation

# (20 Points) Load the data file "BirdStrikes" into an appropriate data object of your choice.
bird_strikes_raw <- read.csv("Bird Strikes.csv", header = TRUE, stringsAsFactors = FALSE) # load raw dataset

# Create helper cleaning function 
clean_data <- function(x){
  require(lubridate)
  x$flight_year <- year(as.Date(x$FlightDate, format = "%m/%d/%Y")) # create a new column with year of each incident
  names(x)[names(x) == 'Aircraft..Airline.Operator'] <- 'carrier' # clean up airline name
  return(x)
}

bird_strikes <- clean_data(bird_strikes_raw) # apply function to clean the dataset

# (20 Points) Write a function called mostStrikesInaYear() that returns the year which had the most bird strike incidents? 
mostStrikesInaYear <- function(x){
  if(missing(x)){
    stop("You need to input the argument...") # Error message if you forget an argument
  } else if(!exists("bird_strikes")) { # test to see if object exists...
    temp <- read.csv("Bird Strikes.csv", header = TRUE, sep = ",") # if not, load it...
    x <- clean_data(temp) # ...and clean it 
  } else {
    attach(x, warn.conflicts = FALSE) # attach object if it's already available
  }
  require(lubridate)
  require(dplyr)
  group_year <- x %>% group_by(flight_year) %>% summarise(total_by_year = n()) 
  max_year <- group_year[which.max(group_year$total_by_year),]
  return(max_year$flight_year)
}

# test function
mostStrikesInaYear(bird_strikes) # 2010


# (20 Points) Write a function called strikesByYear() that returns a dataframe containing for each year the number of bird strikes. 
strikesByYear <- function(x){
    if(missing(x)){
    stop("You need to input the argument...") # Error message if you forget an argument
} else if(!exists("bird_strikes")) { # test to see if object exists...
    temp <- read.csv("Bird Strikes.csv", header = TRUE, sep = ",") # if not, load it...
    x <- clean_data(temp) # and clean it 
  } else {
    attach(x, warn.conflicts = FALSE) # attach object if it's already available
  }
  require(dplyr)
  group_year <- x %>% group_by(flight_year) %>% summarise(total_by_year = n())
  return(data.frame(group_year))
}

# test function
strikesByYear(bird_strikes) # Data frame with 12 obvs and 2 variables


# (10 Points) Write a function called strikesByAirline() that calculates add returns the number of birds strike incidents per airline 
# (excluding military and business) in a dataframe. Store the return result in a dataframe called AirlineStrikes.
strikesByAirline <- function(x){
    if(missing(x)){
    stop("You need to input the argument...") # Error message if you forget an argument
} else if(!exists("bird_strikes")) { # test to see if object exists...
    temp <- read.csv("Bird Strikes.csv", header = TRUE, sep = ",") # if not, load it...
    x <- clean_data(temp) # and clean it 
  } else {
    attach(x, warn.conflicts = FALSE) # attach object if it's already available
  }
  require(dplyr)
  require(stringr)
  group_carrier <-  x %>% 
    group_by(carrier) %>% # group bird_strikes by airline
    summarise(total_by_year = n()) %>% # summarise the total number of incidents
    filter(!str_detect(carrier, '^BUSINESS$|^MILITARY$')) # filter out airlines, "BUSINESS" and "MILITARY"
  AirlineStrikes <- return(data.frame(group_carrier)) # Return as dataframe named AirlineStrikes
}

# test function
AirlineStrikes <- strikesByAirline(bird_strikes) # test is a data frame with 370 obs & 2 variables

# (10 Points) Write a function called mostStrikes() that accepts the dataframe AirlineStrikes 
# from (4) as an argument and returns the airline that has the most bird strike incidents.
# (Note: UNKNOWN occurs most frequently but is a marker for missing data. Your function needs 
# to return a valid airline, i.e., the second most occurring value.)

mostStrikes <- function(x){
    if(missing(x)){
    stop("You need to input the argument...") # Error message if you forget an argument
} else if(!exists("bird_strikes")) { # test to see if object exists...
    temp <- read.csv("Bird Strikes.csv", header = TRUE, sep = ",") # if not, load it...
    x <- clean_data(temp) # and clean it 
  } else {
    attach(x, warn.conflicts = FALSE) # attach object if it's already available
  }
  require(dplyr)
  require(stringr)
  airlines_df <- strikesByAirline(x)
  airlines_df_filtered <- airlines_df %>% filter(!str_detect(carrier, '^UNKNOWN$')) 
  max_airline <- airlines_df_filtered[which.max(airlines_df_filtered$total_by_year),]
  return(as.character(max_airline$carrier))
}

# test function
mostStrikes(bird_strikes) # United Airlines

# (20 Points) For the function mostStrikes() use system.time() to measure the execution time 
# for the original sized data, 2 times the original size and 4 times the original size by 
# duplicating the data set. Write a short report that describes your approach and includes a 
# chart that visualizes the runtime curve (you may do the chart in Excel or R). 
# Based on the curve, find the complexity of the function and define big-O. 
# Hint: Consider using the rbind() function to concatenate data frames.

## Step 1: Create 2x and 4x verion of the data using rbind

# Create 2x copy
bird_strikes_2 <- bird_strikes # make a copy of the original
double_bird_strike <- rbind(bird_strikes, bird_strikes_2) # bind the 2 copies

# Create 4x copy
double_bird_strike_2 <- double_bird_strike # make a copy of the 2x version
quad_bird_strike <- rbind(double_bird_strike, double_bird_strike_2) # bind the 2x version and its duplicate


## Step 2: Run the code below and measure time with Sys.time()

# run the original
start_1 <- Sys.time()
mostStrikes(bird_strikes)
end_1 <- Sys.time()
# run the 2x version
start_2 <- Sys.time()
mostStrikes(double_bird_strike)
end_2 <- Sys.time()
# run the 4x verion
start_4 <- Sys.time()
mostStrikes(quad_bird_strike)
end_4 <- Sys.time()

## Step 3: Calculate runtime of the function on each dataset

time_1 <- end_1 - start_1 # total run time for original
time_2 <- end_2 - start_2 # total run time for the 2x version
time_4 <- end_4 - start_4 # total run time for the 4x version

## Step 4: Use nrow() to determine the number of elements
obvs_base <- nrow(bird_strikes)
obvs_base_2 <- nrow(double_bird_strike)
obvs_base_4 <- nrow(quad_bird_strike)

## Step 5: Create a data frame with the data
speed_test <- data.frame(observations = c(obvs_base,obvs_base_2,obvs_base_4), 
                         time = c(time_1,time_2,time_4))

## Step 6: Plot the data
plot(speed_test$observations, speed_test$time, type = "o",
     xlab = "Elements",ylab = "Time in Seconds",main = "Big-O Complexity of 'mostStrikes()'")

## RESULTS: 
# The complexity of the mostStrikes function is best defined as O(N)
# as the plot shows a linear curve. The number of operations increases linearly
# as the number of elements in the data frame increases. 



