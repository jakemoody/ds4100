###############################
#
# Assignment 1 
# Jake Moody 
#
################################

# (5 Points) Load the data file into an appropriate data object of your choice. 
# Load file as a data frame
airline_data <- read.table("Assignment_1/AirlineDelays.txt", header = TRUE, sep = ",") # load data & create data frame 

# (30 Points) Write a function called TotalNumDelays(Carrier) that finds and returns the total number of delays of a carrier. 
TotalNumDelays <- function(x){
  if(!exists("airline_data")) { # test to see if object exists...
    airline_data <- read.table("AirlineDelays.txt", header = TRUE, sep = ",") # if not, load it
  } else {
    attach(airline_data,warn.conflicts = FALSE) # attach object if it's already available
  }
  carrier_data <- airline_data[airline_data$CARRIER == x,] # filter all records for a certain airline, x
  delays <- carrier_data[carrier_data$DEP_DELAY > 0 | carrier_data$ARR_DELAY > 0,] # filter all records where a delays occured, either departure OR arrival
  num_delays <- nrow(delays[!is.na(delays$CARRIER),]) # remove rows where CARRIER is NA, and then count the number of rows/delays
  return(num_delays) # return the number of delays 
}  

# test function
TotalNumDelays("AA")
# 23,333

# (30 Points) Write a function called TotalDelaysByOrigin(Origin) that finds and returns the total number of delays for a particular airport.
TotalDelaysByOrigin <- function(x){
  if(!exists("airline_data")) { # test to see if object exists...
    airline_data <- read.table("AirlineDelays.txt", header = TRUE, sep = ",") # if not, load it
  } else {
    attach(airline_data, warn.conflicts = FALSE) # attach object if it's already available
  }
  origin_data <- airline_data[airline_data$ORIGIN == x,] # filter all records for a certain ORIGIN, x
  delays <- origin_data[origin_data$DEP_DELAY > 0 | origin_data$ARR_DELAY > 0,] # filter all records where a delayed occured, either departer OR arrival
  num_delays <- nrow(delays[!is.na(delays$ORIGIN),]) # remove rows where ORIGIN is NA and count the number of rows/delays
  return(num_delays) # return the number of delays
}  

# test function
TotalDelaysByOrigin("JFK")
# 4612 
 
# (25 Points) Write a function called AvgDelay(Carrier, Dest) that calculates 
# and returns the average arrival delay for a carrier flying into the 
# specified destination airport.
AvgDelay <- function(x,y){
  if(!exists("airline_data")) { # test to see if object exists...
    airline_data <- read.table("AirlineDelays.txt", header = TRUE, sep = ",") # if not, load it
  } else {
    attach(airline_data, warn.conflicts = FALSE) # attach object if it's already available
  }
  carrier_dest_data <- airline_data[airline_data$CARRIER == x & airline_data$DEST == y,] # filter all records for carrier, x, and destination, y.
  delays <- carrier_dest_data[carrier_dest_data$ARR_DELAY > 0,] # filter only flights that had an arrival delay (i.e ARR_DELAY > 0)
  average_delay <- mean(delays$ARR_DELAY, na.rm = TRUE) # find the average delay, ignore NAs
  return(average_delay) # return the average delay for each airport/destination
}  

# test function
AvgDelay("AA", "ABQ")
# Mine: 42.84184

# (10 Points) Improve your program and functions so that the data file is not loaded repeatedly, but only once or when needed. 
# Each function includes an if/else statement to check and see if the object is available. If not, it will load the file. If it is already available, it will use the object in the workspace
