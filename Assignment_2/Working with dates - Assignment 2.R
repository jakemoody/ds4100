###############################
#
# Assignment 2 
# Jake Moody 
#
################################

# (25 Points) Load the data file "Acquisitions" into an appropriate data object of your choice. 
# The files contains dates and firms into which an investment was made.
acquisitions <- read.csv("Assignment_2/acquisitions.csv", header = TRUE, sep = ",") 

# (75 Points) Write a function called leastInvInterval() that finds the smallest interval between successive investments.
leastInvInterval <- function(x){
  if(!exists("acquisitions")) { # test to see if object exists...
    acquisitions <- read.csv("acquisitions.csv", header = TRUE, sep = ",") # if not, load it
  } else {
    attach(acquisitions, warn.conflicts = FALSE) # attach object if it's already available
  }
  acquisitions$Date <- as.Date(acquisitions$Date, "%m/%d/%Y") # covert to class Date 
  row_min <- min(diff(acquisitions$Date)) # find the minimum of all the differences 
  return(row_min) # return the minimum interval
}

# Execute function
leastInvInterval() # Time difference of 38 days 





