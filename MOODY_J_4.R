###############################
#
# Assignment 4 
# Jake Moody 
#
################################

# Load Packages
require(readxl) # for importing file
require(lubridate) # for date manipulation
require(stringr) # for string manipulation

# (25 Points) Load the data file into a data frame.

market_data <- read_excel("2013 Geographric Coordinate Spreadsheet for U S  Farmers Markets 8'3'1013.xlsx", sheet = 1,skip = 2)

# (50 Points) The seasons are not standardized and would make analysis difficult. 
# Create six levels of seasons: Summer, Fall, Winter, Spring, Year-Round, Half-Year 
# and convert each provided season in the Season1Date column to one of the season levels. 

# Assumptions
# 1) If a record (start date, end date, or both) is missing, it will be ignored and remain NA.
# 2) Make the following assumptions about seasons:
#    - Winter = December, January, February  
#    - Spring = March, April, May
#    - Summer = June, July, August
#    - Fall = September, October, November
#    - Half-year = Any time period between 180 days and 300 days
#    - Full-year = Any time period at 300 or more days
#    - The half-way date of each time period was used to determine if it was Winter, Spring, Summer or Fall

# Split the Season1Date Column into 2 columns
separate <- as.data.frame(str_split_fixed(market_data$Season1Date, " ", n=3)) # split into 3 columns
colnames(separate) <- c("first_date", "grammar", "second_date")
separate$first_date <- as.character(separate$first_date) # standardize all dates as characters for now
separate$second_date <- as.character(separate$second_date) # standardize all dates as characters for now


# Create lookup helper function
lookup <- data.frame(month = c("January","February","March",
                               "April","May","June","July",
                               "August","September","October","November",
                               "December"),
                     date = c("01/01/2012", "02/01/2012",
                                      "03/01/2012", "04/01/2012",
                                      "05/01/2012", "06/01/2012",
                                      "07/01/2012", "08/01/2012",
                                      "09/01/2012", "10/01/2012",
                                      "11/01/2012", "12/01/2012"))

lookup$date <- as.Date(lookup$date, format = "%m/%d/%Y") # classify as date

# Get all dates into the same format
convert_date <- function(x){
  if(!str_detect(x, '/') == TRUE){ # if the observation doesn't contain '/'...
    data <- as.Date(lookup$date[match(x,lookup$month)]) #...use the lookup helper table and classify it as a date
  } else{
    data <- as.Date(x, format = "%m/%d/%Y") # otherwise turn it into a date object
  }
  return(as.Date(data))
}

# Make some new variables in the original data frame
market_data$first_date <- as.Date(sapply(separate$first_date, convert_date),origin = "1970-01-01") # add the cleaned start dates back to the main data frame
market_data$second_date <- as.Date(sapply(separate$second_date, convert_date),origin = "1970-01-01") # add the cleaned end dates back to the main data frame
market_data$time_diff <- market_data$second_date - market_data$first_date # find the total length of the time period in days
market_data$half_way <- market_data$time_diff/2 + market_data$first_date # find the midway point (used to determine seasons)

# Create helper function to determine date
getSeason <- function(DATES) {
  WI <- as.Date("2012-12-01", format = "%Y-%m-%d") # Winter start date
  SP <- as.Date("2012-3-01",  format = "%Y-%m-%d") # Spring start date
  SU <- as.Date("2012-6-01",  format = "%Y-%m-%d") # Summer start date
  FA <- as.Date("2012-9-01",  format = "%Y-%m-%d") # Fall start date
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WI | d <  SP, "Winter", 
          ifelse (d >=  SP & d < SU, "Spring",
                  ifelse (d >= SU & d < FA, "Summer", "Fall")))
}


# For loop to apply the functions 
for(i in 1:nrow(market_data)){ # for each row in the market_data data frame
  if(is.na(market_data$time_diff[i])){ # if the record is NA...
    market_data$period[i] <- NA # assign the period NA as well
  } else {
    if(market_data$time_diff[i] <= 180){  # if time period was less than 180 days...
      market_data$period[i] <- getSeason(market_data$half_way[i]) #...we should consider it a season: Winter, Spring, Summer or Fall based on the date at the half way point of the time interval
    } else if ((market_data$time_diff[i] <= 300) ==TRUE){ # if it's greater than 180, but less than 300...
      market_data$period[i] <- "Half-Year" #...we'll consider it a "Half-Year" 
    } else {
      market_data$period[i] <- "Full-Year" # for anything else greater than 300 days, we'll consider it a full year, the reasoning being it wouldn't be appropriate to call, for example, a 10-month long farmer's market a "half year" one. 
    }
  }
  i <- i + 1 # keep the for loop going
}

# market_data$period contains all of the season categories:
market_data$period <- as.factor(market_data$period)
print(market_data$period)

#(25 Points) Write a retrieval function acceptsWIC() that allows a data scientist to find which markets accept WIC. 
# The function should output a data frame containing only the markets that accept WIC. Use the function and output the data frame.

acceptsWIC <- function(x){ # for raw market data file 'x' 
  accepts_wic <- x[x$WIC == "Y",] # take only records where WIC was 'Y' (accept WIC) and create a new df with those observations
  return(data.frame(accepts_wic)) # save as data frame and return
}

# Test the function
accepts_wic_df <- acceptsWIC(market_data) # Retrive data frame containing only markets that accept WIC
print(accepts_wic_df) # output data frame per instructions


