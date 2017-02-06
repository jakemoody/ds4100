###############################
#
# Assignment 5 
# Jake Moody 
#
################################
# Load libraries
library(XML)
library(stringr)

# (25 Points) Load the XML document at the URL  above directly into a data frame, 
# i.e., do not download the file and save it.

senators <- xmlToDataFrame("http://ds4100.weebly.com/uploads/8/6/5/9/8659576/senators.xml") 

# (50 Points) Write a function senatorName() that returns the names of the senators for a 
# given state, i.e., the function takes a state as an argument and returns the names of the 
# senators for that state in a vector.

senatorName <- function(x){
  by_state <- senators[senators$state == x,] # extract only records with state called in function
  for(i in 1:nrow(by_state)){ # start a for loop to extract names
    names <- paste(by_state$first_name, by_state$last_name) # paste first and last name for each row
    i <- i + 1 # keep the loop going...
  }
  return(names)
}

# Test the function
senatorName("MA")

# (25 Points) Write a function senatorPhone() that returns the phone number for a given senator.
# The function should take the first and last name of the senator as a single argument in the 
# format lastname, firstname, then parse the name, search for a match in the data frame and 
# return the phone number for that senator. For example, senatorPhone("Kerry, John").

senatorPhone <- function(x){
  # parse name from input
  name_parsed <- data.frame(str_split_fixed(x,",",n=2))
  colnames(name_parsed) <- c("last_name", "first_name")
  # filter from the data frame based on last name
  senator_record <- senators[senators$last_name == name_parsed$last_name,] # filter by last name 
  # filter for cases where first name as a character is detected - more robust and accounts for middle initials and suffix
  a <- str_trim(as.character(senator_record$first_name)) # convert to character and trim for matching first_name
  b <- str_trim(as.character(name_parsed$first_name)) # convert to character and trim for matching first_name
  matching_record <- senator_record[which(str_detect(a, b) == TRUE),] # take the row that matches and extracts it
  phone <- matching_record$phone # grab the phone number of the record
  return(phone) # return the phone number
}  
  
# test function 
senatorPhone("Brown, Scott")
senatorPhone("Schumer, Charles") 
senatorPhone("Rockefeller, John")




        