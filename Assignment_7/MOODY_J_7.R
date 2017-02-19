###############################
#
# Assignment 7
# Jake Moody 
#
################################
### Libraries for Web Scraping ###
library(rvest)
library(stringr)

# Scraping beer recipe data from beerrecipe.org. Specifically, we want the beer's name, the style of beer,
# the type, the rating and the number of votes. The script was built built to be parameterized. You can
# input any number of recipe ids which are just integer values and it will test. 

# Get Recipe IDs to Scrape
id <- c("542", "278","339","1111111","316") # vector of beer recipe ids, purposefully 
# put in '1111111' which doesn't exist to show robustness - loop can handle 404 errors

getBeer <- function(id){
  beer_data_new <- data.frame() # Clear up this data frame in case it was used before to avoid duplicates
  beer_data_all <- data.frame() # Clear up this data frame in case it was used before to avoid duplicates
  # For loop to get the data runs through every recipe id
  for(i in 1:length(id)){ # for each id
    # Get URL to Scrape
    url <- paste("https://beerrecipes.org/Recipe/",id[i],sep="") # parameterized URL builder
    test <- try(read_html(url), silent=TRUE) # try the URL
    if('try-error' %in% class(test)){ # if it returns an error (doesn't exist, for example)...
      next #...then skip it
    } else { # otherwise
      # Scrape the data from various nodes in the HMTL 
      scrape_1 <- url %>% read_html() %>% html_nodes("p span") %>% html_text() 
      scrape_2 <- url %>% read_html() %>% html_nodes("p") %>% html_text()
      scrape_3 <- url %>% read_html() %>% html_nodes("a , .mtop-5") %>% html_text()
      scrape_4 <- url %>% read_html() %>% html_nodes("div h1") %>% html_text()
      
      # Then parse the results...
      
      # Parse Beer Style & Description
      split_4 <- unlist(strsplit(scrape_2[5], "\\:")) # clean the string
      split_5 <- unlist(strsplit(split_4[2], "\\(")) # clean the string 
      beer_style <- str_trim(split_5[1]) # get the style of beer
      split_6 <- unlist(strsplit(split_4[3], "\\Y"))
      beer_type <- str_trim(split_6[1]) # gets the type of beer
      description <- scrape_1[2] # parses the description associated with the beer
      beer_name <- scrape_4[3] # parses the beer's name 
      
      # Parse Rating
      split_7 <- unlist(strsplit(scrape_3[40], "\\n")) # clean the string 
      split_8 <- unlist(strsplit(split_7[2], " ")) # clean the string 
      beer_rating <- str_trim(split_8[1]) # parses the beers rating (out of 5 stars)
      rating_votes <- split_8[5] # parses the beers ratings
      
      # Create data frame
      beer_data_new <- data.frame(beer_name, beer_style, beer_type, beer_rating, rating_votes, description, stringsAsFactors = FALSE) # create data frame
      beer_data_all <- rbind(beer_data_all, beer_data_new) # bind with previous loops
      
      # Keep the loop going 
      i <- i + 1 
    }  
  }
  return(beer_data_all)
}  

## Apply getBeer function to scrape the data
getBeer(id)


 