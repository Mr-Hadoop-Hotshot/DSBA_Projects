#-------------------------------------------------------------------------------
#User Path CustomiZation
#-------------------------------------------------------------------------------

work_directory_path <- "F:/Data_Science/Casestudies/Atlas_labs_web_scrape/Final/"
unest_plat_path <- "F:\\Data_Science\\Casestudies\\Atlas_labs_web_scrape\\Final\\unnest_plat_full.csv"
final_data_tidy_path <- "F:\\Data_Science\\Casestudies\\Atlas_labs_web_scrape\\Final\\RAWG_2019.csv"

scheduled_script_path <- "F:\\Data_Science\\Casestudies\\Atlas_labs_web_scrape\\Final\\main_script.R"
#-------------------------------------------------------------------------------
#Work Directory Setup
#-------------------------------------------------------------------------------

setwd(work_directory_path)
getwd()

#-------------------------------------------------------------------------------
# Setting up URL
#-------------------------------------------------------------------------------
url = "https://api.rawg.io/api/games?dates=2019-01-01,2019-12-31&ordering=-added" # API

#-------------------------------------------------------------------------------
#Libraries
#-------------------------------------------------------------------------------

library(httr) # Fetching data from the web
library(jsonlite) # Converting JSON data into readable character values
library(tidyverse) # Applying piping.
library(tidyr) # Data wrangling tools i.e: separate()
library(stringr) # Data Wrangling
library(googlesheets4) # Google sheets access
library(taskscheduleR) #automating the web scrapping process.
library(readr) # Importing data set
#-------------------------------------------------------------------------------
# Fetch Data
#-------------------------------------------------------------------------------
raw_data = httr::GET(url) # Retrieve information from the given URL.
http_status(raw_data)# Extract http status code and converts into a readable format.
                    # 200 means successful.
web_data= content(raw_data) #Retrieves raw character output.
web_data = fromJSON(rawToChar(raw_data$content)) #"rawToChar" converts a length one character into one string character.
                                                 #"fromJSON" coverts javascript html file into readable format.

data_messy= web_data$results # Extracting required file in nested list format.
data_messy = data.frame(data_messy) #Converting the extracted nested list into a dataframe.


#-------------------------------------------------------------------------------
## Unnested Attributes
#-------------------------------------------------------------------------------
messy.df = data_messy %>% select(name,playtime,released,ratings_count) # Filtering Direct attributes
messy.df = data.frame(messy.df) # Converting into a dataframe.
#-------------------------------------------------------------------------------
## Nested Attributes: Platform
#-------------------------------------------------------------------------------
unnest_plat_full = data_messy %>%
              select(name,platforms) %>% 
              unnest(cols=c(platforms),keep_empty=TRUE) # Unnesting list of list platform information.

#-------------------------------------------------------------------------------
## Nested Attributes: Ratings
#-------------------------------------------------------------------------------

unnest_rate=data_messy %>% select(name,ratings) %>% 
  unnest(cols=c(ratings),keep_empty = TRUE) # Unnesting Rating attribute

unnest_rate = data.frame(unnest_rate) # Converting unnested rating into a data frame.

#-------------------------------------------------------------------------------
## Retrieve all saved files
#-------------------------------------------------------------------------------



write.csv(unnest_plat_full,
          unest_plat_path)

unnest_plat = read.csv(unest_plat_path)

plat = unnest_plat %>% select(name,platform.slug) # select important variables from Platform file
rate = unnest_rate %>% select(name,title,count,percent) # Select important variables Rating file.

#-------------------------------------------------------------------------------
## Data Pre processing
#===============================================================================
### Data Pre processing : Platform
#-------------------------------------------------------------------------------

plat_tidy=plat %>% 
  pivot_wider(names_from=platform.slug,values_from=platform.slug) # Converting into a single data frame.

#-------------------------------------------------------------------------------
### Data Pre processing : Rating
#-------------------------------------------------------------------------------


rate_tidy <- rate %>%select(!count) %>%  
  pivot_wider(names_from = title, values_from = percent) # Converting into a single dataframe.

#-------------------------------------------------------------------------------
#Final data set : merging Messy.df + plat_tidy + rate_tidy
#-------------------------------------------------------------------------------

final_data_tidy<- merge((merge(messy.df,rate_tidy,by="name",all.x=TRUE)),
                        plat_tidy,by="name",all.x=TRUE)
final_data_tidy

write.csv(final_data_tidy,final_data_tidy_path)

#-------------------------------------------------------------------------------
#Automatic Update on Google Sheet
#-------------------------------------------------------------------------------
if(gs4_has_token()){
range_write(
  unnested_platform,
  plat,
  sheet=NULL,
  range=NULL,
  col_names=TRUE,
  reformat = TRUE)


range_write(
  unnested_rating,
  rate,
  sheet=NULL,
  range=NULL,
  col_names=TRUE,
  reformat = TRUE)

range_write(
  RAWG_2019,
  final_data_tidy,
  sheet=NULL,
  range=NULL,
  col_names=TRUE,
  reformat = TRUE)
}