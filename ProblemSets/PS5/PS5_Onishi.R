# part 1
library(rvest)
library(tidyverse)
url = "http://www.allcompetitions.com/tri_ironm.php"
path = "#global > table.iron"
imwc = read_html(url)
imwc_mens = imwc %>% html_node(path) %>% html_table()
head(imwc_mens)

# part 2
library(rStrava)
library(dplyr)
app_name = 'Econ Project' 
# integer, assigned by Strava
app_client_id  = '103449' 
# obtain API key, stored in the .Renviron file
app_secret = Sys.getenv("API_KEY")
# create the authentication token; required when scraping Strava data from a profile
stoken = httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all"))
# get activity data
my_acts = get_activity_list(stoken)
# Filer data to activites in Norman, OK
act_data <- compile_activities(my_acts) %>% 
  filter(start_latlng2 < -96, start_latlng2 > -98) %>% 
  filter(start_latlng1 < 36, start_latlng1 > 35) %>% 
  filter(distance > 20)
act_data_df <- as.data.frame(act_data)
# preview of activity data in the Norman area, of this Strava user 
head(act_data_df)
