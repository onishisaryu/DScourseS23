library(rStrava)
library(dplyr)
library(ggmap)

# set up
app_name = 'Econ Project'
app_client_id = '103449'
app_secret = Sys.getenv('API_KEY')

# generate authentication token
stoken = httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all"))

# get activities
activites = get_activity_list(stoken)
# filter activites
Norman_OK = compile_activities(activities) %>% 
  filter(start_latlng2 < -116, start_latlng2 > -125) %>% 
  filter(start_latlng1 < 50, start_latlng1 > 32) %>% 
  filter(distance > 20)

# set up key for google maps
google_key <- Sys.getenv("google_key")
register_google(key=google_key)

# viz 1: heat map
get_heat_map(Norman_OK, key = google_key, col = 'orange', size = 2, distlab = F, units = 'metric', f = .5)

# viz 2: elevation data on a single event
viz2 = NormanOK %>% filter(id == 5605749986)
get_elev_prof(viz2, key = google_key)

# viz 3: elevation data overlaid on map
get_heat_map(viz2, alpha = 1, add_elev = T, f = 0.3, distlab = F, key = google_key, size = 2, maptype = 'satellite', col = 'Spectral')

