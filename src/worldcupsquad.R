#This script 
# 1. scrapes Wikipedia to: 
# - collect the list of all  players of the 2023 RWC
# - extract their place of birth (when available)
#Then
# 2. geolocates 
# - all players with OSM
# - performing some manual corrections
# 3. Export the output as a geopackage

#author: Julien Migozzi, University of Oxford

#loading packages
library(rvest)
library(tidyverse)
library(janitor)
library(Rcrawler)
library(sf)
library(mapview)
library(tidygeocoder)

#1. Source list of players ----

# Get Wikipedia url
url <- "https://en.wikipedia.org/wiki/2023_Rugby_World_Cup_squads"

# Read page
page <- read_html(url)

# Get all the players in one df
all_players <- page %>% 
  html_nodes("table.wikitable") # 12 tables

all_players <- all_players %>%
   rvest::html_table(fill = TRUE) 

#Delete irelevant tables
all_players[[1]] <- NULL
all_players[[23]] <- NULL
all_players[[22]] <- NULL
all_players[[21]] <- NULL


all_players[[1]]$team <-  "New Zealand"
all_players[[2]]$team <-  "France"
all_players[[3]]$team <-  "Italy"
all_players[[4]]$team <-  "Uruguay"
all_players[[5]]$team <-  "Namibia"
all_players[[6]]$team <-  "South Africa"
all_players[[7]]$team <-  "Ireland"
all_players[[8]]$team <-  "Scotland"
all_players[[9]]$team <-  "Tonga"
all_players[[10]]$team <-  "Romania"
all_players[[11]]$team <-  "Wales"
all_players[[12]]$team <-  "Australia"
all_players[[13]]$team <-  "Fiji"
all_players[[14]]$team <-  "Georgia"
all_players[[15]]$team <-  "Portugal"
all_players[[16]]$team <-  "England"
all_players[[17]]$team <-  "Japan"
all_players[[18]]$team <-  "Argentina"
all_players[[19]]$team <-  "Samoa"
all_players[[20]]$team <-  "Chile"

all_players_df <- bind_rows(all_players) %>%
  clean_names()

#Delete numbers in name
all_players_df$player <- gsub("\\s*[12]$", "", all_players_df$player)

# Add url for each player
player_urls <- page %>% 
  html_nodes("td:nth-child(1) a") %>%
  html_attr("href")

player_urls_df <- as.data.frame(player_urls)

# Delete urls other than players
o <- player_urls
o <- o[-c(1:20)]
o <- o[-c(699:length(o))]
o <- o[!grepl("Captain", o)]
o <- o[!grepl("Vice-captain", o)]


# Add urls to the df
all_players_df$url <- o
all_players_df$url <- paste0("https://en.wikipedia.org", all_players_df$url)
all_players_df$url

##2. Webscrape POB from WIki ----

# Initialize an empty dataframe to store results
result_df <- data.frame(url = character(0), birth_info = character(0))

# Iterate through each player's Wikipedia URL (about 10 mins)

for (url in all_players_df$url) {  # Replace with your actual vector
  if (url != "") {
    tryCatch({
      page <- read_html(url)
      # page <- read_html("https://en.wikipedia.org/wiki/Dane_Coles")
      
      # Extract birth information
      info <- page %>%
        html_nodes(".infobox tbody tr:contains('Place of birth')") %>%
        html_text() %>%
        trimws()
      info <- gsub("Place of birth", "", info)
   
      
      if (length(info) == 0) {
        info <- NA
      }
      
      # Append to the result dataframe
      result_df <- result_df %>% 
        add_row(url = url, birth_info = info)
    }, error = function(e) {
      result_df <- result_df %>% 
        add_row(url = url, birth_info = NA)
    })
  } else {
    result_df <- result_df %>% 
      add_row(url = url, birth_info = NA)
  }
}


all_data <- left_join(all_players_df, result_df, by = "url")

all_data$birth_info <-  gsub("\\[\\d+\\]$", "", all_data$birth_info)

#Save file
write.csv(all_data, "data/extract_wikipedia_squad_rwc_players_pob.csv", row.names = F)

# 3 Manual corrections ----

all_data <- all_data %>% 
  mutate(birth_info = case_when(birth_info == "Windhoek, South West Africa" ~ "Windhoek, Namibia",
                       birth_info == "Nelspruit, Transvaal, Republic of South Africa" ~ "Nelspruit, South Africa",
                       birth_info == "Tsholomnqa, Cape Province, Republic of South Africa" ~ "Tsholomnqa, South Africa",
                       birth_info == "Tbilisi, Georgian SSR, Soviet Union" ~ "Tbilisi, Georgia",
                       birth_info == "Dhekelia, British Overseas Territories" ~ "Cyprus",
                       birth_info == "Nauluvatau, Nakelo, Tailevu, Fiji" ~ "Nakelo, Tailevu, Fiji",
                       TRUE ~ birth_info))

all_data <- all_data %>% 
  mutate(birth_info = case_when(player == "Carlos Deus" ~ "Montevideo, Uruguay",
                                player == "Kurt-Lee Arendse" ~ "Paarl, South Africa",
                                player == "Sam Lousi" ~ "Auckland, New Zealand",
                                player == "Solomone Kata" ~ "Neiafu, Tonga",
                                player == "Semi Radradra" ~ "Suva, Fiji",
                                player == "Jiuta Wainiqolo" ~ "Suva, Fiji",
                                player == "Anthony Alves" ~ "Oyonnax, France",
                                player == "José Lima" ~ "Coimbra, Portugal",
                                player == "Nuno Sousa Guedes" ~ "Porto, Portugal",
                                player == "Marika Koroibete" ~ "Naraiyawa, Fiji",
                                player == "Izaia Perese" ~ "Brisbane, Australia",
                                player == "André Gorin" ~ "Lespezi, Romania",
                                player == "Andrés Vilaseca (c)" ~ "Montevideo, Uruguay",
                                player == "Agustín Ormaechea" ~ "Montevideo, Uruguay",
                                
                                
                                
                                TRUE ~ birth_info))

# Split the full_location column by comma
split_location <- strsplit(all_data$birth_info, ",")

# Extract the country part and create a new column
all_data$country <- sapply(split_location, function(x) trimws(x[length(x)]))

all_data <-  all_data %>% 
  mutate(country = case_when(country == "New Caledonia" ~ "France",
                             country == "Iasi Romania" ~ "Romania",
                             country == "Parma" ~ "Italy",
                             country == "USA" ~ "United States",
                             country == "Wallis and Futuna" ~ "France",
                             country == "Wallis and Futuna" ~ "France",
                             country == "Republic of South Africa" ~ "South Africa",
                                                          TRUE ~ country))



# 4. Geocoding ----
# WIth OSM = 10 mins

all_data_geo <-  all_data %>% 
  geocode(address = birth_info, 
          method = "osm",
          verbose = TRUE) 


# Correct coding mistakes
all_data_geo <-  all_data_geo %>% 
  mutate(long = case_when(grepl("Saint-Jean-de-Luz", birth_info) ~ -1.6631, 
                          TRUE~ long) ) %>% 
  mutate(lat = case_when(grepl("Saint-Jean-de-Luz", birth_info) ~ 43.3881, # wrong geocoding
                         TRUE~ lat) ) %>% 
  mutate(long = case_when(grepl("Levuka", birth_info) ~ 178.8334, 
                          TRUE~ long) ) %>% 
  mutate(lat = case_when(grepl("Levuka", birth_info) ~ -17.68069, # no geocoding
                         TRUE~ lat) ) %>% 
  mutate(long = case_when(grepl("Naraiyawa", birth_info) ~ 178.06857, 
                          TRUE~ long) ) %>% 
  mutate(lat = case_when(grepl("Naraiyawa", birth_info) ~ -17.96294, # no geocoding
                         TRUE~ lat) ) %>% 
  mutate(long = case_when(grepl("Tsholomnqa", birth_info) ~  27.528158, 
                          TRUE~ long) ) %>% 
  mutate(lat = case_when(grepl("Tsholomnqa", birth_info) ~ -33.16119, # no geocoding
                         TRUE~ lat) ) 

#Save file
write.csv(all_data_geo, "data/geocoded_squad_rwc_players.csv", row.names = F)

#5. Quick EDA with mapview ----


players_sf <- all_data_geo %>% 
  filter(!is.na(long)) %>% # remove players without pob
   st_as_sf(coords = c("long", "lat"), crs = 4326) # %>% 
#st_jitter(factor = 0.00002) # avoid overlapping of points
mapview(players_sf)


#6. Save spatial data ----


st_write(players_sf, "data/worldcupsquads.gpkg", append = F)

