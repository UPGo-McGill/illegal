############### DATA IMPORT ##################

## Load helpers

source("R/01_helper_functions.R")


## Import Montreal boroughs

montreal <-
  read_sf(dsn = "data", layer = "plateau") %>%
  st_transform(32618) %>% 
  select(MUNID, CODEID, NOM, geometry)

plateau <-
  suppressWarnings(montreal %>% 
  filter(CODEID == "5") %>% 
  st_cast("POLYGON"))


## Import private Airbnb files

property <-
  read_csv("data/Montreal_property.csv") %>%
  select(c(1:7, 50:51)) %>% 
  set_names(c(
    "Property_ID", "Host_ID", "Listing_Title", "Property_Type", "Listing_Type",
    "Created", "Scraped", "Latitude", "Longitude")) %>% 
  arrange(Property_ID) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32618) %>% 
  filter(Property_Type %in% c(
    "House", "Private room in house", "Apartment", "Cabin",
    "Entire condominium", "Townhouse", "Condominium", "Entire apartment",
    "Private room", "Loft", "Place", "Entire house", "Villa", "Guesthouse",
    "Private room in apartment", "Guest suite", "Shared room in dorm",
    "Chalet", "Dorm", "Entire chalet", "Shared room in loft", "Cottage",
    "Resort", "Serviced apartment", "Other", "Bungalow", "Farm stay",
    "Private room in villa", "Entire loft", "Entire villa",
    "Private room in guesthouse", "Island", "Entire cabin", "Vacation home",
    "Entire bungalow", "Earth house", "Nature lodge", "In-law",
    "Entire guest suite", "Shared room in apartment", "Private room in loft",
    "Tiny house", "Castle", "Earth House", "Private room in condominium",
    "Entire place", "Shared room", "Hut", "Private room in guest suite",
    "Private room in townhouse", "Timeshare", "Entire townhouse",
    "Shared room in house", "Entire guesthouse", "Shared room in condominium",
    "Cave", "Private room in cabin", "Dome house",
    "Private room in vacation home", "Private room in dorm",
    "Entire serviced apartment", "Private room in bungalow",
    "Private room in serviced apartment", "Entire Floor", "Entire earth house",
    "Entire castle", "Shared room in chalet", "Shared room in bungalow",
    "Shared room in townhouse", "Entire cottage", "Private room in castle",
    "Private room in chalet", "Private room in nature lodge", "Entire in-law",
    "Shared room in guesthouse", "Casa particular", "Serviced flat", "Minsu",
    "Entire timeshare", "Shared room in timeshare", "Entire vacation home",
    "Entire nature lodge", "Entire island", "Private room in in-law",
    "Shared room in serviced apartment", "Shared room in cabin", "Entire dorm",
    "Entire cave", "Private room in timeshare", "Shared room in guest suite",
    "Private room in cave", "Entire tiny house",
    "Private room in casa particular (cuba)", "Casa particular (cuba)",
    "Private room in cottage", "Private room in tiny house",
    "Entire casa particular", ""))

daily <- read_csv("data/Montreal_daily.csv") %>% 
  set_names(c("Property_ID", "Date", "Status", "Booked_Date", "Price",
              "Reservation_ID")) %>% 
  select(c(1:3, 5)) %>% 
  arrange(Property_ID, Date)


## Trim listings to the Plateau in 2018

property <-
  property %>% 
  filter(Property_ID %in% daily$Property_ID, Scraped >= "2018-01-01",
         Created <= "2018-12-31") %>% 
  st_join(st_buffer(plateau["geometry"], 200), join = st_within, left = FALSE)

daily <- 
  daily %>% 
  filter(Property_ID %in% property$Property_ID, Date >= "2018-01-01",
         Date <= "2018-12-31")


## Join property and daily file

daily <- inner_join(daily, st_drop_geometry(property), by = "Property_ID") %>% 
  select(Property_ID, Date, Status, Price, Host_ID, Listing_Type)


## Import legal permitted Plateau listings and add permit columns to property

permit <- read_csv("data/plateau_legal.csv") %>%
  set_names(c("ETBL_ID", "Property_ID", "Host_ID"))

property <- 
  property %>%
  mutate(Permit = Property_ID %in% permit$Property_ID) %>%
  left_join(permit)


## Find FREH listings

property <- 
  daily %>%
  group_by(Property_ID) %>% 
  summarize(
    n_reserved = sum(Status == "R"),
    n_available = sum(Status != "B"),
    FREH = if_else(
      first(Listing_Type) == "Entire home/apt" & sum(Status == "R") >= 90 &
        sum(Status != "B") >= 183, TRUE, FALSE)) %>% 
  inner_join(property, .)




# entire home multi-listings
listing_type <- "Entire home/apt"
daily <- daily %>% 
    group_by(Listing_Type, Host_ID, Date) %>% 
    mutate(ML = ifelse(
    n() >= 2 & !! listing_type == "Entire home/apt", TRUE, FALSE)) %>% 
    ungroup()

multilistings <- select(daily, c(1,15)) %>% 
  distinct()

multilistings <- filter(multilistings, ML == TRUE)

property$ML <- property$Property_ID %in% multilistings$Property_ID

rm(multilistings)

# least frequently rented multi-listing
multilistings_available <- daily %>% 
  filter(ML == TRUE) %>% 
  group_by(Host_ID, Property_ID) %>% 
  count(Status == "A") 
names(multilistings_available) <- c("Host_ID", "Property_ID", "Available", "n_available")
multilistings_available <-
  filter(multilistings_available, Available == TRUE) %>% 
  select(-c(3))

multilistings_reserved <- daily %>% 
  filter(ML == TRUE) %>% 
  group_by(Host_ID, Property_ID) %>% 
  count(Status == "R") 
names(multilistings_reserved) <- c("Host_ID", "Property_ID", "Reserved", "n_reserved")
multilistings_reserved <-
  filter(multilistings_reserved, Reserved == TRUE) %>% 
  select(-c(3))

multilistings <- merge(multilistings_available, multilistings_reserved, by = c("Host_ID", "Property_ID"), all = TRUE)
rm(multilistings_available, multilistings_reserved)

names(multilistings) <- c("Host_ID", "Property_ID", "n_available", "n_reserved")

multilistings[is.na(multilistings)] <- 0

multilistings$n_available_reserved <- multilistings$n_available + multilistings$n_reserved

multilistings_primary <- multilistings %>% 
  inner_join(multilistings %>% 
               group_by(Host_ID) %>% 
               summarise(n = min(n_available_reserved))) %>% 
  select(c(1,2)) %>% 
  group_by(Host_ID) %>% 
  sample_n(1)

property$ML_primary <- property$Property_ID %in% multilistings_primary$Property_ID

# private rooms / ghost hotels

# determine if legal using the following variables: permit, frequent, ML, ML_primary, GH
  ## double check that this is producing the right results on monday
property <- 
  property %>% 
  mutate(Legal = case_when(
    Listing_Type == "Private room" ~ TRUE,
    Permit == TRUE ~ TRUE,
    ML_primary == TRUE ~ TRUE,
    Frequent == TRUE ~ FALSE,
    ML == TRUE ~ FALSE,
    Frequent == FALSE & ML == FALSE ~ TRUE)) %>% 
    select(c(1:8, 10, 15, 9, 12:14, 11))

property %>% 
  filter(Legal == FALSE)
# evaluate those active dec 31, 2018
start_date <- "2018-12-31" %>% 
  as.Date()
end_date <- "2018-12-31" %>% 
  as.Date()
property <- filter(property, .data$Created <= end_date)
property <- filter(property, .data$Scraped >= start_date)

property %>% 
  filter(Legal == FALSE)

property %>% 
  filter(Listing_Type == "Entire home/apt")

# characteristics of primary multilistings 
mean(multilistings_primary$n_available)
multilistings_not_primary <- multilistings[!multilistings$Property_ID %in% multilistings_primary$Property_ID,]
mean(multilistings_not_primary$n_available)
  # this suggests that there is negligible difference between primary multilistings and the remainder

# what if all multilistings are considered illegal?
  ## needs to be updated on monday - something wrong
property <- 
  property %>% 
  mutate(Legal = case_when(
    Listing_Type == "Private room" ~ TRUE,
    Permit == TRUE ~ TRUE,
    ML == TRUE ~ FALSE,
    ML_primary == TRUE ~ FALSE,
    Frequent == TRUE ~ FALSE))

view(property %>% 
  filter(ML == TRUE, Legal == TRUE, Permit == FALSE))
