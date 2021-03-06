############### DATA IMPORT ##################

## Load helpers

source("R/01_helper_functions.R")

## Import Montreal geometries

montreal <-
  read_sf(dsn = "data", layer = "plateau") %>%
  st_transform(32618) %>% 
  select(MUNID, CODEID, NOM, geometry)

plateau <-
  suppressWarnings(montreal %>% 
  filter(CODEID == "5") %>% 
  st_cast("POLYGON"))

DA <-
  get_census(
    dataset = 'CA16',regions=list(CMA="24462"), level = 'DA', 
    geo_format = "sf") %>%
  st_transform(32618) 


## Import private Airbnb files

property <-
  read_csv("data/Montreal_property_2019.csv", col_types = cols_only(
    `Property ID` = col_character(),
    `Listing Title` = col_character(),
    `Property Type` = col_character(),
    `Listing Type` = col_character(),
    `Created Date` = col_date(format = ""),
    `Last Scraped Date` = col_date(format = ""),
    Latitude = col_double(),
    Longitude = col_double(),
    `Airbnb Property ID` = col_double(),
    `Airbnb Host ID` = col_double(),
    `HomeAway Property ID` = col_character(),
    `HomeAway Property Manager` = col_character())) %>% 
  set_names(c("Property_ID", "Listing_Title", "Property_Type", "Listing_Type",
              "Created", "Scraped", "Latitude", "Longitude", "Airbnb_PID",
              "Airbnb_HID", "HomeAway_PID", "HomeAway_HID")) %>% 
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
    "Entire casa particular", "")) %>% 
  select(-Property_Type)

daily <- 
  read_csv("data/Montreal_daily_2019.csv", col_types = cols(
    `Property ID` = col_character(),
    Date = col_date(format = ""),
    Status = col_factor(levels = c("U", "B", "A", "R")),
    `Booked Date` = col_skip(),
    `Price (USD)` = col_double(),
    `Price (Native)` = col_skip(),
    `Currency Native` = col_skip(),
    `Reservation ID` = col_skip(),
    `Airbnb Property ID` = col_double(),
    `HomeAway Property ID` = col_character())) %>% 
  set_names(c("Property_ID", "Date", "Status", "Price", "Airbnb_PID", 
              "HomeAway_PID")) %>% 
  filter(!is.na(Status)) %>%
  arrange(Property_ID, Date)
  

## Trim listings to the Plateau in May 2018-April 2019 and add raffle results

property <-
  property %>% 
  filter(Property_ID %in% daily$Property_ID,
         Scraped >= "2018-05-01",
         Created <= "2019-04-30") %>% 
  st_join(st_buffer(plateau["geometry"], 200),
          join = st_within, left = FALSE) %>% 
  left_join(read_csv("data/raffle.csv"))

daily <- 
  daily %>% 
  filter(Property_ID %in% property$Property_ID,
         Date >= "2018-05-01",
         Date <= "2019-04-30")


## Join property and daily file

daily <- inner_join(daily, st_drop_geometry(property)) %>% 
  select(Property_ID, Date, Status, Price, Airbnb_PID, HomeAway_PID, Airbnb_HID,
         HomeAway_HID, Listing_Type)


## Import legal permitted Plateau listings and add permit columns to property

permit <- read_csv("data/plateau_legal.csv") %>%
  set_names(c("ETBL_ID", "Property_ID", "Host_ID"))

property <- 
  property %>%
  mutate(Permit = Airbnb_PID %in% permit$Property_ID) %>%
  left_join(permit,
            by = c("Airbnb_PID" = "Property_ID", "Airbnb_HID" = "Host_ID"))


## Find FREH listings and revenue

property <- 
  daily %>%
  group_by(Property_ID) %>% 
  summarize(
    n_reserved = sum(Status == "R"),
    n_available = sum(Status == "A" | Status == "R"),
    revenue = sum((Status == "R") * Price),
    FREH = if_else(
      first(Listing_Type) == "Entire home/apt" & n_reserved >= 90 &
        n_available >= 183, TRUE, FALSE)) %>% 
  inner_join(property, .)


## Find multi-listings

daily <- strr_multilistings(daily, listing_type = Listing_Type,
                            host_ID = Airbnb_HID, date = Date)

property <- 
  daily %>%
  group_by(Property_ID) %>% 
  summarize(ML = as.logical(ceiling(mean(ML)))) %>% 
  inner_join(property, .)


## Identify the least frequently rented multi-listing (LFRML)

property <- 
  property %>% 
  group_by(Airbnb_HID, Listing_Type) %>% 
  mutate(LFRML = case_when(
    Listing_Type != "Entire home/apt" ~ FALSE,
    ML == FALSE                       ~ FALSE,
    n_available == min(n_available)   ~ TRUE,
    TRUE                              ~ FALSE)) %>% 
  ungroup()
  
       
## Resolve any LFRML ties with n_reserved and then random chance

property <- 
  property %>% 
  group_by(Airbnb_HID, Listing_Type) %>% 
  mutate(LFRML = if_else(
    sum(LFRML) > 1 & n_reserved != min(n_reserved),
    FALSE, LFRML)) %>% 
  ungroup()

property <- 
  property %>% 
  group_by(Airbnb_HID, Listing_Type) %>% 
  mutate(prob = sample(0:10000, n(), replace = TRUE),
         LFRML = if_else(
           sum(LFRML) > 1 & prob != max(prob), FALSE, LFRML)) %>% 
  select(-prob)
  

## Test for remaining LFRML ties:
# property %>% st_drop_geometry() %>% group_by(Airbnb_HID) %>%
# filter(sum(LFRML) > 1) %>% summarize(n_LFRML = sum(LFRML))


# Identify ghost hotels

GH_list <-
  strr_ghost(property, Property_ID, Airbnb_HID, Created, Scraped, "2018-05-01",
           "2019-04-30", listing_type = Listing_Type) %>% 
  pull(property_IDs) %>%
  unlist() %>%
  unique()

property <-
  property %>% 
  mutate(GH = if_else(Property_ID %in% GH_list, TRUE, FALSE))

rm(GH_list)


# Determine legality

property <- 
  property %>%   
  mutate(Legal = case_when(
    Permit == TRUE                 ~ TRUE,
    GH == TRUE                     ~ FALSE,
    Listing_Type == "Private room" ~ TRUE,
    FREH == TRUE                   ~ FALSE,
    LFRML == TRUE                  ~ TRUE,
    ML == TRUE                     ~ FALSE,
    TRUE                           ~ TRUE))



## Import Exact Addresses From Permit Data

plateau_address <- read_csv("data/Addresses.csv") %>%
  select(c(1,7,12,13))%>%
  set_names(c("ETBL_ID","Address","Latitude", "Longitude"))

establishment_type <- read_csv("data/EstablishmentType.csv") %>%
  select(c(1,3,4))%>%
  set_names(c("ETBL_ID","ETBL_Code", "Establishment_Type"))
plateau_address <- inner_join(plateau_address, establishment_type)
rm(establishment_type)

plateau_address <- inner_join(st_drop_geometry(property), plateau_address) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32618)
