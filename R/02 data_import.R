############### DATA IMPORT ##################

source("R/01 helper_functions.R")

## import plateau dataset

montreal <- read_sf(dsn = "data", layer = "plateau") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(3347)

plateau <- filter(montreal, CODEID=="5")

plateau_buff <-
  plateau %>%
  summarize(geometry = st_union(geometry)) %>% 
  st_buffer(200)

## import airbnb property file
property <- read_csv("Data/Montreal_property.csv")

names(property) <-
  c("Property_ID", "Host_ID", "Listing_Title", "Property_Type",
    "Listing_Type", "Created", "Scraped", "Country", "State", "City",
    "Zipcode", "Neighborhood", "MSA", "Currency_Native",
    "Average_Daily_Rate_(USD)", "Average_Daily_Rate_(Native)",
    "Annual_Revenue_LTM_(USD)", "Annual_Revenue_LTM_(Native)",
    "Occupancy_Rate_LTM", "Number_of_Bookings_LTM", "Number_of_Reviews",
    "Bedrooms", "Bathrooms", "Max_Guests", "Calendar_Last_Updated",
    "Response_Rate", "Response_Time_(min)", "Superhost",
    "Cancellation_Policy", "Security_Deposit_(USD)",
    "Security_Deposit_(Native)", "Cleaning_Fee_(USD)",
    "Cleaning_Fee_(Native)", "Extra_People_Fee_(USD)",
    "Extra_People_Fee_(Native)", "Published_Nightly_Rate_(USD)",
    "Published_Monthly_Rate_(USD)", "Published_Weekly_Rate_(USD)",
    "Check-in_Time", "Checkout_Time", "Minimum_Stay",
    "Count_Reservation_Days_LTM", "Count_Available_Days_LTM",
    "Count_Blocked_Days_LTM", "Number_of_Photos", "Business_Ready",
    "Instantbook_Enabled", "Listing_URL", "Listing_Main_Image_URL",
    "Latitude", "Longitude", "Overall Rating")

property <- property[,c(1:7,50:51)]
property <- arrange(property, .data$Property_ID) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(3347)

# add housing field
housing_names <- c(
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
  "Entire casa particular", "")

property <-
  property %>%
  mutate(Housing = ifelse(
    .data$Property_Type %in% housing_names, TRUE, FALSE))

## import daily file
daily <- read_csv("Data/Montreal_daily.csv")
names(daily) <- c("Property_ID", "Date", "Status", "Booked_Date", "Price_USD",
                  "Reservation_ID")

daily$Date <- as.Date(daily$Date)
daily$Booked_Date <- as.Date(daily$Booked_Date)
daily <- daily[,c(1:3, 5)]
daily <- arrange(daily, .data$Property_ID, .data$Date)

# make sure listings are present in both tables
property <- filter(property, .data$Property_ID %in% daily$Property_ID)
daily <- filter(daily, .data$Property_ID %in% property$Property_ID)

## intersect montreal listings with the plateau buffer
plateau_property <- property[lengths(st_within(property, plateau_buff))>0,]

## find plateau listings active in 2018
start_date <- "2018-01-01" %>% 
  as.Date()

end_date <- "2018-12-31" %>% 
  as.Date()

  plateau_property <- filter(plateau_property, .data$Scraped >= start_date)
  daily <- filter(daily, .data$Date >= start_date)

  plateau_property <- filter(plateau_property, .data$Created <= end_date)
  daily <- filter(daily, .data$Date <= end_date)

# import legal permitted plateau listings
plateau_permit <- read_csv("Data/plateau_legal.csv") 
names (plateau_permit) <- c("ETBL_ID", "Property_ID", "Host_ID")

# add a permit column
plateau_property$Permit <- plateau_property$Property_ID %in% plateau_permit$Property_ID

# add quebec establishment ID
plateau_property <- left_join(plateau_property, plateau_permit)

# join property and daily file
plateau_daily <- 
  plateau_property %>%
  inner_join(daily, ., by = "Property_ID")

# find those frequently rented/available 
available <- plateau_daily %>% 
            group_by(Host_ID, Property_ID) %>% 
            count(Status == "A")
names(available) <- c("Host_ID", "Property_ID", "Available", "n_available")
available <-
  filter(available, Available == TRUE) %>% 
  select(-c(3))

reserved <- plateau_daily %>% 
            group_by(Host_ID, Property_ID) %>% 
            count(Status == "R")
names(reserved) <- c("Host_ID", "Property_ID", "Reserved", "n_reserved")
reserved <-
  filter(reserved, Reserved == TRUE) %>% 
  select(-c(3))

frequent <- merge(available, reserved, by = c("Property_ID", "Host_ID"), all = TRUE)
rm(available, reserved)

frequent <- frequent %>% 
  filter(n_available >=183 & n_reserved >= 90) 

plateau_property$Frequent <- plateau_property$Property_ID %in% frequent$Property_ID

# entire home multi-listings
listing_type <- "Entire home/apt"
plateau_daily <- plateau_daily %>% 
    group_by(Listing_Type, Host_ID, Date) %>% 
    mutate(ML = ifelse(
    n() >= 2 & !! listing_type == "Entire home/apt", TRUE, FALSE)) %>% 
    ungroup()

multilistings <- select(plateau_daily, c(1,15)) %>% 
  distinct()

multilistings <- filter(multilistings, ML == TRUE)

plateau_property$ML <- plateau_property$Property_ID %in% multilistings$Property_ID

rm(multilistings)

# least frequently rented multi-listing
multilistings_available <- plateau_daily %>% 
  filter(ML == TRUE) %>% 
  group_by(Host_ID, Property_ID) %>% 
  count(Status == "A") 
names(multilistings_available) <- c("Host_ID", "Property_ID", "Available", "n_available")
multilistings_available <-
  filter(multilistings_available, Available == TRUE) %>% 
  select(-c(3))

multilistings_reserved <- plateau_daily %>% 
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

multilistings <- select(multilistings, -c(3,4))

multilistings_primary <- multilistings %>% 
  inner_join(multilistings %>% 
               group_by(Host_ID) %>% 
               summarise(n = min(n_available_reserved))) %>% 
  select(c(1,2)) %>% 
  group_by(Host_ID) %>% 
  sample_n(1)

plateau_property$ML_primary <- plateau_property$Property_ID %in% multilistings_primary$Property_ID

# private rooms / ghost hotels

# determine if legal using the following variables: permit, frequent, ML, ML_primary, GH
plateau_property <- 
  plateau_property %>% 
  mutate(Legal = case_when(
    Permit == TRUE ~ TRUE,
    Frequent == TRUE ~ FALSE,
    ML_primary == TRUE ~ TRUE,
    ML == TRUE ~ FALSE,
    Frequent == FALSE & ML == FALSE ~ TRUE)) %>% 
    select(c(1:8, 10, 15, 9, 12:14, 11))

# evaluate those active dec 31, 2018
start_date <- "2018-12-31" %>% 
  as.Date()
end_date <- "2018-12-31" %>% 
  as.Date()
plateau_property <- filter(plateau_property, .data$Created <= end_date)
plateau_property <- filter(plateau_property, .data$Scraped >= start_date)

plateau_property %>% 
  filter(Legal == TRUE)

