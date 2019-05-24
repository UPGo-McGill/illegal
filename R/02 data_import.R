############### DATA IMPORT ##################

source("R/01 helper_functions.R")

## import plateau dataset

montreal <- read_sf(dsn = ".", layer = "plateau") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(3347)

plateau <- filter(montreal, CODEID=="5")

plateau_buff <-
  plateau %>%
  summarize(geometry = st_union(geometry)) %>% 
  st_buffer(200)

plot (plateau_buff$geometry)
plot(plateau$geometry, add = TRUE)

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
  
plot(plateau_property$geometry)
plot(plateau_buff, add = TRUE)

# import quebec permit file
# quebec_permits <- read_csv("Data/x.csv")

# import legal plateau listings
  ## will have to update if we change the google doc, just adding in now so we can start 
  ## removing listings and checking our work as we go

plateau_legal <- read_csv("Data/plateau_legal.csv") 
names (plateau_legal) <- c("ETBL_ID", "Property_ID", "Host_ID")

# add a legal column
plateau_property$Legal <- plateau_property$Property_ID %in% plateau_legal$Property_ID
plateau_property$Legal <- ifelse(plateau_property$Legal == TRUE, "Yes", "Unsure")

# add quebec establishment ID
plateau_property <- left_join(plateau_property, plateau_legal)

# join property and daily file
plateau_daily <- 
  plateau_property %>%
  inner_join(daily, ., by = "Property_ID")

# any property rented a lot - illegal

# determine entire home multi-listings
listing_type <- "Entire home/apt"
plateau_daily <- plateau_daily %>% 
    group_by(Listing_Type, Host_ID, Date) %>% 
    mutate(ML = ifelse(
    n() >= 2 & !! listing_type == "Entire home/apt", TRUE, FALSE)) %>% 
    ungroup()

# the least frequently rented entire home multi listing is legal, the remainder are illegal
multilistings <- plateau_daily %>% 
    filter(Legal == FALSE, ML == TRUE) %>% 
    group_by(Host_ID, Property_ID) %>% 
    count(Status == "B") 

names(multilistings) <- c("Host_ID", "Property_ID", "Blocked", "n")

# as some operators have multiple listings blocked for the same number of days, take one value
  # from each grouping
multilistings_legal <- multilistings %>% 
  filter(Blocked == TRUE) %>% 
  inner_join(multilistings %>% 
               filter(Blocked == TRUE) %>% 
               group_by(Host_ID) %>% 
               summarise(n = max(n))) %>% 
  select(c(1,2)) %>% 
  group_by(Host_ID) %>% 
  sample_n(1)

multilistings <- multilistings %>%
    filter(Blocked == TRUE) %>% 
    select(-c(3,4))

multilistings$Legal <- multilistings$Property_ID %in% multilistings_legal$Property_ID

# private rooms / ghost hotels - illegal
