############### PROFILE OF LEGAL PLATEAU LISTINGS ##############################

source("R/01 helper_functions.R")

## Import Plateau shapefile

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

## Import AirBnB property file
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

property <- property[,c(1:7,12, 15, 17, 19:20, 26:28, 42:44, 50:51)]
property <- arrange(property, .data$Property_ID) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(3347)

## Import AirBnB daily file
daily <- read_csv("Data/Montreal_daily.csv")
names(daily) <- c("Property_ID", "Date", "Status", "Booked_Date", "Price_USD",
                  "Reservation_ID")

daily$Date <- as.Date(daily$Date)
daily$Booked_Date <- as.Date(daily$Booked_Date)
daily <- daily[,c(1:3, 5)]
daily <- arrange(daily, .data$Property_ID, .data$Date)

## Intersect Montreal property file with the Plateau buffer
plateau_listings <- property[lengths(st_within(property, plateau_buff))>0,]

## Find Plateau listings active in 2018

start_date <- "2018-01-01" %>% 
  as.Date()

end_date <- "2018-12-31" %>% 
  as.Date()

plateau_listings <- filter(plateau_listings, .data$Scraped >= start_date)
daily <- filter(daily, .data$Date >= start_date)

plateau_listings <- filter(plateau_listings, .data$Created <= end_date)
daily <- filter(daily, .data$Date <= end_date)

plot(plateau_listings$geometry)
plot(plateau_buff, add = TRUE)

## Import file linking permits to airbnb listings
quebec_legal <- read_csv("Data/plateau_legal.csv") 
names (quebec_legal) <- c("ETBL_ID", "Property_ID", "Host_ID")

# Add a "legal" column
plateau_listings$Legal <- plateau_listings$Property_ID %in% quebec_legal$Property_ID

# Add Quebec establishment ID
plateau_listings <- left_join(plateau_listings, quebec_legal)

# Filter for legal listings 
plateau_legal <- filter(plateau_listings, Legal == TRUE)

### Data Analysis
## 1 Operators

# 1A Number with 2+ listings

host_multiple <- count(plateau_legal, Host_ID)
