############### DATA IMPORT ##################

source("R/01 helper_functions.R")

## import plateau dataset

montreal <- read_sf(dsn = ".", layer = "plateau") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(3347)
View(montreal)

plateau <- filter(montreal, CODEID=="5")

plateau_buff <-
  plateau %>%
  summarize(geometry = st_union(geometry)) %>% 
  st_buffer(200)

plot(plateau$geometry)

plot (plateau_buff$geometry, add = TRUE)

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
property <- arrange(property, .data$Property_ID)

## import daily file
daily <- read_csv("Data/Montreal_daily.csv")

## intersect montreal listings with the plateau buffer
plateau_listings <- property_file[lengths(st_within(property, plateau_buff))>0,]

