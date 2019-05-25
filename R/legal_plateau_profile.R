############### PROFILE OF LEGAL PLATEAU LISTINGS ##############################

# Filter for legal listings 
plateau_legal <- filter(property, Legal == TRUE)

### Data Analysis
## 1 Operators

# 1A Number with 2+ listings
host_distinct <- count(plateau_legal, Host_ID)

host_2 <- host_distinct %>% filter(n >= 2)

# 1B Number with 10+ listings
host_11 <- host_distinct %>% filter(n > 10)

# 1C Average number of listings per host
summarise(host_2, avg = mean(n))

# 1D Max number of listings per host
summarise(host_2, max = max(n))

ggplot(data = host_2, aes(host_2$n)) + 
  geom_histogram(binwidth = 1)

# 1E Non-human names
# Not sure how to do this

# 1F Proportion of Superhosts
plateau_legal <- as_tibble(plateau_legal)

superhost <- plateau_legal %>% 
  distinct(Host_ID, .keep_all = TRUE)

superhost <- superhost %>% count(Superhost)

# 1G Response rates and times
summarise(plateau_legal, AvgRR = mean(Response_Rate))

mean(plateau_legal$Response_Time_min, na.rm = TRUE)

ggplot(data = plateau_legal, aes(plateau_legal$Response_Time_min)) +
  geom_histogram(binwidth = 5)

filter(plateau_legal, Response_Time_min <= 5)


## 2 Income

# 2A Average listing price
mean(plateau_legal$Average_Daily_Rate_USD, na.rm = TRUE)

# 2B Average LTM income per listing
mean(plateau_legal$Annual_Revenue_LTM_USD, na.rm = TRUE)

# 2C Average LTM income per host
plateau_legal_by_Host <- plateau_legal %>%
  group_by(Host_ID) %>% 
  summarise(AvgAnnual = mean(Annual_Revenue_LTM_USD))

ggplot(plateau_legal_by_Host) +
  geom_point(mapping = aes(x = Host_ID, y = AvgAnnual), 
           stat = "identity")

mean(plateau_legal_by_Host$AvgAnnual, na.rm = TRUE)

# 2D Top earner LTM
plateau_legal_by_Host %>% 
  summarise(maxAnnual = max(AvgAnnual))


## 3 Listing type breakdown
listing_type <- plateau_legal %>% count(Listing_Type)

listing_type <- mutate(listing_type, prop = n/210)


## 4 Availability
# 4A Average number of bookings 
mean(plateau_legal$Number_of_Bookings_LTM, na.rm = TRUE)

# 4B Average days R/A/B
AvgR <- mean(plateau_legal$Count_Reservation_Days_LTM, na.rm = TRUE)
AvgA <- mean(plateau_legal$Count_Available_Days_LTM, na.rm = TRUE)
AvgB <- mean(plateau_legal$Count_Blocked_Days_LTM, na.rm = TRUE)

AvgOcc <- mean(plateau_legal$Occupancy_Rate_LTM, na.rm = TRUE)

count(plateau_legal, Occupancy_Rate_LTM == 0)


## 5 Age of postings 
plateau_legal <- mutate(plateau_legal, 
                        Year_Created = substr(plateau_legal$Created, 1, 4))

year_created <- plateau_legal %>% 
  count(Year_Created) 

ggplot(year_created) +
  geom_histogram(mapping = aes(x = Year_Created, y = n), 
                               bins = 7, stat = "identity")

