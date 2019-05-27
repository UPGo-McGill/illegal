############### PROFILE OF LEGAL PLATEAU LISTINGS ##############################

# Sort into 3 groups
permitted <- property %>% filter(Permit == TRUE) 
permitted_hosts <- permitted %>% count(Host_ID)

legal <- property %>% filter(Legal == TRUE)
legal_hosts <- legal %>% count(Host_ID)

illegal <- property %>% filter(Legal == FALSE)
illegal_hosts <- illegal %>% count(Host_ID)

### Data Analysis
## 1 Operators

# 1A Number with 2+ listings
permitted_hosts %>% filter(n >= 2)

legal_hosts %>% filter(n >= 2)

illegal_hosts %>% filter(n >= 2)

# 1B Number with 11+ listings
permitted_hosts %>% filter(n > 10)

legal_hosts %>% filter(n > 10)

illegal_hosts %>% filter(n > 10)

# 1C Average number of listings per host
permitted_hosts %>% 
  summarise(total = sum(n)) %>% 
  summarise(avg = mean(total))

legal_hosts %>% 
  summarise(total = sum(n)) %>% 
  summarise(avg = mean(total))

illegal_hosts %>% 
  summarise(total = sum(n)) %>% 
  summarise(avg = mean(total))

# 1D Max number of listings per host
permitted_hosts %>% 
  arrange(desc(n))

legal_hosts %>% 
  arrange(desc(n))

illegal_hosts %>% 
  arrange(desc(n))

#ggplot(data = host_2, aes(host_2$n)) + 
  #geom_histogram(binwidth = 1)

# 1E Proportion of Superhosts (LEAVING OUT FOR NOW)
permitted %>% 
  as_tibble() %>% 
  distinct(Host_ID, .keep_all = TRUE) %>% 
  filter(Superhost == TRUE)

legal %>% 
  as_tibble() %>% 
  distinct(Host_ID, .keep_all = TRUE) %>% 
  filter(Superhost == TRUE)

illegal %>% 
  as_tibble() %>% 
  distinct(Host_ID, .keep_all = TRUE) %>% 
  filter(Superhost == TRUE)

# 1G Response rates and times (LEAVING OUT)
permitted %>% 
  summarise(AvgRR = mean(Response_Rate))

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

