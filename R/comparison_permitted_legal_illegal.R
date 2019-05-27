############### PROFILE OF LEGAL PLATEAU LISTINGS ##############################

# Sort into 3 groups
permitted <- property %>% filter(Permit == TRUE) 
permitted_hosts <- permitted %>% count(Host_ID)

legal <- property %>% filter(Legal == TRUE)
legal_hosts <- legal %>% count(Host_ID)

illegal <- property %>% filter(Legal == FALSE)
illegal_hosts <- illegal %>% count(Host_ID)

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

## 2 Income (LEAVING OUT)

# 2A Average listing price
permitted %>% mean(Average_Daily_Rate_USD, na.rm = TRUE)

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
permitted %>% 
  ungroup() %>% 
  count(Listing_Type)

legal %>% 
  ungroup() %>% 
  count(Listing_Type)

illegal %>% 
  ungroup() %>% 
  count(Listing_Type)

## 4 Availability
# 4A Average days reserved
permitted %>% 
  ungroup() %>% 
  summarise(avg = mean(n_reserved))

legal %>% 
  ungroup() %>% 
  summarise(avg = mean(n_reserved))

illegal %>% 
  ungroup() %>% 
  summarise(avg = mean(n_reserved))

# 4B Average days reserved + available
permitted %>% 
  ungroup() %>% 
  summarise(avg = mean(n_available))

legal %>% 
  ungroup() %>% 
  summarise(avg = mean(n_available))

illegal %>% 
  ungroup() %>% 
  summarise(avg = mean(n_available))
