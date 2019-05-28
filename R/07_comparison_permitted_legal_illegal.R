############### PROFILE OF LEGAL PLATEAU LISTINGS ##############################

# Sort into 3 groups
permitted <- property %>% filter(Permit == TRUE) 
permitted_hosts <- permitted %>% count(Host_ID)

legal <- property %>% filter(Legal == TRUE & Permit == FALSE)
legal_hosts <- legal %>% count(Host_ID)

illegal <- property %>% filter(Legal == FALSE)
illegal_hosts <- illegal %>% count(Host_ID)

## 1 Operators

# 1A Number with 2+ listings
permitted_hosts %>% filter(n > 1)

legal_hosts %>% filter(n > 1)

illegal_hosts %>% filter(n > 1)

# 1B Number with 11+ listings
permitted_hosts %>% filter(n > 10)

legal_hosts %>% filter(n > 10)

illegal_hosts %>% filter(n > 10)

# 1C Average number of listings per host
permitted_hosts %>% 
  ungroup() %>% 
  summarise(avg = mean(n))

legal_hosts %>% 
  ungroup() %>% 
  summarise(avg = mean(n))

illegal_hosts %>% 
  ungroup() %>% 
  summarise(avg = mean(n))

# 1D Max number of listings per host
permitted_hosts %>% 
  arrange(desc(n))

legal_hosts %>% 
  arrange(desc(n))

illegal_hosts %>% 
  arrange(desc(n))


## 2 Income

# 2A LTM income for each sub-group
permitted_income <- 
  permitted %>% 
  ungroup() %>% 
  summarise(total = sum(revenue))

permitted_income <- permitted_income$total

permitted_income/nrow(permitted)

legal_income <- 
  legal %>% 
  ungroup() %>% 
  summarise(total = sum(revenue))

legal_income <- legal_income$total

legal_income/nrow(legal)

illegal_income <- 
  illegal %>% 
  ungroup() %>% 
  summarise(total = sum(revenue))

illegal_income <- illegal_income$total

illegal_income/nrow(illegal)

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
