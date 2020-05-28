

###### Unused code

## If we do not have "country" as an input on member sign-up form (which I think we should have)
## we can get countries from Timezones as described below:

# For subsetting, the follwing countries, I'm using the time zone as a proxy
# of where the chapter is located
# unique(rladies_groups$timezone)
# unique(sub(pattern = "\\/.*", "", unique(rladies_groups$timezone)))
# [1] "Europe"    "America"   "Australia" "Africa"    
#  "Canada"    "Asia"      "US"        "Pacific" 
# Please note that Auckland has TZ "Pacific/Auckland"

##### 
##Not doing this now
#groups_usa %>% 
#mutate(dt_created = substr(created, 1, 10)) %>% 
#arrange(desc(dt_created)) %>% 
#select("city", "state", "country", "dt_created", "members")

#####


##### DELETE IN THE FUTURE

# meetup groups ----------------------------------------------------------
#api_key <- Sys.getenv("meetup_key")

# this is needed when you deploy the shiny app

# api_key <- readRDS("meetup_key.RDS")
#all_rladies_groups <- find_groups(text = "r-ladies", api_key = api_key)

# Cleanup
#rladies_groups <- all_rladies_groups[grep(pattern = "rladies|r-ladies|r ladies", 
#                                          x = all_rladies_groups$name,
#                                          ignore.case = TRUE), ]

##### 


#canada <- sort(unique(rladies_groups[grep("Canada", rladies_groups$timezone),]$country))
#created_canada <- groups_canada %>% 
#  mutate(dt_created = substr(created, 1, 10)) %>% 
#  arrange(desc(dt_created)) %>% 
#  select("city", "state", "country", "dt_created", "members")

#latam <- sort(unique(rladies_groups[grep("America", rladies_groups$timezone),]$country))
#created_latam <- groups_latam %>% 
#  mutate(dt_created = substr(created, 1, 10)) %>% 
#  arrange(desc(dt_created)) %>% 
#  select("city", "state", "country", "dt_created", "members")

#europe <- sort(unique(rladies_groups[grep("Europe", rladies_groups$timezone),]$country))
# Add Tbilisi to the Europe group
# for some reason Tbilisi uses the timezone `Asia/Tbilisi` therefore
# it got categorized as Asia. However, Tbilisi is located in Europe
#groups_europe <- rbind(groups_europe,
#                       rladies_groups %>% filter(city == "Tbilisi"))

#created_europe <- groups_europe %>% 
#  mutate(dt_created = substr(created, 1, 10)) %>% 
#  arrange(desc(dt_created)) %>% 
#  select("city", "state", "country", "dt_created", "members")


#africa <- sort(unique(rladies_groups[grep("Africa", rladies_groups$timezone),]$country))

#created_africa <- groups_africa %>% 
#  mutate(dt_created = substr(created, 1, 10)) %>% 
#  arrange(desc(dt_created)) %>% 
#  select("city", "state", "country", "dt_created", "members")

#asia <- sort(unique(rladies_groups[grep("Asia", rladies_groups$timezone),]$country))
# %>% 
#  filter(city != "Tbilisi") # Remove Tbilisi from the Asia group
#created_asia <- groups_asia %>% 
#  mutate(dt_created = substr(created, 1, 10)) %>% 
#  arrange(desc(dt_created)) %>% 
#  select("city", "state", "country", "dt_created", "members")

#australia <- sort(unique(rladies_groups[grep("Australia|Pacific/Auckland", rladies_groups$timezone),]$country))
#created_australia <- groups_australia %>% 
#  mutate(dt_created = substr(created, 1, 10)) %>% 
#  arrange(desc(dt_created)) %>% 
#  select("city", "state", "country", "dt_created", "members")