#load in required libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(readxl)
library(fuzzyjoin)

#load in search data for 2020
setwd("~/Desktop/Thesis/Data/State Data/")
files <-list.files(pattern = "*.csv") 
for (i in 1:length(files)){
  temp_data <- read.delim(files[i], header = TRUE) 
  temp_data <- separate(temp_data, names(temp_data[1]), into= c("Date", "Toilet paper", "Hand Sanitizer", "Shoes",  "Mask"), sep = ",")
  #extract region, create region column
   region <- sub("\\(", "", sub("\\)", "",sub("Mask: ", "", temp_data[1,2])))
   region <- sub("mask: ","", region)
   region <- as.data.frame(rep(region, 53 ))
   temp_data <- cbind(region, temp_data)
   names(temp_data) <- c("region", "date", "toilet_paper", "hand_sanitizer", "shoes", "mask")
  temp_data <- temp_data[-1,]
  #bind search_data together
  if(i==1){
    search_data <- temp_data
  }else{
    search_data <- rbind(search_data, temp_data) 
  }
}
#change types of total data
total_data <- search_data %>% mutate(
  region = as.factor(region), 
  date = as.Date(date),
)


#load in data for price gouging searches 
setwd("~/Desktop/Thesis/Data/Price Gouging Searches/")
files1 <-list.files(pattern = "*.csv") 
for (i in 1:length(files)){
  temp_data1 <- read.delim(files1[i], header = TRUE) 
  temp_data1 <- separate(temp_data1, names(temp_data1[1]), into= c("Date", "Price Gouging"), sep = ",")
  #extract region, create region column
  region <- sub("\\(", "", sub("\\)", "",sub("price gouging: ", "", temp_data1[1,2])))
  region <- sub("mask: ","", region)
  region <- as.data.frame(rep(region, 53 ))
  temp_data1 <- cbind(region, temp_data1)
  names(temp_data1) <- c("region", "date", "price_gouging")
  temp_data1 <- temp_data1[-1,]
  #bind search_data together
  if(i==1){
    search_data1 <- temp_data1
  }else{
    search_data1 <- rbind(search_data1, temp_data1) 
  }
}
#change types of total data
search_data1 <- search_data1 %>% mutate(
  region = as.factor(region), 
  date = as.Date(date),
)

#add to total data 
total_data <- left_join(total_data, search_data1)

 #add in whether state has anti-gouging law 
gouging_states <- c("Alabama", "Arkansas", "California", "Connecticut", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Massachusetts", "Michigan", "Mississippi", "Missouri",  "New Jersey", "New York", "North Carolina", "Oklahoma", "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "West Virginia", "Wisconsin")
total_data <- total_data %>%
  mutate(anti_gouging_law = if_else(region %in% gouging_states, 1, 
    if_else(region == "Colorado" & date > "2020-07-14",1, if_else(region == "Alaska" & date > "2020-04-10",1,0 )))) 
#add in state emergency dates
total_data <- total_data %>% mutate(
  state_emergency_date = ifelse(region == "Alabama", "2020-03-13",
    ifelse(region == "Alaska", "2020-03-11",
      ifelse(region == "Arizona", "2020-03-11",
        ifelse (region == "Arkansas", "2020-03-11",
          ifelse( region == "California", "2020-03-04", 
            ifelse(region == "Colorado", "2020-03-10",
              ifelse( region == "Connecticut", "2020-03-10",
                ifelse( region == "Delaware", "2020-03-12",
                  ifelse(region == "District of Columbia", "2020-03-11",
                    ifelse( region == "Florida", "2020-03-01",
                      ifelse(region == "Georgia", "2020-03-14",
                        ifelse(region == "Hawaii", "2020-03-04",
                          ifelse(region == "Idaho", "2020-03-13",
                            ifelse( region == "Illinois", "2020-04-01",
                              ifelse( region == "Indiana", "2020-03-03",
                                ifelse(region == "Iowa", "2020-03-09",
                                  ifelse(region == "Kansas", "2020-03-12",
                                    ifelse(region == "Kentucky", "2020-03-07",
                                      ifelse(region == "Louisiana", "2020-03-11",
                                        ifelse(region=="Maine", "2020-03-15",
                                          ifelse(region == "Maryland", "2020-03-05",
                                            ifelse(region == "Massachusetts", "2020-03-10",
                                              ifelse(region == "Michigan", "2020-03-10",
                                                ifelse(region == "Minnesota", "2020-03-13",
                                                  ifelse(region == "Mississippi", "2020-03-14",
                                                    ifelse(region == "Missouri", "2020-03-13",
                                                      ifelse(region == "Montana", "2020-03-12",
                                                        ifelse(region == "Nebraska", "2020-03-13",
                                                          ifelse(region == "Nevada", "2020-03-12",
                                                            ifelse(region == "New Hampshire", "2020-03-13",
                                                              ifelse(region == "New Jersey", "2020-03-09",
                                                                ifelse(region == "New Mexico", "2020-03-11",
                                                                  ifelse(region == "New York", "2020-03-07",
                                                                    ifelse(region == "North Carolina", "2020-03-10",
                                                                      ifelse(region == "North Dakota", "2020-03-13",
                                                                        ifelse(region =="Ohio",  "2020-03-14",
                                                                          ifelse(region == "Oklahoma", "2020-03-14", 
                                                                            ifelse(region == "Oregon", "2020-03-08",
                                                                              ifelse(region == "Pennsylvania", "2020-03-06",
                                                                                ifelse(region == "Rhode Island", "2020-03-09",
                                                                                  ifelse(region == "South Carolina", "2020-03-13",
                                                                                    ifelse(region == "Tennessee", "2020-03-12", 
                                                                                      ifelse(region == "Texas", "2020-03-13",
                                                                                        ifelse(region == "Utah", "2020-03-06",
                                                                                          ifelse(region == "Vermont", "2020-03-13",
                                                                                            ifelse(region == "Virginia", "2020-03-12",
                                                                                              ifelse(region == "Washington", "2020-02-19",
                                                                                                ifelse(region == "West Virginia", "2020-03-12",
                                                                                                  ifelse(region == "Wisconsin", "2020-03-12", "2020-03-13"))))))))))))))))))))))))))))))))))))))))))))))))),
  state_emergency = ifelse(date >= state_emergency_date, 1,0)) %>%
  select(-state_emergency_date)


#load in Google Mobility data
setwd("~/Desktop/Thesis/Data/Google Mobility Data/")
temp_mobility <- read_csv("2020_US_Region_Mobility_Report.csv")
mobility_data <- temp_mobility %>% filter(is.na(sub_region_2))%>%
  select(sub_region_1, date, retail_and_recreation_percent_change_from_baseline, grocery_and_pharmacy_percent_change_from_baseline, parks_percent_change_from_baseline, transit_stations_percent_change_from_baseline, workplaces_percent_change_from_baseline, residential_percent_change_from_baseline ) %>%
  filter(!is.na(sub_region_1), date %in% unique(total_data$date))%>%
  rename(region = sub_region_1) 

#add to total data
total_data <- left_join(total_data, mobility_data)

#add in US cases data 
setwd("~/Desktop/Thesis/Data/Us cases/")
temp_cases <- read_csv("uscasesbystate.csv")
us_cases <- temp_cases %>% mutate(
  date = as.Date(paste(year,month,day), format = "%Y%m%d"))
us_cases <- us_cases %>% mutate(
  state = ifelse(statefips == 1, "Alabama",
    ifelse(statefips ==2, "Alaska",
      ifelse(statefips == 4, "Arizona",
        ifelse(statefips == 5, "Arkansas",
          ifelse(statefips == 6, "California",
            ifelse(statefips == 8, "Colorado",
              ifelse(statefips == 9, "Connecticut",
                ifelse(statefips == 10, "Delaware",
                  ifelse(statefips == 11, "District of Columbia",
                    ifelse(statefips == 12, "Florida",
                      ifelse(statefips == 13, "Georgia",
                        ifelse(statefips == 15, "Hawaii",
                          ifelse(statefips == 16, "Idaho",
                            ifelse(statefips == 17, "Illinois",
                              ifelse(statefips == 18, "Indiana", 
                                ifelse(statefips == 19, "Iowa",
                                  ifelse(statefips == 20, "Kansas",
                                    ifelse(statefips == 21, "Kentucky",
                                      ifelse(statefips == 22, "Louisiana",
                                        ifelse(statefips == 23, "Maine",
                                          ifelse(statefips == 24, "Maryland", 
                                            ifelse(statefips == 25,"Massachusetts",
                                              ifelse(statefips == 26,"Michigan",
                                                ifelse(statefips == 27, "Minnesota",
                                                  ifelse(statefips == 28, "Mississippi",
                                                    ifelse(statefips == 29, "Missouri",
                                                      ifelse(statefips ==30, "Montana",
                                                        ifelse(statefips == 31, "Nebraska",
                                                          ifelse(statefips == 32, "Nevada",
                                                            ifelse(statefips == 33, "New Hampshire",
                                                              ifelse(statefips == 34, "New Jersey",
                                                                ifelse (statefips == 35, "New Mexico",
                                                                  ifelse(statefips == 36, "New York",
                                                                    ifelse(statefips == 37, "North Carolina",
                                                                      ifelse(statefips == 38, "North Dakota",
                                                                        ifelse(statefips == 39, "Ohio",
                                                                          ifelse(statefips == 40, "Oklahoma",
                                                                            ifelse(statefips == 41, "Oregon",
                                                                              ifelse(statefips == 42, "Pennsylvania",
                                                                                ifelse(statefips == 44, "Rhode Island",
                                                                                  ifelse(statefips == 45, "South Carolina",
                                                                                    ifelse(statefips == 46, "South Dakota",
                                                                                      ifelse(statefips == 47, "Tennessee",
                                                                                        ifelse(statefips == 48, "Texas",
                                                                                          ifelse(statefips == 49, "Utah",
                                                                                            ifelse(statefips == 50, "Vermont",
                                                                                              ifelse(statefips == 51, "Virginia",
                                                                                                ifelse(statefips == 53, "Washington", "NA"
                                                                                                )))))))))))))))))))))))))))))))))))))))))))))))))
us_cases2<- us_cases%>% mutate(
  state = ifelse(statefips == 54, "West Virginia",
    ifelse(statefips == 55, "Wisconsin", 
      ifelse(statefips == 56, "Wyoming", state)))
) %>% select(-year, -month, -day, -statefips) %>% 
  rename(region = state) 


#add to total data
total_data <- left_join(total_data, us_cases2)


#add in us census data 
setwd("~/Desktop/Thesis/Data/Us census data")
temp_demographics <- read.csv("ACS demographic 2019.csv") 

estimates <-temp_demographics%>%
  select(Label, ends_with("Estimate")) %>%
  gather(State, Estimate, -Label) %>%
  mutate(State = gsub("..Estimate", "", State))

percentages <- temp_demographics %>% 
  select(Label, ends_with("Percent")) %>%
  gather(State, Percentage, -Label) %>%
  mutate(State = gsub("..Percent", "", State))

demographics <- left_join(estimates, percentages, by = c("Label", "State"))
demographics$Label <- str_trim(demographics$Label)
demographic_categories <- c("Total population", "Median age (years)", "Total housing units", "Citizen, 18 and over population", "Hispanic or Latin (of any race)", "White alone", "Black or African American alone",  "American Indian and Alaska Native alone", "Asian alone", "Native Hawaiian and Other Pacific Islander alone")
demographics2 <- demographics %>% 
  mutate(
    Label = as.character(demographics$Label),
    State = gsub("\\.", " ", State)) %>%
  filter(Label %in% demographic_categories) %>%
  unique() %>% 
  rename()
demographics_estimates <- demographics2 %>%
  select(-Percentage) %>%
  spread(Label, Estimate) %>%
  select(State, 'Citizen, 18 and over population', 'Median age (years)', 'Total housing units', 'Total population')
demographics_percentages <- demographics2   %>%
  select (-Estimate) %>%
  spread(Label, Percentage ) %>%
  select(-'Citizen, 18 and over population', -'Median age (years)', -'Total housing units', -'Total population')
demographics_to_merge <- left_join(demographics_estimates, demographics_percentages, by = "State")
demographics_to_merge <- demographics_to_merge %>%
  filter(State != "Puerto Rico") %>%
  rename(region = State)
#add to total data
total_data <- left_join(total_data, demographics_to_merge)

#add in us broadband access 
setwd("~/Desktop/Thesis/Data/Broadband Access")
temp_broadband <- read_excel("broadbandaccess.xls") 
broadband <- temp_broadband[,c(1,14)]
names(broadband) <- c("region", "computer_internet_access")
broadband <- broadband %>% 
  drop_na() %>%
  mutate (
  region = trimws(gsub("\\.", "", region), "right")
) %>%
  slice(-1:-2)
#join to total data
total_data <- total_data %>%
  regex_left_join(broadband) %>%
  select(-region.y) %>%
  rename(region = region.x)


#rename columns 
#replace missing values with 0
final_data <- total_data %>%
  mutate(retail_and_recreation_percent_change_from_baseline = replace_na(retail_and_recreation_percent_change_from_baseline,0), 
    grocery_and_pharmacy_percent_change_from_baseline = replace_na(grocery_and_pharmacy_percent_change_from_baseline,0), 
    parks_percent_change_from_baseline = replace_na(parks_percent_change_from_baseline,0),
    transit_stations_percent_change_from_baseline = replace_na(transit_stations_percent_change_from_baseline,0),
    workplaces_percent_change_from_baseline= replace_na(workplaces_percent_change_from_baseline,0), 
    residential_percent_change_from_baseline = replace_na(residential_percent_change_from_baseline,0)) %>%
  rename(total_population = "Total population",
    india_alaska = "American Indian and Alaska Native alone",
    asian = "Asian alone",
    black = "Black or African American alone",
    pacific = "Native Hawaiian and Other Pacific Islander alone",
    white = "White alone",
    voting_pop = "Citizen, 18 and over population",
    median_age = "Median age (years)",
    total_housing_units = "Total housing units")

#change values of variables 
final_data <- final_data %>% mutate (
  shoes = as.numeric(gsub("<1", "0", shoes)),
  mask = as.numeric(mask),
  toilet_paper = as.numeric(toilet_paper),
  hand_sanitizer = as.numeric(gsub("<1", "0", hand_sanitizer)),
  price_gouging = as.numeric(price_gouging),
  case_count = as.numeric(case_count),
  test_count = as.numeric(test_count),
  death_count = as.numeric(death_count),
  case_rate = as.numeric(case_rate),
  death_rate = as.numeric(death_rate),
  test_rate = as.numeric(test_rate),
  new_case_rate = as.numeric(new_case_rate),
  new_death_rate = as.numeric(new_death_rate),
  new_test_rate = as.numeric(new_test_rate),
  new_case_count = as.numeric(new_case_count),
  new_death_count = as.numeric(new_death_count),
  new_test_count = as.numeric(new_test_count),
  voting_pop = as.numeric(gsub(",", "", voting_pop)),
  median_age = as.numeric(median_age),
  total_housing_units = as.numeric(gsub(",", "", total_housing_units)),
  total_population = as.numeric(gsub(",", "", total_population)),
  india_alaska = as.numeric(gsub("%", "", india_alaska)),
  asian = as.numeric(gsub("%", "", asian)),
  black = as.numeric(gsub("%", "", black)),
  pacific = as.numeric(gsub("%", "", pacific)),
  white = as.numeric(gsub("%", "", white)),
  case_rate = replace_na(case_rate, 0),
  computer_internet_access = as.numeric(computer_internet_access)
)


