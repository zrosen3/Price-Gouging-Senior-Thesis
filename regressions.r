setwd("~/Desktop/Thesis/Data/Data Processing")
source("dataprocessing.R")
relevant_variables <- final_data %>% select(region, date, hand_sanitizer, toilet_paper, mask, shoes, price_gouging, region, anti_gouging_law, state_emergency,  grocery_and_pharmacy_percent_change_from_baseline, case_rate, median_age, total_population, white, computer_internet_access) %>%
 mutate(
   date = as.factor(date),
   region = as.factor(region),
  grocery_and_pharmacy_percent_change_from_baseline = grocery_and_pharmacy_percent_change_from_baseline/10,
   case_rate = case_rate/1000,
   median_age = median_age/100,
   total_population = total_population/10000000,
   white = white/100,
   computer_internet_access = computer_internet_access/10)
#differences in differences estimates
#hand sanitizer 
#state and time fixed effects 
hand_regress_state_time <- lm(hand_sanitizer ~  anti_gouging_law + anti_gouging_law * state_emergency + state_emergency +  grocery_and_pharmacy_percent_change_from_baseline + case_rate + total_population + region + date, data = relevant_variables)

#state fixed effects
hand_regress_state <- lm(hand_sanitizer ~  anti_gouging_law + anti_gouging_law * state_emergency  + state_emergency +  grocery_and_pharmacy_percent_change_from_baseline + case_rate + total_population +  region , data = relevant_variables )


#no fixed effects
hand_regress_no_fixed <- lm(hand_sanitizer ~  anti_gouging_law + anti_gouging_law * state_emergency  + state_emergency +  grocery_and_pharmacy_percent_change_from_baseline + case_rate + total_population, data = relevant_variables )

#toilet paper
#state and time fixed effects
toilet_regress_state_time <- lm(toilet_paper ~ anti_gouging_law + anti_gouging_law * state_emergency   + state_emergency + grocery_and_pharmacy_percent_change_from_baseline + case_rate + total_population + region + date, data = relevant_variables)

#state fixed effects
toilet_regress_state <- lm(toilet_paper ~ anti_gouging_law + anti_gouging_law * state_emergency   + state_emergency + grocery_and_pharmacy_percent_change_from_baseline + case_rate  + total_population +  region, data = relevant_variables)


#no fixed effects
toilet_regress_no_fixed <- lm(toilet_paper ~ anti_gouging_law + anti_gouging_law * state_emergency   + state_emergency + grocery_and_pharmacy_percent_change_from_baseline + case_rate + total_population , data = relevant_variables)

#masks 
#state and time fixed effects
mask_regress_state_time<- lm(mask ~ anti_gouging_law + anti_gouging_law * state_emergency   + state_emergency + grocery_and_pharmacy_percent_change_from_baseline + case_rate + total_population +  region + date, data = relevant_variables)

#state fixed effects
mask_regress_state <- lm(mask ~ anti_gouging_law + anti_gouging_law * state_emergency   + state_emergency + grocery_and_pharmacy_percent_change_from_baseline + case_rate +  total_population + region, data = relevant_variables)


#no fixed effects 
mask_regress_no_fixed <- lm(mask ~ anti_gouging_law + anti_gouging_law * state_emergency   + state_emergency + grocery_and_pharmacy_percent_change_from_baseline + case_rate + total_population, data = relevant_variables)

#shoes 
#state and time fixed effects
shoes_regress_state_time <- lm(shoes ~ anti_gouging_law + anti_gouging_law * state_emergency   + state_emergency + grocery_and_pharmacy_percent_change_from_baseline + case_rate + total_population +   region + date, data =  relevant_variables)

#state fixed effects
shoes_regress_state <- lm(shoes ~ anti_gouging_law + anti_gouging_law * state_emergency   + state_emergency + grocery_and_pharmacy_percent_change_from_baseline + case_rate +  total_population +   region, data =  relevant_variables)

#no fixed effects
shoes_regress_no_fixed <- lm(shoes ~ anti_gouging_law + anti_gouging_law * state_emergency   + state_emergency + grocery_and_pharmacy_percent_change_from_baseline + case_rate + total_population, data =  relevant_variables)


#price gouging searches
#state and time fixed effects
gouging_regress_state_time <- lm(price_gouging ~ anti_gouging_law + anti_gouging_law * state_emergency   + state_emergency + grocery_and_pharmacy_percent_change_from_baseline + case_rate + total_population +   region + date, data =  relevant_variables)

#state fixed effects
gouging_regress_state <- lm(price_gouging ~ anti_gouging_law + anti_gouging_law * state_emergency   + state_emergency + grocery_and_pharmacy_percent_change_from_baseline + case_rate +  total_population +   region, data =  relevant_variables)

#no fixed effects
gouging_regress_no_fixed <- lm(price_gouging ~ anti_gouging_law + anti_gouging_law * state_emergency   + state_emergency + grocery_and_pharmacy_percent_change_from_baseline + case_rate + total_population, data =  relevant_variables)



#check plots of regressions
#plot(hand_regress)
#plot(hand_regress_2)
#plot(shoes_regress)
#plot(shoes_regress_2)
#plot(mask_regress)
#plot(mask_regress_2)
#plot(shoes_regress)
#plot(shoes_regress_2)

#check for collinearity
relevant_variables_numeric <- relevant_variables %>%
   select(-region, -date)
table1 <- matrix(NA, ncol(relevant_variables_numeric), ncol(relevant_variables_numeric))
for(i in 1:ncol(relevant_variables_numeric )){
  for(j in 1:ncol(relevant_variables_numeric)){
   a <- cor(relevant_variables_numeric[,i], relevant_variables_numeric[,j], use = "complete.obs")
  table1[i,j] <- a
  }
}
rownames(table1) <- names(relevant_variables_numeric)
colnames(table1) <- names(relevant_variables_numeric)

#summary(hand_regress_state_time)
#summary(hand_regress_state)
#summary(hand_regress_no_fixed)
#summary(toilet_regress_state_time) 
#summary(toilet_regress_state)
#summary(toilet_regress_no_fixed)
#summary(mask_regress_state_time)
#summary(mask_regress_state)
#summary(mask_regress_no_fixed)
#summary(shoes_regress_state_time)
#summary(shoes_regress_state)
#summary(shoes_regress_no_fixed)

