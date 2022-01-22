#set working directory 
setwd("~/Desktop/Thesis/Data/Data Processing")
#load in regrssions file
source("regressions.R")
 #load in table libraries
library(sjPlot)
#library(sjmisc)
#library(sjlabelled)

#make regression tables 
relevant_coefficients <-  c("(Intercept)", "anti_gouging_law",  "state_emergency", "anti_gouging_law:state_emergency","grocery_and_pharmacy_percent_change_from_baseline", "case_rate") 

predictors <- c("Intercept", "State Anti Price Gouging Law", "State Emergency Declared", "Google Mobility Index for Grocery Stores", "Case Rate", "State Anti Price Gouging Law & State Emergency Declared ")

#hand sanitizer

hand_sanitizer_table <- tab_model(hand_regress_state_time, hand_regress_state, hand_regress_no_fixed, 
  #title = "Table 2: Searches for Hand Sanitizer",
  dv.labels = c("State and Time Fixed Effects", "State Fixed Effects", "No Fixed Effects"),
  terms = relevant_coefficients,
  show.ci = FALSE, 
  show.se = TRUE,
  collapse.se = TRUE,
  show.p = FALSE,
  p.style = "stars",
  show.r2 = TRUE, 
  show.obs = TRUE,
  pred.labels = predictors,
  auto.label = FALSE)

hand_sanitizer_table

#toilet paper
toilet_table<- tab_model(toilet_regress_state_time, toilet_regress_state,toilet_regress_no_fixed, 
 # title = "Searches for Toilet Paper",
  dv.labels = c("State and Time Fixed Effects", "State Fixed Effects", "No Fixed Effects"),
  terms = relevant_coefficients,
  show.ci = FALSE, 
  show.se = TRUE,
  collapse.se = TRUE,
  show.p = FALSE,
  p.style = "stars",
  show.r2 = TRUE, 
  show.obs = TRUE,
  pred.labels = predictors,
  auto.label = FALSE)

toilet_table
#masks 
masks_table <- tab_model(mask_regress_state_time, mask_regress_state,mask_regress_no_fixed, 
  #title = "Searches for Masks",
  dv.labels = c("State and Time Fixed Effects", "State Fixed Effects", "No Fixed Effects"),
  terms = relevant_coefficients,
  show.ci = FALSE, 
  show.se = TRUE,
  collapse.se = TRUE,
  show.p = FALSE,
  p.style = "stars",
  show.r2 = TRUE, 
  show.obs = TRUE,
  pred.labels = predictors,
  auto.label = FALSE)

masks_table 

#shoes
shoes_table <- tab_model(shoes_regress_state_time, shoes_regress_state,shoes_regress_no_fixed, 
  #title = "Searches for Shoes",
  dv.labels = c("State and Time Fixed Effects", "State Fixed Effects", "No Fixed Effects"),
  terms = relevant_coefficients,
  show.ci = FALSE, 
  show.se = TRUE,
  collapse.se = TRUE,
  show.p = FALSE,
  p.style = "stars",
  show.r2 = TRUE, 
  show.obs = TRUE,
  pred.labels = predictors,
  auto.label = FALSE)

shoes_table

#price gouging regression
gouging_table <- tab_model(gouging_regress_state_time, gouging_regress_state,gouging_regress_no_fixed, 
 # title = "Searches for Price Gouging",
  dv.labels = c("State and Time Fixed Effects", "State Fixed Effects", "No Fixed Effects"),
  terms = relevant_coefficients,
  show.ci = FALSE, 
  show.se = TRUE,
  collapse.se = TRUE,
  show.p = FALSE,
  p.style = "stars",
  show.r2 = TRUE, 
  show.obs = TRUE,
  pred.labels = predictors,
  auto.label = FALSE)

gouging_table 




