library(dplyr)

#######################################################################################
# Read in raw data 
#######################################################################################

psid_clean = read.csv("../../data/psid_clean.csv")

# selection criteria and cleaned variables
psid_select = psid_clean %>%   
  filter(Survey_Year >= 1985,
         Age_recode >= 18,
         Age_recode <= 85,
         Race_Head %in% c("Black", "White"),
         Race_Head_2nd == 0,
         !(is.na(Received_Support_Amount_Head_Spouse) & is.na(Provided_Family_Support_NetACS)),
         Marital_Status %in% c("Married", "Single"),
         !is.na(Total_Income_Head_Spouse),
         !is.na(Family_Transfers_Net)) %>%  
  mutate(Marital_Status = factor(Marital_Status, levels= c("Single", "Married"))) %>% 
  select(Family_ID, ER30001, ER30002, Survey_Year, Year,Year_Born, Age,Age_recode, Race_Head, Marital_Status, Family_Unit_Size, Num_Children_FU,
         Provided_Family_Support_NetACS, Received_Support_Amount_Head_Spouse,Family_Transfers_Net,Total_Family_Income, Total_Asset_Income, Labor_Income_Plus_Business_Head,
         Total_Taxable_Income_Head_Spouse, Total_Transfer_Head_Spouse, Total_Taxable_Income_Other, ChildSupport_Alimony_Adjustment, Total_Retirement_Income,
         Total_Labor_Income_Plus_Business, Total_Public_Transfers, Total_Income_Head_Spouse, Total_UnempWorkers_Comp,
         Head_College, Head_College_Degree) 

write.csv(psid_select, "../../data/psid_select.csv")


#######################################################################################
# Aggregate to 2 years
#######################################################################################

psid_model_data <- psid_select %>% 
  # double biennial year
  full_join(psid_select %>%
              filter(Survey_Year >= 1999) %>% 
              mutate(Year = Year -1,
                     Age_recode = Age_recode -1)) %>% 
  # some age adjustments and categories
  mutate(#Year_Born = if_else(Age_recode <= 21, Year_Born -(21 - Age_recode), Year_Born),
         #Year_Born = if_else(Age_recode == 17, Year_Born - 1, Year_Born),
         #Age_recode = if_else(Age_recode == 17, 18, Age_recode),
         #Year_Born = if_else(Age_recode == 84, Year_Born +1, Year_Born),
        # Age_recode = if_else(Age_recode <= 21, 21, Age_recode),
         #Age_recode = if_else(Age_recode == 84, 83, Age_recode),
         age_cat = paste(20 + (Age_recode - 20) %/% 2 * 2, 
                         21 + (Age_recode - 20) %/% 2 * 2, 
                         sep = "-"),
         age_cat_large = paste(18 + (Age_recode - 18) %/% 7 * 7, 
                               24 + (Age_recode - 18) %/% 7 * 7, 
                               sep = "-")) %>% 
        # age_cat = if_else(age_cat == '20-21', '18-21', age_cat)
  select(Family_ID, ER30001, ER30002, Survey_Year, Year, age_cat, everything()) %>%
  # aggregated variables
  group_by(ER30001, ER30002, age_cat) %>% 
  mutate(n = n(),
         # demographics
         min_age = min(Age_recode),
         years_married = sum(Marital_Status == "Married"),
         avg_FU_size= mean(Family_Unit_Size),
         Year_first = min(Year),
         midpoint_age = max(Age_recode),
         midpoint_age2 = midpoint_age* midpoint_age,
         midpoint_age3 = midpoint_age2 * midpoint_age,
         # aggregate transfers
         sum_transfer_out = sum(Provided_Family_Support_NetACS),
         sum_transfer_in = sum(Received_Support_Amount_Head_Spouse),
         sum_net_transfers = sum(Family_Transfers_Net), 
         Received_Support_Indicator = if_else(sum_transfer_in > 0 , 1, 0),
         Provided_Support_Indicator = if_else(sum_transfer_out > 0 , 1, 0),
         # aggregate income
         sum_labor_head = sum(Labor_Income_Plus_Business_Head),
         sum_total_fam = sum(Total_Family_Income),
         sum_labor_income = sum(Total_Labor_Income_Plus_Business),
         sum_uiwc_income = sum(Total_UnempWorkers_Comp),
         sum_labor_uiwc = sum_labor_income +sum_uiwc_income,
         sum_public_transfers = sum(Total_Public_Transfers),
         sum_asset_income = sum(Total_Asset_Income),
         sum_total_income = sum(Total_Income_Head_Spouse),
         sum_nonasset_income = sum(Total_UnempWorkers_Comp + Total_Labor_Income_Plus_Business + Total_Public_Transfers+ Total_Retirement_Income + ChildSupport_Alimony_Adjustment),
         # bottom code income
         sum_total_income = if_else(sum_total_income <= 1.5, 1.5, sum_total_income),
         sum_nonasset_income = if_else(sum_nonasset_income <= 1.5, 1.5, sum_nonasset_income), 
         sum_asset_income = if_else(sum_asset_income <= 100, 1, sum_asset_income), 
         sum_asset_income_threshold = if_else(sum_asset_income > 100, sum_asset_income, 0),
         # log income
         log_asset_income = log(sum_asset_income),
         log_uiwc_income = log(sum_uiwc_income),
         log_labor_income = log(sum_labor_income),
         log_labor_uiwc_income = log(sum_labor_uiwc),
         log_nonasset_income = log(sum_nonasset_income),
         log_total_income = log(sum_total_income)) %>% 
  ungroup() %>% 
  # received/provided in past indicator
  group_by(ER30001, ER30002) %>% 
  arrange(Age_recode) %>% 
  mutate(lag_received = lag(cumsum(Received_Support_Amount_Head_Spouse)),
         lag_provided = lag(cumsum(Provided_Family_Support_NetACS))) %>% 
  ungroup() %>% 
  group_by(ER30001, ER30002, age_cat) %>% 
  arrange(Age_recode) %>% 
  mutate(received_transfer_past = if_else(is.na(sum(lag_received)), NA,
                                          if_else(sum(lag_received > 0) == 2, TRUE, FALSE)),
         provided_transfer_past = if_else(is.na(sum(lag_provided)), NA,
                                          if_else(sum(lag_provided  > 0) == 2, TRUE, FALSE)))%>% 
  ungroup() %>% 
  # Year bins 
  mutate(Year_bins = paste(1983 + ((Year_first-1983) %/% 10) * 10, 
                           1992 + ((Year_first-1983) %/% 10) * 10, 
                           sep = "-"),
         Year_bins = forcats::fct_relevel(Year_bins, c("2013-2022", "2003-2012", "1993-2002", "1983-1992")),
         Birth_Cohort = paste((Year_Born %/% 10) * 10, 
                              (Year_Born %/% 10) * 10 + 9, 
                              sep = "-"),
         # regrouping first and last group
         Birth_Cohort = case_when(Birth_Cohort == "2000-2009"~ "1990-1999",
                                  Birth_Cohort == "1910-1919"~ "1920-1929", 
                                  Birth_Cohort == "1900-1909"~ "1920-1929", 
                                  TRUE ~ Birth_Cohort),
        Birth_Cohort = forcats::fct_relevel(Birth_Cohort, c("1970-1979", "1980-1989", "1990-1999", "1960-1969", "1950-1959","1940-1949","1930-1939", "1920-1929"))) %>% 
  # filter
  filter(n == 2) %>%
  filter(years_married %in% c(0,2)) %>% 
  distinct(ER30001, ER30002, Year_first, Year_bins,Year_Born,Birth_Cohort, age_cat,age_cat_large, min_age, midpoint_age, midpoint_age2, midpoint_age3, Race_Head, Marital_Status, avg_FU_size,Head_College,
           sum_total_fam, sum_total_income,sum_nonasset_income, sum_asset_income, sum_labor_uiwc, sum_public_transfers,sum_labor_head,log_total_income,  log_nonasset_income, log_asset_income,log_labor_income, 
           log_labor_uiwc_income, log_uiwc_income, sum_transfer_out, sum_transfer_in,sum_net_transfers, Provided_Support_Indicator,Received_Support_Indicator, received_transfer_past, provided_transfer_past) %>% 
  arrange(ER30001, ER30002, age_cat)

# write output
write.csv(psid_model_data, "../../data/psid_model_data.csv")

# sample size check
Year_cohort = psid_model_data %>%
  filter(first_obs ==0 ) %>% 
  group_by(Race_Head, Year_bins, Birth_Cohort) %>%
   summarize(n = n())
