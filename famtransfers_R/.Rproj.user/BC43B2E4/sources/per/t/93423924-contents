library(tidyverse)
library(margins)
library(brglm2)
library(stargazer)

#######################################################################################
# Read in clean data
#######################################################################################

psid_model_data = read.csv('../../data/psid_model_data.csv')

# potential top code? but with logs its we dont need to do this i think
# psid_transfer_data = psid_model_data %>% 
#   mutate(age_4cat = case_when(min_age <= 28 ~ "18-29",
#                               min_age %in% 30:44 ~ "30-45",
#                               min_age %in% 46:59 ~ "46-60",
#                               TRUE ~ "60+")) %>% 
#   group_by(Race_Head, age_4cat, Head_College) %>% 
#   # top code transfers
#   mutate(t_in95th = quantile(sum_transfer_in[sum_transfer_in > 1000], 0.97),
#          t_out95th = quantile(sum_transfer_out[sum_transfer_out > 1000], 0.97),
#          sum_transfer_in_raw = sum_transfer_in, 
#          sum_transfer_out_raw = sum_transfer_out,
#          sum_transfer_in = if_else(sum_transfer_in > t_out95th, t_out95th, sum_transfer_in),
#          sum_transfer_out = if_else(sum_transfer_out > t_out95th, t_out95th, sum_transfer_out))

#######################################################################################
# Step 1: Probit
#######################################################################################

############### separate asset and nonasset income
black_probit_out = glm(Provided_Support_Indicator ~ log_nonasset_income + log_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                       family = binomial(link = "probit"),  method = "brglmFit",
                       data = psid_transfer_data %>% filter(Race_Head == "Black")%>% filter(!is.na(provided_transfer_past)))

black_probit_in = glm(Received_Support_Indicator ~ log_nonasset_income + log_asset_income  + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                      family = binomial(link = "probit"),  method = "brglmFit",
                      data = psid_transfer_data %>% filter(Race_Head == "Black")%>% filter(!is.na(provided_transfer_past)))

white_probit_in = glm(Received_Support_Indicator ~ log_nonasset_income + log_asset_income  + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                      family = binomial(link = "probit"), method = "brglmFit",
                      data = psid_transfer_data %>% filter(Race_Head == "White")%>% filter(!is.na(provided_transfer_past)))
white_probit_out = glm(Provided_Support_Indicator ~log_nonasset_income + log_asset_income +midpoint_age +  + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                       family = binomial(link = "probit"), method = "brglmFit",
                       data = psid_transfer_data %>% filter(Race_Head == "White")%>% filter(!is.na(provided_transfer_past)))

test = data.frame(coefs = white_probit_in$coefficients) %>%  tibble::remove_rownames() 
test$coefs
stargazer(white_probit_out, black_probit_out,white_probit_in, black_probit_in )

# marginal effects
models <- list(  white_probit_out = white_probit_out,  black_probit_out = black_probit_out, white_probit_in = white_probit_in, black_probit_in = black_probit_in)

mfx_results <- lapply(models, function(model) summary(margins(model)))
options(scipen = 999)
mfx_results_afe = data.frame(mfx_results$white_probit_out$AME,  mfx_results$black_probit_out$AME,
                             mfx_results$white_probit_in$AME,  mfx_results$black_probit_in$AME)

library(xtable)
print(xtable(mfx_results_afe, digits = c(0,4,4,4,4)))

mfx_results_se = data.frame(mfx_results$white_probit_out$SE,  mfx_results$black_probit_out$SE,
                             mfx_results$white_probit_in$SE,  mfx_results$black_probit_in$SE)
print(xtable(mfx_results_se, digits = c(0,4,4,4,4)))

# total income only 
############### separate asset and nonasset income
black_probit_out = glm(Provided_Support_Indicator ~ log_total_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                       family = binomial(link = "probit"),  method = "brglmFit",
                       data = psid_model_data %>% filter(Race_Head == "Black")%>% filter(!is.na(provided_transfer_past)))

black_probit_in = glm(Received_Support_Indicator ~ log_total_income   + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                      family = binomial(link = "probit"),  method = "brglmFit",
                      data = psid_model_data %>% filter(Race_Head == "Black")%>% filter(!is.na(provided_transfer_past)))

white_probit_in = glm(Received_Support_Indicator ~ log_total_income   + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                      family = binomial(link = "probit"), method = "brglmFit",
                      data = psid_model_data %>% filter(Race_Head == "White")%>% filter(!is.na(provided_transfer_past)))

white_probit_out = glm(Provided_Support_Indicator ~log_total_income  +midpoint_age +  + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                       family = binomial(link = "probit"), method = "brglmFit",
                       data = psid_model_data %>% filter(Race_Head == "White")%>% filter(!is.na(provided_transfer_past)))

stargazer(white_probit_out, black_probit_out,white_probit_in, black_probit_in )


#######################################################################################
# Step 2: OLS
#######################################################################################

# separate asset and nonasset income
black_transfer_out = lm(log(sum_transfer_out) ~ log_nonasset_income + log_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                       data = psid_model_data %>% filter(Race_Head == "Black") %>%  filter(Provided_Support_Indicator == 1,!is.na(provided_transfer_past)))

black_transfer_in = lm(log(sum_transfer_in) ~ log_nonasset_income + log_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                      data = psid_model_data %>% filter(Race_Head == "Black") %>%  filter(Received_Support_Indicator == 1,!is.na(provided_transfer_past)))

white_transfer_out = lm(log(sum_transfer_out) ~ log_nonasset_income + log_asset_income + midpoint_age +midpoint_age2 +  avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                       data = psid_model_data %>% filter(Race_Head == "White") %>%  filter(Provided_Support_Indicator == 1,!is.na(provided_transfer_past)))

white_transfer_in = lm(log(sum_transfer_in) ~ log_nonasset_income + log_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                      data = psid_model_data %>% filter(Race_Head == "White") %>%  filter(Received_Support_Indicator == 1,!is.na(provided_transfer_past)))

stargazer(white_transfer_out, black_transfer_out,white_transfer_in, black_transfer_in )


test = data.frame(coefs = black_transfer_out$coefficients) %>%  tibble::remove_rownames() 
test$coefs

# has asset_income

black_transfer_out = lm(log(sum_transfer_out) ~ log_nonasset_income  + log_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                        data = psid_model_data %>% filter(Race_Head == "Black") %>%  filter(Provided_Support_Indicator == 1,!is.na(provided_transfer_past), log_asset_income >=0))

black_transfer_in = lm(log(sum_transfer_in) ~ log_nonasset_income  + log_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                       data = psid_model_data %>% filter(Race_Head == "Black") %>%  filter(Received_Support_Indicator == 1,!is.na(provided_transfer_past)), log_asset_income >=0) 

white_transfer_out = lm(log(sum_transfer_out) ~ log_nonasset_income  + log_asset_income + midpoint_age +midpoint_age2 +  avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                        data = psid_model_data %>% filter(Race_Head == "White") %>%  filter(Provided_Support_Indicator == 1,!is.na(provided_transfer_past)), log_asset_income >=0)

white_transfer_in = lm(log(sum_transfer_in) ~ log_nonasset_income  + log_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                       data = psid_model_data %>% filter(Race_Head == "White") %>%  filter(Received_Support_Indicator == 1,!is.na(provided_transfer_past)), log_asset_income >=0)

stargazer(white_transfer_out, black_transfer_out,white_transfer_in, black_transfer_in )


# no asset income
black_transfer_out = lm(log(sum_transfer_out) ~ log_nonasset_income  + log_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                        data = psid_model_data %>% filter(Race_Head == "Black") %>%  filter(Provided_Support_Indicator == 1,!is.na(provided_transfer_past), log_asset_income ==0))

black_transfer_in = lm(log(sum_transfer_in) ~ log_nonasset_income  + log_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                       data = psid_model_data %>% filter(Race_Head == "Black") %>%  filter(Received_Support_Indicator == 1,!is.na(provided_transfer_past)), log_asset_income ==0) 

white_transfer_out = lm(log(sum_transfer_out) ~ log_nonasset_income  + log_asset_income + midpoint_age +midpoint_age2 +  avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                        data = psid_model_data %>% filter(Race_Head == "White") %>%  filter(Provided_Support_Indicator == 1,!is.na(provided_transfer_past)), log_asset_income ==0)

white_transfer_in = lm(log(sum_transfer_in) ~ log_nonasset_income  + log_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                       data = psid_model_data %>% filter(Race_Head == "White") %>%  filter(Received_Support_Indicator == 1,!is.na(provided_transfer_past)), log_asset_income ==0)

stargazer(white_transfer_out, black_transfer_out,white_transfer_in, black_transfer_in )


#### NOT log
black_transfer_out = lm(sum_transfer_out ~ sum_nonasset_income+ sum_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                        data = psid_model_data %>% filter(Race_Head == "Black") %>%  filter(Provided_Support_Indicator == 1,!is.na(provided_transfer_past)))

black_transfer_in = lm(sum_transfer_in ~ sum_nonasset_income +  sum_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                       data = psid_model_data %>% filter(Race_Head == "Black") %>%  filter(Received_Support_Indicator == 1,!is.na(provided_transfer_past)))
                       
white_transfer_out = lm(sum_transfer_out ~ sum_nonasset_income + sum_asset_income + midpoint_age +midpoint_age2 +  avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                                               data = psid_model_data %>% filter(Race_Head == "White") %>%  filter(Provided_Support_Indicator == 1,!is.na(provided_transfer_past)))
                       
white_transfer_in = lm(sum_transfer_in ~ sum_nonasset_income + sum_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                                              data = psid_model_data %>% filter(Race_Head == "White") %>%  filter(Received_Support_Indicator == 1,!is.na(provided_transfer_past)))


white_transfer_in = lm(sum_transfer_in ~ log_nonasset_income + sum_asset_income + midpoint_age + midpoint_age2 + avg_FU_size + Marital_Status + received_transfer_past + provided_transfer_past + Birth_Cohort + Year_bins,
                       data = psid_model_data %>% filter(Race_Head == "White") %>%  filter(Received_Support_Indicator == 1,!is.na(provided_transfer_past)))

test = data.frame(coefs = white_transfer_out$coefficients) %>%  tibble::remove_rownames() 
test$coefs
