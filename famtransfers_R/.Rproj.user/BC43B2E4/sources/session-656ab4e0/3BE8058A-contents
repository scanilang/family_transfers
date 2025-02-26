library(tidyverse)


########################################################################
#
# Age Polynomial
#
########################################################################
data_final <- read.csv("data clean/data_final.csv") %>% 
  select(-X)%>% 
  mutate(Support_Indicator = if_else(Support_Amount_Net >= 50, 1, 0),
         Receive_Indicator = if_else(Receive_Head_Spouse_Net >= 50, 1, 0)) %>% 
  mutate(Age2 = Age^2,
         Age3 = Age^3)

black_probit_out_age = glm(Support_Indicator ~  Age+ Age2,
                           family = binomial(link = "probit"), 
                           data = data_final %>% filter(Race == "Black"))

white_probit_out_age = glm(Support_Indicator ~ Age +Age2,
                           family = binomial(link = "probit"), 
                           data = data_final %>% filter(Race == "White"))

black_transfer_out_age = lm(log(Support_Amount_Net) ~  Age + Age2 ,
                            data = data_final %>% filter(Race == "Black") %>% filter(Support_Amount_Net >= 50))

white_transfer_out_age = lm(log(Support_Amount_Net) ~  Age + Age2 ,
                            data = data_final %>% filter(Race == "White") %>% filter(Support_Amount_Net >= 50))

black_probit_in_age = glm(Receive_Indicator ~  Age + Age2 ,
                          family = binomial(link = "probit"), 
                          data = data_final %>% filter(Race == "Black"))

white_probit_in_age = glm(Receive_Indicator ~  Age + Age2,
                          family = binomial(link = "probit"), 
                          data = data_final %>% filter(Race == "White"))

black_transfer_in_age = lm(log(Receive_Head_Spouse_Net) ~ Age + Age2,
                           data = data_final %>% filter(Race == "Black") %>% filter(Receive_Head_Spouse_Net >= 50))

white_transfer_in_age = lm(log(Receive_Head_Spouse_Net) ~ Age + Age2,
                           data = data_final %>% filter(Race == "White") %>% filter(Receive_Head_Spouse_Net >=50))

########################################################################
#
# Transfer Function with Age Polynomial 
#
########################################################################
data_4yr_aggregate = read.csv("data clean/data_4yr_aggregate.csv") %>% 
  mutate(Support_Indicator = if_else(sum_transfer_out >=1000, 1, 0),
         Receive_Indicator = if_else(sum_transfer_in >= 1000, 1, 0),
         Year_1988_2000 = if_else(Year_first_bin == '1988-2000', 1, 0), 
         Year_1984_1990 = if_else(Year_first_bin == '1984-1990', 1, 0), 
         Year_1998_2010 = if_else(Year_first_bin == '1998-2010', 1, 0), 
         Year_2008_2020 = if_else(Year_first_bin == '2008-2020', 1, 0), 
         Birth_1898_1904= if_else(Year_Born_bin == '1898-1904', 1, 0), 
         Birth_1905_1911= if_else(Year_Born_bin == '1905-1911', 1, 0), 
         Birth_1912_1918 = if_else(Year_Born_bin == '1912-1918', 1, 0), 
         Birth_1919_1925 = if_else(Year_Born_bin == '1919-1925', 1, 0), 
         Birth_1926_1932 = if_else(Year_Born_bin == '1926-1932', 1, 0), 
         Birth_1933_1939 = if_else(Year_Born_bin == '1933-1939', 1, 0), 
         Birth_1940_1946 = if_else(Year_Born_bin == '1940-1946', 1, 0), 
         Birth_1947_1953 = if_else(Year_Born_bin == '1947-1953', 1, 0), 
         Birth_1954_1960 = if_else(Year_Born_bin == '1954-1960', 1, 0), 
         Birth_1961_1967 = if_else(Year_Born_bin == '1961-1967', 1, 0), 
         Birth_1968_1974 = if_else(Year_Born_bin == '1968-1974', 1, 0), 
         Birth_1975_1981 = if_else(Year_Born_bin == '1975-1981', 1, 0), 
         Birth_1982_1988= if_else(Year_Born_bin == '1982-1988', 1, 0), 
         Birth_1989_1995= if_else(Year_Born_bin == '1989-1995', 1, 0), 
         Married = if_else(Marital_Status == "Married", 1, 0),
         Single = if_else(Marital_Status == "Single", 1, 0),
         log_sum_fam_income = log(sum_fam_income),
         mid_age = case_when(age_cat == 1 ~ 26.5,
                             age_cat == 2 ~ 30.5,
                             age_cat == 3 ~ 34.5,
                             age_cat == 4 ~ 38.5,
                             age_cat == 5 ~ 42.5,
                             age_cat == 6 ~ 46.5,
                             age_cat == 7 ~ 50.5,
                             age_cat == 8 ~ 54.5,
                             age_cat == 9 ~ 58.5,
                             age_cat == 10 ~ 62.5,
                             age_cat == 11 ~ 66.5,
                             age_cat == 12 ~ 70.5,
                             age_cat == 13 ~ 74.5,
                             age_cat == 14 ~ 78.5,
                             age_cat == 15 ~ 82.5),
         age_probit_out = if_else(Race == "Black", black_probit_out_age$coefficients[1] + mid_age* black_probit_out_age$coefficients[2] + mid_age^2 *black_probit_out_age$coefficients[3],
                                  white_probit_out_age$coefficients[1] + mid_age* white_probit_out_age$coefficients[2] + mid_age^2 *white_probit_out_age$coefficients[3]),
         age_probit_in = if_else(Race == "Black", black_probit_in_age$coefficients[1] + mid_age* black_probit_in_age$coefficients[2] + mid_age^2 *black_probit_in_age$coefficients[3],
                                 white_probit_in_age$coefficients[1] + mid_age* white_probit_in_age$coefficients[2] + mid_age^2 *white_probit_in_age$coefficients[3]),
         age_transfer_out = if_else(Race == "Black", black_transfer_out_age$coefficients[1] + mid_age* black_transfer_out_age$coefficients[2] + mid_age^2 *black_transfer_out_age$coefficients[3],
                                    white_transfer_out_age$coefficients[1] + mid_age* white_transfer_out_age$coefficients[2] + mid_age^2 *white_transfer_out_age$coefficients[3]),
         age_transfer_in = if_else(Race == "Black", black_transfer_in_age$coefficients[1] + mid_age* black_transfer_in_age$coefficients[2] + mid_age^2 *black_transfer_in_age$coefficients[3],
                                   white_transfer_in_age$coefficients[1] + mid_age* white_transfer_in_age$coefficients[2] + mid_age^2 *white_transfer_in_age$coefficients[3])) %>% 
  filter(sum_labor_income_comp_ui <= 1800000)

(white_hh <- ggplot(data_4yr_aggregate %>% filter(Race == "White"), aes(x = log(sum_fam_income), y = Support_Indicator))+
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    theme_classic() + ggtitle("Providing Support White Household Heads") + xlab("log(Total Family Income)") + ylab("") +
    scale_x_continuous(limits = c(0, 16)) )

(black_hh <- ggplot(data_4yr_aggregate %>% filter(Race == "Black"), aes(x = log(sum_fam_income), y = Support_Indicator))+
    geom_point() + 
    geom_smooth(method = "loess", se = FALSE) +
    theme_classic()  + 
    ggtitle("Providing Support Black Household Heads") + 
    xlab("log(Total Family Income)") + ylab("")+
    scale_x_continuous(limits = c(0, 16)) )


data_4yr_aggregate$Marital_Status = relevel(data_4yr_aggregate$Marital_Status, ref = "Single") 

black_probit_out = glm(Support_Indicator ~ log(sum_fam_income) + age_probit_out + avg_FU_size +Marital_Status + received_transfer_past + provided_transfer_past + Year_Born_bin + as.factor(Year_first_bin),
                       family = binomial(link = "probit"), 
                       data = data_4yr_aggregate %>% filter(Race == "Black"))

black_probit_out$coefficients['age_probit_out']*black_probit_out_age$coefficients
black_probit_out$coefficients[1] + black_probit_out$coefficients[3]*black_probit_out_age$coefficients[1]

white_probit_out = glm(Support_Indicator ~ log_sum_fam_income + age_probit_out + avg_FU_size +Marital_Status + received_transfer_past + provided_transfer_past + Year_Born_bin + as.factor(Year_first_bin),
                       family = binomial(link = "probit"), 
                       data = data_4yr_aggregate %>% filter(Race == "White"))

white_probit_out$coefficients[3]*white_probit_out_age$coefficients
white_probit_out$coefficients[1] + white_probit_out$coefficients[3]*white_probit_out_age$coefficients[1]

black_transfer_out = lm(log(sum_transfer_out) ~ log(sum_fam_income) + age_transfer_out + avg_FU_size + Marital_Status + Year_Born_bin + as.factor(Year_first_bin),
                        data = data_4yr_aggregate %>% filter(Race == "Black") %>% filter(sum_transfer_out >= 1000))

black_transfer_out$coefficients[3]*black_transfer_out_age$coefficients
black_transfer_out$coefficients[1] + black_transfer_out$coefficients[3]*black_transfer_out_age$coefficients[1]

white_transfer_out = lm(log(sum_transfer_out) ~ log(sum_fam_income)  + age_transfer_out+ avg_FU_size + Marital_Status + Year_Born_bin + as.factor(Year_first_bin),
                        data = data_4yr_aggregate %>% filter(Race == "White") %>% filter(sum_transfer_out >= 1000))

white_transfer_out$coefficients[3]*white_transfer_out_age$coefficients
white_transfer_out$coefficients[1] + white_transfer_out$coefficients[3]*white_transfer_out_age$coefficients[1]


black_probit_in = glm(Receive_Indicator ~ log(sum_fam_income) + age_probit_in + avg_FU_size +Marital_Status + received_transfer_past + provided_transfer_past + Year_Born_bin + as.factor(Year_first_bin),
                      family = binomial(link = "probit"), 
                      data = data_4yr_aggregate %>% filter(Race == "Black"))

black_probit_in$coefficients[3]*black_probit_in_age$coefficients
black_probit_in$coefficients[1] + black_probit_in$coefficients[3]*black_probit_in_age$coefficients[1]

white_probit_in = glm(Receive_Indicator ~ log(sum_fam_income) + age_probit_in + avg_FU_size +Marital_Status + received_transfer_past + provided_transfer_past + Year_Born_bin + as.factor(Year_first_bin),
                      family = binomial(link = "probit"), 
                      data = data_4yr_aggregate %>% filter(Race == "White"))

white_probit_in$coefficients[3]*white_probit_in_age$coefficients
white_probit_in$coefficients[1] + white_probit_in$coefficients[3]*white_probit_in_age$coefficients[1]

black_transfer_in = lm(log(sum_transfer_in) ~ log(sum_fam_income)  + age_transfer_in + avg_FU_size + Marital_Status + Year_Born_bin + as.factor(Year_first_bin),
                       data = data_4yr_aggregate %>% filter(Race == "Black") %>% filter(sum_transfer_in >= 1000))

black_transfer_in$coefficients[3]*black_transfer_in_age$coefficients
black_transfer_in$coefficients[1] + black_transfer_in$coefficients[3]*black_transfer_in_age$coefficients[1]

white_transfer_in = lm(log(sum_transfer_in) ~ log(sum_fam_income) + age_transfer_in+ avg_FU_size + Marital_Status +Year_Born_bin + as.factor(Year_first_bin),
                       data = data_4yr_aggregate %>% filter(Race == "White") %>% filter(sum_transfer_in >= 1000))

white_transfer_in$coefficients[3]*white_transfer_in_age$coefficients
white_transfer_in$coefficients[1] + white_transfer_in$coefficients[3]*white_transfer_in_age$coefficients[1]

########################################################################
#
# Marginal Effects
#
########################################################################
library(margins)

margins(black_probit_out, variables = "log_sum_fam_income")
margins(black_probit_out, variables = "sum_fam_income")

margins(black_probit_out_age, variables = "Age")
margins(black_probit_out_age, variables = "Age2")


black_probit_out_age
margins(black_receive, variables = "log_total_fam_income")
margins(black_receive, variables = "Age") 
margins(black_receive, variables = "Marital_Status")


########################################################################
#
# W Assets
#
########################################################################
data_assets = data_4yr_aggregate %>% 
  mutate(wealth_bottomcode = if_else(sum_wealth <= 0, 0.0001, sum_wealth),
         Support_Indicator = if_else(sum_transfer_out >=200, 1, 0),
         Receive_Indicator = if_else(sum_transfer_in >= 200, 1, 0))
data_assets$Marital_Status = relevel(data_assets$Marital_Status, ref = "Single") 

black_probit_out = glm(Support_Indicator ~ log(sum_fam_income) + log(wealth_bottomcode) + age_probit_out + avg_FU_size +Marital_Status + received_transfer_past + provided_transfer_past + Year_Born_bin + as.factor(Year_first_bin),
                       family = binomial(link = "probit"), 
                       data = data_assets %>% filter(Race == "Black"))

black_probit_out$coefficients['age_probit_out']*black_probit_out_age$coefficients
black_probit_out$coefficients[1] + black_probit_out$coefficients[3]*black_probit_out_age$coefficients[1]

white_probit_out = glm(Support_Indicator ~ log(sum_fam_income) + log(wealth_bottomcode) + age_probit_out + avg_FU_size +Marital_Status + received_transfer_past + provided_transfer_past + Year_Born_bin + as.factor(Year_first_bin),
                       family = binomial(link = "probit"), 
                       data = data_assets %>% filter(Race == "White"))

white_probit_out$coefficients[3]*white_probit_out_age$coefficients
white_probit_out$coefficients[1] + white_probit_out$coefficients[3]*white_probit_out_age$coefficients[1]

black_transfer_out = lm(log(sum_transfer_out) ~ log(sum_fam_income) + log(wealth_bottomcode) + age_transfer_out + avg_FU_size + Marital_Status + Year_Born_bin + as.factor(Year_first_bin),
                        data = data_assets %>% filter(Race == "Black") %>% filter(sum_transfer_out >= 500))

black_transfer_out$coefficients[3]*black_transfer_out_age$coefficients
black_transfer_out$coefficients[1] + black_transfer_out$coefficients[3]*black_transfer_out_age$coefficients[1]

white_transfer_out = lm(log(sum_transfer_out) ~ log(sum_fam_income) + log(wealth_bottomcode) + age_transfer_out+ avg_FU_size + Marital_Status + Year_Born_bin + as.factor(Year_first_bin),
                        data = data_assets %>% filter(Race == "White") %>% filter(sum_transfer_out >= 500))

white_transfer_out$coefficients[3]*white_transfer_out_age$coefficients
white_transfer_out$coefficients[1] + white_transfer_out$coefficients[3]*white_transfer_out_age$coefficients[1]

black_probit_in = glm(Receive_Indicator ~ log(sum_fam_income) + log(wealth_bottomcode) +age_probit_in + avg_FU_size +Marital_Status + received_transfer_past + provided_transfer_past + Year_Born_bin + as.factor(Year_first_bin),
                      family = binomial(link = "probit"), 
                      data = data_assets %>% filter(Race == "Black"))

black_probit_in$coefficients[3]*black_probit_in_age$coefficients
black_probit_in$coefficients[1] + black_probit_in$coefficients[3]*black_probit_in_age$coefficients[1]

white_probit_in = glm(Receive_Indicator ~ log(sum_fam_income) + log(wealth_bottomcode) + age_probit_in + avg_FU_size +Marital_Status + received_transfer_past + provided_transfer_past + Year_Born_bin + as.factor(Year_first_bin),
                      family = binomial(link = "probit"), 
                      data = data_assets %>% filter(Race == "White"))

white_probit_in$coefficients[3]*white_probit_in_age$coefficients
white_probit_in$coefficients[1] + white_probit_in$coefficients[3]*white_probit_in_age$coefficients[1]

black_transfer_in = lm(log(sum_transfer_in) ~log(sum_fam_income) + log(wealth_bottomcode) +age_transfer_in + avg_FU_size + Marital_Status + Year_Born_bin + as.factor(Year_first_bin),
                       data = data_assets %>% filter(Race == "Black") %>% filter(sum_transfer_in >= 500))

black_transfer_in$coefficients[3]*black_transfer_in_age$coefficients
black_transfer_in$coefficients[1] + black_transfer_in$coefficients[3]*black_transfer_in_age$coefficients[1]

white_transfer_in = lm(log(sum_transfer_in) ~ log(sum_fam_income) + log(wealth_bottomcode) +age_transfer_in+ avg_FU_size + Marital_Status +Year_Born_bin + as.factor(Year_first_bin),
                       data = data_assets %>% filter(Race == "White") %>% filter(sum_transfer_in >= 500))

white_transfer_in$coefficients[3]*white_transfer_in_age$coefficients
white_transfer_in$coefficients[1] + white_transfer_in$coefficients[3]*white_transfer_in_age$coefficients[1]
