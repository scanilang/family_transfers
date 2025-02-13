library(tidyverse)

# import raw data
tas_raw <- read.csv("PSID data/transition_adulthood.csv")

# clean TAS data
tas_clean <- tas_raw %>% 
 mutate(across(matches("^Help_.*Amount_Parents$"), ~ na_if(na_if(., 9999998), 9999999)),
        across(starts_with("Gifts_Inheritance"), ~ na_if(na_if(., 9999998), 9999999)),
        across(starts_with("Gifts_Inheritance"), ~ na_if(na_if(., 9999998), 9999999)),
        across(starts_with("Tuition_Amount"), ~ na_if(na_if(., 9999998), 9999999)))


tas_clean_summary = tas_clean %>% 
  filter(Race %in% c(1,2)) %>% 
  group_by(Race) %>% 
  summarize(n = n(),
            received_personal_laon = sum(Help_Personal_Loan == 1) / n,
            receoved_help_rent_mortgage = sum(Help_Rent_Mortgage == 1) / n,
            received_help_bills = sum(Help_Bills == 1) /n )


tas_clean_college = tas_clean %>% 
  filter(Race %in% c(1,2)) %>% 
  filter(Enrollment_Status >= 3) %>% 
  filter(Enrollment_Status < 99) %>%
  group_by(Race) %>% 
  summarize(n = n(),
            received_help_tuition = sum(Help_Tuition == 1) / n)

tas_clean_college = tas_clean %>% 
  filter(Race %in% c(1,2)) %>% 
  filter(Help_Tuition_Amount_Parents > 0) %>% 
  group_by(Race) %>% 
  summarize(n = n(),
            received_help_tuition = mean(Help_Tuition_Amount_Parents))
            

# 2 year
# group 17-18
# education decision
