library(dplyr)

#######################################################################################
# Read in raw data 
#######################################################################################

psid_clean = read.csv("../../data/psid_clean.csv")

#######################################################################################
# Consumption Floor
#######################################################################################

# 15% of medium head positive prime-working earnings
working_ind = psid_clean %>% 
  filter(Age_recode >= 25 & Age_recode <= 54) %>% 
  filter(Employment_Status_Ind == 1) %>% 
  filter(Labor_Income_Plus_Business_Head > 100) %>% 
  mutate(head_income = Unemployment_Comp_Head + Workers_Comp_Head + Labor_Income_Plus_Business_Head) %>% 
  select(ER30001, ER30002, Year, Age_recode, Employment_Status_Ind, head_income, Labor_Income_Plus_Business_Head, Unemployment_Comp_Head, Workers_Comp_Head) %>% 
  filter(!is.na(head_income))
0.15 *quantile(working_ind$head_income , 0.5)

#######################################################################################
# Family Distribution 
#######################################################################################

psid_fam = psid_clean %>% 
  filter(Survey_Year > 2003) %>% 
  filter(Age_recode >= 30 & Age_recode <= 40) %>% 
  filter(Marital_Status  %in% c("Single", "Married")) %>% 
  filter(Race_Head %in% c("Black", "White")) %>% 
  group_by(Race_Head) %>% 
  mutate( total = n(),   
          Num_Children_FU = if_else(Num_Children_FU >=3 , 3, Num_Children_FU)) %>% 
  ungroup() %>% 
  group_by(Race_Head, Num_Children_FU, Marital_Status) %>% 
  mutate( n = n(),
          perc = n / total) %>% 
  ungroup() %>% 
  distinct(Race_Head, Num_Children_FU, Marital_Status, perc) %>% 
  pivot_wider(names_from = Race_Head, values_from = perc) %>% 
  pivot_wider(names_from = Marital_Status, values_from = c(White, Black)) %>% 
  arrange(Num_Children_FU) %>% 
  select(Num_Children_FU, White_Married, Black_Married, White_Single, Black_Single)

latex_table <- xtable(psid_fam,include.rownames = FALSE,
                      digits = c(0, 0, 4, 4, 4, 4))

write.csv(psid_fam, "../../data/fam_structure.csv")

  