library(tidyverse)

#######################################################################################
# Survival Probabilities
#######################################################################################

mortality_data <- read.delim("../../data/Death_2018.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

mortality_data_clean <- mortality_data %>% 
  filter(Notes == "", Single.Year.Ages.Code < 84, Single.Year.Ages.Code > 61) %>% 
  select(-c(Notes, Single.Race.6.Code, Single.Year.Ages)) %>% 
  rename(Race = Single.Race.6,
         Age = Single.Year.Ages.Code ) %>% 
  mutate(Race = if_else(Race == "White", "White_Ind", "Black_Ind"),
         Rate = Deaths/as.numeric(Population),
         Survival = 1 - Rate) 



#write.csv(mortality_data_clean, "../../data/death_rates_race_age.csv")

mortality_wide = mortality_data_clean %>% 
  select(Race, Survival, Age) %>% 
  as.data.frame(.) %>% 
  pivot_wider(names_from = Race, values_from = Survival) %>% 
  mutate(age_cat = paste(20 + (Age - 20) %/% 2 * 2, 
                  21 + (Age - 20) %/% 2 * 2, 
                  sep = "-")) %>% 
  group_by(age_cat) %>% 
  mutate(Black = prod(Black_Ind),
         White = prod(White_Ind), ,
         j = (min(Age) - 18) / 2 + 1 ) %>% 
  ungroup() %>% 
  distinct(White,  Black, age_cat, j)  
  
rownames(mortality_wide) <- NULL

latex_table <- xtable(mortality_wide %>% 
                        rename(Age = age_cat) %>% 
                        select(Age, White, Black),include.rownames = FALSE,
                      digits = c(0, 4, 4, 4))


write.csv(mortality_wide, "../../data/death_rates_race_age.csv")
