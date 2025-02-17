library(dplyr)

###################################################
# Read in raw data 
###################################################
psid_fam<- read.csv("../data/famtransfer_famfile.csv") %>% rename(ER30001 = ID_1968)
psid_ind<- read.csv("../data/famtransfer_indfile.csv") %>% filter(ER30001 < 3000) %>% select(-Interview_Number)
cpi_data <- openxlsx::read.xlsx("../data/bls_CPI.xlsx") %>% 
  mutate(ratio_2010 = 218.056 / annual.avg) 


###################################################
# Clean data
###################################################
psid_clean = psid_ind %>% 
  # limit to household head
  filter(Relationship_Head %in% c(1,10) & Sequence_Number == 1) %>% 
  left_join(psid_fam, multiple = 'all') %>% # merge individual and family file to get individual id
  mutate(Year = Survey_Year - 1) %>% 
  # Attempt to figure out Youngest Child in or out of FU
  group_by(ER30001, ER30002) %>% 
  mutate(Most_Recent_Child_Born = if_else(Child_Born_Head_Spouse %in% 1:3| Child_Born_Head_Only %in% 1:3 | Child_Born_Spouse_Only %in% 1:3, Year, NA_integer_),
         Most_Recent_Child_Born = if_else(Child_Born_Head_Spouse_2Years %in% 1:3| Child_Born_Head_Only_2Years %in% 1:3 | Child_Born_Spouse_Only_2Years %in% 1:3, Year-1, Most_Recent_Child_Born)) %>% 
  tidyr::fill(Most_Recent_Child_Born, .direction = "down" ) %>% 
  mutate(Age_Youngest = Year - Most_Recent_Child_Born, 
         Have_Dependents = if_else(Age_Youngest <= 21 | Number_Dependents_Head_Spouse > 0, 1, 0)) %>% 
  ungroup() %>% 
  mutate(# Demographic Variables
         Age_Head = case_when(Age_Head >=99 ~ NA,
                              Age_Head == 98 & Survey_Year %in% c(1995,1996) ~ NA, 
                              Age_Head == 0 & Survey_Year %in% c(1994,1995,1996) ~ NA,
                              TRUE ~ Age_Head),
         # Receive Support Variables
         (across(c(Received_Family_Reported_Head,Received_Family_Reported_Spouse,
                   Received_ChildSupport_Reported_Head, Received_ChildSupport_Reported_Spouse,
                   Received_Alimony_Reported_Head), 
                       ~case_when(. == 99999 & Survey_Year == 1993 ~ NA,
                                  . %in% c(99998,99999) & Survey_Year %in% 1994:2001 ~ NA,
                                  . %in% c(999998,999999) & Survey_Year %in% 2003:2021 ~ NA,
                                  TRUE ~ . ))),
         Received_Family_Head = if_else(is.na(Received_Family_Amount_Head),Received_Family_Reported_Head, Received_Family_Amount_Head),
         Received_Family_Spouse = if_else(is.na(Received_Family_Amount_Spouse),Received_Family_Reported_Spouse, Received_Family_Amount_Spouse),
         Received_ChildSupport_Head = if_else(is.na(Received_ChildSupport_Amount_Spouse),Received_ChildSupport_Reported_Head, Received_ChildSupport_Amount_Spouse),
         Received_ChildSupport_Spouse = if_else(is.na(Received_ChildSupport_Amount_Spouse),Received_ChildSupport_Reported_Spouse, Received_ChildSupport_Amount_Spouse),
         Received_Alimony_Head = if_else(is.na(Received_Alimony_Amount_Head), Received_Alimony_Reported_Head, Received_Alimony_Amount_Head),
         Received_Support_Amount_Head_Spouse = Received_Family_Head + Received_Family_Spouse, # this already does not include alimony and child support
         # 1988 Survey Synchronize 
         Supported1 = if_else(Survey_Year == 1988 & Provided_Support_ExPartner_88 > 0, "Ex/Current Partner", as.character(Supported1)),
         Supported2 = if_else(Survey_Year == 1988 & Provided_Support_Children_88 > 0, "Child", as.character(Supported2)),
         Supported3 = if_else(Survey_Year == 1988 & Provided_Support_Parents_88 > 0, "Parent", as.character(Supported3)),
         Supported4 = if_else(Survey_Year == 1988 & Provided_Support_Siblings_88 > 0, "Sibling", as.character(Supported4)),
         Supported5 = if_else(Survey_Year == 1988 & Provided_Support_OtherRelative_88 > 0, "Other relative", as.character(Supported5)),
         Provided_Family_Support_Total_88 = if_else(Survey_Year == 1988, Provided_Support_Total_88 - Provided_Support_NonRelative_88 - Provided_Support_Unknown_88, Provided_Support_Total_88), 
         # Provide Support Variables
         (across(c(Provided_Support_Amount_Head_Spouse, Provided_Alimony_Amount_Head_Spouse, Provided_ChildSupport_Amount_Head_Spouse), 
                 ~case_when(. == 99999 & Survey_Year <= 1992 ~ NA,
                            . == 9999999 & Survey_Year == 1993 ~ NA,
                            . %in% c(9999998,9999999) & Survey_Year %in% 1994:2021 ~ NA,
                            TRUE ~ . ))),
         (across(c(Supported1, Supported2, Supported3, Supported4, Supported5), 
                   ~case_when(. %in% 10:29 & Survey_Year != 1994 ~ "Ex/Current Partner",
                              . %in% 30:39 & Survey_Year != 1994 ~ "Child",
                              . %in% 40:49 & Survey_Year != 1994 ~ "Sibling",
                              . %in% 50:59 & Survey_Year != 1994 ~"Parent",
                              . %in% 60:65 & Survey_Year != 1994 ~ "(Great)grandchild",
                              . %in% 66:69 & Survey_Year != 1994 ~ "(Great)grandparent",
                              . %in% 70:97 & Survey_Year != 1994 ~ "Other relative",
                              . %in% 98 & Survey_Year != 1994 ~ "Nonrelative",
                              . %in% c(16,17,18,19) & Survey_Year == 1994  ~ "Ex or deceased spouse",
                              . %in% c(10,20,22,25,26) & Survey_Year == 1994  ~ "Partner",
                              . %in% 30:43 & Survey_Year == 1994 ~ "Child",
                              . %in% 45:54 & Survey_Year == 1994 ~ "Sibling",
                              . %in% 55:64 & Survey_Year == 1994 ~ "Parent",
                              . %in% c(65,66,71,72) & Survey_Year == 1994 ~ "(Great)grandchild",
                              . %in% c(67:70,73:76) & Survey_Year == 1994 ~ "(Great)grandparent",
                              . %in% 77:97 & Survey_Year == 1994 ~ "Other relative",
                              . %in% c(27,28) & Survey_Year == 1994 ~ "Nonrelative",
                              is.na(.) ~ "NA",
                             TRUE ~ as.character(.)))),
         Provided_Support_Nonrelative = if_else(if_any(starts_with("Supported"), ~ . == "Nonrelative"), 1, 0),
         Provided_Support_Child = if_else(if_any(starts_with("Supported"), ~ . == "Child"), 1, 0),
         Provided_Support_ExPartner = if_else(if_any(starts_with("Supported"), ~ . %in% c("Ex or deceased spouse","Ex/Current Partner")), 1, 0),
         Provided_Support_Unknown = if_else((if_any(starts_with("Supported"), ~ . %in% c("99","998","999")) | if_all(starts_with("Supported"), ~ . == "0")) , 1, 0), 
         Provided_Support_Relative = if_else(if_any(starts_with("Supported"), ~ Provided_Support_Nonrelative != 1 & Provided_Support_Unknown != 1), 1, 0),
         Provided_ChildSupport_Amount_Head_Spouse = if_else(Provided_Support_Child == 0 &  Provided_Support_ExPartner == 0 , 0,Provided_ChildSupport_Amount_Head_Spouse),
         Provided_Family_Support_Total = case_when(Survey_Year == 1988 ~ Provided_Family_Support_Total_88,
                                                   # Unidentified recipient 
                                                   Survey_Year != 1988 & (Provided_Support_Nonrelative == 1|Provided_Support_Unknown == 1) & Provided_Support_Relative == 1~ NA,
                                                   if_all(starts_with("Supported"), ~ . == "0") ~ NA,
                                                   # Nonrelatives only
                                                   Survey_Year != 1988 & Provided_Support_Nonrelative ==1 & Provided_Support_Relative == 0 ~ 0,
                                                   TRUE ~ Provided_Support_Amount_Head_Spouse), 
         Provided_Family_Support_NetACS = case_when(Provided_Family_Support_Total == 0 ~ 0,
                                                  Provided_Family_Support_Total- Provided_ChildSupport_Amount_Head_Spouse - Provided_Alimony_Amount_Head_Spouse < 0 ~ 0,
                                                  TRUE ~ Provided_Family_Support_Total- Provided_ChildSupport_Amount_Head_Spouse - Provided_Alimony_Amount_Head_Spouse), 
         Family_Transfers_Net = Provided_Family_Support_NetACS - Received_Support_Amount_Head_Spouse,
         # Income Variables
         across(c(Unemployment_Comp_Head_Reported,Unemployment_Comp_Spouse_Reported,
                  Workers_Comp_Head_Reported, Workers_Comp_Spouse_Reported), ~case_when(. == 99999 & Survey_Year %in% 1993:2001 ~ NA,
                                                                                        . == 99998 & Survey_Year %in% 1994:2001 ~ NA,
                                                                                        . >= 999998 & Survey_Year %in% 2003:2021 ~ NA,
                                                                                        TRUE ~ .)),
         Unemployment_Comp_Head = if_else(is.na(Unemployment_Comp_Head),Unemployment_Comp_Head_Reported, Unemployment_Comp_Head),
         Unemployment_Comp_Spouse= if_else(is.na(Unemployment_Comp_Spouse),Unemployment_Comp_Spouse_Reported, Unemployment_Comp_Spouse),
         Workers_Comp_Head = if_else(is.na(Workers_Comp_Head),Workers_Comp_Head_Reported, Workers_Comp_Head),
         Workers_Comp_Spouse = if_else(is.na(Workers_Comp_Spouse),Workers_Comp_Spouse_Reported, Workers_Comp_Spouse)) %>% 
  # CPI adjustment
  left_join(cpi_data %>% select(year, ratio_2010), by = c("Year" = "year")) %>% 
  mutate(Total_Family_Income = Total_Family_Income *ratio_2010, 
         Received_Support_Amount_Head_Spouse = Received_Support_Amount_Head_Spouse * ratio_2010,
         Provided_Family_Support_NetACS = Provided_Family_Support_NetACS * ratio_2010,
         Family_Transfers_Net = Family_Transfers_Net * ratio_2010,
         Unemployment_Comp_Head  = Unemployment_Comp_Head * ratio_2010,
         Workers_Comp_Head  = Workers_Comp_Head * ratio_2010,
         Unemployment_Comp_Spouse = Unemployment_Comp_Spouse * ratio_2010,
         Workers_Comp_Spouse  = Workers_Comp_Spouse * ratio_2010) 

write.csv(psid_clean, "../data/psid_clean.csv")

# selection criteria and cleaned variables
psid_select = psid_clean %>%   
  filter(Survey_Year >= 1985,
         Age_Head >= 17,
         Race_Head %in% c(1,2),
         Race_Head_2nd == 0) %>%  
  select(Family_ID, ER30001, ER30002, Survey_Year, Year, Age_Head, Age, Race_Head, Marital_Status, Family_Unit_Size, Num_Children_FU,
         Provided_Family_Support_NetACS, Received_Support_Amount_Head_Spouse,Unemployment_Comp_Head, Unemployment_Comp_Spouse,
         Workers_Comp_Head, Workers_Comp_Spouse)

write.csv(psid_select, "../data/psid_select.csv")

###################################################
# Aggregate to 2 years
###################################################

psid_model_data <- psid_select %>% 
  # double biennial year
  full_join(psid_select %>%
            filter(Survey_Year >= 1999) %>% 
            mutate(Year = Year -1,Age = Age -1)) %>% 
  # age categories
  mutate(age_cat = (Age - 21) %/% 2 + 1) %>% 
       #  age_cat = if_else(Age %in% 17-20, "18-20", age_cat)) %>% 
  # aggregated variables
  group_by(ER30001, ER30002, age_cat) %>% 
  mutate(n = n(),
         sum_transfer_out = sum(Provided_Family_Support_NetACS),
         sum_transfer_in = sum(Received_Support_Amount_Head_Spouse),
         sum_unemp_insurance = sum(Unemployment_Comp_Head + Unemployment_Comp_Spouse),
         sum_workers_comp = sum(Workers_Comp_Head + Workers_Comp_Spouse),
         years_married = sum(Marital_Status == "Married"),
         avg_FU_size= mean(Family_Unit_Size),
         Year_first = min(Year)) %>% 
  ungroup() %>% 
  # received/provided in past
  group_by(ER30001, ER30002) %>% 
  mutate(received_transfer_past = cumsum(sum_transfer_in > 0) > 0 & row_number() > 1,
         provided_transfer_past = cumsum(sum_transfer_out > 0) > 0 & row_number() > 1) %>% 
  ungroup()
  
# write output
write.csv(psid_model_data, "../data/psid_model_data.csv")

###################################################
# Testing 
###################################################

# Age 
# ages =psid_clean %>% 
#   group_by(Race_Head, Age_Head) %>% 
#   summarize(n = n())

# Receive_Family_Reported_Head:
#topcoded to 999997 from 2003 to 2021
#topcoded to 99997 from 1994 to 2001
#topcoded 99,998 in 1993
# 

# Provide_Support_Head:
#topcoded to 99998 from 1985-1992
#topcoded to 9999998 in 1993
#topcoded to 9999997 in 1994-2021

# How much is filtered out when excluding all that answered non_relative
# 318 gave to both a nonrelative and listed relative (non refused answer)
# test= psid_clean %>% 
#   mutate(Provided_Support_Relative = if_else(if_any(starts_with("Supported"), ~ . != "Nonrelative" & .!= "0" & .!= "99" & .!= "999" & .!= "998"), 1, 0)) %>% 
#   filter(Provided_Support_Nonrelative == 1 & Provided_Support_Relative ==1 ) %>% 
#   filter(Provided_Support_NetACS > 0) %>% 
#   select(Family_ID, ER30001,Year, Survey_Year,Age_Head, Race_Head, Marital_Status, Num_Children_FU,Provided_Support_Amount_Head_Spouse, Provided_Alimony_Amount_Head_Spouse,
#          Provided_ChildSupport_Amount_Head_Spouse,Provided_Support_NetACS, Supported1, Supported2, Supported3, Supported4, Supported5, Number_Supported)
