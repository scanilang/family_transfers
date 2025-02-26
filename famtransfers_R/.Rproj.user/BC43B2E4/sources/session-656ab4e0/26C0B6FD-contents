library(dplyr)

###################################################
# Read in raw data 
###################################################
psid_fam<- read.csv("../data/famtransfer_famfile.csv") %>% rename(ER30001 = ID_1968)
cpi_data <- openxlsx::read.xlsx("../data/bls_CPI.xlsx") %>% 
  mutate(ratio_2010 = 218.056 / annual.avg) 


###################################################
# Clean data
###################################################
psid_clean = psid_fam %>% 
  filter(Survey_Year >= 1985) %>% 
  mutate(Year = Survey_Year - 1) %>% 
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
         Provided_ChildSupport_Amount_Head_Spouse_clean = if_else(Provided_Support_Child == 0 &  Provided_Support_ExPartner == 0 , 0,Provided_ChildSupport_Amount_Head_Spouse),
         Provided_Support_NetACS = Provided_Support_Amount_Head_Spouse - Provided_Alimony_Amount_Head_Spouse - Provided_ChildSupport_Amount_Head_Spouse_clean,
         Provided_Support_NetACS_clean = case_when(Provided_Support_NetACS ==0 ~ 0,
                                                  (Provided_Support_Nonrelative == 1|Provided_Support_Unknown == 1) & Provided_Support_Relative == 1~ NA, 
                                                  Provided_Support_Nonrelative ==1 & Provided_Support_Relative == 0 ~ 0,
                                                  Provided_Support_NetACS > 0 & if_all(starts_with("Supported"), ~ . == "0") ~ NA,
                                            # Supported1 == "Ex/Current Partner" 
                                             TRUE ~ Provided_Support_NetACS), 
         Family_Transfers_Net = Provided_Support_NetACS - Received_Support_Amount_Head_Spouse,
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
         Receive_Head_Spouse_Net = Receive_Head_Spouse_Net * ratio_2010,
         Support_Amount_Net = Support_Amount_Net * ratio_2010,
         Unemployment_Comp_Head  = Unemployment_Comp_Head * ratio_2010,
         Workers_Comp_Head  = Workers_Comp_Head * ratio_2010) %>% 
  # selection criteria
  filter(Race_Head %in% c(1,2),
         Race_Head_2nd == 0,# white or black only
         Age_Head >= 17) %>%  # white or black only
  select(Family_ID, ER30001,Year, Survey_Year,Age_Head, Race_Head, Head_College_Degree, Marital_Status, Family_Unit_Size, Num_Children_FU, everything())

psid_select = psid_clean %>% 
  select(Family_ID, ER30001,Year, Survey_Year,Age_Head, Race_Head, Marital_Status, Num_Children_FU,Provided_Support_Amount_Head_Spouse, Provided_Alimony_Amount_Head_Spouse,
         Provided_ChildSupport_Amount_Head_Spouse,Provided_ChildSupport_Amount_Head_Spouse_clean,Provided_Support_NetACS,Provided_Support_NetACS_clean, Provided_Support_Child, Provided_Support_ExPartner,Supported1, Supported2, Supported3, Supported4, Supported5, Number_Supported) %>% 
  filter(Provided_ChildSupport_Amount_Head_Spouse >0 |Provided_Alimony_Amount_Head_Spouse >0 ) #%>% 
 # filter(Provided_Alimony_Amount_Head_Spouse > 0 & Number_Supported > 1)
         
psid_select = psid_clean %>% 
  select(Family_ID, ER30001,Year, Survey_Year,Age_Head, Race_Head, Marital_Status, Num_Children_FU,Provided_Support_Amount_Head_Spouse, ,Provided_Alimony_Amount_Head_Spouse,
         Provided_ChildSupport_Amount_Head_Spouse,Provided_ChildSupport_Amount_Head_Spouse_clean,Provided_Support_NetACS, Provided_Support_NetACS_clean, Supported1, Supported2, Supported3, Supported4, Supported5, Number_Supported) %>% 
  mutate(Support_Not_Child = if_else(if_all(starts_with("Supported"), ~. != "Child"), 1, 0)) %>% 
  filter(Provided_Support_Amount_Head_Spouse >0 ) %>% 
  filter(Support_Not_Child == 1)

test = psid_fam %>% 
  select(Family_ID, ER30001, Survey_Year,Age_Head, Race_Head, Marital_Status, Num_Children_FU,Provided_Support_Amount_Head_Spouse, Provided_Alimony_Amount_Head_Spouse,
         Provided_ChildSupport_Amount_Head_Spouse, Whether_Provided_Support_Head_Spouse, Whether_Alimony, Whether_ChildSupport, Supported1, Supported2, Supported3, Supported4, Supported5, Number_Supported) %>% 
  filter(Supported1 == 0) %>% 
  filter(Provided_Support_Amount_Head_Spouse > 0 & Provided_Support_Amount_Head_Spouse < 100000)
  
psid_select = psid_clean %>% 
  select(Family_ID, ER30001,Year, Survey_Year,Age_Head, Race_Head, Marital_Status, Num_Children_FU,Received_Family_Head, Received_ChildSupport_Head, Received_Alimony_Head)

write.csv(psid_clean, "PSID data/psid_clean.csv")

###################################################
# Aggregate to 2 years
###################################################


###################################################
# Testing topcoding
###################################################

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
