#####################################################################
#Title: Public Health Care Delivery
#Author: Muhammad Uzair Aslam
#Date: 7/1/2021
#Purpose: Analyze Pre and Post natal consultation
#Section of Questionnaire : PSLM 2020 Section J
#####################################################################

#clear global environment
rm(list = ls())

library(foreign)
library(tidyverse)


#load the health data
secJ <- read.spss(file = "E:/IHI-2/Project/IHI-2/Data/PSLM spss data//SecJ.sav",use.value.label=TRUE,to.data.frame=TRUE)
secJ <- as_tibble(secJ)

#select the required data
pre_and_post <- select(secJ,c(Province,Region,district,sjaq01,sjaq2a,sjaq2b,sjaq03,sjaq05,sjaq06,sjaq09,sjaq10,sjaq11a,sjaq11b,sjaq12))

#rename column names
pp_care <- rename(
  pre_and_post,
  province = Province,
  region = Region,
  given_birth = sjaq01,
  prenatal_consult = sjaq2a,prenatal_consult_visits = sjaq2b,
  prenatal_consult_place = sjaq03,
  TT_during_preg = sjaq05,
  No_injections = sjaq06,
  birth_place = sjaq09,
  delivery_assistance = sjaq10,
  postnatal_consult = sjaq11a,postnatal_consult_visits = sjaq11b,
  postnatal_consult_place = sjaq12
)

#find column wise null values
map(pp_care, ~sum(is.na(.)))

############################CODING AND RENAMING VARIABLES################################################################################################
#rename given birth from live Birth = 1 Still Birth = 2 No = 3
levels(pp_care$given_birth)[levels(pp_care$given_birth)=="yes live birth"] <- "1"
levels(pp_care$given_birth)[levels(pp_care$given_birth)=="yes still birth"] <- "2"
levels(pp_care$given_birth)[levels(pp_care$given_birth)=="No"] <- "3"

#rename prenatal_consult from yes = 1 and no = 2
levels(pp_care$prenatal_consult) <- c("1","2")

#change prenatal_consultation type from numeric to factor
pp_care$prenatal_consult_visits = as.factor(pp_care$prenatal_consult_visits)

#rename prenatal_consultation_visits : 1 to 3 visits = 1, 4 to 6 visits = 2, greater than 6 visits = 3
levels(pp_care$prenatal_consult_visits)[levels(pp_care$prenatal_consult_visits)=="1"]  <- "1"
levels(pp_care$prenatal_consult_visits)[levels(pp_care$prenatal_consult_visits)=="2"]  <- "1"
levels(pp_care$prenatal_consult_visits)[levels(pp_care$prenatal_consult_visits)=="3"]  <- "1"
levels(pp_care$prenatal_consult_visits)[levels(pp_care$prenatal_consult_visits)=="4"]  <- "2"
levels(pp_care$prenatal_consult_visits)[levels(pp_care$prenatal_consult_visits)=="5"]  <- "2"
levels(pp_care$prenatal_consult_visits)[levels(pp_care$prenatal_consult_visits)=="6"]  <- "2"
levels(pp_care$prenatal_consult_visits)[levels(pp_care$prenatal_consult_visits)=="7"]  <- "3"
levels(pp_care$prenatal_consult_visits)[levels(pp_care$prenatal_consult_visits)=="8"]  <- "3"
levels(pp_care$prenatal_consult_visits)[levels(pp_care$prenatal_consult_visits)=="9"]  <- "3"
levels(pp_care$prenatal_consult_visits)[levels(pp_care$prenatal_consult_visits)=="21"] <- "3"


#rename prenatal_consult_place : Private Hos = 1,Govt Hos = 2, Home-TBA = 3, Home-LHW =3, Home-LHV = 3,Home Doc = 3,Other = 3   
levels(pp_care$prenatal_consult_place)[levels(pp_care$prenatal_consult_place)=="Private Hosp/Clinic"] <- "1"
levels(pp_care$prenatal_consult_place)[levels(pp_care$prenatal_consult_place)=="Govt Hosp/Clinic"] <- "2"
levels(pp_care$prenatal_consult_place)[levels(pp_care$prenatal_consult_place)=="Home-TBA"] <- "3"
levels(pp_care$prenatal_consult_place)[levels(pp_care$prenatal_consult_place)=="Home-LHW"] <- "3"
levels(pp_care$prenatal_consult_place)[levels(pp_care$prenatal_consult_place)=="Home-LHV"] <- "3"
levels(pp_care$prenatal_consult_place)[levels(pp_care$prenatal_consult_place)=="Home-Doctor"] <- "3"
levels(pp_care$prenatal_consult_place)[levels(pp_care$prenatal_consult_place)=="Other"] <- "4"


#rename TT_during_preg from yes = 1 and no = 2
levels(pp_care$TT_during_preg) <- c("1","2")

#rename birthplace : Private Hos = 1,Govt Hos = 2, Home = 3 ,Others = 4
levels(pp_care$birth_place)[levels(pp_care$birth_place)=="Private Hospital/Clinic"] <- "1"
levels(pp_care$birth_place)[levels(pp_care$birth_place)=="Govt Hospital/Clinic"] <- "2"
levels(pp_care$birth_place)[levels(pp_care$birth_place)=="Home"] <- "3"
levels(pp_care$birth_place)[levels(pp_care$birth_place)=="Other"] <- "4"

#rename delivery assistance with Doctors = 1 , Nurses,LHV,LHW = 2, Others 3
levels(pp_care$delivery_assistance)[levels(pp_care$delivery_assistance)=="Doctor"] <- "1"
levels(pp_care$delivery_assistance)[levels(pp_care$delivery_assistance)=="Nurse"] <- "2"
levels(pp_care$delivery_assistance)[levels(pp_care$delivery_assistance)=="LHV"] <- "2"
levels(pp_care$delivery_assistance)[levels(pp_care$delivery_assistance)=="LHW"] <- "2"
levels(pp_care$delivery_assistance)[levels(pp_care$delivery_assistance)=="Family member/relative/neighbour"] <- "3"
levels(pp_care$delivery_assistance)[levels(pp_care$delivery_assistance)=="Midwife"] <- "3"
levels(pp_care$delivery_assistance)[levels(pp_care$delivery_assistance)=="TBA"] <- "3"
levels(pp_care$delivery_assistance)[levels(pp_care$delivery_assistance)=="Trained Dai"] <- "3"
levels(pp_care$delivery_assistance)[levels(pp_care$delivery_assistance)=="Other"] <- "3"

#rename postnatal_consultancy from yes = 1 and no = 2
levels(pp_care$postnatal_consult) <- c("1","2")

#change prenatal_consultation type from numeric to factor
pp_care$postnatal_consult_visits = as.factor(pp_care$postnatal_consult_visits)

#rename postnatal_consultation_visits : 1 to 2 visits = 1, 3 to 4 visits = 2, greater than 4 visits = 3
levels(pp_care$postnatal_consult_visits)[levels(pp_care$postnatal_consult_visits)=="1"]  <- "1"
levels(pp_care$postnatal_consult_visits)[levels(pp_care$postnatal_consult_visits)=="2"]  <- "1"
levels(pp_care$postnatal_consult_visits)[levels(pp_care$postnatal_consult_visits)=="3"]  <- "2"
levels(pp_care$postnatal_consult_visits)[levels(pp_care$postnatal_consult_visits)=="4"]  <- "2"
levels(pp_care$postnatal_consult_visits)[levels(pp_care$postnatal_consult_visits)=="5"]  <- "3"
levels(pp_care$postnatal_consult_visits)[levels(pp_care$postnatal_consult_visits)=="6"]  <- "3"

#rename postnatal_consult_place : Private Hos = 1,Govt Hos = 2, Home-TBA = 3, Home-LHW =3, Home-LHV = 3,Home Doc = 3,Other = 3   
levels(pp_care$postnatal_consult_place)[levels(pp_care$postnatal_consult_place)=="Private Hosp/Clinic"] <- "1"
levels(pp_care$postnatal_consult_place)[levels(pp_care$postnatal_consult_place)=="Govt Hosp/Clinic"] <- "2"
levels(pp_care$postnatal_consult_place)[levels(pp_care$postnatal_consult_place)=="Home-TBA"] <- "3"
levels(pp_care$postnatal_consult_place)[levels(pp_care$postnatal_consult_place)=="Home-LHW"] <- "3"
levels(pp_care$postnatal_consult_place)[levels(pp_care$postnatal_consult_place)=="Home-LHV"] <- "3"
levels(pp_care$postnatal_consult_place)[levels(pp_care$postnatal_consult_place)=="Home-Doctor"] <- "3"
levels(pp_care$postnatal_consult_place)[levels(pp_care$postnatal_consult_place)=="Other"] <- "4"

####################TABLES#####################################################################################################################################

#TABLE 1
#birth in provinces
province_birth <-
  pp_care %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(total_birth = sum(given_birth == 1) +sum(given_birth == 2),
            total_live_birth = sum(given_birth == 1),
            total_still_birth = sum(given_birth == 2),
            percent_live_birth = (total_live_birth/total_birth)*100,
            percent_still_birth = (total_still_birth/total_birth)*100) %>%
  arrange(province)

#TABLE 2
#birth in districts
district_birth <-
  pp_care %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(total_birth = sum(given_birth == 1) +sum(given_birth == 2),
            total_live_birth = sum(given_birth == 1),
            total_still_birth = sum(given_birth == 2),
            percent_live_birth = (total_live_birth/total_birth)*100,
            percent_still_birth = (total_still_birth/total_birth)*100) %>%
  arrange(district)

write.csv(x=district_birth,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/pre and post natal/district_birth.csv")

#TABLE 3
#province prenatal consultation
province_prenatal <-
  pp_care %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(total_birth = sum(prenatal_consult == 1,na.rm = TRUE) + sum(prenatal_consult == 2,na.rm = TRUE),
            total_consulted = sum(prenatal_consult==1,na.rm = TRUE),
            total_not_consulted = sum(prenatal_consult==2,na.rm = TRUE),
            percent_consulted = (total_consulted/total_birth)*100,
            percent_not_consulted =(total_not_consulted/total_birth)*100 ) %>%
  arrange(province)

#TABLE 4
#district prenatal consultation
district_prenatal <-
  pp_care %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(total_birth = sum(prenatal_consult == 1,na.rm = TRUE) + sum(prenatal_consult == 2,na.rm = TRUE),
            total_consulted = sum(prenatal_consult==1,na.rm = TRUE),
            total_not_consulted = sum(prenatal_consult==2,na.rm = TRUE),
            percent_consulted = (total_consulted/total_birth)*100,
            percent_not_consulted =(total_not_consulted/total_birth)*100 ) %>%
  arrange(district)
write.csv(x=district_prenatal,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/pre and post natal/district_prenatal.csv")

#TABLE 5
#province prenatal consultation visits
province_prenatal_visits <-
  pp_care %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(n(),
            total_visits = sum(prenatal_consult_visits == 1,na.rm = TRUE) + sum(prenatal_consult_visits == 2,na.rm = TRUE) + sum(prenatal_consult_visits == 3,na.rm = TRUE),
            one_to_three = sum(prenatal_consult_visits==1,na.rm = TRUE),
            four_to_six = sum(prenatal_consult_visits == 2,na.rm = TRUE),
            more_than_six = sum(prenatal_consult_visits == 3,na.rm = TRUE),
            percent_one_to_three = (one_to_three/total_visits)*100,
            percent_four_to_six = (four_to_six/total_visits)*100,
            percent_more_than_six = (more_than_six/total_visits)*100,) %>%
  arrange(province)

#TABLE 6
#district prenatal consultation visits
district_prenatal_visits <-
  pp_care %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(n(),
            total_visits = sum(prenatal_consult_visits == 1,na.rm = TRUE) + sum(prenatal_consult_visits == 2,na.rm = TRUE) + sum(prenatal_consult_visits == 3,na.rm = TRUE),
            one_to_three = sum(prenatal_consult_visits==1,na.rm = TRUE),
            four_to_six = sum(prenatal_consult_visits == 2,na.rm = TRUE),
            more_than_six = sum(prenatal_consult_visits == 3,na.rm = TRUE),
            percent_one_to_three = (one_to_three/total_visits)*100,
            percent_four_to_six = (four_to_six/total_visits)*100,
            percent_more_than_six = (more_than_six/total_visits)*100,) %>%
  arrange(district)
write.csv(x=district_prenatal_visits,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/pre and post natal/district_prenatal_visits.csv")

#TABLE 7
#prenatal consultation Place province
province_prenatal_consultation_place <- 
  pp_care %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarise(Total_places = sum(prenatal_consult_place==1,prenatal_consult_place==2,prenatal_consult_place==3,na.rm = TRUE),
            Private_Hospital = sum(prenatal_consult_place==1,na.rm = TRUE),
            Govt_Dispen = sum(prenatal_consult_place==2,na.rm = TRUE),
            Home_LHV = sum(prenatal_consult_place==3,na.rm = TRUE),
            Other = sum(prenatal_consult_place==4,na.rm = TRUE),
            Percent_Private_Hospitals = (Private_Hospital/Total_places)*100,
            Percent_Govt_Hospitals = (Govt_Dispen/Total_places)*100,
            Percent_Home = (Home_LHV/Total_places)*100,
            Percent_Others = (Other/Total_places)*100,
  ) %>%
  arrange(province)

#TABLE 8
#prenatal consultation Place district
district_prenatal_consultation_place <- 
  pp_care %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarise(Total_places = sum(prenatal_consult_place==1,prenatal_consult_place==2,prenatal_consult_place==3,na.rm = TRUE),
            Private_Hospital = sum(prenatal_consult_place==1,na.rm = TRUE),
            Govt_Dispen = sum(prenatal_consult_place==2,na.rm = TRUE),
            Home_LHV = sum(prenatal_consult_place==3,na.rm = TRUE),
            Other = sum(prenatal_consult_place==4,na.rm = TRUE),
            Percent_Private_Hospitals = (Private_Hospital/Total_places)*100,
            Percent_Govt_Hospitals = (Govt_Dispen/Total_places)*100,
            Percent_Home = (Home_LHV/Total_places)*100,
            Percent_Others = (Other/Total_places)*100,
  ) %>%
  arrange(district)
write.csv(x=district_prenatal_consultation_place,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/pre and post natal/district_prenatal_consultation_place.csv")

#TABLE 9
#province tetnus toxoid
province_tetnus_toxoid <-
  pp_care %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(total_response = sum(TT_during_preg == 1,na.rm = TRUE) + sum(TT_during_preg == 2,na.rm = TRUE),
            recieved = sum(TT_during_preg==1,na.rm = TRUE),
            not_recieved = sum(TT_during_preg==2,na.rm = TRUE),
            percent_recieved = (recieved/total_response)*100,
            percent_not_consulted =(not_recieved/total_response)*100 ) %>%
  arrange(province)

#TABLE 10
#district tetanus toxoid
district_tetnus_toxoid <-
  pp_care %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(total_response = sum(TT_during_preg == 1,na.rm = TRUE) + sum(TT_during_preg == 2,na.rm = TRUE),
            recieved = sum(TT_during_preg==1,na.rm = TRUE),
            not_recieved = sum(TT_during_preg==2,na.rm = TRUE),
            percent_recieved = (recieved/total_response)*100,
            percent_not_consulted =(not_recieved/total_response)*100 ) %>%
  arrange(district)
write.csv(x=district_tetnus_toxoid,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/pre and post natal/district_tetnus_toxoid.csv")

#TABLE 11
#province birth_place 
province_birth_place <- 
pp_care %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarise(Total_places = sum(birth_place==1,birth_place==2,birth_place==3,na.rm = TRUE,birth_place==4,na.rm = TRUE),
            Private_Hospital = sum(birth_place==1,na.rm = TRUE),
            Govt_Dispen = sum(birth_place==2,na.rm = TRUE),
            Home = sum(birth_place==3,na.rm = TRUE),
            Other = sum(birth_place==4,na.rm = TRUE),
            Percent_Private_Hospitals = (Private_Hospital/Total_places)*100,
            Percent_Govt_Hospitals = (Govt_Dispen/Total_places)*100,
            Percent_Home = (Home/Total_places)*100,
            Percent_Others = (Other/Total_places)*100,
  ) %>%
  arrange(province)

#TABLE 12
#district birth_place 
district_birth_place <- 
  pp_care %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarise(Total_places = sum(birth_place==1,birth_place==2,birth_place==3,na.rm = TRUE,birth_place==4,na.rm = TRUE),
            Private_Hospital = sum(birth_place==1,na.rm = TRUE),
            Govt_Dispen = sum(birth_place==2,na.rm = TRUE),
            Home = sum(birth_place==3,na.rm = TRUE),
            Other = sum(birth_place==4,na.rm = TRUE),
            Percent_Private_Hospitals = (Private_Hospital/Total_places)*100,
            Percent_Govt_Hospitals = (Govt_Dispen/Total_places)*100,
            Percent_Home = (Home/Total_places)*100,
            Percent_Others = (Other/Total_places)*100,
  ) %>%
  arrange(district)
write.csv(x=district_birth_place,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/pre and post natal/district_birth_place.csv")

#TABLE 13
# province Delivery Assistance 
province_delivery_assistance <- 
  pp_care %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarise(Total= sum(delivery_assistance == 1,delivery_assistance == 2,delivery_assistance == 3,na.rm = TRUE),
            Doc = sum(delivery_assistance==1,na.rm = TRUE),
            Nurses = sum(delivery_assistance==2,na.rm = TRUE),
            Other = sum(delivery_assistance==3,na.rm = TRUE),
            Percent_Doc = (Doc/Total)*100,
            Percent_Nurses = (Nurses/Total)*100,
            Percent_Others = (Other/Total)*100,
  ) %>%
  arrange(province)

#TABLE 13
#district Delivery Assistance
district_delivery_assistance <- 
  pp_care %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarise(Total= sum(delivery_assistance == 1,delivery_assistance == 2,delivery_assistance == 3,na.rm = TRUE),
            Doc = sum(delivery_assistance==1,na.rm = TRUE),
            Nurses = sum(delivery_assistance==2,na.rm = TRUE),
            Other = sum(delivery_assistance==3,na.rm = TRUE),
            Percent_Doc = (Doc/Total)*100,
            Percent_Nurses = (Nurses/Total)*100,
            Percent_Others = (Other/Total)*100,
  ) %>%
  arrange(district)
write.csv(x=district_delivery_assistance,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/pre and post natal/district_delivery_assistance.csv")



#TABLE 14
#province postnatal consultation
province_postnatal <-
  pp_care %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(total_birth = sum(postnatal_consult == 1,na.rm = TRUE) + sum(postnatal_consult == 2,na.rm = TRUE),
            total_consulted = sum(postnatal_consult==1,na.rm = TRUE),
            total_not_consulted = sum(postnatal_consult==2,na.rm = TRUE),
            percent_consulted = (total_consulted/total_birth)*100,
            percent_not_consulted =(total_not_consulted/total_birth)*100 ) %>%
  arrange(province)

#TABLE 15
#district postnatal consultation
district_postnatal <-
  pp_care %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(total_birth = sum(postnatal_consult == 1,na.rm = TRUE) + sum(postnatal_consult == 2,na.rm = TRUE),
            total_consulted = sum(postnatal_consult==1,na.rm = TRUE),
            total_not_consulted = sum(postnatal_consult==2,na.rm = TRUE),
            percent_consulted = (total_consulted/total_birth)*100,
            percent_not_consulted =(total_not_consulted/total_birth)*100 ) %>%
  arrange(district)
write.csv(x=district_postnatal,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/pre and post natal/district_postnatal.csv")


#TABLE 16
#province postnatal consultation visits
province_postnatal_visits <-
  pp_care %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(n(),
            total_visits = sum(postnatal_consult_visits == 1,na.rm = TRUE) + sum(postnatal_consult_visits == 2,na.rm = TRUE) + sum(postnatal_consult_visits == 3,na.rm = TRUE),
            one_to_three = sum(postnatal_consult_visits==1,na.rm = TRUE),
            four_to_six = sum(postnatal_consult_visits == 2,na.rm = TRUE),
            more_than_six = sum(postnatal_consult_visits == 3,na.rm = TRUE),
            percent_one_to_three = (one_to_three/total_visits)*100,
            percent_four_to_six = (four_to_six/total_visits)*100,
            percent_more_than_six = (more_than_six/total_visits)*100,) %>%
  arrange(province)

#TABLE 17
#district prenatal consultation visits
district_postnatal_visits <-
  pp_care %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(n(),
            total_visits = sum(postnatal_consult_visits == 1,na.rm = TRUE) + sum(postnatal_consult_visits == 2,na.rm = TRUE) + sum(postnatal_consult_visits == 3,na.rm = TRUE),
            one_to_three = sum(postnatal_consult_visits==1,na.rm = TRUE),
            four_to_six = sum(postnatal_consult_visits == 2,na.rm = TRUE),
            more_than_six = sum(postnatal_consult_visits == 3,na.rm = TRUE),
            percent_one_to_three = (one_to_three/total_visits)*100,
            percent_four_to_six = (four_to_six/total_visits)*100,
            percent_more_than_six = (more_than_six/total_visits)*100,) %>%
  arrange(district)
write.csv(x=district_postnatal_visits,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/pre and post natal/district_postnatal_visits.csv")


#TABLE 18
#prenatal consultation Place province
province_postnatal_consultation_place <- 
  pp_care %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarise(Total_places = sum(postnatal_consult_place==1,postnatal_consult_place==2,postnatal_consult_place==3,na.rm = TRUE),
            Private_Hospital = sum(postnatal_consult_place==1,na.rm = TRUE),
            Govt_Dispen = sum(postnatal_consult_place==2,na.rm = TRUE),
            Home_LHV = sum(postnatal_consult_place==3,na.rm = TRUE),
            Other = sum(postnatal_consult_place==4,na.rm = TRUE),
            Percent_Private_Hospitals = (Private_Hospital/Total_places)*100,
            Percent_Govt_Hospitals = (Govt_Dispen/Total_places)*100,
            Percent_Home_LHV = (Home_LHV/Total_places)*100,
            Percent_Others = (Other/Total_places)*100,
  ) %>%
  arrange(province)

#TABLE 19
#prenatal consultation Place district
district_postnatal_consultation_place <- 
  pp_care %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarise(Total_places = sum(postnatal_consult_place==1,postnatal_consult_place==2,postnatal_consult_place==3,na.rm = TRUE),
            Private_Hospital = sum(postnatal_consult_place==1,na.rm = TRUE),
            Govt_Dispen = sum(postnatal_consult_place==2,na.rm = TRUE),
            Home_LHV = sum(postnatal_consult_place==3,na.rm = TRUE),
            Other = sum(postnatal_consult_place==4,na.rm = TRUE),
            Percent_Private_Hospitals = (Private_Hospital/Total_places)*100,
            Percent_Govt_Hospitals = (Govt_Dispen/Total_places)*100,
            Percent_Home_LHV = (Home_LHV/Total_places)*100,
            Percent_Others = (Other/Total_places)*100,
  ) %>%
  arrange(district)
write.csv(x=district_postnatal_consultation_place,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/pre and post natal/district_postnatal_consultation_place.csv")


#TABLE 20
#province no of tetnus toxoid

province_no_tt <- 
  pp_care %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarise(Total_responses = sum(No_injections==0,No_injections==1,No_injections==2,na.rm = TRUE),
            without_injection = sum(No_injections == 0,na.rm = TRUE),
            one_injection  = sum(No_injections == 1,na.rm = TRUE),
            two_injection = sum(No_injections == 2,na.rm = TRUE),
            percent_without_inject = (without_injection/Total_responses)*100,
            percent_one_inject = (one_injection/Total_responses)*100,
            percent_two_inject = (two_injection/Total_responses)*100,
            ) %>%
  arrange(province)

#TABLE 21
#district no of tetnus toxoid

district_no_tt <- 
  pp_care %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarise(Total_responses = sum(No_injections==0,No_injections==1,No_injections==2,na.rm = TRUE),
            without_injection = sum(No_injections == 0,na.rm = TRUE),
            one_injection  = sum(No_injections == 1,na.rm = TRUE),
            two_injection = sum(No_injections == 2,na.rm = TRUE),
            percent_without_inject = (without_injection/Total_responses)*100,
            percent_one_inject = (one_injection/Total_responses)*100,
            percent_two_inject = (two_injection/Total_responses)*100,
  ) %>%
  arrange(district)
write.csv(x=district_no_tt,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/pre and post natal/district_no_tt.csv")



