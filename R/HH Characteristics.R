#####################################################################
#Title: Public Health Care Delivery
#Author: Muhammad Uzair Aslam
#Date: 7/19/2021
#Purpose: Analyze Household Characteristics
#Section of Questionnaire : PSLM 2020 Section F1 and Section F2
#####################################################################

#clear global environment
rm(list = ls())

library(foreign)
library(tidyverse)

#load the health data
F1 <- read.spss(file = "E:/IHI-2/Project/IHI-2/Data/PSLM spss data/SecF1.sav",use.value.label=TRUE,to.data.frame=TRUE)
F1 <- as_tibble(F1)

F2 <- read.spss(file = "E:/IHI-2/Project/IHI-2/Data/PSLM spss data/SecF2.sav",use.value.label=TRUE,to.data.frame=TRUE)
F2 <- as_tibble(F2)

#select the required data
secf1 <- select(F1,c(Province,Region,District,SF1q04,SF1q08))
secf2 <- select(F2,c(Province,Region,District,SF2q11))


#rename the columns
secf1 <- rename(secf1, c(rooms = SF1q04,fuel = SF1q08))
secf2 <- rename(secf2,c(toilet = SF2q11))


############################CODING AND RENAMING VARIABLES################################################################################################
#convert rooms to factor variable
secf1$rooms = as.factor(secf1$rooms)

#rename given room from 1 to 2 = 1 , 3 to 4 = 2 , > 5 = 3
levels(secf1$rooms)[levels(secf1$rooms)=="1"] <- "1"
levels(secf1$rooms)[levels(secf1$rooms)=="2"] <- "1"
levels(secf1$rooms)[levels(secf1$rooms)=="3"] <- "2"
levels(secf1$rooms)[levels(secf1$rooms)=="4"] <- "2"
levels(secf1$rooms)[levels(secf1$rooms)=="5"] <- "3"
levels(secf1$rooms)[levels(secf1$rooms)=="6"] <- "3"
levels(secf1$rooms)[levels(secf1$rooms)=="7"] <- "3"
levels(secf1$rooms)[levels(secf1$rooms)=="8"] <- "3"
levels(secf1$rooms)[levels(secf1$rooms)=="9"] <- "3"
levels(secf1$rooms)[levels(secf1$rooms)=="10"] <- "3"
levels(secf1$rooms)[levels(secf1$rooms)=="11"] <- "3"
levels(secf1$rooms)[levels(secf1$rooms)=="12"] <- "3"
levels(secf1$rooms)[levels(secf1$rooms)=="14"] <- "3"
levels(secf1$rooms)[levels(secf1$rooms)=="24"] <- "3"

#rename firewood = 1, LPG,Electricity,Oil,Gas = 2, Dung Cake = 3 , Crop residue = 4 , Charcoal = 5, Others = 6 
levels(secf1$fuel)[levels(secf1$fuel)=="Firewood"] <- "1"
levels(secf1$fuel)[levels(secf1$fuel)=="Gas"] <- "2"
levels(secf1$fuel)[levels(secf1$fuel)=="Kerosene oil"] <- "2"
levels(secf1$fuel)[levels(secf1$fuel)=="Electricity"] <- "2"
levels(secf1$fuel)[levels(secf1$fuel)=="LPG/ Cylinder"] <- "2"
levels(secf1$fuel)[levels(secf1$fuel)=="Dung Cake"] <- "3"
levels(secf1$fuel)[levels(secf1$fuel)=="Crop residue"] <- "4"
levels(secf1$fuel)[levels(secf1$fuel)=="Charcoal/Coal"] <- "5"
levels(secf1$fuel)[levels(secf1$fuel)=="Others (Spedify…….)"] <- "6"

#rename no toilet = 1, flush connected to open drain =2,flush connected to sewerage =2 ,flush connected to septic tank = 2,
#flush to pit =2 , dry =3, composting toilet and others = 4
levels(secf2$toilet)[levels(secf2$toilet)=="No Toilet"] <- "1"
levels(secf2$toilet)[levels(secf2$toilet)=="Flush connected  to open drain"] <- "2"
levels(secf2$toilet)[levels(secf2$toilet)=="Flush connected to public sewerage"] <- "2"
levels(secf2$toilet)[levels(secf2$toilet)=="Flush connected to septic tank"] <- "2"
levels(secf2$toilet)[levels(secf2$toilet)=="Flush connected to pit"] <- "2"
levels(secf2$toilet)[levels(secf2$toilet)=="Dry raised latrine "] <- "3"
levels(secf2$toilet)[levels(secf2$toilet)=="Dry pit latrine"] <- "3"
levels(secf2$toilet)[levels(secf2$toilet)=="Composting toilet"] <- "4"
levels(secf2$toilet)[levels(secf2$toilet)=="Other (specify_____________)"] <- "4"

#########################################TABLES###########################################################################################################
#TABLE 1
#province_no_rooms
province_no_rooms <- 
  secf1 %>%
  filter(Region == " Rural") %>%
  group_by(Province) %>%
  summarise(Total_responses = sum(rooms==1,rooms==2,rooms==3,na.rm = TRUE),
            onetotwo_rooms  = sum(rooms == 1,na.rm = TRUE),
            threetofour_rooms  = sum(rooms == 2,na.rm = TRUE),
            greater_thanfive_rooms  = sum(rooms == 3,na.rm = TRUE),
            percent_one_to_two = (onetotwo_rooms/Total_responses)*100,
            percent_threetofour_rooms = (threetofour_rooms/Total_responses)*100,
            percent_greater_thanfive_rooms = (greater_thanfive_rooms/Total_responses)*100,
  ) %>%
  arrange(Province)

#TABLE 2
#district_no_rooms
district_no_rooms <- 
  secf1 %>%
  filter(Region == " Rural") %>%
  group_by(District) %>%
  summarise(Total_responses = sum(rooms==1,rooms==2,rooms==3,na.rm = TRUE),
            onetotwo_rooms  = sum(rooms == 1,na.rm = TRUE),
            threetofour_rooms  = sum(rooms == 2,na.rm = TRUE),
            greater_thanfive_rooms  = sum(rooms == 3,na.rm = TRUE),
            percent_one_to_two = (onetotwo_rooms/Total_responses)*100,
            percent_threetofour_rooms = (threetofour_rooms/Total_responses)*100,
            percent_greater_thanfive_rooms = (greater_thanfive_rooms/Total_responses)*100,
  ) %>%
  arrange(District)
write.csv(x=district_no_rooms,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/Socia Living Measurement/district_no_rooms.csv")

#TABLE 3
#province fuel used
province_fuel_used <- 
  secf1 %>%
  filter(Region == " Rural") %>%
  group_by(Province) %>%
  summarise(Total_responses = sum(fuel== 1,fuel == 2,fuel == 3,fuel == 4, fuel == 5,fuel == 6,na.rm = TRUE),
            firewood  = sum(fuel == 1,na.rm = TRUE),
            fossil = sum(fuel == 2,na.rm = TRUE),
            dung_Cake = sum(fuel == 3,na.rm = TRUE),
            crop_residue = sum(fuel == 4,na.rm = TRUE),
            charcoal = sum(fuel == 5,na.rm = TRUE),
            others = sum(fuel == 6,na.rm = TRUE),
            percent_firewood = (firewood/Total_responses)*100,
            percent_fossil = (fossil/Total_responses)*100,
            percent_dung_Cake = (dung_Cake/Total_responses)*100,
            percent_crop_residue = (crop_residue/Total_responses)*100,
            percent_charcoal = (charcoal/Total_responses)*100,
            percent_others = (others/Total_responses)*100,
  ) %>%
  arrange(Province)


#TABLE 3
#district fuel used
province_fuel_used <- 
  secf1 %>%
  filter(Region == " Rural") %>%
  group_by(District) %>%
  summarise(Total_responses = sum(fuel== 1,fuel == 2,fuel == 3,fuel == 4, fuel == 5,fuel == 6,na.rm = TRUE),
            firewood  = sum(fuel == 1,na.rm = TRUE),
            fossil = sum(fuel == 2,na.rm = TRUE),
            dung_Cake = sum(fuel == 3,na.rm = TRUE),
            crop_residue = sum(fuel == 4,na.rm = TRUE),
            charcoal = sum(fuel == 5,na.rm = TRUE),
            others = sum(fuel == 6,na.rm = TRUE),
            percent_firewood = (firewood/Total_responses)*100,
            percent_fossil = (fossil/Total_responses)*100,
            percent_dung_Cake = (dung_Cake/Total_responses)*100,
            percent_crop_residue = (crop_residue/Total_responses)*100,
            percent_charcoal = (charcoal/Total_responses)*100,
            percent_others = (others/Total_responses)*100,
  ) %>%
  arrange(District)
write.csv(x=province_fuel_used,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/Socia Living Measurement/province_fuel_used.csv")

#TABLE 4 
#province wise toilets
province_toilet_used <- 
  secf2 %>%
  filter(Region == " Rural") %>%
  group_by(Province) %>%
  summarise(Total_responses = sum(toilet== 1,toilet == 2,toilet == 3,toilet == 4,na.rm = TRUE),
            no_toilet  = sum(toilet == 1,na.rm = TRUE),
            flush = sum(toilet == 2,na.rm = TRUE),
            dry_latrine = sum(toilet == 3,na.rm = TRUE),
            Others = sum(toilet == 4,na.rm = TRUE),
            percent_no_toilet = (no_toilet/Total_responses)*100,
            percent_flush = (flush/Total_responses)*100,
            percent_dry_latrine = (dry_latrine/Total_responses)*100,
            percent_Others = (Others/Total_responses)*100,
  ) %>%
  arrange(Province)

#TABLE 6 
#province wise toilets
district_toilet_used <- 
  secf2 %>%
  filter(Region == " Rural") %>%
  group_by(District) %>%
  summarise(Total_responses = sum(toilet== 1,toilet == 2,toilet == 3,toilet == 4,na.rm = TRUE),
            no_toilet  = sum(toilet == 1,na.rm = TRUE),
            flush = sum(toilet == 2,na.rm = TRUE),
            dry_latrine = sum(toilet == 3,na.rm = TRUE),
            Others = sum(toilet == 4,na.rm = TRUE),
            percent_no_toilet = (no_toilet/Total_responses)*100,
            percent_flush = (flush/Total_responses)*100,
            percent_dry_latrine = (dry_latrine/Total_responses)*100,
            percent_Others = (Others/Total_responses)*100,
  ) %>%
  arrange(District)

write.csv(x = district_toilet_used,file = "E:/IHI-2/Project/IHI-2/tables/Socia Living Measurement/district_toilet_used.csv")
