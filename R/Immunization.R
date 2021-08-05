#####################################################################
#Title: Public Health Care Delivery
#Author: Muhammad Uzair Aslam
#Date: 7/1/2021
#Purpose: Analyze Vaccination and Diarhea of Children
#Section of Questionnaire : PSLM 2020 Section I
#####################################################################

#clear global environment
rm(list = ls())

library(foreign)
library(tidyverse)

#load the immunization data
secI <- read.spss(file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/Data/PSLM spss data/SecI.sav",use.value.label=TRUE,to.data.frame=TRUE)
secI <- as_tibble(secI)

#select the required data
immunization <- select(secI,c(Province,Region,district,siaq03,siaq5a,siaq5b,siaq5c,siaq5d,siaq5e,siaq5f,siaq5g,siaq5h,siaq5i,siaq5j,siaq5k,siaq5l,siaq5m,siaq5n,siaq06,,siaq07,siaq08))

#rename column names
immunization <-rename(
                immunization,
                province = Province,
                region = Region,
                immunized = siaq03,
                BCG =siaq5a,
                penta1 = siaq5b,penta2 = siaq5c,penta3 = siaq5d,
                pneum1 = siaq5e,pneum2 = siaq5f,pneum3 = siaq5g, 
                polio0 = siaq5h,polio1 = siaq5i,polio2 = siaq5j,polio3 = siaq5k,
                IPV = siaq5l,
                measles1 = siaq5m,measles2 = siaq5n,
                suffered_diarrheoa = siaq06,diarrheoa_consultation = siaq07,diar_consultation_place = siaq08
                )

#find column wise null values
map(immunization, ~sum(is.na(.)))

############################CODING AND RENAMING VARIABLES################################################################################################
#rename immunized from yes = 1 and no = 2
levels(immunization$immunized) <- c("1","2")

#rename BCG from yes on card = 1, yes on recall = 1, yes through campaign = 1, no = 2.
levels(immunization$BCG)[levels(immunization$BCG)=="yes on card"] <- "1"
levels(immunization$BCG)[levels(immunization$BCG)=="yes on recall"] <- "1"
levels(immunization$BCG)[levels(immunization$BCG)=="no"] <- "2"
levels(immunization$BCG)[levels(immunization$BCG)=="yes through compaign"] <- "1"

#rename penta3 from yes on card = 1, yes on recall = 1, yes through campaign = 1, no = 2.
levels(immunization$penta3)[levels(immunization$penta3)=="yes on card"] <- "1"
levels(immunization$penta3)[levels(immunization$penta3)=="yes on recall"] <- "1"
levels(immunization$penta3)[levels(immunization$penta3)=="no"] <- "2"
levels(immunization$penta3)[levels(immunization$penta3)=="yes through compaign"] <- "1"

#rename pneum3 from yes on card = 1, yes on recall = 1, yes through campaign = 1, no = 2.
levels(immunization$pneum3)[levels(immunization$pneum3)=="yes on card"] <- "1"
levels(immunization$pneum3)[levels(immunization$pneum3)=="yes on recall"] <- "1"
levels(immunization$pneum3)[levels(immunization$pneum3)=="no"] <- "2"
levels(immunization$pneum3)[levels(immunization$pneum3)=="yes through compaign"] <- "1"


#rename polio3 from yes on card = 1, yes on recall = 1, yes through campaign = 1, no = 2.
levels(immunization$polio3)[levels(immunization$polio3)=="yes on card"] <- "1"
levels(immunization$polio3)[levels(immunization$polio3)=="yes on recall"] <- "1"
levels(immunization$polio3)[levels(immunization$polio3)=="no"] <- "2"
levels(immunization$polio3)[levels(immunization$polio3)=="yes through compaign"] <- "1"

#rename IPV from yes on card = 1, yes on recall = 1, yes through campaign = 1, no = 2.
levels(immunization$IPV)[levels(immunization$IPV)=="yes on card"] <- "1"
levels(immunization$IPV)[levels(immunization$IPV)=="yes on recall"] <- "1"
levels(immunization$IPV)[levels(immunization$IPV)=="no"] <- "2"
levels(immunization$IPV)[levels(immunization$IPV)=="yes through compaign"] <- "1"

#rename measles2 from yes on card = 1, yes on recall = 1, yes through campaign = 1, no = 2.
levels(immunization$measles2)[levels(immunization$measles2)=="yes on card"] <- "1"
levels(immunization$measles2)[levels(immunization$measles2)=="yes on recall"] <- "1"
levels(immunization$measles2)[levels(immunization$measles2)=="no"] <- "2"
levels(immunization$measles2)[levels(immunization$measles2)=="yes through compaign"] <- "1"


#rename suffered diarrheoa from yes = 1 and no = 2
levels(immunization$suffered_diarrheoa) <- c("1","2")


#rename diarrheoa consultation from yes = 1 and no = 2
levels(immunization$diarrheoa_consultation)[levels(immunization$diarrheoa_consultation)=="yes"] <- "1"
levels(immunization$diarrheoa_consultation)[levels(immunization$diarrheoa_consultation)=="no"] <- "2"

#rename diarrohea consultation: private dispensary = 1, govt dispensary/hospital = 2,BHU/RHC = 3 , Others = 4
levels(immunization$diar_consultation_place)[levels(immunization$diar_consultation_place)=="priv.dispensary/hospital/doctor"] <- "1"
levels(immunization$diar_consultation_place)[levels(immunization$diar_consultation_place)=="govt. dispensary/hospital/doctor"] <- "2"
levels(immunization$diar_consultation_place)[levels(immunization$diar_consultation_place)=="basic health unit"] <- "3"
levels(immunization$diar_consultation_place)[levels(immunization$diar_consultation_place)=="lady health worker"] <- "4"
levels(immunization$diar_consultation_place)[levels(immunization$diar_consultation_place)=="nurse/LHV"] <- "4"
levels(immunization$diar_consultation_place)[levels(immunization$diar_consultation_place)=="compounder/chemist"] <- "4"
levels(immunization$diar_consultation_place)[levels(immunization$diar_consultation_place)=="hakeem/herbist/homeopath"] <- "4"
levels(immunization$diar_consultation_place)[levels(immunization$diar_consultation_place)=="other"] <- "4"

####################TABLES#####################################################################################################################################
#TABLE 1
#%immunized in provinces
province_immunised <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(count = n(),
            total_immunised = sum(immunized==1,na.rm = TRUE),
            percent_immunized = (sum(immunized==1,na.rm = TRUE)/n())*100,) %>%
  arrange(province)

#TABLE 2
#%immunized in districts
district_immunised <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(count = n(),
            total_immunised = sum(immunized==1,na.rm = TRUE),
            percent_immunized = (sum(immunized==1,na.rm = TRUE)/n())*100,) %>%
  arrange(district)
write.csv(x=district_immunised,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/immunisation/district_immunized.csv")

#TABLE 3
#BCG in provinces
province_BCG <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(count = n(),
            total_BCG = sum(BCG==1,na.rm = TRUE),
            percent_BCG = (sum(BCG==1,na.rm = TRUE)/n())*100,) %>%
  arrange(province)

#TABLE 4
#BCG in district
district_BCG <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(count = n(),
            total_BCG = sum(BCG==1,na.rm = TRUE),
            percent_BCG = (sum(BCG==1,na.rm = TRUE)/n())*100,) %>%
  arrange(district)
write.csv(x=district_BCG,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/immunisation/district_BCG.csv")

#TABLE 5
#Penta3 in provinces
province_penta3 <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(count = n(),
            total_penta3 = sum(penta3==1,na.rm = TRUE),
            percent_penta3 = (sum(penta3==1,na.rm = TRUE)/n())*100,) %>%
  arrange(province)

#TABLE 6
#Penta in district
district_penta <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(count = n(),
            total_BCG = sum(penta3==1,na.rm = TRUE),
            percent_BCG = (sum(penta3==1,na.rm = TRUE)/n())*100,) %>%
  arrange(district)
write.csv(x=district_penta,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/immunisation/district_penta.csv")

#TABLE 7
#Pneum3 in provinces
province_pneu3 <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(count = n(),
            total_pneu3 = sum(pneum3==1,na.rm = TRUE),
            percent_pneum3 = (sum(pneum3==1,na.rm = TRUE)/n())*100,) %>%
  arrange(province)

#TABLE 8
district_pneu3 <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(count = n(),
            total_pneu3 = sum(pneum3==1,na.rm = TRUE),
            percent_pneum3 = (sum(pneum3==1,na.rm = TRUE)/n())*100,) %>%
  arrange(district)
write.csv(x=district_pneu3,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/immunisation/district_pneu.csv")

#TABLE 9
#polio province
province_polio <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(count = n(),
            total_polio3 = sum(polio3==1,na.rm = TRUE),
            percent_polio3 = (sum(polio3==1,na.rm = TRUE)/n())*100,) %>%
  arrange(province)
#TABLE 10
#polio district
district_polio3 <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(count = n(),
            total_polio3 = sum(polio3==1,na.rm = TRUE),
            percent_polio3 = (sum(polio3==1,na.rm = TRUE)/n())*100,) %>%
  arrange(district)
write.csv(x=district_polio3,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/immunisation/district_polio.csv")

#TABLE 11
#IPV
province_ipv <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(count = n(),
            total_ipv = sum(IPV==1,na.rm = TRUE),
            percent_ipv = (sum(IPV==1,na.rm = TRUE)/n())*100,) %>%
  arrange(province)
#TABLE 12
#IPV
district_IPV <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(count = n(),
            total_IPV = sum(IPV==1,na.rm = TRUE),
            percent_IPV = (sum(IPV==1,na.rm = TRUE)/n())*100,) %>%
  arrange(district)
write.csv(x=district_IPV,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/immunisation/district_IPV.csv")

#TABLE 13
#Measles
province_measles <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(count = n(),
            total_measles = sum(measles2==1,na.rm = TRUE),
            percent_measles = (sum(measles2==1,na.rm = TRUE)/n())*100,) %>%
  arrange(province)

#TABLE 14
#Measles 
district_measles <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(count = n(),
            total_measles = sum(measles2==1,na.rm = TRUE),
            percent_measles = (sum(measles2==1,na.rm = TRUE)/n())*100,) %>%
  arrange(district)
write.csv(x= district_measles,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/immunisation/district_measles.csv")

#TABLE 15
#Province Diarrhoea Suffered
province_diarrhoea <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(count = n(),
            total_diarr = sum(suffered_diarrheoa==1,na.rm = TRUE),
            percent_diarr = (sum(suffered_diarrheoa==1,na.rm = TRUE)/n())*100,) %>%
  arrange(province)

#District Diarrhoea Suffered
#TABLE 16
district_diarr_suff <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(count = n(),
            diarr_suff = sum(suffered_diarrheoa==1,na.rm = TRUE),
            percent_diarr_suff = (sum(suffered_diarrheoa==1,na.rm = TRUE)/n())*100,) %>%
  arrange(district)
write.csv(x= district_diarr_suff,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/immunisation/district_diarr_suff.csv")

#TABLE 17
#Diarrhoea consultation
province_diarrhoea_consultation <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(count = n(),
            total_diarr_consultation = sum(diarrheoa_consultation==1,na.rm = TRUE),
            percent_diarr_consultation = (sum(diarrheoa_consultation==1,na.rm = TRUE)/n())*100,) %>%
  arrange(province)

#TABLE 18
#Diarrhoea consultation
district_diarr_consultation <-
  immunization %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(count = n(),
            diarr_consultation = sum(diarrheoa_consultation==1,na.rm = TRUE),
            percent_diarr_consultation = (sum(diarrheoa_consultation==1,na.rm = TRUE)/n())*100,) %>%
  arrange(district)
write.csv(x= district_diarr_consultation,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/immunisation/district_diarr_consultation.csv")

#TABLE 19
#Diarrhoea Consultation Place
province_diarr_consult_place <- 
  immunization %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarise(Total_Hospitals = sum(diar_consultation_place==1,diar_consultation_place==2,diar_consultation_place==3,diar_consultation_place==4,na.rm = TRUE),
            Private_Hospital = sum(diar_consultation_place==1,na.rm = TRUE),
            Govt_Dispen = sum(diar_consultation_place==2,na.rm = TRUE),
            BHU_RHC = sum(diar_consultation_place==3,na.rm = TRUE),
            Other = sum(diar_consultation_place==4,na.rm = TRUE),
            Percent_Private_Hospitals = (Private_Hospital/Total_Hospitals)*100,
            Percent_Govt_Hospitals = (Govt_Dispen/Total_Hospitals)*100,
            Percent_BHU_RHC = (BHU_RHC/Total_Hospitals)*100,
            Percent_Others = (Other/Total_Hospitals)*100,
  ) %>%
  arrange(province)

#TABLE 20
#Diarrhoea Consultation Place

district_diarr_consult_place <- 
  immunization %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarise(Total_Hospitals = sum(diar_consultation_place==1,diar_consultation_place==2,diar_consultation_place==3,diar_consultation_place==4,na.rm = TRUE),
            Private_Hospital = sum(diar_consultation_place==1,na.rm = TRUE),
            Govt_Dispen = sum(diar_consultation_place==2,na.rm = TRUE),
            BHU_RHC = sum(diar_consultation_place==3,na.rm = TRUE),
            Other = sum(diar_consultation_place==4,na.rm = TRUE),
            Percent_Private_Hospitals = (Private_Hospital/Total_Hospitals)*100,
            Percent_Govt_Hospitals = (Govt_Dispen/Total_Hospitals)*100,
            Percent_BHU_RHC = (BHU_RHC/Total_Hospitals)*100,
            Percent_Others = (Other/Total_Hospitals)*100,
  ) %>%
  arrange(district)
write.csv(x= district_diarr_consult_place,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/immunisation/district_diarr_consult_place.csv")

