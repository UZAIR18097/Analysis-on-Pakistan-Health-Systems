#####################################################################
#Title: Indicator : Public Health Care Delivery
#Author: Muhammad Uzair Aslam
#Date: 6/27/2021
#Purpose: Analyze Health Deliverance of Population
#Section of Questionnaire : PSLM 2020 Section D and Section L
#####################################################################

library(foreign)
library(tidyverse)


#load the health data
secD <- read.spss(file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/Data/PSLM spss data/SecD.sav",use.value.label=TRUE,to.data.frame=TRUE)
secL <- read.spss(file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/Data/PSLM spss data/SecL.sav",use.value.label=TRUE,to.data.frame=TRUE)
secD <- as_tibble(secD)
secL <- as_tibble(secL)

#select the required data
health <- select(secD,c(Province,Region,district,sdaq01,sdaq02,sdaq03,sdaq5a))
satisfied <- select(secL,c(Province,Region,District,facilities,SLq3,SLq5,SLq6,SLq7))

#rename column names
health <-rename(health,province = Province,region = Region,sickorinjured = sdaq01,consulted =sdaq02,health_provider =sdaq03,problem = sdaq5a )
satisfied <- rename(satisfied,satisfied = SLq3,Distance_inKM = SLq5,Mode = SLq6,Time = SLq7)

#find column wise null values
map(health, ~sum(is.na(.)))

############################CODING AND RENAMING VARIABLES################################################################################################
#rename sickorinjured from yes = 1 and no = 2
levels(health$sickorinjured) <- c("1","2")

#convert sickorinjured to numeric 
health$sickorinjured <- as.numeric(as.character(health$sickorinjured))

#rename consulted from yes = 1 and no = 2
levels(health$consulted) <- c("1","2")
#convert consulted to numeric 
health$consulted <- as.numeric(as.character(health$consulted))

#rename health provider: private dispensary = 1, govt dispensary/hospital = 2,BHU/RHC = 3 , Others = 4
levels(health$health_provider)[levels(health$health_provider)=="private dispensary/hospital"] <- "1"
levels(health$health_provider)[levels(health$health_provider)=="govt dispensary/hospital"] <- "2"
levels(health$health_provider)[levels(health$health_provider)=="BHU\\RHC"] <- "3"
levels(health$health_provider)[levels(health$health_provider)=="LHV\\LHW"] <- "4"
levels(health$health_provider)[levels(health$health_provider)=="Hakeem"] <- "4"
levels(health$health_provider)[levels(health$health_provider)=="Homeopathic"] <- "4"
levels(health$health_provider)[levels(health$health_provider)=="Chemist"] <- "4"
levels(health$health_provider)[levels(health$health_provider)=="Siana"] <- "4"
levels(health$health_provider)[levels(health$health_provider)=="Others"] <- "4"
#convert health provider to numeric 
health$health_provider <- as.numeric(as.character(health$health_provider))

#rename problem: satisfy = 1,  doc not present = 2, staff not cope = 2,lady staff not present =2,other = 3 
levels(health$problem)[levels(health$problem)=="satisfy"] <- "1"
levels(health$problem)[levels(health$problem)=="doctor not available"] <- "2"
levels(health$problem)[levels(health$problem)=="staff not coperative"] <- "2"
levels(health$problem)[levels(health$problem)=="female staff not available"] <- "2"
levels(health$problem)[levels(health$problem)=="lack of neatness"] <- "3"
levels(health$problem)[levels(health$problem)=="long wait"] <- "3"
levels(health$problem)[levels(health$problem)=="treatment expensive"] <- "3"
levels(health$problem)[levels(health$problem)=="staff untrained"] <- "3"
levels(health$problem)[levels(health$problem)=="medicine not available"] <- "3"
levels(health$problem)[levels(health$problem)=="unsuccessful treatment"] <- "3"
levels(health$problem)[levels(health$problem)=="other"] <- "3"

##############################################################
#rename satisfied = 1, not satisfied =2
levels(satisfied$satisfied)[levels(satisfied$satisfied)=="Satisfied"] <- "1"
levels(satisfied$satisfied)[levels(satisfied$satisfied)=="Not Satisfied"] <- "2"

#rename distance in KM = 0-0.5 Km = 1, 0.5-1 Km = 2, 1-2 (Km) = 3, 2-5(Km) = 4, 5+ (Km)  = 5
levels(satisfied$Distance_inKM)[levels(satisfied$Distance_inKM)=="0-0.5 (km)"] <- "1"
levels(satisfied$Distance_inKM)[levels(satisfied$Distance_inKM)=="0.5-1 (km)"] <- "1"
levels(satisfied$Distance_inKM)[levels(satisfied$Distance_inKM)=="1-2 (Km)"] <- "1"
levels(satisfied$Distance_inKM)[levels(satisfied$Distance_inKM)=="2-5 (Km)"] <- "2"
levels(satisfied$Distance_inKM)[levels(satisfied$Distance_inKM)=="5+ (Km)"] <- "3"

####################TABLES#####################################################################################################################################
#TABLE 1
#%sick in provinces
province_sick <-
  health %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(count = n(),
            total_sick = sum(sickorinjured==1),
            percent_sick = (sum(sickorinjured==1)/n())*100,) %>%
  arrange(province)
#TABLE 2
#%sick in districts
district_sick <- 
  health %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(count = n(),
            total_sick = sum(sickorinjured==1),
            percent_sick = (sum(sickorinjured==1)/n())*100) %>%
  arrange(district)
write.csv(x=district_sick,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/health/district_sick.csv")
#############################################################################################
#TABLE 3
#%consulted in provinces
province_consulted <-
  health %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarize(total_sick = sum(sickorinjured==1),
            total_consulted = sum(consulted==1,na.rm = TRUE),
            percent_consulted = (sum(consulted==1,na.rm = TRUE)/sum(sickorinjured==1))*100,) %>%
  arrange(province)
#TABLE 4
#%consulted in districts
districts_consulted <-
  health %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarize(total_sick = sum(sickorinjured==1),
            total_consulted = sum(consulted==1,na.rm = TRUE),
            percent_consulted = (sum(consulted==1,na.rm = TRUE)/sum(sickorinjured==1))*100,) %>%
  arrange(district)
write.csv(x=districts_consulted,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/health/district_consulted.csv")
#######################################################################################################
#TABLE 5
#%health care provider in provinces
health_provider_provinces <- 
  health %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarise(Total_Hospitals = sum(health_provider==1,health_provider==2,health_provider==3,health_provider==4,na.rm = TRUE),
            Private_Hospital = sum(health_provider==1,na.rm = TRUE),
            Govt_Dispen = sum(health_provider==2,na.rm = TRUE),
            BHU_RHC = sum(health_provider==3,na.rm = TRUE),
            Other = sum(health_provider==4,na.rm = TRUE),
            Percent_Private_Hospitals = (Private_Hospital/Total_Hospitals)*100,
            Percent_Govt_Hospitals = (Govt_Dispen/Total_Hospitals)*100,
            Percent_BHU_RHC = (BHU_RHC/Total_Hospitals)*100,
            Percent_Others = (Other/Total_Hospitals)*100,
            ) %>%
  arrange(province)
########################################################################################################
#TABLE 6
#health care provider district
health_provider_district <- 
  health %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarise(Total_Hospitals = sum(health_provider==1,health_provider==2,health_provider==3,health_provider==4,na.rm = TRUE),
            Private_Hospital = sum(health_provider==1,na.rm = TRUE),
            Govt_Dispen = sum(health_provider==2,na.rm = TRUE),
            BHU_RHC = sum(health_provider==3,na.rm = TRUE),
            Other = sum(health_provider==4,na.rm = TRUE),
            Percent_Private_Hospitals = (Private_Hospital/Total_Hospitals)*100,
            Percent_Govt_Hospitals = (Govt_Dispen/Total_Hospitals)*100,
            Percent_BHU_RHC = (BHU_RHC/Total_Hospitals)*100,
            Percent_Others = (Other/Total_Hospitals)*100,
  ) %>%
  arrange(district)
write.csv(x=health_provider_district,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/health/health_provider_district.csv")
###########################################################################################################
#TABLE 7
provider_problem_provinces <-
  health %>%
  filter(region == "rural") %>%
  group_by(province) %>%
  summarise(Total_response = sum(problem==1,problem==2,problem==3,na.rm = TRUE),
            Satisfied = sum(problem==1,na.rm = TRUE),
            Staff_lackness = sum(problem==2,na.rm = TRUE),
            Other = sum(problem==3,na.rm = TRUE),
            Percent_Satisfied = (Satisfied/Total_response)*100,
            Percent_Staff_lackness = (Staff_lackness/Total_response)*100,
            Percent_Other_problems = (Other/Total_response)*100,
  ) %>%
  arrange(province)
###########################################################################################################
#TABLE 8
provider_problem_district <-
  health %>%
  filter(region == "rural") %>%
  group_by(district) %>%
  summarise(Total_response = sum(problem==1,problem==2,problem==3,na.rm = TRUE),
            Satisfied = sum(problem==1,na.rm = TRUE),
            Staff_lackness = sum(problem==2,na.rm = TRUE),
            Other = sum(problem==3,na.rm = TRUE),
            Percent_Satisfied = (Satisfied/Total_response)*100,
            Percent_Staff_lackness = (Staff_lackness/Total_response)*100,
            Percent_Other_problems = (Other/Total_response)*100,
  ) %>%
  arrange(district)
write.csv(x= provider_problem_district,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/health/provider_problem_district.csv")
###########################################################################################################
#TABLE 9 province_satisfied_from_services
subset_BHU <- subset(x = satisfied,subset = facilities == "Basic Health Unit")
subset_health <- subset(x = satisfied,subset = facilities == "Health Clinic/Hospital")
# 
# province_satisfied_from_BHU <- 
#   group_by(subset_BHU,Province) %>%
#   filter(Region == " Rural") %>%
#   summarise(total_responses = sum(satisfied == 1,satisfied == 2,na.rm = TRUE ),
#             satisfied_from_BHU = sum(satisfied==1,na.rm = TRUE),
#             percent_satisfied = (satisfied_from_BHU/total_responses)*100
#       )
# 
# province_satisfied_from_Health <- 
#   group_by(subset_health,Province) %>%
#   filter(Region == " Rural") %>%
#   summarise(total_responses = sum(satisfied == 1,satisfied == 2,na.rm = TRUE ),
#             satisfied_from_BHU = sum(satisfied==1,na.rm = TRUE),
#             percent_satisfied = (satisfied_from_BHU/total_responses)*100
#   )

province_satisfied_from_services <- filter(satisfied,facilities == "Basic Health Unit" | facilities == "Family Planning Unit" | facilities == "Health Clinic/Hospital") %>% 
  group_by(Province) %>%
  filter(Region == " Rural") %>%
  summarise(
            total_response  = sum(satisfied == 1,satisfied== 2,na.rm = TRUE ),
            
            total_response_for_BHU = sum(satisfied==1 & facilities == "Basic Health Unit",na.rm = TRUE) + sum(satisfied==2 & facilities == "Basic Health Unit" ,na.rm = TRUE),
            satisfied_from_BHU = sum(satisfied==1 & facilities == "Basic Health Unit" ,na.rm = TRUE),
            
            
            total_response_for_FMU = sum(satisfied==1 & facilities == "Family Planning Unit",na.rm = TRUE) + sum(satisfied==2 & facilities == "Family Planning Unit" ,na.rm = TRUE),
            satisfied_from_FMU = sum(satisfied==1 & facilities == "Family Planning Unit" ,na.rm = TRUE),
            
            
            total_response_for_Health_Clinic = sum(satisfied==1 & facilities == "Health Clinic/Hospital",na.rm = TRUE) + sum(satisfied==2 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),
            satisfied_from_Health_Clinic = sum(satisfied==1 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),
            
            
            
            percent_satisfied_from_BHU = (satisfied_from_BHU/total_response_for_BHU)*100,
            percent_satisfied_from_FMU = (satisfied_from_FMU/total_response_for_FMU)*100,
            percent_satisfied_from_Health_Clinic = (satisfied_from_Health_Clinic/total_response_for_Health_Clinic)*100,
            )%>%
  arrange(Province)
            
#TABLE 10 district_satisfied_from_services
district_satisfied_from_services <- filter(satisfied,facilities == "Basic Health Unit" | facilities == "Family Planning Unit" | facilities == "Health Clinic/Hospital") %>% 
  group_by(District) %>%
  filter(Region == " Rural") %>%
  summarise(
    total_response  = sum(satisfied == 1,satisfied== 2,na.rm = TRUE ),
    
    total_response_for_BHU = sum(satisfied==1 & facilities == "Basic Health Unit",na.rm = TRUE) + sum(satisfied==2 & facilities == "Basic Health Unit" ,na.rm = TRUE),
    satisfied_from_BHU = sum(satisfied==1 & facilities == "Basic Health Unit" ,na.rm = TRUE),
    
    
    total_response_for_FMU = sum(satisfied==1 & facilities == "Family Planning Unit",na.rm = TRUE) + sum(satisfied==2 & facilities == "Family Planning Unit" ,na.rm = TRUE),
    satisfied_from_FMU = sum(satisfied==1 & facilities == "Family Planning Unit" ,na.rm = TRUE),
    
    
    total_response_for_Health_Clinic = sum(satisfied==1 & facilities == "Health Clinic/Hospital",na.rm = TRUE) + sum(satisfied==2 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),
    satisfied_from_Health_Clinic = sum(satisfied==1 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),
    
    
    
    percent_satisfied_from_BHU = (satisfied_from_BHU/total_response_for_BHU)*100,
    percent_satisfied_from_FMU = (satisfied_from_FMU/total_response_for_FMU)*100,
    percent_satisfied_from_Health_Clinic = (satisfied_from_Health_Clinic/total_response_for_Health_Clinic)*100,
  )%>%
  arrange(District)
write.csv(x= district_satisfied_from_services,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/health/district_satisfied_from_services.csv")
            
  
#TABLE 11 province_distance_in_km
# province_distance_from_BHU <- 
#    group_by(subset_BHU,Province) %>%
#    #filter(Region == " Rural") %>%
#    summarise(total_responses = sum(Distance_inKM == 1,Distance_inKM == 2,Distance_inKM == 3,na.rm = TRUE ),
#              distance_0to2 = sum(Distance_inKM==1,na.rm = TRUE),
#              distance_2to5 = sum(Distance_inKM==2,na.rm = TRUE),
#              distance_5plus = sum(Distance_inKM==3,na.rm = TRUE),
#              percent_1 = (distance_0to2/total_responses)*100,
#              percent_2 = (distance_2to5/total_responses)*100,
#              percent_3 = (distance_5plus/total_responses)*100,
#             
#        ) %>%
#   arrange(Province)

province_distance_in_km <- filter(satisfied,facilities == "Basic Health Unit" | facilities == "Family Planning Unit" | facilities == "Health Clinic/Hospital") %>% 
  group_by(Province) %>%
  filter(Region == " Rural") %>%
  summarise(
    total_response  = sum(Distance_inKM == 1,Distance_inKM== 2,Distance_inKM== 3,Distance_inKM== 4,Distance_inKM== 5,na.rm = TRUE ),
    
    total_response_for_BHU = sum(Distance_inKM == 1 & facilities == "Basic Health Unit",na.rm = TRUE) + 
    sum(Distance_inKM == 2 & facilities == "Basic Health Unit" ,na.rm = TRUE) + sum(Distance_inKM == 3 & facilities == "Basic Health Unit" ,na.rm = TRUE)+
    sum(Distance_inKM == 4 & facilities == "Basic Health Unit" ,na.rm = TRUE)+sum(Distance_inKM == 5 & facilities == "Basic Health Unit" ,na.rm = TRUE),
    
    BHU_0to0.5 = sum(Distance_inKM == 1 & facilities == "Basic Health Unit",na.rm = TRUE),
    BHU_0.5to1  = sum(Distance_inKM == 2 & facilities == "Basic Health Unit" ,na.rm = TRUE),
    BHU_1to2 = sum(Distance_inKM == 3 & facilities == "Basic Health Unit" ,na.rm = TRUE),
    BHU_2to5 = sum(Distance_inKM == 4 & facilities == "Basic Health Unit" ,na.rm = TRUE),
    BHU5plus = sum(Distance_inKM == 5 & facilities == "Basic Health Unit" ,na.rm = TRUE),
    
    
    total_response_for_FMU = sum(Distance_inKM == 1 & facilities == "Family Planning Unit",na.rm = TRUE) + 
    sum(Distance_inKM == 2 & facilities == "Family Planning Unit" ,na.rm = TRUE) + sum(Distance_inKM == 3 & facilities == "Family Planning Unit" ,na.rm = TRUE)+
    sum(Distance_inKM == 4 & facilities == "Family Planning Unit" ,na.rm = TRUE)+sum(Distance_inKM == 5 & facilities == "Family Planning Unit" ,na.rm = TRUE),
    
    FMU_0to0.5 = sum(Distance_inKM == 1 & facilities == "Family Planning Unit",na.rm = TRUE),
    FMU_0.5to1  = sum(Distance_inKM == 2 & facilities == "Family Planning Unit" ,na.rm = TRUE),
    FMU_1to2 = sum(Distance_inKM == 3 & facilities == "Family Planning Unit" ,na.rm = TRUE),
    FMU_2to5 = sum(Distance_inKM == 4 & facilities == "Family Planning Unit" ,na.rm = TRUE),
    FMU5plus = sum(Distance_inKM == 5 & facilities == "Family Planning Unit" ,na.rm = TRUE),
    
    total_response_for_HC = sum(Distance_inKM == 1 & facilities == "Health Clinic/Hospital",na.rm = TRUE) + 
      sum(Distance_inKM == 2 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE) + sum(Distance_inKM == 3 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE)+
      sum(Distance_inKM == 4 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE)+sum(Distance_inKM == 5 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),
    
    HC_0to0.5 = sum(Distance_inKM == 1 & facilities == "Health Clinic/Hospital",na.rm = TRUE),
    HC_0.5to1  = sum(Distance_inKM == 2 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),
    HC_1to2 = sum(Distance_inKM == 3 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),
    HC_2to5 = sum(Distance_inKM == 4 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),
    HC5plus = sum(Distance_inKM == 5 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),

    
    
    percent_BHU_0to0.5 = (BHU_0to0.5/total_response_for_BHU)*100,
    percent_BHU_0.5to1 = (BHU_0.5to1/total_response_for_BHU)*100,
    percent_BHU_1to2 = (BHU_1to2/total_response_for_BHU)*100,
    percent_BHU_2to5 = (BHU_2to5/total_response_for_BHU)*100,
    percent_BHU_5plus = (BHU5plus/total_response_for_BHU)*100,
    
    percent_FMU_0to0.5 = (FMU_0to0.5/total_response_for_FMU)*100,
    percent_FMU_0.5to1 = (FMU_0.5to1/total_response_for_FMU)*100,
    percent_FMU_1to2 = (FMU_1to2/total_response_for_FMU)*100,
    percent_FMU_2to5 = (FMU_2to5/total_response_for_FMU)*100,
    percent_FMU_5plus = (FMU5plus/total_response_for_FMU)*100,
    
    percent_HC_0to0.5 = (HC_0to0.5/total_response_for_HC)*100,
    percent_HC_0.5to1 = (HC_0.5to1/total_response_for_HC)*100,
    percent_HC_1to2 = (HC_1to2/total_response_for_HC)*100,
    percent_HC_2to5 = (HC_2to5/total_response_for_HC)*100,
    percent_HC_5plus = (HC5plus/total_response_for_HC)*100,
    
     )%>%
  arrange(Province)
write.csv(x= province_distance_in_km,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/health/province_distance_in_km.csv")

  
#TABLE 12 province_distance_in_km
district_distance_in_km <- filter(satisfied,facilities == "Basic Health Unit" | facilities == "Family Planning Unit" | facilities == "Health Clinic/Hospital") %>% 
  group_by(District) %>%
  filter(Region == " Rural") %>%
  summarise(
    total_response  = sum(Distance_inKM == 1,Distance_inKM== 2,Distance_inKM== 3,Distance_inKM== 4,Distance_inKM== 5,na.rm = TRUE ),
    
    total_response_for_BHU = sum(Distance_inKM == 1 & facilities == "Basic Health Unit",na.rm = TRUE) + 
      sum(Distance_inKM == 2 & facilities == "Basic Health Unit" ,na.rm = TRUE) + sum(Distance_inKM == 3 & facilities == "Basic Health Unit" ,na.rm = TRUE)+
      sum(Distance_inKM == 4 & facilities == "Basic Health Unit" ,na.rm = TRUE)+sum(Distance_inKM == 5 & facilities == "Basic Health Unit" ,na.rm = TRUE),
    
    BHU_0to0.5 = sum(Distance_inKM == 1 & facilities == "Basic Health Unit",na.rm = TRUE),
    BHU_0.5to1  = sum(Distance_inKM == 2 & facilities == "Basic Health Unit" ,na.rm = TRUE),
    BHU_1to2 = sum(Distance_inKM == 3 & facilities == "Basic Health Unit" ,na.rm = TRUE),
    BHU_2to5 = sum(Distance_inKM == 4 & facilities == "Basic Health Unit" ,na.rm = TRUE),
    BHU5plus = sum(Distance_inKM == 5 & facilities == "Basic Health Unit" ,na.rm = TRUE),
    
    
    total_response_for_FMU = sum(Distance_inKM == 1 & facilities == "Family Planning Unit",na.rm = TRUE) + 
      sum(Distance_inKM == 2 & facilities == "Family Planning Unit" ,na.rm = TRUE) + sum(Distance_inKM == 3 & facilities == "Family Planning Unit" ,na.rm = TRUE)+
      sum(Distance_inKM == 4 & facilities == "Family Planning Unit" ,na.rm = TRUE)+sum(Distance_inKM == 5 & facilities == "Family Planning Unit" ,na.rm = TRUE),
    
    FMU_0to0.5 = sum(Distance_inKM == 1 & facilities == "Family Planning Unit",na.rm = TRUE),
    FMU_0.5to1  = sum(Distance_inKM == 2 & facilities == "Family Planning Unit" ,na.rm = TRUE),
    FMU_1to2 = sum(Distance_inKM == 3 & facilities == "Family Planning Unit" ,na.rm = TRUE),
    FMU_2to5 = sum(Distance_inKM == 4 & facilities == "Family Planning Unit" ,na.rm = TRUE),
    FMU5plus = sum(Distance_inKM == 5 & facilities == "Family Planning Unit" ,na.rm = TRUE),
    
    total_response_for_HC = sum(Distance_inKM == 1 & facilities == "Health Clinic/Hospital",na.rm = TRUE) + 
      sum(Distance_inKM == 2 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE) + sum(Distance_inKM == 3 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE)+
      sum(Distance_inKM == 4 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE)+sum(Distance_inKM == 5 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),
    
    HC_0to0.5 = sum(Distance_inKM == 1 & facilities == "Health Clinic/Hospital",na.rm = TRUE),
    HC_0.5to1  = sum(Distance_inKM == 2 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),
    HC_1to2 = sum(Distance_inKM == 3 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),
    HC_2to5 = sum(Distance_inKM == 4 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),
    HC5plus = sum(Distance_inKM == 5 & facilities == "Health Clinic/Hospital" ,na.rm = TRUE),
    
    
    
    percent_BHU_0to0.5 = (BHU_0to0.5/total_response_for_BHU)*100,
    percent_BHU_0.5to1 = (BHU_0.5to1/total_response_for_BHU)*100,
    percent_BHU_1to2 = (BHU_1to2/total_response_for_BHU)*100,
    percent_BHU_2to5 = (BHU_2to5/total_response_for_BHU)*100,
    percent_BHU_5plus = (BHU5plus/total_response_for_BHU)*100,
    
    percent_FMU_0to0.5 = (FMU_0to0.5/total_response_for_FMU)*100,
    percent_FMU_0.5to1 = (FMU_0.5to1/total_response_for_FMU)*100,
    percent_FMU_1to2 = (FMU_1to2/total_response_for_FMU)*100,
    percent_FMU_2to5 = (FMU_2to5/total_response_for_FMU)*100,
    percent_FMU_5plus = (FMU5plus/total_response_for_FMU)*100,
    
    percent_HC_0to0.5 = (HC_0to0.5/total_response_for_HC)*100,
    percent_HC_0.5to1 = (HC_0.5to1/total_response_for_HC)*100,
    percent_HC_1to2 = (HC_1to2/total_response_for_HC)*100,
    percent_HC_2to5 = (HC_2to5/total_response_for_HC)*100,
    percent_HC_5plus = (HC5plus/total_response_for_HC)*100,
    
  )%>%
  arrange(District)
write.csv(x = district_distance_in_km,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/health/district_distance_in_km.csv")










