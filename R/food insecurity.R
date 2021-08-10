#####################################################################
#Title: Indicator : Social Living Measurement
#Author: Muhammad Uzair Aslam
#Date: 7/12/2021
#Purpose: Analyze food insecurity amongst population
#Section of Questionnaire : PSLM 2020 Section K
#####################################################################

#clear global environment
rm(list = ls())

library(foreign)
library(tidyverse)


#load the health data
secK <- read.spss(file = "E:/IHI-2/Project/Data/PSLM spss data/SecK.sav",use.value.label=TRUE,to.data.frame=TRUE)
secK <- as_tibble(secK)

#select the required data
food_ins <- select(secK,c(Province,Region,District,C01,C04,C06,C07,C08))

#######################TABLES###################################################
#TABLE 1
#worried_about_food in provinces
province_worried_not_having_food <-
  food_ins %>%
  filter(Region == "Rural") %>%
  group_by(Province) %>%
  summarize(total_response = sum(C01 == 1) + sum(C01 == 2),
            worried = sum(C01 == 1),
            not_worried = sum(C01 == 2),
            percent_worried = (worried/total_response)*100,
            percent_not_worried = (not_worried/total_response)*100) %>%
  arrange(Province)

#TABLE 2
#worried_about_food in districts
district_worried_not_having_food <-
  food_ins %>%
  filter(Region == "Rural") %>%
  group_by(District) %>%
  summarize(total_response = sum(C01 == 1) + sum(C01 == 2),
            worried = sum(C01 == 1),
            not_worried = sum(C01 == 2),
            percent_worried = (worried/total_response)*100,
            percent_not_worried = (not_worried/total_response)*100) %>%
  arrange(District)

write.csv(x=district_worried_not_having_food,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/Socia Living Measurement/district_worried_not_having_food.csv")


#TABLE 3
#skipped_meal in provinces
province_skipped_meal <-
  food_ins %>%
  #filter(Region == "Rural") %>%
  group_by(Province) %>%
  summarize(total_response = sum(C04 == 1) + sum(C04 == 2),
            skipped = sum(C04 == 1),
            not_skipped = sum(C04 == 2),
            percent_skipped = (skipped/total_response)*100,
            percent_not_skipped = (not_skipped/total_response)*100) %>%
  arrange(Province)

#TABLE 4
#skipped_meal in districts
districts_skipped_meal <-
  food_ins %>%
  filter(Region == "Rural") %>%
  group_by(District) %>%
  summarize(total_response = sum(C04 == 1) + sum(C04 == 2),
            skipped = sum(C04 == 1),
            not_skipped = sum(C04 == 2),
            percent_skipped = (skipped/total_response)*100,
            percent_not_skipped = (not_skipped/total_response)*100) %>%
  arrange(District)

write.csv(x=districts_skipped_meal,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/Socia Living Measurement/ddistricts_skipped_meal.csv")

#TABLE 5
#ran_out_of_food in provinces
province_ran_out_of_food <-
  food_ins %>%
  filter(Region == "Rural") %>%
  group_by(Province) %>%
  summarize(total_response = sum(C06 == 1) + sum(C06 == 2),
            out_of_food = sum(C06 == 1),
            n_out_of_food = sum(C06 == 2),
            percent_out_of_food = (out_of_food/total_response)*100,
            percent_n_out_of_food = (n_out_of_food/total_response)*100) %>%
  arrange(Province)

#TABLE 6
#ran_out_of_food in districts
district_ran_out_of_food <-
  food_ins %>%
  filter(Region == "Rural") %>%
  group_by(District) %>%
  summarize(total_response = sum(C06 == 1) + sum(C06 == 2),
            out_of_food = sum(C06 == 1),
            n_out_of_food = sum(C06 == 2),
            percent_out_of_food = (out_of_food/total_response)*100,
            percent_n_out_of_food = (n_out_of_food/total_response)*100) %>%
  arrange(District)
write.csv(x=district_ran_out_of_food,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/Socia Living Measurement/district_ran_out_of_food.csv")


#TABLE 7
#did_not_eat in provinces
province_did_not_eat <-
  food_ins %>%
  filter(Region == "Rural") %>%
  group_by(Province) %>%
  summarize(total_response = sum(C08 == 1) + sum(C08 == 2),
            out_of_food = sum(C08 == 1),
            n_out_of_food = sum(C08 == 2),
            percent_out_of_food = (out_of_food/total_response)*100,
            percent_n_out_of_food = (n_out_of_food/total_response)*100) %>%
  arrange(Province)

#TABLE 8
#did_not_eat in districts
district_did_not_eat <-
  food_ins %>%
  filter(Region == "Rural") %>%
  group_by(District) %>%
  summarize(total_response = sum(C08 == 1) + sum(C08 == 2),
            out_of_food = sum(C08 == 1),
            n_out_of_food = sum(C08 == 2),
            percent_out_of_food = (out_of_food/total_response)*100,
            percent_n_out_of_food = (n_out_of_food/total_response)*100) %>%
  arrange(District)
write.csv(x=district_did_not_eat,file = "E:/IHI-2/IHI-2 PSLM/Project/IHI-2/tables/Socia Living Measurement/district_did_not_eat.csv")
