#####################################################################
#Title: Indicator : Demographic Graphs
#Author: Muhammad Uzair Aslam
#Date: 7/17/2021
#Purpose: Analyze Pakistan Population Demographics
#Data Sets : PDHS 2017-2018 House Hold Person Recode 
#####################################################################

#clear global environment
rm(list = ls())

library(foreign)
library(tidyverse)


pp_recode <- read.spss(file = "E:/IHI-2/Project/IHI-2/Data/DHS spss data/Household Person Recode.sav",use.value.label=TRUE,to.data.frame=TRUE)

pp <- as_tibble(pp_recode)

#select required variables
pr <- select(pp,c(HV102,HV104,HV105,HV024,HV025))

#rename columns
pr <- rename(pr,c(Usual_resident = HV102,Gender = HV104,Age = HV105,Province = HV024,Region = HV025))

#make age numeric
pr$Age = as.numeric(pr$Age)

#make age bracket
pr$Age_Bracket <- cut(pr$Age, breaks=c(0,1,5,15,34,49,97), right = TRUE)


#make a column that has only males and only females
#select data by provinces and spatial
#####################################PAKISTAN################################################
pakistan_male <- pr %>%
  filter(Usual_resident == "Yes" & Gender == "Male") %>%
  group_by(Age_Bracket) %>%
  summarise(count = n())
pakistan_female <- pr %>%
  filter(Usual_resident == "Yes" & Gender == "Female") %>%
  group_by(Age_Bracket) %>%
  summarise(count = n())
#drop the na row
pakistan_female <- pakistan_female %>% slice(-c(7)) 

#make a new data frame of Pakistan which has Age_Bracket , Male ,Female and Total
pakistan <- data.frame(Age_Bracket = pakistan_male$Age_Bracket,
                     Male = pakistan_male$count,
                     Female = pakistan_female$count)
#rename the age brackets
pakistan$Age_Bracket = c("< 1 year","1 to 5", "6 to 16","17 to 35","36 to 49","50 +")
####################################PUNJAB####################################################
punjab_male <- pr %>%
  filter(Province == "Punjab" & Usual_resident == "Yes" & Gender == "Male") %>%
  group_by(Age_Bracket) %>%
  summarise(count = n())

punjab_female <- pr %>%
  filter(Province == "Punjab" & Usual_resident == "Yes" & Gender == "Female") %>%
  group_by(Age_Bracket) %>%
  summarise(count = n())

#make a new data frame of Punjab which has Age_Bracket , Male ,Female and Total
punjab <- data.frame(Age_Bracket = punjab_male$Age_Bracket,
                     Male = punjab_male$count,
                     Female = punjab_female$count)
#rename the age brackets
punjab$Age_Bracket = c("< 1 year","1 to 5", "6 to 16","17 to 35","36 to 49","50 +")
##############################SINDH#########################################################

sindh_male <- pr %>%
  filter(Province == "Sindh" & Usual_resident == "Yes" & Gender =="Male") %>%
  group_by(Age_Bracket) %>%
  summarise(count = n())

sindh_female <- pr %>%
  filter(Province == "Sindh" & Usual_resident == "Yes" & Gender =="Female") %>%
  group_by(Age_Bracket) %>%
  summarise(count = n())

sindh <- data.frame(Age_Bracket = sindh_male$Age_Bracket,
                     Male = sindh_male$count,
                     Female = punjab_female$count)
#rename the age brackets
sindh$Age_Bracket = c("< 1 year","1 to 5", "6 to 16","17 to 35","36 to 49","50 +")

##############################BALOCHISTAN###################################################
balochistan_male <- pr %>%
  filter(Province == "Balochistan" & Usual_resident == "Yes" & Gender =="Male") %>%
  group_by(Age_Bracket) %>%
  summarise(count = n())

balochistan_female <- pr %>%
  filter(Province == "Balochistan" & Usual_resident == "Yes" & Gender =="Female") %>%
  group_by(Age_Bracket) %>%
  summarise(count = n())

balochistan <- data.frame(Age_Bracket = balochistan_male$Age_Bracket,
                    Male = balochistan_male$count,
                    Female = balochistan_female$count)
#rename the age brackets
balochistan$Age_Bracket = c("< 1 year","1 to 5", "6 to 16","17 to 35","36 to 49","50 +")

##############################KPK##########################################################

kpk_male <- pr %>%
  filter(Province == "KPK" & Usual_resident == "Yes"& Gender =="Male") %>%
  group_by(Age_Bracket) %>%
  summarise(count = n())

kpk_female <- pr %>%
  filter(Province == "KPK" & Usual_resident == "Yes" & Gender =="Female") %>%
  group_by(Age_Bracket) %>%
  summarise(count = n())

kpk <- data.frame(Age_Bracket = kpk_male$Age_Bracket,
                          Male = kpk_male$count,
                          Female = kpk_female$count)
#rename the age brackets
kpk$Age_Bracket = c("< 1 year","1 to 5", "6 to 16","17 to 35","36 to 49","50 +")


#############################PLOTTING THE POPULATION PYRAMID###############################

# PAKISTAN DEMOGRAPHIC PYRAMID
graphics.off()

data <- pakistan%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='Male'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='Male'~1,
                          TRUE~-1))
data$Age <- as.factor(data$Age_Bracket)
data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)
# Plotting
ggplot(data,aes(x=Age,y=PopPerc))+
  geom_bar(aes(fill=Gender),stat='identity')+
  #geom_text(aes(label=abs(PopPerc)),)+
  geom_label(aes(label=abs(PopPerc)),position = 'identity',stat ='identity')+
  coord_flip()+
  scale_fill_manual(name='',values=c('darkred','steelblue'),labels = c("Female","Male"))+
  scale_y_continuous(breaks=seq(-20,20,2),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='',y='Population (%)',
       title='Population Pyramid of Pakistan',
       subtitle=paste('Sample Size :', format(sum(data$Population),big.mark=',')),
       caption ='Source: Pakistan Demographic and Health Survey 2017-2018',)+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')



# PUNJAB DEMOGRAPHIC PYRAMID
graphics.off()

data <- punjab%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='Male'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='Male'~1,
                          TRUE~-1))
data$Age <- as.factor(data$Age_Bracket)
data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)
# Plotting
ggplot(data)+
  geom_bar(aes(x=Age,y=PopPerc,fill=Gender),stat='identity')+
  geom_text(aes(x=Age,y=PopPerc+signal*.3,label=abs(PopPerc)))+
  coord_flip()+
  scale_fill_manual(name='',values=c('purple','brown'),labels = c("Female","Male"))+
  scale_y_continuous(breaks=seq(-20,20,2),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='',y='Population (%)',
       title='Population Pyramid of Punjab',
       subtitle=paste('Sample Size :', format(sum(data$Population),big.mark=',')),
       caption ='Source: Pakistan Demographic and Health Survey 2017-2018',)+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')

# SINDH DEMOGRAPHIC PYRAMID
data <- sindh%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='Male'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='Male'~1,
                          TRUE~-1))
data$Age <- as.factor(data$Age_Bracket)
data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)
# Plotting
ggplot(data)+
  geom_bar(aes(x=Age,y=PopPerc,fill=Gender),stat='identity')+
  geom_text(aes(x=Age,y=PopPerc+signal*.3,label=abs(PopPerc)))+
  coord_flip()+
  scale_fill_manual(name='',values=c('pink','orange'),labels = c("Female","Male"))+
  scale_y_continuous(breaks=seq(-20,20,2),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='',y='Population (%)',
       title='Population Pyramid of Sindh',
       subtitle=paste('Sample Size :', format(sum(data$Population),big.mark=',')),
       caption ='Source: Pakistan Demographic and Health Survey 2017-2018',)+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')

# BALOCHISTAN DEMOGRAPHIC PYRAMID
data <- balochistan%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='Male'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='Male'~1,
                          TRUE~-1))
data$Age <- as.factor(data$Age_Bracket)
data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)
# Plotting
ggplot(data)+
  geom_bar(aes(x=Age,y=PopPerc,fill=Gender),stat='identity')+
  geom_text(aes(x=Age,y=PopPerc+signal*.3,label=abs(PopPerc)))+
  coord_flip()+
  scale_fill_manual(name='',values=c('cyan','green'),labels = c("Female","Male"))+
  scale_y_continuous(breaks=seq(-20,20,2),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='',y='Population (%)',
       title='Population Pyramid of Balochistan',
       subtitle=paste('Sample Size :', format(sum(data$Population),big.mark=',')),
       caption ='Source: Pakistan Demographic and Health Survey 2017-2018',)+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')

# KPK DEMOGRAPHIC PYRAMID
data <- kpk%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='Male'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='Male'~1,
                          TRUE~-1))
data$Age <- as.factor(data$Age_Bracket)
data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)
# Plotting
ggplot(data)+
  geom_bar(aes(x=Age,y=PopPerc,fill=Gender),stat='identity')+
  geom_text(aes(x=Age,y=PopPerc+signal*.3,label=abs(PopPerc)))+
  coord_flip()+
  scale_fill_manual(name='',values=c('gray','darkblue'),labels = c("Female","Male"))+
  scale_y_continuous(breaks=seq(-20,20,2),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='',y='Population (%)',
       title='Population Pyramid of KPK',
       subtitle=paste('Sample Size :', format(sum(data$Population),big.mark=',')),
       caption ='Source: Pakistan Demographic and Health Survey 2017-2018',)+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')
