#####################################################################
#Title: Indicator : Demographic Graphs
#Author: Muhammad Uzair Aslam
#Date: 7/17/2021
#Purpose: Analyze Pakistan Population Demographics
#Data Sets : GLobal Burden Of Disease , World Bank , PDHS 2017-2018
#####################################################################

library(tidyverse)
library(readxl)
library(cowplot)
library(xlsx)
########################################GLOBAL BURDEN OF DISEASE PAKISTAN###########################################

pop_data <- read_excel("E:/IHI-2/IHI-2 Important Tables/Pakistan Demographics.xlsx",sheet = 3)

pop_data <- select(pop_data ,c(Age_Bracket, M, F))
# Manipulating data

data <- pop_data%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='M'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='M'~1,
                          TRUE~-1))
data$Age <- as.factor(data$Age_Bracket)
data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)
# Plotting
ggplot(data)+
  geom_bar(aes(x=Age,y=PopPerc,fill=Gender),stat='identity')+
  geom_text(aes(x=Age,y=PopPerc+signal*.3,label=abs(PopPerc)))+
  coord_flip()+
  scale_fill_manual(name='',values=c('darkred','steelblue'),labels = c("Female","Male"))+
  scale_y_continuous(breaks=seq(-20,20,2),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='',y='Population (%)',
       title='Population Pyramid of Pakistan',
       subtitle=paste('Total resident population in 2019:', format(sum(data$Population),big.mark=',')),
       caption ='Source: Global Health Data Exchange 2019 Population Estimates',)+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')
########################################WHO DATA PLOT PAKISTAN####################################################################
who_data <- read_excel("E:/IHI-2/IHI-2 Global Health Data Exchange/Data/Population/Pakistan Demographics.xlsx",sheet = 5)

pop_data <- select(who_data ,c(Age_Bracket, M, F))
# Manipulating data

data <- pop_data%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='M'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='M'~1,
                          TRUE~-1))
data$Age <- as.factor(data$Age_Bracket)
data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)
# Plotting
ggplot(data)+
  geom_bar(aes(x=Age,y=PopPerc,fill=Gender),stat='identity')+
  geom_text(aes(x=Age,y=PopPerc+signal*.3,label=abs(PopPerc)))+
  coord_flip()+
  scale_fill_manual(name='',values=c('darkred','steelblue'),labels = c("Female","Male"))+
  scale_y_continuous(breaks=seq(-20,20,2),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='',y='Population (%)',
       title='Population Pyramid of Pakistan',
       subtitle=paste('Total resident population in 2020:', format(sum(data$Population),big.mark=',')),
       caption ='Data Source: World Bank',)+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')

###################################CENSUS PAKISTAN##################################################


pakistan_data <- read_excel("E:/IHI-2/Project/tables/IHI-2 Important Tables/IHHN-2 Tables II.xlsx",sheet = 6)

data <- pakistan_data%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='M'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='M'~1,
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
       title='Population Pyramid of Pakistan - Census 2017',
       subtitle=paste('Total Population :', format(sum(data$Population),big.mark=',')),
       caption ='Source: Census 2017',)+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')

###################################PUNJAB#####################################################
punjab_data <- read_excel("E:/IHI-2/Project/tables/IHI-2 Important Tables/IHHN-2 Tables II.xlsx",sheet = 7)

data <- punjab_data%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='M'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='M'~1,
                          TRUE~-1))
data$Age <- as.factor(data$Age_Bracket)
data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)
# Plotting
ggplot(data,aes(x=Age,y=PopPerc))+
  geom_bar(aes(fill=Gender),stat='identity')+
  #geom_text(aes(label=abs(PopPerc)),)+
  geom_label(aes(label=abs(PopPerc)),position = 'identity',stat ='identity')+
  coord_flip()+
  scale_fill_manual(name='',values=c('skyblue1','blue4'),labels = c("Female","Male"))+
  scale_y_continuous(breaks=seq(-20,20,2),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='',y='Population (%)',
       title='Population Pyramid of Punjab - Census 2017',
       subtitle=paste('Total Population :', format(sum(data$Population),big.mark=',')),
       caption ='Source: Census 2017',)+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')

#####################################SINDH############################################################
sindh_data <- read_excel("E:/IHI-2/Project/tables/IHI-2 Important Tables/IHHN-2 Tables II.xlsx",sheet = 8)

data <- sindh_data%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='M'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='M'~1,
                          TRUE~-1))
data$Age <- as.factor(data$Age_Bracket)
data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)
# Plotting
ggplot(data,aes(x=Age,y=PopPerc))+
  geom_bar(aes(fill=Gender),stat='identity')+
  #geom_text(aes(label=abs(PopPerc)),)+
  geom_label(aes(label=abs(PopPerc)),position = 'identity',stat ='identity')+
  coord_flip()+
  scale_fill_manual(name='',values=c('turquoise1','turquoise4'),labels = c("Female","Male"))+
  scale_y_continuous(breaks=seq(-20,20,2),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='',y='Population (%)',
       title='Population Pyramid of Sindh - Census 2017',
       subtitle=paste('Total Population :', format(sum(data$Population),big.mark=',')),
       caption ='Source: Census 2017',)+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')

#####################################SINDH############################################################
sindh_data <- read_excel("E:/IHI-2/Project/tables/IHI-2 Important Tables/IHHN-2 Tables II.xlsx",sheet = 8)

data <- sindh_data%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='M'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='M'~1,
                          TRUE~-1))
data$Age <- as.factor(data$Age_Bracket)
data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)
# Plotting
ggplot(data,aes(x=Age,y=PopPerc))+
  geom_bar(aes(fill=Gender),stat='identity')+
  #geom_text(aes(label=abs(PopPerc)),)+
  geom_label(aes(label=abs(PopPerc)),position = 'identity',stat ='identity')+
  coord_flip()+
  scale_fill_manual(name='',values=c('turquoise1','turquoise4'),labels = c("Female","Male"))+
  scale_y_continuous(breaks=seq(-20,20,2),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='',y='Population (%)',
       title='Population Pyramid of Sindh - Census 2017',
       subtitle=paste('Total Population :', format(sum(data$Population),big.mark=',')),
       caption ='Source: Census 2017',)+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')

#######################################BALOCHISTAN##############################################
baloch_data <- read_excel("E:/IHI-2/Project/tables/IHI-2 Important Tables/IHHN-2 Tables II.xlsx",sheet = 9)

data <- baloch_data%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='M'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='M'~1,
                          TRUE~-1))
data$Age <- as.factor(data$Age_Bracket)
data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)
# Plotting
ggplot(data,aes(x=Age,y=PopPerc))+
  geom_bar(aes(fill=Gender),stat='identity')+
  #geom_text(aes(label=abs(PopPerc)),)+
  geom_label(aes(label=abs(PopPerc)),position = 'identity',stat ='identity')+
  coord_flip()+
  scale_fill_manual(name='',values=c('tan1','tan4'),labels = c("Female","Male"))+
  scale_y_continuous(breaks=seq(-20,20,2),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='',y='Population (%)',
       title='Population Pyramid of Balochistan - Census 2017',
       subtitle=paste('Total Population :', format(sum(data$Population),big.mark=',')),
       caption ='Source: Census 2017',)+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')
####################################KPK##########################################################
kp_data <- read_excel("E:/IHI-2/Project/tables/IHI-2 Important Tables/IHHN-2 Tables II.xlsx",sheet = 10)

data <- kp_data%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='M'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='M'~1,
                          TRUE~-1))
data$Age <- as.factor(data$Age_Bracket)
data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)
# Plotting
ggplot(data,aes(x=Age,y=PopPerc))+
  geom_bar(aes(fill=Gender),stat='identity')+
  #geom_text(aes(label=abs(PopPerc)),)+
  geom_label(aes(label=abs(PopPerc)),position = 'identity',stat ='identity')+
  coord_flip()+
  scale_fill_manual(name='',values=c('lightpink1','lightpink4'),labels = c("Female","Male"))+
  scale_y_continuous(breaks=seq(-20,20,2),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='',y='Population (%)',
       title='Population Pyramid of KPK - Census 2017',
       subtitle=paste('Total Population :', format(sum(data$Population),big.mark=',')),
       caption ='Source: Census 2017',)+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')


################################Women Break Up##################################################
women_data <- read_excel("E:/IHI-2/Project/tables/IHI-2 Important Tables/IHHN-2 Tables II.xlsx",sheet = 11)
g <- ggplot(data, aes(Percent)) 
g + geom_bar(stat = "identity")
