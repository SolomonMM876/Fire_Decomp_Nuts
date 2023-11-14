library(tidyverse)
library(ggplot2)
library(forcats)
library(patchwork)
library(splitstackshape)
library(tidyr)
library(car)
library(vegan)
library(nlme)
library(readxl)
library(ggrepel)

#setwd("\\\\ad.uws.edu.au/dfshare/HomesCMB$/90957135/My Documents/ABS_FIRE/Fire_Decomp_Nuts")
#All of these data can be sourced from 'Nutrient_Script', 
#but have been simplified so only the necessery code is present and no graphs made except PCA's

###Site Descriptions####
Site_Info<-read.csv('Decompisition Site Locations.csv')
Site_Info
colnames(Site_Info)[1]<-'Site'
Site_Info$Site= sub(c('ABS00'),'',Site_Info$Site)
Site_Info$Site= sub(c('ABS0'), '',Site_Info$Site)

#Bray_P####
#Load data
Bray_P <- lapply(c('23-06-07 solomon bray -P samples lot1 2023.csv',
                   '23-06-08 solomon bray-P samples lot2.csv',
                   '23-06-09 solomon bray-p samples lot3.csv'),read.csv)
#Clean
Bray_P<-bind_rows(Bray_P, .id = "sample.ID")
Bray_P$Sample.ID<-sub(c("b"),"",Bray_P$Sample.ID)
Bray_P$Sample.ID<-sub(c("a"),"",Bray_P$Sample.ID)
Bray_P$Sample.ID<-sub(c(" "),"-",Bray_P$Sample.ID)
colnames(Bray_P)[2]<-'Transect'
colnames(Bray_P)[5]<-'Bray.P'
Bray_P$Bray.P<-Bray_P$Bray.P * 50/3.57 #50mL extract for 3.57g's of soil
Bray_P$Units<-'mg/kg'
Bray_P<-Bray_P %>% separate(Transect,c('Site','Transect'))
Bray_P$Site<-as.factor(as.numeric(Bray_P$Site))
Bray_P$Bray.P<-as.numeric(Bray_P$Bray.P)
Bray_P<-Bray_P %>%drop_na(Site)

#merge tech replicates
Site.Mean.Bray<-Bray_P%>%group_by(Site)%>%
  summarise(Bray.P=mean(Bray.P))

Bray_P.Site<-left_join(Site.Mean.Bray,Site_Info, by = join_by(Site))
Bray_All<-left_join(Bray_P,Site_Info, by = join_by(Site))

#N####
#setwd("\\\\ad.uws.edu.au/dfshare/HomesCMB$/90957135/My Documents/ABS_FIRE/Fire_Decomp_Nuts")
#import
NH4NO3<-lapply(c('23-06-13 solomon NH4NO3 LOT1.csv',
                 '23-06-13 solomon NH4NO3 LOT2.csv',
                 '23-06-15 solomon nh4no3 lot3 .csv',
                 '23-06-16 solomon NH4NO3 LOT4 LAST.csv'),read.csv)

#clean and organize
NH4NO3<-bind_rows(NH4NO3, .id = "sample.ID")
NH4NO3$Sample.ID<-sub(c("a"),"",NH4NO3$Sample.ID)
NH4NO3$Sample.ID<-sub(c("b"),"",NH4NO3$Sample.ID)
NH4NO3$Sample.ID<-sub(c(" "),"-",NH4NO3$Sample.ID)
#convert units
NH4NO3$Result<-NH4NO3$Result * 50/5 #50mL extract for 5g's of soil
NH4NO3$Units<-'mg/kg'
colnames(NH4NO3)[2]<-'Transect'
NH4NO3<-NH4NO3 %>% separate(Transect,c('Site','Transect'))
NH4NO3$Site<-as.factor(as.numeric(NH4NO3$Site))
NH4NO3<-NH4NO3 %>%drop_na(Site)
str(NH4NO3)
#remove sites that are not part of 30 sites
#54,55,61,63

New.NH4NO3<-subset(NH4NO3,!Site %in% c(54,55,61,63))



#split to two dfs
NH4<-New.NH4NO3[New.NH4NO3$Test.Name =="Ammonia 2.0",]
colnames(NH4)[6]<-'NH4'

NO3<-New.NH4NO3[New.NH4NO3$Test.Name =="Nitrate 2",]
colnames(NO3)[6]<-'NO3'

#remove extreme outliers
NO3.adj<- NO3[!(NO3$Site == 58 & NO3$Transect == 1), ]
NO3.adj<- NO3.adj[!(NO3.adj$Site == 39 & NO3.adj$Transect == 1), ]

#merge tech replicates
Site.Mean.NH4<-NH4%>%group_by(Site)%>%
  summarise(NH4=mean(NH4))

NH4.Site<-left_join(Site.Mean.NH4,Site_Info, by = join_by(Site))
NH4_All<-left_join(NH4,Site_Info, by = join_by(Site))

Site.Mean.NO3<-NO3.adj%>%group_by(Site)%>%
  summarise(NO3=mean(NO3))

NO3.Site<-left_join(Site.Mean.NO3,Site_Info, by = join_by(Site))
NO3_All<-left_join(NO3,Site_Info, by = join_by(Site))
###Total P####

Total_P<-read.csv("CSBP_Total_P.csv")
names(Total_P)[6]<-'Transect'

#clean and organize
colnames(Total_P)<-Total_P[3,]
ncol(Total_P)
colnames(Total_P)[8]<-"Total.P"
colnames(Total_P)[6]<-"Transect"
Total_P<-Total_P[-c(1,2,3,65),]
Total_P$Total.P<-as.numeric(Total_P$Total.P)
Total_P$Transect<-as.character(Total_P$Transect)

#remove empty columns 
Total_P<-Total_P[-c(2,4)]

Total_P<-separate(Total_P,Transect,into=c('Site','Transect'),sep='-')
Total_P$Site<-as.factor(as.numeric(Total_P$Site))


Site.Mean.T.P<-Total_P%>%group_by(Site)%>%
  summarise(Total.P=mean(Total.P))

Total_P.Site<-left_join(Site.Mean.T.P,Site_Info, by = join_by(Site))
Total_P_All<-left_join(Total_P,Site_Info, by = join_by(Site))
#Total_C+N####
#setwd("\\\\ad.uws.edu.au/dfshare/HomesCMB$/90957135/My Documents/ABS_FIRE/Fire_Decomp_Nuts")
#Load Data
Total_CN<-read.csv("CN SOIL SAMPLES_2.0.csv")

colnames(Total_CN)[1]<-'Transect'
colnames(Total_CN)[5]<-'Carbon'
colnames(Total_CN)[6]<-'Nitrogen'
Total_CN<-Total_CN%>% separate(Transect, c('Site','Transect'))
Total_CN$Site<-as.factor(as.numeric(Total_CN$Site))
Total_CN$Transect<-sub(c("T"),"",Total_CN$Transect)
Total_CN$Carbon<-as.numeric(Total_CN$Carbon)
Total_CN$Nitrogen<-as.numeric(Total_CN$Nitrogen)



Site.Mean.T.C.N<-Total_CN%>%group_by(Site)%>%
  summarise(Carbon=mean(Carbon),
            Nitrogen=mean(Nitrogen))

Total_CN.Site<-left_join(Site.Mean.T.C.N,Site_Info, by = join_by(Site))
Total_CN_All<-left_join(Total_CN,Site_Info, by = join_by(Site))



######Nutrient data#######
Site_Info <- Site_Info %>%
  select(Site, Severity, Interval)

df_list<-list(Site.Mean.T.C.N, Site.Mean.T.P, Site.Mean.Bray,Site.Mean.NH4, Site.Mean.NO3) 
Nutrients.Sites<-Reduce(function(x, y) merge(x, y, all=F), df_list)

Treat.w.nutes<-left_join(Site_Info,Nutrients.Sites, by="Site")
Nutrients.Sites <- mutate_all(Nutrients.Sites, function(x) as.numeric(as.character(x)))
#Treat.w.nutes<-data.frame(Treat.w.nutes, row.names=1)


Nutrient.num <- mutate_all(Nutrients.Sites, function(x) as.numeric(as.character(x)))
Nutrient.num_r<-data.frame(Nutrient.num, row.names=1)


#Nute.pca <- rda( Nutrient.num_r~ Severity + Interval, data=Treat.w.nutes, scale=TRUE)
Nute.pca <- rda( Nutrient.num_r, data=Treat.w.nutes, scale=TRUE)

#Nute.pca<-rda(Nutrient.num, scale=TRUE)
plot(Nute.pca)    
summary(Nute.pca)


Nute.spe2.sc <- scores(Nute.pca, tidy=T)
Nute.spe2.sc.ar <- Nute.spe2.sc %>% 
  filter(score=='species')
Nute.spe2.sc.ar <- tibble::rownames_to_column(Nute.spe2.sc.ar)


Nute_Sites <- Nute.spe2.sc %>%
  filter(score=='sites') %>% 
  rename(Site=label) %>% 
  left_join(Site_Info)

p<-ggplot(Nute_Sites, aes(x=PC1, y=PC2, colour=Severity, shape=Interval, label=Site )) + 
  geom_point(size=5)+ 
  geom_segment(data=Nute.spe2.sc.ar,inherit.aes = FALSE,
               aes(x=0,y=0, xend=PC1, yend=PC2, group=rowname),
               arrow = arrow(type = "closed",length=unit(4,'mm')),
               color= 'black') +
  geom_text(colour='black',size=4)+
  geom_text_repel(data= Nute.spe2.sc.ar, inherit.aes = FALSE,
                  aes(x= PC1,y=PC2,label=rowname))+
  labs(x = "PC1 56%", y = "PC2 16%")+ theme_minimal()

p
#setwd('graphs')
#ggsave(filename=paste('Nute_PCA_num.png'), width = 25, height = 15, dpi = 800, units = "cm", device='png')


plotly::ggplotly(p)

###Veg data####
#setwd("\\\\ad.uws.edu.au/dfshare/HomesCMB$/90957135/My Documents/ABS_FIRE/Fire_Decomp_Nuts")
#Load Data

Veg_Cover<-read_excel("ABS.MER.fielddata.Feb.2023_R.PROCESSED.VEG.COVER_ALL.xlsx", sheet="Site.Level_Data")
head(Veg_Cover)
colnames(Veg_Cover)[1]<-'Site'
Veg_Cover$Site= sub(c('ABS00'),'',Veg_Cover$Site)
Veg_Cover$Site= sub(c('ABS0'), '',Veg_Cover$Site)
head(Veg_Cover)
colnames(Veg_Cover)[4]<-'Interval'
colnames(Veg_Cover)[8]<-'Severity'
colnames(Veg_Cover)[6]<-'Second_Most_Receant_Fire_Year'
colnames(Veg_Cover)[7]<-'Third_Most_Receant_Fire_Year'

#now join both df so only decomp sites are present in df

Veg_Cover_Decomp<-left_join(Site_Info,Veg_Cover, by=c('Site', "Interval",'Severity'))
Veg_Cover<-as.data.frame(Veg_Cover_Decomp)
#remove col with characters
Veg_Cover.num <- Veg_Cover_Decomp %>%
  subset(select=-c( Severity:Sample.Date_Fieldwork))
colnames(Veg_Cover.num)
Veg_Cover.num <- mutate_all(Veg_Cover.num, function(x) as.numeric(as.character(x)))
Veg_Cover.num<-as.data.frame(Veg_Cover.num)






Veg_Cover_r<-data.frame(Veg_Cover, row.names=1)
Veg_Cover_num.r<-data.frame(Veg_Cover.num, row.names=1)


view(Veg_Cover_r)




Veg_Nute.pca <- rda( Veg_Cover_num.r~ Severity + Interval, data=Veg_Cover_r, scale=TRUE)
Veg.pca <- rda( Veg_Cover_num.r, data=Veg_Cover_r, scale=TRUE)
###Making PCA#

plot(Veg.pca)    
summary(Veg.pca)


Veg.spe2.sc <- scores(Veg.pca, tidy=T)
Veg.spe2.sc.ar <- Veg.spe2.sc %>% 
  filter(score=='species')
Veg.spe2.sc.ar <- tibble::rownames_to_column(Veg.spe2.sc.ar)


Veg_Sites <- Veg.spe2.sc %>%
  filter(score=='sites') %>% 
  rename(Site=label) %>% 
  left_join(Site_Info)

#Veg_Sites<-na.omit(Veg_Sites)


p<-ggplot(Veg_Sites, aes(x=PC1, y=PC2, colour=Severity, shape=Interval, label=Site )) + 
  geom_point(size=5)+ 
  geom_segment(data=Veg.spe2.sc.ar,inherit.aes = FALSE,
               aes(x=0,y=0, xend=PC1, yend=PC2, group=rowname),
               arrow = arrow(type = "closed",length=unit(4,'mm')),
               color= 'black') +
  geom_text(colour='black',size=4)+
  geom_text_repel(data= Veg.spe2.sc.ar, inherit.aes = FALSE,
                  aes(x= PC1,y=PC2,label=rowname))+
  labs(x = "PC1 30.5%", y = "PC2 21%")+ theme_minimal()

p
#setwd('graphs')
#ggsave(filename=paste('Veg_PCA_num.png'), width = 25, height = 15, dpi = 800, units = "cm", device='png')

plotly::ggplotly(p)

###Veg and Nute data####

Veg_Cover.num <- mutate_all(Veg_Cover.num, function(x) as.numeric(as.character(x)))
Veg_Nute_Join<-left_join(Nutrients.Sites,Veg_Cover.num, by="Site") #numeric site data
Veg_Nute_Join<-data.frame(Veg_Nute_Join, row.names=1)
str(Veg_Nute_Join)

Veg_Nute_Join_All<-left_join(Treat.w.nutes,Veg_Cover, by="Site")#all site data
colnames(Veg_Nute_Join_All)[3]<-'Interval'
colnames(Veg_Nute_Join_All)[2]<-'Severity'
Veg_Nute_Join_All_r<-data.frame(Veg_Nute_Join_All, row.names=1)

str(Veg_Nute_Join_All)

Veg_Nute.pca <- rda( Veg_Nute_Join~ Severity + Interval, data=Veg_Nute_Join_All_r, scale=TRUE)#RDAs explain 3.5%
Veg_Nute.pca <- rda( Veg_Nute_Join, data=Veg_Nute_Join_All_r, scale=TRUE)
###Making PCA#

plot(Veg_Nute.pca)    
summary(Veg_Nute.pca)

All.spe2.sc <- scores(Veg_Nute.pca, tidy=T)
All.spe2.sc.ar <- All.spe2.sc %>% 
  filter(score=='species')
All.spe2.sc.ar <- tibble::rownames_to_column(All.spe2.sc.ar)


All_Sites <- All.spe2.sc %>%
  filter(score=='sites') %>% 
  rename(Site=label) %>% 
  left_join(Site_Info)



p<-ggplot(All_Sites, aes(x=PC1, y=PC2, colour=Severity, label=Site )) + 
  geom_point(size=5.5)+ 
  geom_segment(data=All.spe2.sc.ar,inherit.aes = FALSE,
               aes(x=0,y=0, xend=PC1, yend=PC2, group=rowname),
               arrow = arrow(type = "closed",length=unit(4,'mm')),
               color= 'black') +
  geom_text(colour='black',size=4)+
  geom_text_repel(data= All.spe2.sc.ar, inherit.aes = FALSE, size=3,
                  aes(x= PC1,y=PC2,label=rowname))+
  labs(x = "PC1 25.1%", y = "PC2 22.7%")+ theme_minimal()

p
#setwd('graphs')
#ggsave(filename=paste('Nutes+Veg_PCA.png'), width = 25, height = 15, dpi = 800, units = "cm", device='png')


plotly::ggplotly(p)

