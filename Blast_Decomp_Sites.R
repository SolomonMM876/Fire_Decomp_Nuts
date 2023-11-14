# load libraries

library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(phyloseq)
library(forcats)
library('psadd')
library('QsRutils')
library('ggordiplots')
#library('pairwiseAdonis')
#devtools::install_github('jfq3/QsRutils')
#devtools::install_github('pmartinezarbizu/pairwiseAdonis/pairwiseAdonis')


source('Site Map.R')
source('Nutrient_Script.R')
source('FunGuild Script.R')
source('PCA with veg and nute script.R')


###load blast data####
Blast_dat<-read.csv('fastfa.blast.ABS.csv')
colnames(Blast_dat)[1]<-'sample_ID'
Blast_dat$sample_ID<-as.factor(Blast_dat$sample_ID)

# transpose all but the first column (name)
# first remember the names col
n <- Blast_dat$sample_ID

# Remove the last column (character column) from Blast_dat
Blast_dat <- Blast_dat[1:(length(Blast_dat) - 1)]

# Transpose the Blast_dat data, excluding the first column (name)
t_Blast_dat <- as.data.frame(x = t(Blast_dat[, -1]))

# Set the column names of t_Blast_dat to the values in 'n'
colnames(t_Blast_dat) <- n

# Convert t_Blast_dat to a tibble and add a 'sample_ID' column with row names
t_Blast_dat <- tibble::rownames_to_column(t_Blast_dat, "sample_ID")

# Remove the '.09FU' suffix from column names using gsub
#names(t_Blast_dat) <- gsub('.09FU', '', names(t_Blast_dat), fixed = TRUE)



#import Blast ID df and clean datya
Blast_ID<-read_excel('fastfa_Site.Info.xlsx')
head(Blast_ID)
Blast_ID$Site= sub(c('ABS00'),'',Blast_ID$Site)
Blast_ID$Site= sub(c('ABS0'), '',Blast_ID$Site)
Blast_ID$sample_ID<-as.character(Blast_ID$sample_ID)
Blast_ID$sample_ID<-sub(c("-"),".",Blast_ID$sample_ID)
Blast_ID$sample_ID<-sub(c("-"),".",Blast_ID$sample_ID)

#join into the same df

Blast_Site<-left_join(Blast_ID,t_Blast_dat, by='sample_ID')

#import only decomp site info
Decomp_Site_Info<-read.csv('Decompisition Site Locations.csv')
Decomp_Site_Info
colnames(Decomp_Site_Info)[1]<-'Site'
Decomp_Site_Info$Site<-as.factor(Decomp_Site_Info$Site)
Decomp_Site_Info$Site= sub(c('ABS00'),'',Decomp_Site_Info$Site)
Decomp_Site_Info$Site= sub(c('ABS0'), '',Decomp_Site_Info$Site)

#now join both df so only decomp sites are present in df
Decomp_Blast<-left_join(Decomp_Site_Info,Blast_Site, by=c('Site','Severity','Interval'))


##join with veg and nute data by site
#first have to deal with transects
Decomp_Blast_Site<-Decomp_Blast%>%
  group_by(Site)%>%
  summarise(across(starts_with('SH'),sum), .names = "{.col}")

#veg_Nut_Join All is made in PCA script

All_ABS_Site<-left_join(Veg_Nute_Join_All, Decomp_Blast_Site, by='Site')
#id cols that begin with SH to make later extraction easier for sample ids
colnames(All_ABS_Site[1:40])
colnames(All_ABS_Site)[24:25]<-c('x.Shrub.Cover_50.200cm_perc' , "x.Shrub.Connected_50.200cm_perc")

################################################

# extract taxonomy info from the blast results
Blast_dat<-read.csv('fastfa.blast.ABS.csv')
Blast_dat<-Blast_dat[-c(1,2), ]   #remove first two rows that do not have taxonomy ids

names(Blast_dat)[1] <- 'OTUId'

# split the 'taxonomy' column in 'tax' by the delimiter
temp <- strsplit(Blast_dat$taxonomy, split=c(';'))

# collapse into a matrix, naming each column
temp <- do.call('rbind', temp)
#kingdom does something weird here
colnames(temp) <- c('kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species')

# any kingdom-to-genus unclassified, change to NA
temp[temp %in% grep('unclassified', temp, value=T)] <- NA

# any species unclassified, change to NA
temp[temp %in% grep('_sp$', temp, value=T)] <- NA

# clean up characters at the start of each remaining annotation
temp <- gsub('^[a-z]\\_\\_', '', temp)

# add new columns onto 'tax' data.frame
tax <- cbind(Blast_dat, temp, stringsAsFactors=F)

# clean up workspace
rm(temp)

tax

#my attempt to join funguild object to tax table

mycorhiz_tax<-left_join(tax,Mycorhiz%>%select(taxon,guild), by=c('genus'='taxon'))







# extract the OTU data from 'df' and convert to matrix format
# using 'starts_with' with 'select' allows us to chose all columns containing OTU counts
otus <- as.matrix(select(All_ABS_Site, starts_with('SH')))

str(otus)
# rows need to be named with the sample names from 'All_ABS_Site', 
# which we can do directly because they are in the same order
rownames(otus) <- All_ABS_Site$Site
dim(otus)

#read in pairs based on PCA
pairs<-read_excel('Pair_Labels.xlsx')
colnames(pairs)[1]<-'Site'
pairs$Site= sub(c('ABS00'),'',pairs$Site)
pairs$Site= sub(c('ABS0'), '',pairs$Site)
pairs$Pair_All<-as.character(pairs$Pair_All)
pairs$Pair_Nute<-as.character(pairs$Pair_Nute)
pairs$Pair_Veg<-as.character(pairs$Pair_Veg)

#Calculate the average PC1 and PC2 scores for all the different PCA's
pairs1 <- All_Sites %>%
  group_by(Pair_All= case_when(Site %in% c(9, 25) ~ "4", 
                               Site %in% c(40, 45) ~ "1",
                               Site %in% c(3, 8) ~ "3",
                               Site %in% c(10, 38) ~ "2",
                               Site %in% c(58, 12) ~ "6",
                               Site %in% c(37, 6) ~ "5",)) %>%
  summarise(All_PC1 = mean(PC1), All_PC2 = mean(PC2)) %>%
  left_join(pairs, by='Pair_All')


pairs2 <- Veg_Sites %>%
  group_by(Pair_Veg= case_when(Site %in% c(1, 31) ~ "4", 
                               Site %in% c(12, 58) ~ "1",
                               Site %in% c(26, 57) ~ "3",
                               Site %in% c(25, 27) ~ "2",
                               Site %in% c(5, 8) ~ "6",
                               Site %in% c(30, 6) ~ "5",)) %>%
  summarise(Veg_PC1 = mean(PC1), Veg_PC2 = mean(PC2)) %>%
  right_join(pairs1, by='Pair_Veg')

pairs3 <- Nute_Sites %>%
  group_by(Pair_Nute= case_when(Site %in% c(12, 31) ~ "4", 
                               Site %in% c(10, 56) ~ "1",
                               Site %in% c(5,49) ~ "3",
                               Site %in% c(26, 8) ~ "2",
                               Site %in% c(11, 29) ~ "6",
                               Site %in% c(7, 34) ~ "5",)) %>%
  summarise(Nute_PC1 = mean(PC1), Nute_PC2 = mean(PC2)) %>%
  right_join(pairs2, by='Pair_Nute')



pairs3 <- All_Sites %>%
  filter(Site %in% c(10,34,8,7,11,57,1,9,12,58,27,56)) %>%
  group_by(Site) %>%
  summarise(All_PC1_S = (PC1), All_PC2_S = (PC2)) %>%
  right_join(pairs3, by='Site')





# extract the sample data from from 'All_ABS_Site' and keep as dataframe format (mix of character and numeric variables)
samps <- select(All_ABS_Site, Site:Dead.Tree.Canopy.Cover_perc) %>%
  left_join(pairs3)




# rows need to be named with the sample names from 'df', 
# which we can do directly because they are in the same order
rownames(samps) <- All_ABS_Site$Site

# extract the taxonomy info for each OTU from 'tax' and convert to matrix format
taxonomy <- as.matrix(select(mycorhiz_tax, phylum:guild))
# rows need to be named with the OTU names from 'tax', 
# which we can do directly because they are in the same order
rownames(taxonomy) <- tax$OTUId
dim(taxonomy)

# merge the three objects into a single phyloseq object using 'merge_phyloseq'
# each function nested within the call to 'merge_phyloseq' creates a special object for that type of data
# because of how the OTU matrix is oriented, we have to specify 'taxa_are_rows=F' 
phy <- merge_phyloseq(otu_table(otus, taxa_are_rows=F), 
                      sample_data(samps), tax_table(taxonomy))

# remove extra objects to keep workspace tidy
#rm(otus, samps, taxonomy)


#rarefy_even_depth()

#####################################################################
#remove sites 56,58,59,60
sub_phy<-subset_samples(phy, !(sample_names(phy)%in% c('56','58','59','60')))
# keep only AM
AM_Myco_phy <- subset_taxa(phy, guild %in% c('Arbuscular Mycorrhizal'))
AM_Myco_sub_phy<-subset_samples(AM_Myco_phy, !(sample_names(phy)%in% c('56','58','59','60')))
# keep only Ecto's
Myco_phy <- subset_taxa(phy, guild %in% c('Arbuscular Mycorrhizal','Ectomycorrhizal'))
Myco_sub_phy<-subset_samples(Myco_phy, !(sample_names(phy)%in% c('56','58','59','60')))


plot_bar(phy, x='Site', fill='phylum') + 
  geom_bar(aes(color=phylum), stat='identity', position='stack')+
  theme(axis.text.x = element_text(angle = -45, vjust = 0, hjust=.5))+
  theme_minimal()



# bar plot of phylum by treatment wihtout sites with low reads
plot_bar(sub_phy, x='Site', fill='phylum') + 
  # add this line to suppress OTU counts
  geom_bar(aes(color=phylum), stat='identity', position='stack')+
  theme(axis.text.x = element_text(angle = -45, vjust = 0, hjust=.5))+
  theme_minimal()



#only AM
plot_bar(AM_Myco_sub_phy, x='Site', fill='genus') + 
  # add this line to suppress OTU counts
  geom_bar(aes(color=genus), stat='identity', position='stack')+
  theme(axis.text.x = element_text(angle = -45, vjust = 0, hjust=.5))+
  theme_minimal()



#All Mycorhiz
plot_bar(Myco_sub_phy, x='Site', fill='genus') + 
  geom_bar(aes(color=genus), stat='identity', position='stack')+
  theme(axis.text.x = element_text(angle = -45, vjust = 0, hjust=.5))+
  theme_minimal()


# make the network object
net <- make_network(phy, 'samples', max.dist=0.9)

# plot the network object, setting symbol colour and shape
plot_network(net, phy, color='Site', shape='Severity')

# plot the ordination using multidimensional scaling of Bray-Curtis distances 
ordination_df<-plot_ordination(Myco_sub_phy, ordinate(Myco_sub_phy, method='PCoA', distance='bray'), 
                color='Severity', shape='Interval', label='Site', justDF = T)
plot_ordination(Myco_sub_phy, ordinate(Myco_sub_phy, method='PCoA', distance='bray'), 
                type='scree')


p<-ggplot(ordination_df, aes(x=Axis.1, y=Axis.2, colour=Severity, shape=Interval, label=Site )) + 
  geom_point(size=5)+ 
  geom_text(colour='black',size=4)+
 # stat_ellipse(type = "norm", linetype = 2) +
 # stat_ellipse(type = "t") +
  labs(x = "PCoA1 12.6%", y = "PCoA2 11.5%")+ theme_minimal()
p






#This analysis is with just Ectos and excluding the funky low read sites

#Permanova pairs with both Nute and veg data
tmp <- subset_samples(Myco_sub_phy, !is.na(Pair_All))
adonis2(otu_table(tmp) ~ All_PC1+ All_PC2 +Severity + Interval, data=as(sample_data(tmp), 'data.frame'))

#Permanova pairs with Nute data
tmp <- subset_samples(Myco_sub_phy, !is.na(Pair_Nute))
adonis2(otu_table(tmp) ~ Nute_PC1+ Severity + Interval, data=as(sample_data(tmp), 'data.frame'))

#Permanova pairs with veg data
tmp <- subset_samples(Myco_sub_phy, !is.na(Pair_Veg))
adonis2(otu_table(tmp) ~ Veg_PC1+ Severity + Interval, data=as(sample_data(tmp), 'data.frame'))


#Permanova no pairs just PC scores
tmp <- subset_samples(Myco_sub_phy, !is.na(All_PC2_S))
adonis2(otu_table(tmp) ~ All_PC1_S +All_PC2_S + Severity + Interval, data=as(sample_data(tmp), 'data.frame'))

#Permanova pairs with s all OTUs
adonis2(otu_table(sub_phy) ~ Severity + Interval , data=as(sample_data(sub_phy), 'data.frame'))

#Permanova pairs with just Ectos
adonis2(otu_table(Myco_sub_phy) ~ Severity + Interval , data=as(sample_data(Myco_sub_phy), 'data.frame'))

