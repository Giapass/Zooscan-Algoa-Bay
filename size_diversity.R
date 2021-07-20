library(vegan)
library(tidyr)
library(tidyselect)

## _____________Size diversity_____________
library(ggplot2);library(plyr);library(dplyr);library(latex2exp);library(RColorBrewer)
library(data.table);library(scales);library(tidyr);library(latex2exp);library(colorRamps)
library(ggforce);library(FSA)
################################################################
###############Extended version of the data per individual and sample###################
library(dplyr)
library(readr)
library(reshape2)
data_zoo <- list.files("C:/Postdoc/Penguin/Data/Zooplancton/task_16073_export_823_20181121_1104",full.names = TRUE) %>% 
  lapply(read.table,header=T) %>% 
  bind_rows ##read and concatenate all the files
#write.csv(data_zoo,"C:/Postdoc/Penguin/Data/Zooplancton/proof.csv")
####################subset the data with just the columns I will need####################
names(data_zoo)
sub_zoo<-data_zoo[,c(1:4,14,62,83,85,97,108,110,116,123,130,135)]
#######add object_annotation_parent_category that is not present in the new version####################
splits <- sapply(sub_zoo$object_annotation_category, function(x) strsplit(x, "[()]")[[1]], USE.NAMES=FALSE)#split object_annotation_category
sub_zoo$object_annotation_category <- splits[1,]#take the first column
sub_zoo$object_annotation_category <- gsub('\\s+', '', sub_zoo$object_annotation_category)#eliminate the spaace
sub_zoo$object_annotation_parent_category <- splits[2,]#take the second column
##formation of groups data_zoo#####
sub_zoo<-sub_zoo %>% 
  mutate(object_annotation_category= car::recode(object_annotation_category, 
                                                 "c('cyphonaute')='Bryozoa';
                                                 c('Doliolida')='Thaliacea';
                                                 c('Oikopleuridae', 'Fritillariidae') = 'Appendicularia';
                                                 c('Hydrozoa', 'Aglaura','Obelia','Porpitidae','actinula','ephyra','gonophore','nectophore')='Cnidaria';
                                                 c('cypris', 'metanauplii') = 'nauplii';
                                                 c('Acartiidae', 'Candaciidae') = 'Calanoida';
                                                 c('Euterpina', 'Goniopsyllus') = 'Harpacticoida';
                                                 c('Corycaeidae', 'Oncaeidae') = 'Poecilostomatoida';
                                                 c('Axiidea', 'protozoea','Anomura') = 'zoea';
                                                 c('Gammaridea', 'Ostracoda','Cumacea','calyptopsis','Mysida','megalopa') = 'othercrustacea';
                                                 c('pluteus') = 'Echinodermata';
                                                 c('Acantharea','Coscinodiscus','Foraminifera','Neoceratium','Phaeodaria','Spumellaria','Spongodiscidae') = 'Harosa';
                                                 c('Bivalvia', 'Cavoliniidae','Creseidae', 'Limacinidae') = 'Mollusca';
                                                 c('fiber','scale','part','seaweed','dead') = 'detritus'"))
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Annelida'& sub_zoo$object_annotation_category=='larvae')] <- 'Annelida'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Holothuroidea'& sub_zoo$object_annotation_category=='larvae')] <- 'Echinodermata'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Tunicata'& sub_zoo$object_annotation_category=='larvae')] <- 'Tunicata'#or Doliolida
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Porcellanidae'& sub_zoo$object_annotation_category=='larvae')] <- 'zoea'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Chaetognatha'& sub_zoo$object_annotation_category=='tail')] <- 'Chaetognatha'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Appendicularia'& sub_zoo$object_annotation_category=='tail')] <- 'Appendicularia'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Actinopterygii'& sub_zoo$object_annotation_category=='egg')] <- 'eggfish'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Mollusca'& sub_zoo$object_annotation_category=='egg')] <- 'Mollusca'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='egg'& sub_zoo$object_annotation_category=='like')] <- 'egg'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='other'& sub_zoo$object_annotation_category=='egg')] <- 'egg'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Nemertea'& sub_zoo$object_annotation_category=='pilidium')] <- 'Nemertea'
unique(sub_zoo$object_annotation_category)
#account for detritus, othertocheck,artefact,multiple and badfocus
length(which(sub_zoo$object_annotation_category=='artefact'))*100/nrow(sub_zoo)#0.27% artefact
length(which(sub_zoo$object_annotation_category=='bubble'))*100/nrow(sub_zoo)#0.02% bubble
length(which(sub_zoo$object_annotation_category=='detritus'))*100/nrow(sub_zoo)#37.2% detritus
zoo_clean<-sub_zoo[-c(which(sub_zoo$object_annotation_category=='artefact')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='bubble')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='detritus')),]
#from clean data
length(which(zoo_clean$object_annotation_category=='badfocus'))*100/nrow(zoo_clean)#1.06% bad focus
length(which(zoo_clean$object_annotation_category=='multiple'))*100/nrow(zoo_clean)#0.12% not identified
length(which(zoo_clean$object_annotation_category=='othertocheck'))*100/nrow(zoo_clean)#0.51% not identified
#now we substract the artefact, detritus,multiple and badfocus, and assign other for otherto check
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='badfocus')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='multiple')),]
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='othertocheck')] <- 'other'
unique(zoo_clean$object_annotation_category)#33 now
zoo_clean_orig<-zoo_clean # variable to retain the 33 groups
####WARNING THIS LINES ARE WRITTEN TO REDUCE THE GROUPS
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Thaliacea')] <- 'other'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Ctenophora')] <- 'other'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='othercrustacea')] <- 'other'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='eggfish')] <- 'other'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Bryozoa')] <- 'other'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Harpacticoida')] <- 'other'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Actinopterygii')] <- 'other'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Annelida')] <- 'other'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Tunicata')] <- 'other'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Podon')] <- 'Cladocera'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Evadne')] <- 'Cladocera'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Penilia')] <- 'Cladocera'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Noctiluca')] <- 'Harosa'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Calanidae')] <- 'Calanoida'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Centropagidae')] <- 'Calanoida'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Subeucalanidae')] <- 'Calanoida'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Temoridae')] <- 'Calanoida'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='egg')] <- 'other'#optional to have just 13 taxa
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Phoronida')] <- 'other'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_category=='Nemertea')] <- 'other'
unique(zoo_clean$object_annotation_category)#13 now
zoo_clean<-zoo_clean[-c(26524,33484),]#I decided to exclude one individual of each net because drive the regression and were underrepresented
################################################################
##### Function for estimation volume of each organism (mm3)#######
Conversion <-function(major, minor){			
  conversion.Unit <- 0.0105833#from pixels to mm
  return((4/3)*pi*((major*conversion.Unit)/2)*((minor*conversion.Unit)/2)^2)#
}

##### 1. To calculate volume per specimen (mm3)################
zoo_clean$volume <- Conversion(zoo_clean$object_major, zoo_clean$object_minor) 
Res <- data.frame(Net =zoo_clean$sample_id,#data together
                  Station = zoo_clean$sample_stationid,
                  Taxa = zoo_clean$object_annotation_category, 
                  Taxa.parent =  zoo_clean$object_annotation_parent_category,
                  Fraction =  zoo_clean$acq_sub_part,
                  Filt.vol =  zoo_clean$sample_tot_vol,
                  volume =  zoo_clean$volume)#the x axis is generated from volume (mm-3) and y is generated from intervals of biovolum which is indoviduals per m3 (m3)
# ######calculate biovolume of each particules in the sample (mm3 ind-1)###################
Res$biovolume<-(Res$Fraction*Res$volume)/Res$Filt.vol#Garcia_comas2014
size_taxa<-aggregate(Res$volume, by=list(Res$Taxa),sd)#arreglar esta linea para calcular el tamano de cada taxa
## build bin size from volume per particles (mm3) and to sequence bin size############
##### To built bin size class create the x-axes not interfering with the data
Bin.Size <- function(BS){
  Normalized_biovolum <- log2(BS$volume)# To normalise volume (mm3) of each particles
  # perfect sphere diameter as minimum limitate for the ESD, here it is 200micron
  Perfect_sphere_limit <- 0.200 #200um~0.2mm is the radio
  # To determine the minimum volume for an ESD 200micron
  minimum_limit_size <- (4*pi*(Perfect_sphere_limit/2)^3)/3#the ESD in volume
  # To define the minimum bin size
  min_bin_size <- floor(log2(minimum_limit_size))#log of the minimum bin size based in theoretical ESD
  # To define the maximum bin size biggest ESD
  max_bin_size <- ceiling(max(Normalized_biovolum))#it will be already log data but not normalized, is the max of my data
  ## Sequencing the bin_size according to the organism volume
  cbind(BS,bin_size=as.numeric(as.character(cut(Normalized_biovolum, seq(min_bin_size,max_bin_size,1),labels=seq(min_bin_size+0.5,max_bin_size-0.5,1),
                                                right=TRUE))))#make the sequence from min to max at step of 1 in log data, finally solved the labels
  #round(Normalized_biovolum)
}
################################################################
## 1. load matrix which was used to build the NBSS
Bin_Size.Data <- data.frame(Bin.Size(Res),log2(Res$volume))#add column of intervals and log2biovolumes
names(Bin_Size.Data)[10]<-"log2vol"#not useful

## 2. Getting the Frequence in each sample per size class
LP.sum.sc <- Bin_Size.Data %>%
  dplyr::group_by(Net,bin_size)%>%dplyr::summarise(n=n())#Now I have number of individual per bin size class in each station

## 3. Reshape df from long to wide format, it means make a 'species table' to be used in shanon index
LP.sum.sc <- spread(LP.sum.sc, bin_size, n, fill = 0)
#Now I have to exclude the bin size smaller than the mode >=-5.5
LP.sum.sc <- LP.sum.sc[,-c(2,3)]
## 4. Calculation the shannon index per station
LP.sc.diversity <-data.frame(station = LP.sum.sc$Net,
                   shannon_size.diversity = diversity(LP.sum.sc[,-1]))
# # ## To standartize indices by the log of the size class maximum possible value
N_2 <- ncol(LP.sum.sc[,c(-1)])#number of size class which will be 13, -1 because does not take the net names
LP.sc.diversity$Std.Shan.size <- LP.sc.diversity$shannon_size.diversity/log(N_2)
#write.csv(LP.sc.diversity,"C:/Postdoc/Penguin/Data/Zooplancton/size_diversity.csv")
f_anovad<-data.frame(LP.sc.diversity$Std.Shan.size, substr(LP.sc.diversity$station,7,8),
                     substr(LP.sc.diversity$station,15,15))
names(f_anovad)<-c('diversity','months','stations')
summary(aov(diversity~months,f_anovad))
summary(aov(diversity~stations,f_anovad))