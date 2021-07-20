###############Extended version of the data per individual and sample###################
library(dplyr)
library(readr)
library(reshape2)
library(car)
library(tidyr)
# data_zoo <- list.files("F:/Postdoc South Africa/Penguin/Data/Zooplancton/task_16073_export_823_20181121_1104",full.names = TRUE) %>%
#   lapply(read.table,header=T) %>%
#   bind_rows ##read and concatenate all the files
#split2(export_823_20210218_1301),split4(export_823_20210218_1155)
data_zoo2 <- list.files("export_823_20210224_1014",full.names = TRUE) %>%#
  lapply(read.table,header=T) %>%
  bind_rows ##read and concatenate all the files
#write.csv(data_zoo,"C:/Postdoc/Penguin/Data/Zooplancton/proof.csv")
####################subset the data with just the columns I will need####################
names(data_zoo2)
#sub_zoo<-data_zoo[,c(1:4,14,20,49,62,83,85,97,108,110,116,123,130,135)]
sub_zoo<-data_zoo2[,c(1:4,14,18,72,34,33,85,101,99,90,125,126,130,134,37)]#
#######add object_annotation_parent_category that is not present in the new version####################
#splits <- sapply(as.character(sub_zoo$object_annotation_category), function(x) strsplit(x,"[<]")[[1]], USE.NAMES=FALSE)#split object_annotation_category
#sub_zoo$object_annotation_category <-lapply(splits, `[[`, 1)
#sub_zoo$object_annotation_category <- splits[1,]#take the first column
#sub_zoo$object_annotation_category <- gsub('\\s+', '', sub_zoo$object_annotation_category)#eliminate the spaace
#sub_zoo$object_annotation_parent_category <- splits[2,]#take the second column
split<-strsplit(as.character(sub_zoo$object_annotation_category),'<')
Taxa_names<-do.call(rbind, split)
sub_zoo$object_annotation_category <-Taxa_names[,1]
sub_zoo$object_annotation_parent_category <-Taxa_names[,2]
sub_zoo<-sub_zoo %>%
  #dplyr::filter(!sample_id=='ab20180118_stn7_vwp2')%>%#eliminate the station 7 that was scanned 200um
  dplyr::filter(!sample_id=='ab20170930_stn3_vwp2')#eliminate the station 3 that had problems
##formation of groups data_zoo#####
sub_zoo<-sub_zoo %>% 
  mutate(object_annotation_category= car::recode(object_annotation_category, 
                                                 "c('cyphonaute')='Bryozoa';
                                                 c('Doliolida')='Tunicata';
                                                 c('Oithonidae')='Cyclopoida';
                                                 c('Oikopleuridae', 'Fritillariidae') = 'Appendicularia';
                                                 c('Penilia', 'Podon', 'Evadne') = 'Cladocera';
                                                 c('Hydrozoa', 'Aglaura','Obelia','Porpitidae','actinula','ephyra','gonophore','nectophore')='Cnidaria';
                                                 c('zoea','megalopa','cypris', 'metanauplii','nauplii','Axiidea', 'protozoea','Anomura','calyptopsis') = 'Larvae_crustacean';
                                                 c('Acartiidae', 'Candaciidae','Temoridae','Subeucalanidae','Calanidae','Centropagidae') = 'Calanoida';
                                                 c('Euterpina', 'Goniopsyllus') = 'Harpacticoida';
                                                 c('Corycaeidae', 'Oncaeidae') = 'Poecilostomatoida';
                                                 #c('Gammaridea', 'Ostracoda','Cumacea','Mysida') = 'Other_crustacean';
                                                 c('pluteus') = 'Echinodermata';
                                                 c('Acantharea','Coscinodiscus','Foraminifera','Neoceratium','Phaeodaria','Spumellaria','Spongodiscidae','Noctiluca') = 'Harosa';
                                                 c('Bivalvia', 'Cavoliniidae','Creseidae', 'Limacinidae') = 'Mollusca';
                                                 c('fiber','scale','seaweed') = 'detritus'"))
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='other'& sub_zoo$object_annotation_category=='part')] <- 'detritus'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Siphonophorae'& sub_zoo$object_annotation_category=='part')] <- 'detritus'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Annelida'& sub_zoo$object_annotation_category=='larvae')] <- 'Annelida'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Holothuroidea'& sub_zoo$object_annotation_category=='larvae')] <- 'Echinodermata'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Tunicata'& sub_zoo$object_annotation_category=='larvae')] <- 'Tunicata'#
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Porcellanidae'& sub_zoo$object_annotation_category=='larvae')] <- 'Larvae_crustacean'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Chaetognatha'& sub_zoo$object_annotation_category=='tail')] <- 'Chaetognatha'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Appendicularia'& sub_zoo$object_annotation_category=='tail')] <- 'Appendicularia'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Actinopterygii'& sub_zoo$object_annotation_category=='egg')] <- 'Egg_fish'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Mollusca'& sub_zoo$object_annotation_category=='egg')] <- 'Mollusca'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='egg'& sub_zoo$object_annotation_category=='like')] <- 'egg'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='other'& sub_zoo$object_annotation_category=='egg')] <- 'egg'
sub_zoo$object_annotation_category[which(sub_zoo$object_annotation_parent_category=='Nemertea'& sub_zoo$object_annotation_category=='pilidium')] <- 'Nemertea'
sort(unique(sub_zoo$object_annotation_category))
#account for detritus,artefact,multiple and badfocus
length(which(sub_zoo$object_annotation_category=='artefact'))*100/nrow(sub_zoo)#0.27% artefact
length(which(sub_zoo$object_annotation_category=='bubble'))*100/nrow(sub_zoo)#0.02% bubble
length(which(sub_zoo$object_annotation_category=='badfocus'))*100/nrow(sub_zoo)#0.85% bad focus
length(which(sub_zoo$object_annotation_category=='multiple'))*100/nrow(sub_zoo)#0.07% not identified
length(which(sub_zoo$object_annotation_category=='detritus'))*100/nrow(sub_zoo)#36.5% detritus
sub_zoo%>%filter(sub_zoo$object_annotation_category=='detritus')%>%count(sample_id, sort = TRUE)
#now account for dead copepods
length(which(sub_zoo$object_annotation_category=='dead'|sub_zoo$object_annotation_category=='part'))*100/nrow(sub_zoo)#1.55% dead copepods
sub_zoo%>%filter(sub_zoo$object_annotation_category=='dead'|sub_zoo$object_annotation_category=='part')%>%count(sample_id, sort = TRUE)
zoo_clean<-sub_zoo[-c(which(sub_zoo$object_annotation_category=='artefact')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='bubble')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='detritus')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='badfocus')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='multiple')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='dead'|zoo_clean$object_annotation_category=='part')),]
sort(unique(zoo_clean$object_annotation_category))#25 groups
#I decided to subtract fish as well. We can use the data of fish larvae. and fish are undersampled anyway
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='Actinopterygii')),]
unique(zoo_clean$object_annotation_category)#24 now
zoo_clean_orig<-zoo_clean # variable to retain the 24 groups
####WARNING THIS LINES ARE WRITTEN TO REDUCE THE GROUPS
names_ab<-c("Calanoida","Cyclopoida","Chaetognatha","Larvae_crustacean","Cnidaria","Appendicularia",
            "Harosa","Cladocera","Mollusca")
Other<-c("Ctenophora","egg","Echinodermata","Phoronida","Egg_fish",
         "Bryozoa","Tunicata","Annelida","Harpacticoida","Ostracoda","Poecilostomatoida",
         "Gammaridea","Mysida","Nemertea","Cumacea")
zoo_clean<-zoo_clean%>% mutate(object_annotation_category=case_when(object_annotation_category %in% Other ~ 'Other',
                           TRUE ~ as.character(object_annotation_category)))
unique(zoo_clean$object_annotation_category)#10 now
#zoo_clean<-zoo_clean[-c(26524,33484),]#STILL VALID FOR NBSS; I decided to exclude one individual of each net because drive the regression and were underrepresented
##############Step 1: Find the abundance (ind/m3)#################
zoo_clean$object_date<-strptime(zoo_clean$object_date,format=('%Y%m%d'))#date format
zoo_clean$sample_stationid<-as.factor(substr(zoo_clean$sample_stationid, 9, 9))#format stations
#zoo_clean$sample_tot_vol, is volume filtered
#zoo_clean$acq_sub_part, is volume split
########Plot Abundance per date, station and volume ESD (equivalent spherical density)########
library(ggplot2)
library(RColorBrewer)
conversion.Unit <- 0.0106#from pixels to mm
zoo_clean$object_feret_mm<-zoo_clean$object_feret*conversion.Unit
ord<-c("Other","Chaetognatha","Cnidaria","Appendicularia","Mollusca","Harosa",
       "Larvae_crustacean","Cladocera","Cyclopoida","Calanoida")
zoo_clean$object_annotation_category<- factor(zoo_clean$object_annotation_category,levels=ord)
zoo_clean%>%group_by(object_date,object_annotation_category)%>%
  ggplot(aes(x=as.factor(object_date), y=object_feret_mm,fill=object_annotation_category))+ 
  geom_violin(trim=TRUE)+theme_bw()+  theme(panel.spacing=unit(0, "lines"))+
  #stat_summary(fun=mean, geom="point", size=1, color="red")+
  scale_fill_manual(values=colorRampPalette(brewer.pal(11,"Spectral"))(10))+
  facet_grid(rows = vars(object_annotation_category),scales='free',switch='y')+
  theme(strip.text.y = element_text(size = 7, angle = 90))+
  theme(axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 7),
    axis.title.y = element_text(size = 10))+xlab("Sampling events")+ylab("Feret (mm)")
ggsave('Feret_taxa.pdf',width=7,height=6)

zoo_clean%>%group_by(object_date,object_annotation_category)%>%
  ggplot(aes(y=object_annotation_category, x=object_feret_mm,fill=object_annotation_category))+ 
  geom_violin(trim=TRUE)+theme_bw()+  theme(panel.spacing=unit(0, "lines"))+
  #stat_summary(fun=mean, geom="point", size=1, color="red")+
  scale_fill_manual(values=colorRampPalette(brewer.pal(11,"Spectral"))(10))+
  facet_grid(rows=vars(as.factor(object_date)),scales='free',switch='y')
ggsave('Feret_month.pdf',width=17,height=16)

#table abundance per feret
abundance_table_feret<-zoo_clean%>%mutate(breaks_feret=cut(object_feret_mm, 
                                                       breaks = c(0.2,0.5,1,2,18),
                                                       right=FALSE,labels = c('0.2-0.5mm',
                                                                              '0.5-1mm',
                                                                              '1-2mm','>2mm')))%>%
  group_by(object_date,sample_stationid,breaks_feret)%>%dplyr::summarise(abundance_m3=(length(breaks_feret)*unique(acq_sub_part))/unique(sample_tot_vol))
abundance_table_feret$breaks_feret<-factor(abundance_table_feret$breaks_feret,levels=c('>2mm','1-2mm','0.5-1mm','0.2-0.5mm'))
abundance_table_feret$object_date<-as.factor(abundance_table_feret$object_date)
levels(abundance_table_feret$object_date) <- c("09-2017","10-2017","12-2017","01-2018","03-2018","05-2018","06-2018","07-2018")
ggplot(data=abundance_table_feret,aes(fill=breaks_feret, y=abundance_m3, x=sample_stationid))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_grey() + labs(x = "Stations",fill = "Feret",y=expression("Abundance (ind. m"^{"-3"}~")"))+
  facet_wrap(~object_date,scales = "fixed",ncol=4)+theme_bw()
ggsave('Abundance.pdf',width=8,height=4)
#averages abundances total from data discretized by months and stations
step1<-abundance_table_feret%>%group_by(object_date,sample_stationid)%>%summarise(Sum_st=sum(abundance_m3))
mean(step1$Sum_st);sd(step1$Sum_st)
########work with 2 loops, first date and second station to find the abundance per taxa, station and date########
density_table<-zoo_clean%>%mutate(object_annotation_category=recode_factor(object_annotation_category,nauplii='Nauplii-Zoea',zoea='Nauplii-Zoea'))%>%
  group_by(object_date,sample_stationid,object_annotation_category)%>%
  dplyr::summarise(abundance_m3=(length(object_annotation_category)*unique(acq_sub_part))/unique(sample_tot_vol))
###################Step 2: Relative abundance###########################################
library(ggplot2)
library(RColorBrewer)
ord<-c("Other","Chaetognatha","Cnidaria","Appendicularia","Mollusca","Harosa",
       "Larvae_crustacean","Cladocera","Cyclopoida","Calanoida")
density_table$object_annotation_category<- factor(density_table$object_annotation_category,levels=ord)
density_table$object_date<- as.factor(density_table$object_date)
levels(density_table$object_date) <- c("09-2017","10-2017","12-2017","01-2018","03-2018","05-2018","06-2018","07-2018")
#Plot relative density per taxa
density_table%>%group_by(object_date,sample_stationid)%>%
ggplot()+geom_bar(mapping=aes(x=sample_stationid,y=abundance_m3,fill=object_annotation_category),
                  position="fill", stat="identity")+
  scale_fill_manual(values=colorRampPalette(brewer.pal(11,"Spectral"))(10))+
  facet_wrap(~object_date,ncol=4)+ labs(x = "Stations",fill = "Taxa",
                              y=expression("Relative Abundance"))+theme_bw()+theme(legend.position = 'bottom')
ggsave('Relative abundance.pdf',width=8,height=4)
density_table%>%
  group_by(object_date,sample_stationid) %>%
  mutate(frequency = abundance_m3 / sum(abundance_m3))%>%group_by(object_date,object_annotation_category)%>%
  summarise(mean (frequency), sd(frequency))
############################Step 3: Biovolume####################################
# library(ggplot2);library(plyr);library(dplyr);library(latex2exp);library(RColorBrewer)
# library(data.table);library(scales);library(tidyr);library(latex2exp);library(colorRamps)
# library(ggforce);library(FSA)
################################################################
##### Function for estimation volume of each organism (mm3)#######
Conversion <-function(major, minor){			
  conversion.Unit <- 0.0106#from pixels to mm
  return((4/3)*pi*((major*conversion.Unit)/2)*(((minor*conversion.Unit)/2)^2))#
}
################################################################
##### 1. To calculate volume per specimen (mm3)################
zoo_clean$volume <- Conversion(zoo_clean$object_major, zoo_clean$object_minor) 
Res <- data.frame(Net =zoo_clean$sample_id,#data together
                  Station = zoo_clean$sample_stationid,
                  Taxa = zoo_clean$object_annotation_category, 
                  Taxa.parent =  zoo_clean$object_annotation_parent_category,
                  Fraction =  zoo_clean$acq_sub_part,
                  Filt.vol =  zoo_clean$sample_tot_vol,
                  volume =  zoo_clean$volume,
                  date=zoo_clean$object_date,
                  #object_esd_mm=zoo_clean$object_esd_mm,
                  object_feret_mm=zoo_clean$object_feret_mm)#the x axis is generated from volume (mm-3) and y is generated from intervals of biovolum which is indoviduals per m3 (m3)
zoo_clean%>%group_by(object_date,object_annotation_category)%>%
  ggplot(aes(y=object_annotation_category, x=log2(volume),fill=object_annotation_category))+ 
  geom_violin(trim=TRUE)+theme_bw()+  theme(panel.spacing=unit(0, "lines"),legend.position="bottom")+
  scale_fill_manual(values=colorRampPalette(brewer.pal(11,"Spectral"))(10))+
  facet_wrap(~as.factor(object_date),nrow=2)+scale_x_continuous(limits = c(-8, 8),breaks=seq(-8,8,2))#
ggsave('Volume_month.pdf',width=15,height=5)
# ######calculate biovolume of each particules in the sample (mm3 ind-1)###################
#Res$biovolume<-(Res$Fraction*Res$volume)/Res$Filt.vol#Garcia_comas2014
#Res1<-data.frame(Res$date,as.integer(Res$Station),Res$Taxa,Res$volume,Res$object_esd_mm)
#names(Res1)<-c('date','Station','Taxa','volume','object_esd_mm')
########Plot Biovolume per date, station and volume ESD (equivalent spherical density)########
biovolume_table_feret<-Res%>%mutate(breaks_feret=cut(object_feret_mm,breaks = c(0.2,0.5,1,2,18),
                             right=FALSE,labels = c('0.2-0.5mm','0.5-1mm',
                                                    '1-2mm','>2mm')))%>%
  group_by(date,Station,breaks_feret)%>%dplyr::summarise(biovolume_feret=(sum(volume)*unique(Fraction))/unique(Filt.vol))
biovolume_table_feret$breaks_feret<-factor(biovolume_table_feret$breaks_feret,
                                          levels=c('>2mm','1-2mm','0.5-1mm','0.2-0.5mm'))
biovolume_table_feret$Station<-as.factor(biovolume_table_feret$Station)
ggplot(data=biovolume_table_feret,aes(fill=breaks_feret, y=biovolume_feret, x=Station))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_grey() + labs(x = "Stations",fill = "ESD",y=expression("Biovolume (mm"^{"3"}~"m"^{"-3"}~")"))+
  facet_wrap(~date,scales = "fixed",ncol=4)+theme_bw()
ggsave('Biovolume.pdf',height=4,width=8)
#averages biovolumes total from data discretized by months and stations
step2<-biovolume_table_feret%>%group_by(date,Station)%>%summarise(Sum_st=sum(biovolume_feret))
  mean(step2$Sum_st);sd(step2$Sum_st)
##barplot biovolume per taxa
biovolume_table<-Res%>%mutate(Taxa=recode_factor(Taxa,nauplii='Nauplii-Zoea',zoea='Nauplii-Zoea'))%>%
  mutate(date=as.factor(date))%>%group_by(date,Station,Taxa)%>%
  dplyr::summarise(biovolume_m3=sum(volume))
biovolume_table$Taxa<- factor(biovolume_table$Taxa,levels=ord)
#Plot relative biovolume per taxa
levels(biovolume_table$date) <- c("09-2017","10-2017","12-2017","01-2018","03-2018","05-2018","06-2018","07-2018")
biovolume_table%>%group_by(date,Station)%>%
  ggplot()+geom_bar(mapping=aes(x=Station,y=biovolume_m3,fill=Taxa),
                    position="fill", stat="identity")+
  scale_fill_manual(values=colorRampPalette(brewer.pal(11,"Spectral"))(10))+
  facet_wrap(~date,ncol=4)+ labs(x = "Stations",fill = "Taxa",
                                 y=expression("Relative biovolume"))+
  theme_bw()
ggsave('Relative biovolume.pdf',width=8,height=4)
biovolume_table%>%
  group_by(date,Station) %>%
  mutate(frequency = biovolume_m3 / sum(biovolume_m3))%>%group_by(date,Taxa)%>%
  summarise(mean (frequency), sd(frequency))
#######################################################################
############Step 4:NBSS per month#########################################
#######ATTENTION: NAMES OF GROUPS DO NOT AFFECT THE CALCUL OF NBSS AND SIZE DIVERSITY####
# perfect sphere diameter as minimum limitate for the ESD, here it is 200micron
Perfect_sphere_limit <- 0.200 #200um~0.2mm is the radio
minimum_limit_size <- (4*pi*(Perfect_sphere_limit/2)^3)/3#the ESD in volume, to determine the minimum volume for an ESD 200micron
Res<-Res%>%dplyr::filter(volume>minimum_limit_size)#check that no particule is lower than minimum_limit_size
min_bin_size<-floor(min(log2(Res$volume)))#minimum size for the classes
max_bin_size<-ceiling(max(log2(Res$volume)))#maximum size for the classes
#Res$Biovolume<-(Res$volume*Res$Fraction)/Res$Filt.vol#biovolume, it can be done after separating by classes but I prefer todo here
df_naito<-Res%>% mutate(Class_size=cut(log2(volume),seq(min_bin_size,max_bin_size,1),right=FALSE))#binning by classes
df_naito$interv<-as.numeric(as.character(cut(log2(df_naito$volume), seq(min_bin_size,max_bin_size,1),
                                    labels=seq(min_bin_size+0.5,max_bin_size,1),right=TRUE)))#label of interval for every particle
table_biom<-df_naito%>%group_by(Net,interv)%>%mutate(Width_size=2^(interv+0.5)-(2^(interv-0.5)))%>%
  summarize(NBSS=log2(((sum(volume)*unique(Fraction))/unique(Filt.vol))/unique(Width_size)),Date=unique(date))%>%
  mutate(Month=as.integer(format(Date,'%m')),Season=case_when(Month==9|Month==10~'Spring',
  Month==12|Month==1~'Summer',Month==3|Month==5~'Autumn',
  Month==6|Month==7~'Winter'))
#Plot NBSS by season
ord_season<-c('Spring','Summer','Autumn','Winter')
table_biom$Season<- factor(table_biom$Season,levels=ord_season)
#Recode months
table_biom<-table_biom%>%mutate(Month=as.factor(month.name[Month]))
#Exclude
find_mode<-table_biom%>%group_by(interv)%>%summarise(NBSSm=mean(NBSS))
inter_mode<-find_mode$interv[which.max(find_mode$NBSSm)]
table_biom<-table_biom%>%mutate(Factor_exclude=cut(interv,breaks=c(-8,-6.5,7.5),labels=c('Exclude','Include')))
library(ggpmisc)
#Plot by season and month
table_biom%>%ggplot(aes(x=interv,y=NBSS))+geom_point(aes(colour=as.factor(Month)))+
  geom_smooth(method="lm",data=table_biom%>%
                dplyr::filter(!Factor_exclude %in% 'Exclude'),aes(group=as.factor(Month),
                                                           colour=as.factor(Month)),formula=y ~ x)+
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),group=as.factor(Month),colour=as.factor(Month)), 
               data=table_biom%>%dplyr::filter(!Factor_exclude %in% 'Exclude'),
               label.x.npc = "right", label.y.npc = "top",
               formula = y ~ x, parse = TRUE, size = 2)+
  facet_wrap(~Season)+theme_bw()
ggsave('NBSS_season.pdf',width=8,height=5)

ptable<-table_biom%>%mutate(Station=as.factor(substring(Net,15,15)))
station_order<-as.character(c(1:8))
ptable$Station<-factor(ptable$Station,levels =station_order)
ptable%>%ggplot(aes(x=interv,y=NBSS))+
  geom_point(aes(colour=as.factor(Station)))+
  scale_color_brewer(palette = "RdBu")+
  geom_smooth(method="lm",data=table_biom%>%
                dplyr::filter(!Factor_exclude %in% 'Exclude'),aes(group=as.factor(Month)),formula=y ~ x)+
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),group=as.factor(Month)), 
               data=table_biom%>%dplyr::filter(!Factor_exclude %in% 'Exclude'),
               label.x.npc = "right", label.y.npc = "top",
               formula = y ~ x, parse = TRUE, size = 2)+
  facet_wrap(~Month)+theme_bw()
ptable%>%ggplot(aes(x=interv,y=NBSS))+
  geom_point(aes(colour=as.factor(Station)))+
  scale_color_brewer(palette = "RdBu")+ guides(color=guide_legend("Stations"))+
  geom_smooth(method="lm",se = FALSE, size=0.7,data=ptable%>%
                dplyr::filter(!Factor_exclude %in% 'Exclude'),aes(group=as.factor(Station),
                 colour=as.factor(Station)),formula=y ~ x)+
  facet_wrap(~Month,nrow=2,labeller = labeller(Month=c('September'="09-2017",
  'October'="10-2017",'December'="12-2017",'January'="01-2018",'March'="03-2018",
  'May'="05-2018",'June'="06-2018",'July'="07-2018")))+
  theme_bw()+theme(panel.spacing=unit(0, "lines"),legend.position = 'bottom')+scale_x_continuous(limits = c(-8, 8),breaks=seq(-8,8,2))
ggsave('NBSS_month_station.pdf',width=10,height=5)

#Plot NBSS by Net with the slopes, r2 and
table_biom%>%ggplot(aes(x=interv,y=NBSS))+geom_point(aes(colour=as.factor(Factor_exclude)))+
geom_smooth(method="lm",col="red",data=table_biom%>%
              dplyr::filter(!Factor_exclude %in% 'Exclude'),formula=y ~ x)+
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               data=table_biom%>%dplyr::filter(!Factor_exclude %in% 'Exclude'),
               label.x.npc = "right", label.y.npc = "top",
               formula = y ~ x, parse = TRUE, size = 4)+theme(legend.position = "none")+
  facet_wrap(~Net)
ggsave('NBSS_net.pdf',width=17,height=10)
####Results for the glmm###########
table_R2<-table_biom%>%dplyr::filter(Factor_exclude=='Include')%>%group_by(Net)%>%
  do(fitNBSS = broom::glance(lm(NBSS ~ interv, data = .)))%>% 
  unnest(fitNBSS)%>%select(Net,r.squared,adj.r.squared,p.value)
table_param<-table_biom%>%dplyr::filter(Factor_exclude=='Include')%>%group_by(Net)%>%
  do(fitNBSS = broom::tidy(lm(NBSS ~ interv, data = .))) %>% 
  unnest(fitNBSS)%>%select(-std.error,-statistic,-p.value)%>%
  pivot_wider(names_from = term,values_from=estimate)
table_NBSS<-left_join(table_R2,table_param,by='Net')
table_NBSS<-table_NBSS%>%mutate(Station=as.factor(substring(Net,15,15)),
                   Month=as.factor(substring(Net,7,8)))
averages_NBSS<-table_NBSS%>%mutate(Station=as.factor(substring(Net,15,15)),
                    Month=as.factor(substring(Net,7,8)))%>%group_by(Month)%>%
  summarise_at(vars(r.squared,p.value,`(Intercept)`,interv),list(mean,sd))
#######Diversity size kernel#####
#Run first all the functions in SizeDiversity_2018.R
#Source:https://limnolam.org/the-measurement-of-size-diversity/
div.data<-df_naito%>%select(Net,volume)%>%mutate(abundance=rep(1,nrow(df_naito)))
noms=unique(div.data$Net)
res1=matrix(NA,length(noms),7)
#Function
for (i in 1:(length(noms))){
  res1[i,]=sizeDiversity2017(x=div.data[div.data$Net==noms[i],]$volume,
                             abund=div.data[div.data$Net==noms[i],]$abund,measuredim=3,gnormaliz=1)}
rownames(res1)=noms
colnames(res1)=c("diversity2","eevenness", "logNdiversity2","bandker", "devxlog","gmeanx", "meanx")
res2<-data.frame(res1)
res3<-res2%>%mutate(Station=as.factor(substring(noms,15,15)),
              Month=as.factor(substring(table_NBSS$Net,7,8)),Net=noms)
boxplot(diversity2~Month,data=res3)
mean(res3$diversity2);sd(res3$diversity2)
averages_sizes<-res3%>%group_by(Month)%>%
  summarise(mean(diversity2),sd(diversity2))
step2$date<-as.factor(as.character(step2$date))
ab_biov<-left_join(step1,step2,by = c("object_date" = "date", "sample_stationid" = "Station"))
ab_biov$Month<-as.factor(substring(ab_biov$object_date,6,7))
table4models<-left_join(table_NBSS,res3,by='Net')
table4models<-left_join(table4models,ab_biov,by=c("Month.x" = "Month", "Station.x" = "sample_stationid"))
table4models<-table4models%>%transmute(Net,station=Station.x,month=Month.x,r2=r.squared,
                                       intercept=`(Intercept)`,slope=interv,size_div=diversity2,
                                       eevenness,abundance=Sum_st.x,biovolume=Sum_st.y)
#######Taxonomic diversity#################################################
data_zoo2 <- list.files("export_823_20210224_1014",full.names = TRUE) %>%#
  lapply(read.table,header=T) %>%
  bind_rows ##read and concatenate all the files
####################Subset groups for DIVERSITY####################
names(data_zoo2)
sub_zoo<-data_zoo2[,c(1:4,14,18,72,34,33,85,101,99,90,125,126,130,134)]
#######add object_annotation_parent_category that is not present in the new version####################
split<-strsplit(as.character(sub_zoo$object_annotation_category),'<')
Taxa_names<-do.call(rbind, split)
sub_zoo$object_annotation_category <-Taxa_names[,1]
sub_zoo$object_annotation_parent_category <-Taxa_names[,2]
sub_zoo<-sub_zoo %>%
  dplyr::filter(!sample_id=='ab20170930_stn3_vwp2')#eliminate the station 3 that had problems
#now clean
zoo_clean<-sub_zoo[-c(which(sub_zoo$object_annotation_category=='artefact')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='bubble')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='detritus')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='badfocus')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='multiple')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='dead'|zoo_clean$object_annotation_category=='part')),]
sort(unique(zoo_clean$object_annotation_category))
##formation of groups data_zoo#####
zoo_clean<-zoo_clean %>% 
  mutate(object_annotation_category= car::recode(object_annotation_category, 
                                                 "c('cyphonaute')='Bryozoa';
                                                 c('Doliolida')='Tunicata';
                                                 c('Cladocera') = 'Podon';
                                                 c('Oikopleuridae', 'Fritillariidae') = 'Appendicularia';
                                                 c('Hydrozoa', 'Aglaura','Obelia','Porpitidae','actinula','ephyra','gonophore','nectophore')='Cnidaria';
                                                 c('zoea','megalopa','protozoea','Anomura') = 'zoea';
                                                 c('metanauplii','nauplii') = 'nauplii';
                                                 c('Euterpina', 'Goniopsyllus') = 'Harpacticoida';
                                                 c('Acantharea','Coscinodiscus','Foraminifera','Neoceratium','Phaeodaria','Spumellaria','Spongodiscidae','Noctiluca') = 'Harosa';
                                                 c('Cavoliniidae','Creseidae', 'Limacinidae') = 'Gastropoda';
                                                 c('fiber','scale','seaweed') = 'detritus'"))
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='Siphonophorae'& zoo_clean$object_annotation_category=='part')] <- 'detritus'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='Annelida'& zoo_clean$object_annotation_category=='larvae')] <- 'Annelida'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='Holothuroidea'& zoo_clean$object_annotation_category=='larvae')] <- 'Echinodermata'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='Echinoidea'& zoo_clean$object_annotation_category=='pluteus')] <- 'Echinodermata'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='Ophiuroidea'& zoo_clean$object_annotation_category=='pluteus')] <- 'Echinodermata'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='Tunicata'& zoo_clean$object_annotation_category=='larvae')] <- 'Tunicata'#
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='Porcellanidae'& zoo_clean$object_annotation_category=='larvae')] <- 'zoea'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='Chaetognatha'& zoo_clean$object_annotation_category=='tail')] <- 'Chaetognatha'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='Appendicularia'& zoo_clean$object_annotation_category=='tail')] <- 'Appendicularia'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='Actinopterygii'& zoo_clean$object_annotation_category=='egg')] <- 'Egg_fish'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='Mollusca'& zoo_clean$object_annotation_category=='egg')] <- 'egg'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='egg'& zoo_clean$object_annotation_category=='like')] <- 'egg'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='other'& zoo_clean$object_annotation_category=='egg')] <- 'egg'
zoo_clean$object_annotation_category[which(zoo_clean$object_annotation_parent_category=='Nemertea'& zoo_clean$object_annotation_category=='pilidium')] <- 'Nemertea'
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='detritus')),]
zoo_clean<-zoo_clean[-c(which(zoo_clean$object_annotation_category=='Actinopterygii')),]
sort(unique(zoo_clean$object_annotation_category))#42 now
###Abundance per taxa
density_table_div<-zoo_clean%>%
  group_by(object_date,sample_stationid,object_annotation_category)%>%
  dplyr::summarise(abundance_m3=round((length(object_annotation_category)*unique(acq_sub_part))/unique(sample_tot_vol)))
table_div<-density_table_div%>%pivot_wider(names_from='object_annotation_category',values_from='abundance_m3')
percent_table<-table_div %>%ungroup()%>%
  summarise(across(where(is.double),~round((sum(!is.na(.x))/62)*100,0)))
View(sort(percent_table,decreasing=TRUE))
write.csv(percent_table,'percent_table.csv')
CV <- function(x){(sd(x,na.rm = TRUE)/mean(x,na.rm = TRUE))*100}
CV_table<-table_div %>%ungroup()%>%
  summarise(across(where(is.double),~CV(.x)))
write.csv(CV_table,'CV_table.csv')
table_div2<-table_div%>%ungroup()%>%replace(is.na(.),0)%>%select(!(object_date:sample_stationid))
library(vegan)
rare<-c('Cumacea','Nemertea','Gammaridea','Mysida','calyptopsis',
        'Ostracoda','Phoronida','Axiidea','Candaciidae')
#H<-diversity(table_div2%>%select(!rare),index='shannon',base=2)#diversity without rare
H<-diversity(table_div2,index='shannon')#diversity with rare
tableH<-table_div%>%select(object_date:sample_stationid)%>%mutate_all(unique)
tableH$H<-H
tableH<-tableH%>%mutate(month=as.factor(substring(object_date,5,6)),
                station=as.factor(substring(sample_stationid,9,9)))%>%ungroup%>%
  select(!(object_date:sample_stationid))
table4modelsf<-left_join(table4models,tableH,by=c("month", "station"))
write.csv(table4modelsf,'index_zoo.csv')
