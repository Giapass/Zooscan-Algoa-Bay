library(purrr)
library(dplyr)
library(PerformanceAnalytics)
#library(vegan)
#library(broom)
library(car)
#library(mgcv)
library(randomForest)
library(pdp)  # for partial, plotPartial, and grid.arrange functions
library(ggplot2)
library(tidyr)
library(patchwork)
##Models zooplankton structure parameters vs environmental variables
#CHL-a
cht<-read.csv('./Chlorophyll.csv',sep=';')
cht<-cht%>%transmute(Station=Station,Depth=factor(Level,labels=c('Surface','Medium','Bottom')),
                           Month=as.factor(Month), Variables='Chlorophyll-a',Values=MeanChl_a)%>%
  filter(!Values>50)

#Temperature, DO and Turbulence
physic<-read.csv('./Oceano_plankton1718.csv',sep=';')
physic<-physic%>%transmute(Station=Station,Depth=factor(Level,labels=c('Surface','Medium','Bottom')),
                Month=as.factor(Month),Temperature,Oxygen,Turbidity=Ntu)%>%
  pivot_longer(Temperature:Turbidity,names_to='Variables',values_to='Values')
#Nutrients
envirot<-read.csv('./allRawNutrients.csv',sep=';')
envirot<-envirot%>%dplyr::select(-DIN,-Nitrite)%>%
  pivot_longer(Phosphate:Ammonium,names_to='Variables',values_to='Values')%>%
  mutate(Month=as.factor(Month))%>%group_by(Station,Depth,Month,Variables)%>%
  summarize(Values=mean(Values,na.rm=TRUE))
all_env<-rbind(physic,cht,envirot)
all_env%>%mutate(Month=factor(Month,level=c(9,10,12,1,3,5,6,7)),
                 Variables=factor(Variables,level=c('Temperature','Turbidity','Oxygen',
                                                    'Nitrate','Ammonium','Silicate',
                                                    'Phosphate','Chlorophyll-a')))%>%
  ggplot(aes(x=Month, y=Values)) + 
  geom_violin(trim= TRUE)+theme_bw()+ theme(panel.spacing=unit(0, "lines"))+
  geom_dotplot(binaxis = "y",method='dotdensity',stackdir = "center",stackratio=0.5,dotsize = 2,
               aes(colour=Depth,fill=Depth, alpha=0.01)) +
  facet_grid(rows = vars(Variables),scales='free',switch='y')+theme(legend.position="top")

ggsave('enviro_paper.pdf',width = 6,height =8)


all_env%>%mutate(Month=factor(Month,level=c(9,10,12,1,3,5,6,7)),
                 Variables=factor(Variables,level=c('Temperature','Turbidity','Oxygen',
                                                    'Nitrate','Ammonium','Silicate',
                                                    'Phosphate','Chlorophyll-a')))%>%
  ggplot(aes(x=Station, y=Values)) + 
  geom_violin(trim= TRUE)+theme_bw()+ theme(panel.spacing=unit(0, "lines"))+
  geom_dotplot(binaxis = "y",method='dotdensity',stackdir = "center",stackratio=0.5,dotsize = 2,
               aes(colour=Depth,fill=Depth, alpha=0.01)) +
  facet_grid(rows = vars(Variables),scales='free',switch='y')+theme(legend.position="top")

ggsave('enviro_supp.pdf',width = 6,height =8)


####
enviro<-read.csv('F:/Postdoc South Africa/Penguin/Data/Nutrients/intg_chl_ntr.csv',sep=';')
names(enviro)
enviro<-enviro[,-c(8,10:14,16,19,20)]#total chl and SST,SBT,DOs,DOb and DIN because corr >0.7 with nitrogen basis and PO4
enviro<-enviro%>%mutate(station=as.integer(substring(Station,2,2)))
enviro<-enviro%>% mutate(logChl_a=log(Chl_a))#%>%mutate_at(vars(PO4,Si,NO3,NO2,NH4,Chl_a,logChl_a,NTU,Temperature,DO),scale)
chart.Correlation(enviro[,c(3:11,13)], histogram=TRUE, pch="+")#corr enviro
envviolin<-enviro%>%select(-c(Station,Chl_a))%>%group_by(station,Month)%>%
  pivot_longer(cols=c(PO4:DO,logChl_a),names_to='Names',values_to='Values')
month_order<-c('9','10','12','1','3','5','6','7')
envviolin$Month<- factor(envviolin$Month,levels=month_order)
ggplot(envviolin, aes(x=Month, y=Values)) + 
  geom_violin(trim= TRUE)+theme_bw()+ theme(panel.spacing=unit(0, "lines"))+
  facet_grid(rows = vars(Names),scales='free',switch='y')
ggsave('violin_env.pdf',width=6,height=6)

#NO2 high correlated with NO3, NH4 and PO4 we can drop it
#Zooplankton variables
#log abundance
table4models<-read.csv('F:/Postdoc South Africa/Paper Zooplankton/index_zoo.csv',header=TRUE)
names(table4models)
tableviolin<-table4models%>%select(!eevenness)%>%mutate_at(vars(biovolume,abundance),log)%>%
  group_by(Net,station,month)%>%
  pivot_longer(cols=r2:H,names_to='Names',values_to='Values')
month_order<-c('9','10','12','1','3','5','6','7')
tableviolin$month<- factor(tableviolin$month,levels=month_order)
ggplot(tableviolin, aes(x=month, y=Values)) + 
  geom_violin(trim= TRUE)+theme_bw()+ theme(panel.spacing=unit(0, "lines"))+
  facet_grid(rows = vars(Names),scales='free',switch='y')
ggsave('violin_var.pdf',width=6,height=6)
ggplot(tableviolin, aes(x=as.factor(station), y=Values)) + 
  geom_violin(trim= TRUE)+theme_bw()+  theme(panel.spacing=unit(0, "lines"))+
  facet_grid(rows = vars(Names),scales='free',switch='y')
#table4models<-table4models%>%mutate(abun_log=log(abundance),biov_log=log(biovolume))
total<-left_join(enviro,table4models, by=c('station'='station','Month'='month'))
chart.Correlation(total[,c(16:23)], histogram=TRUE, pch="+")#indicators, correlation pearson by default
chart.Correlation(total[,c(16,3:11,13)], histogram=TRUE, pch="+")#R2, skewness right
chart.Correlation(total[,c(17,3:11,13)], histogram=TRUE, pch="+")#intercept, skewness right
chart.Correlation(total[,c(18,3:11,13)], histogram=TRUE, pch="+")#slope, almost normal
chart.Correlation(total[,c(19,3:11,13)], histogram=TRUE, pch="+")#size div,almost normal
chart.Correlation(total[,c(20,3:11,13)], histogram=TRUE, pch="+")#evenness, skewness right
chart.Correlation(total[,c(21,3:11,13)], histogram=TRUE, pch="+")#abundance,almost normal
chart.Correlation(total[,c(22,3:11,13)], histogram=TRUE, pch="+")#biovolume,almost normal
chart.Correlation(total[,c(23,3:11,13)], histogram=TRUE, pch="+")#taxonomic diversity,almost normal
#Space time check two ways anova
table4models_s<-table4models%>% mutate_at(vars(r2:H), ~(scale(.) %>% as.vector))
r2_aov<-aov(r2~as.factor(station)+as.factor(month),data=table4models_s)
summary(r2_aov)
hist(r2_aov$residuals)
#Intecepts
intercept_aov<-aov(intercept~as.factor(station)+as.factor(month),data=table4models_s)
summary(intercept_aov)
hist(intercept_aov$residuals)
intercept_post<-TukeyHSD(intercept_aov, 'as.factor(month)')
round(sort(intercept_post$`as.factor(month)`[,4]),3)
#Slope
slope_aov<-aov(slope~as.factor(station)+as.factor(month),data=table4models_s)
summary(slope_aov)
hist(slope_aov$residuals)
slope_post<-TukeyHSD(slope_aov, 'as.factor(month)')
round(sort(slope_post$`as.factor(month)`[,4]),3)
#Size diversity
size_div_aov<-aov(size_div~as.factor(station)+as.factor(month),data=table4models_s)
summary(size_div_aov)
hist(size_div_aov$residuals)
size_div_post<-TukeyHSD(size_div_aov, 'as.factor(month)')
round(sort(size_div_post$`as.factor(month)`[,4]),3)
#Abundance
abundance_aov<-aov(abundance~as.factor(station)+as.factor(month),data=table4models_s)
summary(abundance_aov)
hist(abundance_aov$residuals)
abundance_post<-TukeyHSD(abundance_aov, 'as.factor(month)')
round(sort(abundance_post$`as.factor(month)`[,4]),3)
#Biovolume
biovolume_aov<-aov(biovolume~as.factor(station)+as.factor(month),data=table4models_s)
summary(biovolume_aov)
hist(biovolume_aov$residuals)
biovolume_post<-TukeyHSD(biovolume_aov, 'as.factor(month)')
sort(biovolume_post$`as.factor(month)`[,4])
#H
H_aov<-aov(H~as.factor(station)+as.factor(month),data=table4models_s)
summary(H_aov)
hist(H_aov$residuals)
H_post<-TukeyHSD(H_aov, 'as.factor(month)')
sort(H_post$`as.factor(month)`[,4])
####Random forest#####
library(randomForest)
#R2
r2data<-total[,c(16,3:5,7,9:11,13)]
r2.rf <- randomForest(r2 ~., data = r2data, mtry = 3, nPerm=20,ntree=1000,
                         importance = TRUE, na.action = na.omit) #sqrt(10)=3
print(r2.rf)
plot(r2.rf) 
fit<-cbind(importance(r2.rf,type=1),Name=rep('fit',8))
varImpPlot(r2.rf)
#Slope
sldata<-total[,c(18,3:5,7,9:11,13)]
sl.rf <- randomForest(slope ~., data = sldata, mtry = 3, nPerm=30,ntree=1000,
                      importance = TRUE, na.action = na.omit) 
print(sl.rf)
plot(sl.rf) 
slope<-cbind(importance(sl.rf,type=1),Name=rep('slope',8))
varImpPlot(sl.rf)
partialPlot(sl.rf, pred.data = sldata, x.var = "logChl_a")  # Figure 2
partialPlot(sl.rf, pred.data = sldata, x.var = "Temperature")  # Figure 2
partialPlot(sl.rf, pred.data = sldata, x.var = "Si")  # Figure 2
sl.rf %>%partial(pred.var = "logChl_a")%>%
  plotPartial(smooth = TRUE, lwd = 2, ylab = expression(f(logChl_a)))
sl.rf %>%partial(pred.var = "Temperature")%>%
  plotPartial(smooth = TRUE, lwd = 2, ylab = expression(f(Temperature)))
p1 <- partial(sl.rf, pred.var = c("logChl_a", "Temperature"), plot = TRUE, chull = TRUE)
grid.arrange(p1)
sl.p1<-sl.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "logChl_a") %>%
  autoplot(smooth = TRUE, ylab = expression(f(logChl_a))) +
  theme_light()
sl.p2<-sl.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Temperature") %>%
  autoplot(smooth = TRUE, ylab = expression(f(Temperature))) +
  theme_light()
sl.p3<-sl.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Si") %>%
  autoplot(smooth = TRUE, ylab = expression(f(Si))) +
  theme_light()
slplot<-sl.p1+sl.p2+sl.p3
ggsave('slplot.pdf')
#Intercept
indata<-total[,c(17,3:5,7,9:11,13)]
in.rf <- randomForest(intercept ~., data = indata, mtry = 3, nPerm=20,ntree=1000,
                      importance = TRUE, na.action = na.omit) 
print(in.rf)
plot(in.rf) 
t(importance(in.rf,type=1))
intercept<-cbind(importance(in.rf,type=1),Name=rep('intercept',8))
varImpPlot(in.rf)
partialPlot(in.rf, pred.data = indata, x.var = "Si")  # Figure 2
partialPlot(in.rf, pred.data = indata, x.var = "Temperature")  # Figure 2
in.rf %>%partial(pred.var = "Temperature")%>%
  plotPartial(smooth = TRUE, lwd = 2, ylab = expression(f(Temperature)))
in.p1<-in.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Si") %>%
  autoplot(smooth = TRUE, ylab = expression(f(Si))) +
  theme_light()
in.p2<-in.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Temperature") %>%
  autoplot(smooth = TRUE, ylab = expression(f(Temperature))) +
  theme_light()
inplot<-in.p1+in.p2
ggsave('inplot.pdf')
#evenness
evdata<-total[,c(20,3:5,7,9:11,13)]
ev.rf <- randomForest(eevenness ~., data = evdata, mtry = 3, nPerm=20,ntree=1000,
                      importance = TRUE, na.action = na.omit,keep.forest=TRUE) 
print(ev.rf)
plot(ev.rf) 
importance(ev.rf,type=2)
varImpPlot(ev.rf)
partialPlot(ev.rf, pred.data = evdata, x.var = "Temperature")  # Figure 2
partialPlot(ev.rf, pred.data = evdata, x.var = "NO3")  # Figure 2
partialPlot(ev.rf, pred.data = evdata, x.var = "PO4")  # Figure 2
#Size diversity
szdata<-total[,c(19,3:5,7,9:11,13)]
sz.rf <- randomForest(size_div ~., data = szdata, mtry = 3, nPerm=20,ntree=1000,
                      importance = TRUE, na.action = na.omit) 
print(sz.rf)
plot(sz.rf) 
t(importance(sz.rf,type=1))
s_diversity<-cbind(importance(sz.rf,type=1),Name=rep('size.diversity',8))
varImpPlot(sz.rf)
partialPlot(sz.rf, pred.data = szdata, x.var = "NH4")  # Figure 2
sz.rf %>%partial(pred.var = "NTU")%>%
  plotPartial(smooth = TRUE, lwd = 2, ylab = expression(f(NTU)))
sz.p1<-sz.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "NH4") %>%
  autoplot(smooth = TRUE, ylab = expression(f(NH4))) +
  theme_light()
sz.p2<-sz.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "NTU") %>%
  autoplot(smooth = TRUE, ylab = expression(f(NTU))) +
  theme_light()
szplot<-sz.p1+sz.p2
ggsave('szplot.pdf')
#abundance, with or without logartihm (Garcia-comas log) makes no differences
abdata<-total[,c(21,3:5,7,9:11,13)]
ab.rf <- randomForest(log(abundance) ~., data = abdata, mtry = 3, nPerm=20,ntree=1000,
                      importance = TRUE, na.action = na.omit) 
print(ab.rf)
plot(ab.rf) 
t(importance(ab.rf,type=1))
abundance<-cbind(importance(ab.rf,type=1),Name=rep('ab.rf',8))
varImpPlot(ab.rf)
partialPlot(ab.rf, pred.data = abdata, x.var = "Si")  # Figure 2
partialPlot(ab.rf, pred.data = abdata, x.var = "NH4")  # Figure 2
ab.p1<-ab.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Si") %>%
  autoplot(smooth = TRUE, ylab = expression(f(Si))) +
  theme_light()
ab.p2<-ab.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "NH4") %>%
  autoplot(smooth = TRUE, ylab = expression(f(NH4))) +
  theme_light()
abplot<-ab.p1+ab.p2
ggsave('abplot.pdf')
#biovolume
bidata<-total[,c(22,3:5,7,9:11,13)]
bi.rf <- randomForest(log(biovolume) ~., data = bidata, mtry = 3, nPerm=20,ntree=1000,
                      importance = TRUE, na.action = na.omit) 
print(bi.rf)
plot(bi.rf) 
t(importance(bi.rf,type=1))
volume<-cbind(importance(bi.rf,type=1),Name=rep('volume',8))
varImpPlot(bi.rf)
partialPlot(bi.rf, pred.data = bidata, x.var = "Si")  # Figure 2
bi.p1<-bi.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Si") %>%
  autoplot(smooth = TRUE, ylab = expression(f(Si))) +
  theme_light()
bi.p2<-bi.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Temperature") %>%
  autoplot(smooth = TRUE, ylab = expression(f(Temperature))) +
  theme_light()
abplot<-bi.p1+bi.p2
ggsave('biplot.pdf')
#taxonomic diversity
tddata<-total[,c(23,3:5,7,9:11,13)]
td.rf <- randomForest(H ~., data = tddata, mtry = 3, nPerm=20,ntree=1000,
                      importance = TRUE, na.action = na.omit) 
print(td.rf)
plot(td.rf) 
t(importance(td.rf,type=1))
t_diversity<-cbind(importance(td.rf,type=1),Name=rep('taxonomic.diversity',8))
varImpPlot(td.rf)
partialPlot(td.rf, pred.data = tddata, x.var = "NO3")
td.p1<-td.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "NO3") %>%
  autoplot(smooth = TRUE, ylab = expression(f(NO3))) +
  theme_light()
td.p2<-td.rf %>%  # the %>% operator is read as "and then"
  partial(pred.var = "Temperature") %>%
  autoplot(smooth = TRUE, ylab = expression(f(Temperature))) +
  theme_light()
tdplot<-td.p1+td.p2
ggsave('tdplot.pdf')
data4heat<-rbind(volume,abundance,fit,intercept,slope,t_diversity,s_diversity)
write.csv(data4heat,'RF_output.csv')
library(ComplexHeatmap)
library(circlize)
library(dplyr)
library(tidyr)
data4heat<-read.csv('RF_output.csv',header = T)
names(data4heat)<-c('Variables','IncMSE','Name')
matrixheat<-data4heat%>%pivot_wider(names_from = 'Name',values_from='IncMSE')%>%as.data.frame()%>%
  magrittr::set_rownames(.$Variables) %>%dplyr::select(-Variables) %>%as.matrix
pdf('Heatmap_RF.pdf',width=10,height=8)
Heatmap(t(matrixheat),name='%IncMSE',cluster_rows=T,clustering_distance_rows = "pearson",
        cluster_columns=T,clustering_distance_columns="pearson",col = brewer.pal(n = 5, name = "Blues"),#YlOrRdcolorRamp2(c(0,20,40), c("green","yellow", "red"))
        row_names_side = "left", column_names_side = "bottom",row_names_gp=gpar(fontsize = 15),column_names_gp=gpar(fontsize = 15),
        column_title_gp = gpar(fill = "white"),
        row_labels = c('Volume','Abundance','Linear fit','Intercept','Slope','Taxonomic div.','Size div.'),
        column_labels = c('Phosphates','Silicates','Nitrates','Ammonium','Turbidity','Temperature',
                          expression('Dissolved O'[2]),'Log(Chl-a)'))
dev.off()

########So lets go for GLMM or GAMM#######
#R2
gamr2Chla<-gam(r2~s(logChl_a,bs='cs'),
               data=total,method = 'REML',select=TRUE)#
plot(gamr2Chla)
gamr2PO4<-gam(r2~s(PO4,bs='cs'),
               data=total,method = 'REML',select=TRUE)#
plot(gamr2PO4)
gamr2Si<-gam(r2~s(Si,bs='cs'),
              data=total,method = 'REML',select=TRUE)#
plot(gamr2Si)
gamr2NO3<-gam(r2~s(NO3,bs='cs'),
             data=total,method = 'REML',select=TRUE)#Only this can be non linear
plot(gamr2NO3,residuals = TRUE)
gamr2NO2<-gam(r2~s(NO2,bs='cs'),
              data=total,method = 'REML',select=TRUE)#
plot(gamr2NO2,residuals = TRUE)
gamr2temp<-gam(r2~s(Temperature,bs='cs'),
              data=total,method = 'REML',select=TRUE)#
plot(gamr2temp,residuals = TRUE)
gamr2NTU<-gam(r2~s(NTU,bs='cs'),
               data=total,method = 'REML',select=TRUE)#
plot(gamr2NTU,residuals = TRUE)
gamr2DO<-gam(r2~s(DO,bs='cs'),
              data=total,method = 'REML',select=TRUE)#
plot(gamr2DO,residuals = TRUE)
#slope
gamslChla<-gam(slope~s(logChl_a,bs='cs'),
               data=total,method = 'REML',select=TRUE)#Not really gaussian
plot(gamslChla,residuals = TRUE)
gamslPO4<-gam(slope~s(PO4,bs='cs'),
              data=total,method = 'REML',select=TRUE)#
plot(gamslPO4,residuals = TRUE)
gamslSi<-gam(slope~s(Si,k=5,bs='cs'),
             data=total,method = 'REML',select=TRUE)#
plot(gamslSi,residuals = TRUE)# no linear
gam.check(gamslSi)#check k' to be larger than 1 and p-value no significativo
gamslNO3<-gam(slope~s(NO3,bs='cs'),
              data=total,method = 'REML',select=TRUE)#Only this can be non linear
plot(gamslNO3,residuals = TRUE)#no linear
gam.check(gamslNO3,rep=500)#that can be a possible covariate, responsevsfitted values shuld be like a linear regression
gamslNO2<-gam(slope~s(NO2,bs='cs'),
              data=total,method = 'REML',select=TRUE)#
plot(gamslNO2,residuals = TRUE)
gam.check(gamslNO2,rep=500)#no gausian, heterogenity
gamslNH4<-gam(slope~s(NH4,bs='cs'),
              data=total,method = 'REML',select=TRUE)#
plot(gamslNH4,residuals = TRUE)
gam.check(gamslNH4,rep=500)#no gausian, heterogenity
gamsltemp<-gam(slope~s(Temperature,bs='cs'),
               data=total,method = 'REML',select=TRUE)#
plot(gamsltemp,residuals = TRUE)
gam.check(gamsltemp,rep=500)#linear
gamslNTU<-gam(slope~s(NTU,bs='cs'),
              data=total,method = 'REML',select=TRUE)#
plot(gamslNTU,residuals = TRUE)
gam.check(gamslNTU,rep=500)#no gausian, heterogenity
gamslDO<-gam(slope~s(DO,bs='cs'),
             data=total,method = 'REML',select=TRUE)#
plot(gamslDO,residuals = TRUE)
gam.check(gamslDO,rep=500)#no gausian, heterogenity
#Intercept
purrr::map_dfr(1:30, function(i) data.frame(model = i, 
                                            tidy(glm(as.formula(paste0('casecontrol ~ ', 'rs', i)), data = mydata, family = binomial))))

# ab<-ab[order(ab$month),]#important order by month
# group<-c(rep(1,4),rep(2,4))
# Abund_log<-as.numeric(as.character(ab$Abund_log))
# hist(Abund_log)#normal distribution
# abt<-data.frame(enviro,Abund_log,group)
# abt$Month<-factor(abt$Month)
# #abt$Chla<-enviro_norm$Chla#replace with normalized value
# splom(abt[3:13])
#Multiple regression (aka GLMM) shows cause and effect, then I will use it 
#and may be in association with random forest
library(lattice)
library(nlme)
library(lme4)
require(MuMIn)

#Null model for lm
null_ab<-lm(Abund_log~1,data=abt)
summary(null_ab)
#lmer is the equivalent to lm or glm
#Forward selection (Baayen et al 2008) in mixed models starting from the minimal intercepts-only structure
summary(null_abr<-lmer(Abund_log~(1|Station)+(1|group),data=abt,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
summary(null_abr1<-lmer(Abund_log~(1|Month),data=abt,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
summary(null_abr<-lmer(Abund_log~(1|Station)+(1|Month)+(1|group),data=abt,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
anova(null_abr1,null_abr)#all the residual variance is captured in month
#apparently there is not variability captured by random effects on the station and group (structure autocorrelation)
#Phosphate
#So check first if month are random or is a structured important effect
summary(phos_abr<-lmer(Abund_log~PO4+(1|Month),data=abt,REML=FALSE))#intercept random effect
summary(null_abr1<-lmer(Abund_log~(1|Month),data=abt,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
anova(null_abr1,phos_abr)
summary(phos_ab1<-lm(Abund_log~PO4,data=abt,REML=FALSE))#intercept random effect
summary(phos_ab<-lm(Abund_log~PO4+factor(Month),data=abt))#fixed effect already checked formula and comparison
anova(phos_abr,phos_ab)#the order is important so here must random effects first given
anova(phos_abr,phos_ab1)#the order is important so here must random effects first given
#Months are important
summary(phos_ab1<-lm(Abund_log~PO4+factor(Month),data=abt))#intercept random effect
summary(phos_ab2<-lm(Abund_log~PO4*factor(Month),data=abt))#factor interactions
#summary(phos_abr5<-lmer(Abund_log~(1+PO4|Month),data=abt,REML=FALSE))#slope and random intercept
anova(null_ab,phos_ab1,phos_ab2)#best 3, order is important and null must given first
#coef(phos_abr5)
#Silicate
#So check first if month are random or is a structured important effect
summary(sili_abr<-lmer(Abund_log~Si+(1|Month),data=abt,REML=FALSE))#intercept random effect
summary(sili_ab1<-lm(Abund_log~Si,data=abt))#fixed effect already checked formula and comparison
summary(sili_ab<-lm(Abund_log~Si+factor(Month),data=abt))#fixed effect already checked formula and comparison
anova(sili_abr,sili_ab)#compare AIC
anova(sili_abr,sili_ab1)#compare AIC
#Months are important
summary(sili_ab1<-lm(Abund_log~Si+factor(Month),data=abt))#
summary(sili_ab2<-lm(Abund_log~Si*factor(Month),data=abt))#
anova(null_ab,sili_ab1,sili_ab2)#best 3, order is important and null must given first
#Nitrate
#So check first if month are random or is a structured important effect
summary(nita_abr<-lmer(Abund_log~NO3+(1|Month),data=abt,REML=FALSE))#intercept random effect
summary(nita_ab1<-lm(Abund_log~NO3,data=abt,REML=FALSE))#intercept random effect
summary(nita_ab<-lm(Abund_log~NO3+factor(Month),data=abt))#fixed effect already checked formula and comparison
anova(nita_abr,nita_ab, test="Chisq")#compare AIC
anova(nita_abr,nita_ab1, test="Chisq")#compare AIC
#Months are important
summary(nita_ab1<-lm(Abund_log~NO3+factor(Month),data=abt))#
summary(nita_ab2<-lm(Abund_log~NO3*factor(Month),data=abt))#
anova(null_ab,nita_ab1,nita_ab2)#compare reduction in residual sums not AIC
#Nitrite
#So check first if month are random or is a structured important effect
summary(niti_abr<-lmer(Abund_log~NO2+(1|Month),data=abt,REML=FALSE))#intercept random effect
r.squaredGLMM(niti_abr)
summary(niti_ab1<-lm(Abund_log~NO2,data=abt,REML=FALSE))#intercept random effect
summary(niti_ab<-lm(Abund_log~NO2+factor(Month),data=abt))#fixed effect already checked formula and comparison
anova(niti_abr,niti_ab, test="Chisq")#compare AIC
anova(niti_abr,niti_ab1, test="Chisq")#compare AIC, better with random effet of months than only no2
#Months are important
summary(niti_ab1<-lm(Abund_log~NO2+factor(Month),data=abt))#NO2 is significant
summary(niti_ab2<-lm(Abund_log~NO2*factor(Month),data=abt))#
anova(null_ab,niti_ab1,niti_ab2)#compare reduction in residual sums not AIC
#Ammonium
#So check first if month are random or is a structured important effect
summary(amon_abr<-lmer(Abund_log~NH4+(1|Month),data=abt,REML=FALSE))#intercept random effect
summary(amon_ab<-lm(Abund_log~NH4+factor(Month),data=abt))#fixed effect already checked formula and comparison
summary(amon_ab1<-lm(Abund_log~NH4,data=abt))#fixed effect already checked formula and comparison
anova(amon_abr,amon_ab, test="Chisq")#compare AIC
anova(amon_abr,amon_ab1, test="Chisq")#compare AIC
#Months are important
summary(amon_ab1<-lm(Abund_log~NH4+factor(Month),data=abt))#
summary(amon_ab2<-lm(Abund_log~NH4*factor(Month),data=abt))#
anova(null_ab,amon_ab1,amon_ab2)#compare reduction in residual sums not AIC
#Chlorophyll-a
#So check first if month are random or is a structured important effect
summary(chla_abr<-lmer(Abund_log~Chla+(1|Month),data=abt,REML=FALSE))#intercept random effect
summary(chla_ab1<-lm(Abund_log~Chla,data=abt))#fixed effect already checked formula and comparison
summary(chla_ab<-lm(Abund_log~Chla+factor(Month),data=abt))#fixed effect already checked formula and comparison
anova(chla_abr,chla_ab, test="Chisq")#compare AIC
anova(chla_abr,chla_ab1, test="Chisq")#compare AIC
#Months are important
summary(chla_ab1<-lm(Abund_log~Chla+factor(Month),data=abt))#
summary(chla_ab2<-lm(Abund_log~Chla*factor(Month),data=abt))#
anova(null_ab,chla_ab1,chla_ab2)#
#Salinity
#So check first if month are random or is a structured important effect
summary(sal_abr<-lmer(Abund_log~Sal+(1|Month),data=abt,REML=FALSE))#intercept random effect
summary(sal_ab<-lm(Abund_log~Sal+factor(Month),data=abt))#fixed effect already checked formula and comparison
summary(sal_ab1<-lm(Abund_log~Sal,data=abt))#fixed effect already checked formula and comparison
anova(sal_abr,sal_ab, test="Chisq")#compare AIC
anova(sal_abr,sal_ab1, test="Chisq")#compare AIC
#Months are important
summary(sal_ab1<-lm(Abund_log~+Sal+factor(Month),data=abt))#
summary(sal_ab2<-lm(Abund_log~+Sal*factor(Month),data=abt))#also significant for sal:month5
anova(null_ab,sal_ab1,sal_ab2)#
#Temperature
#So check first if month are random or is a structured important effect
summary(temp_abr<-lmer(Abund_log~Temperature+(1|Month),data=abt,REML=FALSE))#intercept random effect
summary(temp_ab<-lm(Abund_log~Temperature+factor(Month),data=abt))#fixed effect already checked formula and comparison
summary(temp_ab1<-lm(Abund_log~Temperature,data=abt))#fixed effect already checked formula and comparison
anova(temp_abr,temp_ab, test="Chisq")#compare AIC
anova(temp_abr,temp_ab1, test="Chisq")#compare AIC
#Months are important
summary(temp_ab1<-lm(Abund_log~Temperature+factor(Month),data=abt))#
summary(temp_ab2<-lm(Abund_log~Temperature*factor(Month),data=abt))#
anova(null_ab,temp_ab1,temp_ab2)#
#I muss use a subset
abt_sub<-abt[-which(abt$Month==6),]
#Turbidity
#So check first if month are random or is a structured important effect
summary(ntu_abr<-lmer(Abund_log~NTU+(1|Month),data=abt_sub,REML=FALSE))#intercept random effect
summary(ntu_ab<-lm(Abund_log~NTU+factor(Month),data=abt_sub))#fixed effect already checked formula and comparison
summary(ntu_ab1<-lm(Abund_log~NTU,data=abt_sub))#fixed effect already checked formula and comparison
anova(ntu_abr,ntu_ab, test="Chisq")#compare AIC
anova(ntu_abr,ntu_ab1, test="Chisq")#compare AIC
#Months are important
null_absub<-lm(Abund_log~1,data=abt_sub)#null model subset
#Months are important
summary(ntu_ab1<-lm(Abund_log~NTU+factor(Month),data=abt))#
summary(ntu_ab2<-lm(Abund_log~NTU*factor(Month),data=abt))#ntu:10
anova(null_absub,ntu_ab1,ntu_ab2)#
#Oxygen
#So check first if month are random or is a structured important effect
summary(oxy_abr<-lmer(Abund_log~DO+(1|Month),data=abt_sub,REML=FALSE))#intercept random effect
#This is the best model more abundance when more dissolved oxygen
summary(oxy_ab1<-lm(Abund_log~DO,data=abt_sub))#intercept random effect
summary(oxy_ab<-lm(Abund_log~DO+factor(Month),data=abt_sub))#fixed effect already checked formula and comparison
anova(oxy_abr,oxy_ab, test="Chisq")#compare AIC
anova(oxy_abr,oxy_ab1, test="Chisq")#compare AIC
#Months are important
summary(oxy_ab1<-lm(Abund_log~DO+factor(Month),data=abt_sub))#
summary(oxy_ab2<-lm(Abund_log~DO*factor(Month),data=abt_sub))#
anova(null_absub,oxy_ab1,oxy_ab2)#
#Again the best with subsetted data
#Nitrite
#So check first if month are random or is a structured important effect
summary(niti_abr<-lmer(Abund_log~NO2+(1|Month),data=abt_sub,REML=FALSE))#intercept random effect
summary(niti_ab1<-lm(Abund_log~NO2,data=abt_sub))#intercept random effect
summary(niti_ab<-lm(Abund_log~NO2+factor(Month),data=abt_sub))#fixed effect already checked formula and comparison
anova(niti_abr,niti_ab, test="Chisq")#compare AIC
anova(niti_abr,niti_ab1, test="Chisq")#compare AIC, better with random effet of months than only no2

#Ammonium
#So check first if month are random or is a structured important effect
summary(amon_abr<-lmer(Abund_log~NH4+(1|Month),data=abt_sub,REML=FALSE))#intercept random effect
summary(amon_ab<-lm(Abund_log~NH4+factor(Month),data=abt_sub))#fixed effect already checked formula and comparison
summary(amon_ab1<-lm(Abund_log~NH4,data=abt_sub))#fixed effect already checked formula and comparison
anova(amon_abr,amon_ab, test="Chisq")#compare AIC
anova(amon_abr,amon_ab1, test="Chisq")#compare AIC

#Anova for the best
ab_nullr<-lmer(Abund_log ~ (1 | Month), data=abt_sub,REML=FALSE)
summary(oxy_abr<-lmer(Abund_log~DO+(1|Month),data=abt_sub,REML=FALSE))#intercept random effect
summary(niti_abr<-lmer(Abund_log~NO2+(1|Month),data=abt_sub,REML=FALSE))#intercept random effect
summary(amon_abr<-lmer(Abund_log~NH4+(1|Month),data=abt_sub,REML=FALSE))#intercept random effect
summary(oxyno2_abr<-lmer(Abund_log~NO2+DO+(1|Month),data=abt_sub,REML=FALSE))#intercept random effect
anova(ab_nullr,oxy_abr,niti_abr,amon_abr)#compare AIC
anova(ab_nullr,oxy_abr,niti_abr,oxyno2_abr)#compare AIC
#ONLY NO2 IS THE PARAMETER BETTER EXPLAINING THE ABUNDANCE:MORE ABUNDANCE LESS NITRATES

#Biovolume
biovolume<-read.csv2('C:/Postdoc/Penguin/Data/Zooplancton/biovolume_stations.csv')
names(biovolume)
group<-c(rep(1,4),rep(2,4))
biovolume<-biovolume[order(biovolume$month),]#important order by month
biovolume$biovol_station<-as.numeric(as.character(biovolume$biovol_station))
biovol_log<-log(biovolume$biovol_station+1)
biov<-data.frame(enviro,biovol_log,group)
biov$Month<-factor(biov$Month)
splom(biov[3:13])#scatter plot matrix
#Null model for lm
null_biov<-lm(biovol_log~1,data=biov)
summary(null_biov)
#Random intercepts: multiples responses by stations(spatial), by months(time), solve non-independence
#lmer is the equivalent to lm or glm
#Forward selection (Baayen et al 2008) in mixed models starting from the minimal intercepts-only structure
summary(null_biovr<-lmer(biovol_log~(1|Station)+(1|Month)+(1|group),data=biov,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
summary(null_biovr1<-lmer(biovol_log~(1|Month),data=biov,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
anova(null_biovr1,null_biovr)#all the residual variance is captured in month
#apparently there is not significant difference between models with just month and random effects on the station and group (structure autocorrelation)
#Phosphate
#So check first if month are random or is a structured important effect
summary(phos_biovr<-lmer(biovol_log~PO4+(1|Month),data=biov,REML=FALSE))#intercept random effect
summary(phos_biov1<-lm(biovol_log~PO4,data=biov))#fixed effect already checked formula and comparison
summary(phos_biov<-lm(biovol_log~PO4+factor(Month),data=biov))#fixed effect already checked formula and comparison
anova(phos_biovr,phos_biov)#the order is important so here must random effects first given
anova(phos_biovr,phos_biov1)#the order is important so here must random effects first given
#Months are important
summary(phos_biov1<-lm(biovol_log~PO4+factor(Month),data=biov))#intercept random effect
summary(phos_biov2<-lm(biovol_log~PO4*factor(Month),data=biov))#factor interactions
#summary(phos_abr5<-lmer(Abund_log~(1+PO4|Month),data=abt,REML=FALSE))#slope and random intercept
anova(null_biov,phos_biov1,phos_biov2)#best 3, order is important and null must given first
#coef(phos_abr5)
#Silicate
#So check first if month are random or is a structured important effect
summary(sili_biovr<-lmer(biovol_log~Si+(1|Month),data=biov,REML=FALSE))#intercept random effect
r.squaredGLMM(sili_biovr)#give r2 marginal and conditional, marginal account for fixed effect what is needed
summary(sili_biov1<-lm(biovol_log~Si,data=biov))#fixed effect already checked formula and comparison
summary(sili_biov<-lm(biovol_log~Si+factor(Month),data=biov))#fixed effect already checked formula and comparison
anova(sili_biovr,sili_biov)#compare AIC
anova(sili_biovr,sili_biov1)#compare AIC
plot(biov$Si,biov$biovol_log)#high biovolume when less silicates
#Months are important
summary(sili_biov1<-lm(biovol_log~Si+factor(Month),data=biov))#
summary(sili_biov2<-lm(biovol_log~Si*factor(Month),data=biov))#
anova(null_biov,sili_biov1,sili_biov2)#best 3, order is important and null must given first
#Nitrate
#So check first if month are random or is a structured important effect
summary(nita_biovr<-lmer(biovol_log~NO3+(1|Month),data=biov,REML=FALSE))#intercept random effect
summary(nita_biov1<-lm(biovol_log~NO3,data=biov))#intercept random effect
summary(nita_biov<-lm(biovol_log~NO3+factor(Month),data=biov))#fixed effect already checked formula and comparison
anova(nita_biovr,nita_biov, test="Chisq")#compare AIC
anova(nita_biovr,nita_biov1, test="Chisq")#compare AIC
#Months are important
summary(nita_biov1<-lm(biovol_log~NO3+factor(Month),data=biov))#
summary(nita_biov2<-lm(biovol_log~NO3*factor(Month),data=biov))#
anova(null_biov,nita_biov1,nita_biov2)#compare reduction in residual sums not AIC
#Nitrite
#So check first if month are random or is a structured important effect
summary(niti_biovr<-lmer(biovol_log~NO2+(1|Month),data=biov,REML=FALSE))#intercept random effect
summary(niti_biov1<-lm(biovol_log~NO2,data=biov))#fixed effect already checked formula and comparison
summary(niti_biov<-lm(biovol_log~NO2+factor(Month),data=biov))#fixed effect already checked formula and comparison
anova(niti_biovr,nita_biov, test="Chisq")#compare AIC
anova(niti_biovr,nita_biov1, test="Chisq")#compare AIC
#Months are important
summary(niti_biov1<-lm(biovol_log~NO2+factor(Month),data=biov))#NO2 is significant
summary(niti_biov2<-lm(biovol_log~NO2*factor(Month),data=biov))#
anova(null_biov,niti_biov1,niti_biov2)#compare reduction in residual sums not AIC
#Ammonium
#So check first if month are random or is a structured important effect
summary(amon_biovr<-lmer(biovol_log~NH4+(1|Month),data=biov,REML=FALSE))#intercept random effect
summary(amon_biov1<-lm(biovol_log~NH4,data=biov))#fixed effect already checked formula and comparison
summary(amon_biov<-lm(biovol_log~NH4+factor(Month),data=biov))#fixed effect already checked formula and comparison
anova(amon_biovr,amon_biov, test="Chisq")#compare AIC
anova(amon_biovr,amon_biov1, test="Chisq")#compare AIC
#Months are important
summary(amon_biov1<-lm(biovol_log~NH4+factor(Month),data=biov))#
summary(amon_biov2<-lm(biovol_log~NH4+factor(Month),data=biov))#
anova(null_biov,amon_biov1,amon_biov2)#compare reduction in residual sums not AIC
#Chlorophyll-a
#So check first if month are random or is a structured important effect
summary(chla_biovr<-lmer(biovol_log~Chla+(1|Month),data=biov,REML=FALSE))#intercept random effect
summary(chla_biov1<-lm(biovol_log~Chla,data=biov))#fixed effect already checked formula and comparison
summary(chla_biov<-lm(biovol_log~Chla+factor(Month),data=biov))#fixed effect already checked formula and comparison
anova(chla_biovr,chla_biov, test="Chisq")#compare AIC
anova(chla_biovr,chla_biov1, test="Chisq")#compare AIC
#Months are important
summary(chla_biov1<-lm(biovol_log~Chla+factor(Month),data=biov))#
summary(chla_biov2<-lm(biovol_log~Chla+factor(Month),data=biov))#
anova(null_biov,chla_biov1,chla_biov2)#
#Salinity
#So check first if month are random or is a structured important effect
summary(sal_biovr<-lmer(biovol_log~Sal+(1|Month),data=biov,REML=FALSE))#intercept random effect
summary(sal_biov1<-lm(biovol_log~Sal,data=biov))#fixed effect already checked formula and comparison
summary(sal_biov<-lm(biovol_log~Sal+factor(Month),data=biov))#fixed effect already checked formula and comparison
anova(sal_biovr,sal_biov, test="Chisq")#compare AIC
anova(sal_biovr,sal_biov1, test="Chisq")#compare AIC
plot(biov$Sal,biov$biovol_log)#high biovolume when low salinity
#Months are important
summary(sal_biov1<-lm(biovol_log~Sal+factor(Month),data=biov))#
summary(sal_biov2<-lm(biovol_log~Sal*factor(Month),data=biov))#also significant for sal:month5
anova(null_biov,sal_biov1,sal_biov2)#
#Temperature
#So check first if month are random or is a structured important effect
summary(temp_biovr<-lmer(biovol_log~Temperature+(1|Month),data=biov,REML=FALSE))#intercept random effect
summary(temp_biov1<-lm(biovol_log~Temperature,data=biov))#fixed effect already checked formula and comparison
summary(temp_biov<-lm(biovol_log~Temperature+factor(Month),data=biov))#fixed effect already checked formula and comparison
anova(temp_biovr,temp_biov, test="Chisq")#compare AIC
anova(temp_biovr,temp_biov1, test="Chisq")#compare AIC
plot(biov$Temperature,biov$biovol_log)#high biovolume when low temperture
#Months are important
summary(temp_biov1<-lm(biovol_log~Temperature+factor(Month),data=biov))#
summary(temp_biov2<-lm(biovol_log~Temperature*factor(Month),data=biov))#
summary(temp_biov3<-lm(biovol_log~Temperature,data=biov))#
summary(temp_biov4<-lm(biovol_log~factor(Month),data=biov))#
anova(null_biov,temp_biov3,temp_biov4,temp_biov1,temp_biov2)#
#I muss use a subset
biov_sub<-biov[-which(biov$Month==6),]
#Turbidity
#So check first if month are random or is a structured important effect
summary(ntu_biovr<-lmer(biovol_log~NTU+(1|Month),data=biov_sub,REML=FALSE))#intercept random effect
summary(ntu_bio1<-lm(biovol_log~NTU,data=biov_sub))#intercept random effect
summary(ntu_biov<-lm(biovol_log~NTU+factor(Month),data=biov_sub))#fixed effect already checked formula and comparison
anova(ntu_biovr,ntu_biov, test="Chisq")#compare AIC
anova(ntu_biovr,ntu_biov1, test="Chisq")#compare AIC
#Months are important
null_biovsub<-lm(biovol_log~1,data=biov_sub)#null model subset
#Months are important
summary(ntu_biov1<-lm(biovol_log~NTU+factor(Month),data=biov_sub))#
summary(ntu_biov2<-lm(biovol_log~NTU*factor(Month),data=biov_sub))#ntu:10
summary(ntu_biov3<-lm(biovol_log~NTU,data=biov_sub))#alone is significant
summary(ntu_biov4<-lm(biovol_log~factor(Month),data=biov_sub))#
anova(null_biovsub,ntu_biov3,ntu_biov4,ntu_biov1,ntu_biov2)#
#Oxygen
#So check first if month are random or is a structured important effect
summary(oxy_biovr<-lmer(biovol_log~DO+(1|Month),data=biov_sub,REML=FALSE))#intercept random effect
summary(oxy_biov1<-lm(biovol_log~DO,data=biov_sub))#fixed effect already checked formula and comparison
summary(oxy_biov<-lm(biovol_log~DO+factor(Month),data=biov_sub))#fixed effect already checked formula and comparison
anova(oxy_biovr,oxy_biov, test="Chisq")#compare AIC
anova(oxy_biovr,oxy_biov1, test="Chisq")#compare AIC
plot(biov_sub$DO,biov_sub$biovol_log)#high biovolume when low temperture
#Months are important
summary(oxy_biov1<-lm(biovol_log~DO+factor(Month),data=biov_sub))#
summary(oxy_biov2<-lm(biovol_log~DO*factor(Month),data=biov_sub))#
summary(oxy_biov3<-lm(biovol_log~DO,data=biov_sub))#alone is significant
summary(oxy_biov4<-lm(biovol_log~factor(Month),data=biov_sub))#
anova(null_biovsub,oxy_biov3,oxy_biov4,oxy_biov1,oxy_biov2)#
#Anova for the best
biov_nullr<-lmer(biovol_log ~ (1 | Month), data=biov_sub,REML=FALSE)
summary(si_biovr<-lmer(biovol_log~Si+(1|Month),data=biov_sub,REML=FALSE))#intercept random effect
summary(chla_biovr<-lmer(biovol_log~Chla+(1|Month),data=biov_sub,REML=FALSE))#intercept random effect
summary(sal_biovr<-lmer(biovol_log~Sal+(1|Month),data=biov_sub,REML=FALSE))#intercept random effect
summary(tem_biovr<-lmer(biovol_log~Temperature+(1|Month),data=biov_sub,REML=FALSE))#intercept random effect
summary(all_biovr<-lmer(biovol_log~Si+Chla+Sal+DO+NTU+Temperature+(1|Month),data=biov_sub,REML=FALSE))#intercept random effect
summary(sichl_biovr<-lmer(biovol_log~Si+Chla+(1|Month),data=biov_sub,REML=FALSE))#intercept random effect
anova(biov_nullr,si_biovr,sichl_biovr,all_biovr)#compare AIC
#ONLY Si IS THE PARAMETER BETTER EXPLAINING THE BIOVOLUME:MORE BIOVOLUME LESS SILICATES


#Slope
slope<-read.csv2('C:/Postdoc/Penguin/Data/Zooplancton/NBSS_r2_perstation.csv')
names(slope)
slope<-slope[order(slope$month),]#important order by month
slo<-as.numeric(as.character(slope$NBSS_slope))
group<-c(rep(1,4),rep(2,4))
slopet<-data.frame(enviro,slo,group)
slopet$Month<-factor(slopet$Month)
splom(slopet[3:13])#scatter plot matrix
#Null model for lm
null_slope<-lm(slo~1,data=slopet)
summary(null_slope)
#Random intercepts: multiples responses by stations(spatial), by months(time), solve non-independence
#lmer is the equivalent to lm or glm
#Forward selection (Baayen et al 2008) in mixed models starting from the minimal intercepts-only structure
summary(null_sloper<-lmer(slo~(1|Station)+(1|Month)+(1|group),data=slopet,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
summary(null_sloper2<-lmer(slo~(1|Month)+(1|group),data=slopet,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
summary(null_sloper1<-lmer(slo~(1|Month),data=slopet,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
anova(null_sloper1,null_sloper,null_sloper2)#all the residual variance is captured in month
#apparently there is not significant difference between models with just month and random effects on the station and group (structure autocorrelation)
#Phosphate
#So check first if month are random or is a structured important effect
summary(phos_sloper<-lmer(slo~PO4+(1|Month),data=slopet,REML=FALSE))#intercept random effect
summary(phos_sloper1<-lmer(slo~PO4+(1|Month)+(1|group),data=slopet,REML=FALSE))#intercept random effect, not converge
summary(phos_slope1<-lm(slo~PO4,data=slopet))#fixed effect already checked formula and comparison
summary(phos_slope<-lm(slo~PO4+factor(Month),data=slopet))#fixed effect already checked formula and comparison
anova(phos_sloper,phos_sloper1,phos_slope)#the order is important so here must random effects first given
anova(phos_sloper,phos_sloper1,phos_slope1)#the order is important so here must random effects first given
#Months are important
summary(phos_slope1<-lm(slo~PO4+factor(Month),data=slopet))#intercept random effect
summary(phos_slope2<-lm(slo~PO4*factor(Month),data=slopet))#factor interactions
#summary(phos_abr5<-lmer(Abund_log~(1+PO4|Month),data=abt,REML=FALSE))#slope and random intercept
anova(null_slope,phos_slope1,phos_slope2)#best 3, order is important and null must given first
#coef(phos_abr5)
#Silicate
#So check first if month are random or is a structured important effect
summary(sili_sloper<-lmer(slo~Si+(1|Month),data=slopet,REML=FALSE))#intercept random effect
summary(sili_slope1<-lm(slo~Si,data=slopet))#
summary(sili_slope<-lm(slo~Si+factor(Month),data=slopet))#fixed effect already checked formula and comparison
anova(sili_sloper,sili_slope)#compare AIC
anova(sili_sloper,sili_slope1)#compare AIC
#Months are important
summary(sili_slope1<-lm(slo~Si+factor(Month),data=slopet))#
summary(sili_slope2<-lm(slo~Si*factor(Month),data=slopet))#
anova(null_slope,sili_slope1,sili_slope2)#best 3, order is important and null must given first
#Nitrate
#So check first if month are random or is a structured important effect
summary(nita_sloper<-lmer(slo~NO3+(1|Month),data=slopet,REML=FALSE))#intercept random effect
summary(nita_slope1<-lm(slo~NO3,data=slopet))#fixed effect already checked formula and comparison
summary(nita_slope<-lm(slo~NO3+factor(Month),data=slopet))#fixed effect already checked formula and comparison
anova(nita_sloper,nita_slope, test="Chisq")#compare AIC
anova(nita_sloper,nita_slope1, test="Chisq")#compare AIC
#Months are important
summary(nita_slope1<-lm(slo~NO3+factor(Month),data=slopet))#
summary(nita_slope2<-lm(slo~NO3*factor(Month),data=slopet))#
anova(null_slope,nita_slope1,nita_slope2)#compare reduction in residual sums not AIC
#Nitrite
#So check first if month are random or is a structured important effect
summary(niti_sloper<-lmer(slo~NO2+(1|Month),data=slopet,REML=FALSE))#intercept random effect
summary(niti_slope1<-lm(slo~NO2,data=slopet))#fixed effect already checked formula and comparison
summary(niti_slope<-lm(slo~NO2+factor(Month),data=slopet))#fixed effect already checked formula and comparison
anova(niti_sloper,niti_slope, test="Chisq")#compare AIC
anova(niti_sloper,niti_slope1, test="Chisq")#compare AIC
#Months are important
summary(niti_slope1<-lm(slo~NO2+factor(Month),data=slopet))#NO2 is significant
summary(niti_slope2<-lm(slo~NO2*factor(Month),data=slopet))#
anova(null_slope,niti_slope1,niti_slope2)#compare reduction in residual sums not AIC
#Ammonium
#So check first if month are random or is a structured important effect
summary(amon_sloper<-lmer(slo~NH4+(1|Month),data=slopet,REML=FALSE))#intercept random effect
summary(amon_slope1<-lm(slo~NH4,data=slopet))#fixed effect already checked formula and comparison
summary(amon_slope<-lm(slo~NH4+factor(Month),data=slopet))#fixed effect already checked formula and comparison
anova(amon_sloper,amon_slope, test="Chisq")#compare AIC
anova(amon_sloper,amon_slope1, test="Chisq")#compare AIC
#Months are important
summary(amon_slope1<-lm(slo~NH4+factor(Month),data=slopet))#
summary(amon_slope2<-lm(slo~NH4+factor(Month),data=slopet))#
anova(null_slope,amon_slope1,amon_slope2)#compare reduction in residual sums not AIC
#Chlorophyll-a
#So check first if month are random or is a structured important effect
summary(chla_sloper<-lmer(slo~Chla+(1|Month),data=slopet,REML=FALSE))#intercept random effect
summary(chla_slope1<-lm(slo~Chla,data=slopet))#fixed effect already checked formula and comparison
summary(chla_slope<-lm(slo~Chla+factor(Month),data=slopet))#fixed effect already checked formula and comparison
anova(chla_sloper,chla_slope, test="Chisq")#compare AIC
anova(chla_sloper,chla_slope1, test="Chisq")#compare AIC
#Months are important
summary(chla_slope1<-lm(slo~Chla+factor(Month),data=slopet))#
summary(chla_slope2<-lm(slo~Chla+factor(Month),data=slopet))#
anova(null_slope,chla_slope1,chla_slope2)#
#Salinity
#So check first if month are random or is a structured important effect
summary(sal_sloper<-lmer(slo~Sal+(1|Month),data=slopet,REML=FALSE))#intercept random effect
summary(sal_slope1<-lm(slo~Sal,data=slopet))#fixed effect already checked formula and comparison
summary(sal_slope<-lm(slo~Sal+factor(Month),data=slopet))#fixed effect already checked formula and comparison
anova(sal_sloper,sal_slope, test="Chisq")#compare AIC
anova(sal_sloper,sal_slope1, test="Chisq")#compare AIC
#Months are important
summary(sal_slope1<-lm(slo~Sal+factor(Month),data=slopet))#
summary(sal_slope2<-lm(slo~Sal*factor(Month),data=slopet))#also significant for sal:month5
anova(null_slope,sal_slope1,sal_slope2)#
#Temperature
#So check first if month are random or is a structured important effect
summary(temp_sloper<-lmer(slo~Temperature+(1|Month),data=slopet,REML=FALSE))#intercept random effect
r.squaredGLMM(temp_sloper)#give r2 marginal and conditional, marginal account for fixed effect what is needed
summary(temp_slope1<-lm(slo~Temperature,data=slopet))#
summary(temp_slope<-lm(slo~Temperature+factor(Month),data=slopet))#fixed effect already checked formula and comparison
anova(temp_sloper,temp_slope, test="Chisq")#compare AIC
anova(temp_sloper,temp_slope1, test="Chisq")#compare AIC
#Months are important
summary(temp_slope1<-lm(slo~Temperature+factor(Month),data=slopet))#
summary(temp_slope2<-lm(slo~Temperature*factor(Month),data=slopet))#
anova(null_slope,temp_slope1,temp_slope2)#
plot(slopet$Temperature,slopet$slo)
#I muss use a subset
slopet_sub<-slopet[-which(slopet$Month==6),]
#Turbidity
#So check first if month are random or is a structured important effect
summary(ntu_sloper<-lmer(slo~NTU+(1|Month),data=slopet_sub,REML=FALSE))#intercept random effect
summary(ntu_slope1<-lm(slo~NTU,data=slopet_sub))#
summary(ntu_slope<-lm(slo~NTU+factor(Month),data=slopet_sub))#fixed effect already checked formula and comparison
anova(ntu_sloper,ntu_slope, test="Chisq")#compare AIC
anova(ntu_sloper,ntu_slope1, test="Chisq")#compare AIC
#Months are important
null_slopesub<-lm(slo~1,data=slopet_sub)#null model subset
#Months are important
summary(ntu_slope1<-lm(slo~NTU+factor(Month),data=slopet_sub))#
summary(ntu_slope2<-lm(slo~NTU*factor(Month),data=slopet_sub))#ntu:10
anova(null_slopesub,ntu_slope1,ntu_slope2)#
#Oxygen
#So check first if month are random or is a structured important effect
summary(oxy_sloper<-lmer(slo~DO+(1|Month),data=slopet_sub,REML=FALSE))#intercept random effect
summary(oxy_slope1<-lm(slo~DO,data=slopet_sub))#fixed effect already checked formula and comparison
summary(oxy_slope<-lm(slo~DO+factor(Month),data=slopet_sub))#fixed effect already checked formula and comparison
anova(oxy_sloper,oxy_slope, test="Chisq")#compare AIC
anova(oxy_sloper,oxy_slope1, test="Chisq")#compare AIC
#Months are important
summary(oxy_slope1<-lm(slo~DO+factor(Month),data=slopet_sub))#
summary(oxy_slope2<-lm(slo~DO*factor(Month),data=slopet_sub))#
summary(oxy_slope3<-lm(slo~DO,data=slopet_sub))#
summary(oxy_slope4<-lm(slo~factor(Month),data=slopet_sub))#
anova(null_slopesub,oxy_slope3,oxy_slope4,oxy_slope1,oxy_slope2)#
#Anova for the best
slope_nullr<-lmer(slo ~ (1 | Month), data=slopet_sub,REML=FALSE)
summary(tem_sloper<-lmer(slo~Temperature+(1|Month),data=slopet_sub,REML=FALSE))#intercept random effect
summary(niti_sloper<-lmer(slo~NO2+(1|Month),data=slopet_sub,REML=FALSE))#intercept random effect
summary(nita_sloper<-lmer(slo~NO3+(1|Month),data=slopet_sub,REML=FALSE))#intercept random effect
##no sub
slope_nullr<-lmer(slo ~ (1 | Month), data=slopet,REML=FALSE)
summary(tem_sloper<-lmer(slo~Temperature+(1|Month),data=slopet,REML=FALSE))#intercept random effect
summary(niti_sloper<-lmer(slo~NO2+(1|Month),data=slopet,REML=FALSE))#intercept random effect
summary(nita_sloper<-lmer(slo~NO3+(1|Month),data=slopet,REML=FALSE))#intercept random effect
summary(temniti_sloper<-lmer(slo~Temperature+NO2+(1|Month),data=slopet,REML=FALSE))#intercept random effect
summary(all_sloper<-lmer(slo~Temperature+NO2+NO3+(1|Month),data=slopet,REML=FALSE))#intercept random effect
anova(slope_nullr,tem_sloper,niti_sloper,nita_sloper,temniti_sloper,all_sloper)#compare AIC
anova(slope_nullr,tem_sloper,temniti_sloper)#compare AIC
#ONLY TEMPERATURE IS THE PARAMETER BETTER EXPLAINING THE SLOPE:STEEPER SLOPE WHEN LOWER TEMPERATURE

#Linear fit
r2t<-read.csv2('C:/Postdoc/Penguin/Data/Zooplancton/NBSS_r2_perstation.csv')
names(r2t)
r2t<-r2t[order(r2t$month),]#important order by month
r2<-as.numeric(as.character(r2t$r2))
group<-as.factor(c(rep(1,4),rep(2,4)))
fit<-data.frame(enviro,r2,group)
fit$Month<-factor(fit$Month)
splom(fit[3:13])#scatter plot matrix
#Null model for lm
null_fit<-lm(r2~1,data=fit)
summary(null_fit)
#Random intercepts: multiples responses by stations(spatial), by months(time), solve non-independence
#lmer is the equivalent to lm or glm
#Forward selection (Baayen et al 2008) in mixed models starting from the minimal intercepts-only structure
summary(null_fitr<-lmer(r2~(1|Station)+(1|Month)+(1|group),data=fit,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
summary(null_fitr2<-lmer(r2~(1|Month)+(1|group),data=fit,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
summary(null_fitr1<-lmer(r2~(1|Month),data=fit,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
anova(null_fitr1,null_fitr,null_fitr2)#all the residual variance is captured in month
#apparently there is not significant difference between models with just month and random effects on the station and group (structure autocorrelation)
#Phosphate, AIC negative, still search for the smaller value, is no matter positive or negative
#So check first if month are random or is a structured important effect
summary(phos_fitr<-lmer(r2~PO4+(1|Month),data=fit,REML=FALSE))#intercept random effect
summary(phos_fitr1<-lmer(r2~PO4+(1|Month)+(1|group),data=fit,REML=FALSE))#intercept random effect, not converge
summary(phos_fit1<-lm(r2~PO4,data=fit))#fixed effect already checked formula and comparison
summary(phos_fit<-lm(r2~PO4+factor(Month),data=fit))#fixed effect already checked formula and comparison
anova(phos_fitr,phos_fitr1,phos_fit)#the order is important so here must random effects first given
anova(phos_fitr,phos_fit1)#no difference between random or not random effect
#Months are important
summary(phos_fit1<-lm(r2~PO4+factor(Month),data=fit))#intercept random effect
summary(phos_fit2<-lm(r2~PO4*factor(Month),data=fit))#factor interactions
anova(null_fit,phos_fit1,phos_fit2)#best 3, order is important and null must given first
#Silicate
#So check first if month are random or is a structured important effect
summary(sili_fitr<-lmer(r2~Si+(1|Month),data=fit,REML=FALSE))#intercept random effect
summary(sili_fit1<-lm(r2~Si,data=fit))#
summary(sili_fit<-lm(r2~Si+factor(Month),data=fit))#fixed effect already checked formula and comparison
anova(sili_fitr,sili_fit)#compare AIC
anova(sili_fitr,sili_fit1)#compare AIC,no difference between random or not random effect 
#Months are important
summary(sili_fit1<-lm(r2~Si+factor(Month),data=fit))#
summary(sili_fit2<-lm(r2~Si*factor(Month),data=fit))#
anova(null_fit,sili_fit1,sili_fit2)#best 3, order is important and null must given first
plot(fit$Si,fit$r2)
#Nitrate
#So check first if month are random or is a structured important effect
summary(nita_fitr<-lmer(r2~NO3+(1|Month),data=fit,REML=FALSE))#intercept random effect
summary(nita_fit1<-lm(r2~NO3,data=fit))#fixed effect already checked formula and comparison
summary(nita_fit<-lm(r2~NO3+factor(Month),data=fit))#fixed effect already checked formula and comparison
anova(nita_fitr,nita_fit, test="Chisq")#compare AIC
anova(nita_fitr,nita_fit1, test="Chisq")#compare AIC,no difference between random or not random effect 
#Months are important
summary(nita_fit1<-lm(r2~NO3+factor(Month),data=fit))#
summary(nita_fit2<-lm(r2~NO3*factor(Month),data=fit))#
anova(null_fit,nita_fit1,nita_fit2)#compare reduction in residual sums not AIC
#Nitrite
#So check first if month are random or is a structured important effect
summary(niti_fitr<-lmer(r2~NO2+(1|Month),data=fit,REML=FALSE))#intercept random effect
summary(niti_fit1<-lm(r2~NO2,data=fit))#fixed effect already checked formula and comparison
summary(niti_fit<-lm(r2~NO2+factor(Month),data=fit))#fixed effect already checked formula and comparison
anova(niti_fitr,niti_fit, test="Chisq")#compare AIC
anova(niti_fitr,niti_fit1, test="Chisq")#compare AIC,no difference between random or not random effect 
#Months are important
summary(niti_fit1<-lm(r2~NO2+factor(Month),data=fit))#NO2 is significant
summary(niti_fit2<-lm(r2~NO2*factor(Month),data=fit))#
anova(null_fit,niti_fit1,niti_fit2)#compare reduction in residual sums not AIC
#Ammonium
#So check first if month are random or is a structured important effect
summary(amon_fitr<-lmer(r2~NH4+(1|Month),data=fit,REML=FALSE))#intercept random effect
summary(amon_fit1<-lm(r2~NH4,data=fit))#fixed effect already checked formula and comparison
summary(amon_fit<-lm(r2~NH4+factor(Month),data=fit))#fixed effect already checked formula and comparison
anova(amon_fitr,amon_fit, test="Chisq")#compare AIC
anova(amon_fitr,amon_fit1, test="Chisq")#compare AIC,no difference between random or not random effect 
#Months are important
summary(amon_fit1<-lm(r2~NH4+factor(Month),data=fit))#
summary(amon_fit2<-lm(r2~NH4+factor(Month),data=fit))#
anova(null_fit,amon_fit1,amon_fit2)#compare reduction in residual sums not AIC
#Chlorophyll-a
#So check first if month are random or is a structured important effect
summary(chla_fitr<-lmer(r2~Chla+(1|Month),data=fit,REML=FALSE))#intercept random effect
summary(chla_fit1<-lm(r2~Chla,data=fit))#fixed effect already checked formula and comparison
summary(chla_fit<-lm(r2~Chla+factor(Month),data=fit))#fixed effect already checked formula and comparison
anova(chla_fitr,chla_fit, test="Chisq")#compare AIC
anova(chla_fitr,chla_fit1, test="Chisq")#compare AIC,no difference between random or not random effect 
#Months are important
summary(chla_fit1<-lm(r2~Chla+factor(Month),data=fit))#
summary(chla_fit2<-lm(r2~Chla+factor(Month),data=fit))#
anova(null_fit,chla_fit1,chla_fit2)#
#Salinity
#So check first if month are random or is a structured important effect
summary(sal_fitr<-lmer(r2~Sal+(1|Month),data=fit,REML=FALSE))#intercept random effect
summary(sal_fit1<-lm(r2~Sal,data=fit))#fixed effect already checked formula and comparison
summary(sal_fit<-lm(r2~Sal+factor(Month),data=fit))#fixed effect already checked formula and comparison
anova(sal_fitr,sal_fit, test="Chisq")#compare AIC
anova(sal_fitr,sal_fit1, test="Chisq")#compare AIC,no difference between random or not random effect 
#Months are important
summary(sal_fit1<-lm(r2~Sal+factor(Month),data=fit))#
summary(sal_fit2<-lm(r2~Sal*factor(Month),data=fit))#also significant for sal:month5
anova(null_fit,sal_fit1,sal_fit2)#
#Temperature
#So check first if month are random or is a structured important effect
summary(temp_fitr<-lmer(r2~Temperature+(1|Month),data=fit,REML=FALSE))#intercept random effect
summary(temp_fit1<-lm(r2~Temperature,data=fit))#
summary(temp_fit<-lm(r2~Temperature+factor(Month),data=fit))#fixed effect already checked formula and comparison
anova(temp_fitr,temp_fit, test="Chisq")#compare AIC
anova(temp_fitr,temp_fit1, test="Chisq")#compare AIC,no difference between random or not random effect 
#Months are important
summary(temp_fit1<-lm(r2~Temperature+factor(Month),data=fit))#
summary(temp_fit2<-lm(r2~Temperature*factor(Month),data=fit))#
anova(null_fit,temp_fit1,temp_fit2)#
#I muss use a subset
fit_sub<-fit[-which(fit$Month==6),]
#Turbidity
#So check first if month are random or is a structured important effect
summary(ntu_fitr<-lmer(r2~NTU+(1|Month),data=fit_sub,REML=FALSE))#intercept random effect
summary(ntu_fit1<-lm(r2~NTU,data=fit_sub))#
summary(ntu_fit<-lm(r2~NTU+factor(Month),data=fit_sub))#fixed effect already checked formula and comparison
anova(ntu_fitr,ntu_fit, test="Chisq")#compare AIC,no difference between random or not random effect 
anova(ntu_fitr,ntu_fit1, test="Chisq")#compare AIC,no difference between random or not random effect 
#Months are important
null_fitsub<-lm(r2~1,data=fit_sub)#null model subset
#Months are important
summary(ntu_fit1<-lm(r2~NTU+factor(Month),data=fit_sub))#
summary(ntu_fit2<-lm(r2~NTU*factor(Month),data=fit_sub))#ntu:10
anova(null_fitsub,ntu_fit1,ntu_fit2)#no difference between random or not random effect 
#Oxygen
#So check first if month are random or is a structured important effect
summary(oxy_fitr<-lmer(r2~DO+(1|Month),data=fit_sub,REML=FALSE))#intercept random effect
summary(oxy_fit1<-lm(r2~DO,data=fit_sub))#fixed effect already checked formula and comparison
summary(oxy_fit<-lm(r2~DO+factor(Month),data=fit_sub))#fixed effect already checked formula and comparison
anova(oxy_fitr,oxy_fit, test="Chisq")#compare AIC
anova(oxy_fitr,oxy_fit1, test="Chisq")#compare AIC
#Months are important
summary(oxy_fit1<-lm(r2~DO+factor(Month),data=fit_sub))#
summary(oxy_fit2<-lm(r2~DO*factor(Month),data=fit_sub))#
summary(oxy_fit3<-lm(r2~DO,data=fit_sub))#
summary(oxy_fit4<-lm(r2~factor(Month),data=fit_sub))#
anova(null_fitsub,oxy_fit3,oxy_fit4,oxy_fit1,oxy_fit2)#no difference between random or not random effect 
#Anova for the best
fit_nullr<-lmer(r2 ~ (1 | Month), data=fit_sub,REML=FALSE)
summary(si_fitr<-lmer(r2~Si+(1|Month),data=fit_sub,REML=FALSE))#intercept random effect
summary(chla_fitr<-lmer(r2~Chla+(1|Month),data=fit_sub,REML=FALSE))#intercept random effect
summary(nita_fitr<-lmer(r2~NO3+(1|Month),data=fit_sub,REML=FALSE))#intercept random effect
##no sub
fit_nullr<-lmer(r2 ~ (1 | Month), data=fit,REML=FALSE)
summary(si_fitr<-lmer(r2~Si+(1|Month),data=fit,REML=FALSE))#intercept random effect
summary(chla_fitr<-lmer(r2~Chla+(1|Month),data=fit,REML=FALSE))#intercept random effect
summary(sichla_fitr<-lmer(r2~Si+Chla+(1|Month),data=fit,REML=FALSE))#intercept random effect
anova(fit_nullr,si_fitr,chla_fitr,sichla_fitr)#compare AIC
#NONE OF THEM CAN EXPLAIN THE FIT OF SLOPE


#normalized size diversity
sdive<-read.csv2('C:/Postdoc/Penguin/Data/Zooplancton/size_diversity.csv')
names(sdive)#already standardized
sdive<-sdive[order(sdive$month),]#important order by month
sdiv<-as.numeric(as.character(sdive$Std.Shan.size))
group<-as.factor(c(rep(1,4),rep(2,4)))
sdivt<-data.frame(enviro,sdiv,group)
sdivt$Month<-factor(sdivt$Month)
splom(sdivt[3:13])#scatter plot matrix
#Null model for lm
null_sdiv<-lm(sdiv~1,data=sdivt)
summary(null_sdiv)
#Random intercepts: multiples responses by stations(spatial), by months(time), solve non-independence
#lmer is the equivalent to lm or glm
#Forward selection (Baayen et al 2008) in mixed models starting from the minimal intercepts-only structure
summary(null_sdivr<-lmer(sdiv~(1|Station)+(1|Month)+(1|group),data=sdivt,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
summary(null_sdivr2<-lmer(sdiv~(1|Month)+(1|group),data=sdivt,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
summary(null_sdivr1<-lmer(sdiv~(1|Month),data=sdivt,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
anova(null_sdivr1,null_sdivr,null_sdivr2)#all the residual variance is captured in month
#apparently there is not significant difference between models with just month and random effects on the station and group (structure autocorrelation)
#Phosphate, 
#So check first if month are random or is a structured important effect
summary(phos_sdivr<-lmer(sdiv~PO4+(1|Month),data=sdivt,REML=FALSE))#intercept random effect
summary(phos_sdivr1<-lmer(sdiv~PO4+(1|Month)+(1|group),data=sdivt,REML=FALSE))#intercept random effect, not converge
summary(phos_sdiv1<-lm(sdiv~PO4,data=sdivt))#fixed effect already checked formula and comparison
summary(phos_sdiv<-lm(sdiv~PO4+factor(Month),data=sdivt))#fixed effect already checked formula and comparison
anova(phos_sdivr,phos_sdivr1,phos_sdiv)#the order is important so here must random effects first given
anova(phos_sdivr,phos_sdiv1)#no difference between random or not random effect
#Months are important
summary(phos_sdiv1<-lm(sdiv~PO4+factor(Month),data=sdivt))#intercept random effect
summary(phos_sdiv2<-lm(sdiv~PO4*factor(Month),data=sdivt))#factor interactions
anova(null_sdiv,phos_sdiv1,phos_sdiv2)#best 3, order is important and null must given first
#Silicate
#So check first if month are random or is a structured important effect
summary(sili_sdivr<-lmer(sdiv~Si+(1|Month),data=sdivt,REML=FALSE))#intercept random effect
summary(sili_sdiv1<-lm(sdiv~Si,data=sdivt))#
summary(sili_sdiv<-lm(sdiv~Si+factor(Month),data=sdivt))#fixed effect already checked formula and comparison
anova(sili_sdivr,sili_sdiv)#compare AIC
anova(sili_sdivr,sili_sdiv1)#compare AIC
#Months are important
summary(sili_sdiv1<-lm(sdiv~Si+factor(Month),data=sdivt))#
summary(sili_sdiv2<-lm(sdiv~Si*factor(Month),data=sdivt))#
anova(null_sdiv,sili_sdiv1,sili_sdiv2)#best 3, order is important and null must given first
plot(sdivt$Si,sdivt$sdiv)
#Nitrate
#So check first if month are random or is a structured important effect
summary(nita_sdivr<-lmer(sdiv~NO3+(1|Month),data=sdivt,REML=FALSE))#intercept random effect
summary(nita_sdiv1<-lm(sdiv~NO3,data=sdivt))#fixed effect already checked formula and comparison
summary(nita_sdiv<-lm(sdiv~NO3+factor(Month),data=sdivt))#fixed effect already checked formula and comparison
anova(nita_sdivr,nita_sdiv, test="Chisq")#compare AIC
anova(nita_sdivr,nita_sdiv1, test="Chisq")#compare AIC
#Months are important
summary(nita_sdiv1<-lm(sdiv~NO3+factor(Month),data=sdivt))#
summary(nita_sdiv2<-lm(sdiv~NO3*factor(Month),data=sdivt))#
anova(null_sdiv,nita_sdiv1,nita_sdiv2)#compare reduction in residual sums not AIC
#Nitrite
#So check first if month are random or is a structured important effect
summary(niti_sdivr<-lmer(sdiv~NO2+(1|Month),data=sdivt,REML=FALSE))#intercept random effect
summary(niti_sdiv1<-lm(sdiv~NO2,data=sdivt))#fixed effect already checked formula and comparison
summary(niti_sdiv<-lm(sdiv~NO2+factor(Month),data=sdivt))#fixed effect already checked formula and comparison
anova(niti_sdivr,niti_sdiv, test="Chisq")#compare AIC
anova(niti_sdivr,niti_sdiv1, test="Chisq")#compare AIC
#Months are important
summary(niti_sdiv1<-lm(sdiv~NO2+factor(Month),data=sdivt))#
summary(niti_sdiv2<-lm(sdiv~NO2*factor(Month),data=sdivt))#
anova(null_sdiv,niti_sdiv1,niti_sdiv2)#compare reduction in residual sums not AIC
#Ammonium
#So check first if month are random or is a structured important effect
summary(amon_sdivr<-lmer(sdiv~NH4+(1|Month),data=sdivt,REML=FALSE))#intercept random effect
summary(amon_sdiv1<-lm(sdiv~NH4,data=sdivt))#fixed effect already checked formula and comparison
summary(amon_sdiv<-lm(sdiv~NH4+factor(Month),data=sdivt))#fixed effect already checked formula and comparison
anova(amon_sdivr,amon_sdiv, test="Chisq")#compare AIC
anova(amon_sdivr,amon_sdiv1, test="Chisq")#compare AIC
#Months are important
summary(amon_sdiv1<-lm(sdiv~NH4+factor(Month),data=sdivt))#
summary(amon_sdiv2<-lm(sdiv~NH4+factor(Month),data=sdivt))#
anova(null_sdivt,amon_sdiv1,amon_sdiv2)#compare reduction in residual sums not AIC
#Chlorophyll-a
#So check first if month are random or is a structured important effect
summary(chla_sdivr<-lmer(sdiv~Chla+(1|Month),data=sdivt,REML=FALSE))#intercept random effect
summary(chla_sdiv1<-lm(sdiv~Chla,data=sdivt))#fixed effect already checked formula and comparison
summary(chla_sdiv<-lm(sdiv~Chla+factor(Month),data=sdivt))#fixed effect already checked formula and comparison
anova(chla_sdivr,chla_sdiv, test="Chisq")#compare AIC
anova(chla_sdivr,chla_sdiv1, test="Chisq")#compare AIC 
#Months are important
summary(chla_sdiv1<-lm(sdiv~Chla+factor(Month),data=sdivt))#
summary(chla_sdiv2<-lm(sdiv~Chla+factor(Month),data=sdivt))#
anova(null_sdiv,chla_sdiv1,chla_sdiv2)#
#Salinity
#So check first if month are random or is a structured important effect
summary(sal_sdivr<-lmer(sdiv~Sal+(1|Month),data=sdivt,REML=FALSE))#intercept random effect
summary(sal_sdiv1<-lm(sdiv~Sal,data=sdivt))#fixed effect already checked formula and comparison
summary(sal_sdiv<-lm(sdiv~Sal+factor(Month),data=sdivt))#fixed effect already checked formula and comparison
anova(sal_sdivr,sal_sdiv, test="Chisq")#compare AIC
anova(sal_sdivr,sal_sdiv1, test="Chisq")#compare AIC
#Months are important
summary(sal_sdiv1<-lm(sdiv~Sal+factor(Month),data=sdivt))#
summary(sal_sdiv2<-lm(sdiv~Sal*factor(Month),data=sdivt))#
anova(null_sdiv,sal_sdiv1,sal_sdiv2)#
#Temperature
#So check first if month are random or is a structured important effect
summary(temp_sdivr<-lmer(sdiv~Temperature+(1|Month),data=sdivt,REML=FALSE))#intercept random effect
r.squaredGLMM(temp_sdivr)#give r2 marginal and conditional, marginal account for fixed effect what is needed
summary(temp_sdiv1<-lm(sdiv~Temperature,data=sdivt))#
summary(temp_sdiv<-lm(sdiv~Temperature+factor(Month),data=sdivt))#fixed effect already checked formula and comparison
anova(temp_sdivr,temp_sdiv, test="Chisq")#compare AIC
anova(temp_sdivr,temp_sdiv1, test="Chisq")#compare AIC 
#Months are important
summary(temp_sdiv1<-lm(sdiv~Temperature+factor(Month),data=sdivt))#
summary(temp_sdiv2<-lm(sdiv~Temperature*factor(Month),data=sdivt))#
anova(null_sdiv,temp_sdiv1,temp_sdiv2)#
#I muss use a subset
sdivt_sub<-sdivt[-which(sdivt$Month==6),]
#Turbidity
#So check first if month are random or is a structured important effect
summary(ntu_sdivr<-lmer(sdiv~NTU+(1|Month),data=sdivt_sub,REML=FALSE))#intercept random effect
r.squaredGLMM(ntu_sdivr)#give r2 marginal and conditional, marginal account for fixed effect what is needed
summary(ntu_sdiv1<-lm(sdiv~NTU,data=sdivt_sub))#
summary(ntu_sdiv<-lm(sdiv~NTU+factor(Month),data=sdivt_sub))#fixed effect already checked formula and comparison
anova(ntu_sdivr,ntu_sdiv, test="Chisq")#compare AIC
anova(ntu_sdivr,ntu_sdiv1, test="Chisq")#compare AIC
#Months are important
null_sdivsub<-lm(sdiv~1,data=sdivt_sub)#null model subset
#Months are important
summary(ntu_sdiv1<-lm(sdiv~NTU+factor(Month),data=sdivt_sub))#
summary(ntu_sdiv2<-lm(sdiv~NTU*factor(Month),data=sdivt_sub))#ntu:10
anova(null_sdivsub,ntu_sdiv1,ntu_sdiv2)#
#Oxygen
#So check first if month are random or is a structured important effect
summary(oxy_sdivr<-lmer(sdiv~DO+(1|Month),data=sdivt_sub,REML=FALSE))#intercept random effect
summary(oxy_sdiv1<-lm(sdiv~DO,data=sdivt_sub))#fixed effect already checked formula and comparison
summary(oxy_sdiv<-lm(sdiv~DO+factor(Month),data=sdivt_sub))#fixed effect already checked formula and comparison
anova(oxy_sdivr,oxy_sdiv, test="Chisq")#compare AIC
anova(oxy_sdivr,oxy_sdiv1, test="Chisq")#compare AIC
#Months are important
summary(oxy_sdiv1<-lm(sdiv~DO+factor(Month),data=sdivt_sub))#
summary(oxy_sdiv2<-lm(sdiv~DO*factor(Month),data=sdivt_sub))#
summary(oxy_sdiv3<-lm(sdiv~DO,data=sdivt_sub))#
summary(oxy_sdiv4<-lm(sdiv~factor(Month),data=sdivt_sub))#
anova(null_sdivsub,oxy_sdiv3,oxy_sdiv4,oxy_sdiv1,oxy_sdiv2)#
#Anova for the best
sdiv_nullr<-lmer(sdiv ~ (1 | Month), data=sdivt_sub,REML=FALSE)
summary(temp_sdivr<-lmer(sdiv~Temperature+(1|Month),data=sdivt_sub,REML=FALSE))#intercept random effect
r.squaredGLMM(temp_sdivr)#give r2 marginal and conditional, marginal account for fixed effect what is needed
summary(amon_sdivr<-lmer(sdiv~NH4+(1|Month),data=sdivt_sub,REML=FALSE))#intercept random effect
summary(niti_sdivr<-lmer(sdiv~NO2+(1|Month),data=sdivt_sub,REML=FALSE))#intercept random effect
summary(tempntu_sdivr<-lmer(sdiv~Temperature+NTU+(1|Month),data=sdivt_sub,REML=FALSE))#intercept random effect
r.squaredGLMM(tempntu_sdivr)#give r2 marginal and conditional, marginal account for fixed effect what is needed
anova(sdiv_nullr,temp_sdivr,niti_sdivr,tempntu_sdivr)#compare AIC
anova(sdiv_nullr,temp_sdivr,tempntu_sdivr)#compare AIC
plot(sdivt$Temperature,sdivt$sdiv)
plot(sdivt$NTU,sdivt$sdiv)
#BETTER MODDELS ARE SIZE DIVERSITY~TEMPERATURE+NTU+(1|MONTH) AND DIVERSITY~TEMPERATURE+(1|MONTH)
#LOWER TEMPERATURE MORE SIZE DIVERSITY; HIGHER TURBIDITY HIGHER SIZE DIVERSITY. 
#IF WE CONSIDER MONTHS IS DURING AUTUMN AN WINTER THAT PREDICTS BETTER TURBIDITY 
#AND DURING SUMMER AND WINTER THAT PREDICTS BETTER TEMPERATURE

#normalized taxonomic diversity
tdive<-read.csv2('C:/Postdoc/Penguin/Data/Zooplancton/abundance_diversity.csv')
names(tdive)#already standardized
tdive<-tdive[order(tdive$month),]#important order by month
tdiv<-as.numeric(as.character(tdive$Shanon_std))
group<-as.factor(c(rep(1,4),rep(2,4)))
tdivt<-data.frame(enviro,tdiv,group)
tdivt$Month<-factor(tdivt$Month)
splom(tdivt[3:13])#scatter plot matrix
#Null model for lm
null_tdiv<-lm(tdiv~1,data=tdivt)
summary(null_tdiv)
#Random intercepts: multiples responses by stations(spatial), by months(time), solve non-independence
#lmer is the equivalent to lm or glm
#Forward selection (Baayen et al 2008) in mixed models starting from the minimal intercepts-only structure
summary(null_tdivr<-lmer(tdiv~(1|Station)+(1|Month)+(1|group),data=tdivt,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
summary(null_tdivr2<-lmer(tdiv~(1|Month)+(1|group),data=tdivt,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
summary(null_tdivr1<-lmer(tdiv~(1|Month),data=tdivt,REML=FALSE))#index can not be rnom effect because random factor must be< number of observations
anova(null_tdivr1,null_tdivr,null_tdivr2)#all the residual variance is captured in month
#apparently there is not significant difference between models with just month and random effects on the station and group (structure autocorrelation)
#Phosphate, 
#So check first if month are random or is a structured important effect
summary(phos_tdivr<-lmer(tdiv~PO4+(1|Month),data=tdivt,REML=FALSE))#intercept random effect
summary(phos_tdivr1<-lmer(tdiv~PO4+(1|Month)+(1|group),data=tdivt,REML=FALSE))#intercept random effect, not converge
summary(phos_tdiv1<-lm(tdiv~PO4,data=tdivt))#fixed effect already checked formula and comparison
summary(phos_tdiv<-lm(tdiv~PO4+factor(Month),data=tdivt))#fixed effect already checked formula and comparison
anova(phos_tdivr,phos_tdivr1,phos_tdiv)#the order is important so here must random effects first given
anova(phos_tdivr,phos_tdiv1)#no difference between random or not random effect
#Months are important
summary(phos_tdiv1<-lm(tdiv~PO4+factor(Month),data=tdivt))#intercept random effect
summary(phos_tdiv2<-lm(tdiv~PO4*factor(Month),data=tdivt))#factor interactions
anova(null_tdiv,phos_tdiv1,phos_tdiv2)#best 3, order is important and null must given first
#Silicate
#So check first if month are random or is a structured important effect
summary(sili_tdivr<-lmer(tdiv~Si+(1|Month),data=tdivt,REML=FALSE))#intercept random effect
r.squaredGLMM(sili_tdivr)#give r2 marginal and conditional, marginal account for fixed effect what is needed
summary(sili_tdiv1<-lm(tdiv~Si,data=tdivt))#
summary(sili_tdiv<-lm(tdiv~Si+factor(Month),data=tdivt))#fixed effect already checked formula and comparison
anova(sili_tdivr,sili_tdiv)#compare AIC
anova(sili_tdivr,sili_tdiv1)#compare AIC
#Months are important
summary(sili_tdiv1<-lm(tdiv~Si+factor(Month),data=tdivt))#
summary(sili_tdiv2<-lm(tdiv~Si*factor(Month),data=tdivt))#
anova(null_tdiv,sili_tdiv1,sili_tdiv2)#best 3, order is important and null must given first
#Nitrate
#So check first if month are random or is a structured important effect
summary(nita_tdivr<-lmer(tdiv~NO3+(1|Month),data=tdivt,REML=FALSE))#intercept random effect
summary(nita_tdiv1<-lm(tdiv~NO3,data=tdivt))#fixed effect already checked formula and comparison
summary(nita_tdiv<-lm(tdiv~NO3+factor(Month),data=tdivt))#fixed effect already checked formula and comparison
anova(nita_tdivr,nita_tdiv, test="Chisq")#compare AIC
anova(nita_tdivr,nita_tdiv1, test="Chisq")#compare AIC
#Months are important
summary(nita_tdiv1<-lm(tdiv~NO3+factor(Month),data=tdivt))#
summary(nita_tdiv2<-lm(tdiv~NO3*factor(Month),data=tdivt))#
anova(null_tdiv,nita_tdiv1,nita_tdiv2)#compare reduction in residual sums not AIC
#Nitrite
#So check first if month are random or is a structured important effect
summary(niti_tdivr<-lmer(tdiv~NO2+(1|Month),data=tdivt,REML=FALSE))#intercept random effect
summary(niti_tdiv1<-lm(tdiv~NO2,data=tdivt))#fixed effect already checked formula and comparison
summary(niti_tdiv<-lm(tdiv~NO2+factor(Month),data=tdivt))#fixed effect already checked formula and comparison
anova(niti_tdivr,niti_tdiv, test="Chisq")#compare AIC
anova(niti_tdivr,niti_tdiv1, test="Chisq")#compare AIC
#Months are important
summary(niti_tdiv1<-lm(tdiv~NO2+factor(Month),data=tdivt))#NO2 is significant
summary(niti_tdiv2<-lm(tdiv~NO2*factor(Month),data=tdivt))#
anova(null_tdiv,niti_tdiv1,niti_tdiv2)#compare reduction in residual sums not AIC
#Ammonium
#So check first if month are random or is a structured important effect
summary(amon_tdivr<-lmer(tdiv~NH4+(1|Month),data=tdivt,REML=FALSE))#intercept random effect
summary(amon_tdiv1<-lm(tdiv~NH4,data=tdivt))#fixed effect already checked formula and comparison
summary(amon_tdiv<-lm(tdiv~NH4+factor(Month),data=tdivt))#fixed effect already checked formula and comparison
anova(amon_tdivr,amon_tdiv, test="Chisq")#compare AIC
anova(amon_tdivr,amon_tdiv1, test="Chisq")#compare AIC
#Months are important
summary(amon_tdiv1<-lm(tdiv~NH4+factor(Month),data=tdivt))#
summary(amon_tdiv2<-lm(tdiv~NH4+factor(Month),data=tdivt))#
anova(null_tdiv,amon_tdiv1,amon_tdiv2)#compare reduction in residual sums not AIC
#Chlorophyll-a
#So check first if month are random or is a structured important effect
summary(chla_tdivr<-lmer(tdiv~Chla+(1|Month),data=tdivt,REML=FALSE))#intercept random effect
summary(chla_tdiv1<-lm(tdiv~Chla,data=tdivt))#fixed effect already checked formula and comparison
summary(chla_tdiv<-lm(tdiv~Chla+factor(Month),data=tdivt))#fixed effect already checked formula and comparison
anova(chla_tdivr,chla_tdiv, test="Chisq")#compare AIC
anova(chla_tdivr,chla_tdiv1, test="Chisq")#compare AIC,no difference between random or not random effect 
#Months are important
summary(chla_tdiv1<-lm(tdiv~Chla+factor(Month),data=tdivt))#
summary(chla_tdiv2<-lm(tdiv~Chla*factor(Month),data=tdivt))#
anova(null_tdiv,chla_tdiv1,chla_tdiv2)#
#Salinity
#So check first if month are random or is a structured important effect
summary(sal_tdivr<-lmer(tdiv~Sal+(1|Month),data=tdivt,REML=FALSE))#intercept random effect
r.squaredGLMM(sal_tdivr)#give r2 marginal and conditional, marginal account for fixed effect what is needed
summary(sal_tdiv1<-lm(tdiv~Sal,data=tdivt))#fixed effect already checked formula and comparison
summary(sal_tdiv<-lm(tdiv~Sal+factor(Month),data=tdivt))#fixed effect already checked formula and comparison
anova(sal_tdivr,sal_tdiv, test="Chisq")#compare AIC
anova(sal_tdivr,sal_tdiv1, test="Chisq")#compare AIC,no difference between random or not random effect 
#Months are important
summary(sal_tdiv1<-lm(tdiv~Sal+factor(Month),data=tdivt))#
summary(sal_tdiv2<-lm(tdiv~Sal*factor(Month),data=tdivt))#also significant for sal:month5
anova(null_tdiv,sal_tdiv1,sal_tdiv2)#
#Temperature
#So check first if month are random or is a structured important effect
summary(temp_tdivr<-lmer(tdiv~Temperature+(1|Month),data=tdivt,REML=FALSE))#intercept random effect
summary(temp_tdiv1<-lm(tdiv~Temperature,data=tdivt))#
summary(temp_tdiv<-lm(tdiv~Temperature+factor(Month),data=tdivt))#fixed effect already checked formula and comparison
anova(temp_tdivr,temp_tdiv, test="Chisq")#compare AIC
anova(temp_tdivr,temp_tdiv1, test="Chisq")#compare AIC,no difference between random or not random effect 
#Months are important
summary(temp_tdiv1<-lm(tdiv~Temperature+factor(Month),data=tdivt))#
summary(temp_tdiv2<-lm(tdiv~Temperature*factor(Month),data=tdivt))#
anova(null_tdiv,temp_tdiv1,temp_tdiv2)#
#I muss use a subset
tdivt_sub<-tdivt[-which(tdivt$Month==6),]
#Turbidity
#So check first if month are random or is a structured important effect
summary(ntu_tdivr<-lmer(tdiv~NTU+(1|Month),data=tdivt_sub,REML=FALSE))#intercept random effect
summary(ntu_tdiv1<-lm(tdiv~NTU,data=tdivt_sub))#
summary(ntu_tdiv<-lm(tdiv~NTU+factor(Month),data=tdivt_sub))#fixed effect already checked formula and comparison
anova(ntu_tdivr,ntu_tdiv, test="Chisq")#compare AIC,no difference between random or not random effect 
anova(ntu_tdivr,ntu_tdiv1, test="Chisq")#compare AIC,no difference between random or not random effect 
#Months are important
null_tdivsub<-lm(tdiv~1,data=tdivt_sub)#null model subset
#Months are important
summary(ntu_tdiv1<-lm(tdiv~NTU+factor(Month),data=tdivt_sub))#
summary(ntu_tdiv2<-lm(tdiv~NTU*factor(Month),data=tdivt_sub))#
anova(null_tdivsub,ntu_tdiv1,ntu_tdiv2)#no difference between random or not random effect 
#Oxygen
#So check first if month are random or is a structured important effect
summary(oxy_tdivr<-lmer(tdiv~DO+(1|Month),data=tdivt_sub,REML=FALSE))#intercept random effect
summary(oxy_tdiv1<-lm(tdiv~DO,data=tdivt_sub))#fixed effect already checked formula and comparison
summary(oxy_tdiv<-lm(tdiv~DO+factor(Month),data=tdivt_sub))#fixed effect already checked formula and comparison
anova(oxy_tdivr,oxy_tdiv, test="Chisq")#compare AIC
anova(oxy_tdivr,oxy_tdiv1, test="Chisq")#compare AIC
#Months are important
summary(oxy_tdiv1<-lm(tdiv~DO+factor(Month),data=tdivt_sub))#
summary(oxy_tdiv2<-lm(tdiv~DO*factor(Month),data=tdivt_sub))#
summary(oxy_tdiv3<-lm(tdiv~DO,data=tdivt_sub))#
summary(oxy_tdiv4<-lm(tdiv~factor(Month),data=tdivt_sub))#
anova(null_tdivsub,oxy_tdiv3,oxy_tdiv4,oxy_tdiv1,oxy_tdiv2)#no difference between random or not random effect 
#Anova for the best# urgent anova every parameter
tdiv_nullr<-lmer(tdiv ~ (1 | Month), data=tdivt_sub,REML=FALSE)
summary(si_tdivr<-lmer(tdiv~Si+(1|Month),data=tdivt_sub,REML=FALSE))#intercept random effect
summary(sal_tdivr<-lmer(tdiv~Sal+(1|Month),data=tdivt_sub,REML=FALSE))#intercept random effect
summary(sisal_tdivr<-lmer(tdiv~Si+Sal+(1|Month),data=tdivt_sub,REML=FALSE))#intercept random effect
anova(tdiv_nullr,si_tdivr,sal_tdivr,sisal_tdivr)#compare AIC
anova(tdiv_nullr,si_tdivr,sisal_tdivr)#compare AIC
plot(tdivt$Si,tdivt$tdiv)
plot(tdivt$Sal,tdivt$tdiv)
plot(sisal_tdivr)
#not sub
tdiv_nullr<-lmer(tdiv ~ (1 | Month), data=tdivt,REML=FALSE)
summary(si_tdivr<-lmer(tdiv~Si+(1|Month),data=tdivt,REML=FALSE))#intercept random effect
r.squaredGLMM(si_tdivr)#give r2 marginal and conditional, marginal account for fixed effect what is needed
summary(sal_tdivr<-lmer(tdiv~Sal+(1|Month),data=tdivt,REML=FALSE))#intercept random effect
r.squaredGLMM(sal_tdivr)#give r2 marginal and conditional, marginal account for fixed effect what is needed
summary(sisal_tdivr<-lmer(tdiv~Si+Sal+(1|Month),data=tdivt,REML=FALSE))#intercept random effect
r.squaredGLMM(sisal_tdivr)#give r2 marginal and conditional, marginal account for fixed effect what is needed
anova(tdiv_nullr,si_tdivr,sal_tdivr,sisal_tdivr)#compare AIC
anova(tdiv_nullr,sal_tdivr,sisal_tdivr)#compare AIC
#NOTHING CAN EXPLAIN TAXONOMIC DIVERSITY

#WHAT CAN EXPLAIN LINEAR FIT AND TAXONOMIC DIVERSITY?
sdive<-read.csv2('C:/Postdoc/Penguin/Data/Zooplancton/size_diversity.csv')
names(sdive)#already standardized
sdive<-sdive[order(sdive$month),]#important order by month
sdiv<-as.numeric(as.character(sdive$Std.Shan.size))
tdive<-read.csv2('C:/Postdoc/Penguin/Data/Zooplancton/abundance_diversity.csv')
names(tdive)#already standardized
tdive<-tdive[order(tdive$month),]#important order by month
tdiv<-as.numeric(as.character(tdive$Shanon_std))
Abund_log<-as.numeric(as.character(tdive$Abund_log))
group<-as.factor(c(rep(1,4),rep(2,4)))
r2t<-read.csv2('C:/Postdoc/Penguin/Data/Zooplancton/NBSS_r2_perstation.csv')
names(r2t)
r2t<-r2t[order(r2t$month),]#important order by month
r2<-as.numeric(as.character(r2t$r2))
slo<-as.numeric(as.character(r2t$NBSS_slope))
taxa_fit<-data.frame(tdiv,r2,Abund_log,slo,sdiv,group)
taxa_fit$Month<-factor(r2t$month)
#Null model for lm
summary(null_fitr<-lmer(r2~1+(1|Month),data=taxa_fit,REML=FALSE))
summary(taxo_fitr<-lmer(r2~tdiv+(1|Month),data=taxa_fit,REML=FALSE))#intercept random effect
anova(null_fitr,taxo_fitr)#compare AIC,no difference between random or not random effect 

summary(null_fitr<-lmer(tdiv~1+(1|Month),data=taxa_fit,REML=FALSE))
summary(taxo_fitr<-lmer(tdiv~r2+(1|Month),data=taxa_fit,REML=FALSE))#intercept random effect
summary(taxo_fitr<-lme(tdiv~r2,random=~1|Month,data=taxa_fit,na.action = na.omit))#intercept random effect
anova(null_fitr,taxo_fitr)#compare AIC,no difference between random or not random effect 
summary(taxo_fitr<-lmer(sdiv~slo+(1|Month),data=taxa_fit,REML=FALSE))#intercept random effect
anova(null_fitr,taxo_fitr)#compare AIC,no difference between random or not random effect 
summary(taxo_fitr<-lme(sdiv~slo,random=~1|Month,data=taxa_fit,na.action = na.omit))#intercept random effect

plot(taxa_fit$tdiv,taxa_fit$r2)
plot(taxa_fit$tdiv,taxa_fit$slo)
plot(taxa_fit$tdiv,taxa_fit$slo)
plot(taxa_fit$sdiv,taxa_fit$slo)
#Canonical correspondance analysis#######
#is, not possible to use here because response variable
#are measured in different and arbitrary units not really suitable for other
#than community data. The infamous double-standardization of CA
#can balance scarce and abundant species, but it cannot adjust variables in
#different units. CA and CCA are based on row profiles, or allocation of row
#totals among columns in proportions of column totals. This is not meaningful
#if your columns are not expressed in equal and comparable units. Therefore I
#recommend using rda with scaling (Jari Oksanen)
response<-total %>% transmute(r2,intercept,slope,size_div,abun_log,biov_log)%>%
  tidyr::fill(r2,intercept,slope,size_div,abun_log,biov_log)#fill with value before beacuse rda do not acept NAs for response variable
chart.Correlation(response, histogram=TRUE, pch="+")#corr zoo community index
#now rda
rdazoo<- rda(response~PO4+Si+NO3+NO2+NH4+Chl_a+Temperature,
             data=enviro,na.action=na.exclude,scale=TRUE)
plot(rdazoo)
ordiR2step(rda(response~1,
               data=enviro,na.action=na.omit,scale=TRUE), scope= formula(rdazoo),
           direction= "forward", R2scope=TRUE, pstep=1000)#doesnt work with ntu and do
env.signif <- subset(enviro, select = c("Si", "NH4", "NO3", "NO2","Chl_a"))
spe.rda.signif <- rda(response~., data=env.signif)
summary(spe.rda.signif, display=NULL)
(R2adj <- RsquareAdj(spe.rda.signif)$adj.r.squared)
anova.cca(rdazoo, step=1000)
####After exploration decided not to go further to partial RDA, first because to do forward
#selection we have to eliminate DO and NTU, ordi2step doesn work with na.omit or exclude. Second
#beacuse I am not interested in the most important variables to describe the ensamble of zooplankton
#index, some of them are already highly correlated (intercept~slope, intercept~biovol), my objective is
#variable by variable which are the covariates explaining better their trend
