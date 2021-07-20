# Functions for computing size diversity
# version April 2018

# PLEASE FOLLOW THE INSTRUCTIONS LISTED BETWEEN LINES 22 AND 70

# Functions based on previous versions and the papers:
#
# Quintana, X. D., S. Brucet, D. Boix, R. L?pez-Flores, 
# S. Gasc?n, A. Badosa, J. Sala, R. Moreno-Amich and 
# J. J. Egozcue: A non-parametric method for the 
# measurement of size diversity, with emphasis on 
# data standardisation. Limnology and Oceanography: 
# Methods, 6, 75-86 and appendices A, B, 2008
#
# X. D. Quintana, J. J. Egozcue, O. Mart?nez-Abella, 
# R. L?pez-Flores, S. Gasc?n, S. Brucet and D. Boix: 
# Update: A non-parametric method for the measurement 
# of size diversity, with emphasis on data 
# standardization. The measurement of the size evenness, 
# Limnology and Oceanography: Methods, 14, 408-413, 2016

########################################
# INSTRUCTIONS

# 1)  Run the script bellow from line 70 untill the end of the script

# 2a) Prepare an excel file (or directly an R file) with three columns:
#     the sample identifier, de body size and the abundance. 
#     Variable names must be, respectively: "SampleID", "x" and "abund".

# 2b) Eventually you may have no abundances, that is, 
#     every size measure belongs to a single individual;
#     in this case, prepare a file with only two columns: "SampleID" and "x"

# 3)  Enter these data in an R file called "div.data", 
#     for example using the "read.table" command:
div.data<- read.table("clipboard",header=T,sep="\t")
#     A new R file named "div.data" should be created

# 4)  Be sure that your data set do not have
#     zeros, negative or missing values, samples with a single measurement... 

# 5)  Check if your data set is correct:
summary (div.data)

# 6) Introduce in line 56 the dimensionality of your sample. DO NOT FORGET THIS STEP!
#         "measuredim=1" when your body size data are lengths (L),
#         "measuredim=2 when your body size data are recovering (L^2) or
#         "measuredim=3" if your body size data are biomass units (L^3) (e.g. dry weight, carbon, biovolume.)

# 7) Run lines 51 to 59:
noms=names(table(div.data$SampleID))
res1=matrix(NA,length(noms),7)

for (i in 1:(length(noms))){ res1[i,]=sizeDiversity2017(x=div.data[div.data$SampleID==noms[i],]$x,abund=div.data[div.data$SampleID==noms[i],]$abund,
                             measuredim=3,gnormaliz=1)}
rownames(res1)=noms
colnames(res1)=c("diversity2","eevenness", "logNdiversity2","bandker", "devxlog","gmeanx", "meanx")

# 8) Check the results by running next line 
res1

# 9) If you prefer, convert the results to a data frame 
res2<-data.frame(res1)





##############################################################
#########################################
### function logNormal_Entropy computes the entropy of
# a log-normal distribution given its 
# logarithmic standard deviation and logarithmic mean
# logarithmic mean is taken equal to 0 by default,
# which corresponds to standardization by dividing
# by the geometric mean of the data.
#########################################
logNormal_Entropy<-function(desvlog,meanlog=0) 
{
  ctelog <- log(desvlog * sqrt(2.0*pi))
  hexact <- ctelog + 0.5 + meanlog
  return(hexact)
}
#########################################

#########################################
#### function MCkerentropy_abund computes entropy using
# a sample of sizes with abundances or absolute frequencies.
# y     are the values of the logarithm of size
#       logarithms have to be applied beforehand
# abund contains the absolute frequencies 
#       of each logarithmic value
#       it can be fractional
# if data were not normalized  ymean should be added 
# normalization is equivalent to ymean = 0
#    yentropyker = yentropyker + ymean
# bandwidth of kernel is computed internally
# by default; otherwise it can be given as bker
#
# In a standard run, MCkerentropy_abund function is call
# from sizeDiversity2017 function which is used to take logs
# and carry out normalization using the geometric mean.
######################################################
# J.J. Egozcue, July 2017
######################################################


MCkerentropy_abund <- function(y,abund,bker=NULL)
{
	# total abundance
	n=sum(abund)
	# total values
	ny = length(y)
  #log-mean and log-sigma
  ymean <- sum(y*abund)/n
  ystd <- sqrt( sum((y- rep(ymean,length=length(y)) )^2*abund)/n )

  #Constants for Gaussian kernel
  if(is.null(bker)){
  sigma <- (1.06 * ystd)/exp(0.2*log(ny)) } #bandwidth 
  if(!is.null(bker)){sigma=bker}
  sqdospisig <- 1.0/(sqrt(2.0*pi)*sigma) #normal constant
  sigma2 <- sigma * sigma
  csigma <- -0.5/sigma2 #exp constant
  
  #Gaussian kernel on transformed-sample
  fy <- rep(0,length=ny)
  for (i in 1:ny){
    for (j in 1:ny){
      yy <- y[i] - y[j]
      fy[i] <- fy[i] + exp(csigma * yy^2) * abund[j]
    }
  }
  fy <- fy * sqdospisig/n 
  #log density (negative)
  fy <- -log(fy)

  #Monte Carlo estimate entropy
  yentropyker <- sum(fy * abund)/n+ymean
  
  #if data were not normalized ymean should be added 
  #normalization is equivalent to ymean = 0
  
  return(list(oEntropyker=yentropyker,oSigma=sigma))
}
###############################################

#############################################
# function sizeDiversity2017, computes logarithms of sizes
# and carries out the normalization by the geometric mean
# equivalent to subtract the mean of the logarithms.
# It calls MCkerentropy_abund to compute the size diversity
#  x  values of the size (possitive)
#  abund      number of repeated values in x (if any)
#  measuredim correction for dimensionality of 
#             measurements: 1 length 
#                           2 area
#                           3 volume (default)
# gnormaliz=1 normalizes data by geometric mean
# gnormaliz=0 normalizing is not carried out
############################################
# J.J. Egozcue, July 2017
############################################  
sizeDiversity2017<-function(x,abund=NULL,measuredim=3,gnormaliz=1){
	nz=sum(x<=0)
	if(nz>0){
		print("negative or null data points")
	  print(nz)
	  return()
	}
	xlog = log(x)
# abundances are 1 by default
	if(is.null(abund)){abund=rep(1,length(x))}
	
# normalization by geometric mean

  totabund = sum(abund)
  meanx=sum(x*abund)/totabund
	meanxlog = sum(xlog * abund)/totabund
	y = xlog
	if(gnormaliz==1){
	  y = xlog - rep(meanxlog,length(y))	
	}
	meany = sum( y*abund/totabund)
# size diversity
	listsdiv= MCkerentropy_abund(y,abund,bker=NULL)
	diversitye =listsdiv[[1]] -
		log(measuredim) + log(3)
	bandker=listsdiv[[2]]
	devxlog = sqrt(sum( abund*(xlog-meanxlog)^2)/totabund)  
	logNdiversitye = logNormal_Entropy(desvlog=devxlog,meanlog=meany)-
		log(measuredim) + log(3)
	eevenness = exp(diversitye)/exp(logNdiversitye)
sortida<-NULL
sortida<-(c(diversity2=diversitye/log(2),
							eevenness=eevenness,
							logNdiversity2=logNdiversitye/log(2),
							bandker=bandker,
							devxlog=devxlog,
							gmeanx=exp(meanxlog),
							meanx=meanx))
sortida
}
#######################################################	