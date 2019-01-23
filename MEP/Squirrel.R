#################################################################################
#            Distance sampling when detectability < 1                                                                                       #
#
#################################################################################
require("Rdistance")
#UF Squirrels
#Area 1600mx1143m
#Set the working directory you need to replace XXXX with your gator login name
setwd("C:/Users/UF/Desktop")
#create a data frame with the site ID and the length of site in meters
sites=data.frame(siteID=c(1:8),length=c(1870,2211,1934,1946,1425,1768,1886,2236)) #about 2 km
#make sure siteID is a factor
sites[,1]=as.factor(sites[,1]) #casting, numbers or characters
#characters and then numbers to factor conversion
#read in the data file
distsample<-read.table("squirrels.txt",header=T) 
#make sure that the data column names are correct 
colnames(distsample)=c("siteID","dists","groupsize")
#make sure siteID is a factor
distsample[,1]=as.factor(distsample[,1]) #casting as factors and not as numbers
#look at the head of the file
head(distsample)
#take a look at the number of sighting by distance
hist(distsample$dist)
 
#number of observations: we cannot estimate the relationship of detection
# probability and distance from the transect without sufficient
#number of observations
nrow(distsample)

#fit a distance function to the data to determine how detection prob.
#decreases as distance to the transect increases
#half-norm
hn.dfunc<- F.dfunc.estim(distsample$dist,likelihood="halfnorm", expansions = 0, series = "cosine")
hn.dfunc 
#calculating the AIC= 2x LL + or - 2 number of parameters
#uniform
un.dfunc<- F.dfunc.estim(distsample$dist,likelihood="uniform", expansions = 0, series = "cosine")
un.dfunc
#hazard-rate
hr.dfunc<- F.dfunc.estim(distsample$dist,likelihood="hazrate", expansions = 0, series = "cosine")
hr.dfunc
#negative exponential
ne.dfunc<- F.dfunc.estim(distsample$dist,likelihood="negexp", expansions = 0, series = "cosine")
ne.dfunc
# it boostrapped, randonmly redrew the distances with replacement, and refit histograms
#quantiles of the bootstrapping
#setup graphics to plot 2x2
par(mfcol=c(2,2))
#estimate abundance using the half-normal detection function
#half norm
hn.abund<-F.abund.estim(hn.dfunc, distsample,sites,area = 1600*1143, ci = .95, R=500, plot.bs=TRUE)
hn.abund
#uniform
un.abund<-F.abund.estim(un.dfunc, distsample,sites,area = 1600*1143, ci = .95, R=500, plot.bs=TRUE)
un.abund 
#hazard-rate
hr.abund<-F.abund.estim(hr.dfunc, distsample,sites,area = 1600*1143, ci = .95, R=500, plot.bs=TRUE)
hr.abund 
#negative exponential
ne.abund<-F.abund.estim(ne.dfunc, distsample,sites,area = 1600*1143, ci = .95, R=500, plot.bs=TRUE)
ne.abund

#Create vectors to store values
aic<-vector(length=4)
est<-vector(length=4)
low95<-vector(length=4)
up95<-vector(length=4)
#AIC for model selection
aic[1]=AIC(hn.dfunc);aic[2]=AIC(un.dfunc);aic[3]=AIC(hr.dfunc);aic[4]=AIC(ne.dfunc)
#Abundance estimte
est[1]=hn.abund$n.hat;est[2]=un.abund$n.hat;est[3]=hr.abund$n.hat;est[4]=ne.abund$n.hat
#Lower 95% for abundance
low95[1]=hn.abund$ci[1];low95[2]=un.abund$ci[1];low95[3]=hr.abund$ci[1];low95[4]=ne.abund$ci[1]
#Upper 95% for abundance
up95[1]=hn.abund$ci[2];up95[2]=un.abund$ci[2];up95[3]=hr.abund$ci[2];up95[4]=ne.abund$ci[2]
#delta AIC
daic<- aic-min(aic) #difference between delta AIC
#Create a big data frame
results<-data.frame(est,low95,up95,aic,daic) #creates a table of your info
results
#write.table(results,file="results.csv",row.names=FALSE,col.names=FALSE,sep=",")

#est    low95     up95      aic     daic
#1 189.9503 150.7739 238.4494 1023.411 4.083403
#2 246.6601 168.3520 318.8732 1021.675 2.347020
#3 191.7535 134.4588 288.9265 1024.760 5.432847
#4 274.4934 208.2456 345.3338 1019.328 0.000000


boots=cbind(hn.abund$B,un.abund$B,hr.abund$B,ne.abund$B) 
colnames(boots)=c("half-normal","uniform","hazrate","negative-exponential")
par(mfcol=c(1,1))
boxplot(boots,notch=TRUE,ylab="Squirrel population size",col="steelblue")
#showing the outlier in the models, saying that there is about 200 squirrels
