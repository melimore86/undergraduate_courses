#################################
##	Simulator for
##	distance sampling
##
##
##
#################################

require("Rdistance")

#define distance probabilty sampling
#use half normal distribution
#Map the function
hdist=seq(0,100,length=100) #half normal distribution
phdist=dnorm(hdist,0,30)/dnorm(0,0,30) # density function evaluated at 0, going to 0 to 1
plot(hdist,phdist) # detectability falling off as a function

#create a universe of individuals to be sampled
idist=runif(1000,0,100) # 1000 individuals, random from 0 to 100
ipsamp=dnorm(idist,0,30)/dnorm(0,0,30) # probability for sampling the inviduals, sampling
coinflip=runif(1000) #coin flip for detection
sampled=idist[which(coinflip<=ipsamp)]# which is returning the array

#subsample to simulate data collection of 100, generating some data
names=c("siteID","dists","groupsize")
nsamp=round(runif(8,20,40),0) # 8 transects, 10 to 20 indiduals
siteID=vector()
dists=vector()
for(i in 1:8){
	siteID=c(siteID,rep(i,nsamp[i]))
	dists=c(dists,sample(sampled,nsamp[i])) #resample to get distances, creating a list of the samples
}
groupsize=rep(1,length(siteID)) 
dfile=cbind(siteID,dists,groupsize)
colnames(dfile)=names

setwd("C:/Users/UF/Desktop")
write.table(dfile,file="squirrels.txt",row.names=FALSE) #formattnig for R distance

