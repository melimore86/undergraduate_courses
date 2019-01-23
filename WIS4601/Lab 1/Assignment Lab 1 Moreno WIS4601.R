##now I'm going to work with the amphibians data
#first I'm going to set my working directory to the file where
#I saved the text file called amphibians from lab
setwd("F:/WIS4601/Lab 1")
amphibians= read.table ("Amphibians.txt", header= TRUE)
head(amphibians)

length.ELPO<-length(amphibians$SVL[amphibians$Species=="ELPO"])
mean.ELPO<-mean(amphibians$SVL[amphibians$Species=="ELPO"])
(sd.ELPO<-sd(amphibians$SVL[amphibians$Species=="ELPO"]))
(sd.ElCO<-sd(amphibians$SVL[amphibians$Species=="ELCO"]))
length.ELCO<-length(amphibians$SVL[amphibians$Species=="ELCO"])
mean.ELCO<-mean(amphibians$SVL[amphibians$Species=="ELCO"])

#now lets find out the SVL, mean and sd for individuals captures in locations 
#where elevations was greater than 500
setwd("F:/WIS4601/Lab 1")
amphibians= read.table ("Amphibians.txt", header= TRUE)
head(amphibians)

length.ELPO<-length(amphibians$SVL[amphibians$Species =="ELPO" & amphibians$Elevation >500])
mean.ELPO<-mean(amphibians$SVL[amphibians$Species=="ELPO" & amphibians$Elevation >500])
sd.ELPO<-sd(amphibians$SVL[amphibians$Species=="ELPO" & amphibians$Elevation >500])
sd.ElCO<-sd(amphibians$SVL[amphibians$Species=="ELCO" & amphibians$Elevation >500])
length.ELCO<-length(amphibians$SVL[amphibians$Species=="ELCO" & amphibians$Elevation >500])
mean.ELCO<-mean(amphibians$SVL[amphibians$Species=="ELCO" & amphibians$Elevation >500])

#Now lets calculate the correct elevation for individuals captures above 500 m elevation
setwd("F:/WIS4601/Lab 1")
amphibians= read.table ("Amphibians.txt", header= TRUE)
head(amphibians)
amphibians$Cor.Elev= 1.14* amphibians$Elevation

#using another head(amphibians) to check if the corrected column was added
head(amphibians)

mean.ELPO<-mean(amphibians$SVL[amphibians$Species=="ELPO" & amphibians$Cor.Elev >500])
sd.ELPO<-sd(amphibians$SVL[amphibians$Species=="ELPO" & amphibians$Cor.Elev >500])
sd.ElCO<-sd(amphibians$SVL[amphibians$Species=="ELCO" & amphibians$Cor.Elev >500])
mean.ELCO<-mean(amphibians$SVL[amphibians$Species=="ELCO" & amphibians$Cor.Elev >500])
length.ELCO<-length(amphibians$SVL[amphibians$Species=="ELCO" & amphibians$Cor.Elev >500])


