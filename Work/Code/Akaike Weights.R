library(paleoTS)
setwd("/Users/seyib/Desktop") #setting directory
sizeData<-read.delim(file='bodySizes.txt') #reading in data set
sizeData$log10_volume<-log10(sizeData$max_vol) #adding column to dataset to make log volume of data
sizeData$combined_resp<-paste(sizeData$fluid, sizeData$respOrgan, sizeData$circ)
sizeData <- subset(sizeData, combined_resp != "" & combined_resp != "water multi closed")
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
nBins <- nrow(timescale)
myMean <- vector(mode="numeric", length=nBins)
myVar <- myMean
myN <- myVar
myTime <- timescale$age_bottom

names(myVar) <- timescale$interval_name
names(myN) <- timescale$interval_name
names(myTime) <- timescale$interval_name

WaDeCl<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]
WaDeOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="open"),]
WaMuOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="multi" & sizeData[,"circ"]=="open"),]
AiDeCl<-sizeData[which(sizeData[,"fluid"]=="air" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]

for(i in 1:nBins) {
	temp<-WaDeCl[WaDeCl$fad_age > timescale$age_top[i] & WaDeCl$lad_age < timescale$age_bottom[i], ]
	myMean[i]<-mean(temp$log10_volume)
	myVar[i]<-var(temp$log10_volume) 
	myN[i]<-length(temp$log10_volume)
}
myTS <- as.paleoTS(mm=myMean[!is.na(myVar)], vv=myVar[!is.na(myVar)], nn=myN[!is.na(myVar)], tt=myTime[!is.na(myVar)], oldest="last")
fit3models(myTS, method="Joint", pool=FALSE)

myMean1 <- vector(mode="numeric", length=nBins)
myVar1 <- myMean1
myN1 <- myVar1
names(myVar1) <- timescale$interval_name
names(myN1) <- timescale$interval_name
for(i in 1:nBins) {
	temp1<-WaDeOp[WaDeOp$fad_age > timescale$age_top[i] & WaDeOp$lad_age < timescale$age_bottom[i], ]
	myMean1[i]<-mean(temp1$log10_volume)
	myVar1[i]<-var(temp1$log10_volume) 
	myN1[i]<-length(temp1$log10_volume)
}
myTS1 <- as.paleoTS(mm=myMean1[!is.na(myVar1)], vv=myVar1[!is.na(myVar1)], nn=myN1[!is.na(myVar1)], tt=myTime[!is.na(myVar1)], oldest="last")
fit3models(myTS1, method="Joint", pool=FALSE)

myMean2 <- vector(mode="numeric", length=nBins)
myVar2 <- myMean2
myN2 <- myVar2
names(myVar2) <- timescale$interval_name
names(myN2) <- timescale$interval_name
for(i in 1:nBins) {
	temp2<-WaMuOp[WaMuOp$fad_age > timescale$age_top[i] & WaMuOp$lad_age < timescale$age_bottom[i], ]
	myMean2[i]<-mean(temp2$log10_volume)
	myVar2[i]<-var(temp2$log10_volume) 
	myN2[i]<-length(temp2$log10_volume)
}
myTS2 <- as.paleoTS(mm=myMean2[!is.na(myVar2)], vv=myVar2[!is.na(myVar2)], nn=myN2[!is.na(myVar2)], tt=myTime[!is.na(myVar2)], oldest="last")
fit3models(myTS2, method="Joint", pool=FALSE)

myMean3 <- vector(mode="numeric", length=nBins)
myVar3 <- myMean3
myN3 <- myVar3
names(myVar3) <- timescale$interval_name
names(myN3) <- timescale$interval_name
for(i in 1:nBins) {
	temp3<-AiDeCl[AiDeCl$fad_age > timescale$age_top[i] & AiDeCl$lad_age < timescale$age_bottom[i], ]
	myMean3[i]<-mean(temp3$log10_volume)
	myVar3[i]<-var(temp3$log10_volume) 
	myN3[i]<-length(temp3$log10_volume)
}
myTS3 <- as.paleoTS(mm=myMean3[!is.na(myVar3)], vv=myVar3[!is.na(myVar3)], nn=myN3[!is.na(myVar3)], tt=myTime[!is.na(myVar3)], oldest="last")
fit3models(myTS3, method="Joint", pool=FALSE)