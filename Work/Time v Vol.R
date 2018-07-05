setwd("/Users/seyib/Desktop")
sizeData<-read.delim(file='bodySizes.txt')
sizeData$log10_volume<-log10(sizeData$max_vol)
sizeData$combined_resp<-paste(sizeData$fluid, sizeData$respOrgan, sizeData$circ)
sizeData <- subset(sizeData, combined_resp != "" & combined_resp != "water multi closed")
# table(sizeData$combined_resp) shows how many different variations there are with how many values in each category
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
plot(1:10,1:10, type="n", xlim=c(550,0), ylim=c(-2,12), xlab="Geological time (Ma)", ylab=expression(paste("Biovolume (log"[10]," cm"^3*")")), main="Body Size Evolution as Divided by Respiratory Types")
WaDeCl<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]
WaDeOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="open"),]
WaMuOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="multi" & sizeData[,"circ"]=="open"),]
AiDeCl<-sizeData[which(sizeData[,"fluid"]=="air" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]
segments(WaDeCl$fad_age, WaDeCl$log10_volume, WaDeCl$lad_age, WaDeCl$log10_volume, col="red", lwd=0.25)
segments(WaDeOp$fad_age, WaDeOp$log10_volume, WaDeOp$lad_age, WaDeOp$log10_volume, col="darkorange", lwd=0.25)
segments(WaMuOp$fad_age, WaMuOp$log10_volume, WaMuOp$lad_age, WaMuOp$log10_volume, col="forestgreen", lwd=0.25)
segments(AiDeCl$fad_age, AiDeCl$log10_volume, AiDeCl$lad_age, AiDeCl$log10_volume, col="dodgerblue", lwd=0.25)
myMeanWDC <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp<-WaDeCl[WaDeCl$fad_age > timescale$age_top[i] & WaDeCl$lad_age < timescale$age_bottom[i], ]
	myMeanWDC[i]<-mean(temp$log10_volume)
}
lines(timescale$age_mid, myMeanWDC, col="red4", lwd=3)
myMeanWDO <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp1<-WaDeOp[WaDeOp$fad_age > timescale$age_top[i] & WaDeOp$lad_age < timescale$age_bottom[i], ]
	myMeanWDO[i]<-mean(temp1$log10_volume)
}
lines(timescale$age_mid, myMeanWDO, col="darkorange4", lwd=3)
myMeanWMO <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp2<-WaMuOp[WaMuOp$fad_age > timescale$age_top[i] & WaMuOp$lad_age < timescale$age_bottom[i], ]
	myMeanWMO[i]<-mean(temp2$log10_volume)
}
lines(timescale$age_mid, myMeanWMO, col="darkgreen", lwd=3)
myMeanADC <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp3<-AiDeCl[AiDeCl$fad_age > timescale$age_top[i] & AiDeCl$lad_age < timescale$age_bottom[i], ]
	myMeanADC[i]<-mean(temp3$log10_volume)
}
lines(timescale$age_mid, myMeanADC, col="blue4", lwd=3)
legend(550, 12, legend=c("Water, Dedicated organ, Closed system", "Water, Dedicated organ, Open system", "Water, Multi-organ, Open system", "Air, Dedicated organ, Closed system"), col=c("red4", "darkorange4", "darkgreen", "blue4"), lty=1, title="Repiratory System Types", cex=0.8)
