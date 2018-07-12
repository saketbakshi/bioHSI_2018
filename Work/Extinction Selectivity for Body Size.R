setwd("/Users/seyib/Desktop")
sizeData<-read.delim(file='bodySizes.txt')
sizeData$log10_volume<-log10(sizeData$max_vol)
sizeData$combined_resp<-paste(sizeData$fluid, sizeData$respOrgan, sizeData$circ)
sizeData <- subset(sizeData, combined_resp != "" & combined_resp != "water multi closed")
# table(sizeData$combined_resp) shows how many different variations there are with how many values in each category
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')

WaDeCl<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]
WaDeOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="open"),]
WaMuOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="multi" & sizeData[,"circ"]=="open"),]
AiDeCl<-sizeData[which(sizeData[,"fluid"]=="air" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]

plot(1:10,1:10, type="n", xlim=c(550,0), ylim=c(-2,1), xlab="Geological time (Ma)", ylab="Slope of Regression Coeffecient for Extinction Estimated by Volume", main="Progression Over Time of Extinction Selectivity as Estimated by Body Size")

myRegWDC <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp<-WaDeCl[WaDeCl$fad_age > timescale$age_top[i] & WaDeCl$lad_age < timescale$age_bottom[i], ]
	temp$extinct <- 0
	temp$extinct[temp$lad_age < timescale$age_bottom[i] & temp$lad_age >= timescale$age_top[i]] <- 1
	if(sum(temp$extinct) >= 3 & nrow(temp)-sum(temp$extinct) >= 3) {
		glmEqn <- glm(extinct ~ log10_volume, family="binomial", data=temp)
		myRegWDC[i]<- glmEqn$coefficients[2]
	}
}
lines(timescale$age_mid, myRegWDC, col="red4", lwd=3)

myRegWDO <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp1<-WaDeOp[WaDeOp$fad_age > timescale$age_top[i] & WaDeOp$lad_age < timescale$age_bottom[i], ]
	temp1$extinct <- 0
	temp1$extinct[temp1$lad_age < timescale$age_bottom[i] & temp1$lad_age >= timescale$age_top[i]] <- 1
	if(sum(temp1$extinct) >= 3 & nrow(temp1)-sum(temp1$extinct) >= 3) {
		glmEqn1 <- glm(extinct ~ log10_volume, family="binomial", data=temp1)
		myRegWDO[i]<- glmEqn1$coefficients[2]
		}
}
lines(timescale$age_mid, myRegWDO, col="darkorange4", lwd=3)

myRegWMO <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp2<-WaMuOp[WaMuOp$fad_age > timescale$age_top[i] & WaMuOp$lad_age < timescale$age_bottom[i], ]
	temp2$extinct <- 0
	temp2$extinct[temp2$lad_age < timescale$age_bottom[i] & temp2$lad_age >= timescale$age_top[i]] <- 1
	if(sum(temp2$extinct) >= 3 & nrow(temp2)-sum(temp2$extinct) >= 3) {
	glmEqn2 <- glm(extinct ~ log10_volume, family="binomial", data=temp2)
	myRegWMO[i]<- glmEqn2$coefficients[2]
	}
}
lines(timescale$age_mid, myRegWMO, col="darkgreen", lwd=3)

myRegADC <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp3<-AiDeCl[AiDeCl$fad_age > timescale$age_top[i] & AiDeCl$lad_age < timescale$age_bottom[i], ]
	if(nrow(temp3) > 0){
		temp3$extinct <- 0
		temp3$extinct[temp3$lad_age < timescale$age_bottom[i] & temp3$lad_age >= timescale$age_top[i]] <- 1
		if(sum(temp$extinct) >= 3 & nrow(temp)-sum(temp$extinct) >= 3) {
			glmEqn3 <- glm(extinct ~ log10_volume, family="binomial", data=temp3)
			myRegADC[i]<- glmEqn3$coefficients[2]
		}
	}
}
lines(timescale$age_mid, myRegADC, col="blue4", lwd=3)
legend(550, 9, legend=c("Water, Dedicated organ, Closed system", "Water, Dedicated organ, Open system", "Water, Multi-organ, Open system", "Air, Dedicated organ, Closed system"), col=c("red4", "darkorange4", "darkgreen", "blue4"), lty=1, title="Repiratory System Types", cex=0.8)