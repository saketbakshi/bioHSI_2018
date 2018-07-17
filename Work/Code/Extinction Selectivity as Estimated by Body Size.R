source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
setwd("/Users/seyib/Desktop")
sizeData <- read.delim(file='bodySizes.txt')
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
nBins <- nrow(timescale)
sizeData$log10_volume<-log10(sizeData$max_vol)
sizeData$combined_resp<-paste(sizeData$fluid, sizeData$respOrgan, sizeData$circ)
sizeData <- subset(sizeData, combined_resp != "" & combined_resp != "water multi closed")

WaDeCl<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]
WaDeOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="open"),]
WaMuOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="multi" & sizeData[,"circ"]=="open"),]
AiDeCl<-sizeData[which(sizeData[,"fluid"]=="air" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]

wdcExtSel <- data.frame(matrix(NA, nrow=nBins, ncol=3, dimnames=list(timescale$interval_name, c('coef','ci.minus','ci.plus'))))
for(i in 1:nBins) {
	temp <- WaDeCl[WaDeCl$fad_age > timescale$age_top[i] & WaDeCl$lad_age < timescale$age_bottom[i], ]
	if(nrow(temp) > 0) {
		temp$extinct <- 0
		temp$extinct[temp$lad_age < timescale$age_bottom[i] & temp$lad_age >= timescale$age_top[i]] <- 1	
		if(sum(temp$extinct) >= 3 & nrow(temp) >= 6) {
			myGlm <- glm(extinct ~ log10_volume, data=temp, family=binomial)
			wdcExtSel$coef[i] <- myGlm$coefficients[2]
			myCi <- confint(myGlm)
			wdcExtSel$ci.minus[i] <- myCi[2,1]
			wdcExtSel$ci.plus[i] <- myCi[2,2]
		}
	}
}
quartz()
time.plot(c(-2,2), "Log-odds of extinction", main="Time Series of Extinction Selectivity as Estimated by Body Size")
abline(h=0, lty=2)
points(timescale$age_mid, wdcExtSel$coef, pch=16, cex=1.25, col="red4")
segments(timescale$age_mid,wdcExtSel$ci.minus,timescale$age_mid,wdcExtSel$ci.plus, col="red")
legend(250, 1.84, legend=c("Water, Dedicated organ, Closed system"), col=c("red4"), lty=1, title="Repiratory System Type", cex=0.7)

wdoExtSel <- data.frame(matrix(NA, nrow=nBins, ncol=3, dimnames=list(timescale$interval_name, c('coef','ci.minus','ci.plus'))))
for(i in 1:nBins) {
	temp1 <- WaDeOp[WaDeOp$fad_age > timescale$age_top[i] & WaDeOp$lad_age < timescale$age_bottom[i], ]
	if(nrow(temp1) > 0) {
		temp1$extinct <- 0
		temp1$extinct[temp1$lad_age < timescale$age_bottom[i] & temp1$lad_age >= timescale$age_top[i]] <- 1	
		if(sum(temp1$extinct) >= 3 & nrow(temp1) >= 6) {
			myGlm1 <- glm(extinct ~ log10_volume, data=temp1, family=binomial)
			wdoExtSel$coef[i] <- myGlm1$coefficients[2]
			myCi1 <- confint(myGlm1)
			wdoExtSel$ci.minus[i] <- myCi1[2,1]
			wdoExtSel$ci.plus[i] <- myCi1[2,2]
		}
	}
}
quartz()
time.plot(c(-2,2), "Log-odds of extinction", main="Time Series of Extinction Selectivity as Estimated by Body Size")
abline(h=0, lty=2)
points(timescale$age_mid, wdoExtSel$coef, pch=16, cex=1.25, col="darkorange4")
segments(timescale$age_mid,wdoExtSel$ci.minus,timescale$age_mid,wdoExtSel$ci.plus, col="darkorange")
legend(520, 1.84, legend=c("Water, Dedicated organ, Open system"), col=c("darkorange4"), lty=1, title="Repiratory System Type", cex=0.7)

wmoExtSel <- data.frame(matrix(NA, nrow=nBins, ncol=3, dimnames=list(timescale$interval_name, c('coef','ci.minus','ci.plus'))))
for(i in 1:nBins) {
	temp2 <- WaMuOp[WaMuOp$fad_age > timescale$age_top[i] & WaMuOp$lad_age < timescale$age_bottom[i], ]
	if(nrow(temp2) > 0) {
		temp2$extinct <- 0
		temp2$extinct[temp2$lad_age < timescale$age_bottom[i] & temp2$lad_age >= timescale$age_top[i]] <- 1	
		if(sum(temp2$extinct) >= 3 & nrow(temp2) >= 6) {
			myGlm2 <- glm(extinct ~ log10_volume, data=temp2, family=binomial)
			wmoExtSel$coef[i] <- myGlm2$coefficients[2]
			myCi2 <- confint(myGlm2)
			wmoExtSel$ci.minus[i] <- myCi2[2,1]
			wmoExtSel$ci.plus[i] <- myCi2[2,2]
		}
	}
}
quartz()
time.plot(c(-2,2), "Log-odds of extinction", main="Time Series of Extinction Selectivity as Estimated by Body Size")
abline(h=0, lty=2)
points(timescale$age_mid, wmoExtSel$coef, pch=16, cex=1.25, col="darkgreen")
segments(timescale$age_mid,wmoExtSel$ci.minus,timescale$age_mid,wmoExtSel$ci.plus, col="forestgreen")
legend(520, 1.84, legend=c("Water, Multi-organ, Open system"), col=c("darkgreen"), lty=1, title="Repiratory System Type", cex=0.7)

adcExtSel <- data.frame(matrix(NA, nrow=nBins, ncol=3, dimnames=list(timescale$interval_name, c('coef','ci.minus','ci.plus'))))
for(i in 1:nBins) {
	temp3 <- AiDeCl[AiDeCl$fad_age > timescale$age_top[i] & AiDeCl$lad_age < timescale$age_bottom[i], ]
	if(nrow(temp3) > 0) {
		temp3$extinct <- 0
		temp3$extinct[temp3$lad_age < timescale$age_bottom[i] & temp3$lad_age >= timescale$age_top[i]] <- 1	
		if(sum(temp3$extinct) >= 3 & nrow(temp3) >= 6) {
			myGlm3 <- glm(extinct ~ log10_volume, data=temp3, family=binomial)
			adcExtSel$coef[i] <- myGlm3$coefficients[2]
			myCi3 <- confint(myGlm3)
			adcExtSel$ci.minus[i] <- myCi3[2,1]
			adcExtSel$ci.plus[i] <- myCi3[2,2]
		}
	}
}
quartz()
time.plot(c(-2,2), "Log-odds of extinction", main="Time Series of Extinction Selectivity as Estimated by Body Size")
abline(h=0, lty=2)
points(timescale$age_mid, adcExtSel$coef, pch=16, cex=1.25, col="dodgerblue")
segments(timescale$age_mid,adcExtSel$ci.minus,timescale$age_mid,adcExtSel$ci.plus, col="blue4")
legend(520, 1.84, legend=c("Air, Dedicated organ, Closed system"), col=c("dodgerblue"), lty=1, title="Repiratory System Type", cex=0.7)