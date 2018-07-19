source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
setwd("/Users/seyib/Desktop")
sizeData <- read.delim(file='bodySizes.txt')
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
nBins <- nrow(timescale)
sizeData$log10_volume<-log10(sizeData$max_vol)
sizeData$combined_resp<-paste(sizeData$fluid, sizeData$respOrgan, sizeData$circ)
sizeData <- subset(sizeData, is.element(combined_resp, c("air dedicated closed","water dedicated closed","water dedicated open","water multi open")))
sizeData$combined_resp <- factor(sizeData$combined_resp)

WaDeCl<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]
WaDeOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="open"),]
WaMuOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="multi" & sizeData[,"circ"]=="open"),]
AiDeCl<-sizeData[which(sizeData[,"fluid"]=="air" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]

sysProp <- data.frame(matrix(NA, nrow=nBins, ncol=4, dimnames=list(timescale$interval_name, levels(sizeData$combined_resp))))
for(i in 1:nBins) {
	temp <- sizeData[sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i], ]
	respCounts <- table(temp$combined_resp)
	respProp <- respCounts/sum(respCounts)
	sysProp[i,] <- respProp
}

xPoly <- c(timescale$age_mid, rev(timescale$age_mid))
yPoly1 <- c(rep(0, nBins), rev(sysProp[,"air.dedicated.closed"]))
yPoly2 <- c(sysProp[,"air.dedicated.closed"], rev(sysProp[,"air.dedicated.closed"] + sysProp[,"water.dedicated.closed"]))
yPoly3 <- c(sysProp[,"air.dedicated.closed"] + sysProp[,"water.dedicated.closed"], rev(sysProp[,"air.dedicated.closed"] + sysProp[,"water.dedicated.closed"] + sysProp[,"water.dedicated.open"]))
yPoly4 <- c(sysProp[,"air.dedicated.closed"] + sysProp[,"water.dedicated.closed"] + sysProp[,"water.dedicated.open"], rev(sysProp[,"air.dedicated.closed"] + sysProp[,"water.dedicated.closed"] + sysProp[,"water.dedicated.open"] + sysProp[,"water.multi.open"]))

classCols <- c("red","darkorange","forestgreen","dodgerblue")
time.plot(c(0,1), "Proportion of Genera", main="Time Series of Proportion of Respiratory Classes")

polygon(xPoly,yPoly1, col=classCols[4])
polygon(xPoly,yPoly2, col=classCols[1])
polygon(xPoly,yPoly3, col=classCols[2])
polygon(xPoly,yPoly4, col=classCols[3])

legend(479,0.96, legend=c("Water, Dedicated organ, Closed system", "Water, Dedicated organ, Open system", "Water, Multi-organ, Open system", "Air, Dedicated organ, Closed system"), col=c("red4", "darkorange4", "darkgreen", "blue4"), lty=1, title="Respiratory System Types", bg="#ffffff", cex=0.7)