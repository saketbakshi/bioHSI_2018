source("https://github.com/naheim/paleosizePaper/raw/master/sharedCode/functions.r")
setwd("/Users/seyib/Desktop") #set working directory
sizeData<-read.delim(file='bodySizes.txt') #read in data set
sizeData$log10_volume<-log10(sizeData$max_vol) #add colummn to data set to take log of max volume of all species
sizeData$combined_resp<-paste(sizeData$fluid, sizeData$respOrgan, sizeData$circ) #adding a combined column to dataset to sort out respiration types
sizeData <- subset(sizeData, combined_resp != "" & combined_resp != "water multi closed") #taking out data that isn't sorted into any category and taking out organisms that have respiration systems: water, multi organ, closed, because there aren't enough examples to make definite conclusions
# table(sizeData$combined_resp) shows how many different variations there are with how many values in each category
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt') #reading in timescale

#making subsets of different types of usable respiration combinations
WaDeCl<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]
WaDeOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="open"),]
WaMuOp<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="multi" & sizeData[,"circ"]=="open"),]
AiDeCl<-sizeData[which(sizeData[,"fluid"]=="air" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]

quartz() #makes a new plot window to not overwrite a quartz window
time.plot(c(-2,1), "Slope of Regression Coeffecient for Extinction Estimated by Volume", main="Time Series of Extinction Selectivity as Estimated by Body Size", x.axis.pct=18)
#plot(1:10,1:10, type="n", xlim=c(550,0), ylim=c(-2,1), xlab="Geological time (Ma)", ylab="Slope of Regression Coeffecient for Extinction Estimated by Volume", main="Time Series of Extinction Selectivity as Estimated by Body Size") #setting up graph plot

myRegWDC <- vector(mode="numeric", length=nrow(timescale)) #making empty vector that can be filled with correlation coefficient for y~x being extinct~bodysize, where extinct = 1 means it goes extinct. a negative value means that as body size increases, organisms are less likely to go extinct. vice versa for positive value
myPropWDC <- vector(mode="numeric", length=nrow(timescale)) #empty vector that can be filled with proportion of genera that go extinct in a time period
for(i in 1:nrow(timescale)) { #making loop for filling in vector
	temp<-WaDeCl[WaDeCl$fad_age > timescale$age_top[i] & WaDeCl$lad_age < timescale$age_bottom[i], ] #creates temp data that subsets all organisms contained in a time period
	temp$extinct <- 0 #creates a new column called extinct where every organism is set to extant
	temp$extinct[temp$lad_age < timescale$age_bottom[i] & temp$lad_age >= timescale$age_top[i]] <- 1 #filters through all organisms of the interval that become extinct and sets them to 1 in the extinct column
	if(sum(temp$extinct) >= 3 & nrow(temp)-sum(temp$extinct) >= 3) { #insures that if there is not enough data in an interval, it is not calculated and the correlation calculation is skipped over. Prevents huge correlation coefficients that aren't statistically significant
		glmEqn <- glm(extinct ~ log10_volume, family="binomial", data=temp) #does regression calculation for extinct as estimated by volume for time interval
		myRegWDC[i]<- glmEqn$coefficients[2] #inserts value for coefficient into vector
	}
	myPropWDC[i] <- sum(temp$extinct)/nrow(temp) #calculating the proprtion of genera that go extinct in time period
}
lines(timescale$age_mid, myRegWDC, col="red4", lwd=2) #adds line of vector above

myRegWDO <- vector(mode="numeric", length=nrow(timescale))
myPropWDO <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp1<-WaDeOp[WaDeOp$fad_age > timescale$age_top[i] & WaDeOp$lad_age < timescale$age_bottom[i], ]
	temp1$extinct <- 0
	temp1$extinct[temp1$lad_age < timescale$age_bottom[i] & temp1$lad_age >= timescale$age_top[i]] <- 1
	if(sum(temp1$extinct) >= 3 & nrow(temp1)-sum(temp1$extinct) >= 3) {
		glmEqn1 <- glm(extinct ~ log10_volume, family="binomial", data=temp1)
		myRegWDO[i]<- glmEqn1$coefficients[2]
		}
	myPropWDO[i] <- sum(temp1$extinct)/nrow(temp1)
}
lines(timescale$age_mid, myRegWDO, col="darkorange4", lwd=2)

myRegWMO <- vector(mode="numeric", length=nrow(timescale))
myPropWMO <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp2<-WaMuOp[WaMuOp$fad_age > timescale$age_top[i] & WaMuOp$lad_age < timescale$age_bottom[i], ]
	temp2$extinct <- 0
	temp2$extinct[temp2$lad_age < timescale$age_bottom[i] & temp2$lad_age >= timescale$age_top[i]] <- 1
	if(sum(temp2$extinct) >= 3 & nrow(temp2)-sum(temp2$extinct) >= 3) {
		glmEqn2 <- glm(extinct ~ log10_volume, family="binomial", data=temp2)
		myRegWMO[i]<- glmEqn2$coefficients[2]
	}
	myPropWMO[i] <- sum(temp2$extinct)/nrow(temp2)
}
lines(timescale$age_mid, myRegWMO, col="darkgreen", lwd=2)

myRegADC <- vector(mode="numeric", length=nrow(timescale))
myPropADC <- vector(mode="numeric", length=nrow(timescale))
for(i in 1:nrow(timescale)) {
	temp3<-AiDeCl[AiDeCl$fad_age > timescale$age_top[i] & AiDeCl$lad_age < timescale$age_bottom[i], ]
	if(nrow(temp3) > 0){
		temp3$extinct <- 0
		temp3$extinct[temp3$lad_age < timescale$age_bottom[i] & temp3$lad_age >= timescale$age_top[i]] <- 1
		if(sum(temp3$extinct) >= 3 & nrow(temp3)-sum(temp3$extinct) >= 3) {
			glmEqn3 <- glm(extinct ~ log10_volume, family="binomial", data=temp3)
			myRegADC[i]<- glmEqn3$coefficients[2]
		}
	}
	myPropADC[i] <- sum(temp3$extinct)/nrow(temp3)
}
lines(timescale$age_mid, myRegADC, col="blue4", lwd=2)
legend(520, -1.38, legend=c("Water, Dedicated organ, Closed system", "Water, Dedicated organ, Open system", "Water, Multi-organ, Open system", "Air, Dedicated organ, Closed system"), col=c("red4", "darkorange4", "darkgreen", "blue4"), lty=1, title="Repiratory System Types", cex=0.8) #makes legend for each respiration type
quartz() #creates new plot window
#plot(1:10,1:10, type="n", xlim=c(550,0), ylim=c(0,1), xlab="Geological time (Ma)", ylab="Extinction Rate", main="Change of Extinction Rate Over Time") #sets up new plot
time.plot(c(0,1), "Extinction Rate", main="Change of Extinction Rate Over Time", x.axis.pct=6)
lines(timescale$age_mid, myPropWDC, col="red4", lwd=2) #adds lines of extinction rate for each genera
lines(timescale$age_mid, myPropWDO, col="darkorange4", lwd=2)
lines(timescale$age_mid, myPropWMO, col="darkgreen", lwd=2)
lines(timescale$age_mid, myPropADC, col="blue4", lwd=2)
legend(520, 0.96, legend=c("Water, Dedicated organ, Closed system", "Water, Dedicated organ, Open system", "Water, Multi-organ, Open system", "Air, Dedicated organ, Closed system"), col=c("red4", "darkorange4", "darkgreen", "blue4"), lty=1, title="Repiratory System Types", cex=0.7) #creates legend