setwd("/Users/HoL/Desktop")
beta=read.delim(file="echinodermSizes.txt")
urchinData=subset(beta, class=='Echinoidea')
urchinData$max_vol[is.na(urchinData$max_vol) & !is.na(urchinData$calc_max_vol)] <- urchinData$calc_max_vol[is.na(urchinData$max_vol) & !is.na(urchinData$calc_max_vol)] 

myVolX=log10(urchinData$max_vol)
hist(myVolX, main="Volume of Sea Urchins in a Three Dimensional Perspective", xlab="Log of Urchin Volume (mm³)", breaks=10, col="blue4")
meanVol=mean(urchinData$max_vol)
meanVol

quartz(height=6, width=6)
myLengthX=log10(urchinData$max_length)
hist(myLengthX, main="Maximum Length of Sea Urchins in a Two Dimensional Perspective", xlab="Log of Urchin Length (mm)", breaks=10, col="aquamarine3")
meanLength=mean(urchinData$max_length)
meanLength

quartz(height=6, width=6)
myAreaX=log10(urchinData$max_area)
hist(myAreaX, main="Area of Body of Sea Urchins in a Two Dimensional Perspective", xlab="Log of Urchin Area (mm²)", breaks=10, col="cyan3")
meanArea=mean(urchinData$max_area)
meanArea


extant <- subset(urchinData, extant =='t')
extinct <- subset(urchinData, extant =='f')

