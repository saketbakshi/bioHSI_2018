sizeData<-read.delim(file='bodySizes.txt')
timescale <- read.delim(file='https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
plot(1:10,1:10, type="n", xlim=c(550,0), ylim=c(-2,12), xlab="Geological time (Ma)", ylab=expression(paste("Biovolume (log"[10]," cm"^3*")")))
WaDeCl<-sizeData[which(sizeData[,"fluid"]=="water" & sizeData[,"respOrgan"]=="dedicated" & sizeData[,"circ"]=="closed"),]