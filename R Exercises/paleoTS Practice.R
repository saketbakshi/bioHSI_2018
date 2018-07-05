library(paleoTS)
sizeData<-read.delim('bodySizes.txt')
timescale<-read.delim('https://raw.githubusercontent.com/naheim/paleosizePaper/master/rawDataFiles/timescale.txt')
n.bins<-nrow(timescale)
my.mean<-vector(mode="numeric", length=n.bins)
my.var<-vector(mode="numeric", length=n.bins)
my.n<-vector(mode="numeric", length=n.bins)
my.time<-timescale$age_bottom
names(my.mean)<-timescale$interval_name
names(my.var) <- timescale$interval_name
names(my.n) <- timescale$interval_name
names(my.time) <- timescale$interval_name
for(i in 1:n.bins) {
	temp.data <- log10(sizeData$max_vol[sizeData$fad_age > timescale$age_top[i] & sizeData$lad_age < timescale$age_bottom[i]])
	my.mean[i] <- mean(temp.data)
	my.var[i] <- var(temp.data)
	my.n[i] <- length(temp.data)
}
my.ts<-as.paleoTS(mm=my.mean, vv=my.var, nn=my.n,tt=my.time, oldest="last")
fit3models(my.ts, method="Joint", pool=FALSE)
plot(timescale$age_bottom, my.mean, xlim=c(541,0), type="o", pch=20, xlab="Geologic Time (Ma)", ylab=expression(paste("Mean body size (log"[10],"mm)")))
for(i in 1:n.bins) {
	ci <- 1.96 * sqrt(my.var[i]) / sqrt(my.n[i])
	my.x <- rep(timescale$age_bottom[i], 2)
	my.y <- c(my.mean[i] + ci, my.mean[i] - ci)
	lines(my.x, my.y, lwd=0.75, col="red")
}

