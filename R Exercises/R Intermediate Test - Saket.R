#Section 1

set.seed(541)
MatrixElements<-sample(c(TRUE,FALSE), size=96, replace=TRUE)
myMatrix<-array(data=MatrixElements,dim=c(8,12))
myMatrix
#use ?"command" to see what a command can do
# 1. The REPLACE= argument of sample() decides on whether to sample with or without replacement.
# 2. We can multiply the matrix by 1 to convert logicals into numbers.
myMatrix*1
# 3. I would use which(myMatrix)
which(myMatrix)

#Section 2

# 1. According to length(myMatrix[which(myMatrix==7)]) , there are 16 repetitions of 7
# 2. apply(myMatrix,2,sum)
# 3. apply(myMatrix,1,prod)
# 4. myMatrix[which(myMatrix==10)]<-12
# 5. which(myMatrix >3 & myMatrix<8)
# 6. Follow code as below
dataFrame<-as.data.frame(myMatrix)
dataFrame[,12]<-as.character(dataFrame[,12])
# 7. Follow code as below
sums<-rowSums(myMatrix)
dataFrame$V13<- sums >70