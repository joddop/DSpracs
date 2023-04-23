data3<-read.csv(file.choose(),sep = ",",header=T)
cor.test(data3$mpg,data3$wt,alternative = "two.sided",method="pearson")