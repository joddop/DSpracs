data4<-read.csv(file.choose(),sep=",",header=T)
t.test(data4$IQ,alternative = "greater",mu=100)