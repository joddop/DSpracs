ftest=read.csv(file.choose(),sep=",",header=T)
var.test(ftest$G1,ftest$G2,alternative="two.sided")


