data1=read.csv(file.choose(),sep = ",",header=T)
t.test(data1$UK,data1$Germany,alternative="two.sided",var.equal=FALSE)