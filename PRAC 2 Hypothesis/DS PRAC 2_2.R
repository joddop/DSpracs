data2=read.csv(file.choose(),sep = ",",header=T)
t.test(data2$before,data2$after,alternative="greater",paired=T)
t.test(data2$before,data2$after,alternative = "less",paired = T)
t.test(data2$before,data2$after,alternative = "two.sided",paired = T) 
