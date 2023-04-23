eda_data=read.csv(file.choose(),header=TRUE,sep=',')
eda_data
head(eda_data)
summary(eda_data)
str(eda_data)
eda_data[1:8]
head(eda_data,3)
head(eda_data,8)
tail(eda_data,8)
eda_data[,1:5]
newdata1=subset(eda_data,eda_data$Education == "Grad")
newdata1
newdata2=subset(eda_data,eda_data$Age=="51" & eda_data$Gender == "M")
newdata2
a=eda_data[order(eda_data$Education),]
a
a=eda_data[order(eda_data$Education,decreasing=TRUE),]
a
a=colSums(is.na(eda_data))
a
hist(eda_data$Age)
boxplot(eda_data$Age)
mean(eda_data$Age)
min(eda_data$Age)
max(eda_data$Age)
median(eda_data$Age)
counts=table(eda_data$Education,eda_data$Gender)
counts
help("pie")
plot(eda_data$Age,eda_data$Salary)
plot(eda_data$Salary,type="p",col="red",main="Salary",xlim=c(0,10),ylim=c(0,10))
hist(eda_data$Age,col="green",border="red",xlim=c(20,70),ylim=c(0,70))
barplot(counts,main="Data distribution by Education v/s Gender", 
        col=c("blue","red"),legend.text=TRUE,xlim=c(0,10))
x=table(eda_data$Education)
per=round(x/sum(x)*100,1)
pie(x,labels=per,main="City Pie Chart", col=c("blue","red"))
