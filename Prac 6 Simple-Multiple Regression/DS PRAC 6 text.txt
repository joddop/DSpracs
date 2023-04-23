"linear regression"
"use index.csv here"
house<-read.csv(file.choose(),sep=",",header=T)
summary(house)
names(house)
pairs(~death_rate+doctor_avail+hosp_avail+annual_income+density_per_capita,data=house)
housemodel<-lm(density_per_capita~death_rate+doctor_avail+hosp_avail+annual_income,data=house)
summary(housemodel)

"use index1.csv here"
index<-read.csv(file.choose(),sep=",",header=T)
names(index)
pairs(~index+written+language+tech+gk,data=index)
model1<-lm(index~.,data=index)
summary(model1)

index$pred<-fitted(model1)
head(index)
index$res<-residuals(model1)
head(index)


"to check multicolinearity"
install.packages("car")
library(car)
vif(model1)

"plot must be random indicates no heteroscedasticity"
plot(index$pred,index$res,col="red")

"errors are assumed to be normally distributed"
shapiro.test(index$res)


"detecting heteroscedasticity using ncvtest"
library(car)
ncvTest(model1,~written+language+tech+gk)

"detecting autocorelation using derbin watson test d=2(1-r)"
library(car)
durbinWatsonTest(model1)

"influence plot"
influencePlot(model1)
index=index[-33,]


"validation using k fold method "
install.packages("caret")
library("caret")
kfolds<-trainControl(method = "cv",number = 4)
modelkfold<-train(index~written+language+tech+gk,data = index,method="lm",trControl=kfolds)
modelkfold

"validation using repetative k fold"
kfoldsrp=trainControl(method="repeatedcv",number=4,repeats=5)
modelkfoldsrp=train(index~written+language+tech+gk,data=index,method="lm",trControl=kfolds)
modelkfoldsrp

"validation using leave one out"
kfoldsloocv<-trainControl(method = "LOOCV")
kfoldsloocvmodel<-train(index~written+language+tech+gk,data = index,method="lm",trControl=kfoldsloocv)
kfoldsloocvmodel

"model selection forward"
null=lm(index~1,data=index)
full=lm(index~written+language+tech+gk,data=index)
names(index)
step(null,scope=list(lower=null,upper=full),direction="forward")

"model selection backward"
step(full,scope=list(lower=null,upper=full),direction="backward")






