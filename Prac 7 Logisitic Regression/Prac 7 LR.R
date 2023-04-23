loan=read.csv(file.choose(),header=T,sep=",")
head(loan)
summary(loan)
str(loan)
loan$AGE=as.factor(loan$AGE)
str(loan)
names(loan)
"Creating Model"
model1=glm(DEFAULTER~.,family = binomial, data = loan)
summary(model1)
"Global Testing for acceptance of the model"
null=glm(DEFAULTER~1,family=binomial,data=loan)
anova(null,model1,test="Chisq")
"predicting the probabilities"
loan$predprob=round(fitted(model1),2)
head(loan)
"classification and misclassification analysis"
install.packages("gmodels")
library(gmodels)
table(loan$DEFAULTER,fitted(model1)>0.5)
sens=95/(88+95)*100
sens
spc=478/(478+39)*100
spc
"check the trade off between sensitvity & specificity"
table(loan$DEFAULTER,fitted(model1)>0.1)
table(loan$DEFAULTER,fitted(model1)>0.2)
table(loan$DEFAULTER,fitted(model1)>0.3)
table(loan$DEFAULTER,fitted(model1)>0.4)
table(loan$DEFAULTER,fitted(model1)>0.5)

"Goodness of Fit Using Receiver Operational Curve"
pred=predict(model1,loan,type="response")
install.packages("ROCR")
library(ROCR)
rocrpred=prediction(pred,loan$DEFAULTER)
rocrperf=performance(rocrpred,"tpr","fpr")
"to check proper cut off point"
plot(rocrperf,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
"to check coefficients"
exp(coef(model1))