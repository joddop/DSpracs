"use titanic.csv"
titanic=read.csv(file.choose(),header=T)
library(rpart)
fit=rpart(Survived ~ Pclass + Gender + Age + SibSp + Parch + Fare
          + Embarked,data=titanic, method="class")
plot(fit)
text(fit)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

Prediction=predict(fit,titanic,type="class")
Prediction