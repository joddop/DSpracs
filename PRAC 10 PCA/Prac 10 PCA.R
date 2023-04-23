data_iris=iris[1:4]
data_iris

Cov_data=cov(data_iris)
Cov_data

#Find out the eigenvectors and eigenvalues using the covariance matrix
Eigen_data=eigen(Cov_data)
Eigen_data

#Using the inbuilt function
PCA_data=princomp(data_iris,cor="False")
PCA_data

#Lets now compare the output variances
Eigen_data$values
PCA_data$dev^2

PCA_data$loadings[,1:4]
Eigen_data$vectors

summary(PCA_data)
biplot(PCA_data)
screeplot(PCA_data,type="lines")

#Select the first principal component for the second model
model2=PCA_data$loadings[,1]
model2

#for the second model
model2_scores=as.matrix(data_iris)%*%model2
model2_scores

#Loading Libraries for naiveBayes model
library(class)
install.packages("e1071")
library(e1071)

#Fitting the first model over the entire data
mod1=naiveBayes(iris[,1:4], iris[,5])
mod1

#Fitting the second model using the first principal component
mod2=naiveBayes(model2_scores,iris[,5])
mod2

#Accuracy for first model
table(predict(mod1,iris[,1:4]),iris[,5])

#Accuracy for second model
table(predict(mod2,model2_scores),iris[,5])

round(cor(PCA_data$score))