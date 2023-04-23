"k-means clustering"
iris
data("iris")
names(iris)
new_data=subset(iris,select=c(-Species))
new_data

cl=kmeans(new_data,3)
cl

data=new_data
wss=sapply(1:15,
           function(k){kmeans(data,k)$tot.withinss})
plot(1:15,wss,
     type="b",pch=19,frame=FALSE,
     xlab="Number of Clusters k",
     ylab="Total Within Clusters Sum of Squares")

install.packages("cluster")
library(cluster)
clusplot(new_data,cl$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)
cl$cluster
cl$centers


"Agglomarative Clustering"
clusters=hclust(dist(iris[,3:4]))
plot(clusters)
clusterCut=cutree(clusters,3)
table(clusterCut,iris$Species)