
#Import Cereals dataset

Cereals<-read.csv("D:/MACHINE LEARNING/Assignment5/Cereals.csv")

#Q)Data Preprocessing. Remove all cereals with missing values.
#Removing missing values that might be present in the data

sum(is.na(Cereals))
CS<-na.omit(Cereals)
sum(is.na(CS)) #To verify if the missing values are removed
CS1<-CS[,c(-1,-2,-3)]

#Scaling the data

CSF1<-scale(CS1)

library(cluster)   # clustering algorithms
library(tidyverse) # data manipulation
library(factoextra)# clustering visualization

#Q1)Apply hierarchical clustering to the data using Euclidean distance to the normalized 
#measurements. Use Agnes to compare the clustering from single linkage, complete 
#linkage, average linkage, and Ward. Choose the best method.

# Applying hierarchical clustering to the data using Euclidean distance 
# Dissimilarity matrix

dc <- dist(CSF1, method = "euclidean")

# Hierarchical clustering using Complete Linkage

hc_complete <- hclust(dc, method = "complete" )

# Plot the obtained dendogram

plot(hc_complete, cex = 0.6, hang = -1)

#Use Agnes to compare the clustering from single linkage, complete linkage, average linkage, and Ward

c_single<-agnes(CSF1,method= "single")
c_complete<-agnes(CSF1,method = "complete")
c_average<-agnes(CSF1,method = "average")
c_ward<-agnes(CSF1,method = "ward")

#Comparing the agglomerative coefficients of Single,complete,average and ward methods

c_single$ac
c_complete$ac
c_average$ac
c_ward$ac 

#By observing the above values we can say that the best linkage method is ward with agglomerative coefficient of 0.9046042

# visualizing the dendogram using wards method:

pl<-pltree(c_ward,cex=0.6,hang=-1,main = "dendrogram of agnes-wards method")

#Q2)How many clusters would you choose?

#Create the distance matrix

dc<-dist(CSF1,method = "euclidean")

# Wards method for Hierarchical clustering
w_hc<-hclust(dc,method = "ward.D2")

# plotting dendrogram and taking k=2 by observing the distance

plot(w_hc,cex=0.6)
rect.hclust(w_hc,k=2,border = 1:2) 

#For identifying clusters, cut the dendrogram with cutree()

clust<-cutree(w_hc, k=2)

# Number of members in each cluster

table(clust)

#k=2 is cutting the longest path, so I choose k=2.

#Q3)Comment on the structure of the clusters and on their stability.

library(dendextend) # for comparing two dendrograms
library(knitr)
set.seed(123)
Cereals_New<-Cereals

#Removing any missing values that might be present in the data

nd<-na.omit(Cereals_New) 
nd1<-nd[,c(-1,-2,-3)]
nd2<-scale(nd1)
nd3<-as.data.frame(nd2)

#Divide the data and create partitions
p1<-nd[1:55,]
p2<-nd[56:74,]

#Perform clustering using agnes() with single, complete, average and ward with partitioned data

r1<- agnes(scale(p1[,-c(1:3)]),method ="ward")
r2<- agnes(scale(p1[,-c(1:3)]),method="average")
r3<- agnes(scale(p1[,-c(1:3)]),method="complete")
r4<- agnes(scale(p1[,-c(1:3)]),method="single")
cbind(ward=r1$ac,average=r2$ac,complete=r3$ac,single=r4$ac)

c2<-cutree(r1, k=2)

#Calculate the centers
cc<-as.data.frame(cbind(scale(p1[,-c(1:3)]),c2))

center1<-colMeans(cc[cc$c2==1,])

center2<-colMeans(cc[cc$c2==2,])

#Bind the 2 centers

centers<-rbind(center1,center2)
centers

#Calculating Distance
x<-as.data.frame(rbind(centers[,-14],scale(p2[,-c(1:3)])))
y1<-get_dist(x)
y2<-as.matrix(y1)
d1<-data.frame(data=seq(1,nrow(p2),1),clusters=rep(0,nrow(p2)))
for(i in 1:nrow(p2))
{
  d1[i,2]<-which.min(y2[i+2,1:2])
}
d1
y3<-as.data.frame(cbind(CSF1,clust))
cbind(y3$clust[56:74],d1$clusters)
table(y3$clust[56:74]==d1$clusters)

#From the above observed values, we can say that the clusters are fairly stable.

#Q4)Healthy cereals

r<-cbind(nd3,clust)
r[r$clust==1,]
r[r$clust==2,]

#Calculating mean ratings to determine the best cluster.

mean(r[r$clust==1,"rating"])
mean(r[r$clust==2,"rating"])

#From the above cluster analysis, as Cluster 1 has high rating values, we can infer this cluster has more nutrition values.So Cluster1 is healthy for kids

#Since we using distance metric algorithm we essentially need to normalize data, as the features of data are different, hence we need to scale it to similar features.