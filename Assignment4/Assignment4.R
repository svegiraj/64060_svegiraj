#Read the Pharmaceuticals dataset to Pharma
Pharma<-read.csv("D:/MACHINE LEARNING/Assignment4/Pharmaceuticals.csv")
head(Pharma)

#a)Use only the numerical variables (1 to 9) to cluster the 21 firms. Justify the various choices made in 
#conducting the cluster analysis, such as weights for different variables, the specific clustering 
#algorithm(s) used, the number of clusters formed, and so on.

#Data Preparation
#Prior to clustering data, remove or estimate missing data and rescale variables for comparability.
x <- na.omit(Pharma)# listwise deletion of missing
#collecting only the quantitative variables(1-9) to cluster the 21 firms
Pharma1<-Pharma[,3:11]
head(Pharma1)
#Scale all the quantitative variables in the dataframe (Z-score standardize data)
Pharma2<-scale(Pharma1)
head(Pharma2)

#Determining the number of clusters to do the cluster analysis
#Calculating number of clusters using Elbow Method 
#install.packages("factoextra")
library(factoextra)   
fviz_nbclust(Pharma2, kmeans, method = "wss") + labs(subtitle = "Elbow Method")

#Silhouette method for determining number of clusters
fviz_nbclust(Pharma2, kmeans, method = "silhouette")+ labs(subtitle = "Silhouette Method")

set.seed(1)
k5<- kmeans(Pharma2,centers=5,nstart = 25) 
#Visualize the output
k5$centers #centroids
fviz_cluster(k5,data = Pharma2)#Visualize the clusters

#K-Means Cluster Analysis- Fit the data with 5 clusters
fit<-kmeans(Pharma2,5)

#Finding the mean value of all quantitative variables for each cluster
aggregate(Pharma2,by=list(fit$cluster),FUN=mean)

#append cluster assignment
Pharma3<-data.frame(Pharma2,fit$cluster)
Pharma3
#Visualize the cluster plot
library(cluster)
clusplot(Pharma2,fit$cluster,color = TRUE,shade = TRUE,labels = 2,lines = 0)

#b) Interpret the clusters with respect to the numerical variables used in forming the clusters.
#Cluster 1 - Row 8,9,12,14
#Cluster 2 - Row 1,4,7,10,16,19,21
#Cluster 3 - Row 2,6,18
#Cluster 4 - Row 3,5,20
#Cluster 5 - Row 11,13,15,17
#By observing the mean values of all quantitative variables for each cluster
#Cluster 1 has the lowest Market_Cap,highest Beta,lowest PE_Ratio,highest Leverage,highest Rev_Growth.
#Cluster 2 has the lowest Rev_Growth,highest Net_Profit_Margin
#Cluster 3 has the highest PE_Ratio,lowest ROE,lowest ROA,lowest Net_Profit_Margin
#Cluster 4 has the lowest Beta,lowest Asset_Turnover
#Cluster 5 has the highest Market_Cap,highest ROE, highest ROA,highest Asset_Turnover,lowest Leverage

#c) Is there a pattern in the clusters with respect to the numerical variables (10 to 12)? (those not used in forming the clusters)
   
#There is a pattern in the clusters with respect to Media recommendation variable.
#Cluster 1 with highest Beta,highest Leverage,highest Rev_Growth has mostly Moderate Buy Recommendation.
#Cluster 2 with highest Net_Profit_Margin has mostly Hold Recommendation. 
#Cluster 3 with highest PE_Ratio has Hold Recommendation
#Cluster 5 with highest Market_Cap,highest ROE, highest ROA,highest Asset_Turnover has equal Hold and Moderate Buy Recommendation
#Cluster 4 with lowest Asset_Turnover has the Strong Buy Recommendation

#Could see a pattern among the clusters with respect to variables(10 to 12)
#Clusters 1,4 has mostly Moderate Buy Recommendation
#Clusters 2,3 has mostly Hold Recommendation
                                                        
#d) Provide an appropriate name for each cluster using any or all of the variables in the dataset.

#Cluster1 - low Market_Cap,high Beta,low PE_Ratio,high Leverage,high Rev_Growth cluster
#Cluster2 - low Rev_Growth,high Net_Profit_Margin cluster
#Cluster3 - high PE_Ratio,low ROE,low ROA,low Net_Profit_Margin cluster
#Cluster4 - low Beta,low Asset_Turnover cluster
#Cluster5 - high Market_Cap,high ROE, high ROA,high Asset_Turnover,low Leverage cluster




