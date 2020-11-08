#GRIP-TSF-Task-2
#Prediction_Using_Unsupervised_ML(Level-Beginner)
#From given Iris Dataset, predict optimum number of clusters and represent it visually. 
#We have different clustering techniques, k-means clustering, k-medoids clustering, 
#hierarchical clustering, model-based clustering, density-based clustering. 
#We will perform K-Means clustering on the iris dataset.  

#K-Means Clustering
getwd()
set.seed(8953)
Iris= read.csv("Iris.csv")
View(Iris)
Iris$Id= NULL
nrow(Iris)
#Copy the dataset
IrisData= Iris
#Remove species to perform clustering.
IrisData$Species = NULL
View(IrisData)
str(IrisData)
summary(IrisData)
#3-Clusters
(Clusters= kmeans(IrisData, 3))
#Drawing a table between Clustered Data and Actual Data
table(Iris$Species, Clusters$cluster)
#Plotting the clusters we have
plot(IrisData[c("SepalLengthCm", "SepalWidthCm", "PetalLengthCm", "PetalWidthCm")], col=Clusters$cluster)
#From the plots, it is evident that the setosa cluster is very prominent.
#The virginica and versicolour clusters are a little mixed up, and there is not proper demarcation.

