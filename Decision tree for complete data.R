# Decision Tree Classifier
# Reading and Preparing data
Iris= read.csv("Iris.csv")
View(Iris)
Iris$Id= NULL
str(Iris)

# Making a model 
library(rpart)
Rtree= rpart(Species~., 
             data, 
             minsplit= 2, 
             minbucket= 1, 
             cp= -1)
Rtree

# Visualisation of the tree
library(rpart.plot)
rpart.plot(Rtree,
           extra= 2)