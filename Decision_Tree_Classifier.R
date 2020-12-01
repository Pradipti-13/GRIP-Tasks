#TSF-GRIP-Task-6
#Prediction using the Decision Tree Classifier. 

#Read data
data= read.csv("Iris.csv")
View(data)
data$Id= NULL
str(data)

#Split-data 
set.seed(9)
ind= sample(2, nrow(data), replace= T, prob = c(0.8, 0.2))
train= data[ind==1, ]
test= data[ind==2, ]

#Make the model
library(party)
tree= ctree(Species~., train)
tree
plot(tree, type= "simple")

#Error- train data (3.41% error)
p= predict(tree, train)
table(predicted= p, actual= train$Species)
Error= (3+1)/117
Error*100

#Error- test data (6.06% error)
p1= predict(tree, test)
table(predicted= p1, actual= test$Species)
Error1= (1+1)/33
Error1*100

#The model is a little overfitted. Because the error on test data
#is more than the train data.