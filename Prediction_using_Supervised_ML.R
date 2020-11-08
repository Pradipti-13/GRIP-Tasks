#GRIP-TSF-Task-1
#Prediction using Supervised ML (Level-Beginner)
#Prediction of percentage of a student based on number of study hours by linear regression.
getwd()
Student_Scores= read.csv("student_scores.csv")
View(Student_Scores)
#We can see that there is no missing data.
#Let us see the summary of the data. 
summary(Student_Scores)
library(skimr)
skim(Student_Scores)
#Plotting to visualise 
plot(Student_Scores$Scores, Student_Scores$Hours)
#Employing Linear Regression Model
lrmodel= lm(Scores~Hours, data=Student_Scores)
summary(lrmodel)
#Prediction of the New Data Given- A student studies for 9.25 hrs/day.
predictdata= predict(lrmodel, data.frame(Hours= 9.25))
predictdata
#Output- For 9.25 Hours of studying, a student is predicted to get 92.90985 marks according to our model.

