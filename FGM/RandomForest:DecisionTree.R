#STAT 260 Project

#Read in data
fgm.dat <- read.csv(file =  "/Users/yutongsheng/Downloads/FGMClean1.csv", header = TRUE)

#Data Cleaning: Remove "No" from FGM status and people under 12
subFGM <- fgm.dat[fgm.dat$FGM_Status != "No",]
fgm2.dat <- subFGM[subFGM$Current_Age > 12, ]

#Data Cleaning: Make location, education, and support of practice into factor variables
fgm2.dat$LocationClean <- as.factor(fgm2.dat$Location)
fgm2.dat$EducationLevelClean <- as.factor(fgm2.dat$Education_Level)
fgm2.dat$SupportPracticeClean <- as.factor(fgm2.dat$Support_Practice)

#Summarize data
dim(fgm2.dat)
summary(fgm2.dat)
table(fgm2.dat$Support_Practice)
table(fgm2.dat$FGM_Status)

#Create visualization of age and whether they support the practice
boxplot(fgm2.dat$FGM_Age~fgm2.dat$Support_Practice,
        xlab = 'Supports Practice',
        ylab = 'Age of FGM')
#Visualization of age of FGM and location
boxplot(fgm2.dat$FGM_Age~fgm2.dat$LocationClean,
        xlab = 'Location',
        ylab = 'Age of FGM')

#____________________________INITIAL DATA VISUALIZATIONS_________________________________#
#Heat map visualizations for our education and location

#Education Heat Map
#make table with education and support
educationHeatMap <- table(fgm2.dat$SupportPracticeClean, fgm2.dat$EducationLevelClean)
#Create heat map from table
heatmap(educationHeatMap, scale= "none", Rowv = NA, Colv = NA) 
#Add points to heat map that have the number of people in each category
text(fgm2.dat$EducationLevelClean, cex=1)
#make table with three var
?heatmap

#Location Heat Map
#make table with location and support
locationHeatMap <- table(fgm2.dat$SupportPracticeClean, fgm2.dat$LocationClean)
#Create heat map from location heat map table
heatmap(locationHeatMap, scale = "none", Rowv = NA, Colv = NA)

#Scatterplot with spline visualization for age variable
#FGM support on y axis as a 0/1 predictor, age on x axis
#Points should be white/transparent
#There will be a spline to fit the data which shows the proportion of people who support FGM smoothed over age
library(tidyverse)
fgm3.dat <- fgm2.dat %>%
  mutate(SupportPracticeClean2 = case_when(SupportPracticeClean =='Yes' ~1,
                                           SupportPracticeClean=='No' ~ 2))

supportSpline <- smooth.spline(x = fgm3.dat$Current_Age, y= fgm3.dat$SupportPracticeClean2)
plot(fgm3.dat$Current_Age, fgm3.dat$SupportPracticeClean2,
     col = 'antiquewhite3',
     pch = 19,
     xlab = 'Current Age',
     ylab = 'Support of Practice')
points(supportSpline$x, supportSpline$y, type= 'l', col= 'red')
legend('left', 
       bg= 'lightblue',
       box.lwd = 2,
       fill = 'antiquewhite3',
       legend= c("1 - Yes", "2 - No")) 



#____________________________Three Packages_________________________________#
#__________Random Forest___________#
#Random Forest Tree to figure out which predictor is the most important
#Install packages
library(party)
library(randomForest)

testForestFGM <- randomForest(SupportPracticeClean~EducationLevelClean+LocationClean+Current_Age, data = fgm2.dat,
                              ntree = 600, importance = TRUE)
?randomForest
#Use 'importance' function to see which variable is the most important
#"No" is the mean decrease in accuracy for the no column
#"Yes" is the mean decrease in accuracy for the yes column
#MDA is the accuracy of a single tree model
testForestFGM$importance

#Create and plot tree for men and women variables
FGMTree<-ctree(SupportPracticeClean~LocationClean+FGM.Age, data = fgm.dat)
FGMTree1<-ctree(SupportPracticeClean~LocationClean+FGM.Age+EducationLevelClean, data = fgm.dat)

plot(FGMTree)

#__________RPART Tree___________#

install.packages("RColorBrewer")
install.packages("rattle")
#rpart tree 
library(rpart)
# To visualize the tree
library(rattle)
library(RColorBrewer)
library(rpart.plot)
################################

#Basic Rpart tree 
# Selecting only the relevant columns
fgm2.subset <- fgm2.dat[, c('EducationLevelClean', 'LocationClean', 'Current_Age', 'SupportPracticeClean')]
dim(fgm2.subset)
# Create the decision tree model
rpart1 <- rpart(SupportPracticeClean~EducationLevelClean+LocationClean+Current_Age, 
                data = fgm2.subset, 
                method = "class")
rpart1$variable.importance
fancyRpartPlot(rpart1, caption="FGM Rpart Tree")
summary(rpart1)
printcp(rpart1)
plotcp(rpart1)
#rpart1 prediction 
predict1results <- predict(rpart1)
colnames(predict1results)
if (dim(predict1results)[1] == 312 && dim(predict1results)[2] == 2) {
  # Initialize an empty vector to store the Yes or No 
  rpart1names <- character(312)
  
  # Loop through each row
  for (i in 1:nrow(predict1results)) {
    max_index <- which.max(predict1results[i, ])
    rpart1names[i] <- colnames(predict1results)[max_index]
  }
  
  print(rpart1names)
} else {
  print("The matrix does not have the correct dimensions (312x2).")
}

fgm2.subset$predcit1 <- rpart1names
# Calculate accuracy

accuracy1 <- sum(fgm2.subset$predcit1 == fgm2.subset$SupportPracticeClean) / nrow(fgm2.subset)
#accuracy1 is 0.8525
accuracy1

# Create the  table
prediction_matrix1 <- table(Actual = fgm2.subset$SupportPracticeClean, Predicted = fgm2.subset$predcit1)

# Print the table
print(prediction_matrix1)

################################
#Change minbucket  
rpart2 <- rpart(SupportPracticeClean~EducationLevelClean+LocationClean+Current_Age, 
                data = fgm2.subset, 
                method = "class",
                parms = list(split = 'information'),
                minbucket = 20)
fancyRpartPlot(rpart2, caption="FGM Rpart Tree")
rpart2$variable.importance

#rpart2 prediction
predict2results <- predict(rpart2)
colnames(predict2results)
if (dim(predict2results)[1] == 312 && dim(predict2results)[2] == 2) {
  # Initialize an empty vector to store the Yes or No 
  rpart2names <- character(312)
  
  # Loop through each row
  for (i in 1:nrow(predict2results)) {
    max_index <- which.max(predict2results[i, ])
    rpart2names[i] <- colnames(predict2results)[max_index]
  }
  
  print(rpart2names)
} else {
  print("The matrix does not have the correct dimensions (312x2).")
}

fgm2.subset$predcit2 <- rpart2names
# Calculate accuracy

accuracy2 <- sum(fgm2.subset$predcit2 == fgm2.subset$SupportPracticeClean) / nrow(fgm2.subset)
#accuracy2 is 0.839
accuracy2
prediction_matrix2 <- table(Actual = fgm2.subset$SupportPracticeClean, Predicted = fgm2.subset$predcit2)

# Print the table
print(prediction_matrix2)

################################
#Change complexity  
#cp = -1- fully grown tree - overfitting 
rpart3 <- rpart(SupportPracticeClean~EducationLevelClean+LocationClean+Current_Age, 
                data = fgm2.subset, 
                method = "class",
                parms = list(split = 'information'),
                cp = -1)
fancyRpartPlot(rpart3, caption="FGM Rpart Tree")

printcp(rpart1)
rpart3$variable.importance

#rpart3 prediction
predict3results <- predict(rpart3)
colnames(predict3results)
if (dim(predict3results)[1] == 312 && dim(predict3results)[2] == 2) {
  # Initialize an empty vector to store the Yes or No 
  rpart3names <- character(312)
  
  # Loop through each row
  for (i in 1:nrow(predict3results)) {
    max_index <- which.max(predict3results[i, ])
    rpart3names[i] <- colnames(predict3results)[max_index]
  }
  
  print(rpart3names)
} else {
  print("The matrix does not have the correct dimensions (312x2).")
}

fgm2.subset$predcit3 <- rpart3names
# Calculate accuracy

accuracy3 <- sum(fgm2.subset$predcit3 == fgm2.subset$SupportPracticeClean) / nrow(fgm2.subset)
accuracy3

# Create the  table
prediction_matrix3 <- table(Actual = fgm2.subset$SupportPracticeClean, Predicted = fgm2.subset$predcit3)

# Print the table
print(prediction_matrix3)

#__________Party Tree___________#

#Defualt tree 

tree_model <- ctree(Support_Practice ~ Location + Education_Level + Current_Age, data = fgm2.dat)
plot(tree_model)

response_colors <- c("No" = "blue", "Yes" = "red")
plot(tree_model, 
     terminal_panel = node_barplot(tree_model, 
                                   fill = response_colors))


?party
predicted <- predict(tree_model, newdata = fgm2.dat)
actual <- fgm2.dat$Support_Practice 
accuracy_matrix <- table(Predicted = predicted, Actual = actual)
print(accuracy_matrix)
accuracy <- sum(predicted == actual) / length(actual) 

# model
#change paramater:minsplit
controls1 <- ctree_control(minsplit = 6) # I tried with differnt numbers here and result remained the same 
tree_model1 <- ctree(Support_Practice ~ Location + Education_Level + Current_Age, data = fgm2.dat, controls = controls1)
plot(tree_model1)
response_colors <- c("No" = "blue", "Yes" = "red")
plot(tree_model1, 
     terminal_panel = node_barplot(tree_model1, 
                                   fill = response_colors))

predicted2 <- predict(tree_model1, newdata = fgm2.dat)
actual2 <- fgm2.dat$Support_Practice 
accuracy_matrix <- table(Predicted = predicted2, Actual = actual2)
print(accuracy_matrix)
accuracy <- sum(predicted2 == actual2) / length(actual2) 

# Model2 
#Tree with different maximum depth
controls2 <- ctree_control(maxdepth = 10) #tried with different maxdepths and result remained the same
tree_model2 <- ctree(Support_Practice ~ Location + Education_Level + Current_Age, data = fgm2.dat, controls = controls2)
plot(tree_model2)

response_colors <- c("No" = "blue", "Yes" = "red")
plot(tree_model2, 
     terminal_panel = node_barplot(tree_model2, 
                                   fill = response_colors))


predicted3 <- predict(tree_model, newdata = fgm2.dat)
actual3 <- fgm2.dat$Support_Practice 
accuracy_matrix <- table(Predicted = predicted3, Actual = actual3)
print(accuracy_matrix)
accuracy <- sum(predicted3 == actual3) / length(actual3) 






