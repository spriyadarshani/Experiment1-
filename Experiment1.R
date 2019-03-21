#Created By : Swati PRIYADARSHANI
#Goal: To Predict the no of feedback a Blog document is about to receive (Basic Features)

#Loading Training and Test Data for Basic Features (Features from 51 to 60)
#Training Data 
BFeaturesTrainData <- read.csv('C:/Users/swati/Desktop/UB/Spring 2019/ML Assignment/BlogFeedback/blogData_train.csv',header = FALSE)

#Test Data 
BFeaturesTestDataFeb1 <- read.csv('C:/Users/swati/Desktop/UB/Spring 2019/ML Assignment/BlogFeedback/blogData_test-2012.02.01.00_00.csv',header = FALSE)
BFeaturesTestDataFeb2 <- read.csv('C:/Users/swati/Desktop/UB/Spring 2019/ML Assignment/BlogFeedback/blogData_test-2012.02.02.00_00.csv',header = FALSE)
BFeaturesTestDataMar1 <- read.csv('C:/Users/swati/Desktop/UB/Spring 2019/ML Assignment/BlogFeedback/blogData_test-2012.03.01.00_00.csv',header = FALSE)
BFeaturesTestDataMar2 <- read.csv('C:/Users/swati/Desktop/UB/Spring 2019/ML Assignment/BlogFeedback/blogData_test-2012.03.02.00_00.csv',header = FALSE)

# Basic Features selected for training data (columns 51 to 60)
#Train Data
BFeaturesTrain <- BFeaturesTrainData[,c(51:60,281)]

#Test Data
BFeaturesTest1 <- BFeaturesTestDataFeb1[,c(51:60,281)]
BFeaturesTest1
BFeaturesTest2 <- BFeaturesTestDataFeb2[,c(51:60,281)]
BFeaturesTest2
BFeaturesTest3 <- BFeaturesTestDataMar1[,c(51:60,281)]
BFeaturesTest3
BFeaturesTest4 <- BFeaturesTestDataMar2[,c(51:60,281)]
BFeaturesTest4

# Build a linear model from the training data 
mdl_linear <- lm(formula = V281~.,data=BFeaturesTrain)

# Run the training examples through the linearModel
pred_train <- predict(mdl_linear, BFeaturesTrain, se.fit = TRUE)
pred_tvalue <-pred_train$fit

# Run the test examples through the linearModel

pred_linear1 <- predict(mdl_linear,BFeaturesTest1, se.fit = TRUE)
pred_value1 <- pred_linear1$fit
pred_value1

pred_linear2 <- predict(mdl_linear,BFeaturesTest2, se.fit = TRUE)
pred_value2 <- pred_linear2$fit
pred_value2

pred_linear3 <- predict(mdl_linear,BFeaturesTest3, se.fit = TRUE)
pred_value3 <- pred_linear3$fit
pred_value3

pred_linear4 <- predict(mdl_linear,BFeaturesTest4, se.fit = TRUE)
pred_value4 <- pred_linear4$fit
pred_value4

# Calculate the mean of the squared errors (MSE) 
# Train Data
mean2Err.training = mean((BFeaturesTrain$V281-pred_tvalue)^2)
mean2Err.training

# Test Data
mean2Err.test1 = mean((BFeaturesTest1$V281-pred_value1)^2)
mean2Err.test1

mean2Err.test2 = mean((BFeaturesTest2$V281-pred_value2)^2)
mean2Err.test2

mean2Err.test3 = mean((BFeaturesTest3$V281-pred_value3)^2)
mean2Err.test3

mean2Err.test4 = mean((BFeaturesTest4$V281-pred_value4)^2)
mean2Err.test4

summary(mdl_linear)
