#Created By : Swati PRIYADARSHANI
#Goal: To Predict the no of feedback a Blog document is about to receive

#Loading Training and Test Data for Textual Features (Features from 51 to 60)
#Training Data
TextFeaturesTrainData <- read.csv('C:/Users/swati/Desktop/UB/Spring 2019/ML Assignment/BlogFeedback/blogData_train.csv',header = FALSE)

#Test Data
TextFeaturesTestDataFeb1 <- read.csv('C:/Users/swati/Desktop/UB/Spring 2019/ML Assignment/BlogFeedback/blogData_test-2012.02.03.00_00.csv',header = FALSE)
TextFeaturesTestDataFeb2 <- read.csv('C:/Users/swati/Desktop/UB/Spring 2019/ML Assignment/BlogFeedback/blogData_test-2012.02.04.00_00.csv',header = FALSE)
TextFeaturesTestDataMar1 <- read.csv('C:/Users/swati/Desktop/UB/Spring 2019/ML Assignment/BlogFeedback/blogData_test-2012.03.18.00_00.csv',header = FALSE)
TextFeaturesTestDataMar2 <- read.csv('C:/Users/swati/Desktop/UB/Spring 2019/ML Assignment/BlogFeedback/blogData_test-2012.03.19.00_00.csv',header = FALSE)

# Textual Features selected for training data (columns 63 to 262)
#Training Data
TextFeaturesTrain <- BFeaturesTrainData[,c(63:262,281)]

#Testing Data
TextFeaturesTest1 <- BFeaturesTestDataFeb1[,c(63:262,281)]
TextFeaturesTest1
TextFeaturesTest2 <- BFeaturesTestDataFeb2[,c(63:262,281)]
TextFeaturesTest2
TextFeaturesTest3 <- BFeaturesTestDataMar1[,c(63:262,281)]
TextFeaturesTest3
TextFeaturesTest4 <- BFeaturesTestDataMar2[,c(63:262,281)]
TextFeaturesTest4

# Build a linear model from the training data 
mdl_linear <- lm(formula = V281~.,data = TextFeaturesTrain)

# Run the training examples through the linearModel
pred_train <- predict(mdl_linear, TextFeaturesTrain, se.fit = TRUE)
pred_tvalue <-pred_train$fit

# Run the test examples through the linearModel

pred_linear1 <- predict(mdl_linear,TextFeaturesTest1, se.fit = TRUE)
pred_value1 <- pred_linear1$fit
pred_value1

pred_linear2 <- predict(mdl_linear,TextFeaturesTest2, se.fit = TRUE)
pred_value2 <- pred_linear2$fit
pred_value2

pred_linear3 <- predict(mdl_linear,TextFeaturesTest3, se.fit = TRUE)
pred_value3 <- pred_linear3$fit
pred_value3

pred_linear4 <- predict(mdl_linear,TextFeaturesTest4, se.fit = TRUE)
pred_value4 <- pred_linear4$fit
pred_value4

# Calculate the mean of the squared errors (MSE) 
mean2Err.training = mean((BFeaturesTrain$V281-pred_tvalue)^2)
mean2Err.training

mean2Err.test1 = mean((BFeaturesTest1$V281-pred_value1)^2)
mean2Err.test1

mean2Err.test2 = mean((BFeaturesTest2$V281-pred_value2)^2)
mean2Err.test2

mean2Err.test3 = mean((BFeaturesTest3$V281-pred_value3)^2)
mean2Err.test3

mean2Err.test4 = mean((BFeaturesTest4$V281-pred_value4)^2)
mean2Err.test4

summary(mdl_linear)
