---
title: "Excercise Quality Study"
author: "Ivan Jennings"
date: "03/04/2021"
output:
  html_document:
    keep_md: yes
---



## Executive Summary

In this project, my goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants using devices such as Jawbone Up, Nike FuelBand, and Fitbit to predict how well the subjects are exercising. I will be using data from here http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har

The data on quality of exercise is in the "classe" column which has 5 levels - A, B, C, D & E, with A being "the exercise was completed correctly" and the rest being some sort of error the partipant has made during their exercise. I will be predicting the "classe" from a sample of 20 data points.

## Loading Data

First I will load necessary R libraries that I will need to complete my analysis.


```r
library(dplyr)
library(caret)
library(lubridate)
library(ggplot2)
library(randomForest)
library(e1071)
library(GGally)
```

Now I will download the data, load it into R and clean the data for analysis. I will import the testing set as "validation" as I will split the training data further for later validation.


```r
training_file_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testing_file_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(training_file_url, "training_test.csv", method = "curl")
download.file(testing_file_url, "validation.csv", method = "curl")
training_test <- read.csv(file.path(getwd(), "training_test.csv"), row.names = 1, stringsAsFactors = FALSE)
validation <- read.csv(file.path(getwd(), "validation.csv"), row.names = 1, stringsAsFactors = FALSE)
```

## Clean Data

Now that we have the data loaded in, let's explore the data using a few different R commands such as str(testing), str(training) - I haven't included the output here as there are 159 columns as we can see here from the dim command.


```r
dim(training_test)
```

```
## [1] 19622   159
```

Let's go ahead and clean up some of these variables.


```r
## get columns with no data in validation set
rows_with_data <- !as.vector(sapply(validation, function(x)all(is.na(x))))
## drop columns without data in validation set
training_test <- training_test[,rows_with_data]
validation <- validation[,rows_with_data]
## convert char col to factor
training_test$classe <- factor(training_test$classe)
## remove unnecessary data
training_test <- training_test %>%
  select(-c(1:6))
validation <- validation %>%
  select(-c(1:6))
```

Let's check for NAs now that we have cleaned up our data


```r
colSums(is.na(training_test))
```

```
##            roll_belt           pitch_belt             yaw_belt 
##                    0                    0                    0 
##     total_accel_belt         gyros_belt_x         gyros_belt_y 
##                    0                    0                    0 
##         gyros_belt_z         accel_belt_x         accel_belt_y 
##                    0                    0                    0 
##         accel_belt_z        magnet_belt_x        magnet_belt_y 
##                    0                    0                    0 
##        magnet_belt_z             roll_arm            pitch_arm 
##                    0                    0                    0 
##              yaw_arm      total_accel_arm          gyros_arm_x 
##                    0                    0                    0 
##          gyros_arm_y          gyros_arm_z          accel_arm_x 
##                    0                    0                    0 
##          accel_arm_y          accel_arm_z         magnet_arm_x 
##                    0                    0                    0 
##         magnet_arm_y         magnet_arm_z        roll_dumbbell 
##                    0                    0                    0 
##       pitch_dumbbell         yaw_dumbbell total_accel_dumbbell 
##                    0                    0                    0 
##     gyros_dumbbell_x     gyros_dumbbell_y     gyros_dumbbell_z 
##                    0                    0                    0 
##     accel_dumbbell_x     accel_dumbbell_y     accel_dumbbell_z 
##                    0                    0                    0 
##    magnet_dumbbell_x    magnet_dumbbell_y    magnet_dumbbell_z 
##                    0                    0                    0 
##         roll_forearm        pitch_forearm          yaw_forearm 
##                    0                    0                    0 
##  total_accel_forearm      gyros_forearm_x      gyros_forearm_y 
##                    0                    0                    0 
##      gyros_forearm_z      accel_forearm_x      accel_forearm_y 
##                    0                    0                    0 
##      accel_forearm_z     magnet_forearm_x     magnet_forearm_y 
##                    0                    0                    0 
##     magnet_forearm_z               classe 
##                    0                    0
```

```r
colSums(is.na(validation))
```

```
##            roll_belt           pitch_belt             yaw_belt 
##                    0                    0                    0 
##     total_accel_belt         gyros_belt_x         gyros_belt_y 
##                    0                    0                    0 
##         gyros_belt_z         accel_belt_x         accel_belt_y 
##                    0                    0                    0 
##         accel_belt_z        magnet_belt_x        magnet_belt_y 
##                    0                    0                    0 
##        magnet_belt_z             roll_arm            pitch_arm 
##                    0                    0                    0 
##              yaw_arm      total_accel_arm          gyros_arm_x 
##                    0                    0                    0 
##          gyros_arm_y          gyros_arm_z          accel_arm_x 
##                    0                    0                    0 
##          accel_arm_y          accel_arm_z         magnet_arm_x 
##                    0                    0                    0 
##         magnet_arm_y         magnet_arm_z        roll_dumbbell 
##                    0                    0                    0 
##       pitch_dumbbell         yaw_dumbbell total_accel_dumbbell 
##                    0                    0                    0 
##     gyros_dumbbell_x     gyros_dumbbell_y     gyros_dumbbell_z 
##                    0                    0                    0 
##     accel_dumbbell_x     accel_dumbbell_y     accel_dumbbell_z 
##                    0                    0                    0 
##    magnet_dumbbell_x    magnet_dumbbell_y    magnet_dumbbell_z 
##                    0                    0                    0 
##         roll_forearm        pitch_forearm          yaw_forearm 
##                    0                    0                    0 
##  total_accel_forearm      gyros_forearm_x      gyros_forearm_y 
##                    0                    0                    0 
##      gyros_forearm_z      accel_forearm_x      accel_forearm_y 
##                    0                    0                    0 
##      accel_forearm_z     magnet_forearm_x     magnet_forearm_y 
##                    0                    0                    0 
##     magnet_forearm_z           problem_id 
##                    0                    0
```

We can see that there are 0 columns with NA values since we removed the columns that we do not have data for in the final validation set.

## Explore Data

Now that we have a clean set of data, let's split up our data as follows:

training_test (originally training data) will be split 70/30 into training and test data.
validation (originally test data) will be left seperate and we will use part of the original training set as test data instead so that we will only need to use the validation set once at the end.


```r
set.seed(444)
inTrain <- createDataPartition(training_test$classe, p=0.7, list=FALSE)
training <- training_test[inTrain,]
testing <- training_test[-inTrain,]
```

Now that we have split the data into testing and training data we can start to explore the training data further. First let's remove any variables which are correlated with the findCorrelation function.


```r
correlated_variables <- findCorrelation(cor(training[,1:52]), cutoff = 0.6, exact = TRUE)
training <- training[,-(correlated_variables)]
```

Now let's take a look at the remaining variable's correlation.


```r
ggcorr(training[,-26], hjust = .95, size = 3, layout.exp=5)
```

![](index_files/figure-html/correl table-1.png)<!-- -->

We can see in the above figure that the remaining variables are not highly correlated as we have removed any variable that is correlated with another over 60%. Now that we are happy with the remaining variables, let's run some models and then test them. 

## Model Data


```r
set.seed(444) 
tc <- trainControl(method = "cv", number = 10)
```


```r
set.seed(444)
fit_rpart <- train(classe~., method="rpart", data=training, trControl = tc)
fit_rf <- train(classe~., method="rf", data=training, trControl = tc)
```

Let's take a look at the accuracy of the models that we have trained.


```r
fit_rpart
```

```
## CART 
## 
## 13737 samples
##    25 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 12364, 12363, 12362, 12364, 12361, 12363, ... 
## Resampling results across tuning parameters:
## 
##   cp          Accuracy   Kappa     
##   0.02319194  0.4742611  0.31848386
##   0.04831655  0.4390316  0.26589468
##   0.06130268  0.3346419  0.08773813
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was cp = 0.02319194.
```

```r
fit_rf
```

```
## Random Forest 
## 
## 13737 samples
##    25 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 12363, 12364, 12362, 12362, 12364, 12363, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.9821646  0.9774338
##   13    0.9773592  0.9713570
##   25    0.9669487  0.9581898
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 2.
```

```r
fit_rf$finalModel
```

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2
## 
##         OOB estimate of  error rate: 1.65%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3887    8    2    7    2 0.004864311
## B   42 2605    8    0    3 0.019939804
## C    2   33 2346   14    1 0.020868114
## D    1    4   78 2164    5 0.039076377
## E    0    3    3   11 2508 0.006732673
```

We can see that by far the random forest method has the best accuracy at 98.2%, so we will use this model to run our predictions on our testing set that we have. We can also see that the estimated out of sample error rate is 1.65% which is acceptable. Let's now test the model on our unused testing portion of the data.


```r
rf_prediction <- predict(fit_rf, newdata = testing)
confusionMatrix(rf_prediction, testing$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1670   23    2    0    0
##          B    2 1112   14    1    0
##          C    1    2 1003   28    1
##          D    1    0    7  934    3
##          E    0    2    0    1 1078
## 
## Overall Statistics
##                                          
##                Accuracy : 0.985          
##                  95% CI : (0.9816, 0.988)
##     No Information Rate : 0.2845         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.9811         
##                                          
##  Mcnemar's Test P-Value : NA             
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9976   0.9763   0.9776   0.9689   0.9963
## Specificity            0.9941   0.9964   0.9934   0.9978   0.9994
## Pos Pred Value         0.9853   0.9849   0.9691   0.9884   0.9972
## Neg Pred Value         0.9990   0.9943   0.9953   0.9939   0.9992
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2838   0.1890   0.1704   0.1587   0.1832
## Detection Prevalence   0.2880   0.1918   0.1759   0.1606   0.1837
## Balanced Accuracy      0.9958   0.9864   0.9855   0.9833   0.9978
```

We can see that with the testing data we can also get 98.5% accuracy. Let's use our model then to complete our final prediction on the validation data set.

## Prediction


```r
rf_prediction_final <- predict(fit_rf, newdata = validation)
```

## Conclusion

Using the random forest method for predicition, we are expecting to get an out of sample error rate of 1.67%. This gives us an expected value of 0.3 incorrect predictions, so we should expect close to 20/20 of our predictions to be correct. Here are the final predictions


```r
rf_prediction_final
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
