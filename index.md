---
title: "Excercise Quality Study"
author: "Ivan Jennings"
date: "03/04/2021"
output:
  html_document:
    keep_md: yes

---



## Executive Summary

In this project, my goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants using devices such as Jawbone Up, Nike FuelBand, and Fitbit to quantify how well the subjects are exercising. I will be using data from here http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har

## Loading Data

First I will load necessary R libraries that I will need to complete my analysis.


```r
library(dplyr)
library(caret)
library(lubridate)
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
## convert dates to date format
training_test$cvtd_timestamp <- dmy_hm(training_test$cvtd_timestamp)
validation$cvtd_timestamp <- dmy_hm(validation$cvtd_timestamp)
## convert char col to factor
training_test$new_window <- factor(training_test$new_window)
validation$new_window <- factor(validation$new_window)
training_test$new_window <- factor(training_test$new_window)
validation$new_window <- factor(validation$new_window)
training_test$classe <- factor(training_test$classe)
training_test$user_name <- factor(training_test$user_name)
validation$user_name <- factor(validation$user_name)
## remove raw time stamp (drop columns)
training_test <- training_test %>%
  select(-c(2,3))
validation <- validation %>%
  select(-c(2,3))
```

Let's check for NAs now that we have cleaned up our data


```r
colSums(is.na(training_test))
```

```
##            user_name       cvtd_timestamp           new_window 
##                    0                    0                    0 
##           num_window            roll_belt           pitch_belt 
##                    0                    0                    0 
##             yaw_belt     total_accel_belt         gyros_belt_x 
##                    0                    0                    0 
##         gyros_belt_y         gyros_belt_z         accel_belt_x 
##                    0                    0                    0 
##         accel_belt_y         accel_belt_z        magnet_belt_x 
##                    0                    0                    0 
##        magnet_belt_y        magnet_belt_z             roll_arm 
##                    0                    0                    0 
##            pitch_arm              yaw_arm      total_accel_arm 
##                    0                    0                    0 
##          gyros_arm_x          gyros_arm_y          gyros_arm_z 
##                    0                    0                    0 
##          accel_arm_x          accel_arm_y          accel_arm_z 
##                    0                    0                    0 
##         magnet_arm_x         magnet_arm_y         magnet_arm_z 
##                    0                    0                    0 
##        roll_dumbbell       pitch_dumbbell         yaw_dumbbell 
##                    0                    0                    0 
## total_accel_dumbbell     gyros_dumbbell_x     gyros_dumbbell_y 
##                    0                    0                    0 
##     gyros_dumbbell_z     accel_dumbbell_x     accel_dumbbell_y 
##                    0                    0                    0 
##     accel_dumbbell_z    magnet_dumbbell_x    magnet_dumbbell_y 
##                    0                    0                    0 
##    magnet_dumbbell_z         roll_forearm        pitch_forearm 
##                    0                    0                    0 
##          yaw_forearm  total_accel_forearm      gyros_forearm_x 
##                    0                    0                    0 
##      gyros_forearm_y      gyros_forearm_z      accel_forearm_x 
##                    0                    0                    0 
##      accel_forearm_y      accel_forearm_z     magnet_forearm_x 
##                    0                    0                    0 
##     magnet_forearm_y     magnet_forearm_z               classe 
##                    0                    0                    0
```

```r
colSums(is.na(validation))
```

```
##            user_name       cvtd_timestamp           new_window 
##                    0                    0                    0 
##           num_window            roll_belt           pitch_belt 
##                    0                    0                    0 
##             yaw_belt     total_accel_belt         gyros_belt_x 
##                    0                    0                    0 
##         gyros_belt_y         gyros_belt_z         accel_belt_x 
##                    0                    0                    0 
##         accel_belt_y         accel_belt_z        magnet_belt_x 
##                    0                    0                    0 
##        magnet_belt_y        magnet_belt_z             roll_arm 
##                    0                    0                    0 
##            pitch_arm              yaw_arm      total_accel_arm 
##                    0                    0                    0 
##          gyros_arm_x          gyros_arm_y          gyros_arm_z 
##                    0                    0                    0 
##          accel_arm_x          accel_arm_y          accel_arm_z 
##                    0                    0                    0 
##         magnet_arm_x         magnet_arm_y         magnet_arm_z 
##                    0                    0                    0 
##        roll_dumbbell       pitch_dumbbell         yaw_dumbbell 
##                    0                    0                    0 
## total_accel_dumbbell     gyros_dumbbell_x     gyros_dumbbell_y 
##                    0                    0                    0 
##     gyros_dumbbell_z     accel_dumbbell_x     accel_dumbbell_y 
##                    0                    0                    0 
##     accel_dumbbell_z    magnet_dumbbell_x    magnet_dumbbell_y 
##                    0                    0                    0 
##    magnet_dumbbell_z         roll_forearm        pitch_forearm 
##                    0                    0                    0 
##          yaw_forearm  total_accel_forearm      gyros_forearm_x 
##                    0                    0                    0 
##      gyros_forearm_y      gyros_forearm_z      accel_forearm_x 
##                    0                    0                    0 
##      accel_forearm_y      accel_forearm_z     magnet_forearm_x 
##                    0                    0                    0 
##     magnet_forearm_y     magnet_forearm_z           problem_id 
##                    0                    0                    0
```

We can see that there are 0 columns with NA values since we removed the columns that we do not have data for in the final validation set.

## Explore Data

Now that we have a clean set of data, let's split up our data as follows:

training_test (originally training data) will be split 70/30 into training and test data.
validation (originally test data) will be left seperate and we will use part of the original training set as test data instead so that we will only need to use the validation set once at the end.
