
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(readxl)
library(caret)
library(matrixStats)
library(data.table)
library(rmarkdown)
library(Rborist)

# downloadin the CSV from the github repo and creating the dataset
url <- "https://raw.githubusercontent.com/M-Sedighi/Harvardx-DataScience-Capstone-2/main/Video_Games_Sales_as_at_22_Dec_2016.csv"
Video_Games_Sales_as_at_22_Dec_2016 <- read_csv(url)
sales <- Video_Games_Sales_as_at_22_Dec_2016


# preprocessing the age ratings in accordance to ESRB 
ESRB <- sales$Rating
# NA to RP
# E is same as K-A
ESRB[ESRB == "K-A"] <- "E"
ESRB[is.na(ESRB)] <- "RP"
lvls = c("RP", "EC", "E", "E10+", "T", "M", "AO")
lbls = c("Rating Pending", "Early Childhood", "Everyone", "Everyone 10+", "Teen", "Mature", "Adults Only")

ESRB <- factor(ESRB, levels = lvls, labels = lbls,
               ordered = TRUE)


# handling the NA data for genre
sales$Genre[is.na(sales$Genre)] <- "Misc"


# factorizing the predictors
sales <- sales %>% mutate(Publisher = factor(Publisher), Platform = factor(Platform), Genre = factor(Genre), Rating = ESRB)


# calculating the median europe sale ration and creating the Europe release as the output vector
sales <- sales %>% mutate(EU_factor = EU_Sales/(NA_Sales+JP_Sales+Other_Sales))
sum(!is.finite(sales$EU_factor))
EU_scf <- median(sales$EU_factor[is.finite(sales$EU_factor)])
sales <- sales %>% mutate(EU_release = EU_factor > EU_scf)


# handling the NAs in output vector
which(is.na(sales$EU_release))
which(!is.finite(sales$EU_factor))
sales$EU_release[is.na(sales$EU_release)] <- FALSE
table(sales$EU_release)
sales <- sales %>% mutate(EU_release= factor(EU_release))

# splitting the data into training set and test set
set.seed(2, sample.kind="Rounding") 
test_index <- createDataPartition(y = sales$EU_release, times = 1, p = 0.1, list = FALSE)
train_set <- sales[-test_index,]
test_set <- sales[test_index,]

# making sure all NAs are accounted for
which(is.na(train_set$EU_release))
which(is.na(train_set$Publisher))
which(is.na(train_set$Genre))
which(is.na(train_set$Platform))
which(is.na(train_set$Rating))
which(is.na(train_set$NA_Sales))
which(is.na(train_set$JP_Sales))


# first stage decision tree training and evaluation
train_rpartc <- train(EU_release ~ Publisher+Genre+Platform+Year_of_Release+Rating+NA_Sales+JP_Sales,
                      tuneGrid = data.frame(cp = seq(0, 0.1, len = 11)), method = "rpart", data = train_set)

ggplot(train_rpartc)
plot(train_rpartc$finalModel, margin = 0.1)
text(train_rpartc$finalModel, cex = 0.75)


y_hatc <- predict(train_rpartc, test_set)

yc <- data.frame(Y=test_set$EU_release,Y_hat=y_hatc)

confusionMatrix(yc$Y,yc$Y_hat)$overall["Accuracy"]

confusionMatrix(yc$Y,yc$Y_hat)


# second stage decision tree training and evluation
train_rpart <- train(EU_release ~ Publisher+Genre+Platform+Year_of_Release+Rating+NA_Sales+JP_Sales,
                     tuneGrid = data.frame(cp = seq(0, 0.001, len = 11)), method = "rpart", data = train_set)
ggplot(train_rpart)
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)


y_hat <- predict(train_rpart, test_set)

y <- data.frame(Y=test_set$EU_release,Y_hat=y_hat)

confusionMatrix(y$Y,y$Y_hat)$overall["Accuracy"]

confusionMatrix(y$Y,y$Y_hat)


# random forest (Rborist) trainin and evaluation 
train_rf <- train(EU_release ~ Publisher+Genre+Platform+Year_of_Release+Rating+NA_Sales+JP_Sales,
                  method = 'Rborist',
                  data = train_set)

y_hat_2 <- predict(train_rf, test_set)

y_2 <- data.frame(Y=test_set$EU_release,Y_hat=y_hat_2)

confusionMatrix(y_2$Y,y_2$Y_hat)$overall["Accuracy"]

confusionMatrix(y_2$Y,y_2$Y_hat)
