
library(tidyverse)    # includes readr
library(readxl)
library(readr)
library(caret)
library(matrixStats)

Video_Games_Sales_as_at_22_Dec_2016 <- read_csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
sales <- Video_Games_Sales_as_at_22_Dec_2016

train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
ggplot(train_rpart)


ESRB <- sales$Rating
# NA to RP
# E is same as K-A
ESRB[ESRB == "K-A"] <- "E"
ESRB[is.na(ESRB)] <- "RP"
lvls = c("RP", "EC", "E", "E10+", "T", "M", "AO")
lbls = c("Rating Pending", "Early Childhood", "Everyone", "Everyone 10+", "Teen", "Mature", "Adults Only")

ESRB <- factor(ESRB, levels = lvls, labels = lbls,
       ordered = TRUE)

sales$Genre[is.na(sales$Genre)] <- "Misc"

sales <- sales %>% mutate(Rating = ESRB, Platform = factor(Platform),Genre = factor(sales$Genre),
                          Publisher = factor(Publisher), 
                          score = (Critic_Count*Critic_Score+User_Count*User_Score)/(Critic_Count+User_Count))

sum(is.na(sales$Publisher))
factor(Publisher)

ls(sales)

x <- rowWeightedMeans(sales[,c(11,13)], w = sales[,c(12,14)], na.rm = TRUE)

sales$Genre[is.na(sales$Genre)]
