# requires two packages to be installed, randomForest and FSelector

#install.packages("randomForest")
#install.packages("FSelector")
library("randomForest")
library("FSelector")
library(rpart)
library(rattle)
library(rpart.utils)

# change to appropriate directory for your laptop
setwd("C:/Users/Anne/Downloads/subgroup_prediction")

data <- read.csv("Data/Data.csv")

max_datasets <- max(data$dataset)
# for now, make simple

subgroups_base <- data.frame(matrix("", 240, max_datasets+ 1))
names(subgroups_base) <- c("id", paste("dataset", 1:1200, sep="_") )
subgroups_base$id <- 1:240

subgroups_fancy <- subgroups_base

two_rules <- 0

for (i in 1:max_datasets) {
  dataset <- data[data$dataset == i,]
  #dataset[,5:24] <- ifelse(dataset[,5:24] > 0, 1, 0) # if we want to collapse 2's and 1's
  
  x <- dataset[,c(3,5:ncol(dataset))] # covariates plus trt column
  y <- dataset$y # response column
  
  # create datasets for virtual twin predictions
  dataset_all_trt <- dataset
  dataset_all_ctrl <- dataset
  dataset_all_trt$trt <- 1
  dataset_all_ctrl$trt <- 0
  
  # random forest regression
  blah <- randomForest(x,y)
  trt.pred <- predict(blah, dataset_all_trt)
  ctrl.pred <- predict(blah, dataset_all_ctrl)
  
  # virtual twin treatment effect estimates
  z <- trt.pred - ctrl.pred
  
  # try one rule based on all data
  rpart_test <- dataset[,5:ncol(dataset)]
  rpart_test$z <- z
  fit <- rpart(z ~., data = rpart_test, maxdepth = 1)
  
  subgroups_base[,paste("dataset", i, sep="_")] <- ifelse(fit$where == 2, 1, 0)
  subgroups_fancy[,paste("dataset", i, sep="_")] <- ifelse(fit$where == 2, 1, 0)
  diff <- mean(dataset$y[fit$where == 2 & dataset$trt == 1]) - mean(dataset$y[fit$where == 2 & dataset$trt == 0])
  
  if (diff > -.6) {
    # try with two rules for subgroups_fancy
    two_rules <- two_rules + 1
    fit <- rpart(z ~., data = rpart_test, maxdepth = 2)
    
    sbgrp <- fit$where == 4
    subgroups_base[,paste("dataset", i, sep="_")] <- 0
    subgroups_fancy[,paste("dataset", i, sep="_")] <- ifelse(sbgrp, 1, 0)
    diff <- mean(dataset$y[sbgrp & dataset$trt == 1]) - mean(dataset$y[(!sbgrp) & dataset$trt == 0])
    
    if (diff > -.6) {
        # no easy rule to find, I guess!
        two_rules <- two_rules - 1
        subgroups_base[,paste("dataset", i, sep="_")] <- 0
        subgroups_fancy[,paste("dataset", i, sep="_")] <- 0
    }
  }
  
}

write.csv(subgroups_base, "Data/subgroups_base.csv", row.names=F)
write.csv(subgroups_fancy, "Data/subgroups_fancy.csv", row.names=F)