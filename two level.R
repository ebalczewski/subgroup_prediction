subgroup <- function(dataset, rule_col, compare, threshold) {
  sbgrp <- switch(compare,
                  "<" = dataset[,rule_col] < threshold,
                  "<=" = dataset[,rule_col] <= threshold,
                  ">" = dataset[,rule_col] > threshold,
                  ">=" = dataset[,rule_col] >= threshold)
  return(sbgrp)
}

avg_trt_effect <- function(dataset, subgroup) {
  y <- dataset$y
  treated <- dataset$trt == 1
  mean(y[treated & subgroup]) - mean(y[!treated & subgroup])
}


# requires two packages to be installed, randomForest and FSelector

#install.packages("randomForest")
#install.packages("FSelector")
library("randomForest")
library("FSelector")
library(rpart)
#library(rattle)
library(rpart.utils)

# change to appropriate directory for your laptop
setwd("C:/Users/Anne/Downloads/subgroup_prediction")

data <- read.csv("Data/Data.csv")

max_datasets <- max(data$dataset)
# for now, make simple

subgroups_base <- data.frame(matrix("", 240, max_datasets+ 1))
names(subgroups_base) <- c("id", paste("dataset", 1:max_datasets, sep="_") )
subgroups_base$id <- 1:240

subgroups_all <- subgroups_base
subgroups_2level.5 <- subgroups_base
subgroups_2level1 <- subgroups_base


no_diff <- 0
not_first <- 0
not_point5 <- 0
not_one <- 0

not_point5yes2level <- 0
not_oneyes2level <- 0

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
  subgroups_all[,paste("dataset", i, sep="_")] <- ifelse(fit$where == 2, 1, 0)
  subgroups_2level.5[,paste("dataset", i, sep="_")] <- ifelse(fit$where == 2, 1, 0)
  subgroups_2level1[,paste("dataset", i, sep="_")] <- ifelse(fit$where == 2, 1, 0)
  diff <- mean(dataset$y[fit$where == 2 & dataset$trt == 1]) - mean(dataset$y[fit$where == 2 & dataset$trt == 0])
  
  # subgroup doesn't look meaningful
  if (diff > -.6) {
    subgroups_base[,paste("dataset", i, sep="_")] <- 0
    no_diff <- no_diff + 1
  }
  
  rule_col <- names(rpart.lists(fit)$L)[1]
  
  two_level <- FALSE
  point5 <- FALSE
  one <- FALSE
  
  # not in top important rules!
  if (! (rule_col %in% names(fit$variable.importance))) {
    two_level <- TRUE
    
    not_point5 <- not_point5 + 1
    not_one <- not_one + 1
    
    point5 <- TRUE
    one <- TRUE
  }
  else {
    # importance isn't over some threshold
    if (fit$variable.importance[rule_col] < .5) {
      two_level <- TRUE
      not_point5 <- not_point5 + 1
      
      point5 <- TRUE
    }
    
    # importance isn't over another threshold
    if (fit$variable.importance[rule_col] < 1) {
      two_level <- TRUE
      not_one <- not_one + 1
      
      one <- TRUE
    }
  }
  
  if (two_level) {
    # try two level tree
    fit2 <- rpart(z ~., data = rpart_test, maxdepth = 2)
    rule_col1 <- names(rpart.lists(fit)$L)[1]
    rule_col2 <- names(rpart.lists(fit)$L)[2]
    
    if ((rule_col1 %in% names(fit2$variable.importance)) & (rule_col2 %in% names(fit2$variable.importance))) {
      if (point5) {
        if (fit2$variable.importance[rule_col1] >= .5 & fit2$variable.importance[rule_col2] >= .5) {
          subgroups_2level.5[,paste("dataset", i, sep="_")] <- ifelse((fit2$where == 4) | (fit2$where == 2), 1, 0)
          not_point5yes2level <- not_point5yes2level + 1
        }
        else {
          subgroups_2level.5[,paste("dataset", i, sep="_")] <- 0 # no subgroup
        }
      }
      
      if (one) {
        if (fit2$variable.importance[rule_col1] >= 1 & fit2$variable.importance[rule_col2] >= 1) {
          subgroups_2level1[,paste("dataset", i, sep="_")] <- ifelse((fit2$where == 4) | (fit2$where == 2), 1, 0)
          not_oneyes2level <- not_oneyes2level + 1
        }
        else {
          subgroups_2level1[,paste("dataset", i, sep="_")] <- 0 # no subgroup
        }
      }
    }
  }
  
  
  
}

write.csv(subgroups_all, "Data/subgroups_all.csv", row.names=F)
write.csv(subgroups_base, "Data/subgroups_base.csv", row.names=F)
write.csv(subgroups_2level1, "Data/2_level_importance1.csv", row.names=F)
write.csv(subgroups_2level.5, "Data/2_level_importance.5.csv", row.names=F)
