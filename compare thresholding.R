subgroup <- function(dataset, rule_col, compare, threshold) {
  sbgrp <- switch(compare,
                  "<" = dataset[,rule_col] < threshold,
                  "<=" = dataset[,rule_col] <= threshold,
                  ">" = dataset[,rule_col] > threshold,
                  ">=" = dataset[,rule_col] >= threshold,)
  return(sbgrp)
}

avg_trt_effect <- function(y, treated, subgroup) {
  mean(y[treated & subgroup]) - mean(y[!treated & subgroup])
}


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

grp_diffs <- data.frame(matrix(0,max_datasets, 5))
best_grp <- numeric(max_datasets)

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
  
  # extract the decision tree's rule
  rule_col <- names(rpart.lists(fit)$L)[1]
  compare <- attributes(rpart.lists(fit)$L[[1]])$compare
  threshold <- rpart.lists(fit)$L[[1]][1]
  
  subgroups <- lapply(c(-10, -5, 0, 5, 10), 
                      function(x) subgroup(dataset, rule_col, compare, threshold + x))
  
  diffs <- sapply(1:5, function(x) avg_trt_effect(dataset$y, dataset$trt, subgroups[[x]]))
  grp_diffs[i,] <- diffs
  
  # returns 1 if the first element of diffs is the min (for threshold -10)
  # returns 6 if all five elements of diff are > -.6
  best_grp[i] <- which.min(c(diffs, -.59999))
  
  if (best_grp[i] < 6) {
    subgroups_fancy[,paste("dataset", i, sep="_")] <- subgroups[[best_grp[i]]] # how to index a list
    
    # if the decision tree's original threshold yields an acceptable subgroup
    # third element of diffs and subgroups
    if (diffs[3] < -.6) {
      subgroups_base[,paste("dataset", i, sep="_")] <- subgroups[[3]]
    }
        
  }
  
  
}

write.csv(subgroups_base, "Data/subgroups_base.csv", row.names=F)
write.csv(subgroups_fancy, "Data/subgroups_fancy.csv", row.names=F)
