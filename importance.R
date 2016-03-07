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

subgroups_fancier <- subgroups_base
subgroups_all <- subgroups_base
subgroups_fancy <- subgroups_base
subgroups_fancy2 <- subgroups_base
subgroups_fancier2 <- subgroups_base
subgroups_fancy3 <- subgroups_base
subgroups_fancier3 <- subgroups_base

no_diff <- 0
not_first <- 0
not_point5 <- 0
not_one <- 0

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
  subgroups_all[,paste("dataset", i, sep="_")] <- ifelse(fit$where == 2, 1, 0)
  subgroups_fancy2[,paste("dataset", i, sep="_")] <- ifelse(fit$where == 2, 1, 0)
  diff <- mean(dataset$y[fit$where == 2 & dataset$trt == 1]) - mean(dataset$y[fit$where == 2 & dataset$trt == 0])
  
  # subgroup doesn't look meaningful
  if (diff > -.6) {
    subgroups_base[,paste("dataset", i, sep="_")] <- 0
    subgroups_fancier[,paste("dataset", i, sep="_")] <- 0
    subgroups_fancier2[,paste("dataset", i, sep="_")] <- 0
    subgroups_fancier3[,paste("dataset", i, sep="_")] <- 0
    
    no_diff <- no_diff + 1
  }
  
  rule_col <- names(rpart.lists(fit)$L)[1]
  
  # not in top important rules!
  if (! (rule_col %in% names(fit$variable.importance))) {
    subgroups_fancy[,paste("dataset", i, sep="_")] <- 0
    subgroups_fancier[,paste("dataset", i, sep="_")] <- 0
    subgroups_fancy2[,paste("dataset", i, sep="_")] <- 0
    subgroups_fancier2[,paste("dataset", i, sep="_")] <- 0
    subgroups_fancy3[,paste("dataset", i, sep="_")] <- 0
    subgroups_fancier3[,paste("dataset", i, sep="_")] <- 0
    
    not_first <-  not_first + 1
    not_point5 <- not_point5 + 1
    not_one <- not_one + 1
  }
  else {
    # not the most important rule!
    if (rule_col != names(fit$variable.importance)[1]) {
      subgroups_fancy[,paste("dataset", i, sep="_")] <- 0
      subgroups_fancier[,paste("dataset", i, sep="_")] <- 0
      not_first <-  not_first + 1
    }
    
    # importance isn't over some threshold
    if (fit$variable.importance[rule_col] < .5) {
      subgroups_fancy2[,paste("dataset", i, sep="_")] <- 0
      subgroups_fancier2[,paste("dataset", i, sep="_")] <- 0
      not_point5 <- not_point5 + 1
    }
    
    # importance isn't over another threshold
    if (fit$variable.importance[rule_col] < 1) {
      subgroups_fancy3[,paste("dataset", i, sep="_")] <- 0
      subgroups_fancier3[,paste("dataset", i, sep="_")] <- 0
      not_one <- not_one + 1
    }
  }
  
}

write.csv(subgroups_all, "Data/subgroups_all.csv", row.names=F)
write.csv(subgroups_base, "Data/subgroups_base.csv", row.names=F)
write.csv(subgroups_fancy, "Data/subgroups_fancy.csv", row.names=F)
write.csv(subgroups_fancier, "Data/subgroups_fancier.csv", row.names=F)
write.csv(subgroups_fancy2, "Data/subgroups_fancy2.csv", row.names=F)
write.csv(subgroups_fancier2, "Data/subgroups_fancier2.csv", row.names=F)
write.csv(subgroups_fancy3, "Data/subgroups_fancy3.csv", row.names=F)
write.csv(subgroups_fancier3, "Data/subgroups_fancier3.csv", row.names=F)