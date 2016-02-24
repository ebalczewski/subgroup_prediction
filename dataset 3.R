# requires two packages to be installed, randomForest and FSelector

#install.packages("randomForest")
#install.packages("FSelector")
library("randomForest")
library("FSelector")

# change to appropriate directory for your laptop
setwd("C:/Users/Anne/Downloads/subgroup_prediction")

data <- read.csv("Data/Training_Data.csv")

data3 <- data[data$dataset == 3,]
#data3[,5:24] <- ifelse(data3[,5:24] > 0, 1, 0) # if we want to collapse 2's and 1's

x <- data3[,c(3,5:ncol(data3))] # covariates plus trt column
y <- data3$y # response column

# create datasets for virtual twin predictions
data3_all_trt <- data3
data3_all_ctrl <- data3
data3_all_trt$trt <- 1
data3_all_ctrl$trt <- 0

# random forest regression
blah <- randomForest(x,y)
trt.pred <- predict(blah, data3_all_trt)
ctrl.pred <- predict(blah, data3_all_ctrl)

# virtual twin treatment effect estimates
z <- trt.pred - ctrl.pred

# subgroup identified by Innocentive
subgrp <- data3$x5 != 0
boxplot(z[subgrp], z[!subgrp])

# Now do feature selection for only the data with bottom or top 30% of z estimates
alpha <- .05

test <- data3[,5:ncol(data3)] # will use for feature selection
test$target <- ifelse(z < quantile(z, alpha), 1, 0) # bottom or top

test <- test[z < quantile(z, alpha) | z > quantile(z, 1-alpha),] # subset

weights <- chi.squared(target ~ ., test)
# weights # calculates usefulness for each column of data

which(weights > 0) # prints 33

# why does it think column 33 is the useful one? :(
