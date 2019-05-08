library(glmnet)
library(dplyr)
library(tidyr)
library(ggplot2)

data <- readRDS("/Users/lindechen/AHS/Data/train.RDS") 

data <- data[,-1] # remove 'control', an ID variable
data$sex <- data$sex-1 # make 'sex' a 0/1 binary variable
data$data_year <- data$data_year-1997 # normalize 'data_year' to start at 1997
data$weight <- (data$weight - mean(data$weight)) / sd(data$weight) # standardize `weight`
data <- data[-which(is.na(data$housing_qual_index_alternative)),] # remove 2460 rows with NA's

# perform LASSO regression on demographic variables 
# LASSO inherently performs variable selection by shrinking some coefficients to zero

# use educ_cat as outcome variable (factor of 4 levels)
features <- data %>% select(-grad, -educ_cat, -housing_qual_index, -housing_qual_index_alternative) # remove class, grad (redundant), indices 
x <- as.matrix(features)
y <- as.matrix(as.numeric(data[,"educ_cat"])) # only class

# Fit the model
set.seed(67)
cv.lasso <- cv.glmnet(x, y, alpha=1, family = "multinomial", type.measure = "deviance")

# Results
pdf("./base_model/CV_plot.pdf")
plot(cv.lasso)
dev.off()

pdf("./base_model/response_plot.pdf")
plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)
dev.off()

# all coef
coef <- do.call(cbind, coef(cv.lasso, s = cv.lasso$lambda.1se)) # add 1 SE to obtain more parsimonious model

index <- c() # create empty vector to store var. with coefficients in any of 4 categories shrunk to zero
for (i in 1:nrow(coef)) {
  if (sum(coef[i,] == 0) > 0) {
    index = c(index, i)
  }
}

# non-zero demo. coef
nonzero_coef_educ_cat <- coef[-index,] 

saveRDS(nonzero_coef_educ_cat, "./Data/nonzero_coef.RDS")
