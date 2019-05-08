library(glmnet) # load 'glmnet' package
library(dplyr)

data <- readRDS("/Users/lindechen/AHS/Data/df_two_indeces_demographic_vars.RDS") # read index and demographics data provided by Carolina

data <- data[,-1] # remove 'control', an ID variable
data$sex <- data$sex-1 # make 'sex' a 0/1 binary variable
data$data_year <- data$data_year-1997 # normalize 'data_year' to start at 1997
data$weight <- (data$weight - mean(data$weight)) / sd(data$weight) # standardize `weight`

######################### perform LASSO regression on demographic variables #####################
# Note: LASSO inherently performs variable selection by shrinking some coefficients to zero

######## 1. try with 'grad' (see codebook) - CONTINOUS outcome variable
x <- as.matrix(data[,-c(5,6,24,25,26,27,28,29,30)]) # Removes class, educ_cat (redundant), dimension vars, index (both non-demographic)
y <- as.double(as.matrix(data[, 5])) # Only class

# Fitting the model (Lasso: alpha = 1)
set.seed(14)
cv.lasso <- cv.glmnet(x, y, alpha=1, parallel=TRUE, standardize=TRUE, type.measure='mse')

# Results
plot(cv.lasso)
plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)

# all coef
coef <- coef(cv.lasso, s=cv.lasso$lambda.1se)[,1] # add 1 SE to obtain more parsimonious model
# non-zero demo. coef
nonzero_coef_grad <- data.frame(coef = coef[which(coef(cv.lasso, s=cv.lasso$lambda.1se) != 0)])  

saveRDS(nonzero_coef_grad, "./Data/nonzero_coef_grad.RDS")

######## 2. try with `educ_cat` - MULTINOMIAL outcome variable
x <- as.matrix(data[,-c(5,6,24,25,26,27,28,29,30)]) # Removes class, grad (redundant), dimension vars, index (both non-demographic)
y <- as.matrix(as.numeric(data[, 6])) 

# Fitting the model
cv.lasso <- cv.glmnet(x, y, family = "multinomial", alpha=1, type.measure = "deviance")

# Results
plot(cv.lasso)
plot(cv.lasso$glmnet.fit, xvar="lambda", label=TRUE)

# all coefficients
coef <- do.call(cbind, coef(cv.lasso, s = cv.lasso$lambda.1se)) 

index <- c() # create empty vector to store var. with coefficients in any of 4 categories shrunk to zero
for (i in 1:nrow(coef)) {
  if (sum(coef[i,] == 0) > 0) {
    index = c(index, i)
  }
}

# non-zero demo. coef
nonzero_coef_educ_cat <- coef[-index,] 
saveRDS(nonzero_coef_educ_cat, "./Data/nonzero_coef_educ_cat.RDS")
