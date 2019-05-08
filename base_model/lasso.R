# Load libraries.
pacman::p_load(glmnet, dplyr, tidyr, ggplot2)

# Load training data.
data <- readRDS("./Data/train.RDS") 

# remove 'control', an ID variable
data <- data %>%
  select(-control)

# Fix housing_qual_index_alternative.
data$index <- as.vector(data$housing_qual_index_alternative)

data <- data %>%
  select(-housing_qual_index_alternative)

# make 'sex' a 0/1 binary variable - 2 is female.
data <- data %>%
  mutate(female = ifelse(sex == 2, 1, 0)) %>%
  select(-sex)

# normalize 'data_year' to start at 1997
data$data_year <- data$data_year-1997 

# standardize `weight`
data$weight <- (data$weight - mean(data$weight)) / sd(data$weight) 

nrow(data)

data <- data %>%
  filter(complete.cases(data$index))# remove 2460 rows with NA's

63420-nrow(data)

# perform LASSO regression on demographic variables 
# LASSO inherently performs variable selection by shrinking some coefficients to zero

# use educ_cat as outcome variable (factor of 4 levels)
features <- data %>% select(-grad, -educ_cat, -housing_qual_index, -index) # remove class, grad (redundant), indices 
x <- as.matrix(features)
y <- as.matrix(as.numeric(data[,"educ_cat"])) # only class

# Fit the model
set.seed(67)
cv.lasso <- cv.glmnet(x, y, alpha=1, family = "multinomial", type.measure = "deviance")

# Results
pdf("./base_model/CV_plot2.pdf")
plot(cv.lasso)
dev.off()

pdf("./base_model/response_plot2.pdf")
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

saveRDS(nonzero_coef_educ_cat, "./Data/nonzero_coef2.RDS")

# Testing if there is a change. Plots suggest there is.
nonzero_coef_educ_cat1 <- readRDS("./Data/nonzero_coef.RDS")

# Suggesting there was a difference in the data after
# master was merged.
all.equal(nonzero_coef_educ_cat, nonzero_coef_educ_cat1)
