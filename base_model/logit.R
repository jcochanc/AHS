# Load Libraries.
pacman::p_load(nnet, knitr, kableExtra)

# Load data.
data <- readRDS("./Data/train.RDS") # read training data
test <- readRDS("./Data/test.RDS") # read testing data

# Same procedures as lasso.R

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

data <- data %>%
  filter(complete.cases(data$index))# remove 2460 rows with NA's

# Same for test data.
# remove 'control', an ID variable
test <- test %>%
  select(-control)

# Fix housing_qual_index_alternative.
test$index <- as.vector(test$housing_qual_index_alternative)

test <- test %>%
  select(-housing_qual_index_alternative)

# make 'sex' a 0/1 binary variable - 2 is female.
test <- test %>%
  mutate(female = ifelse(sex == 2, 1, 0)) %>%
  select(-sex)

# normalize 'data_year' to start at 1997
test$data_year <- test$data_year-1997 

# standardize `weight`
test$weight <- (test$weight - mean(test$weight)) / sd(test$weight) 

test <- test %>%
  filter(complete.cases(test$index))


# demo. var. by LASSO using `educ_cat` as outcome in update.
vars_educ_cat <- readRDS("./Data/nonzero_coef2.RDS") 

# Perform multinomial logistic regression on vars_educ_cat 
multinom_vars <- rownames(vars_educ_cat)[-1] # remove intercept

# Select vars needed for analysis.
df_educ_cat <- data %>%
  select(educ_cat, index, multinom_vars)

# Run multinom model.
multinom_mod <- multinom(educ_cat ~., df_educ_cat)

# calculate p-values
multinom_coef <- summary(multinom_mod)$coefficients
z <- multinom_coef/summary(multinom_mod)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2 # p-value table for ordinal logistic regression

summary_multinom <- data.frame(t(multinom_coef), t(p)) # summary table for ordinal logistic regression
colnames(summary_multinom) <- c("0-1 coef.", "0-2 coef.", "0-3 coef.", 
                                "0-1 p-val.", "0-2 p-val.", "0-3 p-val.")

saveRDS(summary_multinom, "./Data/summary_multinom2.RDS")

# predict
probs <- data.frame(predict(multinom_mod, type="probs", newdata=test))
prediction <- c()
for (i in 1:nrow(test)) {
  pred = which(probs[i,] == max(probs[i,])) - 1
  prediction = c(prediction, pred)
}

# calculate deviance
comparison <- cbind(test$educ_cat, prediction)
acc_index <- mean(test$educ_cat == prediction) # Accuracy = 37.48%

######################### TRY WIHTOUT INDEX #########################

# Perform multinomial logistic regression on vars_educ_cat 
# Select vars needed for analysis.
df_educ_cat_no_index <- data %>%
  select(educ_cat, multinom_vars)

# Run the model.
multinom_mod <- multinom(educ_cat ~., df_educ_cat_no_index)

# calculate p-values
multinom_coef <- summary(multinom_mod)$coefficients
z <- multinom_coef/summary(multinom_mod)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2 # p-value table for ordinal logistic regression

summary_multinom <- data.frame(t(multinom_coef), t(p)) # summary table for ordinal logistic regression
colnames(summary_multinom) <- c("0-1 coef.", "0-2 coef.", "0-3 coef.", 
                                "0-1 p-val.", "0-2 p-val.", "0-3 p-val.")

# predict
probs <- data.frame(predict(multinom_mod, type="probs", newdata=test))
prediction <- c()
for (i in 1:nrow(test)) {
  pred = which(probs[i,] == max(probs[i,])) - 1
  prediction = c(prediction, pred)
}

# calculate deviance
comparison <- cbind(test$educ_cat, prediction)
acc_index <- mean(test$educ_cat == prediction) # Accuracy = 37.37%
