library(nnet)
library(kableExtra)
library(knitr)

data <- readRDS("./Data/train.RDS") # read training data
test <- readRDS("./Data/test.RDS") # read testing data
data <- data[-which(is.na(data$housing_qual_index_alternative)),] # remove 2460 rows with NA's
test <- test[-which(is.na(test$housing_qual_index_alternative)),] # remove 2460 rows with NA's
vars_educ_cat <- readRDS("./Data/nonzero_coef.RDS") # demo. var. by LASSO using `educ_cat` as outcome

# Perform multinomial logistic regression on vars_educ_cat 
multinom_vars <- rownames(vars_educ_cat)[-1]
df_educ_cat <- data[,c("educ_cat", multinom_vars, "housing_qual_index_alternative")]
multinom_mod <- multinom(educ_cat ~., df_educ_cat)

# calculate p-values
multinom_coef <- summary(multinom_mod)$coefficients
z <- multinom_coef/summary(multinom_mod)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2 # p-value table for ordinal logistic regression

summary_multinom <- data.frame(t(multinom_coef), t(p)) # summary table for ordinal logistic regression
colnames(summary_multinom) <- c("0-1 coef.", "1-2 coef.", "2-3 coef.", 
                                "0-1 p-val.", "1-2 p-val.", "2-3 p-val.")

saveRDS(summary_multinom, "./Data/summary_multinom.RDS")

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
multinom_vars <- rownames(vars_educ_cat)[-1]
df_educ_cat <- data[,c("educ_cat", multinom_vars)]
multinom_mod <- multinom(educ_cat ~., df_educ_cat)

# calculate p-values
multinom_coef <- summary(multinom_mod)$coefficients
z <- multinom_coef/summary(multinom_mod)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2 # p-value table for ordinal logistic regression

summary_multinom <- data.frame(t(multinom_coef), t(p)) # summary table for ordinal logistic regression
colnames(summary_multinom) <- c("0-1 coef.", "1-2 coef.", "2-3 coef.", 
                                "0-1 p-val.", "1-2 p-val.", "2-3 p-val.")

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
