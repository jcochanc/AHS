library(nnet)

data <- readRDS("./Data/train.RDS") # read training data
data <- data[-which(is.na(data$housing_qual_index_alternative)),] # remove 2460 rows with NA's
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
