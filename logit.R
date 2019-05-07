library(nnet)

data <- readRDS("./Data/df_index_demographic_vars.RDS") # provided by Carolina in pca for index
vars_grad <- readRDS("./Data/nonzero_coef_grad.RDS") # demo. var. by LASSO using `grad` as outcome
vars_educ_cat <- readRDS("./Data/nonzero_coef_educ_cat.RDS") # demo. var. by LASSO using `educ_cat` as outcome
df <- data[,-c(24,25,26,27,28,29)] # remove 6 components to create df for regression // use index only

# Perform linear regression on vars_grad (continuous outcome)
lm_vars <- rownames(vars_grad)[-1]
df_grad <- df[,c("grad", lm_vars, "housing_qual_index")]
linear_mod <- lm(grad ~., df_grad)
summary_lm <- summary(linear_mod)$coefficients # summary table for linear regression

saveRDS(summary_lm, "./Data/summary_lm.RDS")

# Perform multinomial logistic regression on vars_educ_cat (categorical outcome)
multinom_vars <- rownames(vars_educ_cat)[-1]
df_educ_cat <- df[,c("educ_cat", multinom_vars, "housing_qual_index")]
multinom_mod <- multinom(educ_cat ~., df_educ_cat)

# calculate p-values
multinom_coef <- summary(multinom_mod)$coefficients
z <- multinom_coef/summary(multinom_mod)$standard.errors
p <- round((1 - pnorm(abs(z), 0, 1))*2,4) # p-value table for ordinal logistic regression

summary_multinom <- data.frame(t(multinom_coef), t(p)) # summary table for ordinal logistic regression
colnames(summary_multinom) <- c("level 1 coef.", "level 2 coef.", "level 3 coef.", 
                                "level 1 p-val.", "level 2 p-val.", "level 3 p-val.")

saveRDS(summary_multinom, "./Data/summary_multinom.RDS")
