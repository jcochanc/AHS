# To run this R script on the HPC, we can run directly on
# the terminal or use a bash script. We will run this using
# bash. The associated R script for the models
# is xgb1.R

# The associated bash script for the xray interaction
# model is xgb_1.sh

# To run from the terminal, we can just call on
# sbatch ~/ML/xgb_1.sh

# Load libraries.
pacman::p_load(caret, dplyr, ggplot2, glue,
               ModelMetrics, OpenMPController,
               readr, vtreat,xgboost, MLmetrics)

set.seed(57)
omp_set_num_threads(28) # caret parallel processing threads

# Now we use the directory path for the data.
train <- readRDS("~/ML/train.RDS")
test <- readRDS("~/ML/test.RDS")

# Load data.
# train <- readRDS("./Data/train.RDS") # read training data
# test <- readRDS("./Data/test.RDS") # read testing data

# remove 'control', an ID variable
train <- train %>%
  select(-control, -race, -male_hh_married, -mar,
         -housing_qual_index, -grad, -race_cat,
         -fam_type_new)

# Racecat has 21 values, we will use race_cat
# We will use mar_cat less categories.
# We drop male_hh_married
# We drop grad as it creates perfect prediction.
# We drop fam_type_new aas it is repetative.

# Fix housing_qual_index_alternative.
train$index <- as.vector(train$housing_qual_index_alternative)

train <- train %>%
  select(-housing_qual_index_alternative)

# make 'sex' a 0/1 binary variable - 2 is female.
train <- train %>%
  mutate(female = ifelse(sex == 2, 1, 0)) %>%
  select(-sex)

# normalize 'train_year' to start at 1997
train$data_year <- train$data_year-1997 

# standardize `weight`
train$weight <- (train$weight - mean(train$weight)) / sd(train$weight) 

train <- train %>%
  filter(complete.cases(train$index))# remove 2460 rows with NA's

# Same for test data.
# remove 'control', an ID variable
test <- test %>%
  select(-control, -race, -male_hh_married, -mar,
         -housing_qual_index, -grad, -race_cat,
         -fam_type_new)

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


treat_plan <- vtreat::designTreatmentsZ(
  dframe = train, # training data
  varlist = colnames(train), 
  codeRestriction = c("clean", "isBAD", "lev"), # derived variables types (drop cat_P)
  verbose = FALSE) # suppress messages

# Data is clean.
score_frame <- treat_plan$scoreFrame %>% 
  dplyr::select(varName, origName, code)

grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = "none",
  verboseIter = FALSE, # no training log
  allowParallel = FALSE # FALSE for reproducible results
)

tr_treated <- vtreat::prepare(treat_plan, train)
te_treated <- vtreat::prepare(treat_plan, test)

set.seed(14)
tr_holdout <- dplyr::sample_frac(tr_treated, 0.2)
hid <- as.numeric(rownames(tr_holdout))
tr_treated <- tr_treated[-hid, ]


input_x <- as.matrix(dplyr::select(tr_treated, -educ_cat))
input_y <- as.factor(tr_treated$educ_cat)
levels(input_y) <- c("zero","one","two","three")

# Data to test later.
te_treated_x <- dplyr::select(te_treated, -educ_cat)
te_treated_y <- as.factor(te_treated$educ_cat)
levels(te_treated_y) <- c("zero","one","two","three")


xgb_base <- caret::train(
  x = input_x,
  y = input_y,
  trControl = train_control,
  tuneGrid = grid_default,
  method = "xgbTree",
  verbose = TRUE,
  metric ="Accuracy"
)

nrounds <- 1000

tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 3, # with n folds 
  verboseIter = FALSE, # no training log
  allowParallel = FALSE # FALSE for reproducible results,
)

xgb_tune <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE,
  metric ="Accuracy"
)


tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$Accuracy, probs = probs), min(x$results$Accuracy))) +
    theme_bw()
}

xgb_tune_plot <- tuneplot(xgb_tune)
ggsave("~/ML/xgb_tune_plot.pdf")
saveRDS(xgb_tune$bestTune, "~/ML/xgb_tunebesttune.RDS")
saveRDS(xgb_tune, "~/ML/xgb_tune.RDS")

#################################################################
# Round 2.
tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                     c(xgb_tune$bestTune$max_depth:4),
                     xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)

xgb_tune2 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid2,
  method = "xgbTree",
  verbose = TRUE,
  metric ="Accuracy"
)

xgb_tune2_plot <- tuneplot(xgb_tune2)
ggsave("~/ML/xgb_tune2_plot.pdf")
saveRDS(xgb_tune2$bestTune, "~/ML/xgb_tune2besttune.RDS")
saveRDS(xgb_tune2, "~/ML/xgb_tune2.RDS")

# Round 3.
tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

xgb_tune3 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid3,
  method = "xgbTree",
  verbose = TRUE,
  metric ="Accuracy"
)

xgb_tune3_plot <- tuneplot(xgb_tune3, probs = .95)
ggsave("~/ML/xgb_tune3_plot.pdf")
saveRDS(xgb_tune3$bestTune, "~/ML/xgb_tune3besttune.RDS")
saveRDS(xgb_tune3, "~/ML/xgb_tune3.RDS")


# Round 4.
tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid4,
  method = "xgbTree",
  verbose = TRUE,
 # objective='reg:logistic',
  metric ="Accuracy"
)

xgb_tune4_plot <- tuneplot(xgb_tune4)
ggsave("~/ML/xgb_tune4_plot.pdf")
saveRDS(xgb_tune4$bestTune, "~/ML/xgb_tune4besttune.RDS")
saveRDS(xgb_tune4, "~/ML/xgb_tune4.RDS")

# Round 5.
tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 10000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune5 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid5,
  method = "xgbTree",
  verbose = TRUE,
 # objective='reg:logistic',
  metric ="Accuracy"
)

xgb_tune5_plot <- tuneplot(xgb_tune5)
ggsave("~/ML/xgb_tune5_plot.pdf")
saveRDS(xgb_tune5$bestTune, "~/ML/xgb_tune5besttune.RDS")
saveRDS(xgb_tune5, "~/ML/xgb_tune5.RDS")

# Final Round.
(final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
))

(xgb_model <- caret::train(
  x = input_x,
  y = input_y,
  trControl = train_control,
  tuneGrid = final_grid,
  method = "xgbTree",
  verbose = TRUE,
  metric ="Accuracy"
))

holdout_x <- dplyr::select(tr_holdout, -educ_cat)
holdout_y <- as.factor(tr_holdout$educ_cat)
levels(holdout_y) <- c("zero","one","two","three")

xgb_base_CM <- caret::confusionMatrix(holdout_y, predict(xgb_base, newdata = holdout_x))
saveRDS(xgb_base_CM, "~/ML/xgb_base_CM.RDS")


# Now we combine all the data.
tr_combi <- bind_rows(tr_treated, tr_holdout)
tr_x <- dplyr::select(tr_combi, -educ_cat)
tr_y <- as.factor(tr_combi$educ_cat)
levels(tr_y) <- c("zero","one","two","three")

xgb_base_final <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = train_control,
  tuneGrid = grid_default,
  method = "xgbTree",
  verbose = TRUE,
  metric ="Accuracy"
)
saveRDS(xgb_base_final, "~/ML/xgb_base_final.RDS")

xgb_model_CM <- caret::confusionMatrix(te_treated_y, predict(xgb_base_final, newdata = te_treated_x))
saveRDS(xgb_model_CM, "~/ML/xgb_model_CM.RDS")

xgb_model_final <- caret::train(
  x = tr_x, # tr_x is data frame, xgbTree needs matrix
  y = tr_y,
  trControl = train_control,
  tuneGrid = final_grid,
  method = "xgbTree",
  verbose = TRUE,
  metric ="Accuracy"
)
saveRDS(xgb_model_final, "~/ML/xgb_model_final.RDS")


xgb_model_final_CM <- caret::confusionMatrix(te_treated_y, predict(xgb_model_final, newdata = te_treated_x))
saveRDS(xgb_model_final_CM, "~/ML/xgb_model_final_CM.RDS")

