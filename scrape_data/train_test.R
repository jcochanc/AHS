pacman::p_load(dplyr)

# Load in the data we will work with.
df <- readRDS("./Data/df_two_indeces_demographic_vars.RDS")

# Confirm by looking at the names.
names(df) # Carolina also confirms.

# Drop the dim vars.
df <- df %>%
  select(-Dim.1, -Dim.2, -Dim.3, -Dim.4, -Dim.5, -Dim.6)

set.seed(67) # Linde's number.

in_train <- sample(1:nrow(df), ceiling(0.8*nrow(df)))

# Split.
train <- df[in_train,]

test <- df[-in_train, ]

# Save.
saveRDS(train, "./Data/train.RDS")
saveRDS(test, "./Data/test.RDS")
