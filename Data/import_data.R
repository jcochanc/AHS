# Libraries.
pacman::p_load(haven)

# Place the entire data set into the "Data" folder.

# We need to import the data but it is too big.
# We will randomly sample and use this rs to get an idea of
# what our data looks like.
set.seed(14)
df <- read_dta("./Data/ahs_allyears_demographics.dta")

# We will take 1% of the data.
nrows_to_sample <- ceiling(nrow(df)/100)

# Sample the rows.
subject_index <- sample(1:nrow(df), nrows_to_sample)

# Reduce the data.
df <- df[subject_index,]

# Save the data.
saveRDS(df, "./Data/sample_df.RDS")
