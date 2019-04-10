# Libraries.
pacman::p_load(dplyr, visdat, tidyr, ggplot2)

# Load the sample data.
df <- readRDS("./Data/sample_df.RDS")

# Overview of data.
vis_dat(df, warn_large_data = FALSE)

# It looks like there are clusters of variables
# with high missingness. We should proceed by removing them
# now and discussing their exclusion.

# Use code from:
# https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html

missing.values <- df %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <- (missing.values
           %>% filter(isna == T) #Remove complete data.
           %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'),
                    labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

percentage.plot

# Confirm that this did what we think it did.
sum(apply(df, 2, anyNA)) # 56 variables with missingness.
ncol(df) - sum(apply(df, 2, anyNA)) # 354 removed. 

# How to proceed? Suggestion is to remove variables with
# more than 25% missing.


