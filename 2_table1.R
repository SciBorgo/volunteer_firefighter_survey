

# Table 1
# Borg DN
# April, 2023

# Load helpers
source('99_helpers.R')

# Load data
d <- readRDS(file = 'firefighter-data.RData')

# Count/integer variables
con_sum(x = age)
cat_sum(x = gender)

quantile_df <- function(x, probs = c(0.25, 0.5, 0.75)) {
  tibble(quantile = probs, value = quantile(x, probs))
}

d %>%
  drop_na(mass) %>%
  reframe(across(mass, quantile_df))

d %>%
  filter(bmi<45) %>%
  reframe(across(bmi, quantile_df))


cat_sum(x = bmi_cat)


cat_sum(x = acceptable_weight)

con_sum_gp(x = age, group = gender)
con_sum_gp(x = mass, group = gender)
con_sum_gp(x = height, group = gender)
con_sum_gp(x = bmi, group = gender)
cat_sum_gp(x1 = bmi_cat, x2 = gender)
cat_sum_gp(x1 = gender, x2 = acceptable_weight)

# Brigade/service variables
cat_sum(x = region_brigade)
con_sum(x = experience_yrs_volunteer)
cat_sum(x = permanent_firefighter)
con_sum(x = experience_yrs_permanent)



#### End

