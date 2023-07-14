

# Helpers

# Summary functions
cat_sum <- function(x) {
  d %>%
    group_by({{x}}) %>%
    count() %>%
    arrange(-n) %>%
    mutate(pc = paste0(round(n/nrow(d)*100,1),"%"))
}

con_sum <- function(x) {
  d %>%
    summarise(mean = mean({{x}}, na.rm = T),
              sd = sd({{x}}, na.rm = T),
              median = median({{x}}, na.rm = T),
              first_quartile = quantile({{x}}, prob = 0.25, na.rm = T),
              third_quartile = quantile({{x}}, prob = 0.75, na.rm = T)
    )
}

cat_sum_gp <- function(x1,x2) {
  d %>%
    group_by({{x1}},{{x2}}) %>%
    count()
}

con_sum_gp <- function(x, group) {
  d %>%
    group_by({{group}}) %>%
    summarise(mean = mean({{x}}, na.rm = T),
              sd = sd({{x}}, na.rm = T),
              median = median({{x}}, na.rm = T),
              first_quartile = quantile({{x}}, prob = 0.25, na.rm = T),
              third_quartile = quantile({{x}}, prob = 0.75, na.rm = T)
    )
}
