

# Age figure
# Borg DN
# April, 2023

# Set seed
set.seed(123)

# Age analysis
d <- readRDS(file = 'firefighter-data.RData')

# Bootstrap mean and 95% intervals
stat <- function(data, indices) {
  return(mean(data[indices]))
}

univariate_bootstrap <- function(x, iter) {
  bootstrap_results <- boot(x, stat, R = iter)
  return(bootstrap_results)
}

results <- univariate_bootstrap(x = d$age, iter = 2000)

results$t %>%
  as.data.frame() %>%
  ggplot()+
  geom_histogram(aes(x = V1), colour = 'white')+
  theme_classic()

print(results)

boot_ci <-
  boot.ci(results,
          conf = 0.95,
          type = 'bca')


boot_res = cbind(boot_ci$t0[1],
                 boot_ci$bca[4],
                 boot_ci$bca[5]) %>%
  as.data.frame(row.names = 1) %>%
  rename(mean = V1,
         lower = V2,
         upper = V3)

boot_res

#### Histogram plot of age
# Summary stats
q90 = reframe(d, quantiles = quantile(age, c(0.05,0.95))) %>%
  mutate(
    q = 90,
    limit = c('lower','upper'))
q50 = reframe(d, quantiles = quantile(age, c(0.25,0.75))) %>%
  mutate(
    q = 50,
    limit = c('lower','upper'))

# Data for arrows (without pointers); go below the x-axis
arr = bind_rows(q50, q90) %>%
  tidyr::spread(limit, quantiles) %>%
  mutate(y = case_when(
    q==90 ~ -1,
    q==50 ~ -2.25
  ),
  yend = y
  )

# Text labels for arrows (high on y-axis)
arr.text = mutate(arr, x=97, label=paste(c('50%','90%'),'of respondents')) # at far right

# Histogram plot
d %>%
  ggplot()+
  geom_histogram(aes(x = age),
                 colour = 'white',
                 size = 0.25,
                 fill = '#6BAED6',
                 binwidth = 1)+
  theme_classic(base_size = 11)+
  theme(panel.grid = element_blank(),
        strip.text.x = element_text(size = 10))+
  scale_x_continuous(breaks = seq(20,80, by = 10))+
  geom_text(data=arr.text, aes(x=x, y=y, label=label), hjust=1, col='black', size=2.75)+
  geom_segment(data=arr, size=1.7, aes(x = lower, y = y, xend = upper, yend = yend),
               lineend='butt', col='#08519C')+ # block arrows
  labs(x = 'Age (years)', y = 'Frequency (count)')+
  geom_pointrange(data = boot_res,
                  aes(xmin = lower,
                      xmax = upper,
                      x = mean,
                      y = -3.5),
                  # shape = 21,
                  # fill = 'white',
                  size = 0.225)

# "#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#08519C" "#08306B"

ggsave(filename = 'figure1.png',
       dpi = 1200,
       width = 4.5,
       height = 2.75)


summary(d$age)



# #### Other exploratory plots
# # Age versus BMI
# d %>%
#   filter(height>120, bmi<50) %>%
#   ggplot(aes(x = age, y = bmi))+
#   geom_point()+
#   theme_classic()+
#   stat_smooth()+
#   labs(x = 'Age (years)', y = 'Body mass index')
# ggsave(filename = 'figure1.png', dpi = 900)
# 
# # Age versus experience
# d %>%
#   mutate(experience_yrs_volunteer = recode_factor(experience_yrs_volunteer,
#                                                   'Under 1' = '1'),
#          experience_yrs_volunteer = as.integer(experience_yrs_volunteer)) %>%
#   filter(experience_yrs_volunteer<10) %>%
#   ggplot(aes(group = experience_yrs_volunteer,
#              x = experience_yrs_volunteer,
#              y = age))+
#   geom_boxplot(outlier.size = -1)+
#   geom_jitter(width = 0.1)+
#   scale_x_continuous(n.breaks = 10)+
#   theme_classic()+
#   labs(x = 'Volunteer experience (years)', y = 'Age (years)')
# ggsave(filename = 'years-against-volunteer-years.png', dpi = 900)


#### End


