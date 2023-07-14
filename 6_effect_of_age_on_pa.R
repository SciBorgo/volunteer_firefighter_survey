

# Physical activity versus population data
# Borg DN
# April 2023

# Load data
d <- readRDS(file = 'firefighter-data.RData') %>%
  mutate(total_physical_activity_min = # for these two variables remove vigorous county as double minutes
           total_min_fitnesswalk +
           total_min_travel_walk +
           total_min_mod_exercise +
           total_min_vig_exercise +
           total_work_activity_min,
         total_exercise_min =
           total_min_fitnesswalk +
           total_min_travel_walk +
           total_min_mod_exercise +
           total_min_vig_exercise)


# Description of activity minutes
summary(d$total_physical_activity_min/7)
summary(d$total_exercise_min/7)

summary(d$total_min_mod_exercise/7)
summary(d$total_min_vig_exercise/7)

# # MCMC settings
# n_samps = 10000
# n_chains = 8

# # Physical activity
# fit <-
#   brm(
#     formula = total_physical_activity_min ~ age,
#     family = negbinomial(link = "log", link_shape = "log"),
#     prior = c(set_prior("normal(0,5)", class = "b"),
#               set_prior("gamma(0.01, 0.01)", class = "shape")),
#     cores = 8,
#     iter = n_samps,
#     chains = n_chains,
#     control = list(adapt_delta = 0.99),
#     seed = 123,
#     data = d)

# Save
#save(fit, file = "fit_age_versus_pa.RData")

# Load model
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_age_versus_pa.RData")

# Summary
print(summary(fit),digits = 4)
gather_draws(fit, b_age) %>% mean_qi(.value<0)
prior_summary(fit)

# Slope
year_slope <- d %>%
  data_grid(age) %>%
  add_epred_draws(fit) %>%
  as.data.frame()

slope <- lm(.epred ~ age, data = year_slope)
slope

slope$coefficients[2]*10 # per decade


d %>%
  data_grid(age) %>%
  add_fitted_draws(fit) %>%
  ggplot()+
  stat_lineribbon(aes(x = age,
                      y = .value),
                  .width = c(0.66,0.95),
                  alpha = 1)+
  theme_classic()+
  labs(y = 'Weekly physical activity time\n(min)',
       x = 'Age (years)')+
  # geom_point(aes(x = age,
  #                y = total_physical_activity_min))+
  scale_y_continuous(n.breaks = 8)+
  scale_x_continuous(n.breaks = 10)+
  coord_cartesian(ylim = c(400,1650))+
  scale_fill_brewer(palette = 1) -> p; p




# Exercise only minutes
# fit <-
#   brm(
#     formula = total_exercise_min ~ age,
#     family = negbinomial(link = "log", link_shape = "log"),
#     prior = c(set_prior("normal(0,5)", class = "b"),
#               set_prior("gamma(0.01, 0.01)", class = "shape")),
#     cores = 8,
#     iter = n_samps,
#     chains = n_chains,
#     control = list(adapt_delta = 0.99),
#     seed = 123,
#     data = d)

#save(fit, file = "fit_age_versus_exercise_min.RData")

# Load model
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_age_versus_exercise_min.RData")

# Summary
print(summary(fit),digits = 4)
gather_draws(fit, b_age) %>% mean_qi(.value>0)
gather_draws(fit, b_age) %>% mean_qi(.value<0)
prior_summary(fit)

# Slope
year_slope <- d %>%
  data_grid(age) %>%
  add_epred_draws(fit) %>%
  as.data.frame()

lm(.epred ~ age, data = year_slope)


d %>%
  data_grid(age) %>%
  add_fitted_draws(fit) %>%
  ggplot()+
  stat_lineribbon(aes(x = age,
                      y = .value),
                  .width = c(0.66,0.95),
                  alpha = 1)+
  theme_classic()+
  labs(y = 'Weekly exercise only time\n(min)',
       x = 'Age (years)')+
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(limits = c(400,1650), n.breaks = 8)+
  scale_fill_brewer(palette = 1) -> p2; p2


# Panel plot
plot_grid(p + theme(legend.position="none"),
          p2 + theme(legend.position="none"),
          ncol = 2,
          nrow = 1, 
          labels = c('A','B'),
          align = 'v', 
          axis = "lr",
          label_size = 12)


# ggsave(file = "figure_age_pa_exercise.png",
#        width = 7,
#        height = 3,
#        dpi = 900)



