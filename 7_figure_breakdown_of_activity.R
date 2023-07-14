

# Physical activity versus population data
# Borg DN
# April 2023

# Load data
d = readRDS(file = 'firefighter-data.RData') %>%
  mutate(total_physical_activity_min = # remove vigorous county as double minutes
           total_min_fitnesswalk +
           total_min_travel_walk +
           total_min_mod_exercise +
           total_min_vig_exercise +
           total_work_activity_min,
         mod_prop = total_min_mod_exercise/total_physical_activity_min,
         vig_prop = total_min_vig_exercise/total_physical_activity_min,
         walk_exercise_prop = total_min_fitnesswalk/total_physical_activity_min,
         walk_prop = total_min_travel_walk/total_physical_activity_min,
         work_prop = total_work_activity_min/total_physical_activity_min)


# Proportion of total pa minutes that is made up of moderate exercise
d %>%
  ggplot(aes(x = age,
             y = mod_prop))+
  geom_point()+
  stat_smooth(method = 'lm')

# Proportion of total pa minutes that is made up of vigorous exercise
d %>%
  ggplot(aes(x = age,
             y = vig_prop))+
  geom_point()+
  stat_smooth(method = 'lm')

# Proportion of total pa minutes that is made up of walking for exercise
d %>%
  ggplot(aes(x = age,
             y = walk_exercise_prop))+
  geom_point()+
  stat_smooth(method = 'lm')

# Proportion of total pa minutes that is made up of walking for travel
d %>%
  ggplot(aes(x = age,
             y = walk_prop))+
  geom_point()+
  stat_smooth(method = 'lm')

# Proportion of total pa minutes that are made up from workplace activity
d %>%
  filter(age<65) %>%
  ggplot(aes(x = age,
             y = work_prop))+
  geom_point()+
  stat_smooth(method = 'lm')


# Check for beta reg
table(d$mod_prop) # Needs zero inflated model, move 1's off boundary
d %>% ggplot()+ geom_histogram(aes(x = mod_prop), colour = 'white')

table(d$vig_prop) # Need zero inflated model
d %>% ggplot()+ geom_histogram(aes(x = vig_prop), colour = 'white')

table(d$walk_prop) # Needs zero and one (intercept only) inflated model
d %>% ggplot()+ geom_histogram(aes(x = walk_prop), colour = 'white')

table(d$walk_exercise_prop) # Needs zero and one (intercept only) inflated model
d %>% ggplot()+ geom_histogram(aes(x = walk_exercise_prop), colour = 'white')

table(d$work_prop) # Needs zero and one (intercept only) inflated model
d %>% ggplot()+ geom_histogram(aes(x = work_prop), colour = 'white')


# MCMC settings
# n_chains = 8
# n_samps = 10000

#### zoib models
#### Model walking for travel
# fit3 <- brm(bf(walk_prop ~ age,
#                zoi ~ age,
#                coi ~ 1),
#             data = d,
#             cores = 8,
#             chains = n_chains,
#             iter = n_samps,
#             family = zero_one_inflated_beta())

# Save
#save(fit3, file = "fit_walk_travel.RData")

# Load model
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_walk_travel.RData")

# Summary
print(summary(fit3), digits = 4)
gather_draws(fit3, b_age) %>% mean_qi(.value>0)
gather_draws(fit3, b_age) %>% mean_qi(.value<0)
prior_summary(fit3)

# Slope
year_slope <- d %>%
  data_grid(age) %>%
  add_epred_draws(fit3) %>%
  as.data.frame()

lm(.epred ~ age, data = year_slope)[[1]][2] %>%
  as.data.frame() %>%
  mutate(decade_prop = .*10,
         decade_pct = decade_prop*100)

d %>%
  data_grid(age) %>%
  add_fitted_draws(fit3) %>%
  ggplot()+
  stat_lineribbon(aes(x = age,
                      y = .value),
                  .width = c(0.66,0.95),
                  alpha = 1)+
  theme_classic()+
  labs(y = 'Walking for travel',
       x = 'Age (years)')+
  # geom_point(data = d,
  #            aes(x = age,
  #                y = walk_prop),
  #            shape = 1,
  #            size = 2,
  #            alpha = 0.2)+
  scale_y_continuous(limits = c(0,0.45), labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(n.breaks = 10)+
  scale_fill_brewer(palette = 1)  -> p4; p4



#### Model walking for exercise (fitness)
# fit2 <- brm(bf(walk_exercise_prop ~ age,
#                zoi ~ age,
#                coi ~ 1),
#             data = d,
#             cores = 8,
#             chains = n_chains,
#             iter = n_samps,
#             family = zero_one_inflated_beta())

# Save
#save(fit2, file = "fit_walk.RData")

# Load model
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_walk.RData")

# Summary
print(summary(fit2), digits = 3)
gather_draws(fit2, b_age) %>% mean_qi(.value>0)
gather_draws(fit2, b_age) %>% mean_qi(.value<0)
prior_summary(fit2)

# Slope
year_slope <- d %>%
  data_grid(age) %>%
  add_epred_draws(fit2) %>%
  as.data.frame()

lm(.epred ~ age, data = year_slope)[[1]][2] %>%
  as.data.frame() %>%
  mutate(decade_prop = .*10,
         decade_pct = decade_prop*100)

d %>%
  data_grid(age) %>%
  add_fitted_draws(fit2) %>%
  ggplot()+
  stat_lineribbon(aes(x = age,
                      y = .value),
                  .width = c(0.66,0.95),
                  alpha = 1)+
  theme_classic()+
  labs(y = 'Walking for fitness',
       x = 'Age (years)')+
  # geom_point(data = d,
  #          aes(x = age,
  #              y = walk_prop),
  #          shape = 1,
  #          size = 2,
  #          alpha = 0.2)+
  scale_y_continuous(limits = c(0,0.45), labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(n.breaks = 10)+
  scale_fill_brewer(palette = 1) -> p3; p3









#### Model moderate exercise
d$mod_prop[d$mod_prop == 1] = 0.99999 # Shift 1s off the boundary
# fit1 <- brm(bf(mod_prop ~ age,
#                zi ~ age),
#             data = d,
#             cores = 8,
#             chains = n_chains,
#             iter = n_samps,
#             family = zero_inflated_beta())

# Save
#save(fit1, file = "fit_mod.RData")

# Load
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_mod.RData")

# Summary
print(summary(fit1), digits = 3)
gather_draws(fit1, b_age) %>% mean_qi(.value>0)
gather_draws(fit1, b_age) %>% mean_qi(.value<0)
prior_summary(fit1)

# Slope
year_slope <- d %>%
  data_grid(age) %>%
  add_epred_draws(fit1) %>%
  as.data.frame()

lm(.epred ~ age, data = year_slope)[[1]][2] %>%
  as.data.frame() %>%
  mutate(decade_prop = .*10,
         decade_pct = decade_prop*100)


d %>%
  data_grid(age) %>%
  add_fitted_draws(fit1) %>%
  ggplot()+
  stat_lineribbon(aes(x = age,
                      y = .value),
                  .width = c(0.66,0.95),
                  alpha = 1)+
  theme_classic()+
  labs(y = 'Moderate activity',
       x = 'Age (years)')+
  # geom_point(data = d,
  #          aes(x = age,
  #              y = mod_prop),
  #          shape = 1,
  #          size = 2,
  #          alpha = 0.2)+
  scale_y_continuous(limits = c(0,0.45), labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(n.breaks = 10)+
  scale_fill_brewer(palette = 1) -> p2; p2





#### Model vigorous exercise
# fit <- brm(bf(vig_prop ~ age,
#               zi ~ age),
#            data = d,
#            cores = 8,
#            chains = n_chains,
#            iter = n_samps,
#            family = zero_inflated_beta())

# Save
#save(fit, file = "fit_vig.RData")

# Load model
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_vig.RData")

# Summary
print(summary(fit), digits = 3)
gather_draws(fit, b_zi_age) %>% mean_qi(.value>0)
gather_draws(fit, b_age) %>% mean_qi(.value<0)
prior_summary(fit)

# Slope
year_slope <- d %>%
  data_grid(age) %>%
  add_epred_draws(fit) %>%
  as.data.frame()

lm(.epred ~ age, data = year_slope)[[1]][2] %>%
  as.data.frame() %>%
  mutate(decade_prop = .*10,
         decade_pct = decade_prop*100)



d %>%
  data_grid(age) %>%
  add_fitted_draws(fit) %>%
  ggplot()+
  stat_lineribbon(aes(x = age,
                      y = .value),
                  .width = c(0.66,0.95),
                  alpha = 1)+
  theme_classic()+
  labs(y = 'Vigirous activity',
       x = 'Age (years)')+
  # geom_point(data = d,
  #            aes(x = age,
  #                y = vig_prop),
  #            shape = 1,
  #            size = 2,
  #            alpha = 0.2)+
  scale_y_continuous(limits = c(0,0.45), labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(n.breaks = 10)+
  scale_fill_brewer(palette = 1) -> p; p
  





#### Model work activity
# hist(d$work_prop, breaks = 20)
# fit4 <- brm(bf(work_prop ~ age,
#                zoi ~ age,
#                coi ~ 1),
#            data = d,
#            cores = 8,
#            chains = n_chains,
#            iter = n_samps,
#            family = zero_one_inflated_beta())

# Save
#save(fit4, file = "fit_work.RData")

# Load model
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_work.RData")

# Check
pp_check(fit4, re_formula = NULL, ndraws = 100)

# Summary
print(summary(fit4), digits = 3)
gather_draws(fit4, b_age) %>% mean_qi(.value>0)
gather_draws(fit4, b_age) %>% mean_qi(.value<0)

gather_draws(fit4, b_zoi_age) %>% mean_qi(.value>0)
gather_draws(fit4, b_zoi_age) %>% mean_qi(.value<0)

prior_summary(fit4)

# Slope
year_slope <- d %>%
  data_grid(age) %>%
  add_epred_draws(fit4) %>%
  as.data.frame()

lm(.epred ~ age, data = year_slope)[[1]][2] %>%
  as.data.frame() %>%
  mutate(decade_prop = .*10,
         decade_pct = decade_prop*100)


# fit5 <- brm(bf(work_prop ~ age,
#                zoi ~ age,
#                coi ~ 1),
#             data = {d %>% filter(age<66)},
#             cores = 8,
#             chains = n_chains,
#             iter = n_samps,
#             family = zero_one_inflated_beta())
# 
# print(summary(fit5), digits = 4)
# #save(fit5, file = "fit_work_under66.RData")






# d %>%
#   data_grid(age) %>%
#   add_fitted_draws(fit4) %>%
#   ggplot()+
#   stat_lineribbon(aes(x = age,
#                       y = .value),
#                   .width = c(0.66,0.95),
#                   alpha = 1)+
#   # stat_lineribbon(data = dsub,
#   #                 aes(x = age,
#   #                     y = .value),
#   #                 .width = c(0.66,0.95),
#   #                 alpha = 1)+
#   theme_classic()+
#   labs(y = 'Work activity',
#        x = 'Age (years)')+
#   # geom_point(data = d,
#   #          aes(x = age,
#   #              y = work_prop),
#   #          shape = 1,
#   #          size = 2,
#   #          alpha = 0.2)+
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
#   scale_x_continuous(n.breaks = 10)+
#   coord_cartesian(ylim = c(0,0.45))+
#   scale_fill_brewer(palette = 1) -> p5; p5



# Panel plot
plot_grid(p4 + theme(legend.position="none"),
          p3 + theme(legend.position="none"),
          p2 + theme(legend.position="none"),
          p + theme(legend.position="none"),
          p5 + theme(legend.position="none"),
          ncol = 2,
          nrow = 3, 
          labels = c('A','B','C','D','E'),
          align = 'v', 
          axis = "lr",
          label_size = 12)


ggsave(file = "figure_activity_distribution.png",
       width = 7,
       height = 7,
       dpi = 900)





#### End

