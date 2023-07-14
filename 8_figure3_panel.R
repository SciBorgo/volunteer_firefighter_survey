

# Figure 3
# July 2023

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

# Load model
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_age_versus_pa.RData")

d %>%
  data_grid(age) %>%
  add_fitted_draws(fit) %>%
  ggplot()+
  stat_lineribbon(aes(x = age,
                      y = .value),
                  .width = c(0.66,0.95),
                  alpha = 1)+
  theme_classic()+
  labs(y = 'Weekly physical\nactivity time (min)',
       x = 'Age (years)')+
  scale_y_continuous(n.breaks = 8)+
  scale_x_continuous(n.breaks = 10)+
  coord_cartesian(ylim = c(400,1650))+
  scale_fill_brewer(palette = 1) -> pA



# Load model
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_age_versus_exercise_min.RData")

d %>%
  data_grid(age) %>%
  add_fitted_draws(fit) %>%
  ggplot()+
  stat_lineribbon(aes(x = age,
                      y = .value),
                  .width = c(0.66,0.95),
                  alpha = 1)+
  theme_classic()+
  labs(y = 'Weekly exercise\nonly time (min)',
       x = 'Age (years)')+
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(limits = c(400,1650), n.breaks = 8)+
  scale_fill_brewer(palette = 1) -> pB


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


# Load model
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_walk_travel.RData")

d %>%
  data_grid(age) %>%
  add_fitted_draws(fit3) %>%
  ggplot()+
  stat_lineribbon(aes(x = age,
                      y = .value),
                  .width = c(0.66,0.95),
                  alpha = 1)+
  theme_classic()+
  labs(y = 'Walking for travel\n(% of total time)',
       x = 'Age (years)')+
  scale_y_continuous(limits = c(0,0.45), labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(n.breaks = 10)+
  scale_fill_brewer(palette = 1)  -> p4


# Load model
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_walk.RData")

d %>%
  data_grid(age) %>%
  add_fitted_draws(fit2) %>%
  ggplot()+
  stat_lineribbon(aes(x = age,
                      y = .value),
                  .width = c(0.66,0.95),
                  alpha = 1)+
  theme_classic()+
  labs(y = 'Walking for fitness\n(% of total time)',
       x = 'Age (years)')+
  scale_y_continuous(limits = c(0,0.45), labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(n.breaks = 10)+
  scale_fill_brewer(palette = 1) -> p3; p3


d$mod_prop[d$mod_prop == 1] = 0.99999 # Shift 1s off the boundary

# Load
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_mod.RData")

d %>%
  data_grid(age) %>%
  add_fitted_draws(fit1) %>%
  ggplot()+
  stat_lineribbon(aes(x = age,
                      y = .value),
                  .width = c(0.66,0.95),
                  alpha = 1)+
  theme_classic()+
  labs(y = 'Moderate exercise\n(% of total time)',
       x = 'Age (years)')+
  scale_y_continuous(limits = c(0,0.45), labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(n.breaks = 10)+
  scale_fill_brewer(palette = 1) -> p2; p2


# Load model
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_vig.RData")

d %>%
  data_grid(age) %>%
  add_fitted_draws(fit) %>%
  ggplot()+
  stat_lineribbon(aes(x = age,
                      y = .value),
                  .width = c(0.66,0.95),
                  alpha = 1)+
  theme_classic()+
  labs(y = 'Vigirous exercise\n(% of total time)',
       x = 'Age (years)')+
  scale_y_continuous(limits = c(0,0.45), labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(n.breaks = 10)+
  scale_fill_brewer(palette = 1) -> p; p


# Load model
load("C:/Users/borgdn/OneDrive - Queensland University of Technology/QFS survey/3_Analysis/fit_work.RData")

d %>%
  data_grid(age) %>%
  add_fitted_draws(fit4) %>%
  ggplot()+
  stat_lineribbon(aes(x = age,
                      y = .value),
                  .width = c(0.66,0.95),
                  alpha = 1)+
  theme_classic()+
  labs(y = 'Work activity\n(% of total time)',
       x = 'Age (years)')+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(n.breaks = 10)+
  coord_cartesian(ylim = c(0,0.45))+
  scale_fill_brewer(palette = 1) -> p5



# Panel plot
plot_grid(pA + theme(legend.position="none"),
          pB + theme(legend.position="none"),
          p4 + theme(legend.position="none"),
          p3 + theme(legend.position="none"),
          p2 + theme(legend.position="none"),
          p + theme(legend.position="none"),
          p5 + theme(legend.position="none"),
          ncol = 2,
          nrow = 4, 
          labels = c('A','B','C','D','E','F','G'),
          align = 'v', 
          axis = "lr",
          label_size = 12)


ggsave(file = "figure3.png",
       width = 7,
       height = 9,
       dpi = 900)





#### End
