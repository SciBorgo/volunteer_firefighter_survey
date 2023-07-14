

# Packages
library(tidyverse)
library(brms)
library(modelr)
library(tidybayes)
library(cowplot)

# Supplement figure - beta regression models
# April 2023
# DN Borg

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


# Plot function
plot_fun <- function(var, x_lab) {
  d %>%
  ggplot()+
  geom_histogram(aes(x = {{var}}), bins = 50, colour = 'white')+
  theme_classic()+
  labs(x = paste({{x_lab}}),
       y = 'Frequency (count)')+
  xlim(-0.1,1.1)+
  coord_cartesian(xlim = c(0,1))
}


# Plots
plot_fun(var = walk_prop, x_lab = 'Walking for travel proportion') -> p1; p1
plot_fun(var = walk_exercise_prop, x_lab = 'Walking for fitness proportion') -> p2; p2
plot_fun(var = mod_prop, x_lab = 'Moderate intensity proportion') -> p3; p3
plot_fun(var = vig_prop, x_lab = 'Vigorous intensity proportion') -> p4; p4
plot_fun(var = work_prop, x_lab = 'Work activity proportion') -> p5; p5

# Panel
plot_grid(p1,
          p2,
          p3,
          p4,
          p5,
          ncol = 2,
          nrow = 3, 
          labels = c('A','B','C','D','E'),
          align = 'v', 
          axis = "lr",
          label_size = 12)


ggsave(file = "supplement_beta.png",
       width = 7,
       height = 7,
       dpi = 900)


#### End









