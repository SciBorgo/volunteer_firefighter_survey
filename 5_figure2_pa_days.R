

# Figure of total physical activity minutes and days
# Borg DN
# April 2023

#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Load data
d <- readRDS(file = 'firefighter-data.RData') %>%
  mutate(ex_strength_days = replace_na(ex_strength_days, 0),
         ex_days = if_else(ex_days >=7, 7, ex_days),
         ex_days = as.numeric(ex_days),
         work_active_days = as.numeric(work_active_days),
         ex_active30mins_days = as.numeric(ex_active30mins_days),
         work_active30mins_days = as.numeric(work_active30mins_days),
         ex_active60mins_days = as.numeric(ex_active60mins_days),
         work_active60mins_days = as.numeric(work_active60mins_days),
         `Days of any physical activity` = cut(ex_days+work_active_days,
                                                            breaks = c(-1,0,4,6,100),
                                                            labels = c('None','1-4','5-6','7')),
         `Days exercised only` = cut(ex_days,
                                                  breaks = c(-1,0,4,6,100),
                                                  labels = c('None','1-4','5-6','7')),
         `Days of any physical activity for at least 30 min` = cut(ex_active30mins_days+work_active30mins_days,
                                                                                breaks = c(-1,0,4,6,100),
                                                                                labels = c('None','1-4','5-6','7')),
         `Days exercised only for at least 30 min` = cut(ex_active30mins_days,
                                                                      breaks = c(-1,0,4,6,100),
                                                                      labels = c('None','1-4','5-6','7')),
         `Days of any physical activity for at least 60 min` = cut(ex_active60mins_days+work_active60mins_days,
                                                                                breaks = c(-1,0,4,6,100),
                                                                                labels = c('None','1-4','5-6','7')),
         `Days exercised only for at least 60 min` = cut(ex_active60mins_days,
                                                                      breaks = c(-1,0,4,6,100),
                                                                      labels = c('None','1-4','5-6','7')),
         `Days of strength or toning activities` = cut(ex_strength_days,
                                                                         breaks = c(-1,0,4,6,100),
                                                                         labels = c('None','1-4','5-6','7')),
         total_physical_activity_min =
           total_min_fitnesswalk +
           total_min_travel_walk +
           total_min_mod_exercise +
           total_min_vig_exercise + # vigirous minutes count for double
           total_work_activity_min,
         total_exercise_min =
           total_min_fitnesswalk +
           total_min_travel_walk +
           total_min_mod_exercise +
           total_min_vig_exercise, # vigirous minutes count for double 
         total_physical_activity_min = cut(total_physical_activity_min,
                                           breaks = c(-1,1,150,301,Inf),
                                           labels = c('1','2','3','4')),
         total_exercise_min = cut(total_exercise_min,
                                  breaks = c(-1,1,150,301,Inf),
                                  labels = c('1','2','3','4'))
         )



# Panel A: Minutes
# Summary plot
d %>%
  dplyr::select(total_physical_activity_min,
                total_exercise_min) %>%
  pivot_longer(cols = everything()) %>%
  rename(Minutes = value) %>%
  group_by(name, Minutes) %>%
  summarise(n = n(),
            pct = (n/nrow(d)*100)) %>%
  mutate(name = recode_factor(name,
                              'total_physical_activity_min' = 'Total physical activity minutes',
                              'total_exercise_min' = 'Total exercise only minutes')) %>%
  ggplot(aes(y = name, x = pct, fill = forcats::fct_rev(Minutes))) +
  geom_col(position = "fill", colour = "black", size = 0.25) +
  theme_light(base_size = 14) +
  scale_fill_viridis_d(end = 0.8,
                       begin = 0.4,
                       option = 'B',
                       direction = -1,
                       labels = c('>300','149-300','1-149','None')) +
  labs(x = "Percentage",
       y = "",
       fill = 'Minutes') +
  scale_x_continuous(labels = scales::percent_format())+
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = T)) -> p1; p1



# Panel B: Days
d %>%
  dplyr::select(`Days of any physical activity`,
         `Days exercised only`,
         `Days of any physical activity for at least 30 min`,
         `Days exercised only for at least 30 min`,
         `Days of any physical activity for at least 60 min`,
         `Days exercised only for at least 60 min`,
         `Days of strength or toning activities`) %>%
  pivot_longer(cols = everything()) %>%
  drop_na() %>%
  rename(Days = value) %>%
  group_by(name, Days) %>%
  summarise(n = n(),
            pct = (n/nrow(d)*100)) %>%
  mutate(Days = recode_factor(Days,
                              '7' = '7',
                              '5-6' = '5-6',
                              '1-4' = '1-4',
                              'None' = 'None')) %>%
  ggplot(aes(y = name, x = pct, fill = Days)) +
  geom_col(position = "fill", colour = "black", size = 0.25) +
  theme_light(base_size = 14) +
  scale_fill_viridis_d(end = 1,
                       option = 'E',
                       direction = -1) +
  #scale_fill_manual(values = cbPalette)+
  labs(x = "Percentage", y = "") +
  scale_x_continuous(labels = scales::percent_format())+
  scale_y_discrete(limits = c(
    'Days of strength or toning activities',
    'Days exercised only for at least 60 min',
    'Days of any physical activity for at least 60 min',
    'Days exercised only for at least 30 min',
    'Days of any physical activity for at least 30 min',
    'Days exercised only',
    'Days of any physical activity'
  ))+theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = T)) -> p2; p2


# Panel
plot_grid(p1,
          p2,
          ncol = 1,
          nrow = 2, 
          scale = 1,
          rel_heights = c(1/3,2/3),
          # labels = c('A','B'),
          # label_size = 20,
          align = 'v',
          axis = "lr")

ggsave(file = "figure2.png",
       width = 10,
       height = 7,
       dpi = 900)


#### End


