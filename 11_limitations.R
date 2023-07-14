

# Physical activity versus population data
# Borg DN
# April 2023

# Load data
d = readRDS(file = 'firefighter-data.RData') %>%
  mutate(total_physical_activity_min = total_physical_activity_min/2,
         total_exercise_min = total_exercise_min/2,
         met_pa_guidelines = cut(total_physical_activity_min,
                                 breaks = c(-1,150,10000),
                                 labels = c('no','yes')),
         met_ex_guidelines = cut(total_exercise_min, breaks = c(-1,150,10000),
                                 labels = c('no','yes')),
         ex_strength_days = replace_na(ex_strength_days, 0),
         met_strength_guidelines = cut(ex_strength_days, breaks = c(-1,2,10000),
                                 labels = c('no','yes'))
         )

table(d$met_pa_guidelines)
table(d$met_ex_guidelines)

#### Tests


# Any time physical activity
d_pa <- rbind(c(3023600,14169400),
              c(364,116)) %>%
  as.data.frame()

d_pa

relrisk(d_pa, conf.level = 0.95, quiet = T, verbose = T, digits = 3)

chisq.test(d_pa, simulate.p.value = T)
sum(d_pa)


d_pa <- rbind(c(14169400,3023600), # inverse for RR
              c(69,411)) %>%
  as.data.frame()

d_pa
relrisk(d_pa, conf.level = 0.95, quiet = T, verbose = T, digits = 3)




# Exercise only activity
d_ex <- rbind(c(2677400,14518100),
              c(232,248)) %>%
  as.data.frame()

d_ex

chisq.test(d_ex, simulate.p.value = T)
relrisk(d_ex, conf.level = 0.95, quiet = T, verbose = T, digits = 3)
sum(d_ex)

d_ex <- rbind(c(14518100,2677400), # reverse for RR
              c(109,371)) %>%
  as.data.frame()

d_ex
relrisk(d_ex, conf.level = 0.95, quiet = T, verbose = T, digits = 3)




#### End







