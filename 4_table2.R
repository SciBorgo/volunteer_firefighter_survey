

# Physical activity versus population data
# Borg DN
# April 2023

# Load data
d <- readRDS(file = 'firefighter-data.RData') %>%
  mutate(met_pa_guidelines = cut(total_physical_activity_min,
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
table(d$met_strength_guidelines)
table(d$work_type)


#### Tests
# Work type on a typical day
d_typical <-
  cbind(c(4876400,2164900,2557100,1521800),
        c(164,54,89,126)) %>%
  as.data.frame()

d_typical

chisq.test(d_typical)
sum(d_typical)


## Relative risks for each category
# Sitting
d_sit <- rbind(c(4876400,(2164900 + 2557100 + 1521800)),
               c(164,(54+89+126))) %>%
  as.data.frame()

relrisk(d_sit, conf.level = 0.95, quiet = T, verbose = T, digits = 3)

# Standing
d_sta <- rbind(c(2164900,(4876400 + 2557100 + 1521800)),
               c(54,(164+89+126))) %>%
  as.data.frame()

relrisk(d_sta, conf.level = 0.95, quiet = T, verbose = T, digits = 3)


# Walking
d_wal <- rbind(c(2557100,(4876400 + 2164900 + 1521800)),
               c(89,(164+54+126))) %>%
  as.data.frame()

relrisk(d_wal, conf.level = 0.95, quiet = T, verbose = T, digits = 3)


# Heavy labour
d_hea <- rbind(c(1521800,(4876400 + 2164900 + 2557100)),
               c(126,(164+54+89))) %>%
  as.data.frame()

relrisk(d_hea, conf.level = 0.95, quiet = T, verbose = T, digits = 3)





# Any time physical activity
d_pa <- rbind(c(3023600,14169400),
              c(411,69)) %>%
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
              c(371,109)) %>%
  as.data.frame()

d_ex

chisq.test(d_ex, simulate.p.value = T)
relrisk(d_ex, conf.level = 0.95, quiet = TRUE, verbose = T, digits = 3)
sum(d_ex)

d_ex <- rbind(c(14518100,2677400), # reverse for RR
              c(109,371)) %>%
  as.data.frame()

d_ex
relrisk(d_ex, conf.level = 0.95, quiet = TRUE, verbose = T, digits = 3)






# Strength (<2 days/week vs. >=2 days/week)
d_st <- rbind(c(8278000,13062000),
              c(111,369)) %>%
  as.data.frame()

d_st

chisq.test(d_st, simulate.p.value = T)
relrisk(d_st, conf.level = 0.95, quiet = TRUE, verbose = T, digits = 3)
sum(d_st)

d_st <- rbind(c(13062000,8278000), # reverse for RR
              c(369,111)) %>%
  as.data.frame()

d_st

relrisk(d_st, conf.level = 0.95, quiet = TRUE, verbose = T, digits = 3)




#### End







