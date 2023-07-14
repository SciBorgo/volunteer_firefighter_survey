

# Missing data
# Borg DN
# April 2023


# Load data
d <- read_csv('fire-data-20-07-22.csv') %>%
  clean_names() %>%
  mutate(submit_date = as.Date(submit_date),
         pounds = as.numeric(weight_lbs),
         pounds = weight_lbs/2.2,
         kgs = as.numeric(weight_kg),
         mass = round(pmax(weight_lbs, kgs, na.rm = T),0),
         bmi = mass/(height/100)^2,
         bmi_cat = cut(bmi, breaks = c(0,18.5,25,30,100)),
         experience_yrs_permanent = as.integer(recode_factor(experience_yrs_permanent, 'Under 1' = '1')),
         experience_yrs_volunteer = as.integer(recode_factor(experience_yrs_volunteer, 'Under 1' = '1')),
         total_physical_activity_min =
           total_min_fitnesswalk +
           total_min_travel_walk +
           total_min_mod_exercise +
           (total_min_vig_exercise*2) + # vigirous minutes count for double
           total_work_activity_min,
         total_exercise_min =
           total_min_fitnesswalk +
           total_min_travel_walk +
           total_min_mod_exercise +
           (total_min_vig_exercise*2)) %>% # vigirous minutes count for double
  replace_with_na(replace = list(ex_days = c(8,10,16,28,40)))

dmiss <-
  d %>%
  dplyr::select(age,
         gender,
         height,
         mass,
         acceptable_weight,
         region_brigade,
         experience_yrs_volunteer,
         permanent_firefighter,
         experience_yrs_permanent,
         ex_days,
         work_active_days,
         ex_strength_days,
         ex_active30mins_days,
         work_active30mins_days,
         ex_active60mins_days,
         work_active60mins_days,
         total_min_fitnesswalk,
         total_min_travel_walk,
         total_min_mod_exercise,
         total_min_vig_exercise,
         total_work_activity_min,
         work_type) %>%
  mutate(age = as.numeric(age),
         gender = as.factor(gender),
         height = as.numeric(height),
         mass = as.numeric(mass),
         ex_strength_days = replace_na(ex_strength_days, 0))

missing_vars <- sapply(dmiss, function(x) sum(is.na(x))) %>%
  .[which(.>0)]

missing_vars

naniar::vis_miss(dmiss) +
  theme(axis.text.x = element_text(angle = 45),
        plot.margin = margin(1,2,1,1, "cm"))

ggsave(file = "supplement_missing_data.png",
       width = 7,
       height = 6,
       dpi = 900)



