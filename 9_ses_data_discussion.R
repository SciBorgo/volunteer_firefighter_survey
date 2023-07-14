

# SES data for discussion
# Borg DN
# May 2023

# Load data
d <- read_csv('ses_january-to-march-2019.csv') %>%
  clean_names()

# Look at variables
names(d)

# Look at a few rows of data
head(d,10)

# Select out brisbane city
dsub <-
  d %>%
  filter(area != 'Brisbane City')

# Summary
mean(dsub$age, na.rm = T)
sd(dsub$age, na.rm = T)

median(dsub$age, na.rm = T)

nrow(dsub)


#### End