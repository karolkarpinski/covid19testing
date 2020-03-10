# Calculate tests per 1 million

library(tidyverse)
library(dplyr)

new_file <- './testnumbers/2020-03-08.csv'
old_file <- './testnumbers/2020-03-06.csv'

population <- read.csv('./populations.csv')
tests <- read.csv(new_file)

tests <- inner_join(population, tests)



old_tests <- read.csv(old_file)

delta <- old_tests %>%
  select(state, total) %>%
  right_join(tests, by = "state") %>%
  mutate(delta = (total.y/total.x - 1)) %>%
  select(state,delta)

tests <- inner_join(tests, delta)

tests["Total", "positive"] <- sum(tests[, "positive"], na.rm = TRUE)
tests["Total", "negative"] <- sum(tests[, "negative"], na.rm = TRUE)
tests["Total", "pending"] <- sum(tests[, "pending"], na.rm = TRUE)
tests["Total", "total"] <- sum(tests[, "total"], na.rm = TRUE)
tests["Total", "population"] <- sum(tests[, "population"], na.rm = TRUE)
tests["Total", "state"] <- "Total"


tests <- tests %>%
  mutate(tests_per_million = total/(population/1000000)) %>%
  mutate(pct_positive = positive/(positive+negative))
  
write.csv(tests, new_file, row.names = FALSE)