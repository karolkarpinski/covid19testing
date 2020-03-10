march5 <-read.csv('./testnumbers/2020-03-05.csv')
march6 <-read.csv('./testnumbers/2020-03-06.csv')
march8 <-read.csv('./testnumbers/2020-03-08.csv')

march5$X <- NULL
march6$X <- NULL
march8$X <- NULL



march5$date <- '2020-03-05'
march6$date <- '2020-03-06'
march8$date <- '2020-03-08'


march5$state <- as.character(march5$state)

march5["Total", "positive"] <- sum(march5[, "positive"], na.rm = TRUE)
march5["Total", "negative"] <- sum(march5[, "negative"], na.rm = TRUE)
march5["Total", "pending"] <- sum(march5[, "pending"], na.rm = TRUE)
march5["Total", "total"] <- sum(march5[, "total"], na.rm = TRUE)
march5["Total", "population"] <- sum(march5[, "population"], na.rm = TRUE)
march5["Total", "state"] <- "Total"

march5 <- march5 %>%
  mutate(tests_per_million = total/(population/1000000)) %>%
  mutate(pct_positive = positive/(positive+negative))

write.csv(march5, './testnumbers/2020-03-05.csv', row.names = FALSE)
write.csv(march6, './testnumbers/2020-03-06.csv', row.names = FALSE)
write.csv(march8, './testnumbers/2020-03-08.csv', row.names = FALSE)

all <- march8 %>%
  bind_rows(march6) %>%
  bind_rows(march5)

write.csv(all, './testnumbers/all.csv', row.names = FALSE)

totals <- all %>%
  filter(state == 'Total')

fig <- plot_ly(
  x = totals$date,
  y = totals$delta,
  name = "Testing totals",
  type = 'scatter', mode = 'lines'
)

fig