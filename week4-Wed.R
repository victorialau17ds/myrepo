
library(ratings)

only_10 <-
  ratings %>%
  group_by(series) %>%
  filter(all(1:10 %in% episode)) %>%
  group_by(series) %>%
  slice(1:10) %>%
  mutate(mean_rate = mean(channels_rank)) %>%
  ungroup()
View(only_10)
# filters values that must meet 1-10 range, instead of including 1-6 or 1-8, also adds a column that will have the average
#weekly ratings
ggplot(data = only_10,
       mapping = aes(x = episode,
                     y = channels_rank, is.na = TRUE,
                     group = series,
                     color = series)) +
  geom_line() +
  geom_point(size = 5)
ggplot(data = only_10,
       mapping = aes(x = series,
                     y = mean_rate, is.na = TRUE,
                     group = series,
                     color = series)) +
  xlab("Season") +
  ylab("Average 7-day rating") +
  geom_line() +
  geom_point(size = 5)