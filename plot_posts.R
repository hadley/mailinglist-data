library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

raw <- read.csv("posts_by_month.csv", stringsAsFactors = FALSE)
raw <- raw |>
  mutate(date = ISOdate(year, match(month, month.name), 1))

raw |> count(list, wt = n_posts, sort = TRUE)
raw |>
  filter(date > today() - years(1)) |>
  count(list, wt = n_posts, sort = TRUE)

# Fill in missing  months with 0 posts.
all_months <- seq(min(raw$date), max(raw$date), by = "month")
posts <- raw |>
  tidyr::complete(list, date = all_months, fill = list(n_posts = 0))

# Order facets by total volume (busiest list first).
order_lists <- posts |>
  group_by(list) |>
  summarise(total = sum(n_posts), .groups = "drop") |>
  arrange(desc(total))
posts$list <- factor(posts$list, levels = order_lists$list)

ggplot(posts, aes(date, n_posts)) +
  geom_line() +
  facet_wrap(~list, scales = "free_y", ncol = 3) +
  scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(limits = function(l) c(0, ceiling(l[2] / 50) * 50)) +
  labs(
    title = "Mailing list activity on stat.ethz.ch",
    subtitle = "(Note varying y-axes)",
    x = NULL,
    y = "Posts per month"
  )

ggsave("posts_by_month.pdf", width = 12, height = 16)
