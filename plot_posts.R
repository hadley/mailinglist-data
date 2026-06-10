#!/usr/bin/env Rscript

# Plot posts/month per mailing list from posts_by_month.csv.
# Missing months (lists that had no archive for a month, or NA counts) are
# filled with 0 so every list shows a continuous monthly series.

# I propose we sunset the five lists with no posts in last 3 years:
# * mailman
# * r-sig-db
# * r-sig-rugs
# * r-sig-wiki
# * r-ug-ottawa

#
# As well as the nine following lists that have had fewer than
# 20 posts in the last three years:
#
#       r-sig-teaching   18
# r-sig-dynamic-models   17
#         r-sig-robust   13
#            r-sig-hpc    8
#              mm-test    6
#           r-sig-jobs    6
#             r-sig-gr    5
#           ess-debian    2
#            r-sig-dcm    1

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
  geom_line(colour = "#08519c", linewidth = 0.3) +
  facet_wrap(~list, scales = "free_y", ncol = 3) +
  scale_x_datetime(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Mailing list activity on stat.ethz.ch",
    subtitle = "Posts per month over the last 3 years (missing months = 0)",
    x = NULL,
    y = "Posts per month"
  )

ggsave("posts_by_month.pdf", p, width = 11, height = 12)
