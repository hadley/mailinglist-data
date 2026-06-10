# stat.ethz.ch mailing list activity

Monthly post counts for every mailing list on
[stat.ethz.ch](https://stat.ethz.ch/mailman/listinfo) over the last 3 years.

## Data

**[`posts_by_month.csv`](posts_by_month.csv)** — one row per list per month:

| column | description |
| --- | --- |
| `list` | mailing list name |
| `year` | calendar year |
| `month` | calendar month (English name) |
| `year_month` | `YYYY-Month`, e.g. `2025-May` |
| `n_posts` | number of messages posted that month |

Counts come from each list's pipermail archive. Pipermail chunks archives at
different intervals (monthly, quarterly, or yearly, and some lists mix them),
so every message is bucketed into its true calendar month by reading the date
on its mbox `From ` line — counts are not prorated across a volume.

28 of the 38 lists have activity in the window. The rest are either private
(no public archive) or dormant (no posts in the last 3 years).

## Scripts

- **[`count_posts.R`](count_posts.R)** — scrapes the lists, downloads the
  archives, and writes `posts_by_month.csv`. Every fetched page/archive is
  cached under `./cache/`, so the script can be interrupted and re-run: it
  skips anything already on disk and only fetches what's left.
- **[`plot_posts.R`](plot_posts.R)** — reads the CSV and renders
  [`posts_by_month.pdf`](posts_by_month.pdf), a faceted plot of posts/month per
  list (missing months filled with 0).

## Acknowledgement

This code was written by Claude Code.
