#!/usr/bin/env Rscript

# Count monthly posts for every mailing list on stat.ethz.ch over the last 3 years.
#
# Strategy
#   1. Scrape the master list index (/mailman/listinfo) for all list names.
#   2. For each list, read its pipermail archive index to discover its archive
#      "volumes". Pipermail chunks archives at one of three intervals, and a
#      list may even mix them over time:
#         monthly    -> 2025-May/      + 2025-May.txt.gz
#         quarterly  -> 2025q4/        + 2025q4.txt.gz
#         yearly     -> 2025/          + 2025.txt.gz
#   3. For every volume that overlaps the last 3 years, download its gzipped
#      mbox (.txt.gz) and count messages per calendar month from the mbox
#      "From " separator lines (each begins a message and ends in a date).
#      This gives true per-month counts regardless of how the volume is chunked.
#
# Caching / resumability
#   Every page/archive fetched is written to ./cache. A file is only downloaded
#   if its cache copy is missing, and known-404 URLs get a ".missing" sentinel
#   so they are not retried. Kill the script at any point and re-run: it skips
#   everything already on disk and only fetches what is left. The CSV is
#   rewritten after each list, so a partial run still yields a usable file.
#
# Output
#   posts_by_month.csv  with columns: list, year, month, year_month, n_posts
#
# Usage
#   Rscript count_posts.R

# ---------------------------------------------------------------- config -----
base_url <- "https://stat.ethz.ch"
listinfo_url <- paste0(base_url, "/mailman/listinfo")
cache_dir <- "cache"
out_file <- "posts_by_month.csv"
years_back <- 3
polite_delay <- 0.5 # seconds to wait after each *real* network fetch
max_retries <- 3 # retries for transient download errors

dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------- utilities -----

# Download `url` into cache file `path`, reusing the cached copy when present.
# Returns one of: "cached", "fetched", or "missing" (permanent 404 / not found).
download_cached <- function(url, path) {
  if (file.exists(path) && file.info(path)$size > 0) {
    return("cached")
  }
  if (file.exists(paste0(path, ".missing"))) {
    return("missing")
  }

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".part")

  for (attempt in seq_len(max_retries)) {
    ok <- tryCatch(
      {
        utils::download.file(
          url,
          tmp,
          method = "libcurl",
          quiet = TRUE,
          mode = "wb"
        )
        TRUE
      },
      error = function(e) conditionMessage(e)
    )

    if (isTRUE(ok)) {
      file.rename(tmp, path)
      Sys.sleep(polite_delay)
      return("fetched")
    }

    # A 404 (or other "not found") is permanent: record a sentinel and stop.
    if (grepl("404|cannot open|Not Found|400", ok, ignore.case = TRUE)) {
      if (file.exists(tmp)) {
        unlink(tmp)
      }
      file.create(paste0(path, ".missing"))
      return("missing")
    }

    # Otherwise treat as transient and back off before retrying.
    Sys.sleep(polite_delay * attempt)
  }

  if (file.exists(tmp)) {
    unlink(tmp)
  }
  "missing"
}

read_cache <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "latin1"), collapse = "\n")
}

# All list names linked from the master listinfo page.
get_list_names <- function() {
  path <- file.path(cache_dir, "listinfo.html")
  if (download_cached(listinfo_url, path) == "missing") {
    stop("Could not download the master listinfo page.")
  }
  html <- read_cache(path)
  m <- regmatches(
    html,
    gregexpr("listinfo/([a-z0-9._-]+)", html, ignore.case = TRUE)
  )[[1]]
  sort(unique(sub("listinfo/", "", m, ignore.case = TRUE)))
}

# The set of "YYYY-Month" months in the last `years_back` years, e.g. "2025-May".
# month.name is base R's locale-independent English month names.
target_months <- function(years_back) {
  today <- Sys.Date()
  cutoff <- seq(today, by = paste0("-", years_back, " years"), length.out = 2)[
    2
  ]
  months <- seq(as.Date(format(cutoff, "%Y-%m-01")), today, by = "month")
  paste0(
    format(months, "%Y"),
    "-",
    month.name[as.integer(format(months, "%m"))]
  )
}

# Archive volumes for a list, taken from "<vol>/date.html" links in its index.
# A volume id is one of: "2025-May" (monthly), "2025q4" (quarterly), "2025"
# (yearly). Returns NULL when the list has no public archive.
list_volumes <- function(list_name) {
  path <- file.path(cache_dir, list_name, "index.html")
  url <- paste0(base_url, "/pipermail/", list_name, "/")
  if (download_cached(url, path) == "missing") {
    return(NULL)
  } # no public archive
  html <- read_cache(path)
  m <- regmatches(
    html,
    gregexpr(
      "[0-9]{4}(?:-[A-Za-z]+|q[1-4])?(?=/date\\.html)",
      html,
      perl = TRUE
    )
  )[[1]]
  unique(m)
}

# The set of "YYYY-Month" months a volume can contain.
volume_months <- function(vol) {
  yr <- substr(vol, 1, 4)
  if (grepl("^[0-9]{4}-[A-Za-z]+$", vol)) {
    # monthly
    vol
  } else if (grepl("^[0-9]{4}q[1-4]$", vol)) {
    # quarterly
    q <- as.integer(sub(".*q", "", vol))
    paste0(yr, "-", month.name[((q - 1) * 3 + 1):((q - 1) * 3 + 3)])
  } else {
    # yearly
    paste0(yr, "-", month.name)
  }
}

# Count posts per month inside one volume's gzipped mbox. Returns a named
# integer vector keyed by "YYYY-Month". Each message in an mbox begins with a
# "From " line whose tail is "... Mon DD HH:MM:SS YYYY".
count_volume <- function(list_name, vol) {
  gz <- file.path(cache_dir, list_name, paste0(vol, ".txt.gz"))
  url <- paste0(base_url, "/pipermail/", list_name, "/", vol, ".txt.gz")
  if (download_cached(url, gz) == "missing") {
    return(integer(0))
  }

  con <- gzfile(gz, "rb")
  on.exit(close(con))
  lines <- readLines(con, warn = FALSE, skipNul = TRUE)

  froms <- grep("^From ", lines, value = TRUE, useBytes = TRUE)
  m <- regmatches(
    froms,
    regexec(
      "^From .* ([A-Z][a-z]{2}) +[0-9]+ [0-9:]+ ([0-9]{4})$",
      froms,
      useBytes = TRUE
    )
  )
  hits <- m[lengths(m) == 3L]
  if (length(hits) == 0) {
    return(integer(0))
  }

  mon <- vapply(hits, `[`, "", 2L)
  yr <- vapply(hits, `[`, "", 3L)
  ym <- paste0(yr, "-", month.name[match(mon, month.abb)])
  tab <- table(ym[!is.na(match(mon, month.abb))])
  stats::setNames(as.integer(tab), names(tab))
}

# ------------------------------------------------------------------ main -----

lists <- get_list_names()
window <- target_months(years_back)
cat(sprintf(
  "Found %d lists. Counting posts for %d months (%s .. %s).\n",
  length(lists),
  length(window),
  window[1],
  window[length(window)]
))

rows <- list()
for (i in seq_along(lists)) {
  lst <- lists[i]
  cat(sprintf("[%2d/%2d] %-22s ", i, length(lists), lst))

  vols <- list_volumes(lst)
  if (is.null(vols)) {
    cat("(no public archive)\n")
    next
  }

  # Keep only volumes that overlap the target window.
  vols <- vols[vapply(vols, function(v) any(volume_months(v) %in% window), NA)]
  if (length(vols) == 0) {
    cat("(no months in window)\n")
    next
  }

  # Tally every overlapping volume, then keep only in-window months.
  counts <- integer(0)
  for (v in vols) {
    cv <- count_volume(lst, v)
    for (ym in names(cv)) {
      prev <- if (ym %in% names(counts)) counts[[ym]] else 0L
      counts[ym] <- prev + cv[[ym]]
    }
  }
  counts <- counts[names(counts) %in% window]

  if (length(counts) == 0) {
    cat("(no posts in window)\n")
    next
  }

  for (ym in names(counts)) {
    rows[[length(rows) + 1]] <- data.frame(
      list = lst,
      year = as.integer(sub("-.*", "", ym)),
      month = sub(".*-", "", ym),
      year_month = ym,
      n_posts = as.integer(counts[[ym]]),
      stringsAsFactors = FALSE
    )
  }
  cat(sprintf(
    "%d volume(s), %d months, %d posts\n",
    length(vols),
    length(counts),
    sum(counts)
  ))

  # Write incrementally so an interruption still leaves a usable CSV.
  utils::write.csv(do.call(rbind, rows), out_file, row.names = FALSE)
}

if (length(rows) == 0) {
  cat("No data collected.\n")
} else {
  result <- do.call(rbind, rows)
  ord <- order(result$list, match(result$year_month, target_months(years_back)))
  result <- result[ord, ]
  utils::write.csv(result, out_file, row.names = FALSE)
  cat(sprintf("\nDone. Wrote %d rows to %s\n", nrow(result), out_file))
}
