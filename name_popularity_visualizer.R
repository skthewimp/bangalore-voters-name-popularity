#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args) {
  defaults <- list(
    csv = "bangalore first names.csv",
    name = NA_character_,
    sex = "all",
    reference_year = 2018L,
    min_age = 18L,
    max_age = 110L,
    min_total_per_year = 5000L,
    min_name_count_per_year = 5L,
    start_year = NA_integer_,
    end_year = NA_integer_,
    smooth_window = 3L,
    out_csv = NA_character_,
    out_png = NA_character_,
    out_dir = "outputs"
  )

  if (length(args) == 0) return(defaults)
  for (a in args) {
    if (!grepl("^--", a)) next
    kv <- strsplit(sub("^--", "", a), "=", fixed = TRUE)[[1]]
    key <- kv[1]
    val <- if (length(kv) >= 2) kv[2] else ""
    if (nzchar(key) && key %in% names(defaults)) defaults[[key]] <- val
  }

  to_int <- function(x, fallback) {
    if (is.na(x) || x == "") return(fallback)
    out <- suppressWarnings(as.integer(x))
    if (is.na(out)) fallback else out
  }

  defaults$reference_year <- to_int(defaults$reference_year, 2018L)
  defaults$min_age <- to_int(defaults$min_age, 18L)
  defaults$max_age <- to_int(defaults$max_age, 110L)
  defaults$min_total_per_year <- to_int(defaults$min_total_per_year, 5000L)
  defaults$min_name_count_per_year <- to_int(defaults$min_name_count_per_year, 5L)
  defaults$start_year <- to_int(defaults$start_year, NA_integer_)
  defaults$end_year <- to_int(defaults$end_year, NA_integer_)
  defaults$smooth_window <- to_int(defaults$smooth_window, 3L)

  defaults
}

normalize_name <- function(x) {
  x <- tolower(trimws(x))
  gsub("[^a-z]", "", x)
}

roll_mean_center <- function(x, k) {
  if (k <= 1) return(x)
  if (k %% 2 == 0) k <- k + 1L
  n <- length(x)
  out <- numeric(n)
  h <- k %/% 2L
  for (i in seq_len(n)) {
    lo <- max(1L, i - h)
    hi <- min(n, i + h)
    out[i] <- mean(x[lo:hi])
  }
  out
}

opt <- parse_args(args)

if (is.na(opt$name) || opt$name == "") {
  stop("Pass --name=<query_name>")
}
if (!(opt$sex %in% c("M", "F", "all"))) {
  stop("--sex must be one of M, F, all")
}
if (!file.exists(opt$csv)) {
  stop(sprintf("CSV not found: %s", opt$csv))
}

query_name <- normalize_name(opt$name)
if (query_name == "") stop("Name is empty after normalization")

out_csv <- if (!is.na(opt$out_csv) && nzchar(opt$out_csv)) {
  opt$out_csv
} else {
  file.path(opt$out_dir, sprintf("%s_popularity.csv", query_name))
}
out_png <- if (!is.na(opt$out_png) && nzchar(opt$out_png)) {
  opt$out_png
} else {
  file.path(opt$out_dir, sprintf("%s_popularity.png", query_name))
}

dir.create(opt$out_dir, recursive = TRUE, showWarnings = FALSE)

dt <- fread(opt$csv, select = c("Age", "Sex", "word", "Name"), showProgress = FALSE)

meta <- list(
  rows_seen = nrow(dt)
)

dt[, AgeNum := suppressWarnings(as.integer(Age))]
dt <- dt[!is.na(AgeNum)]
meta$rows_after_valid_age <- nrow(dt)

dt <- dt[AgeNum >= opt$min_age & AgeNum <= opt$max_age]
meta$rows_after_age_bounds <- nrow(dt)

if (opt$sex != "all") {
  dt <- dt[toupper(Sex) == opt$sex]
}
meta$rows_after_sex <- nrow(dt)

dt[, birth_year := opt$reference_year - AgeNum]

if (!("word" %in% names(dt))) {
  dt[, word := NA_character_]
}
if (!("Name" %in% names(dt))) {
  dt[, Name := NA_character_]
}

dt[, token := normalize_name(word)]
dt[token == "" | is.na(token), token := normalize_name(tstrsplit(Name, " ", keep = 1L, fill = ""))]

base <- dt[, .(total_people = .N), by = birth_year]
hit <- dt[token == query_name, .(name_count = .N), by = birth_year]
res <- merge(base, hit, by = "birth_year", all.x = TRUE)
res[is.na(name_count), name_count := 0L]
setorder(res, birth_year)
res[, share_pct := 100 * name_count / total_people]

res <- res[
  total_people >= opt$min_total_per_year &
    name_count >= opt$min_name_count_per_year
]

if (!is.na(opt$start_year)) res <- res[birth_year >= opt$start_year]
if (!is.na(opt$end_year)) res <- res[birth_year <= opt$end_year]

if (nrow(res) == 0) {
  stop("No years left after filters. Lower thresholds.")
}

res[, share_pct_smoothed := roll_mean_center(share_pct, opt$smooth_window)]

setnames(res, "birth_year", "year")
fwrite(res, out_csv)

p <- ggplot(res, aes(x = year)) +
  geom_line(aes(y = share_pct), color = "#8a8a8a", linewidth = 0.7, alpha = 0.8) +
  geom_line(aes(y = share_pct_smoothed), color = "#1f77b4", linewidth = 1.1) +
  labs(
    title = sprintf("Popularity of '%s' by inferred birth year", query_name),
    subtitle = sprintf(
      "Sex=%s | min_total_per_year=%d | min_name_count_per_year=%d",
      opt$sex, opt$min_total_per_year, opt$min_name_count_per_year
    ),
    x = "Inferred birth year",
    y = "Share of people with this name (%)"
  ) +
  theme_minimal(base_size = 11)

ggsave(out_png, p, width = 11, height = 6, dpi = 150)

cat(sprintf("Saved CSV: %s\n", out_csv))
cat(sprintf("Saved PNG: %s\n", out_png))
cat(sprintf("Rows used after filters: %d\n", nrow(res)))
cat(sprintf(
  "Meta: rows_seen=%d rows_after_valid_age=%d rows_after_age_bounds=%d rows_after_sex=%d\n",
  meta$rows_seen, meta$rows_after_valid_age, meta$rows_after_age_bounds, meta$rows_after_sex
))
cat("Note: birth year is inferred as reference_year - age, so individual rows can be off by ~1 year.\n")
