# make_feedback_twocols.R
# Read only two columns from Bioconductor_{Ethiopia,Kenya,Benin}_course_feedback.csv,
# combine, plot, and write a summary CSV.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(forcats)
  library(ggplot2)
  library(patchwork)
  library(stringr)
})

data_dir <- "gitexclude"
pattern <- "^Bioconductor_(Ethiopia|Kenya|Benin)_course_feedback\\.csv$"
files <- list.files(data_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)

if(length(files) == 0) stop("No matching CSV files found in gitexclude/")

# Columns to keep
keep_cols <- c(
  "How did you rate your R level before the workshop?",
  "How much did the workshop improve your R knowledge?"
)

read_one <- function(path) {
  df <- read_csv(path,
                 show_col_types = FALSE,
                 col_types = cols(.default = "c"),
                 col_select = any_of(keep_cols)) %>%
    mutate(across(everything(), ~ trimws(as.character(.))))
  country <- str_extract(basename(path), "(?i)Ethiopia|Kenya|Benin")
  df$Country <- tools::toTitleCase(tolower(country))
  return(df)
}

lst <- lapply(files, read_one)
feedback <- bind_rows(lst)

# Check required columns exist after reading
missing_cols <- setdiff(keep_cols, colnames(feedback))
if(length(missing_cols) > 0) stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))

# Normalize/fix factor levels (adjust if your data uses different labels)
feedback <- feedback %>%
  mutate(
    R_before = factor(
      `How did you rate your R level before the workshop?`,
      levels = c("Beginner", "Intermediate", "Advanced")
    ),
    Improvement = factor(
      `How much did the workshop improve your R knowledge?`,
      levels = c("1","2","3","4","5")
    )
  ) %>%
  # ensure levels exist even if some are missing
  mutate(
    R_before = fct_expand(R_before, c("Beginner","Intermediate","Advanced")),
    Improvement = fct_expand(Improvement, c("1","2","3","4","5"))
  )

# --- Combined plots ---------------------------------------------------------
p1 <- ggplot(feedback, aes(x = R_before)) +
  geom_bar(fill = "#0072B2") +
  labs(title = "R level before workshop (Combined)", x = "R level", y = "Count") +
  scale_x_discrete(drop = FALSE) +
  theme_minimal()

p2 <- ggplot(feedback, aes(x = Improvement)) +
  geom_bar(fill = "#D55E00") +
  labs(title = "Improvement in R knowledge (Combined)", x = "Improvement (1-5)", y = "Count") +
  scale_x_discrete(drop = FALSE) +
  theme_minimal()

combined_plot <- p1 | p2
ggsave("feedback-twocols-combined.png", combined_plot, width = 10, height = 4.5, dpi = 300)
message("Wrote feedback-twocols-combined.png")

# --- Summary table: counts + percentages per country & combined --------------
make_summary <- function(df, group_name = "Combined") {
  total <- nrow(df)
  tab_R <- df %>%
    group_by(R_before) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(variable = "R_before")
  tab_I <- df %>%
    group_by(Improvement) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(variable = "Improvement")
  tab <- bind_rows(tab_R, tab_I) %>%
    mutate(
      pct = round(100 * count / total, 1),
      country = group_name,
      level = coalesce(as.character(R_before), as.character(Improvement))
    ) %>%
    select(country, variable, level, count, pct)
  return(tab)
}

# Per-country summaries
per_country <- feedback %>%
  group_by(Country) %>%
  group_map(~ make_summary(.x, unique(.x$Country)), .keep = TRUE) %>%
  bind_rows()

# Combined summary
combined <- make_summary(feedback, "Combined")

summary_tbl <- bind_rows(per_country, combined)

write_csv(summary_tbl, "feedback-twocols-summary.csv")
message("Wrote feedback-twocols-summary.csv")

# --- Optional: per-country faceted plots for quick comparison ----------------
p1f <- ggplot(feedback, aes(x = R_before)) +
  geom_bar(fill = "#0072B2") +   # blue
  facet_wrap(~ Country, nrow = 1) +
  labs(title = "R level before by country", x = "R level", y = "Count") +
  scale_x_discrete(drop = FALSE) +
  theme_minimal()

p2f <- ggplot(feedback, aes(x = Improvement)) +
  geom_bar(fill = "#D55E00") +   # orange
  facet_wrap(~ Country, nrow = 1) +
  labs(title = "Improvement in R knowledge by country", x = "Improvement (1-5)", y = "Count") +
  scale_x_discrete(drop = FALSE) +
  theme_minimal()

faceted_plot <- p1f / p2f

ggsave("feedback-twocols-bycountry.png", faceted_plot, width = 12, height = 6, dpi = 300)
message("Wrote feedback-twocols-bycountry.png")
