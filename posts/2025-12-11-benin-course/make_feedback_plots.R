# make_feedback_plots.R
# Generate a single composite PNG from PRIVATE CSV in gitexclude/.
# Output: feedback-summary.png (committed to repo)

suppressPackageStartupMessages({
  library(ggplot2)
  library(readr)
  library(dplyr)
  library(forcats)
  library(patchwork)
})

# Read and clean data
feedback <- read_csv("gitexclude/Bioconductor_Ethiopia_course_feedback.csv", show_col_types = FALSE) |>
  mutate(across(everything(), ~ trimws(as.character(.))))

# Factor: R level before workshop (ensure consistent order)
feedback <- feedback |>
  mutate(`R level grouped` = factor(
    `How did you rate your R level before the workshop?`,
    levels = c("Beginner", "Intermediate", "Advanced")
  ))

# Factor: improvement scale (1–5), keep empty bins if any
feedback <- feedback |>
  mutate(`Improvement factor` = factor(
    `How much did the workshop improve your R knowledge?`,
    levels = c("1", "2", "3", "4", "5")
  ))

# Factor: overall course rating, reversed for descending order in the plot
feedback <- feedback |>
  mutate(`Overall rating factor` = factor(
    `Please tell us your overall rating for the entire course`,
    levels = rev(c("Excellent", "Very good", "Good", "Satisfactory", "Poor"))
  ))

# Plot 1: Overall course rating (top)
p3 <- ggplot(feedback, aes(x = `Overall rating factor`)) +
  geom_bar(fill = "#D55E00") +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Overall Course Rating", x = "Rating", y = "Count") +
  theme_minimal()

# Plot 2: R skill level before workshop (bottom-left)
p1 <- ggplot(feedback, aes(x = `R level grouped`)) +
  geom_bar(fill = "#0072B2") +
  scale_x_discrete(drop = FALSE) +
  labs(title = "R Skill Level Before Workshop", x = "Skill Level", y = "Count") +
  theme_minimal()

# Plot 3: Improvement in R knowledge (bottom-right)
p2 <- ggplot(feedback, aes(x = `Improvement factor`)) +
  geom_bar(fill = "#009E73") +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Improvement in R Knowledge",
       x = "Rating (1 = No improvement, 5 = Significant)", y = "Count") +
  theme_minimal()

# Combine: overall on top; skill + improvement below
combined_plot <- p3 / (p1 | p2)

# Save next to index.qmd
ggsave("feedback-summary.png", combined_plot, width = 10, height = 8, dpi = 300)
cat("Wrote feedback-summary.png\n")
