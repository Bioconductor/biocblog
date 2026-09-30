library(ggplot2)
library(dplyr)
library(readr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(plotly)
library(htmlwidgets)

data_2026 <- read_csv("../media/eurobioc2026_countries.csv")

counts <- data_2026 |>
  rename(
    Country = country,
    Count = participants
  ) |>
  mutate(Country = case_when(
    Country == "United States" ~ "United States of America",
    Country == "South Korea" ~ "Republic of Korea",
    TRUE ~ Country
  ))

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(name != "Antarctica") |>
  left_join(counts, by = c("name" = "Country")) |>
  mutate(log_Count = log1p(Count))

max_count <- ceiling(max(counts$Count, na.rm = TRUE) / 50) * 50

breaks <- unique(c(
  0,
  log1p(1),
  log1p(5),
  log1p(10),
  log1p(50),
  log1p(max_count)
))

labels <- round(expm1(breaks))

p <- ggplot(data = world) +
  geom_sf(
    aes(
      fill = log_Count,
      text = paste0(name, ": ", if_else(is.na(Count), 0L, Count), " participants")
    ),
    color = "white"
  ) +
  scale_fill_viridis(
    option = "magma",
    na.value = "grey",
    name = "Participants",
    limits = c(0, log1p(max_count)),
    breaks = breaks,
    labels = labels,
    direction = -1
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 90),
    expand = FALSE
  ) +
  theme_void() +
  theme(axis.line.y = element_blank())

interactive_map <- ggplotly(p, tooltip = "text") |>
  layout(
    xaxis = list(
      showline = FALSE,
      showticklabels = FALSE,
      zeroline = FALSE,
      title_standoff = 5
    )
  )

saveWidget(
  interactive_map,
  file = "../media/eurobioc2026-participants-map.html",
  selfcontained = TRUE
)
