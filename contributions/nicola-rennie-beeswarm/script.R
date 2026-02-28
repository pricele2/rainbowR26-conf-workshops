library(tidyverse)
library(ggview)
library(ggbeeswarm)
library(ggtext)


# Load data ---------------------------------------------------------------

sexual_general_health <- read_csv("data/additional-ew/sexual_general_health.csv")


# Data wrangling ----------------------------------------------------------

## Perc of population in bad/very bad health by sexual orientation ----
plot_data <- sexual_general_health |>
  group_by(area_name, sexual_orientation) |>
  mutate(population = sum(n)) |>
  ungroup() |>
  filter(
    general_health %in% c("Bad health", "Very bad health"),
    sexual_orientation != "Does not apply"
  ) |>
  mutate(percentage = 100 * n / population) |>
  group_by(area_name, sexual_orientation) |>
  summarise(percentage = sum(percentage)) |>
  ungroup()

## Summary data ----
plot_data$sexual_orientation <- str_wrap(plot_data$sexual_orientation, 12)
summary_data <- plot_data |>
  group_by(sexual_orientation) |>
  summarise(med_perc = median(percentage)) |>
  arrange(med_perc)
plot_data$sexual_orientation <- factor(plot_data$sexual_orientation,
                                       levels = summary_data$sexual_orientation
)
summary_data$sexual_orientation <- factor(summary_data$sexual_orientation,
                                          levels = summary_data$sexual_orientation
)


# Variables ---------------------------------------------------------------

highlight_col <- "#ff6b00"
bg_col <- "white"


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = percentage, y = 0)
) +
  geom_quasirandom(
    colour = highlight_col,
    size = 0.7
  ) +
  geom_segment(
    data = summary_data,
    mapping = aes(x = med_perc, y = -0.4, yend = 0.4),
    colour = "black",
    linewidth = 1
  ) +
  facet_wrap(vars(sexual_orientation), ncol = 1, strip.position = "left") +
  labs(
    title = "People identifying as an *other sexual orientation* most likely to report bad or very bad health",
    subtitle = "Percentage of population in each local authority reporting bad or very bad health by sexual orientation",
    caption = "**Source**: General health by sexual orientation from Office for National Statistics<br>**Graphic**: Nicola Rennie",
    x = "Percentage", y = NULL
  ) +
  scale_x_continuous(limits = c(0, 25), expand = expansion(0, 0)) +
  scale_y_continuous(expand = expansion(0.05, 0.05), breaks = 0) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_textbox_simple(margin = margin(b = 5)),
    plot.caption = element_textbox_simple(margin = margin(t = 5)),
    axis.title.x = element_text(hjust = 1, size = rel(0.9)),
    axis.text.y = element_blank(),
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(10, 15, 10, 10)
  ) +
  canvas(
    width = 5, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

if (interactive()) {
  save_ggplot(
    plot = p,
    file = "contributions/nicola-rennie-beeswarm/beeswarm.png"
  )
}

