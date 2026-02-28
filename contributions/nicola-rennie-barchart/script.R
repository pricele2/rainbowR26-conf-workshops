library(tidyverse)
library(ggview)
library(ggtext)
library(scales)
library(glue)


# Load data ---------------------------------------------------------------

genmod_age_scot <- read_csv("data/genmod_age_scot.csv")


# Data wrangling ----------------------------------------------------------

plot_data <- genmod_age_scot |>
  filter(!str_starts(age, "All people"), response != "Total") |>
  mutate(age = factor(age, levels = c(
    "16 - 24", "25 - 34", "35 - 49", "50 - 64", "65 and over"
  )))

summary_data <- genmod_age_scot |>
  filter(str_starts(age, "All people"))

tot_percent <- round(100 * (summary_data |>
                              filter(response == "Yes: Trans or has a trans history") |>
                              pull(n)) / (summary_data |>
                                            filter(response == "Total") |>
                                            pull(n)), 2)


# Variables ---------------------------------------------------------------

highlight_col <- "#880659"
bg_col <- "white"


# Plot --------------------------------------------------------------------

ggplot(
  data = plot_data,
  mapping = aes(x = age, y = n, fill = str_wrap(response, 30))
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("grey80", "grey50", highlight_col)) +
  labs(
    title = glue("{tot_percent}% of Scotland's population identifies as trans"),
    subtitle = "Percentage of each age group identifying as trans or having a trans history",
    caption = "**Source**: Trans status or history by age (Scotland's Census 2022) from National Records of Scotland<br>**Graphic**: Nicola Rennie",
    x = NULL, y = NULL
  ) +
  coord_cartesian(expand = FALSE) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(vjust = 1),
    legend.key.spacing.x = unit(1, "cm"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_textbox_simple(margin = margin(b = 5)),
    plot.caption = element_textbox_simple(margin = margin(t = 10)),
    plot.margin = margin(10, 15, 10, 15)
  ) +
  canvas(
    width = 7, height = 5,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p


# Save --------------------------------------------------------------------

save_ggplot(
  plot = p,
  file = "contributions/nicola-rennie-barchart/barchart.png"
)
