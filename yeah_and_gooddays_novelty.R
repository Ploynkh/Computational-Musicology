library(tidyverse)

novelty <- read_delim(
  "yeah_novelty.csv",
  delim = ",",
  col_names = FALSE,
  show_col_types = FALSE
) %>%
  rename(
    TIME = X1,
    VALUE = X2
  )

yeah_novelty <- ggplot(novelty, aes(x = TIME, y = VALUE)) +
  geom_segment(aes(xend = TIME, yend = 0),
               linewidth = 0.3,
               color = "#00D5D5") +
  labs(
    title = "Yeah! — Novelty Function",
    subtitle = "First 30 seconds",
    x = "Time (s)",
    y = "Novelty"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "#0D1B2A", color = NA),
    panel.background = element_rect(fill = "#0D1B2A"),
    panel.grid = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "#C9D1D9"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(color = "#F4A261", face = "bold"),
    plot.subtitle = element_text(color = "white")
  )

# ----------  Gooddays novelty -----------

library(tidyverse)

novelty <- read_delim(
  "gooddays_novelty.csv",
  delim = ",",
  col_names = FALSE,
  show_col_types = FALSE
) %>%
  rename(
    TIME = X1,
    VALUE = X2
  )

gooddays_novelty <- ggplot(novelty, aes(x = TIME, y = VALUE)) +
  geom_segment(aes(xend = TIME, yend = 0),
               linewidth = 0.3,
               color = "#00D5D5") +
  labs(
    title = "Good days — Novelty Function",
    subtitle = "First 30 seconds",
    x = "Time (s)",
    y = "Novelty"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "#0D1B2A", color = NA),
    panel.background = element_rect(fill = "#0D1B2A"),
    panel.grid = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "#C9D1D9"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(color = "#F4A261", face = "bold"),
    plot.subtitle = element_text(color = "white")
  )

plot(yeah_novelty)
# plot(gooddays_novelty)
