library(tidyverse)

# Load datasets
rb90s <- read_csv("90s.csv", show_col_types = FALSE)
rbcontemporary <- read_csv("contemporary.csv", show_col_types = FALSE)

# Histogram for 90s R&B
plot_90s <- ggplot(rb90s, aes(x = Tempo)) +
  geom_histogram(
    binwidth = 10,
    fill = "#E76F51",
    color = "white"
  ) +
  labs(
    title = "Tempo Distribution (1990s R&B)",
    x = "Tempo (BPM)",
    y = "Number of Songs"
  ) +
  xlim(50,180) +
  theme_classic()

print(plot_90s)


# Histogram for Contemporary R&B
plot_contemporary <- ggplot(rbcontemporary, aes(x = Tempo)) +
  geom_histogram(
    binwidth = 10,
    fill = "#2A9D8F",
    color = "white"
  ) +
  labs(
    title = "Tempo Distribution (Contemporary R&B)",
    x = "Tempo (BPM)",
    y = "Number of Songs"
  ) +
  xlim(50,180) +
  theme_classic()

print(plot_contemporary)