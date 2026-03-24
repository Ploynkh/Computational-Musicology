library(tidyverse)

bg <- "#0E0A1A"

# ── Load datasets ─────────────────────────────────────────────────────────────
rb90s          <- read_csv("90s.csv",          show_col_types = FALSE)
rbcontemporary <- read_csv("contemporary.csv", show_col_types = FALSE)

# ── Shared dark theme ─────────────────────────────────────────────────────────
theme_dark <- theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = bg, color = NA),
    panel.background = element_rect(fill = bg, color = NA),
    panel.grid.major = element_line(color = "#2E2E32", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    text             = element_text(color = "white"),
    axis.text        = element_text(color = "grey70"),
    axis.title.x     = element_text(face = "bold", size = 11, color = "white",
                                    margin = margin(t = 6)),
    axis.title.y     = element_text(face = "bold", size = 11, color = "white"),
    plot.title       = element_text(face = "bold", size = 13, color = "white"),
    plot.margin      = margin(10, 12, 10, 12)
  )

# ── 1990s tempo histogram ─────────────────────────────────────────────────────
plot_90s <- ggplot(rb90s, aes(x = Tempo)) +
  geom_histogram(binwidth = 10, fill = "#FF2D78", color = bg, alpha = 0.9) +
  labs(
    title = "Tempo Distribution - 1990s R&B",
    x     = "Tempo (BPM)",
    y     = "Number of Songs"
  ) +
  xlim(50, 180) +
  theme_dark

# ── 2020s tempo histogram ─────────────────────────────────────────────────────
plot_contemporary <- ggplot(rbcontemporary, aes(x = Tempo)) +
  geom_histogram(binwidth = 10, fill = "#C77DFF", color = bg, alpha = 0.9) +
  labs(
    title = "Tempo Distribution — 2020s R&B",
    x     = "Tempo (BPM)",
    y     = "Number of Songs"
  ) +
  xlim(50, 180) +
  theme_dark

print(plot_90s)
print(plot_contemporary)

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("images/tempo_dist_90s.png",
       plot = plot_90s,          width = 7, height = 5, dpi = 300, bg = bg)

ggsave("images/tempo_dist_2020s.png",
       plot = plot_contemporary, width = 7, height = 5, dpi = 300, bg = bg)