library(tidyverse)

bg <- "#0E0A1A"

# ── Load data ─────────────────────────────────────────────────────────────────
yeah_novelty <- read_csv("yeah_novelty.csv",
                         col_names = c("time", "novelty"),
                         show_col_types = FALSE)

gooddays_novelty <- read_csv("gooddays_novelty.csv",
                             col_names = c("time", "novelty"),
                             show_col_types = FALSE)

# ── Helper function ───────────────────────────────────────────────────────────
make_novelty <- function(data, track_title, era_label, accent_colour) {
  ggplot(data, aes(x = time, y = novelty)) +
    geom_area(fill = accent_colour, alpha = 0.6) +
    geom_line(colour = accent_colour, linewidth = 0.4) +
    coord_cartesian(xlim = c(0, 30)) +        # ← add this
    labs(
      title    = paste0(track_title, " — Novelty Function"),
      subtitle = paste0("First 30 seconds | ", era_label),
      x        = "Time (s)",
      y        = "Novelty"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, color = "white"),
      plot.subtitle    = element_text(size = 10, color = "grey60"),
      axis.title.x     = element_text(face = "bold", size = 10, color = "white",
                                      margin = margin(t = 6)),
      axis.title.y     = element_text(face = "bold", size = 10, color = "white"),
      axis.text        = element_text(color = "grey70"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#2E2E32", linewidth = 0.4),
      panel.background = element_rect(fill = bg, colour = NA),
      plot.background  = element_rect(fill = bg, colour = NA),
      plot.margin      = margin(12, 12, 12, 12)
    )
}
# ── Generate plots ────────────────────────────────────────────────────────────
plot_yeah     <- make_novelty(yeah_novelty,     "Yeah!",     "1990s R&B", "#FF2D78")
plot_gooddays <- make_novelty(gooddays_novelty, "Good Days", "2020s R&B", "#C77DFF")

plot(plot_yeah)
plot(plot_gooddays)

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("images/yeah_novelty.png",
       plot = plot_yeah,     width = 10, height = 4, dpi = 300, bg = bg)

ggsave("images/gooddays_novelty.png",
       plot = plot_gooddays, width = 10, height = 4, dpi = 300, bg = bg)