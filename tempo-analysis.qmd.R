---
  title: "Tempo Analysis"
format: 
  dashboard:
  orientation: rows
theme: vapor
---
  
  ## Overview
  
  This page examines the tempo characteristics of R&B tracks from two eras: the 1990s and contemporary R&B.  
Tempo is measured in beats per minute (BPM) and provides insight into the rhythmic pace of a song.

By comparing tempo distributions across eras, we can observe whether stylistic shifts in R&B have occurred over time.

---
  
  ## Tempo Distribution – 1990s R&B
  
  ```{r}
ggplot(rb90s, aes(x = Tempo)) +
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