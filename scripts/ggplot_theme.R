library(pacman)

p_load(char = c("tidyverse", 
                "ggtext"))



my_theme <- theme_classic() + 
  theme(
    plot.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "gray93"),
    panel.grid.major = element_line(color = "gray93", size = 0.2),
    strip.background = element_blank(),
    # element textbox is from ggtext
    strip.text = ggtext::element_textbox(
      size = 11, face = "bold",
      color = "black", fill = "gray88", halign = 0.5, 
      r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    )
  )

theme_set(my_theme)


theme_lexis <-
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0, size = 12, face = "bold"),
    plot.background = element_rect(fill = "#ebebeb"),
    panel.background = element_rect(fill = "#ebebeb"),
    plot.subtitle = element_text(size = 10),
    legend.title = element_text(size = 11),
    axis.text.x = element_text(angle = 60, size = 10, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold")
  )
