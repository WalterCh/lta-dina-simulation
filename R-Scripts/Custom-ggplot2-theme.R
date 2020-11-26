# custom ggplot theme
require(extrafont, quietly = TRUE)
require(ggplot2, quietly = TRUE)

loadfonts()
custom_theme <- theme(
  axis.text = element_text(size = 15),
  axis.title = element_text(size = 16),
  legend.text = element_text(size = 15),
  legend.title = element_text(size = 15),
  legend.position = "none",
  panel.background = element_blank(),
  panel.border = element_rect(size = 1, fill = NA),
  plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
  text = element_text(family = "Poppins")
)