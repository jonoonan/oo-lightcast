# Date: 2025-05-08
# Author: Sam Mason

# Instructions: A ggplot2 skeleton including theme. Use for plots. Only works
# when setup.R has been sourced to environment.

ggplot() +
  labs(tag = ir_logo) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray90",
                                        linewidth = 0.25),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "gray90",
                                  linewidth = 0.25),
        strip.background = element_rect(fill = "gray90"),
        legend.position = "bottom",
        legend.key.size = unit(0.8, "lines"),
        plot.title.position = "plot",
        plot.tag = element_path(size = 2.8,
                                vjust = 0, hjust = 0,
                                alpha = 0.5),
        plot.tag.location = "plot",
        plot.tag.position = c(0, 0),
        text = element_text(family = "jost",
                            color = "gray30"),
        strip.text = element_text(face = "bold",
                                  color = "gray30"),
        plot.title = element_text(family = "neuton",
                                  size = 16),
        plot.subtitle = element_text(face = "italic",
                                     size = 10,
                                     color = "gray60"),
        plot.caption = element_text(face = "italic",
                                    size = 7,
                                    color = "gray60"))