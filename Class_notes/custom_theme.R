theme_set(theme_bw())
bw_update <- theme_bw() +
theme(panel.grid.major = element_line(colour = "grey70", size = 0.2, linetype = 2),
      panel.grid.minor = element_line(colour = NA),
      legend.position = "right",
      legend.direction = "vertical",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.key = element_rect(colour = "black"),
      legend.key.height = unit(.50, "cm"),
      #legend.key.width = unit(0.30, "cm"),
      legend.background = element_blank(),
      plot.title = element_text(size = 10, hjust = 0),
      strip.background = element_rect(colour = "white", fill = "white"),
      strip.text = element_text(size = 10),
      text = element_text(family = "Palatino"))
theme_set(bw_update) # Set theme

theme_set(theme_grey())
grey_update <- theme_grey() +
  theme(panel.grid.major = element_line(size = 0.2, linetype = 2),
        plot.title = element_text(size = 10, hjust = 0),
        # panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
        panel.grid.minor = element_line(colour = NA),
        strip.background = element_rect(colour = NA, fill = "antiquewhite2", size = 0.4),
        text = element_text(family = "Palatino"))
theme_set(grey_update) # Set theme
