plot2 <- dfCMJ %>%
  mutate(
    t = time_s,
    rF = force_right,
    lF = force_left,
    cF = force_combined,
    v = velocity_m_s,
    d = displacement_m,
    p = power_w
  ) %>%
  ggplot(aes(x = t)) +
  geom_line(aes(y = cF, color = "Combined"), linewidth = 1) +
  geom_line(aes(y = lF, color = "Left"), linewidth = 1) +
  geom_line(aes(y = rF, color = "Right"), linewidth = 1) +
  #geom_rect(xmax = 1.85, xmin = 1.75, ymax = Inf, ymin = -Inf, fill = "#e6d80030", color = "#00000000") +
  #annotate(geom = "rect", xmax = brk, xmin = unw, ymax = Inf, ymin = -Inf, fill = "#e6d80030", color = "#00000000") +
  #annotate(geom = "rect", xmax = prp, xmin = brk, ymax = Inf, ymin = -Inf, fill = "#dc0ab430", color = "#00000000") +
  #annotate(geom = "rect", xmax = to, xmin = prp, ymax = Inf, ymin = -Inf, fill = "#00bfa030", color = "#00000000") +
  #labs(title = "",
       #x = "Time (s)",
       #y = "Force (N)",
       #color = "",
       #fill = "") +
  #scale_fill_manual(values = c("Combined" = "#0bb4ff")) +  # Set the fill color for the area plot
  scale_color_manual(values = c("Left" = "#c1232b", "Right" = "#ffa300", "Combined" = "#05b702")) +
  guides(fill = guide_legend(override.aes = list(color = NA))) +  # Remove legend symbol for area plot
  theme_void() +# Set line colors
  theme(
    # Panel
    panel.background = element_rect(fill = "#00000000"),
    # Plot 
    plot.background = element_rect(fill = "#00000000"),
    # Legend
    legend.background = element_rect(fill = "#00000000"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(color = "#c2c2c2"),
    legend.title = element_text(color = "#c2c2c2", face = "bold"),
    legend.key = element_rect(fill = "#00000000")
  ) +
  guides(color = guide_legend(override.aes = list(linetype = "solid")))  # Change legend symbols to dots

plot2
p <- plot2 + transition_reveal(t)

p



guides(fill = guide_legend(override.aes = list(color = NA))) +  # Remove legend symbol for area plot
  theme(
    # Panel
    panel.background = element_rect(fill = "#00000000"),
    panel.grid = element_line(color = "#c2c2c2"),
    # Plot 
    plot.background = element_rect(fill = "#1f1f1f"),
    plot.title = element_text(color = "#c2c2c2", face = "bold"),
    # Axis
    axis.title = element_text(color = "#c2c2c2", face = "bold"),
    axis.text = element_text(color = "#c2c2c2"),
    # Legend
    legend.background = element_rect(fill = "#00000000"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(color = "#c2c2c2"),
    legend.title = element_text(color = "#c2c2c2", face = "bold"),
    legend.key = element_rect(fill = "#00000000")
  ) +
  guides(color = guide_legend(override.aes = list(linetype = "solid")))  # Change legend symbols to dots
