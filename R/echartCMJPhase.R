tests <- get_tests_ath(athleteId = players$id[players$name == "Lauren Green"])

dfCMJ <- get_forcetime("gOh2j6cIfkzoMiDUxsgW")


# System Weight
bw <- mean(dfCMJ$force_combined[1:1000])

#-----#

# Time at start of movement
sm <- dfCMJ %>%
  filter(cumsum(displacement_m != 0) > 0) %>% # trim rows before displacement starts%>%
  slice_min(order_by = time_s, n = 1) %>%
  pull("time_s")

#-----#

# Time at takeoff
to <- dfCMJ %>%
  filter(force_combined < 20) %>%
  slice_min(order_by = time_s, n = 1) %>%
  pull("time_s")

#-----#

dfFT <- dfCMJ %>%
  transmute(
    t = time_s,
    Fl = force_left,
    Fr = force_right,
    Fc = force_combined,
    v = velocity_m_s,
    d = displacement_m,
    p = power_w,
    phase = case_when(
      t >= sm & t < to-0.2 & Fc < bw  ~ "unw",
      Fc > bw & v < 0 & t > sm & t < to ~ "brk",
      t < to & v > 0 & t > sm ~ "prp",
      t < sm | t > to ~ "base"
    )
  ) %>%
  filter(t < to)

#-----#

# time at start of Unw phase
unw <- dfFT %>%
  filter( phase == "unw") %>%
  slice_min(order_by = t, n = 1) %>%
  pull("t")

#-----#

# time at start of brk phase
brk <- dfFT %>%
  filter( phase == "brk") %>%
  slice_min(order_by = t, n = 1) %>%
  pull("t")

#-----#

# time at start of prp phase
prp <- dfFT %>%
  filter( phase == "prp") %>%
  slice_min(order_by = t, n = 1) %>%
  pull("t")

# top of max force to set border of plot
maxF <- ceiling(max(dfFT$Fc)/1000)*1000

#-----#

# Max of time to set border of plot
maxT <- max(dfPhase$rtime)

### CMJ-FT-Phase DF
dfPhase <- dfFT %>%
  select(t, Fc, phase, d) %>%
  filter(phase != "base") %>%
  mutate("rtime" = round((t - min(t)) / (max(t) - min(t) ) * 100,2)) %>%
  pivot_wider(names_from = "phase", values_from = "Fc")

### CMJ-FT echart Phase Plot -----
ePlotPhase <- dfPhase |>
  e_chart(rtime, name = "Time (s)") |>
  e_bar(unw, name = "Unw", color = "#e6d80070") |>
  e_bar(brk, name = "Brk", color = "#e6004970") |>
  e_bar(prp, name = "Prp", color = "#00bfa070") |>
  e_line(d, name = "Disp", color = "#ff9801", y_index = 1) |>
  # Customize the x-axis and y-axis limits
  e_x_axis(
    name = "Time (%)",  # Customize the x-axis label
    min = 0,  # Minimum x-axis limit
    max = maxT, # Maximum x-axis limit
    nameLocation = "center",
    nameGap = "25"
  ) |>
  e_y_axis(
    name = "Force (N)", # Customize the y-axis label
    min = 0,  # Minimum x-axis limit
    max = maxF, # Maximum x-axis limit
    nameLocation = "center",
    nameRotate = "90",
    nameGap = "40"
  ) |>
  e_y_axis(
    index = 1,
    name = "Displacement (m)", # Customize the y-axis label
    min = -0.5,  # Minimum x-axis limit
    max = 0.2, # Maximum x-axis limit
    nameLocation = "center",
    nameRotate = "90",
    nameGap = "45"
  ) |>
  e_theme("chalk") |>
  e_title(text = "CMJ Phases", subtext = "Force-Time", left = "center") |>
  e_legend(bottom = TRUE) |>
  # Customize the tooltip
  e_tooltip(trigger = "axis") |>
  e_toolbox_feature(feature = "dataZoom")

ePlotPhase

htmlwidgets::saveWidget(widget = ePlotPhase, file = "~/plot.html")
setwd("~")
webshot::webshot(url = "plot.html", 
                 file = "plot.png")