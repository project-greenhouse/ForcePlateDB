#----------------------------------------#
# 1. CMJ -----
#----------------------------------------#

## DF CMJ Phase Analysis -----
# convert force-time data frame to CMJ specific data frame
df_CMJ <- function(dfFT) {
  
  # df given through parameter
  df <- dfFT %>%
    transmute(
      t = time_s,
      Fl = force_left,
      Fr = force_right,
      Fc = force_combined,
      v = velocity_m_s,
      d = displacement_m,
      p = power_w
    )
  
  #-----#
  
  # Create time points
  # System Weight
  sw <- mean(df$Fc[1:1000])
  
  #-----#
  
  # Time at start of movement
  sm <- df %>%
    filter(cumsum(d != 0) > 0) %>% # trim rows before displacement starts%>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # Time at takeoff
  to <- df %>%
    filter(Fc < 10) %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # min velocity
  mv <- df %>%
    filter(t < to-100) %>%
    filter(v == min(v))%>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # landing
  lnd <- df %>%
    filter(t > to & Fc > 10) %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  # Create new data frame for FD
  df <- df %>%
    transmute(
      phase = case_when(
        t >= sm & t < to-0.2 & Fc < sw  ~ "unw",
        Fc > sw & v < 0 & t < to & t > mv ~ "brk",
        t < to & v > 0  ~ "prp",
        t < sm ~ "quiet",
        t >= lnd ~ "lnd"
      )
    ) %>%
    mutate(
      "rtime" = round((t - min(t)) / (max(t) - min(t) ) * 100,2),
      "rsw" = Fc/sw,
      "peakP" = ifelse(percent_rank(p) > 0.99,percent_rank(p), 0) 
    )
  
  return(df)
}

#----------#

## CMJ F-T All Plot -----
plot_CMJ_ft <- function(CMJdf) {
  # df passed through parameter
  df <- CMJdf
  
  #-----#
  
  # time at start of Unw phase
  unw <- df %>%
    filter( phase == "unw") %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # time at start of brk phase
  brk <- df %>%
    filter( phase == "brk") %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # time at start of prp phase
  prp <- df %>%
    filter( phase == "prp") %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # top of max force to set border of plot
  maxF <- ceiling(max(df$Fc)/1000)*1000
  
  #-----#
  
  # Max of time to set border of plot
  maxT <- max(df$t)
  
  #-----#
  
  ### F-T echart plot -----
  eplot <- df |>
    e_chart(t, name = "Time (s)") |>
    e_bar(Fc, name = "Combined", bind = phase) |> 
    e_line(Fl, name = "Left") |>
    e_line(Fr, name = "Right") |>
    e_mark_line(title = "Unw", data = list(xAxis = unw), itemStyle = list(color = "white")) |>
    e_mark_line(title = "Brk", data = list(xAxis = brk)) |>
    e_mark_line(title = "Prp", data = list(xAxis = prp)) |>
    # custom colors
    e_color(c("#0bb4ff", "#9b19f5", "#ffa300")) |>
    # Customize the x-axis and y-axis limits
    e_x_axis(
      min = 0,  # Minimum x-axis limit
      max = maxT, # Maximum x-axis limit
      name = "Time (s)",  # Customize the x-axis label
      nameLocation = "center",
      nameGap = "25"
    ) |>
    e_y_axis(
      min = 0,  # Minimum y-axis limit
      max = maxF,   # Maximum y-axis limit
      name = "Force (N)", # Customize the y-axis label
      nameLocation = "center",
      nameGap = "45"
    ) |>
    e_title(text = "Force - Time", left = "center") |>
    # Apply a theme (if needed)
    e_theme("chalk") |>
    e_legend(bottom = TRUE) |>
    # Customize the tooltip
    e_tooltip(
      axisPointer = list(
        type = "cross"
      )
    ) |>
    e_toolbox_feature(feature = "dataZoom")
  
  return(eplot)
}

## CMJ F-T Phases Plot -----
plot_CMJ_phases <- function(CMJdf) {
  
  # df passed through parameter
  df <- CMJdf %>%
    select(t, Fc, phase) %>%
    filter(phase != "base") %>%
    pivot_wider(names_from = "phase", values_from = "Fc")
  
  #----------#
  
  # CMJ-FT echart Phase Plot 
  ePlotPhase <- df |>
    e_chart(rtime, name = "Time (s)") |>
    e_bar(quiet, name = "Qt", color = "#6339b070") |>
    e_bar(unw, name = "Unw", color = "#e6d80070") |>
    e_bar(brk, name = "Brk", color = "#e6004970") |>
    e_bar(prp, name = "Prp", color = "#00bfa070") |>
    e_bar(lnd, name = "Lnd", color = "#ff980170") |>
    # Customize the x-axis and y-axis limits
    e_x_axis(
      name = "Time (%)",  # Customize the x-axis label
      nameLocation = "center",
      nameGap = "25"
    ) |>
    e_y_axis(
      name = "Force (N)", # Customize the y-axis label
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
  
  #----------#
  
  return(ePlotPhase)
  
}

## CMJ F-D Plot -----
plot_CMJ_fd <- function(CMJdf) {
  
  # df passed through parameter
  df <- CMJdf 
  
  #-----#
  
  # Time at start of movement
  sm <- df %>%
    filter(cumsum(d != 0) > 0) %>% # trim rows before displacement starts%>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # Time at takeoff
  to <- df %>%
    filter(Fc < 20) %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # transform df
  df <- df %>%
    filter(t > sm & t < to)
  
  #-----#
  
  # F-D echart plot 
  ePlotFD <- df |>
    group_by(phase) |>
    e_charts(d) |>
    e_scatter(rsw) |>
    # Customize the x-axis and y-axis limits
    e_x_axis(
      name = "Displacement (m)",  # Customize the x-axis label
      nameLocation = "center",
      nameGap = "35"
    ) |>
    e_y_axis(
      name = "Relative Body Weight", # Customize the y-axis label
      nameLocation = "center",
      nameRotate = "90",
      nameGap = "35"
    ) |>
    e_theme("chalk") |>
    e_title(text = "Work Curve", subtext = "Force-Displacement", left = "center") |>
    e_legend(bottom = TRUE) |>
    # Customize the tooltip
    e_tooltip(
      axisPointer = list(type = "cross")
    ) |>
    e_toolbox_feature(feature = "dataZoom")
  
  return(ePlotFD)
  
}

## CMJ F-V Plot -----
plot_CMJ_fv <- function(CMJdf) {
  
  # df passed through parameter
  df <- CMJdf 
  
  #-----#
  
  # Time at start of movement
  sm <- df %>%
    filter(cumsum(d != 0) > 0) %>% # trim rows before displacement starts
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # Time at takeoff
  to <- df %>%
    filter(Fc < 20) %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # transform df
  df <- df %>%
    filter(t > sm & t < to) %>%
    pivot_wider(
      id_cols = c("v", "peakP"),
      names_from = "phase",
      values_from = "rsw"
    )
  
  #-----#
  
  # Peak Power
  RFatPP <- df %>%
    filter(p == max(p)) %>%
    slice_min(order_by = v, n = 1) %>%
    pull("rbw") %>%
    as.numeric()
  
  #-----#
  
  # Velocity at Peak Power
  VatPP <- df %>%
    filter(p == max(p)) %>%
    slice_min(order_by = v, n = 1) %>%
    pull("v") %>%
    as.numeric()
  
  #-----#
  
  # F-V echart plot 
  ePlotFV <- dfFV |>
    e_charts(v) |>
    e_scatter(brk) |>
    e_effect_scatter(prp, peakP) |>
    e_scatter(unw) |>
    e_mark_point(data = list(xAxis = VatPP, yAxis = RFatPP, value = "PP"))|>
    # Customize the x-axis and y-axis limits
    e_x_axis(
      name = "Velocity (m/s)" , # Customize the x-axis label
      nameLocation = "center",
      nameGap = "25"
    ) |>
    e_y_axis(
      name = "Relative Body Weight", # Customize the y-axis label
      nameLocation = "center",
      nameRotate = "90",
      nameGap = "35"
    ) |>
    e_theme("chalk") |>
    e_title(text = "Power Curve", subtext = "Force-Velocity", left = "center") |>
    e_legend(bottom = TRUE) |>
    # Customize the tooltip
    e_tooltip(
      trigger = "axis"
    ) |>
    e_toolbox_feature(feature = "dataZoom")
  
  return(ePlotFV)
  
}

#----------------------------------------#
# 2. SJ -----
#----------------------------------------#

## DF SJ Phase Analysis -----
# convert force-time data frame to SJ specific data frame
df_SJ <- function(dfFT) {
  
  # df given through parameter
  df <- dfFT %>%
    transmute(
      t = time_s,
      Fl = force_left,
      Fr = force_right,
      Fc = force_combined,
      v = velocity_m_s,
      d = displacement_m,
      p = power_w,
      phase = "prp"
    )
  
  #-----#
  
  # Create time points
  # System Weight
  sw <- mean(df$Fc[1:1000])
  
  #-----#
  
  # Time at start of movement
  sm <- df %>%
    filter(cumsum(d != 0) > 0) %>% # trim rows before displacement starts%>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # Time at takeoff
  to <- df %>%
    filter(Fc < 10) %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # Create new data frame for FD
  df <- df %>%
    filter(t > sm & t < to ) %>% # trim rows before displacement starts
    mutate(
      "rtime" = round((t - min(t)) / (max(t) - min(t) ) * 100,2),
      "rsw" = Fc/sw,
      "peakP" = ifelse(percent_rank(p) > 0.99,percent_rank(p), 0) 
    )
  
  return(df)
}

#----------#

## SJ F-T Plot -----
plot_SJ_ft <- function(SJdf) {
  # df passed through parameter
  df <- SJdf
  
  #-----#
  # time at start of prp phase
  prp <- dfFT %>%
    filter( phase == "prp") %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # top of max force to set border of plot
  maxF <- ceiling(max(df$Fc)/1000)*1000
  
  #-----#
  
  # Max of time to set border of plot
  maxT <- max(df$t)
  
  #-----#
  
  ### F-T echart plot -----
  eplot <- df |>
    e_chart(t, name = "Time (s)") |>
    e_bar(Fc, name = "Combined", bind = phase) |> 
    e_line(Fl, name = "Left") |>
    e_line(Fr, name = "Right") |>
    e_mark_line(title = "Prp", data = list(xAxis = prp), itemStyle = list(color = "white")) |>
    # custom colors
    e_color(c("#0bb4ff", "#9b19f5", "#ffa300")) |>
    # Customize the x-axis and y-axis limits
    e_x_axis(
      min = 0,  # Minimum x-axis limit
      max = maxT, # Maximum x-axis limit
      name = "Time (s)",  # Customize the x-axis label
      nameLocation = "center",
      nameGap = "25"
    ) |>
    e_y_axis(
      min = 0,  # Minimum y-axis limit
      max = maxF,   # Maximum y-axis limit
      name = "Force (N)", # Customize the y-axis label
      nameLocation = "center",
      nameGap = "45"
    ) |>
    e_title(text = "Force - Time", left = "center") |>
    # Apply a theme (if needed)
    e_theme("chalk") |>
    e_legend(bottom = TRUE) |>
    # Customize the tooltip
    e_tooltip(
      axisPointer = list(
        type = "cross"
      )
    ) |>
    e_toolbox_feature(feature = "dataZoom")
  
  return(eplot)
}

#----------------------------------------#
# 3. CMJ-R -----
#----------------------------------------#

## DF CMJ-R Phase Analysis -----
# convert force-time data frame to CMJ-rebound specific data frame
df_CMJR <- function(dfFT) {
  
  # df given through parameter
  df <- dfFT %>%
    transmute(
      t = time_s,
      Fl = force_left,
      Fr = force_right,
      Fc = force_combined,
      v = velocity_m_s,
      d = displacement_m,
      p = power_w
    )
  
  #-----#
  
  # Create time points
  # System Weight
  sw <- mean(df$Fc[1:1000])
  
  #-----#
  
  # Time at start of movement
  sm <- df %>%
    filter(cumsum(d != 0) > 0) %>% # trim rows before displacement starts%>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # Time at takeoff 1
  to1 <- df %>%
    filter(force_combined < 10) %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # Time at Landing 1
  lnd1 <- df %>%
    filter(Fc > 10 & t > to) %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # Time at takeoff 2
  to2 <- df %>%
    filter(Fc < 10 & t > lnd) %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # Time at Landing 2
  lnd2 <- df %>%
    filter(Fc > 10 & t > to2) %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # Create new data frame for FD
  df <- df %>%
    mutate(
      phase = case_when(
        t < sm  ~ "quiet",
        t >= sm & t < to1-0.2 & Fc < sw  ~ "unw",
        Fc > sw & v < 0 & t < to1 ~ "brk1",
        t < to1 & v > 0  ~ "prp1",
        t > to1 & t < lnd ~ "flt1",
        t > lnd & v < 0 & Fc > sw ~ "brk2",
        t > lnd & v > 0 ~ "prp2",
        t > to2 & t < lnd2 ~ "flt2",
        t > lnd2 ~ "lnd"
      )
    ) %>%
    filter(t > sm & t < to2 ) %>% # trim rows before displacement starts
    mutate(
      "rtime" = round((t - min(t)) / (max(t) - min(t) ) * 100,2),
      "rsw" = Fc/sw,
      "peakP" = ifelse(percent_rank(p) > 0.99,percent_rank(p), 0) 
    )
  
  return(df)
}

#----------#

## CMJ F-T Plot -----
plot_CMJr_ft <- function(CMJrdf) {
  # df passed through parameter
  df <- CMJrdf
  
  #-----#
  
  # time at start of Unw phase
  unw <- df %>%
    filter( phase == "unw") %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # time at start of brk phase
  brk1 <- df %>%
    filter( phase == "brk1") %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # time at start of prp phase
  prp1 <- df %>%
    filter( phase == "prp1") %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # time at start of brk phase
  brk2 <- df %>%
    filter( phase == "brk2") %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # time at start of prp phase
  prp2 <- df %>%
    filter( phase == "prp2") %>%
    slice_min(order_by = t, n = 1) %>%
    pull("t")
  
  #-----#
  
  # top of max force to set border of plot
  maxF <- ceiling(max(df$Fc)/1000)*1000
  
  #-----#
  
  # Max of time to set border of plot
  maxT <- max(df$t)
  
  #-----#
  
  ### F-T echart plot -----
  eplot <- df |>
    e_chart(t, name = "Time (s)") |>
    e_bar(Fc, name = "Combined", bind = phase) |> 
    e_line(Fl, name = "Left") |>
    e_line(Fr, name = "Right") |>
    # Unw Phase Line
    e_mark_line(title = "Unw", data = list(xAxis = unw), itemStyle = list(color = "white")) |>
    # Brk1 Phase Line
    e_mark_line(title = "Brk1", data = list(xAxis = brk1)) |>
    # Prp1 Phase Line
    e_mark_line(title = "Prp1", data = list(xAxis = prp1)) |>
    # Brk2 Phase Line
    e_mark_line(title = "Brk2", data = list(xAxis = brk2)) |>
    # Prp2 Phase Line
    e_mark_line(title = "Prp2", data = list(xAxis = prp2)) |>
    # custom colors
    e_color(c("#0bb4ff", "#9b19f5", "#ffa300")) |>
    # Customize the x-axis and y-axis limits
    e_x_axis(
      min = 0,  # Minimum x-axis limit
      max = maxT, # Maximum x-axis limit
      name = "Time (s)",  # Customize the x-axis label
      nameLocation = "center",
      nameGap = "25"
    ) |>
    e_y_axis(
      min = 0,  # Minimum y-axis limit
      max = maxF,   # Maximum y-axis limit
      name = "Force (N)", # Customize the y-axis label
      nameLocation = "center",
      nameGap = "45"
    ) |>
    e_title(text = "Force - Time", left = "center") |>
    # Apply a theme (if needed)
    e_theme("chalk") |>
    e_legend(bottom = TRUE) |>
    # Customize the tooltip
    e_tooltip(
      axisPointer = list(
        type = "cross"
      )
    ) |>
    e_toolbox_feature(feature = "dataZoom")
  
  return(eplot)
}

## CMJ F-T Phases Plot -----
plot_CMJr_phases <- function(CMJdf) {
  
  # df passed through parameter
  df <- CMJdf %>%
    select(t, Fc, phase) %>%
    pivot_wider(names_from = "phase", values_from = "Fc")
  
  #----------#
  
  # CMJr-FT echart Phase Plot 
  ePlotPhase <- df |>
    e_chart(rtime, name = "Time (s)") |>
    e_bar(quiet, name = "Qt", color = "#6339b070") |>
    e_bar(unw, name = "Unw", color = "#e6d80070") |>
    e_bar(brk1, name = "Brk1", color = "#e6004970") |>
    e_bar(prp1, name = "Prp1", color = "#00bfa070") |>
    e_bar(brk2, name = "Brk2", color = "#e6004970") |>
    e_bar(prp2, name = "Prp2", color = "#00bfa070") |>
    e_bar(lnd, name = "Lnd", color = "#ff980170") |>
    # Customize the x-axis and y-axis limits
    e_x_axis(
      name = "Time (%)",  # Customize the x-axis label
      nameLocation = "center",
      nameGap = "25"
    ) |>
    e_y_axis(
      name = "Force (N)", # Customize the y-axis label
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
  
  #----------#
  
  return(ePlotPhase)
  
}

