# Packages
library(dygraphs)
library(xts)
library(tidyverse)
library(xts)
library(dygraphs)
library(corrplot)

#styler::style_file("Basic Analysis.R")
#attachment::att_from_rscript("Basic Analysis.R")

# ts2225 values to time graphs --------------------------------------------


precip <- xts(ts2225$`Precipitation (mm)`, order.by = ts2225$TIMESTAMP)
dygraph(precip,
        main = "Precipitation",
        ylab = "mm",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y", valueRange = c(floor(min(precip, na.rm = TRUE)), ceiling(max(precip, na.rm = TRUE)))) %>%
  dyRangeSelector()

tair2 <- xts(ts2225$`Tair@2m (deg C)`, order.by = ts2225$TIMESTAMP)
dygraph(tair2,
        main = "Air Temperature @2 m",
        ylab = "°C",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y", valueRange = c(floor(min(tair2, na.rm = TRUE)), ceiling(max(tair2, na.rm = TRUE)))) %>%
  dyRangeSelector()

tair42 <- xts(ts2225$`Tair@42m (deg C)`, order.by = ts2225$TIMESTAMP)
dygraph(tair42,
        main = "Air Temperature @42 m",
        ylab = "°C",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y", valueRange = c(floor(min(tair42, na.rm = TRUE)), ceiling(max(tair42, na.rm = TRUE)))) %>%
  dyRangeSelector()

rh2 <- xts(ts2225$`Rel. humidity@2m (%)`, order.by = ts2225$TIMESTAMP)
dygraph(rh2,
        main = "Relative Humidity @2 m",
        ylab = "%",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y", valueRange = c(0, 100)) %>%
  dyRangeSelector()

rh42 <- xts(ts2225$`Rel. humidity@42m (%)`, order.by = ts2225$TIMESTAMP)
dygraph(rh42,
        main = "Relative Humidity @42 m",
        ylab = "%",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y", valueRange = c(0, 100)) %>%
  dyRangeSelector()

soilT5 <- xts(ts2225$`Soil T@5 cm (deg C)`, order.by = ts2225$TIMESTAMP)
dygraph(soilT5,
        main = "Soil Temperature @5 cm",
        ylab = "°C",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y", valueRange = c(floor(min(soilT5, na.rm = TRUE)), ceiling(max(soilT5, na.rm = TRUE)))) %>%
  dyRangeSelector()

soilT50 <- xts(ts2225$`Soil T@50 cm (deg C)`, order.by = ts2225$TIMESTAMP)
dygraph(soilT50,
        main = "Soil Temperature @50 cm",
        ylab = "°C",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y", valueRange = c(floor(min(soilT50, na.rm = TRUE)), ceiling(max(soilT50, na.rm = TRUE)))) %>%
  dyRangeSelector()

swc5 <- xts(ts2225$`Soil Water Content@5 cm (m3/m3)`, order.by = ts2225$TIMESTAMP)
dygraph(swc5,
        main = "Soil Water Content @5 cm",
        ylab = "m³/m³",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()

swc50 <- xts(ts2225$`Soil Water Content@50 cm (m3/m3)`, order.by = ts2225$TIMESTAMP)
p_swc50 <- dygraph(swc50,
                   main = "Soil Water Content @50 cm",
                   ylab = "m³/m³",
                   xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()
p_swc50

wSind <- xts(ts2225$`Wind speed (m/s)`, order.by = ts2225$TIMESTAMP)
dygraph(wind,
        main = "Wind Speed",
        ylab = "m/s",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()

vpd <- xts(ts2225$`Vapor pressure deficit VPD (kPa)`, order.by = ts2225$TIMESTAMP)
dygraph(vpd,
        main = "Vapor Pressure Deficit",
        ylab = "kPa",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()

swin <- xts(ts2225$`SW_in (W/m^2)`, order.by = ts2225$TIMESTAMP)
dygraph(swin,
        main = "Incoming Shortwave Radiation",
        ylab = "W/m²",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()

swout <- xts(ts2225$`SW_out (W/m^2)`, order.by = ts2225$TIMESTAMP)
dygraph(swout,
        main = "Outgoing Shortwave Radiation",
        ylab = "W/m²",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()

lwin <- xts(ts2225$`LW_in (W/m^2)`, order.by = ts2225$TIMESTAMP)
dygraph(lwin,
        main = "Incoming Longwave Radiation",
        ylab = "W/m²",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()

lwout <- xts(ts2225$`LW_out (W/m^2)`, order.by = ts2225$TIMESTAMP)
dygraph(lwout,
        main = "Outgoing Longwave Radiation",
        ylab = "W/m²",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()

le <- xts(ts2225$`Latent Energy LE (W/m^2)`, order.by = ts2225$TIMESTAMP)
dygraph(le,
        main = "Latent Energy",
        ylab = "W/m²",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()

h <- xts(ts2225$`Sensible Heat H (W/m^2)`, order.by = ts2225$TIMESTAMP)
dygraph(h,
        main = "Sensible Heat",
        ylab = "W/m²",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()

co2 <- xts(ts2225$`CO2 concentration (ppm)`, order.by = ts2225$TIMESTAMP)
dygraph(co2,
        main = "CO<sub>2</sub> concentration",
        ylab = "ppm",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()

gpp <- xts(ts2225$`Gross Primary Productivity  (g C m^-2 (30min)^-1)`,
           order.by = ts2225$TIMESTAMP)
dygraph(gpp,
        main = "Gross Primary Productivity",
        ylab = "g C m⁻² (30 min)⁻¹",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()

nee <- xts(ts2225$`Net Ecosystem Exchange NEE (g C m^-2 (30min)^-1)`,
           order.by = ts2225$TIMESTAMP)
dygraph(nee,
        main = "Net Ecosystem Exchange",
        ylab = "g C m⁻² (30 min)⁻¹",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()

re <- xts(ts2225$`Ecosystem Respiration (RE)  (g C m^-2 (30min)^-1)`,
          order.by = ts2225$TIMESTAMP)
dygraph(re,
        main = "Ecosystem Respiration",
        ylab = "g C m⁻² (30 min)⁻¹",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()


# combined gpp, nee, r
carbon_fluxes <- xts(
  cbind(
    GPP = ts2225$`Gross Primary Productivity  (g C m^-2 (30min)^-1)`,
    NEE = ts2225$`Net Ecosystem Exchange NEE (g C m^-2 (30min)^-1)`,
    RE  = ts2225$`Ecosystem Respiration (RE)  (g C m^-2 (30min)^-1)`
  ),
  order.by = ts2225$TIMESTAMP
)

dygraph(carbon_fluxes, main = "Carbon Fluxes") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y", label = expression(g ~ C ~ m^{
    -2
  } ~ "(30 min)"^{
    -1
  })) %>%
  dySeries("GPP", label = "GPP") %>%
  dySeries("NEE", label = "NEE") %>%
  dySeries("RE", label = "RE") %>%
  dyRangeSelector()

# combined: swc5cm, p
swc_precip <- xts(
  cbind(
    SWC_5cm = ts2225$`Soil Water Content@5 cm (m3/m3)`,
    Precipitation = ts2225$`Precipitation (mm)`
  ),
  order.by = ts2225$TIMESTAMP
)

dygraph(swc_precip, main = "Soil Water Content @5 cm and Precipitation") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dySeries("SWC_5cm", label = "SWC @5 cm", axis = "y") %>%
  dySeries("Precipitation", label = "Precipitation", axis = "y2") %>%
  dyAxis("y", label = "Soil Water Content (m³/m³)") %>%
  dyAxis("y2", label = "Precipitation (mm)", independentTicks = TRUE) %>%
  dyRangeSelector()


# Flag values LE, H, NEE -------------------------------------------------------------
flag_LE <- xts(ts2225$flag_LE, order.by = ts2225$TIMESTAMP)

dygraph(flag_LE,
        main = "LE Flag",
        ylab = "Flag",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 1.2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()


flag_H <- xts(ts2225$flag_H, order.by = ts2225$TIMESTAMP)

dygraph(flag_H,
        main = "H Flag",
        ylab = "Flag",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 1.2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()


flag_NEE <- xts(ts2225$flag_NEE, order.by = ts2225$TIMESTAMP)

dygraph(flag_NEE,
        main = "NEE Flag",
        ylab = "Flag",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 1.2,
    axisLabelFontSize = 14
  ) %>%
  dyAxis("y") %>%
  dyRangeSelector()


# Calculate gap fractions (=#NAs/length) and construct histograms  --------

# Calculate gap fractions (=#NAs/length) and construct histograms of gaps (length of gap versus frequency) for all variables. Use the gappositions function from «help_alltasks.R». Ignore NAs at the very beginning (not all sensors in place as of 1.1.2022) and at the very end (processing not finished)


## Prep  -------------------------------------------------------------------


## calculate gap fractions
## construct histograms of gaps
head(ts2225)
names(ts2225)



# create a list containing one dataframe per variable
ts_datasets <- lapply(
  names(ts2225)[-1],
  # exclude TIMESTAMP
  function(x) {
    ts2225 %>%
      select(TIMESTAMP, all_of(x))
  }
)

# give each dataframe the variable name
names(ts_datasets) <- names(ts2225)[-1]
names(ts_datasets)
head(ts_datasets)

# Values 0 am Anfang und Ende der Zeitreihen entfernen
trim_zero_edges <- function(df) {
  values <- df[, 2]
  
  # erste Position mit einem Wert != 0
  first_valid <- which(values != 0 & !is.na(values))[1]
  
  # letzte Position mit einem Wert != 0
  last_valid <- tail(which(values != 0 & !is.na(values)), 1)
  
  # falls alles 0 oder NA ist
  if (is.na(first_valid) || length(last_valid) == 0) {
    return(df[0, ])
  }
  
  # Datensatz zuschneiden
  df[first_valid:last_valid, ]
}


# auf alle Variablen anwenden
ts_datasets_trimmed <- lapply(ts_datasets, trim_zero_edges)

# Namen behalten
names(ts_datasets_trimmed) <- names(ts_datasets)
head(ts_datasets_trimmed)


# gap fraction calculation
gap_fraction <- sapply(ts_datasets_trimmed, function(df) {
  sum(is.na(df[, 2])) / nrow(df)
})

gap_fraction <- data.frame(variable = names(gap_fraction),
                           gap_fraction = as.numeric(gap_fraction) * 100)
gap_fraction


## Example Hist ------------------------------------------------------------


# prep hist: choose only NAs here
gap_lengths <- lapply(ts_datasets_trimmed, function(df) {
  rle_na <- rle(is.na(df[, 2]))
  
  # only keep NA runs
  rle_na$lengths[rle_na$values]
})

gap_lengths[["CO2 concentration (ppm)"]]

# hist
hist(
  gap_lengths[["CO2 concentration (ppm)"]],
  breaks = 30,
  main = "Gap length distribution - CO2",
  xlab = "Gap length (number of observations)",
  ylab = "Frequency"
)


## Get gap dates -----------------------------------------------------------


# apply start and end date for gaps
get_gap_dates <- function(df) {
  na_rle <- rle(is.na(df[, 2]))
  
  # positions where NA gaps start
  gap_starts <- cumsum(c(1, head(na_rle$lengths, -1)))[na_rle$values]
  
  # lengths of NA gaps
  gap_lengths <- na_rle$lengths[na_rle$values]
  
  # calculate end positions
  gap_ends <- gap_starts + gap_lengths - 1
  
  # dataframe with dates
  data.frame(
    start_date = df$TIMESTAMP[gap_starts],
    end_date = df$TIMESTAMP[gap_ends],
    gap_length = gap_lengths
  )
}

gap_dates <- lapply(ts_datasets_trimmed, get_gap_dates)

names(gap_dates) <- names(ts_datasets_trimmed)

gap_dates[["CO2 concentration (ppm)"]]


## hist for temporal position of the data ---------------------------------


# only keep datasets with actual gaps
gap_dates_filtered <- gap_dates[sapply(gap_dates, nrow) > 0]


# number of plots
n_plots <- length(gap_dates_filtered)

# set plotting layout
par(mfrow = c(3, 3), mar = c(5, 4, 3, 1)) # improve spacing


for (i in names(gap_dates_filtered)) {
  gaps <- gap_dates_filtered[[i]]
  
  plot(
    gaps$start_date,
    gaps$gap_length,
    type = "h",
    main = i,
    xlab = "Date",
    ylab = "Gap length\n(30 min intervals)",
    xaxt = "n"
  )
  
  # add date axis
  axis(1,
       at = pretty(gaps$start_date),
       labels = format(pretty(gaps$start_date), "%Y-%m"))
}


## hist for frequency vs length --------------------------------------------
gap_dates

sum(sapply(gap_dates, nrow) > 0)

# only datasets with gaps
gap_dates_filtered <- gap_dates[sapply(gap_dates, nrow) > 0]


par(mfrow = c(3, 3), mar = c(5, 4, 3, 1))


counter <- 0

for (i in names(gap_dates_filtered)) {
  # new page every 9 plots
  if (counter %% 9 == 0 && counter > 0) {
    readline("Press Enter for next page")
    par(mfrow = c(3, 3), mar = c(5, 4, 3, 1))
  }
  
  gaps <- gap_dates_filtered[[i]]$gap_length
  
  hist(
    gaps,
    breaks = 30,
    main = i,
    xlab = "Gap length (30 min intervals)",
    ylab = "Frequency"
  )
  
  counter <- counter + 1
}


# gappositions = function(ts)  #ts has to be a data frame with two columns, where the first column is time, the second the values
# { gp=list()
# rle_na <- rle(is.na(ts_datasets$[,2]))
# na_starts <- which(rle_na$values & rle_na$lengths > 0)
# gp$na_lengths <- rle_na$lengths[na_starts]
# gp$na_start_positions <- cumsum(c(1, rle_na$lengths))[-length(rle_na$lengths)][na_starts]
# gp$na_times = ts[gp$na_start_positions,1]
# return(gp)
# }
#
# gappositions <- function(ts) {
#
#   gp <- list()
#
#   rle_na <- rle(is.na(ts[,2]))
#
#   na_starts <- which(rle_na$values & rle_na$lengths > 0)
#
#   gp$na_lengths <- rle_na$lengths[na_starts]
#
#   gp$na_start_positions <- cumsum(c(1, rle_na$lengths))[-length(rle_na$lengths)][na_starts]
#
#   gp$na_times <- ts[gp$na_start_positions, 1, drop = TRUE]
#
#   return(gp)
# }
#
#
#
# gappositions(
#   ts_datasets_trimmed[["Ecosystem Respiration (RE)  (g C m^-2 (30min)^-1)"]]
# )


## Precip error search -----------------------------------------------------

library(xts)
library(dygraphs)

# original precipitation series
precip <- xts(ts2225$`Precipitation (mm)`, order.by = ts2225$TIMESTAMP)

# create NA indicator series for missing values
precip_na <- xts(ifelse(is.na(ts2225$`Precipitation (mm)`), 0, NA), order.by = ts2225$TIMESTAMP)

# combine series
precip_plot <- merge(Precipitation = precip, Missing = precip_na)

dygraph(precip_plot,
        main = "Precipitation with missing data",
        ylab = "mm",
        xlab = "") %>%
  dyOptions(
    axisLineWidth = 1.5,
    strokeWidth = 2,
    axisLabelFontSize = 14
  ) %>%
  dySeries("Precipitation", color = "forestgreen") %>%
  dySeries(
    "Missing",
    color = "red",
    strokeWidth = 0,
    drawPoints = TRUE,
    pointSize = 3
  ) %>%
  dyAxis("y", valueRange = c(floor(min(precip, na.rm = TRUE)), ceiling(max(precip, na.rm = TRUE)))) %>%
  dyRangeSelector()
# Add missing values as red points
points(x[na_idx],
       rep(0, sum(na_idx)),
       col = "red",
       pch = 16,
       cex = 0.8)


# Determine the longest gapfree stretch for all variables --------
gap_dates_filtered


library(lubridate)

# define all timestamps
time_seq <- ts2225$TIMESTAMP

# identify rows with no missing values in any variable
complete_rows <- complete.cases(ts2225)

# create runs of continuous complete data
gap_free <- data.frame(time = time_seq, complete = complete_rows) %>%
  mutate(block = cumsum(!complete)) %>%
  filter(complete) %>%
  group_by(block) %>%
  summarise(
    start = min(time),
    end = max(time),
    n_points = n(),
    duration_hours = as.numeric(difftime(end, start, units = "hours")),
    .groups = "drop"
  ) %>%
  arrange(desc(duration_hours))

# longest gap-free period
gap_free[1, ]


# ACF on all variables for the whole dataset & longest strech --------

# I chose the highest 3 gap_free periods
gap_dates_filtered

# extract the three longest gap-free periods
gap_free_top3 <- gap_free %>%
  slice(1:3)

# create three separate datasets
ts2225_no_na_1 <- ts2225 %>%
  filter(TIMESTAMP >= gap_free_top3$start[1],
         TIMESTAMP <= gap_free_top3$end[1])

ts2225_no_na_2 <- ts2225 %>%
  filter(TIMESTAMP >= gap_free_top3$start[2],
         TIMESTAMP <= gap_free_top3$end[2])

ts2225_no_na_3 <- ts2225 %>%
  filter(TIMESTAMP >= gap_free_top3$start[3],
         TIMESTAMP <= gap_free_top3$end[3])

# range(ts2225_no_na_1$TIMESTAMP)
# range(ts2225_no_na_2$TIMESTAMP)
# range(ts2225_no_na_3$TIMESTAMP)




# acf uses 336 = ONE WEEK of DATAPOINTS


# exclude timestamp column
vars <- names(ts2225_no_na_1)[names(ts2225_no_na_1) != "TIMESTAMP"]

# function for ACF calculation
calc_acf <- function(data, variable, lag_max = 48) {
  acf_result <- acf(data[[variable]], lag.max = lag_max, plot = FALSE)
  
  tibble(
    variable = variable,
    lag = as.numeric(acf_result$lag),
    autocorrelation = as.numeric(acf_result$acf)
  )
}

# calculate ACF
acf_no_na_1 <- map_dfr(vars, ~ calc_acf(ts2225_no_na_1, .x))

acf_no_na_1
# acf_no_na_1 is the values that show autocorrelation here

# whole dataset acf() analysis


# all variables except timestamp
vars <- names(ts2225)[names(ts2225) != "TIMESTAMP"]

# function for ACF calculation with NA removal
calc_acf <- function(data, variable, lag_max = 48) {
  acf_result <- acf(na.omit(data[[variable]]), lag.max = lag_max, plot = FALSE)
  
  tibble(
    variable = variable,
    lag = as.numeric(acf_result$lag),
    autocorrelation = as.numeric(acf_result$acf)
  )
}

# calculate ACF for full dataset
acf_ts2225 <- map_dfr(vars, ~ calc_acf(ts2225, .x))

acf_comparison <- bind_rows(
  acf_no_na_1 %>% mutate(dataset = "gap_free"),
  acf_ts2225 %>% mutate(dataset = "full_dataset")
)


ggplot(acf_comparison, aes(lag, autocorrelation, colour = dataset)) +
  geom_line() +
  facet_wrap( ~ variable, scales = "free_y") +
  theme_bw()

#
#
#
# acf_no_na_1 %>%
#   filter(variable == "Tair@2m (deg C)") %>%
#   ggplot(aes(lag, autocorrelation)) +
#   geom_line(linewidth = 0.8) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   scale_x_continuous(
#     breaks = c(1,48,336),
#     labels = c("30 min","1 day","1 week")
#   ) +
#   labs(
#     title = "ACF: Tair@2m (ts2225_no_na_1)",
#     x = "Lag",
#     y = "Autocorrelation"
#   ) +
#   theme_bw()


# Determine the longest gapfree strech for each variable independe --------
library(lubridate)

# function to determine longest gap-free period per variable
find_longest_gapfree <- function(data, variable) {
  df <- data %>%
    select(TIMESTAMP, all_of(variable)) %>%
    filter(!is.na(.data[[variable]])) %>%
    arrange(TIMESTAMP) %>%
    mutate(time_diff = as.numeric(difftime(TIMESTAMP, lag(TIMESTAMP), units = "mins")),
           block = cumsum(if_else(is.na(time_diff) |
                                    time_diff > 30, 1, 0)))
  
  blocks <- df %>%
    group_by(block) %>%
    summarise(
      start = first(TIMESTAMP),
      end = last(TIMESTAMP),
      n_points = n(),
      duration_hours = as.numeric(difftime(end, start, units = "hours")),
      .groups = "drop"
    ) %>%
    arrange(desc(duration_hours))
  
  blocks %>%
    slice(1) %>%
    mutate(variable = variable) %>%
    select(variable, everything())
}


# variables excluding timestamp
vars <- names(ts2225)[names(ts2225) != "TIMESTAMP"]


# calculate longest gap-free stretch for every variable
longest_gapfree_all <- map_dfr(vars, ~ find_longest_gapfree(ts2225, .x))

longest_gapfree_all


# Variables excluding timestamp
vars <- names(ts2225)[names(ts2225) != "TIMESTAMP"]

# Function to calculate ACF
calc_acf <- function(data, variable, lag_max = 48) {
  acf_result <- acf(data[[variable]], lag.max = lag_max, plot = FALSE)
  
  tibble(
    variable = variable,
    lag = as.numeric(acf_result$lag),
    autocorrelation = as.numeric(acf_result$acf)
  )
}

# Full dataset (remove NAs)


calc_acf_full <- function(data, variable, lag_max = 48) {
  acf_result <- acf(na.omit(data[[variable]]), lag.max = lag_max, plot = FALSE)
  
  tibble(
    variable = variable,
    lag = as.numeric(acf_result$lag),
    autocorrelation = as.numeric(acf_result$acf)
  )
}

acf_full <- map_dfr(vars, ~ calc_acf_full(ts2225, .x)) %>%
  mutate(dataset = "Full dataset")


# Top 3 gap-free stretches

gap_free_top3 <- gap_free %>%
  slice(1:3)

acf_gapfree <- map_dfr(seq_len(nrow(gap_free_top3)), function(i) {
  subset_data <- ts2225 %>%
    filter(TIMESTAMP >= gap_free_top3$start[i],
           TIMESTAMP <= gap_free_top3$end[i])
  
  map_dfr(vars, ~ calc_acf(subset_data, .x)) %>%
    mutate(dataset = paste0("Gap-free ", i))
})


# Combine


acf_comparison <- bind_rows(acf_full, acf_gapfree)


# Plot

ggplot(acf_comparison, aes(lag, autocorrelation, colour = dataset)) +
  geom_line(linewidth = 0.7) +
  facet_wrap( ~ variable, scales = "free_y") +
  theme_bw() +
  labs(x = "Lag (30 min intervals)", y = "Autocorrelation", colour = "Dataset")


# acf of variables difference ---------------------------------------------

acf_difference <- acf_comparison %>%
  filter(dataset %in% c("Gap-free 1", "Full dataset")) %>%
  select(variable, lag, dataset, autocorrelation) %>%
  pivot_wider(names_from = dataset, values_from = autocorrelation) %>%
  mutate(difference = `Full dataset` - `Gap-free 1`)

acf_difference

ggplot(acf_difference, aes(lag, difference)) +
  geom_hline(yintercept = 0,
             colour = "grey40",
             linetype = 2) +
  geom_line(colour = "#0072B2", linewidth = 0.8) +
  facet_wrap( ~ variable, scales = "free_y") +
  labs(x = "Lag (30 min intervals)",
       y = expression(ACF[full] - ACF[gap - free]),
       title = "Difference in autocorrelation between the full dataset and the longest gap-free period") +
  theme_bw()

# difference of specific variables at lag of 1 day
acf_diff_48 <- acf_comparison %>%
  filter(lag == 48) %>%
  select(variable, dataset, autocorrelation) %>%
  tidyr::pivot_wider(names_from = dataset, values_from = autocorrelation) %>%
  mutate(difference = `Full dataset` - `Gap-free 1`) %>%
  arrange(difference)

acf_diff_48

ggplot(acf_diff_48, aes(x = difference, y = fct_reorder(variable, difference))) +
  geom_vline(xintercept = 0,
             linetype = 2,
             colour = "grey60") +
  geom_point(size = 3, colour = "darkgreen") +
  labs(x = "Difference in ACF at lag 48\n(Full dataset − Gap-free 1)", y = NULL, title = "Influence of gaps on autocorrelation") +
  theme_bw()


# Replace «short» sequences of NAs with interpolated values (Hint: --------
# : zoo/na.approx


# Scatter Plots between variables -----------------------------------------
# Use Bayeian tools -> correlationPlot for this
library(BayesianTools)

head(ts2225)

ts2225_no_flags <- ts2225 %>%
  select(-flag_LE, -flag_H, -flag_NEE)


corr <- ts2225_no_flags %>%
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs")

{
  plot.new()
  dev.off()
  }

library(corrplot)

corrplot(
  corr,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.cex = 0.5,
  # variable names
  number.cex = 0.5,
  # correlation numbers (if displayed)
  cl.cex = 0.8,
  # color legend text
  tl.col = "black",
  tl.srt = 45
)



# with pearson correlation factors
corr_p <- ts2225_no_flags %>%
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")


corrplot(
  corr_p,
  method = "color",
  type = "upper",
  order = "hclust",
  addCoef.col = "black",
  # show coefficients
  number.cex = 0.5,
  # size of coefficients
  tl.cex = 0.5,
  # variable names
  tl.col = "black",
  tl.srt = 45
)

# scatterplot
library(ggpubr)
# VPD and GPP
ts2225 %>%
  filter(
    !is.na(`Vapor pressure deficit VPD (kPa)`),
    !is.na(`Gross Primary Productivity  (g C m^-2 (30min)^-1)`)
  ) %>%
  ggplot(
    aes(x = `Vapor pressure deficit VPD (kPa)`, y = `Gross Primary Productivity  (g C m^-2 (30min)^-1)`)
  ) +
  stat_cor(
    method = "pearson",
    cor.coef.name = "r",
    label.x.npc = "left",
    label.y.npc = "top"
  ) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Vapor pressure deficit (kPa)", y = "Gross Primary Productivity (g C m⁻² 30 min⁻¹)", title = "Relationship between VPD and GPP") +
  theme_bw()

# T and GPP

ts2225 %>%
  filter(!is.na(`Tair@2m (deg C)`),
         !is.na(`Gross Primary Productivity  (g C m^-2 (30min)^-1)`)) %>%
  ggplot(aes(x = `Tair@2m (deg C)`, y = `Gross Primary Productivity  (g C m^-2 (30min)^-1)`)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(
    method = "pearson",
    cor.coef.name = "r",
    label.x.npc = "left",
    label.y.npc = "top"
  ) +
  labs(x = "Air temperature at 2 m (°C)", y = "Gross Primary Productivity (g C m⁻² 30 min⁻¹)", title = "Relationship between air temperature and GPP") +
  theme_bw(base_size = 14)

# 19°C is the gpp production optimum

# Try: non-linear model fit
ts2225 %>%
  filter(!is.na(`Tair@2m (deg C)`),
         !is.na(`Gross Primary Productivity  (g C m^-2 (30min)^-1)`)) %>%
  ggplot(aes(x = `Tair@2m (deg C)`, y = `Gross Primary Productivity  (g C m^-2 (30min)^-1)`)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "gam",
              formula = y ~ s(x),
              se = TRUE) +
  labs(x = "Air temperature at 2 m (°C)", y = "Gross Primary Productivity (g C m⁻² 30 min⁻¹)", title = "Non-linear relationship between air temperature and GPP") +
  theme_bw(base_size = 14)

# somewhere around 22°C

# co2 concentration and rehum at 2 m
ts2225 %>%
  filter(!is.na(`Rel. humidity@2m (%)`),
         !is.na(`CO2 concentration (ppm)`)) %>%
  ggplot(aes(x = `Rel. humidity@2m (%)`, y = `CO2 concentration (ppm)`)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(
    method = "pearson",
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    label.x.npc = "left",
    label.y.npc = "top"
  ) +
  stat_regline_equation(aes(label = paste(..rr.label..)),
                        label.x.npc = "left",
                        label.y.npc = 0.85) +
  labs(x = "Relative humidity at 2 m (%)", y = "CO₂ concentration (ppm)", title = "CO₂ concentration vs relative humidity") +
  theme_bw(base_size = 14)

# Which ones are clearly non-linear
# precipiation and soil moisture
ts2225 %>%
  filter(!is.na(`Precipitation (mm)`),
         !is.na(`Soil Water Content@5 cm (m3/m3)`)) %>%
  ggplot(aes(x = `Precipitation (mm)`, y = `Soil Water Content@5 cm (m3/m3)`)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(
    method = "pearson",
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    label.x.npc = "left",
    label.y.npc = "top"
  ) +
  stat_regline_equation(aes(label = paste(..rr.label..)),
                        label.x.npc = "left",
                        label.y.npc = 0.85) +
  labs(x = "Precipitation (mm)", y = "Soil water content at 5 cm (m³ m⁻³)", title = "Precipitation vs soil moisture (5 cm)") +
  theme_bw(base_size = 14)


# Calculate Autocorrelation, correlation length and memory ----------------

# MEMORY: In time series analysis, memory describes how much the current value of a time series depends on its past values. In other words, it quantifies temporal dependence or persistence in the data.
# Calculate autocorrelation and decide on correlation length and memory

head(ts2225_no_flags)



gpp <- ts2225_no_flags %>%
  pull(`Gross Primary Productivity  (g C m^-2 (30min)^-1)`)

gpp <- na.omit(gpp)

# MA -> q -> acf

# 1 week
acf_gpp <- acf(gpp,
               lag.max = 48,
               plot = TRUE,
               na.action = na.pass)

# 1 year
acf_gpp <- acf(gpp,
               lag.max = 3 * 17520,
               plot = TRUE,
               na.action = na.pass)

# AR -> p -> pacf
pacf_gpp <- pacf(gpp,
                 lag.max = 3 * 17520,
                 plot = TRUE,
                 na.action = na.pass)

# Memory
acf_values <- acf_gpp$acf[-1]
lags <- acf_gpp$lag[-1]

threshold <- 1 / exp(1)

memory_lag <- lags[which(acf_values < threshold)[1]]

memory_time_hours <- memory_lag * 0.5

memory_time_hours

# GPP has a correlation length of approximately 6 hours.


## Memory for all variables ------------------------------------------------





numeric_data <- ts2225_no_flags %>%
  select(where(is.numeric))


memory_results <- map_dfr(names(numeric_data), function(variable) {
  x <- na.omit(numeric_data[[variable]])
  
  # skip variables with too few observations
  if (length(x) < 100) {
    return(tibble(
      variable = variable,
      correlation_lag = NA,
      memory_hours = NA
    ))
  }
  
  acf_result <- acf(x, lag.max = 17520, plot = FALSE)
  
  acf_values <- acf_result$acf[-1]
  lags <- acf_result$lag[-1]
  
  threshold <- 1 / exp(1)
  
  decay <- which(acf_values < threshold)[1]
  
  tibble(
    variable = variable,
    correlation_lag = ifelse(length(decay) == 0, NA, lags[decay]),
    memory_hours = ifelse(length(decay) == 0, NA, lags[decay] * 0.5)
  )
})

print(memory_results, n = 21)


## add Hurst to short term on top---------------------------------------------------------------


library(pracma)

hurst_results <- map_dfr(names(numeric_data), function(variable) {
  x <- na.omit(numeric_data[[variable]])
  
  if (length(x) < 100) {
    return(tibble(variable = variable, H = NA))
  }
  
  H <- hurstexp(x)$Hs
  
  tibble(variable = variable, H = H)
})

memory_summary <- left_join(memory_results, hurst_results, by = "variable")

print(memory_summary, n = 21)

# Hurst from course -------------------------------------------------------
decompose(ts2225$`Gross Primary Productivity  (g C m^-2 (30min)^-1)`)
detrend(ts2225$`Gross Primary Productivity  (g C m^-2 (30min)^-1)`)

library(TSAutils)
library(forecast)
library(fracdiff)

daily_gpp <- ts2225 %>%
  mutate(date = as.Date(TIMESTAMP)) %>%
  group_by(date) %>%
  summarise(GPP = mean(`Gross Primary Productivity  (g C m^-2 (30min)^-1)`, na.rm = TRUE))

gpp_daily <- ts(daily_gpp$GPP, frequency = 365)

gpp_resid <- deseason(gpp_daily, 365)

gpp_resid <- na.omit(gpp_resid)

fd <- fracdiff(gpp_resid)

H <- fd$d + 0.5

H


# pdf all variables -------------------------------------------------------

library(nortest)
library(e1071)

# select only numeric time series
numeric_ts <- ts2225_no_flags %>%
  select(where(is.numeric))

gaussian_test <- map_dfr(names(numeric_ts), function(var) {
  x <- na.omit(numeric_ts[[var]])
  
  # skip variables with too few values
  if (length(x) < 50) {
    return(NULL)
  }
  
  ad_test <- ad.test(x)
  
  tibble(
    variable = var,
    n = length(x),
    mean = mean(x),
    sd = sd(x),
    skewness = skewness(x),
    kurtosis = kurtosis(x),
    Anderson_Darling = ad_test$statistic,
    p_value = ad_test$p.value,
    Gaussian = ifelse(
      ad_test$p.value > 0.05,
      "approximately Gaussian",
      "non-Gaussian"
    )
  )
})

print(gaussian_test, n = 21)



plot_pdf <- function(data, variable) {
  x <- na.omit(data[[variable]])
  
  df <- tibble(value = x)
  
  ggplot(df, aes(value)) +
    geom_density(linewidth = 1) +
    stat_function(
      fun = dnorm,
      args = list(mean = mean(x), sd = sd(x)),
      linetype = "dashed",
      linewidth = 1
    ) +
    labs(
      title = paste("PDF:", variable),
      subtitle = "Solid = observed density, dashed = Gaussian distribution",
      x = variable,
      y = "Density"
    )
}

names(ts2225_no_flags)

plot_pdf(ts2225_no_flags,
         "Gross Primary Productivity  (g C m^-2 (30min)^-1)")


# Mann Kendall ----------------------------------------------------

# Trend analysis: use the Mann Kendall test to determine p values and sign of trends. Detrend the series

# set values to monthly
library(lubridate)

# put values to monthly
ts2225_monthly <- ts2225_no_flags %>%
  mutate(year = year(TIMESTAMP), month = month(TIMESTAMP)) %>%
  group_by(year, month) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups =
              "drop")

library(Kendall)

mk_results <- map_dfr(names(select(ts2225_monthly, where(is.numeric))), function(variable) {
  x <- ts2225_monthly[[variable]]
  
  x <- na.omit(x)
  
  test <- MannKendall(x)
  
  tibble(
    variable = variable,
    tau = test$tau,
    p_value = test$sl,
    trend =
      case_when(
        test$sl < 0.05 & test$tau > 0 ~ "increasing",
        test$sl < 0.05 & test$tau < 0 ~ "decreasing",
        TRUE ~ "no significant trend"
      )
  )
})

print(mk_results, n = 21)

mk_results_sig <- mk_results %>%
  filter(p_value < 0.05)

mk_results_sig

### fill NAs ----------------------------------------------------------------


# values without NAs -> biggest block with no nas necessary smaller/equal 48

# 1. Function to fill only short NA gaps (≤48)
library(zoo)


# Function: interpolate only NA gaps <= 48 consecutive observations
fill_short_gaps <- function(x, max_gap = 48) {
  na_runs <- rle(is.na(x))
  
  ends <- cumsum(na_runs$lengths)
  starts <- ends - na_runs$lengths + 1
  
  for (i in which(na_runs$values)) {
    gap_length <- na_runs$lengths[i]
    
    if (gap_length <= max_gap) {
      idx <- starts[i]:ends[i]
      
      x[idx] <- approx(
        x = which(!is.na(x)),
        y = x[!is.na(x)],
        xout = idx,
        rule = 1
      )$y
    }
  }
  
  return(x)
}


# Apply interpolation to all numeric variables
ts2225_no_flags_48filled <- ts2225_no_flags %>%
  mutate(across(where(is.numeric), ~ fill_short_gaps(.x, max_gap = 48)))


# Check remaining NAs after interpolation
na_summary <- tibble(
  variable = names(select(ts2225_no_flags, where(is.numeric))),
  NA_before = sapply(select(ts2225_no_flags, where(is.numeric)), \(x) sum(is.na(x))),
  NA_after = sapply(select(
    ts2225_no_flags_48filled, where(is.numeric)
  ), \(x) sum(is.na(x)))
) %>%
  mutate(filled = NA_before - NA_after)

print(na_summary, n = 21)


### Select longest gap free time series part --------------------------------
longest_complete_segment <- function(x) {
  good <- !is.na(x) & !is.nan(x)
  
  # if there are no valid observations
  if (!any(good)) {
    return(NULL)
  }
  
  r <- rle(good)
  
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  
  valid_runs <- which(r$values)
  
  if (length(valid_runs) == 0) {
    return(NULL)
  }
  
  # longest run of TRUEs
  longest <- valid_runs[which.max(r$lengths[valid_runs])]
  
  idx <- starts[longest]:ends[longest]
  
  list(
    start = starts[longest],
    end = ends[longest],
    length = length(idx),
    values = x[idx],
    index = idx
  )
}




numeric_cols <- names(select(ts2225_no_flags, where(is.numeric)))

segments <- map(numeric_cols, ~ longest_complete_segment(ts2225_no_flags[[.x]]))

names(segments) <- numeric_cols

segment_summary <- tibble(
  variable = numeric_cols,
  start = map_int(segments, ~ if (is.null(.x)) {
    NA_integer_
  } else {
    .x$start
  }),
  end = map_int(segments, ~ if (is.null(.x)) {
    NA_integer_
  } else {
    .x$end
  }),
  n = map_int(segments, ~ if (is.null(.x)) {
    0
  } else {
    .x$length
  })
)

segment_summary

gapfree_ts2225 <- map2(segments, names(segments), function(seg, name) {
  if (is.null(seg)) {
    return(NULL)
  }
  
  ts2225_no_flags[seg$index, c("TIMESTAMP", name)]
})

names(gapfree_ts2225) <- numeric_cols

gapfree_ts2225$`Tair@2m (deg C)`

### Graph: RelHum test ------------------------------------------------------


library(xts)
library(dygraphs)

# original series
rh2 <- xts(ts2225$`Rel. humidity@2m (%)`, order.by = ts2225$TIMESTAMP)

# NA indicator series
rh2_na <- xts(ifelse(is.na(rh2), 100, NA), order.by = index(rh2))

# combine
rh2_plot <- merge(rh2 = rh2, NA_gaps = rh2_na)

dygraph(rh2_plot,
        main = "Relative Humidity @2 m",
        ylab = "%",
        xlab = "") %>%
  dySeries("rh2", color = "#1b9e77", strokeWidth = 2) %>%
  dySeries(
    "NA_gaps",
    color = "#D55E00",
    drawPoints = TRUE,
    pointSize = 4,
    strokeWidth = 0
  ) %>%
  dyOptions(axisLineWidth = 1.5, axisLabelFontSize = 14) %>%
  dyAxis("y", valueRange = c(0, 100)) %>%
  dyRangeSelector()


### detrend the series ------------------------------------------------------


### mstl() package ----------------------------------------------------------
#### ONLY ONE VARIABLE
head(gapfree_ts2225)
data.class(gapfree_ts2225)


# mstl() function of the forecast package
library(forecast)

tair_msts <- msts(gapfree_ts2225$`Tair@2m (deg C)`, seasonal.periods = c(48, 17520))

fit <- mstl(tair_msts)

plot(fit)
#### ALL VARIABLES

library(forecast)

# output folder
dir.create("mstl_plots", showWarnings = FALSE)

# store decompositions and remainders
mstl_decomp <- list()
mstl_remainder <- list()

# loop through each variable
for (col in names(gapfree_ts2225)) {
  message("Processing: ", col)
  
  # extract the dataframe for this variable
  df <- gapfree_ts2225[[col]]
  
  # variable values (second column, first is TIMESTAMP)
  x <- df[[2]]
  
  # skip if there are still missing values
  if (any(is.na(x)) || any(is.nan(x))) {
    message("Skipping: ", col)
    next
  }
  
  # create multi-seasonal time series
  x_msts <- msts(x, seasonal.periods = c(48, 17520))
  
  # decomposition
  fit <- mstl(x_msts)
  
  # save decomposition
  mstl_decomp[[col]] <- fit
  
  # create dataframe with timestamps + remainder
  mstl_remainder[[col]] <- data.frame(TIMESTAMP = df$TIMESTAMP, Remainder = fit[, "Remainder"])
  
  # save decomposition plot
  pdf(
    file = file.path("mstl_plots", paste0(gsub(
      "[^[:alnum:]]", "_", col
    ), ".pdf")),
    width = 8,
    height = 7
  )
  
  plot(fit, main = col)
  
  dev.off()
}

plot_mstl <- function(variable) {
  if (!variable %in% names(mstl_decomp)) {
    stop("Variable not found.")
  }
  
  plot(mstl_decomp[[variable]], main = variable)
}

# you can get each plot using the name and
names(ts2225)
plot_mstl("Gross Primary Productivity  (g C m^-2 (30min)^-1)")

# mstl_decomp and mstl_remainder shows the datasets
mstl_remainder$`Precipitation (mm)`

# check for deseasoning
colnames(mstl_decomp$`Precipitation (mm)`)

# PROBLEM: Some no-gap datasets are too short for a good yearly seasonal analysis.
# # Select only time series with annual seasonal component

annual_seasonal_vars <- names(mstl_decomp[sapply(mstl_decomp, function(x) {
  "Seasonal17520" %in% colnames(x)
})])

annual_seasonal_vars
view(annual_seasonal_vars)

mstl_decomp_annual <- mstl_decomp[annual_seasonal_vars]

# Pull out only the remainer
mstl_remainder_annual <- lapply(mstl_decomp_annual, function(x) {
  x[, "Remainder"]
})

library(pracma)



# Produce hurst variables
hurst_results <- map_dfr(names(mstl_remainder_annual), function(variable) {
  H <- hurstexp(mstl_remainder_annual[[variable]])
  
  tibble(
    variable = variable,
    Hs = H$Hs,
    Hrs = H$Hrs,
    He = H$He,
    Hal = H$Hal
  )
})

# Hurst results daily + yearly --------------------------------------------


hurst_results
# One important observation from your table:
#
#   RE has the highest H (0.867).
#
# That makes ecological sense:
#
#   respiration depends on soil carbon pools
# microbial activity
# temperature history
# moisture conditions
#
# These processes integrate environmental conditions over long periods.
#
# GPP is lower because photosynthesis responds more rapidly to:
#
#   radiation
# temperature
# VPD
# phenology
#
# Your results are therefore biologically plausible

# Hurst for datasets without yearly detrend
# Select time series without annual seasonal component
no_annual_seasonal_vars <- names(mstl_decomp)[!names(mstl_decomp) %in% annual_seasonal_vars]

no_annual_seasonal_vars

# MSTL decompositions without yearly seasonality
mstl_decomp_no_annual <- mstl_decomp[no_annual_seasonal_vars]

mstl_remainder_no_annual <- lapply(mstl_decomp_no_annual, function(x) {
  x[, "Remainder"]
})

names(mstl_remainder_no_annual)

library(pracma)



hurst_results_no_annual <- map_dfr(names(mstl_remainder_no_annual), function(variable) {
  x <- mstl_remainder_no_annual[[variable]]
  
  # remove remaining NAs
  x <- na.omit(as.numeric(x))
  
  H <- hurstexp(x)
  
  tibble(
    variable = variable,
    Hs = H$Hs,
    Hrs = H$Hrs,
    He = H$He,
    Hal = H$Hal
  )
})

hurst_results_no_annual

# Power spectra -----------------------------------------------------------

## detrend + z-adap  -------------------------------------------------------


### detrend -----------------------------------------------------------------


names(gapfree_ts2225)
head(gapfree_ts2225)

library(forecast)

annual_z_ts <- list()

for (var in names(gapfree_ts2225)) {
  cat("Processing:", var, "\n")
  
  # extract numeric vector from list-column
  x <- as.numeric(unlist(gapfree_ts2225[[var]]))
  
  # remove missing values
  x <- x[!is.na(x)]
  
  # create half-hourly multiple seasonal time series
  ts_var <- msts(x, seasonal.periods = c(48, 17520))
  
  # MSTL decomposition
  fit <- mstl(ts_var)
  
  # identify annual seasonal component automatically
  seasonal_cols <- grep("Seasonal", colnames(fit), value = TRUE)
  
  if (length(seasonal_cols) == 2) {
    # second seasonal component = annual cycle
    annual_component <- fit[, seasonal_cols[2]]
  } else if (length(seasonal_cols) == 1) {
    warning(paste("Only one seasonal component found for", var))
    
    # assume it is annual if daily component is missing
    annual_component <- fit[, seasonal_cols[1]]
  } else {
    stop(paste("No seasonal component found for", var))
  }
  
  # remove annual cycle
  detrended <- ts_var - annual_component
  
  # z-transform
  annual_z_ts[[var]] <- as.numeric(scale(detrended))
}

names(annual_z_ts)

# example
head(annual_z_ts[["Tair@2m (deg C)"]])


### separate saving of variables --------------------------------------------

dir.create("annual_z_timeseries", showWarnings = FALSE)

for (var in names(annual_z_ts)) {
  filename <- paste0("annual_z_timeseries/", make.names(var), ".csv")
  
  write.csv(data.frame(value = annual_z_ts[[var]]), filename, row.names = FALSE)
}


## Calculate power spectra -------------------------------------------------

tair_z <- annual_z_ts[["Tair@2m (deg C)"]]

spec_tair <- spectrum(tair_z, log = "no")

plot(
  spec_tair$freq,
  spec_tair$spec,
  type = "l",
  log = "xy",
  xlab = "Frequency",
  ylab = "Power",
  main = "Power spectrum - Tair z-transformed"
)


## Power spectra for every variable ---------------------------------------------
# calculate power spectra for every variable

power_spectra <- list()

for (var in names(annual_z_ts)) {
  cat("Calculating spectrum:", var, "\n")
  
  power_spectra[[var]] <- spectrum(annual_z_ts[[var]], plot = FALSE)
}

names(power_spectra)

# frequency to period
power_spectra[["Tair@2m (deg C)"]]

power_spectra_days <- list()

for (var in names(power_spectra)) {
  power_spectra_days[[var]] <- data.frame(period_days = 1 / power_spectra[[var]]$freq / 48,
                                          power = power_spectra[[var]]$spec)
}
head(power_spectra_days[["Tair@2m (deg C)"]])

par(mfrow = c(3, 3))

for (var in names(power_spectra_days)) {
  plot(
    power_spectra_days[[var]]$period_days,
    power_spectra_days[[var]]$power,
    type = "l",
    log = "xy",
    xlab = "Period (days)",
    ylab = "Power",
    main = var
  )
}


## PSD for all variables automaticall --------------------------------------

power_spectra <- list()

for (var in names(annual_z_ts)) {
  cat("PSD:", var, "\n")
  
  power_spectra[[var]] <- spectrum(annual_z_ts[[var]], log = "no", plot = FALSE)
}

power_spectra[["Tair@2m (deg C)"]]$freq # contains freq
power_spectra[["Tair@2m (deg C)"]]$spec # contains power


# Convert frequency to periods --------------------------------------------

period_days <- 1 / spec_tair$freq / 48
plot(
  period_days,
  spec_tair$spec,
  type = "l",
  log = "xy",
  xlab = "Period (days)",
  ylab = "Power"
)


# power spectra visualization --------------------------------------------------------------------

library(forecast)

annual_z_ts <- list()

for (var in names(gapfree_ts2225)) {
  cat("Processing:", var, "\n")
  
  x <- as.numeric(unlist(gapfree_ts2225[[var]]))
  
  x <- x[!is.na(x)]
  
  ts_var <- msts(x, seasonal.periods = c(48, 17520))
  
  fit <- mstl(ts_var)
  
  seasonal_cols <- grep("Seasonal", colnames(fit), value = TRUE)
  
  annual_component <- fit[, seasonal_cols[length(seasonal_cols)]]
  
  detrended <- ts_var - annual_component
  
  annual_z_ts[[var]] <- as.numeric(scale(detrended))
}

names(annual_z_ts)


dir.create("annual_z_timeseries", showWarnings = FALSE)

for (var in names(annual_z_ts)) {
  write.csv(
    data.frame(value = annual_z_ts[[var]]),
    paste0("annual_z_timeseries/", make.names(var), ".csv"),
    row.names = FALSE
  )
}


power_spectra_days <- list()

for (var in names(annual_z_ts)) {
  cat("Calculating PSD:", var, "\n")
  
  spec <- spectrum(annual_z_ts[[var]], plot = FALSE)
  
  power_spectra_days[[var]] <- data.frame(period_days = 1 / spec$freq / 48,
                                          power = spec$spec)
}

names(power_spectra_days)


dir.create("power_spectra", showWarnings = FALSE)

for (var in names(power_spectra_days)) {
  write.csv(
    power_spectra_days[[var]],
    paste0("power_spectra/", make.names(var), "_PSD.csv"),
    row.names = FALSE
  )
}


for (var in names(power_spectra_days)) {
  plot(
    power_spectra_days[[var]]$period_days,
    power_spectra_days[[var]]$power,
    type = "l",
    log = "xy",
    xlab = "Period (days)",
    ylab = "Power",
    main = var
  )
}


plot(
  power_spectra_days[["Tair@2m (deg C)"]]$period_days,
  power_spectra_days[["Tair@2m (deg C)"]]$power,
  type = "l",
  log = "xy",
  xlab = "Period (days)",
  ylab = "Power",
  main = "PSD - Tair@2m z-transformed"
)
