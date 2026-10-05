# ============================================================
# ENV 3387 Environmental Chemistry
# Guest Lecture: Environmental Data at Scale
#
# LIVE DEMO:
# Working with GridMET NetCDF Climate Data in R
#
# Dr. Erich Seamon
# Baylor University
#

# 0. SETUP

rm(list = ls())

library(terra)
library(ncdf4)


# 1. POINT R TO THE GRIDMET FILE

gridmet_dir <-
  "/data/haclab_shared/climate_hub/observed/gridmet/data"

nc_file <-
  file.path(gridmet_dir, "tmmx_2024.nc")


# Does the file exist?
file.exists(nc_file)


# Show the exact file being used
nc_file


# File size in MB
file_size_mb <-
  file.info(nc_file)$size / 1024^2

cat(
  "File size:",
  round(file_size_mb, 1),
  "MB\n"
)


# 2. LOOK INSIDE THE NETCDF FILE

nc <- nc_open(nc_file)


# Print the entire NetCDF description
nc



# 3. WHAT VARIABLES AND DIMENSIONS ARE IN THE FILE?


cat("\nVARIABLES:\n")

names(nc$var)


cat("\nDIMENSIONS:\n")

names(nc$dim)


# Show dimension sizes
for (d in names(nc$dim)) {
  
  cat(
    d,
    "=",
    nc$dim[[d]]$len,
    "\n"
  )
  
}



# 4. INSPECT THE CLIMATE VARIABLE


# Automatically identify the first data variable
climate_variable <- names(nc$var)[1]

cat(
  "\nClimate variable:",
  climate_variable,
  "\n"
)


# Units
units_info <-
  ncatt_get(
    nc,
    climate_variable,
    "units"
  )

units_info


# Long descriptive name
long_name_info <-
  ncatt_get(
    nc,
    climate_variable,
    "long_name"
  )

long_name_info


# Display the information clearly
if (!is.null(units_info$value)) {
  
  cat(
    "\nUnits:",
    units_info$value,
    "\n"
  )
  
}


if (!is.null(long_name_info$value)) {
  
  cat(
    "Description:",
    long_name_info$value,
    "\n"
  )
  
}


# ============================================================
# 5. LOOK AT THE GLOBAL METADATA
# ============================================================

ncatt_get(nc, 0)


# Close the low-level NetCDF connection
nc_close(nc)


# ============================================================
# 6. OPEN THE NETCDF AS A SPATIAL-TEMPORAL RASTER
# ============================================================

tmax <- rast(nc_file)


# Print the SpatRaster
tmax


# ============================================================
# 7. ASK THE DATASET ABOUT ITSELF
# ============================================================

cat("\nROWS:\n")
nrow(tmax)


cat("\nCOLUMNS:\n")
ncol(tmax)


cat("\nNUMBER OF LAYERS:\n")
nlyr(tmax)


cat("\nCELLS PER LAYER:\n")
ncell(tmax)


cat("\nSPATIAL RESOLUTION:\n")
res(tmax)


cat("\nSPATIAL EXTENT:\n")
ext(tmax)


cat("\nCOORDINATE REFERENCE SYSTEM:\n")
crs(tmax)


# ============================================================
# 8. HOW MANY SPACE-TIME VALUES ARE IN THIS FILE?
# ============================================================

total_values <-
  ncell(tmax) * nlyr(tmax)


cat(
  "\nPotential space-time observations:\n"
)


cat(
  format(
    total_values,
    big.mark = ",",
    scientific = FALSE
  ),
  "\n"
)


# Approximate amount of RAM if all values were stored
# as standard 8-byte numeric values at once

approx_gb <-
  total_values * 8 / 1024^3


cat(
  "\nApproximate size as 8-byte numeric values:",
  round(approx_gb, 2),
  "GB\n"
)


# ============================================================
# 9. INSPECT THE TIME DIMENSION
# ============================================================

gridmet_time <- time(tmax)


head(gridmet_time)

tail(gridmet_time)

length(gridmet_time)


# Convert to Date
dates <- as.Date(gridmet_time)


head(dates)

tail(dates)


# ============================================================
# 10. SELECT ONE SUMMER DAY
# ============================================================

target_date <-
  as.Date("2024-07-15")


day_index <-
  which(dates == target_date)


day_index


# Extract that one layer from the data cube
tmax_day <-
  tmax[[day_index]]


tmax_day


# ============================================================
# 11. LOOK AT THE RAW VALUES
# ============================================================

global(
  tmax_day,
  range,
  na.rm = TRUE
)


# ============================================================
# 12. CONVERT KELVIN TO CELSIUS
# ============================================================

# GridMET tmmx is stored as temperature in Kelvin.
#
# Celsius = Kelvin - 273.15

tmax_day_c <-
  tmax_day - 273.15


names(tmax_day_c) <-
  "Maximum_temperature_C"


# Check the new range
global(
  tmax_day_c,
  range,
  na.rm = TRUE
)


# ============================================================
# 13. MAP ONE DAY ACROSS THE UNITED STATES
# ============================================================

plot(
  tmax_day_c,
  main = paste(
    "GridMET Daily Maximum Temperature",
    format(target_date, "%B %d, %Y")
  ),
  axes = TRUE
)


# ============================================================
# 14. ZOOM INTO TEXAS
# ============================================================

# Approximate geographic extent around Texas
texas_extent <-
  ext(
    -107,
    -93,
    25,
    37
  )


texas_tmax <-
  crop(
    tmax_day_c,
    texas_extent
  )


plot(
  texas_tmax,
  main = paste(
    "GridMET Maximum Temperature",
    format(target_date, "%B %d, %Y")
  ),
  axes = TRUE
)


# ============================================================
# 15. CREATE A POINT FOR WACO, TEXAS
# ============================================================

waco_df <-
  data.frame(
    location = "Waco, Texas",
    lon = -97.1467,
    lat = 31.5493
  )


waco_df


waco <-
  vect(
    waco_df,
    geom = c("lon", "lat"),
    crs = "EPSG:4326"
  )


waco


# ============================================================
# 16. ADD WACO TO THE MAP
# ============================================================

plot(
  texas_tmax,
  main = paste(
    "GridMET Maximum Temperature",
    format(target_date, "%B %d, %Y")
  ),
  axes = TRUE
)


points(
  waco,
  pch = 19,
  cex = 1.5
)


text(
  waco,
  labels = "Waco",
  pos = 4,
  cex = 0.9
)


# ============================================================
# 17. EXTRACT JULY 15 TEMPERATURE FOR WACO
# ============================================================

waco_day <-
  extract(
    tmax_day_c,
    waco
  )


waco_day


# Extract the temperature value
waco_day_temp_c <-
  waco_day[1, 2]


cat(
  "\nGridMET maximum temperature in Waco on",
  as.character(target_date),
  "=",
  round(waco_day_temp_c, 1),
  "°C\n"
)


# Convert to Fahrenheit
waco_day_temp_f <-
  waco_day_temp_c * 9 / 5 + 32


cat(
  "Equivalent temperature =",
  round(waco_day_temp_f, 1),
  "°F\n"
)


# ============================================================
# 18. EXTRACT THE ENTIRE YEAR FOR WACO
# ============================================================

waco_extract <-
  extract(
    tmax,
    waco,
    ID = FALSE
  )


# What did we get?
dim(waco_extract)


# First few values
waco_extract[, 1:5]


# ============================================================
# 19. TURN THE EXTRACTION INTO A NORMAL DATA FRAME
# ============================================================

waco_values_K <-
  as.numeric(
    waco_extract[1, ]
  )


waco_daily <-
  data.frame(
    date = dates,
    tmax_K = waco_values_K
  )


# Kelvin to Celsius
waco_daily$tmax_C <-
  waco_daily$tmax_K - 273.15


# Celsius to Fahrenheit
waco_daily$tmax_F <-
  waco_daily$tmax_C * 9 / 5 + 32


head(waco_daily)


summary(waco_daily)


# ============================================================
# 20. PLOT THE WACO TIME SERIES
# ============================================================

plot(
  waco_daily$date,
  waco_daily$tmax_C,
  type = "l",
  lwd = 2,
  xlab = "Date",
  ylab = "Daily maximum temperature (°C)",
  main =
    "2024 GridMET Daily Maximum Temperature — Waco, Texas"
)


grid()


# ============================================================
# 21. ADD AN EXTREME-HEAT THRESHOLD
# ============================================================

# 100°F expressed in Celsius
threshold_100F_C <-
  (100 - 32) * 5 / 9


threshold_100F_C


plot(
  waco_daily$date,
  waco_daily$tmax_C,
  type = "l",
  lwd = 2,
  xlab = "Date",
  ylab = "Daily maximum temperature (°C)",
  main =
    "2024 GridMET Daily Maximum Temperature — Waco, Texas"
)


abline(
  h = threshold_100F_C,
  lty = 2,
  lwd = 2
)


grid()


# ============================================================
# 22. ANSWER SOME SIMPLE ENVIRONMENTAL QUESTIONS
# ============================================================

# Mean daily maximum temperature
mean_tmax <-
  mean(
    waco_daily$tmax_C,
    na.rm = TRUE
  )


cat(
  "\nMean daily maximum temperature:",
  round(mean_tmax, 1),
  "°C\n"
)


# Highest daily maximum temperature
max_tmax <-
  max(
    waco_daily$tmax_C,
    na.rm = TRUE
  )


cat(
  "Highest daily maximum temperature:",
  round(max_tmax, 1),
  "°C\n"
)


# ============================================================
# 23. WHAT WAS THE HOTTEST DAY?
# ============================================================

hottest_day <-
  waco_daily[
    which.max(waco_daily$tmax_C),
  ]


hottest_day


# ============================================================
# 24. HOW MANY DAYS REACHED 100°F?
# ============================================================

days_100F <-
  sum(
    waco_daily$tmax_F >= 100,
    na.rm = TRUE
  )


cat(
  "\nNumber of days with Tmax >= 100°F:",
  days_100F,
  "\n"
)


# ============================================================
# 25. LOOK ONLY AT METEOROLOGICAL SUMMER
# ============================================================

summer <-
  subset(
    waco_daily,
    format(date, "%m") %in%
      c("06", "07", "08")
  )


head(summer)


# Average summer Tmax
summer_mean <-
  mean(
    summer$tmax_C,
    na.rm = TRUE
  )


cat(
  "\nJune-August mean daily maximum temperature:",
  round(summer_mean, 1),
  "°C\n"
)


# Hottest summer day
summer[
  which.max(summer$tmax_C),
]


# ============================================================
# 26. PLOT SUMMER IN FAHRENHEIT
# ============================================================

plot(
  summer$date,
  summer$tmax_F,
  type = "l",
  lwd = 2,
  xlab = "Date",
  ylab = "Daily maximum temperature (°F)",
  main =
    "Summer 2024 GridMET Maximum Temperature — Waco"
)


abline(
  h = 100,
  lty = 2,
  lwd = 2
)


grid()


# ============================================================
# 27. ZOOM IN FAR ENOUGH TO SEE THE GRID
# ============================================================

waco_extent <-
  ext(
    -98.5,
    -96.0,
    30.5,
    32.5
  )


central_texas <-
  crop(
    tmax_day_c,
    waco_extent
  )


plot(
  central_texas,
  main = paste(
    "GridMET Grid Near Waco",
    format(target_date, "%B %d, %Y")
  ),
  axes = TRUE
)


points(
  waco,
  pch = 19,
  cex = 1.5
)


text(
  waco,
  labels = "Waco",
  pos = 4
)


# ============================================================
# 28. SHOW THE DATA-REDUCTION IDEA
# ============================================================

cat(
  "\n============================================\n"
)

cat(
  "FROM DATA CUBE TO LOCAL INFORMATION\n"
)

cat(
  "============================================\n\n"
)


cat(
  "Original potential space-time values:",
  format(
    total_values,
    big.mark = ",",
    scientific = FALSE
  ),
  "\n"
)


cat(
  "Values extracted for Waco:",
  nrow(waco_daily),
  "\n"
)


cat(
  "\nWe started with a continental-scale\n",
  "space-time climate dataset and reduced it\n",
  "to a local environmental time series.\n",
  sep = ""
)


# ============================================================
# 29. FINAL MESSAGE / BRIDGE TO THE HPC LECTURE
# ============================================================

cat(
  "\n\n============================================\n"
)

cat(
  "LIVE DEMO COMPLETE\n"
)

cat(
  "============================================\n\n"
)


cat(
  "Today we analyzed:\n",
  "  - one climate variable\n",
  "  - one year\n",
  "  - one location\n\n",
  sep = ""
)


cat(
  "What happens if we want:\n",
  "  - 45 years?\n",
  "  - 10 climate variables?\n",
  "  - thousands of locations?\n",
  "  - multiple climate models?\n",
  "  - hundreds of simulations?\n\n",
  sep = ""
)


cat(
  "Eventually this becomes a\n",
  "HIGH-PERFORMANCE COMPUTING problem.\n",
  sep = ""
)


# ============================================================
# END
# ============================================================