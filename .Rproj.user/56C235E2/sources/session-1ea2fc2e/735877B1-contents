# 1. Setup

rm(list = ls())

library(terra)
library(ncdf4)

nc_file <- "/data/haclab_shared/climate_hub/observed/gridmet/data/tmmx_2024.nc"

file.exists(nc_file)


# 2. Look inside the NetCDF file

nc <- nc_open(nc_file)

nc
names(nc$var)
names(nc$dim)
nc$var[[1]]$units

nc_close(nc)


# 3. Open the NetCDF as a raster data cube

tmax <- rast(nc_file)

tmax
nrow(tmax)
ncol(tmax)
nlyr(tmax)
res(tmax)
ext(tmax)
time(tmax)

total_values <- ncell(tmax) * nlyr(tmax)

format(
  total_values,
  big.mark = ",",
  scientific = FALSE
)


# 4. Select one day

dates <- as.Date(time(tmax))

target_date <- as.Date("2024-07-15")

day_index <- which(dates == target_date)

tmax_day <- tmax[[day_index]]

tmax_day


# 5. Convert Kelvin to Celsius and map it

tmax_day_c <- tmax_day - 273.15

plot(
  tmax_day_c,
  main = "GridMET Maximum Temperature — July 15, 2024"
)


# 6. Zoom to Texas and add Waco

texas <- crop(
  tmax_day_c,
  ext(-107, -93, 25, 37)
)

waco <- vect(
  data.frame(
    lon = -97.1467,
    lat = 31.5493
  ),
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)

plot(
  texas,
  main = "GridMET Maximum Temperature — Texas"
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


# 7. Extract the July 15 value for Waco

waco_day <- extract(
  tmax_day_c,
  waco
)

waco_day


# 8. Extract the full year for Waco

waco_values <- as.numeric(
  extract(
    tmax,
    waco,
    ID = FALSE
  )[1, ]
)

waco_daily <- data.frame(
  date = dates,
  tmax_F = (waco_values - 273.15) * 9 / 5 + 32
)

head(waco_daily)


# 9. Plot the Waco time series and ask two simple questions

plot(
  waco_daily$date,
  waco_daily$tmax_F,
  type = "l",
  xlab = "Date",
  ylab = "Daily maximum temperature (°F)",
  main = "2024 GridMET Maximum Temperature — Waco, Texas"
)

abline(
  h = 100,
  lty = 2
)

waco_daily[
  which.max(waco_daily$tmax_F),
]

sum(
  waco_daily$tmax_F >= 100,
  na.rm = TRUE
)
