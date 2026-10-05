# 1. Setup

# Load terra for working with raster and gridded spatial data.
library(terra)

# Load ncdf4 so we can inspect the internal structure of the NetCDF file.
library(ncdf4)

# Create a local data folder if it does not already exist.
dir.create("data", showWarnings = FALSE)

# Use a relative file path so the script works for anyone who clones the repository.
nc_file <- "data/tmmx_2024.nc"

# Download the real 2024 GridMET NetCDF file only if it is not already present.
if (!file.exists(nc_file)) {
  download.file(
    "https://github.com/Baylor-HACLab-Classwork/baylor_env3387_fall2026_netcdf_guestlecture1/releases/latest/download/tmmx_2024.nc",
    destfile = nc_file,
    mode = "wb"
  )
}


# 2. Inspect the NetCDF file

# Open the NetCDF file so we can examine its metadata, variables, and dimensions.
nc <- nc_open(nc_file)

# Print a summary of the NetCDF structure.
nc

# List the scientific variables stored in the file.
names(nc$var)

# List the dimensions that organize the data, such as longitude, latitude, and time.
names(nc$dim)

# Close the NetCDF connection after inspecting the file.
nc_close(nc)


# 3. Open it as a raster data cube

# Open the NetCDF with terra as a multi-layer raster object.
tmax <- rast(nc_file)

# Print the raster summary, including dimensions, resolution, extent, and number of layers.
tmax

# Show the number of rows in the spatial grid.
nrow(tmax)

# Show the number of columns in the spatial grid.
ncol(tmax)

# Show the number of raster layers; here, each layer represents one day.
nlyr(tmax)

# Show the spatial resolution in decimal degrees.
res(tmax)

# Show the date associated with each raster layer.
time(tmax)

# Calculate how many total space-time values are represented by the full data cube.
format(ncell(tmax) * nlyr(tmax),big.mark = ",")

# 4. Map one day

# Convert the NetCDF time coordinate into standard R dates.
dates <- as.Date(time(tmax))

# Select the raster layer corresponding to July 15, 2024.
day <- tmax[[which(dates == as.Date("2024-07-15"))]]

# GridMET temperature is stored in Kelvin, so convert this day to Celsius.
day_c <- day - 273.15

# Plot the spatial pattern of maximum temperature for the selected day.
plot(day_c,main = "GridMET Maximum Temperature — July 15, 2024")

# Show a few underlying longitude, latitude, and temperature values from the grid.
head(as.data.frame(day_c, xy = TRUE, na.rm = TRUE))


# 5. Extract Waco and make a time series

# Create a spatial point representing Waco, Texas.
waco <- vect(
  data.frame(
    lon = -97.1467,
    lat = 31.5493
  ),
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)

# Extract all 366 daily GridMET values from the grid cell containing Waco.
waco_values <- as.numeric(extract(tmax, waco, ID = FALSE)[1, ])

# Combine the dates and extracted temperatures into a simple time-series data frame.
waco_daily <- data.frame(date = dates,tmax_F = (waco_values - 273.15) * 9 / 5 + 32)

# Plot the full year of daily maximum temperature at Waco.
plot(
  waco_daily$date,
  waco_daily$tmax_F,
  type = "l",
  xlab = "Date",
  ylab = "Daily maximum temperature (°F)",
  main = "2024 GridMET Maximum Temperature — Waco"
)

# Add a 100°F reference line so extreme-heat days are easy to see.
abline(h = 100, lty = 2)
