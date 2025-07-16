library(terra)
library(ncdf4)
library(sf)
library(future.apply) 
library(dplyr)
library(ggplot2)

# Load county shapefile with consistent CRS
county_sf <- st_read('/Users/quad1089/Library/Mobile Documents/com~apple~CloudDocs/Desktop/CU-PostDoc/Research/Ryan_analysis_rep/tl_2024_us_county/tl_2024_us_county.shp')
county_sf <- st_transform(county, crs = "EPSG:4326")  # Assuming NetCDF is in lat/lon
#subset to CONUS
county_sf <- county_sf %>%
  filter(STATEFP != "15"& STATEFP != "02"   & STATEFP != "72"& STATEFP != "66" & STATEFP != "78" & STATEFP != "60"& STATEFP != "69" )

county_ids <- county$GEOID  

# Open NetCDF separately to extract and subset time
pdsi_path <- '/Users/quad1089/Library/Mobile Documents/com~apple~CloudDocs/Desktop/CU-PostDoc/Research/Ryan_analysis_rep/pdsi.nc'
pdsi_r   <- rast('/Users/quad1089/Library/Mobile Documents/com~apple~CloudDocs/Desktop/CU-PostDoc/Research/Ryan_analysis_rep/pdsi.nc', subds = "pdsi")
nc <- nc_open('/Users/quad1089/Library/Mobile Documents/com~apple~CloudDocs/Desktop/CU-PostDoc/Research/Ryan_analysis_rep/pdsi.nc')

time <- ncvar_get(nc, "day")
time_units <- ncatt_get(nc, "day", "units")$value
time_origin <- strsplit(time_units, "since ")[[1]][2]
dates <- as.Date(time, origin = time_origin)
nc_close(nc)

keep   <- which(format(dates, "%Y") %in% 2004:2025)
pdsi_r <- pdsi_r[[keep]]                 # subset layers lazily
dates  <- dates[keep]


# Group layer indices by year‑month (character like "2005‑07")
month_groups <- split(seq_along(dates), format(dates, "%Y-%m"))

# Let terra use several threads *inside* each worker
terraOptions(threads = 2)                # adjust to your CPU

### ------------------------------------------------------------------
### 1.  Parallel plan
### ------------------------------------------------------------------
plan(multisession, workers = max(1, parallel::detectCores() - 1))

### ------------------------------------------------------------------
### 2.  Do the work in parallel
### ------------------------------------------------------------------
monthly_df <- future_lapply(
  names(month_groups),
  FUN = function(m) {
    # Re-open raster inside worker
    pdsi <- rast(pdsi_path, subds = "pdsi")[[keep]][[month_groups[[m]]]]
    # Average over the month
    m_avg <- if (nlyr(pdsi) > 1) mean(pdsi, na.rm = TRUE) else pdsi
    # Re-create county vect from sf
    county_v <- vect(county_sf)
    vals <- extract(m_avg, county_v, fun = mean, weights = TRUE, na.rm = TRUE)[, 2]
    data.frame(GEOID = county_ids, month = m, pdsi = vals, stringsAsFactors = FALSE)
  },
  future.seed = TRUE
) |> bind_rows()

plan(sequential)
### ------------------------------------------------------------------
### 3.  Result
### ------------------------------------------------------------------
head(monthly_df)


### ------------------------------------------------------------------
### 4.  Map
### ------------------------------------------------------------------
pdsi_summary <- monthly_df %>%
  group_by(GEOID)%>%
  summarise(pdsi = mean(pdsi))

#merge with county shapefile

pdsi_summary <- left_join(county_sf, pdsi_summary)

ggplot(data = pdsi_summary) +
  geom_sf(aes(fill = pdsi)) +
  scale_fill_viridis_c(option = "C")+
  theme_void()

ggsave(
  "mean_pdsi.pdf",
  plot = last_plot(),
  width = 4,
  height = 5,
  units = "in",
  dpi = 300
)
