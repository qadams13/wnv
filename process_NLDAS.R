library(terra)
library(sf)
library(ncdf4)
library(dplyr)
library(purrr)
library(stringr)
library(furrr)
library(progressr)

# ----------------------
# 0. Speed & Progress Setup
# ----------------------
terraOptions(threads = 4)         # Adjust per CPU
plan(multisession, workers = 4)   # Parallel plan
handlers(global = TRUE)           # Enable progress bars
handlers("txtprogressbar")        # Text progress bar (terminal)

# ----------------------
# 1. Load US county shapefile
# ----------------------
counties <- st_read('/Users/quad1089/Library/Mobile Documents/com~apple~CloudDocs/Desktop/CU-PostDoc/Research/Ryan_analysis_rep/tl_2024_us_county/tl_2024_us_county.shp')
counties <- st_transform(counties, crs = "EPSG:4326")  
#subset to CONUS
counties <- counties %>%
  filter(STATEFP != "15"& STATEFP != "02"   & STATEFP != "72"& STATEFP != "66" & STATEFP != "78" & STATEFP != "60"& STATEFP != "69" )

counties_v <- vect(counties)
county_ids <- counties$GEOID

# ----------------------
# 2. List .nc files and parse dates
# ----------------------

nc_files <- list.files("/Users/quad1089/Library/Mobile Documents/com~apple~CloudDocs/Desktop/CU-PostDoc/Research/Ryan_analysis_rep/NLDAS/", pattern = "\\.nc$", full.names = TRUE)
#check for variable names
# Open the file
nc <- nc_open("/Users/quad1089/Library/Mobile Documents/com~apple~CloudDocs/Desktop/CU-PostDoc/Research/Ryan_analysis_rep/NLDAS/NLDAS_MOS0125_M.A201709.020.nc")

# List variable names
names(nc$var) #using SoilM_0_10cm


# 3. Extract YYYY-MM from filenames like "NLDAS_MOS0125_M.A201709.020.nc"
get_month <- function(path) {
  str_extract(path, "A\\d{6}") |>         # e.g. A201709
    str_remove("A") |>                    # remove leading A
    str_replace("(.{4})(.{2})", "\\1-\\2")  # insert dash after year
}
months <- map_chr(nc_files, get_month)


# ----------------------
# 3. Function to extract from one file
# ----------------------
extract_monthly_means <- function(nc_file, month_string, counties_sf) {
  message("Processing: ", basename(nc_file))
  tryCatch({
    counties_v <- vect(counties_sf)             # must re-create inside worker
    county_ids <- counties_sf$GEOID
    
    r <- rast(nc_file, subds = "SoilM_0_10cm")   # use your actual variable name
    
    vals <- extract(r, counties_v, fun = mean, weights = TRUE, na.rm = TRUE)[, 2]
    
    data.frame(GEOID = county_ids, month = month_string, soil_moisture = vals)
  }, error = function(e) {
    warning(paste("Error in:", basename(nc_file), "->", e$message))
    return(NULL)
  })
}
# ----------------------
# 4. Run 
# ----------------------
#testing on just one file:
extract_monthly_means(nc_files[1], months[1])

#on all files:
soil_monthly_df <- with_progress({
  future_map2_dfr(
    nc_files, months,
    ~extract_monthly_means(.x, .y, counties),  
    .options = furrr_options(seed = TRUE),
    .progress = TRUE
  )
})

# ----------------------
# 5. Save or inspect
# ----------------------
write.csv(soil_monthly_df, "county_soilmoisture_monthly.csv", row.names = FALSE)
head(soil_monthly_df)

