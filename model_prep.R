#model development: 

#Enhance regional forecasts of annual WNND cases by incorporating high-resolution climate data
#(e.g., temperature, dewpoint, VPD, precipitation) that better represent the biologically relevant 
#exposure period (summer).

#merge all datasets
dir <- "/Users/quad1089/Library/Mobile Documents/com~apple~CloudDocs/Desktop/CU-PostDoc/Research/wnv/"
wnv <- read.csv(paste0(dir, "clean_wnv_cases.csv"))
pdsi <- read.csv(paste0(dir, "monthly_pdsi.csv")) %>%
  na.omit() %>% #getting rid of non-contiguous US states/territories
  filter(month != "2025-07")%>%
  select(!X)
pdsi$year <- substr(pdsi$month, 1, 4)
pdsi$month <- substr(pdsi$month, 6, 7)
pdsi$month = str_remove(pdsi$month, "^0+")
pdsi$month <- as.integer(pdsi$month)
pdsi$year <- as.integer(pdsi$year)

pdsi$GEOID <- sprintf(paste0("%0", 5, "d"), pdsi$GEOID)
pdsi$GEOID  <- as.character(pdsi$GEOID)

temp <- read.csv(paste0(dir, "county_tmp_monthly.csv"))
temp$GEOID <- sprintf(paste0("%0", 5, "d"), temp$GEOID)
temp$GEOID  <- as.character(temp$GEOID)

dpt <- read.csv(paste0(dir, "county_dtmean_monthly.csv")) %>%
  rename(dpt = tmean_avg)
dpt$GEOID <- sprintf(paste0("%0", 5, "d"), dpt$GEOID)
dpt$GEOID  <- as.character(dpt$GEOID)

vpd <- read.csv(paste0(dir, "county_vpdmean_monthly.csv")) %>%
  rename(vpd = vpdmean_avg)
vpd$GEOID <- sprintf(paste0("%0", 5, "d"), vpd$GEOID)
vpd$GEOID  <- as.character(vpd$GEOID)

precip <-read.csv(paste0(dir, "county_precip_monthly.csv")) 
precip$GEOID <- sprintf(paste0("%0", 5, "d"), precip$GEOID)
precip$GEOID  <- as.character(precip$GEOID)

climate <- left_join(pdsi, temp, by = c("GEOID", "month", "year"))
climate1 <- left_join(climate, dpt, by = c("GEOID", "month", "year"))
climate2 <- left_join(climate1, vpd, by = c("GEOID", "month", "year"))
climate3 <- left_join(climate2, precip, by = c("GEOID", "month", "year"))


#population centers: 
library(sf)
library(dplyr)
library(purrr)
library(readr)

cenpop_df <- read_csv("CenPop2020.csv", col_types = cols(
  STATEFP = col_character(),
  COUNTYFP = col_character(),
  LATITUDE = col_double(),
  LONGITUDE = col_double()
))
# Create full county GEOID
cenpop_df <- cenpop_df %>%
  mutate(GEOID = paste0(str_pad(STATEFP, 2, "left", "0"),
                        str_pad(COUNTYFP, 3, "left", "0")))

#  Convert to spatial points
cenpop_sf <- st_as_sf(cenpop_df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# 4. Load NEON shapefile (if not already loaded)
neon_shapefile <- st_read("NEONDomains_2024/NEON_Domains.shp")

# Spatial join: assign each county point to NEON polygon
county_neon_join <- st_join(cenpop_sf, neon_shapefile, join = st_within)

# Clean output: Keep GEOID and NEON region identifier
county_to_neon <- county_neon_join %>%
  st_drop_geometry() %>%
  filter(STATEFP != "02", STATEFP != "72" ) %>%
  select(GEOID, neon_region = OBJECTID) 

#Save to file
write_csv(county_to_neon, "county_to_neon_lookup.csv")

#join NEON file to wnv cases: 
# 1. Summarize total neuro cases by county across all years
wnv_county_total <- wnv %>%
  group_by(StCoFIPS) %>%
  summarise(total_cases = sum(neuro_cases, na.rm = TRUE), .groups = "drop") %>%
  filter(total_cases > 0)

# 2. Keep only counties with non-zero total cases
wnv_cases <- wnv %>%
  semi_join(wnv_county_total, by = "StCoFIPS") %>%
  mutate(neuro_cases1 = neuro_cases+0.1) %>%
  mutate(log_cases = log(neuro_cases1)) %>%
  rename(GEOID = StCoFIPS) %>%
  select(!X)

#padding with zeros and converting to character for merge
wnv_cases$GEOID <- sprintf(paste0("%0", 5, "d"), wnv_cases$GEOID)
wnv_cases$GEOID  <- as.character(wnv_cases$GEOID)


# Add neon_region to both datasets
climate_df <- climate3 %>%
  left_join(county_to_neon, by = "GEOID")

wnv_df <- wnv_cases %>%
  left_join(county_to_neon, by = "GEOID") %>%
  rename(year = Year)

#restrict to NEON regions with > 1000 cases over the period
wnv_df %>%
  group_by(neon_region) %>%
  summarise(cases = sum(neuro_cases))

wnv_df_subset <- wnv_df %>%
  filter(neon_region %in% c(1, 2, 3, 5, 6, 8, 9, 10, 11, 14, 17))

neon_wnv_annual <- wnv_df_subset %>%
  group_by(neon_region, year) %>%
  summarise(cases = sum(neuro_cases))

write.csv(neon_wnv_annual, "neon_wnv_annual.csv")

climate_df_subset <- climate_df %>%
  filter(neon_region %in% c(1, 2, 3, 5, 6, 8, 9, 10, 11, 14, 17))
# Assign same log_cases to all monthly records in that county and year
climate_weighted <- left_join(climate_df_subset, wnv_df_subset, by = c("GEOID", "year"
))

climate_weighted <- climate_df_subset %>%
  left_join(wnv_df_subset,
            by = c("GEOID", "year", "neon_region"))  # one-to-many join


#average weather vars per month per neon region
neon_climate_monthly <- climate_weighted %>%
  filter(!is.na(log_cases)) %>%  # exclude counties/years with zero WNND cases
  group_by(neon_region, year, month) %>%
  summarise(
    pdsi_wt = sum(pdsi * log_cases, na.rm = TRUE) / sum(log_cases, na.rm = TRUE),
    temp_wt = sum(tmean_avg * log_cases, na.rm = TRUE)/sum(log_cases, na.rm = TRUE),
    dpt_wt = sum(dpt * log_cases, na.rm = TRUE)/sum(log_cases, na.rm = TRUE),
    vpd_wt = sum(vpd * log_cases, na.rm = TRUE)/sum(log_cases, na.rm = TRUE),
    prec_wt = sum(ppt_mean * log_cases, na.rm = TRUE)/sum(log_cases, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(neon_climate_monthly, "neon_climate_monthly.csv")

