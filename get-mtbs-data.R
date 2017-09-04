# load packages
# if you don't have these, you can install via install.packages()
# e.g., install.packages('dplyr')
library(dplyr)
library(tidyr)
library(rgdal)

# Acquiring raw fire and ecoregion data ----------------------------------------
# Check to see whether the raw data files exist locally
# and download any missing files.

# Create a data directory if necessary
raw_prefix <- file.path("data", "raw")
if (!dir.exists(raw_prefix)) {
  dir.create(raw_prefix, recursive = TRUE)
  print('Creating directory for raw data')
}


ecoregion_prefix <- file.path(raw_prefix, "us_eco_l3")
ecoregion_shp <- file.path(ecoregion_prefix, "us_eco_l3.shp")
if (!file.exists(ecoregion_shp)) {
  print('Download ecoregion data from the EPA')
  loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip"
  dest <- paste0(ecoregion_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = ecoregion_prefix)
  unlink(dest)
  stopifnot(file.exists(ecoregion_shp))
}


mtbs_prefix <- file.path(raw_prefix, "mtbs_fod_pts_data")
mtbs_shp <- file.path(mtbs_prefix, 'mtbs_fod_pts_20170501.shp')
if (!file.exists(mtbs_shp)) {
  print('Downloading fire event data from USGS')
  loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/fod_pt_shapefile/mtbs_fod_pts_data.zip"
  dest <- paste0(mtbs_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = mtbs_prefix)
  unlink(dest)
  stopifnot(file.exists(mtbs_shp))
}





# Match each fire event to an ecoregion --------------------------------
# For each fire event, determine the ecoregion where it ignited
print('Matching fire events to ecoregions')
ecoregion_file <- list.files(pattern = 'us_eco_l3.shp$',
                             recursive = TRUE,
                             full.names = TRUE)
ecoregions <- readOGR(ecoregion_file)

mtbs_file <- list.files(pattern = 'mtbs_fod_pts_.*\\.shp$',
                        recursive = TRUE,
                        full.names = TRUE)
mtbs <- readOGR(mtbs_file)
mtbs <- spTransform(mtbs, CRS(proj4string(ecoregions)))
mtbs <- subset(mtbs, !(STATE %in% c("Alaska", "Hawaii", "Puerto Rico")))

# overlay points to ecoregions (returns a data frame with one row per fire)
mtbs_overlay <- over(mtbs, ecoregions)





# Tidy up the fire event data  -------------------------------------
print('Tidying fire event data')

# merge fire data + ecoregion data
event_d <- bind_cols(as.data.frame(mtbs), mtbs_overlay)
names(event_d) <- tolower(names(event_d))

make_yearmonth <- function(year, month) {
  # helper function to make a 'yearmonth' variable
  # e.g., make_yearmonth(2010, 3) returns '2010-03'
  paste(year, sprintf("%02d", month), sep = "-")
}

# select just the columns we need and create a yearmonth variable
event_d <- event_d %>%
  tbl_df %>%
  filter(!is.na(na_l3name)) %>%
  select(fire_id, fire_year, fire_mon, fire_day, lat, long, p_acres, state,
         starts_with('na'), -ends_with('code')) %>%
  mutate(yearmonth = make_yearmonth(fire_year, fire_mon))





# Summarize fire events at the ecoregion level ----------------------------
# generate counts and total burn area for each yearmonth X ecoregion combination
summary_d <- event_d %>%
  group_by(yearmonth, na_l3name) %>%
  summarize(n_fire = n(), 
            total_acres_burned = sum(p_acres)) %>%
  ungroup %>%
  complete(yearmonth, na_l3name,
           fill = list(n_fire = 0, 
                       total_acres_burned = 0))





# Integrate fire data with climate data -----------------------------------
# We will load some pre-processed climate data from Amazon Web Services
print('Integrating climate data')
climate_url <- 'https://s3-us-west-2.amazonaws.com/earthlab-gridmet/teaching/ecoregion_summaries.csv'
climate_data <- read.csv(climate_url) %>%
  tbl_df %>%
  mutate(yearmonth = make_yearmonth(year, month)) %>%
  spread(variable, wmean) %>%
  filter(year < 2017) %>%
  rename(mean_potential_et = pet,
         total_precip = pr,
         max_air_temp = tmmx,
         mean_wind_speed = vs,
         na_l3name = NA_L3NAME)

event_d <- event_d %>%
  left_join(climate_data) %>%
  select(-fire_year, -fire_mon) %>%
  rename(day = fire_day, 
         acres_burned = p_acres) %>%
  select(fire_id, year, month, day, lat, long, acres_burned, starts_with('na'), 
         mean_potential_et, total_precip, max_air_temp, mean_wind_speed)

summary_d <- summary_d %>%
  left_join(climate_data)



# Finally, write csv files of our tidy data -------------------------------
destination_dir <- file.path('data', 'processed')
print(paste('Writing output files to', destination_dir))

if (!dir.exists(destination_dir)) {
  dir.create(destination_dir, recursive = TRUE)
}

event_d %>%
  write.csv(file.path(destination_dir, 'fire-events.csv'), 
            row.names = FALSE)

summary_d %>%
  write.csv(file.path(destination_dir, 'fire-summary.csv'), 
            row.names = FALSE)
