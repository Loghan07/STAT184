
# Problem 2 -------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)


lib_raw <- read_csv("raw/libraries.csv")
area_raw <- st_read("raw/community_areas.geojson") 

nrow(lib_raw)
names(lib_raw)
nrow(area_raw)
names(area_raw)

# Code to find the coordinates of each library 
lib_xy <- lib_raw |>
  separate(LOCATION, into = c("latitude", "longitude"), sep = ",") |>
  mutate(latitude = parse_number(latitude),
         longitude = parse_number(longitude))

lib_xy |> select(BRANCH, latitude, longitude) |> head()

# checking the data
n_before <- nrow(lib_xy)

bad_coord <- is.na(lib_xy$latitude) | is.na(lib_xy$longitude) |
  !between(lib_xy$latitude, -90, 90) | !between(lib_xy$longitude, -180, 180)

dup_record <- duplicated(lib_xy[c("BRANCH", "ADDRESS")])

shared_xy <- duplicated(lib_xy[c("latitude", "longitude")]) & !dup_record

cleaning_log <- tibble(
  records_before        = n_before,
  bad_or_missing_coords = sum(bad_coord),
  duplicate_records     = sum(dup_record),
  shared_coords_kept    = sum(shared_xy)
)
cleaning_log

library(purrr) 


cleaning_log$records_after <- nrow(lib_xy)
cleaning_log
# 82 records before cleaning and 82 records after cleaning
# 0 missing coordinates, 0 duplicate coordinates,0 records of places sharing
# the same coordinates. Since nothing was removed, we are still at 82 clean records

lib_pts <- st_sfc(
  map(seq_len(nrow(lib_xy)),
      \(i) st_point(c(lib_xy$longitude[i], lib_xy$latitude[i]))),
  crs = "OGC:CRS84"
)

sites_wgs <- st_sf(site_id   = seq_len(nrow(lib_xy)),
                   site_name = lib_xy$BRANCH,
                   geometry  = lib_pts) 

# CRS: EPSG:26916 (NAD83 / UTM zone 16N) , units = meters 
sites_m <- st_transform(sites_wgs, 26916)

st_crs(sites_m)$input
sites_m


# Drawing a circle with radius 500m/ 1000m afround every library
buf500  <- st_buffer(sites_m, units::set_units(500, m))  |> summarise()
buf1000 <- st_buffer(sites_m, units::set_units(1000, m)) |> summarise()

# finding the area of each zone

buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = c(
    as.numeric(units::set_units(st_area(buf500),  km^2)),
    as.numeric(units::set_units(st_area(buf1000), km^2))
  )
)
buffer_summary

# checking that the 1000m area is at least as large as teh 500m area
# it says true 
buffer_summary$union_area_km2[2] >= buffer_summary$union_area_km2[1]

# making sure that the boundaries are in the same units
areas_m <- st_transform(area_raw, 26916)
src <- "Source: City of Chicago Data Portal (libraries; community areas), accessed 2026-11-08"

# Map 1 
p_sites <- ggplot() +
  geom_sf(data = areas_m, fill = "white", color = "grey") +
  geom_sf(data = sites_m, aes(color = "Library branch (n = 82)")) +
  labs(title = "Chicago Public Library Branches",
       color = "Layer", caption = src) +
  theme_bw()

# map 2
p_buffers <- ggplot() +
  geom_sf(data = areas_m, fill = "white", color = "grey") +
  geom_sf(data = buf500, aes(fill = "Combined 500 m buffer"), alpha = 0.5) +
  geom_sf(data = sites_m, aes(color = "Library branch")) +
  labs(title = "Libraries and Their Combined 500 m Buffer",
       fill = "Area", color = "Point", caption = src) +
  theme_bw()

p_sites
p_buffers

# making the data_sources.csv file

data_sources <- tibble(
  dataset = c("Libraries - Locations, Contact Information, and Usual Hours of Operation",
              "Boundaries - Community Areas"),
  publisher = "City of Chicago Data Portal",
  url = c("https://data.cityofchicago.org/d/x8fc-8rcq",
          "https://data.cityofchicago.org/d/igwz-8jzy"),
  date_accessed = "2026-11-08",
  license = "not specified on portal"
)
write_csv(data_sources, "data_sources.csv")
write_sf(sites_m, "sites_m.gpkg")


# Interpretation
# There are 82 libraries spread out accross Chicago.There spread evenly in
# the north of chicago, and beocme sparsley spread out the further south you go.
# Within 500m of atleast one library there are 64.3 km2, and within 1000m, there
# are 250 km2
