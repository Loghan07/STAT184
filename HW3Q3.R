
# Problem 3 ---------------------
library(ggplot2)
library(dplyr)
library(readr)
library(sf)


sites_m <- st_read("sites_m.gpkg") 
areas_m <- st_read("raw/community_areas.geojson") |> st_transform(26916)

#  ID and name for each community
areas_clean <- areas_m |>
  select(district_id = area_numbe, district_name = community)

# Boundary checks
areas_clean |> st_drop_geometry() |> count(district_id) |> filter(n > 1)
areas_clean |> st_drop_geometry() |> filter(is.na(district_id))
nrow(areas_clean)

# boundary check
all(st_is_valid(areas_clean))
# Result is true

# making sure that the library and boundary data uses the same CRS
st_crs(sites_m) == st_crs(areas_clean)
# true

# Which area does the library fall into
i <- st_intersects(sites_m, areas_clean)

site_match_audit <- tibble(
  site_id = sites_m$site_id,
  n_area_matches = lengths(i)
)

# How many library match 0, 1 or more than one area | all 82 are paired with 1 community
site_match_audit |> count(n_area_matches)
write_csv(site_match_audit, "site_match_audit.csv")

# Keep only libraries that matched exactly one area
sites_assigned <- sites_m |> filter(lengths(i) == 1)
n_unassigned <- nrow(sites_m) - nrow(sites_assigned)
n_unassigned    # prints 0

# Check which library sites did not match exactly one area
site_match_audit |>
  filter(n_area_matches != 1)
# returns an empty tibble, proves every pair is connected to exactly 1 community

# Attach the area ID and name to each assigned library
sites_joined <- st_join(sites_assigned, areas_clean)

# Count libraries per area
counts <- sites_joined |>
  st_drop_geometry() |>
  count(district_id, name = "n_sites")

# Joins back to ALL 77 areas so zero-count areas are kept.
district_counts_sf <- areas_clean |>
  left_join(counts, by = join_by(district_id)) |>
  mutate(n_sites = coalesce(n_sites, 0L))

# Area in km2 and libraries per km2
district_counts_sf$area_km2 <- as.numeric(units::set_units(st_area(district_counts_sf), km^2))
district_counts_sf$sites_per_km2 <- district_counts_sf$n_sites / district_counts_sf$area_km2

# Required checks Returns 0 
all(district_counts_sf$area_km2 > 0)
sum(district_counts_sf$n_sites) == nrow(sites_assigned)
sum(is.na(district_counts_sf$n_sites))    


district_counts <- district_counts_sf |>
  st_drop_geometry() |>
  select(district_id, district_name, n_sites, area_km2, sites_per_km2)

print(district_counts)
write_csv(district_counts, "district_counts.csv")

# top areas by count and by density
district_counts |> arrange(desc(n_sites)) |> head(8)
district_counts |> arrange(desc(sites_per_km2)) |> head(8)


n_zero <- sum(district_counts_sf$n_sites == 0)
bb <- st_bbox(district_counts_sf)     

src3 <- paste0("Source: City of Chicago Data Portal (libraries; community areas), accessed 2026-11-08.\n",
               "Libraries left unassigned: ", n_unassigned, " of ", nrow(sites_m), ".")

# Map 1 
p_count <- ggplot() +
  geom_sf(data = filter(district_counts_sf, n_sites == 0), fill = "grey", color = "white") +
  geom_sf(data = filter(district_counts_sf, n_sites > 0), aes(fill = n_sites), color = "white") +
  scale_fill_gradientn(colors = sf.colors(20), name = "Libraries\n(count)") +
  coord_sf(xlim = c(bb[["xmin"]], bb[["xmax"]]), ylim = c(bb[["ymin"]], bb[["ymax"]])) +
  labs(title = "Number of Libraries per Community Area",
       subtitle = paste0("Grey = 0 libraries (", n_zero, " areas); no area has a missing count"),
       caption = src3) +
  theme_bw()

# Map 2
p_density <- ggplot() +
  geom_sf(data = filter(district_counts_sf, n_sites == 0), fill = "grey", color = "white") +
  geom_sf(data = filter(district_counts_sf, n_sites > 0), aes(fill = sites_per_km2), color = "white") +
  scale_fill_gradientn(colors = sf.colors(20), name = "Libraries\nper km\u00b2") +
  coord_sf(xlim = c(bb[["xmin"]], bb[["xmax"]]), ylim = c(bb[["ymin"]], bb[["ymax"]])) +
  labs(title = "Library Density per Community Area",
       subtitle = paste0("Grey = 0 libraries (", n_zero, " areas); no area has a missing value"),
       caption = src3) +
  theme_bw()

p_count
p_density

# Interpretation

# There are 5 community areas that have the most libraries which are: Austin,
# Humboldt Park, New City, Near West Side, and Portage Park with 3 libraries in 
# each community

# There are a total of 16 community areas that do not have a library

# Douglas has the highest library density with approximately .47 library per 
# squared kilometer

# When comparing library count and density  shows that having more libraries
# doesn't necessarily mean that an area has a higher density. Density is 
# comparing the amount of libraries to the amount of area in a given
# community. 


