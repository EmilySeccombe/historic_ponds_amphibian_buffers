# Recent Amphibian Data near Historic Ponds
# 06/08/2025 Emily Seccombe
# This file imports a list of historic ponds and a dataset of more recent
# amphibian records.
# Analysis checks how many records are near to the historic ponds / which
# historic ponds have amphibian records nearby.

# Fixed values ----
# Set year to use as start of recent amphibian records of interest.
year_onwards <- 2018
# Set buffer value. Amphibian records within this distance from the historic
#ponds will be included in results.
buffer_distance <- 100
# Grid reference specificity. A 6 figure grid reference will be 8 characters
# long (without spaces) due to starting 2 letters. 
grid_ref_specificity <- 8


# Citations ----
# Data source:
# Citation for NBN Atlas: 
# NBN Atlas occurrence download at https://nbnatlas.org accessed on 28 May 2025.
# Citation for dataset:
# Copyright British Dragonfly Society Recording Scheme 2025. Dragonfly records from the British Dragonfly Society Recording Scheme. Occurrence dataset on the NBN Atlas.

# Citations for R and packages used:
# R:
# R Core Team (2025). _R: A Language and Environment for Statistical
# Computing_. R Foundation for Statistical Computing, Vienna,
# Austria. <https://www.R-project.org/>.
# R version 4.5.0 (2025-04-11 ucrt)

# dplyr:
#Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr:
#  A Grammar of Data Manipulation_. doi:10.32614/CRAN.package.dplyr
#<https://doi.org/10.32614/CRAN.package.dplyr>, R package version
#1.1.4, <https://CRAN.R-project.org/package=dplyr>.

# readxl:
# Wickham H, Bryan J (2025). _readxl: Read Excel Files_.
# doi:10.32614/CRAN.package.readxl
# <https://doi.org/10.32614/CRAN.package.readxl>, R package version
# 1.4.5, <https://CRAN.R-project.org/package=readxl>.

# sf:
# Pebesma, E., & Bivand, R. (2023). Spatial Data Science: With
# Applications in R. Chapman and Hall/CRC.
# https://doi.org/10.1201/9780429459016

# sp:
# Pebesma E, Bivand R (2005). “Classes and methods for spatial data
# in R.” _R News_, *5*(2), 9-13.
# <https://CRAN.R-project.org/doc/Rnews/>.

# rnrfa:
# Vitolo, Claudia, Fry, Matthew, Buytaert, Wouter (2016). “rnrfa: an
# R package to retrieve, filter and visualize data from the UK
# National River Flow Archive.” _The R Journal_, *8*(2), 102-116.
# <https://journal.r-project.org/archive/2016/RJ-2016-036/index.html>


# Install packages ----
desired_packages <- c("dplyr", "sf", "sp", "rnrfa")

for (pkg in desired_packages) {
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}


# Read in and tidy data ----
# Read in the amphibian data:
amphibian_data <- read.csv("amph_shrop_2018_2025/amph_shrop_2018_2025.csv")
amphibian_data <- amphibian_data %>% filter(Start.date.year >= year_onwards)

# Read in the grid reference list:
grid_list <- read.csv("grid_refs.csv", stringsAsFactors = FALSE)

# Remove any spaces in the grid references then set grid reference precision
# level to ensure only records with minimum 6-figures are used.
grid_list$GridRefsToLookForSpeciesRecords = gsub(" ", "", grid_list$GridRefsToLookForSpeciesRecords, fixed=T)
grid_list$grPrecision <- nchar(grid_list$GridRefsToLookForSpeciesRecords)
grid_list <- grid_list %>% filter(grPrecision >= grid_ref_specificity)

amphibian_data$OSGR = gsub(" ", "", amphibian_data$OSGR, fixed=T)
amphibian_data$grPrecision <- nchar(amphibian_data$OSGR)
amphibian_data <- amphibian_data %>% filter(grPrecision > grid_ref_specificity)

# Select desired columns. Rename so that both datasets have a column of
# grid references with a matching name.
grid_list <- grid_list %>% select(GridRefsToLookForSpeciesRecords) 
amphibian_data <- amphibian_data %>% select(Scientific.name, Start.date.year, OSGR) 
amphibian_data <- amphibian_data %>%
  rename(
    GridRefsOfInterest = OSGR
  )
grid_list <- grid_list %>%
  rename(
    GridRefsOfInterest = GridRefsToLookForSpeciesRecords
  )

# Change to eastings and northings (required to make a spatial frame)
x = osg_parse(grid_list$GridRefsOfInterest)
grid_list$easting = x[[1]]
grid_list$northing = x[[2]]

x = osg_parse(amphibian_data$GridRefsOfInterest)
amphibian_data$easting = x[[1]]
amphibian_data$northing = x[[2]]


# Spatial analysis ----
# Convert to spatial frames
locations_sf <- st_as_sf(grid_list, coords = c("easting", "northing"), crs = 27700)
amphib_sf <- st_as_sf(amphibian_data, coords = c("easting", "northing"), crs = 27700)

# Add a 100m buffer around the grid references of ponds
locations_buffer <- st_buffer(locations_sf, dist = 100)

# Search for amphibian records in the buffers. 
# Each row corresponds to an amphibian record that was within a buffer area
amphibians_in_buffers <- st_join(locations_buffer, amphib_sf, join = st_contains)

# Rename columns for clarity of what grid references refer to.
amphibians_in_buffers <- amphibians_in_buffers %>%
  rename(
    GridRefsOfInterest = GridRefsOfInterest.x
  )
amphibians_in_buffers <- amphibians_in_buffers %>%
  rename(
    GridRefsFromAmphData = GridRefsOfInterest.y
  )

# Remove rows where the amphibian record is not in a buffer 
amphibians_in_buffers <- amphibians_in_buffers %>%
  filter(!is.na(GridRefsFromAmphData))

# Count how many amphibian records within the buffers are within the buffer
# for each pond in the grid reference list.
record_counts <- amphibians_in_buffers %>%
  st_drop_geometry() %>%
  group_by(GridRefsOfInterest) %>%
  summarise(amphibian_count = n())


# Merge counts into grid_list.
grid_list_with_counts <- left_join(grid_list, record_counts, by = "GridRefsOfInterest")

# Replace NA (no records found) with 0.
grid_list_with_counts$amphibian_count[is.na(grid_list_with_counts$amphibian_count)] <- 0


# Export output ----
# Export the list of amphibian records within buffers
amphibians_df <- st_drop_geometry(amphibians_in_buffers)
write.csv(amphibians_df, "amphibians_df.csv", row.names = FALSE)
# Export the list of grid references with amphibian records nearby
write.csv(grid_list_with_counts, "grid_list_with_counts.csv", row.names = FALSE)
