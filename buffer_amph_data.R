

# Install packages ----
desired_packages <- c("readxl", "dplyr", "sf", "sp", "rnrfa")

for (pkg in desired_packages) {
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}


# Read in the amphibian data:
amphibian_data <- read.csv("amph_shrop_2018_2025/amph_shrop_2018_2025.csv")
amphibian_data <- amphibian_data %>% filter(Start.date.year >= 2018)

# Read in the grid reference list:
grid_list <- read.csv("grid_refs.csv", stringsAsFactors = FALSE)

# Remove any spaces in the grid references then set grid reference precision
# level to ensure only records with minimum 6-figures are used.
grid_list$GridRefsToLookForSpeciesRecords = gsub(" ", "", grid_list$GridRefsToLookForSpeciesRecords, fixed=T)
grid_list$grPrecision <- nchar(grid_list$GridRefsToLookForSpeciesRecords)
grid_list <- grid_list %>% filter(grPrecision > 7)

amphibian_data$OSGR = gsub(" ", "", amphibian_data$OSGR, fixed=T)
amphibian_data$grPrecision <- nchar(amphibian_data$OSGR)
amphibian_data <- amphibian_data %>% filter(grPrecision > 7)

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

# Export the list of amphibian records within buffers
amphibians_df <- st_drop_geometry(amphibians_in_buffers)
write.csv(amphibians_df, "amphibians_df.csv", row.names = FALSE)
# Export the list of grid references with amphibian records nearby
write.csv(grid_list_with_counts, "grid_list_with_counts.csv", row.names = FALSE)
