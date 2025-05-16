library(terra)

# Set paths and load data
data_path <- Sys.getenv("DATA_PATH")
vector_path <- paste0(Sys.getenv("VEC_PATH"), "caatinga.gpkg")

# List all TIFF files in directory
tif_files <- list.files(data_path, pattern = ".*(tif)$", full.names = TRUE)

# Function to filter files by year pattern
filter_files_by_year <- function(file_list, year_pattern) {
  grep(year_pattern, file_list, value = TRUE)
}

# Load Caatinga boundary vector
caatinga_boundary <- vect(vector_path)

analysis_years <- 2021:2022  #1985:2023
target_crs <- "EPSG:4674"  # SIRGAS 2000

# Group files by year
files_by_year <- lapply(analysis_years, 
                        function(year) {
                          filter_files_by_year(tif_files, as.character(year))
                        })
names(files_by_year) <- analysis_years

# Load and project rasters to target crs
projected_rasters <- lapply(files_by_year, function(year_files) {
  lapply(year_files, function(file_path) {
    project(rast(file_path), target_crs, method = "near")
  })
})

# Create SpatRasterCollections and merge them by year
raster_collections <- lapply(projected_rasters, sprc)
merged_rasters <- lapply(raster_collections, function(collection) {
  merge(collection, algo = 1)
})

# Plotting part
# just for testing...
# some helper functions
extract_year_from_filename <- function(filename) {
  pattern <- ".*_(\\d{4})$"
  sub(pattern, "\\1", filename)
}

create_plot_title <- function(year) {
  sprintf("Queimadas em %s", year)
}

# plot each cropped raster
for (raster in merged_rasters) {
  year <- extract_year_from_filename(names(raster))
  plot_title <- create_plot_title(year)
  
  raster |> 
    crop(caatinga_boundary, mask = TRUE, snap = "out", extend = TRUE) |> 
    terra::plot(
      main = plot_title,
      col = c("black", "white"),
      buffer=T
    )
}
