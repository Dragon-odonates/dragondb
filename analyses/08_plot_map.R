# Header #############################################################
#
# Author: Reto Schmucki & Lisa Nicvert
# Email: retoschm@ceh.ac.uk & lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2024-10-15
#
# Script Description: map of our records


# Libraries ---------------------------------------------------------------

library(here)
library(data.table)
library(RPostgres)

library(rnaturalearth)
library(terra)
library(ggplot2)
library(ggspatial)
library(sf)

library(dragondb)

# Connect to DB -----------------------------------------------------------
# Connect to "local" DB
con <- dbConnect(
  drv       = RPostgres::Postgres(), # dbDriver("PostgreSQL"),
  dbname    = "dragon",
  host      = "localhost", # "192.168.0.75",
  port      = 5432,
  user      = Sys.getenv('USERNAME'),
  password  = Sys.getenv('PASSWORD')
)

# Query ---------------------------------------------------------------

occ <- dbGetQuery(con, 'SELECT "species",
                        ST_AsText("decimalCoordinates") AS "decimalCoordinates",
                        "eventDate", "Event"."datasetID", "datasetName", "parentDatasetID"
                        FROM "Taxon"
                        LEFT JOIN "Occurrence"
                            ON "Taxon"."taxonID" = "Occurrence"."taxonID"
                        LEFT JOIN "Event"
                            ON "Event"."eventID" = "Occurrence"."eventID"
                        LEFT JOIN "EventDate"
                            ON "Event"."eventDateID" = "EventDate"."eventDateID"
                        LEFT JOIN "Location"
                            ON "Event"."locationID" = "Location"."locationID"
                        LEFT JOIN "Dataset"
                            ON "Event"."datasetID" = "Dataset"."datasetID";')

# Transform to sf
occ_sf <- st_as_sf(occ, wkt = "decimalCoordinates")
st_crs(occ_sf) <- 4326

# Transform coordinates
occ_sf <- st_transform(occ_sf, 3035)

# Map ---------------------------------------------------------------------

# Get countries
eu <- ne_countries(scale = 50,
                   continent = "Europe", returnclass = "sf")
af <- ne_countries(scale = 50,
                   continent = "Africa", returnclass = "sf")
as <- ne_countries(scale = 50,
                   continent = "Asia", returnclass = "sf")

# Crop Europe and change CRS
eu_box <- st_make_grid(st_bbox(ext(c(-30, 52, 27, 74)), crs = 4326),
                       n = 1, what = "polygons")
eu_sf <- st_make_valid(st_crop(st_make_valid(rbind(eu, af, as)),
                               st_buffer(eu_box, 0)))
eu_3035 <- st_make_valid(st_transform(eu_sf[, "sov_a3"],
                                      crs = 3035))

## define grid hexagon
grid_scale <- 50000
terrestrial_eu_grid <- st_make_grid(st_make_grid(st_bbox(st_buffer(eu_3035,
                                                                   grid_scale)),
                                                 n = 1, what = "polygons"),
                                cellsize = grid_scale,
                                what = "polygons",
                                square = FALSE,
                                flat_topped = TRUE)

terrestrial_eu_grid1 <- terrestrial_eu_grid[unique(unlist(st_intersects(eu_3035,
                                                                        terrestrial_eu_grid)))]

hex_transect_density <- data.table(hex_id = seq_along(terrestrial_eu_grid1))

transect_density <- data.table(hex_with = unlist(st_intersects(
  occ_sf,
  terrestrial_eu_grid1)))[, .N, by = hex_with]

hex_transect_density1 <- merge(hex_transect_density,
                               transect_density,
                               by.x = "hex_id", by.y = "hex_with",
                               all.x = TRUE)

hex_transect_density1[is.na(N), N := 0]

hex_transect_density1_sf <- st_sf(unique(hex_transect_density1),
                                  terrestrial_eu_grid1)

main_data <- st_read(here("data/data_map/main_map.shp"))

my_breaks <- seq(from = 1, to = 14, by = 2)
hex_transect_density1_sf[hex_transect_density1_sf$N > 0, ]$N <- log(hex_transect_density1_sf[hex_transect_density1_sf$N > 0, ]$N)

record_density_map <- ggplot() +
                        geom_sf(data = main_data,
                                fill = "grey95", colour = NA) +
                        geom_sf(data = hex_transect_density1_sf[hex_transect_density1_sf$N > 0, ],
                                aes(fill = N), colour = NA) +
                        geom_sf(data = main_data, fill = NA,
                                colour = "grey5", lwd = 0.2) +
                        scale_fill_gradient2(name = "N Record (log)",
                                            breaks = my_breaks,
                                            labels = my_breaks,
                                            low = "cyan4",
                                            mid = "goldenrod2",
                                            high = "red",
                                            midpoint = 8,
                                            na.value = NA,
                                            guide = guide_legend(override.aes = list(shape = 19, size = 5))) +
                        coord_sf(xlim = as.numeric(st_bbox(main_data)[c(1, 3)]) - c(0, 300000),
                                 ylim = as.numeric(st_bbox(main_data)[c(2, 4)]) + c(250000, -215000)) +
                        annotation_scale(location = "br", width_hint = 0.5) +
                        annotation_north_arrow(location = "bl", which_north = "true",
                                               pad_x = unit(0.3, "in"), pad_y = unit(0.5, "in"),
                                               style = north_arrow_fancy_orienteering) +
                        theme(panel.background = element_rect(fill = "aliceblue"),
                              axis.ticks.x = element_blank(),
                              # axis.text.x = element_blank(),
                              panel.grid = element_line(color = "white", linewidth = 0.3, linetype = 1),
                              legend.background = element_rect(fill = "transparent"),
                              legend.key = element_rect(fill = "transparent"),
                              legend.position.inside = c(0.15, 0.68),
                              legend.text = element_text(face = "bold", size = 10),
                              legend.title = element_text(face = "bold", size = 10),
                              axis.text.y = element_text(size = 10))

record_density_map

ggsave("outputs/dragonfly_data_density_map_v2.png",
       record_density_map, width = 7, height = 5,
       dpi = 300)
