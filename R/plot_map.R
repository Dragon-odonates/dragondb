# Header #############################################################
#
# Author: Reto Schmucki & Lisa Nicvert
# Email: retoschm@ceh.ac.uk & lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2024-10-15
#
# Script Description: 


# Libraries ---------------------------------------------------------------

# remotes::install_github("hrbrmstr/mgrs")

library(readxl)
library(data.table)
library(sf)
library(mgrs)
library(stringr)
library(here)

# Read data ---------------------------------------------------------------

## Austria -----
aus <- data.table(read_excel("data/data_raw/Odonata_Austria_Export.xlsx", 
                             sheet = "Tabelle1"))

aus <- setnames(aus, names(aus), 
                c("species_scientific_name", "sp_authority", "sp_year", 
                  "fauna_europea_id", "adult", "exuvia", "larvea", "location_name", 
                  "year", "month", "day", "x_coord", "y_coord", "location_comment", "city"))
aus[, crs := 4326]
aus[, dataset := "aus"] # Add dataset
aus[ , ":="(x_coord = as.numeric(x_coord), y_coord = as.numeric(y_coord))]

## Belgium -----
### BelgieData.rds -----
bel1 <- data.table(readRDS("data/data_raw/belgieData.rds"))
bel1 <- setnames(bel1, names(bel1), 
                c("species_scientific_name", "species_names_nl", 
                  "species_code", "date", "day", "month", "year", "utm1_mgrs"))
bel1 <- bel1[, utm1_mgrs := paste0("31U", utm1_mgrs)]

# Convert MGRS to lat-lon (needs remotes::install_github("hrbrmstr/mgrs"))
bel1[ , ":="(y_coord = mgrs_to_latlng(bel1$utm1_mgrs)$lat, 
             x_coord = mgrs_to_latlng(bel1$utm1_mgrs)$lng)]
bel1[, crs := 4326]


bel1[, count := 1] # Add count
bel1[, dataset := "bel1"]
bel1[ , ":="(x_coord = as.numeric(x_coord), y_coord = as.numeric(y_coord))]

bel1 <- bel1[, .(species_scientific_name, date, x_coord, y_coord)]
bel1[, ":="(date = as.IDate(date, format="%d/%m/%Y"))]

### 2024-076_libellen_EN.rds -----
bel2 <- fread("data/data_raw/2024-076_libellen_EN.csv")

# Select column subset
bel2 <- bel2[, .(name_lat, date, entry_date, amount, activity, method, 
                 lifestage, sex, lon, lat, location,
                 municipality, submunicipality, province,
                 source, status, embargo_date, permission_observer,
                 notes, precision, id_checklist, transect, type_transect)]
# What is certain? embargo_date? permission_observer? precision? 
# delivery_date (vs entry_date)?
# id_checklist? transect?

setnames(bel2, names(bel2), 
         c("species_scientific_name", "date", "entry_date", "count", "activity",
           "record_type", 
           "stage", "sex", "x_coord", "y_coord", "location", 
           "municipality", "submunicipality", "province",
           "data_source", "status", "embargo_date", "permission_observer",
           "notes", "precision", "id_checklist", "transect", "type_transect"))

# bel2[, ":="(day = mday(as.IDate(date, format="%Y/%m/%d")), 
#             month = month(as.IDate(date, format="%Y/%m/%d")),
#             year = year(as.IDate(date, format="%Y/%m/%d")))]

bel2[, ":="(x_coord = as.numeric(gsub("\\,", "\\.", x_coord)), 
            y_coord = as.numeric(gsub("\\,", "\\.", y_coord)))]
bel2[, dataset := "bel2"]

# Join 2 belgium dataframes
bel <- rbind(bel1, bel2, fill = TRUE)

## Cyprus -----

### Cyprus corrested -----
cyp1 <- fread("data/data_raw/Cyprus corrected.csv")
cyp1 <- setnames(cyp1, names(cyp1), 
                 c("grid_ref_number", "species_scientific_name", "stage", 
                   "count", "larvae", "exuviae", "day", "month", "year", 
                   "y_coord", "x_coord", "record_type",
                   "accuracy", "country", "recorder", "location", 
                   "comment", "location_name", "recorders"))
# Difference between recorder and recorders?

cyp1[, y_coord := gsub("\\xff", "", gsub("\\,", "\\.", y_coord, 
                                         useBytes = TRUE), useBytes = TRUE)]
cyp1[, x_coord := gsub("\\xff", "", gsub("\\,", "\\.", x_coord, 
                                         useBytes = TRUE), useBytes = TRUE)]
# cyp1[, date := as.IDate(paste(year, month, day, sep = "-", 
#                               format = "%Y-%m-%d"))]

### Other Cyprus files -----

lf <- list.files("data/data_raw/Cyprus Data dragonflies")

cyp2 <- data.table()
for(i in seq_along(lf)){
    cyp_data <- data.table(read_excel(file.path("data", "data_raw",
                                                "Cyprus Data dragonflies", lf[i]), 
                                      col_types = "text",
                                      sheet = "Sheet1"))
    # cyp_data[1356, Exuviae]
    cyp_data <- setnames(cyp_data, names(cyp_data), 
                         c("grid_ref_number", "species_scientific_name", 
                           "date", "day", "month", "year", "stage", "count", 
                           "larvae", "exuviae", "x_coord", "y_coord", 
                           "longitude_ms", "latitude_ms", 
                           "location_name", "altitutde_m"))
    cyp_data[, y_coord := gsub("\\xff", "", 
                               gsub("\\,", "\\.", y_coord, useBytes = TRUE), 
                               useBytes = TRUE)]
    cyp_data[, x_coord := gsub("\\xff", "", 
                               gsub("\\,", "\\.", x_coord, useBytes = TRUE), 
                               useBytes = TRUE)]
    
    cyp2 <- rbind(cyp2, cyp_data)
}

# Correct X
cyp2[larvae == "X" | exuviae == "X", c("larvae", "exuviae") := 1]

# Bind Cyprus datasets
cyp <- rbind(cyp1, cyp2, fill = TRUE)
cyp[, dataset := "cyp"]
# cyp[, ":="(x_coord = as.numeric(x_coord), y_coord = as.numeric(y_coord))]
# Doesn't work bc trailing/leading spaces and sometimes points

# nas <- which(is.na(as.numeric(cyp2$x_coord)))
# cyp2[nas, x_coord]

## Netherlands -----
# Records table
ned <- data.table(readRDS("data/data_raw/Dutchdata2023.RDS"))
ned <- setnames(ned, names(ned), 
                c("record_url", "species_names_nl", "year", "month", "day", 
                  "date_scale", "x", "y", "location_scale", "count", "stage", 
                  "data_source"))
# What is datum_schaal?

ned_coord <- data.table(st_coordinates(
  st_transform(st_as_sf(ned, coords = c("x", "y"), 
                        crs = 28992), 4326)))

ned[, ":="(x_coord = ned_coord$X, y_coord=ned_coord$Y, crs=4326)]

# Species table
sp_ned <- fread("data/data_raw/Soortcodes_nl_sci.csv")
setnames(sp_ned, 
         c("srt_scientific","srt_Nednaam"), 
         c("species_scientific_name", "species_names_nl"))

sp_ned[, species_names_nl := tolower(species_names_nl)]
ned <- merge(ned, sp_ned[, .(species_scientific_name, species_names_nl)], 
             by = "species_names_nl", all.x = TRUE)
ned[, dataset := "ned"]

## France -----

### STELI -----
fr_steli <- fread("data/data_raw/STELI_data_FR_DMS.csv")

# Filter taxonomic level and columns
# fr_steli <- fr_steli[rang %in% c("ES", "SSES"), 
#                      c("taxon", "date", "duree", "insee_commune", "nom_commune", 
#                        "effectif", "id_observateur", "protocole",
#                        "id_site", "nom_site", "lon centroid site", "lat centroid site" )]
sp_list <- data.table(taxon = fr_steli[, unique(taxon)])
sp_list[, species_scientific_name := str_trim(gsub("\\(", "", 
                                        substr(sp_list$taxon, 1, 
                                        sapply(gregexpr("[A-Z]", sp_list$taxon), `[`, 2)-1)))]
fr_steli <- merge(fr_steli, sp_list, by = "taxon")
fr_steli[, .(taxon, species_scientific_name, rang)]

# setnames(fr_steli, names(fr_steli), c("species_scientific_names_long", "date", "duration", "region_id",
#                     "region_name", "count", "recorder_id", "monitoring_scheme", "location_id", "location_name",
#                     "x_coord", "y_coord", "species_scientific_name"))
setnames(fr_steli, names(fr_steli), 
         str_replace_all(names(fr_steli), " ", "_"))

fr_steli[, ":="(day = mday(as.IDate(date, format = "%d/%m/%Y")), 
                month = month(as.IDate(date, format = "%d/%m/%Y")), 
                year = year(as.IDate(date, format = "%d/%m/%Y")))]
fr_steli[, dataset := "fr_steli"]

### Opie -----

fr_opie <- fread("data/data_raw/France Opportunistics data (Opie)/odonata_202410091558.csv")

# fr_opie <- fr_opie[, .(taxon, latitude, longitude, commune, date, observateur)]
sp_list <- data.table(taxon = fr_opie[, unique(taxon)])

sp_list[, species_scientific_name := str_trim(gsub("\\(", "", 
                                                   substr(sp_list$taxon, 1, 
                                                          sapply(gregexpr("[A-Z]", sp_list$taxon), `[`, 2)-1)))]

fr_opie <- merge(fr_opie, sp_list, by = "taxon")

# setnames(fr_opie, names(fr_opie),
#          c("species_scientific_names_long", "y_coord", 
#            "x_coord", "region_nae", "date", 
#            "recorder_name","species_scientific_name"))
setnames(fr_opie, names(fr_opie), 
         str_replace_all(names(fr_opie), " ", "_"))

fr_opie[, ":="(day = mday(as.IDate(date)), 
               month = month(as.IDate(date)), 
               year = year(as.IDate(date)))]

fr_opie[, dataset := "fr_opie"]


# Merge data --------------------------------------------------------------

all_data <- rbind(aus[!is.na(x_coord) & !is.na(y_coord), .(dataset, x_coord, y_coord, species_scientific_name, year)], 
                bel[!is.na(x_coord) & !is.na(y_coord), .(dataset, x_coord, y_coord, species_scientific_name, year)], 
                ned[!is.na(x_coord) & !is.na(y_coord), .(dataset, x_coord, y_coord, species_scientific_name, year)],
                cyp[!is.na(x_coord) & !is.na(y_coord), .(dataset, x_coord, y_coord, species_scientific_name, year)],
                fr[!is.na(x_coord) & !is.na(y_coord), .(dataset, x_coord, y_coord, species_scientific_name, year)],
                fr_steli[!is.na(x_coord) & !is.na(y_coord), .(dataset, x_coord, y_coord, species_scientific_name, year)])

all_data <- all_data[ , x_coord := as.numeric(x_coord)]
all_data <- all_data[ , y_coord := as.numeric(y_coord)]

all_data_sf <- st_as_sf(all_data[!is.na(x_coord) & !is.na(y_coord) & x_coord != "" & y_coord != "", ], coords = c("x_coord", "y_coord"), crs = 4326)
all_data_sf <- st_transform(all_data_sf, 3035)
plot(st_geometry(all_data_sf))

# Map ---------------------------------------------------------------------

library(rnaturalearth)
library(terra)
library(ggplot2)
library(ggspatial)

library(sf)

eu <- ne_countries(scale = 50, continent = "Europe", returnclass = "sf")
af <- ne_countries(scale = 50, continent = "Africa", returnclass = "sf")
as <- ne_countries(scale = 50, continent = "Asia", returnclass = "sf")

eu_box <- st_make_grid(st_bbox(ext(c(-20, 52, 27, 74)), crs = 4326), n = 1, what = "polygons")
eu_sf <- st_make_valid(st_crop(st_make_valid(rbind(eu, af, as)), st_buffer(eu_box, 0)))
eu_3035 <- st_make_valid(st_transform(eu_sf[, "sov_a3"], crs = 3035))

## define grid hexagon
grid_scale <- 50000
terrestrial_eu_grid <- st_make_grid(st_make_grid(st_bbox(st_buffer(eu_3035, grid_scale)), n = 1, what = "polygons"), 
                                cellsize = grid_scale, 
                                what = "polygons", 
                                square = FALSE, 
                                flat_topped = TRUE)
terrestrial_eu_grid1 <- terrestrial_eu_grid[unique(unlist(st_intersects(eu_3035, terrestrial_eu_grid)))]

hex_transect_density <- data.table(hex_id = seq_along(terrestrial_eu_grid1))
transect_density <- data.table(hex_with = unlist(st_intersects(
  all_data_sf[as.integer(all_data_sf$year)>=2014,], 
  terrestrial_eu_grid1)))[, .N, by = hex_with]

hex_transect_density1 <- merge(hex_transect_density, 
                               transect_density, 
                               by.x = "hex_id", by.y = "hex_with", 
                               all.x = TRUE)
hex_transect_density1[is.na(N), N := 0]
hex_transect_density1_sf <- st_sf(unique(hex_transect_density1), 
                                  terrestrial_eu_grid1)

main_data <- st_read("data/data_map/main_map.shp")

my_breaks <- seq(from = 1, to = 14, by = 2)
hex_transect_density1_sf[hex_transect_density1_sf$N > 0, ]$N <- log(hex_transect_density1_sf[hex_transect_density1_sf$N > 0, ]$N)

record_density_map <- ggplot() +
                        geom_sf(data = main_data, fill = "grey95", colour = NA) +
                        geom_sf(data = hex_transect_density1_sf[hex_transect_density1_sf$N > 0, ], aes(fill = N), colour = NA) +
                        geom_sf(data = main_data, fill = NA, colour = "grey5", lwd = 0.2) +
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
                              axis.text.x = element_blank(),
                              panel.grid = element_line(color = "white", linewidth = 0.3, linetype = 1),
                              legend.background = element_rect(fill = "transparent"),
                              legend.key = element_rect(fill = "transparent"),
                              legend.position = c(0.15, 0.68),
                              legend.text = element_text(face = "bold", size = 10),
                              legend.title = element_text(face = "bold", size = 10),
                              axis.text.y = element_text(size = 10))

record_density_map

ggsave("outputs/dragonfly_data_density_map.png", record_density_map, dpi = 300)
