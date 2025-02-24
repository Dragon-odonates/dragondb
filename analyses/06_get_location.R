# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2025-02-21
#
# Script Description: get admin info for locations in the data



# Librairies --------------------------------------------------------------
library(here)

library(data.table)
library(rnaturalearth)
library(sf)

read_folder <- here("data/03_data_clean")

# Read data ---------------------------------------------------------------
ls <- list.files(read_folder,
                 full.names = TRUE)
ls <- grep(pattern = "tmp$", ls, invert = TRUE, value = TRUE)
nam <- gsub("\\.csv$", "", basename(ls))

dat <- lapply(ls,
              fread,
              header = TRUE,
              na.strings = c("", "NA"),
              # nrow = 100000,
              sep = ",")
names(dat) <- nam


# Get unique coordinates --------------------------------------------------

## For sites -----
lodf <- lapply(dat,
              function(d) unique(d[, .(decimalCoordinates)]))
lodf <- lapply(lodf,
               function(d) d[, isParentCoordinate := FALSE])

## For parent sites -----
# IDs of datasets that have at least one parent coordinates
names_par <- names(which(sapply(dat,
                                function(d) !all(is.na(d$parentCoordinates)))))

# Get these datasets
par_loc <- copy(dat[names_par]) # For now it's only France_STELI

# Format parent coordinates
par_loc <- lapply(par_loc,
                  function(d) {
                    res <- unique(d[, .(parentCoordinates)])
                    setnames(res,
                             old = "parentCoordinates",
                             new = "decimalCoordinates")
                    res[, isParentCoordinate := TRUE]
                  })


# Add parents as standalone coordinates
for (n in names_par) {
  lodf[[n]] <-  rbind(par_loc[[n]], lodf[[n]], fill = TRUE)
}

# Get only non-empty coordinates
lodf <- lapply(lodf,
               function(d) d[grep("EMPTY", decimalCoordinates, invert = TRUE), ])
lodf <- lapply(names(lodf),
               function(nam) lodf[[nam]][, dataset := nam])

lodf <- rbindlist(lodf, use.names = TRUE)

# Query -------------------------------------------------------------------

# Get Natural earth countries/counties
geo <- ne_download(scale = 10, type = "states")
geo <- geo[, c("admin", "name_en")]
names(geo) <- c("country", "county", "geometry")
geo <- st_make_valid(geo)

# Convert to sf
lodf <- st_as_sf(lodf, wkt = "decimalCoordinates")
st_crs(lodf) <- 4326
lodf <- st_make_valid(lodf,
                      s2_options = s2::s2_options(simplify_edge_chains = TRUE))

# v <- (st_is_valid(lodf, reason = TRUE))
# unique(v)
# lodf[which(v != "Valid Geometry"),]
#
# g <- st_geometry(lodf[which(v != "Valid Geometry"),])
# plot(g[[1]])

# Use planar geometry to avoid error
# https://github.com/r-spatial/sf/issues/1902
sf_use_s2(FALSE)

# Get indices of country and county points are in
system.time(geo_inter <- st_intersects(lodf, geo))

# Get values, and drop geom
geo_inter <- lapply(geo_inter,
                    function(g) data.table(st_drop_geometry(geo[g,])))

# Check for issues
pb_geo <- which(sapply(geo_inter, nrow) != 1)
if (length(pb_geo) != 0) {
  warning("Some points belong to non-unique units")

  # Try to (partially) resolve issue
  for (k in pb_geo) {
    if (nrow(geo_inter[[k]]) == 0) { # No matching unit
      # Set to NA
      geo_inter[[k]] <- data.table(country = NA,
                                   county = NA)
    } else { # Several units
      countries <- geo_inter[[k]]$country
      ucountry <- unique(countries)

      if (length(ucountry) == 1) { # Several counties, but unique country
        # Set country to ucountry and county to NA
        geo_inter[[k]] <- data.table(country = ucountry,
                                     county = NA)
      } else { # Several countries
        # Set to NA
        geo_inter[[k]] <- data.table(country = NA,
                                     county = NA)
      }
    }
  }
}

# List to data.table
geo_inter <- do.call("rbind", geo_inter)
lodf <- data.table(lodf)
lodf[, decimalCoordinates := st_as_text(decimalCoordinates)]

# Set country/county values for points with coordinates (same order)
lodf[, country := geo_inter$country]
lodf[, county := geo_inter$county]

# Write data --------------------------------------------------------------

write.table(lodf,
            file = here("data/precomputed/country.csv"),
            row.names = FALSE,
            qmethod = "double",
            sep = ",")

