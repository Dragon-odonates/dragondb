# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2024-12-17
#
# Script Description: format data for database and add columns



# Libraries ---------------------------------------------------------------
library(here)
library(data.table)

library(RPostgres)

library(readxl)

library(rvest) # read htlm
library(stringr)

# For spatial info
library(rnaturalearth)
library(sf)
library(dplyr)

library(dragondb)

read_folder <- here("data/03_data_clean/subset")

populate_db <- TRUE # run the code that populates the db?

# Read data ---------------------------------------------------------------

ls <- list.files(read_folder,
                 full.names = TRUE)
ls <- grep(pattern = "tmp$", ls, invert = TRUE, value = TRUE)
nam <- gsub("\\.csv$", "", basename(ls))

dat <- lapply(ls,
              fread,
              header = TRUE,
              na.strings = c("", "NA"),
              nrows = 10,
              sep = ",")
names(dat) <- nam

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
# Check DB
dbListTables(con)

# dbDisconnect(con)

# Atomize data & copy to DB -----------------------------------------------

## Species -----
colnames <- colnames_DB(con, "Taxon", rm_ID = FALSE)

spdf <- lapply(dat, function(d) d[, ..colnames])

spdf <- do.call("rbind",
                c(spdf, fill = TRUE))

spdf <- unique(spdf)
spdf <- na.omit(spdf)

if (populate_db) {
  dbAppendTable(con, "Taxon", spdf)
}

## Recorder -----
cols <- c("recordedBy", "recordedByID")

### Get recorder name for Nl -----
recorder_css_class <- ".app-content-title"

recorderID <- unique(na.omit(dat$Netherlands$recordedByID))

for (i in 1:length(recorderID)) {
  page <- recorderID[i]
  txt <- read_html(page)

  observer_name <- txt |>
    html_node(css = recorder_css_class) |>
    html_text(trim = TRUE)

  dat$Netherlands[recordedByID == page, recordedBy := observer_name]
}

redf <- lapply(dat, df_all_cols, cols = cols)

redf <- lapply(redf, unique)
redf <- lapply(redf, rm_all_na)

redf <- do.call("rbind",
                c(redf, fill = TRUE))

setnames(redf,
         old = c("recordedBy", "recordedByID"),
         new = c("name", "recorderID_orig"))

if (populate_db) {
  dbAppendTable(con, "Recorder", redf)
}

### Add recorder columns in dat -----
rec_db <- dbGetQuery(con, 'SELECT "recorderID", "recorderID_orig" AS "recordedByID",
                           "name" AS "recordedBy"
                            FROM "Recorder";')
rec_db <- data.table(rec_db)

dat <- lapply(dat,
              function(d) {
                # Add columns to data if not present
                cols_to_add <- cols[!(cols %in% colnames(d))]
                d[, (cols_to_add) := NA]

                # Join
                res <- join_dt_na(d, rec_db, cols1 = cols)
                res[, (cols_to_add) := NULL]
              })

lapply(dat, function(d) any(is.na(d$recorderID)))
# Belgium1,2 and Cyprus1,2 have NAs because they have no observer

## EventDate -----
cols <- colnames_DB(con, "EventDate", rm_ID = TRUE)

dtdf <- lapply(dat, df_all_cols, cols = cols)
dtdf <- lapply(dtdf, function(d) d[, eventTime := as.ITime(eventTime)])

dtdf <- do.call("rbind",
                c(dtdf, fill = TRUE))
dtdf <- unique(dtdf)
dtdf <- rm_all_na(dtdf)

if (populate_db) {
  dbAppendTable(con, "EventDate", dtdf)
}

### Add date ID column in dat -----
dt_db <- dbGetQuery(con, 'SELECT * FROM "EventDate";')
dt_db <- data.table(dt_db)

dat <- lapply(dat,
              function(d) {
                # Add columns to data if not present
                cols_to_add <- cols[!(cols %in% colnames(d))]
                d[, (cols_to_add) := NA]

                # Join
                res <- join_dt_na(d, dt_db, cols1 = cols)
                res[, (cols_to_add) := NULL]
              })

lapply(dat, function(d) any(is.na(d$eventDateID)))
# No NAs dates \o/

## Dataset -----

# Get datasets IDs
dadf <- lapply(dat, df_all_cols, c("datasetID", "parentDatasetID"))
dadf <- lapply(dadf, unique)
dadf <- do.call("rbind",
                dadf)

# Add parents as standalone datasets
add_par <- na.omit(unique(dadf$parentDatasetID))
add_par <- data.table(datasetID = add_par,
                      parentDatasetID = NA)

dadf <- rbind(add_par, dadf)

# Add datasets infos
datinfo <- data.table(read_excel(here("data/metadata/metadata.xlsx"),
                                 sheet = 1))

datmerge <- datinfo[, .(datasetID, isParentDataset, datasetName,
                        description)]

dadf <- datmerge[dadf, on = "datasetID"]

if (populate_db) {
  dbAppendTable(con, "Dataset", dadf)
}

## Contact -----
cols <- colnames_DB(con, "Contact", rm_ID = FALSE)

contacts_df <- data.table(read_excel(here("data/metadata/metadata.xlsx"),
                                     sheet = 2))
contacts_datasets <- data.table(read_excel(here("data/metadata/metadata.xlsx"),
                                           sheet = 3))

# Get only contacts for datasets in DB ---
# Get datasets in DB
dadf_db <- dbGetQuery(con, 'SELECT * FROM "Dataset";')
dadf_db <- data.table(dadf_db)

# Get contacts that are for this dataset
co_indb <- contacts_datasets[datasetID %in% dadf_db$datasetID, ]
contacts_df <- contacts_df[contactID %in% co_indb$contactID, ]

codf <- df_all_cols(contacts_df, cols = cols)

if (populate_db) {
  dbAppendTable(con, "Contact", codf)
}

## DatasetContact -----
dcdf <- contacts_datasets[datasetID %in% dadf_db$datasetID, ]

cols <- colnames_DB(con, "DatasetContact", rm_ID = FALSE)
dcdf <- df_all_cols(dcdf, cols = cols)

if (populate_db) {
  dbAppendTable(con, "DatasetContact", dcdf)
}

## Location -----
cols <- colnames_DB(con, "Location", rm_ID = TRUE)

cols2 <- cols
cols2[cols2 == "country"] <- "verbatimCountry"
cols2[cols2 == "county"] <- "verbatimCounty"

### Prepare data -----
# Get a subset of data with only relevant columns
lodf <- lapply(dat, df_all_cols, cols = cols2)
lodf <- lapply(lodf, unique)

# Add parent coordinates
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
                  })

# Add parents as standalone coordinates
for (n in names_par) {
  lodf[[n]] <-  rbind(par_loc[[n]], lodf[[n]], fill = TRUE)
}

# Get Natural earth countries/counties
geo <- ne_download(scale = 10, type = "states")
geo <- geo[, c("admin", "name_en")]
names(geo) <- c("country", "county", "geometry")
geo <- st_make_valid(geo)

for (i in seq_along(lodf)) {
  dati <- lodf[[i]]

  # Add an ID to find back NA coordinates
  dati[, ID := 1:nrow(dati)]

  # Get non-empty coordinates
  pts_coord <- dati[grep("EMPTY", decimalCoordinates, invert = TRUE), ]

  if (nrow(pts_coord) != 0) { # If some data have coordinates

    # Convert to sf
    pts_coord <- st_as_sf(pts_coord, wkt = "decimalCoordinates")
    st_crs(pts_coord) <- 4326

    # Get indices of country and county points are in
    geo_inter <- st_intersects(pts_coord, geo)

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
    pts_coord <- data.table(pts_coord)

    # Set country/county values for points with coordinates (same order)
    pts_coord[, country := geo_inter$country]
    pts_coord[, county := geo_inter$county]

    # Merge new info to complete info where points have no coords
    pts_coord <- pts_coord[, .(country, county, ID)]
    dati <- pts_coord[dati, on = "ID"]

  } else { # There are no coordinates
    # Replace values with values in data
    dati[, country := verbatimCountry]
    dati[, county := verbatimCounty]
  }

  # Remove temporary ID
  dati[, ID := NULL]

  # Set list element
  lodf[[i]] <- dati

}

# # Sanity check
# lapply(lodf, function(d) head(d[, .(decimalCoordinates,
#                                    country,
#                                    county)]))
#
# pts_plot <- lapply(lodf,
#                    function(d) d[, .(decimalCoordinates, country)])
# pts_plot <- lapply(seq_along(pts_plot),
#                    function(i) pts_plot[[i]][, src := names(pts_plot)[i]])
# pts_plot <- do.call("rbind", pts_plot)
# pts_plot <- st_as_sf(pts_plot, wkt = "decimalCoordinates")
# st_crs(pts_plot) <- 4326
#
# bbox <- st_bbox(pts_plot)
#
# # Get Natural Earth contries
# countries <- ne_countries(continent = c("Europe", "Asia"),
#                           scale = 10)
# countries <- st_make_valid(countries)
#
# library(ggplot2)
# ggplot() +
#   geom_sf(data = countries, fill = "grey75") +
#   geom_sf(data = pts_plot, aes(color = src),
#           size = 1, show.legend = "point") +
#   scale_color_viridis_d() +
#   xlim(bbox["xmin"], bbox["xmax"]) +
#   ylim(bbox["ymin"], bbox["ymax"])

lodf <- do.call("rbind", lodf)

# Remove verbatim
lodf <- df_all_cols(lodf, cols)

if (populate_db) {
  dbAppendTable(con, "Location", lodf)
}


### Add locationID to dat -----
# get locationID ---
# Complete request with country/county (some joins need these values)
loc_db_nocoord <- dbGetQuery(con, 'SELECT "locationID",
                             ST_AsText("decimalCoordinates") AS "decimalCoordinates",
                             "coordinateUncertaintyInMeters",
                             "locality", "municipality", "county", "country"
                             FROM "Location";')
loc_db_nocoord <- data.table(loc_db_nocoord)

# Standardize wkt
loc_db_nocoord[, decimalCoordinates := std_coord_text(decimalCoordinates)]

# Remove county/country (for joins that don't need it)
loc_db_coord <- copy(loc_db_nocoord[, c("county", "country") := NULL])

# Get columns to make join on
# If dat has coordinates
cols_coord <- c("decimalCoordinates",
                "coordinateUncertaintyInMeters",
                "locality", "municipality")
# If dat has no coordinates
# For original data
cols1_nocoord <- c(cols_coord, c("verbatimCountry", "verbatimCounty"))
# For db
cols2_nocoord <- c(cols_coord, c("country", "county"))

datnames <- names(dat)

dat <- lapply(names(dat),
               function(n) {
                 print(paste(n, "------------"))
                 # In all cases: add location ID for event

                 # Standardize wkt
                 dat[[n]][, decimalCoordinates := std_coord_text(decimalCoordinates)]

                 # Join
                 dat_na <- copy(dat[[n]][is.na(decimalCoordinates), ])
                 dat_coord <- copy(dat[[n]][!is.na(decimalCoordinates), ])

                 if (nrow(dat_na) != 0) { # for NA coordinates
                   # Add columns to data, including verbatimCountry/County
                   cols_to_add <- cols1_nocoord[!(cols1_nocoord %in% colnames(dat_na))]
                   dat_na[, (cols_to_add) := NA]

                   res1 <- join_dt_na(dat_na,
                                      loc_db_nocoord,
                                      cols1 = cols1_nocoord,
                                      cols2 = cols2_nocoord)
                 } else {
                   res1 <- data.table()
                 }
                 if (nrow(dat_coord) != 0) { # for non-na coordinates
                   # Add columns to data, without verbatimCountry/County
                   cols_to_add <- cols_coord[!(cols_coord %in% colnames(dat_coord))]
                   dat_coord[, (cols_to_add) := NA]

                   res2 <- join_dt_na(dat_coord,
                                      loc_db_coord,
                                      cols1 = cols_coord)
                   res2[, (cols_to_add) := NULL]
                 } else {
                   res2 <- data.table()
                 }

                 res <- rbind(res1, res2)

                 if ( any(is.na(res$eventLocationID)) ) {
                   warning("There are NA locations IDs in dataset ", n, " :(")
                 }

                 if ("parentCoordinates" %in% names(res)) { # If dataset has parentCoord
                   # Also add location for parent

                   # Standardize wkt
                   res[, parentCoordinates := std_coord_text(parentCoordinates)]

                   # Rename parent coordinates (to avoid conflicts with names already in data)
                   loc_db_par <- loc_db_coord[, .(locationID, decimalCoordinates)]
                   setnames(loc_db_par,
                            old = c("decimalCoordinates", "locationID"),
                            new = c("parentCoordinates", "parentLocationID"))
                   # Join
                   # We assume parent coordinates to have coordinates
                   res <- join_dt_na(res,
                                     loc_db_par,
                                     cols1 = "parentCoordinates")

                   if ( any(is.na(res$parentLocationID)) ) {
                       warning("There are NA parent locations IDs in dataset ", n, " :(")
                   }

                 }
                 return(res)
               })
names(dat) <- datnames

## Event -----
# Get columns names that are used to get table
cols <- colnames_DB(con, "Event", rm_ID = TRUE)
cols <- cols[cols!= "parentEventID"]
cols <- c(cols, "parentLocationID")

evdf <- lapply(dat, df_all_cols, cols = cols)

### Reformat to add parent event -----

# IDs of datasets that have parents
names_par <- names(which(sapply(dat,
                                function(d) !all(is.na(d$parentLocationID)))))

# Add parent events and isParent
evdf <- lapply(names(evdf),
               function(n) {
                 if (n %in% names_par) { # if this dataset has parent events
                   # Add parents as standalone events
                   res <- unique(evdf[[n]][, .(parentLocationID, datasetID)])
                   setnames(res,
                            old = "parentLocationID",
                            new = "locationID")

                   res[, isParentEvent := TRUE] # set isParent to TRUE for parents
                   evdf[[n]][, isParentEvent := FALSE] # FALSE for others

                   # Add parent events
                   rbind(res, evdf[[n]], fill = TRUE)

                   } else { # dataset has no parent
                     # All these events have no parent and are not parents
                     evdf[[n]][, isParentEvent := FALSE]
                     evdf[[n]][, parentLocationID := NA]
                   }
                 })
names(evdf) <- datnames

evdf <- lapply(evdf, df_all_cols, cols = cols)

evdf <- data.table(do.call("rbind", evdf))

evdf <- unique(evdf)

if (populate_db) {
  # Add parent events
  evdf_par <- evdf[isParentEvent == TRUE, ]
  evdf_par <- evdf_par[, parentLocationID := NULL] # remove LocationID column

  dbAppendTable(con, "Event", evdf_par)

  # Get parent event IDs
  par_id <- dbGetQuery(con, 'SELECT "eventID" AS "parentEventID",
                             "Event"."locationID" AS "parentLocationID"
                              FROM "Event"
                              LEFT JOIN "Location" ON "Event"."locationID" = "Location"."locationID";')
  par_id <- data.table(par_id)

  # Add parentEventID to children events
  evdf_nopar <- evdf[isParentEvent == FALSE, ]

  # Join on parent location
  evdf_nopar <- par_id[evdf_nopar,
                       on = "parentLocationID"]
  evdf_nopar <- evdf_nopar[, parentLocationID := NULL]

  dbAppendTable(con, "Event", evdf_nopar)
}

### Add event ID to dat -----

ev_db <- dbGetQuery(con, 'SELECT "eventID", "locationID", "eventDateID",
                          "recorderID", "datasetID", "eventType", "parentEventID",
                          "isParentEvent"
                           FROM "Event";')
ev_db <- data.table(ev_db)

# Remove parent events because we don't want to join them
ev_db <- ev_db[isParentEvent == FALSE, ]
ev_db[, isParentEvent := NULL] # also don(t need this column)

cols <- colnames(ev_db)
cols <- cols[!(cols %in% c("eventID", "parentEventID"))]

dat <- lapply(dat,
              function(d) {
                # Add columns to data if not present
                cols_to_add <- cols[!(cols %in% colnames(d))]
                d[, (cols_to_add) := NA]

                # Join
                res <- join_dt_na(d, ev_db, cols1 = cols)
                res[, (cols_to_add) := NULL]
              })

lapply(dat, function(d) any(is.na(d$eventID)))
# No events are NA

## Occurrence -----
cols <- colnames_DB(con, "Occurrence", rm_ID = TRUE)

oc_df <- lapply(dat, df_all_cols, cols)
oc_df <- do.call("rbind", oc_df)
oc_df <- unique(oc_df)

if (populate_db) {
  dbAppendTable(con, "Occurrence", oc_df)
}
