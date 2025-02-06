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


# Replace all NA values with "__NA__"

#' Replace `NA` values
#'
#' Replace `NA` values in a data.table with a placeholder
#'
#' @param dt the data.table
#' @param SDcols the columns in which to replace values
#' @param placeholder the placeholder
#' @param rev reverse (i.e. replace placeholder back with `NA`).
#'
#' @returns the data.table where NA values are replaced with `placeholder`
#' (or the reverse if `rev` is `TRUE`)
#' @export
replace_NA <- function(dt, SDcols, placeholder = "__NA__", rev = FALSE) {

  if (!rev) {
    dt[, (names(.SD)) := lapply(.SD,
                                function(x) fifelse(is.na(x), placeholder, x)),
       .SDcols = SDcols]
  } else {
    dt[, (names(.SD)) := lapply(.SD,
                                function(x) fifelse(x == placeholder, NA, x)),
       .SDcols = SDcols]
  }

}

# Read data ---------------------------------------------------------------

ls <- list.files(read_folder,
                 full.names = TRUE)
ls <- grep(pattern = "tmp$", ls, invert = TRUE, value = TRUE)
nam <- gsub("\\.csv$", "", basename(ls))

dat <- lapply(ls,
              fread,
              header = TRUE,
              na.strings = c("", "NA"),
              # nrows = 15,
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

dbAppendTable(con, "Taxon", spdf)

## Recorder -----
cols <- c("recordedBy", "recordedByID")

obdf <- lapply(dat, df_all_cols, cols = cols)

obdf <- lapply(obdf, unique)
obdf <- lapply(obdf, rm_all_na)

# Get observer name for Nl
observer_css_class <- ".app-content-title"

obdf$Netherlands[, recordedBy := as.character(recordedBy)]

for (i in 1:nrow(obdf$Netherlands)) {
  page <- obdf$Netherlands$recordedByID[i]
  txt <- read_html(page)

  observer_name <- txt |>
    html_node(css = observer_css_class) |>
    html_text(trim = TRUE)
  obdf$Netherlands[i, recordedBy := observer_name]
}

obdf <- do.call("rbind",
                c(obdf, fill = TRUE))

setnames(obdf,
         old = c("recordedBy", "recordedByID"),
         new = c("name", "recorderID_orig"))

dbAppendTable(con, "Recorder", obdf)


## Date -----
cols <- colnames_DB(con, "EventDate", rm_ID = TRUE)

dtdf <- lapply(dat, df_all_cols, cols = cols)
dtdf <- lapply(dtdf, function(d) d[, eventTime := as.ITime(eventTime)])

dtdf <- do.call("rbind",
                c(dtdf, fill = TRUE))
dtdf <- unique(dtdf)
dtdf <- rm_all_na(dtdf)

dbAppendTable(con, "EventDate", dtdf)

## Dataset -----
cols <- colnames_DB(con, "Dataset", rm_ID = FALSE)
cols <- cols[cols != "description"]

datinfo <- data.table(read_excel(here("data/metadata/metadata.xlsx"),
                                 sheet = 1))

datinfo_par <- datinfo[isParentDataset == TRUE, ]
datinfo_nopar <- datinfo[isParentDataset == FALSE, ]

# Add dataset/parent datasets to dat

ind_nopar <- which(names(dat) %in% datinfo_nopar$datasetName)

lapply(seq_along(dat),
       function(i) {
         if (i %in% ind_nopar) { # This dataset has no children datasets
           # Get dataset name and ID
           dati <- datinfo_nopar[datasetName == names(dat)[i],
                                 .(datasetID, datasetName)]

           # Set name and ID in dat
           dat[[i]][, datasetName := dati$datasetName]
           dat[[i]][, datasetID := dati$datasetID]

           # Not a parent dataset
           dat[[i]][, isParentDataset := FALSE]
         } else { # This dataset has children datasets
           # Get dataset ID
           id <- datinfo_par[datasetName == names(dat)[i], datasetID]

           # Set ID in dat
           dat[[i]][, parentDataset :=  id]

           # Is a parent dataset
           dat[[i]][, isParentDataset := TRUE]
         }
       })

dadf <- lapply(dat, df_all_cols, cols)
dadf <- lapply(dadf, unique)
dadf <- do.call("rbind",
                dadf)

# Add description
datmerge <- datinfo[, c("datasetID", "description")]

dadf <- datmerge[dadf,
                 on = "datasetID"]


# Generate missing datasets IDs
# get parents datasets for datasets with no ID
parNA <- dadf[is.na(datasetID), parentDataset]

if(any(is.na(parNA))) {
  warning("There are IDless datasets with no parent.")
}

# Generate children datasets on the model parent's first letter + X + number
did <- paste0(str_sub(parNA, 1, 1), "X")
did <- tapply(did, INDEX = did,
              function(d) {
                sq <- seq(1, length(d))
                str_pad(sq, 2, pad = "0")
              })

did <- lapply(seq_along(did),
              function(i) paste0(names(did)[i], did[[i]]))

did <- unlist(did)

dadf[is.na(datasetID), datasetID := did]

# Add missing IDs to dat
ind_par <- which(names(dat) %in% datinfo_par$datasetName)

lapply(ind_par,
       function(i){
         d <- dat[[i]]
         for (k in 1:nrow(d)) {
           namk <- d[k, ]$datasetName
           pk <- d[k, ]$parentDataset
           idk <- dadf[datasetName == namk & parentDataset == pk, ]$datasetID
           dat[[i]][k, datasetID := idk]
         }
       })

# Add parents datasets
cols2 <- c("datasetID", "datasetName", "description", "isParentDataset")
datinfo_par_add <- datinfo_par[datasetID %in% dadf$parentDataset,
                               ..cols2]

dadf <- rbind(datinfo_par_add, dadf, fill = TRUE)

dbAppendTable(con, "Dataset", dadf)

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

dbAppendTable(con, "Contact", codf)

## DatasetContact -----
dcdf <- contacts_datasets[datasetID %in% dadf_db$datasetID, ]

setnames(dcdf,
         old = c("datasetID", "contactID"),
         new = c("dataset", "contact"))
cols <- colnames_DB(con, "DatasetContact", rm_ID = FALSE)
dcdf <- df_all_cols(dcdf, cols = cols)

dbAppendTable(con, "DatasetContact", dcdf)

## Location -----
cols <- colnames_DB(con, "Location", rm_ID = TRUE)
cols <- cols[!(cols %in% c("county", "country"))] # Remove computed values

lodf <- lapply(dat, df_all_cols, cols = cols)

# Get Natural earth countries/counties
geo <- ne_download(scale = 10, type = "states")
geo <- geo[, c("admin", "name_en")]
names(geo) <- c("country", "county", "geometry")
geo <- st_make_valid(geo)

for (i in seq_along(lodf)) {
  dati <- lodf[[i]]
  # Add an ID to find back NA coordinates
  dati[, ID := 1:nrow(dati)]

  # Convert to sf
  pts_coord <- dati[decimalCoordinates != "POINT EMPTY",]

  if (nrow(pts_coord) != 0) { # If some data have coordinates
    pts_coord <- st_as_sf(pts_coord, wkt = "decimalCoordinates")
    st_crs(pts_coord) <- 4326

    # Get country and county points are in
    geo_inter <- st_intersects(pts_coord, geo)

    geo_inter <- lapply(geo_inter,
                        function(g) data.table(st_drop_geometry(geo[g,])))

    pb_geo <- which(sapply(geo_inter, nrow) != 1)

    if (length(pb_geo) != 0) {
      warning("Some points belong to non-unique units")

      for (k in pb_geo) {
        if (nrow(geo_inter[[k]]) == 0) { # No matching unit
          # Set to NA
          geo_inter[[k]] <- data.table(country = NA,
                                       county = NA)
        } else { # Several units
          countries <- geo_inter[[k]]$country
          ucountry <- unique(countries)

          if (length(ucountry) == 1) {
            # Set country to ucountry and county to NA
            geo_inter[[k]] <- data.table(country = ucountry,
                                         county = NA)
          } else {
            # Set to NA
            geo_inter[[k]] <- data.table(country = NA,
                                         county = NA)
          }
        }
      }
    }

    # Get countries for each point
    geo_inter <- do.call("rbind", geo_inter)

    pts_coord <- data.table(pts_coord) # convert to datatable again

    pts_coord[, country := geo_inter$country]
    pts_coord[, county := geo_inter$county]

    # Format data
    pts_coord <- pts_coord[, .(country, county, ID)] # get relevant data for merge
    dati <- pts_coord[dati, on = "ID"] # merge data for which country/county
    # have been retrieved
  } else { # There are no coordinates in the dataset
    dati[, c("country", "county") := NA] # Set computed values to NA
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

lodfu <- unique(lodf)

# For the moment this removes data with only county or country
# To amend after (problem is some Cyprus data don't have coordinates)
lodfu <- lodfu[decimalCoordinates != "POINT EMPTY",]

dbAppendTable(con, "Location", lodfu)

## Event -----
cols <- colnames_DB(con, "Event", rm_ID = TRUE)

# Remove columns that we will get from joins
cols <- cols[!(cols %in% c("location", "eventDate", "recorder", "dataset"))]
# Add columns necessary for joins
cols <- c("decimalCoordinates", "eventDate", "eventTime", "eventDateUncertainty",
          "recordedBy", "recordedByID",
          "datasetID",
          cols)

evdf <- lapply(dat, df_all_cols, cols = cols)

# Need to get locationID, dateID, recorderID, datasetID

# get locationID ---
loc_db <- dbGetQuery(con, 'SELECT "locationID",
                    ST_AsText("decimalCoordinates") AS "decimalCoordinates"
                    FROM "Location";')
loc_db <- st_as_sf(loc_db, wkt = "decimalCoordinates")

evdf <- lapply(evdf,
               function(d) st_as_sf(d, wkt = "decimalCoordinates"))

evdf <- lapply(evdf,
               function(d) data.table(st_drop_geometry(d |> st_join(loc_db))))


# Check IDs were retrieved
lapply(evdf, function(d) !any(is.na(d$locationID)))
# All good except Cyprus that has NA coord

# get dateID ---
dat_db <- dbGetQuery(con, 'SELECT "eventDateID", "eventDate", "eventTime", "eventDateUncertainty"
                    FROM "EventDate";')
dat_db <- data.table(dat_db)

cols <- c("eventDate", "eventTime", "eventDateUncertainty")

# Convert to character to prepare for placeholder
dat_db[, names(.SD) := lapply(.SD, as.character),
       .SDcols = cols]

lapply(evdf,
       function(e) {
         e[, names(.SD) := lapply(.SD, as.character),
             .SDcols = cols]
       })

# Replace NAs
lapply(evdf, replace_NA, SDcols = cols)
replace_NA(dat_db,
           SDcols = cols)


evdf <- lapply(evdf,
               function(e) {
                 dat_db[e,
                        on = c("eventDate",
                               "eventTime",
                               "eventDateUncertainty")]
                 })

lapply(evdf, replace_NA,
       SDcols =  cols,
       rev = TRUE)

# Check IDs are here
lapply(evdf, function(d) !any(is.na(d$eventDateID)))
# Ok except Cyprus1 again

# get recorderID
rec_db <- dbGetQuery(con, 'SELECT "recorderID", "recorderID_orig", "name"
                     FROM "Recorder";')
rec_db <- data.table(rec_db)

dbcols <- c("recorderID_orig", "name")
dfcols <- c("recordedBy", "recordedByID")

# Convert to character to prepare for placeholder
rec_db[, names(.SD) := lapply(.SD, as.character),
       .SDcols = dbcols]

lapply(evdf,
       function(e) {
         e[, names(.SD) := lapply(.SD, as.character),
           .SDcols = dfcols]
       })

# Replace NAs
lapply(evdf, replace_NA, SDcols = dfcols)
replace_NA(rec_db,
           SDcols = dbcols)

evdf <- lapply(evdf,
               function(e) {
                 rec_db[e,
                        on = c(name = "recordedBy",
                               recorderID_orig = "recordedByID")]
               })

lapply(evdf, replace_NA,
       SDcols = dbcols,
       rev = TRUE)

# Check IDs are here
lapply(evdf, function(d) !any(is.na(d$recorderID)))


# Get datasets IDs ---
# They were already added to dat after Dataset insertion

# Check IDs are here
lapply(evdf, function(d) !any(is.na(d$datasetID)))
# All IDs have been found

lapply(evdf,
       setnames,
       old = c("locationID", "eventDateID", "recorderID", "datasetID"),
       new = c("location", "eventDate", "recorder", "dataset"))

cols <- colnames_DB(con, "Event", rm_ID = TRUE)

evdf <- lapply(evdf, df_all_cols, cols = cols)

evdf <- data.table(do.call("rbind", evdf))

evdfu <- unique(evdf)

dbAppendTable(con, "Event", evdfu)

## Occurrence -----
cols <- colnames_DB(con, "Occurrence", rm_ID = TRUE)

cols <- cols[!(cols %in% c("location", "date", "recorder", "dataset"))]
cols <- c("decimalCoordinates", "eventDate", "eventTime", "eventDateUncertainty",
          "recordedBy", "recordedByID",
          "datasetName",
          cols)

# Need to get ID for event, taxon, location, date

