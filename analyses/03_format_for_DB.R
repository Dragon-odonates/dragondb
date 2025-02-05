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
# cols <- colnames_DB(con, "Date", rm_ID = TRUE)

cols <- c("eventDate", "eventTime", "eventDateUncertainty")

dtdf <- lapply(dat, df_all_cols, cols = cols)
dtdf <- lapply(dtdf, function(d) d[, eventTime := as.ITime(eventTime)])

dtdf <- do.call("rbind",
                c(dtdf, fill = TRUE))
dtdf <- unique(dtdf)
dtdf <- rm_all_na(dtdf)

setnames(dtdf,
         old = c("eventDate", "eventTime", "eventDateUncertainty"),
         new = c("date", "time", "dateUncertainty"))

dbAppendTable(con, "Date", dtdf)

## Parent dataset -----
datinfo <- data.table(read_excel(here("data/metadata/metadata.xlsx"),
                                 sheet = 1))

# Get parent datasets
padf <- datinfo[isParentDataset == TRUE, ]

# Get parent datasets in data
padf <- padf[datasetName %in% names(dat), ]

# Get only relevant columns
cols <- colnames_DB(con, "ParentDataset", rm_ID = FALSE)
setnames(padf,
         old = c("datasetID", "datasetName"),
         new = cols)
padf <- df_all_cols(padf, cols)

dbAppendTable(con, "ParentDataset", padf)

## Dataset -----
dadf <- lapply(dat, df_all_cols,
               cols = c("datasetName", "parentDataset"))
dadf <- lapply(dadf, unique)
dadf <- lapply(dadf, as.data.table)

# Get parent dataset
padf_db <- dbGetQuery(con, 'SELECT *
                    FROM "ParentDataset";')
padf_db <- data.table(padf_db)

# Set dataset names :
# We set names only for list elements that are not parent
# (because parents already have a dataset name)
ind_nopar <- which(!(names(dadf) %in% padf_db$parentDatasetName))
lapply(ind_nopar,
       function(i) dadf[[i]][, datasetName := names(dadf)[i] ])

# Set parent datasets:
# We do that only for list elements that are parents (the others don't
# have parents)
ind_par <- which(names(dadf) %in% padf_db$parentDatasetName)
lapply(ind_par,
       function(i) {
         id <- padf_db[parentDatasetName == names(dadf)[i],
                       parentDatasetID]
         dadf[[i]][, parentDataset :=  as.character(parentDataset)]
         dadf[[i]][, parentDataset :=  id]
       })

dadf <- do.call("rbind",
                dadf)

# Add dataset info at dataset level
cols <- colnames_DB(con, "Dataset")

datmerge <- datinfo[, c("datasetID", "datasetName", "samplingProtocol",
                        "description")]

dadf <- datmerge[dadf,
                 on = "datasetName"]

# Generate missing datasets IDs
parNA <- dadf[is.na(datasetID), parentDataset] # get parents datasets for datasets
# with no ID

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


# Replace empty info (sampling, description) for some datasets with parents"
ind_par <- which(!is.na(dadf$parentDataset)) # get indexs of datasets to check

for (i in ind_par) {
  par <- dadf[i, parentDataset] # get parent's ID
  if (is.na(dadf[i, samplingProtocol])) { # protocol is empty
    # Replace with parent's
    par_sp <- datinfo[datasetID == par, samplingProtocol]
    dadf[i, samplingProtocol := par_sp]
  }

  if (is.na(dadf[i, description])) { # description is empty
    # Replace with parent's
    par_de <- datinfo[datasetID == par, description]
    dadf[i, description := par_de]
  }
}

dbAppendTable(con, "Dataset", dadf)

## Contact -----
cols <- colnames_DB(con, "Contact", rm_ID = FALSE)

contacts_df <- data.table(read_excel(here("data/metadata/metadata.xlsx"),
                                     sheet = 2))
contacts_datasets <- data.table(read_excel(here("data/metadata/metadata.xlsx"),
                                           sheet = 3))

# Get only contacts for datasets in DB ---
padf_db <- dbGetQuery(con, 'SELECT * FROM "ParentDataset";')
padf_db <- data.table(padf_db)
dadf_db <- dbGetQuery(con, 'SELECT * FROM "Dataset";')
dadf_db <- data.table(dadf_db)

co_db <- contacts_datasets[(datasetID %in% c(padf_db$parentDatasetID, dadf_db$datasetID)), ]
contacts_df <- contacts_df[contactID %in% co_db$contactID, ]

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

## ParentDatasetContact -----
pcdf <- contacts_datasets[datasetID %in% padf_db$parentDatasetID, ]

setnames(pcdf,
         old = c("datasetID", "contactID"),
         new = c("parentDataset", "contact"))
cols <- colnames_DB(con, "ParentDatasetContact", rm_ID = FALSE)
pcdf <- df_all_cols(pcdf, cols = cols)

dbAppendTable(con, "ParentDatasetContact", pcdf)


## Location -----
cols <- colnames_DB(con, "Location", rm_ID = TRUE)
cols <- cols[!(cols %in% c("county", "country"))] # Remove computed values

lodf <- lapply(dat, df_all_cols, cols = cols)

# Get Natural Earth contries
countries <- ne_countries(continent = c("Europe", "Asia")) |>
  select(name_long, admin) |>
  rename(country = name_long)
countries <- st_make_valid(countries)


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
    country_inter <- st_intersects(pts_coord, countries)

    pb_countries <- which(sapply(country_inter, length) != 1)
    if (length(pb_countries) != 0) {
      warning("Some points belong to non-unique countries")
      country_inter[pb_countries] <- NA
    }
    country_inter <- unlist(country_inter)
    country_inter <- countries[country_inter, ]$admin

    pts_coord$country <- country_inter

    # Get unique countries
    ucountries <- unique(pts_coord$country)

    # Get regions for this country
    regions_i <- ne_states(country = ucountries) |>
      select(name_en, admin) |>
      rename(county = name_en)
    regions_i <- st_make_valid(regions_i)

    region_inter <- st_intersects(pts_coord, regions_i)

    pb_regions <- which(sapply(region_inter, length) != 1)
    if (length(pb_regions) != 0) {
      warning("Some points belong to non-unique regions")
      region_inter[pb_regions] <- NA
    }

    region_inter <- unlist(region_inter)
    region_inter <- regions_i[region_inter, ]$county

    pts_coord$county <- region_inter

    # Format data
    pts_coord <- data.table(pts_coord) # convert to datatable again
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
# lapply(dat, function(d) head(d[, .(decimalCoordinates,
#                                    country,
#                                    county)]))
#
# # library(ggplot2)
#
# pts <- lapply(dat, function(d) d[, .(decimalCoordinates, country)])
# pts <- do.call("rbind", pts)
# pts <- st_as_sf(pts, wkt = "decimalCoordinates")
# st_crs(pts) <- 4326
#
# bbox <- st_bbox(pts)
# ggplot() +
#   geom_sf(data = countries, fill = "grey75") +
#   geom_sf(data = pts, aes(col = country)) +
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

cols <- cols[!(cols %in% c("location", "date", "recorder", "dataset"))]
cols <- c("decimalCoordinates", "eventDate", "eventTime", "eventDateUncertainty",
          "recordedBy", "recordedByID",
          "datasetName",
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
dat_db <- dbGetQuery(con, 'SELECT "dateID", "date", "time", "dateUncertainty"
                    FROM "Date";')
dat_db <- data.table(dat_db)

dbcols <- c("date", "time", "dateUncertainty")
dfcols <- c("eventDate", "eventTime", "eventDateUncertainty")

# Convert to character to prepare for placeholder
dat_db[, names(.SD) := lapply(.SD, as.character),
       .SDcols = dbcols]

lapply(evdf,
       function(e) {
         e[, names(.SD) := lapply(.SD, as.character),
             .SDcols = dfcols]
       })

# Replace NAs
lapply(evdf, replace_NA, SDcols = dfcols)
replace_NA(dat_db,
           SDcols = dbcols)


evdf <- lapply(evdf,
               function(e) {
                 dat_db[e,
                        on = c("date" = "eventDate",
                               "time" = "eventTime",
                               "dateUncertainty" = "eventDateUncertainty")]
                 })

lapply(evdf, replace_NA,
       SDcols =  dbcols,
       rev = TRUE)

# Check IDs are here
lapply(evdf, function(d) !any(is.na(d$dateID)))
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

# We use padf_db and dadf_db computed above for dataset insertion

# Get list elements that are not parent and add their dataset ID
ind_nopar <- which(!(names(evdf) %in% padf_db$parentDatasetName))

lapply(ind_nopar,
       function(i) {
         ds_name <- names(evdf)[i]
         evdf[[i]][, datasetID := dadf_db[datasetName == ds_name, datasetID]]
       })

# Get list elements that are parent and add their parent dataset ID
# and datasetID
ind_par <- which(names(evdf) %in% padf_db$parentDatasetName)
lapply(ind_par,
       function(i) {
         # Add parent
         ps_name <- names(evdf)[i]
         evdf[[i]][, parentDatasetID := padf_db[parentDatasetName == ps_name, parentDatasetID]]
         # Add dataset
         ds_id <- lapply(1:nrow(evdf[[i]]),
                         function(k) {
                           nam <- evdf[[i]][k, datasetName]
                           dadf_db[datasetName == nam, datasetID]
                         })
         ds_id <- unlist(ds_id)
         evdf[[i]][, datasetID := ds_id]
       })


# Check IDs are here
lapply(evdf, function(d) !any(is.na(d$datASETid)))
# All IDs have been found

lapply(evdf,
       setnames,
       old = c("locationID", "dateID", "recorderID", "datasetID"),
       new = c("location", "date", "recorder", "dataset"))

cols <- colnames_DB(con, "Event", rm_ID = TRUE)

evdf <- lapply(evdf, df_all_cols, cols = cols)

evdf_df <- data.table(do.call("rbind", evdf))

evdf_dfu <- unique(evdf_df)

dbAppendTable(con, "Event", evdf_dfu)

## Occurrence -----
cols <- colnames_DB(con, "Occurrence", rm_ID = TRUE)

cols <- cols[!(cols %in% c("location", "date", "recorder", "dataset"))]
cols <- c("decimalCoordinates", "eventDate", "eventTime", "eventDateUncertainty",
          "recordedBy", "recordedByID",
          "datasetName",
          cols)

# Need to get ID for event, taxon, location, date
