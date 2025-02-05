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
cols <- colnames_DB(con, "Date", rm_ID = TRUE)

cols[cols == "date"] <- "eventDate"

dtdf <- lapply(dat, df_all_cols, cols = cols)

dtdf <- do.call("rbind",
                c(dtdf, fill = TRUE))
dtdf <- unique(dtdf)
dtdf <- rm_all_na(dtdf)

setnames(dtdf, old = "eventDate", new = "date")

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

lodf <- lapply(dat, df_all_cols, cols = cols)

# Get Natural Earth data
countries <- ne_countries(continent = "Europe") |>
  select(name_long, admin) |>
  rename(country = name_long)
countries <- st_make_valid(countries)

regions <- ne_states(country = countries$name) |>
  select(name_en, admin) |>
  rename(county = name_en)
regions <- st_make_valid(regions)

for (i in seq_along(dat)) {
  pts <- dat[[i]]
  # Add an ID to find back NA coordinates
  pts[, ID := 1:nrow(pts)]

  # Convert to sf
  pts_coord <- pts[decimalCoordinates != "POINT EMPTY"]

  if (nrow(pts_coord) != 0) { # If some pts have coordinates
    pts_coord <- st_as_sf(pts_coord, wkt = "decimalCoordinates")
    st_crs(pts_coord) <- 4326

    # Get country and county points are in
    pts_coord <- st_intersection(pts_coord,
                                 countries)
    pts_countries <- unique(pts_coord$admin)
    pts_regions <- regions |>
      filter(admin %in% pts_countries) |>
      select(-admin)
    pts_coord <- st_intersection(pts_coord,
                                 pts_regions)
    pts_coord <- pts_coord |> select(-admin)

    # Format data
    pts_coord <- data.table(pts_coord)
    pts_coord <- pts_coord[, .(ID, country, county)]

    pts <- pts_coord[pts, on = "ID"]
    dat[[i]] <- pts[, ID := NULL]
  }
  # Else do nothing

}

lapply(dat, head)
lapply(dat, function(d) any(is.na(d$country)))

# library(ggplot2)
# ggplot() +
#   geom_sf(data = countries, fill = "grey75") +
#   geom_sf(data = pts) +
#   xlim(-20, 40) + ylim(30, 70)


# Format data -------------------------------------------------------------

# Add country
countries <- c("Austria",
               "Belgium",
               "Belgium",
               "Cyprus",
               "Cyprus",
               "Netherlands",
               "France",
               "France")
names(countries) <- names(dat)


## parentDataset ------------------------------------------------------



# Not sure for Belgium1/2 data and Austria
dat_info[, samplingProtocol := c("transect",
                                 "opportunistic",
                                 "opportunistic",
                                 "opportunistic",
                                 "opportunistic",
                                 "site counts",
                                 "site counts",
                                 "opportunistic",
                                 "opportunistic")]
