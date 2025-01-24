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
              nrows = 15,
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

## Observer -----
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

dtdf <- lapply(dat, df_all_cols, cols = cols)

dtdf <- do.call("rbind",
                c(dtdf, fill = TRUE))
dtdf <- unique(dtdf)
dtdf <- rm_all_na(dtdf)

dbAppendTable(con, "Date", dtdf)

## Parent dataset -----
padf <- data.table(parentDatasetName = c("Belgium2",
                                         "France_OPIE"))

dbAppendTable(con, "ParentDataset", padf)

## Dataset -----
cols <- colnames_DB(con, "Dataset", rm_ID = TRUE)

dadf <- lapply(dat, df_all_cols, cols = cols)
dadf <- lapply(dadf, unique)
dadf <- lapply(dadf, as.data.table)

# Get parent dataset
padf_db <- dbGetQuery(con, 'SELECT *
                    FROM "ParentDataset";')
padf_db <- data.table(padf_db)

dadf <- lapply(seq_along(dadf),
               function(i) {
                 datname <- names(dadf)[i]
                 df <- dadf[[i]]

                 df[, parentDataset := as.numeric(parentDataset)]

                 if (all(is.na(df$datasetName)) ) { # There is no dataset name
                   df[, datasetName := datname] # use list names
                 } else { # There are sub-datasets -> add parent dataset
                   parent_ID <- padf_db[parentDatasetName == datname,
                                        parentDatasetID]
                   df[, parentDataset := parent_ID]
                 }
               })

dadf <- do.call("rbind",
                dadf)

# Add sampling protocol
datinfo <- data.table(read_excel(
  here("data/metadata/02_modified/Datasets_review.xlsx")))

# Add parent datasets IDs
pa_in_dat <- padf_db[parentDatasetID %in% unique(dadf$parentDataset),]
datinfo <- pa_in_dat[datinfo,
                     on = "parentDatasetName"]

# Complete sampling info
dadf[, samplingProtocol := as.character(samplingProtocol)]

for (i in 1:nrow(dadf)) {
  ds <- dadf[i, datasetName]

  # TO FIX LATER
  if (ds == "Cyprus2") {
    ds <- "Cyprus1"
  }

  if (!is.null(ds)) {
    dii <- datinfo_ds[datasetName == ds,]

    # TO FIX LATER
    if (ds == "Belgium1") {
      dii <- dii[1, ]
    }
  } else {
    dii <- data.frame()
  }

  if (nrow(dii) != 0) { # Case dataset has a sampling protocol
    dadf[i, samplingProtocol := dii$samplingProtocol]
  } else { # dataset doesn't have a sampling protocol: use parent
    pds <- dadf[i, parentDataset]
    dii <- datinfo[parentDatasetID == pds,]

    if (nrow(dii) == 0) {
      warning("No sampling found for dataset ", ds)
    }
    dadf[i, samplingProtocol := dii$samplingProtocol]
  }
}

dbAppendTable(con, "Dataset", dadf)

## Contact -----
cols <- colnames_DB(con, "Contact", rm_ID = TRUE)

contacts_df <- data.table(read_excel(here("data/metadata/01_raw/contacts.xlsx"),
                                     sheet = 1))

contacts_df$ID <- 1:nrow(contacts_df)

# Separate datasets ---
# Separate names
split <- lapply(contacts_df$datasetName, str_split, pattern = ", ")
split <- lapply(split, unlist)

# Format to df
split <- data.table(do.call("rbind", split))
split[, ID := contacts_df$ID]
# Wide to long
split <- melt(split, id.vars = "ID",
              value.name = "datasetName")
split[, variable := NULL]
split <- split[!is.na(datasetName), ] # remove empty values
split <- unique(split) # remove duplicates introduced by rbind

contacts_df[, datasetName := NULL] # rm datasetNames

contacts_df <- split[contacts_df, on = "ID"] # merge to get duplicated contacts

# Get only contacts in DB
padf_db <- dbGetQuery(con, 'SELECT * FROM "ParentDataset";')
padf_db <- data.table(padf_db)
dadf_db <- dbGetQuery(con, 'SELECT * FROM "Dataset";')
dadf_db <- data.table(dadf_db)

contacts_df <- contacts_df[(datasetName %in% dadf_db$datasetName) |
                           (parentDatasetName %in% padf_db$parentDatasetName),]

codf <- df_all_cols(contacts_df, cols = cols)

codf <- unique(codf)

dbAppendTable(con, "Contact", codf)


## DatasetContact -----

# Get datasets in DB
dadf_db <- data.table(dbGetQuery(con, 'SELECT "datasetID", "datasetName"
                                 FROM "Dataset";'))

# Add dataset ID to contacts table
cols <- c("datasetID", "datasetName",
          "contactName", "contactEmail")
dcdf <- contacts_df[dadf_db,
                on = "datasetName", ..cols]

# Add contacts ID to contacts
codf_db <- data.table(dbGetQuery(con, 'SELECT "contactID", "contactName", "contactEmail"
                                 FROM "Contact";'))

dcdf <- codf_db[dcdf,
                on = c("contactName", "contactEmail")]
dcdf <- na.omit(dcdf, cols = "contactID") # some datasets have no contact

# Insert in table
dcdf <- dcdf[, .(datasetID, contactID)]

setnames(dcdf,
         old = c("datasetID", "contactID"),
         new = c("dataset", "contact"))

dbAppendTable(con, "DatasetContact", dcdf)

## ParentDatasetContact -----

# Get parent datasets in DB
pddf_db <- data.table(dbGetQuery(con, 'SELECT "parentDatasetID", "parentDatasetName"
                                 FROM "ParentDataset";'))

# Add parents dataset ID to contacts
cols <- c("parentDatasetID", "parentDatasetName",
          "contactName", "contactEmail")
dcdf <- contacts_df[pddf_db,
                    on = "parentDatasetName", ..cols]

# Add contacts ID to contacts
dcdf <- codf_db[dcdf,
                on = c("contactName", "contactEmail")]
dcdf <- na.omit(dcdf, cols = "contactID") # some parent datasets have no contact

# Insert in table
dcdf <- dcdf[, .(parentDatasetID, contactID)]

setnames(dcdf,
         old = c("parentDatasetID", "contactID"),
         new = c("parentDataset", "contact"))

dbAppendTable(con, "ParentDatasetContact", dcdf)

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
  pts_coord <- na.omit(pts, cols = "decimalCoordinates")
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
