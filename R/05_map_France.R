# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2025-01-07
#
# Script Description: map French data


# Libraries ---------------------------------------------------------------
library(here)
library(data.table)

# For spatial info
library(rnaturalearth)
library(sf)
library(dplyr)

library(ggplot2)
library(scales)

library(dragondb)

read_folder <- here("data/03_data_clean")


# Read data ---------------------------------------------------------------
ls <- list.files(read_folder,
                 full.names = TRUE)
ls <- grep(pattern = "France", ls, value = TRUE)
nam <- gsub("\\.csv$", "", basename(ls))

dat <- lapply(ls,
              fread,
              header = TRUE,
              sep = ",")


# Format data -------------------------------------------------------------
dat[[1]][, source := "OPIE"]
dat[[2]][, source := "STELI"]

dat <- lapply(dat,
              function(d) d[, .(scientificName, taxonID,
                                decimalCoordinates,
                                eventDate, source)])


dat <- do.call("rbind", dat)

# Add regions -------------------------------------------------------------

regions <- ne_states(country = "France") |>
  select(name, region, type) |>
  rename(department = name)
# regions <- ne_download(scale = 10,
#                        type = "admin_1_states_provinces",
#                        category = "cultural")
# regions <- regions |> filter(admin == "France")

regions <- st_make_valid(regions)

# lim <- st_bbox(regions |> filter(type == "Metropolitan département"))
#
# plot(regions,
#      ylim = lim[c(2, 4)],
#      xlim = lim[c(1, 3)])

# Intersect ---------------------------------------------------------------

# Convert to sf
dat <- na.omit(dat, cols = "decimalCoordinates")
dat <- st_as_sf(dat, wkt = "decimalCoordinates")
st_crs(dat) <- 4326

# Get centroid
# st_geometry(dat) <- st_centroid(dat)

# Get regions points are in
# datsub <- dat[sample(1:nrow(dat), size = 1000), ]

# plot(st_geometry(datsub))
# plot(st_geometry(regions), add = TRUE)

dat <- st_intersection(dat, regions)
st_geometry(datsub)[!st_geometry(datsub) %in% st_geometry(datsub2)]

missing <- datsub[!(st_geometry(datsub) %in% st_geometry(datsub2)),]

plot(st_geometry(missing), col = "red", pch = 16)
plot(st_geometry(regions), add = TRUE)


# Summarize ---------------------------------------------------------------

regions_metro <- regions |>
  filter(type == "Metropolitan département")

# By region
dat_region <- datsub2 |>
  st_drop_geometry() |>
  group_by(region) |>
  summarize(nobs = n()) |>
  distinct()

regions_df <- st_sfc()
st_crs(regions_df) <- 4326

for (r in unique(regions_metro$region)) {
  # Get region polygon
  un <- st_union(regions_metro |> filter(region == r))

  # Add region name
  res <- st_sf(region = r, geom = un)

  # Add to df
  regions_df <- rbind(regions_df, res)
}

# Add nobs
regions_df <- regions_df |>
  left_join(dat_region, by = "region")


# By department
dat_dpt <- datsub2 |>
  st_drop_geometry() |>
  group_by(department) |>
  summarize(nobs = n())

dpt_df <- regions_metro |>
  left_join(dat_dpt, by = "department")

# Plot --------------------------------------------------------------------

# Regions
ggplot(regions_df) +
  geom_sf(aes(fill = nobs),
          show.legend = FALSE) +
  scale_fill_gradient(low = "#bbfff1", high = "#004b45") +
  geom_sf_text(aes(label = nobs,
                   col = nobs > mean(nobs, na.rm = TRUE)),
               show.legend = FALSE) +
  scale_color_manual(values = c("black", "white")) +
  theme_void()

# Departments
ggplot(dpt_df) +
  geom_sf(aes(fill = nobs),
          show.legend = FALSE) +
  scale_fill_gradient(low = "#bbfff1", high = "#004b45") +
  geom_sf_text(aes(label = nobs,
                   col = nobs > mean(nobs, na.rm = TRUE)),
               show.legend = FALSE) +
  scale_color_manual(values = c("black", "white")) +
  theme_void()
