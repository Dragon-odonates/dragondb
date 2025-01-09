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

library(rgbif)

library(dragondb)

read_folder <- here("data/03_data_clean")
fig_path <- here("outputs/05_France")

# Prepare data ---------------------------------------------------------------
## Read data ----------------------------------------------------------------

ls <- list.files(read_folder,
                 full.names = TRUE)
ls <- grep(pattern = "France", ls, value = TRUE)
nam <- gsub("\\.csv$", "", basename(ls))

dat <- lapply(ls,
              fread,
              header = TRUE,
              sep = ",")

## Format data -------------------------------------------------------------
dat[[1]][, source := "OPIE"]
dat[[2]][, source := "STELI"]

dat <- lapply(dat,
              function(d) d[, .(scientificName, taxonID,
                                decimalCoordinates,
                                eventDate, source)])


dat <- do.call("rbind", dat)

## Get regions -------------------------------------------------------------

dpt <- ne_states(country = "France") |>
  select(name, region, type) |>
  rename(department = name)
# dpt <- ne_download(scale = 10,
#                        type = "admin_1_states_provinces",
#                        category = "cultural")
# dpt <- dpt |> filter(admin == "France") |>
#   select(name, region, type) |>
#   rename(department = name)

dpt <- st_make_valid(dpt)

## Intersect ---------------------------------------------------------------

# Convert to sf
dat <- na.omit(dat, cols = "decimalCoordinates")
dat <- st_as_sf(dat, wkt = "decimalCoordinates")
st_crs(dat) <- 4326

# Get centroid
# st_geometry(dat) <- st_centroid(dat)

# Get regions points are in
id <- st_intersects(dat, dpt)
id[sapply(id, function(i) length(i) == 0)] <- NA
id <- unlist(id)

rmerge <- st_drop_geometry(dpt[id, ])

dat <- cbind(dat, rmerge)


# saveRDS(object = dat, file = here("data/05_France/data.rds"))
# dat <- readRDS(here("data/05_France/data.rds"))


## Add NAs ---------------------------------------------------------------

# Select NA values
dat_na <- dat[which(is.na(dat$region)), ] |>
  select(-c(region, type, department))
nrow(dat_na)

dpt_metro <- dpt |>
  filter(type == "Metropolitan département")

ggplot(dat_na) +
  geom_sf(data = dpt_metro) +
  geom_sf(color = "red") +
  theme_void()

# Add buffer around departments to select NAs
reg_buff <- st_buffer(dpt, dist = 500)

id <- st_intersects(dat_na, reg_buff)
id[sapply(id, function(i) length(i) != 1)] <- NA
# != 1 because with buffers some points are in several departments
id <- unlist(id)

rmerge <- st_drop_geometry(reg_buff[id, ])

dat_na <- cbind(dat_na, rmerge)

# Replace NAs
dat[which(is.na(dat$region)), ] <- dat_na

# Plot final NAs
sum(is.na(dat$region))

dat_na <- dat[which(is.na(dat$region)), ] |>
  select(-c(region, type, department))

ggplot(dat_na) +
  geom_sf(data = dpt_metro) +
  geom_sf(color = "cornflowerblue") +
  theme_void()

# saveRDS(object = dat, file = here("data/05_France/data_nas.rds"))

## Read pre-treated data -------------------------------------

dat <- readRDS(here("data/05_France/data_nas.rds"))

dpt <- ne_states(country = "France") |>
  select(name, region, type) |>
  rename(department = name)

dpt <- st_make_valid(dpt)

# Prepare data -------------------------------------------------
dpt_metro <- dpt |> filter(type == "Metropolitan département")

# Get administrative regions
regions <- st_sfc()
st_crs(regions) <- 4326

for (r in unique(dpt$region)) {
  # Get region polygon
  dpt_in_reg <- dpt |> filter(region == r)
  un <- st_union(dpt_in_reg)

  # Add region name
  res <- st_sf(region = r,
               type = unique(dpt_in_reg$type),
               geom = un)

  # Add to df
  regions <- rbind(regions, res)
}

regions_metro <- regions |>
  filter(type == "Metropolitan département")

# Analyze/plot ---------------------------------------------------------------

## Plot nobs maps --------------------------------------------------------------------

# By region
dat_regions <- dat |>
  st_drop_geometry() |>
  group_by(region) |>
  filter(!is.na(scientificName) & !is.na(region)) |>
  summarize(nobs = n()) |>
  distinct()

# Add nobs
dat_regions <- regions_metro |>
  left_join(dat_regions, by = "region")

# By department
dat_dpt <- dat |>
  st_drop_geometry() |>
  group_by(department) |>
  filter(!is.na(scientificName) & !is.na(region)) |>
  summarize(nobs = n())

dat_dpt <- dpt_metro |>
  left_join(dat_dpt, by = "department")

# lim <- st_bbox(dpt |> filter(type == "Metropolitan département"))
#
# plot(dpt,
#      ylim = lim[c(2, 4)],
#      xlim = lim[c(1, 3)])


# Regions
ggplot(dat_regions) +
  geom_sf(aes(fill = nobs),
          show.legend = FALSE) +
  scale_fill_gradient(low = "#bbfff1", high = "#004b45",
                      transform = "log10") +
  geom_sf_text(aes(label = format(nobs, big.mark = " "),
                   col = log10(nobs) > max(log10(nobs), na.rm = TRUE)/2),
               show.legend = FALSE,
               size = 6) +
  scale_color_manual(values = c("black", "white")) +
  theme_void()

ggsave(file.path(fig_path, "map_regions.png"),
       width = 30, height = 30, units = "cm", dpi = 300)


# Departments
ggplot(dat_dpt) +
  geom_sf(aes(fill = nobs),
          show.legend = FALSE) +
  scale_fill_gradient(low = "#bbfff1", high = "#004b45",
                      na.value = "grey90",
                      transform = "log10") +
  geom_sf_text(aes(label = format(nobs, big.mark = " "),
                   col = log10(nobs) > max(log10(nobs), na.rm = TRUE)/2),
               show.legend = FALSE,
               size = 4) +
  scale_color_manual(values = c("black", "white"),
                     na.value = "transparent") +
  theme_void()

ggsave(file.path(fig_path, "map_dpt.png"),
       width = 30, height = 30, units = "cm", dpi = 300)


## By program -----------------------------------------------------------

df_source <- st_drop_geometry(dat) |>
  group_by(source, department) |>
  summarize(nobs = n())

df_source <- dpt_metro |>
  right_join(df_source, by = "department")

ggplot(df_source) +
  facet_grid(cols = vars(source)) +
  geom_sf(data = regions_metro, fill = "grey90") +
  geom_sf(aes(fill = nobs),
          show.legend = FALSE) +
  scale_fill_gradient(low = "#bbfff1", high = "#004b45",
                      transform = "log10") +
  geom_sf_text(aes(label = format(nobs, big.mark = " "),
                   col = log10(nobs) > max(log10(nobs), na.rm = TRUE)/2),
               show.legend = FALSE,
               size = 2) +
  scale_color_manual(values = c("black", "white")) +
  theme_void()

ggsave(file.path(fig_path, "map_opie_vs_steli.png"),
       width = 30, height = 15, units = "cm", dpi = 300)

st_drop_geometry(df_source) |>
  group_by(source) |>
  summarise(nobs = sum(nobs))


## Species stats -----------------------------------------------------------

backbone_check <- name_backbone_checklist(unique(dat$scientificName))

sp_backbone <- backbone_check |>
  filter(rank %in% c("SPECIES", "SUBSPECIES"))

dat_sp <- dat |>
  filter(!is.na(scientificName)) |>
  filter(scientificName %in% sp_backbone$verbatim_name)

(sp_df <- st_drop_geometry(dat_sp) |>
  group_by(scientificName) |>
  summarize(nobs = n()) |>
   arrange(desc(nobs)))

nrow(sp_df[!is.na(sp_df$scientificName), ])

sp_df_plot <- sp_df[1:20, ]

ggplot(sp_df_plot) +
  geom_col(aes(y = reorder(scientificName, nobs), x = nobs),
           fill = "darkorange") +
  scale_x_continuous(expand = c(0,0)) +
  xlab("Occurrences") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(face = "italic",
                                   size = 8))

ggsave(file.path(fig_path, "sp_occ.png"),
       width = 12, height = 8, units = "cm", dpi = 300)

## Species maps -----------------------------------------------------


# By region
dat_regions <- dat_sp |>
  st_drop_geometry() |>
  group_by(region) |>
  filter(!is.na(scientificName) & !is.na(region)) |>
  summarize(nsp = length(unique(scientificName))) |>
  distinct()

# Add nobs
dat_regions <- regions_metro |>
  left_join(dat_regions, by = "region")

# By department
dat_dpt <- dat_sp |>
  st_drop_geometry() |>
  group_by(department) |>
  filter(!is.na(scientificName) & !is.na(region)) |>
  summarize(nsp = length(unique(scientificName)))

dat_dpt <- dpt_metro |>
  left_join(dat_dpt, by = "department")

(sp_in_regions <- dat_sp |>
  st_drop_geometry() |>
  group_by(region, scientificName) |>
  filter(!is.na(scientificName) & !is.na(region)) |>
  summarize() |>
  arrange(region, scientificName))


# Regions
ggplot(dat_regions) +
  geom_sf(aes(fill = nsp),
          show.legend = FALSE) +
  scale_fill_gradient(low = "#fadeff", high = "#42054d") +
  geom_sf_text(aes(label = nsp,
                   col = nsp > max(nsp)/2),
               show.legend = FALSE,
               size = 8) +
  scale_color_manual(values = c("black", "white")) +
  theme_void()

ggsave(file.path(fig_path, "map_regions_nsp.png"),
       width = 30, height = 30, units = "cm", dpi = 300)


# Departments
ggplot(dat_dpt) +
  geom_sf(aes(fill = nsp),
          show.legend = FALSE) +
  scale_fill_gradient(low = "#fadeff", high = "#42054d",
                      na.value = "grey90") +
  geom_sf_text(aes(label = nsp,
                   col = nsp > max(nsp, na.rm = TRUE)/2),
               show.legend = FALSE,
               size = 4) +
  scale_color_manual(values = c("black", "white")) +
  theme_void()

ggsave(file.path(fig_path, "map_dpt.png"),
       width = 30, height = 30, units = "cm", dpi = 300)

