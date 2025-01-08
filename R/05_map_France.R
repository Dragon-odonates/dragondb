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

library(doParallel)
library(foreach)

read_folder <- here("data/03_data_clean")
fig_path <- here("outputs/05_France")

detectCores()
registerDoParallel(cores = detectCores()-2)

# Read data all.tests = # Read data ---------------------------------------------------------------
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
# regions <- regions |> filter(admin == "France") |>
#   select(name, region, type) |>
#   rename(department = name)

regions <- st_make_valid(regions)

# Intersect ---------------------------------------------------------------

# Convert to sf
dat <- na.omit(dat, cols = "decimalCoordinates")
dat <- st_as_sf(dat, wkt = "decimalCoordinates")
st_crs(dat) <- 4326

# Get centroid
# st_geometry(dat) <- st_centroid(dat)

# Get regions points are in
id <- st_intersects(dat, regions)
id[sapply(id, function(i) length(i) == 0)] <- NA
id <- unlist(id)

rmerge <- st_drop_geometry(regions[id, ])

dat <- cbind(dat, rmerge)


# saveRDS(object = dat, file = here("data/05_France/data.rds"))
# dat <- readRDS(here("data/05_France/data.rds"))


# Add NAs ---------------------------------------------------------------

# Select NA values
dat_na <- dat[which(is.na(dat$region)), ] |>
  select(-c(region, type, department))
nrow(dat_na)

regions_metro <- regions |>
  filter(type == "Metropolitan département")

ggplot(dat_na) +
  geom_sf(data = regions_metro) +
  geom_sf(color = "red") +
  theme_void()

# Add buffer around regions to select NAs
reg_buff <- st_buffer(regions, dist = 500)

id <- st_intersects(dat_na, reg_buff)
id[sapply(id, function(i) length(i) != 1)] <- NA
# != 1 because with buffers some points are in several regions
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
  geom_sf(data = regions_metro) +
  geom_sf(color = "cornflowerblue") +
  theme_void()

# saveRDS(object = dat, file = here("data/05_France/data_nas.rds"))
# dat <- readRDS(here("data/05_France/data_nas.rds"))

# Summarize ---------------------------------------------------------------

# By region
dat_region <- dat |>
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
dat_dpt <- dat |>
  st_drop_geometry() |>
  group_by(department) |>
  summarize(nobs = n())

dpt_df <- regions_metro |>
  left_join(dat_dpt, by = "department")

# Plot maps --------------------------------------------------------------------

# lim <- st_bbox(regions |> filter(type == "Metropolitan département"))
#
# plot(regions,
#      ylim = lim[c(2, 4)],
#      xlim = lim[c(1, 3)])


# Regions
ggplot(regions_df) +
  geom_sf(aes(fill = nobs),
          show.legend = FALSE) +
  scale_fill_gradient(low = "#bbfff1", high = "#004b45",
                      transform = "log10") +
  geom_sf_text(aes(label = format(nobs, big.mark = " "),
                   col = log10(nobs) > max(log10(nobs), na.rm = TRUE)/2),
               show.legend = FALSE,
               size = 4) +
  scale_color_manual(values = c("black", "white")) +
  theme_void()

ggsave(file.path(fig_path, "map_regions.png"),
       width = 30, height = 30, units = "cm", dpi = 300)


# Departments
ggplot(dpt_df) +
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


# By program -----------------------------------------------------------

df_source <- st_drop_geometry(dat) |>
  group_by(source, department) |>
  summarize(nobs = n())

df_source <- regions_metro |>
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


# Species stats -----------------------------------------------------------

(dat_sp <- st_drop_geometry(dat) |>
  group_by(scientificName) |>
  summarize(nobs = n()) |>
   arrange(desc(nobs)))

nrow(dat_sp[!is.na(dat_sp$scientificName), ])

dat_sp_plot <- dat_sp[1:50, ]

ggplot(dat_sp_plot) +
  geom_col(aes(y = reorder(scientificName, nobs), x = nobs)) +
  scale_x_continuous(expand = c(0,0)) +
  xlab("# occurrences") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 5),
        axis.text.y = element_text(face = "italic",
                                   size = 5))


ggsave(file.path(fig_path, "sp_occ.png"),
       width = 15, height = 10, units = "cm", dpi = 300)
