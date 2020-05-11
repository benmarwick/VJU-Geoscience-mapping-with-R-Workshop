# Could you please choose some of the sheets to draw map of coring sites and
# plot graphs of sediment physical parameters and/or diagrams of microfossils?

# In this script we make three kinds of maps:

# 1. A country-level map to show the general location of a study area
# 2. An aerial photo map of a small area to show detail of sampling locations
# 3. A bathymetry contours and raster map


#------------ one time  only install pkgs -----------------------------------------------
# setting up by installing pkg for the first time
# we only do this one time for our computer
# no need to repeat every time we want to
# make the maps

pkgs <- c(
  "akima",
  "cowplot",
  "devtools",
  "dplyr",
  "ggmap",
  "here",
  "leaflet",
  "magick",
  "mapview",
  "readxl",
  "reshape2",
  "shadowtext",
  "tidyverse",
  "tmaptools"
)

# install.packages(pkgs)

# devtools::install_github("3wen/legendMap")
# devtools::install_github("slowkow/ggrepel")
















#------------ read in the data  -----------------------------------------------


library(tidyverse)
library(here)

coring_sites <-
  readxl::read_excel(here("data/Data availability - PANGEA_version_3.xlsx"))

location_label <-
  tibble(x = 108,
         y = 15,
         label="Bien Ho Lake")





























#------------ A country-level map -----------------------------------------------

library(ggmap)
library(tmaptools)

# get base map tiles for Vietnam
base_map_vietnam <-
  ggmap(get_stamenmap(rbind(as.numeric(paste(geocode_OSM("vietnam")$bbox))),
                      zoom = 7))

# add layer of our core site locations
library(shadowtext)
library(legendMap)

base_map_vietnam_and_label <-
  base_map_vietnam +
  geom_point(data = coring_sites,
             aes(x = Longtitude,
                 y = Latitude),
             size = 3,
             colour = "red") +
  geom_shadowtext(data = location_label,
                  aes(x,
                      y,
                      label=label)) +
  scale_bar(lon = 110,
            lat = 9,
            legend_size = 2,
            distance_lon = 200, distance_lat = 20,
            distance_legend = 70, dist_unit = "km",
            arrow_length = 100, arrow_distance = 100, arrow_north_size = 6)

base_map_vietnam_and_label












#------------ An aerial photo map of a small area -----------------------------------------------

# zoom into the lake, and interactive map

library(leaflet)
library(mapview)

m <-
  leaflet() %>%
  addProviderTiles('Esri.WorldImagery') %>%
  setView(coring_sites$Longtitude[5],
          coring_sites$Latitude[5],
          zoom = 14.5)

# save it
mapshot(m, file = here("figures/lake_view.png"))

# crop it and save cropped image
library(magick)
image_read(here("figures/lake_view.png")) %>%
  image_crop("500x700+100+100") %>%
  image_write(here("figures/lake_view_cropped.png"))

























#------------ A bathymetry contours and raster map -----------------------------------------------

# close up map of lake and bathymetry

bathymetry <-
  readxl::read_excel(here("data/Data availability - PANGEA_version_3.xlsx"),
                     sheet = "Bathymetry")

# tidy the data
bathymetry_tidy <-
  bathymetry %>%
  mutate(longitude = `Longtitude (oE)`,
         latitude = `Latitude (oN)`,
         depth = `Depth (m)`) %>%
  dplyr::select(longitude,
                latitude,
                depth) %>%
  drop_na()

# plot the raw measurement data
ggplot() +
  geom_point(data = bathymetry_tidy,
             aes(x = longitude,
                 y = latitude,
                 colour = depth))

# interpolate to make a raster
library(akima)
fld <- interp(x = bathymetry_tidy$longitude,
              y = bathymetry_tidy$latitude,
              z = bathymetry_tidy$depth,
              duplicate =  "strip")

# prepare data in long format
df <- reshape2::melt(fld$z, na.rm = TRUE)
names(df) <- c("x", "y", "depth")
df$Lon <- fld$x[df$x]
df$Lat <- fld$y[df$y]

# make the bathymetry map
bathymetry_map <-
  ggplot() +
  geom_tile(data  = df,
            aes(x = Lon,
                y = Lat,
                fill = depth),
            width = 0.001, height = 0.001) +
  stat_contour(data = df,
               aes(x = Lon,
                   y = Lat,
                   z = depth),
               colour = "red",
               size = 0.2) +
  ggtitle("Bien Ho Lake bathymetrey") +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_continuous(name = "Depth") +
  coord_map() +
  theme_void() +
  scale_bar(lon = 107.990,
            lat = 14.033,
            legend_size = 4,
            distance_lon = 0.4, distance_lat = 0.05,
            distance_legend = 0.1, dist_unit = "km",
            arrow_length = 0.2, arrow_distance = 0.2, arrow_north_size = 6)

bathymetry_map

# put coring sites on the bathy map
library(ggrepel)
bathymetry_map_and_sites <-
  bathymetry_map +
  geom_point(data = coring_sites,
             aes(x = Longtitude,
                 y = Latitude),
             size = 3.75,
             colour = "black") +
  geom_point(data = coring_sites,
             aes(x = Longtitude,
                 y = Latitude),
             size = 3,
             colour = "red") +
  geom_text_repel(data = coring_sites,
                  aes(x = Longtitude,
                      y = Latitude,
                      label = Note),
                  size = 3,
                  force = 20,
                  bg.r = 0.1,
                  bg.color = "black",
                  colour = "white")




















# --------------- Combine all three maps and save to disk -----------------------------------------------

# combine the maps into one panel
library(cowplot)

lake_map <-
  ggdraw() +
  draw_image(here("figures/lake_view_cropped.png"),
             scale = 0.87)

left_side <-
  plot_grid(base_map_vietnam_and_label,
            lake_map,
            align = 'v',
            axis = 'lr',
            ncol = 1)

plot_grid(left_side,
          bathymetry_map_and_sites,
          ncol = 2)

# save image
ggsave(here("figures/base_map_vietnam_lake_map_bathymetry_panel.png"),
       height = 10,
       width = 14)









