# Prep Colombia ICT data for maps
# Author: Tim Essam, GeoCenter
# Date: 2019_12_03
# Notes:


# Load Dependencies and Fonts ---------------------------------------------
source(file.path("Scripts/00_setup.R"))

pacman::p_load(
  "raster", "sp", "rgdal", "rmapshaper", "geojsonio", "rnaturalearth",
  "rnaturalearthdata", "ggsflabel", "tidytext", "ggtext", "extrafont", "extrafontdb",
  "formattable", "RColorBrewer"
)


# extrafont::loadfonts() # For custom fonts such as SegoeUI used here

# devtools::install_github("renkun-ken/formattable")
# devtools::install_github("ropensci/rnaturalearthhires") # to grab polygons
# devtools::install_github("yutannihilation/ggsflabel")

# Loading ICT and shapefile to join and map -------------------------------

# Load the crosswalk (cw) so we can easily join shapefile to DANE data extract
source(file.path(rpath, "KHR_cw.R")) # -- doesn't exist...yet

# With cw loaded, we can now join to the shapefile to have it merge ready for below
khm_admin1 <- st_read(file.path(gispath, ))


# Natural Earth -----

# World polygons required for the non-AOI map; These will be faded with alpha calls in ggplot later
world <- ne_countries(scale = "large", returnclass = "sf")

# Get the list of countries needed for the filter call above
world %>%
  st_drop_geometry() %>%
  filter(str_detect(sovereignt, "Thail|Viet|Lao|Cam")) %>%
  count(sovereignt)

# Filter out neighbors and admin0 for AIO
khr_nbrs <- world %>%
  filter(sovereignt %in% c("Thailand", "Vietnam", "Laos"))

khr_admin0 <- world %>% filter(sovereignt == "Cambodia")

# pull in the Cambodia admin1 file from natural earth database
khr_admin1 <- ne_states(country = "cambodia", returnclass = "sf")

# Add in the water bodies
khr_water <- st_read(file.path(gispath, "water_bodies2015", "water-bodies.shp"))
khr_water2 <- st_read(file.path(gispath, "ne_10m_lakes", "ne_10m_lakes.shp")) %>%
  filter(str_detect(name_abb, "Tonle Sap"))


# SR_LR is the SRTM dem data. This gives the visual pop of terrain that PG loves to showcase
ne_geo <- raster::raster(file.path(gispath, "SR_LR", "SR_LR.tif"))
ne_ocean <- st_read(file.path(gispath, "ne_10m_ocean", "ne_10m_ocean.shp"))
ne_cities <- st_read(file.path(gispath, "ne_10m_populated_places_simple", "ne_10m_populated_places_simple.shp"))

# What is the bounding box we are dealing w/? Should be Colombia with a little wiggle room for ocean
st_bbox(khr_admin0)
mapRange <- c(range(st_coordinates(khr_admin0)[, 1]), range(st_coordinates(khr_admin0)[, 2]))

# Crop raster using vectov extent to get nice terrain feature
# crop the lidar raster using the vector extent
# Add a bit of padding the bounding box
ne_ocean_chop <- st_crop(ne_ocean, xmin = 101.5, ymin = 9.5, xmax = 108, ymax = 17.5)
ne_geo_chop <- crop(ne_geo, ne_ocean_chop)


# Need a data frame to get ggplot to render the raster data
spdf <- as(ne_geo_chop, "SpatialPixelsDataFrame") %>% as.data.frame(.)



# Base maps ---------------------------------------------------------------

# Terrain basemap adds texture to plots
terrain <- ggplot() +
  geom_tile(data = spdf, aes(x = x, y = y, alpha = SR_LR)) +
  scale_alpha(name = "", range = c(0.6, 0), guide = F) +
  theme(legend.position = "none") +
  geom_sf(data = ne_ocean_chop, fill = "#d7ecff", colour = "NA", alpha = 0.85)

# Adds in the surrounding countries and overlays on terrain to get textured map
oth_countries <- terrain +
  geom_sf(data = khr_nbrs, fill = "#d9d9d9", alpha = 0.35, size = 0.20, colour = "#252525") +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)])
# geom_sf_text(data = world_chop, aes(label = sovereignt)) +
# geom_sf(data = col_admin0, colour = "black", alpha = 0.75)

# drops in cities for context
oth_countries_cities <- terrain +
  geom_sf(data = khr_nbrs, fill = "#d9d9d9", alpha = 0.35, size = 0.20, colour = "#252525") +
  geom_sf(data = khr_water, fill = "#bee0ff", alpha = 0.70, colour = "#8bc8ff", size = 0.25) +
  geom_sf(
    data = ne_cities %>% filter(sov0name == "Cambodia", str_detect(featurecla, "Admin-0*")),
    alpha = 0.10
  ) +
  geom_sf_text_repel(
    data = ne_cities %>% filter(sov0name == "Cambodia", str_detect(featurecla, "Admin-0*")),
    aes(label = name)
  ) +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)])



# Choropleths on top of basemaps ------------------------------------------
source <- str_c("Created by USAID GeoCenter on ", today(), "     |     Data source: TBD")

theme_set(theme_minimal())
map_clean <- theme(
  legend.position = "top",
  legend.justification = "left",
  axis.text = element_blank(),
  panel.grid = element_blank(),
  axis.ticks = element_blank(),
  strip.text = element_text(hjust = 0),
  legend.key.height = unit(0.4, "cm"),
  text = element_text(family = "SegoeUI")
)




# Basemap of Admin 2 ------------------------------------------------------


basemap_color <- "#EEE7D7"

# Basemap of Colombia for reference
base_terrain <- oth_countries +
  map_clean +
  labs(x = "", y = "", caption = str_c("Created by USAID GeoCenter on ", today()))

ggsave(file.path(imagepath, "COL_basemap_terrain.pdf"),
  plot = base_terrain,
  height = 11, width = 8.5, dpi = "retina", units = "in"
)


# Create a custom color palette for the number of Admin 1s
colors_needed <- length(unique(khr_admin1$name))
dept_colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(colors_needed)
palette(colorRampPalette(brewer.pal(12, "Set3"))(colors_needed))
plot(1:colors_needed, 1:colors_needed, col = 1:colors_needed, pch = 19, cex = 5)

base_map <-
  base_terrain +
  geom_sf(data = khr_admin1, aes(fill = name), size = 0.5, colour = "#525252", alpha = 0.5) +
  scale_fill_manual(values = dept_colors) +
  geom_sf(data = khr_admin0, colour = "white", fill = "NA", size = 1) +
  geom_sf(data = khr_admin0, colour = "black", fill = "NA", size = 0.5) +
  geom_sf_text_repel(data = khr_admin1, aes(label = name), colour = "black", size = 2.5) +
  geom_sf(data = khr_water, fill = "#bee0ff", alpha = 0.70, colour = "#8bc8ff", size = 0.25) +
  map_clean + theme(legend.position = "none", text = element_text(family = "SegoeUI")) +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) +
  labs(
    x = "", y = "", caption = str_c("Created by USAID GeoCenter on ", today()),
    title = str_c("**", "Administrative Departments of Colombia", "**")
  ) +
  theme(plot.title = element_markdown())


ggsave(file.path(imagepath, "KHR_basemap_admin1.png"),
  plot = base_map,
  height = 11, width = 8.5, dpi = 600, units = "in" # device = cairo_pdf
)
