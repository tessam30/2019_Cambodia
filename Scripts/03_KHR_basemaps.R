# Prep Colombia ICT data for maps
# Author: Tim Essam, GeoCenter
# Date: 2019_12_03
# Notes:


# Load Dependencies and Fonts ---------------------------------------------
source(file.path("Scripts/00_setup.R"))

pacman::p_load(
  "raster", "sp", "rgdal", "rmapshaper", "geojsonio", "rnaturalearth",
  "rnaturalearthdata", "ggsflabel", "tidytext", "ggtext", "extrafont", "extrafontdb",
  "formattable", "RColorBrewer", "smoothr", "lwgeom"
)


# extrafont::loadfonts() # For custom fonts such as SegoeUI used here

# devtools::install_github("renkun-ken/formattable")
# devtools::install_github("ropensci/rnaturalearthhires") # to grab polygons
# devtools::install_github("yutannihilation/ggsflabel")

# Loading ICT and shapefile to join and map -------------------------------

# Load the crosswalk (cw) so we can easily join shapefile to DANE data extract
source(file.path(rpath, "khm_cw.R")) # -- doesn't exist...yet

# With cw loaded, we can now join to the shapefile to have it merge ready for below
khm_admin1 <- st_read(file.path(gispath, "gadm36_KHM_shp", "gadm36_KHM_1.shp"))
khm_admin0 <- st_read(file.path(gispath, "gadm36_KHM_shp", "gadm36_KHM_0.shp"))

khm_poverty <- st_read(file.path(gispath, "poverty_2015", "poverty_rate_commune.shp")) %>% 
  mutate(poverty = pov_rate / 100)


# Natural Earth Data -----

# World polygons required for the non-AOI map; These will be faded with alpha calls in ggplot later
world <- ne_countries(scale = "large", returnclass = "sf")

# Get the list of countries needed for the filter call above
world %>%
  st_drop_geometry() %>%
  filter(str_detect(sovereignt, "Thail|Viet|Lao|Cam")) %>%
  count(sovereignt)

# Filter out neighbors and admin0 for AIO
khm_nbrs <- world %>%
  filter(sovereignt %in% c("Thailand", "Vietnam", "Laos"))

# Not needed, using the GADM files above b/c of hole in the middle of mission shapefile for admin1s
#khm_admin0 <- world %>% filter(sovereignt == "Cambodia")

# pull in the Cambodia admin1 file from natural earth database -- using mission shapefile
#khm_admin1 <- ne_states(country = "cambodia", returnclass = "sf")

# Add in the water bodies
khm_water <- st_read(file.path(gispath, "water_bodies2015", "water-bodies.shp"))
khm_water2 <- st_read(file.path(gispath, "ne_10m_lakes", "ne_10m_lakes.shp")) %>%
  filter(str_detect(name_abb, "Tonle Sap"))


# SR_LR is the SRTM dem data. This gives the visual pop of terrain that PG loves to showcase
ne_geo <- raster::raster(file.path(gispath, "SR_LR", "SR_LR.tif"))
ne_ocean <- st_read(file.path(gispath, "ne_10m_ocean", "ne_10m_ocean.shp"))
ne_cities <- st_read(file.path(gispath, "ne_10m_populated_places_simple", "ne_10m_populated_places_simple.shp"))


# Bounding box and geoprocessing ----- 
# What is the bounding box we are dealing w/? Should be Colombia with a little wiggle room for ocean
st_bbox(khm_admin0)
mapRange <- c(range(st_coordinates(khm_admin0)[, 1]), range(st_coordinates(khm_admin0)[, 2]))

# Crop raster using vectov extent to get nice terrain feature
# crop the lidar raster using the vector extent
# Add a bit of padding the bounding box
ne_ocean_chop <- st_crop(ne_ocean, xmin = 101.5, ymin = 9.5, xmax = 108, ymax = 17.5)
ne_geo_chop <- crop(ne_geo, ne_ocean_chop)

ne_cities_trunc <- ne_cities %>% filter(str_detect(name, "Siem|Minh|Penh|Kampong Cham|Sisophon|Battambang|Sihanouk"))


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
  geom_sf(data = khm_nbrs, fill = "#d9d9d9", alpha = 0.35, size = 0.20, colour = "#252525") +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)])
# geom_sf_text(data = world_chop, aes(label = sovereignt)) +
# geom_sf(data = col_admin0, colour = "black", alpha = 0.75)

# drops in cities for context
oth_countries_cities <- terrain +
  geom_sf(data = khm_nbrs, fill = "#d9d9d9", alpha = 0.35, size = 0.20, colour = "#252525") +
  geom_sf(data = khm_water, fill = "#bee0ff", alpha = 0.70, colour = "#8bc8ff", size = 0.25) +
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

# Create a custom color palette for the number of Admin 1s
colors_needed <- length(unique(khm_admin1$NAME_1))
dept_colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(colors_needed)
palette(colorRampPalette(brewer.pal(12, "Set3"))(colors_needed))
plot(1:colors_needed, 1:colors_needed, col = 1:colors_needed, pch = 19, cex = 5)

basemap_color <- "#EEE7D7"

# Basemap of Colombia for reference
basemap <- oth_countries +
  geom_sf(data = khm_admin1, aes(fill = NAME_1), size = 0.25, colour = "white", alpha = 0.75) +
  scale_fill_manual(values = dept_colors) +
  geom_sf(data = khm_admin0, colour = "white", fill = "NA", size = 1) +
  geom_sf(data = khm_admin0, colour = "black", fill = "NA", size = 0.5) +
  geom_sf_text_repel(data = khm_admin1, aes(label = NAME_1), colour = "black", size = 2.5) +
  #geom_sf(data = khm_water, fill = "#bee0ff", alpha = 0.70, colour = "#8bc8ff", size = 0.25) +
  #geom_sf(data = ne_cities_trunc, alpha = 0.90) +
  map_clean + theme(legend.position = "none") +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) +
  labs(x = "", y = "", caption = str_c("Created by USAID GeoCenter on ", today()),
       title = "Cambodia provinces")

ggsave(file.path(imagepath, "KHM_basemap.pdf"),
  plot = base_terrain,
  height = 8.5, width = 11, dpi = "retina", units = "in"
)


# Poverty map based on 2015 data ------------------------------------------

pov_map <-
  base_terrain +
  geom_sf(data = khm_poverty, aes(fill = poverty), size = 0.5, colour = "NA", alpha = 0.75) +
  #scale_fill_manual(values = dept_colors) +
  scale_fill_viridis_c(option = "D", direction = -1, labels = scales::percent_format()) +

  geom_sf(data = khm_admin0, colour = "white", fill = "NA", size = 1) +
  geom_sf(data = khm_admin0, colour = "black", fill = "NA", size = 0.5) +
  #geom_sf_text_repel(data = khm_admin1, aes(label = name), colour = "black", size = 2.5) +
  geom_sf(data = khm_water, fill = "#bee0ff", alpha = 0.70, colour = "#8bc8ff", size = 0.25) +
  geom_sf(data = ne_cities_trunc, alpha = 0.90) +
  geom_sf_text_repel(data = ne_cities_trunc, aes(label = name), alpha = 0.90, family = "SegoeUI") +
  map_clean + theme(legend.position = "top", text = element_text(family = "SegoeUI"),
                    legend.justification = "left",
                    legend.key.height = unit(0.4, "cm")) +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) +
  labs(
    x = "", y = "", caption = str_c("Created by USAID GeoCenter on ", today()),
    title = str_c("**", "Poverty rates in Cambodia as of 2015", "**"),
    subtitle = "Darker colors indicate higher poverty rates",
    fill = "poverty rate"
  ) +
  theme(plot.title = element_markdown())


ggsave(file.path(imagepath, "khm_poverty_2015.png"),
  plot = base_map,
  height = 8.5, width = 11, dpi = 600, units = "in" # device = cairo_pdf
)


# Population maps based on 2019 Census ------------------------------------

khm_census <- read_csv(file.path(datapath, "KHM_2019_admin1_pop.csv"))
khm_census_geo <- 
  khm_admin1 %>% 
  left_join(., khm_census, by = c("GID_1"))

oth_countries + 
  geom_sf(data = khm_census_geo, aes(fill = round(total_2019/1e6, 1)), colour = "white", size = 0.5) +
  scale_fill_viridis_c(option = "A", direction = -1, alpha = 0.75, labels = unit_format(unit = "M", accuracy = 0.1)) + 
  map_clean + theme(legend.key.width = unit(1, "cm")) +
  coord_sf(xlim = mapRange[c(1:2)], ylim = mapRange[c(3:4)]) +
  labs(fill = "total population   ")

khm_census_sex <- 
  khm_admin1 %>% 
  left_join(., khm_census, by = c("GID_1")) %>% 
  gather(sex, pop, Males:Females)


