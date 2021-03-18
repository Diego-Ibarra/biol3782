library('sf')
library('geojsonio')
library(ggplot2)
library(dplyr)
library(raster)

# Read vector data
aoi_boundary_HARV <- st_read(
  "raw_data/NEON-DS-Site-Layout-Files/HARV/HarClip_UTMZ18.shp")

# Plot sf vector object
ggplot() + 
  geom_sf(data = aoi_boundary_HARV, size = 3, color = "black", fill = "cyan1") + 
  ggtitle("AOI Boundary Plot") + 
  coord_sf()

# for task 1 -------------------------------------------------------------------
#import data
lines_HARV <- st_read("raw_data/NEON-DS-Site-Layout-Files/HARV/HARV_roads.shp")
point_HARV <- st_read("raw_data/NEON-DS-Site-Layout-Files/HARV/HARVtower_UTM18N.shp")

#Check class
class(lines_HARV)
class(point_HARV)

#CRS and extent of each object
st_crs(lines_HARV)
st_crs(point_HARV)

st_bbox(lines_HARV)
st_bbox(point_HARV)
# ------------------------------------------------------------------------------


# Subset data to include only TYPE "footpath" data
footpath_HARV <- lines_HARV %>%
  filter(TYPE == "footpath")

nrow(footpath_HARV)

# Plot footpath_HARV
ggplot() + 
  geom_sf(data = footpath_HARV) +
  ggtitle("NEON Harvard Forest Field Site", subtitle = "Footpaths") + 
  coord_sf()+
  theme_classic()

# PLot each footpath with a different color
ggplot() + 
  geom_sf(data = footpath_HARV, aes(color = factor(OBJECTID)), size = 1.5) +
  labs(color = 'Footpath ID') +
  ggtitle("NEON Harvard Forest Field Site", subtitle = "Footpaths") + 
  coord_sf()+
  theme_classic()


# PLot all TYPES in lines_HARV
road_colors <- c("blue", "green", "navy", "purple")

ggplot() +
  geom_sf(data = lines_HARV, aes(color = TYPE)) + 
  scale_color_manual(values = road_colors) +
  labs(color = 'Road Type') +
  ggtitle("NEON Harvard Forest Field Site", subtitle = "Roads & Trails") + 
  coord_sf()+
  theme_classic()


# PLotting from multiple shapefiles
ggplot() + 
  geom_sf(data = aoi_boundary_HARV, fill = "grey", color = "grey") +
  geom_sf(data = lines_HARV, aes(color = TYPE), size = 1) +
  geom_sf(data = point_HARV) +
  ggtitle("NEON Harvard Forest Field Site") + 
  coord_sf()+
  theme_classic()

# Map with custom legend
ggplot() + 
  geom_sf(data = aoi_boundary_HARV, fill = "grey", color = "grey") +
  geom_sf(data = lines_HARV, aes(color = TYPE),
          show.legend = "line", size = 1) +
  geom_sf(data = point_HARV, aes(fill = Sub_Type), color = "black") +
  scale_color_manual(values = road_colors, name = "Line Type") + 
  scale_fill_manual(values = "black", name = "Tower Location") +
  ggtitle("NEON Harvard Forest Field Site") + 
  coord_sf()+
  theme_classic()

# Plotting raster and vector data together -------------------------------------
library(raster)

DSM_HARV <- raster("raw_data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif") 

DTM_HARV <- raster("raw_data/NEON-DS-Airborne-Remote-Sensing/HARV/DTM/HARV_dtmCrop.tif")

CHM_HARV <- DSM_HARV - DTM_HARV 

CHM_HARV_df <- as.data.frame(CHM_HARV, xy = TRUE) 


# Make plot with raster and vectors
ggplot() +
  geom_raster(data = CHM_HARV_df, aes(x = x, y = y, fill = layer)) +
  geom_sf(data = lines_HARV, color = "black") +
  geom_sf(data = aoi_boundary_HARV, color = "grey20", size = 1) +
  geom_sf(data = point_HARV, pch = 8) +
  ggtitle("NEON Harvard Forest Field Site w/ Canopy Height Model") + 
  coord_sf() +
  theme_classic()



# Working with spatial data from different sources -----------------------------
state_boundary_US <- st_read("raw_data/NEON-DS-Site-Layout-Files/US-Boundary-Layers/US-State-Boundaries-Census-2014.shp")


# Map of Contiguous US State Boundaries
ggplot() +
  geom_sf(data = state_boundary_US) +
  ggtitle("Map of Contiguous US State Boundaries") +
  coord_sf()+
  theme_classic()


# Include a boundary layer of the United States
country_boundary_US <- st_read("raw_data/NEON-DS-Site-Layout-Files/US-Boundary-Layers/US-Boundary-Dissolved-States.shp")

ggplot() +
  geom_sf(data = country_boundary_US, color = "gray18", size = 2) +
  geom_sf(data = state_boundary_US, color = "gray40") +
  ggtitle("Map of Contiguous US State Boundaries") +
  coord_sf()+
  theme_classic()

# Map of us state boundaries + country boundary + point_HARV
ggplot() +
  geom_sf(data = country_boundary_US, size = 2, color = "gray18") +
  geom_sf(data = state_boundary_US, color = "gray40") +
  geom_sf(data = point_HARV, shape = 19, color = "purple") +
  ggtitle("Map of Contiguous US State Boundaries") +
  coord_sf()+
  theme_classic()

# Import HARV_PlotLocations.csv
plot_locations_HARV <-
  read.csv("raw_data/NEON-DS-Site-Layout-Files/HARV/HARV_PlotLocations.csv")

str(plot_locations_HARV)

# Create CRS object
utm18nCRS <- st_crs(point_HARV)

utm18nCRS

# Convert csv to sf object
plot_locations_sp_HARV <- st_as_sf(plot_locations_HARV, coords = c("easting", "northing"), crs = utm18nCRS)



# Map of locations
ggplot() +
  geom_sf(data = plot_locations_sp_HARV) +
  ggtitle("Map of Plot Locations")


# Map of raster + boundary
ggplot() +
  geom_raster(data = CHM_HARV_df, aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = aoi_boundary_HARV, color = "blue", fill = NA) +
  coord_sf()+
  theme_classic()



# Crop raster
CHM_HARV_Cropped <- crop(x = CHM_HARV, y = aoi_boundary_HARV) 



# Plot the cropped CHM data
CHM_HARV_Cropped_df <- as.data.frame(CHM_HARV_Cropped, xy = TRUE)

ggplot() +
  geom_sf(data = st_as_sfc(st_bbox(CHM_HARV)), fill = "green",
          color = "green", alpha = .2) +  
  geom_raster(data = CHM_HARV_Cropped_df,
              aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(name = "Canopy Height", colors = terrain.colors(10)) + 
  coord_sf() +
  theme_classic()

# PLot only cropped portion
ggplot() +
  geom_raster(data = CHM_HARV_Cropped_df,
              aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = aoi_boundary_HARV, color = "blue", fill = NA) + 
  scale_fill_gradientn(name = "Canopy Height", colors = terrain.colors(10)) + 
  coord_sf() +
  theme_classic()


# Crop the data
CHM_plots_HARVcrop_df <- crop(x = CHM_HARV, y = plot_locations_sp_HARV)  %>% 
  as.data.frame(xy = TRUE)

head(CHM_plots_HARVcrop_df)


# Cropped plot + locations
ggplot() + 
  geom_raster(data = CHM_plots_HARVcrop_df, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(name = "Canopy Height", colors = terrain.colors(10)) + 
  geom_sf(data = plot_locations_sp_HARV) + 
  coord_sf() +
  theme_classic()

# Define new extent
new_extent <- extent(732161.2, 732238.7, 4713249, 4713333)


# Crop raster to new extent
CHM_HARV_manual_cropped <- crop(x = CHM_HARV, y = new_extent)

# Convert to a dataframe
CHM_HARV_manual_cropped_df <- as.data.frame(CHM_HARV_manual_cropped, xy = TRUE)


# PLot new extent
ggplot() + 
  geom_sf(data = aoi_boundary_HARV, color = "blue", fill = NA) +
  geom_raster(data = CHM_HARV_manual_cropped_df,
              aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(name = "Canopy Height", colors = terrain.colors(10)) + 
  coord_sf() +
  theme_classic()


# Extract tree heights
tree_height <- extract(x = CHM_HARV, y = aoi_boundary_HARV, df = TRUE)

str(tree_height)


# Histogram of extracted tree heights
ggplot() + 
  geom_histogram(data = tree_height, aes(x = layer)) +
  ggtitle("Histogram of CHM Height Values (m)") +
  xlab("Tree Height") + 
  ylab("Frequency of Pixels")+
  theme_classic()

# Extract mean pixel value from raster
mean_tree_height_tower <- extract(x = CHM_HARV,
                                  y = point_HARV,
                                  buffer = 20,
                                  fun = mean)

mean_tree_height_tower
