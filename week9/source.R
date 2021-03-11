library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)

# Load Harvard Forest metadata information
HARV_dsmCrop_info <- capture.output(
  GDALinfo("raw_data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")
)

# Load Harvard Forest raster
DSM_HARV <- 
  raster("raw_data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_dsmCrop.tif")

# Store Harvard Forest raster in a data frame
DSM_HARV_df <- as.data.frame(DSM_HARV, xy = TRUE)

# Quick map
ggplot() +
  geom_raster(data = DSM_HARV_df , aes(x = x, y = y, fill = HARV_dsmCrop)) +
  scale_fill_viridis_c() +
  coord_quickmap()+
  theme_classic()

# Store mins and max
DSM_HARV <- setMinMax(DSM_HARV)

# Historgram
ggplot() +
  geom_histogram(data = DSM_HARV_df, aes(HARV_dsmCrop),bins = 40, color="black", fill="steelblue")+
  theme_classic()

# Clasifying elevation
DSM_HARV_df <- DSM_HARV_df %>%
  mutate(fct_elevation = cut(HARV_dsmCrop, breaks = 3))

ggplot() +
  geom_bar(data = DSM_HARV_df, aes(fct_elevation))+
  theme_classic()

# Fine tune the clasification of elevation
custom_bins <- c(300, 350, 400, 450)

DSM_HARV_df <- DSM_HARV_df %>%
  mutate(fct_elevation_2 = cut(HARV_dsmCrop, breaks = custom_bins))

ggplot() +
  geom_bar(data = DSM_HARV_df, aes(fct_elevation_2))+
  theme_classic()

# Plot clasified elevation
ggplot() +
  geom_raster(data = DSM_HARV_df , aes(x = x, y = y, fill = fct_elevation_2)) + 
  coord_quickmap()+
  theme_classic()

# Terrain color palette
terrain.colors(3)

# Map with terrain color palette
ggplot() +
  geom_raster(data = DSM_HARV_df , aes(x = x, y = y,
                                       fill = fct_elevation_2)) + 
  scale_fill_manual(values = terrain.colors(3)) + 
  coord_quickmap()+
  theme_classic()

# Load hillshade
DSM_hill_HARV <-
  raster("raw_data/NEON-DS-Airborne-Remote-Sensing/HARV/DSM/HARV_DSMhill.tif")

DSM_hill_HARV

# Convert to data frame
DSM_hill_HARV_df <- as.data.frame(DSM_hill_HARV, xy = TRUE) 

str(DSM_hill_HARV_df)

# Map hillshade
ggplot() +
  geom_raster(data = DSM_hill_HARV_df,
              aes(x = x, y = y, alpha = HARV_DSMhill)) + 
  scale_alpha(range =  c(0.15, 0.65), guide = "none") + 
  coord_quickmap()+
  theme_classic()

# Elevation with hillshade
ggplot() +
  geom_raster(data = DSM_HARV_df , 
              aes(x = x, y = y, 
                  fill = HARV_dsmCrop)) + 
  geom_raster(data = DSM_hill_HARV_df, 
              aes(x = x, y = y, 
                  alpha = HARV_DSMhill)) +  
  scale_fill_viridis_c() +  
  scale_alpha(range = c(0.15, 0.65), guide = "none") +  
  ggtitle("Elevation with hillshade") +
  coord_quickmap()+
  theme_classic()

# Import the DTM and DTM hillshade data and convert them to dataframes
DTM_HARV <- raster("raw_data/NEON-DS-Airborne-Remote-Sensing/HARV/DTM/HARV_dtmCrop.tif")  

DTM_HARV_df <- as.data.frame(DTM_HARV, xy = TRUE)

DTM_hill_HARV <- raster("raw_data/NEON-DS-Airborne-Remote-Sensing/HARV/DTM/HARV_DTMhill_WGS84.tif") 

DTM_hill_HARV_df <- as.data.frame(DTM_hill_HARV, xy = TRUE)


# Reproject DTM_hill_HARV raster data to match the DTM_HARV raster CRS
DTM_hill_UTMZ18N_HARV <- projectRaster(DTM_hill_HARV,
                                       crs = crs(DTM_HARV))

# Adjust resolution 
DTM_hill_UTMZ18N_HARV <- projectRaster(DTM_hill_HARV,
                                       crs = crs(DTM_HARV),
                                       res = res(DTM_HARV)) 

# Overlay of DTM_HARV_df with its hillshade 
DTM_hill_HARV_2_df <- as.data.frame(DTM_hill_UTMZ18N_HARV, xy = TRUE)

ggplot() +
  geom_raster(data = DTM_HARV_df , 
              aes(x = x, y = y, 
                  fill = HARV_dtmCrop)) + 
  geom_raster(data = DTM_hill_HARV_2_df, 
              aes(x = x, y = y, 
                  alpha = HARV_DTMhill_WGS84)) +
  scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10)) + 
  coord_quickmap()+
  theme_classic()

# Calculating Canopy Height Model using raster math
CHM_HARV <- DSM_HARV - DTM_HARV

CHM_HARV_df <- as.data.frame(CHM_HARV, xy = TRUE)

ggplot() +
  geom_raster(data = CHM_HARV_df , 
              aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(name = "Canopy Height", colors = terrain.colors(10)) + 
  coord_quickmap()+
  theme_classic()

# Histogram of Canopy Height Model
ggplot(CHM_HARV_df) +
  geom_histogram(aes(layer),bins = 40, color="black", fill="steelblue")+
  theme_classic()


# CHM calculationusing the overlay() function
CHM_ov_HARV <- overlay(DSM_HARV,
                       DTM_HARV,
                       fun = function(r1, r2) { return( r1 - r2) })

CHM_ov_HARV_df <- as.data.frame(CHM_ov_HARV, xy = TRUE)


# Map CHM
ggplot() +
  geom_raster(data = CHM_ov_HARV_df, 
              aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(name = "Canopy Height", colors = terrain.colors(10)) + 
  coord_quickmap()+
  theme_classic()

# Load RGB data BAND 1
RGB_band1_HARV <- raster("raw_data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif")

# Convert this data into a dataframe and plot it
RGB_band1_HARV_df  <- as.data.frame(RGB_band1_HARV, xy = TRUE)

ggplot() +
  geom_raster(data = RGB_band1_HARV_df,
              aes(x = x, y = y, alpha = HARV_RGB_Ortho)) + 
  coord_quickmap()+
  theme_classic()

# Load RGB data BAND 2
RGB_band2_HARV <-  raster("raw_data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif", band = 2)

# Convert this data into a dataframe and plot it
RGB_band2_HARV_df <- as.data.frame(RGB_band2_HARV, xy = TRUE)


ggplot() +
  geom_raster(data = RGB_band2_HARV_df,
              aes(x = x, y = y, alpha = HARV_RGB_Ortho)) + 
  coord_equal()+
  theme_classic()

# Load RGB Stack
RGB_stack_HARV <- stack("raw_data/NEON-DS-Airborne-Remote-Sensing/HARV/RGB_Imagery/HARV_RGB_Ortho.tif")

# Convert to data frame
RGB_stack_HARV_df  <- as.data.frame(RGB_stack_HARV, xy = TRUE)

# Histogram of RGB_stack_HARV_df
ggplot() +
  geom_histogram(data = RGB_stack_HARV_df, aes(HARV_RGB_Ortho.1),bins = 40, color = "black", fill = "steelblue")+
  theme_classic()


# PLot second band
ggplot() +
  geom_raster(data = RGB_stack_HARV_df,
              aes(x = x, y = y, alpha = HARV_RGB_Ortho.2)) + 
  coord_quickmap()+
  theme_classic()

# Plot pseudo-color image
plotRGB(RGB_stack_HARV,
        r = 1, g = 2, b = 3)

# Image using stretch="lin"
plotRGB(RGB_stack_HARV,
        r = 1, g = 2, b = 3,
        scale = 800,
        stretch = "lin")

# Image using stretch="hist"
plotRGB(RGB_stack_HARV,
        r = 1, g = 2, b = 3,
        scale = 800,
        stretch = "hist")


