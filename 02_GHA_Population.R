# 03_Population_Map
# Exprimenting with plotting the population data for context


# load CIESEN/Facebook population raster data
# Downloaded from https://www.ciesin.columbia.edu/repository/hrsl/hrsl_gha_v1.zip
library(rgdal)
library(rasterVis)
library(raster)
library(viridis)
library(ggthemes) # theme_map()
pop_raster <- raster(file.path(datapath, "hrsl_gha_v1", "hrsl_gha_pop.tif"))
crs(pop_raster) <- "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs"
crs(gha_sf1_new)

gha_spdf <- as(pop_raster, "SpatialPixelsDataFrame")
gha_pop_df <- as.data.frame(gha_spdf)
colnames(gha_pop_df) <- c("value", "x", "y")

# Read in admin 0
adm0 <- readOGR(dsn = file.path(datapath, "Admin0"), layer = "Admin0") 

ggplot() +
  #geom_sf(data = gha_sf1_new, colour = grey40K, fill = NA) +
  geom_tile(data = gha_pop_df , aes(x = x, y = y,
                                       fill = value)) +
  scale_fill_viridis_c(option = "A") +
  coord_sf() +
  theme(panel.grid = element_line(colour = "white"),
        panel.background = element_rect(colour = "white"))
  

    
    
    