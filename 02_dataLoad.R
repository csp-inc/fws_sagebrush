######################################
## DATA COLLECTION & PRE-PROCESSING ##
#############################################################

## Function to load features, set common crs, and fix any invalid geometries
load_f <- function(f) {
  proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  read_sf(f) %>%
    st_transform(proj.crs) %>%
    st_buffer(dist = 0)
}
proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"




## Load AOIs
biome <- load_f(paste0(data.dir,"US_Sagebrush_Biome_2019.shp"))
nv <- load_f(paste0(data.dir,"BLM_NV_District_Offices.shp"))



########################################################
## Load sagebrush rasters 2009-2020 into stack

# List all rasters
files <- list.files(paste0(data.dir,"Sagebrush_2009_2020/"), full.names = TRUE)
# Get indices of only .img files
keeps <- grep(pattern = "img", x = files)
# Retain only those keeps
(files <- files[keeps] %>% sort(.)) 


# Load all rasters directly into a stack.
remove(stack)
stack <- raster(files[1])
for (i in 2:length(files)){
  stack <- addLayer(stack, raster(files[i]))
}

# Confirm crs ok?
isTRUE(print(crs(stack[[1]])) == proj.crs)

# Just retain a few for memory
names(stack) # keep 2010, 2015, 2020
stack <- stack[[c(2,6,11)]]
names(stack)

# Crop 1 raster to Nevada as test case
nv2020 <- stack[[3]] %>%
  crop(nv) %>%
  mask(nv)

boxplot(nv2020, nv)


plot(stack$rcmap_sagebrush_2009)
plot(stack[[1]])
crs(stack[[1]])

hist(stack[[1]])




hist(stack[[1]])



stack <- stack(list.rasters[[1]])
for(i in 2:length(list.rasters)) r <- addLayer(r, stack[[i]])
plot(r)


names(list.rasters[[11]])
plot(list.rasters[[11]])



########################################################
## Test zonal stats  
# # Turn AOIs into comparable rasters; must be multipolygon
nv <- st_cast(nv, "MULTIPOLYGON")
nv.r <- fasterize(nv, stack[[11]])
# # Error: cannot allocate vector of size 46.5 Gb




zonal.stats(list.rasters[[11]], nv, stats = "mean")


list.rasters
names(list.rasters[[1]])
zonal.stats(list.rasters[[i]], curr, stats="mean")



## 
boo <- st_cast(nv, "MULTIPOLYGON")

foo <- fasterize(boo, list.rasters[[1]])


?fasterize
## Stats by zones
zonal(curr, poly.r, fun = "mean", na.rm = TRUE)