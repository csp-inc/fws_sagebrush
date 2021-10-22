######################################
## DATA COLLECTION & PRE-PROCESSING ##
######################################


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
plot(biome)
plot(nv)


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

# Confirm crs ok? (asText else get crs object)
isTRUE(crs(stack[[1]], asText = TRUE) == proj.crs)


# Just retain a few for memory
names(stack) # keep 2010, 2015, 2020
stack <- stack[[c(2,6,11)]]
names(stack)

##############################################################
## Test case of Nevada with coarser data
# Crop 1 raster to Nevada as test case
start <- Sys.time()
nv2020 <- stack[[3]] %>%
  crop(nv) %>%
  mask(nv)
print(Sys.time() - start) # ~4 min

# Resample to coarser resolution 
nv2020 <- raster::aggregate(nv2020, fact = 10)
plot(nv2020)

# Turn Nevada into raster; convert to ok format first
nv <- st_cast(nv, "MULTIPOLYGON")
# Raster will need numeric; add and create look-up
nv$id <- as.numeric(1:6)
lu.nv <- data.frame(c(1:6),nv$PARENT_NAM) ; colnames(lu.nv) <- c("id","district_name")
# Set template raster; nb didn't work with higher-res raster -- too much memory
nv.r <- fasterize(nv, nv2020, field = "id")


boxplot(nv2020, nv.r)
hist(nv2020)

