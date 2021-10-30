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

# Confirm crs ok? Check first one. (asText else get crs object)
isTRUE(crs(stack[[1]], asText = TRUE) == proj.crs)


# Just retain a few to save memory
names(stack) # keep 2010, 2015, 2020
stack <- stack[[c(2,6,11)]]
names(stack)


##############################################################
## Test case of Nevada with coarser data
start <- Sys.time()
stack_10_15_20 <- stack %>%
  crop(nv) %>%
  mask (nv) %>%
  raster::aggregate(fact = 10)
print(Sys.time() - start) # 12 min


# Turn Nevada into raster; convert to ok format first
nv <- st_cast(nv, "MULTIPOLYGON")
# Raster will need numeric; add and create look-up
nv$id <- as.numeric(1:6)
lu.nv <- data.frame(c(1:6),nv$PARENT_NAM) ; colnames(lu.nv) <- c("id","district_name")
# Set template raster; nb didn't work with higher-res raster -- too much memory
nv.r <- fasterize(nv, stack_10_15_20[[1]], field = "id")


boxplot(stack_10_15_20[[1]], nv.r)


#####################################################

# Extract mean raster values to Nevada units
foo <- raster::extract(stack_10_15_20, nv,
                       fun = mean, na.rm = TRUE,
                       # start with layer 1, do 3 layers, stick in df
                       layer = 1, nl = 3, df = TRUE)
view(foo)

# ^# do to stack specifying which layer to start with and # of layers (nlayer)

# ID rcmap_sagebrush_2020
# 1  1             4.919563
# 2  2             6.658754
# 3  3             3.362309
# 4  4             5.174511
# 5  5             4.569575
# 6  6             1.131831
boo <- raster::extract(nv2020x0, nv, fun = mean, na.rm = TRUE, df = TRUE)
# ID rcmap_sagebrush_2020
# 1  1             5.766832
# 2  2             7.078882
# 3  3             4.448844
# 4  4             6.632233
# 5  5             5.762023
# 6  6             3.938680


# Let's take avg of 5% sagebrush cover
# How much does that equate to over Nevada?

# Get res (m), square it to get pixel area (sq m), x #pixels, x % cover, convert to sq km
# 1000m x 1000m = 1,000,000 sq m = 1 sq km
sqm2sqkm <- 1/1000000

(res(nv2020)[1])^2*ncell(nv2020)*0.05*sqm2sqkm #19035
# ^ That's HUGE. Is it possible it's including entire bounding box of Nevada?
ncell(nv2020) #5287500 -- is this bounding box?
ncell(nv2020[!is.na(nv2020)]) #5287500 -- suggests it's excluding NAs from count
isTRUE(nrow(nv2020) * ncol(nv2020) == ncell(nv2020)) # but then why are these =?
ncell(nv2020[nv2020>0]) #2318424
(res(nv2020)[1])^2*ncell(nv2020[nv2020>0])*0.04*0.001 #8346326

raster::area(nv2020)
nrow(nv2020) * ncol(nv2020)

?ncell


goo <- st_bbox(nv)
raster::area(nv2020)
?st_geometry
plot(nv)


## END GOAL
# values are % sagebrush in each pixel
# need average % for each district office, for each year
  # plot trendline
  # generate linear trendline
  # extract slope (annual loss) and plot to map

