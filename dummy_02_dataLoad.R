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
print(Sys.time() - start) # 12-19 min


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
vals <- raster::extract(stack_10_15_20, nv,
                       fun = mean, na.rm = TRUE,
                       # start with layer 1, do 3 layers, stick in df
                       layer = 1, nl = 3, df = TRUE)


# Convert sagebrush percentage for all pixels in zone into sq km

# Define function to convert
sqm2sqkm <- function(x, na.rm = TRUE) (res(stack_10_15_20)[1]^2 * ncell(stack_10_15_20)) * x/100 * 1/1000000

# Apply conversion to pixel percentage columns
vals_sqkm <- vals %>%
  mutate_at(vars(starts_with("rcmap")),
            # funs(sqkm = sqm2sqkm)) # appends suffix sqkm; w/o funs
            sqm2sqkm) # w/o specifying funs, mutate_at overrides orig vars


# Tidy dataset for applying models
foo <- vals_sqkm %>% t() %>% data.frame() # transpose
colnames(foo) <- paste0("zone_",foo[1,]) # name cols by zone
foo <- foo[-c(1),] # nix extraneous column
foo$yr <- as.numeric(right(rownames(foo),4)) # fill yr col with suffix yr
rownames(foo) <- NULL # nix extraneous row names
data <- foo %>%
  gather(key = "zone", value = "sqkm", -yr) # turn zone into variable



