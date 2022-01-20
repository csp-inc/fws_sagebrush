###########################################################
###########################################################
###########################################################
## IF WORKING WITH GEE RESULTS, SKIP THIS TIF PROCESSING ##
###########################################################
###########################################################
###########################################################




today <- Sys.Date()

# maybe necessary if some raster aren't loading
# unlink(".RData")

######################################
## DATA COLLECTION & PRE-PROCESSING ##
######################################

## Function to load features, set common crs, and fix any invalid geometries
load_f <- function(f) {
  proj.crs <- "+proj=longlat +datum=WGS84 +no_defs"
  proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  read_sf(f) %>%
    st_transform(proj.crs) %>%
    st_make_valid() %>%
    st_buffer(dist = 0)
}
# proj.crs <- "+proj=longlat +datum=WGS84 +no_defs"
proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"


## Load AOIs
# Load biome
biome <- load_f(paste0(data.dir, "US_Sagebrush_Biome_2019.shp"))
plot(biome)

# # Create template from biome
# template <- raster(extent(biome),
#                    crs = proj.crs,
#                    res = 90,
#                    vals = 1)
# plot(template)
# res(template)


# Load ecoregions
# Below is from original shapefile off ScienceBase: https://www.sciencebase.gov/catalog/item/55c77f7be4b08400b1fd82d4
eco <- load_f(paste0(data.dir, "us_eco_l3_merged_features.shp"))
eco$US_L3NAME
# Retain select ecoregions and assign groups
gb <- c("Central Basin and Range", "Mojave Basin and Range", "Sonoran Basin and Range")
gp <- c("High Plains", "Southwestern Tablelands", "Northwestern Glaciated Plains", "Northwestern Great Plains")
keeps <- c(
           #Great Basin Group
           "Central Basin and Range", "Mojave Basin and Range", "Sonoran Basin and Range",
           #Great Plains group
           "High Plains", "Southwestern Tablelands", "Northwestern Glaciated Plains", "Northwestern Great Plains",
           #Intermountain West
           "Cascades", "Sierra Nevada", "Eastern Cascades Slopes and Foothills", "Columbia Plateau",
           "Blue Mountains", "Snake River Plain", "Northern Rockies",
           "Idaho Batholith", "Middle Rockies", "Wyoming Basin",
           "Wasatch and Uinta Mountains", "Colorado Plateaus", "Southern Rockies",
           "Arizona/New Mexico Plateau", "Arizona/New Mexico Mountains", "Northern Basin and Range")

eco <- eco[eco$US_L3NAME %in% keeps,] %>%
  mutate(group = ifelse(US_L3NAME %in% gb, "Great Basin",
                        ifelse(US_L3NAME %in% gp, "Great Plains", "Intermountain West")))

eco <- eco %>%
  group_by(group) %>%
  summarize(geometry = st_union(geometry))
plot(eco) # tiny scrap in lower lobe of InterMt West...

# st_write(eco, paste0(data.dir,"eco_grps.shp"), driver = "ESRI Shapefile")


#Alt: load shapefiles from DT, which checks out against above processing, but doesn't retain names.
# eco_alt <- load_f(paste0(data.dir, "SEIecoregions.shp"))
# plot(eco_alt)

# Test case of Great Basin
# gb <- st_cast(eco[eco$group == "Great Basin",], "MULTIPOLYGON")
# plot(gb)
# # Raster will need numeric; add and create look-up
# gb$US_L3CODE <- as.numeric(gb$US_L3CODE)
# lu.gb <- data.frame(gb$US_L3CODE,gb$US_L3NAME) 
# # Rasterize
# gb.r <- fasterize(gb, template, field = "US_L3CODE")
# plot(gb.r)



###########################################################
## Load cores (defend), grow, mitigate zones and project ##
###########################################################

# List all rasters (skip 2016-2019; no longer using)
files <- list.files(paste0(data.dir), full.names = TRUE)
# Get indices of only .img files
keeps <- grep(pattern = "Q5sc3.tif$", x = files)
# Retain only those keeps
(files <- files[keeps] %>% sort(.)) 


# Load all rasters directly into a stack.
remove(stack)
stack <- raster(files[1]) # initiate stack w 1st
for (i in 2:length(files)){ # start w 2nd
  stack <- addLayer(stack, raster(files[i])) # add other layers
}

# What are spatial parameters?
crs(stack) ; res(stack)
# CRS arguments: +proj=longlat +datum=WGS84 +no_defs 
# 0.0008084838 0.0008084838
nlayers(stack)


# Project from lat/long to equal area to get pixels into m (though they'll be diff x & y)
# Increase memory (else often fails)
memory.limit() #16122
memory.limit(size = 20000)

# Turn on/off resolution; specify nearest neighbor else defaults to bilinear interp.
start <- Sys.time()
# boo <- projectRaster(stack[[1]],crs = proj.crs, res = 90, method = 'ngb',)
# writeRaster(boo, filename = paste0(data.dir,names(stack[[1]]),"_",today,".tif"), format = "GTiff", overwrite = TRUE)
stackp <- projectRaster(stack, crs = proj.crs, res = 90, method = 'ngb')#,
                        # filename = paste0(data.dir,names(stackp),today), bylayer = TRUE, format = "GTiff")
print(Sys.time() - start) #~4hrs for 6 with no res setting; ~2 hrs for 5 with res at 90; ~22 hrs for 5 w res at 90, nearest neighbor
# names(stackp) <- paste0(names(stackp), "_p")
names(stackp) <- paste0(names(stackp), "_90m_p")

# Save rasters
start <- Sys.time()
writeRaster(stackp, filename = paste0(data.dir,names(stackp),"_",today), bylayer = TRUE, format = "GTiff" )
print(Sys.time() - start) # 5 hrs 5 at 90m
plot(stackp[[1]])
crs(stackp[[1]])
res(stackp[[1]]) #65.9 89.2 # 90 90



# Re-load
# List all rasters (skip 2016-2019; no longer using)
files <- list.files(paste0(data.dir), full.names = TRUE)
# Get indices of only .img files
keeps <- grep(pattern = "Q5sc3_90m_p_2021-12-24.tif$", x = files)
# Retain only those keeps
(files <- files[keeps] %>% sort(.)) 

stackp <- raster(files[1]) # initiate stack w 1st
for (i in 2:length(files)){ # start w 2nd
  stackp <- addLayer(stackp, raster(files[i])) # add other layers
}

plot(stackp[[5]])
freq(stackp[[5]])
plot(stack[[5]])
freq(stack)[[5]]






