today <- Sys.Date()

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

# Create template from biome
template <- raster(extent(biome),
                   crs = proj.crs,
                   res = 90,
                   vals = 1)
plot(template)
res(template)


# Load ecoregions
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

plot(eco, max.plot = 14)
remove(gb, gp, keeps)

# Test case of Great Basin
gb <- st_cast(eco[eco$group == "Great Basin",], "MULTIPOLYGON")
# plot(gb)
# # Raster will need numeric; add and create look-up
# gb$US_L3CODE <- as.numeric(gb$US_L3CODE)
# lu.gb <- data.frame(gb$US_L3CODE,gb$US_L3NAME) 
# # Rasterize
# gb.r <- fasterize(gb, template, field = "US_L3CODE")
# plot(gb.r)



##########################################################
## Load cores (defend), grow, mitigate zones into stack ##
##########################################################

# List all rasters
files <- list.files(paste0(data.dir), full.names = TRUE)
# Get indices of only .img files
keeps <- grep(pattern = "Q5sc3.tif", x = files)
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

## FIXME: set resolution to 90x90 when projecting
start <- Sys.time()
stackp <- projectRaster(stack, crs = proj.crs)
print(Sys.time() - start) #~4hrs
names(stackp) <- paste0(names(stackp), "_p")

# Save rasters
writeRaster(stackp, filename = paste0(data.dir,names(stackp)), bylayer = TRUE, format = "GTiff" )
plot(stackp[[1]])
crs(stackp[[1]])
res(stackp[[1]]) #65.9 89.2

remove(stack)


# Create loop 
yr <- vector()
zone <- vector()
temp <- vector()
cnt <- vector()
sqm <- vector()
sqkm <- vector()

start <- Sys.time()
# for (i in 1){
for (i in 1:nlayers(stackp)){
  for (j in c(1,2,3)) {
    yr <- c(yr, as.numeric(mid(names(stackp[[i]]), 13, 4)))
    zone <- c(zone, paste0("zone",j))
    temp1 <- freq(stackp[[i]], value = j)
    cnt <- c(cnt, temp1)
    temp2 <- round(temp1 * res(stackp[[i]])[1] * res(stackp[[i]])[2],0)
    sqm <- c(sqm, temp2)
    sqkm <- c(sqkm, round(temp2/1000000,0))
  }
}
gc() # free unused memory
print(Sys.time() - start) #35 min

remove(summary)

# Combine into dataframe, though all end up as characters so convert (but not zone)
(summary <- as.data.frame(cbind(yr, zone, cnt, sqm, sqkm)))
summary <- summary %>%
  mutate_at(vars(yr, cnt, sqm, sqkm), as.numeric) 
str(summary)
write.csv(summary, paste0(out.dir, "core_cnts_area_", today, ".csv"))

sum(summary[1:3,5])



# Return number of pixels with core values
freq(stackp[[1]], value = 1) # 47748631
freq(stackp[[1]], value = 2) # 61437686
freq(stackp[[1]], value = 3) # 27323692

# Return number of pixels with core values
freq(stackp[[6]], value = 1) # 31728169
freq(stackp[[6]], value = 2) # 56919352
freq(stackp[[6]], value = 3) # 28863346

# ref: https://stackoverflow.com/questions/40698818/calculating-area-of-raster-with-certain-values-in-r
foo <- stackp[[1]]
sum(stackp[[1]][] == 1) # NA
sum(stackp[[1]][] == 1) * res(stackp[[1]])[1] * res(stackp[[1]])[2]
sum(foo[] == 1) # NA
sum(foo[] > 1, na.rm = TRUE) # 94651486



?area

# ref: https://stackoverflow.com/questions/43629657/calculate-area-for-different-land-cover-classes-in-a-raster-in-r
aggregate(getValues(area(foo, weights=FALSE)), by=list(getValues(foo)), sum)
?getValues

# # Try with lapply.
# start <- Sys.time()
# stackp <- lapply(X = stack, FUN = projectRaster, crs = proj.crs)
# print(Sys.time() - start)
# 
# # Try in parallel (ref: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html)
# numCores <- detectCores()
# start <- Sys.time()
# stackp <- mclapply(x = stack, FUN = projectRaster, crs = proj.crs, mc.cores = numCores)
# print(Sys.time() - start)




# Extract mean raster values to GB units
start <- Sys.time()
vals <- raster::extract(stack[[1]], gb,
                        fun = mean, na.rm = TRUE)#,
                        # start with layer 1, do all 6 layers, stick in df
                        # layer = 1, nl = 6, df = TRUE)
print(Sys.time() - start) # 2.31192 hours



foo <- raster(extent(gb),
                   crs = "+proj=longlat +datum=WGS84 +no_defs",
                   # res = 90,
                   vals = 1)
crs(foo)
boo <- fasterize(gb, foo)
crs(boo)
plot(boo)
start <- Sys.time()
zoo <- projectRaster(boo, crs = proj.crs)
print(Sys.time() - start)
