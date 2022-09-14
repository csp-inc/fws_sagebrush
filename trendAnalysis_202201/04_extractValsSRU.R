# ------------------------------------------------------------------------------
# Extract values to SRUs
# ------------------------------------------------------------------------------

## Set up work space
setwd("G:/My Drive/2FWS Sagebrush/FWS Sagebrush/04_methods_analyses/")
wd <- "G:/My Drive/2FWS Sagebrush/FWS Sagebrush/04_methods_analyses/"


## Function to load features, set common crs, and fix any invalid geometries
load_f <- function(f) {
  proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  read_sf(f) %>%
    st_transform(proj.crs) %>%
    st_make_valid() %>%
    st_buffer(dist = 0)
}
proj.crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"


## Load data, incl multiband rasters as stack. Here, annual grass.
sru <- load_f("data/Draft_WestWideSRUs.shp")
(ag20 <- raster("data/annual grass 2020.tif"))
(agAll <- stack("data/multiband_rasters/ag_1km.tif"))


## Extract mean ag cover in 2020 to SRUs, round to 2 decimal places, stow in df
start <- Sys.time()
boo <- exact_extract(ag20, sru, fun = "mean") %>% round(2) %>% as.data.frame()
(end <- start - Sys.time()) # ~25 sec CL machine
view(boo)


## Loop to process all years at once (nb levels/yn summaries are kinda meaningless)
start <- Sys.time()
foo <- exact_extract(agAll, sru, fun = "mean") %>% round(2) %>% as.data.frame()
(end <- start - Sys.time())  # ~30 sec CL machine

par(mfrow=c(1,2))
plot(agAll[[1]])
plot(agAll[[5]])
