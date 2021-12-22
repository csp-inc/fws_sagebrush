######################################
## SUMMARIZE & PLOT RATES OF CHANGE ##
######################################

## Create simply linear model on change in cover for each zone over time

# Ref: https://stackoverflow.com/questions/37395059/running-several-linear-regressions-from-a-single-dataframe-in-r
# Ref: https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
# Store each zone's model (as a list) in a column of a dataframe. 
models <- data %>% group_by(zone) %>% do(fitSage = lm(sqkm ~ yr, data=.))
# Can see summaries of each model with the following  
tidy(models$fitSage[[1]]) #; summary(models$fitSage[[1]])
tidy(models$fitSage[[2]]) #; summary(models$fitSage[[2]])
# Alt for summaries:
# data %>% group_by(zone) %>%
#   do(fitSage = augment(lm(sqkm ~ yr, data = .))) %>%
#   unnest(fitSage)
# Alt for looping models:
# https://stackoverflow.com/questions/36695360/apply-formula-to-every-column-of-dataframe-in-r
# Exclude yr column from apply, apply to columns (2), put models into list
# list.mods <- apply(foo[ ,-1], 2, function(x) lm(x ~ foo$yr))



########################################################

## Predict sagebrush cover for each region in future year
# New dataframe must have same # variables; leave zone empty
new <- data.frame(yr = as.numeric(2050),
                  zone = as.character("zone_dummy")) 

# Create loop to predict future values and rates of change
zone <- vector()
rate <- vector() # for rates of change (coefficient of x)
sqkm.x50 <- vector() # for y when x = 2050
yr.y0 <- vector() # for x (yr) when y = 0
yrs.til <- vector() # num yrs til y = 0

# Loop through each mode; inverse predict from chemCal package
# Often calling as list
for (i in 1:length(models$fitSage)){
  zone <- c(zone, models$zone[[i]])
  rate <- c(rate, models$fitSage[[i]]$coefficients[[2]]) # gives list output; take only yr coeff (slope)
  sqkm.x50 <- c(sqkm.x50, predict(models$fitSage[[i]], newdata = new)[[1]]) # [[1]] to remove name
  yr.y0 <- c(yr.y0, inverse.predict(models$fitSage[[i]], 0)[[1]]) # gives list output; take 1st item (ie prediction)
  yrs.til <- c(yrs.til, inverse.predict(models$fitSage[[i]], 0)[[1]] - 2021) # subtract prediction from now
}

(summary <- as.data.frame(cbind(zone, rate, sqkm.x50, yr.y0, yrs.til)))


########################################################
## Plot trendlines for each zone

p <- ggplot(data = data, aes(x = yr, y = sqkm)) + geom_point()
p + facet_wrap(~zone) + theme_caitlin()



######################################################
## Plot slope to map
# Join summary data to shapefile. Set common column name for joining
nrow(summary) # 6
length(nv) # 6
summary$id <- c(1:nrow(summary))

nv.summ <- left_join(nv, summary, by = c("id" = "id"))

m <- ggplot(nv.summ) +
  geom_sf(aes(fill = rate))

m
