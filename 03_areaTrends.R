#####################################
## TREND ANALYSIS WITH GEE RESULTS ##
#####################################

today <- paste0(mid(Sys.Date(),3,2),
                mid(Sys.Date(),6,2),
                mid(Sys.Date(),9,2))




##########################################
## Set-up for stressor trends by ecoregion

# Load data
data <- read.csv("G:/My Drive/2FWS Sagebrush/FWS Sagebrush/analyses/output/stressorsXlevelXecoregion_220113.csv")

# Put all severities into one column
data <- data %>% 
  pivot_longer(cols = c("low", "mod", "high", "vhigh"), names_to = "severity", values_to = "area")

# Minor column tweaks
# Calc million acres
data$area <- round(data$area/1000000,1)
# Turn eco into a factor and rename levels
(data$eco <- factor(data$eco))
levels(data$eco) = c("Great Basin", "Great Plains", "Intermountain West")
# Turn stressors into a factor, re-order existing levels (otherwise alph), and rename levels
data$stressor <- factor(data$stressor, levels = c("annualGrass", "trees", "hmi"))
levels(data$stressor) = c("Invasive annual grass", "Conifers", "Human modification")
# Turn stressor into an ordered factor and rename levels
data$severity <- ordered(data$severity,
                      levels = c("vhigh", "high", "mod", "low"))
levels(data$severity) <- c("v. high", "high", "mod.", "low")

# Nb tidyverse uses forcats w/ e.g., fct_rev for on-the-fly re-ordering in plots. 
# But here I'm just setting factors in desired order to avoid worrying about legend titles



###################################
## plots: 1 stressor x 3 ecoregions

# Create list to store plots in
plots_stressor <- list()

# Loop through the three stressors
for (i in 1:length(levels(data$stressor))){
  name <- paste0(levels(data$stressor)[i])
  
  # Subset data to each stressor
  temp <- data %>% filter(stressor == paste0(name))
  plot <- ggplot(temp, aes(x = year, y = area, fill = severity)) +
    geom_bar(position="stack", stat="identity") +
    facet_wrap(~eco) + # Plot by ecoregion
    labs(title = paste0("Trends in ", name),
         y = "millions of acres") +
    scale_fill_brewer(palette = "BrBG")
  
    # Save plot
  pdf(paste0(out.dir,"plots/", "trend_stressor_", name, "_", today, ".pdf"), width = 8, height = 4) 
  print(plot) # 15 missing rows b/c no very high for annual grass/hmi
  dev.off()
  
  # Store plot in list
  plots_stressor[[i]] <- plot
  }

# plot(plots_stressor[[1]])
# plot(plots_stressor[[2]])
# plot(plots_stressor[[3]])



###################################
## plots: 1 ecoregion x 3 stressors

# Create list to store plots in
plots_ecoregion <- list()

# Loop through the three stressors
for (i in 1:length(levels(data$eco))){
  name <- paste0(levels(data$eco)[i])
  
  # Subset data to each ecoregion
  temp <- data %>% filter(eco == paste0(name))
  plot <- ggplot(temp, aes(x = year, y = area, fill = severity)) +
    geom_bar(position="stack", stat="identity") +
    facet_wrap(~stressor) + # Plot by stressor
    labs(title = paste0("Trends in stressors within the", name),
         y = "millions of acres") +
    scale_fill_brewer(palette = "BrBG")
  
  # Save plot
  pdf(paste0(out.dir,"plots/", "trend_ecoreg_", name, "_", today, ".pdf"), width = 8, height = 4) 
  print(plot) # 10 missing rows b/c no very high for annual grass/hmi
  dev.off()
  
  # Store plot in list
  plots_ecoregion[[i]] <- plot
}

# plot(plots_ecoregion[[1]])
# plot(plots_ecoregion[[2]])
# plot(plots_ecoregion[[3]])






###################################
## plots: 1 ecoregion x 3 stressors

# Create list to store plots in
plots_ecoregion <- list()

# Loop through the three stressors
for (i in 1:length(levels(data$eco))){
  name <- paste0(levels(data$eco)[i])
  
  # Subset data to each ecoregion
  temp <- data %>% filter(eco == paste0(name))
  plot <- ggplot(temp, aes(x = year, y = area, fill = severity)) +
    geom_bar(position="stack", stat="identity") +
    facet_wrap(~stressor) + # Plot by stressor
    labs(title = paste0("Trends in stressors within the ", name),
         y = "millions of acres") +
    scale_fill_brewer(palette = "BrBG")
  
  # Save plot
  pdf(paste0(out.dir,"plots/", "trend_ecoreg_", name, "_", today, ".pdf"), width = 8, height = 4) 
  print(plot) # 10 missing rows b/c no very high for annual grass/hmi
  dev.off()
  
  # Store plot in list
  plots_ecoregion[[i]] <- plot
}

# plot(plots_ecoregion[[1]])
# plot(plots_ecoregion[[2]])
# plot(plots_ecoregion[[3]])









###################################################
## Set-up for 2020 stressors X 3 ecoregion X 3 cores

# Cores are Core Sagebrusy Areas, Growth Opp Areas, Highly Impacted Areas

# Load data
data <- read.csv("G:/My Drive/2FWS Sagebrush/FWS Sagebrush/analyses/output/stressorsXlevelXecoregionXcores_2020_220120.csv")

# Put all severities into one column
data <- data %>% 
  pivot_longer(cols = c("low", "mod", "high", "vhigh"), names_to = "severity", values_to = "area")

# Minor column tweaks
# No need for year
data$year <- NULL
# Calc million acres
data$area <- round(data$area/1000000,1)
# Turn eco into a factor and rename levels
(data$eco <- factor(data$eco))
levels(data$eco) = c("Great Basin", "Great Plains", "Intermountain West")
# Turn stressors into a factor, re-order existing levels (otherwise alph), and rename levels
data$stressor <- factor(data$stressor, levels = c("annualGrass", "trees", "hmi"))
levels(data$stressor) = c("Invasive annual grass", "Conifers", "Human modification")
# Turn stressor into an ordered factor and rename levels
data$severity <- ordered(data$severity,
                         levels = c("vhigh", "high", "mod", "low"))
levels(data$severity) <- c("v. high", "high", "mod.", "low")


################################################
## table: 2020 stressors X 3 ecoregion X 3 cores

# Combine ecoregions
data <- data %>%
  group_by(stressor, cores, severity) %>%
  summarise(area = sum(area, na.rm = TRUE))

data <- data %>%
  pivot_wider(names_from = severity,
              values_from = area)

write.csv(data, paste0(out.dir, "stressor_table_", today,".csv"))












##############################
## Set-up for core area trends

# Load data
data <- read.csv("G:/My Drive/2FWS Sagebrush/FWS Sagebrush/analyses/output/cores_220113.csv")


# Put all severities into one column
data <- data %>% 
  pivot_longer(cols = c("zone1", "zone2", "zone3"), names_to = "zone", values_to = "area")

# Minor column tweaks
# Calc million acres
data$area <- round(data$area/1000000,1)
# Turn eco into a factor and rename levels
(data$eco <- factor(data$eco))
levels(data$eco) = c("Great Basin", "Great Plains", "Intermountain West")
# Turn zone into a factor, reorer levels, and rename levels
(data$zone <- factor(data$zone, levels = c("zone3", "zone2", "zone1")))
(levels(data$zone) <- c("HIA", "GOA", "CSA"))



###############################
## plot: cores x 3 ecoregions

# Plot by ecoregion
plot <- ggplot(data, aes(x = year, y = area, fill = zone)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~eco) +
  labs(title = paste0("Trends in core areas"),
       y = "millions of acres") +
  scale_fill_brewer(palette = "BrBG")
plot

# Save plot
pdf(paste0(out.dir,"plots/", "trend_core_by_ecoregion_", today, ".pdf"), width = 8, height = 4) 
print(plot) # 15 missing rows b/c no very high for annual grass/hmi
dev.off()


#######################
## Core plots biomewide

colors <- c("#EBDEB7", "#9ECAE1", "#002769") # tan, light blue, dark blue

# Plot by ecoregion; reverse factor order so cores are are bottom.
plot <- ggplot(data, aes(x = year, y = area, fill = zone)) +
  geom_bar(position="stack", stat="identity") +
  labs(y = "millions of acres") + #,
       # title = paste0("Trends in core areas"#),
  scale_fill_manual(values = colors) +
  theme_classic(base_size = 16) +
  theme(legend.title = element_blank(),
        legend.position="right",
        legend.margin=ggplot2::margin(c(rep(0,3),-15)), #t,r,b,l
        legend.box.margin=ggplot2::margin(rep(0,4)),
        text = element_text(family = 'serif')) # R's default fonts incl. serif
        
plot


# Save plot
pdf(paste0(out.dir,"plots/", "trend_core_biomewide_", today, ".pdf"), width = 8, height = 4) 
print(plot) # 15 missing rows b/c no very high for annual grass/hmi
dev.off()


display.brewer.pal(4, "BrBG")
display.brewer.pal(3, "BrBG")

  