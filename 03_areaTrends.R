#####################################
## TREND ANALYSIS WITH GEE RESULTS ##
#####################################

today <- Sys.Date()


#############################
## Set-up for stressor trends

# Load data
data <- read.csv("G:/My Drive/2FWS Sagebrush/FWS Sagebrush/analyses/output/allStressors_210113.csv")

# Put all severities into one column
data <- data %>% 
  pivot_longer(cols = c("low", "mod", "high", "vhigh"), names_to = "severity", values_to = "area")

# Minor column tweaks
# Calc million acres
data$area <- round(data$area/1000000,1)
# Turn eco into a factor and re-order existing levels and rename levels
data$eco <- factor(data$eco, levels = c("greatBasin", "greatPlains", "intmtWest"))
levels(data$eco) = c("Great Basin", "Great Plains", "Intermountain West")
# Turn stressors into a factor and re-order existing levels and rename levels
data$stressor <- factor(data$stressor, levels = c("annualGrass", "trees", "hmi"))
levels(data$stressor) = c("annual grass", "conifers", "human modification")
# Turn stressor into an ordered factor and rename levels
data$severity <- ordered(data$severity,
                      levels = c("vhigh", "high", "mod", "low"))
levels(data$severity) <- c("v. high", "high", "mod.", "low")



##############################
## Stressor plots by ecoregion

# Subset to each ecoregion
gb <- data %>% filter(eco == "greatBasin")
gp <- data %>% filter(eco == "greatPlains")
iw <- data %>% filter(eco == "intmtWest")

# Plot for each ecoregion
display.brewer.all(4)
display.brewer.pal(4, "BrBG")

plot_gb <- ggplot(data = gb, aes(x = year, y = area, fill = severity)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~stressor) +
  labs(title = "Trends in stressors within the Great Basin",
       y = "millions of acres") +
  scale_fill_brewer(palette = "BrBG")
plot_gb # 10 missing rows b/c no very high for annual grass/hmi

plot_gp <- ggplot(data = gp, aes(x = year, y = area, fill = severity)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~stressor) +
  labs(title = "Trends in stressors within the Great Plains",
       y = "millions of acres")  +
  scale_fill_brewer(palette = "BrBG")
plot_gp # 10 missing rows b/c no very high for annual grass/hmi

plot_iw <- ggplot(data = iw, aes(x = year, y = area, fill = severity)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~stressor)  +
  labs(title = "Trends in stressors within the Intermountain West",
       y = "millions of acres")  +
  scale_fill_brewer(palette = "BrBG")
plot_iw # 10 missing rows b/c no very high for annual grass/hmi


# Save
pdf(paste0(out.dir, "gb_stressor_trends_", today, ".pdf"), width = 8, height = 4) 
print(plot_gb)
dev.off()

pdf(paste0(out.dir, "gp_stressor_trends_", today, ".pdf"), width = 8, height = 4) 
print(plot_gp)
dev.off()

pdf(paste0(out.dir, "iw_stressor_trends_", today, ".pdf"), width = 8, height = 4) 
print(plot_iw)
dev.off()


#############################
## Stressor plots by stressor

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


##############################
## Stressor plots by ecoregion

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

# plot(plots_ecoregion)
# plot(plots_ecoregion[[2]])
# plot(plots_ecoregion[[3]])


