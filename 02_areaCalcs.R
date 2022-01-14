###########################################################
###########################################################
###########################################################
## IF WORKING WITH GEE RESULTS, SKIP THIS TIF PROCESSING ##
### (OR SKIP TO THE BOTTOM AND LOAD SAVED CSV RESULTS) ####
###########################################################
###########################################################
###########################################################



# ##################################################
# ## Calc area of each zone within each timeframe ##
# ##################################################
# 
# # Create loop 
# yr <- vector() 
# zone <- vector()
# temp <- vector()
# cnt <- vector()
# sqm <- vector()
# sqkm <- vector()
# 
# start <- Sys.time()
# # for (i in 1){
# for (i in 1:nlayers(stackp)){
#   for (j in c(1,2,3)) {
#     yr <- c(yr, as.numeric(mid(names(stackp[[i]]), 13, 4)))
#     zone <- c(zone, paste0("zone",j))
#     temp1 <- freq(stackp[[i]], value = j)
#     cnt <- c(cnt, temp1)
#     # If res not specified during projection, x and y will differ
#     temp2 <- round(temp1 * res(stackp[[i]])[1] * res(stackp[[i]])[2],0)
#     sqm <- c(sqm, temp2)
#     # sqkm <- c(sqkm, round(temp2/1000000,0))
#   }
# }
# gc() # free unused memory
# print(Sys.time() - start) #35 min ; 15 min for 5 layers w 90 m 
# 
# # Combine into dataframe, though all end up as characters so convert (but not zone)
# remove(temp1, temp2, summary)
# (summary <- as.data.frame(cbind(yr, zone, cnt, sqm, sqkm)))
# summary <- summary %>%
#   mutate_at(vars(yr, cnt, sqm), as.numeric) %>%
#   mutate(sqkm = round(sqm/1000000,0),
#          acres = round(sqkm * 247.105, 0),
#          acres_mil = round(acres / 1000000, 0))
# 
# # str(summary)
# # write.csv(summary, paste0(out.dir, "core_cnts_area_", today, ".csv"), row.names = FALSE)


summary <- read.csv(paste0(out.dir, "core_cnts_area_2021-12-27.csv"))[,2:6] %>%
  mutate(acres_mil = round(acres / 1000000, 0))


p <- ggplot(data = summary, aes(x = yr, y = acres_mil)) + geom_point() + geom_smooth(method=lm)
p + facet_wrap(~zone)

p <- ggplot(data = summary, aes(x = yr, y = acres_mil, fill = zone)) +
  geom_bar(position="stack", stat="identity")
p

