# # Basic functions --------------------------------------------------------------
#
# # why is my data sideways??
# rotate <- function(x) {t(apply(x, 2, rev))}
#
# # Read weather data ------------------------------------------------------------
#
# # Fetch data from TerraClim, PRISM, or WorldClim
# # Will write to tmp directory or user-specified
# download_data <- function(database, years, lat.range, lon.range, variable, directory=tempdir()){
#
# }
#
# # Convert .bil to .nc for easy comparison
# convert_PRISM <- function(x){
#
# }
#
# # Read data from locally stored files
# read_data <- function(){
#
# }
#
# # Load data directly into the working environment
# select_layer <- function(layer=layer) {
#   delete_file <- !any(grep(paste("./terraclimate_", layer, ".nc", sep=""), list.files(full.names = T)))
#   ncdat <- ncdf4::nc_open(unzip(paste("./data/terraclimate_", layer, ".zip", sep="")))
#   if(delete_file){file.remove(paste("./terraclimate_", layer, ".nc", sep=""))}
#   return(ncdat)
# }
#
# open_layer <- function(layer=layer){
#   invisible(assign(layer, ncdf4::ncvar_get(select_layer(layer), layer),
#                    envir = .GlobalEnv))
# }
#
# # Define sites -----------------------------------------------------------------
#
# find_coords <- function(lookup, ref){
#   unlist(lapply(lookup, function(x){
#     which.min(abs(ref - x))
#
#   }))
# }
#
#
# upload_df <- function(x){
#   inFile <- x
#   if (is.null(inFile)){
#     df <- read.csv("./data/naba-sites.csv")[,c(4:5)]
#     colnames(df) <- tolower(colnames(df))
#   }else{
#     df <- read.csv(inFile$datapath, header = TRUE,sep = ",")
#   }
#   return(df)
# }
#
#
#
#
# # Extract variables ------------------------------------------------------------
#
#
# apply_function <- function(dat, measure=c("min", "max", "mean", "sum"), timeframe=c("seasonal", "annual", "monthly")){
#
#   if(timeframe%in%c("seasonal", "annual")){
#     if(!is.null(dim(dat))){
#       dat <- apply(dat, 1, measure, na.rm=T)
#     }
#   }
#   return(dat)
# }
#
# select_times <- function(all.years, selected.year,
#                          all.months, selected.months,
#                          timeframe){
#   if(timeframe=="annual"){
#     times_selected <- all.years%in%selected.year
#   }else{
#     times_selected <- (all.years%in%selected.year & all.months%in%selected.months)
#   }
#   return(times_selected)
# }
#
# get_metrics <- function(x){
#   times_selected <- select_times(selected.year = x,
#                                  all.years = all.years,
#                                  all.months = all.months,
#                                  selected.months = selected.months,
#                                  timeframe=timeframe)
#
#   dat <- calculate_metric(times_selected = times_selected,
#                           lat=lat, lon=lon, lat.range=lat.range, lon.range=lon.range,
#                           layer=layer, measure=measure, timeframe=timeframe)
#   return(dat)
# }
#
#
# calculate_metric <- function(lat, lon, lat.range, lon.range,
#                              layer, measure, timeframe,
#                              times_selected){
#
#   vars <- eval(as.name(layer))[find_coords(lon, lon.range), find_coords(lat, lat.range), times_selected]
#   if(!is.null(dim(vars))){
#     if(length(dim(vars))==3){
#       vars <- apply(vars, 3, diag)
#     }else{
#       vars <- diag(vars)
#     }
#   }
#
#   metrics <- apply_function(vars, measure, timeframe)
#   return(metrics)
# }
#
# generate_labels <- function(timeframe, layer, measure, years, months=NULL){
#   if(timeframe=="monthly"){
#     labels <- paste(layer, months, rep(years, each=length(months)), sep=".")
#   }
#   if(timeframe=="seasonal"){
#     labels <- paste(measure, timeframe, layer, paste(months, collapse="-"), years, sep=".")
#   }
#   if(timeframe=="annual"){
#     labels <- paste(measure, timeframe, layer, years, sep=".")
#   }
#   return(labels)
#
# }
#
#
# subset_data <- function(selected.years, lat=lat, lon=lon, lat.range=lat.range, lon.range=lon.range,
#                         layer=layer, measure=measure, timeframe=timeframe,
#                         all.years=all.years, all.months=all.months, selected.months=selected.months){
#   invisible(lapply(layer, open_layer))
#
#   #  selected_dat <-  cbind(lon, lat, matrix(sapply(selected.years, calculate_metric, layer=layer, lat=lat, lon=lon, lat.range=lat.range, lon.range=lon.range, measure=measure, times_selected=times_selected, timeframe=timeframe), byrow=F, nrow=length(lat)))
#   selected_dat <-  cbind(lon, lat, matrix(sapply(selected.years, get_metrics), byrow=F, nrow=length(lat)))
#   colnames(selected_dat) <- append(c("lat", "lon"), generate_labels(timeframe = timeframe, layer = layer, measure = measure, years = selected.years, months = selected.months))
#   rm(list=as.character(layer), envir = .GlobalEnv)
#   return(selected_dat)
# }
#
# # Climate departures -----------------------------------------------------------
#
# calc_trends <- function(x, method=c("sen", "ols")){
#   if(all(is.na(x))){
#     NA
#   }else{
#
#     if(method=="ols"){
#       huh <- as.numeric(lm(x ~ seq_along(x))$coefficients[2])
#       return(huh)
#     }
#
#     if(method=="sen"){
#       trend::sens.slope(x[!is.na(x), drop=F])$estimate
#     }
#
#   }
# }
#
#
# get_trends <- function(dat, trend.type){
#   dist.trends <- apply(dat, 1, function(x){
#     calc_trends(x, method=trend.type)
#   })
#   return(dist.trends)
# }
#
# sed <- function(f, p){
#   ((f - mean(p, na.rm=T))^2)/sd(p, na.rm=T)
# }
#
# calc_distance <- function(baseline, comparison, dist.metric=c("mahalanobis", "sed")){
#   if(dist.metric=="mahalanobis"){
#     if(matrixcalc::is.singular.matrix(cov(baseline))%in%c(TRUE, NA)){
#       d <- rep(NA, dim(comparison)[1])
#     }else{
#       d <- stats::mahalanobis(comparison, apply(baseline, 2, mean, na.rm=T), cov(baseline))
#
#     }
#   }
#   if(dist.metric=="sed"){
#     d <- rowSums(sed(comparison, baseline))
#   }
#   return(d)
# }
#
#
# get_distances <- function(vars, scope=c("site", "study"), baseline.years, comparison.years, dist.method=c("mahalanobis", "sed")){
#   user.years <- sort(unique(c(baseline.years, comparison.years)))
#   baselineperiod <- user.years %in% baseline.years
#   comparisonperiod <- user.years %in% comparison.years
#
#   merged_dat <- do.call(cbind, lapply(vars, function(x){x}))
#   if(any(merged_dat=="Inf" & !is.na(merged_dat))){
#     merged_dat[merged_dat=="Inf"] <- NA
#   }
#   if(any(merged_dat=="-Inf" & !is.na(merged_dat))){
#     merged_dat[merged_dat=="-Inf"] <- NA
#   }
#
#   types <- rep(c(rep("base", length(baselineperiod)), rep("comp", length(comparisonperiod))), length(vars))
#
#   # re-apply matrix to sort out dimension error in calc_distance
#   # baseline vs comparison issue solved by new var user.years (kluuuudge)
#   if(scope=="site"){
#     d.mat <- apply(merged_dat, 1, function(x){
#       calc_distance(matrix(matrix(as.numeric(x), ncol=length(vars))[baselineperiod,], ncol=length(vars)),
#                     matrix(matrix(as.numeric(x), ncol=length(vars))[comparisonperiod,], ncol=length(vars)), dist.method)
#     })
#   }
#   if(scope=="study"){
#
#     #### RERUN PACKAGE WITH FIX
#     overall_baseline <- apply(merged_dat[,types=="base"], 2, mean, na.rm=T)
#
#     d.mat <- apply(merged_dat[,types=="comp"], 1, function(x){
#       calc_distance(matrix(overall_baseline, ncol=length(vars)),
#                     matrix(x, ncol=length(vars)), dist.method)
#
#     })
#   }
#
#   return(d.mat)
# }
#
#
#
#
# # Climate analogs --------------------------------------------------------------
#
# approximate_bounds <- function(lat, lon, dist, add_buffer=0.01){
#   earth <- 6378137
#   off.lat <- dist/earth + add_buffer
#   off.lon <- dist/(earth*cos(pi*lat)/180) + add_buffer
#
#   bounds <- matrix(c(lat-off.lat, lat+off.lat, lon-off.lon, lon+off.lon), ncol=2, byrow=T)
#   colnames(bounds) <- c("min", "max")
#   rownames(bounds) <- c("lat", "lon")
#   return(bounds)
# }
#
# # Note: need to make grid size interpretable
# clip_site <- function(lat, lon, radius, grid.size){
#   bounding.box <- approximate_bounds(lat, lon, dist=radius)
#   poss.lat <- seq(bounding.box[1], bounding.box[3], length.out=grid.size)
#   poss.lon <- seq(bounding.box[2], bounding.box[4], length.out=grid.size)
#
#   coords <- data.frame(latitude=(rep(poss.lat, length(poss.lon))), longitude=rep(poss.lon, each=length(poss.lat)))
#
#   rad.dist <- apply(coords, 1, function(x){
#     geosphere::distHaversine(c(lon, lat), rev(x))
#   })
#   within_distance <- coords[rad.dist<=radius,]
#   return(within_distance)
# }
#
# area_analogs <- function(departures, threshold){
#   apply(departures, 2, function(x){
#     sum(x<=threshold)
#   })
# }
#
#
#
# # Download data ----------------------------------------------------------------
#
# find_files <- function() {
#   rev(list.files(tmpdirectory,
#                  recursive = T,
#                  full.names = T)[grep(
#                    "shiny-terraclim-downloads",
#                    list.files(tmpdirectory, recursive = T, full.names = T)
#                  )])
# }
#
# merge_files <- function(){
#   files.list <- find_files()
#   myMergedData <- do.call(cbind, lapply(files.list, read.csv))
#   return(myMergedData)
# }
#
# purge_directory <- function(){
#   files.list <- find_files()
#   invisible(file.remove(files.list))
# }
#
#
# # Plots ------------------------------------------------------------------------
#
# plot_trends <- function(trends, type=c("sites", "regional"), mapdim){
#
#   if(type=="sites"){
#     load("./data/west-shape.rda")
#     raster::plot(west, col="grey90", axes=F, legend=F, box=F)
#
#     points(lat ~ lon, cex=2, pch=21,
#            bg=viridis::rocket(256, begin=1, end=0)[cut(trends, 256)])
#   }
#
#
#   if(type=="regional"){
#     trends <- array(trends, dim=c(mapdim,1))
#
#     rightside.up <- rotate(trends[,,1])
#
#     trend.map <- raster::raster(t(rightside.up))
#
#     raster::plot(trend.map, col=viridis::rocket(256, begin=1, end=0), axes=F, legend=T, box=F)
#   }
#
#
# }
