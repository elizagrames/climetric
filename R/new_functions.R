#' Generate a script to download NetCDF files
#' @description Generate a bash script that can be run to download data from the
#' Climatology Lab. Currently only TerraClimate is supported.
#' @param bbox an object of class extent indicating the bounding box, in latitude and longitude, to download (i.e. min and max lon and lat)
#' @param layers a character vector indicating which climate/weather layers to download
#' @param startyear an integer indicating the first year of data to download
#' @param endyear an integer indicating the last year of data to download
#' @param db a character vector indicating which database to use; currently only TerraClimate is supported
#' @param writefile logical; if TRUE, the resulting bash script is written locally
#' @param directory a path to a local directory where the bash script should be written if writefile=TRUE
#' @return if writefile=TRUE, a .sh file in the specified directory; if writefile=FALSE, a character vector
#' @details
#' If downloading TerraClimate data, layers should correspond to the 17 TerraClimate variables:
#' \describe{
#' \item{aet}{aet: Actual Evapotranspiration, monthly total (mm)}
#' \item{def}{def: Climate Water Deficit, monthly total (mm)}
#' \item{pet}{pet: Potential evapotranspiration, monthly total (mm)}
#' \item{ppt}{ppt: Precipitation, monthly total (mm)}
#' \item{q}{q: Runoff, monthly total (mm)}
#' \item{soil}{soil: Soil Moisture, total column - at end of month (mm)}
#' \item{srad}{srad: Downward surface shortwave radiation (W/m2)}
#' \item{swe}{swe: Snow water equivalent - at end of month (mm)}
#' \item{tmax}{tmax: Max Temperature, average for month (C)}
#' \item{tmin}{tmin: Min Temperature, average for month (C)}
#' \item{vap}{vap: Vapor pressure, average for month (kPa)}
#' \item{ws}{ws: Wind speed, average for month (m/s)}
#' \item{vpd}{vpd: Vapor Pressure Deficit, average for month (kpa)}
#' \item{PDSI}{PDSI: Palmer Drought Severity Index, at end of month (unitless)}
#' }
generate_wget <- function(bbox, layers, startyear, endyear,
                          db="TerraClimate",
                          writefile=TRUE, directory="./"){

  if(db=="TerraClimate"){
    base_script <- climetric::skeleton_wget
    base_script <- gsub("user.min.lon", bbox[1], base_script)
    base_script <- gsub("user.max.lon", bbox[2], base_script)
    base_script <- gsub("user.min.lat", bbox[3], base_script)
    base_script <- gsub("user.max.lat", bbox[4], base_script)
    base_script <- gsub("user.start.year", startyear, base_script)
    base_script <- gsub("user.end.year", endyear, base_script)
    clean_layers <- paste("'", layers, "'", sep="")

    clean_layers <- paste(clean_layers, collapse=" ")

    base_script <- gsub("user.variables", clean_layers, base_script)

    if(writefile){
      writeLines(base_script, paste(directory, "download_data.sh", sep=""))
    }else{
      return(base_script)
    }



  }

  if(db=="MACA"){
print("Eliza has not written the script logic for MACA downloads yet, use the MACA data portal: https://climate.northwestknowledge.net/MACA/data_portal.php")
  }

}

#' Run a bash script to download NetCDF files
#' @description Given a bash script produced by generate_wget(), runs the script
#' and downloads results to the specified directory
#' @param script.path a bash file (ext = .sh) including the path to the file
#' @param directory a path to a directory indicating where results should be stored
#' @return downloads NetCDF files to the users' machine
run_wget <- function(script.path, directory="./"){
  command <- paste("bash", script.path)
  system(command)
}

#' Converts coordinates or polygons to bounding boxes
#' @description If using a list of coordinates (e.g. study sites) or a polygon
#' (e.g. a species range map) to define the area of interest, this will convert
#' it to an extent
#' @param lat a numeric or character vector of latitudes
#' @param lon a numeric or character vector of longitudes
#' @param polygon an object of class SpatialPolygons
#' @return an object of class extent
make_extent <- function(lat=NULL, lon=NULL, polygon=NULL){
  if(!is.numeric(lat) & !is.null(lat)){
    lat <- as.numeric(lat)
  }
  if(!is.numeric(lon) & !is.null(lon)){
    lon <- as.numeric(lon)
  }

  if(!is.null(polygon)){
    output <- raster::extent(polygon)
  }else{
    output <- raster::extent(min(lon), max(lon), min(lat), max(lat))
  }

return(output)
}

#' Converts a NetCDF file to a RasterBrick
#' @description Takes NetCDF files from the Climatology Lab and converts them into
#' a RasterBrick object
#' @param file a path to a NetCDF (ext = .nc) file
#' @param layername a string indicating the name of the layer if different from the filename
#' @return a RasterBrick object
brick_nc <- function(file, layername=NULL){

  # guess name from file path
  if(is.null(layername)){
    layername <- gsub("\\.nc", "", strsplit(file, "_")[[1]][2])
  }
  ncdat <- ncdf4::nc_open(file)
  lat <- ncdf4::ncvar_get(ncdat, "lat")
  lon <- ncdf4::ncvar_get(ncdat, "lon")

  crs.proj <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")

  layer <- raster::brick(ncdf4::ncvar_get(ncdat, layername),
                         xmn=min(lat), xmx=max(lat),
                         ymn=min(lon), ymx=max(lon),
                         crs=crs.proj)
  layer <- raster::t(layer)
  return(layer)

}

#' Associate brick layers with seasons
#' @description Determine which raster layers correspond to which times of year based
#' on what each layer represents and which hemisphere to use; currently, only
#' temperate seasons are supported.
#' @param brick a RasterBrick object
#' @param layer_type a character vector indicating if each layer represents a month or a year
#' @param hemisphere a character vector indicating if temperate northern or southen hemisphere seasons should be used
#' @return a named list of numbers indicating which layer belongs to which time classification
define_seasons <- function(brick, layer_type="month", hemisphere="northern"){
  if(layer_type=="month"){
    nyears <- dim(brick)[3]/12
    if(hemisphere=="northern"){
      summer <- c(rep(NA, 5), rep(1, 3), rep(NA, 4))*rep(1:nyears, each=12)
      fall <- c(rep(NA, 8), rep(1, 3), NA)*rep(1:nyears, each=12)
      winter <- append(c(NA, NA), c(rep(NA, 9), rep(1, 3))*rep(1:nyears, each=12))[1:length(fall)]
      spring <- c(rep(NA, 2), rep(1, 3), rep(NA, 7))*rep(1:nyears, each=12)-1
    }

    if(hemisphere=="southern"){
      winter <- c(rep(NA, 5), rep(1, 3), rep(NA, 4))*rep(1:nyears, each=12)
      spring <- c(rep(NA, 8), rep(1, 3), NA)*rep(1:nyears, each=12)
      summer <- append(c(NA, NA), c(rep(NA, 9), rep(1, 3))*rep(1:nyears, each=12))[1:length(fall)]
      fall <- c(rep(NA, 2), rep(1, 3), rep(NA, 7))*rep(1:nyears, each=12)-1
    }
    annual <- rep(1:nyears, each=12)
    wateryear <- rep(1:nyears, each=12)
    wateryear <- append(rep(0, 9), wateryear)+1
    wateryear <- wateryear[1:length(annual)]

    seasons <- list(winter, spring, summer, fall, annual, wateryear)
    names(seasons) <- c("winter", "spring", "summer", "fall", "annual", "wateryear")

  }else{
    nyears <- dim(brick)[3]
    annual <- rep(1:nyears)
    seasons <- list(annual)
    names(seasons) <- "annual"
  }

return(seasons)
}

#' Extract climate and weather variables
#' @description Pulls climate or weather variables from a RasterBrick for
#' different time periods and summarizes them by simple metrics
#' @param brick an object of class RasterBrick
#' @param measures a character vector indicating how variables should be summarized (allows mean, min, max, and sum)
#' @param timeframe a character vector indicating the time over which variables should be summarized (allows annual, winter, spring, summer, fall, or wateryear)
#' @param writefile logical; if TRUE, saves a .csv file to the users' machine
#' @param varname a character vector indicating which climate or weather variable you are extracting (e.g. tmin, tmax)
#' @param layer_type a character vector indicating if each layer represents a month or a year
#' @param hemisphere a character vector indicating if temperate northern or southen hemisphere seasons should be used
#' @param directory a path to where files should be written if writefile=TRUE
#' @return if writefile=TRUE, .csv files of matrices; if writefile=FALSE, a list of lists of matrices
extract_data <- function(brick, measures=c("mean"),
                         timeframe=c("annual", "winter", "spring", "summer", "fall"),
                         writefile=T, varname=NULL, layer_type="month", hemisphere="northern",
                         directory="./"){
  if(is.null(varname)&writefile){
    print("Hint: you might want to specify a variable name so you know which .csv file is which if you are using multiple variables!")
  }

  get_seasons <- define_seasons(brick=brick, layer_type=layer_type, hemisphere = hemisphere)

  seasons <- get_seasons[names(get_seasons)%in%timeframe]

  if(!writefile){
    chaoslist <- list()
    length(chaoslist) <- length(seasons)
    names(chaoslist) <- names(seasons)
    for(c in 1:length(chaoslist)){
      chaoslist[[c]] <- list()
      length(chaoslist[[c]]) <- length(measures)
      names(chaoslist[[c]]) <- measures
    }
  }

  for(m in measures){
    for(s in 1:length(seasons)){
      if(names(seasons)[s]%in%c("annual", "wateryear")){
        tmp <- raster::stackApply(brick, seasons[[s]], eval(as.name(m)))
      }else{
        tmp <- raster::stackApply(brick, seasons[[s]], eval(as.name(m)))[[-1]]
      }
      tmp2 <- apply(raster::as.array((tmp)), 3, as.numeric)

      filename <- paste(directory, names(seasons)[s], "-", m, "-", varname, ".csv", sep="")
      if(writefile){write.csv(tmp2, filename)}else{
        chaoslist[[s]][[m]] <- tmp2
      }
    }
  }

  if(!writefile){return(chaoslist)}else{print("All files written!")}

}

#' Calculate different distance metrics
#' @description Calculates either the Mahalanobis distance or Standardized
#' Euclidean Distance between baseline (historical) and comparison (future) climates
#' @param baseline a matrix containing variables for the baseline period
#' @param comparison a matrix containing variables for the comparison period
#' @param dist.metric a character vector indicating if Mahalanobis ("mahalanobis") or SED ("sed") should be calculated
#' @return a matrix of distance measures for the comparison period
calc_distance <- function(baseline, comparison, dist.metric=c("mahalanobis", "sed")){
  if(dist.metric=="mahalanobis"){
    if(matrixcalc::is.singular.matrix(cov(baseline))%in%c(TRUE, NA)){
      d <- rep(NA, dim(comparison)[1])
    }else{
      d <- stats::mahalanobis(comparison, apply(baseline, 2, mean, na.rm=T), cov(baseline))

    }
  }
  if(dist.metric=="sed"){
    sed <- function(f, p){
      ((f - mean(p, na.rm=T))^2)/sd(p, na.rm=T)
    }
    d <- rowSums(sed(comparison, baseline))
  }
  return(d)
}


#' Calculate climate departures
#' @description Calculates univariate or multivariate climate departures from
#' baseline conditions
#' @param vars a list of matrices with equal dimensions containing climate or weather data; nrow is the number of cells or points and ncol is the number of years
#' @param baseline.years an integer vector of which years should be used for the historical period
#' @param comparison.years an integer vector of which years should be the future period relative to baseline
#' @param dist.method a character vector indicating if Mahalanobis ("mahalanobis") or
#' Standardized Euclidean ("sed") distance should be used
#' @param yearbounds a numeric vector of length 2 indicating the first and last year in the data
#' @details This function makes all sorts of assumptions about your data, like that
#' each column represents an individual year and that those years are sequential
#' with the earliest year in the first column and no gaps between years.
#'
#' If multiple variables are passed to the function (i.e. the list of vars has
#' length > 2), it calculates a multivariate departure; if only one variable is
#' passed to the function it returns a univariate departure.
#' @return a matrix of climate departures where each row is a cell or point and each column is a year in the comparison period
get_departures <- function(vars, baseline.years, comparison.years,
                           dist.method=c("mahalanobis", "sed"),
                           yearbounds=NULL){
  if(!is.null(yearbounds)){
    user.years <- seq(yearbounds[1], yearbounds[2], 1)
  }else{
    user.years <- sort(unique(c(baseline.years, comparison.years)))
    user.years <- seq(min(user.years), max(user.years), 1)
  }
  baselineperiod <- user.years %in% baseline.years
  comparisonperiod <- user.years %in% comparison.years

  merged_dat <- do.call(cbind, lapply(vars, function(x){x}))
  if(any(merged_dat=="Inf" & !is.na(merged_dat))){
    merged_dat[merged_dat=="Inf"] <- NA
  }
  if(any(merged_dat=="-Inf" & !is.na(merged_dat))){
    merged_dat[merged_dat=="-Inf"] <- NA
  }

  types <- rep(c(rep("base", length(baselineperiod)), rep("comp", length(comparisonperiod))), length(vars))

  d.mat <- apply(merged_dat, 1, function(x){
    calc_distance(matrix(matrix(as.numeric(x), ncol=length(vars))[baselineperiod,], ncol=length(vars)),
                  matrix(matrix(as.numeric(x), ncol=length(vars))[comparisonperiod,], ncol=length(vars)), dist.method)
  })
  rownames(d.mat) <- comparison.years

  return(t(d.mat))
}


#' Calculate trends in variables
#' @description Calculates trends over time in climate and weather variables or climate departures
#' @param dat a matrix where each row is a cell or site and each column is a year
#' @param trend.type a character vector indicating whether a Theil-Sen ("sen") or 'slope slope' ("ols") estimate should be returned
#' @return a numeric vector equal to the number of rows (cells or sites) in dat
get_trends <- function(dat, trend.type){
  calc_trends <- function(x, method=c("sen", "ols")){
    if(all(is.na(x))){
      NA
    }else{

      if(method=="ols"){
        huh <- as.numeric(lm(x ~ seq_along(x))$coefficients[2])
        return(huh)
      }

      if(method=="sen"){
        trend::sens.slope(x[!is.na(x), drop=F])$estimate
      }

    }
  }

  dist.trends <- apply(dat, 1, function(x){
    calc_trends(x, method=trend.type)
  })
  return(dist.trends)
}

#' A template for wget requests
#'
#' @format A character vector containing text as a scaffold for generate_wget()
"skeleton_wget"
