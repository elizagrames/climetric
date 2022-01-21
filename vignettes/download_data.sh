#!/bin/sh
#==========================================
#GOAL: download rectangular subset of data from TerraClimate
#Author: Katherine Hegewisch (khegewisch@uidaho.edu)
#==========================================
#        PARAMETERS
#==========================================
#latitude bounding box -90.0 to 90.0 (North is positive, South is negative)
LAT_MIN="35.00207901"
LAT_MAX="42.00030899"

#longitude bounding box: -180.0 to 180.0 (East is positive, West is negative)
LON_MIN="-120.00534821"
LON_MAX="-114.03933716"

#variable choices: tmax,tmin,ws,vpd,vap,swe,srad,soil,q,ppt,pet,def,aet,PDSI
VARIABLES=('def' 'srad' 'tmax' 'ppt' 'tmin' 'srad' 'PDSI' 'vpd' 'swe' 'pet' 'aet' 'q')

#time range choices
TIME_START="1958-01-01T00%3A00%3A00Z"
TIME_END="2020-12-01T00%3A00%3A00Z"
#==========================================
#       DOWNLOAD REQUESTS
#==========================================
ncssPath="http://thredds.northwestknowledge.net:8080/thredds/ncss"

for variable in "${VARIABLES[@]}"
do
filename="agg_terraclimate_${variable}_1958_CurrentYear_GLOBE.nc"
queryString="$ncssPath/${filename}?"
queryString="$queryString&var=${variable}"
queryString="$queryString&south=${LAT_MIN}&north=${LAT_MAX}&west=${LON_MIN}&east=${LON_MAX}&horizStride=1"
queryString="$queryString&time_start=${TIME_START}&time_end=${TIME_END}&timeStride=1"
queryString="$queryString&disableProjSubset=on&addLatLon=true&accept=netcdf"

newfilename="terraclimate_${variable}.nc"
wget -nc -c -nd "$queryString" -O "$newfilename"
done
