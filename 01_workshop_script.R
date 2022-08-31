# check dependencies and install if necessary
requiered_packages <- c("stringr", "lubridate", "tibble", "dplyr", "purrr",
                        "ggplot2", "ggforce", "viridis", "runner", "rmarkdown",
                        "knitr", "kableExtra", "tidyr","plotly")
missing_packages <- requiered_packages[!(requiered_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)

# directory to unzip source files
setwd("C:/path/to/directory") # !!!! EDIT HERE !!!!

# === Install package ===
install.packages("http://labgis.ibot.cas.cz/myclim/myClim_latest.tar.gz", repos=NULL, build_vignettes=TRUE)


## Set working directory to unzip downloaded or git cloned folder
path<-paste0(Sys.getenv("userprofile"),"\\downloads\\")
setwd(paste0(path,"2022_MEB_myClim_workshop-main"))

# setwd("C:/Users/#####/downloads/2022_MEB_myClim_workshop-main/") # edit


library(myClim)
## Read without metadata
# read from TOMST files
tms.f <- mc_read_files(c("data_91184101_0.csv","data_94184102_0.csv",
                         "data_94184103_0.csv"), dataformat_name="TOMST"
                       ,silent = T)

# read from HOBO files
hob.f <- mc_read_files(c("20024354_comma.csv"), 
                       dataformat_name="HOBO",
                       date_format = "%y.%m.%d %H:%M:%S",
                       silent = T)

# read all Tomst files from current directory
tms.d <- mc_read_files(".", dataformat_name="TOMST",recursive = F,silent = T)

# read from data.frame
meteo.table<-readRDS("airTmax_meteo.rds") # wide format data frame 
meteo <- mc_read_wide(meteo.table,sensor_id = "T_C", 
                      sensor_name = "airTmax",silent = T)

## Read with metadata
# provide two tables. Can be csv files or r data.frame
ft<-read.table("files_table.csv",sep=",",header = T)
lt<-read.table("localities_table.csv",sep=",",header = T)

tms.m <- mc_read_data(files_table = "files_table.csv",
                      localities_table =lt,
                      silent = T)


# clean runs automaticaly while reading  
tms <- mc_prep_clean(tms.m) # clean series
tms.info <- mc_info_clean(tms) # call cleaning log


tms <- mc_prep_solar_tz(tms) # calculate solar time

# provide user defined offset to UTC in minutes 
# for conversion to political time use offset in minutes. 
tms.usertz <- mc_prep_meta_locality(tms,values=as.list(c(A1E05=60,
                                                       A2E32=0,
                                                       A6W79=120)),
                                    param_name = "tz_offset")


# simulate calibration data (sensor shift/offset to add)
i<-mc_info(tms)
calib_table<-data.frame(serial_number=i$serial_number,
                        sensor_id=i$sensor_id,
                        datetime=as.POSIXct("2016-11-29",tz="UTC"),
                        cor_factor=0.398,
                        cor_slope=0)

# load calibration to myClim metadata 
tms.load<-mc_prep_calib_load(tms,calib_table)

## run calibration for selected sensors
tms<-mc_prep_calib(tms.load,sensors = c("TM_T",
                                        "TMS_T1",
                                        "TMS_T2",
                                        "TMS_T3"))

## mc_info_count(tms)
## mc_info_clean(tms)
## mc_info(tms)




## crop the time-series
start<-as.POSIXct("2021-01-01",tz="UTC")
end<-as.POSIXct("2021-03-31",tz="UTC")
tms<-mc_prep_crop(tms,start,end)


## simulate another myClim object and rename some localities and sensors
tms1<-tms
tms1<-mc_prep_meta_locality(tms1, list(A1E05="ABC05", A2E32="CDE32"), 
                            param_name="locality_id") # change locality ID

tms1<-mc_prep_meta_sensor(tms1, values=list(TMS_T1="TMS_Tsoil", TMS_T2="TMS_Tair2cm"),
                          localities = "A6W79", param_name="name") # change sensor names

## merge two myClim objects Prep-format
tms.m<-mc_prep_merge(list(tms,tms1))
tms.im<-mc_info(tms.m) # see info 

## Filtering 
tms.m<-mc_filter(tms.m,localities = "A6W79",reverse = T) # delete one locality.
tms.m<-mc_filter(tms.m,sensors = c("TMS_T2","TMS_T3"),reverse = F) # keep only two sensor
tms.if<-mc_info(tms.m) # see info 

## upload metadata from data frame

# load  data frame with metadata (coordinates)
metadata<-readRDS("metadata.rds")

# upload metadata from data.frame
tms.f<-mc_prep_meta_locality(tms.f, values=metadata)

## upload metadata from named list
tms.usertz<-mc_prep_meta_locality(tms,values=as.list(c(A1E05=57,
                                                       A2E32=62,
                                                       A6W79=55)),
                                  param_name = "tz_offset")





# one locality with two downloads in time 
data <- readRDS("join_example.rds") 

joined_data <- mc_join(data, comp_sensors=c("TMS_T1", "TMS_T2"))

#> Locality: 94184102
#> Problematic interval: 2020-12-01 UTC--2020-12-31 23:45:00 UTC
#> Older logger TMS 94184102
#>      tag               start                 end
#> 1 source 2020-10-06 09:15:00 2020-12-31 23:45:00
#>                                                                 value
#> 1 D:\\Git\\microclim\\examples\\data\\join\\1deg\\data_94184102_0.csv
#> Newer logger TMS 94184102
#>      tag      start                 end                                                               value
#> 1 source 2020-12-01 2021-04-07 11:45:00 D:\\Git\\microclim\\examples\\data\\join\\1deg\\data_94184102_1.csv
#> Loggers are different. They cannot be joined automatically.
#> 
#> 1: use older logger
#> 2: use newer logger
#> 3: use always older logger
#> 4: use always newer logger
#> 5: exit
#> 
#> Write choice number or start datetime of use newer logger in format YYYY-MM-DD hh:mm.
#> CHOICE>


rm(list=setdiff(ls(), c("tms","hob.f"))) # environment cleaning

## lines
# mc_plot_line(tms,filename = "lines.png",
#              sensors = c("TMS_T3","TMS_TMSmoisture"),png_width = 2500)

tms.plot <- mc_filter(tms,localities = "A6W79")

p <- mc_plot_line(tms.plot,filename = "lines.pdf",sensors = c("TMS_T3","TMS_T1","TMS_TMSmoisture"))
p <- p+ggplot2::scale_x_datetime(date_breaks = "1 week", date_labels = "%W")
p <- p+ggplot2::xlab("week")
p <- p+ggplot2::aes(size=sensor_name)
p <- p+ggplot2::scale_size_manual(values = c(1,1,2))
p <- p+ggplot2::guides(size = "none")
p <- p+ggplot2::scale_color_manual(values=c("hotpink","pink", "darkblue"),name=NULL)


## raster
# mc_plot_raster(tms,filename = "raster.png",
#                 sensors = c("TMS_T3","TM_T"),png_width = 2500,png_height = 500)
mc_plot_raster(tms,filename = "raster.pdf",sensors = c("TMS_T3","TM_T"))


# with defaults only convert Prep-format  to Calc-format
tms.ag <- mc_agg(tms,fun = NULL, period = NULL) 

# aggregate to daily mean, range, coverage, and 95 percentile. 
tms.day <- mc_agg(tms, fun=c("mean","range","coverage","percentile"),
                percentiles = 95, period = "day")

# it warns you that it cropped start and end of your data. 

# aggregate all time-series, return one value per sensor.
tms.all <- mc_agg(tms, fun=c("mean","range","coverage","percentile"),
                percentiles = 95, period = "all")

# aggregate with your custom function. (how many records above 30°C per month)
tms.all.custom <- mc_agg(tms, fun=list(TMS_T3="below5"),period = "month",
                         custom_functions = list(below5=function(x){length(x[x<(-5)])}))
mc_info(tms.all.custom)
r<-mc_reshape_long(tms.all.custom)


## calculate virtual sensor VWC from raw Tomst moisture
tms.calc <- mc_calc_vwc(tms.ag,soiltype = "loamy sand A")
# call mc_data_vwc_parameters() for soil selection (sand, loam, peat....)

## virtual sensor with growing and freezing degree days
tms.calc <- mc_calc_gdd(tms.calc,sensor = "TMS_T3",)
tms.calc <- mc_calc_fdd(tms.calc,sensor = "TMS_T3")
# mc_plot_line(tms.calc,"gdd.pdf",sensors = c("GDD5","TMS_T3"))

## virtual sensor to estimate snow presence from 2 cm air temperature 
tms.calc <- mc_calc_snow(tms.calc,sensor = "TMS_T2")
# mc_plot_line(tms.calc,"snow.pdf",sensors = c("snow","TMS_T2"))

## summary data.frame of snow estimation
tms.snow <- mc_calc_snow_agg(tms.calc)

##  virtual sensor with VPD
hobo.vpd <- mc_calc_vpd(hob.f)




# calculate standard myClim envi from your data 
temp_env <- mc_env_temp(tms,period="all")
moist_env <- mc_env_moist(tms.calc,period="all") 
vpd_env <- mc_env_vpd(hobo.vpd,period = "all")



## wide table of air temperature and soil moisture
tms.wide <- mc_reshape_wide(tms.calc,sensors = c("TMS_T3","vwc_moisture"))

## long table of air temperature and soil moisture
tms.long <- mc_reshape_long(tms.calc,sensors = c("TMS_T3","vwc_moisture"))

tms.long.all <- mc_reshape_long(tms.all)

