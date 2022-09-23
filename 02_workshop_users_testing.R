

# =================== working directory ======================================
## set working directory to unzip source files

setwd("C:/pathto/directory") # !!!! EDIT HERE !!!!
# path<-paste0(Sys.getenv("userprofile"),"\\downloads\\")
# setwd(paste0(path,"2022_MEB_myClim_workshop-main"))


# =================== Install packages ======================================
# check dependencies and install if necessary
requiered_packages <- c("stringr", "lubridate", "tibble", "dplyr", "purrr",
                        "ggplot2", "ggforce", "viridis", "runner", "rmarkdown",
                        "knitr", "kableExtra", "tidyr","plotly")
missing_packages <- requiered_packages[!(requiered_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)

install.packages("http://labgis.ibot.cas.cz/myclim/myClim_latest.tar.gz", repos=NULL, build_vignettes=TRUE)


################## myClim magic in few lines ##############################
library(myClim)
tms.f <- mc_read_files(c("V08_data_94204221_2022_07_02_0.csv"), dataformat_name="TOMST",silent = T)
tms.f <- mc_read_files(c("V01_data_94204222_2022_07_02_0.csv"), dataformat_name="TOMST",silent = T)

# read from HOBO files
hob.f <- mc_read_files(c("20024354_comma.csv"), 
                       dataformat_name="HOBO",
                       date_format = "%y.%m.%d %H:%M:%S",
                       silent = T)

# read any (micro)climatic data from data.frame
meteo.table<-readRDS("airTmax_meteo.rds") # wide format data frame 
meteo <- mc_read_wide(meteo.table,sensor_id = "T_C", 
                      sensor_name = "airTmax",silent = T)

## Reading logger data with metadata ==========================================
# provide two tables. Can be csv files or r data.frame
ft<-read.table("files_table.csv",sep=",",header = T)
lt<-read.table("localities_table.csv",sep=",",header = T)

tms.m <- mc_read_data(files_table = "files_table.csv",
                      localities_table =lt,
                      silent = T,clean = F)


## Time zones & calibration ========================================
tms <- mc_prep_clean(tms.m) # clean series
mc_info_clean(tms) # call cleaning log

tms <- mc_prep_solar_tz(tms) # calculate solar time
mc_info_meta(tms) # call metadata info

# provide user defined offset to UTC in minutes 
# for conversion to political time use offset in minutes. 
tms.usertz <- mc_prep_meta_locality(tms,values=as.list(c(A1E05=60,
                                                       A2E32=0,
                                                       A6W79=120)),
                                    param_name = "tz_offset")

## Calibration ------------------------------------
# simulate calibration data (sensor shift/offset to add)
i<-mc_info(tms)
calib_table<-data.frame(serial_number=i$serial_number,
                        sensor_id=i$sensor_id,
                        datetime=as.POSIXct("2016-11-29",tz="UTC"),
                        cor_factor=0.398,
                        cor_slope=0)

# load calibration to myClim metadata (will not change the records)
tms.load<-mc_prep_calib_load(tms,calib_table)

## run calibration for selected sensors (will change the records)
tms<-mc_prep_calib(tms.load,sensors = c("TS_T",
                                        "TMS_T1",
                                        "TMS_T2",
                                        "TMS_T3"))

mc_info_count(tms)
mc_info_clean(tms)
mc_info(tms)


## crop the time-series ------------------
start<-as.POSIXct("2021-01-01",tz="UTC")
end<-as.POSIXct("2021-03-31",tz="UTC")
tms<-mc_prep_crop(tms,start,end)


## Filtering -----------------------------
tms.m<-mc_filter(tms.m,localities = "A6W79",reverse = T) # delete one locality.
tms.m<-mc_filter(tms.m,sensors = c("TMS_T2","TMS_T3"),reverse = F) # keep only two sensor
mc_info(tms.m)

## update metadata -------------------------
metadata<-readRDS("metadata.rds") # load  data frame with metadata (coordinates)
tms.f<-mc_prep_meta_locality(tms.f, values=metadata) # update metadata from data.frame


## JOINING data in time ==============================================
data <- readRDS("join_example.rds") # one locality with two downloads in time
joined_data <- mc_join(data, comp_sensors=c("TMS_T1", "TMS_T2")) # interactive join


## Plotting ==========================================================
rm(list=setdiff(ls(), c("tms","hob.f")))

## lines------------------------------------------------------------------
tms.plot <- mc_filter(tms,localities = "A6W79") # prepare data for plotting (select one locality)
p <- mc_plot_line(tms.plot,filename = "lines.pdf",sensors = c("TMS_T3","TMS_T1","TMS_TMSmoisture"))
p

# you can play with ggplot object yourself
p <- p+ggplot2::scale_x_datetime(date_breaks = "1 week", date_labels = "%W")
p <- p+ggplot2::xlab("week")
p <- p+ggplot2::aes(size=sensor_name)
p <- p+ggplot2::scale_size_manual(values = c(1,1,2))
p <- p+ggplot2::guides(size = "none")
p <- p+ggplot2::scale_color_manual(values=c("hotpink","pink", "darkblue"),name=NULL)


## raster --------------------------------------------------------------
mc_plot_raster(tms,filename = "raster.pdf",sensors = c("TMS_T3","TM_T"))


## aggregation in time ==================================================
# aggregate to daily mean, range, coverage, and 95 percentile. 
tms.day <- mc_agg(tms, fun=c("mean","range","coverage","percentile"),
                percentiles = 95, period = "day")

# aggregate all time-series, return one value per sensor.
tms.all <- mc_agg(tms, fun=c("mean","range","coverage","percentile"),
                percentiles = 95, period = "all")

# aggregate with your custom function. (how many records above 30°C per month)
tms.all.custom <- mc_agg(tms, fun=list(TMS_T3="below5"),period = "month",
                         custom_functions = list(below5=function(x){length(x[x<(-5)])}))
mc_info(tms.all.custom)
r<-mc_reshape_long(tms.all.custom)


## calculate microclimatic variables (virtual sensors) ===============================

# Volumetric Water Content from raw TOMST raw moisture ------------------------------
tms.calc <- mc_calc_vwc(tms,soiltype = "loamy sand A")
# mc_data_vwc_parameters() # see for soil selection (sand, loam, peat....)

## virtual sensor with growing and freezing degree days -----------------------------
tms.calc <- mc_calc_gdd(tms.calc,sensor = "TMS_T3")
tms.calc <- mc_calc_fdd(tms.calc,sensor = "TMS_T3")
# mc_plot_line(tms.calc,"gdd.pdf",sensors = c("GDD5","TMS_T3"))

## virtual sensor to estimate snow presence from 2 cm air temperature ---------------- 
tms.calc <- mc_calc_snow(tms.calc,sensor = "TMS_T2")
# mc_plot_line(tms.calc,"snow.pdf",sensors = c("snow","TMS_T2"))

## summary data.frame of snow estimation
tms.snow <- mc_calc_snow_agg(tms.calc)

##  virtual sensor with VPD ---------------------------------------------------------
hobo.vpd <- mc_calc_vpd(hob.f)


## AUTOPILOT: calculate standard myClim envi ======================================= 
temp_env <- mc_env_temp(tms,period="all")
moist_env <- mc_env_moist(tms.calc,period="all") 
vpd_env <- mc_env_vpd(hobo.vpd,period = "all")


## Reshapeing =====================================================================
## wide table of air temperature and soil moisture
tms.wide <- mc_reshape_wide(tms.calc,sensors = c("TMS_T3","vwc_moisture"))

## long table of air temperature and soil moisture
tms.long <- mc_reshape_long(tms.calc,sensors = c("TMS_T3","vwc_moisture"))
