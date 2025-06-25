#Met data

#meteorological data from FCR https://portal.edirepository.org/nis/mapbrowse?packageid=edi.389.8
#2015-2023
options(timeout = 300)
metdata <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/389/8/d4c74bbb3b86ea293e5c52136347fbb0")

# Visualizing metdata####

metdata0 <- metdata|>
 mutate(Date = as_date(DateTime))|>
 mutate(DOY = yday(Date))|>
   relocate(DOY, .before = DateTime)|>
   relocate(Date, .before = DateTime)
 
##### Function for plotting meteorological variables #### 
 metplots <- function(yearz, variable, maxx = NULL){
   
   metviz <- metdata0|>
     filter(year(DateTime) == yearz) #filtering for the year specified
   
   
   ggplot(metviz, aes(x = DateTime, y = {{variable}}))+
     geom_path()+
     ggtitle(paste(deparse(substitute(variable)), yearz))+
     theme_minimal()+
     scale_y_continuous(limits = c(0, maxx))  # setting consistent y-axis limits
   
 }
 
##### Precipitation ####
#not including all of this for now
metdataprecip <- metdata0 |> 
  group_by(week(Date), year(DateTime))|> 
  mutate(precip_weekly = sum(Rain_Total_mm, na.rm = TRUE))|>
  ungroup()|>
  relocate(Date, .before = DateTime)|>
  relocate(precip_weekly, .before = Rain_Total_mm)
 
 b1 <- metplots(2015, precip_daily, maxx = 80)
 b2 <- metplots(2016, precip_daily, maxx = 80)
 b3 <- metplots(2017, precip_daily, maxx = 80)
 b4 <- metplots(2018, precip_daily, maxx = 80)
 b5 <- metplots(2019, precip_daily, maxx = 80)
 b6 <- metplots(2020, precip_daily, maxx = 80)
 b7 <- metplots(2021, precip_daily, maxx = 80)
 b8 <- metplots(2022, precip_daily, maxx = 80)
 b9 <- metplots(2023, precip_daily, maxx = 80)
 
precips<- plot_grid(
   b1, b2, b3,
   b4, b5, b6, 
   b7, b8, b9,
  ncol = 3
)
 print(b1)
 
 print(precips)
# 
# ##### Air temps #### 
# 
# #b1 <- metplots(2015, AirTemp_C_Average, maxx = 50)
# #b2 <- metplots(2016, AirTemp_C_Average, maxx = 50)
# #b3 <- metplots(2017, AirTemp_C_Average, maxx = 50)
# #b4 <- metplots(2018, AirTemp_C_Average, maxx = 50)
# #b5 <- metplots(2019, AirTemp_C_Average, maxx = 50)
# #b6 <- metplots(2020, AirTemp_C_Average, maxx = 50)
# #b7 <- metplots(2021, AirTemp_C_Average, maxx = 50)
# #b8 <- metplots(2022, AirTemp_C_Average, maxx = 50)
# #b9 <- metplots(2023, AirTemp_C_Average, maxx = 50)
# 
# 
# #temps<- plot_grid(
# #  b1, b2, b3,
# #  b4, b5, b6, 
# #  b7, b8, b9,
# #  ncol = 3
# #)
# 
# #print(temps)
# 
# ##### dailyaverage and dailymax for temps #### 
# metdatatemps <- metdataprecip |> 
#   group_by(Date, year(DateTime))|> 
#   mutate(daily_airtempavg = mean(AirTemp_C_Average, na.rm = TRUE))|>
#   mutate(maxdaily_airtemp = max(AirTemp_C_Average, na.rm = TRUE))|>
#   mutate(mindaily_airtemp = min(AirTemp_C_Average, na.rm = TRUE))|>
#   ungroup()|>
#   relocate(daily_airtempavg, .before = AirTemp_C_Average)|>
#   relocate(maxdaily_airtemp, .before = AirTemp_C_Average)|>
#   relocate(mindaily_airtemp, .before = AirTemp_C_Average)|>
#   select(Date,daily_airtempavg, maxdaily_airtemp, mindaily_airtemp, precip_daily)
# 
# 
# ##### precip and temp to final_datanpratio #### 
# 
# metdata_join <- metdatatemps |> 
#   group_by(Date) |> 
#   summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) |> 
#   distinct()
# 
# final_datamet <- final_datanpratio|>
#   left_join(metdata_join, by = c("Date"), relationship = "many-to-many")
# 
#  