#Single age by MSOA mapping
#Inspired by this tweet: https://twitter.com/undertheraedar/status/1609862994166484993
#Uses same colour scheme

#This line shows you the existing working directory
getwd()

#The ls() code lists all of the objects in your workspace. 
#The rm() code removes objects in your workspace. 
#You can begin your code with the rm() function to clear all of the objects from your workspace to start with a clean environment. 
#This way the workspace is empty and everything you create is clearly visible.
rm(list = ls())

#######################################################################################################################################
### LIBRARIES ###
#######################################################################################################################################
#These lines import the libraries to be used in the analysis
#Note: There are many additional libraries which are not used in this script, they have been left for convenience
#Note: All will need to have been installed prior running this file
library(readstata13) 
library(tidyverse) 
library(questionr) 
library(patchwork) 
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ggmap)
library(ggplot2)
library(rgeos)
library(broom)
library(dplyr)
library(tmap)
library(terra)
library(sf)
library(pct)
library(plotly)
library(readxl)
library(stringr)   


#######################################################################################################################################
### CONVERTING SHP FILES INTO R DATA ###
#######################################################################################################################################
###############################################################################################
# MSOA
###############################################################################################
#Reading in the shp file using sf package
#https://datacarpentry.org/r-raster-vector-geospatial/06-vector-open-shapefile-in-r/
#File obtained here: https://geoportal.statistics.gov.uk/datasets/ons::middle-layer-super-output-areas-december-2021-boundaries-super-generalised-clipped-ew-bsc/explore?location=52.808964%2C-2.465415%2C7.08
boundary_MSOA_2021 <- st_read("YOUR WORKING DIRECTORY/MSOA_2021_EW_BSC.shp")

#Viewing a blank map to test this has worked
MSOA_map = tm_shape(boundary_MSOA_2021) +
  tm_borders() 
MSOA_map

#Sometimes it throws an error: The shape boundary_MSOA_2021 is invalid (after reprojection). See sf::st_is_valid 
#The line below rectifies this
boundary_MSOA_2021= st_make_valid(boundary_MSOA_2021)

#Viewing a blank map to test this has worked with no errors
MSOA_map = tm_shape(boundary_MSOA_2021)+
  tm_borders()
MSOA_map


###############################################################################################
#Parliamentary boundaries#
###############################################################################################
#Reading in the shp file using sf package
#https://datacarpentry.org/r-raster-vector-geospatial/06-vector-open-shapefile-in-r/
#File obtained here: https://geoportal.statistics.gov.uk/datasets/ons::westminster-parliamentary-constituencies-dec-2021-uk-buc/explore?location=55.215744%2C-3.315966%2C6.92

boundary_parl_2021 <- st_read("YOUR WORKING DIRECTORY/Westminster_Parliamentary_Constituencies_(Dec_2021)_UK_BUC.shp")

#Filtering only England and Wales constituency boundaries
boundary_parl_2021 <- filter(boundary_parl_2021, OBJECTID <= 533 |OBJECTID >= 611)

#Viewing a blank map to test this has worked
parl_map = tm_shape(boundary_parl_2021) +
  tm_borders() 

parl_map

#Sometimes it throws an error: The shape boundary_parl_2021 is invalid (after reprojection). See sf::st_is_valid 
#The line below rectifies this
boundary_parl_2021 = st_make_valid(boundary_parl_2021)

parl_map = tm_shape(boundary_parl_2021)+
tm_borders()
parl_map

#######################################################################################################################################
### WORKING DIRECTORY ###
#######################################################################################################################################
#Setting the working directory
setwd("YOUR WORKING DIRECTORY/") 

#The data used in this map is downloaded from NOMIS
#https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2027
#Geography: 2021 super output areas - mid layer = ALL
#Age: All individual ages
#Percent: Value only

#A small amount of manipulation to the downloaded file has been done
#Renamed file as Age by MSOA Census 2021 (All ages)
#Renamed sheet as Sheet1
#Saved as xlsx not csv

#Create a folder called Data inside your working directory
#Importing the sheet Sheet1 from the excel file
#Removing the first rows
df <- read_excel("Data/Age by MSOA Census 2021 (All ages).xlsx", 
                 sheet = "Sheet1", col_names = TRUE, 
                 skip = 7 #skip some nonsense rows
)

#Deleted the unnecessary end few and 1st row 
df <- df[-(7266:7272),]
df <- df[-1,]

#Renaming variables in data frame due to length
df <- df %>% 
  rename(
    MSOA21CD = '2021 super output area - middle layer')

#Extracting the Geographic code only so this can be merged to the geography df
df$MSOA21CD <- df$MSOA21CD %>% 
  substr(start = 1, stop = 9)

#Using pivot longer to reshape data in order to calculate median
df2 <- pivot_longer(df, cols = c(2:102), names_to = "Age", values_to = "count")

#Ensuring Age and MSOA21CD are the correct form
df2$Age <- as.numeric(df2$Age)
df2$MSOA21CD <- lapply(df2$MSOA21CD, as.character)

#This code makes a dataframe called med with the geographic code and the median age!
med <- df2 %>%
  group_by(MSOA21CD) %>%
  summarise(median_age = median(rep(Age,count)))

#Merging the boundary data and the data to be plotted, on variable name:MSOA21CD
boundary_MSOA_2021 <- merge(boundary_MSOA_2021,med,by="MSOA21CD")

#The code below didn't work for ages due to the following error:
#Error: Fill argument neither colors nor valid variable name(s)
#This was rectified by ensuring the merge above is stored as the orignal df name


#########################################################################################
#### Plot of median age in UK by MSOA ###################################################
#########################################################################################
#Ensuring mode is set to plot
tmap_mode("plot")

#Simple plot of median age using default colours
tm_shape(boundary_MSOA_2021) + 
tm_polygons(col = "median_age") 


#Plot with custom breaks with 8 age bands

MSOA_8band <- tm_shape(boundary_MSOA_2021)+
  tm_polygons(col="median_age", 
             title="Group",
             breaks = c(0,9, 25, 41, 57, 67,76,94,100),
             n=8,
             palette = 'YlOrRd',
             size='Modal age',
             labels = c('Babies','Gen Z','Millennials','Gen X','Boomers II','Boomers I','Silent Generation','Greatest Generation')) +
  tm_legend(position = c("right", "top")) +
  tm_layout(title= 'Modal age',  title.position = c('left', 'top')) +
  tm_credits("Source: 2021 Census",
             position = c("LEFT", "BOTTOM"))
MSOA_8band 


#Plot with custom breaks with 5 age bands
#This is also a borderless map
MSOA_5band <- tm_shape(boundary_MSOA_2021)+
  tm_polygons(col="median_age", 
              title="",
              breaks = c(0,25, 41, 57,76,100),
              n=5,
              palette = c("#72468C","#D03240","#00B5BC","#0B559E","#FFBF00"),
              size='Modal age',
              labels = c('Gen Z or younger: Born 1997–2021','Millennials: Born 1981–1996','Gen X: Born 1965–1980','Boomers: Born 1946–1964','Silent Generation or older: Born 1945 or earlier'))+
  tm_legend(position = c("right", "top")) +
  tm_layout(title= 'Median age',  title.position = c('left', 'top')) +
  tm_credits("Source: 2021 Census",
             position = c("RIGHT", "BOTTOM")) +
  tm_layout(frame = FALSE)
MSOA_5band 

#########################################################################################
#### Plot including London ONLY #########################################################
#########################################################################################
#To create London map you only need to filter data (not produce a base map of London)
#Creating a new dataframe with this filter
boundary_MSOA_2021_ldn<- boundary_MSOA_2021 %>% filter(OBJECTID >= 1 & OBJECTID <= 932)

MSOA_5band_ldn  <- tm_shape(boundary_MSOA_2021_ldn)+
  tm_polygons(col="median_age", 
              title="",
              breaks = c(0,25, 41, 57,76,100),
              n=5,
              palette = c("#72468C","#D03240","#00B5BC","#0B559E","#FFBF00"),
              size='Modal age',
              labels = c('Gen Z or younger: Born 1997–2021','Millennials: Born 1981–1996','Gen X: Born 1965–1980','Boomers: Born 1946–1964','Silent Generation or older: Born 1945 or earlier'))+
  tm_legend(position = c("right", "top")) +
  tm_layout(title= 'Median age',  title.position = c('left', 'top')) +
  tm_credits("Source: 2021 Census",
             position = c("LEFT", "BOTTOM"))

MSOA_5band_ldn

#########################################################################################
#### MAKING INTERACTIVE MAPS ############################################################
#########################################################################################
#How to produce interactive maps: https://bookdown.org/nicohahn/making_maps_with_r5/docs/tmap.html

#Change to interactive map mode
tmap_mode("view")

#########################################################################################
#### Interactive map by MSOA ############################################################
#########################################################################################

#Code is slightly adapted from plots
#Can choose what is presented using popup.vars
#Can choose what format the data is presented using popup.format
#Can introduce basemap when saving, there is however a click option if this is excluded

#Interactive map of median age by MSOA
int_21_medage_MSOA <- tm_shape(boundary_MSOA_2021)+
  tm_polygons(col="median_age", 
              alpha=0.7,
              title="",
              breaks = c(0,25, 41, 57,76,100),
              n=5,
              palette = c("#72468C","#D03240","#00B5BC","#0B559E","#FFBF00"),
              size='median_age',
              border.alpha = 0.5,
              popup.vars=c("Median age"="median_age"),#https://stackoverflow.com/questions/41940403/popup-on-a-shape-using-tmap
              popup.format=list(median_age=list(digits=0)), #https://stackoverflow.com/questions/41940403/popup-on-a-shape-using-tmap
              labels = c('Gen Z or younger: Born 1997–2021','Millennials: Born 1981–1996','Gen X: Born 1965–1980','Boomers: Born 1946–1964','Silent Generation or older: Born 1945 or earlier'))+
  tm_scale_bar(position = c("left", "top"), width = 0.15)+
  tm_basemap(server="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") #Displays street view- from documentation of basemap

int_21_medage_MSOA

#Have saved to rpubs using Publish in top right hand corner of viewer

#########################################################################################
#### Interactive map with parliamentary constituency overlay ############################
#########################################################################################
#You are able to overlay other geographic boundaries using another tm_shape 
#By adding "tm_shape(boundary_parl_2021) + tm_borders(,lwd=3)"

int_21_medage_MSOA_parl <- tm_shape(boundary_MSOA_2021) +
   tm_polygons(col="median_age", 
              alpha=0.7,
              title="",
              breaks = c(0,25, 41, 57,76,100),
              n=5,
              palette = c("#72468C","#D03240","#00B5BC","#0B559E","#FFBF00"),
              size='median_age',
              border.alpha = 0.5,
              popup.vars=c("Median age"="median_age"),#https://stackoverflow.com/questions/41940403/popup-on-a-shape-using-tmap
              popup.format=list(median_age=list(digits=0)), #https://stackoverflow.com/questions/41940403/popup-on-a-shape-using-tmap
              labels = c('Gen Z or younger: Born 1997–2021','Millennials: Born 1981–1996','Gen X: Born 1965–1980','Boomers: Born 1946–1964','Silent Generation or older: Born 1945 or earlier'))+
              tm_scale_bar(position = c("left", "top"), width = 0.15) +
  tm_shape(boundary_parl_2021) +
  tm_borders(, lwd=3)

int_21_medage_MSOA_parl

#Have saved to rpubs using Publish in top right hand corner of viewer
