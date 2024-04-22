
#' ---
#' title: audio dataset description
#' author: lucas gaspar
#' date: 2020-08-d09
#' ---
#' 
#'   
#'   # clean
rm(list = ls())

# packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(vegan)) install.packages("vegan")
if(!require(lumaidate)) install.packages("lumaidate")  
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")
if(!require(writexl)) install.packages("writexl")
if(!require(matrix)) install.packages("matrix")
if(!require(zoo)) install.packages("zoo")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggpuma)) install.packages("ggpuma")
if(!require(vegan)) install.packages("vegan")

##########################################################
# description complete dataset

setwd("/home/gasparzinho/Documents/backpup/O2/description")
dir()
## import data ----------------------------------------------------------
mestra_mor <- readr::read_csv2("00_morning_names_subdataset.csv")
mestra_mor

mestra_nig <- readr::read_csv2("00_night_names_subdataset.csv")
mestra_nig

forest <- readr::read_csv2("00_metadata_forest_coord_ccm2.csv")
forest

forest_land <- forest %>% 
  dplyr::select(landscape, Flo2km) %>% 
  dplyr::group_by(landscape) %>% 
  dplyr::distinct(landscape, .keep_all = TRUE ) 

forest_env <- forest %>% 
  dplyr::select(env, Flo2km) %>% 
  dplyr::group_by(env) %>% 
  dplyr::distinct(env, .keep_all = TRUE ) 

forest_cod <- forest %>% 
  dplyr::select(codigo, Flo2km) %>% 
  dplyr::group_by(codigo) %>% 
  dplyr::distinct(codigo, .keep_all = TRUE )

# morning
name <- substr(mestra_mor$x, 1,29)

mestra_mor$x <- name

mestra_mor <- mestra_mor %>% 
  dplyr::rename(filename = x) %>% 
  dplyr::mutate(env = substr(filename, 28,29), landscape = substr(filename, 1,6), date = substr(filename, 12,19), time = substr(filename, 21,26) ) 
mestra_mor

mestra_mor_land <-  mestra_mor %>% 
  dplyr::mutate(se = rep(1, each = nrow( mestra_mor))) %>% 
  dplyr::select(landscape, se) %>% 
  dplyr::group_by(landscape) %>% 
  dplyr::summarise_each(sum) %>% 
  dplyr::right_join(forest_land, by = "landscape") 
mestra_mor_land
mestra_mor_land$Flo2km <- as.numeric(mestra_mor_land$Flo2km)



plot.dir <- "./plots"

ggplot(data=mestra_mor_land , aes(x=reorder(landscape, -Flo2km), y=se)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=se), vjust=2.2, color="white", size=2.5)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  xlab("landscape") + ylab("nº of minutes")  
ggsave("barplot_minutes_by_landscape.png", path = plot.dir)


# environment
mestra_mor_env <-  mestra_mor %>% 
  dplyr::mutate(se = rep(1, each = nrow( mestra_mor))) %>% 
  dplyr::select(env, se) %>% 
  dplyr::group_by(env) %>% 
  dplyr::summarise_each(sum) %>% 
  dplyr::right_join(forest_env, by = "env") 
mestra_mor_env
mestra_mor_env$Flo2km <- as.numeric(mestra_mor_env$Flo2km)


ggplot(data=mestra_mor_env , aes(x=reorder(env, -Flo2km), y=se)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=se), vjust=2.2, color="white", size=3.5)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  xlab("environment") + ylab("nº of minutes")  
ggsave("barplot_minutes_by_env.png", path = plot.dir)

# sampling point
mestra_mor_cod <-  mestra_mor %>% 
  dplyr::mutate(codigo = paste(landscape, env, sep = "_")) %>% 
  dplyr::mutate(se = rep(1, each = nrow( mestra_mor))) %>% 
  dplyr::select(codigo, se) %>% 
  dplyr::group_by(codigo) %>% 
  dplyr::summarise_each(sum) %>% 
  dplyr::right_join(forest_cod, by = "codigo") 
mestra_mor_cod
mestra_mor_cod$Flo2km <- as.numeric(mestra_mor_cod$Flo2km)


ggplot(data=mestra_mor_cod , aes(x=reorder(codigo, -Flo2km), y=se)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=se), vjust=2.2, color="black", size=2.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  xlab("sampling point") + ylab("nº of minutes")  
ggsave("barplot_minutes_by_sampling_point.png", path = plot.dir)


# time
mestra_mor_time<-  mestra_mor %>% 
  #dplyr::mutate(codigo = paste(landscape, env, sep = "_")) %>% 
  dplyr::mutate(se = rep(1, each = nrow( mestra_mor))) %>% 
  dplyr::select(time, se) %>% 
  dplyr::group_by(time) %>% 
  dplyr::summarise_each(sum)  
#dplyr::right_join(forest_cod, by = "codigo") 
mestra_mor_time
#mestra_mor_cod$Flo2km <- as.numeric(mestra_mor_cod$Flo2km)


ggplot(data= mestra_mor_time , aes(x=time, y=se)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=se), vjust=2.2, color="white", size=3.0)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  xlab("time") + ylab("nº of minutes")  
ggsave("barplot_minutes_by_time.png", path = plot.dir)

# date
mestra_mor_date<-  mestra_mor %>% 
  #dplyr::mutate(codigo = paste(landscape, env, sep = "_")) %>% 
  dplyr::mutate(se = rep(1, each = nrow( mestra_mor))) %>% 
  dplyr::select(date, se) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise_each(sum)  
#dplyr::right_join(forest_cod, by = "codigo") 
mestra_mor_date
#mestra_mor_cod$Flo2km <- as.numeric(mestra_mor_cod$Flo2km)


ggplot(data= mestra_mor_date , aes(x= date, y=se)) +
  geom_bar(stat="identity", fill="steelblue")+
  #geom_text(aes(label=se), vjust=2.2, color="black", size=3.0)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  xlab("date") + ylab("nº of minutes")  
ggsave("barplot_minutes_by_date.png", path = plot.dir)


# date and landscape
mestra_mor_land_date<-  mestra_mor %>% 
  dplyr::mutate(land_date = paste(landscape, date, sep = "_")) %>% 
  dplyr::mutate(se = rep(1, each = nrow( mestra_mor))) %>% 
  dplyr::select(land_date, se) %>% 
  dplyr::group_by(land_date) %>% 
  dplyr::summarise_each(sum)  
#dplyr::right_join(forest_cod, by = "codigo") 
mestra_mor_land_date
#mestra_mor_cod$Flo2km <- as.numeric(mestra_mor_cod$Flo2km

ggplot(data=  mestra_mor_land_date , aes(x= land_date, y=se)) +
  geom_bar(stat="identity", fill="steelblue")+
  #geom_text(aes(label=se), vjust=2.2, color="black", size=3.0)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  xlab("land date") + ylab("nº of minutes")  
ggsave("barplot_minutes_by_land_date.png", path = plot.dir)

# time and landscape
mestra_mor_land_time<-  mestra_mor %>% 
  dplyr::mutate(land_time = paste(landscape, time, sep = "_")) %>% 
  dplyr::mutate(se = rep(1, each = nrow( mestra_mor))) %>% 
  dplyr::select(land_time, se) %>% 
  dplyr::group_by(land_time) %>% 
  dplyr::summarise_each(sum)  
#dplyr::right_join(forest_cod, by = "codigo") 
mestra_mor_land_time
#mestra_mor_cod$Flo2km <- as.numeric(mestra_mor_cod$Flo2km

ggplot(data=   mestra_mor_land_time , aes(x= land_time, y=se)) +
  geom_bar(stat="identity", fill="steelblue")+
  #geom_text(aes(label=se), vjust=2.2, color="black", size=3.0)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  xlab("land time") + ylab("nº of minutes")  
ggsave("barplot_minutes_by_land_time.png", path = plot.dir)
