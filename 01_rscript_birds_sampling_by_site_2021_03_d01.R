
#' ---
#' title: sample minutes and anayiiys by site (landscape + environment)
#' author: lucas gaspar
#' date: 2020-07-d31
#' ---

# clean
rm(list = ls())

# packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(vegan)) install.packages("vegan")
if(!require(lubridate)) install.packages("lubridate")  
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")
if(!require(writexl)) install.packages("writexl")
if(!require(Matrix)) install.packages("Matrix")
if(!require(zoo)) install.packages("zoo")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(broom)) install.packages("broom")
if(!require(tidymodels)) install.packages("tidymodels")
if(!require(GGally)) install.packages("GGally")
if(!require(ggpmisc)) install.packages("ggpmisc")

# directory
setwd("/home/gasparzinho/Documents/backpup/ANALISYS_2020_06_d01/00_scripts_gaspar_master/after_meet/aves/00_after_quali/by_env")
dir()

#import objects
#load("only_sampling_by_site.rda")

## import data ----------------------------------------------------------
mestra <- readr::read_csv2("00_matriz_wide_SPECIES_biodiversity_informations.csv")
colnames(mestra)

# creat a data.frame for landscape and sampling each ,20 100 times.

SAMPLES_NUMBER <- 100
SAMPLES_SIZE   <- 20
SAMPLES_COUNTER<- 0
mestra_sampling <- NULL

for (i in 1:SAMPLES_NUMBER)
{
  for (site in unique(mestra$code))
  {
    SAMPLES_COUNTER <- SAMPLES_COUNTER + 1
    aux <- subset(mestra, code == site)
    aux <- aux [sample(1:nrow(aux)),  ]
    aux <- aux [1:SAMPLES_SIZE, ]
    aux$cod_sample <- SAMPLES_COUNTER
    
    mestra_sampling <- rbind(mestra_sampling, aux)
  }
}

dim(mestra_sampling)
colnames(mestra_sampling)
save.image("sampling_by_site.rda")

# remove possible NA
colnames(mestra_sampling)
mestra_sampling[is.na(mestra_sampling)] <- 0

# acoustic indices mean
mestra_sampling_acoustic_mean <- mestra_sampling %>% 
  dplyr::select(cod_sample,
                ADI_300_12000_50db:AR) %>% 
  dplyr::group_by(cod_sample) %>% 
  dplyr::summarize(ADI_300_12000_50db = mean(ADI_300_12000_50db), 
                   ADI_300_12000_75db = mean(ADI_300_12000_75db),
                   ADI_300_22050_50db = mean(ADI_300_22050_50db),
                   ADI_300_22050_75db = mean(ADI_300_22050_75db),                              
                   ADI_1000_12000_50db = mean(ADI_1000_12000_50db), 
                   ADI_1000_12000_75db = mean(ADI_1000_12000_75db), 
                   ADI_1000_22050_50db = mean(ADI_1000_22050_50db),
                   ADI_1000_22050_75db = mean(ADI_1000_22050_75db),                          
                   AEI_300_12000_50db = mean(AEI_300_12000_50db),
                   AEI_300_12000_75db = mean(AEI_300_12000_75db),
                   AEI_300_22050_50db = mean(AEI_300_22050_50db),
                   AEI_300_22050_75db = mean(AEI_300_22050_75db),                              
                   AEI_1000_12000_50db = mean(AEI_1000_12000_50db),
                   AEI_1000_12000_75db = mean(AEI_1000_12000_75db),
                   AEI_1000_22050_50db = mean(AEI_1000_22050_50db), 
                   AEI_1000_22050_75db = mean(AEI_1000_22050_75db),                             
                   ACI_300_12000_FFT512 = mean(ACI_300_12000_FFT512), 
                   ACI_300_22050_FFT512 = mean(ACI_300_22050_FFT512),
                   ACI_1000_12000_FFT512 = mean(ACI_1000_12000_FFT512),
                   ACI_1000_22050_FFT512 = mean(ACI_1000_22050_FFT512),                           
                   BIO_300_12000_FFT512 = mean(BIO_300_12000_FFT512), 
                   BIO_300_22050_FFT512 = mean(BIO_300_22050_FFT512),
                   BIO_1000_12000_FFT512 = mean(BIO_1000_12000_FFT512),
                   BIO_1000_22050_FFT512 = mean(BIO_1000_22050_FFT512),                           
                   NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 = mean(NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512),
                   NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 = mean(NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512), 
                   NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512 = mean(NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512),
                   NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 = mean(NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512),
                   H_ws512 = mean(H_ws512),
                   Ht = mean(Ht), 
                   AR = mean(AR))

# prepar table for biodiversity analisys
mestra_sampling_bio <- mestra_sampling %>% 
  dplyr::select(cod_sample, agel_cyan:zono_cape)%>% 
  dplyr::group_by(cod_sample) %>%
  dplyr::summarise_each(sum) 
mestra_sampling_bio

mestra_sampling_bio_sp <- mestra_sampling_bio %>%
  dplyr::select(agel_cyan:zono_cape) 
mestra_sampling_bio_sp 

# 2.1 -  richness 
mestra_sampling_bio_sp_rich <- mestra_sampling_bio_sp   %>% 
  dplyr::mutate(rich_sample = specnumber(mestra_sampling_bio_sp)) 
mestra_sampling_bio_sp_rich 

# 2.3  abundance
mestra_sampling_bio_sp_abund <- mestra_sampling_bio_sp   %>% 
  dplyr::mutate(abund_sample =  rowSums(mestra_sampling_bio_sp))
mestra_sampling_bio_sp_abund

# 2.4 Simpson's diveristy 
mestra_sampling_bio_sp_invsimp <- mestra_sampling_bio_sp %>% 
  dplyr::mutate(sample_invsimp = vegan::diversity(mestra_sampling_bio_sp , index="invsimpson"))
mestra_sampling_bio_sp_invsimp$sample_invsimp

mestra_sampling_bio_sp_simp <- mestra_sampling_bio_sp %>% 
  dplyr::mutate(sample_simp = vegan::diversity(mestra_sampling_bio_sp , index="simpson"))
mestra_sampling_bio_sp_simp$sample_simp

mestra_sampling_bio_sp_shan <- mestra_sampling_bio_sp %>% 
  dplyr::mutate(sample_shan = vegan::diversity(mestra_sampling_bio_sp , index="shannon"))
mestra_sampling_bio_sp_shan$sample_shan

#add new columns i original table
mestra_sampling_bio$sample_rich  <-     mestra_sampling_bio_sp_rich$rich_sample
mestra_sampling_bio$sample_abund <-     mestra_sampling_bio_sp_abund$abund_sample
mestra_sampling_bio$sample_invsimp  <-     mestra_sampling_bio_sp_invsimp$sample_invsimp
mestra_sampling_bio$sample_simp  <-     mestra_sampling_bio_sp_simp$sample_simp
mestra_sampling_bio$sample_shan  <-     mestra_sampling_bio_sp_shan$sample_shan

colnames(mestra_sampling_bio)

# round decimal number sand organize the columns
mestra_sampling_bio <- mestra_sampling_bio %>% 
  dplyr::select(cod_sample, sample_rich:sample_shan, everything()) %>% 
  dplyr::mutate_at(vars(sample_shan, sample_simp, sample_invsimp), funs(round(.,2)))
colnames(mestra_sampling_bio)

# select interest columns
mestra_sampling_bio_result <- mestra_sampling_bio %>% 
  dplyr::select(cod_sample, sample_rich:sample_shan)
mestra_sampling_bio_result

# prepar mestra
colnames(mestra_sampling)
mestra_sampling_new <- mestra_sampling %>% 
  dplyr::select(cod_sample, filename:se, NI:Flo2km_site)
colnames(mestra_sampling_new)

colnames(mestra_sampling_new_bioresults_ai)
# merge  biodiveristy results with oirginal table
mestra_sampling_new_bioresults_ai <- mestra_sampling_new  %>% 
  dplyr::right_join(mestra_sampling_bio_result, by = "cod_sample") %>% 
  dplyr::right_join(mestra_sampling_acoustic_mean, by = "cod_sample") %>% 
mestra_sampling_new_bioresults_ai

colnames(mestra_sampling_new_bioresults_ai)

# prepar table for forestcover merge
mestra_sampling_new_bioresults_ai_forest <- mestra_sampling_new_bioresults_ai %>% 
  dplyr::mutate(code = paste(landscape, env, sep = "_"))
colnames(mestra_sampling_new_bioresults_ai_forest)

# distinc




write.csv2(mestra_sampling_new_bioresults_ai_forest, "03_main_mestra_master_gaspar_all_RESULTS_by_site.csv")


setwd("/home/gasparzinho/Documents/backpup/ANALISYS_2020_06_d01/00_scripts_gaspar_master/after_meet/aves/00_after_quali/by_env")
dir()

mestra_sampling_new_bioresults_ai_forest <- read.csv2("04_main_mestra_master_gaspar_all_RESULTS_by_site_to_model.csv")
mestra_sampling_new_bioresults_ai_forest_select <- mestra_sampling_new_bioresults_ai_forest %>% 
  dplyr::select(cod_sample, filename, code, env, Flo_1km_lsmean:AR) %>% 
  dplyr::group_by(cod_sample) %>% 
  dplyr::distinct(cod_sample, .keep_all = TRUE)
write.csv2(mestra_sampling_new_bioresults_ai_forest_select, "04_main_mestra_master_gaspar_all_RESULTS_by_site_to_model.csv")
  

# -------------------------------------------------------------------------------------------
#import forest data
forest <- read.csv2( "00_metadata_forest_coord_ccm2.csv" )
forest

forest$Flo2km <- as.numeric(forest$Flo2km)

# round decimal numbers
forest_selec <- forest %>% 
  dplyr::select(codigo,Flo2km) %>% 
  dplyr::mutate_at(vars(Flo2km), funs(round(.,2))) %>% 
  tidyr::separate(codigo, c("landscape", "env"))
forest_selec

# format for enter in dplyr
forest_selec <- as_tibble(forest_selec)

# average of forest cover by landscape
forest_selec_mean <- forest_selec  %>% 
  dplyr::select( landscape, Flo2km) %>% 
  dplyr::group_by(landscape) %>%
  dplyr::summarize(Flo2km_lsmean = mean(Flo2km)) %>% 
  dplyr::arrange(Flo2km_lsmean)
forest_selec_mean

forest_selec_mean$landscape

# merge forest cover in orginal table
mestra_sampling_bioresults_forest_data <- mestra_sampling_new_bioresults_ai_forest %>% 
  dplyr::right_join(forest_selec_mean, by = "landscape" ) %>% 
  dplyr::mutate_at(vars(Flo2km_lsmean), funs(round(.,2)))
colnames(mestra_sampling_bioresults_forest_data)

 # summarise the matrix
mestra_sampling_bioresults_forest_data_group <- mestra_sampling_bioresults_forest_data %>% 
  dplyr::group_by(cod_sample) %>% 
 dplyr::distinct(cod_sample, .keep_all = TRUE)
mestra_sampling_bioresults_forest_data_group

write.csv2(mestra_sampling_bioresults_forest_data_group, "02_main_mestra_master_gaspar_all_RESULTS_by_site.csv")

#save.image("only_sampling_by_site.rda")
#' end-----------------------------------------------------------------------------------------------------------------------
