
#' ---
#' title: model selection
#' author: lucas gaspar
#' date: 2020-08-d29
#' ---
#' 
#'   
#'   # clean
rm(list = ls())

# packages                                                        
if(!require(readxl)) install.packages("readxl")
if(!require(bbmle)) install.packages("bbmle")
if(!require(dplyr)) install.packages("dplyr")
if(!require(rsq)) install.packages("rsq")
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(usdm)) install.packages("usdm")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lme4)) install.packages("lme4")


# directory
setwd("C:\\Users\\Lucas.Gaspar\\Documents\\backpup\\ANALISYS_2020_06_d01\\00_scripts_gaspar_master\\after_meet\\aves\\00_after_quali\\by_env")
dir()

load("all_models_script02.rda")
## import data ----------------------------------------------------------
mestra_birds <- readr::read_csv2("04_main_mestra_master_gaspar_all_RESULTS_by_site_to_model.csv")
colnames(mestra_birds)

summary(mestra_birds$sample_rich)
summary(mestra_birds$sample_abund)
summary(mestra_birds$sample_invsimp)

mestra_birds$env <- str_replace_all(mestra_birds$env, "aa", "pasture")
mestra_birds$env <- str_replace_all(mestra_birds$env, "br", "swamp")
mestra_birds$env <- str_replace_all(mestra_birds$env, "ma", "forest")

mestra_birds_variables <- mestra_birds %>% 
  dplyr::select(sample_rich:AR, env)

mestra_birds_variables <- subset(mestra_birds_variables, env == "forest" )

mestra_birds_select <- mestra_birds %>% 
  dplyr::select(sample_rich:sample_shan)

## VIF Analisys!!Reduce colinearity

pairs(mestra_birds_select)

# VIF rich  -------------------------------------


mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                ADI_300_22050_50db+ 
                ADI_300_22050_75db+ 
                ADI_1000_12000_50db+ 
                ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                AEI_300_22050_50db+ 
                AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                ACI_300_12000_FFT512+ 
                ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                BIO_300_12000_FFT512+ 
                BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich) 
riq_vif1 <- as.data.frame(car::vif(mod.riq))
View(riq_vif1)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                ADI_300_22050_50db+ 
                ADI_300_22050_75db+ 
                ADI_1000_12000_50db+ 
                ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                AEI_300_22050_50db+ 
                AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                BIO_300_12000_FFT512+ 
                BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich) 

riq_vif2 <- as.data.frame(car::vif(mod.riq))
View(riq_vif2)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                ADI_300_22050_50db+ 
                ADI_300_22050_75db+ 
                ADI_1000_12000_50db+ 
                ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                AEI_300_22050_50db+ 
                AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich) 
riq_vif3 <- as.data.frame(car::vif(mod.riq))
View(riq_vif3)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                ADI_300_22050_50db+ 
                ADI_300_22050_75db+ 
                ADI_1000_12000_50db+ 
                ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                #ADI_1000_22050_75db+ 
                AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                AEI_300_22050_50db+ 
                AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich) 
riq_vif4 <- as.data.frame(car::vif(mod.riq))
View(riq_vif4)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                ADI_300_22050_50db+ 
                ADI_300_22050_75db+ 
                ADI_1000_12000_50db+ 
                ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                #ADI_1000_22050_75db+ 
                AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich) 
riq_vif5 <- as.data.frame(car::vif(mod.riq))
View(riq_vif5)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                ADI_300_22050_50db+ 
                ADI_300_22050_75db+ 
                ADI_1000_12000_50db+ 
                ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                #ADI_1000_22050_75db+ 
                AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich) 
riq_vif6 <- as.data.frame(car::vif(mod.riq))
View(riq_vif6)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                ADI_300_22050_50db+ 
                ADI_300_22050_75db+ 
                ADI_1000_12000_50db+ 
                ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                #ADI_1000_22050_75db+ 
                AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich) 
riq_vif7 <- as.data.frame(car::vif(mod.riq))
View(riq_vif7)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                ADI_300_22050_75db+ 
                ADI_1000_12000_50db+ 
                ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                #ADI_1000_22050_75db+ 
                AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich) 
riq_vif8 <- as.data.frame(car::vif(mod.riq))
View(riq_vif8)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif9 <- as.data.frame(car::vif(mod.riq))
View(riq_vif9)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif10 <- as.data.frame(car::vif(mod.riq))
View(riq_vif10)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif11 <- as.data.frame(car::vif(mod.riq))
View(riq_vif11)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                #BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif12 <- as.data.frame(car::vif(mod.riq))
View(riq_vif12)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                #BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif13 <- as.data.frame(car::vif(mod.riq))
View(riq_vif13)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                #AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                #BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif14 <- as.data.frame(car::vif(mod.riq))
View(riq_vif14)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                #AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                #BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif15 <- as.data.frame(car::vif(mod.riq))
View(riq_vif15)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                #AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                #AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                #BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif16 <- as.data.frame(car::vif(mod.riq))
View(riq_vif16)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                #ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                #AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                #AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                ACI_1000_22050_FFT512+
                #BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif17 <- as.data.frame(car::vif(mod.riq))
View(riq_vif17)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                #ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                #AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                #AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                #ACI_1000_22050_FFT512+
                #BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif18 <- as.data.frame(car::vif(mod.riq))
View(riq_vif18)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                #ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                #AEI_300_12000_50db+ 
                AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                #AEI_1000_22050_50db+ 
                #AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                #ACI_1000_22050_FFT512+
                #BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif19 <- as.data.frame(car::vif(mod.riq))
View(riq_vif19)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                #ADI_1000_22050_50db+ 
                ADI_1000_22050_75db+ 
                #AEI_300_12000_50db+ 
                #AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                #AEI_1000_22050_50db+ 
                #AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                #ACI_1000_22050_FFT512+
                #BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif20 <- as.data.frame(car::vif(mod.riq))
View(riq_vif20)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                #ADI_1000_22050_50db+ 
                #ADI_1000_22050_75db+ 
                #AEI_300_12000_50db+ 
                #AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                #AEI_1000_22050_50db+ 
                #AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                #ACI_1000_22050_FFT512+
                #BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif21 <- as.data.frame(car::vif(mod.riq))
View(riq_vif21)

mod.riq <- lm(sample_rich~ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                #ADI_1000_22050_50db+ 
                #ADI_1000_22050_75db+ 
                #AEI_300_12000_50db+ 
                #AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                #AEI_1000_22050_50db+ 
                #AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                #ACI_1000_22050_FFT512+
                #BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                #H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif22 <- as.data.frame(car::vif(mod.riq))
View(riq_vif22)

mod.riq <- lm(sample_rich~#ADI_300_12000_50db+
                ADI_300_12000_75db+
                #ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                #ADI_1000_12000_75db+
                #ADI_1000_22050_50db+ 
                #ADI_1000_22050_75db+ 
                #AEI_300_12000_50db+ 
                #AEI_300_12000_75db+ 
                #AEI_300_22050_50db+ 
                #AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #AEI_1000_12000_75db+ 
                #AEI_1000_22050_50db+ 
                #AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                #ACI_1000_22050_FFT512+
                #BIO_300_12000_FFT512+ 
                #BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                #H_ws512+ 
                Ht+
                AR, data = mestra_birds_rich)
riq_vif23 <- as.data.frame(car::vif(mod.riq))
View(riq_vif23)

mod.riq <- lm(sample_rich~
                ADI_300_12000_75db+
                AEI_1000_12000_50db+
                ACI_1000_12000_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                Ht+
                AR,
              data = mestra_birds_rich)
riq_vif_final <- as.data.frame(car::vif(mod.riq))
as.data.frame(riq_vif_final)

# VIF abund  ---------------------------------------------------------
mestra_birds_abund <- mestra_birds %>% 
  dplyr::select(ADI_300_12000_50db:AR, sample_abund)
colnames(mestra_birds_abund)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  ADI_300_22050_50db+ 
                  ADI_300_22050_75db+ 
                  ADI_1000_12000_50db+ 
                  ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  AEI_300_22050_50db+ 
                  AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  ACI_300_12000_FFT512+ 
                  ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  BIO_300_12000_FFT512+ 
                  BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund) 
abund_vif1 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif1)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  ADI_300_22050_50db+ 
                  ADI_300_22050_75db+ 
                  ADI_1000_12000_50db+ 
                  ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  AEI_300_22050_50db+ 
                  AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  BIO_300_12000_FFT512+ 
                  BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund) 

abund_vif2 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif2)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  ADI_1000_12000_50db+ 
                  ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  AEI_300_22050_50db+ 
                  AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  BIO_300_12000_FFT512+ 
                  BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund) 
abund_vif3 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif3)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  ADI_1000_12000_50db+ 
                  ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  AEI_300_22050_50db+ 
                  AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund) 
abund_vif4 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif4)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  ADI_1000_12000_50db+ 
                  ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund) 
abund_vif5 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif5)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  ADI_1000_12000_50db+ 
                  ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif6 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif6)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  ADI_1000_12000_50db+ 
                  ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif7 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif7)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  ADI_1000_12000_50db+ 
                  ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif8 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif8)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif9 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif9)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif10 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif10)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  #ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif11 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif11)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  #ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  #BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif12 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif12)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  #ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  #BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif13 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif13)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  #AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  #ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  #BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif14 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif14)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  #AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  AEI_1000_22050_75db+ 
                  #ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  #BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif15 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif15)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  #AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  #AEI_1000_22050_75db+ 
                  #ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  #BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif16 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif16)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  #ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  #AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  #AEI_1000_22050_75db+ 
                  #ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  ACI_1000_22050_FFT512+
                  #BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif17 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif17)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  #ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  #AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  #AEI_1000_22050_75db+ 
                  #ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  #ACI_1000_22050_FFT512+
                  #BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif18 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif18)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  #ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  #AEI_300_12000_50db+ 
                  AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  #AEI_1000_22050_50db+ 
                  #AEI_1000_22050_75db+ 
                  #ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  #ACI_1000_22050_FFT512+
                  #BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif19 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif19)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  #ADI_1000_22050_50db+ 
                  ADI_1000_22050_75db+ 
                  #AEI_300_12000_50db+ 
                  #AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  #AEI_1000_22050_50db+ 
                  #AEI_1000_22050_75db+ 
                  #ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  #ACI_1000_22050_FFT512+
                  #BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif20 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif20)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  #ADI_1000_22050_50db+ 
                  #ADI_1000_22050_75db+ 
                  #AEI_300_12000_50db+ 
                  #AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  #AEI_1000_22050_50db+ 
                  #AEI_1000_22050_75db+ 
                  #ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  #ACI_1000_22050_FFT512+
                  #BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif21 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif21)

mod.abund <- lm(sample_abund~ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  #ADI_1000_22050_50db+ 
                  #ADI_1000_22050_75db+ 
                  #AEI_300_12000_50db+ 
                  #AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  #AEI_1000_22050_50db+ 
                  #AEI_1000_22050_75db+ 
                  #ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  #ACI_1000_22050_FFT512+
                  #BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  #H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif22 <- as.data.frame(car::vif(mod.abund))
#View(abund_vif22)

mod.abund <- lm(sample_abund~#ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #ADI_300_22050_50db+ 
                  #ADI_300_22050_75db+ 
                  #ADI_1000_12000_50db+ 
                  #ADI_1000_12000_75db+
                  #ADI_1000_22050_50db+ 
                  #ADI_1000_22050_75db+ 
                  #AEI_300_12000_50db+ 
                  #AEI_300_12000_75db+ 
                  #AEI_300_22050_50db+ 
                  #AEI_300_22050_75db+ 
                  AEI_1000_12000_50db+ 
                  #AEI_1000_12000_75db+ 
                  #AEI_1000_22050_50db+ 
                  #AEI_1000_22050_75db+ 
                  #ACI_300_12000_FFT512+ 
                  #ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  #ACI_1000_22050_FFT512+
                  #BIO_300_12000_FFT512+ 
                  #BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  #H_ws512+ 
                  Ht+
                  AR, data = mestra_birds_abund)
abund_vif23 <- as.data.frame(car::vif(mod.abund))
View(abund_vif23)

mod.abund <- lm(sample_abund~
                  ADI_300_12000_75db+
                  AEI_1000_12000_50db+
                  ACI_1000_12000_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  Ht+
                  AR,
                data = mestra_birds_abund)
abund_vif_final <- as.data.frame(car::vif(mod.abund))
as.data.frame(abund_vif_final)

# VIF diversidade -----------------------------------------------------
mestra_birds_invisimp <- mestra_birds %>% 
  dplyr::select(ADI_300_12000_50db:AR, sample_invsimp)
mestra_birds_invisimp

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    ADI_300_22050_50db+ 
                    ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    ADI_1000_12000_75db+
                    ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    AEI_300_22050_50db+ 
                    AEI_300_22050_75db+ 
                    AEI_1000_12000_50db+ 
                    AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    ACI_300_12000_FFT512+ 
                    ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    BIO_300_12000_FFT512+ 
                    BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif0 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif0)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    ADI_300_22050_50db+ 
                    ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    ADI_1000_12000_75db+
                    ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    AEI_300_22050_50db+ 
                    AEI_300_22050_75db+ 
                    AEI_1000_12000_50db+ 
                    AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    BIO_300_12000_FFT512+ 
                    BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif1 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif1)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    ADI_1000_12000_75db+
                    ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    AEI_300_22050_50db+ 
                    AEI_300_22050_75db+ 
                    AEI_1000_12000_50db+ 
                    AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    BIO_300_12000_FFT512+ 
                    BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif2 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif2)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    ADI_1000_12000_75db+
                    ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    AEI_300_22050_50db+ 
                    AEI_300_22050_75db+ 
                    AEI_1000_12000_50db+ 
                    AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif3 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif3)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    ADI_1000_12000_75db+
                    ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    AEI_1000_12000_50db+ 
                    AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif4 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif4)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    ADI_1000_12000_75db+
                    ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    #5 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    AEI_1000_12000_50db+ 
                    AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif5 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif5)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    #5 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    AEI_1000_12000_50db+ 
                    AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif6 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif6)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    #5 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif7 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif7)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    #5 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif8 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif8)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    #5 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif9 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif9)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    #5 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif10 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif10)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    #5 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    #11 BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif11 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif11)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    #5 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    #12 AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    #11 BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif12 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif12)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    #5 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    #12 AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    #11 BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    #13 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif13 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif13)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    #5 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    #12 AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    #11 BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    #14 NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    #13 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif14 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif14)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    #5 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    #12 AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    #14 AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    #11 BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    #14 NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    #13 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif14 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif14)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    #ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    #5 15 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    #12 AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    #14 AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    ACI_1000_22050_FFT512+
                    #11 BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    #14 NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    #13 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif15 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif15)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    #ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    AEI_300_12000_75db+ 
                    #5 15 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    #12 AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    #14 AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    #16 ACI_1000_22050_FFT512+
                    #11 BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    #14 NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    #13 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif16 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif16)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    #ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    AEI_300_12000_50db+ 
                    #17 AEI_300_12000_75db+ 
                    #5 15 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    #12 AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    #14 AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    #16 ACI_1000_22050_FFT512+
                    #11 BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    #14 NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    #13 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif17 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif17)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    #ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    #18 AEI_300_12000_50db+ 
                    #17 AEI_300_12000_75db+ 
                    #5 15 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    #12 AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    #14 AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    #16 ACI_1000_22050_FFT512+
                    #11 BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    #14 NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    #13 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif18 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif18)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    #ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    ADI_1000_22050_75db+ 
                    #18 AEI_300_12000_50db+ 
                    #17 AEI_300_12000_75db+ 
                    #5 15 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    #12 AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    #14 AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    #16 ACI_1000_22050_FFT512+
                    #11 BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    #14 NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    #13 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    #19 H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif19 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif19)

mod.invsimp <- lm(sample_invsimp~ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    #ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    #20 ADI_1000_22050_75db+ 
                    #18 AEI_300_12000_50db+ 
                    #17 AEI_300_12000_75db+ 
                    #5 15 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    #12 AEI_1000_12000_50db+ 
                  #7 AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                    #14 AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    #16 ACI_1000_22050_FFT512+
                    #11 BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    #14 NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    #13 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    #19 H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif20 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif20)

mod.invsimp <- lm(sample_invsimp~#21 ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #8 ADI_300_22050_50db+ 
                    #2 ADI_300_22050_75db+ 
                    #ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #6 ADI_1000_22050_50db+ 
                    #20 ADI_1000_22050_75db+ 
                    #18 AEI_300_12000_50db+ 
                    #17 AEI_300_12000_75db+ 
                    #5 15 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    #12 AEI_1000_12000_50db+ 
                  #7 AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                    #14 AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    #16 ACI_1000_22050_FFT512+
                    #11 BIO_300_12000_FFT512+ 
                    #3 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    #14 NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    #13 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    #19 H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif21 <- as.data.frame(car::vif(mod.invsimp))
View(invsimp_vif21)

mod.invsimp <- lm(sample_invsimp~
                    ADI_300_12000_75db+
                    AEI_1000_22050_50db+ 
                    ACI_1000_12000_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
invsimp_vif <- as.data.frame(car::vif(mod.invsimp))
invsimp_vif

# final models -----------------------

## without environment ------------------------------------------------
# rich
mod.riq <- lm(sample_rich~
                ADI_300_12000_75db+
                AEI_1000_12000_50db+
                ACI_1000_12000_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                Ht+
                AR,
              data = mestra_birds_rich)

mod.riq
install.packages("RcmdrPlugin.KMggplot2") 

# GLM
#single
mod.riq.NULL   <- glm(sample_rich~1, data = mestra_birds)
mod.riq.ADI_300_12000_75db  <- glm(sample_rich~ADI_300_12000_75db, data = mestra_birds)
mod.riq.AEI_1000_12000_50db  <- glm(sample_rich~AEI_1000_12000_50db, data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512  <- glm(sample_rich~ACI_1000_12000_FFT512, data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512  <- glm(sample_rich~BIO_1000_12000_FFT512, data = mestra_birds)
mod.riq.BIO_1000_22050_FFT512  <- glm(sample_rich~BIO_1000_22050_FFT512, data = mestra_birds)
mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_rich~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.riq.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_rich~NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.riq.Ht  <- glm(sample_rich~Ht, data = mestra_birds)
mod.riq.AR  <- glm(sample_rich~AR, data = mestra_birds)




#residuos -----------------------------------------
setwd("C:\\Users\\Lucas.Gaspar\\Documents\\backpup\\ANALISYS_2020_06_d01\\00_scripts_gaspar_master\\after_meet\\aves\\00_after_quali\\by_env\\2023\\MISTOS\\redisuos")

resid_riq_single <- list(mod.riq.NULL, mod.riq.ADI_300_12000_75db, 
                         mod.riq.AEI_1000_12000_50db, 
                         mod.riq.ACI_1000_12000_FFT512, 
                         mod.riq.BIO_1000_12000_FFT512, 
                         mod.riq.BIO_1000_22050_FFT512,  
                         mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512,
                         mod.riq.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                         mod.riq.Ht,
                         mod.riq.AR)



for(i in resid_riq_single){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
} 
# bivariate
mod.riq.ADI_300_12000_75db.AEI_1000_12000_50db  <- glm(sample_rich~ADI_300_12000_75db+AEI_1000_12000_50db, data = mestra_birds)
mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512  <- glm(sample_rich~ADI_300_12000_75db+ACI_1000_12000_FFT512, data = mestra_birds)
mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512  <- glm(sample_rich~ADI_300_12000_75db+BIO_1000_12000_FFT512, data = mestra_birds)
mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512  <- glm(sample_rich~ADI_300_12000_75db+BIO_1000_22050_FFT512, data = mestra_birds)
mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_rich~ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.riq.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_rich~ADI_300_12000_75db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.riq.ADI_300_12000_75db.Ht  <- glm(sample_rich~ADI_300_12000_75db+Ht, data = mestra_birds)
mod.riq.ADI_300_12000_75db.AR  <- glm(sample_rich~ADI_300_12000_75db+AR, data = mestra_birds)

mod.riq.AEI_1000_12000_50db.ACI_1000_12000_FFT512  <- glm(sample_rich~AEI_1000_12000_50db+ACI_1000_12000_FFT512, data = mestra_birds)
mod.riq.AEI_1000_12000_50db.BIO_1000_12000_FFT512  <- glm(sample_rich~AEI_1000_12000_50db+BIO_1000_12000_FFT512, data = mestra_birds)
mod.riq.AEI_1000_12000_50db.BIO_1000_22050_FFT512  <- glm(sample_rich~AEI_1000_12000_50db+BIO_1000_22050_FFT512, data = mestra_birds)
mod.riq.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_rich~AEI_1000_12000_50db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.riq.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_rich~AEI_1000_12000_50db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.riq.AEI_1000_12000_50db.Ht  <- glm(sample_rich~AEI_1000_12000_50db+Ht, data = mestra_birds)
mod.riq.AEI_1000_12000_50db.AR  <- glm(sample_rich~AEI_1000_12000_50db+AR, data = mestra_birds)

mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512  <- glm(sample_rich~ACI_1000_12000_FFT512+BIO_1000_12000_FFT512, data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_rich~ACI_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_rich~ACI_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_rich~ACI_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.Ht  <- glm(sample_rich~ACI_1000_12000_FFT512+Ht, data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.AR  <- glm(sample_rich~ACI_1000_12000_FFT512+AR, data = mestra_birds)

mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_rich~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_rich~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_rich~BIO_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.Ht  <- glm(sample_rich~BIO_1000_12000_FFT512+Ht, data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.AR  <- glm(sample_rich~BIO_1000_12000_FFT512+AR, data = mestra_birds)

mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_rich~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_rich~BIO_1000_22050_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.riq.BIO_1000_22050_FFT512.Ht  <- glm(sample_rich~BIO_1000_22050_FFT512+Ht, data = mestra_birds)
mod.riq.BIO_1000_22050_FFT512.AR  <- glm(sample_rich~BIO_1000_22050_FFT512+AR, data = mestra_birds)

mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_rich~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht  <- glm(sample_rich~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+Ht, data = mestra_birds)
mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR  <- glm(sample_rich~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+AR, data = mestra_birds)

mod.riq.Ht.AR  <- glm(sample_rich~Ht+AR, data = mestra_birds)





resid_riq_bivariate <- list(mod.riq.ADI_300_12000_75db.AEI_1000_12000_50db ,
                            mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512  ,
                            mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512 , 
                            mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512 , 
                            mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  ,
                            mod.riq.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 ,
                            mod.riq.ADI_300_12000_75db.Ht  ,
                            mod.riq.ADI_300_12000_75db.AR  ,
                            
                            mod.riq.AEI_1000_12000_50db.ACI_1000_12000_FFT512 ,
                            mod.riq.AEI_1000_12000_50db.BIO_1000_12000_FFT512 ,
                            mod.riq.AEI_1000_12000_50db.BIO_1000_22050_FFT512  ,
                            mod.riq.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 ,
                            mod.riq.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                            mod.riq.AEI_1000_12000_50db.Ht  ,
                            mod.riq.AEI_1000_12000_50db.AR ,
                            
                            mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512  ,
                            mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512 ,
                            mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  ,
                            mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 ,
                            mod.riq.ACI_1000_12000_FFT512.Ht ,
                            mod.riq.ACI_1000_12000_FFT512.AR ,
                            
                            mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512 ,
                            mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 ,
                            mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 ,
                            mod.riq.BIO_1000_12000_FFT512.Ht ,
                            mod.riq.BIO_1000_12000_FFT512.AR  ,
                            
                            mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 ,
                            mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 ,
                            mod.riq.BIO_1000_22050_FFT512.Ht ,
                            mod.riq.BIO_1000_22050_FFT512.AR ,
                            
                            mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 ,
                            mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht ,
                            mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR,
                            
                            mod.riq.Ht.AR )

for(i in resid_riq_bivariate){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
} 

#abund
mod.abund <- lm(sample_abund~
                  ADI_300_12000_75db+
                  AEI_1000_12000_50db+
                  ACI_1000_12000_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  Ht+
                  AR,
                data = mestra_birds_abund)
mod.abund

# GLM
#single
mod.abund.NULL   <- glm(sample_abund~1, data = mestra_birds)
mod.abund.ADI_300_12000_75db  <- glm(sample_abund~ADI_300_12000_75db, data = mestra_birds)
mod.abund.AEI_1000_12000_50db  <- glm(sample_abund~AEI_1000_12000_50db, data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512  <- glm(sample_abund~ACI_1000_12000_FFT512, data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512, data = mestra_birds)
mod.abund.BIO_1000_22050_FFT512  <- glm(sample_abund~BIO_1000_22050_FFT512, data = mestra_birds)
mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_abund~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.abund.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.abund.Ht  <- glm(sample_abund~Ht, data = mestra_birds)
mod.abund.AR  <- glm(sample_abund~AR, data = mestra_birds)

### mistos
mod.abund.NULL   <- glm(sample_abund~1, data = mestra_birds)
mod.abund.ADI_300_12000_75db  <- glm(sample_abund~ADI_300_12000_75db, data = mestra_birds)
mod.abund.AEI_1000_12000_50db  <- glm(sample_abund~AEI_1000_12000_50db, data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512  <- glm(sample_abund~ACI_1000_12000_FFT512, data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512, data = mestra_birds)
mod.abund.BIO_1000_22050_FFT512  <- glm(sample_abund~BIO_1000_22050_FFT512, data = mestra_birds)
mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_abund~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.abund.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.abund.Ht  <- glm(sample_abund~Ht, data = mestra_birds)
mod.abund.AR  <- glm(sample_abund~AR, data = mestra_birds)


resid_abund_single <- list(mod.abund.NULL  ,
                           mod.abund.ADI_300_12000_75db ,
                           mod.abund.AEI_1000_12000_50db, 
                           mod.abund.ACI_1000_12000_FFT512  ,
                           mod.abund.BIO_1000_12000_FFT512 ,
                           mod.abund.BIO_1000_22050_FFT512  ,
                           mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 ,
                           mod.abund.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  ,
                           mod.abund.Ht , 
                           mod.abund.AR )

for(i in resid_abund_single){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
} 

# bivariate
mod.abund.ADI_300_12000_75db.AEI_1000_12000_50db  <- glm(sample_abund~ADI_300_12000_75db+AEI_1000_12000_50db, data = mestra_birds)
mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512  <- glm(sample_abund~ADI_300_12000_75db+ACI_1000_12000_FFT512, data = mestra_birds)
mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512  <- glm(sample_abund~ADI_300_12000_75db+BIO_1000_12000_FFT512, data = mestra_birds)
mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512  <- glm(sample_abund~ADI_300_12000_75db+BIO_1000_22050_FFT512, data = mestra_birds)
mod.abund.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_abund~ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.abund.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~ADI_300_12000_75db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.abund.ADI_300_12000_75db.Ht  <- glm(sample_abund~ADI_300_12000_75db+Ht, data = mestra_birds)
mod.abund.ADI_300_12000_75db.AR  <- glm(sample_abund~ADI_300_12000_75db+AR, data = mestra_birds)

mod.abund.AEI_1000_12000_50db.ACI_1000_12000_FFT512  <- glm(sample_abund~AEI_1000_12000_50db+ACI_1000_12000_FFT512, data = mestra_birds)
mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512  <- glm(sample_abund~AEI_1000_12000_50db+BIO_1000_12000_FFT512, data = mestra_birds)
mod.abund.AEI_1000_12000_50db.BIO_1000_22050_FFT512  <- glm(sample_abund~AEI_1000_12000_50db+BIO_1000_22050_FFT512, data = mestra_birds)
mod.abund.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_abund~AEI_1000_12000_50db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.abund.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~AEI_1000_12000_50db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.abund.AEI_1000_12000_50db.Ht  <- glm(sample_abund~AEI_1000_12000_50db+Ht, data = mestra_birds)
mod.abund.AEI_1000_12000_50db.AR  <- glm(sample_abund~AEI_1000_12000_50db+AR, data = mestra_birds)

mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512  <- glm(sample_abund~ACI_1000_12000_FFT512+BIO_1000_12000_FFT512, data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_abund~ACI_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_abund~ACI_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~ACI_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.Ht  <- glm(sample_abund~ACI_1000_12000_FFT512+Ht, data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.AR  <- glm(sample_abund~ACI_1000_12000_FFT512+AR, data = mestra_birds)

mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.Ht  <- glm(sample_abund~BIO_1000_12000_FFT512+Ht, data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.AR  <- glm(sample_abund~BIO_1000_12000_FFT512+AR, data = mestra_birds)

mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_abund~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~BIO_1000_22050_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.abund.BIO_1000_22050_FFT512.Ht  <- glm(sample_abund~BIO_1000_22050_FFT512+Ht, data = mestra_birds)
mod.abund.BIO_1000_22050_FFT512.AR  <- glm(sample_abund~BIO_1000_22050_FFT512+AR, data = mestra_birds)

mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht  <- glm(sample_abund~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+Ht, data = mestra_birds)
mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR  <- glm(sample_abund~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+AR, data = mestra_birds)

mod.abund.Ht.AR  <- glm(sample_abund~Ht+AR, data = mestra_birds)

# residual

resid_abund_bivar <- list(mod.abund.ADI_300_12000_75db.AEI_1000_12000_50db , 
                          mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512  ,
                          mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512 ,
                          mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512 ,
                          mod.abund.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 ,
                          mod.abund.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  ,
                          mod.abund.ADI_300_12000_75db.Ht,
                          mod.abund.ADI_300_12000_75db.AR ,
                          
                          mod.abund.AEI_1000_12000_50db.ACI_1000_12000_FFT512  ,
                          mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512 ,
                          mod.abund.AEI_1000_12000_50db.BIO_1000_22050_FFT512 ,
                          mod.abund.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 ,
                          mod.abund.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  ,
                          mod.abund.AEI_1000_12000_50db.Ht ,
                          mod.abund.AEI_1000_12000_50db.AR ,
                          
                          mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512  ,
                          mod.abund.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512  ,
                          mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 , 
                          mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 ,
                          mod.abund.ACI_1000_12000_FFT512.Ht  ,
                          mod.abund.ACI_1000_12000_FFT512.AR  ,
                          
                          mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  ,
                          mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 ,
                          mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 ,
                          mod.abund.BIO_1000_12000_FFT512.Ht  ,
                          mod.abund.BIO_1000_12000_FFT512.AR ,
                          
                          mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  ,
                          mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 ,
                          mod.abund.BIO_1000_22050_FFT512.Ht  ,
                          mod.abund.BIO_1000_22050_FFT512.AR  ,
                          
                          mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  ,
                          mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht,
                          mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR ,
                          
                          mod.abund.Ht.AR  )
for(i in resid_abund_bivar){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
} 

#div
mod.invsimp <- lm(sample_invsimp~
                    ADI_300_12000_75db+
                    AEI_1000_22050_50db+ 
                    ACI_1000_12000_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    Ht+
                    AR, data = mestra_birds_invisimp)
mod.invsimp 
View(mestra_birds_invisimp)
# GLM
#single
mod.invsimp.NULL   <- glm(sample_invsimp~1, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db  <- glm(sample_invsimp~ADI_300_12000_75db, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db  <- glm(sample_invsimp~AEI_1000_12000_50db, data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512  <- glm(sample_invsimp~ACI_1000_12000_FFT512, data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512  <- glm(sample_invsimp~BIO_1000_12000_FFT512, data = mestra_birds)
mod.invsimp.BIO_1000_22050_FFT512  <- glm(sample_invsimp~BIO_1000_22050_FFT512, data = mestra_birds)
mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.invsimp.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_invsimp~NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.invsimp.Ht  <- glm(sample_invsimp~Ht, data = mestra_birds)
mod.invsimp.AR  <- glm(sample_invsimp~AR, data = mestra_birds)

# residual
resid_invsimp_single <- list(mod.invsimp.NULL ,
mod.invsimp.ADI_300_12000_75db , 
mod.invsimp.AEI_1000_12000_50db ,
mod.invsimp.ACI_1000_12000_FFT512  ,
mod.invsimp.BIO_1000_12000_FFT512  ,
mod.invsimp.BIO_1000_22050_FFT512  ,
mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 ,
mod.invsimp.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 ,
mod.invsimp.Ht ,
mod.invsimp.AR  )

for(i in resid_invsimp_single){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
} 
# bivariate
mod.invsimp.ADI_300_12000_75db.AEI_1000_12000_50db  <- glm(sample_invsimp~ADI_300_12000_75db+AEI_1000_12000_50db, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512  <- glm(sample_invsimp~ADI_300_12000_75db+ACI_1000_12000_FFT512, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512  <- glm(sample_invsimp~ADI_300_12000_75db+BIO_1000_12000_FFT512, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.BIO_1000_22050_FFT512  <- glm(sample_invsimp~ADI_300_12000_75db+BIO_1000_22050_FFT512, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_invsimp~ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_invsimp~ADI_300_12000_75db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.Ht  <- glm(sample_invsimp~ADI_300_12000_75db+Ht, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.AR  <- glm(sample_invsimp~ADI_300_12000_75db+AR, data = mestra_birds)

mod.invsimp.AEI_1000_12000_50db.ACI_1000_12000_FFT512  <- glm(sample_invsimp~AEI_1000_12000_50db+ACI_1000_12000_FFT512, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.BIO_1000_12000_FFT512  <- glm(sample_invsimp~AEI_1000_12000_50db+BIO_1000_12000_FFT512, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.BIO_1000_22050_FFT512  <- glm(sample_invsimp~AEI_1000_12000_50db+BIO_1000_22050_FFT512, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_invsimp~AEI_1000_12000_50db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_invsimp~AEI_1000_12000_50db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.Ht  <- glm(sample_invsimp~AEI_1000_12000_50db+Ht, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.AR  <- glm(sample_invsimp~AEI_1000_12000_50db+AR, data = mestra_birds)

mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512  <- glm(sample_invsimp~ACI_1000_12000_FFT512+BIO_1000_12000_FFT512, data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_invsimp~ACI_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_invsimp~ACI_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_invsimp~ACI_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.Ht  <- glm(sample_invsimp~ACI_1000_12000_FFT512+Ht, data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.AR  <- glm(sample_invsimp~ACI_1000_12000_FFT512+AR, data = mestra_birds)

mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_invsimp~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_invsimp~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_invsimp~BIO_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.Ht  <- glm(sample_invsimp~BIO_1000_12000_FFT512+Ht, data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.AR  <- glm(sample_invsimp~BIO_1000_12000_FFT512+AR, data = mestra_birds)

mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  <- glm(sample_invsimp~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, data = mestra_birds)
mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_invsimp~BIO_1000_22050_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.invsimp.BIO_1000_22050_FFT512.Ht  <- glm(sample_invsimp~BIO_1000_22050_FFT512+Ht, data = mestra_birds)
mod.invsimp.BIO_1000_22050_FFT512.AR  <- glm(sample_invsimp~BIO_1000_22050_FFT512+AR, data = mestra_birds)

mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds)
mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht  <- glm(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+Ht, data = mestra_birds)
mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR  <- glm(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+AR, data = mestra_birds)

mod.invsimp.Ht.AR  <- glm(sample_invsimp~Ht+AR, data = mestra_birds)

# residual

resid_invsimp_bivar <- list(mod.invsimp.ADI_300_12000_75db.AEI_1000_12000_50db ,
                            mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512  ,
                            mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512 ,
                            mod.invsimp.ADI_300_12000_75db.BIO_1000_22050_FFT512 ,
                            mod.invsimp.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  ,
                            mod.invsimp.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , 
                            mod.invsimp.ADI_300_12000_75db.Ht ,
                            mod.invsimp.ADI_300_12000_75db.AR  ,
                            
                            mod.invsimp.AEI_1000_12000_50db.ACI_1000_12000_FFT512 ,
                            mod.invsimp.AEI_1000_12000_50db.BIO_1000_12000_FFT512,  
                            mod.invsimp.AEI_1000_12000_50db.BIO_1000_22050_FFT512  ,
                            mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  ,
                            mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  ,
                            mod.invsimp.AEI_1000_12000_50db.Ht  ,
                            mod.invsimp.AEI_1000_12000_50db.AR  ,
                            
                            mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512  ,
                            mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512  ,
                            mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512  ,
                            mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 ,
                            mod.invsimp.ACI_1000_12000_FFT512.Ht  ,
                            mod.invsimp.ACI_1000_12000_FFT512.AR ,
                            
                            mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  ,
                            mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 ,
                            mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 ,
                            mod.invsimp.BIO_1000_12000_FFT512.Ht  ,
                            mod.invsimp.BIO_1000_12000_FFT512.AR ,
                            
                            mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 ,
                            mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  ,
                            mod.invsimp.BIO_1000_22050_FFT512.Ht ,
                            mod.invsimp.BIO_1000_22050_FFT512.AR ,
                            
                            mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  ,
                            mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht  ,
                            mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR ,
                            
                            mod.invsimp.Ht.AR )

for(i in resid_invsimp_bivar){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
}

## with environment ------------------------------------------------
# rich

# GLM
#single
mod.riq.ADI_300_12000_75db.env  <- glm(sample_rich~ADI_300_12000_75db+env, data = mestra_birds)
mod.riq.AEI_1000_12000_50db.env  <- glm(sample_rich~AEI_1000_12000_50db+env, data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.env  <- glm(sample_rich~ACI_1000_12000_FFT512+env, data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.env  <- glm(sample_rich~BIO_1000_12000_FFT512+env, data = mestra_birds)
mod.riq.BIO_1000_22050_FFT512.env  <- glm(sample_rich~BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_rich~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.riq.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_rich~NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.riq.Ht.env  <- glm(sample_rich~Ht+env, data = mestra_birds)
mod.riq.AR.env  <- glm(sample_rich~AR+env, data = mestra_birds)

# residual

resid_riq_single_env <- list(mod.riq.ADI_300_12000_75db.env  ,
                             mod.riq.AEI_1000_12000_50db.env  ,
                             mod.riq.ACI_1000_12000_FFT512.env  ,
                             mod.riq.BIO_1000_12000_FFT512.env , 
                             mod.riq.BIO_1000_22050_FFT512.env  ,
                             mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                             mod.riq.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                             mod.riq.Ht.env  ,
                             mod.riq.AR.env )

for(i in resid_riq_single_env){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
}

# bivariate
mod.riq.ADI_300_12000_75db.AEI_1000_12000_50db.env  <- glm(sample_rich~ADI_300_12000_75db+AEI_1000_12000_50db+env, data = mestra_birds)
mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512.env  <- glm(sample_rich~ADI_300_12000_75db+ACI_1000_12000_FFT512+env, data = mestra_birds)
mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512.env  <- glm(sample_rich~ADI_300_12000_75db+BIO_1000_12000_FFT512+env, data = mestra_birds)
mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512.env  <- glm(sample_rich~ADI_300_12000_75db+BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_rich~ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.riq.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_rich~ADI_300_12000_75db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.riq.ADI_300_12000_75db.Ht.env  <- glm(sample_rich~ADI_300_12000_75db+Ht+env, data = mestra_birds)
mod.riq.ADI_300_12000_75db.AR.env  <- glm(sample_rich~ADI_300_12000_75db+AR+env, data = mestra_birds)

mod.riq.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env  <- glm(sample_rich~AEI_1000_12000_50db+ACI_1000_12000_FFT512+env, data = mestra_birds)
mod.riq.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env  <- glm(sample_rich~AEI_1000_12000_50db+BIO_1000_12000_FFT512+env, data = mestra_birds)
mod.riq.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env  <- glm(sample_rich~AEI_1000_12000_50db+BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.riq.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_rich~AEI_1000_12000_50db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.riq.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_rich~AEI_1000_12000_50db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.riq.AEI_1000_12000_50db.Ht.env  <- glm(sample_rich~AEI_1000_12000_50db+Ht+env, data = mestra_birds)
mod.riq.AEI_1000_12000_50db.AR.env  <- glm(sample_rich~AEI_1000_12000_50db+AR+env, data = mestra_birds)

mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env  <- glm(sample_rich~ACI_1000_12000_FFT512+BIO_1000_12000_FFT512+env, data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env  <- glm(sample_rich~ACI_1000_12000_FFT512+BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_rich~ACI_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_rich~ACI_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.Ht.env  <- glm(sample_rich~ACI_1000_12000_FFT512+Ht+env, data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.AR.env  <- glm(sample_rich~ACI_1000_12000_FFT512+AR+env, data = mestra_birds)

mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env  <- glm(sample_rich~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_rich~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_rich~BIO_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.Ht.env  <- glm(sample_rich~BIO_1000_12000_FFT512+Ht+env, data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.AR.env  <- glm(sample_rich~BIO_1000_12000_FFT512+AR+env, data = mestra_birds)

mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_rich~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_rich~BIO_1000_22050_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.riq.BIO_1000_22050_FFT512.Ht.env  <- glm(sample_rich~BIO_1000_22050_FFT512+Ht+env, data = mestra_birds)
mod.riq.BIO_1000_22050_FFT512.AR.env  <- glm(sample_rich~BIO_1000_22050_FFT512+AR+env, data = mestra_birds)

mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_rich~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env  <- glm(sample_rich~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+Ht+env, data = mestra_birds)
mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env  <- glm(sample_rich~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+AR+env, data = mestra_birds)

mod.riq.Ht.AR.env  <- glm(sample_rich~Ht+AR+env, data = mestra_birds)

#residual

resid_riq_bivar_env <- list(mod.riq.ADI_300_12000_75db.AEI_1000_12000_50db.env  ,
                            mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512.env ,
                            mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512.env ,
                            mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512.env ,
                            mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env ,
                            mod.riq.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                            mod.riq.ADI_300_12000_75db.Ht.env  ,
                            mod.riq.ADI_300_12000_75db.AR.env ,
                            
                            mod.riq.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env ,
                            mod.riq.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env ,
                            mod.riq.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env  ,
                            mod.riq.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                            mod.riq.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                            mod.riq.AEI_1000_12000_50db.Ht.env ,
                            mod.riq.AEI_1000_12000_50db.AR.env  ,
                            
                            mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env  ,
                            mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env ,
                            mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env ,
                            mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                            mod.riq.ACI_1000_12000_FFT512.Ht.env ,
                            mod.riq.ACI_1000_12000_FFT512.AR.env  ,
                            
                            mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env  ,
                            mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                            mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                            mod.riq.BIO_1000_12000_FFT512.Ht.env ,
                            mod.riq.BIO_1000_12000_FFT512.AR.env  ,
                            
                            mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                            mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                            mod.riq.BIO_1000_22050_FFT512.Ht.env ,
                            mod.riq.BIO_1000_22050_FFT512.AR.env ,
                            
                            mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env , 
                            mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env  ,
                            mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env  ,
                            
                            mod.riq.Ht.AR.env )


for(i in resid_riq_bivar_env ){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
}

#abund
# GLM
#single
mod.abund.NULL   <- glm(sample_abund~1, data = mestra_birds)
mod.abund.ADI_300_12000_75db.env  <- glm(sample_abund~ADI_300_12000_75db+env, data = mestra_birds)
mod.abund.AEI_1000_12000_50db.env  <- glm(sample_abund~AEI_1000_12000_50db+env, data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.env  <- glm(sample_abund~ACI_1000_12000_FFT512+env, data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.env  <- glm(sample_abund~BIO_1000_12000_FFT512+env, data = mestra_birds)
mod.abund.BIO_1000_22050_FFT512.env  <- glm(sample_abund~BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_abund~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.abund.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_abund~NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.abund.Ht.env  <- glm(sample_abund~Ht+env, data = mestra_birds)
mod.abund.AR.env  <- glm(sample_abund~AR+env, data = mestra_birds)

# bivariate
mod.abund.ADI_300_12000_75db.AEI_1000_12000_50db.env  <- glm(sample_abund~ADI_300_12000_75db+AEI_1000_12000_50db+env, data = mestra_birds)
mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512.env  <- glm(sample_abund~ADI_300_12000_75db+ACI_1000_12000_FFT512+env, data = mestra_birds)
mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512.env  <- glm(sample_abund~ADI_300_12000_75db+BIO_1000_12000_FFT512+env, data = mestra_birds)
mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512.env  <- glm(sample_abund~ADI_300_12000_75db+BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.abund.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_abund~ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.abund.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_abund~ADI_300_12000_75db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.abund.ADI_300_12000_75db.Ht.env  <- glm(sample_abund~ADI_300_12000_75db+Ht+env, data = mestra_birds)
mod.abund.ADI_300_12000_75db.AR.env  <- glm(sample_abund~ADI_300_12000_75db+AR+env, data = mestra_birds)

mod.abund.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env  <- glm(sample_abund~AEI_1000_12000_50db+ACI_1000_12000_FFT512+env, data = mestra_birds)
mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env  <- glm(sample_abund~AEI_1000_12000_50db+BIO_1000_12000_FFT512+env, data = mestra_birds)
mod.abund.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env  <- glm(sample_abund~AEI_1000_12000_50db+BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.abund.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_abund~AEI_1000_12000_50db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.abund.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_abund~AEI_1000_12000_50db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.abund.AEI_1000_12000_50db.Ht.env  <- glm(sample_abund~AEI_1000_12000_50db+Ht+env, data = mestra_birds)
mod.abund.AEI_1000_12000_50db.AR.env  <- glm(sample_abund~AEI_1000_12000_50db+AR+env, data = mestra_birds)

mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env  <- glm(sample_abund~ACI_1000_12000_FFT512+BIO_1000_12000_FFT512+env, data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env  <- glm(sample_abund~ACI_1000_12000_FFT512+BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_abund~ACI_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_abund~ACI_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.Ht.env  <- glm(sample_abund~ACI_1000_12000_FFT512+Ht+env, data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.AR.env  <- glm(sample_abund~ACI_1000_12000_FFT512+AR+env, data = mestra_birds)

mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env  <- glm(sample_abund~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_abund~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_abund~BIO_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.Ht.env  <- glm(sample_abund~BIO_1000_12000_FFT512+Ht+env, data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.AR.env  <- glm(sample_abund~BIO_1000_12000_FFT512+AR+env, data = mestra_birds)

mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_abund~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_abund~BIO_1000_22050_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.abund.BIO_1000_22050_FFT512.Ht.env  <- glm(sample_abund~BIO_1000_22050_FFT512+Ht+env, data = mestra_birds)
mod.abund.BIO_1000_22050_FFT512.AR.env  <- glm(sample_abund~BIO_1000_22050_FFT512+AR+env, data = mestra_birds)

mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_abund~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env  <- glm(sample_abund~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+Ht+env, data = mestra_birds)
mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env  <- glm(sample_abund~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+AR+env, data = mestra_birds)

mod.abund.Ht.AR.env  <- glm(sample_abund~Ht+AR+env, data = mestra_birds)

# residual

resid_abund_bivar_env <- list(mod.abund.ADI_300_12000_75db.AEI_1000_12000_50db.env  ,
                              mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512.env ,
                              mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512.env ,
                              mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512.env ,
                              mod.abund.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env ,
                              mod.abund.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env , 
                              mod.abund.ADI_300_12000_75db.Ht.env  ,
                              mod.abund.ADI_300_12000_75db.AR.env ,
                              
                              mod.abund.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env ,
                              mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env  ,
                              mod.abund.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env  ,
                              mod.abund.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                              mod.abund.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                              mod.abund.AEI_1000_12000_50db.Ht.env  ,
                              mod.abund.AEI_1000_12000_50db.AR.env  ,
                              
                              mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env  ,
                              mod.abund.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env , 
                              mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                              mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                              mod.abund.ACI_1000_12000_FFT512.Ht.env ,
                              mod.abund.ACI_1000_12000_FFT512.AR.env ,
                              
                              mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env ,
                              mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env , 
                              mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                              mod.abund.BIO_1000_12000_FFT512.Ht.env  ,
                              mod.abund.BIO_1000_12000_FFT512.AR.env  ,
                              
                              mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env,  
                              mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                              mod.abund.BIO_1000_22050_FFT512.Ht.env ,
                              mod.abund.BIO_1000_22050_FFT512.AR.env  ,
                              
                              mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                              mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env  ,
                              mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env  ,
                              
                              mod.abund.Ht.AR.env  )

for(i in resid_abund_bivar_env ){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
}

#div
# GLM
#single
mod.invsimp.NULL   <- glm(sample_invsimp~1, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.env  <- glm(sample_invsimp~ADI_300_12000_75db+env, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.env  <- glm(sample_invsimp~AEI_1000_12000_50db+env, data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.env  <- glm(sample_invsimp~ACI_1000_12000_FFT512+env, data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.env  <- glm(sample_invsimp~BIO_1000_12000_FFT512+env, data = mestra_birds)
mod.invsimp.BIO_1000_22050_FFT512.env  <- glm(sample_invsimp~BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.invsimp.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_invsimp~NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.invsimp.Ht.env  <- glm(sample_invsimp~Ht+env, data = mestra_birds)
mod.invsimp.AR.env  <- glm(sample_invsimp~AR+env, data = mestra_birds)

# residual

resid_diver_single_env <- list(mod.invsimp.NULL   ,
                               mod.invsimp.ADI_300_12000_75db.env  ,
                               mod.invsimp.AEI_1000_12000_50db.env  ,
                               mod.invsimp.ACI_1000_12000_FFT512.env  ,
                               mod.invsimp.BIO_1000_12000_FFT512.env  ,
                               mod.invsimp.BIO_1000_22050_FFT512.env ,
                               mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env ,
                               mod.invsimp.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                               mod.invsimp.Ht.env  ,
                               mod.invsimp.AR.env)

for(i in resid_diver_single_env ){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
}

# bivariate
mod.invsimp.ADI_300_12000_75db.AEI_1000_12000_50db.env  <- glm(sample_invsimp~ADI_300_12000_75db+AEI_1000_12000_50db+env, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512.env  <- glm(sample_invsimp~ADI_300_12000_75db+ACI_1000_12000_FFT512+env, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512.env  <- glm(sample_invsimp~ADI_300_12000_75db+BIO_1000_12000_FFT512+env, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.BIO_1000_22050_FFT512.env  <- glm(sample_invsimp~ADI_300_12000_75db+BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_invsimp~ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_invsimp~ADI_300_12000_75db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.Ht.env  <- glm(sample_invsimp~ADI_300_12000_75db+Ht+env, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.AR.env  <- glm(sample_invsimp~ADI_300_12000_75db+AR+env, data = mestra_birds)

mod.invsimp.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env  <- glm(sample_invsimp~AEI_1000_12000_50db+ACI_1000_12000_FFT512+env, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env  <- glm(sample_invsimp~AEI_1000_12000_50db+BIO_1000_12000_FFT512+env, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env  <- glm(sample_invsimp~AEI_1000_12000_50db+BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_invsimp~AEI_1000_12000_50db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_invsimp~AEI_1000_12000_50db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.Ht.env  <- glm(sample_invsimp~AEI_1000_12000_50db+Ht+env, data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.AR.env  <- glm(sample_invsimp~AEI_1000_12000_50db+AR+env, data = mestra_birds)

mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env  <- glm(sample_invsimp~ACI_1000_12000_FFT512+BIO_1000_12000_FFT512+env, data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env  <- glm(sample_invsimp~ACI_1000_12000_FFT512+BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_invsimp~ACI_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_invsimp~ACI_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.Ht.env  <- glm(sample_invsimp~ACI_1000_12000_FFT512+Ht+env, data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.AR.env  <- glm(sample_invsimp~ACI_1000_12000_FFT512+AR+env, data = mestra_birds)

mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env  <- glm(sample_invsimp~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512+env, data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_invsimp~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_invsimp~BIO_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.Ht.env  <- glm(sample_invsimp~BIO_1000_12000_FFT512+Ht+env, data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.AR.env  <- glm(sample_invsimp~BIO_1000_12000_FFT512+AR+env, data = mestra_birds)

mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- glm(sample_invsimp~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+env, data = mestra_birds)
mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_invsimp~BIO_1000_22050_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.invsimp.BIO_1000_22050_FFT512.Ht.env  <- glm(sample_invsimp~BIO_1000_22050_FFT512+Ht+env, data = mestra_birds)
mod.invsimp.BIO_1000_22050_FFT512.AR.env  <- glm(sample_invsimp~BIO_1000_22050_FFT512+AR+env, data = mestra_birds)

mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- glm(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+env, data = mestra_birds)
mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env  <- glm(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+Ht+env, data = mestra_birds)
mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env  <- glm(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+AR+env, data = mestra_birds)

mod.invsimp.Ht.AR.env  <- glm(sample_invsimp~Ht+AR+env, data = mestra_birds)

# residual

resid_diver_bivar_env <- list(mod.invsimp.ADI_300_12000_75db.AEI_1000_12000_50db.env  ,
                              mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512.env  ,
                              mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512.env  ,
                              mod.invsimp.ADI_300_12000_75db.BIO_1000_22050_FFT512.env ,
                              mod.invsimp.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                              mod.invsimp.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                              mod.invsimp.ADI_300_12000_75db.Ht.env  ,
                              mod.invsimp.ADI_300_12000_75db.AR.env  ,
                              
                              mod.invsimp.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env  ,
                              mod.invsimp.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env ,
                              mod.invsimp.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env  ,
                              mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env ,
                              mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                              mod.invsimp.AEI_1000_12000_50db.Ht.env  ,
                              mod.invsimp.AEI_1000_12000_50db.AR.env ,
                              
                              mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env  ,
                              mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env ,
                              mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                              mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                              mod.invsimp.ACI_1000_12000_FFT512.Ht.env ,
                              mod.invsimp.ACI_1000_12000_FFT512.AR.env  ,
                              
                              mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env ,
                              mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                              mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                              mod.invsimp.BIO_1000_12000_FFT512.Ht.env  ,
                              mod.invsimp.BIO_1000_12000_FFT512.AR.env  ,
                              
                              mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                              mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                              mod.invsimp.BIO_1000_22050_FFT512.Ht.env  ,
                              mod.invsimp.BIO_1000_22050_FFT512.AR.env ,
                              
                              mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                              mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env  ,
                              mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env  ,
                              
                              mod.invsimp.Ht.AR.env )


for(i in resid_diver_bivar_env ){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
}



# ------------------------------------------------------------------------------------------------

# MISTO


## with environment ------------------------------------------------
# rich

# GLM
#single
mod.riq.ADI_300_12000_75db.env  <- lmer(sample_rich~ADI_300_12000_75db + (1 | env), data = mestra_birds)
mod.riq.AEI_1000_12000_50db.env  <- lmer(sample_rich~AEI_1000_12000_50db + (1 | env), data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.env  <- lmer(sample_rich~ACI_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.env  <- lmer(sample_rich~BIO_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.BIO_1000_22050_FFT512.env  <- lmer(sample_rich~BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_rich~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_rich~NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.Ht.env  <- lmer(sample_rich~Ht + (1 | env), data = mestra_birds)
mod.riq.AR.env  <- lmer(sample_rich~AR + (1 | env), data = mestra_birds)

# residual

resid_riq_single_env <- list(mod.riq.ADI_300_12000_75db.env  ,
                             mod.riq.AEI_1000_12000_50db.env  ,
                             mod.riq.ACI_1000_12000_FFT512.env  ,
                             mod.riq.BIO_1000_12000_FFT512.env , 
                             mod.riq.BIO_1000_22050_FFT512.env  ,
                             mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                             mod.riq.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                             mod.riq.Ht.env  ,
                             mod.riq.AR.env )

for(i in resid_riq_single_env){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
}

# bivariate
mod.riq.ADI_300_12000_75db.AEI_1000_12000_50db.env  <- lmer(sample_rich~ADI_300_12000_75db+AEI_1000_12000_50db + (1 | env), data = mestra_birds)
mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512.env  <- lmer(sample_rich~ADI_300_12000_75db+ACI_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512.env  <- lmer(sample_rich~ADI_300_12000_75db+BIO_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512.env  <- lmer(sample_rich~ADI_300_12000_75db+BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_rich~ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_rich~ADI_300_12000_75db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.ADI_300_12000_75db.Ht.env  <- lmer(sample_rich~ADI_300_12000_75db+Ht + (1 | env), data = mestra_birds)
mod.riq.ADI_300_12000_75db.AR.env  <- lmer(sample_rich~ADI_300_12000_75db+AR + (1 | env), data = mestra_birds)

mod.riq.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env  <- lmer(sample_rich~AEI_1000_12000_50db+ACI_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env  <- lmer(sample_rich~AEI_1000_12000_50db+BIO_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env  <- lmer(sample_rich~AEI_1000_12000_50db+BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.riq.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_rich~AEI_1000_12000_50db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_rich~AEI_1000_12000_50db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.AEI_1000_12000_50db.Ht.env  <- lmer(sample_rich~AEI_1000_12000_50db+Ht + (1 | env), data = mestra_birds)
mod.riq.AEI_1000_12000_50db.AR.env  <- lmer(sample_rich~AEI_1000_12000_50db+AR + (1 | env), data = mestra_birds)

mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env  <- lmer(sample_rich~ACI_1000_12000_FFT512+BIO_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env  <- lmer(sample_rich~ACI_1000_12000_FFT512+BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_rich~ACI_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_rich~ACI_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.Ht.env  <- lmer(sample_rich~ACI_1000_12000_FFT512+Ht + (1 | env), data = mestra_birds)
mod.riq.ACI_1000_12000_FFT512.AR.env  <- lmer(sample_rich~ACI_1000_12000_FFT512+AR + (1 | env), data = mestra_birds)

mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env  <- lmer(sample_rich~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_rich~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_rich~BIO_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.Ht.env  <- lmer(sample_rich~BIO_1000_12000_FFT512+Ht + (1 | env), data = mestra_birds)
mod.riq.BIO_1000_12000_FFT512.AR.env  <- lmer(sample_rich~BIO_1000_12000_FFT512+AR + (1 | env), data = mestra_birds)

mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_rich~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_rich~BIO_1000_22050_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.BIO_1000_22050_FFT512.Ht.env  <- lmer(sample_rich~BIO_1000_22050_FFT512+Ht + (1 | env), data = mestra_birds)
mod.riq.BIO_1000_22050_FFT512.AR.env  <- lmer(sample_rich~BIO_1000_22050_FFT512+AR + (1 | env), data = mestra_birds)

mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_rich~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env  <- lmer(sample_rich~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+Ht + (1 | env), data = mestra_birds)
mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env  <- lmer(sample_rich~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+AR + (1 | env), data = mestra_birds)

mod.riq.Ht.AR.env  <- lmer(sample_rich~Ht+AR + (1 | env), data = mestra_birds)

#residual

resid_riq_bivar_env <- list(mod.riq.ADI_300_12000_75db.AEI_1000_12000_50db.env  ,
                            mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512.env ,
                            mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512.env ,
                            mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512.env ,
                            mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env ,
                            mod.riq.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                            mod.riq.ADI_300_12000_75db.Ht.env  ,
                            mod.riq.ADI_300_12000_75db.AR.env ,
                            
                            mod.riq.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env ,
                            mod.riq.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env ,
                            mod.riq.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env  ,
                            mod.riq.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                            mod.riq.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                            mod.riq.AEI_1000_12000_50db.Ht.env ,
                            mod.riq.AEI_1000_12000_50db.AR.env  ,
                            
                            mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env  ,
                            mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env ,
                            mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env ,
                            mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                            mod.riq.ACI_1000_12000_FFT512.Ht.env ,
                            mod.riq.ACI_1000_12000_FFT512.AR.env  ,
                            
                            mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env  ,
                            mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                            mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                            mod.riq.BIO_1000_12000_FFT512.Ht.env ,
                            mod.riq.BIO_1000_12000_FFT512.AR.env  ,
                            
                            mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                            mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                            mod.riq.BIO_1000_22050_FFT512.Ht.env ,
                            mod.riq.BIO_1000_22050_FFT512.AR.env ,
                            
                            mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env , 
                            mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env  ,
                            mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env  ,
                            
                            mod.riq.Ht.AR.env )


for(i in resid_riq_bivar_env ){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
}

#abund
# lmer
#single
mod.abund.NULL   <- lmer(sample_abund~1, data = mestra_birds)
mod.abund.ADI_300_12000_75db.env  <- lmer(sample_abund~ADI_300_12000_75db + (1 | env), data = mestra_birds)
mod.abund.AEI_1000_12000_50db.env  <- lmer(sample_abund~AEI_1000_12000_50db + (1 | env), data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.env  <- lmer(sample_abund~ACI_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.env  <- lmer(sample_abund~BIO_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.BIO_1000_22050_FFT512.env  <- lmer(sample_abund~BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_abund~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_abund~NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.Ht.env  <- lmer(sample_abund~Ht + (1 | env), data = mestra_birds)
mod.abund.AR.env  <- lmer(sample_abund~AR + (1 | env), data = mestra_birds)

# bivariate
mod.abund.ADI_300_12000_75db.AEI_1000_12000_50db.env  <- lmer(sample_abund~ADI_300_12000_75db+AEI_1000_12000_50db + (1 | env), data = mestra_birds)
mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512.env  <- lmer(sample_abund~ADI_300_12000_75db+ACI_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512.env  <- lmer(sample_abund~ADI_300_12000_75db+BIO_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512.env  <- lmer(sample_abund~ADI_300_12000_75db+BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.abund.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_abund~ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_abund~ADI_300_12000_75db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.ADI_300_12000_75db.Ht.env  <- lmer(sample_abund~ADI_300_12000_75db+Ht + (1 | env), data = mestra_birds)
mod.abund.ADI_300_12000_75db.AR.env  <- lmer(sample_abund~ADI_300_12000_75db+AR + (1 | env), data = mestra_birds)

mod.abund.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env  <- lmer(sample_abund~AEI_1000_12000_50db+ACI_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env  <- lmer(sample_abund~AEI_1000_12000_50db+BIO_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env  <- lmer(sample_abund~AEI_1000_12000_50db+BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.abund.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_abund~AEI_1000_12000_50db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_abund~AEI_1000_12000_50db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.AEI_1000_12000_50db.Ht.env  <- lmer(sample_abund~AEI_1000_12000_50db+Ht + (1 | env), data = mestra_birds)
mod.abund.AEI_1000_12000_50db.AR.env  <- lmer(sample_abund~AEI_1000_12000_50db+AR + (1 | env), data = mestra_birds)

mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env  <- lmer(sample_abund~ACI_1000_12000_FFT512+BIO_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env  <- lmer(sample_abund~ACI_1000_12000_FFT512+BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_abund~ACI_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_abund~ACI_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.Ht.env  <- lmer(sample_abund~ACI_1000_12000_FFT512+Ht + (1 | env), data = mestra_birds)
mod.abund.ACI_1000_12000_FFT512.AR.env  <- lmer(sample_abund~ACI_1000_12000_FFT512+AR + (1 | env), data = mestra_birds)

mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env  <- lmer(sample_abund~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_abund~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_abund~BIO_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.Ht.env  <- lmer(sample_abund~BIO_1000_12000_FFT512+Ht + (1 | env), data = mestra_birds)
mod.abund.BIO_1000_12000_FFT512.AR.env  <- lmer(sample_abund~BIO_1000_12000_FFT512+AR + (1 | env), data = mestra_birds)

mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_abund~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_abund~BIO_1000_22050_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.BIO_1000_22050_FFT512.Ht.env  <- lmer(sample_abund~BIO_1000_22050_FFT512+Ht + (1 | env), data = mestra_birds)
mod.abund.BIO_1000_22050_FFT512.AR.env  <- lmer(sample_abund~BIO_1000_22050_FFT512+AR + (1 | env), data = mestra_birds)

mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_abund~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env  <- lmer(sample_abund~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+Ht + (1 | env), data = mestra_birds)
mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env  <- lmer(sample_abund~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+AR + (1 | env), data = mestra_birds)

mod.abund.Ht.AR.env  <- lmer(sample_abund~Ht+AR + (1 | env), data = mestra_birds)

# residual

resid_abund_bivar_env <- list(mod.abund.ADI_300_12000_75db.AEI_1000_12000_50db.env  ,
                              mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512.env ,
                              mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512.env ,
                              mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512.env ,
                              mod.abund.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env ,
                              mod.abund.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env , 
                              mod.abund.ADI_300_12000_75db.Ht.env  ,
                              mod.abund.ADI_300_12000_75db.AR.env ,
                              
                              mod.abund.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env ,
                              mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env  ,
                              mod.abund.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env  ,
                              mod.abund.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                              mod.abund.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                              mod.abund.AEI_1000_12000_50db.Ht.env  ,
                              mod.abund.AEI_1000_12000_50db.AR.env  ,
                              
                              mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env  ,
                              mod.abund.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env , 
                              mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                              mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                              mod.abund.ACI_1000_12000_FFT512.Ht.env ,
                              mod.abund.ACI_1000_12000_FFT512.AR.env ,
                              
                              mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env ,
                              mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env , 
                              mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                              mod.abund.BIO_1000_12000_FFT512.Ht.env  ,
                              mod.abund.BIO_1000_12000_FFT512.AR.env  ,
                              
                              mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env,  
                              mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                              mod.abund.BIO_1000_22050_FFT512.Ht.env ,
                              mod.abund.BIO_1000_22050_FFT512.AR.env  ,
                              
                              mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                              mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env  ,
                              mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env  ,
                              
                              mod.abund.Ht.AR.env  )

for(i in resid_abund_bivar_env ){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
}

#div
# lmer
#single
mod.invsimp.NULL   <- lmer(sample_invsimp~1, data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.env  <- lmer(sample_invsimp~ADI_300_12000_75db + (1 | env), data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.env  <- lmer(sample_invsimp~AEI_1000_12000_50db + (1 | env), data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.env  <- lmer(sample_invsimp~ACI_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.env  <- lmer(sample_invsimp~BIO_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.BIO_1000_22050_FFT512.env  <- lmer(sample_invsimp~BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_invsimp~NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.Ht.env  <- lmer(sample_invsimp~Ht + (1 | env), data = mestra_birds)
mod.invsimp.AR.env  <- lmer(sample_invsimp~AR + (1 | env), data = mestra_birds)

# residual

resid_diver_single_env <- list(mod.invsimp.NULL   ,
                               mod.invsimp.ADI_300_12000_75db.env  ,
                               mod.invsimp.AEI_1000_12000_50db.env  ,
                               mod.invsimp.ACI_1000_12000_FFT512.env  ,
                               mod.invsimp.BIO_1000_12000_FFT512.env  ,
                               mod.invsimp.BIO_1000_22050_FFT512.env ,
                               mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env ,
                               mod.invsimp.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                               mod.invsimp.Ht.env  ,
                               mod.invsimp.AR.env)

for(i in resid_diver_single_env ){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
}

# bivariate
mod.invsimp.ADI_300_12000_75db.AEI_1000_12000_50db.env  <- lmer(sample_invsimp~ADI_300_12000_75db+AEI_1000_12000_50db + (1 | env), data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512.env  <- lmer(sample_invsimp~ADI_300_12000_75db+ACI_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512.env  <- lmer(sample_invsimp~ADI_300_12000_75db+BIO_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.BIO_1000_22050_FFT512.env  <- lmer(sample_invsimp~ADI_300_12000_75db+BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_invsimp~ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_invsimp~ADI_300_12000_75db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.Ht.env  <- lmer(sample_invsimp~ADI_300_12000_75db+Ht + (1 | env), data = mestra_birds)
mod.invsimp.ADI_300_12000_75db.AR.env  <- lmer(sample_invsimp~ADI_300_12000_75db+AR + (1 | env), data = mestra_birds)

mod.invsimp.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env  <- lmer(sample_invsimp~AEI_1000_12000_50db+ACI_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env  <- lmer(sample_invsimp~AEI_1000_12000_50db+BIO_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env  <- lmer(sample_invsimp~AEI_1000_12000_50db+BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_invsimp~AEI_1000_12000_50db+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_invsimp~AEI_1000_12000_50db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.Ht.env  <- lmer(sample_invsimp~AEI_1000_12000_50db+Ht + (1 | env), data = mestra_birds)
mod.invsimp.AEI_1000_12000_50db.AR.env  <- lmer(sample_invsimp~AEI_1000_12000_50db+AR + (1 | env), data = mestra_birds)

mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env  <- lmer(sample_invsimp~ACI_1000_12000_FFT512+BIO_1000_12000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env  <- lmer(sample_invsimp~ACI_1000_12000_FFT512+BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_invsimp~ACI_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_invsimp~ACI_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.Ht.env  <- lmer(sample_invsimp~ACI_1000_12000_FFT512+Ht + (1 | env), data = mestra_birds)
mod.invsimp.ACI_1000_12000_FFT512.AR.env  <- lmer(sample_invsimp~ACI_1000_12000_FFT512+AR + (1 | env), data = mestra_birds)

mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env  <- lmer(sample_invsimp~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_invsimp~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_invsimp~BIO_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.Ht.env  <- lmer(sample_invsimp~BIO_1000_12000_FFT512+Ht + (1 | env), data = mestra_birds)
mod.invsimp.BIO_1000_12000_FFT512.AR.env  <- lmer(sample_invsimp~BIO_1000_12000_FFT512+AR + (1 | env), data = mestra_birds)

mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  <- lmer(sample_invsimp~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_invsimp~BIO_1000_22050_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.BIO_1000_22050_FFT512.Ht.env  <- lmer(sample_invsimp~BIO_1000_22050_FFT512+Ht + (1 | env), data = mestra_birds)
mod.invsimp.BIO_1000_22050_FFT512.AR.env  <- lmer(sample_invsimp~BIO_1000_22050_FFT512+AR + (1 | env), data = mestra_birds)

mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  <- lmer(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 + (1 | env), data = mestra_birds)
mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env  <- lmer(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+Ht + (1 | env), data = mestra_birds)
mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env  <- lmer(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+AR + (1 | env), data = mestra_birds)

mod.invsimp.Ht.AR.env  <- lmer(sample_invsimp~Ht+AR + (1 | env), data = mestra_birds)

# residual

resid_diver_bivar_env <- list(mod.invsimp.ADI_300_12000_75db.AEI_1000_12000_50db.env  ,
                              mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512.env  ,
                              mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512.env  ,
                              mod.invsimp.ADI_300_12000_75db.BIO_1000_22050_FFT512.env ,
                              mod.invsimp.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                              mod.invsimp.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                              mod.invsimp.ADI_300_12000_75db.Ht.env  ,
                              mod.invsimp.ADI_300_12000_75db.AR.env  ,
                              
                              mod.invsimp.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env  ,
                              mod.invsimp.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env ,
                              mod.invsimp.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env  ,
                              mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env ,
                              mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                              mod.invsimp.AEI_1000_12000_50db.Ht.env  ,
                              mod.invsimp.AEI_1000_12000_50db.AR.env ,
                              
                              mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env  ,
                              mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env ,
                              mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                              mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                              mod.invsimp.ACI_1000_12000_FFT512.Ht.env ,
                              mod.invsimp.ACI_1000_12000_FFT512.AR.env  ,
                              
                              mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env ,
                              mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                              mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                              mod.invsimp.BIO_1000_12000_FFT512.Ht.env  ,
                              mod.invsimp.BIO_1000_12000_FFT512.AR.env  ,
                              
                              mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env  ,
                              mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env ,
                              mod.invsimp.BIO_1000_22050_FFT512.Ht.env  ,
                              mod.invsimp.BIO_1000_22050_FFT512.AR.env ,
                              
                              mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env  ,
                              mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env  ,
                              mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env  ,
                              
                              mod.invsimp.Ht.AR.env )


for(i in resid_diver_bivar_env ){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
}



























# -----------------------------------------------------------------------------------------------
## grid images

#single models
setwd("C:\\Users\\Lucas.Gaspar\\Documents\\backpup\\ANALISYS_2020_06_d01\\00_scripts_gaspar_master\\after_meet\\aves\\00_after_quali\\by_env\\2023\\single\\2023")

dir()

install.packages("EBImage")

img = readPNG("sample_abund ~ 1.png")
img2 = readPNG("sample_invsimp ~ NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.png"  )

images <- list.files()

# load sample image
img <- EBImage::readImage(images)

# downsample to reduce memory consumption and for faster processing
img <- resize(img, 192)

# build the layout matrix with additional separating cells
nx <- 3 # number of images in a row
ny <- 4 # number of images in a column

cols <- 2*nx-1
rows <- 2*ny-1
m <- matrix(0, cols, rows)
m[2*(1:nx)-1, 2*(1:ny)-1] <- 1:(nx*ny)
m <- t(m)

# relative spacing
pad <- .1

w <- rep(1, cols)
w[!(1:cols)%%2] <- pad
h <- rep(1, rows)
h[!(1:rows)%%2] <- pad * dim(img)[1L]/dim(img)[2L]

layout(m, widths = w, heights = h)

layout.show(nx*ny)

for(j in images){
  name <- as.character(j)
  
for (i in 1:(nx*ny)) {
  png(filename=paste(name, ".png", sep = ""))
  display(img, method="raster", all=F)
  dev.off()
}}

## construct sample image stack
img_stack <- combine(replicate(nx*ny, img, simplify=FALSE))




       


### models analysis -----

# milton's function
r2.extract<-function(model.list)
{ 
  model.list.df<- data.frame(model.list)
  
  model.list.df$r2<-NULL
  for (i in 1:nrow(model.list.df))
  {
    model.aux<-row.names(model.list.df)
    model.aux.obj<-	eval(parse(text=paste(model.aux[i],sep="")))
    r2.aux<-eval(parse(text=paste("rsq(",model.aux[i],")", sep="")))
    model.list.df$r2[i] <- r2.aux
  }
  return(model.list.df)
}

# --------------- 
#residuos -----------------------------------------


rich_models <- list(mod.riq.NULL , 
                          mod.riq.ADI_300_12000_75db , mod.riq.AEI_1000_12000_50db , mod.riq.ACI_1000_12000_FFT512,mod.riq.BIO_1000_12000_FFT512 , 
                         mod.riq.BIO_1000_22050_FFT512 , mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 , mod.riq.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , 
                         mod.riq.Ht,  mod.riq.AR , 
                         
                          mod.riq.ADI_300_12000_75db.AEI_1000_12000_50db , mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512 , mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512 , 
                         mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512 , mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 , 
                         mod.riq.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , mod.riq.ADI_300_12000_75db.Ht , mod.riq.ADI_300_12000_75db.AR ,
                         mod.riq.AEI_1000_12000_50db.ACI_1000_12000_FFT512 , mod.riq.AEI_1000_12000_50db.BIO_1000_12000_FFT512, mod.riq.AEI_1000_12000_50db.BIO_1000_22050_FFT512, 
                         mod.riq.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 , mod.riq.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , 
                         mod.riq.AEI_1000_12000_50db.Ht, mod.riq.AEI_1000_12000_50db.AR, mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512, mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512,  
                         mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                         mod.riq.ACI_1000_12000_FFT512.Ht, mod.riq.ACI_1000_12000_FFT512.AR, mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512, mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, 
                         mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, mod.riq.BIO_1000_12000_FFT512.Ht, mod.riq.BIO_1000_12000_FFT512.AR , 
                         mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                         mod.riq.BIO_1000_22050_FFT512.Ht, mod.riq.BIO_1000_22050_FFT512.AR,mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                         mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht, mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR, mod.riq.Ht.AR,
                         
                         mod.riq.ADI_300_12000_75db.env, mod.riq.AEI_1000_12000_50db.env, mod.riq.ACI_1000_12000_FFT512.env, mod.riq.BIO_1000_12000_FFT512.env,
                         mod.riq.BIO_1000_22050_FFT512.env, mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.riq.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                         mod.riq.Ht.env, mod.riq.AR.env,  mod.riq.ADI_300_12000_75db.AEI_1000_12000_50db.env, mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512.env, 
                         mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512.env,  mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512.env, mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, 
                         mod.riq.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, mod.riq.ADI_300_12000_75db.Ht.env, mod.riq.ADI_300_12000_75db.AR.env, 
                         mod.riq.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env , mod.riq.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env, mod.riq.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env, 
                         mod.riq.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.riq.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                         mod.riq.AEI_1000_12000_50db.Ht.env, mod.riq.AEI_1000_12000_50db.AR.env, mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env, mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env,
                         mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, 
                         mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, mod.riq.ACI_1000_12000_FFT512.Ht.env, mod.riq.ACI_1000_12000_FFT512.AR.env, 
                         mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env, mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                         mod.riq.BIO_1000_12000_FFT512.Ht.env, mod.riq.BIO_1000_12000_FFT512.AR.env, mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                         mod.riq.BIO_1000_22050_FFT512.Ht.env , mod.riq.BIO_1000_22050_FFT512.AR.env, mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env,
                         mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env, mod.riq.Ht.AR.env)

setwd("C:\\Users\\Lucas.Gaspar\\Documents\\backpup\\ANALISYS_2020_06_d01\\00_scripts_gaspar_master\\after_meet\\aves\\00_after_quali\\by_env\\2023")

for(i in rich_models){
  name <- as.character(i$call[2])
  png(filename=paste(name, ".png", sep = ""))
  plot(density(resid(i, type="deviance")), col = "red", main = i$call[2])
  dev.off()
} 


# richness models results
results_birds_AICc_riq <- bbmle::ICtab(mod.riq.NULL, 
                                       mod.riq.ADI_300_12000_75db, mod.riq.AEI_1000_12000_50db, mod.riq.ACI_1000_12000_FFT512,mod.riq.BIO_1000_12000_FFT512, 
                                       mod.riq.BIO_1000_22050_FFT512, mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.riq.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                       mod.riq.Ht,  mod.riq.AR, 
                                       
                                       mod.riq.ADI_300_12000_75db.AEI_1000_12000_50db, mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512, mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512, 
                                       mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512, mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, 
                                       mod.riq.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, mod.riq.ADI_300_12000_75db.Ht, mod.riq.ADI_300_12000_75db.AR,
                                       mod.riq.AEI_1000_12000_50db.ACI_1000_12000_FFT512 , mod.riq.AEI_1000_12000_50db.BIO_1000_12000_FFT512, mod.riq.AEI_1000_12000_50db.BIO_1000_22050_FFT512, 
                                       mod.riq.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.riq.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                       mod.riq.AEI_1000_12000_50db.Ht, mod.riq.AEI_1000_12000_50db.AR, mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512, mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512,  
                                       mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                       mod.riq.ACI_1000_12000_FFT512.Ht, mod.riq.ACI_1000_12000_FFT512.AR, mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512, mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, 
                                       mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, mod.riq.BIO_1000_12000_FFT512.Ht, mod.riq.BIO_1000_12000_FFT512.AR , 
                                       mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                       mod.riq.BIO_1000_22050_FFT512.Ht, mod.riq.BIO_1000_22050_FFT512.AR,mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                       mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht, mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR, mod.riq.Ht.AR,
                                       
                                       mod.riq.ADI_300_12000_75db.env, mod.riq.AEI_1000_12000_50db.env, mod.riq.ACI_1000_12000_FFT512.env, mod.riq.BIO_1000_12000_FFT512.env,
                                       mod.riq.BIO_1000_22050_FFT512.env, mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.riq.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                                       mod.riq.Ht.env, mod.riq.AR.env,  mod.riq.ADI_300_12000_75db.AEI_1000_12000_50db.env, mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512.env, 
                                       mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512.env,  mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512.env, mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, 
                                       mod.riq.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, mod.riq.ADI_300_12000_75db.Ht.env, mod.riq.ADI_300_12000_75db.AR.env, 
                                       mod.riq.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env , mod.riq.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env, mod.riq.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env, 
                                       mod.riq.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.riq.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                                       mod.riq.AEI_1000_12000_50db.Ht.env, mod.riq.AEI_1000_12000_50db.AR.env, mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env, mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env,
                                       mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, 
                                       mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, mod.riq.ACI_1000_12000_FFT512.Ht.env, mod.riq.ACI_1000_12000_FFT512.AR.env, 
                                       mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env, mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                                       mod.riq.BIO_1000_12000_FFT512.Ht.env, mod.riq.BIO_1000_12000_FFT512.AR.env, mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                                       mod.riq.BIO_1000_22050_FFT512.Ht.env , mod.riq.BIO_1000_22050_FFT512.AR.env, mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env,
                                       mod.riq.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env, mod.riq.Ht.AR.env,
                                       type= c("AICc"), nobs=nrow(mestra_birds), weights=T, base=F, delta=T)  

results_birds_AICc_riq.r2 <- as.data.frame(r2.extract(results_birds_AICc_riq))
results_birds_AICc_riq.r2
View(results_birds_AICc_riq.r2)

# abund models results
results_birds_AICc_abund <- bbmle::ICtab(mod.abund.NULL, 
                                         mod.abund.ADI_300_12000_75db, mod.abund.AEI_1000_12000_50db, mod.abund.ACI_1000_12000_FFT512,mod.abund.BIO_1000_12000_FFT512, 
                                         mod.abund.BIO_1000_22050_FFT512, mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.abund.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                         mod.abund.Ht,  mod.abund.AR, 
                                         
                                         mod.abund.ADI_300_12000_75db.AEI_1000_12000_50db, mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512, mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512, 
                                         mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512, mod.abund.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, 
                                         mod.abund.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, mod.abund.ADI_300_12000_75db.Ht, mod.abund.ADI_300_12000_75db.AR,
                                         mod.abund.AEI_1000_12000_50db.ACI_1000_12000_FFT512 , mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512, mod.abund.AEI_1000_12000_50db.BIO_1000_22050_FFT512, 
                                         mod.abund.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.abund.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                         mod.abund.AEI_1000_12000_50db.Ht, mod.abund.AEI_1000_12000_50db.AR, mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512, mod.abund.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512,  
                                         mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                         mod.abund.ACI_1000_12000_FFT512.Ht, mod.abund.ACI_1000_12000_FFT512.AR, mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512, mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, 
                                         mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, mod.abund.BIO_1000_12000_FFT512.Ht, mod.abund.BIO_1000_12000_FFT512.AR , 
                                         mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                         mod.abund.BIO_1000_22050_FFT512.Ht, mod.abund.BIO_1000_22050_FFT512.AR,mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                         mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht, mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR, mod.abund.Ht.AR,
                                         
                                         mod.abund.ADI_300_12000_75db.env, mod.abund.AEI_1000_12000_50db.env, mod.abund.ACI_1000_12000_FFT512.env, mod.abund.BIO_1000_12000_FFT512.env,
                                         mod.abund.BIO_1000_22050_FFT512.env, mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.abund.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                                         mod.abund.Ht.env, mod.abund.AR.env,  mod.abund.ADI_300_12000_75db.AEI_1000_12000_50db.env, mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512.env, 
                                         mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512.env,  mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512.env, mod.abund.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, 
                                         mod.abund.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, mod.abund.ADI_300_12000_75db.Ht.env, mod.abund.ADI_300_12000_75db.AR.env, 
                                         mod.abund.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env , mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env, mod.abund.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env, 
                                         mod.abund.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.abund.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                                         mod.abund.AEI_1000_12000_50db.Ht.env, mod.abund.AEI_1000_12000_50db.AR.env, mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env, mod.abund.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env,
                                         mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, 
                                         mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, mod.abund.ACI_1000_12000_FFT512.Ht.env, mod.abund.ACI_1000_12000_FFT512.AR.env, 
                                         mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env, mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                                         mod.abund.BIO_1000_12000_FFT512.Ht.env, mod.abund.BIO_1000_12000_FFT512.AR.env, mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                                         mod.abund.BIO_1000_22050_FFT512.Ht.env , mod.abund.BIO_1000_22050_FFT512.AR.env, mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env,
                                         mod.abund.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env, mod.abund.Ht.AR.env,
                                         type= c("AICc"), nobs=nrow(mestra_birds), weights=T, base=F, delta=T)  

results_birds_AICc_abund.r2 <- as.data.frame(r2.extract(results_birds_AICc_abund))
results_birds_AICc_abund.r2
View(results_birds_AICc_abund.r2)

#  diveristy models results
results_birds_AICc_invsimp <- bbmle::ICtab(mod.invsimp.NULL, 
                                           mod.invsimp.ADI_300_12000_75db, mod.invsimp.AEI_1000_12000_50db, mod.invsimp.ACI_1000_12000_FFT512,mod.invsimp.BIO_1000_12000_FFT512, 
                                           mod.invsimp.BIO_1000_22050_FFT512, mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.invsimp.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                           mod.invsimp.Ht,  mod.invsimp.AR, 
                                           
                                           mod.invsimp.ADI_300_12000_75db.AEI_1000_12000_50db, mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512, mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512, 
                                           mod.invsimp.ADI_300_12000_75db.BIO_1000_22050_FFT512, mod.invsimp.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, 
                                           mod.invsimp.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, mod.invsimp.ADI_300_12000_75db.Ht, mod.invsimp.ADI_300_12000_75db.AR,
                                           mod.invsimp.AEI_1000_12000_50db.ACI_1000_12000_FFT512 , mod.invsimp.AEI_1000_12000_50db.BIO_1000_12000_FFT512, mod.invsimp.AEI_1000_12000_50db.BIO_1000_22050_FFT512, 
                                           mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                           mod.invsimp.AEI_1000_12000_50db.Ht, mod.invsimp.AEI_1000_12000_50db.AR, mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512, mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512,  
                                           mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                           mod.invsimp.ACI_1000_12000_FFT512.Ht, mod.invsimp.ACI_1000_12000_FFT512.AR, mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512, mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, 
                                           mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, mod.invsimp.BIO_1000_12000_FFT512.Ht, mod.invsimp.BIO_1000_12000_FFT512.AR , 
                                           mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512, mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                           mod.invsimp.BIO_1000_22050_FFT512.Ht, mod.invsimp.BIO_1000_22050_FFT512.AR,mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                           mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht, mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR, mod.invsimp.Ht.AR,
                                           
                                           mod.invsimp.ADI_300_12000_75db.env, mod.invsimp.AEI_1000_12000_50db.env, mod.invsimp.ACI_1000_12000_FFT512.env, mod.invsimp.BIO_1000_12000_FFT512.env,
                                           mod.invsimp.BIO_1000_22050_FFT512.env, mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.invsimp.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                                           mod.invsimp.Ht.env, mod.invsimp.AR.env,  mod.invsimp.ADI_300_12000_75db.AEI_1000_12000_50db.env, mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512.env, 
                                           mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512.env,  mod.invsimp.ADI_300_12000_75db.BIO_1000_22050_FFT512.env, mod.invsimp.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, 
                                           mod.invsimp.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, mod.invsimp.ADI_300_12000_75db.Ht.env, mod.invsimp.ADI_300_12000_75db.AR.env, 
                                           mod.invsimp.AEI_1000_12000_50db.ACI_1000_12000_FFT512.env , mod.invsimp.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env, mod.invsimp.AEI_1000_12000_50db.BIO_1000_22050_FFT512.env, 
                                           mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                                           mod.invsimp.AEI_1000_12000_50db.Ht.env, mod.invsimp.AEI_1000_12000_50db.AR.env, mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env, mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512.env,
                                           mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, 
                                           mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, mod.invsimp.ACI_1000_12000_FFT512.Ht.env, mod.invsimp.ACI_1000_12000_FFT512.AR.env, 
                                           mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512.env, mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                                           mod.invsimp.BIO_1000_12000_FFT512.Ht.env, mod.invsimp.BIO_1000_12000_FFT512.AR.env, mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.env, mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, 
                                           mod.invsimp.BIO_1000_22050_FFT512.Ht.env , mod.invsimp.BIO_1000_22050_FFT512.AR.env, mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env, mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.Ht.env,
                                           mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.AR.env, mod.invsimp.Ht.AR.env,
                                           type= c("AICc"), nobs=nrow(mestra_birds), weights=T, base=F, delta=T)  

results_birds_AICc_invsimp.r2 <- as.data.frame(r2.extract(results_birds_AICc_invsimp))
results_birds_AICc_invsimp.r2
View(results_birds_AICc_invsimp.r2)


# save.image("all_models_script02.rda")

# compilation
results_birds_AICc_riq.r2.select <- results_birds_AICc_riq.r2[1:5,]
results_birds_AICc_abund.r2.select <- results_birds_AICc_abund.r2[1:5,]
results_birds_AICc_invsimp.r2.select <- results_birds_AICc_invsimp.r2[1:5,]

results_birds.multiple.env <- rbind(results_birds_AICc_riq.r2.select,  results_birds_AICc_abund.r2.select , results_birds_AICc_invsimp.r2.select )
results_birds.multiple.env

results_birds.multiple.env$mod <- row.names(results_birds.multiple.env)
results_birds.multiple.env$mod <- factor(results_birds.multiple.env$mod, levels=as.character(results_birds.multiple.env$mod))

# plot for R² visualization

setwd("C:\\Users\\Lucas.Gaspar\\Documents\\backpup\\ANALISYS_2020_06_d01\\00_scripts_gaspar_master\\after_meet\\aves\\00_after_quali\\by_env\\2023\\MISTOS")
# plot with results
colnames(results_birds.multiple.env)
results_birds.multiple.env
results_birds.multiple.env$indice <- c(rep("richness",5), rep("number of tags",5),rep("tag diversity",5))

results_birds.multiple.env$mod <- c("rich~BIO+ACI+env", "rich~BIO+AR+env", "rich~ADI+BIO+env", "rich~BIO+ACI", "rich~BIO+NDSI01+env",
                                    "abund~BIO+ACI+env", "abund~BIO+AR+env", "abund~BIO+ADI+env", "abund~BIO+AEI+env", "abund~BIO+NDSI01",
                                    "div~BIO+ACI+env", "div~BIO+NDSI01+env","div~BIO+ACI", "div~BIO+NDSI01","div~BIO+AR+env")

results_birds.multiple.env$mod <- factor(results_birds.multiple.env$mod, levels=rev(results_birds.multiple.env$mod))



fp <- ggplot(data=results_birds.multiple.env, aes(x=mod, y=r2)) + #ymin=lower, ymax=upper
  geom_point(aes(col=indice)) + 
  # geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("lmer") + ylab("R²") +
  theme_bw()  # use a white background
print(fp)
ggsave("plot_results_models_lands_birds_without_env.png")

dd <- as.data.frame(results_birds.multiple.env)
View(dd)
View(results_birds.multiple.env)
dd <- apply(dd, 1, as.character)

# save
write.table(dd, "00_model_selection_AIC_birds_by_landscape_multiple_indices_without_enviroment_2023_04_d31.csv")


results_birds.multiple.env <- rbind(results_birds_AICc_riq.r2,  results_birds_AICc_abund.r2, results_birds_AICc_invsimp.r2 )
results_birds.multiple.env
results_birds.multiple.env$mod <- row.names(results_birds.multiple.env)
results_birds.multiple.env$mod <- factor(results_birds.multiple.env$mod, levels=as.character(results_birds.multiple.env$mod))
results_birds.multiple.env$indice <- c(rep("richness",87), rep("number of tags",87),rep("tag diversity",87))
write.csv(as.list(results_birds.multiple.env), "00_model_selection_AIC_birds_by_landscape_multiple_indices_without_enviroment_2021_03_d01_all_data.csv")

colnames(mestra_birds)


# directory ----------------------------------------------------
setwd("C:\\Users\\Lucas.Gaspar\\Documents\\backpup\\ANALISYS_2020_06_d01\\00_scripts_gaspar_master\\after_meet\\aves\\00_after_quali\\by_env\\2023\\MISTOS")
dir()
load("all_models_script02.rda")

## birds models visualization
dir()
plot.dir <- "./plots/models"

# superfie plots 
####### ---------- plots

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(broom)) install.packages("broom")
if(!require(GGally)) install.packages("GGally")
if(!require(ggpmisc)) install.packages("ggpmisc")
if(!require(metR)) install.packages("metR")
if(!require(viridis)) install.packages("viridis")
if(!require(scales)) install.packages("scale")
if(!require(devtools)) install.packages("devtools")
if(!require(cowplot)) install.packages("cowplot")

## bests models
#rich
mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env
# mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 PLOT  + envornment
m<-predict(mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 )
x <-mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512$data$ACI_1000_12000_FFT512
z <- mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env$data$env
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","ACI_1000_12000_FFT512")

psi<-data.frame(predict(mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$ACI_1000_12000_FFT512<-fs$ACI_1000_12000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

na<-1:(10000-length(z))
na[na>0]<-NA
zna<-c(z,na)

show_col(viridis_pal(option = "C")(12))

plot_rich_01 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ACI_1000_12000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna, colour = zna),shape=16,size=2,alpha=0.3) +
  scale_color_manual(values=c("#F58C46FF", "#D5546EFF","#8707A6FF")) +
  xlab("BIO") +
  ylab("ACI") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Species \nrichness")
plot_rich_01
png("./plots/models/01_mod.riq.ACI.BIO.env.png", width=900, height=900)
plot_rich_01
dev.off()

# mod.riq.BIO_1000_12000_FFT512.AR + envornment
m<-predict(mod.riq.BIO_1000_12000_FFT512.AR )
x <-mod.riq.BIO_1000_12000_FFT512.AR$data$BIO_1000_12000_FFT512 
y <-mod.riq.BIO_1000_12000_FFT512.AR$data$AR
z <- mod.riq.BIO_1000_12000_FFT512.AR.env$data$env
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","AR")

psi<-data.frame(predict(mod.riq.BIO_1000_12000_FFT512.AR , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$AR<-fs$AR

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

na<-1:(10000-length(z))
na[na>0]<-NA
zna<-c(z,na)

plot.rich_02 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=AR , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna, colour = zna),shape=16,size=2,alpha=0.3) +
  scale_color_manual(values=c("#F58C46FF", "#D5546EFF","#8707A6FF")) +
  xlab("BIO") +
  ylab("AR") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Species \nrichness")
plot.rich_02
png("./plots/models/02_mod.riq.BIO.AR.env.png", width=900, height=900)
plot.rich_02
dev.off()

# mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512+ envornment
m<-predict(mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512 )
x <-mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$ADI_300_12000_75db
z <- mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512.env$data$env
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","ADI_300_12000_75db")

psi<-data.frame(predict(mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$ADI_300_12000_75db<-fs$ADI_300_12000_75db

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

na<-1:(10000-length(z))
na[na>0]<-NA
zna<-c(z,na)

plot.rich_03 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ADI_300_12000_75db , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna, colour = zna),shape=16,size=2,alpha=0.3) +
  scale_color_manual(values=c("#F58C46FF", "#D5546EFF","#8707A6FF")) +
  xlab("BIO") +
  ylab("ADI") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Species \nrichness")
plot.rich_03
png("./plots/models/03_mod.riq.BIO.ADI.env.png", width=900, height=900)
plot.rich_03
dev.off()

# mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 PLOT
m<-predict(mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 )
x <-mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512$data$ACI_1000_12000_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","ACI_1000_12000_FFT512")

psi<-data.frame(predict(mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$ACI_1000_12000_FFT512<-fs$ACI_1000_12000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.rich_04 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ACI_1000_12000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO") +
  ylab("ACI") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Species \nrichness")
plot.rich_04
png("./plots/models/04_mod.riq.ACI.BIO.png", width=900, height=900)
plot.rich_04
dev.off()

# mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+  ENV
m<-predict(mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512)
x <-mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
z <- mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env$data$env
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512")

psi<-data.frame(predict(mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512<-fs$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

na<-1:(10000-length(z))
na[na>0]<-NA
zna<-c(z,na)

plot.rich_05 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna, colour = zna),shape=16,size=2,alpha=0.3) +
  scale_color_manual(values=c("#F58C46FF", "#D5546EFF","#8707A6FF")) +
  xlab("BIO") +
  ylab("NDSI-01") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Species \nrichness")
plot.rich_05
png("./plots/models/05_mod.riq.BIO.NDSI_b1000_12000_a1000_22050.env.png", width=900, height=900)
plot.rich_05
dev.off()

## abund ------------

# mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 PLOT  + envornment
m<-predict(mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 )
x <-mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512$data$ACI_1000_12000_FFT512
z <- mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env$data$env
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","ACI_1000_12000_FFT512")

psi<-data.frame(predict(mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$ACI_1000_12000_FFT512<-fs$ACI_1000_12000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

na<-1:(10000-length(z))
na[na>0]<-NA
zna<-c(z,na)

plot_abund_01 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ACI_1000_12000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna, colour = zna),shape=16,size=2,alpha=0.3) +
  scale_color_manual(values=c("#F58C46FF", "#D5546EFF","#8707A6FF")) +
  xlab("BIO") +
  ylab("ACI") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Number \nof tags")
plot_abund_01
png("./plots/models/01_mod.abund.ACI.BIO.env.png", width=900, height=900)
plot_abund_01
dev.off()

# mod.abund.BIO_1000_12000_FFT512.AR + envornment
m<-predict(mod.abund.BIO_1000_12000_FFT512.AR )
x <-mod.abund.BIO_1000_12000_FFT512.AR$data$BIO_1000_12000_FFT512 
y <-mod.abund.BIO_1000_12000_FFT512.AR$data$AR
z <- mod.abund.BIO_1000_12000_FFT512.AR.env$data$env
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","AR")

psi<-data.frame(predict(mod.abund.BIO_1000_12000_FFT512.AR , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$AR<-fs$AR

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

na<-1:(10000-length(z))
na[na>0]<-NA
zna<-c(z,na)

plot.abund_02 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=AR , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna, colour = zna),shape=16,size=2,alpha=0.3) +
  scale_color_manual(values=c("#F58C46FF", "#D5546EFF","#8707A6FF")) +
  xlab("BIO") +
  ylab("AR") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Number \nof tags")
plot.abund_02
png("./plots/models/02_mod.abund.BIO.AR.env.png", width=900, height=900)
plot.abund_02
dev.off()

# mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512+ envornment
m<-predict(mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512 )
x <-mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$ADI_300_12000_75db
z <- mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512.env$data$env
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","ADI_300_12000_75db")

psi<-data.frame(predict(mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$ADI_300_12000_75db<-fs$ADI_300_12000_75db

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

na<-1:(10000-length(z))
na[na>0]<-NA
zna<-c(z,na)

plot.abund_03 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ADI_300_12000_75db , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna, colour = zna),shape=16,size=2,alpha=0.3) +
  scale_color_manual(values=c("#F58C46FF", "#D5546EFF","#8707A6FF")) +
  xlab("BIO") +
  ylab("ADI") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Number \nof tags")
plot.abund_03
png("./plots/models/03_mod.abund.BIO.ADI.env.png", width=900, height=900)
plot.abund_03
dev.off()

# mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512 envornment
m<-predict(mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512)
x <-mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512$data$AEI_1000_12000_50db
z <- mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512.env$data$env
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","AEI_1000_12000_50db")

psi<-data.frame(predict(mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$AEI_1000_12000_50db<-fs$AEI_1000_12000_50db

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

na<-1:(10000-length(z))
na[na>0]<-NA
zna<-c(z,na)

plot.abund_04 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=AEI_1000_12000_50db, z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna, colour = zna),shape=16,size=2,alpha=0.3) +
  scale_color_manual(values=c("#F58C46FF", "#D5546EFF","#8707A6FF")) +
  xlab("BIO") +
  ylab("AEI") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Number \nof tags")
plot.abund_04
png("./plots/models/04_mod.abund.BIO.AEI.env.png", width=900, height=900)
plot.abund_04
dev.off()

# mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512envornment
m<-predict(mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512)
x <-mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
z <- mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env$data$env
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512")

psi<-data.frame(predict(mod.abund.AEI_1000_12000_50db.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512<-fs$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

na<-1:(10000-length(z))
na[na>0]<-NA
zna<-c(z,na)

plot.abund_05 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna, colour = zna),shape=16,size=2,alpha=0.3) +
  scale_color_manual(values=c("#F58C46FF", "#D5546EFF","#8707A6FF")) +
  xlab("BIO") +
  ylab("NDSI-01") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Number \nof tags")
plot.abund_05
png("./plots/models/05_mod.abund.BIO.NDSI1.env.png", width=900, height=900)
plot.abund_05
dev.off()

## diversity ------------

# mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 PLOT  + envornment
m<-predict(mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 )
x <-mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512$data$ACI_1000_12000_FFT512
z <- mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512.env$data$env
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","ACI_1000_12000_FFT512")

psi<-data.frame(predict(mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$ACI_1000_12000_FFT512<-fs$ACI_1000_12000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

na<-1:(10000-length(z))
na[na>0]<-NA
zna<-c(z,na)

plot_invsimp_01 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ACI_1000_12000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna, colour = zna),shape=16,size=2,alpha=0.3) +
  #scale_color_manual(values=c("#F58C46FF", "#D5546EFF","#8707A6FF")) +
  scale_color_manual(values=c("pasture" = "#F58C46FF", "swamp" = "#D5546EFF", "forest" = "#8707A6FF")) +
  xlab("BIO") +
  ylab("ACI") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Tags \ndiversity")
plot_invsimp_01
png("./plots/models/01_mod.invsimp.ACI.BIO.env.png", width=900, height=900)
plot_invsimp_01
dev.off()

#### legend !
plot_div_legend <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ACI_1000_12000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna, colour = zna),shape=16,size=2,alpha=0.3) +
  scale_color_manual(values=c("pasture" = "#F58C46FF", "forest" = "#D5546EFF", "swamp" = "#8707A6FF")) +
  xlab("x") +
  ylab("x") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(30,80),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(10.7, "cm"),
        legend.key.width = unit(10.7,"cm")) +
  labs(fill = "Tags \ndiversity")
plot_div_legend
png("./plots/models/lenged.invsimp.png", width=900, height=900)
plot_div_legend
dev.off()

# mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512envornment
m<-predict(mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512)
x <-mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
z <- mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.env$data$env
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512")

psi<-data.frame(predict(mod.invsimp.AEI_1000_12000_50db.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512<-fs$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

na<-1:(10000-length(z))
na[na>0]<-NA
zna<-c(z,na)

plot.invsimp_02 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna, colour = zna),shape=16,size=2,alpha=0.3) +
  scale_color_manual(values=c("#F58C46FF", "#D5546EFF","#8707A6FF")) +
  xlab("BIO") +
  ylab("NDSI-01") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Tags \ndiversity")
plot.invsimp_02
png("./plots/models/02_mod.invsimp.BIO.NDSI1.env.png", width=900, height=900)
plot.invsimp_02
dev.off()

# mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 PLOT
m<-predict(mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 )
x <-mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512$data$ACI_1000_12000_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","ACI_1000_12000_FFT512")

psi<-data.frame(predict(mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$ACI_1000_12000_FFT512<-fs$ACI_1000_12000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_03 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ACI_1000_12000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO") +
  ylab("ACI") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Tags \ndiversity")
plot.invsimp_03
png("./plots/models/03_mod.invsimp.ACI.BIO.png", width=900, height=900)
plot.invsimp_03
dev.off()

# mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 PLOT
m<-predict(mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 )
x <-mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512")

psi<-data.frame(predict(mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512<-fs$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_04 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO") +
  ylab("NDSI-01") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Tags \ndiversity")
plot.invsimp_04
png("./plots/models/04_mod.invsimp.BIO.NDSI1.png", width=900, height=900)
plot.invsimp_04
dev.off()

# mod.invsimp.BIO_1000_12000_FFT512.AR + envornment
m<-predict(mod.invsimp.BIO_1000_12000_FFT512.AR )
x <-mod.invsimp.BIO_1000_12000_FFT512.AR$data$BIO_1000_12000_FFT512 
y <-mod.invsimp.BIO_1000_12000_FFT512.AR$data$AR
z <- mod.invsimp.BIO_1000_12000_FFT512.AR.env$data$env
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","AR")

psi<-data.frame(predict(mod.invsimp.BIO_1000_12000_FFT512.AR , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$AR<-fs$AR

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

na<-1:(10000-length(z))
na[na>0]<-NA
zna<-c(z,na)

plot.invsimp_05 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=AR , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna, colour = zna),shape=16,size=2,alpha=0.3) +
  scale_color_manual(values=c("#F58C46FF", "#D5546EFF","#8707A6FF")) +
  xlab("BIO") +
  ylab("AR") +
  theme(axis.text=element_text(size=32,colour="black"), 
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5)) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=1, colour="black")) +
  theme(legend.key = element_blank(), 
        legend.title=element_text(size=32), 
        legend.text=element_text(size=30),
        legend.position = c(0.1,5),
        legend.background = element_rect(fill="grey95"),
        legend.key.size = unit(1.7, "cm"),
        legend.key.width = unit(1.7,"cm")) +
  labs(fill = "Tags \ndiversity")
plot.invsimp_05
png("./plots/models/05_mod.invsimp.BIO.AR.env.png", width=900, height=900)
plot.invsimp_05
dev.off()
save.image("all_models_script02.rda")

#save.image("plots_models.rda")

# ------------------------------------------------ Fixed the labels

























