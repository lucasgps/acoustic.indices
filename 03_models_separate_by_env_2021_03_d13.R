
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
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(broom)) install.packages("broom")
if(!require(GGally)) install.packages("GGally")
if(!require(ggpmisc)) install.packages("ggpmisc")
if(!require(metR)) install.packages("metR")
if(!require(viridis)) install.packages("viridis")
if(!require(scales)) install.packages("scale")
if(!require(devtools)) install.packages("devtools")
if(!require(cowplot)) install.packages("cowplot")



# directory
setwd("/home/gasparzinho/Documents/backpup/ANALISYS_2020_06_d01/00_scripts_gaspar_master/after_meet/aves/00_after_quali/by_env")
dir()

## plot save directory
dir()
plot.dir.forest <- "./plots/models_forest_only"
plot.dir.pasture <- "./plots/models_pasture_only"
plot.dir.swamp <- "./plots/models_swamp_only"

# load("all_models_results_script_03.rda")

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

### forest ------------------------------------------------------------------------------------
### forest ------------------------------------------------------------------------------------
### forest ------------------------------------------------------------------------------------
### forest ------------------------------------------------------------------------------------

mestra_birds_variables <- subset(mestra_birds_variables, env == "forest" )

# VIFs 

# to forest richness --------------------------------------------------------------------------
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
                AR, data = mestra_birds_variables) 
riq_vif0 <- as.data.frame(car::vif(mod.riq))
# View(riq_vif0)

mod.riq <- lm(sample_rich~#ADI_300_12000_50db+
                #ADI_300_12000_75db+
                #7 ADI_300_22050_50db+ 
                #ADI_300_22050_75db+ 
                #ADI_1000_12000_50db+ 
                ADI_1000_12000_75db+
                #ADI_1000_22050_50db+ 
                #3 ADI_1000_22050_75db+ 
                #AEI_300_12000_50db+ 
                #AEI_300_12000_75db+ 
                #5 AEI_300_22050_50db+ 
                #4 AEI_300_22050_75db+ 
                AEI_1000_12000_50db+ 
                #6 AEI_1000_12000_75db+ 
                #AEI_1000_22050_50db+ 
                #AEI_1000_22050_75db+ 
                #ACI_300_12000_FFT512+ 
                #1 ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                #ACI_1000_22050_FFT512+
                #BIO_300_12000_FFT512+ 
                #2 BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                #NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                #NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                #H_ws512+ 
                Ht,
              data = mestra_birds_variables) 
riq_vif1 <- as.data.frame(car::vif(mod.riq))
# View(riq_vif1)

mod.riq.forest <- lm(sample_rich~
                          ADI_1000_12000_75db+
                          AEI_1000_12000_50db+ 
                          ACI_1000_12000_FFT512+ 
                          BIO_1000_12000_FFT512+ 
                          BIO_1000_22050_FFT512+ 
                          NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                          Ht,
              data = mestra_birds_variables) 
mod.riq.forest

# to forest abundance ----------------------------------------------------------------------------
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
                AR, data = mestra_birds_variables) 
abund_vif0 <- as.data.frame(car::vif(mod.abund))
# View(riq_vif0)

mod.abund <- lm(sample_abund~#21 ADI_300_12000_50db+
                  #10 ADI_300_12000_75db+
                  #8 ADI_300_22050_50db+ 
                  #22 ADI_300_22050_75db+ 
                  #9 ADI_1000_12000_50db+ 
                  ADI_1000_12000_75db+
                  #15 ADI_1000_22050_50db+ 
                  #3 ADI_1000_22050_75db+ 
                  #13 AEI_300_12000_50db+ 
                  #17 AEI_300_12000_75db+ 
                  #5 AEI_300_22050_50db+ 
                  #4 AEI_300_22050_75db+ 
                  #19 AEI_1000_12000_50db+ 
                  #7 AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  #6 AEI_1000_22050_75db+ 
                  ACI_300_12000_FFT512+ 
                  #1 ACI_300_22050_FFT512+ 
                  #12 ACI_1000_12000_FFT512+ 
                  #18 ACI_1000_22050_FFT512+
                  #11 BIO_300_12000_FFT512+ 
                  #2 BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  #24 NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  #14 NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #16 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  #20  H_ws512+ 
                  Ht
                  #23 AR
                 ,data = mestra_birds_variables) 
abund_vif0 <- as.data.frame(car::vif(mod.abund))
# View(abund_vif0)

mod.abund.forest <- lm(sample_abund~
                            ADI_1000_12000_75db+
                            AEI_1000_22050_50db+ 
                            ACI_300_12000_FFT512+ 
                            BIO_1000_12000_FFT512+ 
                            BIO_1000_22050_FFT512+ 
                            NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                            Ht,
                data = mestra_birds_variables) 
mod.abund.forest


# to forest diversity ---------------------------------------------------------------------------
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
                  AR, data = mestra_birds_variables) 
invisimp_vif0 <- as.data.frame(car::vif(mod.invsimp))
# View(invisimp_vif0)

mod.invsimp <- lm(sample_invsimp~#17 ADI_300_12000_50db+
                    #8 ADI_300_12000_75db+
                    #7 ADI_300_22050_50db+ 
                    #18 ADI_300_22050_75db+ 
                    #9 ADI_1000_12000_50db+ 
                    ADI_1000_12000_75db+
                    #11 ADI_1000_22050_50db+ 
                    #3 ADI_1000_22050_75db+ 
                    #12 AEI_300_12000_50db+ 
                    #16 AEI_300_12000_75db+ 
                    #5 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    AEI_1000_12000_50db+ 
                    #6 AEI_1000_12000_75db+ 
                    #15 AEI_1000_22050_50db+ 
                    #14 AEI_1000_22050_75db+ 
                    ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    #10 ACI_1000_12000_FFT512+ 
                    #15 ACI_1000_22050_FFT512+
                    #9 BIO_300_12000_FFT512+ 
                    #2 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    #20 NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    #13 NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    #14 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    #16 H_ws512+ 
                    Ht
                    #19 AR
                    , data = mestra_birds_variables) 
invisimp_vif1 <- as.data.frame(car::vif(mod.invsimp))
# View(invisimp_vif1)

mod.invsimp.forest <- lm(sample_invsimp~
                    ADI_1000_12000_75db+
                    AEI_1000_12000_50db+ 
                    ACI_300_12000_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    Ht,
                  data = mestra_birds_variables) 
mod.invsimp.forest


## summary 
mod.riq.forest <- lm(sample_rich~
                       ADI_1000_12000_75db+
                       AEI_1000_12000_50db+ 
                       ACI_1000_12000_FFT512+ 
                       BIO_1000_12000_FFT512+ 
                       BIO_1000_22050_FFT512+ 
                       NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                       Ht,
                     data = mestra_birds_variables) 
mod.riq.forest

mod.abund.forest
mod.abund.forest <- lm(sample_abund~
                         ADI_1000_12000_75db+
                         AEI_1000_22050_50db+ 
                         ACI_300_12000_FFT512+ 
                         BIO_1000_12000_FFT512+ 
                         BIO_1000_22050_FFT512+ 
                         NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                         Ht,
                       data = mestra_birds_variables) 
mod.invsimp.forest
mod.invsimp.forest <- lm(sample_invsimp~
                           ADI_1000_12000_75db+
                           AEI_1000_12000_50db+ 
                           ACI_300_12000_FFT512+ 
                           BIO_1000_12000_FFT512+ 
                           BIO_1000_22050_FFT512+ 
                           NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                           Ht,
                         data = mestra_birds_variables) 
mod.invsimp.forest


# GLM FOREST ------------------------------------------------------------------------------------
mod.riq.forest
#single
mod.riq.NULL   <- glm(sample_rich~1, data = mestra_birds_variables)
mod.riq.ADI_1000_12000_75db  <- glm(sample_rich~ ADI_1000_12000_75db, data = mestra_birds_variables)
mod.riq.AEI_1000_12000_50db  <- glm(sample_rich~AEI_1000_12000_50db, data = mestra_birds_variables)
mod.riq.ACI_1000_12000_FFT512  <- glm(sample_rich~ACI_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_12000_FFT512  <- glm(sample_rich~BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_22050_FFT512  <- glm(sample_rich~BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_rich~NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.riq.Ht  <- glm(sample_rich~Ht, data = mestra_birds_variables)

# bivariate
mod.riq.ADI_1000_12000_75db.AEI_1000_12000_50db  <- glm(sample_rich~ADI_1000_12000_75db+AEI_1000_12000_50db, data = mestra_birds_variables)
mod.riq.ADI_1000_12000_75db.ACI_1000_12000_FFT512  <- glm(sample_rich~ADI_1000_12000_75db+ACI_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.ADI_1000_12000_75db.BIO_1000_12000_FFT512  <- glm(sample_rich~ADI_1000_12000_75db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.ADI_1000_12000_75db.BIO_1000_22050_FFT512  <- glm(sample_rich~ADI_1000_12000_75db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.ADI_1000_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_rich~ADI_1000_12000_75db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.riq.ADI_1000_12000_75db.Ht  <- glm(sample_rich~ADI_1000_12000_75db+Ht, data = mestra_birds_variables)

mod.riq.AEI_1000_12000_50db.ACI_1000_12000_FFT512  <- glm(sample_rich~AEI_1000_12000_50db+ACI_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.AEI_1000_12000_50db.BIO_1000_12000_FFT512  <- glm(sample_rich~AEI_1000_12000_50db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.AEI_1000_12000_50db.BIO_1000_22050_FFT512  <- glm(sample_rich~AEI_1000_12000_50db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_rich~AEI_1000_12000_50db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.riq.AEI_1000_12000_50db.Ht  <- glm(sample_rich~AEI_1000_12000_50db+Ht, data = mestra_birds_variables)

mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512  <- glm(sample_rich~ACI_1000_12000_FFT512+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_rich~ACI_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_rich~ACI_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.riq.ACI_1000_12000_FFT512.Ht  <- glm(sample_rich~ACI_1000_12000_FFT512+Ht, data = mestra_birds_variables)

mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_rich~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_rich~BIO_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_12000_FFT512.Ht  <- glm(sample_rich~BIO_1000_12000_FFT512+Ht, data = mestra_birds_variables)

mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_rich~BIO_1000_22050_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_22050_FFT512.Ht  <- glm(sample_rich~BIO_1000_22050_FFT512+Ht, data = mestra_birds_variables)

#abund
mod.abund.forest

# GLM
#single
mod.abund.NULL   <- glm(sample_abund~1, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db  <- glm(sample_abund~ADI_300_12000_75db, data = mestra_birds_variables)
mod.abund.AEI_1000_22050_50db  <- glm(sample_abund~AEI_1000_22050_50db, data = mestra_birds_variables)
mod.abund.ACI_300_12000_FFT512  <- glm(sample_abund~ACI_300_12000_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_12000_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_22050_FFT512  <- glm(sample_abund~BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.abund.Ht  <- glm(sample_abund~Ht, data = mestra_birds_variables)

# bivariate
mod.abund.ADI_300_12000_75db.AEI_1000_22050_50db  <- glm(sample_abund~ADI_300_12000_75db+AEI_1000_22050_50db, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.ACI_300_12000_FFT512  <- glm(sample_abund~ADI_300_12000_75db+ACI_300_12000_FFT512, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512  <- glm(sample_abund~ADI_300_12000_75db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512  <- glm(sample_abund~ADI_300_12000_75db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~ADI_300_12000_75db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.Ht  <- glm(sample_abund~ADI_300_12000_75db+Ht, data = mestra_birds_variables)

mod.abund.AEI_1000_22050_50db.ACI_300_12000_FFT512  <- glm(sample_abund~AEI_1000_22050_50db+ACI_300_12000_FFT512, data = mestra_birds_variables)
mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512  <- glm(sample_abund~AEI_1000_22050_50db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.AEI_1000_22050_50db.BIO_1000_22050_FFT512  <- glm(sample_abund~AEI_1000_22050_50db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.AEI_1000_22050_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~AEI_1000_22050_50db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.abund.AEI_1000_22050_50db.Ht  <- glm(sample_abund~AEI_1000_22050_50db+Ht, data = mestra_birds_variables)

mod.abund.ACI_300_12000_FFT512.BIO_1000_12000_FFT512  <- glm(sample_abund~ACI_300_12000_FFT512+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.ACI_300_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_abund~ACI_300_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.ACI_300_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~ACI_300_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.abund.ACI_300_12000_FFT512.Ht  <- glm(sample_abund~ACI_300_12000_FFT512+Ht, data = mestra_birds_variables)

mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_12000_FFT512.Ht  <- glm(sample_abund~BIO_1000_12000_FFT512+Ht, data = mestra_birds_variables)

mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_abund~BIO_1000_22050_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_22050_FFT512.Ht  <- glm(sample_abund~BIO_1000_22050_FFT512+Ht, data = mestra_birds_variables)

#div
mod.invsimp.forest

# GLM
#single
mod.invsimp.NULL   <- glm(sample_invsimp~1, data = mestra_birds_variables)
mod.invsimp.ADI_1000_12000_75db  <- glm(sample_invsimp~ADI_1000_12000_75db, data = mestra_birds_variables)
mod.invsimp.AEI_1000_12000_50db  <- glm(sample_invsimp~AEI_1000_12000_50db, data = mestra_birds_variables)
mod.invsimp.ACI_300_12000_FFT512  <- glm(sample_invsimp~ACI_300_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_12000_FFT512  <- glm(sample_invsimp~BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_22050_FFT512  <- glm(sample_invsimp~BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_invsimp~NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.invsimp.Ht  <- glm(sample_invsimp~Ht, data = mestra_birds_variables)

# bivariate
mod.invsimp.ADI_1000_12000_75db.AEI_1000_12000_50db  <- glm(sample_invsimp~ADI_1000_12000_75db+AEI_1000_12000_50db, data = mestra_birds_variables)
mod.invsimp.ADI_1000_12000_75db.ACI_300_12000_FFT512  <- glm(sample_invsimp~ADI_1000_12000_75db+ACI_300_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_1000_12000_75db.BIO_1000_12000_FFT512  <- glm(sample_invsimp~ADI_1000_12000_75db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_1000_12000_75db.BIO_1000_22050_FFT512  <- glm(sample_invsimp~ADI_1000_12000_75db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_1000_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_invsimp~ADI_1000_12000_75db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_1000_12000_75db.Ht  <- glm(sample_invsimp~ADI_1000_12000_75db+Ht, data = mestra_birds_variables)

mod.invsimp.AEI_1000_12000_50db.ACI_300_12000_FFT512  <- glm(sample_invsimp~AEI_1000_12000_50db+ACI_300_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.AEI_1000_12000_50db.BIO_1000_12000_FFT512  <- glm(sample_invsimp~AEI_1000_12000_50db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.AEI_1000_12000_50db.BIO_1000_22050_FFT512  <- glm(sample_invsimp~AEI_1000_12000_50db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_invsimp~AEI_1000_12000_50db+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.invsimp.AEI_1000_12000_50db.Ht  <- glm(sample_invsimp~AEI_1000_12000_50db+Ht, data = mestra_birds_variables)

mod.invsimp.ACI_300_12000_FFT512.BIO_1000_12000_FFT512  <- glm(sample_invsimp~ACI_300_12000_FFT512+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.ACI_300_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_invsimp~ACI_300_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.ACI_300_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_invsimp~ACI_300_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.invsimp.ACI_300_12000_FFT512.Ht  <- glm(sample_invsimp~ACI_300_12000_FFT512+Ht, data = mestra_birds_variables)

mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_invsimp~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_invsimp~BIO_1000_12000_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_12000_FFT512.Ht  <- glm(sample_invsimp~BIO_1000_12000_FFT512+Ht, data = mestra_birds_variables)

mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  <- glm(sample_invsimp~BIO_1000_22050_FFT512+NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_22050_FFT512.Ht  <- glm(sample_invsimp~BIO_1000_22050_FFT512+Ht, data = mestra_birds_variables)

### models analysis -----

# milton's function
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

# richness models results
mod.riq.forest
results_birds_AICc_riq_forest <- bbmle::ICtab(mod.riq.NULL, 
                                              mod.riq.ADI_1000_12000_75db, 
                                              mod.riq.AEI_1000_12000_50db,
                                              mod.riq.ACI_1000_12000_FFT512, 
                                              mod.riq.BIO_1000_12000_FFT512, 
                                              mod.riq.BIO_1000_22050_FFT512, 
                                              mod.riq.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                              mod.riq.Ht,
                                              mod.riq.ADI_1000_12000_75db.AEI_1000_12000_50db, 
                                              mod.riq.ADI_1000_12000_75db.ACI_1000_12000_FFT512, 
                                              mod.riq.ADI_1000_12000_75db.BIO_1000_12000_FFT512, 
                                              mod.riq.ADI_1000_12000_75db.BIO_1000_22050_FFT512, 
                                              mod.riq.ADI_1000_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                              mod.riq.ADI_1000_12000_75db.Ht,
                                              mod.riq.AEI_1000_12000_50db.ACI_1000_12000_FFT512,
                                              mod.riq.AEI_1000_12000_50db.BIO_1000_12000_FFT512,
                                              mod.riq.AEI_1000_12000_50db.BIO_1000_22050_FFT512,
                                              mod.riq.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                              mod.riq.AEI_1000_12000_50db.Ht, 
                                              mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512,
                                              mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512, 
                                              mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                              mod.riq.ACI_1000_12000_FFT512.Ht, 
                                              mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512, 
                                              mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                              mod.riq.BIO_1000_12000_FFT512.Ht, 
                                              mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                              mod.riq.BIO_1000_22050_FFT512.Ht,
                                       type= c("AICc"), nobs=nrow(mestra_birds_variables), weights=T, base=F, delta=T)  

results_birds_AICc_riq_forest.r2 <- as.data.frame(r2.extract(results_birds_AICc_riq_forest))
results_birds_AICc_riq_forest.r2
# View(results_birds_AICc_riq_forest.r2)

# abund models results
results_birds_AICc_abund_forest <- bbmle::ICtab(mod.abund.NULL, 
                                         mod.abund.ADI_300_12000_75db,
                                         mod.abund.AEI_1000_22050_50db, 
                                         mod.abund.ACI_300_12000_FFT512,
                                         mod.abund.BIO_1000_12000_FFT512, 
                                         mod.abund.BIO_1000_22050_FFT512, 
                                         mod.abund.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                         mod.abund.Ht,
                                         mod.abund.ADI_300_12000_75db.AEI_1000_22050_50db,
                                         mod.abund.ADI_300_12000_75db.ACI_300_12000_FFT512,
                                         mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512, 
                                         mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512, 
                                         mod.abund.ADI_300_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                         mod.abund.ADI_300_12000_75db.Ht,
                                         mod.abund.AEI_1000_22050_50db.ACI_300_12000_FFT512,
                                         mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512,
                                         mod.abund.AEI_1000_22050_50db.BIO_1000_22050_FFT512,
                                         mod.abund.AEI_1000_22050_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                         mod.abund.AEI_1000_22050_50db.Ht,  
                                         mod.abund.ACI_300_12000_FFT512.BIO_1000_12000_FFT512,
                                         mod.abund.ACI_300_12000_FFT512.BIO_1000_22050_FFT512,
                                         mod.abund.ACI_300_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                         mod.abund.ACI_300_12000_FFT512.Ht, 
                                         mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512,  
                                         mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                         mod.abund.BIO_1000_12000_FFT512.Ht,
                                         mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                         mod.abund.BIO_1000_22050_FFT512.Ht,
                                         type= c("AICc"), nobs=nrow(mestra_birds_variables), weights=T, base=F, delta=T)  
results_birds_AICc_abund_forest.r2 <- as.data.frame(r2.extract(results_birds_AICc_abund_forest))
results_birds_AICc_abund_forest.r2
# View(results_birds_AICc_abund_forest.r2)

# richness models results
colnames(mestra_birds_variables)
mod.invsimp.forest
results_birds_AICc_invsimp_forest <- bbmle::ICtab(mod.invsimp.NULL, 
                                           mod.invsimp.ADI_1000_12000_75db, 
                                           mod.invsimp.AEI_1000_12000_50db,
                                           mod.invsimp.ACI_300_12000_FFT512, 
                                           mod.invsimp.BIO_1000_12000_FFT512, 
                                           mod.invsimp.BIO_1000_22050_FFT512, 
                                           mod.invsimp.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                           mod.invsimp.Ht,
                                           mod.invsimp.ADI_1000_12000_75db.AEI_1000_12000_50db, 
                                           mod.invsimp.ADI_1000_12000_75db.ACI_300_12000_FFT512, 
                                           mod.invsimp.ADI_1000_12000_75db.BIO_1000_12000_FFT512, 
                                           mod.invsimp.ADI_1000_12000_75db.BIO_1000_22050_FFT512, 
                                           mod.invsimp.ADI_1000_12000_75db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                           mod.invsimp.ADI_1000_12000_75db.Ht,
                                           mod.invsimp.AEI_1000_12000_50db.ACI_300_12000_FFT512,
                                           mod.invsimp.AEI_1000_12000_50db.BIO_1000_12000_FFT512,
                                           mod.invsimp.AEI_1000_12000_50db.BIO_1000_22050_FFT512,
                                           mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                           mod.invsimp.AEI_1000_12000_50db.Ht, 
                                           mod.invsimp.ACI_300_12000_FFT512.BIO_1000_12000_FFT512,
                                           mod.invsimp.ACI_300_12000_FFT512.BIO_1000_22050_FFT512, 
                                           mod.invsimp.ACI_300_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                           mod.invsimp.ACI_300_12000_FFT512.Ht, 
                                           mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512, 
                                           mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512, 
                                           mod.invsimp.BIO_1000_12000_FFT512.Ht, 
                                           mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                                           mod.invsimp.BIO_1000_22050_FFT512.Ht,
                                           type= c("AICc"), nobs=nrow(mestra_birds_variables), weights=T, base=F, delta=T)  

results_birds_AICc_invsimp_forest.r2 <- as.data.frame(r2.extract(results_birds_AICc_invsimp_forest))
results_birds_AICc_invsimp_forest.r2
# View(results_birds_AICc_invsimp_forest.r2)

str(results_birds_AICc_riq_forest.r2)
str(results_birds_AICc_abund_forest.r2)
str(results_birds_AICc_invsimp_forest.r2)
# save.image("all_models_script001.rda")

# compilation
results_birds.multiple.models_forest <- rbind(results_birds_AICc_riq_forest.r2, results_birds_AICc_abund_forest.r2 , results_birds_AICc_invsimp_forest.r2)
results_birds.multiple.models_forest
write.csv2(results_birds.multiple.models_forest, "00_model_selection_AIC_birds_by_SITE_FOREST_ONLY_2021_03_d13.csv")

#save.image("forest_models_2021_03_d13.rda")

# --------------------------------------------------Forest Models Visualization -------------------------------------------------

# load("forest_models_2021_03_d13.rda")
summary(mestra_birds_variables$sample_rich)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.0    24.0    28.0    28.3    33.0    50.0 

## surface plots to FOREST
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

plot.rich_01 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ACI_1000_12000_FFT512 , z=Predicted))+
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
plot.rich_01
png("./plots/models_forest_only/01_mod.riq.ACI.BIO.png", width=900, height=900)
plot.rich_01
dev.off()

# mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
m<-predict(mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 )
x <-mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
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

plot.rich_02 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO") +
  ylab("NDSI") +
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
png("./plots/models_forest_only/02_mod.riq.BIO.NDSIpng", width=900, height=900)
plot.rich_02
dev.off()

# mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
m<-predict(mod.riq.ACI_1000_12000_FFT512.Ht )
x <-mod.riq.ACI_1000_12000_FFT512.Ht$data$ACI_1000_12000_FFT512
y <-mod.riq.ACI_1000_12000_FFT512.Ht$data$Ht
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("ACI_1000_12000_FFT512","Ht")

psi<-data.frame(predict(mod.riq.ACI_1000_12000_FFT512.Ht , newdata=fs))
colnames(psi)<-"Predicted"

psi$ACI_1000_12000_FFT512<-fs$ACI_1000_12000_FFT512
psi$Ht<-fs$Ht

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.rich_03 <-ggplot(data=psi, aes(x=ACI_1000_12000_FFT512, y=Ht , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("ACI") +
  ylab("Ht") +
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
png("./plots/models_forest_only/03_mod.riq.ACI_Htpng", width=900, height=900)
plot.rich_03
dev.off()

# mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
m<-predict(mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 )
x <-mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$ACI_1000_12000_FFT512
y <-mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("ACI_1000_12000_FFT512","NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512")

psi<-data.frame(predict(mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 <-fs$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 
psi$ACI_1000_12000_FFT512<-fs$ACI_1000_12000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.rich_04 <-ggplot(data=psi, aes(x=ACI_1000_12000_FFT512 , y=NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512  , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("ACI") +
  ylab("NDSI") +
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
png("./plots/models_forest_only/04_mod.riq.ACI.NDSI.png", width=900, height=900)
plot.rich_04
dev.off()

# mod.riq.BIO_1000_12000_FFT512.Ht
m<-predict(mod.riq.BIO_1000_12000_FFT512.Ht )
x <-mod.riq.BIO_1000_12000_FFT512.Ht$data$BIO_1000_12000_FFT512 
y <-mod.riq.BIO_1000_12000_FFT512.Ht$data$Ht
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","Ht")

psi<-data.frame(predict(mod.riq.BIO_1000_12000_FFT512.Ht , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$Ht<-fs$Ht

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.rich_05 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=Ht , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO") +
  ylab("Ht") +
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
png("./plots/models_forest_only/05_mod.riq.BIO.Ht.png", width=900, height=900)
plot.rich_05
dev.off()

# top five to forest abundance  --------------------------------------
summary(mestra_birds_variables$sample_abund)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13.00   54.00   79.00   73.67   91.00  143.00 

# mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
m<-predict(mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 )
x <-mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512")

psi<-data.frame(predict(mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512<-fs$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.abund_01 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO") +
  ylab("NDSI") +
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
  labs(fill = "Number of Tags")
plot.abund_01
png("./plots/models_forest_only/01_mod.abund.BIO.NDSI.png", width=900, height=900)
plot.abund_01
dev.off()

# mod.abund.ACI_300_12000_FFT512.BIO_1000_12000_FFT512
m<-predict(mod.abund.ACI_300_12000_FFT512.BIO_1000_12000_FFT512 )
x <-mod.abund.ACI_300_12000_FFT512.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.ACI_300_12000_FFT512.BIO_1000_12000_FFT512$data$ACI_300_12000_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","ACI_300_12000_FFT512")

psi<-data.frame(predict(mod.abund.ACI_300_12000_FFT512.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$ACI_300_12000_FFT512<-fs$ACI_300_12000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.abund_02 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ACI_300_12000_FFT512 , z=Predicted))+
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
  labs(fill = "Number of Tags")
plot.abund_02
png("./plots/models_forest_only/02_mod.abund.BIO.ACI.png", width=900, height=900)
plot.abund_02
dev.off()

# mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512
m<-predict(mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512 )
x <-mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$ADI_300_12000_75db
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

plot.abund_03 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ADI_300_12000_75db , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
  labs(fill = "Number of Tags")
plot.abund_03
png("./plots/models_forest_only/03_mod.abund.BIO.ADI.png", width=900, height=900)
plot.abund_03
dev.off()

# mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512
m<-predict(mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512 )
x <-mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512$data$AEI_1000_22050_50db
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","AEI_1000_22050_50db")

psi<-data.frame(predict(mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$AEI_1000_22050_50db<-fs$AEI_1000_22050_50db

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.abund_04 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=AEI_1000_22050_50db, z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
  labs(fill = "Number of Tags")
plot.abund_04
png("./plots/models_forest_only/04_mod.abund.BIO.AEI.png", width=900, height=900)
plot.abund_04
dev.off()

# mod.abund.BIO_1000_12000_FFT512.Ht
m<-predict(mod.abund.BIO_1000_12000_FFT512.Ht )
x <-mod.abund.BIO_1000_12000_FFT512.Ht$data$BIO_1000_12000_FFT512 
y <-mod.abund.BIO_1000_12000_FFT512.Ht$data$Ht
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","Ht")

psi<-data.frame(predict(mod.abund.BIO_1000_12000_FFT512.Ht , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$Ht<-fs$Ht

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.abund_05 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=Ht, z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO") +
  ylab("Ht") +
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
  labs(fill = "Number of Tags")
plot.abund_05
png("./plots/models_forest_only/05_mod.abund.BIO.Ht.png", width=900, height=900)
plot.abund_05
dev.off()


# to forest diversity ---------------------------------

summary(mestra_birds_variables$sample_invsimp)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.64   11.21   14.25   14.38   17.58   27.26 

# mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
m<-predict(mod.invsimp.ACI_300_12000_FFT512.Ht )
x <-mod.invsimp.ACI_300_12000_FFT512.Ht$data$ACI_300_12000_FFT512
y <-mod.invsimp.ACI_300_12000_FFT512.Ht$data$Ht
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("ACI_300_12000_FFT512","Ht")

psi<-data.frame(predict(mod.invsimp.ACI_300_12000_FFT512.Ht , newdata=fs))
colnames(psi)<-"Predicted"

psi$ACI_300_12000_FFT512<-fs$ACI_300_12000_FFT512
psi$Ht<-fs$Ht

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_01 <-ggplot(data=psi, aes(x=ACI_300_12000_FFT512, y=Ht , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("ACI") +
  ylab("Ht") +
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
  labs(fill = "Tags diversity")
plot.invsimp_01
png("./plots/models_forest_only/01_mod.invsimp.ACI_Ht.png", width=900, height=900)
plot.invsimp_01
dev.off()

# mod.invsimp.AEI_1000_12000_50db.Ht
m<-predict(mod.invsimp.AEI_1000_12000_50db.Ht )
x <-mod.invsimp.AEI_1000_12000_50db.Ht$data$AEI_1000_12000_50db
y <-mod.invsimp.AEI_1000_12000_50db.Ht$data$Ht
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("AEI_1000_12000_50db","Ht")

psi<-data.frame(predict(mod.invsimp.AEI_1000_12000_50db.Ht , newdata=fs))
colnames(psi)<-"Predicted"

psi$AEI_1000_12000_50db<-fs$AEI_1000_12000_50db
psi$Ht<-fs$Ht

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_02 <-ggplot(data=psi, aes(x=AEI_1000_12000_50db, y=Ht , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("AEI") +
  ylab("Ht") +
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
  labs(fill = "Tags diversity")
plot.invsimp_02
png("./plots/models_forest_only/02_mod.invsimp.AEI_Ht.png", width=900, height=900)
plot.invsimp_02
dev.off()

#mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
m<-predict(mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 )
x <-mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$AEI_1000_12000_50db
y <-mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$data$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("AEI_1000_12000_50db","NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512")

psi<-data.frame(predict(mod.invsimp.AEI_1000_12000_50db.NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$AEI_1000_12000_50db<-fs$AEI_1000_12000_50db
psi$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512<-fs$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_03 <-ggplot(data=psi, aes(x=AEI_1000_12000_50db, y=NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("AEI") +
  ylab("NDSI") +
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
  labs(fill = "Tags diversity")
plot.invsimp_03
png("./plots/models_forest_only/03_mod.invsimp.AEI_NDSI.png",  width=900, height=900)
plot.invsimp_03
dev.off()

#mod.invsimp.ADI_1000_12000_75db.Ht
m<-predict(mod.invsimp.ADI_1000_12000_75db.Ht )
x <-mod.invsimp.ADI_1000_12000_75db.Ht$data$ADI_1000_12000_75db
y <-mod.invsimp.ADI_1000_12000_75db.Ht$data$Ht
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("ADI_1000_12000_75db","Ht")

psi<-data.frame(predict(mod.invsimp.ADI_1000_12000_75db.Ht , newdata=fs))
colnames(psi)<-"Predicted"

psi$ADI_1000_12000_75db<-fs$ADI_1000_12000_75db
psi$Ht<-fs$Ht

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_04 <-ggplot(data=psi, aes(x=ADI_1000_12000_75db, y=Ht , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("ADI") +
  ylab("Ht") +
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
  labs(fill = "Tags diversity")
plot.invsimp_04
png("./plots/models_forest_only/04_mod.invsimp.ADI_Ht.png",  width=900, height=900)
plot.invsimp_04
dev.off()

#mod.invsimp.BIO_1000_22050_FFT512.Ht
m<-predict(mod.invsimp.BIO_1000_22050_FFT512.Ht )
x <-mod.invsimp.BIO_1000_22050_FFT512.Ht$data$BIO_1000_22050_FFT512
y <-mod.invsimp.BIO_1000_22050_FFT512.Ht$data$Ht
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_22050_FFT512","Ht")

psi<-data.frame(predict(mod.invsimp.BIO_1000_22050_FFT512.Ht , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_22050_FFT512<-fs$BIO_1000_22050_FFT512
psi$Ht<-fs$Ht

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_05 <-ggplot(data=psi, aes(x=BIO_1000_22050_FFT512, y=Ht , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO") +
  ylab("Ht") +
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
  labs(fill = "Tags diversity")
plot.invsimp_05
png("./plots/models_forest_only/05_mod.invsimp.BIO_Ht.png",  width=900, height=900)
plot.invsimp_05
dev.off()

### pasture ------------------------------------------------------------------------------------
### pasture ------------------------------------------------------------------------------------
### pasture ------------------------------------------------------------------------------------
### pasture ------------------------------------------------------------------------------------
mestra_birds_variables <- mestra_birds %>% 
  dplyr::select(sample_rich:AR, env)

mestra_birds_variables <- subset(mestra_birds_variables, env == "pasture" )
# View(mestra_birds_variables)
# VIFs 

# to pasture richness --------------------------------------------------------------------------
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
                AR, data = mestra_birds_variables) 
riq_vif0 <- as.data.frame(car::vif(mod.riq))
# View(riq_vif0)

mod.riq <- lm(sample_rich~# 19 ADI_300_12000_50db+
                ADI_300_12000_75db+
                #2 ADI_300_22050_50db+ 
                #3 ADI_300_22050_75db+ 
                ADI_1000_12000_50db+ 
                #9 ADI_1000_12000_75db+
                #8 ADI_1000_22050_50db+ 
                #20 ADI_1000_22050_75db+ 
                #22 AEI_300_12000_50db+ 
                #7 AEI_300_12000_75db+ 
                #6 AEI_300_22050_50db+ 
                #4 AEI_300_22050_75db+ 
                #13 AEI_1000_12000_50db+ 
                #12 AEI_1000_12000_75db+ 
                #18 AEI_1000_22050_50db+ 
                #23 AEI_1000_22050_75db+ 
                #10 ACI_300_12000_FFT512+ 
                #1 ACI_300_22050_FFT512+ 
                ACI_1000_12000_FFT512+ 
                #21 ACI_1000_22050_FFT512+
                #14 BIO_300_12000_FFT512+ 
                #5 BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                #11 NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #24 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                #15 NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                #17 H_ws512+ 
                #Ht+
                AR, data = mestra_birds_variables) 
riq_vif1 <- as.data.frame(car::vif(mod.riq))
# View(riq_vif1)

mod.riq.pasture  <- lm(sample_rich~ADI_300_12000_75db+
                 ADI_1000_12000_50db+ 
                 ACI_1000_12000_FFT512+ 
                 BIO_1000_12000_FFT512+ 
                 BIO_1000_22050_FFT512+ 
                 NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                 AR, data = mestra_birds_variables)
mod.riq.pasture 


# to pasture abundance--------------------------------------------------------------------------
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
                AR, data = mestra_birds_variables) 
abund_vif0 <- as.data.frame(car::vif(mod.abund))
# View(abund_vif0)

mod.abund <- lm(sample_abund~#18 ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #2 ADI_300_22050_50db+ 
                  #3 ADI_300_22050_75db+ 
                  ADI_1000_12000_50db+ 
                  #9 ADI_1000_12000_75db+
                  #8 ADI_1000_22050_50db+ 
                  #19 ADI_1000_22050_75db+ 
                  #21 AEI_300_12000_50db+ 
                  #7 AEI_300_12000_75db+ 
                  #6 AEI_300_22050_50db+ 
                  #4 AEI_300_22050_75db+ 
                  #12 AEI_1000_12000_50db+ 
                  #17 AEI_1000_12000_75db+ 
                  #16 AEI_1000_22050_50db+ 
                  #22 AEI_1000_22050_75db+ 
                  #10 ACI_300_12000_FFT512+ 
                  #1 ACI_300_22050_FFT512+ 
                  ACI_1000_12000_FFT512+ 
                  #20 ACI_1000_22050_FFT512+
                  #13 BIO_300_12000_FFT512+ 
                  #5 BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  #11 NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #23 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  #14 NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  #15 H_ws512+ 
                  #Ht+
                  AR, data = mestra_birds_variables) 
abund_vif1 <- as.data.frame(car::vif(mod.abund))
# View(abund_vif1)

mod.abund.pasture <- lm(sample_abund~ ADI_300_12000_75db+
                  ADI_1000_12000_50db+ 
                  ACI_1000_12000_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+
                  AR, data = mestra_birds_variables) 
mod.abund.pasture

# to pasture invsimp --------------------------------------------------------------------------
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
                  AR, data = mestra_birds_variables) 
invsimp_vif0 <- as.data.frame(car::vif(mod.invsimp))
# View(invsimp_vif0)

mod.invsimp <- lm(sample_invsimp~#18 ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #2 ADI_300_22050_50db+ 
                    #3 ADI_300_22050_75db+ 
                    ADI_1000_12000_50db+ 
                    #8 ADI_1000_12000_75db+
                    #9 ADI_1000_22050_50db+ 
                    #19 ADI_1000_22050_75db+ 
                    #21 AEI_300_12000_50db+ 
                    #7 AEI_300_12000_75db+ 
                    #6 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    #12 AEI_1000_12000_50db+ 
                    #17 AEI_1000_12000_75db+ 
                    #16 AEI_1000_22050_50db+ 
                    #22 AEI_1000_22050_75db+ 
                    #10 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    ACI_1000_12000_FFT512+ 
                    #20 ACI_1000_22050_FFT512+
                    #13 BIO_300_12000_FFT512+ 
                    #5 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    #11 NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    #24 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    #14 NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    #15 H_ws512+ 
                    #25 Ht+
                    AR, data = mestra_birds_variables) 
invsimp_vif1 <- as.data.frame(car::vif(mod.invsimp))
# View(invsimp_vif1)

mod.invsimp.pasture <- lm(sample_invsimp~ADI_300_12000_75db+
                    ADI_1000_12000_50db+ 
                    ACI_1000_12000_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    AR, data = mestra_birds_variables) 
mod.invsimp.pasture

# summary 
mod.riq.pasture  <- lm(sample_rich~ADI_300_12000_75db+
                         ADI_1000_12000_50db+ 
                         ACI_1000_12000_FFT512+ 
                         BIO_1000_12000_FFT512+ 
                         BIO_1000_22050_FFT512+ 
                         NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                         AR, data = mestra_birds_variables)
mod.riq.pasture 

mod.abund.pasture <- lm(sample_abund~ ADI_300_12000_75db+
                          ADI_1000_12000_50db+ 
                          ACI_1000_12000_FFT512+ 
                          BIO_1000_12000_FFT512+ 
                          BIO_1000_22050_FFT512+ 
                          NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+
                          AR, data = mestra_birds_variables) 
mod.abund.pasture

mod.invsimp.pasture <- lm(sample_invsimp~ADI_300_12000_75db+
                            ADI_1000_12000_50db+ 
                            ACI_1000_12000_FFT512+ 
                            BIO_1000_12000_FFT512+ 
                            BIO_1000_22050_FFT512+ 
                            NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                            AR, data = mestra_birds_variables) 
mod.invsimp.pasture

# GLM PASTURE  ------------------------------------------------------------------------------------
# richness
#single
mod.riq.NULL   <- glm(sample_rich~1, data = mestra_birds_variables)
mod.riq.ADI_300_12000_75db  <- glm(sample_rich~ ADI_300_12000_75db, data = mestra_birds_variables)
mod.riq.ADI_1000_12000_50db  <- glm(sample_rich~ ADI_1000_12000_50db, data = mestra_birds_variables)
mod.riq.ACI_1000_12000_FFT512  <- glm(sample_rich~ACI_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_12000_FFT512  <- glm(sample_rich~BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_22050_FFT512  <- glm(sample_rich~BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_rich~NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.riq.AR  <- glm(sample_rich~AR, data = mestra_birds_variables)

#bivariate
mod.riq.ADI_300_12000_75db.ADI_1000_12000_50db  <- glm(sample_rich~ ADI_300_12000_75db+ADI_1000_12000_50db, data = mestra_birds_variables)
mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512  <- glm(sample_rich~ ADI_300_12000_75db+ACI_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512  <- glm(sample_rich~ ADI_300_12000_75db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512  <- glm(sample_rich~ ADI_300_12000_75db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_rich~ ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.riq.ADI_300_12000_75db.AR  <- glm(sample_rich~ ADI_300_12000_75db+AR, data = mestra_birds_variables)

mod.riq.ADI_1000_12000_50db.ACI_1000_12000_FFT512  <- glm(sample_rich~ ADI_1000_12000_50db+ACI_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.ADI_1000_12000_50db.BIO_1000_12000_FFT512  <- glm(sample_rich~ ADI_1000_12000_50db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.ADI_1000_12000_50db.BIO_1000_22050_FFT512  <- glm(sample_rich~ ADI_1000_12000_50db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.ADI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_rich~ ADI_1000_12000_50db+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.riq.ADI_1000_12000_50d.AR  <- glm(sample_rich~ ADI_1000_12000_50db+AR, data = mestra_birds_variables)

mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512  <- glm(sample_rich~ACI_1000_12000_FFT512+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_rich~ACI_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_rich~ACI_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.riq.ACI_1000_12000_FFT512.AR  <- glm(sample_rich~ACI_1000_12000_FFT512+AR, data = mestra_birds_variables)

mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_rich~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_rich~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_12000_FFT512.AR  <- glm(sample_rich~BIO_1000_12000_FFT512+AR, data = mestra_birds_variables)

mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_rich~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_22050_FFT512.AR  <- glm(sample_rich~BIO_1000_22050_FFT512+AR, data = mestra_birds_variables)

# abund 
#single
mod.abund.NULL   <- glm(sample_abund~1, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db  <- glm(sample_abund~ ADI_300_12000_75db, data = mestra_birds_variables)
mod.abund.ADI_1000_12000_50db  <- glm(sample_abund~ ADI_1000_12000_50db, data = mestra_birds_variables)
mod.abund.ACI_1000_12000_FFT512  <- glm(sample_abund~ACI_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_12000_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_22050_FFT512  <- glm(sample_abund~BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_abund~NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.abund.AR  <- glm(sample_abund~AR, data = mestra_birds_variables)

#bivariate
mod.abund.ADI_300_12000_75db.ADI_1000_12000_50db  <- glm(sample_abund~ ADI_300_12000_75db+ADI_1000_12000_50db, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512  <- glm(sample_abund~ ADI_300_12000_75db+ACI_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512  <- glm(sample_abund~ ADI_300_12000_75db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512  <- glm(sample_abund~ ADI_300_12000_75db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_abund~ ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.AR  <- glm(sample_abund~ ADI_300_12000_75db+AR, data = mestra_birds_variables)

mod.abund.ADI_1000_12000_50db.ACI_1000_12000_FFT512  <- glm(sample_abund~ ADI_1000_12000_50db+ACI_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.ADI_1000_12000_50db.BIO_1000_12000_FFT512  <- glm(sample_abund~ ADI_1000_12000_50db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.ADI_1000_12000_50db.BIO_1000_22050_FFT512  <- glm(sample_abund~ ADI_1000_12000_50db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.ADI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_abund~ ADI_1000_12000_50db+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.abund.ADI_1000_12000_50db.AR  <- glm(sample_abund~ ADI_1000_12000_50db+AR, data = mestra_birds_variables)

mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512  <- glm(sample_abund~ACI_1000_12000_FFT512+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_abund~ACI_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_abund~ACI_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.abund.ACI_1000_12000_FFT512.AR  <- glm(sample_abund~ACI_1000_12000_FFT512+AR, data = mestra_birds_variables)

mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_12000_FFT512.AR  <- glm(sample_abund~BIO_1000_12000_FFT512+AR, data = mestra_birds_variables)

mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_abund~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_22050_FFT512.AR  <- glm(sample_abund~BIO_1000_22050_FFT512+AR, data = mestra_birds_variables)

# diversity
#single
mod.invsimp.NULL   <- glm(sample_invsimp~1, data = mestra_birds_variables)
mod.invsimp.ADI_300_12000_75db  <- glm(sample_invsimp~ ADI_300_12000_75db, data = mestra_birds_variables)
mod.invsimp.ADI_1000_12000_50db  <- glm(sample_invsimp~ ADI_1000_12000_50db, data = mestra_birds_variables)
mod.invsimp.ACI_1000_12000_FFT512  <- glm(sample_invsimp~ACI_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_12000_FFT512  <- glm(sample_invsimp~BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_22050_FFT512  <- glm(sample_invsimp~BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.invsimp.AR  <- glm(sample_invsimp~AR, data = mestra_birds_variables)

#bivariate
mod.invsimp.ADI_300_12000_75db.ADI_1000_12000_50db  <- glm(sample_invsimp~ ADI_300_12000_75db+ADI_1000_12000_50db, data = mestra_birds_variables)
mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512  <- glm(sample_invsimp~ ADI_300_12000_75db+ACI_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512  <- glm(sample_invsimp~ ADI_300_12000_75db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_300_12000_75db.BIO_1000_22050_FFT512  <- glm(sample_invsimp~ ADI_300_12000_75db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_invsimp~ ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_300_12000_75db.AR  <- glm(sample_invsimp~ ADI_300_12000_75db+AR, data = mestra_birds_variables)

mod.invsimp.ADI_1000_12000_50db.ACI_1000_12000_FFT512  <- glm(sample_invsimp~ ADI_1000_12000_50db+ACI_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_1000_12000_50db.BIO_1000_12000_FFT512  <- glm(sample_invsimp~ ADI_1000_12000_50db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_1000_12000_50db.BIO_1000_22050_FFT512  <- glm(sample_invsimp~ ADI_1000_12000_50db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_invsimp~ ADI_1000_12000_50db+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_1000_12000_50db.AR  <- glm(sample_invsimp~ ADI_1000_12000_50db+AR, data = mestra_birds_variables)

mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512  <- glm(sample_invsimp~ACI_1000_12000_FFT512+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_invsimp~ACI_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_invsimp~ACI_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.invsimp.ACI_1000_12000_FFT512.AR  <- glm(sample_invsimp~ACI_1000_12000_FFT512+AR, data = mestra_birds_variables)

mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_invsimp~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_invsimp~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_12000_FFT512.AR  <- glm(sample_invsimp~BIO_1000_12000_FFT512+AR, data = mestra_birds_variables)

mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_invsimp~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_22050_FFT512.AR  <- glm(sample_invsimp~BIO_1000_22050_FFT512+AR, data = mestra_birds_variables)


### models analysis -----

# milton's function
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


# richness models results
mod.riq.pasture
results_birds_AICc_riq_pasture <- bbmle::ICtab(mod.riq.NULL,
                                               mod.riq.ADI_300_12000_75db,
                                               mod.riq.ADI_1000_12000_50db,
                                               mod.riq.ACI_1000_12000_FFT512,
                                               mod.riq.BIO_1000_12000_FFT512,
                                               mod.riq.BIO_1000_22050_FFT512,
                                               mod.riq.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.riq.AR,
                                               mod.riq.ADI_300_12000_75db.ADI_1000_12000_50db,
                                               mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512,
                                               mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512,
                                               mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512,
                                               mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.riq.ADI_300_12000_75db.AR,
                                               mod.riq.ADI_1000_12000_50db.ACI_1000_12000_FFT512,
                                               mod.riq.ADI_1000_12000_50db.BIO_1000_22050_FFT512,
                                               mod.riq.ADI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.riq.ADI_1000_12000_50d.AR,
                                               mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512,
                                               mod.riq.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512,
                                               mod.riq.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.riq.ACI_1000_12000_FFT512.AR,
                                               mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512,
                                               mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.riq.BIO_1000_12000_FFT512.AR,
                                               mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.riq.BIO_1000_22050_FFT512.AR,
                                              type= c("AICc"), nobs=nrow(mestra_birds_variables), weights=T, base=F, delta=T)  

results_birds_AICc_riq_pasture.r2 <- as.data.frame(r2.extract(results_birds_AICc_riq_pasture))
results_birds_AICc_riq_pasture.r2

# abundance
mod.abund.pasture
results_birds_AICc_abund_pasture <- bbmle::ICtab(mod.abund.NULL,
                                               mod.abund.ADI_300_12000_75db,
                                               mod.abund.ADI_1000_12000_50db,
                                               mod.abund.ACI_1000_12000_FFT512,
                                               mod.abund.BIO_1000_12000_FFT512,
                                               mod.abund.BIO_1000_22050_FFT512,
                                               mod.abund.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.abund.AR,
                                               mod.abund.ADI_300_12000_75db.ADI_1000_12000_50db,
                                               mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512,
                                               mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512,
                                               mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512,
                                               mod.abund.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.abund.ADI_300_12000_75db.AR,
                                               mod.abund.ADI_1000_12000_50db.ACI_1000_12000_FFT512,
                                               mod.abund.ADI_1000_12000_50db.BIO_1000_22050_FFT512,
                                               mod.abund.ADI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.abund.ADI_1000_12000_50db.AR,
                                               mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512,
                                               mod.abund.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512,
                                               mod.abund.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.abund.ACI_1000_12000_FFT512.AR,
                                               mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512,
                                               mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.abund.BIO_1000_12000_FFT512.AR,
                                               mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.abund.BIO_1000_22050_FFT512.AR,
                                               type= c("AICc"), nobs=nrow(mestra_birds_variables), weights=T, base=F, delta=T)  

results_birds_AICc_abund_pasture.r2 <- as.data.frame(r2.extract(results_birds_AICc_abund_pasture))
results_birds_AICc_abund_pasture.r2

# diversity
mod.invsimp.pasture
results_birds_AICc_invsimp_pasture <- bbmle::ICtab(mod.invsimp.NULL,
                                               mod.invsimp.ADI_300_12000_75db,
                                               mod.invsimp.ADI_1000_12000_50db,
                                               mod.invsimp.ACI_1000_12000_FFT512,
                                               mod.invsimp.BIO_1000_12000_FFT512,
                                               mod.invsimp.BIO_1000_22050_FFT512,
                                               mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.invsimp.AR,
                                               mod.invsimp.ADI_300_12000_75db.ADI_1000_12000_50db,
                                               mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512,
                                               mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512,
                                               mod.invsimp.ADI_300_12000_75db.BIO_1000_22050_FFT512,
                                               mod.invsimp.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.invsimp.ADI_300_12000_75db.AR,
                                               mod.invsimp.ADI_1000_12000_50db.ACI_1000_12000_FFT512,
                                               mod.invsimp.ADI_1000_12000_50db.BIO_1000_22050_FFT512,
                                               mod.invsimp.ADI_1000_12000_50db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.invsimp.ADI_1000_12000_50db.AR,
                                               mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512,
                                               mod.invsimp.ACI_1000_12000_FFT512.BIO_1000_22050_FFT512,
                                               mod.invsimp.ACI_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.invsimp.ACI_1000_12000_FFT512.AR,
                                               mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512,
                                               mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.invsimp.BIO_1000_12000_FFT512.AR,
                                               mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                                               mod.invsimp.BIO_1000_22050_FFT512.AR,
                                               type= c("AICc"), nobs=nrow(mestra_birds_variables), weights=T, base=F, delta=T)  

results_birds_AICc_invsimp_pasture.r2 <- as.data.frame(r2.extract(results_birds_AICc_invsimp_pasture))
results_birds_AICc_invsimp_pasture.r2


# View(results_birds_AICc_invsimp_pasture.r2)
str(results_birds_AICc_riq_pasture.r2)
str(results_birds_AICc_abund_pasture.r2)
str(results_birds_AICc_invsimp_pasture.r2)
# save.image("all_models_script001.rda")

# compilation
results_birds.multiple.models_pasture <- rbind(results_birds_AICc_riq_pasture.r2, results_birds_AICc_abund_pasture.r2 , results_birds_AICc_invsimp_pasture.r2)
results_birds.multiple.models_pasture
write.csv2(results_birds.multiple.models_pasture, "00_model_selection_AIC_birds_by_SITE_pasture_ONLY_2021_03_d13.csv")

# --------------------------------------------------PASTURE Models Visualization -------------------------------------------------

summary(mestra_birds_variables$sample_rich)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.00   24.00   32.00   30.38   39.00   56.00 

# View(mestra_birds_variables)

## surface plots to pasture
# mod.riq.BIO_1000_12000_FFT512.AR PLOT
m<-predict(mod.riq.BIO_1000_12000_FFT512.AR )
x <-mod.riq.BIO_1000_12000_FFT512.AR$data$BIO_1000_12000_FFT512 
y <-mod.riq.BIO_1000_12000_FFT512.AR$data$AR
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

plot.rich_01 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=AR , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
plot.rich_01
png("./plots/models_pasture_only/01_mod.riq.ACI.AR.png", width=900, height=900)
plot.rich_01
dev.off()

# mod.riq.ACI_1000_12000_FFT512.AR
m<-predict(mod.riq.ACI_1000_12000_FFT512.AR )
x <-mod.riq.ACI_1000_12000_FFT512.AR$data$ACI_1000_12000_FFT512 
y <-mod.riq.ACI_1000_12000_FFT512.AR$data$AR
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("ACI_1000_12000_FFT512","AR")

psi<-data.frame(predict(mod.riq.ACI_1000_12000_FFT512.AR , newdata=fs))
colnames(psi)<-"Predicted"

psi$ACI_1000_12000_FFT512 <-fs$ACI_1000_12000_FFT512 
psi$AR<-fs$AR

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.rich_02 <-ggplot(data=psi, aes(x=ACI_1000_12000_FFT512 , y=AR , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("ACI") +
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
png("./plots/models_pasture_only/02_mod.riq.ACI.AR.png", width=900, height=900)
plot.rich_02
dev.off()

# mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512
m<-predict(mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512 )
x <-mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512
y <-mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$ADI_300_12000_75db
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","ADI_300_12000_75db")

psi<-data.frame(predict(mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512<-fs$BIO_1000_12000_FFT512
psi$ADI_300_12000_75db<-fs$ADI_300_12000_75db

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.rich_03 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512, y=ADI_300_12000_75db , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
png("./plots/models_pasture_only/03_mod.riq.BIO_ADI.png", width=900, height=900)
plot.rich_03
dev.off()

# mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512
m<-predict(mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512 )
x <-mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512$data$ACI_1000_12000_FFT512
y <-mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512$data$ADI_300_12000_75db
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("ACI_1000_12000_FFT512","ADI_300_12000_75db")

psi<-data.frame(predict(mod.riq.ADI_300_12000_75db.ACI_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$ADI_300_12000_75db <-fs$ADI_300_12000_75db 
psi$ACI_1000_12000_FFT512<-fs$ACI_1000_12000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.rich_04 <-ggplot(data=psi, aes(x=ACI_1000_12000_FFT512 , y=ADI_300_12000_75db  , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("ACI") +
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
plot.rich_04
png("./plots/models_pasture_only/04_mod.riq.ACI.ADI.png", width=900, height=900)
plot.rich_04
dev.off()

# mod.riq.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512
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

plot.rich_05 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ACI_1000_12000_FFT512 , z=Predicted))+
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
plot.rich_05
png("./plots/models_pasture_only/05_mod.riq.BIO.ACI.png", width=900, height=900)
plot.rich_05
dev.off()

# top five to pasture abundance  --------------------------------------
summary(mestra_birds_variables$sample_abund)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 9.00   67.00   87.00   85.16  110.00  165.00 

# mod.abund.BIO_1000_12000_FFT512.AR
m<-predict(mod.abund.BIO_1000_12000_FFT512.AR )
x <-mod.abund.BIO_1000_12000_FFT512.AR$data$BIO_1000_12000_FFT512 
y <-mod.abund.BIO_1000_12000_FFT512.AR$data$AR
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

plot.abund_01 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=AR , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
  labs(fill = "Number of Tags")
plot.abund_01
png("./plots/models_pasture_only/01_mod.abund.BIO.AR.png", width=900, height=900)
plot.abund_01
dev.off()

# mod.abund.ACI_1000_12000_FFT512.AR
m<-predict(mod.abund.ACI_1000_12000_FFT512.AR )
y <-mod.abund.ACI_1000_12000_FFT512.AR$data$AR
x <-mod.abund.ACI_1000_12000_FFT512.AR$data$ACI_1000_12000_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("ACI_1000_12000_FFT512","AR")

psi<-data.frame(predict(mod.abund.ACI_1000_12000_FFT512.AR , newdata=fs))
colnames(psi)<-"Predicted"

psi$AR<-fs$AR
psi$ACI_1000_12000_FFT512<-fs$ACI_1000_12000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.abund_02 <-ggplot(data=psi, aes(x=ACI_1000_12000_FFT512, y=AR , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("ACI") +
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
  labs(fill = "Number of Tags")
plot.abund_02
png("./plots/models_pasture_only/02_mod.abund.ACI.AR.png", width=900, height=900)
plot.abund_02
dev.off()

# mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512
m<-predict(mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512 )
x <-mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512$data$ACI_1000_12000_FFT512 
y <-mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512$data$ADI_300_12000_75db
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("ACI_1000_12000_FFT512","ADI_300_12000_75db")

psi<-data.frame(predict(mod.abund.ADI_300_12000_75db.ACI_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$ACI_1000_12000_FFT512 <-fs$ACI_1000_12000_FFT512 
psi$ADI_300_12000_75db<-fs$ADI_300_12000_75db

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.abund_03 <-ggplot(data=psi, aes(x=ACI_1000_12000_FFT512 , y=ADI_300_12000_75db , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("ACI") +
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
  labs(fill = "Number of Tags")
plot.abund_03
png("./plots/models_pasture_only/03_mod.abund.ACI.ADI.png", width=900, height=900)
plot.abund_03
dev.off()

# mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512
m<-predict(mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512 )
x <-mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$ADI_300_12000_75db
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

plot.abund_04 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ADI_300_12000_75db, z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
  labs(fill = "Number of Tags")
plot.abund_04
png("./plots/models_pasture_only/04_mod.abund.BIO.ADI.png", width=900, height=900)
plot.abund_04
dev.off()

# mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512
m<-predict(mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512 )
x <-mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.ACI_1000_12000_FFT512.BIO_1000_12000_FFT512$data$ACI_1000_12000_FFT512
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

plot.abund_05 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ACI_1000_12000_FFT512, z=Predicted))+
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
  labs(fill = "Number of Tags")
plot.abund_05
png("./plots/models_pasture_only/05_mod.abund.BIO.ACI.png", width=900, height=900)
plot.abund_05
dev.off()


# to pasture diversity ---------------------------------
summary(mestra_birds_variables$sample_invsimp )

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.080   9.828  14.240  14.599  19.305  27.710 


# mod.invsimp.BIO_1000_12000_FFT512.AR
m<-predict(mod.invsimp.BIO_1000_12000_FFT512.AR )
x <-mod.invsimp.BIO_1000_12000_FFT512.AR$data$BIO_1000_12000_FFT512
y <-mod.invsimp.BIO_1000_12000_FFT512.AR$data$AR
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","AR")

psi<-data.frame(predict(mod.invsimp.BIO_1000_12000_FFT512.AR , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512<-fs$BIO_1000_12000_FFT512
psi$AR<-fs$AR

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_01 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512, y=AR , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
  labs(fill = "Tags diversity")
plot.invsimp_01
png("./plots/models_pasture_only/01_mod.invsimp.BIO_AR.png", width=900, height=900)
plot.invsimp_01
dev.off()

# mod.invsimp.ACI_1000_12000_FFT512.AR
m<-predict(mod.invsimp.ACI_1000_12000_FFT512.AR )
x <-mod.invsimp.ACI_1000_12000_FFT512.AR$data$ACI_1000_12000_FFT512
y <-mod.invsimp.ACI_1000_12000_FFT512.AR$data$AR
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("ACI_1000_12000_FFT512","AR")

psi<-data.frame(predict(mod.invsimp.ACI_1000_12000_FFT512.AR , newdata=fs))
colnames(psi)<-"Predicted"

psi$ACI_1000_12000_FFT512<-fs$ACI_1000_12000_FFT512
psi$AR<-fs$AR

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_02 <-ggplot(data=psi, aes(x=ACI_1000_12000_FFT512, y=AR , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("ACI") +
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
  labs(fill = "Tags diversity")
plot.invsimp_02
png("./plots/models_pasture_only/02_mod.invsimp.ACI_AR.png", width=900, height=900)
plot.invsimp_02
dev.off()

#mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512
m<-predict(mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512 )
x <-mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512
y <-mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$ADI_300_12000_75db
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","ADI_300_12000_75db")

psi<-data.frame(predict(mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512<-fs$BIO_1000_12000_FFT512
psi$ADI_300_12000_75db<-fs$ADI_300_12000_75db

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_03 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512, y=ADI_300_12000_75db , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
  labs(fill = "Tags diversity")
plot.invsimp_03
png("./plots/models_pasture_only/03_mod.invsimp.BIO_ADI.png",  width=900, height=900)
plot.invsimp_03
dev.off()

#mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512
m<-predict(mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512 )
x <-mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512$data$ACI_1000_12000_FFT512
y <-mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512$data$ADI_300_12000_75db
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("ACI_1000_12000_FFT512","ADI_300_12000_75db")

psi<-data.frame(predict(mod.invsimp.ADI_300_12000_75db.ACI_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$ACI_1000_12000_FFT512<-fs$ACI_1000_12000_FFT512
psi$ADI_300_12000_75db<-fs$ADI_300_12000_75db

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_04 <-ggplot(data=psi, aes(x=ACI_1000_12000_FFT512, y=ADI_300_12000_75db , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("ACI") +
  ylab("ADI-01") +
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
  labs(fill = "Tags diversity")
plot.invsimp_04
png("./plots/models_pasture_only/04_mod.invsimp.ACI_ADI.png",  width=900, height=900)
plot.invsimp_04
dev.off()

#mod.invsimp.ADI_1000_12000_50db.ACI_1000_12000_FFT512
m<-predict(mod.invsimp.ADI_1000_12000_50db.ACI_1000_12000_FFT512 )
x <-mod.invsimp.ADI_1000_12000_50db.ACI_1000_12000_FFT512$data$ACI_1000_12000_FFT512
y <-mod.invsimp.ADI_1000_12000_50db.ACI_1000_12000_FFT512$data$ADI_1000_12000_50db
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("ACI_1000_12000_FFT512","ADI_1000_12000_50db")

psi<-data.frame(predict(mod.invsimp.ADI_1000_12000_50db.ACI_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$ACI_1000_12000_FFT512<-fs$ACI_1000_12000_FFT512
psi$ADI_1000_12000_50db<-fs$ADI_1000_12000_50db

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_05 <-ggplot(data=psi, aes(x=ACI_1000_12000_FFT512, y=ADI_1000_12000_50db , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("ACI") +
  ylab("ADI-02") +
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
  labs(fill = "Tags diversity")
plot.invsimp_05
png("./plots/models_pasture_only/05_mod.invsimp.ACI_ADI.png",  width=900, height=900)
plot.invsimp_05
dev.off()


### swamp ------------------------------------------------------------------------------------
### swamp ------------------------------------------------------------------------------------
### swamp ------------------------------------------------------------------------------------
### swamp ------------------------------------------------------------------------------------

## import data ----------------------------------------------------------

mestra_birds_variables <- mestra_birds %>% 
  dplyr::select(sample_rich:AR, env)
# View(mestra_birds)

mestra_birds_variables <- subset(mestra_birds_variables, env == "swamp" )
colnames(mestra_birds_variables)
# View(mestra_birds_variables)

# to swamp richness --------------------------------------------------------------------------
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
                AR, data = mestra_birds_variables) 
riq_vif0 <- as.data.frame(car::vif(mod.riq))
# View(riq_vif0)

mod.riq <- lm(sample_rich~#20 ADI_300_12000_50db+
                ADI_300_12000_75db+
                #2 ADI_300_22050_50db+ 
                #3 ADI_300_22050_75db+ 
                #11 ADI_1000_12000_50db+ 
                #8 ADI_1000_12000_75db+
                #7 ADI_1000_22050_50db+ 
                #13 ADI_1000_22050_75db+ 
                #18 AEI_300_12000_50db+ 
                #19 AEI_300_12000_75db+ 
                #5 AEI_300_22050_50db+ 
                #4 AEI_300_22050_75db+ 
                #16 AEI_1000_12000_50db+ 
                #7 AEI_1000_12000_75db+ 
                AEI_1000_22050_50db+ 
                #17 AEI_1000_22050_75db+ 
                #12 ACI_300_12000_FFT512+ 
                #1 ACI_300_22050_FFT512+ 
                #17 ACI_1000_12000_FFT512+ 
                #23 ACI_1000_22050_FFT512+
                #10 BIO_300_12000_FFT512+ 
                #6 BIO_300_22050_FFT512+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                #14 NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                #9 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                #21 NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                #15 H_ws512+ 
                Ht,
                #24 AR,
              data = mestra_birds_variables) 
riq_vif1 <- as.data.frame(car::vif(mod.riq))
# View(riq_vif1)

mod.riq.swamp <- lm(sample_rich~
                ADI_300_12000_75db+
                AEI_1000_22050_50db+ 
                BIO_1000_12000_FFT512+ 
                BIO_1000_22050_FFT512+ 
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                Ht,
              data = mestra_birds_variables) 
mod.riq.swamp

# to swamp  abund--------------------------------------------------------------------------
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
                AR, data = mestra_birds_variables) 
abund_vif0 <- as.data.frame(car::vif(mod.abund))
# (abund_vif0)

mod.abund <- lm(sample_abund~#20 ADI_300_12000_50db+
                  ADI_300_12000_75db+
                  #2 ADI_300_22050_50db+ 
                  #13 ADI_300_22050_75db+ 
                  #12 ADI_1000_12000_50db+ 
                  #8 ADI_1000_12000_75db+
                  #9 ADI_1000_22050_50db+ 
                  #3 ADI_1000_22050_75db+ 
                  #19 AEI_300_12000_50db+ 
                  #17 AEI_300_12000_75db+ 
                  #4 AEI_300_22050_50db+ 
                  #5 AEI_300_22050_75db+ 
                  #16 AEI_1000_12000_50db+ 
                  #7 AEI_1000_12000_75db+ 
                  AEI_1000_22050_50db+ 
                  #18 AEI_1000_22050_75db+ 
                  #12 ACI_300_12000_FFT512+ 
                  #1 ACI_300_22050_FFT512+ 
                  #17 ACI_1000_12000_FFT512+ 
                  #22 ACI_1000_22050_FFT512+
                  #11 BIO_300_12000_FFT512+ 
                  #6 BIO_300_22050_FFT512+ 
                  BIO_1000_12000_FFT512+ 
                  BIO_1000_22050_FFT512+ 
                  #14 NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                  NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                  #10 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                  #21 NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                  #15 H_ws512+ 
                  Ht,
                  #AR,
                data = mestra_birds_variables) 
abund_vif1 <- as.data.frame(car::vif(mod.abund))
# View(abund_vif1)

mod.abund.swamp <- lm(sample_abund~
                             ADI_300_12000_75db+
                             AEI_1000_22050_50db+ 
                             BIO_1000_12000_FFT512+ 
                             BIO_1000_22050_FFT512+ 
                             NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                             Ht,
                data = mestra_birds_variables) 
mod.abund.swamp 

# to swamp diversity--------------------------------------------------------------------------
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
                  AR, data = mestra_birds_variables) 
invsimp_vif0 <- as.data.frame(car::vif(mod.invsimp))
# (invsimp_vif0)

mod.invsimp <- lm(sample_invsimp~#22 ADI_300_12000_50db+
                    ADI_300_12000_75db+
                    #2 ADI_300_22050_50db+ 
                    #3 ADI_300_22050_75db+ 
                    #13 ADI_1000_12000_50db+ 
                    #9 ADI_1000_12000_75db+
                    #8 ADI_1000_22050_50db+ 
                    #15 ADI_1000_22050_75db+ 
                    #20 AEI_300_12000_50db+ 
                    #21 AEI_300_12000_75db+ 
                    #5 AEI_300_22050_50db+ 
                    #4 AEI_300_22050_75db+ 
                    #18 AEI_1000_12000_50db+ 
                    #7 AEI_1000_12000_75db+ 
                    AEI_1000_22050_50db+ 
                    #19 AEI_1000_22050_75db+ 
                    #14 ACI_300_12000_FFT512+ 
                    #1 ACI_300_22050_FFT512+ 
                    #19 ACI_1000_12000_FFT512+ 
                    #25  ACI_1000_22050_FFT512+
                    #11 BIO_300_12000_FFT512+ 
                    #6 BIO_300_22050_FFT512+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    #16 NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    #10 NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512+
                    #24 NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512+ 
                    #17 H_ws512+ 
                    Ht+
                    AR, data = mestra_birds_variables) 
invsimp_vif1 <- as.data.frame(car::vif(mod.invsimp))
# View(invsimp_vif1)

mod.invsimp.swamp <- lm(sample_invsimp~
                    ADI_300_12000_75db+
                    AEI_1000_22050_50db+ 
                    BIO_1000_12000_FFT512+ 
                    BIO_1000_22050_FFT512+ 
                    NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                    Ht+
                    AR, data = mestra_birds_variables) 
mod.invsimp.swamp


# Summary 
mod.riq.swamp <- lm(sample_rich~
                      ADI_300_12000_75db+
                      AEI_1000_22050_50db+ 
                      BIO_1000_12000_FFT512+ 
                      BIO_1000_22050_FFT512+ 
                      NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                      Ht,
                    data = mestra_birds_variables) 
mod.riq.swamp


mod.abund.swamp <- lm(sample_abund~
                        ADI_300_12000_75db+
                        AEI_1000_22050_50db+ 
                        BIO_1000_12000_FFT512+ 
                        BIO_1000_22050_FFT512+ 
                        NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                        Ht,
                      data = mestra_birds_variables) 
mod.abund.swamp 



mod.invsimp.swamp <- lm(sample_invsimp~
                          ADI_300_12000_75db+
                          AEI_1000_22050_50db+ 
                          BIO_1000_12000_FFT512+ 
                          BIO_1000_22050_FFT512+ 
                          NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+ 
                          Ht+
                          AR, data = mestra_birds_variables) 
mod.invsimp.swamp

# GLM SWAMP ------------------------------------------------------------------------------------
# richness
#single
mod.riq.NULL   <- glm(sample_rich~1, data = mestra_birds_variables)
mod.riq.ADI_300_12000_75db  <- glm(sample_rich~ ADI_300_12000_75db, data = mestra_birds_variables)
mod.riq.AEI_1000_22050_50db  <- glm(sample_rich~ AEI_1000_22050_50db, data = mestra_birds_variables)
mod.riq.BIO_1000_12000_FFT512  <- glm(sample_rich~BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_22050_FFT512  <- glm(sample_rich~BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_rich~NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.riq.Ht  <- glm(sample_rich~Ht, data = mestra_birds_variables)

#bivariate
mod.riq.ADI_300_12000_75db.AEI_1000_22050_50db  <- glm(sample_rich~ ADI_300_12000_75db+AEI_1000_22050_50db, data = mestra_birds_variables)
mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512  <- glm(sample_rich~ ADI_300_12000_75db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512  <- glm(sample_rich~ ADI_300_12000_75db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_rich~ ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.riq.ADI_300_12000_75db.Ht  <- glm(sample_rich~ ADI_300_12000_75db+Ht, data = mestra_birds_variables)

mod.riq.AEI_1000_22050_50db.BIO_1000_12000_FFT512  <- glm(sample_rich~ AEI_1000_22050_50db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.riq.AEI_1000_22050_50db.BIO_1000_22050_FFT512  <- glm(sample_rich~ BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.AEI_1000_22050_50db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_rich~ AEI_1000_22050_50db+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.riq.AEI_1000_22050_50db.Ht  <- glm(sample_rich~ AEI_1000_22050_50db+Ht, data = mestra_birds_variables)

mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_rich~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_rich~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_12000_FFT512.Ht  <- glm(sample_rich~BIO_1000_12000_FFT512+Ht, data = mestra_birds_variables)

mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_rich~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.riq.BIO_1000_22050_FFT512.Ht  <- glm(sample_rich~BIO_1000_22050_FFT512+Ht, data = mestra_birds_variables)

mod.riq.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512.Ht  <- glm(sample_rich~NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+Ht, data = mestra_birds_variables)


# GLM SWAMP ------------------------------------------------------------------------------------
# abund
#single
mod.abund.NULL   <- glm(sample_abund~1, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db  <- glm(sample_abund~ ADI_300_12000_75db, data = mestra_birds_variables)
mod.abund.AEI_1000_22050_50db  <- glm(sample_abund~ AEI_1000_22050_50db, data = mestra_birds_variables)
mod.abund.BIO_1000_12000_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_22050_FFT512  <- glm(sample_abund~BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_abund~NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.abund.Ht  <- glm(sample_abund~Ht, data = mestra_birds_variables)

#bivariate
mod.abund.ADI_300_12000_75db.AEI_1000_22050_50db  <- glm(sample_abund~ ADI_300_12000_75db+AEI_1000_22050_50db, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512  <- glm(sample_abund~ ADI_300_12000_75db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512  <- glm(sample_abund~ ADI_300_12000_75db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_abund~ ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.abund.ADI_300_12000_75db.Ht  <- glm(sample_abund~ ADI_300_12000_75db+Ht, data = mestra_birds_variables)

mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512  <- glm(sample_abund~ AEI_1000_22050_50db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.abund.AEI_1000_22050_50db.BIO_1000_22050_FFT512  <- glm(sample_abund~ BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.AEI_1000_22050_50db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_abund~ AEI_1000_22050_50db+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.abund.AEI_1000_22050_50db.Ht  <- glm(sample_abund~ AEI_1000_22050_50db+Ht, data = mestra_birds_variables)

mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_abund~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_12000_FFT512.Ht  <- glm(sample_abund~BIO_1000_12000_FFT512+Ht, data = mestra_birds_variables)

mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_abund~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.abund.BIO_1000_22050_FFT512.Ht  <- glm(sample_abund~BIO_1000_22050_FFT512+Ht, data = mestra_birds_variables)

mod.abund.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512.Ht  <- glm(sample_abund~NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+Ht, data = mestra_birds_variables)

# GLM SWAMP ------------------------------------------------------------------------------------
# invsimp
#single
mod.invsimp.NULL   <- glm(sample_invsimp~1, data = mestra_birds_variables)
mod.invsimp.ADI_300_12000_75db  <- glm(sample_invsimp~ ADI_300_12000_75db, data = mestra_birds_variables)
mod.invsimp.AEI_1000_22050_50db  <- glm(sample_invsimp~ AEI_1000_22050_50db, data = mestra_birds_variables)
mod.invsimp.BIO_1000_12000_FFT512  <- glm(sample_invsimp~BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_22050_FFT512  <- glm(sample_invsimp~BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.invsimp.Ht  <- glm(sample_invsimp~Ht, data = mestra_birds_variables)

#bivariate
mod.invsimp.ADI_300_12000_75db.AEI_1000_22050_50db  <- glm(sample_invsimp~ ADI_300_12000_75db+AEI_1000_22050_50db, data = mestra_birds_variables)
mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512  <- glm(sample_invsimp~ ADI_300_12000_75db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_300_12000_75db.BIO_1000_22050_FFT512  <- glm(sample_invsimp~ ADI_300_12000_75db+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_invsimp~ ADI_300_12000_75db+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.invsimp.ADI_300_12000_75db.Ht  <- glm(sample_invsimp~ ADI_300_12000_75db+Ht, data = mestra_birds_variables)

mod.invsimp.AEI_1000_22050_50db.BIO_1000_12000_FFT512  <- glm(sample_invsimp~ AEI_1000_22050_50db+BIO_1000_12000_FFT512, data = mestra_birds_variables)
mod.invsimp.AEI_1000_22050_50db.BIO_1000_22050_FFT512  <- glm(sample_invsimp~ BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.AEI_1000_22050_50db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_invsimp~ AEI_1000_22050_50db+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.invsimp.AEI_1000_22050_50db.Ht  <- glm(sample_invsimp~ AEI_1000_22050_50db+Ht, data = mestra_birds_variables)

mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512  <- glm(sample_invsimp~BIO_1000_12000_FFT512+BIO_1000_22050_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_invsimp~BIO_1000_12000_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_12000_FFT512.Ht  <- glm(sample_invsimp~BIO_1000_12000_FFT512+Ht, data = mestra_birds_variables)

mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512  <- glm(sample_invsimp~BIO_1000_22050_FFT512+NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, data = mestra_birds_variables)
mod.invsimp.BIO_1000_22050_FFT512.Ht  <- glm(sample_invsimp~BIO_1000_22050_FFT512+Ht, data = mestra_birds_variables)

mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512.Ht  <- glm(sample_invsimp~NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512+Ht, data = mestra_birds_variables)



### models analysis -----

# milton's function
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


# richness models results
mod.riq.swamp
results_birds_AICc_riq_swamp <- bbmle::ICtab(mod.riq.NULL,
                                             mod.riq.ADI_300_12000_75db,
                                             mod.riq.AEI_1000_22050_50db,  
                                             mod.riq.BIO_1000_12000_FFT512, 
                                             mod.riq.BIO_1000_22050_FFT512, 
                                             mod.riq.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, 
                                             mod.riq.Ht, 
                                             mod.riq.ADI_300_12000_75db.AEI_1000_22050_50db, 
                                             mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512, 
                                             mod.riq.ADI_300_12000_75db.BIO_1000_22050_FFT512,
                                             mod.riq.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, 
                                             mod.riq.ADI_300_12000_75db.Ht, 
                                             mod.riq.AEI_1000_22050_50db.BIO_1000_12000_FFT512, 
                                             mod.riq.AEI_1000_22050_50db.BIO_1000_22050_FFT512, 
                                             mod.riq.AEI_1000_22050_50db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, 
                                             mod.riq.AEI_1000_22050_50db.Ht,  
                                             mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512,  
                                             mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,  
                                             mod.riq.BIO_1000_12000_FFT512.Ht, 
                                             mod.riq.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, 
                                             mod.riq.BIO_1000_22050_FFT512.Ht, 
                                             mod.riq.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512.Ht, 
                                          type= c("AICc"), nobs=nrow(mestra_birds_variables), weights=T, base=F, delta=T)  

results_birds_AICc_riq_swamp.r2 <- as.data.frame(r2.extract(results_birds_AICc_riq_swamp))
results_birds_AICc_riq_swamp.r2

# abundance models results
mod.abund.swamp
results_birds_AICc_abund_swamp <- bbmle::ICtab(mod.abund.NULL,
                                             mod.abund.ADI_300_12000_75db,
                                             mod.abund.AEI_1000_22050_50db,  
                                             mod.abund.BIO_1000_12000_FFT512, 
                                             mod.abund.BIO_1000_22050_FFT512, 
                                             mod.abund.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, 
                                             mod.abund.Ht, 
                                             mod.abund.ADI_300_12000_75db.AEI_1000_22050_50db, 
                                             mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512, 
                                             mod.abund.ADI_300_12000_75db.BIO_1000_22050_FFT512,
                                             mod.abund.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, 
                                             mod.abund.ADI_300_12000_75db.Ht, 
                                             mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512, 
                                             mod.abund.AEI_1000_22050_50db.BIO_1000_22050_FFT512, 
                                             mod.abund.AEI_1000_22050_50db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, 
                                             mod.abund.AEI_1000_22050_50db.Ht,  
                                             mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512,  
                                             mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,  
                                             mod.abund.BIO_1000_12000_FFT512.Ht, 
                                             mod.abund.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, 
                                             mod.abund.BIO_1000_22050_FFT512.Ht, 
                                             mod.abund.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512.Ht, 
                                             type= c("AICc"), nobs=nrow(mestra_birds_variables), weights=T, base=F, delta=T)  

results_birds_AICc_abund_swamp.r2 <- as.data.frame(r2.extract(results_birds_AICc_abund_swamp))
results_birds_AICc_abund_swamp.r2

# diversity results
mod.invsimp.swamp
results_birds_AICc_invsimp_swamp <- bbmle::ICtab(mod.invsimp.NULL,
                                               mod.invsimp.ADI_300_12000_75db,
                                               mod.invsimp.AEI_1000_22050_50db,  
                                               mod.invsimp.BIO_1000_12000_FFT512, 
                                               mod.invsimp.BIO_1000_22050_FFT512, 
                                               mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, 
                                               mod.invsimp.Ht, 
                                               mod.invsimp.ADI_300_12000_75db.AEI_1000_22050_50db, 
                                               mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512, 
                                               mod.invsimp.ADI_300_12000_75db.BIO_1000_22050_FFT512,
                                               mod.invsimp.ADI_300_12000_75db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, 
                                               mod.invsimp.ADI_300_12000_75db.Ht, 
                                               mod.invsimp.AEI_1000_22050_50db.BIO_1000_12000_FFT512, 
                                               mod.invsimp.AEI_1000_22050_50db.BIO_1000_22050_FFT512, 
                                               mod.invsimp.AEI_1000_22050_50db.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, 
                                               mod.invsimp.AEI_1000_22050_50db.Ht,  
                                               mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512,  
                                               mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,  
                                               mod.invsimp.BIO_1000_12000_FFT512.Ht, 
                                               mod.invsimp.BIO_1000_22050_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512, 
                                               mod.invsimp.BIO_1000_22050_FFT512.Ht, 
                                               mod.invsimp.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512.Ht, 
                                               type= c("AICc"), nobs=nrow(mestra_birds_variables), weights=T, base=F, delta=T)  

results_birds_AICc_invsimp_swamp.r2 <- as.data.frame(r2.extract(results_birds_AICc_invsimp_swamp))
results_birds_AICc_invsimp_swamp.r2




# View(results_birds_AICc_invsimp_swamp.r2)
str(results_birds_AICc_riq_swamp.r2)
str(results_birds_AICc_abund_swamp.r2)
str(results_birds_AICc_invsimp_swamp.r2)
# save.image("all_models_script001.rda")

# compilation
results_birds.multiple.models_swamp <- rbind(results_birds_AICc_riq_swamp.r2, results_birds_AICc_abund_swamp.r2 , results_birds_AICc_invsimp_swamp.r2)
results_birds.multiple.models_swamp
write.csv2(results_birds.multiple.models_swamp, "00_model_selection_AIC_birds_by_SITE_swamp_ONLY_2021_03_d13.csv")

#save.image("all_models_results_script_03.rda")

# --------------------------------------------------Forest Models Visualization -------------------------------------------------

summary(mestra_birds_variables$sample_rich)
# View(mestra_birds_variables)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.00   21.00   28.00   27.67   35.00   51.00 

## surface plots to swamp
# mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512 PLOT
m<-predict(mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512 )
x <-mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.riq.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$ADI_300_12000_75db
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

plot.rich_01 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ADI_300_12000_75db , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
plot.rich_01
png("./plots/models_swamp_only/01_mod.riq.BIO.ADI.png", width=900, height=900)
plot.rich_01
dev.off()

# mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512
m<-predict(mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512 )
x <-mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512$data$BIO_1000_22050_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","BIO_1000_22050_FFT512")

psi<-data.frame(predict(mod.riq.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$BIO_1000_22050_FFT512<-fs$BIO_1000_22050_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.rich_02 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=BIO_1000_22050_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO-01") +
  ylab("BIO-02") +
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
png("./plots/models_swamp_only/02_mod.riq.BIO1.BIO2png", width=900, height=900)
plot.rich_02
dev.off()

# mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512
m<-predict(mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 )
x <-mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512$data$BIO_1000_12000_FFT512
y <-mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512$data$NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512")

psi<-data.frame(predict(mod.riq.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512<-fs$BIO_1000_12000_FFT512
psi$NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512<-fs$NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.rich_03 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512, y=NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO") +
  ylab("NDSI") +
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
png("./plots/models_swamp_only/03_mod.riq.BIO_NDSI.png", width=900, height=900)
plot.rich_03
dev.off()

# mod.riq.AEI_1000_22050_50db.BIO_1000_12000_FFT512
m<-predict(mod.riq.AEI_1000_22050_50db.BIO_1000_12000_FFT512 )
x <-mod.riq.AEI_1000_22050_50db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512
y <-mod.riq.AEI_1000_22050_50db.BIO_1000_12000_FFT512$data$AEI_1000_22050_50db
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","AEI_1000_22050_50db")

psi<-data.frame(predict(mod.riq.AEI_1000_22050_50db.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$AEI_1000_22050_50db <-fs$AEI_1000_22050_50db 
psi$BIO_1000_12000_FFT512<-fs$BIO_1000_12000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.rich_04 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=AEI_1000_22050_50db  , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
  labs(fill = "Species \nrichness")
plot.rich_04
png("./plots/models_swamp_only/04_mod.riq.BIO.AEI.png", width=900, height=900)
plot.rich_04
dev.off()

# mod.riq.BIO_1000_12000_FFT512
plot.mod.rich.BIO_1000_12000_FFT512 <-ggplot(data=mestra_birds_variables , aes(x=BIO_1000_12000_FFT512, y=sample_rich))+
  geom_point(aes(),shape=16,size=2,alpha=0.3) +
  geom_smooth(method = "glm", se = FALSE, color = "black") +
  xlab("BIO") + ylab("Richness") +
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
        legend.key.width = unit(1.7,"cm"))
plot.mod.rich.BIO_1000_12000_FFT512
png("./plots/models_swamp_only/05_mod.riq.BIO.png", width=900, height=900)
plot.mod.rich.BIO_1000_12000_FFT512
dev.off()

# top five to swamp abundance  --------------------------------------
summary(mestra_birds_variables$sample_abund)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.00   45.00   63.00   64.03   88.00  138.00 

# mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512
m<-predict(mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512 )
x <-mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$ADI_300_12000_75db
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

plot.abund_01 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=ADI_300_12000_75db , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
  labs(fill = "Number of Tags")
plot.abund_01
png("./plots/models_swamp_only/01_mod.abund.BIO.ADI.png", width=900, height=900)
plot.abund_01
dev.off()

# mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512
m<-predict(mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512 )
x <-mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512$data$AEI_1000_22050_50db
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","AEI_1000_22050_50db")

psi<-data.frame(predict(mod.abund.AEI_1000_22050_50db.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$AEI_1000_22050_50db<-fs$AEI_1000_22050_50db

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.abund_02 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=AEI_1000_22050_50db , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
  labs(fill = "Number of Tags")
plot.abund_02
png("./plots/models_swamp_only/02_mod.abund.BIO.AEI.png", width=900, height=900)
plot.abund_02
dev.off()

# mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512
m<-predict(mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 )
x <-mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512$data$NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512")

psi<-data.frame(predict(mod.abund.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512<-fs$NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.abund_03 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO") +
  ylab("NDSI") +
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
  labs(fill = "Number of Tags")
plot.abund_03
png("./plots/models_swamp_only/03_mod.abund.BIO.NDSI.png", width=900, height=900)
plot.abund_03
dev.off()

# mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512
m<-predict(mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512 )
x <-mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512$data$BIO_1000_12000_FFT512 
y <-mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512$data$BIO_1000_22050_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","BIO_1000_22050_FFT512")

psi<-data.frame(predict(mod.abund.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512 <-fs$BIO_1000_12000_FFT512 
psi$BIO_1000_22050_FFT512<-fs$BIO_1000_22050_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.abund_04 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512 , y=BIO_1000_22050_FFT512, z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO-01") +
  ylab("BIO-02") +
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
  labs(fill = "Number of Tags")
plot.abund_04
png("./plots/models_swamp_only/04_mod.abund.BIO1.BIO2.png", width=900, height=900)
plot.abund_04
dev.off()


# mod.abund.BIO_1000_12000_FFT512
plot.mod.abund.BIO_1000_12000_FFT512 <-ggplot(data=mestra_birds_variables , aes(x=BIO_1000_12000_FFT512, y=sample_abund))+
  geom_point(aes(),shape=16,size=2,alpha=0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  xlab("BIO") + ylab("Number of Tags") +
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
        legend.key.width = unit(1.7,"cm"))
plot.mod.abund.BIO_1000_12000_FFT512
png("./plots/models_swamp_only/05_mod.abund.BIO.png", width=900, height=900)
plot.mod.abund.BIO_1000_12000_FFT512
dev.off()

# to swamp diversity ---------------------------------

# mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512
m<-predict(mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512 )
x <-mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512
y <-mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512$data$ADI_300_12000_75db
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","ADI_300_12000_75db")

psi<-data.frame(predict(mod.invsimp.ADI_300_12000_75db.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512<-fs$BIO_1000_12000_FFT512
psi$ADI_300_12000_75db<-fs$ADI_300_12000_75db

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_01 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512, y=ADI_300_12000_75db , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
  labs(fill = "Tags diversity")
plot.invsimp_01
png("./plots/models_swamp_only/01_mod.invsimp.BIO_ADI.png", width=900, height=900)
plot.invsimp_01
dev.off()

# mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512
m<-predict(mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512 )
x <-mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512$data$BIO_1000_12000_FFT512
y <-mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512$data$BIO_1000_22050_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","BIO_1000_22050_FFT512")

psi<-data.frame(predict(mod.invsimp.BIO_1000_12000_FFT512.BIO_1000_22050_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512<-fs$BIO_1000_12000_FFT512
psi$BIO_1000_22050_FFT512<-fs$BIO_1000_22050_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_02 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512, y=BIO_1000_22050_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO-01") +
  ylab("BIO-02") +
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
  labs(fill = "Tags diversity")
plot.invsimp_02
png("./plots/models_swamp_only/02_mod.invsimp.BIO1_BIO2", width=900, height=900)
plot.invsimp_02
dev.off()

#mod.invsimp.AEI_1000_22050_50db.BIO_1000_12000_FFT512
m<-predict(mod.invsimp.AEI_1000_22050_50db.BIO_1000_12000_FFT512 )
x <-mod.invsimp.AEI_1000_22050_50db.BIO_1000_12000_FFT512$data$BIO_1000_12000_FFT512
y <-mod.invsimp.AEI_1000_22050_50db.BIO_1000_12000_FFT512$data$AEI_1000_22050_50db
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","AEI_1000_22050_50db")

psi<-data.frame(predict(mod.invsimp.AEI_1000_22050_50db.BIO_1000_12000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512<-fs$BIO_1000_12000_FFT512
psi$AEI_1000_22050_50db<-fs$AEI_1000_22050_50db

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_03 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512, y=AEI_1000_22050_50db , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
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
  labs(fill = "Tags diversity")
plot.invsimp_03
png("./plots/models_swamp_only/03_mod.invsimp.BIO_AEI",  width=900, height=900)
plot.invsimp_03
dev.off()

#mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512
m<-predict(mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 )
x <-mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512$data$BIO_1000_12000_FFT512
y <-mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512$data$NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512
d<-as.data.frame(cbind(m, x, y))

f<-seq(range(d$x)[1],range(d$x)[2], length=100)
s<-seq(range(d$y)[1],range(d$y)[2], length=100)
fs<-expand.grid(f,s)
colnames(fs)<-c("BIO_1000_12000_FFT512","NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512")

psi<-data.frame(predict(mod.invsimp.BIO_1000_12000_FFT512.NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 , newdata=fs))
colnames(psi)<-"Predicted"

psi$BIO_1000_12000_FFT512<-fs$BIO_1000_12000_FFT512
psi$NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512<-fs$NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512

na<-1:(10000-length(x))
na[na>0]<-NA
xna<-c(x,na)

na<-1:(10000-length(y))
na[na>0]<-NA
yna<-c(y,na)

plot.invsimp_04 <-ggplot(data=psi, aes(x=BIO_1000_12000_FFT512, y=NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 , z=Predicted))+
  geom_raster(aes(fill=Predicted), show.legend = T) +
  #scale_fill_distiller(palette = "RdBu", direction = -1) +
  scale_fill_viridis(option = "D") +
  geom_contour(aes(z=Predicted), color="black", size=0.8, alpha=0.5)+
  geom_text_contour(vjust=1.2, size=12, stroke = 0.05)+
  geom_point(aes(x=xna, y=yna),shape=16,size=2,alpha=0.3) +
  xlab("BIO") +
  ylab("NDSI") +
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
  labs(fill = "Tags diversity")
plot.invsimp_04
png("./plots/models_swamp_only/04_mod.invsimp.BIO_NDSI.png",  width=900, height=900)
plot.invsimp_04
dev.off()

# mod.invsimp.BIO_1000_12000_FFT512
plot.mod.invsimp.BIO_1000_12000_FFT512 <-ggplot(data=mestra_birds_variables , aes(x=BIO_1000_12000_FFT512, y=sample_invsimp))+
  geom_point(aes(),shape=16,size=2,alpha=0.3) +
  geom_smooth(method = "glm", se = FALSE, color = "black") +
  xlab("BIO") + ylab("Diversity") +
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
        legend.key.width = unit(1.7,"cm"))
plot.mod.invsimp.BIO_1000_12000_FFT512
png("./plots/models_swamp_only/05_mod.invsimp.BIO.png", width=900, height=900)
plot.mod.invsimp.BIO_1000_12000_FFT512
dev.off()

# end --------------------------------------------------------------------











