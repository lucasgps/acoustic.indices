                    #' ---
                    #' title: calcul acoustic indices
                    #' author: lucas gaspar
                    #' date: 2020-01-01
                    #' ---
                    
                    if(!require(soundecology)) install.packages("soundecology")  
                    if(!require(seewave)) install.packages("seewave")  
                    if(!require(tuneR)) install.packages("tuneR")  
                    if(!require(dplyr)) install.packages("dplyr")  
                    
                    # directory
                    setwd("/home/gasparzinho/Documents/backpup/ANALISYS_2020_06_d01/data_anuros/audios_files/audio_files_tags")
                    
                    ## import data ----------------------------------------------------------
                    list.files<-dir(pattern = "wav")
                    list.files
                    
                    n.rec=length(list.files)
                    n.rec


#ACI e NDSI sao calculados pelo Analysis Program

## ADI   ----                  
    
    #diferentes cortes na freq max e min com db no defaut
    multiple_sounds(directory=".", resultfile="ADI_300_12000_50db.csv", soundindex="acoustic_diversity", max_freq = 12000, db_threshold = -50, freq_step = 300, shannon = TRUE)
    multiple_sounds(directory=".", resultfile="ADI_300_22050_50db.csv", soundindex="acoustic_diversity", max_freq = 22050, db_threshold = -50, freq_step = 300, shannon = TRUE)
    
    multiple_sounds(directory=".", resultfile="ADI_300_12000_75db.csv", soundindex="acoustic_diversity", max_freq = 12000, db_threshold = -75, freq_step = 300, shannon = TRUE)
    multiple_sounds(directory=".", resultfile="ADI_300_22050_75db.csv", soundindex="acoustic_diversity", max_freq = 22050, db_threshold = -75, freq_step = 300, shannon = TRUE)
    
    multiple_sounds(directory=".", resultfile="ADI_1000_12000_50db.csv", soundindex="acoustic_diversity", max_freq = 12000, db_threshold = -50, freq_step = 1000, shannon = TRUE)
    multiple_sounds(directory=".", resultfile="ADI_1000_22050_50db.csv", soundindex="acoustic_diversity", max_freq = 22050, db_threshold = -50, freq_step = 1000, shannon = TRUE)
    
    multiple_sounds(directory=".", resultfile="ADI_1000_12000_75db.csv", soundindex="acoustic_diversity", max_freq = 12000, db_threshold = -75, freq_step = 1000, shannon = TRUE)
    multiple_sounds(directory=".", resultfile="ADI_1000_22050_75db.csv", soundindex="acoustic_diversity", max_freq = 22050, db_threshold = -75, freq_step = 1000, shannon = TRUE)
    
    ## H   ----    
    
    multiple_sounds(directory=".", resultfile="H_WS512.csv", soundindex="H", wl=512)
    
    
    ## AEI   ----    
    
    
    #diferentes cortes na freq max e min com db no defaut
    
    multiple_sounds(directory=".", resultfile="AEI_300_12000_50db.csv", soundindex="acoustic_evenness", max_freq = 12000, db_threshold = -50, freq_step = 300)
    multiple_sounds(directory=".", resultfile="AEI_300_22050_50db.csv", soundindex="acoustic_evenness", max_freq = 22050, db_threshold = -50, freq_step = 300)
    
    multiple_sounds(directory=".", resultfile="AEI_300_12000_75db.csv", soundindex="acoustic_evenness", max_freq = 12000, db_threshold = -75, freq_step = 300)
    multiple_sounds(directory=".", resultfile="AEI_300_22050_75db.csv", soundindex="acoustic_evenness", max_freq = 22050, db_threshold = -75, freq_step = 300)
    
    multiple_sounds(directory=".", resultfile="AEI_1000_12000_50db.csv", soundindex="acoustic_evenness", max_freq= 12000, db_threshold = -50, freq_step = 1000)
    multiple_sounds(directory=".", resultfile="AEI_1000_22050_50db.csv", soundindex="acoustic_evenness", max_freq = 22050, db_threshold = -50, freq_step = 1000)
    
    multiple_sounds(directory=".", resultfile="AEI_1000_12000_75db.csv", soundindex="acoustic_evenness", max_freq= 12000, db_threshold = -75, freq_step = 1000)
    multiple_sounds(directory=".", resultfile="AEI_1000_22050_75db.csv", soundindex="acoustic_evenness", max_freq = 22050, db_threshold = -75, freq_step = 1000)
    
    ## BIO   ----    
    
    multiple_sounds(directory=".", resultfile="BIO_300_12000_FFT512.csv", soundindex="bioacoustic_index", max_freq = 12000, min_freq = 300, fft_w = 512)
    multiple_sounds(directory=".", resultfile="BIO_300_22050_FFT512.csv", soundindex="bioacoustic_index", max_freq = 22050, min_freq = 300, fft_w = 512)
    
    multiple_sounds(directory=".", resultfile="BIO_1000_12000_FFT512.csv", soundindex="bioacoustic_index", max_freq = 12000, min_freq = 1000, fft_w = 512)
    multiple_sounds(directory=".", resultfile="BIO_1000_22050_FFT512.csv", soundindex="bioacoustic_index", max_freq = 22050, min_freq = 1000, fft_w = 512)
    
    ## NDSI   ----    
    
    multiple_sounds(directory=".", resultfile="NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512.csv", soundindex="ndsi", anthro_max = 1000, anthro_min = 300, fft_w = 512,bio_min = 1000, bio_max = 22050)
    multiple_sounds(directory=".", resultfile="NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.csv", soundindex="ndsi", anthro_max = 1000, anthro_min = 300, fft_w = 512,bio_min = 1000, bio_max = 12000)
    
    multiple_sounds(directory=".", resultfile="NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.csv", soundindex="ndsi", anthro_max = 2000, anthro_min = 1000, fft_w = 512,bio_min = 2000, bio_max = 22050)
    multiple_sounds(directory=".", resultfile="NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512.csv", soundindex="ndsi", anthro_max = 2000, anthro_min = 1000, fft_w = 512,bio_min = 2000, bio_max = 12000)
    
    
    ## ACI   ----    
    
    multiple_sounds(directory=".", resultfile="ACI_300_12000_FFT512.csv", soundindex="acoustic_complexity", max_freq = 12000, min_freq = 300, fft_w = 512)
    multiple_sounds(directory=".", resultfile="ACI_300_22050_FFT512.csv", soundindex="acoustic_complexity", max_freq = 22050, min_freq = 300, fft_w = 512)
    
    multiple_sounds(directory=".", resultfile="ACI_1000_12000_FFT512.csv", soundindex="acoustic_complexity", max_freq= 12000, min_freq = 1000, fft_w = 512)
    multiple_sounds(directory=".", resultfile="ACI_1000_22050_FFT512.csv", soundindex="acoustic_complexity", max_freq = 22050, min_freq = 1000, fft_w = 512)
    
    #Acoustic Richness (AR)
    #looping para ler os arquivos .wav no diret?rio
    
    arquivos <- list.files(pattern = "*.wav", full.names = T) 
    
    nome <- str_split(arquivos, "/|\\.", simplify = T)
    
    arquivos <- setNames(arquivos, nm = nome[,3])
    
    
AR.stats <- NULL    
    #AR
    for (i in arquivos){
      xx <- AR(readWave(i))
      xx <- data.frame(xx)
      xx$filename <- i
      AR.stats<-rbind(AR.stats, xx)
    }
    
    AR.stats
    
    #calute AR formula
    AR_real <- (rank(AR.stats[,1]) * rank(AR.stats[,2]))/(nrow(AR.stats)*nrow(AR.stats))
    AR_real <- as.data.frame(AR_real)
    AR_real
    
    #compilation of columns
    AR_results <- cbind(AR.stats, AR_real)
    AR_results
    
    write.csv2(AR_results, "AR_results.csv")
    
      
    
    
    ## import data ----------------------------------------------------------
    list.files<-dir(pattern = "wav")
    list.files
    
    n.rec=length(list.files)
    n.rec
    
    
    
    
    arquivos <- list.files(pattern = "*.wav", full.names = T) 
    
    nome <- str_split(arquivos, "/|\\.", simplify = T)
    
    arquivos <- setNames(arquivos, nm = nome[,3])
    
    # 300 hz 12000hz
    ACI.stats_300_12000<-NULL
    for (i in arquivos) {
      xx <- ACI(readWave(i) ,flim = c(0.3, 12))
      xx <- data.frame(xx)
      xx$filename <- i
      ACI.stats_300_12000 <-rbind(ACI.stats_300_12000, xx)
    }
    
    ACI.stats_300_12000
    write.csv(ACI.stats_300_12000, "ACI_results_300_12000.csv")
    
    
    # 1000 hz 12000hz
    ACI.stats_1000_12000<-NULL
    for (i in arquivos) {
      xx <- ACI(readWave(i), flim = c(1, 12))
      xx <- data.frame(xx)
      xx$filename <- i
      ACI.stats_1000_12000 <-rbind(ACI.stats_1000_12000, xx)
    }
    
    ACI.stats_1000_12000
    write.csv(ACI.stats_1000_12000, "ACI_results_1000_12000.csv")
    
    # 300 hz 22050hz      
    ACI.stats_300_22050<-NULL
    for (i in arquivos) {
      xx <- ACI(readWave(i), flim = c(0.3, 22.05))
      xx <- data.frame(xx)
      xx$filename <- i
      ACI.stats_300_22050 <-rbind(ACI.stats_300_22050, xx)
    }
    
    ACI.stats_300_22050
    write.csv(ACI.stats_300_22050, "ACI_results_300_220500.csv")
    
    # 1000 hz 22050hz
    ACI.stats_1000_22050<-NULL
    for (i in arquivos) {
      xx <- ACI(readWave(i), flim = c(1, 22.05))
      xx <- data.frame(xx)
      xx$filename <- i
      ACI.stats_1000_22050 <-rbind(ACI.stats_1000_22050, xx)
    }
    
    ACI.stats_1000_22050
    write.csv(ACI.stats_1000_22050, "ACI_results_1000_220500.csv")
    
    
#'
#'    compilation
#'        

setwd("./")

ACI_results_300_12000 <- read.csv("ACI_results_300_12000.csv")
ACI_results_300_12000$filename <- substr(ACI_results_300_12000$filename, 9,37)
 
ACI_results_300_12000_new <- ACI_results_300_12000 %>% 
  dplyr::rename(ACI_results_300_12000  = xx) %>% 
  dplyr::arrange(filename)

#                                            
ACI_results_300_220500 <- read.csv("ACI_results_300_220500.csv")
ACI_results_300_220500$filename <- substr(ACI_results_300_220500$filename, 9,37)

ACI_results_300_220500_new <- ACI_results_300_220500 %>% 
  dplyr::rename(ACI_results_300_220500  = xx) %>% 
  dplyr::arrange(filename)
#
ACI_results_1000_12000<- read.csv("ACI_results_1000_12000.csv")
ACI_results_1000_12000$filename <- substr(ACI_results_1000_12000$filename, 9,37)

ACI_results_1000_12000_new <- ACI_results_1000_12000 %>% 
  dplyr::rename(ACI_results_1000_12000  = xx) %>% 
  dplyr::arrange(filename)

#
ACI_results_1000_220500 <- read.csv("ACI_results_1000_220500.csv")
ACI_results_1000_220500$filename <- substr(ACI_results_1000_220500$filename, 9,37)

ACI_results_1000_220500_new <- ACI_results_1000_220500 %>% 
  dplyr::rename(ACI_results_1000_220500  = xx) %>% 
  dplyr::arrange(filename)

#
ADI_300_12000_50db <- read.csv("ADI_300_12000_50db.csv")
ADI_300_12000_50db$FILENAME <- substr(ADI_300_12000_50db$FILENAME, 7,35)

ADI_300_12000_50db_new <- ADI_300_12000_50db %>% 
  dplyr::rename(ADI_300_12000_50db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
ADI_300_12000_75db <- read.csv("ADI_300_12000_75db.csv")
ADI_300_12000_75db$FILENAME <- substr(ADI_300_12000_75db$FILENAME, 7,35)

ADI_300_12000_75db_new <- ADI_300_12000_75db %>% 
  dplyr::rename(ADI_300_12000_75db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
ADI_300_22050_50db <- read.csv("ADI_300_22050_50db.csv")
ADI_300_22050_50db$FILENAME <- substr(ADI_300_22050_50db$FILENAME, 7,35)

ADI_300_22050_50db_new <- ADI_300_22050_50db %>% 
  dplyr::rename(ADI_300_22050_50db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
ADI_300_22050_75db <- read.csv("ADI_300_22050_75db.csv")
ADI_300_22050_75db$FILENAME <- substr(ADI_300_22050_75db$FILENAME, 7,35)

ADI_300_22050_75db_new <- ADI_300_22050_75db %>% 
  dplyr::rename(ADI_300_22050_75db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
ADI_1000_12000_50db <- read.csv("ADI_1000_12000_50db.csv")
ADI_1000_12000_50db$FILENAME <- substr(ADI_1000_12000_50db$FILENAME, 7,35)

ADI_1000_12000_50db_new <- ADI_1000_12000_50db %>% 
  dplyr::rename(ADI_1000_12000_50db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
ADI_1000_12000_75db <- read.csv("ADI_1000_12000_75db.csv")
ADI_1000_12000_75db$FILENAME <- substr(ADI_1000_12000_75db$FILENAME, 7,35)

ADI_1000_12000_75db_new <- ADI_1000_12000_75db %>% 
  dplyr::rename(ADI_1000_12000_75db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
ADI_1000_22050_50db <- read.csv("ADI_1000_22050_50db.csv")
ADI_1000_22050_50db$FILENAME <- substr(ADI_1000_22050_50db$FILENAME, 7,35)

ADI_1000_22050_50db_new <- ADI_1000_22050_50db %>% 
  dplyr::rename(ADI_1000_22050_50db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
ADI_1000_22050_75db <- read.csv("ADI_1000_22050_75db.csv")
ADI_1000_22050_75db$FILENAME <- substr(ADI_1000_22050_75db$FILENAME, 7,35)

ADI_1000_22050_75db_new <- ADI_1000_22050_75db %>% 
  dplyr::rename(ADI_1000_22050_75db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
AEI_300_12000_50db <- read.csv("AEI_300_12000_50db.csv")
AEI_300_12000_50db$FILENAME <- substr(AEI_300_12000_50db$FILENAME, 7,35)

AEI_300_12000_50db_new <- AEI_300_12000_50db %>% 
  dplyr::rename(AEI_300_12000_50db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
AEI_300_12000_75db <- read.csv("AEI_300_12000_75db.csv")
AEI_300_12000_75db$FILENAME <- substr(AEI_300_12000_75db$FILENAME, 7,35)

AEI_300_12000_75db_new <- AEI_300_12000_75db %>% 
  dplyr::rename(AEI_300_12000_75db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
AEI_300_22050_50db <- read.csv("AEI_300_22050_50db.csv")
AEI_300_22050_50db$FILENAME <- substr(AEI_300_22050_50db$FILENAME, 7,35)

AEI_300_22050_50db_new <- AEI_300_22050_50db %>% 
  dplyr::rename(AEI_300_22050_50db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
AEI_300_22050_75db <- read.csv("AEI_300_22050_75db.csv")
AEI_300_22050_75db$FILENAME <- substr(AEI_300_22050_75db$FILENAME, 7,35)

AEI_300_22050_75db_new <- AEI_300_22050_75db %>% 
  dplyr::rename(AEI_300_22050_75db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
AEI_1000_12000_50db <- read.csv("AEI_1000_12000_50db.csv")
AEI_1000_12000_50db$FILENAME <- substr(AEI_1000_12000_50db$FILENAME, 7,35)

AEI_1000_12000_50db_new <- AEI_1000_12000_50db %>% 
  dplyr::rename(AEI_1000_12000_50db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
AEI_1000_12000_75db <- read.csv("AEI_1000_12000_75db.csv")
AEI_1000_12000_75db$FILENAME <- substr(AEI_1000_12000_75db$FILENAME, 7,35)

AEI_1000_12000_75db_new <- AEI_1000_12000_75db %>% 
  dplyr::rename(AEI_1000_12000_75db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
AEI_1000_22050_50db <- read.csv("AEI_1000_22050_50db.csv")
AEI_1000_22050_50db$FILENAME <- substr(AEI_1000_22050_50db$FILENAME, 7,35)

AEI_1000_22050_50db_new <- AEI_1000_22050_50db %>% 
  dplyr::rename(AEI_1000_22050_50db  = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
AEI_1000_22050_75db <- read.csv("AEI_1000_22050_75db.csv")
AEI_1000_22050_75db$FILENAME <- substr(AEI_1000_22050_75db$FILENAME, 7,35)

AEI_1000_22050_75db_new <- AEI_1000_22050_75db %>% 
  dplyr::rename(AEI_1000_22050_75db = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
AR_results <- read.csv2("AR_results.csv")
AR_results$filename <- substr(AR_results$filename, 9,37)

AR_results_new <- AR_results %>% 
  dplyr::rename(AR_results  = AR_real) %>% 
  dplyr::arrange(filename)


#
BIO_300_12000_FFT512 <- read.csv("BIO_300_12000_FFT512.csv")
BIO_300_12000_FFT512$FILENAME <- substr(BIO_300_12000_FFT512$FILENAME, 7,35)

BIO_300_12000_FFT512_new <- BIO_300_12000_FFT512 %>% 
  dplyr::rename(BIO_300_12000_FFT512 = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
BIO_300_22050_FFT512 <- read.csv("BIO_300_22050_FFT512.csv")
BIO_300_22050_FFT512$FILENAME <- substr(BIO_300_22050_FFT512$FILENAME, 7,35)

BIO_300_22050_FFT512_new <- BIO_300_22050_FFT512 %>% 
  dplyr::rename(BIO_300_22050_FFT512 = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
BIO_1000_12000_FFT512 <- read.csv("BIO_1000_12000_FFT512.csv")
BIO_1000_12000_FFT512$FILENAME <- substr(BIO_1000_12000_FFT512$FILENAME, 7,35)

BIO_1000_12000_FFT512_new <- BIO_1000_12000_FFT512 %>% 
  dplyr::rename(BIO_1000_12000_FFT512 = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
BIO_1000_22050_FFT512 <- read.csv("BIO_1000_22050_FFT512.csv")
BIO_1000_22050_FFT512$FILENAME <- substr(BIO_1000_22050_FFT512$FILENAME, 7,35)

BIO_1000_22050_FFT512_new <- BIO_1000_22050_FFT512 %>% 
  dplyr::rename(BIO_1000_22050_FFT512 = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512<- read.csv("NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512.csv")
NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512$FILENAME <- substr(NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512$FILENAME, 7,35)

NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512_new <- NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 %>% 
  dplyr::rename(NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 <- read.csv("NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512.csv")
NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512$FILENAME <- substr(NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512$FILENAME, 7,35)

NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512_new <- NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 %>% 
  dplyr::rename(NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512 <- read.csv("NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512.csv")
NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512$FILENAME <- substr(NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512$FILENAME, 7,35)

NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512_new <- NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512 %>% 
  dplyr::rename(NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512 = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)

#
NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512<- read.csv("NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512.csv")
NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$FILENAME <- substr(NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512$FILENAME, 7,35)

NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512_new <- NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 %>% 
  dplyr::rename(NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 = LEFT_CHANNEL) %>% 
  dplyr::arrange(FILENAME)


# compilation

new_table <- data.frame(matrix(NA, nrow = 1764, ncol = 31))

  
new_table_indices <- new_table %>% 
  dplyr::mutate(filename = NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512_new$FILENAME,
                NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512 = NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512_new$NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512,
                NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512 = NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512_new$NDSI_AMAX2000_AMIN1000_BMAX12000_BMIN2000_FFT512,
                NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512 = NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512_new$NDSI_AMAX1000_AMIN300_BMAX22050_BMIN1000_FFT512,
                NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512 = NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512_new$NDSI_AMAX1000_AMIN300_BMAX12000_BMIN1000_FFT512,
                BIO_1000_22050_FFT512 = BIO_1000_22050_FFT512_new$BIO_1000_22050_FFT512,
                BIO_1000_22050_FFT512 = BIO_1000_22050_FFT512_new$BIO_1000_22050_FFT512,
                BIO_300_22050_FFT512 = BIO_300_22050_FFT512_new$BIO_300_22050_FFT512,
                BIO_300_12000_FFT512 = BIO_300_12000_FFT512_new$BIO_300_12000_FFT512,
                AR = AR_results_new$AR_results,
                Ht = AR_results_new$Ht,
                AEI_1000_22050_75db = AEI_1000_22050_75db_new$AEI_1000_22050_75db,
                AEI_1000_22050_50db = AEI_1000_22050_50db_new$AEI_1000_22050_50db,
                AEI_1000_12000_75db = AEI_1000_12000_75db_new$AEI_1000_12000_75db,
                AEI_1000_12000_50db = AEI_1000_12000_50db_new$AEI_1000_12000_50db,
                AEI_300_22050_75db = AEI_300_22050_75db_new$AEI_300_22050_75db,
                AEI_300_22050_50db = AEI_300_22050_50db_new$AEI_300_22050_50db,
                AEI_300_12000_75db = AEI_300_12000_75db_new$AEI_300_12000_75db,
                AEI_300_12000_50db = AEI_300_12000_50db_new$AEI_300_12000_50db,
                ADI_1000_22050_75db = ADI_1000_22050_75db_new$ADI_1000_22050_75db,
                ADI_1000_22050_50db = ADI_1000_22050_50db_new$ADI_1000_22050_50db,
                ADI_1000_12000_75db = ADI_1000_12000_75db_new$ADI_1000_12000_75db,
                ADI_1000_12000_50db = ADI_1000_12000_50db_new$ADI_1000_12000_50db,
                ADI_300_22050_75db = ADI_300_22050_75db_new$ADI_300_22050_75db,
                ADI_300_22050_50db = ADI_300_22050_50db_new$ADI_300_22050_50db,
                ADI_300_12000_75db = ADI_300_12000_75db_new$ADI_300_12000_75db,
                ADI_300_12000_50db = ADI_300_12000_50db_new$ADI_300_12000_50db,
                ACI_results_1000_220500 = ACI_results_1000_220500_new$ACI_results_1000_220500,
                ACI_results_1000_12000 = ACI_results_1000_12000_new$ACI_results_1000_12000,
                ACI_results_300_220500 = ACI_results_300_220500_new$ACI_results_300_220500,
                ACI_results_300_12000 = ACI_results_300_12000_new$ACI_results_300_12000) %>% 
  dplyr::select(filename, NDSI_AMAX2000_AMIN1000_BMAX22050_BMIN2000_FFT512:ACI_results_300_12000)

out.dir <- "/home/gasparzinho/Documents/backpup/ANALISYS_2020_06_d01/data_anuros/analises"
write.csv2(new_table_indices, "00_mestra_IA_night.csv")
  
