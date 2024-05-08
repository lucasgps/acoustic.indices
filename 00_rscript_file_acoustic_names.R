################################################################################
#######             TABLES INDICES COMPILATION FROM ANALYSIS PROGRAM     #######
#######                                                                  #######
#######                                                                  #######
#######                                                                  #######
#######                Gaspar - LEEC - 23/01/2020                        #######
################################################################################


#install.packages("stringr")
#install.packages("chron")
#install.packages("dplyr")
#install.packages("lubridate")
#install.packages("plyr")

#packages
#install.packages("stringr")
library(stringr)
library(chron)
library(dplyr)
library(lubridate)
library(plyr)

# não pode ter nenhum CSV solto nesse diretório

#directory
setwd("/home/lucas/Área de Trabalho/TRABALHO_DENGUE/resultrs_teste_noite")

#list audio files
list.files<-dir(pattern = "Towsey.Acoustic.Indices.csv", recursive = TRUE)
list.files

n.rec=length(list.files)
n.rec


#looping for read sheets and creat a new column with new name for macth with tags files
for (i in list.files){
  tab <- read.csv(i)                                             #read
  resultminute_mod <- tab$ResultMinute
  resultminute_mod <- str_pad(0:14, pad = 0, width = 2 , "left") # here depend of dataset size! *TO CHAGE* if necessary
  resultminute_mod <- paste0(00, resultminute_mod,0)             # chage to time
  resultminute_mod <- paste0(00, resultminute_mod,0)             # again for stay correcty form - i don't no why
  time  <- paste(substr(tab$FileName,21, 26))                # time part of name
  name_part1 <- substr(tab$FileName, 1, 20)                      # firts part of name
  name_part3 <- substr(tab$FileName, 27,29)                      # last part of name
  year <- substr(tab$FileName, 12,15)                            #separate name of column Filename
  month <- substr(tab$FileName, 16,17)
  day <- substr(tab$FileName, 18,19)
  hour <- substr(tab$FileName, 21,22)
  minute <- substr(tab$FileName, 23,24)
  second <- substr(tab$FileName, 25,26)
  date_oficial <- paste(year, month, day, sep ="-")
  time_oficial <- paste( hour, minute, second, sep = ":")
  data_and_time <- paste (date_oficial, time_oficial, sep = ":")
  hour <- substr(resultminute_mod, 1,2)                         #Separate times of column ResultMinute
  minute <- substr(resultminute_mod, 3,4)
  second <- substr(resultminute_mod, 5,6)
  wav <- ".wav"
  minutes_oficial <- paste (hour, minute, second, sep = ":")
  read_time_oficial<-  times(time_oficial)                      #read vectors how time
  read_resultminute <-  times(minutes_oficial)
  new_time <- read_time_oficial + read_resultminute             #sum vectors
  new_time_oficial <- str_remove_all(new_time, pattern = ":")   #remove : enter time
  compilation_name_part <- as.data.frame(cbind (name_part1, new_time_oficial, 
                                                name_part3, wav, deparse.level = 1)) #building new name oficial
  tab$final_new_name <-  do.call(paste, c(compilation_name_part[c("name_part1", "new_time_oficial",
                                                                  "name_part3","wav")], sep = ""))  #concatenating 
  write.csv2(tab, file = file(basename(i)), quote=FALSE, row.names = F, col.names= T)  #save
}

# end ---------------------------------------