##################################################################################################################################################
###################                                                                                               ################################
###################                   Padroniza??o de etiquetas e matriz de biodiversidade                        ################################
###################                                                                                               ################################
###################                              2020_07_d05   Gaspar                                            ################################ 
##################################################################################################################################################   

#install.packages("stringr")
library(stringr)

#directory
setwd("/home/gasparzinho/Documents/backpup/ANALISYS_2020_06_d01/data_anuros/etiquetas/tags")

#list files for pattern
dir.txts.all<-dir(".", pattern=".txt$")
n.rec = length(dir.txts.all)
n.rec

head(dir.txts.all)


# standardization of tags

for (i in dir.txts.all){
  xx = read.table(i, head=T, sep="\t", fill = TRUE, na.strings = c("NULL","NA"))           ############### read table
  colnames(xx) <- c("Selection","View","Channel","Begin.Time..s.","End.Time..s.","Low.Freq..Hz.",
                    "High.Freq..Hz.","species")                                     ##############  pattern name of columns
  #xx$species <- tolower(xx$species)
  #aux2<-xx[!duplicated(xx$Selection), ]                                               ##############   remove duplicate
  #aux2[,2]<-"Spectrogram 1"                                                             ##############  change names of "view" collun
  #xx[,8] <- tolower(xx[,8])                                                           ################ all lower case
  #aux2[,1] <- seq(1, nrow(aux2))                                                           ############## cahne names of "selection" for numbers of species
  write.table(xx, file = file(basename(i)), sep="\t", na="NA", quote=FALSE, row.names = F, col.names= T)  
} 


for (i in dir.txts.all){
  xx = read.table(i, head=T, sep="\t", fill = TRUE, na.strings = c("NULL","NA","")) 
  xx$species <- xx$species %>%
    str_replace_all(c("BOANALBO" = "boan_albo",
                      "PHYSCUVI" = "phys_cuvi",
                      "SCINCROS" = "scin_cros",
                      "ELACCESA" = "elac_cesa",
                      "RHINORNA" = "rhin_orna",
                      "APLALEUC" = "apla_leuc",
                      "ISCHGUEN" = "isch_guen",
                      "LEPTFUSC" = "lept_fusc",
                      "ISCHJUIP" = "isch_juip",
                      "BOANFABE" = "boan_fabe",
                      "PHYSOLFE" = "phys_olfe",
                      "DENDMINU" = "dend_minu",
                      "PHYLBURM" = "phys_burm",
                      "SCINFUSCOV" = "scin_fusc",
                      "BOANBISC" = "boan_bisc",
                      "PHYLBURM" = "phys_burm",
                      "PROCBOIE" = "proc_boie",
                      "BOANPRAS" = "boan_pras",
                      "ISCHPARV" = "osch_parv",
                      "SCINCROS" = "scin_cros",
                      "RHINICTE" = "rhin_icte",
                      "SCINHAYI" = "scin_hayi",
                      "SCINFUSCOM" = "scin_fusc",
                      "PHYLBURM" = "phys_burm",
                      "tete" = "boan_albo",
                      "BOANFABE" = "boan_fabe",
                      "PROC_BOIE" = "proc_boie",
                      "BOANALBO" = "boan_albo",
                      "BOANALBO" = "boan_albo",
                      "BOANALBO" = "boan_albo",
                      "BOANALBO" = "boan_albo",
                      "BOANALBO" = "boan_albo",
                      "BOANALBO" = "boan_albo"))
  write.table(xx, file = file(basename(i)), sep="\t", na="NA", quote=FALSE, row.names = F, col.names= T)  
 }
#found erros

TAB<-NULL
files.with.troubles<-NULL
files.with.diff.colnames<-NULL
cont<-0

dir.txts.all<-dir(".", pattern=".txt$")
n.rec = length(dir.txts.all)
n.rec

for (i in dir.txts.all){
  cont<- cont+1
  
  try(xx.tab<-read.table(i, head=T, sep="\t"))
  if (ncol(xx.tab)!=8 |  nrow(xx.tab)==0) {files.with.troubles<-c(files.with.troubles, i)} 
  
  if (ncol(xx.tab)==8)
  {
    if (nrow(xx.tab)>0)
    {
      xx.tab$filename<-i
      if (length(unique(colnames(xx.tab)!=colnames(TAB)))==2)
      {
        files.with.diff.colnames<-data.frame(rbind(cbind(colnames_errados=i),files.with.diff.colnames))
        colnames(xx.tab)<-colnames(TAB)
      }
      TAB<-data.frame(rbind(TAB, xx.tab))
    }
  }
}

TAB$species <- tolower(TAB$species)
#save   
write.csv2(TAB, "..//00_complete_info_TAB_all_tags_compilation.csv", sep="\t", na="NA", quote=FALSE, row.names = F, col.names= T)


# create table of tags compilation
lista.completa <- NULL
aux<-subset(TAB, select=c(filename, species)) 
TABB<-data.frame(rbind(lista.completa, aux))



TAB <- TABB
dim(TAB)
head(TAB)

write.csv2(TAB, "..//00_TAB_all_tags_compilation.csv", sep="\t", na="NA", quote=FALSE, row.names = F, col.names= T)

# creat troubles control
#TAB$species<-toupper(TAB[,2])
print(paste("Tinha", length(dir.txts.all), "Arquivos"))
print(paste("Foram lidos", cont, "Arquivos"))
print(paste("Desses", length(files.with.troubles), "ou n?o tinham dados, ou n?o tinham 8 colunas"))
print(paste("E foram observados", length(files.with.diff.colnames), "com nomes diferentes de colunas"))


#unlist(strsplit(TAB$filename, "_"))

# separate files names
pesquisador<-substr(TAB$filename,1,5)   
paisagem<-substr(TAB$filename,7,12) 
channel<-substr(TAB$filename,15,15)  
year<-substr(TAB$filename,18,21)  
month<-substr(TAB$filename,22,23)
day<-substr(TAB$filename,24,25)
hour<-substr(TAB$filename,27,28)
minutes<-substr(TAB$filename,29,30)
seconds<-substr(TAB$filename,31,32)
date<-substr(TAB$filename,18,25) 
time <- substr(TAB$filename, 27,32)
ambiente <- substr(TAB$filename, 34,35)

#create new coluns wich info
TAB$pesquisador <- pesquisador
TAB$paisagem <- paisagem
TAB$horario <- time
TAB$data <- date
TAB$ambiente <- ambiente

TAB.juncao <- TAB

######################################################################################################
#################           change possible wrong names             ##################################
######################################################################################################
tags_tab <- TAB.juncao 

data <- tags_tab$species

#data <- tolower(data)
tags_tab_2 <- tags_tab

tags_tab_2$species <- tolower(tags_tab_2$species)

# change noises names
tags_tab_2$species  <- tags_tab$species %>%
  str_replace_all(c("ruido_anuro" = "biof_anuro",
                    "ruido_ave" = "biof_ave",
                    "ruido_festa" = "antro_musica",
                    "vazio" = "NA",
                    "ruido_gado" = "biof_gado",
                    "chuva" = "geof_chuva",
                    "ruido_bovino" = "biof_bovino",
                    "ruido_gado" = "biof_bovino",
                    "ruido_vaca" = "biof_bovino",
                    "vaca" = "biof_bovino",
                    "ruido_inseto" = "biof_inseto",
                    "ruido_chuva" = "geof_chuva",
                    "ruido_vento" = "geof_vento",
                    "ruido" = "ruido_verificar",
                    "ruido_carro" = "antro_veiculo",
                    "ruido_defudo" = "ruido_defundo",
                    "ruido_aviao" = "antro_aviao",
                    "ruido_serra" = "antro_serra",
                    "ruido_vozes" = "biof_vozes",
                    "ruido_musica" = "antro_musica",
                    "gall_gall" = "biof_gallus_gallus",
                    "biog_gallus_gallus" = "biof_gallus_gallus",
                    "bof_inseto" = "biof_inseto",
                    "latido_cachorro"= "biof_cachorro",
                    "grall_grall" = "biof_gallus_gallus",
                    "ruido_ganso" = "biof_anser_anser",
                    "phys_cuvi" = "biof_anuro",
                    "ruidi_inseto" = "biof_inseto",
                    "biog_gallus_gallus" = "biof_gallus_gallus",
                    "biof_gallus_gallu" = "biof_gallus_gallus",
                    "antr_aviao" = "antro_aviao",
                    "antro_desconhecido" = "antro_NI",
                    "biof_anfibio" = "biof_anuro",
                    "biof_anfibios" = "biof_anuro",
                    "biof_gallus_galluss" = "biof_gallus_gallus",
                    "biof_bugio" = "biof_primata_alouatta",
                    "biof_callicebus_cf" = "biof_callicebus_nigrifons",
                    "biof_cabra" = "biof_caprino",
                    "biof_correnteza" = "geof_correnteza",
                    "biof_galinha" = "biof_gallus_gallus",
                    "biof_galinha_dangola" = "biof_numida_meeagris",
                    "biof_galinhadangola" = "biof_numida_meeagris",
                    "biof_galinha_angola" = "biof_numida_meeagris",
                    "antro_serra" = "antro_motosserra",
                    "biof_ineto_fraco" = "biof_inseto_fraco",
                    "biof_insetos" = "biof_inseto",
                    "biof_nymp_holl_calopsita" = "biof_nymphicus_hollandicus",
                    "biof_outro" = "biof_ni",
                    "biof_tambor" = "biof_tamborilado",
                    "biof_voz" = "biof_vozes",
                    "CFF" = "NI",
                    "ruido_inseto_forte" = "biof_inseto",
                    "ruido_vento" = "geof_vento",
                    "ruido_galo" = "biof_gallus_gallus",
                    "nano_bras"= "biof_anuro",
                    "ni_cff" = "NI",
                    "ni_cf" = "NI",
                    "ruido_verificar_alouatta" = "biof_alouatta",
                    "ruido_verificar_antropogenico" = "antro_NI",
                    "ruido_verificar_antropico" = "antro_NI",
                    "ruido_verificar_aviao" = "antro_aviao",
                    "ruido_verificar_avi�o" = "antro_aviao",
                    "ruido_verificar_cachorro" = "biof_cachorro",
                    "ruido_verificar_caminh�o" = "antro_veiculo",
                    "ruido_verificar_caminhao" = "antro_veiculo",
                    "ruido_verificar_carro" = "antro_veiculo",
                    "ruido_verificar_cavalo" = "biof_equino",
                    "biof_cavalo" = "biof_equino",
                    "biof_burro" = "biof_equino",
                    "ruido_verificar_chuv" = "geog_chuva",
                    "ruido_verificar_cigarra" = "biof_inseto_cigarra",
                    "biof_cigarra" = "biof_inseto_cigarra",
                    "ruido_verificar_cigarra_forte" = "biof_inseto_cigarra",
                    "ruido_verificar_correnteza" = "geof_correnteza",
                    "ruido_verificar_defundo" = "ruido_defundo",
                    "ruido_verificar_dendropsophus" = "biof_dendrosophus_sp",
                    "ruido_verificar_dendropsophus cf. minutus" = "biof_dendrosophus_sp",
                    "ruido_verificar_desconhecido" = "ruido_NI",
                    "ruido_verificar_galinha" = "biof_gallus_gallus",
                    "ruido_verificar_galinha-dangola" = "biof_numida_meeagris",
                    "ruido_verificar_galo" = "biof_gallus_gallus",
                    "ruido_verificar_moto" = "antro_moto",
                    "ruido_verificar_motosserra" = "antro_motoserra",
                    "antro_serra" = "antro_motosserra",
                    "ruido_verificar_ni" = "ruido_NI",
                    "ruido_verificar_outro" = "ruido_NI",
                    "ruido_verificar_ovelha" = "biof_caprino",
                    "ruido_verificar_passos" = "biof_passos",
                    "ruido_verificar_peru" = "biof_meleagris_gallopavo",
                    "ruido_verificar_pessoas" = "biof_vozes",
                    "ruido_verificar_primata" = "biof_primata",
                    "ruido_verificar_proceratophrys" = "biof_anuro_proceratophrys",
                    "ruido_verificar_serra" = "antro_motoserra",
                    "ruido_verificar_trovao" = "geof_trovao",
                    "ruido_verificar_veiculo" = "antro_veiculo",
                    "ruido_verificar_voz" = "biof_vozes",
                    "ruido_verificar_vozes" = "biof_vozes",
                    "biof_anuro_folhico" = "biof_anuro",
                    "geof_primata" = "biof_primata",
                    "ruido_galinha" = "biof_gallus_gallus",
                    "ruido_ruido_proceratophrys" = "biof_sapo_ruido_proceratophrys",
                    "ruido_pessoas" = "biof_vozes",
                    "ruido_motoserra" = "antro_motoserra",
                    "ruido_cavalo" = "biof_cavalo",
                    "biof_biof_gallus_gallus" = "biof_gallus_gallus",
                    "biof_cachorro_chorando" = "biof_cachorro",
                    "biof_callicebus_nigrifens" = "biof_callicebus_nigrifrons",
                    "biof_gallus_gallus_angola" = "biof_numida_meleagris",
                    "biof_gallus_gallus_dangola" = "biof_numida_meleagris",
                    "biof_gallus_gallusdangola" = "biof_numida_meleagris",
                    "biof_gallus_gallus-dangola" = "biof_numida_meleagris",
                    "biof_gallus_gallusdeangola" = "biof_numida_meleagris",
                    "biof_gallus_gaulls" = "biof_gallus_gallus",
                    "biof_ineto_fraco" = "biof_inseto_fraco",
                    "biof_inseto_muito_forte" = "biof_inseto_intenso",
                    "biof_madeira" = "biof_tamborilado",
                    "biof_vozeses" = "biof_vozes",
                    "ruido_autom�vel" = "antro_veiculo",
            
                    "ruido_defudo" = "ruido_defundo",
                    "ruido_facao" = "antro_facao",
                    "ruido_latido" = "cachorro",
                    "ruido_mosc" = "biof_mosc",
                    "ruido_trator" = "antro_trator",
                    "ruido_verificar" = "ruido_NI"
                    ))

#change genere names
tags_tab_2$species <- tags_tab_2$species  %>%
  str_replace_all(c("ruido_verificar_" = "ruido_",
                   "turd_sp" = "turdus_sp",
                    "picu_sp" = "picumnus_sp",
                    "brot_sp" = "brotogeris_sp",
                    "cyan_sp" = "cyanocorax_sp",
                    "lept_sp" = "leptotila_sp",
                    "pata_sp" = "patagioenas_sp",
                    "cola_campe" = "cola_camp",
                    "pene_sp" = "penelope_sp",
                    "phae_sp" = "phaethornis_sp",
                    "ramp_sp" = "ramphastos_sp",
                    "spor_sp" = "sporophila_sp",
                    "tang_sp" = "tangara_sp",
                    "todi_sp" = "todirostrum_sp",
                    "veni_sp" = "veniliornis_sp",
                    "amaz_sp" = "amazilia_sp",
                   "picumnus_spp" = "picumnus_sp"))

#change species names                    
tags_tab_2$species <- tags_tab_2$species %>%
  str_replace_all(c("thau_saya" = "tang_saya",
                    "tol_sulp" = "tolm_sulp",
                    "schi_viri" = "schi_vire",
                    "snix_spix" = "syna_spix",
                    "stri_gris" = "sitt_gris",
                    "todi_cene" = "todi_cine",
                    "basi_rufi" = "basi_culi",
                    "cryp+tata" = "cryp_tata",
                    "cyan_crys" = "cyan_chry",
                    "pinta_sulpp" = "pita_sulp",
                    "pinta_sulpp" = "pita_sulp",
                    "poec_plumb" = "poec_plum",
                    "pseu_guira" = "pseu_guir",
                    "rham_dico" = "ramp_dico",
                    "coni_ppec" = "coni_spec",
                    "cyan_chryt" = "cyan_chry",
                    "cyan_cryz" = "cyan_chry",
                    "habia_rubica" = "habi_rubi",
                    "lath_eule_cf" = "lath_eule",
                  
                    "lep_verr" = "lept_verr",
                    "letp_verr" = "lept_verr",
                    "NI_cf" = "NI",
                    "NI_NI" = "NI",
                    "phar_pret" = "phae_pret",
                    "phat_pret" = "phae_pret",
                    "phil_rufo" = "phil_rufu",
                    "pinta_sulpp" = "pita_silp",
                    "poec_plub" = "poec_plum",
                    "veniliornis_spil" = "veni_spil",
                    "turdus_NI" = "turd_sp",
                    "tudus_sp" = "turd_sp",
                    "torg_musc" = "trog_musc",
                    "tolm_suph" = "tolm_sulp",
                    "thrau_saya" = "thra_saya",
                    "thra_palm" = "tang_palm",
                    "thal_glauc" = "thal_glau",
                    "syna_front" = "syna_fron",
                    "sina_spix" = "syna_spix",
                    "chri_caud" = "chir_caud",
                    "cyan_cris" = "cyan_chry",
                    "cryp+tata" = "cryp_tata",
                    "cyan_crist" = "cyan_chry",
                    "tolm_sulph" = "tolm_sulp",
                    "tang_saqya" = "tang_saya",
                    "tang_saqya" = "tang_saya",
                    "pta_sulp" = "pita_sulp",
                    "pita_sulp" = "pita_sulp",
                    "pita_sul" = "pinta_sulp",
                    "proc_albi" = "proc_nudi",
                    "chlo_luci" = "chro_luci",
                    "chic_caud" = "chir_caud",
                    "cran_pali" = "cran_pall",
                    "pach_poli" = "pach_poly",
                    "picu_cirr" = "picumnus_sp",
                    "disy_ment" = "dysi_ment",
                    "myrm_lori" = "myrm_squa",
                    "myia_macu" = "myio_macu",
                    "clycl_guja" = "cycl_guja",
                    "spor_caue" = "spor_caer",
                    "sert_subc" = "serp_subc",
                    "pene_obso" = "pene_obsc",
                    "spil_gutt" = "psil_gutt",
                    "pata_caya" = "pata_caye",
                    "hemi_obso" = "hemi_diop",
                    "pene_obso" = "pene_obsc",
                    "thra_saya" = "tang_saya",
                    "basi_rufi" = "basi_culi",
                    "brot_quir" = "brot_chir",
                    "cari_crist" = "cari_cris",
                    "furn_ufu" = "furn_rufu",
                    "herps_rufi" = "herp_rufi",
                    "lept_verre" = "lept_verr",
                    "pata_caya" = "pata_caye",
                    "phys_cuvi" = "biof_anuro",
                    "tha_saya" = "tang_saya",
                    "todi_poly" = "todi_poli",
                    "atti_rudo" = "atti_rufu",
                    "atti_rufo" = "atti_rufu",
                    "basi_coli" = "basi_culi",
                    "brot_quir" = "brot_chir",
                    "cari_crist" = "cari_cris",
                    "chic_caud" = "chir_caud",
                    "cran_pali" = "cran_pall",
                    "cryp_parv." = "cryp_parv",
                    "cryp_tata." = "cryp+tata",
                    "cyck_guja" = "cycl_guja",
                    "disi_ment" = "dysi_ment",
                    "elae_obso" = "elae_obsc",
                    "spoc_nudi" = "proc_nudi",
                    "furn_ufu" = "furn_rufu",
                    "grall_vari" = "gral_vari",
                    "herp_rufi." = "herp_rufi",
                    "pach_poli" = "pach_poly",
                    "baso_culi" = "basi_culi",
                    "atuo_leuc" = "auto_leuc",
                    "cryp+tata" = "cryp_tata",
                    "cyan_chryt" = "cyan_chry",
                    "cylc_guja" = "cycl_guja",
                    "geog_chuva" = "geof_chuva",
                    "geot_aqui" = "geot_aequ",
                    "gnom_chop" = "gnor_chop",
                    "habia_rubi" = "habi_rubi"
                    ))




## anuros correções
tags_tab_2$species <- tags_tab_2$species %>%
  str_replace_all(c(#"ruido_inseto/morcego" = "biof_morcego",
                    "ruido_morcego" = "biof_morcego",
                    "ruido_cf_cachorro" = "biof_cachorro",
                    "ruido_uru capoeira" = "biof_ave",
                    #"anuro_sp?" = "NI1",
                    "anuro_sp" = "NI1",
                    "ruido_batida asa" = "biof_ave",
                    "boan_cf_beck" = "NI1",
                    "ruido_avio" = "antro_aviao",
                    "SP" = "NI1",
                    "ruido_motocicleta_CG_125" = "antro_moto",
                    "ruido_nseto" = "antro_martelada",
                    "ruido_canideo" =  "biof_cachorro",
                    "ruido_sp" = "ruido_NI",
                    "ruido_estranho" = "ruido_NI",
                    "ruiido_inseto" = "biof_inseto",
                    "ruido_sem_som" = "ruido_defundo",
                    #"ruido_inseto]" = "biof_inseto",
                    "ruido_tiro" = "antro_tiro",
                    "ruido_buzina" = "antro_buzina",
                    "ruido_moto_CG_125" = "antro_moto",
                    "ruido_avio_pousando" = "antro_aviao",
                    "ruido_motocicleta_1000_cc" = "antro_moto",
                    "ave" = "biof_ave",
                    "ruido_" = "ruido_NI",
                    "ruiso_inseto" = "biof_inseto",
                    "ruido_ineto" = "biof_inseto",
                    "ruideo_chuva" = "geof_chuva",
                    "ruido_insto" = "biof_inseto",
                    "ruido_foguete" = "antro_fogos_artificio",
                    "ruido_sino" = "biof_sino",
                    #"ruido_inseto[" = "biof_inseto",
                    "Ruido_trovao" = "geof_trovao",
                    "Ruido_vento" = "geof_vento",
                    "Ruido_gota" = "geof_chuva",
                    "ruido_gado" = "biof_bovino",
                    "ruido_latino" = "biof_cachorro",
                    "ruido_NI_geof_chuva" = "geof_chuva",
                    "ruido_NI_cachorro" = "biof_cachorro",
                    "ruido_NI_uru capoeira" = "biof_ave",
                    #"ruido_NI_trov�o" = "geof_trovao",
                    "ruido_NI_martelada" = "antro_martelada",
                    "ruido_NI_geof_chuva" = "geof_chuva",
                    #"ruido_NI_avia�" = "antro_aviao",
                    "aden_mar" = "aden_marm",
                    "aden_marmo" = "aden_marm",
                    "adenmarm" = "aden_marm",
                    "antro_aviao_pousando" = "antro_aviao",
                    "antro_moto_cg_125" = "antro_moto",
                    "antro_motocicleta_1000_cc" = "antro_moto",
                    "antro_motocicleta_cg_125" = "antro_moto",
                    "anuro_sp_biof_anuro" = "NI2",
                    "aplaleuc" = "apla_leuc",
                    "aplas_leuc" = "apla_leuc",
                    "ave" = "biof_ave",
                    "biof_anuro" = "NI3",
                    "biof_geof_chuva" = "geof_chuva",
                    #"biof_inseto/morcego" = "biof_morcego",
                    #"biof_inseto[" = "biof_inseto",
                    #"biof_inseto]" = "biof_inseto",
                    "biof_inseto_cigarra" = "biof_cigarra",
                    "boan_faber" = "boan_fabe",
                    "boanalbo" = "boan_albo",
                    "boanfabe" = "boan_fabe",
                    "boanpras" = "boan_pras",
                    "dend_min?" = "dend_minu",
                    "dend_nanus" = "dend_nanu",
                    "dendminu" = "dent_minu",
                    "isch_guent" = "isch_guen",
                    "isch_juip" = "isch_juip",
                    "ischguen" = "isch_guen",
                    "ischjuip" = "osch_juip",
                    "iscn_guent" = "iscn_guen",
                    "leptfusc" = "lept_fusc",
                    "phy_olfer" = "phyl_olfe",
                    "phylburm" = "phyl_burm",
                    "phys_olfer" = "phys_olfe",
                    "physcuvi" = "phys_cuvi",
                    "physolfe" = "phyl_olfe",
                    "proc_boiei" = "proc_boie",
                    "procboie" = "proc_boie",
                    "ruideo_geof_chuva" = "geof_chuva",
                    "ruido_NI_" = "ruido_NI",
                   # "ruido_NI_avia�" = "antro_aviao",
                    "ruido_NI_batida asa" = "biof_ave",
                    "ruido_NI_boi" = "biof_bovino",
                    "ruido_NI_buzina" = "antro_buzina",
                    "ruido_NI_cachoroo" = "biof_cachorro",
                    "ruido_NI_cachorro" = "biof_cachorro",
                    "ruido_NI_canideo" = "biof_cachorro",
                    "ruido_NI_conversa" = "antro_conversa",
                    "ruido_NI_estranho" = "ruido_NI",
                    "ruido_NI_foguete" = "antro_fogos",
                    "ruido_NI_geof_chuva" = "geof_chuva",
                    "ruido_NI_gota" = "geof_chuva",
                    "ruido_NI_grilo" = "biof_inseto",
                    "ruido_NI_ineto" = "biof_inseto",
                    "ruido_NI_insto" = "biof_inseto",
                    "ruido_NI_latido" = "biof_cachorro",
                    "ruido_NI_martelada" = "antro_matelada",
                    "ruido_NI_morcego" = "biof_morcego", 
                    "ruido_NI_morecego" = "biof_morcego",
                    "ruido_NI_musica" = "antro_musica",
                    "ruido_NI_nseto" = "biof_inseto",
                    "ruido_NI_sem_som" = "ruido_NI",
                    "ruido_NI_sino" = "antro_sino",
                    "ruido_NI_sp" = "biof_NI",
                    "ruido_NI_tiro" = "antro_tiro",
                   # "ruido_NI_trov�o" = "geof_trovao",
                    "ruido_NI_uru capoeira" = "biof_ave",
                    "ruiido_inseto" = "biof_inseto",
                    "ruiso_inseto" = "biof_inseto",
                    "scin_crosp" = "scin_cros",
                    #"scin_fusc???" = "scin_fusc",
                    "scin_fuscov" = "scin_fusc" ))






#cbro names 2015
tags_tab_2$species <- tags_tab_2$species %>%
  str_replace_all(c("arre_flav" = "arre_semi",
                    "trog_aedo" = "trog_musc",
                    "thra_saya" = "tang_saya"))

# others
#new05_data <- new04_data %>%
# str_replace_all(c( "?" = "NI"))



# atualiza a tabela completa com as novas tags e salva em cima
TAB.juncao <- tags_tab_2 %>% 
  dplyr::select(filename, species) %>% 
  dplyr::arrange(filename)

TAB.2 <- TAB %>% 
  dplyr::arrange(filename)
TAB.2$species <- TAB.juncao$species

write.csv2(TAB.2, "..//00_complete_info_TAB_all_tags_compilation.csv", sep="\t", na="NA", quote=FALSE, row.names = F, col.names= T)

#save
write.csv2(TAB.juncao, "..//001_tabela_etiquetas_2019_09_d10_FULL.csv", col.names=T, row.names=F, append=F, sep="\t", quote=F)
write.csv2(as.data.frame(files.with.troubles), "..//02_tabela_etiquetas_2019_09_d10_problemas.csv", col.names=T, row.names=F, append=F, sep="\t", quote=F)
write.csv2(as.data.frame(files.with.diff.colnames), "..//03_tabela_etiquetas_2019_09_d10_ColunasChecars.csv", col.names=T, row.names=F, append=F, sep="\t", quote=F)

colnames(TAB.juncao)
TAB.juncao <- TAB.2
#create datafram with freq of species for each pesquisador
etiquetacao.frequencia<-as.data.frame(table(TAB.juncao$pesquisador, TAB.juncao$filename))
etiquetacao.frequencia<-subset(etiquetacao.frequencia, Freq>0)
colnames(etiquetacao.frequencia)<-c("pesquisador", "filename", "Freq")

etiquetacao.frequencia.stat<-aggregate(Freq~pesquisador, etiquetacao.frequencia, sum)
colnames(etiquetacao.frequencia.stat)<-c("pesquisador", "etiquetas")
etiquetacao.frequencia.stat$numfiles<-aggregate(Freq~pesquisador, etiquetacao.frequencia, length)[,2]
etiquetacao.frequencia.stat


etiquetas.tipos<-as.data.frame(table(TAB.juncao$species, TAB.juncao$pesquisador))
colnames(etiquetas.tipos)<-c("species", "pesquisador", "Freq")
etiquetas.tipos<-etiquetas.tipos[order(etiquetas.tipos$species),]
etiquetas.tipos<-subset(etiquetas.tipos, Freq>0)
head(etiquetas.tipos)


## prepar table quantidade de tags   ### ta meio zuado ainda isso!!!!!!!!!!!!!
etiquetas.tipos.wide<-reshape(etiquetas.tipos, v.names = "Freq", idvar = "species",
                              timevar = "pesquisador", direction = "wide")
head(etiquetas.tipos.wide)

write.csv2(etiquetas.tipos.wide, "..//04_tab_quantidade_de_tags_por_pesquisador.csv", col.names=T, row.names=F, append=F, sep="\t", quote=F)

######## preparing for biodiversity analysis!!!! #####

TAB.juncao

#directory
#setwd("/media/lucas/743AA7C23AA7802A/00_EXTERN_HD/ETIQUETAS/2020_03_d16_ETIQUETAS_2020_aves_ok")

#list files for pattern
#dir.txts.all_part2<-dir(".", pattern=".txt$")
#n.rec = length(dir.txts.all_part2)
#n.rec

#head(dir.txts.all_part2)

#prepare complete list of species
#for (i in dir.txts.all_part2){
#aux_part2 <- read.table(i, head=T, sep="\t", na.strings = c("NULL","NA",""))      #leia i como tabela
#aux$filename<-i
#aux2<-subset(aux, select=c(filename, species))                 ################ all lower case                                           
#lista.completa<-data.frame(rbind(lista.completa, aux2))}



#check erros names

tags_check <- TAB.juncao$species
tags_check

#open csv and confer
write.csv2(tags_check, "..//05_tags_check.csv", col.names=T, row.names=F, append=F, sep="\t", quote=F)

#correc names

tags_check_finish <- TAB.juncao$species %>%
  str_replace_all(c("AMG153" = "NI",
                    "amo_hume" = "ammo_hume",
                    "geof_gado" = "biof_gado",
                    "biof_geof_chuva" = "geof_chuva",
                    "antro_ni_defundo" = "ruido_defundo",
                    "base_culi" = "basi_culi",
                    "biof_anuros" = "biof_anuro",
                    "biof_gallus_gallus_Dangola" = "biof_numida_meleagris",
                    "biof_tamboriladoilado" = "biof_tamborilado",
                    "biof_voo_inseto" = "biof_inseto_voo",
                    "carneiro" = "biof_caprino",
                    "colu_tapl" = "colu_talp",
                    "khs_070" = "NI",
                    "khs_071" = "NI",
                    "khs_090" = "NI",
                    "khs_091" = "NI",
                    "khs_106" = "NI",
                    "khs_140" = "NI",
                    "N1" = "NI",
                    "NI1" = "NI",
                    "NI2" = "NI",
                    "NI3" = "NI",
                    "pica_sp" = "picumnus_sp",
                    "pinta_sulpp" = "pita_sulp",
                    "rw_014" = "NI",
                    "rw_255" = "NI",
                    "rw_256" = "NI",
                    "rw_257" = "NI",
                    "rw_302" = "NI",
                    "rw_303" = "NI",
                    "rw_304" = "NI",
                    "rw_305" = "NI",
                    "rw_306" = "NI",
                    "rw_307" = "NI",
                    "rw_308" = "NI",
                    "rw_309" = "NI",
                    "rw_309" = "NI",
                    "rw_309" = "NI",
                    "rw_309" = "NI",
                    "rw_310" = "NI",
                    "rw_311" = "NI",
                    "rw_313" = "NI",
                    "rw_314" = "NI",
                    "rw_315" = "NI",
                    "rw_316" = "NI",
                    "rw_317" = "NI",
                    "rw_318" = "NI",
                    "rw_319" = "NI",
                    "rw_320" = "NI",
                    "rw_321" = "NI",
                    "rw_322" = "NI",
                    "rw_333" = "NI",
                    "rw_334" = "NI",
                    "rw_335" = "NI",
                    "rw_336" = "NI",
                    "rw_337" = "NI",
                    "rw_338" = "NI",
                    "rw_339" = "NI",
                    "rw_340" = "NI",
                    "rw_341" = "NI",
                    "rw_342" = "NI",
                    "rw_343" = "NI",
                    "rw_344" = "NI",
                    "rw_390" = "NI",
                    "rw_391" = "NI",
                    "rw_392" = "NI",
                    "rw_393" = "NI",
                    "rw_394" = "NI",
                    "rw_395" = "NI",
                    "rw_396" = "NI",
                    "ruido_chuva" = "geof_chuva",
                    "ruido_vaca" = "biof_gado",
                    "Thra_saya" = "tang_saya",
                    "turd_sp" = "turdus_sp",
                    "antro_veiculos" = "antro_veiculo",
                    "cryp+tata" = "cryp_tata",
                    "lembra o hypo gutt, mas nao eee" = "NI",
                    "ruido_NI_aviao" = "antro_aviao",
                    "geof_geof_chuva" = "geof_chuva",
                    "ni1" = "NI",
                    "ni2" = "NI",
                    "ruido_mosca" = "inseto",
                    "geof_gado" = "biof_gado",
                    "geof_aviao" = "biof_aviao",
                    "Zono_cape" = "zono_cape",
                    "zono_Cape" = "zono_cape",
                    "piry_leuc" = "pyri_leuc",
                    "ruido_ni_ganso" = "biof_anser_anser",
                    "ruido_ni_callicebus" = "biof_callicebus_nigrifrons",
                    "ruido_ni_geof_chuva_fraca" = "geof_chuva_fraca"))
TAB.juncao$species <- tags_check_finish
head(TAB.juncao)


#check again
#open csv and confer
write.csv2(TAB.juncao$species, "..//06_tags_check_again.csv", col.names=T, row.names=F, append=F, sep="\t", quote=F)

#
complete.list <- subset(TAB.juncao, select=c(filename, species))
head(complete.list)

complete.list$presaus<-1


#o v.names ? como eu quero completar minha matriz, "idvar" o que vai na coluna A; "TIMEVAR" na primeira linha;
complete.list.wide<-reshape(complete.list, v.names ="presaus", idvar = "filename",
                             timevar = "species", direction = "wide")
#remove presaus of name
colnames(complete.list.wide)<-gsub("presaus\\.", "", colnames(complete.list.wide))


colnames(complete.list.wide)<-tolower(colnames(complete.list.wide))
complete.list.wide[is.na(complete.list.wide)]<-0


#salvando
write.csv2(complete.list.wide, "..//0000_matriz_wide_species_biodiversity.csv")


