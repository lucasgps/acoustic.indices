
#' ---
#' title: number of filess
#' author: lucas gaspar
#' date: 2020-06-d19
#' ---

# clean
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

# directory
setwd("/home/gasparzinho/Documents/backpup/ANALISYS_2020_06_d01/00_scripts_gaspar_master/after_meet/aves/00_after_quali/by_env")
dir()


colnames(save_mestra)
## import data ----------------------------------------------------------
mestra <- readr::read_csv2("00_matriz_wide_SPECIES_biodiversity_morning.csv")
mestra

acoustic <- readr::read_csv2("00_mestra_AI.csv")
acoustic

#' prepar tables ----

# merge acoustic indices
mestra_acoustic <- mestra %>% 
  dplyr::inner_join(acoustic, by = "filename")

#import forest data
forest <- read.csv2( "00_metadata_forest_coord_ccm2.csv" )
forest

### 1km ---------------------------
forest$Flo_1km <- as.numeric(forest$Flo_1km)

# round decimal numbers
forest_selec <- forest %>% 
  dplyr::select(codigo,Flo_1km) %>% 
  dplyr::mutate_at(vars(Flo_1km), funs(round(.,2))) %>% 
  tidyr::separate(codigo, c("landscape", "env"))
forest_selec

# format for enter in dplyr
forest_selec <- as_tibble(forest_selec)

# average of forest cover by landscape
forest_selec_mean <- forest_selec  %>% 
  dplyr::select( landscape, Flo_1km) %>% 
  dplyr::group_by(landscape) %>%
  dplyr::summarize(Flo_1km_lsmean = mean(Flo_1km)) %>% 
  dplyr::arrange(Flo_1km_lsmean)
forest_selec_mean

forest_selec_mean$landscape

# merge forest cover in orginal table by landscape
mestra_acoustic_forest_ls <- mestra_acoustic %>% 
  dplyr::right_join(forest_selec_mean, by = "landscape" ) %>% 
  dplyr::mutate_at(vars(Flo_1km_lsmean), funs(round(.,2)))
colnames(mestra_acoustic_forest_ls)

# average of forest cover by landscape
forest_selec_mean_code <- forest_selec  %>% 
  dplyr::mutate(code = paste(landscape, env, sep = "_")) %>% 
  dplyr::select( code, Flo_1km) %>% 
  dplyr::group_by(code) %>%
  dplyr::summarize(Flo_1km_site = mean(Flo_1km)) %>% 
  dplyr::arrange(Flo_1km_site)
forest_selec_mean_code

# merge forest cover in orginal tableby code
mestra_acoustic_forest_ls_code <- mestra_acoustic_forest_ls %>% 
  dplyr::mutate(code = paste(landscape, env, sep = "_")) %>% 
  dplyr::right_join(forest_selec_mean_code, by = "code" ) %>% 
  dplyr::mutate_at(vars(Flo_1km_lsmean), funs(round(.,2)))
colnames(mestra_acoustic_forest_ls_code)

## 2km ------------
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

# merge forest cover in orginal table by landscape
mestra_acoustic_forest_ls <- mestra_acoustic_forest_ls_code %>% 
  dplyr::right_join(forest_selec_mean, by = "landscape" ) %>% 
  dplyr::mutate_at(vars(Flo2km_lsmean), funs(round(.,2)))
colnames(mestra_acoustic_forest_ls)

# average of forest cover by landscape
forest_selec_mean_code <- forest_selec  %>% 
  dplyr::mutate(code = paste(landscape, env, sep = "_")) %>% 
  dplyr::select( code, Flo2km) %>% 
  dplyr::group_by(code) %>%
  dplyr::summarize(Flo2km_site = mean(Flo2km)) %>% 
  dplyr::arrange(Flo2km_site)
forest_selec_mean_code

# merge forest cover in orginal tableby code
mestra_acoustic_forest_ls_code_new <- mestra_acoustic_forest_ls %>% 
  dplyr::right_join(forest_selec_mean_code, by = "code" ) %>% 
  dplyr::mutate_at(vars(Flo2km_site), funs(round(.,2)))
colnames(mestra_acoustic_forest_ls_code_new)


#' end prepar table
#' begin amount visualization 

setwd("/home/gasparzinho/Documents/backpup/ANALISYS_2020_06_d01/00_scripts_gaspar_master/after_meet/aves/00_after_quali/by_env")
save_mestra <- mestra_acoustic_forest_ls_code_new %>% 
  dplyr::select(filename:Flo2km_site)
write.csv2(save_mestra, "mestra_acoustic_forest_ls_code_new.csv")

# amount minutes by landscape
ta_ls <- ta %>% 
  dplyr::select (se, landscape) %>%
  dplyr::group_by(landscape) %>% 
  dplyr::summarise_each(sum)
ta_ls

mean_by_landscape <- sum(ta_ls$se)/nrow(ta_ls)
mean_by_landscape #116

plot.dir <- "./plots/gerais"


vetor <- as.vector(ta_ls$se)
vetor
sd(vetor)
summary(vetor)

# number of minutes per landscape
ggplot(data=ta_ls, aes(x=landscape , y=se)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=se), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  xlab("Landscape") + ylab("nº of minutes")  
ggsave("barplot_minutes_by_landscape.png", path = plot.dir)

# tags number os taxas
colnames(ta)

ta_rich <- ta %>% 
  dplyr::select(landscape,agel_cyan:zono_cape) %>% 
  dplyr::group_by(landscape) %>% 
  dplyr::summarise_each(sum) 

ta_rich <- ta_rich %>% 
  dplyr::mutate(RICH = specnumber(ta_rich))
ta_rich$RICH

summary(ta_rich$RICH)
sd(ta_rich$RICH)

ta_abund <- ta %>% 
  dplyr::select(landscape,agel_cyan:zono_cape) %>% 
  dplyr::group_by(landscape) %>% 
  dplyr::summarise_each(sum) 
ta_abund

abund <- rowSums(ta_abund[,2:ncol(ta_abund)])
abund 


summary(abund)
sd(abund)
# richness average by landscape
sum(ta_rich$RICH)/22
# number of NI tags
number_tags_ni <- sum(ta$NI)
number_tags_ni # 808

# number of taxon tags
ta_sp <- ta %>% 
  dplyr::select(agel_cyan:zono_cape)
ta_sp

n_tags_by_taxa <- colSums(ta_sp)
n_tags_by_taxa

summary(as.vector(n_tags_by_taxa))
sd(as.vector(n_tags_by_taxa))

sum(as.vector(n_tags_by_taxa))

species_number <-  ncol(ta_sp[ , !grepl( "_sp", names( ta_sp))])
species_number # 199

species_number_tag <- sum(rowSums(ta_sp[ , !grepl( "_sp", names( ta_sp))]))
species_number_tag # 9278

genero_number <-  ncol(ta_sp[ , grepl( "_sp" , names( ta_sp ) ) ])
genero_number # tres não especies com sp... entao total de genro = 15

genero_number_tag <- sum(rowSums(ta_sp[ , grepl( "_sp" , names( ta_sp ) ) ]))
genero_number_tag # 351

total_number_tag <- number_tags_ni + genero_number_tag + species_number_tag
total_number_tag # 10437

total_number_tag_no_ni <-  genero_number_tag + species_number_tag
total_number_tag_no_ni # 9629

# most abundance species
head(sort(n_tags_by_taxa, decreasing = T))

# number of minutes per environment
ta_env <- ta %>% 
  dplyr::select (se, env) %>% 
  dplyr::group_by(env) %>% 
  dplyr::summarise_each(sum)
ta_env

ta_env$env <- str_replace_all(ta_env$env, "aa", "pasture")
ta_env$env <- str_replace_all(ta_env$env, "br", "swamp")
ta_env$env <- str_replace_all(ta_env$env, "ma", "forest")
ta_env

#pasture 883 minutes
#swamp 746 minutes
# forest 934 minutes

mean <- sum(ta_env$se)/nrow(ta_env)
mean #854

# number of minute per environment
ggplot(data=ta_env, aes(x=env, y=se)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=se), vjust=1.6, color="white", size=3.5)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1)) +
  xlab("Environment") + ylab("nº of minutes")  
ggsave("barplot_minutes_by_environment.png", path = plot.dir)

# number of minutes per site
ta_cod <- ta %>%
  dplyr::mutate(codigo = paste(landscape, env, sep = "_")) %>% 
  dplyr::select (se, codigo) %>% 
  dplyr::group_by(codigo) %>% 
  dplyr::summarise_each(sum) %>% 
  dplyr::arrange(-se)
  ta_cod 

mean <- sum(ta_cod$se)/nrow(ta_cod)
mean #39

ggplot(data=ta_cod , aes(x=codigo, y=se)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=se), vjust=2.2, color="black", size=3.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  xlab("sampling point") + ylab("nº of minutes")  
ggsave("barplot_minutes_by_site.png", path = plot.dir)

# biodiveristy birdsa tags
setwd("./birds")
dir()

# species tags by environment
tags_by_sp_by_env <- NULL

ta_env <- ta %>%
  dplyr::select(env, agel_cyan:zono_cape) %>% 
  dplyr::group_by(env) %>% 
  dplyr::summarize_each(sum)
ta_env

col <- colnames(ta_env)
aa<- paste(ta_env[1,2:ncol(ta_env)], rownames(FALSE))
br<- paste(ta_env[2,2:ncol(ta_env)], rownames(FALSE))
ma<- paste(ta_env[3,2:ncol(ta_env)], rownames(FALSE)) 
new <- cbind(colnames(ta_env[,2:ncol(ta_env)]), aa, br, ma)
head(new)
ta_env_long <- as.data.frame(new)
head(ta_env_long) 
ta_env_long[is.na(ta_env_long)] <- 0

ta_env_long$aa <- as.numeric(ta_env_long$aa)
ta_env_long$br <-  as.numeric(ta_env_long$br)
ta_env_long$ma <- as.numeric(ta_env_long$ma)

long <- ta_env_long %>% 
  dplyr::rename(sp = V1) %>% 
  dplyr::mutate(total = rowSums(ta_env_long[,2:4]))
long

write.csv2(long, "01_species_tags_quatidade_2020_10_d05")

plot.dir <- "../plots/gerais"

ggplot(data=long , aes(x=reorder(sp, -total), y=total)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=""), vjust=2.2, color="black", size=0.5)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 7)) +
  xlab("Species") + ylab("nº of tags")  
ggsave("barplot_tags_species.png",  width = 100, height = 100, units = "cm" , limitsize = FALSE, path = plot.dir)

# merge with taxonomic table
ta_names <- read.csv2("00_species_names_for_merge.csv")
ta_names 

new <- ta_names %>% 
  dplyr::inner_join(long, by = "sp") %>% 
  dplyr::select(seq, name_in_taxon_order, sp, ma, aa, br, total )
new

# save
write.csv2(new, "02_species_tags_quatidade_names_2020_10_d05.csv")

merge <- read.csv2("00_merge_env_english_names_mod.csv")

new <- new %>% 
  dplyr::select(name_in_taxon_order, ma, aa, br, total) %>% 
  dplyr::rename(Scientific.Name = name_in_taxon_order )


merge_tags <- merge %>% 
  dplyr::full_join(new, by = "Scientific.Name")
write.csv2(merge_tags, "03_publish_merge_names_tags_2020_10_d05.csv")

# by environment
ta_long <- tidyr::gather(ta, key = sp, value = tags, NI:zono_cape, factor_key=TRUE )
ta_long[is.na(ta_long)]<-0
colnames(ta_long)

ta_long_gro <- ta_long %>% 
  dplyr::group_by(env) %>% 
  dplyr::summarise(tags= sum(tags))
ta_long_gro

ta_long_gro$env <- str_replace_all(ta_long_gro$env, "aa", "pasture")
ta_long_gro$env <- str_replace_all(ta_long_gro$env, "br", "swamp")
ta_long_gro$env <- str_replace_all(ta_long_gro$env, "ma", "forest")
ta_long_gro
#aa 4083
#br 2560
#ma 3794

# number of minute per environment
ggplot(data=ta_long_gro, aes(x=env, y=tags)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=tags), vjust=1.6, color="white", size=3.5)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1)) +
  xlab("Environment") + ylab("nº of species tagg")  
ggsave("barplot_tagg_number_by_environment.png", path = plot.dir)

# total
setwd("../")
dir()

ta <- read.csv2("00_matriz_wide_SPECIES_biodiversity_informations.csv")
ta
colnames(ta)

ta_sum <- ta %>% 
  dplyr::select(NI,agel_cyan:zono_cape)

soma <- colSums(ta_sum)
species <- colnames(ta_sum)

new <- as.data.frame(cbind(species, soma))
new

setwd("./birds")
write.csv2(new, "number_of_tags_birds_2020_10_d05.csv")

new <- read.csv2("number_of_tags_birds_2020_10_d05.csv")
ggplot(data=new , aes(x=reorder(species, -soma), y=soma)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=""), vjust=2.2, color="black", size=0.5)+
  xlab("Species Rank") + ylab("nº of tags")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1,size = 7 ))
ggsave("barplot_tagg_number_species.png", path = plot.dir)


# end ---------------------------------------------------------------------
#' Dissimilarity Jaccard

# table to landscape
# prepar table
colnames(save_mestra)
mestra_group_ls <- save_mestra %>% 
  dplyr::select(landscape, agel_cyan:zono_cape) %>%   # sem NI
  dplyr::group_by(landscape) %>% 
  dplyr::summarise_each(sum) %>% 
  dplyr::distinct(landscape, .keep_all = TRUE ) 
head(mestra_group_ls )

# change value per 0 and 1
mestra_group_ls <- data.frame(mestra_group_ls)
colnames(mestra_group_ls)
logical <- data.frame(mestra_group_ls[,2:ncol(mestra_group_ls)] >= 1)
col <- sapply(logical, is.logical)
logical[,col] <- lapply(logical[,col], as.numeric)
logical

new_mestra_ls <- NULL
new_mestra_ls <- cbind(mestra_group_ls$landscape, logical)
new_mestra_ls

# table to site
# prepar table
colnames(save_mestra)

mestra_group_cod <- save_mestra %>% 
  dplyr::mutate(codigo = paste(landscape, env, sep = "_")) %>% 
  dplyr::select(codigo, agel_cyan:zono_cape) %>% 
  dplyr::arrange(codigo) %>% 
  dplyr::group_by(codigo) %>% 
  dplyr::summarise_each(sum) %>% 
  dplyr::distinct(codigo, .keep_all = TRUE ) 
head(mestra_group_cod)

# change value per 0 and 1
mestra_group_cod <- data.frame(mestra_group_cod)
logical <- data.frame(mestra_group_cod[,2:ncol(mestra_group_cod)] >= 1)
col <- sapply(logical, is.logical)
logical[,col] <- lapply(logical[,col], as.numeric)
logical

new_mestra_cod <- NULL
new_mestra_cod <- cbind(mestra_group_cod$codigo, logical)
new_mestra_cod

#' Jaccard index ----

# codigo - localidade
numeric_cdo <- new_mestra_cod[,1:ncol(new_mestra_cod)]

distance <- vegdist(new_mestra_cod[,2:ncol(new_mestra_cod)], method="jaccard", na.rm = TRUE, binaty = TRUE)
cluster <- hclust(distance)
cluster$cod <- mestra_group_cod$codigo
ggdendrogram(cluster , rotate = TRUE, size = 2)

plot(cluster, hang = -1,
     main = "Cluster Dendrogram by Jaccard", sub = NULL,
     xlab = "Site Sampling", ylab = "Height", leaflab = ls)

# landscape
numeric_ls <- new_mestra_ls[,2:ncol(new_mestra_ls)]

ls <- new_mestra_ls$landscape

distance <- vegdist(new_mestra_ls[,2:ncol(new_mestra_ls)] , method="jaccard", na.rm = TRUE, binaty = TRUE)
cluster <- hclust(distance)

plot(cluster, labels = NULL, hang = -1,
     main = "Cluster Dendrogram by Jaccard", sub = NULL,
     xlab = "Landscape", ylab = "Height", leaflab = ls)
# end ------------------------------------------------------------------------------------------------------------
