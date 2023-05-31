rm(list=ls())
library(insee)
library(doremifasol)
library(eurostat)
library(OECD)
library(plyr)
library(dplyr)
library(ggplot2)
library(readxl) 
library(tsibble)
library(tibble)
library(janitor)
library(tsbox)
library(knitr)
library(tempdisagg)
library(labelled)
library(stringr)
library(Hmisc)
library(lubridate)
library(conflicted)
library(base)
library(tidyr)
library(cowplot)
library(ggpubr)
library(cowplot)
library(lubridate)
conflict_prefer("plot_grid", "cowplot")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("replace", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("relocate", "dplyr")

#Définition du chemin d'accès
#setwd("C:/Users/144313/Documents/GitHub/stage_ofce")
setwd <- setwd("C:/Users/gasto/Documents/GitHub/Statjunior/projets-r-statjunior/decomposition_ipc/")
getwd()

##Date de la base 100 
date_base100 <- '2021-06-01'
date_base100_bis <- '2018-12-01'
scale_ipc1_base100 <- function(x, na.rm = FALSE) x/x[analyse_ipc1[,1] == date_base100]*100
scale_ipc2_base100 <- function(x, na.rm = FALSE) x/x[analyse_ipc2[,1] == date_base100]*100
scale_ipc2_bis_base100 <- function(x, na.rm = FALSE) x/x[analyse_ipc2_bis[,1] == date_base100_bis]*100
scale_ipc3_base100 <- function(x, na.rm = FALSE) x/x[analyse_ipc3[,1] == date_base100]*100
scale_ipc4_base100 <- function(x, na.rm = FALSE) x/x[analyse_ipc4[,1] == date_base100]*100



list_dataset <- get_dataset_list()
#idbank_ipc = get_idbank_list("IPC-2015")

# list_idbank_ipc1 =
#   get_idbank_list("IPC-2015") %>%
#   filter(NATURE == "INDICE") %>% #indice
#   filter(MENAGES_IPC == "ENSEMBLE") %>%  #ensemble
#   filter(REF_AREA == "FE") %>% #france entiere
#   filter(FREQ_label_fr == "Mensuelle")  %>%  #mensuelle
#   filter(COICOP2016_label_fr == "00 - Ensemble" | COICOP2016_label_fr ==  "01 - Produits alimentaires et boissons non alcoolisées" | COICOP2016_label_fr ==  "02 - Boissons alcoolisées, tabac et stupéfiants"|
#            COICOP2016_label_fr ==  "03 - Articles d'habillement et chaussures" | COICOP2016_label_fr ==  "04 - Logement, eau, gaz, électricité et autres combustibles" | COICOP2016_label_fr ==  "05 - Meubles, articles de ménage et entretien courant du foyer"|
#            COICOP2016_label_fr ==  "06 - Santé"| COICOP2016_label_fr ==  "07 - Transports"| COICOP2016_label_fr ==  "08 - Communications"|
#            COICOP2016_label_fr ==  "09 - Loisirs et culture" | COICOP2016_label_fr ==  "10 - Enseignement"| COICOP2016_label_fr ==  "11 - Restaurants et hôtels"|
#            COICOP2016_label_fr ==  "12 - Biens et services divers")

list_idbank_ipc2 = 
  get_idbank_list("IPC-2015") %>% 
  filter(NATURE == "INDICE") %>% #indice
  filter(MENAGES_IPC == "ENSEMBLE") %>%  #ensemble
  filter(REF_AREA == "FE") %>% #france entiere
  filter(FREQ_label_fr == "Mensuelle")  %>%  #mensuelle
  filter(PRIX_CONSO_label_fr !="Ensemble, y compris loyers fictifs") 

list_idbank_ipc3 = 
  get_idbank_list("IPC-2015") %>% 
  filter(NATURE == "INDICE") %>% #indice
  filter(MENAGES_IPC == "ENSEMBLE") %>%
  mutate(code_coicop = substr(COICOP2016_label_fr, 1, 2)) %>%  #ensemble
  filter(REF_AREA == "FE") %>% #france entiere
  filter(FREQ_label_fr == "Mensuelle")  %>% 
  filter(code_coicop == "01")

list_idbank_ipc4 = 
  get_idbank_list("IPC-2015") %>% 
  filter(NATURE == "INDICE") %>% #indice
  filter(MENAGES_IPC == "ENSEMBLE") %>%
  mutate(code_coicop = substr(COICOP2016_label_fr, 1, 2)) %>%  #ensemble
  filter(REF_AREA == "FE") %>% #france entiere
  filter(FREQ_label_fr == "Mensuelle")  %>% 
  filter(code_coicop == "04")


list_idbank_pond_ipc= 
  get_idbank_list("IPC-2015") %>% 
  filter(NATURE == "POND") %>% 
  filter(PRIX_CONSO_label_fr !="Ensemble, y compris loyers fictifs" & PRIX_CONSO_label_fr !="Sans objet")


# idbank_ipc1 = list_idbank_ipc1 %>% pull(idbank)
idbank_ipc2 = list_idbank_ipc2 %>% pull(idbank)
idbank_ipc3 = list_idbank_ipc3 %>% pull(idbank)
idbank_ipc4 = list_idbank_ipc4 %>% pull(idbank)
idbank_pond_ipc = list_idbank_pond_ipc %>% pull(idbank)
# data_ipc1 =
#   get_insee_idbank(idbank_ipc1) %>%
#   split_title()

data_ipc2 = 
  get_insee_idbank(idbank_ipc2) %>% 
  split_title()

data_ipc3 = 
  get_insee_idbank(idbank_ipc3) %>% 
  split_title()

data_ipc4 = 
  get_insee_idbank(idbank_ipc4) %>% 
  split_title()

data_pond_ipc = 
  get_insee_idbank(idbank_pond_ipc) %>% 
  split_title()

isj <- get_insee_idbank('001768593')  
isj <- isj |>
  select(DATE, OBS_VALUE) %>% 
  rename("gliss_annuel_isj" = "OBS_VALUE") 
  
# #Indice mensuel par catégorie COICOP + Ensemble en base 100 en juin 2021
# analyse_ipc1 <- filter(data_ipc1, DATE >= as.Date(date_base100) &
#                        TITLE_FR5 !="Ensemble y compris loyers fictifs") %>%
#   mutate(TITLE_FR6 = ifelse(TITLE_FR5 == "Ensemble", "Ensemble", TITLE_FR6)) %>%
#   select(DATE, OBS_VALUE, TITLE_FR6) %>%
#   pivot_wider(names_from = TITLE_FR6, values_from = OBS_VALUE) %>%
#   arrange(DATE) %>%
#   mutate_at(vars(-1), scale_ipc1_base100) %>%
#   pivot_longer(cols=c(-1), names_to = "Composante", values_to = "value")
# 
# 
# plot_ipc1 <- ggplot(data=analyse_ipc1, aes(x=DATE, y=value, fill = Composante)) +
#   geom_line(aes(col = Composante)) + geom_point(aes(col = Composante)) +
#   labs(y = "Base 100 en juin 2021", x = "Mois", title = "Evolution des composantes de l'inflation depuis juin 2021") +
#   #scale_fill_manual(values = c("blue", "red", "brown", "purple", "green", "black")) +
#   #geom_line(aes(y= ipch_glob), color = "black", size = 1.02) +
#   #facet_wrap( ~ Country) +
#   theme(legend.position="bottom", legend.text = element_text(size = 7)) +
#   theme_bw()
# plot_ipc1


analyse_ipc2 <- filter(data_ipc2, DATE >= as.Date(date_base100)) %>% 
  filter(TITLE_FR5 == "Services" |
  TITLE_FR5 == "Tabac" |
  TITLE_FR5 == "Alimentation" |
  TITLE_FR5 == "Énergie" |
  TITLE_FR5 == 'Produits manufacturés'|
  TITLE_FR5 == 'Ensemble') %>% 
  filter(TITLE_FR1 == "Indice des prix à la consommation") %>% 
  select(DATE, OBS_VALUE, TITLE_FR5) 

analyse_ipc2 <- pivot_wider(analyse_ipc2, names_from = TITLE_FR5, values_from = OBS_VALUE) %>% 
  arrange(DATE)
analyse_ipc2 <- mutate_at(analyse_ipc2, vars(-1), scale_ipc2_base100) 

analyse_ipc2 <- pivot_longer(analyse_ipc2, cols=c(-1), names_to = "Composante", values_to = "value")
analyse_ipc2 <- analyse_ipc2 |>
  group_by(Composante) |>
  arrange(DATE) |>
  mutate(gliss_annuel = value/lag(value, 12)-1)

plot_ipc2 <- ggplot(data=analyse_ipc2, aes(x=DATE, y=value, fill = Composante)) +
  geom_line(aes(col = Composante), size =1.1)  +
  labs(title = "Evolution des composantes de l'inflation depuis juin 2021", 
      subtitle = "Lecture : Au mois d'avril 2023, les prix alimentaires ont augmenté de 20 % par rapport à juin 2021. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
      y = "Base 100 en juin 2021", x = "Mois") +
  scale_color_manual(values = c("red", "#C77CFF", "blue", "#00BA38",  "orange",  "#D39200"))+
  geom_line(data = filter(analyse_ipc2, Composante == "Ensemble"), size = 1.5, color = "blue") +
  scale_linetype_manual(values=c("solid", "solid","twodashed","solid","solid","solid"))+
  theme_grey()+
  theme(legend.position="bottom",
    plot.title = element_text(color = "#333333", size = 18, face = "bold"),
    legend.title = element_text(size=9), 
    legend.text = element_text(size=12),
    plot.subtitle = element_text(color = "#333333", face = "italic")
  )
plot_ipc2



###Analyse des prix alimentaires 
analyse_ipc3 <- filter(data_ipc3, DATE >= as.Date(date_base100)) %>% 
  select(DATE, OBS_VALUE, TITLE_FR6, TITLE_FR5) 
analyse_ipc3$TITLE_FR5 <-gsub("Nomenclature Coicop :", "", analyse_ipc3$TITLE_FR5)
analyse_ipc3$TITLE_FR5 <- gsub("^\\s+|\\s+$", "", analyse_ipc3$TITLE_FR5) 
analyse_ipc3$TITLE_FR6 <- gsub("Produits alimentaires n.c.a.", "Produits alimentaires non classés ailleurs", analyse_ipc3$TITLE_FR6) 
analyse_ipc3$alim_restriction <- substr(analyse_ipc3$TITLE_FR5, 1, 6) 
analyse_ipc3 <- mutate(analyse_ipc3, verif = ifelse(TITLE_FR5 == alim_restriction,1,0)) |>
  filter(verif == 1) |>
  select(-TITLE_FR5, -verif, -alim_restriction)

analyse_ipc3 <- pivot_wider(analyse_ipc3, names_from = TITLE_FR6, values_from = OBS_VALUE) %>% 
  arrange(DATE)
analyse_ipc3 <- mutate_at(analyse_ipc3, vars(-1), scale_ipc3_base100) 

analyse_ipc3 <- pivot_longer(analyse_ipc3, cols=c(-1), names_to = "Composante", values_to = "value")
analyse_ipc3%>% 
  # transform to date format with lubridate
  mutate(DATE = ymd(DATE))
analyse_ipc3 <- analyse_ipc3 |>
  group_by(Composante) |>
  arrange(DATE) |>
  mutate(gliss_annuel = value/lag(value, 12)-1) |>
  mutate(gliss_mensuel = value/lag(value, 1)-1) |>
  mutate(value_max_date = ifelse(DATE == max(DATE) | DATE ==max(analyse_ipc3$DATE[analyse_ipc3$DATE!=max(analyse_ipc3$DATE)]), value, NA_real_)) 

prepare_plot_ipc3 <- filter(analyse_ipc3, !is.na(value_max_date)) |>
  mutate(gliss_annuel = gliss_annuel*100) |>
  mutate(gliss_mensuel = gliss_mensuel*100) 
  

plot_niveau_ipc3 <- ggplot(data = analyse_ipc3,
                          aes(x=DATE, y=value, fill = Composante)) +
  geom_line(aes(col = Composante)) + geom_point(aes(col = Composante)) +
  labs(title = "Evolution des composantes de l'inflation depuis mars 2022", 
       subtitle = "Lecture : Au mois de mars 2023, les prix alimentaires et boissons non alcoolisées augmentent de 17 % par rapport à mars 2022. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Base 100 en mars 2022", x = "Mois") +
  #scale_color_manual(values = c("purple","#CC0000", "#333333", "blue", "#006000", "brown"))+
  geom_line(data = filter(analyse_ipc3, Composante == "Ensemble"), size = 1.5) +
  scale_linetype_manual(values=c("solid", "solid","twodashed","solid","solid","solid"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.text = element_text(size=7),legend.title = element_text(size=8), 
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")
  )
plot_niveau_ipc3

plot_ipc3_mensuel <-ggplot(data = prepare_plot_ipc3,
                    aes(x = gliss_mensuel,
                    y = reorder(Composante,gliss_mensuel), 
                    fill =  as.character(DATE))) + 
                    #fill=factor(ifelse(Composante=="Produits alimentaires et boissons non alcoolisées","Highlighted","Normal")))) +
  geom_col( position = "dodge2") +
  labs(title = "Evolution mensuelle des prix alimentaires en avril 2023", 
       subtitle = "Lecture : Au mois d'avril 2023, les prix des légumes ont reculé de 3,1 % \npar rapport à mars 2023. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphique : @statjunior", 
       y = "Type de produit alimentaire", x = "Pourcentage (%)") +
  #scale_fill_manual(name = "Composante", values=c("red","grey50")) +
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=12),
        axis.text=element_text(size=12), 
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic")
  )
plot_ipc3_mensuel

plot_ipc3_annuel <-ggplot(data = prepare_plot_ipc3,
                           aes(x = gliss_annuel,
                               y = reorder(Composante,gliss_mensuel), 
                               fill =  as.character(DATE)))+
  geom_col( position = "dodge2") +
  labs(title = "Glissement annuel des prix alimentaires en mars 2023", 
       subtitle = "Lecture : Au mois de mars 2023, les prix des produits alimentaires et boissons non alcoolisées ont augmenté de 17 % par rapport à mars 2022. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Type de produit alimentaire", x = "Pourcentage (%)") +
  scale_fill_manual(name = "Composante", values=c("red","grey50")) +
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=7),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")
  )
plot_ipc3_annuel

###Logement, eau, electricite, gaz et autres combustibles
analyse_ipc4 <- filter(data_ipc4, DATE >= as.Date(date_base100)) %>% 
  select(DATE, OBS_VALUE, TITLE_FR6, TITLE_FR5) 
analyse_ipc4$TITLE_FR5 <-gsub("Nomenclature Coicop :", "", analyse_ipc4$TITLE_FR5)
analyse_ipc4$TITLE_FR5 <- gsub("^\\s+|\\s+$", "", analyse_ipc4$TITLE_FR5) 
analyse_ipc4$alim_restriction <- substr(analyse_ipc4$TITLE_FR5, 1, 6) 
analyse_ipc4 <- mutate(analyse_ipc4, verif = ifelse(TITLE_FR5 == alim_restriction,1,0)) |>
  filter(verif == 1) |>
  select(-TITLE_FR5, -verif, -alim_restriction) |>
  filter(TITLE_FR6 == "Électricité" | TITLE_FR6 =="Gaz" | TITLE_FR6 =="Combustibles liquides"| TITLE_FR6 =="Combustibles solides" | TITLE_FR6 =="Énergie thermique")

analyse_ipc4 <- pivot_wider(analyse_ipc4, names_from = TITLE_FR6, values_from = OBS_VALUE) %>% 
  arrange(DATE)
analyse_ipc4 <- mutate_at(analyse_ipc4, vars(-1), scale_ipc4_base100) 

analyse_ipc4 <- pivot_longer(analyse_ipc4, cols=c(-1), names_to = "Composante", values_to = "value")
analyse_ipc4%>% 
  # transform to date format with lubridate
  mutate(DATE = ymd(DATE))
analyse_ipc4 <- analyse_ipc4 |>
  group_by(Composante) |>
  arrange(DATE) |>
  mutate(gliss_annuel = value/lag(value, 12)-1) |>
  mutate(gliss_mensuel = value/lag(value, 1)-1) |>
  mutate(value_max_date = ifelse(DATE == max(DATE), value, NA_real_)) 

prepare_plot_ipc4 <- filter(analyse_ipc4, !is.na(value_max_date)) |>
  mutate(gliss_annuel = gliss_annuel*100) |>
  mutate(gliss_mensuel = gliss_mensuel*100) 

plot_ipc4_annuel <-ggplot(data = prepare_plot_ipc4,
                          aes(x = gliss_annuel,
                              y = reorder(Composante,gliss_annuel))) +
  geom_col() +
  labs(title = "Glissement annuel des prix énergétiques en mars 2023", 
       subtitle = "Lecture : Au mois de mars 2023, les prix de l'électricité a augmenté de 11 % par rapport à mars 2022. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Type de produit énergétique", x = "Pourcentage (%)") +
  scale_fill_manual(name = "Composante", values=c("red","grey50")) +
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=7),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")
  )
plot_ipc4_annuel



###Pondérations et glissement annuel par composante
analyse_pond <- filter(data_pond_ipc, DATE >=as.Date('2015-01-01')) |>
  filter(TITLE_FR5 == "Services" | 
           TITLE_FR5 == "Tabac" | 
           TITLE_FR5 == "Alimentation" | 
           TITLE_FR5 == "Énergie" | 
           TITLE_FR5 == 'Produits manufacturés'| 
           TITLE_FR5 == 'Ensemble') %>% 
  filter(TITLE_FR1 =="Pondérations de l'indice des prix à la consommation") %>% 
  filter(TITLE_FR4 == "France") %>% 
  select(DATE, OBS_VALUE, TITLE_FR5) %>% 
  rename(c("Composante"="TITLE_FR5", "pond"="OBS_VALUE")) %>% 
  mutate(pond=pond/10000) %>% 
  mutate(DATE2 = substr(DATE, 1, 4)) %>% 
  select(-DATE)


analyse_ipc2_bis <- filter(data_ipc2, DATE >= as.Date(date_base100_bis)) %>% 
  filter(TITLE_FR5 == "Services" |
           TITLE_FR5 == "Tabac" |
           TITLE_FR5 == "Alimentation" |
           TITLE_FR5 == "Énergie" |
           TITLE_FR5 == 'Produits manufacturés'|
           TITLE_FR5 == 'Ensemble') %>% 
  filter(TITLE_FR1 == "Indice des prix à la consommation") %>% 
  select(DATE, OBS_VALUE, TITLE_FR5) 

analyse_ipc2_bis <- pivot_wider(analyse_ipc2_bis, names_from = TITLE_FR5, values_from = OBS_VALUE) %>% 
  arrange(DATE)
analyse_ipc2_bis  <- mutate_at(analyse_ipc2_bis , vars(-1), scale_ipc2_bis_base100) 

analyse_ipc2_bis  <- pivot_longer(analyse_ipc2_bis , cols=c(-1), names_to = "Composante", values_to = "value")
analyse_ipc2_bis  <- analyse_ipc2_bis  |>
  group_by(Composante) |>
  arrange(DATE) |>
  mutate(gliss_annuel = value/lag(value, 12)-1)

analyse_pond2_bis  <- filter (analyse_ipc2_bis , !is.na(gliss_annuel)) |>
  select(DATE, Composante, gliss_annuel) %>% 
  mutate(DATE2 = substr(DATE, 1, 4)) 

gliss_composante_ipc <- left_join(analyse_pond2_bis , analyse_pond, by = c("DATE2", "Composante"))
gliss_composante_ipc <- left_join(gliss_composante_ipc,isj, by = c("DATE"))
gliss_composante_ipc <- mutate(gliss_composante_ipc, contrib_gliss_ipc = gliss_annuel*pond) |>
  filter(!is.na(pond)) |>
  group_by(DATE) %>% 
  mutate(contrib_gliss_ipc= contrib_gliss_ipc*100) %>% 
  mutate(gliss_ipc = sum(contrib_gliss_ipc)) %>% 
  mutate(DATE = as.Date(DATE)) %>% 
  mutate(contrib_max = contrib_gliss_ipc) %>% 
  mutate(contrib_max = ifelse(DATE !=max(gliss_composante_ipc$DATE), NA, contrib_max)) %>% 
  select(DATE, Composante, contrib_gliss_ipc, gliss_ipc, contrib_max, gliss_annuel_isj)

gliss_composante_ipc$Composante<- as.factor(gliss_composante_ipc$Composante)

####Contribution à l'inflation par composante
plot_contrib_ipc <-  ggplot(data=gliss_composante_ipc , aes(x=DATE, y= contrib_gliss_ipc, fill = forcats::fct_rev(Composante))) +
  geom_col()+
  geom_line(aes(y = gliss_ipc, color =  "IPC")) + geom_point(aes(y = gliss_ipc, color =  "IPC")) +
  geom_line(aes(y = gliss_annuel_isj, color = "Indice sous-jacent"), type = "twodashed") +
  labs(title = "Contribution des postes de consommation au glissement annuel de l'IPC", 
       subtitle = "Lecture : Au mois d'avril 2023, l'alimentation contribue pour 2,4 points au glissement annuel de l'inflation (+5,9 %)  \nSource : Insee \nCalculs et graphiques : @statjunior", 
       y = "Pourcentage d'inflation (%)", x = "Mois", fill='Composante') +
  #scale_color_manual(values = c("#333333"))
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 12, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)
  ) +
   scale_colour_manual(values = c("IPC" = "black", "Indice sous-jacent" = "red"))+
  scale_x_date(limits = as.Date(c("2019-12-01", NA))) +
  ylim(-1.5,7) +
  scale_fill_discrete(breaks=c("Alimentation","Énergie", "Produits manufacturés","Services", "Tabac"))
plot_contrib_ipc 

gliss_composante_ipc <- mutate(gliss_composante_ipc, contrib_percent = contrib_gliss_ipc/gliss_ipc) |>
  mutate(contrib_percent = round(contrib_percent, 2))


gliss_alim <- filter(analyse_ipc2,substr(DATE,6,7)=="03" | substr(DATE,6,7)=="06" |substr(DATE,6,7)=="09"|substr(DATE,6,7)=="12") |>
  filter(Composante == "Alimentation")
write_xlsx(gliss_alim,'plots/ab.xlsx')