#######Chômage BIT et contraintes sur l'offre de travail 
list(rm=ls())
library(insee)
library(plyr)
library(dplyr)
library(ggplot2)
library(readxl) 
library(writexl)
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
conflict_prefer("rename", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("replace", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("relocate", "dplyr")
conflict_prefer("select", "dplyr")


setwd <- setwd("C:/Users/gasto/Documents/GitHub/Statjunior/projets-r-statjunior/chomage_bit/")

insee_dataset = get_dataset_list() 

#########Chomage BIT et taux de chomage de longue durée
get_chom_bit = 
  get_idbank_list("CHOMAGE-TRIM-NATIONAL") %>% 
  filter(NATURE == "TAUX") 

idbank_chom_bit = get_chom_bit %>% pull(idbank)

chom_bit = 
  get_insee_idbank(idbank_chom_bit) %>% 
  split_title() %>% 
  filter(OBS_STATUS == "A") %>% 
  select(DATE, TIME_PERIOD, OBS_VALUE, TITLE_FR1, TITLE_FR2, TITLE_FR3, TITLE_FR4, TITLE_FR5) %>% 
  filter(TITLE_FR3 == "France hors Mayotte")

p_chom_bit <- filter(chom_bit, TITLE_FR1 == "Taux de chômage au sens du BIT" | TITLE_FR1 == "Taux de chômage de longue durée")
p_chom_bit <- filter(p_chom_bit, TITLE_FR2 == "Ensemble")

p_chom_age <- filter(chom_bit, TITLE_FR1 == "Taux de chômage au sens du BIT") |>
  filter(TITLE_FR2 == "Ensemble des 25 à 49 ans" | TITLE_FR2 =="Ensemble des moins de 25 ans" | TITLE_FR2 =="Ensemble des 50 ans ou plus")

####Plot Graphique chomage BIT et chomage de longue durée
plot_chom_bit_duree <-  ggplot(data=p_chom_bit, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = TITLE_FR1), size = 1.1)+
  labs(title = "Taux de chômage au sens du BIT et taux de chômage de longue durée", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux de chômage au sens du BIT est de X %.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Taux de chômage au sens du BIT (%)", x = "Année", color=NULL) +
  #scale_color_manual(values = c("#333333"))
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 12, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)
  ) 
plot_chom_bit_duree

###Plot du chomage BIT par tranche d'age 
plot_chom_bit_age <-  ggplot(data=p_chom_age, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = TITLE_FR2), size = 1.1)+
  labs(title = "Taux de chômage au sens du BIT par tranche d'âge", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux de chômage au sens du BIT est de X %.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Taux de chômage au sens du BIT (%)", x = "Année", color=NULL) +
  #scale_color_manual(values = c("#333333"))
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 12, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)
  ) 
plot_chom_bit_age

############Chomage élargi (BIT + halo + sous-emploi)
get_elargi = 
  get_idbank_list("CHOMAGE-TRIM-NATIONAL") %>% 
  filter(NATURE == "PROPORTION") 

idbank_chom_elargi = get_elargi %>% pull(idbank)

chom_elargi = 
  get_insee_idbank(idbank_chom_elargi) %>% 
  split_title() %>% 
  filter(OBS_STATUS == "A") %>% 
  select(DATE, TIME_PERIOD, OBS_VALUE, TITLE_FR1, TITLE_FR2, TITLE_FR3, TITLE_FR4, TITLE_FR5) %>% 
  filter(TITLE_FR3 == "France hors Mayotte")


p_chom_elargi <- filter(chom_elargi, TITLE_FR1 == "Part des chômeurs parmi les participants au marché du travail (emploi, chômage, halo)" | 
                       TITLE_FR1 == "Part des personnes au chômage ou dans le halo parmi les participants (emploi, chômage, halo) au marché du travail" | 
                       TITLE_FR1 ==  "Part des personnes contraintes sur leur offre de travail (halo, chômage, sous-emploi) parmi les participants au marché du travail (emploi, chômage, halo)") |>
  mutate(TITLE_FR1 = case_when(
    TITLE_FR1 == "Part des chômeurs parmi les participants au marché du travail (emploi, chômage, halo)"~"Chômage BIT", 
    TITLE_FR1 == "Part des personnes au chômage ou dans le halo parmi les participants (emploi, chômage, halo) au marché du travail"~"Chômage + halo du chomage", 
    TITLE_FR1 == "Part des personnes contraintes sur leur offre de travail (halo, chômage, sous-emploi) parmi les participants au marché du travail (emploi, chômage, halo)"~"Chômage + halo du chomage + sous-emploi"
  ))

####Plot contraintes sur l'offre de travail (chomage BIT + halo + sous-emploi)
plot_chom_elargi <-  ggplot(data=p_chom_elargi, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = TITLE_FR1), size =1.1)+
  labs(title = "Contraintes sur l'offre de travail (chômage, halo du chômage et sous-emploi)", 
       subtitle = "Lecture : Au 1er trimestre 2023, les contraintes sur l'offre de travail (chômage BIT + halo + sous-emploi) représentent X % des participants au marché du travail.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Part parmi les participants au marché du travail (%)", x = "Année", color=NULL) +
  #scale_color_manual(values = c("#333333"))
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 12, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)
  ) 
plot_chom_elargi

###Halo par tranche d'age
p_alo_age <- filter(chom_elargi, TITLE_FR1 == "Personnes dans le halo autour du chômage") |>
            filter(TITLE_FR2 == "Ensemble de 25 à 49 ans (part dans la population)" | 
                   TITLE_FR2 == "Ensemble de 15 à 24 ans (part dans la population)" | 
                   TITLE_FR2 == "Ensemble de 50 à 64 ans (part dans la population)" |
                  TITLE_FR2 ==  "Ensemble de 15 à 64 ans (part dans la population)") |>
  mutate(TITLE_FR2 = case_when(
    TITLE_FR2 == "Ensemble de 25 à 49 ans (part dans la population)"~"25 - 49 ans", 
    TITLE_FR2 == "Ensemble de 15 à 24 ans (part dans la population)"~"15 - 24 ans", 
    TITLE_FR2 == "Ensemble de 50 à 64 ans (part dans la population)"~"50 - 64 ans", 
    TITLE_FR2 ==  "Ensemble de 15 à 64 ans (part dans la population)"~"Ensemble des 15 à 64 ans"
  ))


###Plot halo par tranche d'age
plot_alo_age <-  ggplot(data=p_alo_age, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = TITLE_FR2), size =1.1)+
  labs(title = "Halo du chômage par tranche d'âge", 
       subtitle = "Lecture : Au 1er trimestre 2023, 3 % des 50-64 ans sont dans le halo du chômage.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Part de la population (%)", x = "Année", color=NULL) +
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 12, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)
  ) 
plot_alo_age


####Taux d'emploi
list_emploi_bit = 
  get_idbank_list("EMPLOI-BIT-TRIM") %>% 
  filter(NATURE == "TAUX") %>% 
  filter(INDICATEUR_label_fr == "Taux d'emploi" | 
         INDICATEUR_label_fr == "Taux d'activité" | 
         INDICATEUR_label_fr == "Taux d'emploi à temps complet" |
         INDICATEUR_label_fr == "Taux d'emploi à temps partiel" |
         INDICATEUR_label_fr == "Taux d'emploi en CDD ou intérim" |
        INDICATEUR_label_fr =="Taux d'emploi en CDI" |
        INDICATEUR_label_fr =="Taux d'emploi en équivalent temps plein" |
        INDICATEUR_label_fr =="Taux de personnes en situation de sous-emploi")

idbank_emploi_bit= list_emploi_bit%>% pull(idbank)

try_emploi_bit = 
  get_insee_idbank(idbank_emploi_bit) %>% 
  split_title() %>% 
  filter(OBS_STATUS == "A") %>% 
  filter(DATE == max(DATE)) %>% 
  pull(IDBANK)

emploi_bit = 
  get_insee_idbank(try_emploi_bit) %>% 
  split_title() %>% 
  filter(OBS_STATUS == "A") 

emploi_bit <- emploi_bit |>
  mutate(TITLE_FR2 = ifelse(IDBANK == "010605845", 
                             "Ensemble", TITLE_FR2)) |>
  mutate(TITLE_FR3 = ifelse(IDBANK == "010605845", 
                             "France hors Mayotte", TITLE_FR3)) |>
  mutate(TITLE_FR4 = ifelse(is.na(TITLE_FR4), "Données CVS", TITLE_FR4)) |>
  mutate(age = str_extract(TITLE_FR2, "\\d{1,2} à \\d{1,2} ans")) |>
  mutate(age = ifelse(TITLE_FR2 =="Ensemble des 50 ans ou plus", "50 ans ou plus", age)) |>
  mutate(sexe = if_else(str_detect(TITLE_FR2, "Femmes"), "Femmes", "")) |>
  mutate(sexe = if_else(str_detect(TITLE_FR2, "Hommes"), "Hommes", sexe)) |> 
  mutate(sexe = if_else(str_detect(TITLE_FR2, "Ensemble"), "Ensemble", sexe)) |> 
  mutate(sexe = if_else(str_detect(TITLE_FR2, "Total"), "Ensemble", sexe)) |>
  mutate(age = ifelse(is.na(age), "Ensemble", age)) |>
  mutate(TITLE_FR1 = case_when(
    TITLE_FR1 =="Personnes actives (taux d'activité) au sens du BIT"~"Taux d'activité (BIT)", 
    TITLE_FR1 =="Taux des personnes en situation de sous-emploi"~"Taux de sous-emploi",
      TITLE_FR1 =="Taux d'emploi en équivalent temps plein au sens du BIT"~"Taux d'emploi en EQTP",
      TITLE_FR1 =="Taux d'emploi en contrat à durée indéterminée (CDI) au sens du BIT"~"Taux d'emploi en CDI",
      TITLE_FR1 =="Taux d'emploi en contrat à durée déterminée (CDD) ou intérim au sens du BIT"~"Taux d'emploi en CDD ou intérim",
      TITLE_FR1 =="Taux d'emploi à temps partiel au sens du BIT"~"Taux d'emploi à temps partiel",
    TITLE_FR1 =="Taux d'emploi à temps complet au sens du BIT"~"Taux d'emploi à temps complet",
    TITLE_FR1 =="Personnes en emploi (taux d'emploi) au sens du BIT"~"Taux d'emploi (BIT)"
    ))
  
###Taux d'emploi BIT 
tx_emploi <- filter(emploi_bit, TITLE_FR1 == "Taux d'emploi (BIT)")
tx_emploi_sexe <- filter(tx_emploi, age =="15 à 64 ans" & (sexe == "Hommes" |sexe == "Femmes"))
tx_emploi_age <- filter(tx_emploi, sexe == "Ensemble") |>
  filter(age == "15 à 24 ans" | age =="15 à 64 ans" |age =="25 à 49 ans" |age =="50 à 64 ans")

plot_tx_emploi_age <-  ggplot(data=tx_emploi_age, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = age), size =1.1)+
  labs(title = "Taux d'emploi au sens du BIT par tranche d'âge", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux d'emploi au sens du BIT des 15 - 64 ans est de X %.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Part de la population (%)", x = "Année", color=NULL) +
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=14)
  ) 
plot_tx_emploi_age

plot_tx_emploi_sexe <-  ggplot(data=tx_emploi_sexe, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = sexe), size =1.1)+
  labs(title = "Taux d'emploi au sens du BIT par sexe", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux d'emploi au sens du BIT des femmes est de X %.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Part de la population (%)", x = "Année", color=NULL) +
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=14)
  ) 
plot_tx_emploi_sexe

###Taux d'activité BIT
tx_act <- filter(emploi_bit, TITLE_FR1 == "Taux d'activité (BIT)")
tx_act_sexe <- filter(tx_act, age =="15 à 64 ans" & (sexe == "Hommes" |sexe == "Femmes"))
tx_act_age <- filter(tx_act, sexe == "Ensemble") |>
  filter(age == "15 à 24 ans" | age =="15 à 64 ans" |age =="25 à 49 ans" |age =="50 à 64 ans")

plot_tx_act_age <-  ggplot(data=tx_act_age, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = age), size =1.1)+
  labs(title = "Taux d'activité au sens du BIT par tranche d'âge", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux d'activité au sens du BIT des 15 - 64 ans est de X %.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Part de la population (%)", x = "Année", color=NULL) +
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=14)
  ) 
plot_tx_act_age

plot_tx_act_sexe <-  ggplot(data=tx_act_sexe, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = sexe), size =1.1)+
  labs(title = "Taux d'activité au sens du BIT par sexe", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux d'activité au sens du BIT des femmes est de X %.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Part de la population (%)", x = "Année", color=NULL) +
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=14)
  ) 
plot_tx_act_sexe

###Taux d'emploi à temps complet
tx_complet <- filter(emploi_bit, TITLE_FR1 == "Taux d'emploi à temps complet")
tx_complet_sexe <- filter(tx_complet, age =="15 à 64 ans" & (sexe == "Hommes" |sexe == "Femmes"))
tx_complet_age <- filter(tx_complet, sexe == "Ensemble") |>
  filter(age == "15 à 24 ans" | age =="15 à 64 ans" |age =="25 à 49 ans" |age =="50 à 64 ans")

plot_tx_complet_age <-  ggplot(data=tx_complet_age, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = age), size =1.1)+
  labs(title = "Taux d'emploi à temps complet par tranche d'âge", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux d'emploi à temps complet des 15 - 64 ans est de X %.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Part de la population (%)", x = "Année", color=NULL) +
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=14)
  ) 
plot_tx_complet_age

plot_tx_complet_sexe <-  ggplot(data=tx_complet_sexe, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = sexe), size =1.1)+
  labs(title = "Taux d'emploi à temps complet par sexe", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux d'emploi à temps complet des femmes est de X %.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Part de la population (%)", x = "Année", color=NULL) +
  theme_grey()+
  scale_y_continuous(breaks=seq(40,70,5))+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=14)
  )  
plot_tx_complet_sexe

###Taux d'emploi à temps partiel
tx_partiel <- filter(emploi_bit, TITLE_FR1 == "Taux d'emploi à temps partiel")
tx_partiel_sexe <- filter(tx_partiel, age =="15 à 64 ans" & (sexe == "Hommes" |sexe == "Femmes"))
tx_partiel_age <- filter(tx_partiel, sexe == "Ensemble") |>
  filter(age == "15 à 24 ans" | age =="15 à 64 ans" |age =="25 à 49 ans" |age =="50 à 64 ans")

plot_tx_partiel_age <-  ggplot(data=tx_partiel_age, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = age), size =1.1)+
  labs(title = "Taux d'emploi à temps partiel par tranche d'âge", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux d'emploi à temps partiel des 15 - 64 ans est de X %.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Part de la population (%)", x = "Année", color=NULL) +
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=14)
  ) 
plot_tx_partiel_age

plot_tx_partiel_sexe <-  ggplot(data=tx_partiel_sexe, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = sexe), size =1.1)+
  labs(title = "Taux d'emploi à temps partiel par sexe", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux d'emploi à temps partiel des femmes est de X %.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Part de la population (%)", x = "Année", color=NULL) +
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=14)
  )  
plot_tx_partiel_sexe

###Taux d'emploi en CDI 
tx_cdi_cdd <- filter(emploi_bit,  TITLE_FR1 == "Taux d'emploi en CDD ou intérim")

plot_tx_cdi_cdd <-  ggplot(data=tx_cdi_cdd, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = TITLE_FR1), size =1.1)+
  labs(title = "Taux d'emploi en contrat à durée déterminée (CDD) ou intérim au sens du BIT", 
       subtitle = "Lecture : Au 1er trimestre 2023, les emplois en CDD ou intérim représentent X points du taux d'emploi d'ensemble.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Contribution au taux d'emploi d'ensemble (points de pourcentage)", x = "Année", color=NULL) +
  theme_grey()+
  theme(legend.position="bottom",
        axis.text=element_text(size=11),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=14)
  ) 
plot_tx_cdi_cdd


###Taux d'emploi en EQTP 
tx_emploi_eqtp <- filter(emploi_bit,  TITLE_FR1 == "Taux d'emploi en EQTP")
tx_emploi_eqtp_sexe <- filter(tx_emploi_eqtp, age =="15 à 64 ans" & (sexe == "Hommes" |sexe == "Femmes"))
tx_emploi_eqtp_age <- filter(tx_emploi_eqtp, sexe == "Ensemble") |>
  filter(age == "15 à 24 ans" | age =="15 à 64 ans" |age =="25 à 49 ans" |age =="50 à 64 ans")

plot_tx_emploi_eqtp_age <-  ggplot(data=tx_emploi_eqtp_age, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = age), size =1.1)+
  labs(title = "Taux d'emploi en équivalent temps plein (EQTP) par tranche d'âge", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux d'emploi en EQTP des 15 - 64 ans est de 64 %.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Part de la population (%)", x = "Année", color=NULL) +
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=14)
  ) 
plot_tx_emploi_eqtp_age

plot_tx_emploi_eqtp_sexe <-  ggplot(data=tx_emploi_eqtp_sexe, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = sexe), size =1.1)+
  labs(title = "Taux d'emploi en équivalent temps plein (EQTP) par sexe", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux d'emploi en EQTP des femmes est de X %.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Part de la population (%)", x = "Année", color=NULL) +
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=14)
  )  
plot_tx_emploi_eqtp_sexe


###Sous-emploi
tx_ss_emploi <- filter(emploi_bit,  TITLE_FR1 == "Taux de sous-emploi")

tx_ss_emploi <-  ggplot(data=tx_ss_emploi, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE, color = TITLE_FR1), size =1.1)+
  labs(title = "Contribution du sous-emploi parmi le taux d'emploi", 
       subtitle = "Lecture : Au 1er trimestre 2023, la contribution du sous-emploi au taux d'emploi d'ensemble est de X % .  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Contribution au taux d'emploi d'ensemble (points de pourcentage)", x = "Année", color=NULL) +
  theme_grey()+
  theme(legend.position="bottom",
        axis.text=element_text(size=11),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=14)
  ) 
tx_ss_emploi


###Nombre moyen d'heures travaillées 
nb_heures <- get_insee_idbank('010752066')

plot_nb_heures <-  ggplot(data=nb_heures, aes(x=DATE, y= OBS_VALUE)) +
  geom_line(aes(y = OBS_VALUE), color = "orange", size =1.1)+
  labs(title = "Nombre moyen d’heures travaillées par semaine et par emploi", 
       subtitle = "Lecture : Au 1er trimestre 2023, la durée moyenne hebdomadaire de travail est de X heures par emploi. \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi  (Dernier point : 1er trimestre 2023) \nGraphique : @statjunior", 
       y = "Nombre d'heures hebdomadaires moyen", x = "Année", color=NULL) +
  theme_grey()+
  theme(legend.position="bottom",
        axis.text=element_text(size=11),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=14)
  ) 
plot_nb_heures

####Nombre de personnes au chomage BIT ou dans le halo 
get_personnes_elargi = 
  get_idbank_list("CHOMAGE-TRIM-NATIONAL") %>% 
  filter(NATURE == "VALEUR_ABSOLUE") 

idbank_chom_personnes = get_personnes_elargi %>% pull(idbank)

chom_personnes = 
  get_insee_idbank(idbank_chom_personnes) %>% 
  split_title() %>% 
  filter(OBS_STATUS == "A") %>% 
  select(DATE, TIME_PERIOD, OBS_VALUE, TITLE_FR1, TITLE_FR2, TITLE_FR3, TITLE_FR4, TITLE_FR5) %>% 
  filter(TITLE_FR3 == "France hors Mayotte") |>
  mutate(age = str_extract(TITLE_FR2, "\\d{1,2} à \\d{1,2} ans")) |>
  mutate(age = ifelse(TITLE_FR2 =="Ensemble des 50 ans ou plus (en milliers)", "50 ans ou plus", age)) |>
  mutate(sexe = if_else(str_detect(TITLE_FR2, "Femmes"), "Femmes", "")) |>
  mutate(sexe = if_else(str_detect(TITLE_FR2, "Hommes"), "Hommes", sexe)) |> 
  mutate(sexe = if_else(str_detect(TITLE_FR2, "Ensemble"), "Ensemble", sexe)) |> 
  mutate(sexe = if_else(str_detect(TITLE_FR2, "Total"), "Ensemble", sexe)) |>
  mutate(age = ifelse(is.na(age), "Ensemble", age)) |>
  mutate(sexe = ifelse(is.na(sexe), "Ensemble", sexe)) |>
  mutate(OBS_VALUE = OBS_VALUE / 1000)

decompo_chomage <-filter(chom_personnes,TITLE_FR2 == "Inactifs disponibles mais ne faisant pas de démarche active de recherche d'emploi (en milliers)" | 
                           TITLE_FR2 =="Inactifs faisant des démarches actives de recherche d'emploi mais non disponibles (en milliers)" | 
                           TITLE_FR2 == "Inactifs souhaitant travailler mais non disponibles et ne faisant pas de démarche active de recherche d'emploi (en milliers)" |
                           TITLE_FR2 ==  "Ensemble (en milliers)") |>
  mutate(title = ifelse(TITLE_FR2 == "Inactifs disponibles mais ne faisant pas de démarche active de recherche d'emploi (en milliers)" | 
                          TITLE_FR2 == "Inactifs faisant des démarches actives de recherche d'emploi mais non disponibles (en milliers)" |
                          TITLE_FR2 == "Inactifs souhaitant travailler mais non disponibles et ne faisant pas de démarche active de recherche d'emploi (en milliers)", 
                        TITLE_FR2, NA_real_)) |>
  mutate(title = ifelse(TITLE_FR1 == "Chômeurs au sens du BIT", 
                        TITLE_FR1, title)) |>
  filter(DATE >= as.Date("2003-01-01")) |>
  arrange(DATE) |>
  filter(!is.na(title))

decompo_chomage <- mutate(decompo_chomage, title = as.character(title)) |>
  mutate(title = ifelse(title =="Inactifs faisant des démarches actives de recherche d'emploi mais non disponibles (en milliers)", "Halo : pas disponible immédiatement", title)) |>
  mutate(title = ifelse(title =="Inactifs disponibles mais ne faisant pas de démarche active de recherche d'emploi (en milliers)","Halo : pas de recherche active d'emploi", title)) |>
  mutate(title = ifelse(title =="Inactifs souhaitant travailler mais non disponibles et ne faisant pas de démarche active de recherche d'emploi (en milliers)","Halo : pas de recherche active et pas disponible immédiatement", title)) |>
  mutate(title = ifelse(title == 'Chômeurs au sens du BIT', 'Chômeurs BIT', title))

plot_personnes_chomage_halo <-  ggplot(data=decompo_chomage, aes(x=DATE, y= OBS_VALUE, fill = forcats::fct_relevel(title, c("Halo : pas de recherche active et pas disponible immédiatement", 
                                                                                                                            "Halo : pas de recherche active d'emploi",
                                                                                                                            "Halo : pas disponible immédiatement","Chômeurs BIT"
                                                                                                )))) +
  geom_col()+
  labs(title = "Nombre de personnes au chômage ou dans le halo du chômage", 
       subtitle = "Lecture : Au 1er trimestre 2023, X millions de personnes de plus de 15 ans sont au chômage ou dans le halo du chômage. \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi  (dernier point : 1er trimestre 2023) \nCalculs et graphique : @statjunior", 
       y = "Millions", x = "Date", fill='title') +
  #scale_color_manual(values = c("#333333"))
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 12, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)
  ) +
  scale_fill_manual(values = c("#99CCFF", "#00B8E7", "#3333FF","#CC00CC")) +
  scale_x_date(limits = as.Date(c("2003-01-01", NA)))  + 
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
plot_personnes_chomage_halo

plot_part_chomage_halo <-  ggplot(data=decompo_chomage, aes(x=DATE, y= OBS_VALUE, fill = forcats::fct_relevel(title, c("Halo : pas de recherche active et pas disponible immédiatement", 
                                                                                                                       "Halo : pas de recherche active d'emploi",
                                                                                                                       "Halo : pas disponible immédiatement","Chômeurs BIT"
                                                                                                    )))) +
  geom_col(position="fill")+
  labs(title = "Part des personnes au chômage ou dans le halo du chômage", 
       subtitle = "Lecture : Au 1er trimestre 2023, les chômeurs au sens du BIT représentent X % des personnes sans emploi et en recherche d'emploi.  \nSource : Insee - Enquête Emploi (dernier point : 1er trimestre 2023) \nCalculs et graphique : @statjunior", 
       y = "Pourcentage", x = "Date", fill='title') +
  #scale_color_manual(values = c("#333333"))
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 12, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)
  ) +
  scale_fill_manual(values = c("#99CCFF", "#00B8E7", "#3333FF","#CC00CC")) +
  scale_x_date(limits = as.Date(c("2003-01-01", NA))) + 
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
plot_part_chomage_halo


###Nombre de personnes dans le halo 
decompo_halo <- filter(decompo_chomage, title!="Chômeurs BIT") |>
  group_by(DATE)|>
  mutate(total_halo = sum(OBS_VALUE))

plot_personnes_halo <-  ggplot(data=decompo_halo, aes(x=DATE, y= OBS_VALUE, fill = forcats::fct_relevel(title, c("Halo : pas de recherche active et pas disponible immédiatement", 
                                                                                                                            "Halo : pas de recherche active d'emploi",
                                                                                                                            "Halo : pas disponible immédiatement"
                                                                                                                        )))) +
  geom_col()+
  labs(title = "Nombre de personnes dans le halo du chômage", 
       subtitle = "Lecture : Au 1er trimestre 2023, 2 millions de personnes de plus de 15 ans sont dans le halo du chômage.  \nSource : Insee - Enquête Emploi \nChamp : France hors Mayotte \nCalculs et graphique : @statjunior", 
       y = "Millions", x = "Date", fill='title') +
  #scale_color_manual(values = c("#333333"))
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 12, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)
  ) +
  scale_fill_manual(values = c("#99CCFF", "#00B8E7", "#3333FF")) +
  scale_x_date(limits = as.Date(c("2003-01-01", NA)))  + 
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
plot_personnes_halo

###Nombre de personnes en sous-emploi
nb_ssemploi <- get_insee_idbank("010605873","010605872","010605871") %>% 
  split_title() %>% 
  filter(OBS_STATUS == "A") |>
  mutate(OBS_VALUE = OBS_VALUE/1000)

nb_ssemploi <- nb_ssemploi |>
  mutate(TITLE_FR2== as.character(TITLE_FR2)) |>
  mutate(title = ifelse(TITLE_FR2 =="Temps partiel, souhaitant travailler plus d'heures, disponible pour le faire et à la recherche d'un autre emploi (en milliers)", "Sous emploi: souhaite travailler davantage, disponible ET en recherche", NA_real_)) |>
  mutate(title = ifelse(TITLE_FR2 =="Temps partiel, souhaitant travailler plus d'heures, disponible pour le faire mais sans recherche d'un autre emploi (en milliers)","Sous emploi: souhaite travailler davantage, disponible MAIS sans recherche", title)) |>
  mutate(title = ifelse(TITLE_FR2 =="Temps plein, ou temps partiel, ayant involontairement travaillé moins que d'habitude (en milliers)","Sous emploi: ayant involontairement travaillé moins que d'habitude", title)) 

nb_ssemploi <- select(nb_ssemploi, DATE, OBS_VALUE, title) |>
  group_by(DATE) |>
  mutate(total_ssemploi = sum(OBS_VALUE))

plot_personnes_ssemploi <-  ggplot(data=nb_ssemploi, aes(x=DATE, y= OBS_VALUE, fill = forcats::fct_relevel(title, c("Sous emploi: ayant involontairement travaillé moins que d'habitude", "Sous emploi: souhaite travailler davantage, disponible MAIS sans recherche", "Sous emploi: souhaite travailler davantage, disponible ET en recherche"
                                                                                                                              )))) +
  geom_col()+
  labs(title = "Nombre de personnes en sous-emploi par catégorie", 
       subtitle = "Lecture : Au 1er trimestre 2023, 1,3 million de personnes de plus de 15 ans sont en situation de sous-emploi.  \nSource : Insee - Enquête Emploi \nChamp : France hors Mayotte \nCalculs et graphique : @statjunior", 
       y = "Millions", x = "Date", fill='title') +
  #scale_color_manual(values = c("#333333"))
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 12, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)
  ) +
  scale_fill_manual(values = c("#FFCC00", "orange", "#993000"))+
  scale_x_date(limits = as.Date(c("2003-01-01", NA)))  + 
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
plot_personnes_ssemploi

###Fusion de l'ensemble personnes chom BIT + personnes halo + personnes sous-emploi
decompo_chomage <- select(decompo_chomage,DATE, OBS_VALUE, title)
nb_ssemploi2 <- select(nb_ssemploi, -total_ssemploi)

decompo_full <- rbind(decompo_chomage, nb_ssemploi2)
decompo_full <- decompo_full |>
  group_by(DATE) |>
  mutate(total = sum(OBS_VALUE))

plot_full_personnes_besoin_emploi <-  ggplot(data=decompo_full, aes(x=DATE, y= OBS_VALUE, fill = forcats::fct_relevel(title, c("Sous emploi: ayant involontairement travaillé moins que d'habitude", 
                                                                                                                               "Sous emploi: souhaite travailler davantage, disponible MAIS sans recherche", 
                                                                                                                               "Sous emploi: souhaite travailler davantage, disponible ET en recherche", 
                                                                                                                               "Halo : pas de recherche active et pas disponible immédiatement", 
                                                                                                                               "Halo : pas de recherche active d'emploi",
                                                                                                                               "Halo : pas disponible immédiatement","Chômeurs BIT"
                                                                                                                                )))) +
  geom_col()+
  labs(title = "Nombre de personnes ayant besoin d'un emploi (au sens le plus large)", 
       subtitle = "Lecture : Au 1er trimestre 2023, 5,4 millions de personnes de plus de 15 ans souhaitent travailler ou travailler davantage.  \nChamp : France hors Mayotte \nSource : Insee - Enquête Emploi (dernier point : 1er trimestre 2023) \nCalculs et graphique : @statjunior", 
       y = "Millions", x = "Date", fill='title') +
  #scale_color_manual(values = c("#333333"))
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 12, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)
  ) +
  scale_x_date(limits = as.Date(c("2003-01-01", NA)))  + 
  scale_fill_manual(values = c("#FFCC00", "orange", "#993000","#99CCFF", "#00B8E7", "#3333FF","#CC00CC")) +
  guides(fill=guide_legend(nrow=4,byrow=TRUE))
plot_full_personnes_besoin_emploi