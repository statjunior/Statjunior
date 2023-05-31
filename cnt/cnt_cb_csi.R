list(rm=ls())
library(insee)
library(doremifasol)
library(eurostat)
library(OECD)
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

#Définition du chemin d'accès
setwd <- setwd("C:/Users/gasto/Documents/GitHub/Statjunior/projets-r-statjunior/cnt/")
getwd()

##################### PARAMETRES A FIXER AVANT DE FAIRE TOURNER LE CODE 
####Date de la base 100 pour les graphiques en niveau (actuellement 2019-T4)
scale_2019t4 <- function(x, na.rm = FALSE) x/x[cnt_cb_complet[,1] == "2019-10-01"]*100
scale_2018t4 <- function(x, na.rm = FALSE) x/x[cnt_cb_complet[,1] == "2018-10-01"]*100
scale_2000t1 <- function(x, na.rm = FALSE) x/x[cnt_cb_complet[,1] == "2000-01-01"]*100


####LIENS des bases à télécharger non disponibles dans le package 
lien_export_val <- "https://www.insee.fr/fr/statistiques/fichier/6960359/t_exports_val.xls"
lien_import_val <- "https://www.insee.fr/fr/statistiques/fichier/6960359/t_imports_val.xls"

file_export_val <- "t_exports_val.xls"
file_import_val <- "t_imports_val.xls"

export_cb_val <- read_excel(file_export_val, 
                       sheet = "Niveaux",skip = 7) |>
  rename("DATE" = "...1", "export_energ"="DE", "export_raf"= "C2") |>
  select(DATE, "export_energ", "export_raf") |>
  filter(!is.na(export_energ))

import_cb_val <- read_excel(file_import_val, 
                            sheet = "Niveaux",skip = 7) |>
  rename("DATE" ="...1","import_energ"="DE", "import_raf"= "C2") |>
  select(DATE, "import_energ", "import_raf") |>
  filter(!is.na(import_energ))
############# 
###A FAIRE

###Intérêt net des intérêts versés par les APU (en % de la VA totale), lissée sur 4 trimestres 

###Productivité horaire globale et PIB sur longue période => OK 
###Niveau des composantes du PIB depuis 2019 => OK 
###VA par branche => OK
###Productivité par branche =>OK 
###VA, heures travaillées et productivité par branche =>OK
###Glissement des déflateurs : PIB, VA marchande non agricole, Conso ménages, FBCF, Exports, Imports => OK
###Balance commerciale en biens et services et capacité de financement en % de la VA nationale => OK
###Evolution du taux de marge par branche par rapport au T4-2018
###EBE Energie (DE), raffinerie (C2), services de transport (HZ) => OK
###PIB et Pouvoir d'achat des ménages + Pouvoir d'achat par UC 


###Liste des CNT à télécharger
list_dataset <- get_dataset_list()

list_idbank_pib = 
  get_idbank_list("CNT-2014-PIB-EQB-RF") 
# 
# list_idbank_cb =
# get_idbank_list("CNT-2014-CB")
# # 
list_idbank_opebs =
  get_idbank_list("CNT-2014-OPERATIONS") %>%
  filter(
  OPERATION_label_fr == "P3M - Dépenses de consommation des ménages")
# 
pib = list_idbank_pib %>% pull(idbank)
opebs = list_idbank_opebs %>% pull(idbank)
# cb = list_idbank_cb %>% pull(idbank)


######Décomposition du PIB par composante (C+FBCF+X-M+VS)
pib_2014 = 
  get_insee_idbank(pib) %>% 
  split_title() 

pib_2014_bis <- pib_2014 |>
  mutate(TITLE_FR1 = case_when(
    TITLE_FR1 == "Produit intérieur brut total"~"pib_tot",
    TITLE_FR1 == "Dépenses de consommation totales"~"conso_tot", 
    TITLE_FR1 == "Dépenses de consommation totales"~"conso_tot",
    TITLE_FR1 == "Exportations"~"export_tot",
    TITLE_FR1 == "FBCF de l'ensemble des secteurs institutionnels"~"fbcf_tot",
    TITLE_FR1 == "FBCF Total"~"fbcf_tot", 
    TITLE_FR1 == "Importations"~"import_tot",
    TITLE_FR1 == "Variations de stocks"~"vs_tot",
    TITLE_FR1 =="Variation des stocks"~"vs_tot",
    TITLE_FR1 == "Dépenses de consommation collective des APU"~"conso_apu_col",
    TITLE_FR1 == "Dépenses de consommation des APU"~"conso_apu",
    TITLE_FR1 == "Dépenses de consommation des ISBLSM"~"conso_isblsm",
    TITLE_FR1 == "Dépenses de consommation des ménages"~"conso_men",
    TITLE_FR1 == "Dépenses de consommation individualisable des APU"~"conso_apu_ind",
    TITLE_FR1 == "Dépenses de consommation des APU collectives"~"conso_apu_col",
    TITLE_FR1 == "FBCF des administrations publiques"~"fbcf_apu",
    TITLE_FR1 == "FBCF des APU"~"fbcf_apu",
    TITLE_FR1 == "FBCF des entreprises financières"~"fbcf_ef",
    TITLE_FR1 == "FBCF des entreprises non financières"~"fbcf_enf",
    TITLE_FR1 == "FBCF des ISBLSM"~"fbcf_isblsm",
    TITLE_FR1 == "FBCF des ménages"~"fbcf_men",
    TITLE_FR1 == "FBCF des sociétés financières"~"fbcf_sf"
        ), 

    TITLE_FR2 = case_when(
      TITLE_FR2=="Contribution à l'évolution du PIB"~"contrib_pib",
    TITLE_FR2 =="Évolution du Produit intérieur brut total"~"croissance_trim_pib",
    TITLE_FR2 =="Valeur aux prix courants"~"val",
    TITLE_FR2 =="Volume aux prix de l'année précédente chaînés"~"vol"
        ),

    TITLE_FR3 = case_when(
      TITLE_FR3=="Valeur aux prix courants"~"val",
      TITLE_FR3 =="Volume aux prix de l'année précédente chaînés"~"vol"
          )) |>
  
  filter(!is.na(TITLE_FR1)) |>
  mutate(TITLE_FR3 = ifelse(is.na(TITLE_FR3) & (TITLE_FR2 == "val" | TITLE_FR2 == "vol"), TITLE_FR2, TITLE_FR3))

contrib_pib_eer <- filter(pib_2014_bis, TITLE_FR2 =="contrib_pib") |>
  pivot_wider(names_from = TITLE_FR1, values_from = OBS_VALUE, id_cols = DATE) |>
  mutate(croissance_pib = conso_men+conso_apu_ind+conso_apu_col+conso_isblsm+fbcf_tot+export_tot+vs_tot-import_tot) |>
  select(DATE, croissance_pib, conso_men,conso_apu_ind,conso_apu_col,conso_isblsm,fbcf_tot,export_tot,vs_tot,import_tot) |>
  mutate(conso = conso_men+conso_apu_ind+conso_apu_col + conso_isblsm) |>
  select(-conso_men,-conso_apu_ind,-conso_apu_col,-conso_isblsm) |>
  mutate(import_tot=-import_tot) |>
  pivot_longer(cols=c(3:7), names_to = "Composante", values_to = "value") |>
  mutate(Composante = case_when(
    Composante=="conso"~"Consommation",
    Composante =="export_tot"~"Export",
    Composante =="import_tot"~"Import",
    Composante =="fbcf_tot"~"Investissement",
    Composante =="vs_tot"~"Variations de stocks",
  ))
contrib_pib_eer$Composante<- as.factor(contrib_pib_eer$Composante)



pib_eer <- filter(pib_2014_bis, TITLE_FR2 !="contrib_pib" | is.na(TITLE_FR2)) |>
  select(-TITLE_FR2) |>
  mutate(titre = paste0(TITLE_FR1,"_", TITLE_FR3)) |>
  pivot_wider(names_from = titre, values_from = OBS_VALUE, id_cols = DATE) |>
  mutate(deflateur_pib = pib_tot_val/pib_tot_vol*100) |>
  mutate(deflateur_conso_men = conso_men_val/conso_men_vol*100) |>
  mutate(deflateur_fbcf = fbcf_tot_val/fbcf_tot_vol*100) |>
  mutate(deflateur_export = export_tot_val/export_tot_vol*100) |>
  mutate(deflateur_import = import_tot_val/import_tot_vol*100) |>
  arrange(DATE) |>
  mutate(gliss_annuel_deflateur_pib = ((deflateur_pib/lag(deflateur_pib, 4))-1)*100) |>
  mutate(gliss_annuel_deflateur_conso_men = ((deflateur_conso_men/lag(deflateur_conso_men, 4))-1)*100) |>
  mutate(gliss_annuel_deflateur_fbcf = ((deflateur_fbcf/lag(deflateur_fbcf, 4))-1)*100) |>
  mutate(gliss_annuel_deflateur_export = ((deflateur_export/lag(deflateur_export, 4))-1)*100) |>
  mutate(gliss_annuel_deflateur_import = ((deflateur_import/lag(deflateur_import, 4))-1)*100) 

pib_eer_2019 <- mutate_at(pib_eer, vars(2:36), scale_2019t4)

# cb_2014 =
#   get_insee_idbank(cb) %>%
#   split_title()
# 
# pib_2014_restrict <- filter(pib_2014, DATE == as.Date("2010-01-01"))
# opebs_2014_restrict <- filter(ope_bs_2014, DATE == as.Date("2010-01-01"))
# cb_2014_restrict <- filter(cb_2014, DATE == as.Date("2010-01-01"))


######### COMPTES NATIONAUX TRIMESTRIELS
cb_2014 <- get_insee_dataset("CNT-2014-CB")
#SMNA-CNT : ensemble principalement marchand non agricole
#DI-CNT : industrie ensemble 
#A17-FZ : construction 
cnt_cb_complet <- cb_2014 |>
  filter(OPERATION == "B1" | OPERATION == "B2" | OPERATION == "EMP" | OPERATION == "EMPNS" |  OPERATION == "DHES" |  OPERATION == "DHET" |  OPERATION == "VHTS" |  OPERATION == "VHTT" |  OPERATION == "D1") |>
  filter(CNA_PRODUIT == "DIM-CNT" | CNA_PRODUIT == "A17-FZ" | CNA_PRODUIT == "DSM-CNT" | CNA_PRODUIT =="DSN-CNT" | CNA_PRODUIT == "SMNA-CNT" | CNA_PRODUIT == "D-CNT") |>
  mutate(op_pr_val = paste(OPERATION, CNA_PRODUIT, VALORISATION, UNIT_MEASURE, sep = "-")) |>
  select(DATE, TIME_PERIOD, OBS_VALUE, OPERATION, CNA_PRODUIT, VALORISATION, UNIT_MEASURE, op_pr_val) |>
  spread(key = "op_pr_val",
         value = "OBS_VALUE") 
# cnt_cb_complet$DATE <- str_replace(cnt_cb_complet$DATE,"-01-", "-03-")
# cnt_cb_complet$DATE <- str_replace(cnt_cb_complet$DATE,"-04-", "-06-")
# cnt_cb_complet$DATE <- str_replace(cnt_cb_complet$DATE,"-07-", "-09-")
# cnt_cb_complet$DATE <- str_replace(cnt_cb_complet$DATE,"-10-", "-12-")
cnt_cb_complet$DATE <-as.Date(paste(cnt_cb_complet$DATE,1,sep="-"))
cnt_cb_complet <- mutate_at(cnt_cb_complet,  vars(7:78), list(~ recode(., `NULL` = 0))) |>
  mutate_at(vars(7:78), as.numeric) |>
  mutate_at(vars(7:78), ~replace(., is.na(.), 0)) |>
  group_by(DATE) |>
  summarise_at(vars(6:77),              # Specify column
               list(name = sum)) |>
  rename("va_man"= "B1-DIM-CNT-L-EUROS_name", "va_term" = "B1-DSM-CNT-L-EUROS_name", 
         "va_ternm" = "B1-DSN-CNT-L-EUROS_name","ebe_man"= "B2-DIM-CNT-SO-EUROS_name", 
         "ebe_term" = "B2-DSM-CNT-SO-EUROS_name", "ebe_ternm" = "B2-DSN-CNT-SO-EUROS_name",
         "wage_man"="D1-DIM-CNT-SO-EUROS_name", 
         "wage_term"="D1-DSM-CNT-SO-EUROS_name", "wage_ternm"= "D1-DSN-CNT-SO-EUROS_name", 
         "eff_travail_salman"="DHES-DIM-CNT-SO-HEURES_name", "eff_travail_salterm"= "DHES-DSM-CNT-SO-HEURES_name", 
         "eff_travail_salternm" ="DHES-DSN-CNT-SO-HEURES_name", "eff_travail_globman"= "DHET-DIM-CNT-SO-HEURES_name", 
         "eff_travail_globterm"="DHET-DSM-CNT-SO-HEURES_name", "eff_travail_globternm"= "DHET-DSN-CNT-SO-HEURES_name", 
         "emp_etp_globman"="EMP-DIM-CNT-SO-ETP_name", "emp_ind_globman"= "EMP-DIM-CNT-SO-INDIVIDUS_name", 
         "emp_glob_etp_term"="EMP-DSM-CNT-SO-ETP_name", "emp_glob_ind_term"="EMP-DSM-CNT-SO-INDIVIDUS_name", 
         "emp_glob_etp_ternm"="EMP-DSN-CNT-SO-ETP_name", "emp_glob_ind_ternm"="EMP-DSN-CNT-SO-INDIVIDUS_name", 
         "empns_etp_man" ="EMPNS-DIM-CNT-SO-ETP_name", "empns_ind_man" ="EMPNS-DIM-CNT-SO-INDIVIDUS_name", 
         "empns_etp_term"="EMPNS-DSM-CNT-SO-ETP_name", "empns_ind_term"="EMPNS-DSM-CNT-SO-INDIVIDUS_name",
         "empns_etp_ternm"="EMPNS-DSN-CNT-SO-ETP_name", "empns_ind_ternm"="EMPNS-DSN-CNT-SO-INDIVIDUS_name", 
         "vol_travail_sal_man"="VHTS-DIM-CNT-SO-HEURES_name", "vol_travail_sal_term"="VHTS-DSM-CNT-SO-HEURES_name", 
         "vol_travail_sal_ternm"="VHTS-DSN-CNT-SO-HEURES_name", "vol_travail_glob_man"="VHTT-DIM-CNT-SO-HEURES_name", 
         "vol_travail_glob_term"="VHTT-DSM-CNT-SO-HEURES_name", "vol_travail_glob_ternm" ="VHTT-DSN-CNT-SO-HEURES_name", 
         "va_marng"= "B1-SMNA-CNT-L-EUROS_name",
         "ebe_marng"= "B2-SMNA-CNT-SO-EUROS_name",
         "wage_marng"="D1-SMNA-CNT-SO-EUROS_name",
         "eff_travail_salmarng"="DHES-SMNA-CNT-SO-HEURES_name",
         "eff_travail_globmarng"="DHET-SMNA-CNT-SO-HEURES_name",
         "emp_ind_globmarng"= "EMP-SMNA-CNT-SO-INDIVIDUS_name",
         "emp_etp_globmarng"="EMP-SMNA-CNT-SO-ETP_name",
         "empns_etp_marng" ="EMPNS-SMNA-CNT-SO-ETP_name", 
         "empns_ind_marng" ="EMPNS-SMNA-CNT-SO-INDIVIDUS_name",
         "vol_travail_sal_marng"="VHTS-SMNA-CNT-SO-HEURES_name",
         "vol_travail_glob_marng"="VHTT-SMNA-CNT-SO-HEURES_name",
         "va_ag"= "B1-A17-FZ-L-EUROS_name",
         "ebe_ag"= "B2-A17-FZ-SO-EUROS_name",
         "wage_ag"="D1-A17-FZ-SO-EUROS_name",
         "eff_travail_salag"="DHES-A17-FZ-SO-HEURES_name",
         "eff_travail_globag"="DHET-A17-FZ-SO-HEURES_name",
         "emp_ind_globag"= "EMP-A17-FZ-SO-INDIVIDUS_name",
         "emp_etp_globag"="EMP-A17-FZ-SO-ETP_name",
         "empns_etp_ag" ="EMPNS-A17-FZ-SO-ETP_name", 
         "empns_ind_ag" ="EMPNS-A17-FZ-SO-INDIVIDUS_name",
         "vol_travail_sal_ag"="VHTS-A17-FZ-SO-HEURES_name",
         "vol_travail_glob_ag"="VHTT-A17-FZ-SO-HEURES_name", 
         "va_tot"= "B1-D-CNT-L-EUROS_name",
         "ebe_tot"= "B2-D-CNT-SO-EUROS_name",
         "wage_tot"="D1-D-CNT-SO-EUROS_name",
         "eff_travail_saltot"="DHES-D-CNT-SO-HEURES_name",
         "eff_travail_globtot"="DHET-D-CNT-SO-HEURES_name",
         "emp_ind_globtot"= "EMP-D-CNT-SO-INDIVIDUS_name",
         "emp_etp_globtot"="EMP-D-CNT-SO-ETP_name",
         "empns_etp_tot" ="EMPNS-D-CNT-SO-ETP_name", 
         "empns_ind_tot" ="EMPNS-D-CNT-SO-INDIVIDUS_name",
         "vol_travail_sal_tot"="VHTS-D-CNT-SO-HEURES_name",
         "vol_travail_glob_tot"="VHTT-D-CNT-SO-HEURES_name",) |>
  select(-"B1-DIM-CNT-V-EUROS_name", -"B1-A17-FZ-V-EUROS_name", -"B1-DSN-CNT-V-EUROS_name",
         -"B1-DSM-CNT-V-EUROS_name")

deflateur_vamarng <- select(cnt_cb_complet, DATE, "B1-SMNA-CNT-V-EUROS_name","va_marng") |>
  rename("va_valeur_marng"="B1-SMNA-CNT-V-EUROS_name") |>
  mutate(deflateur_va_marng = va_valeur_marng/va_marng*100) |>
  mutate(gliss_deflateur_va_marng = ((deflateur_va_marng /lag(deflateur_va_marng, 4))-1)*100) %>% 
  select(-va_valeur_marng, -va_marng)
# deflateur_vamarng$DATE <- str_replace(deflateur_vamarng$DATE,"-03-", "-01-")
# deflateur_vamarng$DATE <- str_replace(deflateur_vamarng$DATE,"-06-", "-04-")
# deflateur_vamarng$DATE <- str_replace(deflateur_vamarng$DATE,"-09-", "-07-")
# deflateur_vamarng$DATE <- str_replace(deflateur_vamarng$DATE,"-12-", "-10-")
deflateur_vamarng$DATE <- as.Date(deflateur_vamarng$DATE)

va_val_vol <- select(cnt_cb_complet, DATE, "B1-SMNA-CNT-V-EUROS_name","B1-D-CNT-V-EUROS_name", va_marng, va_tot) |>
  rename(va_valeur_marng = "B1-SMNA-CNT-V-EUROS_name", va_valeur_tot = "B1-D-CNT-V-EUROS_name")

cnt_cb_complet <- cnt_cb_complet |>
  select(-"B1-SMNA-CNT-V-EUROS_name", -"B1-D-CNT-V-EUROS_name") |>
  arrange(DATE) |>
  mutate(tcam_va_marng = ifelse(substr(DATE, 6, 7) == "03", (va_marng+lead(va_marng, 1)+lead(va_marng, 2)+lead(va_marng, 3))/(lag(va_marng, 4)+lag(va_marng, 3)+lag(va_marng, 2)+lag(va_marng, 1))-1, NA)) |>
  mutate(tcam_va_tot = ifelse(substr(DATE, 6, 7) == "03", (va_tot+lead(va_tot, 1)+lead(va_tot, 2)+lead(va_tot, 3))/(lag(va_tot, 4)+lag(va_tot, 3)+lag(va_tot, 2)+lag(va_tot, 1))-1, NA))
cnt_cb_complet <-  fill(cnt_cb_complet, tcam_va_marng, tcam_va_tot) 
cnt_cb_complet <-  mutate(cnt_cb_complet, tcam_va_marng = ifelse(substr(DATE, 1, 4) == "2022", NA, tcam_va_marng))
cnt_cb_complet <-  mutate(cnt_cb_complet, tcam_va_tot = ifelse(substr(DATE, 1, 4) == "2022", NA, tcam_va_tot))

cnt_cb_complet <- cnt_cb_complet |>
  mutate(prod_tete_ag = va_ag/emp_ind_globag*100) |>
  mutate(prod_tete_man = va_man/emp_ind_globman*100) |>
  mutate(prod_tete_term = va_term/emp_glob_ind_term*100) |>
  mutate(prod_tete_ternm = va_ternm/emp_glob_ind_ternm*100) |>
  mutate(prod_tete_marng = va_marng/emp_ind_globmarng*100) |>
  mutate(prod_tete_tot = va_tot/emp_ind_globtot*100) |>
  mutate(smpt_cnt_tot = wage_tot/emp_ind_globtot*100) |>
  mutate(smpt_cnt_marng = wage_marng/emp_ind_globmarng*100) |>
  
  mutate(prod_travail_ag = va_ag/vol_travail_glob_ag*100) |>
  mutate(prod_travail_man = va_man/vol_travail_glob_man*100) |>
  mutate(prod_travail_term = va_term/vol_travail_glob_term*100) |>
  mutate(prod_travail_ternm = va_ternm/vol_travail_glob_ternm*100) |>
  mutate(prod_travail_marng = va_marng/vol_travail_glob_marng*100) |>
  mutate(prod_travail_tot = va_tot/vol_travail_glob_tot*100) |>
  mutate(tcam_prod_travail_tot = ifelse(substr(DATE, 6, 7) == "03", (prod_travail_tot+lead(prod_travail_tot, 1)+lead(prod_travail_tot, 2)+lead(prod_travail_tot, 3))/(lag(prod_travail_tot, 4)+lag(prod_travail_tot, 3)+lag(prod_travail_tot, 2)+lag(prod_travail_tot, 1))-1, NA)) |>
  mutate(tcam_prod_tete_tot = ifelse(substr(DATE, 6, 7) == "03", (prod_tete_tot+lead(prod_tete_tot, 1)+lead(prod_tete_tot, 2)+lead(prod_tete_tot, 3))/(lag(prod_tete_tot, 4)+lag(prod_tete_tot, 3)+lag(prod_tete_tot, 2)+lag(prod_tete_tot, 1))-1, NA))


cnt_cb_complet <-  fill(cnt_cb_complet, tcam_prod_travail_tot, tcam_prod_tete_tot) 
cnt_cb_complet <-  mutate(cnt_cb_complet, tcam_prod_travail_tot= ifelse(substr(DATE, 1, 4) == "2022", NA, tcam_prod_travail_tot))
cnt_cb_complet <-  mutate(cnt_cb_complet, tcam_prod_tete_tot = ifelse(substr(DATE, 1, 4) == "2022", NA, tcam_prod_tete_tot)) 

cnt_cb_complet <- relocate(cnt_cb_complet, tcam_va_marng, tcam_va_tot, tcam_prod_travail_tot, tcam_prod_tete_tot, .after = prod_travail_tot)

###Passage en base 100 à la date choisie 
cb_complet_base100 <- mutate_at(cnt_cb_complet, vars(2:81), scale_2019t4)


####Décomposition du CNT manufacturier 
cnt_indus <- cb_2014|>
  filter(CNA_PRODUIT == "DIM-CNT" 
         |CNA_PRODUIT =="DI-CNT"
         |CNA_PRODUIT =="A17-C1"
         |CNA_PRODUIT =="A17-C2"
         |CNA_PRODUIT =="A17-C3"
         |CNA_PRODUIT =="A17-C4"
         |CNA_PRODUIT =="A17-C5"
         |CNA_PRODUIT =="A17-DE"
         |CNA_PRODUIT =="A17-FZ") |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="DIM-CNT", "man")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="DI-CNT", "indus")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-C1", "alim")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-C2", "raf")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-C3", "info")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-C4", "trans")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-C5", "autre")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-DE", "energ")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-FZ", "constr")) |>
  
  
  filter(OPERATION == "B1" | OPERATION == "B2" |  OPERATION == "EMP" | OPERATION == "EMPNS" |  OPERATION == "DHES" |  OPERATION == "DHET" |  OPERATION == "VHTS" |  OPERATION == "VHTT" |  OPERATION == "D1") |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="B1", "va")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="B2", "ebe")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="EMP", "emp")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="EMPNS", "empns")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="DHES", "duree_travail_sal")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="DHET", "duree_travail_tot")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="VHTS", "volume_travail_sal")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="VHTT", "volume_travail_tot")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="D1", "wage")) |>
  mutate(UNIT_MEASURE  = replace(UNIT_MEASURE , UNIT_MEASURE =="EUROS", "euro")) |>
  mutate(UNIT_MEASURE  = replace(UNIT_MEASURE , UNIT_MEASURE =="HEURES", "h")) |>
  mutate(UNIT_MEASURE  = replace(UNIT_MEASURE , UNIT_MEASURE =="INDIVIDUS", "ind")) |>
  mutate(UNIT_MEASURE  = replace(UNIT_MEASURE , UNIT_MEASURE =="ETP", "etp")) |>
  filter(VALORISATION!= "V") |>
  mutate(op_pr_val = paste(OPERATION, CNA_PRODUIT, UNIT_MEASURE, sep = "_")) |>
  select(DATE, TIME_PERIOD, OBS_VALUE, OPERATION, CNA_PRODUIT, VALORISATION, UNIT_MEASURE, op_pr_val) |>
  spread(key = "op_pr_val",
         value = "OBS_VALUE") 
# cnt_indus$DATE <- str_replace(cnt_indus$DATE,"-01-", "-03-")
# cnt_indus$DATE <- str_replace(cnt_indus$DATE,"-04-", "-06-")
# cnt_indus$DATE <- str_replace(cnt_indus$DATE,"-07-", "-09-")
# cnt_indus$DATE <- str_replace(cnt_indus$DATE,"-10-", "-12-")
cnt_indus$DATE <-as.Date(paste(cnt_indus$DATE,1,sep="-"))
cnt_indus <- mutate_at(cnt_indus,  vars(7:105), list(~ recode(., `NULL` = 0))) |>
  mutate_at(vars(7:105), as.numeric) |>
  mutate_at(vars(7:105), ~replace(., is.na(.), 0)) |>
  group_by(DATE) |>
  summarise_at(vars(6:104),              # Specify column
               list(name = sum)) 

names(cnt_indus) = gsub(pattern = "_name", replacement = "", x = names(cnt_indus))
names(cnt_indus) = gsub(pattern = "_euro", replacement = "", x = names(cnt_indus))
names(cnt_indus) = gsub(pattern = "_h", replacement = "", x = names(cnt_indus))


cnt_indus <- cnt_indus |>
  mutate(prod_tete_indus = va_indus/emp_indus_ind*100) |>
  mutate(prod_tete_man = va_man/emp_man_ind*100) |>
  mutate(prod_tete_alim = va_alim/emp_alim_ind*100) |>
  mutate(prod_tete_raf = va_raf/emp_raf_ind*100) |>
  mutate(prod_tete_info = va_info/emp_info_ind*100) |>
  mutate(prod_tete_trans = va_trans/emp_trans_ind*100) |>
  mutate(prod_tete_autre = va_autre/emp_autre_ind*100) |>
  mutate(prod_tete_energ = va_energ/emp_energ_ind*100) |>
  mutate(prod_tete_constr = va_constr/emp_constr_ind*100) |>
  
  mutate(tx_marge_indus = ebe_indus/va_indus*100) |>
  mutate(tx_marge_man = ebe_man/va_man*100) |>
  mutate(tx_marge_alim = ebe_alim/va_alim*100) |>
  mutate(tx_marge_raf = ebe_raf/va_raf*100) |>
  mutate(tx_marge_info = ebe_info/va_info*100) |>
  mutate(tx_marge_trans = ebe_trans/va_trans*100) |>
  mutate(tx_marge_autre = ebe_autre/va_autre*100) |>
  mutate(tx_marge_energ = ebe_energ/va_energ*100) |>
  mutate(tx_marge_constr = ebe_constr/va_constr*100) |>
  
  mutate(prod_travail_indus = va_indus/volume_travail_tot_indus*100) |>
  mutate(prod_travail_man = va_man/volume_travail_tot_man*100) |>
  mutate(prod_travail_alim = va_alim/volume_travail_tot_alim*100) |>
  mutate(prod_travail_raf = va_raf/volume_travail_tot_raf*100) |>
  mutate(prod_travail_info = va_info/volume_travail_tot_info*100) |>
  mutate(prod_travail_trans = va_trans/volume_travail_tot_trans*100) |>
  mutate(prod_travail_autre = va_autre/volume_travail_tot_autre*100) |>
  mutate(prod_travail_energ = va_energ/volume_travail_tot_energ*100) |>
  mutate(prod_travail_constr = va_constr/volume_travail_tot_constr*100) |>
  
  mutate(smpt_indus = wage_indus/emp_indus_ind*100) |>
  mutate(smpt_man = wage_man/emp_man_ind*100) |>
  mutate(smpt_alim = wage_alim/emp_alim_ind*100) |>
  mutate(smpt_raf = wage_raf/emp_raf_ind*100) |>
  mutate(smpt_info = wage_info/emp_info_ind*100) |>
  mutate(smpt_trans = wage_trans/emp_trans_ind*100) |>
  mutate(smpt_autre = wage_autre/emp_autre_ind*100) |>
  mutate(smpt_energ = wage_energ/emp_energ_ind*100) |>
  mutate(smpt_constr = wage_constr/emp_constr_ind*100) 

cnt_indus_base100 <- mutate_at(cnt_indus, vars(2:136), scale_2019t4)

###CNT services
cnt_service <- cb_2014|>
  filter(CNA_PRODUIT == "DSM-CNT"  
         |CNA_PRODUIT =="DSN-CNT"
         |CNA_PRODUIT =="A17-GZ"
         |CNA_PRODUIT =="A17-HZ"
         |CNA_PRODUIT =="A17-IZ"
         |CNA_PRODUIT =="A17-JZ"
         |CNA_PRODUIT =="A17-KZ"
         |CNA_PRODUIT =="A17-LZ"
         |CNA_PRODUIT =="A17-MN"
         |CNA_PRODUIT =="A17-RU") |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="DSM-CNT", "termar")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="DSN-CNT", "ternm")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-GZ", "com")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-HZ", "trans")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-IZ", "heb")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-JZ", "infocom")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-KZ", "fin")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-LZ", "imm")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-MN", "sciad")) |>
  mutate(CNA_PRODUIT  = replace(CNA_PRODUIT, CNA_PRODUIT=="A17-RU", "autre")) |>
  
  filter(OPERATION == "B1" | OPERATION == "B2" | OPERATION == "EMP" | OPERATION == "EMPNS" |  OPERATION == "DHES" |  OPERATION == "DHET" |  OPERATION == "VHTS" |  OPERATION == "VHTT" |  OPERATION == "D1") |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="B1", "va")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="B2", "ebe")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="EMP", "emp")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="EMPNS", "empns")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="DHES", "duree_travail_sal")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="DHET", "duree_travail_tot")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="VHTS", "volume_travail_sal")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="VHTT", "volume_travail_tot")) |>
  mutate(OPERATION  = replace(OPERATION, OPERATION=="D1", "wage")) |>
  mutate(UNIT_MEASURE  = replace(UNIT_MEASURE , UNIT_MEASURE =="EUROS", "euro")) |>
  mutate(UNIT_MEASURE  = replace(UNIT_MEASURE , UNIT_MEASURE =="HEURES", "h")) |>
  mutate(UNIT_MEASURE  = replace(UNIT_MEASURE , UNIT_MEASURE =="INDIVIDUS", "ind")) |>
  mutate(UNIT_MEASURE  = replace(UNIT_MEASURE , UNIT_MEASURE =="ETP", "etp")) |>
  filter(VALORISATION!= "V") |>
  mutate(op_pr_val = paste(OPERATION, CNA_PRODUIT, UNIT_MEASURE, sep = "_")) |>
  select(DATE, TIME_PERIOD, OBS_VALUE, OPERATION, CNA_PRODUIT, VALORISATION, UNIT_MEASURE, op_pr_val) |>
  spread(key = "op_pr_val",
         value = "OBS_VALUE") 
# cnt_service$DATE <- str_replace(cnt_service$DATE,"-01-", "-03-")
# cnt_service$DATE <- str_replace(cnt_service$DATE,"-04-", "-06-")
# cnt_service$DATE <- str_replace(cnt_service$DATE,"-07-", "-09-")
# cnt_service$DATE <- str_replace(cnt_service$DATE,"-10-", "-12-")
cnt_service$DATE <-as.Date(paste(cnt_service$DATE,1,sep="-"))
cnt_service <- mutate_at(cnt_service,  vars(7:116), list(~ recode(., `NULL` = 0))) |>
  mutate_at(vars(7:116), as.numeric) |>
  mutate_at(vars(7:116), ~replace(., is.na(.), 0)) |>
  group_by(DATE) |>
  summarise_at(vars(6:115),              # Specify column
               list(name = sum)) 

names(cnt_service) = gsub(pattern = "_h_name", replacement = "", x = names(cnt_service))
names(cnt_service) = gsub(pattern = "_euro", replacement = "", x = names(cnt_service))
names(cnt_service) = gsub(pattern = "_name", replacement = "", x = names(cnt_service))

#cnt_service <- mutate_at(cnt_service, vars(2:111), scale_2019t4)

cnt_service <- cnt_service |>
  mutate(prod_tete_termar = va_termar/emp_termar_ind*100) |>
  mutate(prod_tete_ternm = va_ternm/emp_ternm_ind*100) |>
  mutate(prod_tete_com = va_com/emp_com_ind*100) |>
  mutate(prod_tete_trans = va_trans/emp_trans_ind*100) |>
  mutate(prod_tete_heb = va_heb/emp_heb_ind*100) |>
  mutate(prod_tete_infocom = va_infocom/emp_infocom_ind*100) |>
  mutate(prod_tete_fin = va_fin/emp_fin_ind*100) |>
  mutate(prod_tete_imm = va_imm/emp_imm_ind*100) |>
  mutate(prod_tete_sciad = va_sciad/emp_sciad_ind*100) |>
  mutate(prod_tete_autre = va_autre/emp_autre_ind*100) |>
  
  mutate(prod_travail_termar = va_termar/volume_travail_tot_termar*100) |>
  mutate(prod_travail_ternm = va_ternm/volume_travail_tot_ternm*100) |>
  mutate(prod_travail_com = va_com/volume_travail_tot_com*100) |>
  mutate(prod_travail_trans = va_trans/volume_travail_tot_trans*100) |>
  mutate(prod_travail_heb = va_heb/volume_travail_tot_heb*100) |>
  mutate(prod_travail_infocom = va_infocom/volume_travail_tot_infocom*100) |>
  mutate(prod_travail_fin = va_fin/volume_travail_tot_fin*100) |>
  mutate(prod_travail_imm = va_imm/volume_travail_tot_imm*100) |>
  mutate(prod_travail_sciad = va_sciad/volume_travail_tot_sciad*100) |>
  mutate(prod_travail_autre = va_autre/volume_travail_tot_autre*100) |>
  
  mutate(smpt_termar = wage_termar/emp_termar_ind*100) |>
  mutate(smpt_ternm = wage_ternm/emp_ternm_ind*100) |>
  mutate(smpt_com = wage_com/emp_com_ind*100) |>
  mutate(smpt_trans = wage_trans/emp_trans_ind*100) |>
  mutate(smpt_heb = wage_heb/emp_heb_ind*100) |>
  mutate(smpt_infocom = wage_infocom/emp_infocom_ind*100) |>
  mutate(smpt_fin = wage_fin/emp_fin_ind*100) |>
  mutate(smpt_imm = wage_imm/emp_imm_ind*100) |>
  mutate(smpt_sciad = wage_sciad/emp_sciad_ind*100)  |>
  mutate(smpt_autre = wage_autre/emp_autre_ind*100) 

cnt_service_base100 <- mutate_at(cnt_service, vars(2:141), scale_2019t4)



#############GRAPHIQUES COMPTES DE BRANCHE ##############

###Croissance trimestrielle du PIB + Composantes 
croiss_trim_pib <-  ggplot(data=contrib_pib_eer, aes(x=DATE, y=value, fill=forcats::fct_rev(Composante))) +
  geom_col()+
  geom_line(aes(y = croissance_pib)) + geom_point(aes(y = croissance_pib)) +
  labs(title = "Croissance trimestrielle du PIB et contributions à la croissance", 
       subtitle = "Lecture: Au 1er trimestre 2023, la croissance du PIB est de X %. La consommation contribue à X points de la croissance du PIB. \nSource : Insee (Comptes Nationaux Trimestriels, 1e estimation du T1-2023)\nCalculs et graphiques : @statjunior", 
       y = "Points de pourcentage", x = "Trimestre") +
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 12, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)
  ) +
  scale_x_date(limits = as.Date(c("2020-11-01", NA))) +
  scale_fill_discrete(breaks=c('Consommation', 'Investissement', 'Export', "Import", "Variations de stocks"))+
  #scale_color_manual(values=c("#F8766D","#C77CFF", "#00BA38", "#D39200", "#00B8E7"))+
  ylim(-2.5,5) 
croiss_trim_pib

###Niveau des composantes en volume par rapport au T4-2019 
plot_niv_composante_pib <-ggplot(data = pib_eer_2019,
                             aes(x = DATE)) +
  geom_line(aes(y = pib_tot_vol, color = "PIB"), size = 1.5) + 
  geom_line(aes(y = conso_tot_vol, color = "Consommation"), size = 1.1) + 
  geom_line(aes(y = fbcf_tot_vol, color = "Investissement"), size = 1.1) + 
  geom_line(aes(y = export_tot_vol, color = "Export"), size = 1.1) + 
  geom_line(aes(y = import_tot_vol, color = "Import"), size = 1.1) + 
  # geom_point(aes(y = pib_tot_vol, color = "PIB"), size = 1.3) + 
  # geom_point(aes(y = conso_tot_vol, color = "Consommation"), size = 1.1) + 
  # geom_point(aes(y = fbcf_tot_vol, color = "Investissement"), size = 1.1) + 
  # geom_point(aes(y = export_tot_vol, color = "Export"), size = 1.1) + 
  # geom_point(aes(y = import_tot_vol, color = "Import"), size = 1.1) + 
  labs(title = "Evolution des composantes du PIB depuis fin 2019", 
       subtitle = "Lecture : Au 1er trimestre 2023, le PIB croit de X % depuis le dernier trimestre 2019. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Base 100 au 4e trimestre 2019", x = "Date") +
  scale_x_date(limits = as.Date(c("2019-10-01", NA))) + 
  scale_color_manual(values = c("PIB"="red", "Consommation" = "orange", "Investissement" = "#00B8E7", "Export" ="#00BA38", "Import" = "#C77CFF", "Investissement" = "#D39200"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) + 
  ylim(65,110)
plot_niv_composante_pib 

##Glissement des déflateurs au dernier trimestre connu 
gliss_deflateurs <- select(pib_eer, DATE, 
                           gliss_annuel_deflateur_pib, gliss_annuel_deflateur_fbcf, gliss_annuel_deflateur_conso_men, gliss_annuel_deflateur_export, gliss_annuel_deflateur_import) |>
  filter(DATE == max(DATE)) 

gliss_deflateurs <- left_join(gliss_deflateurs, deflateur_vamarng, by = "DATE") |>
  select(-deflateur_va_marng)
gliss_deflateurs  <- pivot_longer(gliss_deflateurs, cols=c(-1), names_to = "Composante", values_to = "value") |>
  mutate(Composante = case_when(
    Composante == "gliss_annuel_deflateur_pib"~"PIB",
    Composante == "gliss_annuel_deflateur_fbcf"~"Investissement",
    Composante == "gliss_annuel_deflateur_conso_men"~"Consommation des ménages",
    Composante == "gliss_annuel_deflateur_export"~"Export",
    Composante == "gliss_annuel_deflateur_import"~"Import",
    Composante == "gliss_deflateur_va_marng"~"VA marchande",
  ))

plot_gliss_deflateurs <-ggplot(data = gliss_deflateurs,
                               aes(x = forcats::fct_relevel(Composante, c("PIB", "VA marchande", "Consommation des ménages", "Investissement", "Export", "Import"), after = 1),
                                   y = value)) +
  geom_col() +
  labs(title = "Glissement annuel des déflateurs de la comptabilité nationale", 
       subtitle = "Lecture : Au 1er trimestre 2023, le déflateur du PIB évolue de X %. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Pourcentage", x = "Composante du PIB") +
  scale_color_manual(name = "Composante", values=c("red","grey50","grey50","grey50","grey50","grey50")) +
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=7),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) +
  scale_fill_discrete(breaks=c("PIB","Consommation des ménages", "Investissement","Export", "Import"))
plot_gliss_deflateurs


####Niveau des déflateurs par rapport au T4-2019
niveau_deflateurs <- select(pib_eer, DATE, deflateur_pib, deflateur_conso_men, deflateur_export, deflateur_import, deflateur_fbcf) 
niveau_deflateurs <-left_join(niveau_deflateurs , deflateur_vamarng, by = "DATE") |>
  select(-gliss_deflateur_va_marng)

niveau_deflateurs_base100  <- mutate_at(niveau_deflateurs, vars(-1), scale_2019t4)

plot_niv_deflateurs <-ggplot(data = niveau_deflateurs_base100,
                             aes(x = DATE)) +
  geom_line(aes(y = deflateur_pib, color = "PIB"), size = 1.3) + 
  geom_line(aes(y = deflateur_va_marng, color = "VA marchande"), size = 1.1) + 
  geom_line(aes(y = deflateur_conso_men, color = "Conso des ménages"), size = 1.1) + 
  geom_line(aes(y = deflateur_export, color = "Export"), size = 1.1) + 
  geom_line(aes(y = deflateur_import, color = "Import"), size = 1.1) + 
  geom_line(aes(y = deflateur_fbcf, color = "Investissement"), size = 1.1) +
  # geom_point(aes(y = deflateur_pib, color = "PIB")) + 
  # geom_point(aes(y = deflateur_conso_men, color = "Conso des ménages")) + 
  # geom_point(aes(y = deflateur_export, color = "Export")) + 
  # geom_point(aes(y = deflateur_import, color = "Import")) + 
  # geom_point(aes(y = deflateur_fbcf, color = "Investissement")) +
  labs(title = "Evolution des déflateurs de la comptabilité nationale", 
       subtitle = "Lecture : Au 1er trimestre 2023, le déflateur du PIB évolue de X % par rapport au dernier trimestre 2019. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Pourcentage", x = "Composante du PIB") +
  scale_x_date(limits = as.Date(c("2019-10-01", NA))) + 
  scale_color_manual(values = c("PIB"="#F8766D", "VA marchande" = "orange", "Conso des ménages" = "#00B8E7", "Export" ="#00BA38", "Import" = "#C77CFF", "Investissement" = "#D39200"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=7),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) + 
  ylim(90,130)
plot_niv_deflateurs

###VA globale et productivité du travail 
prod_travail <- select(cnt_cb_complet, DATE, prod_travail_marng, va_marng)

prod_travail_2000 <- mutate_at(prod_travail, vars(-1), scale_2000t1)
prod_travail_2019 <- mutate_at(prod_travail, vars(-1), scale_2019t4)

plot_prod_travail_2000 <-ggplot(data = prod_travail_2000,
                             aes(x = DATE)) +
  geom_line(aes(y = va_marng, color = "Valeur ajoutée globale"), size = 1.3) + 
  geom_line(aes(y = prod_travail_marng, color = "Productivité horaire du travail"), size = 1.1) + 
  labs(title = "Solde des échanges extérieurs", 
       subtitle = "Lecture : Au 1er trimestre 2023, le solde de la balance commerciale est déficitaire de X points de la valeur ajoutée nationale. \nSource : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "Base 100 au 1er trimestre 2000", x = "Année") +
  scale_x_date(limits = as.Date(c("2000-01-01", NA))) + 
  scale_color_manual(values = c("Valeur ajoutée globale"="#F8766D","Productivité horaire du travail" = "#00B8E7"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) +
  ylim(100, 140)
plot_prod_travail_2000

plot_prod_travail_2019 <-ggplot(data = prod_travail_2019,
                           aes(x = DATE)) +
  geom_line(aes(y = va_marng, color = "Valeur ajoutée globale"), size = 1.3) + 
  geom_line(aes(y = prod_travail_marng, color = "Productivité horaire du travail"), size = 1.1) + 
  labs(title = "Valeur ajoutée et productivité du travail depuis la crise de la Covid", 
       subtitle = "Lecture : Au 1er trimestre 2023, la productivité horaire du travail recule de X % par rapport au 4e trimestre 2019. \nSource : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphique : @statjunior", 
       y = "Base 100 au 4e trimestre 2019", x = "Année") +
  scale_x_date(limits = as.Date(c("2019-10-01", NA))) + 
  scale_color_manual(values = c("Valeur ajoutée globale"="#F8766D","Productivité horaire du travail" = "#00B8E7"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) +
  ylim(80, 120)
plot_prod_travail_2019

#####VA par branche par rapport au T4-2019 
full_va <- select(cnt_cb_complet, DATE, va_tot, va_marng, va_ag, va_man, va_term, va_ternm)
va_man <- select(cnt_indus, DATE, va_alim, va_constr, va_energ, va_indus, va_info, va_raf, va_trans)|>
  rename("va_mat_trans"="va_trans")
va_service <- select(cnt_service, DATE, va_com, va_fin, va_heb, va_imm,va_infocom, va_sciad, va_trans)  |>
  rename("va_service_trans"="va_trans")
full_va <- full_join(full_va, va_man, by = "DATE", type = "left")
full_va <- full_join(full_va, va_service, by = "DATE", type = "left")
full_va <- full_va |>
  select(DATE, va_tot, va_marng, va_indus, va_alim, va_energ, va_raf, va_info, va_mat_trans, va_energ, va_constr, 
va_term, va_com, va_service_trans, va_heb, va_infocom, va_fin, va_imm, va_sciad, va_ternm, va_ag) |>
  relocate(DATE, va_tot, va_marng, va_indus, va_alim, va_energ, va_raf, va_info, va_mat_trans, va_energ, va_constr, va_term, va_com, va_service_trans, va_heb, va_infocom, va_fin, va_imm, va_sciad, va_ternm, va_ag, .before = NULL)

full_va_2019 <- mutate_at(full_va, vars(-1), scale_2019t4)
full_va_2019 <- mutate_at(full_va_2019, vars(-1), ~.-100) |>
  filter(DATE ==max(DATE) | DATE ==max(full_va_2019$DATE[full_va_2019$DATE!=max(full_va_2019$DATE)])) |>
  mutate(DATE = as.character(DATE))
full_va_2019 <- pivot_longer(full_va_2019,cols=c(-1), names_to = "Composante", values_to = "value") |>
  mutate(Composante = case_when(
    Composante == "va_tot"~"Global",
    Composante == "va_marng"~"Marchand non agricole",
    Composante == "va_indus"~"Industrie",
    Composante == "va_alim"~"Agro-alimentaire",
    Composante == "va_energ"~"Energie, eau, déchets",
    Composante == "va_raf"~"Cokéfaction-raffinage",
    Composante == "va_info"~"Biens d'équipement",
    Composante == "va_mat_trans"~"Matériels de transport",
    Composante == "va_constr"~"Construction",
    Composante == "va_term"~"Tertiaire marchand",
    Composante == "va_com"~"Commerce",
    Composante == "va_service_trans"~"Services de transport",
    Composante == "va_heb"~"Hébergement-restauration",
    Composante == "va_infocom"~"Info-communication",
    Composante == "va_fin"~"Services financiers",
    Composante == "va_imm"~"Services immobiliers",
    Composante == "va_sciad"~"Services aux entreprises",
    Composante == "va_ternm"~"Tertiaire non marchand",
    Composante == "va_ag"~"Agriculture"
    
  ))

  ###Graphique VA par rapport à 2019
plot_va_sectorielle_2019t4 <-ggplot(data = full_va_2019,
                               aes(x = forcats::fct_relevel(Composante, c("Global", "Marchand non agricole","Industrie","Agro-alimentaire",
                                                                       "Energie, eau, déchets","Cokéfaction-raffinage","Biens d'équipement",
                                                                           "Matériels de transport", "Construction",
                                                                          "Tertiaire marchand", "Commerce",  "Services de transport",
                                                                          "Hébergement-restauration","Info-communication","Services financiers",
                                                                           "Services immobiliers","Services aux entreprises","Tertiaire non marchand","Agriculture"), after = 1),
                                y = value, fill = DATE)) +
  geom_col(position = "dodge") +
  labs(title = "Evolution de la Valeur Ajoutée par secteur depuis la crise du Covid", 
       subtitle = "Lecture : Au 1er trimestre 2023, la valeur ajoutée des secteurs marchands non agricoles progresse de X % par rapport au 4e trimestre 2019. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Variation par rapport au T4-2019 (%)", x = "Secteur") +
  #scale_color_manual(name = "Composante", values=c("red","grey50","grey50","grey50","grey50","grey50")) +
  theme_grey()+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(size=9), 
        legend.text = element_text(size=7),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic"))
  plot_va_sectorielle_2019t4                                           
                                                                         
####Productivité du travail par branche par rapport au T4-2019 
full_prod_travail <- select(cnt_cb_complet, DATE, prod_travail_tot, prod_travail_marng, prod_travail_ag, prod_travail_man, prod_travail_term, prod_travail_ternm)
prod_travail_man <- select(cnt_indus, DATE, prod_travail_alim, prod_travail_constr, prod_travail_energ, prod_travail_indus, prod_travail_info, prod_travail_raf, prod_travail_trans)|>
  rename("prod_travail_mat_trans"="prod_travail_trans")
prod_travail_service <- select(cnt_service, DATE, prod_travail_com, prod_travail_fin, prod_travail_heb, prod_travail_imm,prod_travail_infocom, prod_travail_sciad, prod_travail_trans)  |>
  rename("prod_travail_service_trans"="prod_travail_trans")
full_prod_travail <- full_join(full_prod_travail, prod_travail_man, by = "DATE", type = "left")
full_prod_travail <- full_join(full_prod_travail, prod_travail_service, by = "DATE", type = "left")
full_prod_travail <- full_prod_travail |>
  select(DATE, prod_travail_tot, prod_travail_marng, prod_travail_indus, prod_travail_alim, prod_travail_energ, prod_travail_raf, prod_travail_info, prod_travail_mat_trans, prod_travail_energ, prod_travail_constr, 
         prod_travail_term, prod_travail_com, prod_travail_service_trans, prod_travail_heb, prod_travail_infocom, prod_travail_fin, prod_travail_imm, prod_travail_sciad, prod_travail_ternm, prod_travail_ag) |>
  relocate(DATE, prod_travail_tot, prod_travail_marng, prod_travail_indus, prod_travail_alim, prod_travail_energ, prod_travail_raf, prod_travail_info, prod_travail_mat_trans, prod_travail_energ, prod_travail_constr, prod_travail_term, prod_travail_com, prod_travail_service_trans, prod_travail_heb, prod_travail_infocom, prod_travail_fin, prod_travail_imm, prod_travail_sciad, prod_travail_ternm, prod_travail_ag, .before = NULL)

full_prod_travail_2019 <- mutate_at(full_prod_travail, vars(-1), scale_2019t4)
full_prod_travail_2019 <- mutate_at(full_prod_travail_2019, vars(-1), ~.-100) |>
  filter(DATE ==max(DATE))
full_prod_travail_2019 <- pivot_longer(full_prod_travail_2019,cols=c(-1), names_to = "Composante", values_to = "value") |>
  mutate(Composante = case_when(
    Composante == "prod_travail_tot"~"Global",
    Composante == "prod_travail_marng"~"Marchand non agricole",
    Composante == "prod_travail_indus"~"Industrie",
    Composante == "prod_travail_alim"~"Agro-alimentaire",
    Composante == "prod_travail_energ"~"Energie, eau, déchets",
    Composante == "prod_travail_raf"~"Cokéfaction-raffinage",
    Composante == "prod_travail_info"~"Biens d'équipement",
    Composante == "prod_travail_mat_trans"~"Matériels de transport",
    Composante == "prod_travail_constr"~"Construction",
    Composante == "prod_travail_term"~"Tertiaire marchand",
    Composante == "prod_travail_com"~"Commerce",
    Composante == "prod_travail_service_trans"~"Services de transport",
    Composante == "prod_travail_heb"~"Hébergement-restauration",
    Composante == "prod_travail_infocom"~"Info-communication",
    Composante == "prod_travail_fin"~"Services financiers",
    Composante == "prod_travail_imm"~"Services immobiliers",
    Composante == "prod_travail_sciad"~"Services aux entreprises",
    Composante == "prod_travail_ternm"~"Tertiaire non marchand",
    Composante == "prod_travail_ag"~"Agriculture"
    
  ))


  ###graphique productivité du travail 
plot_prod_travail_sectorielle_2019t4 <-ggplot(data = full_prod_travail_2019,
                                    aes(x = forcats::fct_relevel(Composante, c("Global", "Marchand non agricole","Industrie","Agro-alimentaire",
                                                                               "Energie, eau, déchets","Cokéfaction-raffinage","Biens d'équipement",
                                                                               "Matériels de transport", "Construction",
                                                                               "Tertiaire marchand", "Commerce",  "Services de transport",
                                                                               "Hébergement-restauration","Info-communication","Services financiers",
                                                                               "Services immobiliers","Services aux entreprises","Tertiaire non marchand","Agriculture"), after = 1),
                                        y = value)) +
  geom_col() +
  labs(title = "Evolution de la Productivité horaire du travail par secteur depuis la crise du Covid", 
       subtitle = "Lecture : Au 1er trimestre 2023, la productivité horaire du travail des secteurs marchands non agricoles progresse de X % par rapport au 4e trimestre 2019. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Variation par rapport au T4-2019 (%)", x = "Secteur") +
  #scale_color_manual(name = "Composante", values=c("red","grey50","grey50","grey50","grey50","grey50")) +
  theme_grey()+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(size=9), 
        legend.text = element_text(size=7),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) +
  scale_fill_discrete(breaks=c("Global", "Marchand non agricole","Industrie","Agro-alimentaire",
                               "Energie, eau, déchets","Cokéfaction-raffinage","Biens d'équipement",
                               "Matériels de transport", "Construction",
                               "Tertiaire marchand", "Commerce",  "Services de transport",
                               "Hébergement-restauration","Info-communication","Services financiers",
                               "Services immobiliers","Services aux entreprises","Tertiaire non marchand","Agriculture"))
plot_prod_travail_sectorielle_2019t4         

###graphique heures travaillées
full_heures <- select(cnt_cb_complet, DATE, vol_travail_glob_tot, vol_travail_glob_marng, vol_travail_glob_ag, vol_travail_glob_man, vol_travail_glob_term, vol_travail_glob_ternm)
volume_travail_man <- select(cnt_indus, DATE, volume_travail_tot_alim, volume_travail_tot_constr, volume_travail_tot_energ, volume_travail_tot_indus, volume_travail_tot_info, volume_travail_tot_raf, volume_travail_tot_trans)|>
  rename("volume_travail_tot_mat_trans"="volume_travail_tot_trans")
volume_travail_service <- select(cnt_service, DATE, volume_travail_tot_com, volume_travail_tot_fin, volume_travail_tot_heb, volume_travail_tot_imm,volume_travail_tot_infocom, volume_travail_tot_sciad, volume_travail_tot_trans)  |>
  rename("volume_travail_tot_service_trans"="volume_travail_tot_trans")
full_heures <- full_join(full_heures, volume_travail_man, by = "DATE", type = "left")
full_heures <- full_join(full_heures, volume_travail_service, by = "DATE", type = "left")
full_heures <- full_heures |>
  select(DATE, vol_travail_glob_tot, vol_travail_glob_marng, volume_travail_tot_indus, volume_travail_tot_alim, volume_travail_tot_energ, volume_travail_tot_raf, volume_travail_tot_info, volume_travail_tot_mat_trans, volume_travail_tot_energ, volume_travail_tot_constr, 
         vol_travail_glob_term, volume_travail_tot_com, volume_travail_tot_service_trans, volume_travail_tot_heb, volume_travail_tot_infocom, volume_travail_tot_fin, volume_travail_tot_imm, volume_travail_tot_sciad, vol_travail_glob_ternm, vol_travail_glob_ag) |>
  relocate(DATE, vol_travail_glob_tot, vol_travail_glob_marng, volume_travail_tot_indus, volume_travail_tot_alim, volume_travail_tot_energ, volume_travail_tot_raf, volume_travail_tot_info, volume_travail_tot_mat_trans, volume_travail_tot_energ, volume_travail_tot_constr, 
           vol_travail_glob_term, volume_travail_tot_com, volume_travail_tot_service_trans, volume_travail_tot_heb, volume_travail_tot_infocom, volume_travail_tot_fin, volume_travail_tot_imm, volume_travail_tot_sciad, vol_travail_glob_ternm, vol_travail_glob_ag, .before = NULL)

full_heures_2019 <- mutate_at(full_heures, vars(-1), scale_2019t4)
full_heures_2019 <- mutate_at(full_heures_2019, vars(-1), ~.-100) |>
  filter(DATE ==max(DATE))
full_heures_2019 <- pivot_longer(full_heures_2019,cols=c(-1), names_to = "Composante", values_to = "value") |>
  mutate(Composante = case_when(
    Composante == "vol_travail_glob_tot"~"Global",
    Composante == "vol_travail_glob_marng"~"Marchand non agricole",
    Composante == "volume_travail_tot_indus"~"Industrie",
    Composante == "volume_travail_tot_alim"~"Agro-alimentaire",
    Composante == "volume_travail_tot_energ"~"Energie, eau, déchets",
    Composante == "volume_travail_tot_raf"~"Cokéfaction-raffinage",
    Composante == "volume_travail_tot_info"~"Biens d'équipement",
    Composante == "volume_travail_tot_mat_trans"~"Matériels de transport",
    Composante == "volume_travail_tot_constr"~"Construction",
    Composante == "vol_travail_glob_term"~"Tertiaire marchand",
    Composante == "volume_travail_tot_com"~"Commerce",
    Composante == "volume_travail_tot_service_trans"~"Services de transport",
    Composante == "volume_travail_tot_heb"~"Hébergement-restauration",
    Composante == "volume_travail_tot_infocom"~"Info-communication",
    Composante == "volume_travail_tot_fin"~"Services financiers",
    Composante == "volume_travail_tot_imm"~"Services immobiliers",
    Composante == "volume_travail_tot_sciad"~"Services aux entreprises",
    Composante == "vol_travail_glob_ternm"~"Tertiaire non marchand",
    Composante == "vol_travail_glob_ag"~"Agriculture"
    
  ))

    ###Graphique heures travaillées
plot_heures_travaillées_2019t4 <-ggplot(data = full_heures_2019,
                                              aes(x = forcats::fct_relevel(Composante, c("Global", "Marchand non agricole","Industrie","Agro-alimentaire",
                                                                                         "Energie, eau, déchets","Cokéfaction-raffinage","Biens d'équipement",
                                                                                         "Matériels de transport", "Construction",
                                                                                         "Tertiaire marchand", "Commerce",  "Services de transport",
                                                                                         "Hébergement-restauration","Info-communication","Services financiers",
                                                                                         "Services immobiliers","Services aux entreprises","Tertiaire non marchand","Agriculture"), after = 1),
                                                  y = value)) +
  geom_col() +
  labs(title = "Evolution du Volume d'heures travaillées par secteur depuis la crise du Covid", 
       subtitle = "Lecture : Au 1er trimestre 2023, la productivité horaire du travail des secteurs marchands non agricoles progresse de X % par rapport au 4e trimestre 2019. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Variation par rapport au T4-2019 (%)", x = "Secteur") +
  #scale_color_manual(name = "Composante", values=c("red","grey50","grey50","grey50","grey50","grey50")) +
  theme_grey()+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90),
        legend.title = element_text(size=9), 
        legend.text = element_text(size=7),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) +
  scale_fill_discrete(breaks=c("Global", "Marchand non agricole","Industrie","Agro-alimentaire",
                               "Energie, eau, déchets","Cokéfaction-raffinage","Biens d'équipement",
                               "Matériels de transport", "Construction",
                               "Tertiaire marchand", "Commerce",  "Services de transport",
                               "Hébergement-restauration","Info-communication","Services financiers",
                               "Services immobiliers","Services aux entreprises","Tertiaire non marchand","Agriculture"))
plot_heures_travaillées_2019t4   

####graphique cumulé de la VA, heures travaillées et prod par secteur 
full_va_prod_2019 <- left_join(full_va_2019, full_prod_travail_2019, by = c("DATE", "Composante")) |>
  rename("value_va"="value.x", "value_prod_travail"="value.y")
full_va_prod_2019 <- left_join(full_va_prod_2019, full_heures_2019, by = c("DATE", "Composante")) |>
  rename("value_heures"= "value")

full_va_prod_2019 <- pivot_longer(full_va_prod_2019,cols=c(3,5), names_to = "variable", values_to = "value") |>
  mutate(variable = case_when(
    variable == "value_heures"~"Heures travaillées",
    variable == "value_va"~"Valeur ajoutée"
    ))
full_va_prod_2019$variable <- as.factor(full_va_prod_2019$variable)
full_va_prod_2019$variable <- relevel(full_va_prod_2019$variable,ref = "Valeur ajoutée")

plot_va_prod_heures_2019t4 <-ggplot(data = full_va_prod_2019,
                                        aes(y = value, fill = variable,x = forcats::fct_relevel(Composante, c("Global", "Marchand non agricole","Industrie","Agro-alimentaire",
                                                                                   "Energie, eau, déchets","Cokéfaction-raffinage","Biens d'équipement",
                                                                                   "Matériels de transport", "Construction",
                                                                                   "Tertiaire marchand", "Commerce",  "Services de transport",
                                                                                   "Hébergement-restauration","Info-communication","Services financiers",
                                                                                   "Services immobiliers","Services aux entreprises","Tertiaire non marchand","Agriculture"), after = 1), 
                                           
                                            )) +
  geom_col(position='dodge') +
  geom_point(aes(y=value_prod_travail, color =  "Productivité horaire du travail"),size = 1.5,) +
  labs(title = "Evolution de la Productivité horaire du travail par secteur depuis la crise du Covid", 
       subtitle = "Lecture : Au 1er trimestre 2023, la productivité horaire du travail des secteurs marchands non agricoles recule de X % par rapport au 4e trimestre 2019. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Variation par rapport au T4-2019 (%)", x = "Secteur") +
  #scale_color_manual(name = "Composante", values=c("red","grey50","grey50","grey50","grey50","grey50")) +
  theme_grey()+
  scale_colour_manual(values = c("Productivité horaire du travail" = "purple")) +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, size = 10),
        legend.title = element_text(size=9), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic"))
plot_va_prod_heures_2019t4   

####Déficit commerical en Biens et services (avec et sans énergie)
def_commercial <- mutate(pib_2014_bis, compo = paste0(TITLE_FR1, "_",TITLE_FR3)) |>
  filter(is.na(TITLE_FR2)) |>
  filter(compo == "import_tot_val" |compo == "import_tot_vol"|compo == "export_tot_vol"|compo == "export_tot_val") |>
  select(DATE, OBS_VALUE, compo) |>
  pivot_wider(names_from = compo, values_from = OBS_VALUE, id_cols = DATE) |>
  mutate(def_commercial_valeur = export_tot_val-import_tot_val) 

def_commercial <- left_join(def_commercial, va_val_vol, by = "DATE") |>
  select(-va_valeur_marng, -va_tot, -va_marng) |>
  mutate(def_commercial_vatot = def_commercial_valeur/va_valeur_tot*100)

def_commercial_energie <- left_join(import_cb_val, export_cb_val, by ="DATE") |>
  mutate(def_energ=(export_energ+export_raf-import_raf-import_energ)*1000) 

def_commercial_energie$DATE <- str_replace(def_commercial_energie$DATE,"T1", "-01-01")
def_commercial_energie$DATE <- str_replace(def_commercial_energie$DATE,"T2", "-04-01")
def_commercial_energie$DATE <- str_replace(def_commercial_energie$DATE,"T3", "-07-01")
def_commercial_energie$DATE <- str_replace(def_commercial_energie$DATE,"T4", "-10-01")
def_commercial_energie <- mutate(def_commercial_energie, DATE =as.Date(DATE))

def_commercial <- left_join(def_commercial, def_commercial_energie, by ="DATE") |>
  mutate(def_hors_energie = def_commercial_valeur - def_energ) |>
  mutate(def_hors_energie_vatot = def_hors_energie/va_valeur_tot*100)

plot_def_commercial <-ggplot(data = def_commercial ,
                             aes(x = DATE)) +
  geom_line(aes(y = def_commercial_vatot, color = "Solde extérieur"), size = 1.3) + 
  geom_line(aes(y = def_hors_energie_vatot, color = "Hors secteurs énergétiques"), size = 1.1) + 
  labs(title = "Solde des échanges extérieurs", 
       subtitle = "Lecture : Au 1er trimestre 2023, le solde de la balance commerciale est déficitaire de X points de la valeur ajoutée nationale. \nSource : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "Pourcentage de la valeur ajoutée nationale (%)", x = "Année") +
  scale_x_date(limits = as.Date(c("1970-01-01", NA))) + 
  scale_color_manual(values = c("Solde extérieur"="#F8766D","Hors secteurs énergétiques" = "#00B8E7"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) + 
  ylim(-6,6)
plot_def_commercial

######Graphiques marges et va superprofits 
###Graphique de VA superprofits 
va_glob<- select(cnt_cb_complet, DATE, va_tot)
ebe_service <- select(cnt_service, DATE, ebe_trans, ebe_com, va_trans) |>
  rename(ebe_trans_service = ebe_trans)
ebe_indus <- select(cnt_indus, DATE, ebe_raf, ebe_energ, ebe_alim, va_alim, va_energ, va_raf) 
ebe_superprofits <- left_join(ebe_indus, va_glob, by = "DATE") 
ebe_superprofits <- left_join(ebe_superprofits, ebe_service, by = "DATE")
ebe_superprofits <- ebe_superprofits |> 
  select(DATE, ebe_energ, va_tot, ebe_raf, ebe_trans_service, ebe_alim, ebe_com, va_alim, va_energ, va_trans, va_raf) |>
  mutate(tx_marge_va_glob_energ = ebe_energ/va_tot) |>
  mutate(tx_marge_va_glob_trans = ebe_trans_service/va_tot) |>
  mutate(tx_marge_raf = ebe_raf/va_tot) |>
  mutate(tx_marge_alim_tot= ebe_alim/va_tot) |>
  mutate(tx_marge_alim_secteur= ebe_alim/va_alim) |>
  mutate(tx_marge_alim_secteur= tx_marge_alim_secteur*100) |>
  mutate(tx_marge_energ_secteur = ebe_energ/va_energ*100) |>
  mutate(tx_marge_raf_secteur = ebe_raf/va_raf*100) |>
  mutate(tx_marge_service_trans_secteur = ebe_trans_service/va_trans*100) |>
  mutate(tx_marge_com= ebe_com/va_tot) |>
  mutate(tx_marge_superprofits_energie = tx_marge_raf+tx_marge_va_glob_trans+tx_marge_va_glob_energ)
  
###EBE Energie (DE/energ), raffinerie (C2/raf), services de transport (HZ/trans) rapportée à la VA totale 
graph_superprofits_energie <- ggplot(subset(ebe_superprofits, DATE >= as.Date('1970-01-01')), aes(x = DATE)) +
 geom_line(aes(y = tx_marge_superprofits_energie), color  = "blue", size = 1.1)  +
  labs(title = "Excédent Brut d'Exploitation des secteurs énergétiques, de raffinerie et des services de transport", 
       subtitle = "Lecture : Au 1er trimestre 2023, l'excédent brut d'exploitation des secteurs énergétiques, raffinerie et service de transport représente X % de la valeur ajoutée totale française. \nSource : Insee (Comptes trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "EBE / VA totale française", x = "Année") +
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=7),
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")
  )
graph_superprofits_energie

#####Taux de marges 
graph_tx_marge_energie_alim <- ggplot(subset(ebe_superprofits, DATE >= as.Date('1970-01-01')), aes(x = DATE)) +
   geom_line(aes(y = tx_marge_alim_secteur, color = "Industrie agro-alimentaire"), size = 1.1)  +
   geom_line(aes(y = tx_marge_energ_secteur, color = "Energie, eau, déchets"), size = 1.1)  +
 geom_line(aes(y = tx_marge_raf_secteur, color = "Cockéfaction et raffinage"))  +
 geom_line(aes(y = tx_marge_service_trans_secteur, color = "Services de transport"), size = 1.1)  +
  labs(title = "Taux de marge des secteurs énergétiques et alimentaires", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux de marge de l'industrie agro-alimentaire est de X %. \nSource : Insee (Comptes trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "Taux de marge", x = "Année")  +
  scale_colour_manual(values = c("Industrie agro-alimentaire" = "red", "Energie, eau, déchets" = "brown", "Cockéfaction et raffinage" = "black",  "Services de transport" = "purple")) +
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")
  )
graph_tx_marge_energie_alim

graph_marge_va_energie_alim <- ggplot(subset(ebe_superprofits, DATE >= as.Date('1970-01-01')), aes(x = DATE)) +
  geom_line(aes(y = tx_marge_alim_tot, color = "Industrie agro-alimentaire"), size = 1.1)  +
   geom_line(aes(y = tx_marge_superprofits_energie, color = "Energie, raffinerie, service de transports"), size = 1.1)  +
  labs(title = "Excédents Bruts d'Exploitation des secteurs énergétiques et de l'industrie agro-alimentaire", 
       subtitle = "Lecture : Au 1er trimestre 2023, l'excédent brut d'exploitation des secteurs énergétiques, raffinerie et service de transport représente X % de la valeur ajoutée totale française. \nSource : Insee (Comptes trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "EBE / VA totale française", x = "Année") +
  scale_colour_manual(values = c("Energie, raffinerie, service de transports" = "#00B8E7", "Industrie agro-alimentaire" = "#F8766D")) +
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")
      ) 
graph_marge_va_energie_alim

########Dépenses de conso trimestrielles 
cb_conso_trim =
  get_insee_idbank(opebs) %>%
  split_title()

analyse_conso_menages <- filter(cb_conso_trim, TITLE_FR3=="Volume aux prix de l'année précédente chaînés") |>
  # filter(TITLE_FR2=="Produits agro-alimentaires" | TITLE_FR2=="Services financiers" | TITLE_FR2=="Construction" | 	
  # TITLE_FR2=="Énergie, eau, déchets" | TITLE_FR2=="Cokéfaction et raffinage" | TITLE_FR2=="Services marchands" | TITLE_FR2=="Services immobiliers") |>
  select(DATE, OBS_VALUE, TITLE_FR2) |>
  pivot_wider(names_from = TITLE_FR2, values_from = OBS_VALUE, id_cols = DATE) 

scale_conso_men <- function(x, na.rm = FALSE) x/x[analyse_conso_menages[,1] == "2019-10-01"]*100
analyse_conso_menages_base2019 <- mutate_at(analyse_conso_menages, vars(-1),scale_conso_men)
analyse_conso_menages_base2019 <- pivot_longer(analyse_conso_menages_base2019, cols=c(-1),names_to = "Composante", values_to = "value")

analyse_conso_menages_base2019 <-filter(analyse_conso_menages_base2019, Composante=="Produits agro-alimentaires" |
                                          Composante=="Énergie, eau, déchets" | 
                                          Composante=="Cokéfaction et raffinage")
plot_conso_menages_cnt <- ggplot(data=analyse_conso_menages_base2019, aes(x=DATE, y=value, fill = Composante)) +
  geom_line(aes(col = Composante), size =1.1)  +
  labs(title = "Evolution de la consommation énergétique et agro-alimentaire",
       subtitle = "Lecture : Au 1er trimestre, la consommation de produits agro-alimentaire recule en volume de X %. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior",
       y = "Base 100 au 4e trimestre 2019", x = "Année") +
  scale_color_manual(values = c("#00B8E7", "#C77CFF", "#F8766D"))+
  scale_linetype_manual(values=c("solid", "solid","solid","solid","solid","solid"))+
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")
  ) +
  scale_x_date(limits = as.Date(c("2010-10-01", NA))) +ylim(60, 120)
plot_conso_menages_cnt

#######Dépenses de conso des ménages mensuelle 

conso_menages <- get_insee_dataset("CONSO-MENAGES-2014")
conso_alim <- conso_menages |>
  filter(PRODUIT_CONSO_MENAGES == "ALIMENTAIRE-HORS-TABAC") |>
  select(DATE, OBS_VALUE) 

gliss_alim <- filter(analyse_ipc2, Composante == "Alimentation") |>
  select(DATE, gliss_annuel)

conso_alim <- left_join(conso_alim, gliss_alim, by = "DATE") 
conso_alim <- mutate(conso_alim, gliss_annuel = gliss_annuel*100) 
conso_alim <- filter(conso_alim, !is.na(gliss_annuel))


graph_conso_alim <- ggplot(subset(conso_alim, DATE >= as.Date('2000-01-01')), aes(x = DATE)) +
  geom_line(aes(y =gliss_annuel, color = "Consommation alimentaire en volume (échelle de gauche)"), size = 1.1) +
  geom_line(aes(y = OBS_VALUE, color = "Glissement annuel des prix alimentaires (échelle de droite)"), size = 1.1) +
  labs(title = "Consommation des ménages en produits alimentaires (hors tabac)", 
       subtitle = "Lecture : Au 1er trimestre 2023, la consommation des ménages en produits alimentaires (hors tabac) correspond aux volumes du mois de XX 20XX. \nSource : Insee (Comptes trimestriels - Volumes aux prix de l'année précédente chaînés) \nCalculs et graphiques : @statjunior", 
       y = "Milliards d'euros", x = "Année") +
  #scale_y_continuous(limits = c(-2,18), sec.axis = sec_axis(~(.+2)*1.2), name="Pourcentage") +
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")
  ) +ylim(-2,18)
graph_conso_alim


###Graphique conso mensuelle énergie + cokéfaction-raffinage 
conso_energie <- conso_menages |>
  filter(PRODUIT_CONSO_MENAGES == "PRODUITS-PETROLIERS" | PRODUIT_CONSO_MENAGES == "ENERGIE_DEC2") |>
  select(DATE, OBS_VALUE, PRODUIT_CONSO_MENAGES) |>
  pivot_wider(names_from = PRODUIT_CONSO_MENAGES, values_from = OBS_VALUE, id_cols = DATE) |>
  rename(petrole = "PRODUITS-PETROLIERS", full_energie = "ENERGIE_DEC2")

graph_conso_energie <- ggplot(subset(conso_energie, DATE >= as.Date('2000-01-01')), aes(x = DATE)) +
  geom_line(aes(y =petrole, color = "Produits pétroliers"), size = 1.1) +
  geom_line(aes(y = full_energie, color = "Energie, eau, déchets et cokéfaction-raffinage"), size = 1.1) +
  labs(title = "Consommation d'énergie et de produits pétroliers par les ménages", 
       subtitle = "Lecture : Au 1er trimestre 2023, la consommation des ménages en produits pétroliers correspond aux volumes du mois de XX 20XX. \nSource : Insee (Comptes trimestriels - Volumes aux prix de l'année précédente chaînés) \nCalculs et graphiques : @statjunior", 
       y = "Milliards d'euros", x = "Année") +
  #scale_y_continuous(limits = c(-2,18), sec.axis = sec_axis(~(.+2)*1.2), name="Pourcentage") +
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")
  )
#+ylim(-2,18)
graph_conso_energie
################## Comptes des secteurs institutionnels / Comptes d'agents #######

### Dépense de consommation finale des ménages en volume et en valeur
cnt_eq_pib <- get_insee_dataset("CNT-2014-PIB-EQB-RF")

##Total de la dépense de conso finale des ménages et des dépenses finales individualisables des APU 
conso_finale_individualisable <- cnt_eq_pib |>
  filter(OPERATION == "P31" | OPERATION == "P3") |>
  filter(CNA_PRODUIT == "D-CNT") |>
  mutate(sect_value = paste(SECT_INST, VALORISATION, sep = "_")) |>
  spread(key = "sect_value",
         value = "OBS_VALUE") |>
  rename("men_vol" = "S14_L",  "men_val" = "S14_V", "ISBLSM_vol" = "S15_L", 
         "ISBLSM_val" ="S15_V" ,"apu_ind_vol"=  "SO_L", "apu_ind_val" = "SO_V" ) |>
  select(1, 23:28)
conso_finale_individualisable <- mutate_at(conso_finale_individualisable, vars(2:7), ~replace(., is.na(.), 0)) |>
  group_by(DATE) |>
  summarise_at(vars(1:6),              # Specify column
               list(name = sum)) 
names(conso_finale_individualisable) = gsub(pattern = "_name", replacement = "", x = names(conso_finale_individualisable))

conso_finale_individualisable  <- mutate(conso_finale_individualisable, deflateur_conso_finale_men = men_val/men_vol*100) |>
  mutate(deflateur_apu_ind = apu_ind_val/apu_ind_vol*100) |>
  mutate(deflateur_isblsm = ISBLSM_val/ISBLSM_vol*100) |>
  mutate(deflateur_conso_final_ind = (men_val+ISBLSM_val+apu_ind_val)/(men_vol+ISBLSM_vol+apu_ind_vol)*100)

###Agrégation avec le RDB et le RDBA
file_rdb <- "data_ofce/rdb_menages.xlsx"
rdb_cnt <- read_excel(file_rdb, 
                      sheet = "Feuil1")
rdb_cnt$DATE <- str_replace(rdb_cnt$DATE,"T1", "-01-01")
rdb_cnt$DATE <- str_replace(rdb_cnt$DATE,"T2", "-04-01")
rdb_cnt$DATE <- str_replace(rdb_cnt$DATE,"T3", "-07-01")
rdb_cnt$DATE <- str_replace(rdb_cnt$DATE,"T4", "-10-01")
rdb_cnt$DATE <-as.Date(paste(rdb_cnt$DATE,1,sep="-"))
rdb_cnt <- left_join(rdb_cnt, conso_finale_individualisable, by = "DATE")
rdb_cnt$DATE <- str_replace(rdb_cnt$DATE,"-01-01", "-03-01")
rdb_cnt$DATE <- str_replace(rdb_cnt$DATE,"-04-01", "-06-01")
rdb_cnt$DATE <- str_replace(rdb_cnt$DATE,"-07-01", "-09-01")
rdb_cnt$DATE <- str_replace(rdb_cnt$DATE,"-10-01", "-12-01")
rdb_cnt$DATE <-as.Date(paste(rdb_cnt$DATE,1,sep="-"))
rdb_cnt <- mutate(rdb_cnt, pa_rdb_menages = RDB_menages_valeur/deflateur_conso_finale_men*100) 
rdb_cnt2 <- rdb_cnt 
rdb_cnt <-   mutate(rdb_cnt, pa_rdba_menages = RDBA_menages_valeur/deflateur_conso_final_ind*100) |>
  select(DATE, RDB_menages_valeur, RDBA_menages_valeur,  pa_rdb_menages, pa_rdba_menages) |>
  mutate(pa_rdb_menages = pa_rdb_menages/pa_rdb_menages[DATE == "2019-12-01"]*100) |>
  mutate(pa_rdba_menages = pa_rdba_menages/pa_rdba_menages[DATE == "2019-12-01"]*100) |>
  mutate(RDB_menages_valeur = RDB_menages_valeur/RDB_menages_valeur[DATE == "2019-12-01"]*100) |>
  mutate(RDBA_menages_valeur = RDBA_menages_valeur/RDBA_menages_valeur[DATE == "2019-12-01"]*100) |>
  arrange(DATE) 

### En variations trimestrielles 
rdb_var_trim <- rdb_cnt |>
  mutate(var_trim_rdb_menages = pa_rdb_menages/lag(pa_rdb_menages, +1)-1) |>
  mutate(var_trim_rdba_menages = pa_rdba_menages/lag(pa_rdba_menages, +1)-1) |>
  select(DATE, var_trim_rdb_menages, var_trim_rdba_menages) |>
  rename("Pouvoir d'achat du RDB" = "var_trim_rdb_menages", "Pouvoir d'achat du RDBA" = "var_trim_rdba_menages")
rdb_var_trim <- pivot_longer(rdb_var_trim, cols=c(2,3), names_to = "rdb_rdba", values_to = "value")

#########################
###EQUILIBRE DU PIB###
val_exp <- "data_ofce/eq-cnt/exp_val_produit.xls"
cnt_val_exp <- read_excel(val_exp, sheet = "Niveaux", skip = 7) |>
  rename("tot_exp" = "...21") |>
  select(DATE, DE, C2, tot_exp) |>
  filter(!is.na(DE)) |>
  mutate(exp_tot_energie = DE+C2) |>
  mutate(exp_petrole = C2) |>
  select(-DE, -C2)
cnt_val_exp$DATE <-   str_replace(cnt_val_exp$DATE,"T1", "-01-01")
cnt_val_exp$DATE <-   str_replace(cnt_val_exp$DATE,"T2", "-04-01")
cnt_val_exp$DATE <-   str_replace(cnt_val_exp$DATE,"T3", "-07-01")
cnt_val_exp$DATE <-   str_replace(cnt_val_exp$DATE,"T4", "-10-01")
cnt_val_exp$DATE <-as.Date(paste(cnt_val_exp$DATE,1,sep="-"), format = "%Y-%m-%d")

val_imp <- "data_ofce/eq-cnt/import_val_produit.xls"
cnt_val_imp <- read_excel(val_imp, sheet = "Niveaux", skip = 7) |>
  rename("tot_imp" = "...21") |>
  select(DATE, DE, C2, tot_imp) |>
  filter(!is.na(DE)) |>
  mutate(imp_tot_energie = DE+C2) |>
  mutate(imp_petrole = C2) |>
  select(-DE, -C2)
cnt_val_imp$DATE <-   str_replace(cnt_val_imp$DATE,"T1", "-01-01")
cnt_val_imp$DATE <-   str_replace(cnt_val_imp$DATE,"T2", "-04-01")
cnt_val_imp$DATE <-   str_replace(cnt_val_imp$DATE,"T3", "-07-01")
cnt_val_imp$DATE <-   str_replace(cnt_val_imp$DATE,"T4", "-10-01")
cnt_val_imp$DATE <-as.Date(paste(cnt_val_imp$DATE,1,sep="-"), format = "%Y-%m-%d")


cnt_val_exp_imp <- left_join(cnt_val_imp, cnt_val_exp, by = "DATE") |>
  mutate(eq_exp_imp_energie = exp_tot_energie-imp_tot_energie) |>
  mutate(eq_exp_imp_petrole = exp_petrole - imp_petrole) |>
  mutate(tot_exp_imp = tot_exp - tot_imp)

pib_valeur <- get_insee_idbank('010565707') |>
  select(DATE, OBS_VALUE) |>
  rename(pib_trim= OBS_VALUE) |>
  mutate(pib_trim = pib_trim/1000)

cnt_val_exp_imp <- left_join(cnt_val_exp_imp, pib_valeur, by = "DATE") 
cnt_val_exp_imp <- cnt_val_exp_imp |>
  arrange(DATE) |>
  mutate(ratio_energ_pib = eq_exp_imp_energie/pib_trim) |>
  mutate(ratio_petrole_pib = eq_exp_imp_petrole/pib_trim) |>
  mutate(trim_ratio_energ_pib = -(ratio_energ_pib/lag(ratio_energ_pib, 1)-1)) |> 
  arrange(DATE) |>
  mutate(trim_ratio_petrole_pib = ifelse(ratio_petrole_pib>0 & lag(ratio_petrole_pib, 1) >0, ratio_petrole_pib/lag(ratio_petrole_pib, 1)-1, NA)) |> 
  arrange(DATE) |>
  mutate(trim_ratio_petrole_pib = ifelse(ratio_petrole_pib<0 & lag(ratio_petrole_pib, 1) <0, -(ratio_petrole_pib/lag(ratio_petrole_pib, 1)-1), trim_ratio_petrole_pib)) |> 
  arrange(DATE) |>
  mutate(trim_ratio_petrole_pib = ifelse(ratio_petrole_pib>0 & lag(ratio_petrole_pib, 1) <0, -(ratio_petrole_pib/lag(ratio_petrole_pib, 1)-1), trim_ratio_petrole_pib)) |> 
  arrange(DATE) |>
  mutate(trim_ratio_petrole_pib = ifelse(ratio_petrole_pib<0 & lag(ratio_petrole_pib, 1) >0, (ratio_petrole_pib/lag(ratio_petrole_pib, 1)-1), trim_ratio_petrole_pib)) |> 
  mutate(ratio_energ_pib = ratio_energ_pib*100) |>
  mutate(ratio_petrole_pib = ratio_petrole_pib*100) 
mean_ratio_energ_over70<- colMeans(cnt_val_exp_imp[85:290,12])
mean_ratio_petrole_over70<- colMeans(cnt_val_exp_imp[85:290,13])
mean_ratio_energ_over70 <- as.numeric(mean_ratio_energ_over70)
mean_ratio_petrole_over70 <- as.numeric(mean_ratio_petrole_over70)
cnt_val_exp_imp$DATE <-as.Date(paste(cnt_val_exp_imp$DATE,1,sep="-"), format = "%Y-%m-%d")

###Graphique énergie
cnt_val_exp_imp_graph <- mutate(cnt_val_exp_imp, trim_ratio_energ_pib = trim_ratio_energ_pib/0.5)
scale = 0.5
plot_eq_energie <- ggplot(cnt_val_exp_imp_graph, aes(x = DATE)) + 
  geom_line(aes(y=ratio_energ_pib, color = "Déficit en niveau (gauche)"), size = 1.03) + 
  geom_point(aes(y=ratio_energ_pib, color = "Déficit en niveau (gauche)")) + 
  geom_bar(aes(x = DATE, y = trim_ratio_energ_pib, color = "Croissance trimestrielle du déficit (droite)"), stat="identity", fill = "brown") +
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name="Croissance trimestrielle du déficit (droite)")) +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[99]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[121]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[237]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[290]), linetype=4, color = "black") +
  geom_line(aes(y = mean_ratio_energ_over70), linetype = 4, color = "red" ) +
  labs(x = "Année", y = "Points de pourcentage du PIB trimestriel", 
       title = "Déficit commercial du secteur énergétique en part du PIB trimestriel")  +
  scale_x_date(limits = as.Date(c("1970-01-01", NA))) +
  scale_color_manual(values = c("Déficit en niveau (gauche)" = "#4E84C4", "Croissance trimestrielle du déficit (droite)" = "brown")) +
  theme(legend.position="bottom", legend.text = element_text(size = 7))

pdf('graphs/CNT/chocs_energie/def_commercial_energie.pdf')
plot_eq_energie 
dev.off()

#Graphique pétrole
cnt_val_exp_imp_graph_petrole <- mutate(cnt_val_exp_imp, trim_ratio_petrole_pib = trim_ratio_petrole_pib/1) |>
  mutate(trim_ratio_petrole_pib = ifelse(trim_ratio_petrole_pib > 1 | trim_ratio_petrole_pib < -1, NA, trim_ratio_petrole_pib))

plot_eq_petrole <- ggplot(cnt_val_exp_imp_graph_petrole, aes(x = DATE)) + 
  geom_line(aes(y=ratio_petrole_pib, color = "Déficit en niveau (gauche)"), size = 1.03) + 
  geom_point(aes(y=ratio_petrole_pib, color = "Déficit en niveau (gauche)")) + 
  #geom_bar(aes(x = DATE, y = trim_ratio_petrole_pib, color = "Croissance trimestrielle du déficit (droite)"), stat="identity", fill = "brown") +
  #scale_y_continuous(sec.axis = sec_axis(~.*scale2, name="Croissance trimestrielle du déficit (droite)")) +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph_petrole$DATE[99]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph_petrole$DATE[121]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph_petrole$DATE[237]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph_petrole$DATE[290]), linetype=4, color = "black") +
  geom_line(aes(y = mean_ratio_petrole_over70), linetype = 4, color = "red" ) +
  labs(x = "Année", y = "Points de pourcentage du PIB trimestriel", 
       title = "Déficit commercial de la branche cokéfaction-raffinage en part du PIB trimestriel")  +
  scale_x_date(limits = as.Date(c("1970-01-01", NA))) +
  scale_color_manual(values = c("Déficit en niveau (gauche)" = "#4E84C4"
                                  #  , "Croissance trimestrielle du déficit (droite)" = "brown"
  )) +
  theme(legend.position="bottom", legend.text = element_text(size = 7))
plot_eq_petrole

pdf('graphs/CNT/chocs_energie/def_commercial_petrole.pdf')
plot_eq_petrole
dev.off()


###Déflateur des prix de VA 
cnt_va <- filter(cnt, OPERATION =="B1" & CNA_PRODUIT == "D-CNT") |>
  filter(VALORISATION == "L" | VALORISATION == "V") |>
  select(DATE, OBS_VALUE, VALORISATION) |>
  rename(va_tot = OBS_VALUE) |>
  mutate(va_tot = va_tot/1000) |>
  spread(key = VALORISATION, value = va_tot) |>
  mutate(deflateur_prix_va = V/L*100) |>
  select(-L, -V)

###Couts salariaux unitaires 
cout_sal_unit <- "data_ofce/eq-cnt/cout_sal_unit.xls"
cnt_cout_sal_unit <- read_excel(cout_sal_unit, sheet = "Niveaux", skip = 7) |>
  filter(!is.na(prix_prod))
cnt_cout_sal_unit$DATE <-   str_replace(cnt_cout_sal_unit$DATE,"T1", "-01-01")
cnt_cout_sal_unit$DATE <-   str_replace(cnt_cout_sal_unit$DATE,"T2", "-04-01")
cnt_cout_sal_unit$DATE <-   str_replace(cnt_cout_sal_unit$DATE,"T3", "-07-01")
cnt_cout_sal_unit$DATE <-   str_replace(cnt_cout_sal_unit$DATE,"T4", "-10-01")
cnt_cout_sal_unit$DATE <-as.Date(paste(cnt_cout_sal_unit$DATE,1,sep="-"), format = "%Y-%m-%d")

###Taux de marges des entreprises décomposé
tx_marge <- "data_ofce/eq-cnt/tx_marge_snf.xls"
cnt_tx_marge_snf <- read_excel(tx_marge, sheet = "Evolutions")  |>
  filter(!is.na(tx_marge))
cnt_tx_marge_snf$DATE <-   str_replace(cnt_tx_marge_snf$DATE,"T1", "-01-01")
cnt_tx_marge_snf$DATE <-   str_replace(cnt_tx_marge_snf$DATE,"T2", "-04-01")
cnt_tx_marge_snf$DATE <-   str_replace(cnt_tx_marge_snf$DATE,"T3", "-07-01")
cnt_tx_marge_snf$DATE <-   str_replace(cnt_tx_marge_snf$DATE,"T4", "-10-01")
cnt_tx_marge_snf$DATE <-as.Date(paste(cnt_tx_marge_snf$DATE,1,sep="-"), format = "%Y-%m-%d")

###Déflateur conso des ménages
rdb_cnt2 <-rdb_cnt2 |>
  select(DATE, deflateur_conso_finale_men, RDB_menages_valeur)
rdb_cnt2$DATE <-   str_replace(rdb_cnt2$DATE,"T1", "-01-01")
rdb_cnt2$DATE <-   str_replace(rdb_cnt2$DATE,"T2", "-04-01")
rdb_cnt2$DATE <-   str_replace(rdb_cnt2$DATE,"T3", "-07-01")
rdb_cnt2$DATE <-   str_replace(rdb_cnt2$DATE,"T4", "-10-01")
rdb_cnt2$DATE <-as.Date(paste(rdb_cnt2$DATE,1,sep="-"), format = "%Y-%m-%d")
write_xlsx(rdb_cnt2, "data_ofce/deflateur_conso_menages_t2_2002.xlsx")

###Décomposition du RDB nominal (part de revenus/prestations)
decompo_rdb <- "data_ofce/eq-cnt/rdb_men_decomposition.xls"
decompo_rdb_cnt <- read_excel(decompo_rdb, sheet = "Niveaux") 
decompo_rdb_cnt$DATE <-   str_replace(decompo_rdb_cnt$DATE,"T1", "-01-01")
decompo_rdb_cnt$DATE <-   str_replace(decompo_rdb_cnt$DATE,"T2", "-04-01")
decompo_rdb_cnt$DATE <-   str_replace(decompo_rdb_cnt$DATE,"T3", "-07-01")
decompo_rdb_cnt$DATE <-   str_replace(decompo_rdb_cnt$DATE,"T4", "-10-01")
decompo_rdb_cnt$DATE <-as.Date(paste(decompo_rdb_cnt$DATE,1,sep="-"), format = "%Y-%m-%d")

###Déficit public hors et avec charge d'interet 
def_apu <- "data_ofce/eq-cnt/def_apu.xls"
def_apu_cnt <- read_excel(def_apu, sheet = "def_apu") |>
  select(1, 2, 4, 33, 34) |>
  filter(!is.na(deficit))
def_apu_cnt$DATE <-   str_replace(def_apu_cnt$DATE,"T1", "-01-01")
def_apu_cnt$DATE <-   str_replace(def_apu_cnt$DATE,"T2", "-04-01")
def_apu_cnt$DATE <-   str_replace(def_apu_cnt$DATE,"T3", "-07-01")
def_apu_cnt$DATE <-   str_replace(def_apu_cnt$DATE,"T4", "-10-01")
def_apu_cnt$DATE <-as.Date(paste(def_apu_cnt$DATE,1,sep="-"), format = "%Y-%m-%d")
