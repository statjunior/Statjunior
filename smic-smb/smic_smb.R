###Evolution SMIC et SMB catégoriels de la DARES
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


setwd <- setwd("C:/Users/gasto/Documents/GitHub/Statjunior/projets-r-statjunior/smic-smb/")

###Functions 
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1:-2,]
}

empty <- function(df) {
  empty_columns <- colSums(is.na(df) | df== "") == nrow(df)
  df <- df[,!empty_columns]
}

scale_2019t4 <- function(x, na.rm = FALSE) x/x[base_salaires[,1] == "2019-12-01"]*100
scale_2005t3 <- function(x, na.rm = FALSE) x/x[historique_sal_smb_smic[,1] == "2005-09-01"]*100
scale_2017t2 <- function(x, na.rm = FALSE) x/x[base_salaires[,1] == "2017-06-01"]*100
scale_2017t2_histo <- function(x, na.rm = FALSE) x/x[histo_smb[,1] == "2017-06-01"]*100
scale_2017t2_smic <- function(x, na.rm = FALSE) x/x[smic_analyse[,1] == "2017-06-01"]*100
scale_2017t2_ipc<- function(x, na.rm = FALSE) x/x[ipc_analyse[,1] == "2017-06-01"]*100
scale_2015_histo_ipc <- function(x, na.rm = FALSE) x/((x[histo_ipc[,1] == "2015-03-01"]+x[histo_ipc[,1] == "2015-06-01"]+x[histo_ipc[,1] == "2015-09-01"]+x[histo_ipc[,1] == "2015-12-01"])/4)*100
scale_salaire_reel <- function(x, na.rm = FALSE) x/x[base_salaires["ipc",]]*100

###Fichiers à télécharger
###SMB Dares : à télécharger tous les trimestres...
smb <- read_excel("Dares_serie_salaire_de_base_t4_2022.xlsx", 
                                                  sheet = "Sal. mens. ensemble", skip = 5)

smbo <- read_excel("Dares_serie_salaire_de_base_t4_2022.xlsx", sheet ="Sal. mens. ouv.", 
                   skip = 5)
smbe <- read_excel("Dares_serie_salaire_de_base_t4_2022.xlsx", sheet ="Sal. mens. emp.", 
                   skip = 5)
smbpi <- read_excel("Dares_serie_salaire_de_base_t4_2022.xlsx", sheet ="Sal. mens. PI", 
                    skip = 5)
smbc <- read_excel("Dares_serie_salaire_de_base_t4_2022.xlsx", sheet ="Sal. mens. cadres", 
                   skip = 5)


indice_fp <- read_excel("indice_fp.xlsx")|>
  rename(OBS_VALUE = cum_fp_1970)

histo_smb <- read_excel("historique_smb_detail.xlsx")
histo_ipc <- read_excel("ipc_histo.xlsx") |>
  rename(OBS_VALUE = cum_inf_1990)

###Smic et IPC insee
smic <- get_insee_idbank('000879878') 
ipc <- get_insee_idbank('001769682')

###SMBS
smbs <- as.data.frame(t(smb))
smbs <- header.true(smbs)
smbs <- empty(smbs) 

smbs <- smbs |>
  rename(year = 1, smbs = ENS) |>
  mutate(year=substr(year, 1, 4)) |>
  select(year, smbs) |>
  mutate(mois = row_number()) |>
  filter(year !="3 mois" | year !="6 mois"| year !="12 mois") |>
  mutate(mois = mois %%4) |>
  mutate(mois = case_when(
    mois == 0 ~ "03-01",
    mois == 1 ~ "06-01",
    mois == 2 ~ "09-01",
    mois == 3 ~ "12-01",
  )) |>
  mutate(DATE = paste0(year, "-",mois)) |>
  select(-year, -mois) |>
  mutate(DATE = as.Date(DATE)) |>
  relocate(DATE, .before = NULL) |>
  filter(!is.na(DATE))

###SMB ouvrier
smbo <- as.data.frame(t(smbo))
smbo <- header.true(smbo)
smbo <- empty(smbo) 

smbo <- smbo |>
  rename(year = 1, smbo = ENS) |>
  select(year, smbo) |>
  mutate(year=substr(year, 1, 4)) |>
  mutate(mois = row_number()) |>
  filter(year !="3 mois" | year !="6 mois"| year !="12 mois") |>
  mutate(mois = mois %%4) |>
  mutate(mois = case_when(
    mois == 0 ~ "03-01",
    mois == 1 ~ "06-01",
    mois == 2 ~ "09-01",
    mois == 3 ~ "12-01",
  )) |>
  mutate(DATE = paste0(year, "-",mois)) |>
  select(-year, -mois) |>
  mutate(DATE = as.Date(DATE)) |>
  relocate(DATE, .before = NULL) |>
  filter(!is.na(DATE))

###SMB cadre
smbc <- as.data.frame(t(smbc))
smbc <- header.true(smbc)
smbc <- empty(smbc) 

smbc <- smbc |>
  rename(year = 1, smbc = ENS) |>
  select(year, smbc) |>
  mutate(year=substr(year, 1, 4)) |>
  mutate(mois = row_number()) |>
  filter(year !="3 mois" | year !="6 mois"| year !="12 mois") |>
  mutate(mois = mois %%4) |>
  mutate(mois = case_when(
    mois == 0 ~ "03-01",
    mois == 1 ~ "06-01",
    mois == 2 ~ "09-01",
    mois == 3 ~ "12-01",
  )) |>
  mutate(DATE = paste0(year, "-",mois)) |>
  select(-year, -mois) |>
  mutate(DATE = as.Date(DATE)) |>
  relocate(DATE, .before = NULL) |>
  filter(!is.na(DATE))

###SMB PI 
smbpi <- as.data.frame(t(smbpi))
smbpi <- header.true(smbpi)
smbpi <- empty(smbpi) 

smbpi <- smbpi |>
  rename(year = 1, smbpi = ENS) |>
  select(year, smbpi) |>
  mutate(year=substr(year, 1, 4)) |>
  mutate(mois = row_number()) |>
  filter(year !="3 mois" | year !="6 mois"| year !="12 mois") |>
  mutate(mois = mois %%4) |>
  mutate(mois = case_when(
    mois == 0 ~ "03-01",
    mois == 1 ~ "06-01",
    mois == 2 ~ "09-01",
    mois == 3 ~ "12-01",
  )) |>
  mutate(DATE = paste0(year, "-",mois)) |>
  select(-year, -mois) |>
  mutate(DATE = as.Date(DATE)) |>
  relocate(DATE, .before = NULL) |>
  filter(!is.na(DATE))

###SMB Employés
smbe <- as.data.frame(t(smbe))
smbe <- header.true(smbe)
smbe <- empty(smbe) 

smbe <- smbe |>
  rename(year = 1, smbe = ET) |>
  select(year, smbe) |>
  mutate(year=substr(year, 1, 4)) |>
  mutate(smbe = ifelse(year=="2023", 114,smbe)) |>
  mutate(mois = row_number()) |>
  filter(year !="3 mois" | year !="6 mois"| year !="12 mois") |>
  mutate(mois = mois %%4) |>
  mutate(mois = case_when(
    mois == 0 ~ "03-01",
    mois == 1 ~ "06-01",
    mois == 2 ~ "09-01",
    mois == 3 ~ "12-01",
  )) |>
  mutate(DATE = paste0(year, "-",mois)) |>
  select(-year, -mois) |>
  mutate(DATE = as.Date(DATE)) |>
  relocate(DATE, .before = NULL) |>
  filter(!is.na(DATE))

###SMB secteur
smb_secteur <- as.data.frame(t(smb))
smb_secteur<- header.true(smb_secteur)
smb_secteur <- empty(smb_secteur) 

smb_secteur <- smb_secteur |>
  rename(year = 1, smb_industrie = ET, smb_construction=EU, smb_tertiaire=EV) |>
  mutate(year=substr(year, 1, 4)) |>
  select(year, smb_industrie, smb_construction, smb_tertiaire) |>
  mutate(mois = row_number()) |>
  filter(year !="3 mois" | year !="6 mois"| year !="12 mois") |>
  mutate(mois = mois %%4) |>
  mutate(mois = case_when(
    mois == 0 ~ "03-01",
    mois == 1 ~ "06-01",
    mois == 2 ~ "09-01",
    mois == 3 ~ "12-01",
  )) |>
  mutate(DATE = paste0(year, "-",mois)) |>
  select(-year, -mois) |>
  mutate(DATE = as.Date(DATE)) |>
  relocate(DATE, .before = NULL) |>
  filter(!is.na(DATE))

###SMIC
smic_analyse <- select(smic, DATE, OBS_VALUE) |>
  mutate(trim = ifelse(substr(DATE, 6, 7) == "03" | substr(DATE, 6, 7) == "06" 
                      | substr(DATE, 6, 7) == "09" | substr(DATE, 6, 7) == "12",1,0  )) |>
  arrange(DATE) |>
  mutate(smic_trim = (OBS_VALUE+lag(OBS_VALUE,1)+lag(OBS_VALUE, 2))/3) |>
  filter(trim == 1) |>
  select(-trim, -OBS_VALUE) |>
  rename(smic = smic_trim)

###IPC
ipc_analyse <- select(ipc, DATE, OBS_VALUE) |>
  mutate(trim = ifelse(substr(DATE, 6, 7) == "03" | substr(DATE, 6, 7) == "06" 
                       | substr(DATE, 6, 7) == "09" | substr(DATE, 6, 7) == "12",1,0  )) |>
  arrange(DATE) |>
  mutate(ipc_trim = (OBS_VALUE+lag(OBS_VALUE,1)+lag(OBS_VALUE, 2))/3) |>
  filter(trim == 1) |>
  select(-trim, -OBS_VALUE) |>
  rename(ipc = ipc_trim)

###Point d'indice
indice_fp <- select(indice_fp, DATE, OBS_VALUE) |>
  mutate(trim = ifelse(substr(DATE, 6, 7) == "03" | substr(DATE, 6, 7) == "06" 
                       | substr(DATE, 6, 7) == "09" | substr(DATE, 6, 7) == "12",1,0  )) |>
  arrange(DATE) |>
  mutate(indice_fp_trim = (OBS_VALUE+lag(OBS_VALUE,1)+lag(OBS_VALUE, 2))/3) |>
  filter(trim == 1) |>
  select(-trim, -OBS_VALUE) |>
  rename(indice_fp = indice_fp_trim)


###Histo IPC
histo_ipc <- select(histo_ipc, DATE, OBS_VALUE) |>
  mutate(trim = ifelse(substr(DATE, 6, 7) == "03" | substr(DATE, 6, 7) == "06" 
                       | substr(DATE, 6, 7) == "09" | substr(DATE, 6, 7) == "12",1,0  )) |>
  arrange(DATE) |>
  mutate(histo_ipc_trim = (OBS_VALUE+lag(OBS_VALUE,1)+lag(OBS_VALUE, 2))/3) |>
  filter(trim == 1) |>
  select(-trim, -OBS_VALUE) |>
  rename(histo_ipc = histo_ipc_trim) |>
  mutate(DATE = as.Date(DATE))

histo_ipc <- mutate_at(histo_ipc, vars(-1), scale_2015_histo_ipc)
histo_ipc <- filter(histo_ipc, DATE <as.Date("1990-03-01"))
histo_ipc <- rename(histo_ipc, ipc=histo_ipc)
histo_ipc <- rbind(histo_ipc,ipc_analyse)

###SMB historique 
histo_smb <- histo_smb |>
  mutate(DATE = as.Date(DATE))
histo_smb <- mutate_at(histo_smb, vars(2:9), scale_2017t2_histo)

histo_smb <- mutate_at(histo_smb, vars(2:9), scale_2017t2_histo)

###Merge 
base_salaires <- left_join(smbs, smbc, by = "DATE")
base_salaires <- left_join(base_salaires, smbpi, by = "DATE")
base_salaires <- left_join(base_salaires, smbe, by = "DATE")
base_salaires <- left_join(base_salaires, smbo, by = "DATE")
base_salaires <- left_join(base_salaires, smb_secteur, by = "DATE")
base_salaires <- left_join(base_salaires, smic_analyse, by = "DATE")
base_salaires <- left_join(base_salaires, ipc_analyse, by = "DATE")
base_salaires <- mutate_if(base_salaires , is.character, as.numeric)

base_salaires_nom<- mutate_at(base_salaires, vars(2:11), scale_2017t2)
base_sal_reel <- mutate_at(base_salaires_nom, vars(2:10),  ~./ipc*100-100)

###Merge histo 
base_salaire_histo <- select(base_salaires_nom, DATE,smbs, smbo, smbe, smbpi, smbc, 
                             smb_construction, smb_tertiaire, smb_industrie)
salaire_histo <- rbind(histo_smb, base_salaire_histo) |>
  distinct(DATE, .keep_all = TRUE)

ipc_analyse <-  mutate_at(ipc_analyse , vars(-1), scale_2017t2_ipc)
smic_analyse <- mutate_at(smic_analyse, vars(-1), scale_2017t2_smic)

historique_sal_smb_smic <- left_join(smic_analyse, salaire_histo, by = "DATE")
historique_sal_smb_smic <- left_join(historique_sal_smb_smic , ipc_analyse, by = "DATE")
historique_sal_smb_smic <- mutate_at(historique_sal_smb_smic, vars(-1), scale_2005t3)
historique_sal_reel <- mutate_at(historique_sal_smb_smic , vars(2:10),  ~./ipc*100-100)


###graphique 
plot_smic_smb_reel <-ggplot(data = base_sal_reel,
                            aes(x = DATE)) +
  geom_line(aes(y = smic, color = "SMIC"), size = 1.3) + 
  geom_line(aes(y = smbs, color = "SMB"), size = 1.3) + 
  geom_line(aes(y = smbo, color = "SMB Ouvriers"), size = 1) + 
  geom_line(aes(y = smbe, color = "SMB Employés"), size = 1) + 
  geom_line(aes(y = smbpi, color = "SMB Professions intermédiaires"), size = 1) + 
  geom_line(aes(y = smbc, color = "SMB Cadres"), size = 1) +
  labs(title = "Evolution du pouvoir d'achat du SMIC et du Salaire mensuel de base depuis la crise du Covid", 
       subtitle = "Lecture : Au 1er trimestre 2023, le pouvoir d'achat du Salaire mensuel de base recule de 1,4 % par rapport au 2e trimestre 2017. \nSource : Dares (Salaires de base), Insee (Smic mensuel brut, Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Variation par rapport au 2e trimestre 2017 (%)", x = "Date") +
  scale_x_date(limits = as.Date(c("2017-06-01", NA))) + 
  scale_color_manual(values = c("SMB"="red", "SMB Cadres" = "orange", "SMIC" = "#C77CFF", "SMB Employés" ="#00BA38", "SMB Ouvriers" = "#00B8E7", "SMB Professions intermédiaires" = "#D39200"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14,face = "italic")) + 
  ylim(-5,5)
plot_smic_smb_reel

plot_smic_smb_secteur_reel <-ggplot(data = base_sal_reel,
                            aes(x = DATE)) +
  geom_line(aes(y = smb_industrie, color = "SMB Industrie"), size = 1.3) + 
  geom_line(aes(y = smb_construction, color = "SMB Construction"), size = 1.3) + 
  geom_line(aes(y = smb_tertiaire, color = "SMB Tertiaire"), size = 1) + 
  labs(title = "Evolution du pouvoir d'achat du salaire mensuel de base par secteur d'activité", 
       subtitle = "Lecture : Au 1er trimestre 2023, le pouvoir d'achat du Salaire mensuel de base tertiaire recule de 1,4 % par rapport au 2e trimestre 2017. \nSource : Dares (Salaires de base), Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Variation par rapport au 2e trimestre 2017 (%)", x = "Date") +
  scale_x_date(limits = as.Date(c("2017-06-01", NA))) + 
  scale_color_manual(values = c("SMB Industrie" = "brown", "SMB Construction" = "#C77CFF", "SMB Tertiaire" ="#00BA38"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) + 
  ylim(-5,5)
plot_smic_smb_secteur_reel

histo_smb_reel <-ggplot(data = historique_sal_reel,
                                 aes(x = DATE)) +
  geom_line(aes(y = smic, color = "SMIC"), size = 1.3) + 
  geom_line(aes(y = smbs, color = "SMB"), size = 1.3) + 
  geom_line(aes(y = smbo, color = "SMB Ouvriers"), size = 1) + 
  geom_line(aes(y = smbe, color = "SMB Employés"), size = 1) + 
  geom_line(aes(y = smbpi, color = "SMB Professions intermédiaires"), size = 1) + 
  geom_line(aes(y = smbc, color = "SMB Cadres"), size = 1) +
  labs(title = "Evolution du pouvoir d'achat du SMIC et du Salaire mensuel de base depuis la crise du Covid", 
       subtitle = "Lecture : Au 1er trimestre 2023, le pouvoir d'achat du Salaire mensuel de base recule de X % par rapport au 2e trimestre 2017. \nSource : Dares (Salaires de base), Insee (Smic mensuel brut, Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Variation par rapport au 2e trimestre 2017 (%)", x = "Date") +
  scale_x_date(limits = as.Date(c("2005-09-01", NA))) + 
  scale_color_manual(values = c("SMB"="red", "SMB Cadres" = "orange", "SMIC" = "#C77CFF", "SMB Employés" ="#00BA38", "SMB Ouvriers" = "#00B8E7", "SMB Professions intermédiaires" = "#D39200"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) + 
  ylim(-2,15)
histo_smb_reel

plot_smic_smb_nom <-ggplot(data = base_salaires_nom,
                            aes(x = DATE)) +
  geom_line(aes(y = smic, color = "SMIC"), size = 1.5) + 
  geom_line(aes(y = smbs, color = "SMB"), size = 1.5) + 
  # geom_line(aes(y = smbo, color = "SMB Ouvriers"), size = 1.1) + 
  # geom_line(aes(y = smbe, color = "SMB Employés"), size = 1.1) + 
  # geom_line(aes(y = smbpi, color = "SMB Professions intermédiaires"), size = 1.1) + 
  # geom_line(aes(y = smbc, color = "SMB Cadres"), size = 1.1) +
  geom_line(aes(y = ipc, color = "Indice des prix à la consommation"), size = 1.5) +
  labs(title = "Evolution du salaire mensuel de base nominal, du Smic et de l'inflation depuis juin 2017", 
       subtitle = "Lecture : Au 1er trimestre 2023, le niveau des prix est supérieur de 15 % à celui de juin 2017, contre 13 % pour le salaire mensuel de base. \nSource : Insee (Indice des prix à la consommation) \nCalculs et graphiques : @statjunior", 
       y = "Base 100 au 2e trimestre 2017", x = "Date") +
  scale_x_date(limits = as.Date(c("2017-06-01", NA))) + 
  scale_color_manual(values = c("Indice des prix à la consommation"="red", "SMB"="blue", "SMIC" = "purple"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=14),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic")) + 
  ylim(99,120)
plot_smic_smb_nom



####SMB et point d'indice
smb_fp <- left_join(histo_ipc, indice_fp, by = "DATE")
smb_fp <- left_join(smb_fp, salaire_histo, by = "DATE") |>
  mutate(DATE = as.Date(DATE)) |>
  filter(DATE >=as.Date('1985-03-01'))

scale_fp_85t1 <- function(x, na.rm = FALSE) x/x[smb_fp[,1] == "1985-03-01"]*100
smb_fp_nom<- mutate_at(smb_fp, vars(-1), scale_fp_85t1)
smb_fp_reel <- mutate_at(smb_fp_nom, vars(2:7),  ~./histo_ipc*100-100)


fp_spb_reel <-ggplot(data = smb_fp_reel,
                        aes(x = DATE)) +
  #geom_line(aes(y = smbs, color = "SMB réel"), size = 1.3) + 
  geom_line(aes(y = indice_fp, color = "Point d'indice en valeur réelle"), size = 1.3) + 
  labs(title = "Pouvoir d'achat du point d'indice de la fonction publique depuis 1985", 
       subtitle = "Lecture : Au 4e trimestre 2022, le pouvoir d'achat du point d'indice de la fonction publique baisse de 25 % par rapport à 1985. \nSource : Institut des Politiques Publiques (Point d'indice), Insee (IPC) \nCalculs et graphiques : @statjunior", 
       y = "Variation par rapport au 1e trimestre 1985 (%)", x = "Date") +
  scale_x_date(limits = as.Date(c("1985-03-01", NA))) + 
  scale_color_manual(values = c("Point d'indice en valeur réelle"="orange"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) + 
  ylim(-30,0)
fp_spb_reel

fp_smb_nom <-ggplot(data = smb_fp_nom,
                     aes(x = DATE)) +
  geom_line(aes(y = ipc, color = "IPC"), size = 1.3) + 
  geom_line(aes(y = indice_fp, color = "Point d'indice"), size = 1.3) + 
  labs(title = "Point d'indice en valeur nominale et indice des prix depuis 1985", 
       subtitle = "Lecture : Au 1er trimestre 2023, le point d'indice a été multiplié par 1,5 et les prix à la consommation par 2,04 par rapport au 1er trimestre 1985. \nSource : Institut des Politiques Publiques (Point d'indice), Insee (IPC) \nCalculs et graphiques : @statjunior", 
       y = "Variation par rapport au 1e trimestre 1985 (%)", x = "Date") +
  scale_x_date(limits = as.Date(c("1985-03-01", NA))) + 
  scale_color_manual(values = c("Point d'indice"="orange", "IPC" = "#00B8E7"))+
  theme_grey()+
  scale_x_date(limits = as.Date(c("1985-03-01", NA))) + 
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) + 
  ylim(100, 220)
fp_smb_nom




setwd <- setwd("C:/Users/gasto/Documents/GitHub/Statjunior/projets-r-statjunior/cnt/")
smpt <- read_excel("data/smpt.xlsx")
setwd <- setwd("C:/Users/gasto/Documents/GitHub/Statjunior/projets-r-statjunior/smic-smb/")

smbs <- mutate(smbs, DATE = case_when(substr(DATE, 6, 7) == "03"~paste0(substr(DATE, 1, 5),"01",substr(DATE, 8, 10)), 
                               substr(DATE, 6, 7) == "06"~paste0(substr(DATE, 1, 5),"04",substr(DATE, 8, 10)),
                               substr(DATE, 6, 7) == "09"~paste0(substr(DATE, 1, 5),"07",substr(DATE, 8, 10)),
                               substr(DATE, 6, 7) == "12"~paste0(substr(DATE, 1, 5),"10",substr(DATE, 8, 10))))
smbs <- mutate(smbs, DATE = as.Date(DATE))
smpt <- left_join(smpt,smbs, by = "DATE") |>
  filter(DATE >=as.Date('2017-04-01')) 

smpt <- mutate(smpt, DATE = as.Date(DATE)) |>
  mutate(smbs = as.numeric(smbs)) |>
  mutate(smpt_reel = smpt_cnt_marng/deflateur_conso_men*100) |> 
  mutate(smb_reel = smbs/deflateur_conso_men*100) 


plot_smpt_smb <-ggplot(data = smpt,
                                    aes(x = DATE)) +
  geom_line(aes(y = smpt_reel, color = "SMPT réel"), size = 1.3) + 
  geom_line(aes(y = smb_reel, color = "SMB réel"), size = 1.3) + 
  #geom_line(aes(y = smb_tertiaire, color = "SMB Tertiaire"), size = 1) + 
  labs(title = "Evolution du pouvoir d'achat du SMB et du Salaire Moyen Par Tête", 
       subtitle = "Lecture : Au 4e trimestre 2022, le Salaire Moyen Par Tête (SMPT) réel recule de 5 % par rapport au 2e trimestre 2017. \nSource : Dares (Salaires de base), Insee (Comptes nationaux : SMPT et déflateur de la consommation des ménages) \nCalculs et graphiques : @statjunior", 
       y = "Base 100 au 2e trimestre 2017 (%)", x = "Date") +
  scale_x_date(limits = as.Date(c("2017-04-01", NA))) + 
  scale_color_manual(values = c("SMPT réel" = "red", "SMB réel" = "blue"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) + 
  ylim(80,105)
plot_smpt_smb
