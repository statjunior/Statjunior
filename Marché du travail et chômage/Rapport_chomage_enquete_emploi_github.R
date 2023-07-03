---
  title: "Chômage et contraintes d'emploi. Résultats de l'Enquête Emploi de l'Insee"
date: "`r format(Sys.time(), '%B %d, %Y')`"
output: 
  github_document:
  toc: true # table of content true
toc_depth: 3  # upto three depths of headings (specified by #, ## and ###)
number_sections: true  ## if you want number sections at each table header
---

  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
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


setwd <- setwd("C:/Users/gasto/Documents/GitHub/Statjunior/projets-r-statjunior/chomage_bit")

```

# Présentation

Ce rapport **Rmarkdwon** synthétise les résultats sur le chômage et les contraintes d'emploi exprimés dans l'Enquête Emploi sur la période 2003 - 2023. Le pas des graphiques est trimestriel. Le dernier point connu est le 1er trimestre 2023. 

Le rapport détaille les différentes catégories de contraintes d'emploi exprimées dans l'emploi. D'une part, les chômeurs au sens du BIT, et d'autre part les personnes dans le halo du chômage ou en situation de sous-emploi contraint. 

Le rapport présente à la fois les résultats en taux et en valeur absolue (nombre de personnes concernées)


```{r}
insee_dataset = get_dataset_list() 

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

```

# Taux de chômage 
## Chômage au sens du BIT et taux de chômage de longue durée 

```{r, fig.dim = c(14, 9)}
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
print(plot_chom_bit_duree)
```

## Taux de chômage BIT par tranche d'âge

```{r, fig.dim = c(14, 9)}
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
print(plot_chom_bit_age)
```


## Taux de chômage élargi (BIT + halo du chômage + sous-emploi involontaire)
### En proportion des participants au marché du travail 
```{r, fig.dim = c(14, 9)}
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
print(plot_chom_elargi)

```



### Halo du chômage par tranche d'age

```{r, fig.dim = c(14, 9)}
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
print(plot_alo_age)
```

# Nombre de personnes au chômage ou ayant des contraintes d'emploi (halo, sous-emploi)

## Nombre de personnes au chomage BIT ou dans le halo 
```{r, fig.dim = c(14, 9)}
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

print(plot_personnes_chomage_halo)
```

## Part des personnes comptées comme chômeurs BIT parmi les chômeurs semi-élargis (BIT + halo du chomage)
```{r, fig.dim = c(14, 9)}
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

print(plot_part_chomage_halo)
```


## Nombre de personnes dans le halo du chômage

```{r, fig.dim = c(14, 9)}
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

print(plot_personnes_halo)
```


## Nombre de personnes en sous-emploi

```{r, fig.dim = c(14, 9)}
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

plot_personnes_ssemploi <-  ggplot(data=nb_ssemploi, aes(x=DATE, 
                                                         y= OBS_VALUE, 
                                                         fill = forcats::fct_relevel(title, c("Sous emploi: ayant involontairement travaillé moins que d'habitude", "Sous emploi: souhaite travailler davantage, disponible MAIS sans recherche", "Sous emploi: souhaite travailler davantage, disponible ET en recherche")))) +
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

print(plot_personnes_ssemploi)
```

## Nombre total de personnes exprimant des contraintes d'emploi : chômeurs BIT, halo du chomage et sous-emploi 

```{r, fig.dim = c(14, 9)}
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

print(plot_full_personnes_besoin_emploi)

```

