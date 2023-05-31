########A exécuter après cnt_cb

#Définition du chemin d'accès
setwd <- setwd("C:/Users/gasto/Documents/GitHub/Statjunior/projets-r-statjunior/cnt/")
getwd()

####Date de la base 100 pour les graphiques en niveau 
scale_2019t4 <- function(x, na.rm = FALSE) x/x[rdb_menages[,1] == "2019-10-01"]*100
scale_2018t4 <- function(x, na.rm = FALSE) x/x[cnt_cb_complet[,1] == "2018-10-01"]*100
scale_2000t1 <- function(x, na.rm = FALSE) x/x[rdb_menages[,1] == "2000-01-01"]*100

#Bien télécharger la décompo du taux de marge des SNF 
decompo_tm_snf <- read_excel(file_decompo_tx_marge_snf, 
                             sheet = "Evolutions",skip = 5) |>
  rename("DATE" = "...1", "tx_marge" = "...2", "var_tx_marge" = "...3", "contrib_productivite" = 4, 
         "contrib_sal_reel" = 5, "contrib_cot_soc" = 6, "contrib_terme_echange" = 7, "contrib_autres_facteurs" = 8) |>
  filter(!is.na(tx_marge)) 
decompo_tm_snf$DATE <- str_replace(decompo_tm_snf$DATE,"T1", "-01-01")
decompo_tm_snf$DATE <- str_replace(decompo_tm_snf$DATE,"T2", "-04-01")
decompo_tm_snf$DATE <- str_replace(decompo_tm_snf$DATE,"T3", "-07-01")
decompo_tm_snf$DATE <- str_replace(decompo_tm_snf$DATE,"T4", "-10-01")
decompo_tm_snf$DATE <- as.Date(decompo_tm_snf$DATE)

####Bien télécharger Pouvoir d'achat et ratio du compte des ménages: 
#https://www.insee.fr/fr/statistiques/6960365?sommaire=6795771 
file_rdb_uc <- "t_pouvachat_val.xls"
rdb_uc <- read_excel(file_rdb_uc,  sheet = "Evolutions", skip = 7) |>
   select("...1", "B6/UC", "(B6/P3prix)/UC") |>
   rename(DATE = "...1", rd_uc = "B6/UC", pa_rdb_uc = "(B6/P3prix)/UC")
rdb_uc$DATE <-   str_replace(rdb_uc$DATE,"T1", "-01-01")
rdb_uc$DATE <-   str_replace(rdb_uc$DATE,"T2", "-04-01")
rdb_uc$DATE <-   str_replace(rdb_uc$DATE,"T3", "-07-01")
rdb_uc$DATE <-   str_replace(rdb_uc$DATE,"T4", "-10-01")
rdb_uc$DATE <- as.Date(rdb_uc$DATE)
rdb_uc <- filter(rdb_uc, DATE >= as.Date("1978-01-01")) |>
   mutate(cum_rdb_uc = ifelse(DATE == as.Date('1978-01-01'), 100, NA)) |>
   mutate(cum_pa_uc = ifelse(DATE == as.Date('1978-01-01'), 100, NA)) |>
   arrange(DATE)

i <-2
while(i <=nrow(rdb_uc)){
   rdb_uc[i, "cum_rdb_uc"] <- rdb_uc[i-1, "cum_rdb_uc"]*(1+(rdb_uc[i, "rd_uc"]/100))
   i <- i+1
 }
i <-2
while(i <=nrow(rdb_uc)){
   rdb_uc[i, "cum_pa_uc"] <- rdb_uc[i-1, "cum_pa_uc"]*(1+(rdb_uc[i, "pa_rdb_uc"]/100))
   i <- i+1
}
rdb_uc <- select(rdb_uc, -rd_uc, -pa_rdb_uc)

####A faire : 
#capacité besoin de financement vis a vis du RDM en % PIB
#taux de marge décomposé par secteur depuis 2018 
#evolution du PIB, PA et PA par UC sur longue période et en base 100 en 2019
#taux d'épargne 

list_idbank_csi = 
  get_idbank_list("CNT-2014-CSI") 

csi = list_idbank_csi %>% pull(idbank)

csi_2014 = 
  get_insee_idbank(csi) %>% 
  split_title() 

liste_t1 <- distinct(csi_2014, TITLE_FR1)


#########################Pouvoir d'achat des ménages de LT par rapport au PIB 
rdb_menages <- filter(csi_2014, TITLE_FR1 == "Revenu disponible brut des ménages (y compris les entreprises individuelles)") |>
  select(DATE, OBS_VALUE) |>
  rename(rdbrut_menages = OBS_VALUE)
pib_deflateur_conso_menages <- select(pib_eer, DATE, pib_tot_vol, deflateur_conso_men, pib_tot_val) 
rdb_menages <- left_join(rdb_menages, pib_deflateur_conso_menages, by = "DATE")
rdb_menages <- left_join(rdb_menages, rdb_uc, by = "DATE")

rdb_menages_2000 <- mutate_at(rdb_menages, vars(2:4), scale_2000t1)
rdb_menages_2000 <- mutate_at(rdb_menages, vars(-1), scale_2000t1)
rdb_menages_2019 <- mutate_at(rdb_menages, vars(-1), scale_2019t4)

rdb_menages_2000 <- mutate(rdb_menages_2000,pa_menages = rdbrut_menages/deflateur_conso_men*100)
rdb_menages_2019 <- mutate(rdb_menages_2019,pa_menages = rdbrut_menages/deflateur_conso_men*100)


plot_rdb_deflateur2000 <- ggplot(data = rdb_menages_2000,
                                   aes(x = DATE)) +
  geom_line(aes(y = rdbrut_menages, color = "Revenu Disponible Brut"), size = 1.3) + 
  geom_line(aes(y = cum_rdb_uc, color = "RDB par Unité de Consommation"), size = 1.1, linetype = "dashed") + 
  geom_line(aes(y = deflateur_conso_men, color = "Prix à la consommation des ménages (en compatabilité nationale)"), size = 1.1) + 
  labs(title = "Évolution depuis 2000 du Revenu disponible brut et des prix à la consommation des ménages", 
       subtitle = "Lecture : Au 1er trimestre 2023, le pouvoir d'achat du revenu disponible brut des ménages recule de X points par rapport à la fin 2019. \nSource : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "Base 100 au 1er trimestre 2000", x = "Année") +
  scale_x_date(limits = as.Date(c("2000-01-01", NA))) + 
  scale_color_manual(values = c("Revenu Disponible Brut"="#00B8E7","RDB par Unité de Consommation"="purple", "Prix à la consommation des ménages (en compatabilité nationale)" = "red"))+
  theme_gray()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) + 
  ylim(98,200)


plot_rdb_menages2000 <-ggplot(data = rdb_menages_2000,
                             aes(x = DATE)) +
  geom_line(aes(y = pa_menages, color = "Pouvoir d'achat du Revenu Disponible Brut"), size = 1.3) + 
  geom_line(aes(y = cum_pa_uc, color = "Pouvoir d'achat par Unité de Consommation"), size = 1.1, linetype = "dashed") + 
  geom_line(aes(y = pib_tot_vol, color = "PIB en volume"), size = 1.1) + 
  labs(title = " Évolution depuis 2000 du pouvoir d’achat du revenu disponible brut (RDB) des ménages et du PIB", 
       subtitle = "Lecture : Au 1er trimestre 2023, le pouvoir d'achat du revenu disponible brut des ménages recule de X points par rapport à la fin 2019. \nSource : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "Base 100 au 1er trimestre 2000", x = "Année") +
  scale_x_date(limits = as.Date(c("2000-01-01", NA))) + 
  scale_color_manual(values = c("Pouvoir d'achat du Revenu Disponible Brut"="#00B8E7","Pouvoir d'achat par Unité de Consommation" = "purple", "PIB en volume" = "orange"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) + 
  ylim(98,150)


plot_pa_men2000 <- plot_grid(plot_rdb_deflateur2000, plot_rdb_menages2000, ncol = 1, nrow = 2)
plot_pa_men2000

plot_rdb_deflateur2019 <- ggplot(data = rdb_menages_2019,
                                 aes(x = DATE)) +
  geom_line(aes(y = rdbrut_menages, color = "Revenu Disponible Brut"), size = 1.3) + 
  geom_line(aes(y = cum_rdb_uc, color = "RDB par Unité de Consommation"), size = 1.1, linetype = "dashed") + 
  geom_line(aes(y = deflateur_conso_men, color = "Prix à la consommation des ménages (en compatabilité nationale)"), size = 1.1) + 
  labs(title = " Évolution depuis 2019 du Revenu disponible brut et des prix à la consommation des ménages", 
       subtitle = "Lecture : Au 1er trimestre 2023, le pouvoir d'achat du revenu disponible brut des ménages recule de X points par rapport à la fin 2019. \nSource : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "Base 100 au 1er trimestre 2000", x = "Année") +
  scale_x_date(limits = as.Date(c("2019-10-01", NA))) + 
  scale_color_manual(values = c("Revenu Disponible Brut"="#00B8E7","RDB par Unité de Consommation"="purple", "Prix à la consommation des ménages (en compatabilité nationale)" = "red"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) + 
  ylim(95,115)


plot_rdb_menages2019 <-ggplot(data = rdb_menages_2019,
                              aes(x = DATE)) +
  geom_line(aes(y = pa_menages, color = "Pouvoir d'achat du Revenu Disponible Brut"), size = 1.3) + 
  geom_line(aes(y = cum_pa_uc, color = "Pouvoir d'achat par Unité de Consommation"), size = 1.1, linetype = "dashed") + 
  geom_line(aes(y = pib_tot_vol, color = "PIB en volume"), size = 1.1) + 
  labs(title = " Évolution depuis 2019 du pouvoir d’achat du revenu disponible brut (RDB) des ménages et du PIB", 
       subtitle = "Lecture : Au 1er trimestre 2023, le pouvoir d'achat du revenu disponible brut des ménages recule de X points par rapport à la fin 2019. \nSource : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "Base 100 au 1er trimestre 2000", x = "Année") +
  scale_x_date(limits = as.Date(c("2019-10-01", NA))) + 
  scale_color_manual(values = c("Pouvoir d'achat du Revenu Disponible Brut"="#00B8E7","Pouvoir d'achat par Unité de Consommation" = "purple", "PIB en volume" = "orange"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) + 
  ylim(80,110)

plot_pamen2019 <- plot_grid(plot_rdb_deflateur2019, plot_rdb_menages2019, ncol = 1, nrow = 2)
plot_pamen2019 

####Contribution au pouvoir d'achat 
decompo_rdb_menages <- filter(csi_2014, TITLE_FR1 == "Excédent brut d'exploitation des ménages hors EI (y compris revenus mixtes)" |
                                TITLE_FR1 == "Excédent brut d'exploitation des entrepreneurs individuels (y compris revenus mixtes)" |
                                TITLE_FR1 =="Masse salariale reçue par les ménages (y compris les entreprises individuelles)" |
                              TITLE_FR1 =="Cotisations sociales employeurs reçues par les ménages (y compris les entreprises individuelles)" | 
                                TITLE_FR1 =="Cotisations imputées reçues par les ménages (y compris les entreprises individuelles)" |
                                TITLE_FR1 == "Intérêts reçus par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 == "Dividendes reçus par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 == "Revenus de la propriété attribués aux assurés reçus par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 == "Loyers des terrains et gisements reçus par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 == "Intérêts versés par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 == "Loyers des terrains et gisements versés par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 ==  "Prestations de sécurité sociale en espèces reçues par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 ==  "Autres prestations d'assurance sociale reçues par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 ==  "Prestations d'assistance sociale en espèces reçues par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 ==  "Indemnités d'assurance-dommage reçues par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 ==  "Transferts courants divers reçus par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 ==  "Impôts sur le revenu versés par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 ==  "Autres impôts courants versés par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 ==  "Cotisations sociales employeurs versées par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 ==  "Cotisations imputées versées par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 ==  "Cotisations sociales effectives à la charge des ménages versées par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 ==  "Primes nettes d'assurance-dommage versées par les ménages (y compris les entreprises individuelles)"|
                              TITLE_FR1 ==  "Transferts courants divers versés par les ménages (y compris les entreprises individuelles)"|
                                TITLE_FR1 == "Revenu disponible brut des ménages (y compris les entreprises individuelles)"
                              ) |>
  mutate(TITLE_FR1 =case_when (
    TITLE_FR1 == "Excédent brut d'exploitation des ménages hors EI (y compris revenus mixtes)" ~"ebe_hors_ei",
    TITLE_FR1 == "Excédent brut d'exploitation des entrepreneurs individuels (y compris revenus mixtes)" ~"ebe_ei",
      TITLE_FR1 =="Masse salariale reçue par les ménages (y compris les entreprises individuelles)" ~"sal_r",
      TITLE_FR1 =="Cotisations sociales employeurs reçues par les ménages (y compris les entreprises individuelles)" ~"cot_soc_r", 
    TITLE_FR1 == "Cotisations imputées reçues par les ménages (y compris les entreprises individuelles)"~"cot_imputees_r",
    TITLE_FR1 == "Intérêts reçus par les ménages (y compris les entreprises individuelles)"~"interet_r",
      TITLE_FR1 == "Dividendes reçus par les ménages (y compris les entreprises individuelles)"~"dividende_r",
      TITLE_FR1 == "Revenus de la propriété attribués aux assurés reçus par les ménages (y compris les entreprises individuelles)"~"rev_prop_assures_r",
      TITLE_FR1 == "Loyers des terrains et gisements reçus par les ménages (y compris les entreprises individuelles)"~"loyers_r",
      TITLE_FR1 == "Intérêts versés par les ménages (y compris les entreprises individuelles)"~"interet_v",
      TITLE_FR1 == "Loyers des terrains et gisements versés par les ménages (y compris les entreprises individuelles)"~"loyers_v",
      TITLE_FR1 ==  "Prestations de sécurité sociale en espèces reçues par les ménages (y compris les entreprises individuelles)"~"presta_ss_espece_r",
      TITLE_FR1 ==  "Autres prestations d'assurance sociale reçues par les ménages (y compris les entreprises individuelles)"~"autres_presta_ss_r",
      TITLE_FR1 ==  "Prestations d'assistance sociale en espèces reçues par les ménages (y compris les entreprises individuelles)"~"presta_ass_espece_r",
      TITLE_FR1 ==  "Indemnités d'assurance-dommage reçues par les ménages (y compris les entreprises individuelles)"~"chom_r",
      TITLE_FR1 ==  "Transferts courants divers reçus par les ménages (y compris les entreprises individuelles)"~"transfert_divers_r",
      TITLE_FR1 ==  "Impôts sur le revenu versés par les ménages (y compris les entreprises individuelles)"~"ir_v",
      TITLE_FR1 ==  "Autres impôts courants versés par les ménages (y compris les entreprises individuelles)"~"autres_impots_v",
      TITLE_FR1 ==  "Cotisations sociales employeurs versées par les ménages (y compris les entreprises individuelles)"~"cot_soc_v",
      TITLE_FR1 ==  "Cotisations imputées versées par les ménages (y compris les entreprises individuelles)"~"cot_imputees_v",
      TITLE_FR1 ==  "Cotisations sociales effectives à la charge des ménages versées par les ménages (y compris les entreprises individuelles)"~"cot_soc_eff_v",
      TITLE_FR1 ==  "Primes nettes d'assurance-dommage versées par les ménages (y compris les entreprises individuelles)"~"chom_v",
      TITLE_FR1 ==  "Transferts courants divers versés par les ménages (y compris les entreprises individuelles)"~"transfert_divers_v",
      TITLE_FR1 == "Revenu disponible brut des ménages (y compris les entreprises individuelles)"~"rdb_vrai"
  )) |>
  select(DATE, OBS_VALUE, TITLE_FR1) |>
  pivot_wider(names_from = TITLE_FR1, values_from = OBS_VALUE, id_cols = DATE)  |>
  mutate(rdb_test = ebe_hors_ei + ebe_ei + 
           sal_r+cot_soc_r+cot_imputees_r+interet_r+dividende_r+rev_prop_assures_r+ loyers_r - 
           interet_v - loyers_v +
           presta_ss_espece_r + autres_presta_ss_r+presta_ass_espece_r+ chom_r+transfert_divers_r - 
           ir_v - autres_impots_v - cot_soc_v - cot_imputees_v - cot_soc_eff_v - chom_v - transfert_divers_v) |>
  
  mutate(contrib_ebe = ebe_hors_ei) |>
  mutate(contrib_sal = sal_r+ebe_ei) |>
  mutate(contrib_presta = presta_ss_espece_r + autres_presta_ss_r+presta_ass_espece_r+ chom_r+transfert_divers_r- transfert_divers_v - chom_v) |>
  mutate(contrib_impot = ir_v + autres_impots_v + cot_soc_v + cot_imputees_v + cot_soc_eff_v - cot_soc_r-cot_imputees_r) |>
  mutate(contrib_propriete = loyers_r+rev_prop_assures_r+dividende_r+interet_r-interet_v - loyers_v) 

decompo_rdb_menages <- left_join(decompo_rdb_menages, pib_deflateur_conso_menages, by = "DATE") |>
  select(-pib_tot_vol)

decompo_rdb_menages <- decompo_rdb_menages |>
  arrange(DATE) |>
  mutate(var_sal = ((contrib_sal/lag(contrib_sal,1))-1)*100) |>
  mutate(var_ebe = ((contrib_ebe/lag(contrib_ebe,1))-1)*100)  |>
  mutate(var_impot = ((contrib_impot/lag(contrib_impot,1))-1)*100) |>
  mutate(var_prop = ((contrib_propriete/lag(contrib_propriete,1))-1)*100) |>
  mutate(var_presta = ((contrib_presta/lag(contrib_presta,1))-1)*100) |>
  mutate(var_pc = ((deflateur_conso_men/lag(deflateur_conso_men,1))-1)*100) |>
  mutate(part_sal = contrib_sal/rdb_test) |>
  mutate(part_impot = -contrib_impot/rdb_test) |>
  mutate(part_presta = contrib_presta/rdb_test) |>
  mutate(part_prop = contrib_propriete/rdb_test) |>
  mutate(part_ebe = contrib_ebe/rdb_test) |>
  mutate(sum = part_sal + part_impot + part_presta + part_ebe + part_prop) |>
  mutate(contrib_def_sal = part_sal*var_sal) |>
  mutate(contrib_def_impot = part_impot*var_impot) |>
  mutate(contrib_def_pc = -var_pc) |>
  mutate(contrib_def_ebe = part_ebe*var_ebe) |>
  mutate(contrib_def_presta = part_presta*var_presta) |>
  mutate(contrib_def_prop = part_prop*var_prop)

decompo_rdb_menages_def <- select(decompo_rdb_menages, DATE, contrib_def_sal, contrib_def_impot, contrib_def_pc, contrib_def_ebe, contrib_def_presta, contrib_def_prop) |>
  pivot_longer(cols=c(-1), names_to = "Composante", values_to = "value") |>
  filter(DATE >=as.Date('2019-10-01'))


var_pa_menages <- rdb_menages_2019 |>
  arrange(DATE) |>
  mutate(var_pa = ((pa_menages/lag(pa_menages,1))-1)*100) |>
  mutate(var_pa_uc = ((cum_pa_uc/lag(cum_pa_uc,1))-1)*100) |>
  select(DATE, var_pa, var_pa_uc) 

decompo_rdb_menages_def <- left_join(decompo_rdb_menages_def, var_pa_menages, by = "DATE")
decompo_rdb_menages_def <- decompo_rdb_menages_def |>
  mutate(Composante = case_when(
    Composante == "contrib_def_ebe"~"EBE des ménages purs", 
    Composante =="contrib_def_sal"~"Revenus d'activité", 
    Composante =="contrib_def_impot"~"Impôts et cotisations", 
    Composante =="contrib_def_presta"~"Prestations sociales", 
    Composante =="contrib_def_prop"~"Revenus de la propriété", 
    Composante =="contrib_def_pc"~"Prix à la consommation des ménages", 
  ))
decompo_rdb_menages_def <- mutate(decompo_rdb_menages_def, DATE2 = DATE+45)

plot_decompo_pa_menages <-  ggplot(data=decompo_rdb_menages_def, aes(x=DATE2, y=value, fill=forcats::fct_relevel(Composante, c("Revenus d'activité", 'EBE des ménages purs',
                                                                                                                              'Revenus de la propriété', "Prestations sociales", "Impôts et cotisations", "Prix à la consommation des ménages")))) +
  geom_col(position = "dodge2")+
  geom_line(aes(y = var_pa, color =  "Var. trimestrielle du pouvoir d'achat"), linewidth = 1.3) +
  labs(title = "Contributions à l'évolution du pouvoir d'achat et variation trimestrielle du pouvoir d'achat des ménages", 
       subtitle = "Lecture: Au 1er trimestre 2023, le pouvoir d'achat perd X % en variation trimestrielle. La hausse des prix contribue à - X points de pouvoir d'achat. \nSource : Insee (Comptes Nationaux Trimestriels, Résultats détaillés du T1-2023)\nCalculs et graphiques : @statjunior", 
       y = "Contribution (en point de pourcentage)", x = "Trimestre") +
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 12, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)
  ) +
  scale_colour_manual(values = c("Var. trimestrielle du pouvoir d'achat" = "blue")) + 
  scale_x_date(limits = as.Date(c("2019-10-01", NA))) +
  scale_fill_discrete(name = "Contribution", breaks=c("Revenus d'activité", 'EBE des ménages purs', 'Revenus de la propriété', "Prestations sociales", "Impôts et cotisations", "Prix à la consommation des ménages"))+
  #scale_color_manual(values=c("#F8766D","#C77CFF", "#00BA38", "#D39200", "#00B8E7"))+
  ylim(-10,10) 
plot_decompo_pa_menages


###Taux d'épargne des ménages

tx_ep_menage <- filter(csi_2014, TITLE_FR1 == "Épargne des ménages (y compris les entreprises individuelles)" |
                                 TITLE_FR1 == "Revenu disponible brut des ménages (y compris les entreprises individuelles)" | 
                                 TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des ménages (y compris les entreprises individuelles)") |>
     mutate(TITLE_FR1 = case_when(
        TITLE_FR1 == "Épargne des ménages (y compris les entreprises individuelles)"~"epargne_men",
         TITLE_FR1 == "Revenu disponible brut des ménages (y compris les entreprises individuelles)"~ "rdb_men",
         TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des ménages (y compris les entreprises individuelles)"~"capbesfi_men" 
       )) |>
     pivot_wider(names_from = TITLE_FR1, values_from = OBS_VALUE, id_cols = DATE) |>
     mutate(tx_epargne = epargne_men/rdb_men*100) |>
      mutate(tx_epargne_financiere = capbesfi_men/rdb_men*100) |>
     mutate(DATE = as.Date(DATE))

plot_tx_epargne_men <-ggplot(data = tx_ep_menage,
                              aes(x = DATE)) +
    geom_line(aes(y = tx_epargne, color = "Taux d'épargne des ménages"), size = 1.3) + 
    geom_line(aes(y = tx_epargne_financiere, color = "Taux d'épargne financière des ménages"), size = 1.1) + 
    labs(title = "Taux d'épargne des ménages", 
                  subtitle = "Lecture : Au 1er trimestre 2023, le taux d'épargne des ménages est de X % . \nSource : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
                  y = "Pourcentage (%)", x = "Année") +
    scale_x_date(limits = as.Date(c("2000-01-01", NA))) + 
    scale_color_manual(values = c("Taux d'épargne des ménages"="#00B8E7","Taux d'épargne financière des ménages" = "#F8766D"))+
     theme_grey()+
  theme(legend.position="bottom",
                 legend.title = element_text(size=10), 
                legend.text = element_text(size=12),
                    plot.title = element_text(color = "#333333", size = 14, face = "bold"),
                    plot.subtitle = element_text(color = "#333333", face = "italic")) 
plot_tx_epargne_men

####Entreprises (SNF)
decompo_tm_snf_2018t4 <- filter(decompo_tm_snf, DATE >as.Date('2018-10-10')) |>
  mutate(sum_var_tx_marge = sum(var_tx_marge)) |>
  mutate(sum_prod = sum(contrib_productivite)) |>
  mutate(sum_sal = sum(contrib_sal_reel)) |>
  mutate(sum_cotsoc = sum(contrib_cot_soc)) |>
  mutate(sum_echange = sum(contrib_terme_echange)) |>
  mutate(sum_autres = sum(contrib_autres_facteurs)) |>
  filter(DATE == max(DATE)) |>
  select(DATE, sum_var_tx_marge, sum_prod, sum_sal, sum_cotsoc, sum_echange, sum_autres) |> 
  rename(c("Taux de marge"="sum_var_tx_marge", "Productivité" = sum_prod, "Masse salariale réelle" = sum_sal, 
           "Cotisations sociales" = "sum_cotsoc", "Termes de l'échange" = "sum_echange", "Autres facteurs" = sum_autres)) |>
  pivot_longer(cols=c(-1), names_to = "Composante", values_to = "value")


plot_decompo_tm_snf_2018 <-ggplot(data = decompo_tm_snf_2018t4,
                                  aes(x = forcats::fct_relevel(Composante, c("Taux de marge", "Productivité","Masse salariale réelle","Cotisations sociales",
                                                                             "Termes de l'échange","Autres facteurs"), after = 1),
                                      y = value, fill = forcats::fct_relevel(Composante, c("Taux de marge", "Productivité","Masse salariale réelle","Cotisations sociales",
                                                                                           "Termes de l'échange","Autres facteurs"), after = 1))) +
  #fill=factor(ifelse(Composante=="Taux de marge","Highlighted","Normal")
  
  geom_col(position = "dodge", show.legend = FALSE) +
  labs(title = "Décomposition de la variation du taux de marge des sociétés non financières depuis la fin 2018", 
       subtitle = "Lecture : Depuis fin 2018, la baisse du taux de cotisation employeur contribue pour X points à la hausse du taux de marge des SNF. \nSource : Insee (Comptes trimestriels - Résultats détaillés T1 2023) \nCalculs et graphiques : @statjunior", 
       y = "Variation en points de pourcentage par rapport au 4e trimestre 2018", x = "Facteur de contribution") +
  #scale_color_manual(name = "Composante", values=c("red","grey50","grey50","grey50","grey50","grey50")) 
  scale_fill_manual(values=c("orange", "#00B8E7", "#00B8E7", "#00B8E7", "#00B8E7", "#00B8E7")) + 
  theme_bw()+
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 0, size = 12),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=7),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 14, face = "italic"))
plot_decompo_tm_snf_2018
#Taux de marge : EBE / VA
#Taux d'investissement : FBCF / VA
#Taux d'autofinancement : EB / FBCF

compte_snf <- filter(csi_2014, TITLE_FR1 == "Excédent brut d'exploitation des sociétés non financières" |
                            TITLE_FR1 == "Valeur ajoutée des sociétés non financières" | 
                            TITLE_FR1 == "Épargne des sociétés non financières" |
                             TITLE_FR1 == "Formation brute de capital fixe des sociétés non financières") |>
  
  mutate(TITLE_FR1 = case_when(
                 TITLE_FR1 == "Excédent brut d'exploitation des sociétés non financières" ~"ebe_snf",
                 TITLE_FR1 == "Valeur ajoutée des sociétés non financières" ~"va_snf", 
                 TITLE_FR1 == "Épargne des sociétés non financières" ~"eb_snf",
                 TITLE_FR1 == "Formation brute de capital fixe des sociétés non financières"~"fbcf_snf"
                 )) |>
  pivot_wider(names_from = TITLE_FR1, values_from = OBS_VALUE, id_cols = DATE) |>
  mutate(tx_marge_snf = ebe_snf/va_snf*100) |>
  mutate(tx_fbcf_snf = fbcf_snf/va_snf*100) |>
  mutate(tx_autofinancement_snf = eb_snf/fbcf_snf*100) 
  
plot_compte_snf <-ggplot(data = compte_snf,
                              aes(x = DATE)) +
  geom_line(aes(y = tx_marge_snf, color = "Taux de marge"), size = 1.3) + 
  geom_line(aes(y = tx_fbcf_snf, color = "Taux d'investissement"), size = 1.3) + 
  #geom_line(aes(y = tx_autofinancement_snf, color = "Taux d'autofinancement"), size = 1.3) + 
  labs(title = "Taux de marge et d’investissement des sociétés non financières", 
       subtitle = "Lecture : Au 1er trimestre 2023, le taux d'investissement des sociétés non financières est de X %. \nSource : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "% de la valeur ajoutée des SNF", x = "Année") +
  scale_x_date(limits = as.Date(c("1970-10-01", NA))) + 
  scale_color_manual(values = c("Taux de marge"="#F8766D","Taux d'investissement" = "#00B8E7"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) 
plot_compte_snf 
                             
#####APU 
###dynamique des recettes fiscales 
recette_apu <- filter(csi_2014, TITLE_FR1 == "TVA reçue par les administrations publiques" | 
                        TITLE_FR1 =="Droits de douane reçus par les administrations publiques"|
                        TITLE_FR1 =="Impôts sur les produits reçus par les administrations publiques"|
                        TITLE_FR1 =="Impôts sur la main d'oeuvre reçus par les administrations publiques"|
                        TITLE_FR1 =="Autres impôts sur la production reçus par les administrations publiques"|
                        TITLE_FR1 =="Impôts sur le revenu reçus par les administrations publiques"|
                        TITLE_FR1 =="Autres impôts courants reçus par les administrations publiques"|
                        TITLE_FR1 =="Cotisations imputées reçues par les administrations publiques"|
                        TITLE_FR1 =="Cotisations sociales employeurs reçues par les administrations publiques"|
                        TITLE_FR1 =="Cotisations sociales effectives à la charge des ménages reçues par les administrations publiques" | 
                        TITLE_FR1 =="Total des recettes des APU" |
                        TITLE_FR1 =="Total des dépenses des APU" | 
                        TITLE_FR1 =="Subventions versées par les APU" |
                        
                        TITLE_FR1 == "Consommations intermédiaires des administrations publiques"|
                        TITLE_FR1 == "Rémunérations salariales versées par les administrations publiques"|
                        TITLE_FR1 =="Impôts sur la production des administrations publiques"|
                        TITLE_FR1 =="Loyers des terrains et gisements versés par les administrations publiques"|
                        TITLE_FR1 == "Impôts sur le revenu versés par les administrations publiques"|
                        TITLE_FR1 =="Impôts sur la main d'oeuvre versés par les administrations publiques"|
                        TITLE_FR1 =="Autres impôts sur la production versés par les administrations publiques"|
                        TITLE_FR1 == "Intérêts versés par les administrations publiques" |
                      TITLE_FR1 =="Prestations de sécurité sociale en espèces versées par les administrations publiques" |
                      TITLE_FR1 =="Prestations d'assistance sociale en espèces versées par les administrations publiques" |
                      TITLE_FR1 =="Autres prestations d'assurance sociale versées par les administrations publiques" |
                      TITLE_FR1 =="Transferts sociaux en nature de produits marchands versés par les administrations publiques" |
                      TITLE_FR1 =="Transferts sociaux en nature de produits non marchands versés par les administrations publiques" |
                      TITLE_FR1 =="Transferts courants entre APU versés par les administrations publiques" |
                      TITLE_FR1 =="Primes nettes d'assurance-dommage versées par les administrations publiques" |
                      TITLE_FR1 =="Coopération internationale versée par les administrations publiques" |
                      TITLE_FR1 =="Transferts courants divers versés par les administrations publiques" |
                      TITLE_FR1 =="Ressources propres de l'UE versées par les administrations publiques" |
                      TITLE_FR1 =="Autres transferts en capital à payer par les administrations publiques" |
                      TITLE_FR1 =="Aides à l'investissement à payer par les administrations publiques" |
                      TITLE_FR1 =="Formation brute de capital fixe des administrations publiques" |
                      TITLE_FR1 =="Acquisitions nettes d'actifs non produits des administrations publiques"
                      ) |>

          mutate(TITLE_FR1 = case_when(
            TITLE_FR1 == "TVA reçue par les administrations publiques"~"tva",  
                   TITLE_FR1 =="Droits de douane reçus par les administrations publiques"~"douane", 
                   TITLE_FR1 =="Impôts sur les produits reçus par les administrations publiques"~"impot_produit", 
                   TITLE_FR1 =="Impôts sur la main d'oeuvre reçus par les administrations publiques"~"impot_main_oeuvre", 
                   TITLE_FR1 =="Autres impôts sur la production reçus par les administrations publiques"~"autre_impot_prod", 
                   TITLE_FR1 =="Impôts sur le revenu reçus par les administrations publiques"~"ir", 
                   TITLE_FR1 =="Autres impôts courants reçus par les administrations publiques"~"autre_impot_courant", 
                   TITLE_FR1 =="Cotisations imputées reçues par les administrations publiques"~"cot_imputees", 
                   TITLE_FR1 =="Cotisations sociales employeurs reçues par les administrations publiques"~"cot_soc_employeurs", 
                   TITLE_FR1 =="Cotisations sociales effectives à la charge des ménages reçues par les administrations publiques"~"cot_soc_menages", 
                   TITLE_FR1 =="Total des recettes des APU"~"tot_recettes_apu",
                   TITLE_FR1 =="Total des dépenses des APU"~"tot_depenses_apu", 
                  TITLE_FR1 =="Subventions versées par les APU"~"tot_subventions_apu", 
          
            
            TITLE_FR1 == "Consommations intermédiaires des administrations publiques"~"ci_apu", 
            TITLE_FR1 == "Rémunérations salariales versées par les administrations publiques"~"masse_salariale_apu", 
            TITLE_FR1 =="Impôts sur la production des administrations publiques"~"impot_prod_apu", 
            TITLE_FR1 =="Loyers des terrains et gisements versés par les administrations publiques"~"loyers_verses_apu", 
            TITLE_FR1 == "Impôts sur le revenu versés par les administrations publiques"~"impots_rev_pat_verses_apu", 
            TITLE_FR1 =="Impôts sur la main d'oeuvre versés par les administrations publiques"~"impots_main_oeuvre_verses_apu", 
             TITLE_FR1 =="Autres impôts sur la production versés par les administrations publiques"~"autres_impots_production_verses_apu", 
            
            TITLE_FR1 == "Intérêts versés par les administrations publiques"~"interets_verses_apu",
            
            TITLE_FR1 =="Prestations de sécurité sociale en espèces versées par les administrations publiques" ~"presta_ss_especes_versees_apu",
            TITLE_FR1 =="Prestations d'assistance sociale en espèces versées par les administrations publiques" ~"presta_ass_especes_versees_apu",
            TITLE_FR1 =="Autres prestations d'assurance sociale versées par les administrations publiques" ~"autres_presta_ass_versees_apu",
            TITLE_FR1 =="Transferts sociaux en nature de produits marchands versés par les administrations publiques" ~"transfert_nature_marchand_verse_apu",
            TITLE_FR1 =="Transferts sociaux en nature de produits non marchands versés par les administrations publiques" ~"transfert_nature_non_marchand_verse_apu",
            TITLE_FR1 =="Primes nettes d'assurance-dommage versées par les administrations publiques"~"ass_chom_verse_apu",
            
            TITLE_FR1 =="Transferts courants entre APU versés par les administrations publiques"~"transfert_entre_apu_verse_apu",
            TITLE_FR1 =="Coopération internationale versée par les administrations publiques"~"coop_int_versee_apu",
            TITLE_FR1 =="Transferts courants divers versés par les administrations publiques"~"transfert_courants_divers_apu",
            TITLE_FR1 =="Ressources propres de l'UE versées par les administrations publiques"~"ressources_ue_verse_apu",
            
            TITLE_FR1 =="Autres transferts en capital à payer par les administrations publiques"~"transfert_capital_verse_apu",
            TITLE_FR1 =="Aides à l'investissement à payer par les administrations publiques"~"aide_investissmt_verse_apu",
            TITLE_FR1 =="Formation brute de capital fixe des administrations publiques"~"fbcf_apu",
            TITLE_FR1 =="Acquisitions nettes d'actifs non produits des administrations publiques"~"acquisition_nette_actif_non_produit_par_apu",
            )) |>
            pivot_wider(names_from = TITLE_FR1, values_from = OBS_VALUE, id_cols = DATE) |>
  mutate(taxe_conso = tva+douane) |>
  mutate(taxe_production = impot_produit + impot_main_oeuvre + autre_impot_prod) |>
  mutate(impot_rev_pat = ir + autre_impot_courant) |>
  mutate(cot_soc = cot_imputees + cot_soc_employeurs + cot_soc_menages) |>
  
  ####
  mutate(tot_depenses_fonctionnement = masse_salariale_apu + impot_prod_apu + ci_apu + loyers_verses_apu + impots_rev_pat_verses_apu + impots_main_oeuvre_verses_apu + autres_impots_production_verses_apu) |>
  mutate(tot_depenses_transferts_sociaux = presta_ss_especes_versees_apu + presta_ass_especes_versees_apu + autres_presta_ass_versees_apu + ass_chom_verse_apu + transfert_nature_marchand_verse_apu) |>
  mutate(tot_autres_transferts = transfert_entre_apu_verse_apu + coop_int_versee_apu + transfert_courants_divers_apu + ressources_ue_verse_apu) |>
  mutate(total_investissement = transfert_capital_verse_apu + aide_investissmt_verse_apu + acquisition_nette_actif_non_produit_par_apu + fbcf_apu) |>
  arrange(DATE) |>
  mutate(def_taxe_conso = ((taxe_conso/lag(taxe_conso, 4))-1)*100) |>
  mutate(def_taxe_production = ((taxe_production/lag(taxe_production, 4))-1)*100) |>
  mutate(def_taxe_rev_pat = ((impot_rev_pat/lag(impot_rev_pat, 4))-1)*100) |>
  mutate(def_cot_soc = ((cot_soc/lag(cot_soc, 4))-1)*100) 

recette_apu <- left_join(recette_apu , pib_deflateur_conso_menages, by = "DATE") |> 
  select(-deflateur_conso_men, -pib_tot_vol) |>
  arrange(DATE) |>
  mutate(def_pib = ((pib_tot_val/lag(pib_tot_val, 4))-1)*100) |>
  mutate(elasticite_taxe_conso = def_taxe_conso/def_pib) |>
  mutate(elasticite_taxe_production = def_taxe_production/def_pib) |>
  mutate(elasticite_taxe_rev_pat = def_taxe_rev_pat/def_pib) |>
  mutate(elasticite_cot_soc = def_cot_soc/def_pib) |>
  mutate(tot_recettes_apu_pib = tot_recettes_apu/pib_tot_val*100) |>
  mutate(tot_depenses_apu_pib = tot_depenses_apu/pib_tot_val*100) |>
  mutate(depenses_subventions_apu_pib = tot_subventions_apu/pib_tot_val*100) |>
  mutate(tot_depenses_hors_subvention_apu_pib = tot_depenses_apu_pib - depenses_subventions_apu_pib) |>

  mutate(depenses_fonctionnement_pib = tot_depenses_fonctionnement/pib_tot_val*100) |>
  mutate(depenses_interet_pib = interets_verses_apu/pib_tot_val*100) |>
  mutate(depenses_transferts_sociaux_pib = tot_depenses_transferts_sociaux/pib_tot_val*100) |>
  mutate(depenses_autres_transferts_pib = tot_autres_transferts/pib_tot_val*100) |>
  mutate(depenses_investissement_pib = total_investissement/pib_tot_val*100) |>
  mutate(tot_decompo_globale = depenses_fonctionnement_pib + depenses_interet_pib + 
                              depenses_transferts_sociaux_pib +depenses_autres_transferts_pib + 
                              depenses_investissement_pib+ depenses_subventions_apu_pib)

fill2022 <- filter(recette_apu, DATE == "2022-10-01")

#Plot dépenses recettes en % du PIB valeur 
plot_deficit_apu_pib <- ggplot(data = recette_apu,
                               aes(x = DATE)) +
  geom_line(aes(y = tot_recettes_apu_pib, color = "Total des recettes"), size = 1.3) + 
  geom_line(aes(y = tot_depenses_apu_pib, color = "Total des dépenses"), size = 1.3) + 
  geom_line(aes(y = tot_depenses_hors_subvention_apu_pib, color = "Total des dépenses hors subventions"), size = 1.1) + 
  
  labs(title = "Recettes et dépenses des administrations publiques rapportées au PIB", 
       subtitle = "\nSource : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "% du PIB valeur", x = "Année") +
  scale_x_date(limits = as.Date(c("1980-01-01", NA))) + 
  scale_color_manual(values = c("Total des dépenses"="#F8766D","Total des dépenses hors subventions"="purple", "Total des recettes" = "#00B8E7"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) +
  ylim(45, NA)


######Décomposition des dépenses 
#Dépenses de fonctionnement 
plot_dep_fonctionnement_apu_pib <- ggplot(data = recette_apu,
                                   aes(x = DATE)) +
  geom_line(aes(y = depenses_fonctionnement_pib, color = "Dépenses de fonctionnement"), size = 1.3) + 
  labs(title = "Total des dépenses de fonctionnement", 
       subtitle = "Source : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "% du PIB valeur", x = "Année") +
  scale_x_date(limits = as.Date(c("1980-01-01", NA))) + 
  scale_color_manual(values = c("Dépenses de fonctionnement"="blue"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) +ylim(16, NA)
 

#Total des subventions 
plot_subventions_apu_pib <- ggplot(data = recette_apu,
                               aes(x = DATE)) +
  geom_line(aes(y = depenses_subventions_apu_pib, color = "Total des subventions"), size = 1.3) + 
  labs(title = "Subventions versées par les administrations publiques", 
       subtitle = "Source : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "% du PIB valeur", x = "Année") +
  scale_x_date(limits = as.Date(c("1980-01-01", NA))) + 
  scale_color_manual(values = c("Total des subventions"="purple"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) 


#Total des intérêts versés
plot_interet_verses_apu_pib <- ggplot(data = recette_apu,
                                   aes(x = DATE)) +
  geom_line(aes(y = depenses_interet_pib, color = "Total des intérêts versés"), size = 1.3) + 
  labs(title = "Charges d'intérêts versées par les administrations publiques", 
       subtitle = "Source : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "% du PIB valeur", x = "Année") +
  scale_x_date(limits = as.Date(c("1980-01-01", NA))) + 
  scale_color_manual(values = c("Total des intérêts versés"="#993300"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) 


plot_transferts_sociaux_nature_pib <- ggplot(data = recette_apu,
                                      aes(x = DATE)) +
  geom_line(aes(y = depenses_transferts_sociaux_pib, color = "Total des transferts sociaux ou marchands en nature"), size = 1.3) + 
  labs(title = "Transferts sociaux et apparentés (en espèce ou en nature)", 
       subtitle = "Source : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior",        
       y = "% du PIB valeur", x = "Année") +
  scale_x_date(limits = as.Date(c("1980-01-01", NA))) + 
  scale_color_manual(values = c("Total des transferts sociaux ou marchands en nature"="#FF33FF"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) + ylim(15, NA) 


plot_autres_transferts_pib <- ggplot(data = recette_apu,
                                             aes(x = DATE)) +
  geom_line(aes(y = depenses_autres_transferts_pib, color = "Autres transferts entre administrations"), size = 1.3) + 
  labs(title = "Transferts entre administrations", 
       subtitle = "Source : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "% du PIB valeur", x = "Année") +
  scale_x_date(limits = as.Date(c("1980-01-01", NA))) + 
  scale_color_manual(values = c("Autres transferts entre administrations"="orange"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) 


plot_investissement_apu_pib <- ggplot(data = recette_apu,
                                     aes(x = DATE)) +
  geom_line(aes(y = depenses_autres_transferts_pib, color = "Dépenses reliées à l'investissement"), size = 1.3) + 
  labs(title = "Dépenses reliées à l'investissement", 
       subtitle = "Source : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "% du PIB valeur", x = "Année") +
  scale_x_date(limits = as.Date(c("1980-01-01", NA))) + 
  scale_color_manual(values = c("Dépenses reliées à l'investissement"="#339900"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) 


plot_decompo_depenses <- plot_grid(plot_dep_fonctionnement_apu_pib, plot_subventions_apu_pib,  
                                   plot_transferts_sociaux_nature_pib, plot_autres_transferts_pib, 
                                   plot_investissement_apu_pib, plot_interet_verses_apu_pib, ncol = 2, nrow = 3)
plot_decompo_depenses


#######Déficit des APU
deficit_apu <- filter(csi_2014, TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des administrations publiques" | 
                        TITLE_FR1 == "Intérêts reçus par les administrations publiques" | 
                        TITLE_FR1 =="Intérêts versés par les administrations publiques") |>
  mutate(TITLE_FR1 = case_when(
    TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des administrations publiques"~"def_apu", 
    TITLE_FR1 == "Intérêts reçus par les administrations publiques"~"interet_recu_apu", 
    TITLE_FR1 == "Intérêts versés par les administrations publiques"~"interet_verses_apu"
  )) |>
  select(DATE, OBS_VALUE, TITLE_FR1) |>
  pivot_wider(names_from = TITLE_FR1, values_from = OBS_VALUE, id_cols = DATE) |>
  mutate(def_sans_interet = def_apu+interet_verses_apu-interet_recu_apu)

deficit_apu <- left_join(deficit_apu, pib_deflateur_conso_menages, by = "DATE") 
deficit_apu <- select(deficit_apu, -deflateur_conso_men, -pib_tot_vol) |>
  mutate(def_total_pib = def_apu/pib_tot_val*100) |>
  mutate(def_primaire_pib = def_sans_interet/pib_tot_val*100) |>
  mutate(trim1 = ifelse(substr(DATE, 6, 7)== "01", 1, 0 )) |>
  mutate(trim2 = ifelse(substr(DATE, 6, 7)== "04", 1, 0 )) |>
  mutate(trim3 = ifelse(substr(DATE, 6, 7)== "07", 1, 0 )) 

plot_deficit_apu_pib <- ggplot(data = deficit_apu,
                           aes(x = DATE)) +
  geom_line(aes(y = def_primaire_pib, color = "Déficit primaire (hors charge d'intérêt reçue et versée)"), size = 1.3) + 
  geom_line(aes(y = def_total_pib, color = "Déficit total"), size = 1.3) + 
  geom_hline(yintercept=-3, linetype="dashed", color = "black") +
  labs(title = "Déficit public des APU et déficit primaire hors charge d'intérêt rapporté au PIB", 
       subtitle = "Lecture : Au 1er trimestre 2023, le déficit public est de X % du PIB. \nSource : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "% du PIB valeur", x = "Année") +
  scale_x_date(limits = as.Date(c("1970-10-01", NA))) + 
  scale_color_manual(values = c("Déficit total"="#F8766D","Déficit primaire (hors charge d'intérêt reçue et versée)" = "#00B8E7"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) 
plot_deficit_apu_pib

interet_apu <- filter(csi_2014, TITLE_FR1 == "Intérêts reçus par les administrations publiques" | 
                       TITLE_FR1 =="Intérêts versés par les administrations publiques") |>
  mutate(TITLE_FR1 = case_when(
    TITLE_FR1 == "Intérêts reçus par les administrations publiques"~"interet_recu_apu", 
    TITLE_FR1 == "Intérêts versés par les administrations publiques"~"interet_verses_apu"
  )) |>
  pivot_wider(names_from = TITLE_FR1, values_from = OBS_VALUE, id_cols = DATE)  |>
  mutate(interet_net = (interet_verses_apu - interet_recu_apu)/1000)

interet_apu <- left_join(interet_apu, pib_deflateur_conso_menages, by = "DATE") 
interet_apu <- select(interet_apu, -deflateur_conso_men, -pib_tot_vol) |>
  mutate(interet_pib = (interet_net*1000)/pib_tot_val*100)

plot_interet_val <- ggplot(data = interet_apu,
                                 aes(x = DATE)) +
  geom_line(aes(y = interet_net), color = "#00B8E7", size = 1.3) + 
  labs(title = "Charge d'intérêt nette nominale des administrations publiques", 
       subtitle = "Lecture : Au 1er trimestre 2023, le pouvoir d'achat du revenu disponible brut des ménages recule de X points par rapport à la fin 2019. \nSource : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "Milliards d'€", x = "Année") +
  scale_x_date(limits = as.Date(c("1970-10-01", NA))) + 
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) 


plot_interet_pib <- ggplot(data = interet_apu,
                           aes(x = DATE)) +
  geom_line(aes(y = interet_pib), color = "#00B8E7", size = 1.3) + 
  labs(title = "Charge d'intérêt nette des administrations publiques en proportion du PIB nominal", 
       subtitle = "Lecture : Au 1er trimestre 2023, le pouvoir d'achat du revenu disponible brut des ménages recule de X points par rapport à la fin 2019. \nSource : Insee (Comptes nationaux Trimestriels - Dernier point : 1er trimestre 2023) \nCalculs et graphiques : @statjunior", 
       y = "% du PIB valeur", x = "Année") +
  scale_x_date(limits = as.Date(c("1970-10-01", NA))) + 
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic")) 

plot_interet_apu <- plot_grid(plot_interet_val, plot_interet_pib, ncol = 1, nrow = 2)
plot_interet_apu 

#####Capacité besoin de financement
capbesfi <- filter(csi_2014, TITLE_FR1 == "Capacité (+) ou besoin (-) de financement du reste du monde" |
                     TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des sociétés non financières" | 
                     TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des ISBLSM" |
                     TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des administrations publiques" |
                     TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des ménages (y compris les entreprises individuelles)" |
                     TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des sociétés financières") |>
  
  mutate(TITLE_FR1 = case_when( 
  TITLE_FR1 == "Capacité (+) ou besoin (-) de financement du reste du monde" ~"total",
  TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des sociétés non financières" ~"snf", 
  TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des ISBLSM" ~"isblsm",
  TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des administrations publiques" ~"apu",
  TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des ménages (y compris les entreprises individuelles)" ~"men",
  TITLE_FR1 == "Capacité (+) ou besoin (-) de financement des sociétés financières"~"sf"
  )) |>
  select(DATE, OBS_VALUE, TITLE_FR1) |>
  pivot_wider(names_from = TITLE_FR1, values_from = OBS_VALUE, id_cols = DATE)

capbesfi <- left_join(capbesfi, pib_deflateur_conso_menages, by = "DATE") |>
  select(-pib_tot_vol, -deflateur_conso_men) |>
  mutate(capbesfi_apu_pib = apu/pib_tot_val*100) |>
  mutate(capbesfi_men_pib = men/pib_tot_val*100) |>
  mutate(capbesfi_sf_pib = sf/pib_tot_val*100) |>
  mutate(capbesfi_snf_pib = snf/pib_tot_val*100) |>
  mutate(capbesfi_isblsm_pib = isblsm/pib_tot_val*100) |>
  mutate(capbesfi_total_pib = total/pib_tot_val*100) |>
  select(DATE, capbesfi_apu_pib, capbesfi_men_pib, capbesfi_sf_pib, capbesfi_snf_pib, capbesfi_isblsm_pib,capbesfi_total_pib) |>
  pivot_longer(cols=c(2:6), names_to = "Composante", values_to = "value") |>
  mutate(Composante = case_when(
    Composante == "capbesfi_isblsm_pib"~"ISBLSM",
    Composante == "capbesfi_men_pib"~"Ménages",
    Composante == "capbesfi_sf_pib"~"Sociétés financières",
    Composante == "capbesfi_snf_pib"~"Sociétés non financières",
    Composante == "capbesfi_apu_pib"~"APU",
  ))  |>
  mutate(value = round(value, 2)) |>
  mutate(capbesfi_total_pib = -capbesfi_total_pib) |>
  mutate(capbesfi_total_pib = round(capbesfi_total_pib, 2))


plot_capbesfi <-  ggplot(data=capbesfi, aes(x=DATE, y=value, fill=forcats::fct_relevel(Composante, c("Ménages", 
                                                                                                     'Sociétés financières',
                                                                                                     "Sociétés non financières", 
                                                                                                     "ISBLSM", 
                                                                                                     "APU" )))) +
  geom_col()+
  geom_line(aes(y = capbesfi_total_pib, color =  "Total vis-à-vis du reste du monde"), linewidth = 1.3) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(title = "Capacité (+) ou Besoin (-) de financement de l'économie française vis à vis du reste du monde", 
       subtitle = "Lecture: Au 1er trimestre 2023, l'économie française est en besoin de financement vis à vis du reste du monde à hauteur de X points de PIB. \nSource : Insee (Comptes Nationaux Trimestriels, Résultats détaillés du T1-2023)\nCalculs et graphiques : @statjunior", 
       y = "% du PIB nominal", x = "Trimestre") +
  theme_grey()+
  theme(legend.position="bottom",
        plot.title = element_text(color = "#333333", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "#333333", size = 12, face = "italic"),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12)
  ) +
  scale_colour_manual(values = c("Total vis-à-vis du reste du monde" = "blue")) + 
  scale_x_date(limits = as.Date(c("2000-01-01", NA))) +
  scale_fill_discrete(name = "Contribution", breaks=c("Ménages", 
                                                      'Sociétés financières',
                                                      "Sociétés non financières", 
                                                      "ISBLSM", 
                                                      "APU" ))+
  #scale_color_manual(values=c("#F8766D","#C77CFF", "#00BA38", "#D39200", "#00B8E7"))+
  ylim(-18,18) 
plot_capbesfi
 


###EQUILIBRE DU PIB###
###Chargement du CNT 
load(file = file.path("data_ofce", "cnt.RData"))
load(file = file.path("data_ofce", "cnt_operations.RData"))
rdb_cnt2_file <- "data_ofce/deflateur_conso_menages_t2_2002.xlsx"
rdb_cnt2 <- read_excel(rdb_cnt2_file)

##########Déflateurs ########
###Déflateur des prix de VA 
cnt_va <- filter(cnt, OPERATION =="B1" & CNA_PRODUIT == "D-CNT") |>
  filter(VALORISATION == "L" | VALORISATION == "V") |>
  select(DATE, OBS_VALUE, VALORISATION) |>
  rename(va_tot = OBS_VALUE) |>
  mutate(va_tot = va_tot/1000) |>
  spread(key = VALORISATION, value = va_tot) |>
  mutate(deflateur_prix_va = V/L*100) |>
  select(-L, -V)

###Déflateur de l'investissement
def_fbcf_men <- "data_ofce/deflateur_fbcf_men.xlsx"
deflateur_fbcf_men <- read_excel(def_fbcf_men)


###Déflateur de la conso des ménages 
def_conso_men <- "data_ofce/deflateur_conso_menages_t2_2002.xlsx"
deflateur_conso_men <- read_excel(def_conso_men) |>
  select(DATE, deflateur_conso_finale_men) 
deflateur_conso_men$DATE <-   str_replace(deflateur_conso_men$DATE,"-03-01", "-01-01")
deflateur_conso_men$DATE <-   str_replace(deflateur_conso_men$DATE,"-06-01", "-04-01")
deflateur_conso_men$DATE <-   str_replace(deflateur_conso_men$DATE,"-09-01", "-07-01")
deflateur_conso_men$DATE <-   str_replace(deflateur_conso_men$DATE,"-12-01", "-10-01")  
deflateur_conso_men$DATE <-as.Date(paste(deflateur_conso_men$DATE,1,sep="-"), format = "%Y-%m-%d")

###############
####Exportations-importations et déficit en % du PIB en valeur
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

###Chargement capacité-besoin de financement RDM global
cap_bes_fi_rdm <- "data_ofce/eq-cnt/compte_agent_t2_2022.xls"
rdm_capbes_fi <- read_excel(cap_bes_fi_rdm, sheet = "RdM", skip = 6) |>
  rename("DATE" = "...1", "cap_bes_fi_rdm" = "B9NF") |>
  select(DATE, cap_bes_fi_rdm) |>
  filter(!is.na(cap_bes_fi_rdm)) 
rdm_capbes_fi$DATE <-   str_replace(rdm_capbes_fi$DATE,"T1", "-01-01")
rdm_capbes_fi$DATE <-   str_replace(rdm_capbes_fi$DATE,"T2", "-04-01")
rdm_capbes_fi$DATE <-   str_replace(rdm_capbes_fi$DATE,"T3", "-07-01")
rdm_capbes_fi$DATE <-   str_replace(rdm_capbes_fi$DATE,"T4", "-10-01")
rdm_capbes_fi$DATE <-as.Date(paste(rdm_capbes_fi$DATE,1,sep="-"), format = "%Y-%m-%d")
rdm_capbes_fi <- left_join(rdm_capbes_fi, pib_valeur, by = "DATE") 
rdm_capbes_fi <-  mutate(rdm_capbes_fi, ratio_capbesfi_pib = (cap_bes_fi_rdm/pib_trim*100)) |>
  select(-pib_trim) 

cnt_val_exp_imp <- left_join(cnt_val_exp_imp,rdm_capbes_fi,  by = "DATE") 
mean_ratio_capbesfi_over70 <- colMeans(cnt_val_exp_imp[85:290,17])
cnt_val_exp_imp <- mutate(cnt_val_exp_imp, ratio_capbesfi_pib_scale = -ratio_capbesfi_pib)

###Graphique énergie
cnt_val_exp_imp_graph <- mutate(cnt_val_exp_imp, trim_ratio_energ_pib = trim_ratio_energ_pib/0.5) |>
  mutate(ratio_contrefact_capbesfi = ratio_capbesfi_pib+ratio_energ_pib) |>
  mutate(ratio_capbesfi_contrefact_pib_scale = -ratio_contrefact_capbesfi)
scale = -1
plot_eq_energie <- ggplot(cnt_val_exp_imp_graph, aes(x = DATE)) + 
  geom_line(aes(y=ratio_energ_pib, color = "Déficit de la branche énergie (gauche)"), size = 1.03) + 
  geom_point(aes(y=ratio_energ_pib, color = "Déficit de la branche énergie (gauche)")) + 
  geom_line(aes(y=ratio_capbesfi_pib_scale, color = "Besoin de financement RDM (droite)"), size = 1.03) + 
  geom_point(aes(y=ratio_capbesfi_pib_scale, color = "Besoin de financement RDM (droite)")) + 
  # geom_bar(aes(x = DATE, y = trim_ratio_energ_pib, color = "Croissance trimestrielle du déficit (droite)"), stat="identity", fill = "brown") +
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name="Points de pourcentage du PIB trimestriel")) +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[99]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[121]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[237]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[290]), linetype=4, color = "black") +
  geom_line(aes(y = mean_ratio_energ_over70), linetype = 4, color = "blue" ) +
  geom_line(aes(y = mean_ratio_capbesfi_over70), linetype = 4, color = "brown" ) +
  labs(x = "Année", y = "Points de pourcentage du PIB trimestriel", 
       title = "Déficit commercial du secteur énergétique en part du PIB trimestriel")  +
  scale_x_date(limits = as.Date(c("1970-01-01", NA))) +
  scale_color_manual(values = c("Déficit de la branche énergie (gauche)" = "#4E84C4", "Besoin de financement RDM (droite)" = "brown")) +
  theme(legend.position="bottom", legend.text = element_text(size = 7))

pdf('graphs/CNT/chocs_energie/def_commercial_energie.pdf')
plot_eq_energie 
dev.off()
# write.xlsx(as.data.frame(cnt_val_exp_imp_graph), file = "C:/Users/gasto/Documents/STAGE OFCE 2022/Data/stage_ofce/note_IMK/files/note_imk_figures.xlsx",
#            sheetName="fig10", append=TRUE)

plot_eq_energie_english <- ggplot(cnt_val_exp_imp_graph, aes(x = DATE)) + 
  geom_line(aes(y=ratio_energ_pib, color = "Energy branch trade-deficit (left)"), size = 1.03) + 
  geom_point(aes(y=ratio_energ_pib, color = "Energy branch trade-deficit (left)")) + 
  geom_line(aes(y=ratio_capbesfi_pib_scale, color = "Borrowing requirement vis-à-vis RoW (right)"), size = 1.03) + 
  geom_point(aes(y=ratio_capbesfi_pib_scale, color = "Borrowing requirement vis-à-vis RoW (right)")) + 
  # geom_bar(aes(x = DATE, y = trim_ratio_energ_pib, color = "Croissance trimestrielle du déficit (droite)"), stat="identity", fill = "brown") +
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name="Quarterly GDP perceptange points")) +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[99]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[121]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[237]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[290]), linetype=4, color = "black") +
  geom_line(aes(y = mean_ratio_energ_over70), linetype = 4, color = "blue" ) +
  geom_line(aes(y = mean_ratio_capbesfi_over70), linetype = 4, color = "brown" ) +
  labs(x = "Year", y = "Quarterly GDP percentage points", 
       title = "Energy branch trade-deficit as a share of quarterly GDP")  +
  scale_x_date(limits = as.Date(c("1970-01-01", NA))) +
  scale_color_manual(values = c("Energy branch trade-deficit (left)" = "#4E84C4", "Borrowing requirement vis-à-vis RoW (right)" = "brown")) +
  theme(legend.position="bottom", legend.text = element_text(size = 7))

pdf('graphs/IPC-IPCH/graph-imk/fig10_def_commercial_energie.pdf')
plot_eq_energie_english 
dev.off()

###Graphique cap-bes avec et hors énergie
plot_eq_energie_hors_energie <- ggplot(cnt_val_exp_imp_graph, aes(x = DATE)) + 
  geom_line(aes(y=(ratio_capbesfi_pib)*-1, color = "Total"), size = 1.03) + 
  geom_point(aes(y=(ratio_capbesfi_pib)*-1, color = "Total")) +
  geom_line(aes(y=(ratio_contrefact_capbesfi)*-1, color = "Hors énergie"), size = 1.03) + 
  geom_point(aes(y=(ratio_contrefact_capbesfi)*-1, color = "Hors énergie")) +
  # geom_bar(aes(x = DATE, y = trim_ratio_energ_pib, color = "Croissance trimestrielle du déficit (droite)"), stat="identity", fill = "brown") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[99]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[121]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[237]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[290]), linetype=4, color = "black") +
  labs(x = "Année", y = "Points de pourcentage du PIB trimestriel", 
       title = "Capacité et besoin de financement vis à vis du RDM avec et sas énergie")  +
  scale_x_date(limits = as.Date(c("1970-01-01", NA))) +
  scale_color_manual(values = c("Total" = "brown", "Hors énergie" = "purple")) +
  theme(legend.position="bottom", legend.text = element_text(size = 7))
plot_eq_energie_hors_energie

pdf('graphs/CNT/chocs_energie/capbesfi_energie_hors_energie.pdf')
plot_eq_energie_hors_energie
dev.off()

plot_eq_energie_hors_energie_english <- ggplot(cnt_val_exp_imp_graph, aes(x = DATE)) + 
  geom_line(aes(y=(ratio_capbesfi_pib)*-1, color = "Global"), size = 1.03) + 
  geom_point(aes(y=(ratio_capbesfi_pib)*-1, color = "Global")) +
  geom_line(aes(y=(ratio_contrefact_capbesfi)*-1, color = "Excluding energy"), size = 1.03) + 
  geom_point(aes(y=(ratio_contrefact_capbesfi)*-1, color = "Excluding energy")) +
  # geom_bar(aes(x = DATE, y = trim_ratio_energ_pib, color = "Croissance trimestrielle du déficit (droite)"), stat="identity", fill = "brown") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[99]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[121]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[237]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph$DATE[290]), linetype=4, color = "black") +
  labs(x = "Year", y = "Quarterly GDP percentage points", 
       title = "Capacity and borrowing requirements (global and without energy")  +
  scale_x_date(limits = as.Date(c("1970-01-01", NA))) +
  scale_color_manual(values = c("Global" = "brown", "Excluding energy" = "purple")) +
  theme(legend.position="bottom", legend.text = element_text(size = 7))
plot_eq_energie_hors_energie_english

pdf('graphs/IPC-IPCH/graph-imk/fig11_capbesfi_energie_hors_energie.pdf')
plot_eq_energie_hors_energie_english
dev.off()

#Graphique pétrole
cnt_val_exp_imp_graph_petrole <- arrange(cnt_val_exp_imp, DATE) |>
  mutate(trim_ratio_petrole_pib = ratio_petrole_pib-lag(ratio_petrole_pib, 1)) |>
  mutate(ratio_capbesfi_pib_scale = ratio_capbesfi_pib_scale/2)

scale2 = -2
plot_eq_petrole <- ggplot(cnt_val_exp_imp_graph_petrole, aes(x = DATE)) + 
  geom_line(aes(y=ratio_petrole_pib, color = "Déficit de la branche énergie (gauche)"), size = 1.03) + 
  geom_point(aes(y=ratio_petrole_pib, color = "Déficit de la branche énergie (gauche)")) + 
  geom_line(aes(y=ratio_capbesfi_pib_scale, color = "Besoin de financement RDM (droite)"), size = 1.03) + 
  geom_point(aes(y=ratio_capbesfi_pib_scale, color = "Besoin de financement RDM (droite)")) +
  #geom_bar(aes(x = DATE, y = trim_ratio_petrole_pib, color = "Variation trimestrielle du déficit (droite)"), stat="identity", fill = "brown") +
  scale_y_continuous(sec.axis = sec_axis(~.*scale2, name="Points de pourcentage du PIB trimestriel")) +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph_petrole$DATE[99]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph_petrole$DATE[121]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph_petrole$DATE[237]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph_petrole$DATE[290]), linetype=4, color = "black") +
  geom_line(aes(y = mean_ratio_capbesfi_over70), linetype = 4, color = "brown" ) +
  geom_line(aes(y = mean_ratio_petrole_over70), linetype = 4, color = "blue" ) +
  labs(x = "Année", y = "Points de pourcentage du PIB trimestriel", 
       title = "Déficit commercial de la branche cokéfaction-raffinage en part du PIB trimestriel")  +
  scale_x_date(limits = as.Date(c("1970-01-01", NA))) +
  scale_color_manual(values = c("Déficit de la branche énergie (gauche)" = "#4E84C4"
                                  , "Besoin de financement RDM (droite)" = "brown")) +
  theme(legend.position="bottom", legend.text = element_text(size = 7))
plot_eq_petrole

pdf('graphs/CNT/chocs_energie/def_commercial_petrole.pdf')
plot_eq_petrole
dev.off()



###Couts salariaux unitaires 
cout_sal_unit <- "data_ofce/eq-cnt/cout_sal_unit.xls"
cnt_cout_sal_unit <- read_excel(cout_sal_unit, sheet = "Niveaux", skip = 7) |>
  filter(!is.na(prix_prod))
cnt_cout_sal_unit$DATE <-   str_replace(cnt_cout_sal_unit$DATE,"T1", "-01-01")
cnt_cout_sal_unit$DATE <-   str_replace(cnt_cout_sal_unit$DATE,"T2", "-04-01")
cnt_cout_sal_unit$DATE <-   str_replace(cnt_cout_sal_unit$DATE,"T3", "-07-01")
cnt_cout_sal_unit$DATE <-   str_replace(cnt_cout_sal_unit$DATE,"T4", "-10-01")
cnt_cout_sal_unit$DATE <-as.Date(paste(cnt_cout_sal_unit$DATE,1,sep="-"), format = "%Y-%m-%d")

###Décomposition du RDB nominal (part de revenus/prestations)
decompo_rdb <- "data_ofce/eq-cnt/rdb_men_decomposition.xls"
decompo_rdb_cnt <- read_excel(decompo_rdb, sheet = "Niveaux") 
decompo_rdb_cnt$DATE <-   str_replace(decompo_rdb_cnt$DATE,"T1", "-01-01")
decompo_rdb_cnt$DATE <-   str_replace(decompo_rdb_cnt$DATE,"T2", "-04-01")
decompo_rdb_cnt$DATE <-   str_replace(decompo_rdb_cnt$DATE,"T3", "-07-01")
decompo_rdb_cnt$DATE <-   str_replace(decompo_rdb_cnt$DATE,"T4", "-10-01")
decompo_rdb_cnt$DATE <-as.Date(paste(decompo_rdb_cnt$DATE,1,sep="-"), format = "%Y-%m-%d")

demo_uc <- "data_ofce/eq-cnt/trim_uc_menages.xlsx"
uc_menages <- read_excel(demo_uc, sheet = "T_2101") |>
  select(-2:-4) |>
  arrange(year) |>
  mutate(cum_men = ifelse(year == 1959, 100, NA)) |>
  mutate(cum_uc = ifelse(year == 1959, 100, NA)) |>
  mutate(cum_pop = ifelse(year == 1959, 100, NA)) 

decompo_rdb_cnt <- mutate(decompo_rdb_cnt, year = as.numeric(substr(DATE, 1, 4)))
decompo_rdb_cnt <- left_join(decompo_rdb_cnt, uc_menages, by = "year") |>
  filter(year >=1959)

i <-2
while(i <=nrow(decompo_rdb_cnt)){
  decompo_rdb_cnt[i, "cum_pop"] <- decompo_rdb_cnt[i-1, "cum_pop"]*(1+(decompo_rdb_cnt[i, "trim_pop_moy"]))
  i <- i+1
}
i <-2
while(i <=nrow(decompo_rdb_cnt)){
  decompo_rdb_cnt[i, "cum_men"] <- decompo_rdb_cnt[i-1, "cum_men"]*(1+(decompo_rdb_cnt[i, "trim_men"]))
  i <- i+1
}
i <-2
while(i <=nrow(decompo_rdb_cnt)){
  decompo_rdb_cnt[i, "cum_uc"] <- decompo_rdb_cnt[i-1, "cum_uc"]*(1+(decompo_rdb_cnt[i, "trim_uc"]))
  i <- i+1
}
decompo_rdb_cnt <- left_join(decompo_rdb_cnt, deflateur_conso_men, by = "DATE")
decompo_rdb_cnt_restrict <- select(decompo_rdb_cnt, DATE, B6_rdb, deflateur_conso_finale_men, 20:23) |>
  mutate(group = ifelse(substr(DATE, 1, 4) == "1973", 1, NA)) |>
  mutate(group = ifelse(substr(DATE, 1, 4) == "1978", 2, group)) |>
  mutate(group = ifelse(substr(DATE, 1, 4) == "2019", 3, group)) |>
  mutate(group = as.numeric(group)) 
sum_rdb <- decompo_rdb_cnt_restrict  |>
  group_by(group) |>
  summarise_at(vars(2:3), funs(mean(.))) |>
  filter(group !=is.na(group)) |>
  mutate(DATE = c('1973-10-01', '1978-10-01', '2021-04-01')) |>
  select(-group)
sum_rdb$DATE <- as.Date(paste(sum_rdb$DATE,1,sep="-"), format = "%Y-%m-%d")

sum_rdb_else <- select(decompo_rdb_cnt_restrict, DATE, B6_rdb, deflateur_conso_finale_men) |>
  filter((DATE > as.Date('1973-10-01') & DATE <= as.Date('1975-01-01')) | 
           (DATE > as.Date('1978-10-01') & DATE <= as.Date('1981-03-01')) | 
           (DATE >= as.Date('2021-07-01'))) 
sum_rdb <- rbind(sum_rdb, sum_rdb_else) |>
  arrange(DATE)
decompo_rdb_cnt_restrict <- select(decompo_rdb_cnt_restrict, -B6_rdb, -deflateur_conso_finale_men) 
decompo_rdb_cnt_restrict <- left_join(sum_rdb, decompo_rdb_cnt_restrict, by = "DATE")

rdbuc<- "data_ofce/eq-cnt/rdb_rdb_uc.xls"
rdbuc <- read_excel(rdbuc, sheet = "Evolutions") |>
  select(DATE, rdb, rdb_uc) 
rdbuc$DATE <-   str_replace(rdbuc$DATE,"T1", "-01-01")
rdbuc$DATE <-   str_replace(rdbuc$DATE,"T2", "-04-01")
rdbuc$DATE <-   str_replace(rdbuc$DATE,"T3", "-07-01")
rdbuc$DATE <-   str_replace(rdbuc$DATE,"T4", "-10-01")
rdbuc$DATE <- as.Date(paste(rdbuc$DATE,1,sep="-"), format = "%Y-%m-%d")
rdbuc <- filter(rdbuc, DATE >= as.Date('2018-10-01')) |>
  mutate(cum_rdb = ifelse(DATE == as.Date('2018-10-01'), 100, NA)) |>
  mutate(cum_rdb_uc = ifelse(DATE == as.Date('2018-10-01'), 100, NA)) |>
  arrange(DATE)
i <-2
while(i <=nrow(rdbuc)){
  rdbuc[i, "cum_rdb"] <- rdbuc[i-1, "cum_rdb"]*(1+(rdbuc[i, "rdb"]/100))
  i <- i+1
}
i <-2
while(i <=nrow(rdbuc)){
  rdbuc[i, "cum_rdb_uc"] <- rdbuc[i-1, "cum_rdb_uc"]*(1+(rdbuc[i, "rdb_uc"]/100))
  i <- i+1
}
rdbuc <- select(rdbuc, DATE, cum_rdb, cum_rdb_uc) |>
  mutate(group = ifelse(substr(DATE, 1, 4) == "2019", 1, NA)) |>
  mutate(group = as.numeric(group)) 
rdbuc_restrict <- rdbuc  |>
  group_by(group) |>
  summarise_at(vars(2:3), funs(mean(.))) |>
  filter(group !=is.na(group)) |>
  mutate(DATE = c('2021-04-01')) |>
  select(-group)
rdbuc_restrict$DATE <- as.Date(paste(rdbuc_restrict$DATE,1,sep="-"), format = "%Y-%m-%d")
rdbuc <- filter(rdbuc, DATE >= as.Date('2021-07-01')) |> select(-group)
rdbuc <- rbind(rdbuc, rdbuc_restrict) |> arrange(DATE)
rm(rdbuc_restrict)
decompo_rdb_cnt_restrict <- left_join(decompo_rdb_cnt_restrict, rdbuc, by = "DATE")
decompo_rdb_cnt_restrict <- mutate(decompo_rdb_cnt_restrict, cum_men = ifelse(DATE< as.Date('1976-01-01'), cum_men/cum_men[DATE=='1973-10-01']*100, cum_men)) |>
  mutate(cum_pop = ifelse(DATE < as.Date('1976-01-01'), cum_pop/cum_pop[DATE=='1973-10-01']*100, cum_pop)) |>
  mutate(cum_uc = ifelse(DATE < as.Date('1976-01-01'), cum_uc/cum_uc[DATE=='1973-10-01']*100, cum_uc)) |>
  mutate(cum_rdb = ifelse(DATE < as.Date('1976-01-01'),B6_rdb/B6_rdb[DATE=='1973-10-01']*100, cum_rdb)) |>
  mutate(cum_men = ifelse(DATE > as.Date('1976-01-01')  & DATE<as.Date('1982-01-01'), cum_men/cum_men[DATE=='1978-10-01']*100, cum_men)) |>
  mutate(cum_pop = ifelse(DATE > as.Date('1976-01-01') & DATE<as.Date('1982-01-01'), cum_pop/cum_pop[DATE=='1978-10-01']*100, cum_pop)) |>
  mutate(cum_uc = ifelse(DATE > as.Date('1976-01-01') & DATE<as.Date('1982-01-01'), cum_uc/cum_uc[DATE=='1978-10-01']*100, cum_uc)) |>
  mutate(cum_rdb = ifelse(DATE > as.Date('1976-01-01') & DATE<as.Date('1982-01-01'),B6_rdb/B6_rdb[DATE=='1978-10-01']*100, cum_rdb)) |>
  #select(-B6_rdb, -group) |>
  mutate(deflateur_conso_finale_men = ifelse(DATE > as.Date('1976-01-01') & DATE<as.Date('1982-01-01'),deflateur_conso_finale_men/deflateur_conso_finale_men[DATE=='1978-10-01']*100, deflateur_conso_finale_men)) |>
  mutate(deflateur_conso_finale_men = ifelse(DATE < as.Date('1976-01-01'),deflateur_conso_finale_men/deflateur_conso_finale_men[DATE=='1973-10-01']*100, deflateur_conso_finale_men)) |>
  mutate(deflateur_conso_finale_men = ifelse(DATE > as.Date('2019-01-01'),deflateur_conso_finale_men/deflateur_conso_finale_men[DATE=='2021-04-01']*100, deflateur_conso_finale_men)) |>
  mutate(cum_rdb = ifelse(DATE > as.Date('2021-01-01'),cum_rdb/cum_rdb[DATE=='2021-04-01']*100, cum_rdb)) |>
  mutate(cum_rdb = ifelse(DATE > as.Date('2021-01-01'),cum_rdb/deflateur_conso_finale_men*100, cum_rdb)) |>
  mutate(cum_rdb_uc = ifelse(DATE > as.Date('2021-01-01'),cum_rdb_uc/cum_rdb_uc[DATE=='2021-04-01']*100, cum_rdb_uc)) |>
  mutate(cum_rdb_uc = ifelse(DATE > as.Date('2021-01-01'),cum_rdb_uc/deflateur_conso_finale_men*100, cum_rdb_uc)) |>
  mutate(cum_rdb = ifelse(DATE < as.Date('2021-01-01'), cum_rdb/deflateur_conso_finale_men*100, cum_rdb)) |>
  mutate(cum_rdb_uc = ifelse(DATE < as.Date('2021-01-01'), (cum_rdb/(cum_uc*0.01)), cum_rdb_uc)) |>
  mutate(cum_rdb_men = ifelse(DATE < as.Date('2021-01-01'), (cum_rdb/(cum_men*0.01)), NA)) |>
  mutate(cum_rdb_pop = ifelse(DATE < as.Date('2021-01-01'), (cum_rdb/(cum_pop*0.01)), NA)) |>
  mutate(cum_nom_rdb = cum_rdb*deflateur_conso_finale_men/100)
plot_rdbuc73 <- ggplot(data=subset(decompo_rdb_cnt_restrict, DATE>= as.Date('1973-10-01') & DATE < as.Date('1976-01-01')), aes(x=DATE)) +
  geom_line(aes(y = cum_rdb, color = "PP of GDI"))   + 
  geom_point(aes(y = cum_rdb, color = "PP of GDI"))   + 
  geom_line(aes(y = cum_rdb_uc, color = "PP of GDI per CI"))   + 
  geom_point(aes(y = cum_rdb_uc, color = "PP of GDI per CI"))   + 
  geom_line(aes(y = cum_rdb_men, color = "PP of GDI per household"))   + 
  geom_point(aes(y = cum_rdb_men, color = "PP of GDI per household"))   + 
  geom_line(aes(y = cum_rdb_pop, color = "PP of GDI per capita"))   + 
  geom_point(aes(y = cum_rdb_pop, color = "PP of GDI per capita"))   + 
  labs( y = "Index (based 100)", x  = "Année") + 
  scale_color_manual(values = c("PP of GDI" = "red", "PP of GDI per CI" = "blue", "PP of GDI per household" = "brown", "PP of GDI per capita" = "purple")) +
  theme(legend.position="bottom", 
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 5), 
        plot.title = element_text(size=8), 
        axis.text.x = element_text(angle = 90))  
plot_rdbuc73
plot_rdbnom73 <- ggplot(data=subset(decompo_rdb_cnt_restrict, DATE>= as.Date('1973-10-01') & DATE < as.Date('1976-01-01')), aes(x=DATE)) +
  geom_line(aes(y = cum_nom_rdb, color = "RDB"))   + 
  geom_point(aes(y = cum_nom_rdb, color = "RDB"))   + 
  geom_line(aes(y = deflateur_conso_finale_men, color = "Deflator for household consumption expenditure"))   + 
  geom_point(aes(y = deflateur_conso_finale_men, color = "Deflator for household consumption expenditure"))   + 
  labs( y = "Index (based 100)", x  = "Year") + 
  scale_color_manual(values = c("RDB" = "red", "Deflator for household consumption expenditure" = "blue")) +
  theme(legend.position="bottom", 
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 5), 
        plot.title = element_text(size=8), 
        axis.text.x = element_text(angle = 90))  
plot_rdbnom73
plot_rdbuc81 <- ggplot(data=subset(decompo_rdb_cnt_restrict, DATE>= as.Date('1978-10-01') & DATE <= as.Date('1981-01-01')), aes(x=DATE)) +
  geom_line(aes(y = cum_rdb, color = "PP of GDI"))   + 
  geom_point(aes(y = cum_rdb, color = "PP of GDI"))   + 
  geom_line(aes(y = cum_rdb_uc, color = "PP of GDI per CI"))   + 
  geom_point(aes(y = cum_rdb_uc, color = "PP of GDI per CI"))   + 
  geom_line(aes(y = cum_rdb_men, color = "PP of GDI per household"))   + 
  geom_point(aes(y = cum_rdb_men, color = "PP of GDI per household"))   + 
  geom_line(aes(y = cum_rdb_pop, color = "PP of GDI per capita"))   + 
  geom_point(aes(y = cum_rdb_pop, color = "PP of GDI per capita"))   + 
  labs( y = "Index (based 100)", x  = "Year") + 
  scale_color_manual(values = c("PP of GDI" = "red", "PP of GDI per CI" = "blue", "PP of GDI per household" = "brown", "PP of GDI per capita" = "purple")) +
  theme(legend.position="bottom", 
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 5), 
        plot.title = element_text(size=8), 
        axis.text.x = element_text(angle = 90))  

plot_rdbuc81
plot_rdbnom81 <- ggplot(data=subset(decompo_rdb_cnt_restrict, DATE>= as.Date('1978-10-01') & DATE <= as.Date('1981-01-01')), aes(x=DATE)) +
  geom_line(aes(y = cum_nom_rdb, color = "GDI"))   + 
  geom_point(aes(y = cum_nom_rdb, color = "GDI"))   + 
  geom_line(aes(y = deflateur_conso_finale_men, color = "Deflator for household consumption expenditure"))   + 
  geom_point(aes(y = deflateur_conso_finale_men, color = "Deflator for household consumption expenditure"))   + 
  labs( y = "Index (based 100)", x  = "Year") + 
  scale_color_manual(values = c("GDI" = "red", "Deflator for household consumption expenditure" = "blue")) +
  theme(legend.position="bottom", 
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 5), 
        plot.title = element_text(size=8), 
        axis.text.x = element_text(angle = 90))  
plot_rdbnom81
plot_rdbuc22 <- ggplot(data=subset(decompo_rdb_cnt_restrict, DATE>= as.Date('2020-10-01')), aes(x=DATE)) +
  geom_line(aes(y = cum_rdb, color = "PP of GDI"))   + 
  geom_point(aes(y = cum_rdb, color = "PP of GDI"))   + 
  geom_line(aes(y = cum_rdb_uc, color = "PP of GDI per CI"))   + 
  geom_point(aes(y = cum_rdb_uc, color = "PP of GDI per CI"))   + 
  geom_line(aes(y = cum_rdb_men, color = "PP of GDI per household"))   + 
  geom_point(aes(y = cum_rdb_men, color = "PP of GDI per household"))   + 
  geom_line(aes(y = cum_rdb_pop, color = "PP of GDI per capita"))   + 
  geom_point(aes(y = cum_rdb_pop, color = "PP of GDI per capita"))   + 
  labs( y = "Index (based 100)", x  = "Year") + 
  scale_color_manual(values = c("PP of GDI" = "red", "PP of GDI per CI" = "blue", "PP of GDI per household" = "brown", "PP of GDI per capita" = "purple")) +
  theme(legend.position="bottom", 
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 5), 
        plot.title = element_text(size=8), 
        axis.text.x = element_text(angle = 90))  
plot_rdbuc22 
plot_rdbnom22 <- ggplot(data=subset(decompo_rdb_cnt_restrict, DATE>= as.Date('2021-04-01')), aes(x=DATE)) +
  geom_line(aes(y = cum_nom_rdb, color = "GDI"))   + 
  geom_point(aes(y = cum_nom_rdb, color = "GDI"))   + 
  geom_line(aes(y = deflateur_conso_finale_men, color = "Deflator for household consumption expenditure"))   + 
  geom_point(aes(y = deflateur_conso_finale_men, color = "Deflator for household consumption expenditure"))   + 
  labs( y = "Index (based 100)", x  = "Year") + 
  scale_color_manual(values = c("GDI" = "red", "Deflator for household consumption expenditure" = "blue")) +
  theme(legend.position="bottom", 
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 5), 
        plot.title = element_text(size=8), 
        axis.text.x = element_text(angle = 90))  
plot_rdbnom22


plot_rdbuc <- ggarrange(plot_rdbuc73, plot_rdbuc81 , plot_rdbuc22,
                        align='hv', labels=c("1974-1975 (VS 1973)", "1979-1981 (VS 1978)", "2021-2022 (VS 2019)"),
                        common.legend = T, font.label = list(size = 7, color = "black"))
plot_rdbuc

plot_rdbuc_title <- annotate_figure(plot_rdbuc, top = text_grob("Purchase Power of Gross Disposable Income (GDI)", 
                                                                color = "black", face = "bold", size = 11))
plot_rdbuc_title

plot_rdbucnom <- ggarrange(plot_rdbnom73, plot_rdbnom81 , plot_rdbnom22,
                           align='hv', labels=c("1974-1975 (VS 1973)", "1979-1981 (VS 1978)", "2021-2022 (VS 2019)"),
                           common.legend = T, font.label = list(size = 7, color = "black"))
plot_rdbucnom

plot_rdbnom_title <- annotate_figure(plot_rdbucnom, top = text_grob("Gross Disposable Income and Deflator for final consumption expenditure of households", 
                                                                    color = "black", face = "bold", size =7))
plot_rdbnom_title

pdf('graphs/IPC-IPCH/graph-imk/fig13_menage_rdb_73_79_21.pdf')
plot_rdbuc_title
dev.off()

pdf('graphs/IPC-IPCH/graph-imk/fig14_rdb_deflateur_conso_73_79_21.pdf')
plot_rdbnom_title
dev.off()
# write.xlsx(as.data.frame(decompo_rdb_cnt_restrict), file = "C:/Users/gasto/Documents/STAGE OFCE 2022/Data/stage_ofce/note_IMK/files/note_imk_figures.xlsx",
#            sheetName="fig13&14", append=TRUE)
###Meme graphs en francais
# plot_rdbuc73
# plot_rdbnom73 <- ggplot(data=subset(decompo_rdb_cnt_restrict, DATE>= as.Date('1973-10-01') & DATE < as.Date('1976-01-01')), aes(x=DATE)) +
#   geom_line(aes(y = cum_nom_rdb, color = "RDB"))   + 
#   geom_point(aes(y = cum_nom_rdb, color = "RDB"))   + 
#   geom_line(aes(y = deflateur_conso_finale_men, color = "Déflateur de la consommation des ménages"))   + 
#   geom_point(aes(y = deflateur_conso_finale_men, color = "Déflateur de la consommation des ménages"))   + 
#   labs( y = "Niveau (base 100)", x  = "Année") + 
#   scale_color_manual(values = c("RDB" = "red", "Déflateur de la consommation des ménages" = "blue")) +
#   theme(legend.position="bottom", 
#         legend.title = element_text(size = 7), 
#         legend.text = element_text(size = 5), 
#         plot.title = element_text(size=8), 
#         axis.text.x = element_text(angle = 90))  
# plot_rdbnom73
# plot_rdbuc81 <- ggplot(data=subset(decompo_rdb_cnt_restrict, DATE>= as.Date('1978-10-01') & DATE <= as.Date('1981-01-01')), aes(x=DATE)) +
#   geom_line(aes(y = cum_rdb, color = "PA du RDB"))   + 
#   geom_point(aes(y = cum_rdb, color = "PA du RDB"))   + 
#   geom_line(aes(y = cum_rdb_uc, color = "PA du RDB par UC"))   + 
#   geom_point(aes(y = cum_rdb_uc, color = "PA du RDB par UC"))   + 
#   geom_line(aes(y = cum_rdb_men, color = "PA du RDB par ménage"))   + 
#   geom_point(aes(y = cum_rdb_men, color = "PA du RDB par ménage"))   + 
#   geom_line(aes(y = cum_rdb_pop, color = "PA du RDB par personne"))   + 
#   geom_point(aes(y = cum_rdb_pop, color = "PA du RDB par personne"))   + 
#   labs( y = "Niveau (base 100)", x  = "Année") + 
#   scale_color_manual(values = c("PA du RDB" = "red", "PA du RDB par UC" = "blue", "PA du RDB par ménage" = "brown", "PA du RDB par personne" = "purple")) +
#   theme(legend.position="bottom", 
#         legend.title = element_text(size = 7), 
#         legend.text = element_text(size = 5), 
#         plot.title = element_text(size=8), 
#         axis.text.x = element_text(angle = 90))  
# 
# plot_rdbuc81
# plot_rdbnom81 <- ggplot(data=subset(decompo_rdb_cnt_restrict, DATE>= as.Date('1978-10-01') & DATE <= as.Date('1981-01-01')), aes(x=DATE)) +
#   geom_line(aes(y = cum_nom_rdb, color = "RDB"))   + 
#   geom_point(aes(y = cum_nom_rdb, color = "RDB"))   + 
#   geom_line(aes(y = deflateur_conso_finale_men, color = "Déflateur de la consommation des ménages"))   + 
#   geom_point(aes(y = deflateur_conso_finale_men, color = "Déflateur de la consommation des ménages"))   + 
#   labs( y = "Niveau (base 100)", x  = "Année") + 
#   scale_color_manual(values = c("RDB" = "red", "Déflateur de la consommation des ménages" = "blue")) +
#   theme(legend.position="bottom", 
#         legend.title = element_text(size = 7), 
#         legend.text = element_text(size = 5), 
#         plot.title = element_text(size=8), 
#         axis.text.x = element_text(angle = 90))  
# plot_rdbnom81
# plot_rdbuc22 <- ggplot(data=subset(decompo_rdb_cnt_restrict, DATE>= as.Date('2020-10-01')), aes(x=DATE)) +
#   geom_line(aes(y = cum_rdb, color = "PA du RDB"))   + 
#   geom_point(aes(y = cum_rdb, color = "PA du RDB"))   + 
#   geom_line(aes(y = cum_rdb_uc, color = "PA du RDB par UC"))   + 
#   geom_point(aes(y = cum_rdb_uc, color = "PA du RDB par UC"))   + 
#   geom_line(aes(y = cum_rdb_men, color = "PA du RDB par ménage"))   + 
#   geom_point(aes(y = cum_rdb_men, color = "PA du RDB par ménage"))   + 
#   geom_line(aes(y = cum_rdb_pop, color = "PA du RDB par personne"))   + 
#   geom_point(aes(y = cum_rdb_pop, color = "PA du RDB par personne"))   + 
#   labs( y = "Niveau (base 100)", x  = "Année") + 
#   scale_color_manual(values = c("PA du RDB" = "red", "PA du RDB par UC" = "blue", "PA du RDB par ménage" = "brown", "PA du RDB par personne" = "purple")) +
#   theme(legend.position="bottom", 
#         legend.title = element_text(size = 7), 
#         legend.text = element_text(size = 5), 
#         plot.title = element_text(size=8), 
#         axis.text.x = element_text(angle = 90))  
# plot_rdbuc22 
# plot_rdbnom22 <- ggplot(data=subset(decompo_rdb_cnt_restrict, DATE>= as.Date('2021-04-01')), aes(x=DATE)) +
#   geom_line(aes(y = cum_nom_rdb, color = "RDB"))   + 
#   geom_point(aes(y = cum_nom_rdb, color = "RDB"))   + 
#   geom_line(aes(y = deflateur_conso_finale_men, color = "Déflateur de la consommation des ménages"))   + 
#   geom_point(aes(y = deflateur_conso_finale_men, color = "Déflateur de la consommation des ménages"))   + 
#   labs( y = "Niveau (base 100)", x  = "Année") + 
#   scale_color_manual(values = c("RDB" = "red", "Déflateur de la consommation des ménages" = "blue")) +
#   theme(legend.position="bottom", 
#         legend.title = element_text(size = 7), 
#         legend.text = element_text(size = 5), 
#         plot.title = element_text(size=8), 
#         axis.text.x = element_text(angle = 90))  
# plot_rdbnom22
# 
# 
# plot_rdbuc <- ggarrange(plot_rdbuc73, plot_rdbuc81 , plot_rdbuc22,
#                         align='hv', labels=c("1974-1975 (VS 1973)", "1979-1981 (VS 1978)", "2021-2022 (VS 2019)"),
#                         common.legend = T, font.label = list(size = 7, color = "black"))
# plot_rdbuc
# 
# plot_rdbuc_title <- annotate_figure(plot_rdbuc, top = text_grob("Pouvoir d'achat du Revenu Disponible Brut", 
#                                                                 color = "black", face = "bold", size = 11))
# plot_rdbuc_title
# 
# plot_rdbucnom <- ggarrange(plot_rdbnom73, plot_rdbnom81 , plot_rdbnom22,
#                            align='hv', labels=c("1974-1975 (VS 1973)", "1979-1981 (VS 1978)", "2021-2022 (VS 2019)"),
#                            common.legend = T, font.label = list(size = 7, color = "black"))
# plot_rdbucnom
# 
# plot_rdbnom_title <- annotate_figure(plot_rdbucnom, top = text_grob("RDB et déflateur de la consommation des ménages", 
#                                                                     color = "black", face = "bold", size = 11))
# plot_rdbnom_title
# pdf('graphs/IPC-IPCH/graph-imk/fig13_menage_rdb_73_79_21.pdf')
# plot_rdbuc_title
# dev.off()
# 
# pdf('graphs/IPC-IPCH/graph-imk/fig14_rdb_deflateur_conso_73_79_21.pdf')
# plot_rdbnom_title
# dev.off()
###Taux de marges des entreprises décomposé
tx_marge <- "data_ofce/eq-cnt/decompo_marges_enf_t2_2022.xls"
cnt_tx_marge_enf <- read_excel(tx_marge, sheet = "Evolutions")  |>
  filter(!is.na(tx_marge))
cnt_tx_marge_enf$DATE <-   str_replace(cnt_tx_marge_enf$DATE,"T1", "-01-01")
cnt_tx_marge_enf$DATE <-   str_replace(cnt_tx_marge_enf$DATE,"T2", "-04-01")
cnt_tx_marge_enf$DATE <-   str_replace(cnt_tx_marge_enf$DATE,"T3", "-07-01")
cnt_tx_marge_enf$DATE <-   str_replace(cnt_tx_marge_enf$DATE,"T4", "-10-01")
cnt_tx_marge_enf$DATE <-as.Date(paste(cnt_tx_marge_enf$DATE,1,sep="-"), format = "%Y-%m-%d")
cnt_tx_marge_enf_restrict <- filter(cnt_tx_marge_enf, (DATE >= as.Date('1973-10-01') & DATE <= as.Date('1975-01-01')) | 
                                      (DATE >= as.Date('1978-10-01') & DATE <= as.Date('1981-01-01')) | 
                                      (DATE >= as.Date('2019-10-01'))) |>
  mutate(group = ifelse(DATE > as.Date('1973-10-01') & DATE <= as.Date('1975-01-01'), 1, 0)) |>
  mutate(group = ifelse(DATE > as.Date('1978-10-01') & DATE <= as.Date('1981-01-01'), 2, group)) |>
  mutate(group = ifelse(DATE > as.Date('2019-10-01'), 3, group)) |>
  group_by(group) |>
  summarise_at(vars(3:8), funs(sum(.))) |>
  filter(group !=0) |>
  mutate(group = ifelse(group == 1, "1975-T1", group)) |>
  mutate(group = ifelse(group == 2, "1981-T1", group)) |>
  mutate(group = ifelse(group == 3, "2022-T2", group))


cnt_tx_marge_enf_restrict <- rename(cnt_tx_marge_enf_restrict , "Global"= "var_trim_tx_marge", 
                                    "Productivity" = "prod", 
                                    "Real wages" = "sal_reel", 
                                    "Social contributions" = "cot_soc", 
                                    "Ratio VA prices/consumption prices" = "ratio_va_conso", 
                                    "Others" = "autres")
cnt_tx_marge_enf_restrict <- pivot_longer(cnt_tx_marge_enf_restrict ,  cols=c(2:7), names_to = "Composante", values_to = "value") 

cnt_tx_marge_enf <- rename(cnt_tx_marge_enf, "Global"= "var_trim_tx_marge", 
                           "Productivity" = "prod", 
                           "Real wages" = "sal_reel", 
                           "Social contributions" = "cot_soc", 
                           "Ratio VA prices/consumption prices" = "ratio_va_conso", 
                           "Others" = "autres")
cnt_tx_marge_enf <- pivot_longer(cnt_tx_marge_enf,  cols=c(3:8), names_to = "Composante", values_to = "value") |>
  filter(DATE > as.Date('1972-10-01'))

p_tx_marge <- ggplot(data = cnt_tx_marge_enf, aes(x = DATE)) 
plot_tx_marge <- p_tx_marge + geom_line(aes(y = tx_marge))  +
  labs(y = "Non-financial firm mark-up (in %)", x  = "Year") +
  annotate("rect", xmin = as.Date('1974-01-01'), xmax = as.Date('1975-01-01'), ymin = 30, ymax = 60, alpha = .5) + 
  annotate("rect", xmin = as.Date('1979-01-01'), xmax = as.Date('1981-01-01'), ymin = 30, ymax = 60, alpha = .5) + 
  annotate("rect", xmin = as.Date('2021-07-01'), xmax = as.Date('2022-07-01'), ymin = 30, ymax = 60, alpha = .5) 
plot_tx_marge

cnt_tx_marge_enf_restrict$Composante <- factor(cnt_tx_marge_enf_restrict$Composante, levels=c("Global", 'Productivity','Real wages','Social contributions','Ratio VA prices/consumption prices','Others'))

# write.xlsx(as.data.frame(cnt_tx_marge_enf_restrict), file = "C:/Users/gasto/Documents/STAGE OFCE 2022/Data/stage_ofce/note_IMK/files/note_imk_figures.xlsx",
#            sheetName="fig16", append=TRUE)

###Restriction du premier point et dernier point pour chaque crise


plot_trim_tx_marge73 <- ggplot(data=subset(cnt_tx_marge_enf_restrict, group =="1975-T1"), aes(x=group, y=value, fill=Composante)) +
  geom_bar(position=position_dodge(), stat='identity')   + 
  #geom_line(aes(y = tx_marge, color = "Taux de marge (échelle de droite)")) + 
  #geom_point(aes(y=tx_marge, color = "Taux de marge (échelle de droite)")) +
  labs(y = "Quarterly change (in %)", x  = "Year") + 
  scale_color_manual(values = c("Mark-up (right scale)" = "black")) +
  theme(legend.position="bottom", 
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 5), 
        plot.title = element_text(size=8), 
        axis.text.x = element_text(angle = 90))  
plot_trim_tx_marge73

plot_trim_tx_marge81 <- ggplot(data=subset(cnt_tx_marge_enf_restrict, group =="1981-T1"), aes(x=group, y=value, fill=Composante)) +
  geom_bar(position=position_dodge(), stat='identity')   + 
  #geom_line(aes(y = tx_marge, color = "Taux de marge (échelle de droite)")) + 
  #geom_point(aes(y=tx_marge, color = "Taux de marge (échelle de droite)")) +
  labs( y = "Quarterly change (in %)", x  = "Year") + 
  scale_color_manual(values = c("Mark-up (right scale)" = "black")) +
  theme(legend.position="bottom", 
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 5), 
        plot.title = element_text(size=8), 
        axis.text.x = element_text(angle = 90))  
plot_trim_tx_marge81
plot_trim_tx_marge22 <- ggplot(data=subset(cnt_tx_marge_enf_restrict, group =="2022-T2"), aes(x=group, y=value, fill=Composante)) +
  geom_bar(position=position_dodge(), stat='identity')   + 
  #geom_line(aes(y = tx_marge, color = "Taux de marge (échelle de droite)")) + 
  #geom_point(aes(y=tx_marge, color = "Taux de marge (échelle de droite)")) +
  labs( y = "Quarterly change (in %)", x  = "Year") + 
  scale_color_manual(values = c("Mark-up (right scale)" = "black")) +
  theme(legend.position="bottom", 
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 5), 
        plot.title = element_text(size=8), 
        axis.text.x = element_text(angle = 90))  
plot_trim_tx_marge22

plot_tx_marge <- ggarrange(plot_tx_marge, plot_trim_tx_marge73, plot_trim_tx_marge81 , plot_trim_tx_marge22,
                           align='hv', labels=c("Mark-up", "T1-1975 (VS T4-1973)", "T1-1981 (VS T4-1978)", "T2-2022 (VS T4-2019)"),
                           common.legend = T, font.label = list(size = 7, color = "black"))
plot_tx_marge

plot_tx_marge_title <- annotate_figure(plot_tx_marge , top = text_grob("Distribution of mark-up of non-financial firms", 
                                                                       color = "black", face = "bold", size = 11))
plot_tx_marge_title

pdf('graphs/IPC-IPCH/graph-imk/fig16_decompo_tx_marge_73_79_21.pdf')
plot_tx_marge_title
dev.off()


###Déficit public hors et avec charge d'interet 
bfm_apu <- read_excel(cap_bes_fi_rdm, sheet = "BFM_APU", skip = 3) 
bfm_apu$DATE <-   str_replace(bfm_apu$DATE,"T1", "-01-01")
bfm_apu$DATE <-   str_replace(bfm_apu$DATE,"T2", "-04-01")
bfm_apu$DATE <-   str_replace(bfm_apu$DATE,"T3", "-07-01")
bfm_apu$DATE <-   str_replace(bfm_apu$DATE,"T4", "-10-01")
bfm_apu$DATE <-as.Date(paste(bfm_apu$DATE,1,sep="-"), format = "%Y-%m-%d")
bfm_apu <- rename(bfm_apu, apu_vab = B1, "apu_sal" = D11, 
                  apu_ebe = B2, apu_rdb = B6, apu_epargne_brut = B8, 
                  apu_bescapfi = B9NF, apu_rdba = B7) |> 
  mutate(apu_rev_prim = rowSums(across(c("D211":"D45_R"))) - D41_V - D45_V) |>
  mutate(apu_rev_sec = rowSums(across(c("D51_R":"D75_R"))) - rowSums(across(c("D51_V":"D76")))) |>
  mutate(apu_conso = P31+P32) |>
  mutate(apu_inv = rowSums(across(c("D92_V":"NP"))) - rowSums(across(c("D91":"D99_R")))) |>
  relocate(apu_vab, apu_sal, apu_ebe, apu_rev_prim, apu_rev_sec, apu_rdb, apu_conso, apu_epargne_brut, apu_inv, apu_bescapfi, .after = "DATE") |>
  mutate(apu_recette_prim = rowSums(across(c("D211":"D45_R")))) |>
  mutate(apu_depense_prim = -(D41_V + D45_V)) |>
  mutate(apu_recette_sec = rowSums(across(c("D51_R":"D75_R")))) |>
  mutate(apu_depense_sec = -rowSums(across(c("D51_V":"D76")))) |>
  mutate(apu_recette_inv = rowSums(across(c("D91":"D99_R")))) |>
  mutate(apu_depense_inv = -rowSums(across(c("D92_V":"NP")))) |>
  mutate(apu_depense_conso_ind = -P31) |>
  mutate(apu_depense_conso_col = -P32) |>
  relocate(apu_recette_prim, apu_recette_sec, apu_recette_inv, apu_depense_prim, apu_depense_sec, apu_depense_conso_ind, apu_depense_conso_col, apu_depense_inv, .after = "apu_bescapfi") |>
  mutate(apu_ebe_sans_int = apu_ebe) |>
  mutate(apu_depense_primaire_sans_int = apu_depense_prim + D41_V) |>
  mutate(apu_recette_primaire_sans_int = apu_recette_prim - D41_R) |>
  mutate(apu_recette_sec_sans_int = apu_recette_sec) |>
  mutate(apu_depense_sec_sans_int = apu_depense_sec) |>
  mutate(apu_rdb_sans_int = apu_ebe + apu_recette_primaire_sans_int + apu_recette_sec_sans_int + apu_depense_primaire_sans_int + apu_depense_sec_sans_int) |> 
  mutate(apu_depense_conso_ind_sans_int = -P31) |>
  mutate(apu_depense_conso_col_sans_int = -P32) |>
  mutate(apu_epargne_brut_sans_int = apu_rdb_sans_int + apu_depense_conso_ind_sans_int + apu_depense_conso_col_sans_int) |>
  mutate(apu_recette_inv_sans_int = apu_recette_inv) |>
  mutate(apu_depense_inv_sans_int = apu_depense_inv) |>
  mutate(apu_bescapfi_sans_int = apu_epargne_brut_sans_int + apu_recette_inv_sans_int + apu_depense_inv_sans_int) |>
  mutate(apu_charge_interet = -D41_V + D41_R) |>
  relocate(apu_ebe_sans_int, apu_recette_primaire_sans_int, apu_recette_sec_sans_int, apu_depense_primaire_sans_int, apu_depense_sec_sans_int,
           apu_rdb_sans_int, apu_depense_conso_ind_sans_int, apu_depense_conso_col_sans_int, apu_epargne_brut_sans_int, apu_recette_inv_sans_int, 
           apu_depense_inv_sans_int,apu_bescapfi_sans_int, apu_charge_interet, .after = "DATE")

bfm_apu <- left_join(bfm_apu, pib_valeur, by = "DATE")  |>
  mutate(tx_deficit_primaire = apu_bescapfi_sans_int/pib_trim*100) |>
  mutate(tx_deficit_avec_interet = apu_bescapfi/pib_trim*100)

apu_sans_int <- select(bfm_apu, 1:14, tx_deficit_primaire, tx_deficit_avec_interet, pib_trim)

###PLOT Deficit avec et sans interet
p_def_prim <- ggplot(apu_sans_int, aes(x = DATE)) 
plot_def_prim <- p_def_prim + 
  geom_line(aes(y = tx_deficit_primaire, color = "Déficit primaire")) + 
  geom_point(aes(y = tx_deficit_primaire, color = "Déficit primaire")) + 
  geom_line(aes(y = tx_deficit_avec_interet, color = "Déficit avec charge d'intérêt")) + 
  geom_point(aes(y = tx_deficit_avec_interet, color = "Déficit avec charge d'intérêt")) + 
  labs(title = "Taux de déficit primaire et avec charge d'intérêt", y = "Taux de déficit (en %)", x = "Année") + 
  scale_color_manual(values = c("Déficit primaire" = "blue", "Déficit avec charge d'intérêt" = "red")) + 
  annotate("rect", xmin = as.Date('1974-01-01'), xmax = as.Date('1975-01-01'), ymin = -15, ymax = 10, alpha = .5) + 
  annotate("rect", xmin = as.Date('1979-01-01'), xmax = as.Date('1981-01-01'), ymin = -15, ymax = 10, alpha = .5) + 
  annotate("rect", xmin = as.Date('2021-07-01'), xmax = as.Date('2022-07-01'), ymin = -15, ymax = 10, alpha = .5) +
  theme(legend.position="bottom", legend.text = element_text(size = 9))  + 
  scale_x_date(limits = as.Date(c("1970-01-01", NA))) 
plot_def_prim


pdf('graphs/CNT/chocs_energie/def_apu_avec_sans_interet.pdf')
plot_def_prim
dev.off()

###Plot decomposition deficit par recette et dépense
apu_sans_int <- relocate(apu_sans_int, apu_ebe_sans_int, apu_recette_primaire_sans_int,apu_recette_sec_sans_int, 
                         apu_depense_sec_sans_int, apu_depense_conso_ind_sans_int, apu_depense_conso_col_sans_int, 
                         apu_recette_inv_sans_int, apu_depense_inv_sans_int, apu_charge_interet,.after = "DATE") 
apu_sans_int  <- mutate_at(apu_sans_int, vars(2:10), funs(. /pib_trim*100)) 

mean73 <- filter(apu_sans_int, DATE >=as.Date('1973-01-01') & DATE <= as.Date('1973-10-01')) |>
  mutate(apu_ebe_sans_int= mean(apu_ebe_sans_int)) |>
  mutate(apu_recette_primaire_sans_int = mean(apu_recette_primaire_sans_int)) |>
  mutate(apu_recette_sec_sans_int = mean(apu_recette_sec_sans_int)) |>
  mutate(apu_depense_sec_sans_int= mean(apu_depense_sec_sans_int)) |>
  mutate(apu_depense_conso_ind_sans_int  = mean(apu_depense_conso_ind_sans_int)) |>
  mutate(apu_depense_conso_col_sans_int  = mean(apu_depense_conso_col_sans_int)) |>
  mutate(apu_recette_inv_sans_int  = mean(apu_recette_inv_sans_int)) |>
  mutate(apu_depense_inv_sans_int  = mean(apu_depense_inv_sans_int)) |>
  mutate(tx_deficit_primaire = mean(tx_deficit_primaire)) |>
  filter(DATE== max(DATE)) |>
  select(1:9, tx_deficit_primaire)

mean78 <- filter(apu_sans_int, DATE >=as.Date('1978-01-01') & DATE <= as.Date('1978-10-01')) |>
  mutate(apu_ebe_sans_int= mean(apu_ebe_sans_int)) |>
  mutate(apu_recette_primaire_sans_int = mean(apu_recette_primaire_sans_int)) |>
  mutate(apu_recette_sec_sans_int = mean(apu_recette_sec_sans_int)) |>
  mutate(apu_depense_sec_sans_int= mean(apu_depense_sec_sans_int)) |>
  mutate(apu_depense_conso_ind_sans_int  = mean(apu_depense_conso_ind_sans_int)) |>
  mutate(apu_depense_conso_col_sans_int  = mean(apu_depense_conso_col_sans_int)) |>
  mutate(apu_recette_inv_sans_int  = mean(apu_recette_inv_sans_int)) |>
  mutate(apu_depense_inv_sans_int  = mean(apu_depense_inv_sans_int)) |>
  mutate(tx_deficit_primaire = mean(tx_deficit_primaire)) |>
  filter(DATE== max(DATE)) |>
  select(1:9, tx_deficit_primaire)

mean19 <- filter(apu_sans_int, DATE >=as.Date('2019-01-01') & DATE <= as.Date('2019-10-01')) |>
  mutate(apu_ebe_sans_int= mean(apu_ebe_sans_int)) |>
  mutate(apu_recette_primaire_sans_int = mean(apu_recette_primaire_sans_int)) |>
  mutate(apu_recette_sec_sans_int = mean(apu_recette_sec_sans_int)) |>
  mutate(apu_depense_sec_sans_int= mean(apu_depense_sec_sans_int)) |>
  mutate(apu_depense_conso_ind_sans_int  = mean(apu_depense_conso_ind_sans_int)) |>
  mutate(apu_depense_conso_col_sans_int  = mean(apu_depense_conso_col_sans_int)) |>
  mutate(apu_recette_inv_sans_int  = mean(apu_recette_inv_sans_int)) |>
  mutate(apu_depense_inv_sans_int  = mean(apu_depense_inv_sans_int)) |>
  mutate(tx_deficit_primaire = mean(tx_deficit_primaire)) |>
  filter(DATE== max(DATE)) |>
  select(1:9, tx_deficit_primaire)

# mean73  <- rename(mean73, "EBE" = apu_ebe_sans_int,
#                                                "Recettes indirectes (production & conso)" = apu_recette_primaire_sans_int,
#                                                "Recettes directes (revenus, cotisations)" = apu_recette_sec_sans_int,
#                                                #"Charges d'intérêt" = apu_charge_interet,
#                                                "Transferts sociaux espèces" = apu_depense_sec_sans_int,
#                                                "Conso individualisable" =  apu_depense_conso_ind_sans_int,
#                                                "Conso collective" = apu_depense_conso_col_sans_int,
#                                                "Recettes du capital" = apu_recette_inv_sans_int,
#                                                "Subvention prod. et FBCF" = apu_depense_inv_sans_int)
# mean78  <- rename(mean78, "EBE" = apu_ebe_sans_int,
#                                     "Recettes indirectes (production & conso)" = apu_recette_primaire_sans_int,
#                                     "Recettes directes (revenus, cotisations)" = apu_recette_sec_sans_int,
#                                     #"Charges d'intérêt" = apu_charge_interet,
#                                     "Transferts sociaux espèces" = apu_depense_sec_sans_int,
#                                     "Conso individualisable" =  apu_depense_conso_ind_sans_int,
#                                     "Conso collective" = apu_depense_conso_col_sans_int,
#                                     "Recettes du capital" = apu_recette_inv_sans_int,
#                                     "Subvention prod. et FBCF" = apu_depense_inv_sans_int)
# mean19  <- rename(mean19, "EBE" = apu_ebe_sans_int,
#                                     "Recettes indirectes (production & conso)" = apu_recette_primaire_sans_int,
#                                     "Recettes directes (revenus, cotisations)" = apu_recette_sec_sans_int,
#                                     #"Charges d'intérêt" = apu_charge_interet,
#                                     "Transferts sociaux espèces" = apu_depense_sec_sans_int,
#                                     "Conso individualisable" =  apu_depense_conso_ind_sans_int,
#                                     "Conso collective" = apu_depense_conso_col_sans_int,
#                                     "Recettes du capital" = apu_recette_inv_sans_int,
#                                     "Subvention prod. et FBCF" = apu_depense_inv_sans_int)

mean19  <- rename(mean19, "GOP" = apu_ebe_sans_int, 
                  "Indirect taxation" = apu_recette_primaire_sans_int, 
                  "Direct taxation" = apu_recette_sec_sans_int, 
                  #"Interest expenses" = apu_charge_interet, 
                  "Cash-social transfers" = apu_depense_sec_sans_int, 
                  "Individual consump. exp." =  apu_depense_conso_ind_sans_int, 
                  "Collective consump. exp." = apu_depense_conso_col_sans_int, 
                  "Capital taxation" = apu_recette_inv_sans_int, 
                  "Production subsidy and GFCF" = apu_depense_inv_sans_int)
mean78  <- rename(mean78, "GOP" = apu_ebe_sans_int, 
                  "Indirect taxation" = apu_recette_primaire_sans_int, 
                  "Direct taxation" = apu_recette_sec_sans_int, 
                  #"Interest expenses" = apu_charge_interet, 
                  "Cash-social transfers" = apu_depense_sec_sans_int, 
                  "Individual consump. exp." =  apu_depense_conso_ind_sans_int, 
                  "Collective consump. exp." = apu_depense_conso_col_sans_int, 
                  "Capital taxation" = apu_recette_inv_sans_int, 
                  "Production subsidy and GFCF" = apu_depense_inv_sans_int)
mean73  <- rename(mean73, "GOP" = apu_ebe_sans_int, 
                  "Indirect taxation" = apu_recette_primaire_sans_int, 
                  "Direct taxation" = apu_recette_sec_sans_int, 
                  #"Interest expenses" = apu_charge_interet, 
                  "Cash-social transfers" = apu_depense_sec_sans_int, 
                  "Individual consump. exp." =  apu_depense_conso_ind_sans_int, 
                  "Collective consump. exp." = apu_depense_conso_col_sans_int, 
                  "Capital taxation" = apu_recette_inv_sans_int, 
                  "Production subsidy and GFCF" = apu_depense_inv_sans_int)
mean19 <- pivot_longer(mean19, cols=c(2:9), names_to = "Composante", values_to = "value") 


mean73 <- pivot_longer(mean73, cols=c(2:9), names_to = "Composante", values_to = "value") 


mean78 <- pivot_longer(mean78, cols=c(2:9), names_to = "Composante", values_to = "value") 
mean_apu <- rbind(mean73, mean78, mean19)
rm(mean73, mean78, mean19)


# apu_sans_int  <- rename(apu_sans_int, "EBE" = apu_ebe_sans_int, 
#                        "Recettes indirectes (production & conso)" = apu_recette_primaire_sans_int, 
#                        "Recettes directes (revenus, cotisations)" = apu_recette_sec_sans_int, 
#                        "Charges d'intérêt" = apu_charge_interet, 
#                        "Transferts sociaux espèces" = apu_depense_sec_sans_int, 
#                        "Conso individualisable" =  apu_depense_conso_ind_sans_int, 
#                        "Conso collective" = apu_depense_conso_col_sans_int, 
#                        "Recettes du capital" = apu_recette_inv_sans_int, 
#                        "Subvention prod. et FBCF" = apu_depense_inv_sans_int)

apu_sans_int  <- rename(apu_sans_int, "GOP" = apu_ebe_sans_int, 
                        "Indirect taxation" = apu_recette_primaire_sans_int, 
                        "Direct taxation" = apu_recette_sec_sans_int, 
                        #"Interest expenses" = apu_charge_interet, 
                        "Cash-social transfers" = apu_depense_sec_sans_int, 
                        "Individual consump. exp." =  apu_depense_conso_ind_sans_int, 
                        "Collective consump. exp." = apu_depense_conso_col_sans_int, 
                        "Capital taxation" = apu_recette_inv_sans_int, 
                        "Production subsidy and GFCF" = apu_depense_inv_sans_int)


apu_sans_int <- pivot_longer(apu_sans_int, cols=c(2:9), names_to = "Composante", values_to = "value") 
apu_sans_int <- select(apu_sans_int, DATE, tx_deficit_primaire, Composante, value)


# apu_sans_int$Composante <- factor(apu_sans_int$Composante, levels=c('EBE', 'Recettes indirectes (production & conso)', 'Recettes du capital',"Recettes directes (revenus, cotisations)", 
#                                                                     "Transferts sociaux espèces", "Conso individualisable", "Conso collective", "Subvention prod. et FBCF"
#                                                                     #, "Charges d'intérêt"
#                                                                     ))
apu_sans_int$Composante <- factor(apu_sans_int$Composante, levels=c("GOP","Indirect taxation","Capital taxation","Direct taxation",
                                                                    "Cash-social transfers", 
                                                                    "Individual consump. exp.", 
                                                                    "Collective consump. exp.",
                                                                    "Production subsidy and GFCF"))
#, "Charges d'intérêt"


######PLOT DECOMPOSITION APU PAR TYPE DE DEPENSE ET DE RECETTE
plot_apu_compo <- ggplot(data=apu_sans_int, aes(x=DATE, y=value, fill=Composante)) +
  geom_bar(position='stack', stat='identity') +
  geom_line(aes(y = tx_deficit_primaire, color = "Déf. primaire")) + 
  geom_point(aes(y = tx_deficit_primaire, color = "Déf. primaire")) + 
  scale_color_manual(values = c("Déf. primaire" = "blue")) + 
  labs(title = "Décomposition du Compte trimestriel des APU", y = "Pourcentage du PIB", x = "Année") + 
  annotate("rect", xmin = as.Date('1974-01-01'), xmax = as.Date('1975-01-01'), ymin = -60, ymax = 60, alpha = .5) + 
  annotate("rect", xmin = as.Date('1979-01-01'), xmax = as.Date('1981-01-01'), ymin = -60, ymax = 60, alpha = .5) + 
  annotate("rect", xmin = as.Date('2021-07-01'), xmax = as.Date('2022-07-01'), ymin = -60, ymax = 60, alpha = .5) +
  theme(legend.position="bottom", legend.text = element_text(size = 4), legend.title = element_text(size = 4))  + 
  scale_x_date(limits = as.Date(c("1970-01-01", NA)))
#scale_fill_manual(values = c("#4E84C4","#D16103")) 
plot_apu_compo

pdf('graphs/CNT/chocs_energie/decompo_apu_sans_interet7022.pdf')
plot_apu_compo
dev.off()

###Restriction de ce graphique sur les intervalles 
apu_restrict <- filter(apu_sans_int, DATE != as.Date('2019-10-01') & DATE != as.Date('1973-10-01') & DATE != as.Date('1978-10-01')) 

apu_restrict <- filter(apu_restrict, (DATE >= as.Date('1973-10-01') & DATE <= as.Date('1975-01-01')) | 
                         (DATE >= as.Date('1978-10-01') & DATE <= as.Date('1981-03-01')) | 
                         (DATE == as.Date('2019-10-01')) | 
                         (DATE >= as.Date('2021-07-01'))) 
apu_restrict <- rbind(apu_restrict, mean_apu) |>
  arrange(DATE)
apu_restrict <- mutate(apu_restrict, DATE = as.character(DATE))
apu_restrict <- mutate(apu_restrict, DATE=ifelse(DATE == "2019-10-01", "2021-04-01", DATE)) 
apu_restrict$DATE <- as.Date(paste(apu_restrict$DATE,1,sep="-"), format = "%Y-%m-%d")

apu_restrict <- mutate(apu_restrict, group = ifelse(DATE== as.Date('1975-01-01'), 1, NA)) |>
  mutate(group = ifelse(DATE == as.Date('1981-01-01') & DATE <= as.Date('1981-01-01'), 2, group)) |>
  mutate(group = ifelse(DATE == as.Date('2022-04-01'), 3, group)) |>
  mutate(group = ifelse(DATE == as.Date('2021-04-01') | DATE == as.Date('1978-10-01') | DATE == as.Date('1973-10-01'), 4, group)) |>
  filter(group !=is.na(group)) |>
  spread(key = "Composante",
         value = "value") |>
  relocate(group, .after = "DATE") |>
  arrange(DATE)|>
  summarise_at(vars(3:11), funs((.)-lag(., 1))) |>
  mutate(DATE = c(NA, '1975-T1', NA, '1981-T1', NA, '2022-T2')) |>
  filter(DATE !=is.na(DATE)) |>
  pivot_longer(cols=c(2:9), names_to = "Composante", values_to = "value") 

plot_apu_restrict_73  <- ggplot(data=subset(apu_restrict, DATE == '1975-T1'), aes(x=DATE, y=value, fill=Composante)) +
  geom_bar(position='stack', stat='identity') + 
  geom_line(aes(y = tx_deficit_primaire, color = "Primary deficit")) + 
  geom_point(aes(y = tx_deficit_primaire, color = "Primary deficit")) + 
  scale_color_manual(values = c("Primary deficit" = "blue")) + 
  labs( y = "GDP percentage", x = "Year") + 
  theme(legend.position="bottom", legend.text = element_text(size = 4), legend.title = element_text(size = 4))  

plot_apu_restrict_81 <- ggplot(data=subset(apu_restrict, DATE == '1981-T1'), aes(x=DATE, y=value, fill=Composante)) +
  geom_bar(position='stack', stat='identity') + 
  geom_line(aes(y = tx_deficit_primaire, color = "Primary deficit")) + 
  geom_point(aes(y = tx_deficit_primaire, color = "Primary deficit")) + 
  scale_color_manual(values = c("Primary deficit" = "blue")) + 
  labs( y = "GDP percentage", x = "Year") + 
  theme(legend.position="bottom", legend.text = element_text(size = 4), legend.title = element_text(size = 4))  

plot_apu_restrict_22  <- ggplot(data=subset(apu_restrict, DATE == '2022-T2'), aes(x=DATE, y=value, fill=Composante)) +
  geom_bar(position='stack', stat='identity') + 
  geom_line(aes(y = tx_deficit_primaire, color = "Primary deficit")) + 
  geom_point(aes(y = tx_deficit_primaire, color = "Primary deficit")) + 
  scale_color_manual(values = c("Primary deficit" = "blue")) + 
  labs( y = "GDP percentage", x = "Year") + 
  theme(legend.position="bottom", legend.text = element_text(size = 4), legend.title = element_text(size = 4))  

plot_apu_restrict_73 
plot_apu_restrict_81
plot_apu_restrict_22

plot_apu_decompo <- ggarrange(plot_apu_restrict_73, plot_apu_restrict_81 , plot_apu_restrict_22,
                              align='hv', labels=c("1974-1975 (VS moy. 1973)", "1979-1981 (VS moy. 1978)", "2021-2022 (VS moy. 2019)"),
                              common.legend = T, font.label = list(size = 7, color = "black"))
plot_apu_decompo

plot_apu_decompo_title <- annotate_figure(plot_apu_decompo, top = text_grob("Decomposition of Public Administration Quarterly National Accounts", 
                                                                            color = "black", face = "bold", size = 11))
plot_apu_decompo_title

pdf('graphs/IPC-IPCH/graph-imk/fig17_apu_decompo_73_79_21_variation.pdf')
plot_apu_decompo_title
dev.off()
# write.xlsx(as.data.frame(apu_restrict), file = "C:/Users/gasto/Documents/STAGE OFCE 2022/Data/stage_ofce/note_IMK/files/note_imk_figures.xlsx",
#            sheetName="fig17", append=TRUE)
###Chargement BFM par agent économique
bfm_all <- read_excel(cap_bes_fi_rdm, sheet = "BFM_all") 
bfm_all$DATE <-   str_replace(bfm_all$DATE,"T1", "-01-01")
bfm_all$DATE <-   str_replace(bfm_all$DATE,"T2", "-04-01")
bfm_all$DATE <-   str_replace(bfm_all$DATE,"T3", "-07-01")
bfm_all$DATE <-   str_replace(bfm_all$DATE,"T4", "-10-01")
bfm_all$DATE <-as.Date(paste(bfm_all$DATE,1,sep="-"), format = "%Y-%m-%d")
bfm_all[, "Entreprises"] <- bfm_all[, 3] + bfm_all[, 4] 
bfm_all <- relocate(bfm_all, Entreprises, .before = "Sociétés NF") |>
  select(-'Sociétés NF', -'Sociétés F')
bfm_all <- left_join(bfm_all, pib_valeur, by = "DATE") 
bfm_all <- arrange(bfm_all, DATE) |>
  mutate(var_bfm_men = (Ménages-lag(Ménages, 1))*100) |>
  mutate(var_bfm_ent = (Entreprises - lag(Entreprises, 1))*100) |>
  mutate(var_bfm_isblsm = (ISBLSM - lag(ISBLSM, 1))*100) |>
  mutate(var_bfm_apu = (APU - lag(APU, 1))*100) 
bfm_all <- mutate_at(bfm_all, vars(2:6), funs(. /pib_trim*100))
bfm_ratio_energ <- select(cnt_val_exp_imp_graph, DATE, ratio_energ_pib, ratio_petrole_pib) 
bfm_all <- left_join(bfm_all, bfm_ratio_energ, by = "DATE")

mean73 <- filter(bfm_all, DATE >=as.Date('1973-01-01') & DATE <= as.Date('1973-10-01')) |>
  mutate(Ménages = mean(Ménages)) |>
  mutate(Entreprises = mean(Entreprises)) |>
  mutate(ISBLSM = mean(ISBLSM)) |>
  mutate(APU = mean(APU)) |>
  mutate(RDM = mean(RDM)) |>
  mutate(ratio_energ_pib = mean(ratio_energ_pib)) |>
  filter(DATE == max(DATE)) |>
  select(1:6, 13)

mean78 <- filter(bfm_all, DATE >=as.Date('1978-01-01') & DATE <= as.Date('1978-10-01')) |>
  mutate(Ménages = mean(Ménages)) |>
  mutate(Entreprises= mean(Entreprises)) |>
  mutate(ISBLSM = mean(ISBLSM)) |>
  mutate(APU = mean(APU)) |>
  mutate(ratio_energ_pib = mean(ratio_energ_pib)) |>
  mutate(RDM = mean(RDM)) |>
  filter(DATE == max(DATE)) |>
  select(1:6, 13)

mean19 <- filter(bfm_all, DATE >=as.Date('2019-01-01') & DATE <= as.Date('2019-10-01')) |>
  mutate(Ménages = mean(Ménages)) |>
  mutate(Entreprises = mean(Entreprises)) |>
  mutate(ISBLSM = mean(ISBLSM)) |>
  mutate(APU = mean(APU)) |>
  mutate(ratio_energ_pib = mean(ratio_energ_pib)) |>
  mutate(RDM = mean(RDM)) |>
  filter(DATE == max(DATE)) |>
  select(1:6, 13)

mean19 <- pivot_longer(mean19, cols=c(2:5), names_to = "agent", values_to = "value") |>
  mutate(RDM_oppose = -RDM) |>
  select(-RDM)

mean73 <- pivot_longer(mean73, cols=c(2:5), names_to = "agent", values_to = "value") |>
  mutate(RDM_oppose = -RDM) |>
  select(-RDM)

mean78 <- pivot_longer(mean78, cols=c(2:5), names_to = "agent", values_to = "value") |>
  mutate(RDM_oppose = -RDM) |>
  select(-RDM)
mean_bfm <- rbind(mean73, mean78, mean19)
rm(mean73, mean78, mean19)
bfm_all <- pivot_longer(bfm_all, cols=c(2:5), names_to = "agent", values_to = "value") |>
  mutate(RDM_oppose = -RDM) 


bfm_all$agent <- factor(bfm_all$agent, levels=c('APU','ISBLSM', 'Entreprises',  "Ménages"))


###Plot décomposition capacité-besoin financement
plot_agent_capfi <- ggplot(data=bfm_all, aes(x=DATE, y=value, fill=agent)) +
  geom_bar(position='stack', stat='identity') +
  geom_line(aes(x = DATE, y = RDM_oppose, color = "Capacité-besoin de financement")) + 
  geom_point(aes(x = DATE, y = RDM_oppose, color = "Capacité-besoin de financement")) + 
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph_petrole$DATE[99]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph_petrole$DATE[121]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph_petrole$DATE[237]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(cnt_val_exp_imp_graph_petrole$DATE[290]), linetype=4, color = "black") +
  labs(y = "Points de pourcentage du PIB", x = "Année") 
#scale_fill_manual(values = c("#4E84C4","#D16103")) 

plot_agent_capfi7022 <-  plot_agent_capfi +  
  geom_line(aes(y = ratio_energ_pib, color = "Déficit commercial du secteur énergie")) + 
  geom_point(aes(y = ratio_energ_pib, color = "Déficit commercial du secteur énergie")) + 
  # geom_line(aes(y = ratio_petrole_pib, color = "Déficit commercial du secteur pétrole")) + 
  # geom_point(aes(y = ratio_petrole_pib, color = "Déficit commercial du secteur pétrole")) + 
  scale_x_date(limits = as.Date(c("1970-01-01", NA))) + 
  labs(title = "Décomposition de la capacité-besoin de financement en pourcentage du PIB nominal") + 
  scale_color_manual(values = c("Déficit commercial du secteur énergie" = "blue", "Capacité-besoin de financement" = "black" 
                                  #,"Déficit commercial du secteur pétrole" = "purple"
  ))
plot_agent_capfi7022 

bfm_all_restrict <- filter(bfm_all, DATE != as.Date('2019-10-01') & DATE != as.Date('1973-10-01') & DATE != as.Date('1978-10-01')) |>
  select(DATE, ratio_energ_pib, agent, value, RDM_oppose )
bfm_all_restrict <- filter(bfm_all_restrict, (DATE >= as.Date('1973-10-01') & DATE <= as.Date('1975-01-01')) | 
                             (DATE >= as.Date('1978-10-01') & DATE <= as.Date('1981-03-01')) | 
                             (DATE == as.Date('2019-10-01')) | 
                             (DATE >= as.Date('2021-07-01'))) 
bfm_all_restrict <- rbind(bfm_all_restrict, mean_bfm) |>
  arrange(DATE)
bfm_all_restrict <- mutate(bfm_all_restrict, DATE = as.character(DATE))
bfm_all_restrict <- mutate(bfm_all_restrict, DATE=ifelse(DATE == "2019-10-01", "2021-04-01", DATE)) 
bfm_all_restrict$DATE <- as.Date(paste(bfm_all_restrict$DATE,1,sep="-"), format = "%Y-%m-%d")

plot_agent_capfi_bis <- ggplot(data=bfm_all_restrict, aes(x=DATE, y=value, fill=agent)) +
  geom_bar(position='stack', stat='identity') +
  geom_line(aes(x = DATE, y = RDM_oppose, color = "Capacity-borrowing requirement")) + 
  geom_point(aes(x = DATE, y = RDM_oppose, color = "Capacity-borrowing requirement")) + 
  geom_vline(xintercept = as.numeric(bfm_all_restrict$DATE[1]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(bfm_all_restrict$DATE[25]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(bfm_all_restrict$DATE[65]), linetype=4, color = "black") +
  labs(y = "Quarterly GDP percentage points", x = "Year") 
#scale_fill_manual(values = c("#4E84C4","#D16103")) 

plot_agent_capfi73 <- plot_agent_capfi_bis +   
  geom_line(aes(y = ratio_energ_pib, color = "Energy sector trade-deficit")) + 
  geom_point(aes(y = ratio_energ_pib, color = "Energy sector trade-deficit")) +
  scale_x_date(limits = as.Date(c("1973-08-01", "1975-03-01"))) + 
  scale_color_manual(values = c("Energy sector trade-deficit" = "blue", "Capacity-borrowing requirement" = "black" )) +
  theme(legend.title = element_text(size = 7), legend.text = element_text(size = 7))
plot_agent_capfi73

plot_agent_capfi79 <- plot_agent_capfi_bis +   
  geom_line(aes(y = ratio_energ_pib, color = "Energy sector trade-deficit")) + 
  geom_point(aes(y = ratio_energ_pib, color = "Energy sector trade-deficit")) +
  scale_x_date(limits = as.Date(c("1978-08-01", "1981-03-01"))) + 
  scale_color_manual(values = c("Energy sector trade-deficit" = "blue", "Capacity-borrowing requirement" = "black" )) +
  theme(legend.title = element_text(size = 7), legend.text = element_text(size = 7))
plot_agent_capfi79

plot_agent_capfi21 <- plot_agent_capfi_bis +   
  geom_line(aes(y = ratio_energ_pib, color = "Energy sector trade-deficit")) + 
  geom_point(aes(y = ratio_energ_pib, color = "Energy sector trade-deficit")) +
  scale_x_date(limits = as.Date(c("2021-02-01", "2022-06-01"))) + 
  scale_color_manual(values = c("Energy sector trade-deficit" = "blue", "Capacity-borrowing requirement" = "black" )) + 
  theme(legend.title = element_text(size = 7), legend.text = element_text(size = 7))
plot_agent_capfi21

###Répartition capacité-besoin financement 
pdf('graphs/CNT/chocs_energie/repartition_agent_bescapfi.pdf')
plot_agent_capfi7022
dev.off()

plot_agent_capfi_decompo <- ggarrange(plot_agent_capfi73, plot_agent_capfi79, plot_agent_capfi21,
                                      align='hv', labels=c("1974-1975 (VS average 1973)", "1979-1981 (VS average 1978)", "2021-2022 (VS average 2019)"),
                                      common.legend = T, font.label = list(size = 7, color = "black"))
plot_agent_capfi_decompo 

# write.xlsx(as.data.frame(bfm_all_restrict), file = "C:/Users/gasto/Documents/STAGE OFCE 2022/Data/stage_ofce/note_IMK/files/note_imk_figures.xlsx",
#            sheetName="fig12", append=TRUE)
pdf('graphs/IPC-IPCH/graph-imk/fig12_decompo_agent_capfi_73_79_21.pdf')
plot_agent_capfi_decompo 
dev.off()


###Ménages décomposition de la BFM 
bfm_menages <- read_excel(cap_bes_fi_rdm, sheet = "BFM_menages", skip = 4) |>
  mutate(men_ebe = B2_EI + B2_MHEI)  |>
  mutate(men_rev_primaire = rowSums(across(c("D11":"D45_R"))) - D41_V - D45_V) |>
  mutate(men_rev_secondaire = rowSums(across(c("D621":"D75_R"))) - rowSums(across(c("D51":"D75_V")))) |>
  mutate(men_investissement = (rowSums(across(c("D91":"NP"))) - D92 - D99_R)) |>
  mutate(transfert_nat = D631 + D632) |>
  relocate(men_ebe, men_rev_primaire, men_rev_secondaire, P31, men_investissement, D11, B6, B8, B9NF, transfert_nat, B7, .before = B2_EI) |>
  rename("men_salb" = "D11", "men_rdb" = "B6", "men_ep_brut" = "B8", "men_capbesfi" = "B9NF", "men_rdba" = "B7", "men_conso" = "P31") |>
  mutate(tx_conso_rdb = men_conso/men_rdb*100) |>
  mutate(tx_inv_rdb = men_investissement/men_rdb*100) |>
  mutate(tx_capfi_rdb = men_capbesfi/men_rdb*100) 
bfm_menages$DATE <-   str_replace(bfm_menages$DATE,"T1", "-01-01")
bfm_menages$DATE <-   str_replace(bfm_menages$DATE,"T2", "-04-01")
bfm_menages$DATE <-   str_replace(bfm_menages$DATE,"T3", "-07-01")
bfm_menages$DATE <-   str_replace(bfm_menages$DATE,"T4", "-10-01")
bfm_menages$DATE <-as.Date(paste(bfm_menages$DATE,1,sep="-"), format = "%Y-%m-%d")

bfm_menages2 <- left_join(bfm_menages, deflateur_conso_men, by = "DATE") 
bfm_menages2 <-  mutate(bfm_menages2, pa_rdb = men_rdb/deflateur_conso_finale_men*100) |>
  arrange(DATE) |>
  mutate(var_trim_tx_conso = ((tx_conso_rdb/lag(tx_conso_rdb, 1))-1)*100) |>
  mutate(var_trim_tx_inv = ((tx_inv_rdb/lag(tx_inv_rdb, 1))-1)*100) 


mean_pa_rdb73<- colMeans(bfm_menages2[97:100,"pa_rdb"])
mean_tx_inv_73 <- colMeans(bfm_menages2[97:100,"tx_inv_rdb"])
mean_tx_cons_73 <- colMeans(bfm_menages2[97:100,"tx_conso_rdb"])
mean_tx_capfi73 <- colMeans(bfm_menages2[97:100,"tx_capfi_rdb"])
mean_pa_rdb73 <- as.numeric(mean_pa_rdb73)
mean_tx_inv_73 <- as.numeric(mean_tx_inv_73)
mean_tx_cons_73 <- as.numeric(mean_tx_cons_73)
mean_tx_capfi73 <- as.numeric(mean_tx_capfi73)
mean_pa_rdb78<- colMeans(bfm_menages2[117:120,"pa_rdb"])
mean_tx_inv_78 <- colMeans(bfm_menages2[117:120,"tx_inv_rdb"])
mean_tx_cons_78 <- colMeans(bfm_menages2[117:120,"tx_conso_rdb"])
mean_tx_capfi78 <- colMeans(bfm_menages2[117:120,"tx_capfi_rdb"])
mean_pa_rdb78 <- as.numeric(mean_pa_rdb78)
mean_tx_inv_78 <- as.numeric(mean_tx_inv_78)
mean_tx_cons_78 <- as.numeric(mean_tx_cons_78)
mean_tx_capfi78 <- as.numeric(mean_tx_capfi78)
mean_pa_rdb19<- colMeans(bfm_menages2[281:284,"pa_rdb"])
mean_tx_inv_19 <- colMeans(bfm_menages2[281:284,"tx_inv_rdb"])
mean_tx_cons_19 <- colMeans(bfm_menages2[281:284,"tx_conso_rdb"])
mean_tx_capfi19 <- colMeans(bfm_menages2[281:284,"tx_capfi_rdb"])
mean_pa_rdb19 <- as.numeric(mean_pa_rdb19)
mean_tx_inv_19 <- as.numeric(mean_tx_inv_19)
mean_tx_cons_19 <- as.numeric(mean_tx_cons_19)
mean_tx_capfi19 <- as.numeric(mean_tx_capfi19)

bfm_menages74 <- filter(bfm_menages2, DATE>=as.Date('1974-01-01') & DATE <=as.Date('1975-01-01')) |>
  select(DATE, pa_rdb, tx_inv_rdb, tx_conso_rdb, tx_capfi_rdb) 
bfm_menages74 <- cbind(bfm_menages74, mean_pa_rdb73) |>
  mutate(pa_rdb = pa_rdb/mean_pa_rdb73*100) |>
  select(-mean_pa_rdb73)
mean73 <- c('1973-10-01', 100,mean_tx_inv_73, mean_tx_cons_73, mean_tx_capfi73)
mean73 <- as.data.frame(t(mean73)) |>
  rename(DATE = V1, pa_rdb = V2, tx_inv_rdb = V3, tx_conso_rdb = V4, tx_capfi_rdb = V5) |>
  mutate(DATE = as.Date(DATE)) |>
  mutate_if((is.character), as.numeric)
bfm_menages74 <- rbind(bfm_menages74, mean73) |>
  arrange(DATE) |>
  pivot_longer(cols=c(3:5), names_to = "Composante", values_to = "value") 
bfm_menages74$Composante <-   str_replace(bfm_menages74$Composante,"tx_conso_rdb", "Taux de consommation")
bfm_menages74$Composante <-   str_replace(bfm_menages74$Composante,"tx_inv_rdb", "Taux d'investissement")
bfm_menages74$Composante <-   str_replace(bfm_menages74$Composante,"tx_capfi_rdb", "Taux de capacité de financement")
bfm_menages74$DATE <-as.Date(paste(bfm_menages74$DATE,1,sep="-"), format = "%Y-%m-%d")
bfm_menages74$Composante <- factor(bfm_menages74$Composante, levels=c('Taux de capacité de financement',"Taux d'investissement", 'Taux de consommation'))


bfm_menages79 <- filter(bfm_menages2, DATE>=as.Date('1979-01-01') & DATE <=as.Date('1981-01-01')) |>
  select(DATE, pa_rdb, tx_inv_rdb, tx_conso_rdb, tx_capfi_rdb) 
bfm_menages79 <- cbind(bfm_menages79, mean_pa_rdb78) |>
  mutate(pa_rdb = pa_rdb/mean_pa_rdb78*100) |>
  select(-mean_pa_rdb78)
mean78 <- c('1978-10-01', 100,mean_tx_inv_78, mean_tx_cons_78, mean_tx_capfi78)
mean78 <- as.data.frame(t(mean78)) |>
  rename(DATE = V1, pa_rdb = V2, tx_inv_rdb = V3, tx_conso_rdb = V4, tx_capfi_rdb = V5) |>
  mutate(DATE = as.Date(DATE)) |>
  mutate_if((is.character), as.numeric)
bfm_menages79 <- rbind(bfm_menages79, mean78) |>
  arrange(DATE) |>
  pivot_longer(cols=c(3:5), names_to = "Composante", values_to = "value") 
bfm_menages79$Composante <-   str_replace(bfm_menages79$Composante,"tx_conso_rdb", "Taux de consommation")
bfm_menages79$Composante <-   str_replace(bfm_menages79$Composante,"tx_inv_rdb", "Taux d'investissement")
bfm_menages79$Composante <-   str_replace(bfm_menages79$Composante,"tx_capfi_rdb", "Taux de capacité de financement")
bfm_menages79$DATE <-as.Date(paste(bfm_menages79$DATE,1,sep="-"), format = "%Y-%m-%d")
bfm_menages79$Composante <- factor(bfm_menages79$Composante, levels=c('Taux de capacité de financement',"Taux d'investissement", 'Taux de consommation'))


bfm_menages21 <- filter(bfm_menages2, DATE>=as.Date('2021-07-01')) |>
  select(DATE, pa_rdb, tx_inv_rdb, tx_conso_rdb, tx_capfi_rdb) 
bfm_menages21 <- cbind(bfm_menages21, mean_pa_rdb19) |>
  mutate(pa_rdb = pa_rdb/mean_pa_rdb19*100) |>
  select(-mean_pa_rdb19)
mean19<- c('2021-04-01', 100, mean_tx_inv_19, mean_tx_cons_19, mean_tx_capfi19)
mean19 <- as.data.frame(t(mean19)) |>
  rename(DATE = V1, pa_rdb = V2, tx_inv_rdb = V3, tx_conso_rdb = V4, tx_capfi_rdb = V5) |>
  mutate(DATE = as.Date(DATE)) |>
  mutate_if((is.character), as.numeric)
bfm_menages21 <- rbind(bfm_menages21, mean19) |>
  arrange(DATE) |>
  pivot_longer(cols=c(3:5), names_to = "Composante", values_to = "value") 
bfm_menages21$Composante <-   str_replace(bfm_menages21$Composante,"tx_conso_rdb", "Taux de consommation")
bfm_menages21$Composante <-   str_replace(bfm_menages21$Composante,"tx_inv_rdb", "Taux d'investissement")
bfm_menages21$Composante <-   str_replace(bfm_menages21$Composante,"tx_capfi_rdb", "Taux de capacité de financement")
bfm_menages21$DATE <-as.Date(paste(bfm_menages21$DATE,1,sep="-"), format = "%Y-%m-%d")
bfm_menages21$Composante <- factor(bfm_menages21$Composante, levels=c('Taux de capacité de financement',"Taux d'investissement", 'Taux de consommation'))



plot_decompo_bfm_menages7375 <- ggplot(data=bfm_menages74, aes(x=DATE, y=value, fill=Composante)) +
  geom_bar(position='stack', stat='identity') +
  geom_line(aes(y= pa_rdb, color = "Pouvoir d'achat du RDB"), size = 1.02) + 
  geom_point(aes(y= pa_rdb, color = "Pouvoir d'achat du RDB"), size = 1.02) + 
  geom_vline(xintercept = as.numeric(bfm_menages74$DATE[1]), linetype=4, color = "black") +
  labs(y = "Points de PIB trimestriel", x = "Année") +
  scale_color_manual(values = c("Pouvoir d'achat du RDB" = "purple")) +
  theme(legend.text = element_text(size = 5))+
  scale_x_date(limits = as.Date(c("1973-08-01", "1975-03-01"))) 

plot_decompo_bfm_menages7981 <- ggplot(data=bfm_menages79, aes(x=DATE, y=value, fill=Composante)) +
  geom_bar(position='stack', stat='identity') +
  geom_line(aes(y= pa_rdb, color = "Pouvoir d'achat du RDB"), size = 1.02) + 
  geom_point(aes(y= pa_rdb, color = "Pouvoir d'achat du RDB"), size = 1.02) + 
  geom_vline(xintercept = as.numeric(bfm_menages79$DATE[1]), linetype=4, color = "black") +
  labs(y = "Points de PIB trimestriel", x = "Année") +
  scale_color_manual(values = c("Pouvoir d'achat du RDB" = "purple")) +
  theme(legend.text = element_text(size = 5))+
  scale_x_date(limits = as.Date(c("1978-08-01", "1981-03-01"))) 

plot_decompo_bfm_menages2122 <- ggplot(data=bfm_menages21, aes(x=DATE, y=value, fill=Composante)) +
  geom_bar(position='stack', stat='identity') +
  geom_line(aes(y= pa_rdb, color = "Pouvoir d'achat du RDB"), size = 1.02) + 
  geom_point(aes(y= pa_rdb, color = "Pouvoir d'achat du RDB"), size = 1.02) + 
  geom_vline(xintercept = as.numeric(bfm_menages21$DATE[1]), linetype=4, color = "black") +
  labs(y = "Points de PIB trimestriel", x = "Année") +
  scale_color_manual(values = c("Pouvoir d'achat du RDB" = "purple")) +
  theme(legend.text = element_text(size = 5))+
  scale_x_date(limits = as.Date(c("2021-02-01", "2022-06-01"))) 

plot_decompo_bfm_menages7375
plot_decompo_bfm_menages7981
plot_decompo_bfm_menages2122
plot_decompo_bfm_menages <- ggarrange(plot_decompo_bfm_menages7375, plot_decompo_bfm_menages7981, plot_decompo_bfm_menages2122,
                                      align='hv', labels=c("1974 (VS 1973)", "1979-1981 (VS 1978)", "2021-2022 (VS 2019)"),
                                      common.legend = T, font.label = list(size = 9, color = "black"))

plot_decompo_bfm_menages_title <- annotate_figure(plot_decompo_bfm_menages, top = text_grob("Décomposition du compte trimestriel des ménages", 
                                                                                            color = "black", face = "bold", size = 11))
plot_decompo_bfm_menages_title

pdf('graphs/CNT/chocs_energie/decompo_cnt_menages_73_79_21.pdf')
plot_decompo_bfm_menages_title
dev.off()


###Taux d'épargne 
bfm4 <- rbind(bfm_menages74, bfm_menages79, bfm_menages21)  |>
  pivot_wider(names_from = Composante, values_from = value) |>
  rename('tx_cons' = "Taux de consommation") |>
  mutate(tx_ep = 100-tx_cons) |>
  select(DATE, tx_ep)
bfm4$DATE2 <- bfm4$DATE
bfm4$DATE2 <-  str_replace(bfm4$DATE2,"-01-01","-T1")
bfm4$DATE2 <-  str_replace(bfm4$DATE2,"-04-01","-T2")
bfm4$DATE2 <-  str_replace(bfm4$DATE2,"-07-01","-T3")
bfm4$DATE2 <-  str_replace(bfm4$DATE2,"-10-01","-T4")
bfm4$DATE2 <-  str_replace(bfm4$DATE2,"1973-T4","1973")
bfm4$DATE2 <-  str_replace(bfm4$DATE2,"1978-T4","1978")
bfm4$DATE2 <-  str_replace(bfm4$DATE2,"2021-T2","2019")
bfm4$DATE2 <- as.factor(bfm4$DATE2)
plot_tx_epargne73 <- ggplot(subset(bfm4, DATE <= as.Date('1975-01-01')), aes(x = DATE2, y = tx_ep, group = 1)) + geom_point(stat='summary', fun.y=sum) +
  stat_summary(fun.y=sum, geom="line") + labs(y = "Households saving rate", x = "Quarter") + 
  theme(legend.text = element_text(size = 8), axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = (bfm4$DATE2[1]), linetype=1, color = "blue") +
  geom_vline(xintercept = (bfm4$DATE2[8]), linetype=1, color = "blue") +
  geom_vline(xintercept = (bfm4$DATE2[17]), linetype=1, color = "blue") 
plot_tx_epargne73  

plot_tx_epargne79 <- ggplot(subset(bfm4, DATE > as.Date('1975-01-01') & DATE <as.Date('1985-01-01')), aes(x = DATE2, y = tx_ep, group = 1)) + geom_point(stat='summary', fun.y=sum) +
  stat_summary(fun.y=sum, geom="line") + labs(y = "Households saving rate", x = "Quarter") + 
  theme(legend.text = element_text(size = 8), axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = (bfm4$DATE2[7]), linetype=1, color = "blue") 
plot_tx_epargne79 

plot_tx_epargne21 <- ggplot(subset(bfm4, DATE > as.Date('2019-01-01')), aes(x = DATE2, y = tx_ep, group = 1)) + geom_point(stat='summary', fun.y=sum) +
  stat_summary(fun.y=sum, geom="line") + labs(y = "Households saving rate", x = "Quarter") + 
  theme(legend.text = element_text(size = 8), axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = (bfm4$DATE2[17]), linetype=1, color = "blue") 
plot_tx_epargne21 

plot_tx_ep <- ggplot(subset(bfm_menages, DATE > as.Date('1973-01-01')), aes(x= DATE, y = (100-tx_conso_rdb))) +
  geom_line() +
  labs(y = "Household saving rate (percentage)", x = "Year") +
  annotate("rect", xmin = as.Date('1974-01-01'), xmax = as.Date('1975-01-01'), ymin = 10, ymax = 30, alpha = .5) + 
  annotate("rect", xmin = as.Date('1979-01-01'), xmax = as.Date('1981-01-01'), ymin = 10, ymax = 30, alpha = .5) + 
  annotate("rect", xmin = as.Date('2021-07-01'), xmax = as.Date('2022-07-01'), ymin = 10, ymax = 30, alpha = .5) 
plot_decompo_bfm_txep <- ggarrange(plot_tx_ep, plot_tx_epargne73, plot_tx_epargne79,plot_tx_epargne21,
                                   align='hv', labels=c("Global", "1974 (VS 1973)", "1979-1981 (VS 1978)", "2021-2022 (VS 2019)"),
                                   common.legend = T, font.label = list(size = 9, color = "black"))

plot_decompo_bfm_txep_title <- annotate_figure(plot_decompo_bfm_txep, top = text_grob("Household saving rate", 
                                                                                      color = "black", face = "bold", size = 11))
plot_decompo_bfm_txep_title

pdf('graphs/IPC-IPCH/graph-imk/fig15_tx_epargne_menages_73_79_21.pdf')
plot_decompo_bfm_txep_title
dev.off()

# write.xlsx(as.data.frame(bfm4), file = "C:/Users/gasto/Documents/STAGE OFCE 2022/Data/stage_ofce/note_IMK/files/note_imk_figures.xlsx",
#            sheetName="fig15", append=TRUE)
###Var trimestrielle
bfm_menages3 <- bfm_menages2 
bfm_menages3 <-  select(bfm_menages3, DATE, var_trim_tx_conso, var_trim_tx_inv) 
bfm_menages3 <- filter(bfm_menages3, (DATE > as.Date('1973-10-01') & DATE <= as.Date('1975-01-01')) | 
                         (DATE > as.Date('1978-10-01') & DATE <= as.Date('1981-03-01')) | 
                         (DATE >= as.Date('2021-07-01')))
bfm_menages3$DATE2 <- bfm_menages3$DATE
bfm_menages3$DATE2 <-  str_replace(bfm_menages3$DATE2,"-01-01","T1")
bfm_menages3$DATE2 <-  str_replace(bfm_menages3$DATE2,"-04-01","T2")
bfm_menages3$DATE2 <-  str_replace(bfm_menages3$DATE2,"-07-01","T3")
bfm_menages3$DATE2 <-  str_replace(bfm_menages3$DATE2,"-10-01","T4")
bfm_menages3 <- rename(bfm_menages3, "Consommation" = "var_trim_tx_conso", "Investissement" = "var_trim_tx_inv" )
bfm_menages3 <- pivot_longer(bfm_menages3, cols=c(2:3), names_to = "Composante", values_to = "value") 

###En variation trimestrielle de % de taux 
plot_var_decompo_bfm_menages_all <- ggplot(data=bfm_menages3, aes(x=DATE2, y=value, fill=Composante)) +
  geom_bar(position=position_dodge(), stat='identity') +
  geom_vline(xintercept = as.numeric(bfm_menages3$DATE2[1]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(bfm_menages3$DATE2[11]), linetype=4, color = "black") +
  geom_vline(xintercept = as.numeric(bfm_menages3$DATE2[29]), linetype=4, color = "black") +
  labs(y = "Points de pourcentage", x = "Année") +
  #scale_color_manual(values = c("Pouvoir d'achat du RDB" = "purple")) +
  theme(legend.text = element_text(size = 8), axis.text.x = element_text(angle = 90)) 
#scale_x_date(limits = as.Date(c("2021-02-01", "2022-06-01"))) 
plot_var_decompo_bfm_menages_all