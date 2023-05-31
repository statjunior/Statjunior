

list_idbank_pole_emploi = 
  get_idbank_list("DEMANDES-EMPLOIS-NATIONALES") %>% 
  filter(REF_AREA_label_fr == "France hors Mayotte" | REF_AREA_label_fr == "France métropolitaine") 

 
poleemploi = list_idbank_pole_emploi %>% pull(idbank)

chom_pol_emploi =get_insee_idbank(poleemploi) %>% 
  split_title() 

chom_pol_emploi <- filter(chom_pol_emploi, TITLE_FR6 != "Série arrêtée")

total_abc <- filter(chom_pol_emploi, TITLE_FR2 == "Catégorie A" |
                      TITLE_FR2 == "Catégorie B" |
                      TITLE_FR2 == "Catégorie C") |>
  filter(TITLE_FR4 == "Ensemble") |>
  filter()
plot_total_abc<-ggplot(data = total_abc,
                                aes(x = DATE)) +
  geom_line(aes(y = OBS_VALUE, color = "Total A, B et C"), size = 1.2) + 
  #geom_line(aes(y = smb_tertiaire, color = "SMB Tertiaire"), size = 1) + 
  labs(title = "Evolution de la productivité du travail et du pouvoir d'achat du Salaire Moyen par Tête", 
       subtitle = "Lecture : Au 4e trimestre 2022, le pouvoir d'achat du salaire moyen par tête recule de 3,4 % (et la productivité par tête de 4 %) par rapport au 4e trimestre 2019. \nChamp : Secteurs marchands non agricoles \nSource : Insee (Comptes nationaux trimestriels, dernier point : T4-2022) \nCalculs et graphiques : @statjunior", 
       y = "Base 100 au 1er trimestre 1990", x = "Date") +
  scale_color_manual(values = c("Total A, B et C" = "purple"))+
  theme_grey()+
  theme(legend.position="bottom",
        legend.title = element_text(size=9), 
        legend.text = element_text(size=12),
        plot.title = element_text(color = "#333333", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "#333333", face = "italic"))
plot_total_abc