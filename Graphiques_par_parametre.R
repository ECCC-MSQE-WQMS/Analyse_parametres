# ---
# title: "Graphiques par paramètres"
# author: "Martin Jean & Annie Picard"
# date: 2022-01-17 
# output:
#   html_notebook: default
# html_document:
#   df_print: paged
# pdf_document: default
# ---
#   
#   ## Objectif du script
#   
#   L'objectif du présent script est de créer un tableau de même qu'une série de graphiques illustrant la répartition des paramètres échantillonnés.
# 
# - Tableau synthèse de la répartition des paramètres par échantillon
# - Série de graphiques illustrant la répartition des paramètres par échantillon
# 
# ## Sources de données
# 
# les données récentes sont compilées dans un fichier "donnees/Sorties_calculateur/Tableau_parametres_long.csv", créé à partir du script "01-Lecture_donnees.Rmd".
# 
#
# Option to avoid scientific notation, personally I prefer to have scientific notation disabled when looking at numbers
options(scipen = 999)

# Load the required packages
library(tidyverse)
library(ggforce)
library(gridExtra)
library(gt)
library(writexl)

# ```


# Importation des données

tableau_donnees <- read.csv2("resultats/Tableau_parametres_long.csv", fileEncoding = "UTF-8")



## Construction des graphiques - fréquences annuelles


tableau_donnees_parametre <- tableau_donnees %>%
  group_by(Station, Annee, Parametre) %>%
  summarise(nSamples = n_distinct(Date)) %>%
  mutate(Annee = as.factor(Annee))

# Créer liste des paramètres
tableau_donnees_parametre <- arrange(tableau_donnees_parametre, tableau_donnees_parametre$Parametre)
liste_parametres <- unique(tableau_donnees_partenaire$Parametre)
  
  
  liste_graphiques <- list()

  # Boucle par parametre pour créer et ajouter le graphique au fichier PDF
  for (leParametre in liste_parametres) {
    le_graphique <- tableau_donnees_parametre %>%
      filter(Parametre == leParametre) %>%
      ggplot(aes(x = nSamples)) +
      geom_histogram() +
      xlim(0, 20) +
      ggtitle(leParametre) +
      xlab("Fréquence") +
      ylab("n") +
      # labs(fill = "Year") +
      theme_bw() +
      theme(
        axis.text.x = element_text(colour = "grey20", size = 8, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9, face = "bold"),
        
      )
    liste_graphiques[[leParametre]] <- le_graphique

    print(le_graphique)
  }
  graphiques_combines <- marrangeGrob(grobs = liste_graphiques, nrow = 2, ncol = 2, top=quote("Fréquences annuelles d'échantillonnage par paramètre - Qc (2017-2019)"))
  ggsave(paste("figures/Repartition_Parametres_Frequence", ".pdf", sep = ""), graphiques_combines)

  
  # Contruction des graphiques - fréquences saisonnières
  
  tableau_saisons_parametres <- tableau_donnees %>%
    group_by(Station, Annee, Saison, Parametre) %>%
    summarise(nSamples = n_distinct(Date)) %>%
    mutate(Annee = as.factor(Annee))
  
  # Créer liste des paramètres
  tableau_saisons_parametres <- arrange(tableau_saisons_parametres, tableau_saisons_parametres$Parametre)

  
  liste_graphiques_saisons <- list()
  
  # Boucle par parametre pour créer et ajouter le graphique au fichier PDF
  for (leParametre in liste_parametres) {
    le_graphique <- tableau_saisons_parametres %>%
      filter(Parametre == leParametre) %>%
      ggplot(aes(x = Saison)) +
      geom_bar() +
      #xlim(0, 20) +
      ggtitle(leParametre) +
      xlab("Fréquence") +
      ylab("n") +
      # labs(fill = "Year") +
      theme_bw() +
      theme(
        axis.text.x = element_text(colour = "grey20", size = 8, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9, face = "bold"),
        
      )
    liste_graphiques_saisons[[leParametre]] <- le_graphique
    
    print(le_graphique)
  }
  graphiques_combines <- marrangeGrob(grobs = liste_graphiques_saisons, nrow = 2, ncol = 2, top=quote("Fréquences saisonnières d'échantillonnage par paramètre - Qc (2017-2019)"))
  ggsave(paste("figures/Repartition_Parametres_Saisons", ".pdf", sep = ""), graphiques_combines)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Construction du tableau
  
  
  tableau_saisons_parametres <- tableau_donnees %>%
    group_by(Station, Parametre, Saison) %>%
    summarise(nSamples = n_distinct(Station)) %>%
    pivot_wider(names_from = Saison, values_from = nSamples) %>% 
    select(Parametre, Station, Spring, Summer, Fall, Winter) %>% 
    arrange(Parametre, Station)
  
  
  # Créer liste des paramètres
  liste_parametres <- unique(tableau_saisons_parametres$Parametre)
  
  
#tableau_saisons_parametres <- as.data.frame(tableau_saisons_parametres)
#tableau_saisons_parametres <- arrange(tableau_saisons_parametres, Station) # Tri par station
#tableau_saisons_parametres_large <- as.data.frame(tableau_saisons_parametres) %>% 
#  group_by(Station, Annee) %>% 
#  summarise(NEchantillons = n_distinct(Parametre)) %>% 
#  pivot_wider(names_from = Annee, values_from = NEchantillons, values_fill = 0) #%>% 
#
#tableau_saisons_parametres_large <- as.data.frame(tableau_saisons_parametres_large)
## Exportation d'une version Excel du tableau de résultats
  
  write_xlsx(tableau_saisons_parametres, 
             "resultats/tableaux/Tableau_Parametres_saisonnier.xlsx")


