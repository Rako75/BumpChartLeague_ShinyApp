# ⚽ Classement Bump Chart — Championnats de Football

Application R Shiny interactive pour visualiser l'évolution du classement des championnats de football européens au fil des saisons.

[![Shiny App](https://img.shields.io/badge/Shiny-Live%20App-blue?style=flat&logo=r)](https://rakostats.shinyapps.io/Evolution_classement_par_ligue/)

## 🎯 Fonctionnalités

- **5 championnats majeurs** : Premier League, La Liga, Serie A, Bundesliga, Ligue 1
- **Données historiques** : Saisons de 1992 à 2024
- **Personnalisation** : Sélection jusqu'à 10 clubs à mettre en avant
- **Visualisation dynamique** : Graphiques bump chart
- **Logos des clubs** : Affichage automatique des logos (si disponibles)
- **Thèmes** : Mode sombre et clair
- **Export** : Téléchargement en haute résolution (PNG, 400 DPI)

## 📊 Aperçu

L'application permet de suivre la progression des équipes tout au long d'une saison, avec :
- Évolution du classement par journée
- Nombre de points pour chaque équipe
- Mise en évidence des clubs sélectionnés

## 🚀 Installation

### Prérequis

```r
install.packages(c(
  "shiny",
  "worldfootballR",
  "dplyr",
  "ggplot2",
  "ggtext",
  "ggimage",
  "shinythemes",
  "shinyWidgets"
))
```

### Lancement local

```r
# Cloner le repository
git clone https://github.com/votre-username/nom-du-repo.git
cd nom-du-repo

# Lancer l'application
shiny::runApp()
```

## 📁 Structure du projet

```
.
├── app.R                          # Application principale
├── README.md
├── LICENSE
└── .gitignore
```

## 🎨 Utilisation

1. **Sélectionnez un championnat** parmi les 5 disponibles
2. **Choisissez une saison** (1992-2024)
3. **Définissez le nombre de clubs** à mettre en avant (1-10)
4. **Sélectionnez les clubs** dans les menus déroulants
5. **Cliquez sur "Générer le graphique"**
6. **Téléchargez** votre visualisation

## 📦 Dossier des logos

Pour afficher les logos des clubs :
1. Créez un dossier `Logos clubs de football/`
2. Ajoutez les logos au format PNG, JPG ou SVG
3. Nommez les fichiers avec le nom du club (ex: `arsenal.png`)

> **Note** : Les logos ne sont pas inclus dans ce repository pour des raisons de droits d'auteur.

## 🔧 Technologies

- **R Shiny** : Framework web interactif
- **worldfootballR** : API Transfermarkt pour les données
- **ggplot2** : Visualisation de données
- **ggimage** : Intégration des logos
- **shinythemes** : Interface moderne

## 📝 Sources de données

Les données proviennent de [Transfermarkt](https://www.transfermarkt.com/) via le package `worldfootballR`.



## 📄 Licence

MIT License - Voir le fichier [LICENSE](LICENSE) pour plus de détails.

## ⚠️ Disclaimer

Cette application est à but éducatif et analytique. Les logos et données appartiennent à leurs propriétaires respectifs.
