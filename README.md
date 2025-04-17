# Data Exploration and Modeling App

## Contexte du projet
Ce projet a été réalisé en binôme . Il consiste en la création et le déploiement d'une application web R Shiny, permettant :

- de charger et prétraiter tout type de jeu de données,
- de réaliser des analyses exploratoires,
- de former et comparer des modèles de classification supervisée.

Lien vers l'application : [BaseApp - Shiny App](https://omar-namous.shinyapps.io/BaseApp/)

---

## Cas d'étude présenté

Nous avons appliqué l'application sur un jeu de données d'attrition des employés d'IBM.

### 1. Préparation des données
- **Dimensions** : 1470 lignes, 13 colonnes.
- **Valeurs manquantes** : Aucune détectée.
- **Outliers** : Correction de 270 outliers sur les colonnes numériques pour améliorer la qualité des modèles.

### 2. Problème de déséquilibre des classes
La variable cible **Attrition** était déséquilibrée (`Yes` ≈ 300 vs `No` ≈ 1200).
Des méthodes d'équilibrage ont été intégrées :
- **Data Level** : Random Under-Sampling, Random Over-Sampling.
- **Algorithm Level** : Cost-Sensitive Learning, One-Class Learning.

Le Random Under-Sampling s'est révélé le plus performant pour ce cas.

---

## Analyse Exploratoire

- **Heatmap de corrélations** : Identification des dépendances entre variables.
- **Analyse univariée** : Étude des distributions des variables numériques et catégorielles.
- **Analyse bivariée** : Étude de la relation entre les variables (numérique-numérique, catégorique-catégorique).

### Points importants :
- L'âge est un facteur discriminant de l'attrition.
- Le département est significativement associé à l'attrition (p-value = 0.0045).
- Le niveau d'éducation n'est pas significativement associé.

---

## Modélisation et comparaison de modèles

Nous avons comparé plusieurs algorithmes :

| Modèle Comparé | Meilleur modèle sélectionné |
|:--------------:|:---------------------------:|
| Random Forest vs Decision Tree | Decision Tree |
| Decision Tree vs KNN | KNN |
| KNN vs Logistic Regression | Logistic Regression |
| Logistic Regression vs SVM | Logistic Regression |

**Conclusion** :  
La **régression logistique** a fourni les meilleurs résultats sur ce jeu de données, en termes de précision, rappel, accuracy et courbe ROC.

---

## Technologies utilisées
- **Langage** : R
- **Framework** : Shiny
- **Libraries** : caret, ggplot2, dplyr, shinythemes, etc.

---

