# FrenchTriageR 🚨

**Implémentation de l'algorithme de triage médical FRENCH en R**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/votre_compte/FrenchTriageR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/votre_compte/FrenchTriageR/actions)

## 📖 Description

Ce package R permet l'implémentation du système de triage médical FRENCH utilisé dans les services d'urgence français. Il a été développé en collaboration avec des professionnels de santé et validé sur des cas cliniques standards.

**Note importante** : Aucune donnée réelle de patients n'est incluse dans ce dépôt. Les exemples utilisent des données synthétiques générées algorithmiquement.

## ✨ Fonctionnalités clés

- 🧹 Préprocessing des données médicales
- 🩺 Algorithme de triage FRENCH complet
- 📊 Visualisation des résultats
- 📁 Export des résultats au format Excel
- ✅ Validation des entrées médicales
- 🔒 Conforme aux bonnes pratiques de confidentialité

## ⚙️ Installation

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("votre_compte/FrenchTriageR")
```

## 🚀 Utilisation basique
```r
library(FrenchTriageR)

# Charger les données d'exemple synthétiques
data_path <- system.file("extdata", "sample_synthetic_data.xlsx", package = "FrenchTriageR")
synthetic_data <- preprocess_data(data_path)

# Appliquer l'algorithme de triage
results <- apply_french_triage(synthetic_data)

# Générer un rapport
generate_triage_report(results)

# Visualiser la distribution
plot_triage_distribution(results)
```

## ⚠️ Avertissements importants
Ce logiciel ne doit pas être utilisé pour des décisions médicales réelles

Les résultats doivent toujours être validés par un professionnel de santé qualifié

Les données d'exemple fournies sont entièrement synthétiques

## 📜 Licence
MIT License - Voir le fichier [LICENSE](https://github.com/edlansiaux/FrenchTriageR/blob/main/LICENSE) pour plus de détails
