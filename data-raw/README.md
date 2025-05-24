# Données Brutes - Triage FRENCH

## Sources
- Hospital A : Données 2023 (format Excel)
- Hospital B : Export SIU 2023 (format CSV)

## Protocole d'Anonymisation
1. Suppression des identifiants directs
2. Cryptage SHA256 des IDs patients
3. Agrégation temporelle mensuelle

## Régénération des Données
```bash
Rscript 01_import-raw-data.R
Rscript 02_clean-french-triage.R
