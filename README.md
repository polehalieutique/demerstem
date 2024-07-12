# Demerstem <img src="sticker_demerstem.png" alt="drawing" width="120" align="right"/></img>


## Overview

This packages aims to provide function to help for stocks assessment process in West Africa DLS context and combining surplus-production and length-based model.
In particular in provides : 
1. Delta-glm method to extract year effect and compute indices abundance
2. Surplus-production model with pseudo-equilibrium fitting
3. Polymodal decomposition to extract numbers-at-age
4. Rectified pseudo-cohort to estimate F-at-age and elaborate a diagnosis on stock status 

Ce package a été developpé durant le projet DEMERSTEM en appui à l'évaluation des stocks d'Afrique de l'Ouest en contexte data-limited et combinant des modèles basés sur les tailles et les captures. 
En particulier il fournit : 
1. Une méthode d'extraction d'indices d'abondance par delta-GLM
2. Un modèle de production ajusté en pseudo-équilibre
3. Une estimation du nombre d'invidus à chaque âge par décomposition polymodale
4. Les F aux âges par utilisation des pseudo cohortes rectifiées

## Installation

```r
# You need devtools to use the function install_github()
library(devtools)

# You also need to install Jags for bayesian simulation
Disponible ici : https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Windows/

# Vous pouvez alors installer le package 

install_github("polehalieutique/demerstem")

library(demerstem)
packageVersion("demerstem")
```
