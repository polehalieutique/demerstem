# Demerstem

## Overview

![alt text](./sticker_demerstem.png?raw=true)

img[alt=sticker_demerstem] { width: 50px; }

use_logo("./sticker_demerstem.png", geometry = "240x278", retina = TRUE)

This packages aims to provide function to help for stocks assessment process

Package developpé par le projet DEMERSTEM en appui à l'évaluation des stock

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
