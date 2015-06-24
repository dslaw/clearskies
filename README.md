# Clearskies - Clear Sky Models in R

Clearskies is an R package for modeling solar irradiance.

## Installation

The R package can be installed from github using devtools:

```
install.packages("devtools")
library(devtools)
install_github('dslaw/clearskies')
library(clearskies)
```

Installation has been tested on Windows and Linux (Debian 8.0) with R 3.1.1
"Sock it to Me".

## Models

| Clear Sky Model               | Function Name | Parameter arguments |
|-------------------------------|---------------|---------------------|
| Adnot-Bourges-Campana-Gicquel | ABCG          | a, b                |
| Ineichen-Perez                | Ineichen      | a, b, c, TL         |
| Robledo-Soler                 | RS            | a, b, c             |

## Example - Fitting the Robledo-Soler model

```
eugene_oregon <- list(Latitude = 44.05, Longitude = -123.07, Timezone = -8)
day <- list(DayOfYear = 110, Year = 2014, Interval = 1)
params <- list(a = 1159.24, b = 1.179, c = -0.0019) # defaults
model <- clear_sky('RS', eugene_oregon, day, parameters = params)
length(model)
# [1] 1440
```
