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
| Ineichen-Perez                | Ineichen      | a, b, c, TL\*       |
| Robledo-Soler                 | RS            | a, b, c             |

\* Linke turbidity factor

## Example
Fitting the Robledo-Soler model to Easter day, 2014.

```
eugene_oregon <- list(Latitude = 44.05, Longitude = -123.07, Timezone = -8)
day <- list(DayOfYear = 110, Year = 2014, Interval = 1)
params <- list(a = 1159.24, b = 1.179, c = -0.0019) # defaults
model <- clear_sky('RS', x = day, y = eugene_oregon, parameters = params)

model$model
# [1] "RS"

min(model$predicted)
# [1] 8.892498e-17

max(model$predicted)
# [1] 851.6543
```
