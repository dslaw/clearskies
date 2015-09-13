# Clearskies - Clear Sky Models in R

Clearskies is an R package for modeling solar irradiance.

## Installation

The R package can be installed from github using devtools:

```
install.packages("devtools")
library(devtools)
install_github("dslaw/clearskies")
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
Fitting Robledo-Soler model to a single day.

```
eugene_oregon <- list(Latitude = 44.05, Longitude = -123.07, Timezone = -8)
day <- list(DayOfYear = 245, Year = 2014, Interval = 1)
params <- list(a = 1159.24, b = 1.179, c = -0.0019) # defaults

dat <- eugene[ eugene[["DayOfYear"]] == day$DayOfYear, ]
observed <- dat[["Ghi"]]

model <- clear_sky("RS", x = day, y = eugene_oregon, parameters = params,
                   data = observed)
model <- clear_points(model, thresholds = thresholds, window_len = 10L)

summary(model)
# Model: RS
# 1440 predicted points over 1 days
#
# Observed:
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#     0.0     0.0    68.5   221.8   340.5   969.0
#
# Predicted:
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#    0.00    0.00   69.83  269.40  588.00  811.30
#
# Number of clear points: 771  Percent clear: 53.54%

# Plot observed and predicted GHI with clear points in foreground
require(ggplot2)
p <- plot(model, use.ggplot = TRUE)
p + ggtitle("Robledo-Soler Model")
```
<img src="https://github.com/dslaw/clearskies/blob/master/figs/example.png" width="75%" height="75%">
