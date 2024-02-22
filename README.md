# r_quarto_ghpage

## Background

This dashboard is part of my exercise in developing using [Quarto Dashboards](https://quarto.org/docs/dashboards/), publishing on [Github Pages](https://pages.github.com/), and then automatically updated it using [Github Actions](https://github.com/features/actions).

## Data and Packages

### Input Data
The input data for this dashboard is downloaded using a [Weather API](https://github.com/pace11/weather-api?tab=readme-ov-file) which acts as a (unofficial) weather API wrapper from XML to JSON references from [API of BMKG(Badan Meteorologi, Klimatologi, dan Geofisika)](https://data.bmkg.go.id).

### Packages

The dashboard uses the following packages:

-   Data visualization
    -   [highcharter](https://jkunst.com/highcharter/index.html)
-   Utility
    -   [dplyr](https://dplyr.tidyverse.org/)
    -   [tidyr](https://tidyr.tidyverse.org/)
    -   [lubridate](https://lubridate.tidyverse.org/)
    -   [janitor](https://cran.r-project.org/web/packages/janitor/index.html)