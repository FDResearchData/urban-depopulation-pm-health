# Lower particulate pollution and mortality burden associated with urban depopulation across Chinese cities

This repository contains shareable analysis-ready data and code supporting the main analyses and figures in the manuscript.

## Data

Files are stored in the `data/` directory.

- `city_level_data.csv`: analysis-ready city-year dataset used for the main city-level analyses.
- `health_inputs.csv`: age-, sex- and city-specific inputs for the PM2.5 mortality calculations.
- `health_results.csv`: city-level modeled PM2.5 and mortality results.
- `fig1.csv`–`fig4.csv`: source data for Figs. 1–4.

Firm-level administrative microdata are not redistributed in this repository. Only aggregated results derived from these data are provided where permitted.

## Code

Files are stored in the `code/` directory.

- `main_sdm.R`: primary and policy-adjusted Spatial Durbin Models and impact decomposition.
- `health_analysis.R`: mortality calculations from the provided city-specific PM2.5 exposure scenarios.
- `fig1.R`–`fig4.R`: scripts used to reproduce the main figures.

The current scripts also reference `data/spatial_weights.csv` for the SDM analysis and `data/city_boundaries.gpkg` for Figs. 1 and 4; these spatial files are not yet included in this branch.

## Software

R 4.3  
Python 3.9  
Stata/SE 18  
QGIS 3.6

## License

The MIT License applies to the code in this repository. Data remain subject to the terms of their original sources where applicable.
