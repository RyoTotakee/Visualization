This Zenodo repository provides the code and supporting materials used for the analyses and figures in the manuscript:

"AIM-ALPHA v1.0: A partial equilibrium model of global agriculture and land use at basin-level resolution"


This repository includes R code for processing data and generating the figures.
After placing the required datasets into the data/ directory, you can reproduce the
outputs by either:

1. Opening RStudio and running prog/Visualization.R

or

2. Running the shell script run.sh

The generated figures will be saved in the /output/plot/ directory.


Datasets:

To run the scripts, you will need to download the required data from the SSP Public Database (Version 2.0), available at:

    https://tntcat.iiasa.ac.at/SspDb

Please place the downloaded SSP data into the "data" directory of this repository
before running the code. The SSP database is provided by IIASA under its own terms of use, and therefore the original data cannot be redistributed here.


This repository also contains several external datasets used in the analysis.
These datasets are provided in processed and reformatted versions based on the
original sources listed below:

FAOSTAT (CC BY 4.0)
FAO. 2025. Food Balances (2010-). Accessed on 24 November 2025. https://www.fao.org/faostat/en/#data/FBS Licence: CC-BY-4.0.
FAO. 2025. Producer Prices. Accessed on 24 November 2025. https://www.fao.org/faostat/en/#data/PP Licence: CC-BY-4.0.
FAO. 2025. Crops and livestock products. Accessed on 24 November 2025. https://www.fao.org/faostat/en/#data/QCL Licence: CC-BY-4.0.
FAO. 2025. Land Use. Accessed on 24 November 2025. 
https://www.fao.org/faostat/en/#data/RL Licence: CC-BY-4.0.
FAO. 2025. Emissions totals. Accessed on 24 November 2025. https://www.fao.org/faostat/en/#data/GT Licence: CC-BY-4.0.

EDGAR (CC BY 4.0)
Crippa, M., Guizzardi, D., Pagani, F., Banja, M., Muntean, M., Schaaf, E., Monforti-Ferrario, F., Becker, W., Quadrelli, R., Risquez Martin, A., Taghavi-Moharamli, P., Köykkä, J., Grassi, G., Rossi, S., Melo, J., Oom, D., Branco, A., San-Miguel, J., Manca, G., Pisoni, E., Vignati, E. and Pekar, F., GHG emissions of all world countries, Publications Office of the European Union, Luxembourg, 2024, doi:10.2760/4002897, JRC138862

LUH2
Hurtt, G. C., L. Chini, R. Sahajpal, S. Frolking, B. L. Bodirsky, K. Calvin, J. C. Doelman, J. Fisk, S. Fujimori, K. K. Goldewijk, T. Hasegawa, P. Havlik, A. Heinimann, F. Humpenöder, J. Jungclaus, Jed Kaplan, J. Kennedy, T. Kristzin, D. Lawrence, P. Lawrence, L. Ma, O. Mertz, J. Pongratz, A. Popp, B. Poulter, K. Riahi, E. Shevliakova, E. Stehfest, P. Thornton, F. N. Tubiello, D. P. van Vuuren, X. Zhang (2020). Harmonization of Global Land-Use Change and Management for the Period 850-2100 (LUH2) for CMIP6.Geoscientifc Model Development Discussions. https://doi.org/10.5194/gmd-2019-360

FAO AQUASTAT dataset "Major hydrological basins of the world" (CC BY-NC-SA 3.0 IGO)
Food and Agriculture Organization of the United Nations, FAO Land and Water Division. Major hydrological basins of the world. Latest update: 02/02/2011. Accessed (dd-mm/yy) from: https://data.apps.fao.org/catalog//iso/7707086d-af3c-41cc-8aa5-323d8609b2d1

World Bank Official Boundaries (CC BY 4.0)
World Bank. World Bank Official Boundaries. Accessed on 24 November 2025.
https://datacatalog.worldbank.org/search/dataset/0038272/World-Bank-Official-Boundaries

