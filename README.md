
# Software quality assessment of fourteen hydrological models

[Arnald Puy](https://www.arnaldpuy.com/) et al.

This study conducts a static analysis of the code of fourteen global hydrological models (five written in
Fortran, four in Python and five including both languages) to get an insight into their complexity,
maintainability and cognitive load. The goal is to appraise the risk of bugs and of 
introducing undesired behaviours in future model updates. The models are the following:

* [CTSM](https://github.com/ESCOMP/CTSM)  - Community Terrestrial Systems Model.    
* [CWatM](https://github.com/iiasa/CWatM) - Community Water Model.      
* [DBH](https://hydro.iis.u-tokyo.ac.jp/DBH/index_files/Page394.htm) - Distributed Biosphere-Hydrological Model.
* [GR4J](https://github.com/EdgarEspitia/GR4J) - Génie Rural à 4 paramètres Journalier.       
* [H08](https://github.com/h08model/H08) - H08 Global Hydrological Model.
* [HBV](https://github.com/johnrobertcraven/hbv_hydromodel) - Hydrologiska Byråns Vattenbalansavdelning model.      
* HydroPy - (Hydrological model implemented in Python) 
* [HYPE](https://sourceforge.net/projects/hype/files/) - Hydrological Predictions for the Environment.
* [MHM](https://zenodo.org/records/8279545) - Mesoscale Hydrologic Model.       
* [ORCHIDEE](https://forge.ipsl.jussieu.fr/orchidee/browser/branches/ORCHIDEE-MICT/tags/ORCHIDEE_MICT_8.4.1) - Organising Carbon and Hydrology In Dynamic Ecosystems. 
* [PCR-GLOBWB](https://github.com/UU-Hydro/PCR-GLOBWB_model) - PCRaster Global Water Balance model.
* [SACRAMENTO](https://github.com/NOAA-OWP/sac-sma) - Sacramento Soil Moisture Accounting Model.
* [SWAT](https://swatplus.gitbook.io/docs/source-code) - Soil and Water Assessment Tool.  
* [VIC](https://github.com/UW-Hydro/VIC) - Variable Infiltration Capacity model.

### Data

The "dataset" folder contains the data produced in this study. 

* `results_sqa.xlsx`: It contains three tabs:

** `descriptive_stats`:

** `maintainability_index`:

** `score`:

### Code

We offer the code in `.R`, `.pdf` and `.Rmd`. Our entire workflow can be run and the 
results replicated from either of these files. The user must run the code from the 
same folder where the files in the generated data section are stored for a successful 
compilation.

