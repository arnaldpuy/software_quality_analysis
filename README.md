
# Software quality assessment of fourteen hydrological models

[Arnald Puy](https://www.arnaldpuy.com/) et al.

This study conducts a static analysis of the code of fourteen global hydrological models (five written in
Fortran, four in Python and five including both languages) to get an insight into their complexity,
maintainability and cognitive load. The goal is to appraise the risk of bugs and of 
introducing undesired behaviours in future model updates. The models are the following:

* [CTSM](https://github.com/ESCOMP/CTSM)  - Community Terrestrial Systems Model.    
* CWatM - Community Water Model.      
* DBH - Distributed Biosphere-Hydrological Model.
* GR4J - Génie Rural à 4 paramètres Journalier.       
* H08 - H08 Global Hydrological Model.
* HBV - Hydrologiska Byråns Vattenbalansavdelning model.      
* HydroPy - (Hydrological model implemented in Python) 
* HYPE - Hydrological Predictions for the Environment.
* MHM - Mesoscale Hydrologic Model.       
* ORCHIDEE - Organising Carbon and Hydrology In Dynamic Ecosystems. 
* PCR-GLOBWB - PCRaster Global Water Balance model.
* SACRAMENTO - Sacramento Soil Moisture Accounting Model.
* SWAT - Soil and Water Assessment Tool.  
* VIC - Variable Infiltration Capacity model.

### Code

We offer the code in `.R`, `.pdf` and `.Rmd`. Our entire workflow can be run and the 
results replicated from either of these files. The user must run the code from the 
same folder where the files in the generated data section are stored for a successful 
compilation.
