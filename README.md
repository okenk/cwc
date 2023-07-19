# Barataria Bay DWH food web model

This includes the code and model files for the following publication:

Oken, K.L., K.W. Able, K. de Mutsert, F.J. Fodrie, P.C. López-Duarte, C.W. Martin, M.J. McCann, J.A. Olin, M.J. Polito, B.J. Roberts, O.P. Jensen. (Accepted) 
Fishery closures, more than predator release, increased persistence of nearshore fishes and invertebrates to the Deepwater Horizon oil spill. _Estuaries and Coasts_. 
Preprint at: 10.31219/osf.io/csfqn

This repository contains a number of model files generated during development. The final model files are:
* [Model](https://github.com/okenk/cwc/blob/main/Data/GroupInfo_data_deleted.csv)
* [Diet](https://github.com/okenk/cwc/blob/main/Data/Diet.csv)
* [Stanzas](https://github.com/okenk/cwc/blob/main/Data/stanzas.csv)

The R code to run the Rpath model is [here](https://github.com/okenk/cwc/blob/main/R/ecopath_inputs.R). Note installation of a specific Rpath commit is required; later
commits do not allow for leading stanzas that are not the oldest age group. Installation of 
[this branch of my own fork](https://github.com/okenk/Rpath/tree/before_stanza_changes) is acceptable. 

The code to run the generalized equilibrium model is [here](https://github.com/okenk/cwc/blob/main/R/runTradeOffmodel.R).

If you have any questions, feel free to contact me with a [github issue](https://github.com/okenk/cwc/issues) or [by email](mailto:kiva.oken@gnoaa.gov).
