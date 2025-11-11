# Analysis of satellite tracking data from two Black-capped Petrels (Pterodroma hasitata), 2019

This repository contains R scripts used in the analysis of satellite tracking data from two Black-capped Petrels (Pterodroma hasitata) published in the Journal of Caribbean Ornithology: 
Yvan G. Satgé, J. Brian Patteson, Bradford S. Keitt, Chris P. Gaskin, and Patrick G.R. Jodice. 2025. Satellite tracking supports hypotheses of breeding allochrony and allopatry in the Endangered Pterodroma hasitata (Black-capped Petrel, Diablotin). 2025. Journal of Caribbean Ornithology 38:59–66. https://doi.org/10.55431/jco.2025.38.59-66 

## Article abstract

Pterodroma hasitata, the Black-capped Petrel (locally known as Diablotin), is the only extant gadfly petrel nesting in the Caribbean. The species is listed as globally Endangered by the IUCN and was recently listed as endangered under the U.S. Endangered Species Act. Pterodroma hasitata show a phenotypic gradient, ranging from a darker, smaller form to a paler, heavier form, that is reflected in a strong genetic structure. This phylogenetic divergence suggests the existence of at least two distinct breeding populations. We report on pre-breeding movements of two male Pterodroma hasitata, one of each form, tracked by satellite from non-breeding areas in Gulf Stream waters of the western North Atlantic Ocean to breeding locations in Hispaniola in late 2019. Based on a combination of tracking locations, location error classes, battery voltage, and satellite communication schedules, we infer that the light-form petrel visited a nest in central Dominican Republic during 2 to 8 October and 9 to 15 October, and the dark form visited a nest in southeastern Haiti during 9 to 22 November and 29 November to 3 December. This information supports earlier suggestions that Pterodroma hasitata forms breed in allochrony and in allopatry, both of which may be a driver of speciation. 

## R script author

Yvan G. Satgé, Department of Forestry and Environmental Conservation, and South Carolina Cooperative Fish & Wildlife Research Unit, Clemson University, Clemson, SC.


## Files

`1_Track modelling.R`: Downloads tracking data from Movebank.org; runs a state-space model (random walk); plots satellite data and fitted data.

`2_Calculations.R`: Calculates battery levels of satellite tracking devices. 

`3_Figure 2`: Creates maps and plots used in Figure 2 of the article. 

Tracking data are available from Movebank.org, study 746910348 (https://doi.org/10.5441/001/1.0786vv78)

