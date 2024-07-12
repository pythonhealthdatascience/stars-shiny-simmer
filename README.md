[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12733947.svg)](https://doi.org/10.5281/zenodo.12733947)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Read the Docs](https://readthedocs.org/projects/pip/badge/?version=latest)](https://tommonks.github.io/treat-sim-rsimmer/)
[![ORCID: Harper](https://img.shields.io/badge/ORCID-0000--0001--5274--5037-brightgreen)](https://orcid.org/0000-0001-5274-5037)
[![ORCID: Monks](https://img.shields.io/badge/ORCID-0000--0003--2631--4481-brightgreen)](https://orcid.org/0000-0003-2631-4481)
[![ORCID: Heather](https://img.shields.io/badge/ORCID-0000--0002--6596--3479-brightgreen)](https://orcid.org/0000-0002-6596-3479)
[![ORCID: Mustafee](https://img.shields.io/badge/ORCID-0000--0002--2204--8924-brightgreen)](https://orcid.org/0000-0002-2204-8924)

# 💫  Sharing Tools and Artefacts for Reproducible Simulations 2.0: a template for a `shiny` interface to a `simmer` DES model.

> 🏗 **WORK IN PROGRESS** (STARS 2.0) 🏗️   
  > The materials in this repo are experimental. 


>  🐍 **Looking for an interface to a Python DES model?** 
  >  We have tested code available for [streamlit](https://github.com/pythonhealthdatascience/stars-streamlit-example) and [Shiny for Python](https://github.com/pythonhealthdatascience/stars-ciw-example)

## Overview

  The materials and methods in this documentation support work towards developing the **S.T.A.R.S healthcare framework** (**S**haring **T**ools and **A**rtifacts for **R**eproducible **S**imulations in healthcare).  Long term S.T.A.R.S aims to support researchers share open simulation models regardless of language choice, improve the quality of sharing, and reduce the workload required to meet high standards of open science for the modelling and simulation community.

> The code and written materials are a **work in progress** towards STARS version 2.0. It is not recommended to use these materials in simulation practice at the moment.
  
This repo demonstrates the application of sharing a discrete-event simulation model in R and associated research artefacts:  
  
  * All artefacts in this repository are linked to study researchers via ORCIDs;
  * Model code is made available under the MIT license;
  * The `simmer` has been structured in a way to enable easy connection to a simple `shiny` dashboard interface;
  * [**To do**: validate and test R dependencies managed through `renv`]
  *  The R code and simmer model are documented and explained in a quarto website served up by GitHub pages;
  * [**To do**: the materials are deposited and made citatable using Zenodo;]
  * [**To do**: The models are sharable with other researchers and the NHS without the need to install software.]
  
## Citation

Please cite our code if you use it ☺

> Monks, T., Harper, A., Heather, A., & Mustafee, N. (2024). Sharing Tools and Artefacts for Reproducible Simulations 2.0: a template for a `shiny` interface to a `simmer` DES model (v0.1.0). Zenodo. https://doi.org/10.5281/zenodo.12733947

```bibtex
@software{stars_shiny_simmer,
  author       = {Monks, Thomas and
                  Harper, Alison and
                  Heather, Amy and
                  Mustafee, Navonil},
  title        = {{Sharing Tools and Artefacts for Reproducible 
                   Simulations 2.0: a template for a `shiny`
                   interface to a `simmer` DES model}},
  month        = jul,
  year         = 2024,
  publisher    = {Zenodo},
  version      = {v0.1.0},
  doi          = {10.5281/zenodo.12733947},
  url          = {https://doi.org/10.5281/zenodo.12733947}
}
```

We do not have a publication to support this work yet.  For now, please cite the Journal of Simulation article that reports STARS 1.0 and our pilot work.

```bibtex
@article{towards_stars_jos_paper,
author = {Thomas Monks, Alison Harper and Navonil Mustafee},
title = {Towards sharing tools and artefacts for reusable simulations in healthcare},
journal = {Journal of Simulation},
volume = {0},
number = {0},
pages = {1--20},
year = {2024},
publisher = {Taylor \& Francis},
doi = {10.1080/17477778.2024.2347882},
URL = { https://doi.org/10.1080/17477778.2024.2347882},
}
```


## Funding

This work was supported by the Medical Research Council [grant number MR/Z503915/1]

## Case study model

**This example is based on exercise 13 from Nelson (2013) page 170.**  Please also credit this work is you use our materials.

> *Nelson. B.L. (2013). [Foundations and methods of stochastic simulation](https://www.amazon.co.uk/Foundations-Methods-Stochastic-Simulation-International/dp/1461461596/ref=sr_1_1?dchild=1&keywords=foundations+and+methods+of+stochastic+simulation&qid=1617050801&sr=8-1). Springer.* 

We adapt a textbook example from Nelson (2013): a terminating discrete-event simulation model of a U.S based treatment centre. In the model, patients arrive to the health centre between 6am and 12am following a non-stationary Poisson process. On arrival, all patients sign-in and are triaged into two classes: trauma and non-trauma. Trauma patients include impact injuries, broken bones, strains or cuts etc. Non-trauma include acute sickness, pain, and general feelings of being unwell etc. Trauma patients must first be stabilised in a trauma room. These patients then undergo treatment in a cubicle before being discharged. Non-trauma patients go through registration and examination activities. A proportion of non-trauma patients require treatment in a cubicle before being discharged. The model predicts waiting time and resource utilisation statistics for the treatment centre. The model allows managers to ask question about the physical design and layout of the treatment centre, the order in which patients are seen, the diagnostic equipment needed by patients, and the speed of treatments. For example: “what if we converted a doctors examination room into a room where nurses assess the urgency of the patients needs.”; or “what if the number of patients we treat in the afternoon doubled” 

## Online version of model

A prototype version of the shiny interface to the simmer DES model is hosted on Shiny Apps: https://pythonhealthdatascience.shinyapps.io/stars_shiny_simmer


## Online documentation produced by Quarto

[![Read the Docs](https://readthedocs.org/projects/pip/badge/?version=latest)](https://pythonhealthdatascience.github.io/stars-treat-simmer)

* The documentation can be access at [https://pythonhealthdatascience.github.io/stars-treat-simmer](https://pythonhealthdatascience.github.io/stars-treat-simmer)
