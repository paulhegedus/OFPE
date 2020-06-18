
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OFPE <img src="man/figures/msu_coa_logo.png" align="right" width="120" /> <img src="man/figures/OFPE_logo.png" align="right" width="120" />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--06--17-yellowgreen.svg)](/commits/master)
[![R build
status](https://github.com/paulhegedus/OFPE/workflows/R-CMD-check/badge.svg)](https://github.com/paulhegedus/OFPE/actions)
[![Travis build
status](https://travis-ci.com/paulhegedus/OFPE.svg?branch=master)](https://travis-ci.com/paulhegedus/OFPE)

*\<Package Vesion?\>* <!-- badges: end -->

## Overview

The [On-Field Precision Experiments
project](https://sites.google.com/site/ofpeframework/) (OFPE) package is
developed by the Agroecology Lab at Montana State University and
provides the tools for researchers and producers to execute data
intensive field management. Users are able to manage and analyze data
collected from individual fields to make field specific recommendations
of agricultural inputs. This project requires the experimental
application of input rates to develop crop response functions for
predicting outcomes of various management scenarios.

This package contains tools for creating a database to store field
specific information, importing data collected on-farms or from
remotely-sensed sources to the database, enriching yield and protein
datasets with covariate data, analysis and simulation of winter wheat
yield and grain able input rates (nitrogen fertilizer or seed), and
generation of input prescriptions based on field specific predictions
based on field specific data.

More information of the MSU OFPE Project can be found at;

  - The OFPE website: <https://sites.google.com/site/ofpeframework/>
  - The OFPE package vignettes
  - The OFPE package complementary website:
    <https://paulhegedus.github.io/OFPE-Website/>

## Installation

You can install the released version of OFPE from
[GitHub](https://github.com) with:

``` r
# install.packages("devtools")
devtools::install_github("paulhegedus/OFPE")
```

## Intended Use/Disclaimer

This package is intended to be used with dryland winter wheat harvest
and application data collected in Montana, USA. The OFPE packages is
intended to provide the codebase and documentation for the management,
analysis, and development of agricultural input management prescriptions
used by crop managers or researchers. The codebase constitutes the
backend of the associate [OFPE Shiny web
application](https://paulhegedus.shinyapps.io/OFPE_AnalysisAndSim_App_v1/?_ga=2.189182059.1336631904.1592115204-590292424.1592115204).
This package was developed using conventional and organic winter wheat
data and intended to inform site-specific management decisions of
nitrogen fertilizer or seed rates.

The OFPE package is not intended for use outside of Montana or for crops
other than dryland winter wheat. We are not liable for any damages,
actions, or outcomes of any decision made while using this this package
or influenced by any associated intellectual property.

## Requirements

The workflows outlined and supported by functions in this package
operate with the assumption that the user has access to or has created a
PostgreSQL database. Associated tools such as PostGIS are required for
data management. If a user is setting up a database on a local machine,
the user will need to download [PostgreSQL]() and enable a [postgres]()
server account. This process is describe in this
[tutorial](https://paulhegedus.github.io/OFPE-Website/postgres_setup.html).
This is not needed if you have credentials to a preexisting OFPE
formatted database.

This package includes functions for interfacing with Google Earth Engine
and Google Drive. To access and operate these functions, or for complete
usage of the OFPE workflow, the user will need to set up a [Google Earth
Engine](https://earthengine.google.com) account and sign up for [Google
Drive](https://www.google.com/drive/).

While not required, it is assumed that the user has access and uses
grain protein percent as a response variable complementary to winter
wheat crop yield. This package assumes that the user is gathering
protein data with [NextInstruments](https://www.nextinstruments.net)
[CropScan 3300H](http://www.nextinstruments.net/index.php/products/cropscan)
infrared on-combine monitor. Users can visualize their observed protein
data from the CropScan monitor with the [OFPE Protein Vis
App](https://paulhegedus.shinyapps.io/OFPE_Protein_Application/?_ga=2.163574524.1336631904.1592115204-590292424.1592115204).

Other on-farm data such as yield and as-applied fertilizer or seed rates
do not require a specific brand association. This code has been tested
with John Deere and Case equipment.

## Workflow/Vignettes

The vignettes of the OFPE package follow the OFPE framework workflow.
This process consists of creating a database with user specified field
and farm data, importing data collected on-fields and from open sources,
enriching yield and protein datasets, analyzing response of yield and
protein to variable input rates, predicting net-return outcomes of
management strategies, and generating site-specific prescriptions of
inputs.

For more detailed information about the OFPE data workflow and
framework, see this
[website](https://paulhegedus.github.io/OFPE-Website/). This includes
the tutorials linked above, as well as activity and component diagrams
of the processes/vignettes described below.

Below is a schematic of the general On-Field Precision Experiments data
workflow, with a more detailed description of the data handling process
[here](https://paulhegedus.github.io/OFPE-Website/ofpe_overview.html).
![OFPE data framework showing the circular process of experiment
creation and application, data collection, and prescription and
experiment generation. Figured made by Bruce Maxwell
(2015).](man/figures/ofpe_framework.png)

### Database Creation/Management

This process begins with the creation of a spatial database for storing
data gathered from farms and from satellite sources. This database is
set up in a specific OFPE format to support the ensuing workflow and
requires the user to specify the boundaries of fields selected for data
intensive management and the farm boundary within which a farmer’s
fields fall. This is a one-time process where once the database is set
up it will only need management to keep it up to date.

The user will need access to an OFPE formatted database or need to
create their own. In order to do this, see this
[tutorial](https://paulhegedus.github.io/OFPE-Website/postgres_setup.html)
for downloading PostgreSQL and setting up a local postgres server. If
using a pre-existing database, the user will need the host, username,
password, and database driver. The database schematic and format can be
found
[here](https://paulhegedus.github.io/OFPE-Website/db_creation.html).

The user will need access to or need to create necessary farm and field
boundaries associated with their experimental fields to set up their
database. These farm boundaries that encompass the fields within a
farmer’s ownership/management purview are imported as assets into Google
Earth Engine and imported into the database. See this
[tutorial](https://paulhegedus.github.io/OFPE-Website/create_shp_qgis.html)
for creating a shapefile of a field and farm boundary.

The process for creating a database is outlined in the activity diagram
on [this
page](https://paulhegedus.github.io/OFPE-Website/db_creation.html),
where a component diagram can also be found.

### Data Import

The stream of data available from agricultural fields include data
collected from farm machinery during normal farm operations such as
seeding, spraying, and harvesting. The OFPE project focuses on winter
wheat yield and protein as response variables and nitrogen fertilizer or
seed rates as explanatory variables, depending on whether the field is
farmed conventionally or organically, respectively. These data are
collected from the farm machinery as ESRI Shapefiles, barring CropScan
protein data that is gathered as a comma-separated values file. These
data can be batch uploaded to the database through a script that
automatically identifies and organizes each upload.

The process for importing on-farm data to the database is outlined in
the activity diagram on [this
page](https://paulhegedus.github.io/OFPE-Website/dat_import.html), where
a component diagram can also be found.

Remote sensing data is collected from Google Earth Engine and includes
data temporally variable data such as weather and static data such as
topogrophy. The user is responsible for running the Google Earth Engine
Javascript code to download data for each year required. See this
[tutorial](https://paulhegedus.github.io/OFPE-Website/gee_setup.html)
for adding farm boundaries to GEE as asset and this
[tutorial](https://paulhegedus.github.io/OFPE-Website/run_gee.html) for
editing and using Javascript code. This data is downloaded to Google
Drive, where the data can be batch uploaded to the database. See this
[tutorial](https://paulhegedus.github.io/OFPE-Website/gd_setup.html) for
setting up Google Drive to receive your data from Google Earth Engine.

The process for importing Google Earth Engine data to the database is
outlined in the activity diagram on [this
page](https://paulhegedus.github.io/OFPE-Website/dat_import.html), where
a component diagram can also be found.

### Data Aggregation

Raw data from various sources need to be wrangled and aggregated
together before statistical analysis and model development. Because
yield and protein data are related to crop production and quality, the
source of winter wheat value, covariate and explanatory data are used to
enrich yield and protein datasets Due to equipment differences, yield
and protein are gathered at different temporal resolutions (3 and 10
seconds, respectively) and subsequent spatial resolution, so are treated
as separate datasets and not combined based on interpolation or
estimation of protein data. The OFPE project aims to use the finest
resolution of data possible to make decisions with data that has as
little natural variation removed as possible.

The activity workflow for enriching yield and protein datasets is
described on [this
page](https://paulhegedus.github.io/OFPE-Website/data_agg.html), where a
component diagram can also be found.

### Analysis/Simulation

Once all explanatory and covariate data is aggregated to the response
data observations, it can be used to train models of crop production
(yield) or quality (protein). Experimentally varying input rates
(fertilizer or seed) is crucial to developing the function for the
relationship between the crop response and input. These models are then
used in a Monte Carlo simulation that randomly selects an economic
condition (winter wheat price, price of fertilizer or seed) to generate
the probability of outcomes. Because the farmer has to make a decision
on their input rates by March 30th for winter wheat, the farmer will not
know the weather conditions for the rest of the growing season, so the
user will be able to select a year or scenario to simulate.

The activity workflow for simulating management outcomes is described on
[this page](https://paulhegedus.github.io/OFPE-Website/sim_anal.html),
where a component diagram can also be found.

### Prescription Generation

After the user analyzes the field specific data from their specified
year and decides on a management strategy, a prescriptive fertilizer or
seed map can be generated based on optimizing inputs. This optimization
can take the form of profit maximization or profit maximization and
pollution minimization. This prescription can reflect a specified price
scenario or the *mean/median* of inputs across simulation results. The
prescription is exported as a shapefile that the farmer can upload to
their equipment and apply the inputs.

The activity workflow for generating prescriptions is described on [this
page](https://paulhegedus.github.io/OFPE-Website/rx_gen.html), where a
component diagram can also be found.

# Funding <img src="man/figures/msu_coa_logo.png" align="right" width="120" /> <img src="man/figures/MTIOE_logo.png" align="right" width="120" /> <img src="man/figures/DIFM_logo.png" align="right" width="120" /> <img src="man/figures/MREDI_logo.png" align="right" width="200" />

Montana State University College of Agriculture, Montana Fertilizer
Advisory Commitee (MFAC), Montana Research and Economic Development
Initiative (MREDI), University of Illinois Data Intensive Farm
Management (DIFM) Project, Montana Institute on Ecosystems
