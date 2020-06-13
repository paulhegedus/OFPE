
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OFPE <img src="man/figures/msu_coa_logo.png" align="right" width="120" /> <img src="man/figures/OFPE_logo.png" align="right" width="120" />

<!-- badges: start -->

[![Last-changedate](https://img.shields.io/badge/last%20change-2020--06--12-yellowgreen.svg)](/commits/master)
[![Travis build
status](https://travis-ci.com/paulhegedus/OFPE.svg?branch=master)](https://travis-ci.com/paulhegedus/OFPE)
[![R build
status](https://github.com/paulhegedus/OFPE/workflows/R-CMD-check/badge.svg)](https://github.com/paulhegedus/OFPE/actions)

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

## Installation

You can install the released version of OFPE from
[GitHub](https://github.com) with:

``` r
# install.packages("devtools")
devtools::install_github("paulhegedus/OFPE")
```

## Intended Use/Disclaimer

The OFPE packages is intended to make the management, analysis, and
development of agricultural input management prescriptions easy. This
package is intended to be used with dryland winter wheat harvest and
application data collected in Montana, USA or Manitoba, Canada. The
codebase constitutes the backend of the associate [OFPE Shiny web
application](*URL*). This package was developed using conventional and
organic winter wheat data and intended to inform site-specific
management decisions of nitrogen fertilizer or seed rates.

The OFPE package is not intended for use outside of Montana and Manitoba
or for crops other than dryland winter wheat. We are not liable for any
damages, actions, or outcomes of any decision made while using this this
package or influenced by any associated intellectual property.

## Requirements

The workflows outlined and supported by functions in this package
operate with the assumption that the user has access to or has created a
PostgreSQL database. Associated tools such as PostGIS are required for
data management. If a user is setting up a database on a local machine,
the user will need to download [PostgreSQL]() and enable a [postgres]()
server account.

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
infrared on-combine monitor.

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

Below is a schematic of the general On-Field Precision Experiments data
workflow framework. ![Key is found in the top left corner of the
schematic. Green arrows represent processes that require the OFPE
package. The PostgreSQL database in the center of the figure can be
stored on a cloud server or a local computer. Light blue boxes represent
alternate modules for executable
processes.](man/figures/ofpe_data_workflow.png)

### Database Creation/Management

This process begins with the creation of a spatial database for storing
data gathered from farms and from satellite sources. This database is
set up in a specific manner to support the ensuing workflow and requires
the user to specify the boundaries of fields selected for data intensive
management and the farm boundary within which a farmer’s fields fall.
This is a one-time process where once the database is set up it will
only need management to keep it up to date.

The user will need access to or need to create necessary farm and field
boundaries associated with their experimental fields to set up their
database. These farm boundaries that encompass the fields within a
farmer’s ownership/management purview are imported as assets into Google
Earth Engine and imported into the database. See [this
link](*TODO:%20Tutorial%20as%20html%20for%20making%20farm/field%20boundaries*)
for creating a shapefile of a field and farm boundary.

The process for creating a database is outlined below; ![Blue processes
are chained together by arrows, and black boxes indicate the addition of
user inputs.](man/figures/ofpe_db_mgmt.png)

And a diagram of the components for executing the database creation and
management workflow are outlined below. \[TODO: Caption for component
diagram.\]\<(man/figures/ofpe\_db\_comp.png)\>

### Data Import

The stream of data available from agricultural fields include data
collected from farm machinery during normal farm operations such as
seeding, spraying, and harvesting. The OFPE project focuses on winter
wheat yield and protein as response variables and nitrogen fertilizer or
seed rates as explanatory variables, depending on whether the field is
farmed conventionally or organically, respectively. These data are
collected from the farm machinery as ESRI Shapefiles, barring CropScan
protein data that is gathered as a comma-separated values file.

Remote sensing data is collected **TODO:activity diagram**
**TODO:component diagram**

### Data Aggregation

**TODO:activity diagram** **TODO:component diagram**

### Analysis/Simulation

**TODO:activity diagram** **TODO:component diagram**

### Prescription Generation

**TODO:activity diagram** **TODO:component diagram**
