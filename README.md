
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OFPE <img src="man/figures/msu_coa_logo.png" align="right" width="120" /> <img src="man/figures/OFPE_logo.png" align="right" width="120" />

<!-- badges: start -->

[![Last-changedate](https://img.shields.io/badge/last%20change-2020--06--12-yellowgreen.svg)](/commits/master)
[![Travis build
status](https://travis-ci.com/paulhegedus/OFPE.svg?branch=master)](https://travis-ci.com/paulhegedus/OFPE)
[![R build
status](https://github.com/paulhegedus/OFPE/workflows/R-CMD-check/badge.svg)](https://github.com/paulhegedus/OFPE/actions)
<!-- badges: end -->

## Overview

The On-Farm Precision Experiments (OFPE) package is developed by the
Agroecology Lab at Montana State University and provides the tools for
researchers and producers to manage and analyze data collected from
individual fields to make field specific recommendations of agricultural
inputs. This project requires the experimental application of input
rates to develop crop response functions for predicting outcomes of
various management scenarios.

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
codebase constitutes the backend of the associate OFPE Shiny web
application \<*URL*\>. This package was developed using conventional and
organic winter wheat data and intended to inform site-specific
management decisions of nitrogen fertilizer or seed rates.

The OFPE package is not intended for use outside of Montana and Manitoba
or for crops other than dryland winter wheat. We are not liable for any
damages, actions, or outcomes of any decision made while using this this
package or any associated intellectual property.

## Requirements

The workflows outlined and supported by functions in this package
operate with the assumption that the user has access to or has created a
PostgreSQL database. Associated tools such as PostGIS are required for
data management.  Instructions for downloading PostgreSQL and setting up
a postgres server can be found here: \<*URL*\>

This package includes functions for interfacing with Google Eart Engine
and

While not required, it is assumed that the user is using grain protein
percent

The functions and workflows operate on the premise that the user has
created or access to a PostgreSQL database

## Workflow/Vignettes

The vignettes of the OFPE package follow

### Database Creation/Management

**TODO:activity diagram** **TODO:component diagram**

### Data Import

**TODO:activity diagram** **TODO:component diagram**

### Data Aggregation

**TODO:activity diagram** **TODO:component diagram**

### Analysis/Simulation

**TODO:activity diagram** **TODO:component diagram**

### Prescription Generation

**TODO:activity diagram** **TODO:component diagram**

## Example

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
