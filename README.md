

# NFI-calc-chain-2025

Repository for scripts of the Calculation Workshop in March 2025. This project expands on the
training provided in March 2025.

## Objectives

1.  Get source data from ONA server directly or indirectly (master CSV with full names).
2.  Prepare the data by splitting the master CSV into entities and harmonizing variable names
    (lower case, underscore separation, full names as much as possible, starting with entity
    name)
3.  Clean the data (typos in entity codes mostly).
4.  Assign land cover section to each tree.
5.  Use ratio estimator to account for mixed land use in plots and subplots.
6.  Use CEO Phase 1 plots to generate more reliable estimates of carbon stocks, tree density and
    biodiversity (main species, richness)

## Description

The main scripts of projects are:

-   R/user/00-user-inputs.R --- Collect user inputs:

    -   Recompile data from source: TRUE/FALSE, useful when new data is added or data
        preparation scripts are changed.

    -   Download new data from ONA server: TRUE/FALSE, if TRUE, request user and password to ONA
        server then download the master CSV file with all NFI records.

    -   Remove all prepared and source data: TRUE/FALSE, useful when downloading new data and
        recompiling

    -   File name of NFI master CSV from ONA: TXT, indicate which file from 'data-source'
        contains the data, in case of manual download.

    -   File name of CEO file: TXT, indicate which file from 'data-source' contains the CEO
        observations.

    -   Time zone: Specify time zone to get time stamps correctly positioned.

-   R/setup/... --- Setup environment, prepare (optional) and load data

-   R/user/00-common.R --- Functions and objects useful for calculations and figures

-   R/user/01-clean-... --- Script to correct typos and entry errors found during the analysis.

-   
