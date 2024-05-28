# Repository for: A market for 30x30 in the ocean
## By: Juan Carlos Villaseñor-Derbez, Christopher Costello & Andrew Plantinga

> Please note this repository contains multiple branches.

## Repository structure 

The repository is set-up with `renv`. You should be able to have an identical copy of the R packages used here by running `renv::restore()`.

There is a script that loads some default options and values into the environment. It's not crucial, and it should be loaded directly by the `.Rprofile` file upon start-up (assuming you are running RStudio). Feel free to source it yourself.

Alternatively, the project also has a [`Makefile`](https://github.com/jcvdav/transferable_conservation/blob/master/Makefile) that outlines the relation between input files, scripts, and output files needed to reproduce our results. To reproduce them, simply type `make -B` in your command line of preference. See below for a DAG representation of the makefile.

![](makefile-dag.png)

### Data

- Some raw data are available (in `raw_data`) when it was below GitHub's 100 MB size limit. Otherwise, it's probably best that you download it directly from the data provider's page. We're also happy to provide these data directly, simple e-mail me at `juancarlos@ucsb.edu`. Please indicate if you have a preferred delivery method.

- Input data are provided in the `clean_data` folder.

- Our output data (supply curves, trade equilibrium etc) are all found under `results` (either `output_data` or `processed_dats`).

- There is a csv file, requested by Science, which contains the data required to produce Figure 1 of the main paper.

### Scripts

- The scripts relevant to the analysis are found under `scripts/03_descriptive_content`, `scripts/04_analysis`, and `scripts/05_figures_and_tables`. Cleaning and processing scripts are also provided for reference.

---------
