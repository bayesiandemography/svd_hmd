---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# svd_hmd

Use data from the [Human Mortality Database](https://www.mortality.org/Home/Index) to construct summary measures of age-sex patterns in human mortality. The measures are constructed by applying a singular value decomposition to logged mortality rates, and then rescaling. For details, see [link to paper].


## Contents

**Makefile** - A [makefile](https://www.gnu.org/software/make/) that can be used to run the scripts in **src** in the correct order.

**data** - A folder with the data. If you would like to replicate the calculations, you will need to populate this folder by downloading zipped files from the Statistics column of the Previous Versions table [here](https://www.mortality.org/Data/ZippedDataFiles). (Requires registering as a Human Mortality Database user.)

**out** - Folder for output from the calculations. Only the final outputs are stored on GitHub.

**src** - R code to do the calculations.

## Format of the output files





