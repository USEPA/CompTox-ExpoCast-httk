---
  title: "Kreutz et al. (2022): Analysis of Plasma Binding Measured by GC"
author: "Anna Kreutz, John F. Wambaugh, and Barbara Wetmore"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{i) Kreutz (2022): PFAS Plasma Binding}
%\VignetteEngine{knitr::rmarkdown}
%\VignetteEncoding{UTF-8}
---
  *Please send questions to wambaugh.john@epa.gov*
  
  from "X"


## Abstract

## HTTK Version

This vignette was created with **httk** v2.3.0. Although we attempt to maintain 
backward compatibility, if you encounter issues with the latest release of 
**httk**
  and cannot easily address the changes, historical versions of httk are 
available from: https://cran.r-project.org/src/contrib/Archive/httk/
  
  ## Prepare for session
```{r}
knitr::opts_chunk$set(collapse = TRUE, comment = '#>')
options(rmarkdown.html_vignette.check_title = FALSE)
```

### Load the relevant libraries
If you get the message "Error in library(X) : there is no package called 'X'"
then you will need to install that package: 
  
  From the R command prompt:
  
  install.packages("X")

Or, if using RStudio, look for 'Install Packages' under 'Tools' tab.

```{r load_packages, eval = FALSE}
# Clear the memory:
rm(list=ls()) 
#
#
# Setup
#
#
library(ggplot2)
library(httk)
```

```{r load_packages, eval = FALSE}
non.pfas <- chem.phys
````