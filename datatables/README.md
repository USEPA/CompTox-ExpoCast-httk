<img src="../httk/man/figures/httk-logo.png" align="right" width="50%"/>

# Data and Processing Scripts for "httk" Parameters

Until we have a production version of a database, we rely on a script to compile HTTK data (including in vitro measurements, in vivo measurements, and physico-chemical properties) from an ever-growing list of various papers. The script and supporting data are stored in their own Git repository:

https://CCTE-bitbucket.epa.gov/scm/httkdat/httkdatatables.git

The script load_package_data_tables.R is used to add each tables data using the function add_datatable. The script generates a Tables.RData file (to be placed within the “httk/data” directory), that contains most of the relevant chemical-specific data. In particular, the table chem.physical_and_invitro.data contains the physico-chemical properties and in vitro TK data for each chemical. The data from each source should be maintained in the Git repository. The script also creates a sysdata.rda file (to be placed within the “httk/R” directory) contains data used internally by the R functions but that is not exported to the user. Finally, the script generates text versions of key tables to allow Git to more easily track the changes (the binary files are backed up but change tracking is unavailable).

If you add data to the files, make sure to run:

```library(tools)
add_datalist(".",force=T)
```

## Data Curation

  There are two types of parameters in httk – physiological and chemical. Physiological parameters are specific to a model and species (and potentially a sex or life-stage). Most pre

All data are stored in a git repository called “httkdatatables”. Running the script “load_package_data_tables.R” creates .RData files that are then moved into the “httk/data” directory for incorporation into the package.

Roxygen makes use of the file “httk/R/data.R” to generate documentation for all data included in the package, and this file should be updated regularly to match updates to the data.

Physiology Data Extracted from Peer-Reviewed Literature

To date all the physiological parameters for the toxicokinetic models have come from the peer reviewed literature. These data are stored in a file “pkdata.xlsx”. Individual sheets from this file are read by load_package_data.tables.R. The contents of the sheets are then written to text files so that changes in them may be managed with git. Each parameter added should include the reference from where that parameter was obtained. The reference should be stored in the “pkdata.xlsx” file and added to the “httk/R/data.R” file for the user documentation.

### Physico-Chemical Properties

All the httk models require chemical-specific physicochemical properties such as hydrophobicity (Pow) and ionization equilibria (pKa). We calculate these using OPEn structure–activity/property Relationship App (OPERA)  which we access via the CompTox Chemicals Dashboard (https://comptox.epa.gov/dashboard). As part of running ““load_package_data_tables.R” a list of chemical identifiers is generated. These identifiers are then used with the CompTox Chemicals Dashboard batch search (https://comptox.epa.gov/dashboard/dsstoxdb/batch_search) to download physico-chemical property predictions. Batch search from the das

### New Chemical-specific Data
There is a separate SOP for “HTTK Data Curation”, so in the following two sections we provide only a brief overview. In general, each chemical is described by multiple in vitro measurements, at a minimum hepatic clearance (metabolism by liver cells) and fraction unbound in plasma. We obtain these data from multiple sources.

### Curating in vitro TK Data
Most of the data in the HTTK package initially came from the Rotroff et al. (2010), Wetmore et al. (2012), and Wetmore et al. (2015) publications, with a new bolus from EPA HTTK contract research. However, there are many examples of papers in the peer reviewed literature that provide relevant information. The data of particular interest are:
*	fraction unbound in plasma
*	intrinsic hepatic clearance (hepatocyte incubation assay)
*	fraction bioavailable
*	blood:plasma ratio
*	volume of distribution

As we expand the model, we may eventually also want:
*	blood:air ratio
*	metabolic KM, Vmax for a Michaelis-Menten equation
Often a paper will only provide some of these values. One of the things the function get_cheminfo() checks is whether the combination of data from all sources is sufficient to run a given model.

These data could be from:
* A contractor to EPA (such as Cyprotex)
*	A collaborator (such as the Wetmore lab, JRC, or Health Canada)
*	The Peer-Reviewed Literature, see: https://www.dropbox.com/sh/cihtmmcgkgbblvr/AABLLnX4g70b6HV-2V23aErqa?dl=0

For each we need to create a text file (or files) with the data from the paper in the httk-datatables repository. It helps to add DTXSID's to these tables using the batch search feature from the CompTox chemicals dashboard (so that we know which chemicals we have). We already have a large number of chemicals with Fup from the Lombardo 2018 paper, but until we have Clint we cant make most predictions. So, even if a new paper only has Clint its worth trying to pull the data.

Each paper may have an HTML version on-line where it may be easier to extract the data, but PDF's are also attached. In some cases I was able to download CSV version of the tables.
Each table can be loaded into HTTK by adding a call to add_chemtable in the load_package_data_tables.R script in httk-datatables. add_chemtable treats the first data it sees preferentially, so we want papers from the external literature to be added after the EPA papers (Wetmore 2012, 2015 and Wambaugh 2019). 
 
 