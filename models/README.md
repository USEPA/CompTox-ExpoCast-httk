<img src="../httk/man/figures/httk-logo.png" align="right" width="50%"/>

#  MCSim and C Models for HTTK

# Organization

Files relevant to each model are kept in their own sub-directory. For example,
the base HT-PBTK model resides in httk-models/ptbk. In that directory there are four
key files:

* modelpbtk.model - The MCSim description of the dynamic model
* modelpbtk-raw.c - The C translation from MCsim, without modification
* modelpbtk.c - The C translation with key functions renamed so as to avoid collisions
  when other models are added to HTTK. This file should be identical to what is in
  httk/src.
* modelinfo_pbtk.R - This R code populates the entries for PBTK in the model.list. HTTK
  core functions are agnostic as to how many models there are, they just interact with
  whatever is in model list. This code, if placed in httk/R makes the package aware of
  the model.
* solve_pbtk.R - This R function calls HTTK function solve_model with arguments better
  tailored to running the specific HT-PBTK model. solve_model.R (provided for 
  reference in the main directory) is a "Swiss Army knife" function with more options
  than most models needs.

# Model Integration Procedure:

First, create .model file and type in command prompt (with MCSIM/mod in path): 

```
mod -R <name>.model <name>-raw.c
```

This generates the <name>.c and <name>_inits.R files.

Delete the *ints.R file -- we do not need it.

In order to avoid duplicate names, function names in these newly created files 
must be changed.

The changes (adding _<name>, an arbitrary choice) in the initialization file 
include: initparms_<name>, getParms_<name> (calling function from .c within 
initparms argument), Outputs_<name>, and initState_<name>.

The changes in the .c file (when 'forcings' are used) include: 
* Updating the names of C functions, including initmod_<name>,
 initforc_<name>, getParms_<name>, derivs_<name>, jac_<name>, event_<name>,
 and root_<name>.
* Removing or commenting out, using "//", C functions under "Function
 definitions for delay differential equations" and "Calling R code will
 ensure that input y has same dimension as yini".
    
The changes in the .c file (when 'forcings' are *not* used) include:
* Updating the names of C functions, including initmod_<name>, initforc_<name>
 (if not deleting or commenting out),  getParms_<name>, derivs_<name>,
 jac_<name>, event_<name>, and root_<name>. 
* Removing or commenting out, using "//", C functions under "Forcing (Input)
 functions", "Function definitions for delay differential equations", and
 "Calling R code will ensure that input y has same dimension as yini".

These function names then must also be changed in the corresponding 
solve_pbtk function, specifically the state and parameter initilaizers as 
well as the function calls in the function ode.

Parameter names passed to initparams must match those in .model file.

To add a model to the httk R package, move the model_[MODEL].c file (with function 
names changed) to the httk/src directory and add the modelinfo_[MODEL] to the httk/R
directory and then reinstall the package.

## Questions
If you have questions, comments, or concerns please contact:

### Principal Investigator 
John Wambaugh [wambaugh.john@epa.gov]

### Lead Software Engineer 
Sarah Davidson [Davidson.Sarah.E@epa.gov]