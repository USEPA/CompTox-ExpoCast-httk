# How to build a new pbtk model from .model files:
#First, create .model file and type in command prompt (with MCSIM/mod in path): mod -R <name>.model <name>.c
#This generates the <name>.c and <name>_inits.R files.
#In order to avoid duplicate names, function names in these newly created files must be changed.
#The changes (adding _<name>, an arbitrary choice) in the initialization file include: initparms_<name>, getParms_<name> (calling function from .c within initparms argument), Outputs_<name>, and initState_<name>.
#The changes in the .c file include: initmod_<name>, initforc_<name> (or delete function), getParms_<name>, derivs_<name>, jac_<name>, event_<name>, and root_<name>.
#These function names then must also be changed in the corresponding solve_pbtk function, specifically the state and parameter initilaizers as well as the function calls in the function ode.
#Parameter names passed to initparams must match those in .model file.