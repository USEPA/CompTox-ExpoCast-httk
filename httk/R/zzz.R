  #R CMD CHECK throws notes about "no visible binding for global variable", for
  #each time a data.table column name is used without quotes. This quiets
  #concerns of R CMD check re: the .'s that appear in pipelines

if(getRversion() >= "2.15.1") 
{
  utils::globalVariables(c(".","chem.physical_and_invitro.data","well_param"))
}

## assume your package has some code that assigns ".obj1" and ".obj2"
## but not in a way that codetools can find.
## In the same source file (to remind you that you did it) add:
#if(getRversion() >= "2.15.1")  utils::globalVariables(c(".obj1", "obj2"))

## To suppress messages about a run-time calculated native symbol, 
## save it to a local variable.

## At top level, put this:
#if(getRversion() >= "3.1.0") utils::suppressForeignCheck("localvariable")

## Within your function, do the call like this:
#localvariable <- if (condition) entry1 else entry2
#.Call(localvariable, 1, 2, 3)

## HOWEVER, it is much better practice to write code
## that can be checked thoroughly, e.g.
#if(condition) .Call(entry1, 1, 2, 3) else .Call(entry2, 1, 2, 3)

