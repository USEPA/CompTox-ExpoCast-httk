CAS.checksum <- function(CAS.string)
{
  test.num <- 0
  multiplier <- 1
  if(is.factor(CAS.string)) CAS.string <- as.character(CAS.string)
  for (i in (nchar(CAS.string)-2):1)
    if (!is.na(as.numeric(substr(CAS.string,i,i))))
    {
      test.num <- test.num + as.numeric(substr(CAS.string,i,i))*multiplier
      multiplier <- multiplier + 1
    }
  if (is.na(test.num%%10 == as.numeric(substr(CAS.string,nchar(CAS.string),nchar(CAS.string))))) return(F)
  return (test.num%%10 == as.numeric(substr(CAS.string,nchar(CAS.string),nchar(CAS.string))))
}


#' Add a paramter value to the chem.physical_and_invitro.data table
#' 
#' This internal function is used by \code{\link{add_chemtable}} to add a single 
#' new parameter to the table of chemical parameters. It should not be typically
#' used from the command line.
#' 
#' @param this.table Object of class data.frame containing one row per chemical.
#' 
#' @param this.CAS The Chemical Abstracts Service registry number (CAS-RN)
#' correponding to the parameter value
#'
#' @param compound.name A name associated with the chemical (defaults to NULL)
#'
#' @param this.property The property being added/modified.
#'
#' @param value The value being assigned to this.property.
#'
#' @param reference This is the reference for the data in the new table. This
#' may be omitted if a column in data.list gives the reference value for each
#' chemical.
#'
#' @param species This is the species for the data in the new table. This may
#' be omitted if a column in data.list gives the species value for each
#' chemical or if the data are not species-specific (e.g., MW).
#'
#' @param overwrite If overwrite=TRUE then data in current.table will be
#' replaced by any data in new.table that is for the same chemical and
#' property. If overwrite=FALSE (DEFAULT) then new data for the same chemical
#' and property are ignored.  Funbound.plasma values of 0 (below limit of
#' detection) are overwritten either way.
#' 
#' @param sig.fig Sets the number of significant figures stored (defaults to 4)
#' 
#' @param clint.pvalue.overwrite If TRUE then the Cl_int p-value is set to NA 
#' when the Cl_int value is changed unless a new p-value is provided. (defaults
#' to TRUE) 
#' 
#' @param allow.na If TRUE (default is FALSE) then NA values are written to the
#' table, otherwise they are ignored.
#'
#' @return \item{data.frame}{A new data.frame containing the data in
#' current.table augmented by new.table} 
#' @author John Wambaugh
augment.table <- function(
  this.table,
  this.CAS,
  compound.name=NULL,
  this.property,
  value,
  species=NULL,
  reference,
  overwrite=FALSE,
  sig.fig = 4,
  clint.pvalue.overwrite=TRUE,
  allow.na=FALSE)
{
  # Columns stored in chem.phys_and_invitro.table:
  CHEM.ID.COLS<-c(
    "Compound",
    "CAS",
    "CAS.Checksum",
    "DTXSID",
    "Formula",
    "SMILES.desalt")
  CHEM.PHYS.COLS<-c(
    "MW",
    "logP",
    "logPwa",
    "logHenry",
    "logVP",
    "logWSol",
    "MP",
    "pKa_Donor",
    "pKa_Accept",
    "logMA")
  # These colmuns have "[SPECIES]." prepended to their name:
  CHEM.INVITRO.COLS <- c(
    "Clint",
    "Clint.pValue",
    "Funbound.plasma",
    "Fgutabs",
    "Rblood2plasma"
    ,"Caco2.Pab")
  # In the table we create, each word in most column names is capitalized:
  COLUMN.CAPITALIZATION.EXCEPTIONS <- c(
    "Clint.pValue",
    "logP",
    "logMA",
    "logPwa",
    "logHenry",
    "logWSol",
    "MP",
    "MW",
    "CAS",
    "CAS.Checksum",
    "pKa_Donor",
    "pKa_Accept",
    "SMILES.desalt",
    "DTXSID",
    "Formula",
    "Caco2.Pab")
  # Many data are converted to numeric,but sometimes we want to preserve strings:
  AS.NUMERIC.EXCEPTIONS <- c(
    "pKa_Donor",
    "pKa_Accept",
    "SMILES.desalt",
    "Funbound.plasma",
    "Clint",
    "DTXSID",
    "SMILES.desalt",
    "Formula",
    "Caco2.Pab")
  
  if (!is.na(value)) if (value == "") value <- NA
  if (!is.na(value) | allow.na)
  {
    if (tolower(this.property) %in% tolower(COLUMN.CAPITALIZATION.EXCEPTIONS))
    {
      this.property <- COLUMN.CAPITALIZATION.EXCEPTIONS[
        tolower(COLUMN.CAPITALIZATION.EXCEPTIONS)==tolower(this.property)]
    } else {
      this.property <- tolower(this.property)
      substring(this.property,1,1) <- toupper(substring(this.property,1,1))
    }
  
    data.cols <- c(
      "Reference",
      "Species",
      CHEM.ID.COLS,
      CHEM.PHYS.COLS,
      CHEM.INVITRO.COLS)
    if (!(tolower(this.property) %in% tolower(data.cols)))
      stop(paste("Parameter", this.property,
        "not matched by columns in our data table."))
    
  # If one of the species-specific parameters is being set (that is, anything in
  # chem.invitro.cols) then species must either be set for the whole table or read
  # in from a column in that table:
    if ((this.property %in% CHEM.INVITRO.COLS) & is.null(species)) stop("Either \
  argument \"species\" must be set for whole table or \"Species\" must be \
  matched to a \"new.table\" column in argument \"data.list\".")
  
    if (!is.null(compound.name))
    {
      compound.name <- tolower(compound.name)
      substring(compound.name,1,1) <- toupper(substring(compound.name,1,1))
      compound.name <- iconv(compound.name, from="UTF-8", to='ASCII//TRANSLIT')
    } else {
      compound.name <- this.CAS
    }
  
    this.property.nospecies <- this.property
    if (this.property %in% CHEM.INVITRO.COLS)
    {
      if (!is.null(species))
      {
        species <- tolower(species)
        substring(species,1,1) <- toupper(substring(species,1,1))
      }
      this.property<-paste(species,this.property,sep=".")
    }
    if (is.na(this.CAS)) return(this.table)
    if (!(this.CAS %in% this.table[,"CAS"]))
    {
      if (is.null(compound.name)) compound.name <- this.CAS
      if (!is.null(this.table))
      {
        this.row <- this.table[1,]
        this.row[] <- NA
      } else {
        this.row <- as.data.frame(compound.name,stringsAsFactors=FALSE)
        colnames(this.row) <- "Compound"
      }
      this.row[,"Compound"] <- compound.name
      this.row[,"All.Compound.Names"] <- compound.name
      this.row[,"CAS"] <- this.CAS
      this.row[,"CAS.Checksum"] <- CAS.checksum(this.CAS)
      if (!is.null(species)) this.row[,"All.Species"] <- species
      else this.row[,"All.Species"] <- "None"
  #    if (!is.null(this.table))
  #    {
  #      this.row <- cbind(this.row,t(as.data.frame(rep(NA,dim(this.table)[2]-5))))
  #      colnames(this.row) <- colnames(this.table)
  #    } else colnames(this.row) <- c("Compound","All.Compound.Names","CAS","CAS.Checksum","All.Species")
      rownames(this.row) <- this.CAS
      this.table <- rbind(this.table,this.row)
    }
  #  if (!(this.property %in% chem.prop.cols)) stop(paste(this.property,"not a valid property"))
    index <- which(this.table[,"CAS"]==this.CAS)
    if (!is.null(compound.name))
    {
      if (!(compound.name %in% 
        strsplit(this.table[index,"All.Compound.Names"],"[|]")[[1]]))
      {
        this.table[index,"All.Compound.Names"] <- 
          paste(this.table[index,"All.Compound.Names"],compound.name,sep="|")
      }
    }
    if (!is.null(species))
    {
      if (this.property %in% paste(species,CHEM.INVITRO.COLS,sep="."))
      {
        if (!(species %in% 
          strsplit(this.table[index,"All.Species"],"[|]")[[1]]))
        {
          if (this.table[index,"All.Species"]=="None") 
          {
            this.table[index,"All.Species"] <- species
          } else {
            this.table[index,"All.Species"] <- 
              paste(this.table[index,"All.Species"],species,sep="|")
          }
        }
      }
    }
    if (!(this.property %in% colnames(this.table)))
    {
      this.table[,this.property] <- NA
      ref.name <- paste(this.property,"Reference",sep=".")
      this.table[,ref.name] <- NA
    }
    if (is.na(this.table[index,this.property]) | 
      overwrite | 
      (this.property == paste(species,'Funbound.plasma',sep=".") & 
        this.table[index,this.property] == 0))
    {
      if (!(this.property.nospecies %in% AS.NUMERIC.EXCEPTIONS))
      {
        this.table[index,this.property] <- signif(as.numeric(value), sig.fig)
      } else {
        if (class(this.table[,this.property])!='character')
        {  
          this.table[,this.property] < as.character(this.table[,this.property])
        }
# Check to see if this is actually a number and we can use sig figs:        
        if (!is.na(as.numeric(value)))
        {
          if (as.character(as.numeric(value)) == as.character(value))
          {
            value <- signif(as.numeric(value), sig.fig)
          }
        }
        this.table[index,this.property] <- as.character(value)
      }
# If Clint is changed, the Clint.pvalue should usually be eliminated:
      if (this.property.nospecies == "Clint" & clint.pvalue.overwrite)
      {
        this.table[index,paste(this.property,".pValue",sep="")] <- NA
        this.table[index,paste(this.property,".pValue.Reference",sep="")] <- NA
      }
# There is a difference between no pKa prediction available, a prediciton of no pKa:
      if(!is.na(this.table[index,this.property]) & 
        this.table[index,this.property] == "" & 
        this.property %in% c("pKa_Donor","pKa_Accept"))
      {
        this.table[index,this.property] <- NA
      }
      ref.name <- paste(this.property,"Reference",sep=".")
      this.table[index,ref.name] <- reference
    }
    CHEM.PHYS.COLS <- sort(c(
      CHEM.PHYS.COLS,
      paste(CHEM.PHYS.COLS,"Reference",sep=".")))
    col.order <- c(
      CHEM.ID.COLS[CHEM.ID.COLS %in% colnames(this.table)],
      "All.Compound.Names",
      CHEM.PHYS.COLS[CHEM.PHYS.COLS %in% colnames(this.table)],
      "All.Species")
    col.order <- c(
      col.order,
      sort(colnames(this.table)[!(colnames(this.table) %in% col.order)]))
    
    if (any(is.na(this.table[,"All.Species"]))) browser()
    return(this.table[,col.order])
  } else {
    return(this.table)
  }
}


#' Add a table of chemical information for use in making httk predictions.
#' 
#' This function adds chemical-specific information to the table
#' chem.physical_and_invitro.data. This table is queried by the model
#' parameterization functions when attempting to parameterize a model, so
#' adding sufficient data to this table allows additional chemicals to be
#' modeled.
#' 
#' 
#' @param new.table Object of class data.frame containing one row per chemical,
#' with each chemical minimally described by a CAS number.
#' 
#' @param data.list This list identifies which properties are to be read from
#' the table. Each item in the list should point to a column in the table
#' new.table. Valid names in the list are: 'Compound', 'CAS', 'DSSTox.GSID'
#' 'SMILES.desalt', 'Reference', 'Species', 'MW', 'logP', 'pKa_Donor',
#' 'pKa_Accept', 'logMA', 'Clint', 'Clint.pValue', 'Funbound.plasma',
#' 'Fgutabs', 'Rblood2plasma'.
#' 
#' @param current.table This is the table to which data are being added.
#' 
#' @param reference This is the reference for the data in the new table. This
#' may be omitted if a column in data.list gives the reference value for each
#' chemical.
#' 
#' @param species This is the species for the data in the new table. This may
#' be omitted if a column in data.list gives the species value for each
#' chemical or if the data are not species-specific (e.g., MW).
#' 
#' @param overwrite If overwrite=TRUE then data in current.table will be
#' replaced by any data in new.table that is for the same chemical and
#' property. If overwrite=FALSE (DEFAULT) then new data for the same chemical
#' and property are ignored.  Funbound.plasma values of 0 (below limit of
#' detection) are overwritten either way.
#' 
#' @param sig.fig Sets the number of significant figures stored (defaults to 4)
#' 
#' @param clint.pvalue.overwrite If TRUE then the Cl_int p-value is set to NA 
#' when the Cl_int value is changed unless a new p-value is provided. (defaults
#' to TRUE) 
#'
#' @param allow.na If TRUE (default is FALSE) then NA values are written to the
#' table, otherwise they are ignored.
#'
#' @return \item{data.frame}{A new data.frame containing the data in
#' current.table augmented by new.table} 
#' @author John Wambaugh
#' @examples
#' 
#' \donttest{
#' my.new.data <- as.data.frame(c("A","B","C"),stringsAsFactors=FALSE)
#' my.new.data <- cbind(my.new.data,as.data.frame(c("111-11-2","222-22-0","333-33-5"),
#'                      stringsAsFactors=FALSE))
#' my.new.data <- cbind(my.new.data,as.data.frame(c("DTX1","DTX2","DTX3"),
#'                     stringsAsFactors=FALSE))
#' my.new.data <- cbind(my.new.data,as.data.frame(c(200,200,200)))
#' my.new.data <- cbind(my.new.data,as.data.frame(c(2,3,4)))
#' my.new.data <- cbind(my.new.data,as.data.frame(c(0.01,0.02,0.3)))
#' my.new.data <- cbind(my.new.data,as.data.frame(c(0,10,100)))
#' colnames(my.new.data) <- c("Name","CASRN","DTXSID","MW","LogP","Fup","CLint")
#' 
#' chem.physical_and_invitro.data <- add_chemtable(my.new.data,
#'                                   current.table=chem.physical_and_invitro.data,
#'                                   data.list=list(
#'                                   Compound="Name",
#'                                   CAS="CASRN",
#'                                   DTXSID="DTXSID",
#'                                   MW="MW",
#'                                   logP="LogP",
#'                                   Funbound.plasma="Fup",
#'                                   Clint="CLint"),
#'                                   species="Human",
#'                                   reference="MyPaper 2015")
#' parameterize_steadystate(chem.name="C")  
#' calc_css(chem.name="B")                                
#' }
#' 
#' @export add_chemtable
add_chemtable <- function(
  new.table, 
  data.list, 
  current.table=NULL, 
  reference=NULL,
  species=NULL, 
  overwrite=FALSE,
  sig.fig = 4,
  clint.pvalue.overwrite=TRUE,
  allow.na=FALSE)
{
# Let's make the capitalization consistent in data.list:
  exceptions <- c("Clint.pValue","logP","logPwa","logMA","logHenry","logWSol","MP","MW","CAS","CAS.Checksum","pKa_Donor","pKa_Accept","SMILES.desalt","DTXSID","Formula","Caco2.Pab")
  for (this.name in names(data.list))
  {
    if (tolower(this.name) %in% tolower(exceptions)) this.name <- exceptions[tolower(exceptions)==tolower(this.name)]
    else {
      this.name <- tolower(this.name)
      substring(this.name,1,1) <- toupper(substring(this.name,1,1))
    }
  }
  
# We either need to have reference set for the whole table, or read in from a
#column from that table:
  if (is.null(reference) & !("Reference" %in% names(data.list))) stop("Either \
argument \"reference\" must be set for whole table or \"Reference\" must be \
matched to a \"new.table\" column in argument \"data.list\".")

  if (!is.null(reference) & ("Reference" %in% names(data.list))) stop("Reference\
 cannot be specifed by both \"reference=\" and \"data.list\".")

  if (!is.null(species) & ("Species" %in% names(data.list))) stop("Species\
 cannot be specifed by both \"species=\" and \"data.list\".")
  if (!is.null(species)) this.species <- species

  if (!any(c("CAS") %in% names(data.list))) stop("\"CAS\" must be one of the \
columns in \"data.list\".")

# Identify which entries in data.list are being added to the table:
  new.data <- names(data.list)[!(names(data.list) %in% c("CAS","Compound","Reference","Species"))]
  if (!is.null(reference)) this.reference <- reference
  for (this.row in 1:dim(new.table)[1])
  {
    this.CAS <- new.table[this.row,data.list[["CAS"]]]
    if ("Compound" %in% names(data.list))
    {
      this.compound <- tolower(new.table[this.row,data.list[["Compound"]]])
    }
    else this.compound <- NULL
    if (is.null(reference)) this.reference <- new.table[this.row,data.list[["Reference"]]]
    if (is.null(species))
    {
      if ("Species" %in% names(data.list))
      {
        this.species <- tolower(new.table[this.row,data.list[["Species"]]])
        substring(this.species,1,1) <- toupper(substring(this.species,1,1))
      } else {
        this.species <- NULL
      }
    }
    for (this.data in new.data)
    {
      if (!(data.list[[this.data]] %in% colnames(new.table))) stop(paste(data.list[[this.data]],
        "is not a column in the new table."))
      current.table <- augment.table(
        current.table,
        this.CAS,
        this.compound,
        this.property=this.data,
        value=new.table[this.row, data.list[[this.data]]],
        reference=this.reference,
        species=this.species,
        overwrite=overwrite,
        sig.fig=sig.fig,
        clint.pvalue.overwrite=clint.pvalue.overwrite,
        allow.na=allow.na)
    }
  }

  return(current.table)
}
