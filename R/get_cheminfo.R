# This function displays the information specified in "info=" for all chemicals for which the PBPK model can be paramterized.
get_cheminfo <- function(info="CAS",species="Human",exclude.fub.zero=NA,fub.lod.default=0.005,model='3compartmentss',default.to.human=F)
{ 
  chem.physical_and_invitro.data <- chem.physical_and_invitro.data
  if(tolower(species) == 'human') species <- 'Human' 
  else if(tolower(species) == 'rat') species <- 'Rat'
  else if(tolower(species) == 'dog') species <- 'Dog'
  else if(tolower(species) == 'rabbit') species <- 'Rabbit'
  else if(tolower(species) == 'mouse') species <- 'Mouse'
  else stop("Only species of human, rat, mouse, rabbit, and dog accepted.")
  
  model <- tolower(model)
  species.fub <- NULL
  species.clint <- NULL
  species.clint.pvalue <- NULL
  if (model == "pbtk" | model == "3compartment" | model == "1compartment" | model == "3compartmentss")
  {
    if(!all(c(paste0(species,'.Funbound.plasma'),paste0(species,'.Clint')) %in% colnames(chem.physical_and_invitro.data)) & !default.to.human) incomplete.data <- T
    else{    
      if(paste0(species,'.Funbound.plasma') %in% colnames(chem.physical_and_invitro.data))  species.fub <- paste0(species,'.Funbound.plasma')
      else{
        species.fub <- 'Human.Funbound.plasma'
        warning('Human values substituted for Funbound.plasma.')
      }
      if(paste0(species,'.Clint') %in% colnames(chem.physical_and_invitro.data)){
        species.clint <- paste0(species,'.Clint')
        species.clint.pvalue <- paste0(species,'.Clint.pValue')
      }else{
        species.clint <- 'Human.Clint'
        species.clint.pvalue <- 'Human.Clint.pValue'
        warning('Human values substituted for Clint and Clint.pValue.')
      }
        necessary.params <- c(species.clint,species.fub,"MW","logP")
        if(is.na(exclude.fub.zero)) exclude.fub.zero <- T
      incomplete.data <- F
    }
  }else if(model == 'schmitt'){
    if(!paste0(species,'.Funbound.plasma') %in% colnames(chem.physical_and_invitro.data) & !default.to.human) incomplete.data <- T
    else{
      if(paste0(species,'.Funbound.plasma') %in% colnames(chem.physical_and_invitro.data))  species.fub <- paste0(species,'.Funbound.plasma')
      else{
        species.fub <- 'Human.Funbound.plasma'
        warning('Human values substituted for Funbound.plasma.')
      }
      if('clint.pvalue' %in% tolower(info) | any(tolower(info) == 'all')){
        if(paste0(species,'.Clint') %in% colnames(chem.physical_and_invitro.data)){
          species.clint <- paste0(species,'.Clint')
          species.clint.pvalue <- paste0(species,'.Clint.pValue')
        }else if(default.to.human){
          species.clint <- 'Human.Clint'
          species.clint.pvalue <- 'Human.Clint.pValue'
          warning('Human values substituted for Clint and Clint.pValue.')
        }else stop('Set default.to.human to TRUE for Clint values with selected species') 
      }   
      necessary.params <- c(species.fub,"logP")
      if (is.na(exclude.fub.zero)) exclude.fub.zero <- T  
      incomplete.data <- F
    } 
  }else stop("Valid models are currently only: pbtk, 1compartment, 3compartment, schmitt, and 3compartmentss.")
  if(!incomplete.data){
    good.chemicals.index <- apply(chem.physical_and_invitro.data[,necessary.params],1,function(x) all(!is.na(x)))
    if (exclude.fub.zero) good.chemicals.index <- good.chemicals.index & (chem.physical_and_invitro.data[,species.fub]>0) 
    good.chemical.data <- chem.physical_and_invitro.data[good.chemicals.index,] 
  
      if('mw' %in% tolower(info)) info <- c('MW',info[tolower(info) != 'mw'])
      if('pka_accept' %in% tolower(info)) info <- c('pKa_Accept',info[tolower(info) != 'pka_accept'])
      if('pka_donor' %in% tolower(info)) info <- c('pKa_Donor',info[tolower(info) != 'pka_donor'])
      if('logp' %in% tolower(info)) info <- c('logP',info[tolower(info) != 'logp'])
      if('compound' %in% tolower(info)) info <- c('Compound',info[tolower(info) != 'compound'])
      if('cas' %in% tolower(info)) info <- c('CAS',info[tolower(info) != 'cas'])
      if('dsstox_substance_id' %in% tolower(info)) info <- c('DSSTox_Substance_Id',info[tolower(info) != 'dsstox_substance_id'])
      if('structure_formula' %in% tolower(info)) info <- c('Structure_Formula',info[tolower(info) != 'structure_formula'])
      if('substance_type' %in% tolower(info)) info <- c('Substance_Type',info[tolower(info) != 'substance_type'])
  
    
    valid.info <- c("Compound","CAS","MW","logP","pKa_Accept","pKa_Donor","logMA","Clint","Clint.pValue","Funbound.plasma","DTXSID","Formula")
  
    if (any(toupper(info)=="ALL")) info <- valid.info
    
    if (any(!(toupper(info) %in% toupper(valid.info)))) stop(paste("Data on",info[!(info %in% valid.info)],"not available. Valid options are:",paste(valid.info,collapse=" ")))
  
    if (toupper("Clint") %in% toupper(info)) info[toupper(info)==toupper("Clint")] <- species.clint
    if (toupper("Clint.pValue") %in% toupper(info)) info[toupper(info)==toupper("Clint.pValue")] <- species.clint.pvalue
    if (toupper("Funbound.plasma") %in% toupper(info)) info[toupper(info)==toupper("Funbound.plasma")] <- species.fub
   
   #if (!toupper(paste(species,"Clint",sep=".")) %in% toupper(colnames(chem.physical_and_invitro.data))) stop(paste("Species",species,"not found."))
      
    columns <- colnames(chem.physical_and_invitro.data)
  #    c("Compound","CAS","MW",paste(species,"Clint",sep="."),paste(species,"Fub",sep="."))
    this.subset <- good.chemical.data[,toupper(colnames(chem.physical_and_invitro.data))%in%toupper(columns)]
    
    if('CAS' %in% info) rownames(this.subset) <- NULL 
    
   # this.subset <- suppressWarnings(this.subset[!is.na(as.numeric(this.subset[,paste(species,"Clint",sep=".")])),])
    #this.subset <- suppressWarnings(this.subset[!is.na(as.numeric(this.subset[,paste(species,"Fub",sep=".")])),])
    if (exclude.fub.zero) this.subset <- suppressWarnings(this.subset[as.numeric(this.subset[,species.fub])!=0,])  
    else this.subset[suppressWarnings(as.numeric(this.subset[,species.fub]) == 0),species.fub] <- fub.lod.default
                                
    data.table <- this.subset
    return.info <- data.table[,info]
  }else return.info <- NULL 
    
  return(return.info)
}