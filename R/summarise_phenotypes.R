#' Summarise phenotypes
#' @param dF dataframe of phenotypes for every individual
#' @param phenotype column name or vector of column names of phenotypes to summarise. unquoted.
#' @param addgrp additional columns to group by. column name or vector of column names
#' @param statistic named list of function(s) to summarise with. name of your list entry will be used to name the new column 
#' @param na.rm logical. remove rows with NA. default is TRUE. 
#' @export
#' @import rlang
#' @return dataframe with summary columns named as `{statistic}_{phenotype}`
#' @details the default is to group by `gtype`, `idate`, `jobdate`. For PSII data you need to additionally specify `parameter`
   
   
summarise_phenotypes <- function(dF=NULL, phenotype=NULL, addgrp=NULL, statistic = list(med=median, sd=sd, se=se), na.rm=TRUE){
  
  assertthat::assert_that(!is.null({{phenotype}}), msg = 'You need to specify the column to summarise')
  assertthat::assert_that(all(c('gtype','idate','jobdate') %in% colnames(dF)), msg = 'Your dataframe is missing 1 or more required columns (gtype, idate, jobdate)')
  
  if(na.rm){
    dF <- na.omit(dF)
  }
  
  
  dFsumm <- dF %>%
    group_by(gtype, idate, jobdate, {{addgrp}}) %>%
    summarise(across({{phenotype}}, {{statistic}}, .names = "{fn}_{col}" )) 
  
  if(length(phenotype)==1){
    dFsumm %>%
      rename_with(.cols=everything(), .fn = function(x,y){     
      str_replace(x,glue::glue('_{y}'),'')
    }, phenotype)
  } else {
    dFsumm
  }
}


#' Standard Error
#' @param x numeric vector
#' @export
se <- function(x, ...){
  sd(x,...)/sqrt(length(x))
}