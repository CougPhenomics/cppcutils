
#' Isolate a reference phenotype
#'
#' @param refcol column name 
#' @param refvalue string to identify reference
#' @param ... additional columns to keep in the outpur
#' @return dataframe

reference_phenotype <- function(dF, statcol_central= 'med', statcol_spread = 'sd', refcol = 'gtype', refvalue = 'WT', ...){
  
  dF %>%
    ungroup %>% 
    filter(refcol==refvalue) %>%
    select(idate, jobdate, -gtype, refcentral = {{statcol_central}}, refspread={{statcol_spread}}, ...)
  
}

#' Compute a relative phenotype change
#' 
#' @param dF dataframe for which to compute the change
#' @param chgmetric string to quantify change. current option is log(x,avg,base=2)
#' @export
#' @return dataframe
#' @details this function is called after `summarise_phentypes()` for a single phenotype

compute_change <- function(dF, statcol_central, statcol_spread, refcol, refvalue, chgmetric=NULL){
  
  if(is.null(chgmetric)){
    chgmetric <- function(x,xavg){log(x/xavg,base=2)}
  }
  
  avgdF <- reference_phenotype(dF, refcol, refvalue)
  
  full_join(dF, 
            avgdF, 
            by=c('idate','jobdate')) %>% 
    mutate(chg = log({{statcol_central}}/refcentral,base=2))
  
  
}
