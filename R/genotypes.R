
#' Get and format genotype map of an experiment
#' @param fn filename of genotypemap usually stored at data/genotype_map.csv in the experiment folder
#' @param wtcode string used to identify wild-type
#' @param gtypeorder vector of genotype levels in custom order
#' @return tibble, with gtype as a factor. 
#' @import forcats
#' @export
#' @details The standard columns for this file are plantbarcode, roi, gtype and should map each unique position in the hotel to a genotype
#' the genotypemap is part of every experiment and maps the barcode and roi # with the genotype.

get_genotypemap <- function(fn,
                            wtcode = 'WT',
                            gtypeorder = NULL) {
  
  gtypemap <- read_csv(fn, col_types = cols('gtype' = 'f'))
  
  assertthat::assert_that(wtcode %in% gtypemap$gtype,  msg = glue::glue('The `wtcode` {wtcode} is not found in the genotypemap. are you using a different code for wild-type?'))
  
  if (is.null(gtypeorder)) {
    
    gtypemap %>%
      mutate(gtype = fct_relevel(gtype, sort),
             gtype = fct_relevel(gtype, {{wtcode}}, after = 0)) #putting WT to the front of the list so we can color it black always
    
  } else {
    
    gtypemap %>%
      mutate(gtype = fct_relevel(gtype, gtypeorder))
  }
}


#' Define standard colors for genotypes
#' @param gtypemap dataframe of the genotypemap
#' @param colorpal optional color palette to use for the genotypes. 
#' @return named vector of colors with genotypes as names
#' @export
#' @details 
#' This should be run after `get_genotypemap()` which takes filename for genotypemap and creates and orders gtype levels
#' Each set of genotypes are to be given a custom color with wild-type 'black'. The output of this function is designed to be used with scale_<color/fill>_manual values argument. The colors of the genotypes will be consistent even if only a subset of genotypes are plotted. 
#' The default colors are 'black' for wild-type and then RColorBrewer palettes Set1 and Pastel1

genotype_colors <- function(gtypemap, colorpal=NULL){
  
  u_gtypes = levels(gtypemap$gtype)
  n_gtypes = length(u_gtypes)
  # default behavior. 'black' is always WT
  if(is.null(colorpal) & n_gtypes <= 19){
    gtypeColors = c('black',RColorBrewer::brewer.pal(9,'Set1'),RColorBrewer::brewer.pal(9,'Pastel1'))
    names(gtypeColors) <- u_gtypes
  } else if(is.null(colorpal) & n_types>19){
    stop('You have more than 19 genotypes. You will need to provide your own color palette using the `colorpal` argument.')
  } else {
    gtypeColors = colorpal
    names(gtypeColors) <- u_gtypes
  }
  
  return(gtypeColors) 
}




#' Filter genotypes from experiment for the analysis
#' @param gtypemap dataframe of the genotypemap
#' @param plantprefix prefix for filename that include tray and roi #s to be included. 'all' is preserved for no filtering.
#' @param rejectgtypes logical. default=FALSE. TRUE means the complementary set of genotypes specified by plantprefix file will be analyzed.
#' @export
#' @details 
#' this should be run after `get_genotypemap()`
#' the plantprefix is part of `<plantprefix>_gtypes_[A-Z].csv`. Each such file with a common plantprefix identifies the poster child individuals that get used for a collage. The suffix A,B,... are to allow multiple columns of genotypes in the collage. All suffixed files are read in this function.

filter_gtype <- function(gtypemap, plantprefix, rejectgtypes=F ){
  
  if(plantprefix!='all'){  
    fns = fs::dir_ls(datadir,regex=glue::glue('{plantprefix}_plants_[A-Z].csv$'))
    example_plants = purrr::map_df(fns, .f=read_csv)
    gtypekeep = inner_join(example_plants, gtypemap) 
    
  } else {
    gtypekeep = gtypemap 
  }
  if(rejectgtypes){
    # can't look at reject genotypes if there is no filter 
    if(plantprefix=='all'){
      stop('you asked for reject genotypes but did not filter the genotypes so there will be nothing to plot.')
    }
    gtypekeep <- gtypemap %>% 
      anti_join(gtypekeep, by='gtype') %>% 
      add_row(gtype = 'Col-0') %>% 
      mutate(gtype = factor(gtype,levels=levels(gtypemap$gtype)))
  }
  
  gtypekeep <- gtypekeep %>% 
    select(gtype) %>% 
    distinct(gtype) %>% 
    mutate(gtype_c = as.character(gtype))
  
  return(gtypekeep)
  
}
