#' Format plot labels with italics for mutant names
#' @param names character vector of names
#' @export
#' @details designed to be used in scale_*_* for the labels argument. wild-type can be 'WT' or 'Col-0'

italic_labels <- function(names){
  if(is.factor(names)){
    names = as.character(names)
  }
  sapply(strsplit(names,'[. ]'), 
         function(x) {
           if( all(x != 'WT' & x != 'Col-0' & length(x)>1)){
             x=tolower(x)
             # parse(text = paste0(x[1],".","italic(", tolower(x[2]), ")"))
             bquote(paste(.(x[1]),".",italic(.(x[2]))))
             # substitute(paste(treatment,".",italic(gtype), list(treatment = tolower(x[1]) , gtype=tolower(x[2]) ) ))
           } else if(all(x!='WT' & x!='Col-0' & length(x)==1)){
             x=tolower(x)
             substitute(italic(gtype), list(gtype=tolower(x) ) )
             # parse(text = paste0("italic(", tolower(x), ")"))
           } else if(length(x)>1){
             paste(x[1],x[2],sep='.')
           } else {
             x
           }
         })}



#' Dark cowplot theme
#' @param bkgrdcol valid color for background. default = 'grey40'
#' @param ... arguments for `theme_cowplot()`
#' @export
#' @details see `theme_cowplot()` for input arguments.
#' panel.background is set to grey40 to better accommodate black and light colors when number of genotypes is >9
#' legend.key is also set to grey40
theme_darkcowplot <- function(bkgrdcol = 'grey40', ...){
  cowplot::theme_cowplot(...) +
    theme(panel.background = element_rect(fill=bkgrdcol),
          legend.key = element_rect(fill=bkgrdcol))}
