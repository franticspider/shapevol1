


#' Create a shape gene
#' 
#' @param att the Attribute, e.g. "DirectionX" or "CrossSection",
#' @param valtyp the Value or Type of the attribute, e.g. 0 or "Circle"
#' @param status the Active status of the gene, False = not working, True =  working
#' @param start the starting and stopping conditions, within which the gene remeains working if it is active
#' @param stop the stopping condition
#' @param dom the dominance
#' @return a dataframe containing the gene
#' @examples
#' g1 <- sgene("DirectionX",0,T,0,1.5,1)
sgene <- function(att,valtyp,status=F,start,stop,dom){
  return(data.frame(att=att,valtyp=valtyp,status=status,start,stop,dom))
}


