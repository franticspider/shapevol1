


#' Create a shape gene
#' 
#' @param att the Attribute, e.g. "DirectionX" or "CrossSection",
#' @param valtyp the Value or Type of the attribute, e.g. 0 or "Circle"
#' @param status the Active status of the gene, False = not working, True =  working
#' @param start the starting and stopping conditions, within which the gene remeains working if it is active
#' @param stop the stopping condition
#' @param dom the dominance
#' @return a dataframe containing the gene
#' @export
#' @examples
#' g1 <- sgene("DirectionX",0,T,0,1.5,1)
sgene <- function(att,valtyp,status=F,start,stop,dom){
  return(data.frame(att=att,valtyp=valtyp,status=status,start,stop,dom,stringsAsFactors = F))
}



#' Create a position entry
#' 
#' @param X the X position
#' @param Y the Y position
#' @param Z the Z position
#' @return a dataframe containing the position info
#' @export
#' @examples
#' pp <- spos(0.0,0.0,0.1)
spos <- function(X,Y,Z){
  return(data.frame(X=X,Y=Y,Z=Z,stringsAsFactors = F))
}


#' Update an array of positions
#'
#' @pram pos an array of new positions
#' @param X the X position
#' @param Y the Y position
#' @param Z the Z position
#' @param err a tolerance value used to eliminate duplicates
#' @return a dataframe containing the position info
#' @export
#' @examples
#' pp <- spos(0.0,0.0,0.1)
addpos <- function(X,Y,Z,pos=NULL,err=0.01){
  tmp <- spos(X,Y,Z)
  if(is.null(pos))
    pos <- tmp
  else{
    elim <- data.frame(X = abs(pos$X-tmp$X), Y = abs(pos$Y-tmp$Y), Z = abs(pos$Z-tmp$Z))
    elim <- elim[elim$X < err & elim$Y <err & elim$Z < err,]
    
    if(nrow(elim)==0)
      pos <- rbind(pos,tmp)
  }
  return(pos)
}