


#' Finds the appropriate row number given the gene number and XYZ position
#' 
#' @param gno the gene number (genes are in multiples of 3),
#' @param xyzno whether to mutate on the X,Y or Z axis
#' @return the row number
#' @examples
#' rno <- getrowno(7,3)
getrowno <- function(gno,xyzno){
  rno <- (3*gno)+xyzno
  return (rno)
}




#' mutate a single gene in a geneset given the parameters of the mutation
#' 
#' @param gno the gene number (genes are in multiples of 3),
#' @param xyzno whether to mutate on the X,Y or Z axis
#' @param mtype the mutation type
#' @param mdir whether to increase or decrease the value
#' @param geneset the set of genes within which to carry out the mutation
#' @param verbose whether to generate messages while processing
#' @return a copy of the input geneset with the mutation
#' @examples
#' mutant <- domut(3,1,2,1,geneset)
domut <- function(gno,xyzno,mtype,mdir,geneset,verbose=T){
  #
  #valtype mutation
  rno <- getrowno(gno,xyzno)
  
  if(verbose)
  message(sprintf("Mutating row %d gno=%d xyzno=%d, mtype = %d, mdir = %d",rno,gno,xyzno,mtype,mdir))
  
  if(mtype == 1){
    if(verbose)message(sprintf("Valtyp Mutation at row %d",rno))
    #if(sample(2,1)==1)
    if(mdir==1)
      mval <-  1
    else
      mval <- -1
    geneset$valtyp[rno] <- as.numeric(geneset$valtyp[rno]) + mval
  }
  
  #start or stop
  #NB! We don't (currently) pass in an equivalent of the mdir value to define the mutation...
  if(mtype == 2 | mtype == 3){
    if(verbose)message("Start/stop Mutation")
    geneset[rno,2+mtype] <- geneset[rno,2+mtype]+(sample(6,1)-4)
  }
  
  #Dominance
  if(mtype ==4){
    #browser()
    if(verbose)message("Dominance Mutation")
    srno <- (gno*3)+1
    dval <- geneset$dom[srno]
    
    if(mdir==1)
      mval <- 1
    else
      mval <- -1
    
    mdval <- dval + mval
    
    if(nrow(geneset[geneset$dom == mdval,])>0){
      if(verbose){
        message("duplicate domval")
      }
      geneset$dom[geneset$dom == mdval] <- dval
    }
    
    for(aa in 1:3){
      geneset$dom[(3*gno)+aa] <- mdval
    }
  }
  
  return(geneset)
}



#' generate a single mutation in a geneset
#'
#' @param geneset the set of genes within which to carry out the mutation
#' @param verbose whether to generate messages while processing
#' @return a copy of the input geneset with the mutation
#' @examples
#' mutant <- mutate.gene(geneset)
mutate.gene <- function(geneset,verbose=F){
  
  #select a genegroup for direction:
  gno <- sample(((nrow(geneset)/3)-1), 1)
  xyzno <- sample(3,1)
  
  #pick a mutant type
  mtype <- sample(4,1)
  
  mdir <- sample(2,1)
  
  geneset <- domut(gno,xyzno,mtype,mdir,geneset,verbose)
  
  return (geneset)
}



#' Get the euclidean distance between two novelty search vectors
#' 
#' @param v1 first vector of values
#' @param v2 second vector of values
#' @return euclidean distance between the vectors
#' @examples
#' dist <- NSdist(v1,v2)
NSdist <- function(v1,v2,cols=c(1:6,8)){
  dist <- 0
  for(cc in 1:length(cols)){
    vv <- cols[cc]
    dist <- dist + ((v1[1,vv]-v2[1,vv])*(v1[1,vv]-v2[1,vv]))
  }
  
  dist <- sqrt(dist)
  return(dist)
  
}