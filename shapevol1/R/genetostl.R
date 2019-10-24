



geneval <- function(genes,att,pos){
  
  gd <- gene[gene$att == att,]
  
  
}


printvertex <- function(v){
  
  sprintf("vertex %0.2f %0.2f %0.2f\n",as.numeric(v[1]),as.numeric(v[2]),as.numeric(v[3]))
  
}



#Draw what we have
draw_gene_to_stl <- function(pos,cs,len,dia){

  cat("facet normal 0 0 0\n")
  cat("outer loop\n")
  cat(printvertex(pos+c(-dia,0,0)))  
  cat(printvertex(pos+c(dia,0,0)))  
  cat(printvertex(pos+c(0,0,len)))  
  
  cat("endloop\n")
  cat("endfacet\n")
}

drawCStoSTL <- function(pos,cs,len,dia){
  
  if(cs == "Square"){
        
        #Face 1 Lower left triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos+c(-dia, dia,0)))  
        cat(printvertex(pos+c( dia, dia,0)))
        cat(printvertex(pos+c(-dia, dia,len)))
        cat("endloop\nendfacet\n")    
        
        #upper right triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos+c(-dia, dia,len)))  
        cat(printvertex(pos+c( dia, dia,len)))
        cat(printvertex(pos+c( dia, dia,0)))
        cat("endloop\nendfacet\n")    
        
        #Face 2 Lower left triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos+c( dia,-dia,0)))  
        cat(printvertex(pos+c( dia, dia,0)))
        cat(printvertex(pos+c( dia,-dia,len)))
        cat("endloop\nendfacet\n")    
        
        #upper right triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos+c( dia,-dia,len)))  
        cat(printvertex(pos+c( dia, dia,len)))
        cat(printvertex(pos+c( dia, dia,0)))
        cat("endloop\nendfacet\n")    
        
        #Face 3 Lower left triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos+c(-dia,-dia,0)))  
        cat(printvertex(pos+c( dia,-dia,0)))
        cat(printvertex(pos+c(-dia,-dia,len)))
        cat("endloop\nendfacet\n")    
        
        #upper right triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos+c(-dia,-dia,len)))  
        cat(printvertex(pos+c( dia,-dia,len)))
        cat(printvertex(pos+c( dia,-dia,0)))
        cat("endloop\nendfacet\n")    
        
  
        #Face 4 Lower left triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos+c(-dia,-dia,0)))  
        cat(printvertex(pos+c(-dia, dia,0)))
        cat(printvertex(pos+c(-dia,-dia,len)))
        cat("endloop\nendfacet\n")    
        
        #upper right triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos+c(-dia,-dia,len)))  
        cat(printvertex(pos+c(-dia, dia,len)))
        cat(printvertex(pos+c(-dia, dia,0)))
        cat("endloop\nendfacet\n")    
        
    return()  
  }
  
  #if we don't have a cross section, then draw a single facet
  draw_gene_to_stl(pos,cs,len,dia)
}







#' Create an stl file from a single gene
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
genetostlfile <- function(fn="shape.stl",gene,pos=c(0,0,0),runlim=1000){
  
  running <- T
  
  message(sprintf("writing stl file to %s",fn))
  
  
  sink(file = fn)
  cat("solid Exported from Blender-2.80 (sub 75)\n")
  
  iterations<-0
  #Scan the gene for a shape and size
  while (running){
    
    message(sprintf("Iteration %d",iterations))
    
    cs <- gene[gene$att == "Cross section",]
    if(nrow(cs) == 1){
      message(sprintf("Cross section is %s",cs$valtyp[1]))
      active_cs <- cs$valtyp[1]
    }
    else{
      message("Multiple cross sections!")
    }
    
    
    ls <- gene[gene$att == "Length",]
    if(nrow(ls) == 1){
      message(sprintf("Length is %0.2f",as.numeric(ls$valtyp[1])))
      active_len <- as.numeric(ls$valtyp[1])
    }
    else{
      message("Multiple Lengths!")
    }
    
    
    ds <- gene[gene$att == "Diameter",]
    if(nrow(ds) == 1){
      message(sprintf("Diameter is %0.2f",as.numeric(ds$valtyp[1])))
      active_dia <- as.numeric(ds$valtyp[1])
    }
  
    # CALCULATE THE DIRECTION:
    dirs <- gene[gene$att == "DirectionX" | gene$att == "DirectionY" | gene$att == "DirectionZ", ]
    message(sprintf("Found %d direction genes",nrow(dirs)))
    dirs <- dirs[as.numeric(dirs$valtyp)>0,]
    message(sprintf("Found %d nonzero direction genes, direction is %s",nrow(dirs),dirs$att))
  
    
    #Calculate this iteration's nodes
    
    #Draw what we have
    #draw_gene_to_stl(pos,active_cs,active_len,active_dia)
    drawCStoSTL(pos,active_cs,active_len,active_dia)
    
    #Update posn
    if(dirs$att == "DirectionX")pos[1] <- pos[1]+active_len
    if(dirs$att == "DirectionY")pos[2] <- pos[2]+active_len
    if(dirs$att == "DirectionZ")pos[3] <- pos[3]+active_len
    
    #update status
    for(dd in 1:nrow(dirs)){
      if(dirs$att[dd] == "DirectionX")
        if(pos[1] > dirs$stop[dd]){
          message("Reached DirectionX stop");running<-F}
      if(dirs$att[dd] == "DirectionY")
        if(pos[2] > dirs$stop[dd]){
          message("Reached DirectionY stop");running<-F}
      if(dirs$att[dd] == "DirectionZ")
        if(pos[3] > dirs$stop[dd]){
          message("Reached DirectionZ stop");running<-F}
    }
    
    
    
    
    # Limit infinite runs
    if(iterations > runlim){message("finished");running<-F}
    iterations <- iterations + 1
    
    message(sprintf("Running is %d, iterations = %d, runlim = %d",running,iterations,runlim))
  }
  
  cat("solid Exported from Blender-2.80 (sub 75)\n")
  sink()
}

