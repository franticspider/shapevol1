





#' Create an STL of a cross-section shape primitive
#' This version uses two 'blocks' to avoid overlapping cross section,
#' and starts writing the first block 1/2 dia away from the input point
#' this value is the 4th entry in inpos - but if old inpos vectors are put in of length 3, 
#' a default is used. 
#' 
#' @param inpos the start position of the shape
#' @param cs the cross section. Currently only "Square" is implemented
#' @param len the length of the shape primitive
#' @param rad the "radmeter" of the cross-section (or width if it's a square)
#' @param dir  the direction of growth. Defaults to "Z" (vertical)
#' @param offset the position offset (used when writing multiple shapes to one file)
#' @param verbose whether to print messages while processing
#' @param verbosestl whether to print messages into the stl file (can crash some viewers)
#' @return nothing
#' @examples
#' drawCStoSTL(pos,active_cs,active_len,active_dia,dir = active_dir)
drawCStoSTLv2 <- function(inpos,cs,len,rad,dir="Z",offset=c(0,0,0),verbose = T,verbosestl=F,initonly=F){
  
  # We have TWO radmeter offsets:
  # Input radmeter offset, idoff, which can be calculated from inpos[4]
  # Output radmeter offset, odoff, which can be calculated from rad
  
  odoff = rad
  # start pos might be 0,0,0 - add a rad offset to it
  if(length(inpos) < 4){
    idoff = rad
  }else{
    idoff = inpos[4]
  }
  
  
  # Safety checks
  if(is.data.frame(inpos)){
    if(nrow(inpos) != 1){
      message(sprintf("ERROR! bad number of rows (%d) in first argument of drawCStoSTL",nrow(inpos)))
      return()
    }
    else{
      if(verbose)
        message(sprintf("inside drawCStoSTL Setting pos to %0.0f,%0.0f,%0.0f",inpos$X[1],inpos$Y[1],inpos$Z[1]))
      if(verbosestl)
        cat(sprintf("#inside drawCStoSTL Setting pos to %0.0f,%0.0f,%0.0f\n",inpos$X[1],inpos$Y[1],inpos$Z[1]))
      pos <- c(inpos$X[1],inpos$Y[1],inpos$Z[1])
      if(verbosestl)
        cat(sprintf("#inside drawCStoSTL pos is now %0.0f,%0.0f,%0.0f; len is %0.0f; rad is %0.0f\n",pos[1],pos[2],pos[3],len,rad))
    }
  }
  else{
    pos<-inpos
  }
  
  #Make the shape primitive
  if(cs == "Square"){
    
    if(initonly){
      cw<-F
      #TODO: tidy this up with a triangle-drawing function
      #Start face 1
      drawTriangle(pos,offset,c(-rad, rad,  idoff),c( rad, rad,  idoff),c( rad,-rad,  idoff),dir,cw)
      drawTriangle(pos,offset,c( rad,-rad,  idoff),c(-rad,-rad,  idoff),c(-rad, rad,  idoff),dir,cw)
      
      # EDGE COMPONENT
      #Face 1 Lower left triangle
      drawTriangle(pos,offset,c(-rad, rad, idoff),c( rad, rad, idoff),c(-rad, rad,-idoff),dir,cw)
      drawTriangle(pos,offset,c(-rad, rad,-idoff),c( rad, rad,-idoff),c( rad, rad, idoff),dir,cw)
      
      #Face 2 Lower left triangle
      drawTriangle(pos,offset,c( rad, -rad, idoff),c( rad, rad, idoff),c( rad,-rad,-idoff),dir,cw)
      drawTriangle(pos,offset,c( rad, -rad,-idoff),c( rad, rad,-idoff),c( rad, rad, idoff),dir,cw)
      #drawTriangle(pos,offset,c( rad,-rad,  idoff),c( rad, rad,  idoff),c( rad,-rad,len-odoff),dir,cw)
      #drawTriangle(pos,offset,c( rad,-rad,len-odoff),c( rad, rad,len-odoff),c( rad, rad,  idoff),dir,cw)
      
      #Face 3 Lower left triangle
      drawTriangle(pos,offset,c(-rad, -rad, idoff),c( rad, -rad, idoff),c(-rad,-rad,-idoff),dir,cw)
      drawTriangle(pos,offset,c(-rad, -rad,-idoff),c( rad, -rad,-idoff),c( rad,-rad, idoff),dir,cw)
      #drawTriangle(pos,offset,c(-rad,-rad,  idoff),c( rad,-rad,  idoff),c(-rad,-rad,len-odoff),dir,cw)
      #drawTriangle(pos,offset,c(-rad,-rad,len-odoff),c( rad,-rad,len-odoff),c( rad,-rad,  idoff),dir,cw)
      
      #Face 4 Lower left triangle
      drawTriangle(pos,offset,c(-rad,-rad, idoff),c(-rad, rad, idoff),c(-rad,-rad,-idoff),dir,cw)
      drawTriangle(pos,offset,c(-rad,-rad,-idoff),c(-rad, rad,-idoff),c(-rad, rad, idoff),dir,cw)
      #drawTriangle(pos,offset,c(-rad,-rad,  idoff),c(-rad, rad,  idoff),c(-rad,-rad,len-odoff),dir,cw)
      #drawTriangle(pos,offset,c(-rad,-rad,len-odoff),c(-rad, rad,len-odoff),c(-rad, rad,  idoff),dir,cw)
      
      
      #End face 1
      drawTriangle(pos,offset,c(-rad, rad,  -idoff),c( rad, rad,  -idoff),c( rad,-rad,  -idoff),dir,cw)
      drawTriangle(pos,offset,c( rad,-rad,  -idoff),c(-rad,-rad,  -idoff),c(-rad, rad,  -idoff),dir,cw)
      
      return()  
      
    }else{
      if(len<0){
        cw <- T
        idoff <- idoff*-1
        odoff <- odoff*-1
      }else
        cw <- F
      
      #TODO: tidy this up with a triangle-drawing function
      #Start face 1
      drawTriangle(pos,offset,c(-rad, rad,  idoff),c( rad, rad,  idoff),c( rad,-rad,  idoff),dir,cw)
      drawTriangle(pos,offset,c( rad,-rad,  idoff),c(-rad,-rad,  idoff),c(-rad, rad,  idoff),dir,cw)
      
      # EDGE COMPONENT
      #Face 1 Lower left triangle
      drawTriangle(pos,offset,c(-rad, rad, idoff),c( rad, rad, idoff),c(-rad, rad,   len-odoff),dir,cw)
      drawTriangle(pos,offset,c(-rad, rad, len-odoff),c( rad, rad,   len-odoff),c( rad, rad, idoff),dir,cw)
      
      #Face 2 Lower left triangle
      drawTriangle(pos,offset,c( rad,-rad,  idoff),c( rad, rad,  idoff),c( rad,-rad,len-odoff),dir,cw)
      drawTriangle(pos,offset,c( rad,-rad,len-odoff),c( rad, rad,len-odoff),c( rad, rad,  idoff),dir,cw)
      
      #Face 3 Lower left triangle
      drawTriangle(pos,offset,c(-rad,-rad,  idoff),c( rad,-rad,  idoff),c(-rad,-rad,len-odoff),dir,cw)
      drawTriangle(pos,offset,c(-rad,-rad,len-odoff),c( rad,-rad,len-odoff),c( rad,-rad,  idoff),dir,cw)
      
      #Face 4 Lower left triangle
      drawTriangle(pos,offset,c(-rad,-rad,  idoff),c(-rad, rad,  idoff),c(-rad,-rad,len-odoff),dir,cw)
      drawTriangle(pos,offset,c(-rad,-rad,len-odoff),c(-rad, rad,len-odoff),c(-rad, rad,  idoff),dir,cw)
      
      
      # NODE COMPONENT
      #Face 1 Lower left triangle
      drawTriangle(pos,offset,c(-rad, rad, len-odoff),c( rad, rad, len-odoff),c(-rad, rad, len+odoff),dir,cw)
      drawTriangle(pos,offset,c(-rad, rad, len+odoff),c( rad, rad, len+odoff),c( rad, rad, len-odoff),dir,cw)
      
      #Face 2 Lower left triangle
      drawTriangle(pos,offset,c( rad,-rad, len-odoff),c( rad, rad, len-odoff),c( rad,-rad, len+odoff),dir,cw)
      drawTriangle(pos,offset,c( rad,-rad, len+odoff),c( rad, rad, len+odoff),c( rad, rad, len-odoff),dir,cw)
      
      #Face 3 Lower left triangle
      drawTriangle(pos,offset,c(-rad,-rad, len-odoff),c( rad,-rad, len-odoff),c(-rad,-rad, len+odoff),dir,cw)
      drawTriangle(pos,offset,c(-rad,-rad, len+odoff),c( rad,-rad, len+odoff),c( rad,-rad, len-odoff),dir,cw)
      
      #Face 4 Lower left triangle
      drawTriangle(pos,offset,c(-rad,-rad, len-odoff),c(-rad, rad, len-odoff),c(-rad,-rad, len+odoff),dir,cw)
      drawTriangle(pos,offset,c(-rad,-rad, len+odoff),c(-rad, rad, len+odoff),c(-rad, rad, len-odoff),dir,cw)
      
      
      
      
      #End face 1
      drawTriangle(pos,offset,c(-rad, rad,len+odoff),c( rad, rad,len+odoff),c( rad,-rad,len+odoff),dir,cw)
      drawTriangle(pos,offset,c( rad,-rad,len+odoff),c(-rad,-rad,len+odoff),c(-rad, rad,len+odoff),dir,cw)
      
      return()  
    }
  }
  
  #if we don't have a cross section, then draw a single facet
  #draw_gene_to_stl(pos,cs,len,dia)
}




#' Create an STL of a cross-section shape primitive
#' this version works fine, but produces overlapping artefacts in the slicer
#' Use drawCStoSTLv2 to avoid this
#' 
#' @param inpos the start position of the shape
#' @param cs the cross section. Currently only "Square" is implemented
#' @param len the length of the shape primitive
#' @param dia the "diameter" of the cross-section (or width if it's a square)
#' @param dir  the direction of growth. Defaults to "Z" (vertical)
#' @param offset the position offset (used when writing multiple shapes to one file)
#' @param verbose whether to print messages while processing
#' @param verbosestl whether to print messages into the stl file (can crash some viewers)
#' @return nothing
#' @examples
#' drawCStoSTL(pos,active_cs,active_len,active_dia,dir = active_dir)
drawCStoSTL <- function(inpos,cs,len,dia,dir="Z",offset=c(0,0,0),verbose = T,verbosestl=F){
  
  if(is.data.frame(inpos)){
    if(nrow(inpos) != 1){
      message(sprintf("ERROR! bad number of rows (%d) in first argument of drawCStoSTL",nrow(inpos)))
      return()
    }
    else{
      if(verbose)
        message(sprintf("inside drawCStoSTL Setting pos to %0.0f,%0.0f,%0.0f",inpos$X[1],inpos$Y[1],inpos$Z[1]))
      if(verbosestl)
        cat(sprintf("#inside drawCStoSTL Setting pos to %0.0f,%0.0f,%0.0f\n",inpos$X[1],inpos$Y[1],inpos$Z[1]))
      pos <- c(inpos$X[1],inpos$Y[1],inpos$Z[1])
      if(verbosestl)
        cat(sprintf("#inside drawCStoSTL pos is now %0.0f,%0.0f,%0.0f; len is %0.0f; dia is %0.0f\n",pos[1],pos[2],pos[3],len,dia))
    }
  }
  else{
    pos<-inpos
  }
  
  if(cs == "Square"){
    if(len<0)
      cw <- T
    else
      cw <- F
    
    #TODO: tidy this up with a triangle-drawing function
    #Bottom face 1
    drawTriangle(pos,offset,c(-dia, dia,  0),c( dia, dia,  0),c( dia,-dia,  0),dir,cw)
    drawTriangle(pos,offset,c( dia,-dia,  0),c(-dia,-dia,  0),c(-dia, dia,  0),dir,cw)
    
    #Face 1 Lower left triangle
    drawTriangle(pos,offset,c(-dia, dia,  0),c( dia, dia,  0),c(-dia, dia,len),dir,cw)
    drawTriangle(pos,offset,c(-dia, dia,len),c( dia, dia,len),c( dia, dia,  0),dir,cw)
    
    #Face 2 Lower left triangle
    drawTriangle(pos,offset,c( dia,-dia,  0),c( dia, dia,  0),c( dia,-dia,len),dir,cw)
    drawTriangle(pos,offset,c( dia,-dia,len),c( dia, dia,len),c( dia, dia,  0),dir,cw)
    
    #Face 3 Lower left triangle
    drawTriangle(pos,offset,c(-dia,-dia,  0),c( dia,-dia,  0),c(-dia,-dia,len),dir,cw)
    drawTriangle(pos,offset,c(-dia,-dia,len),c( dia,-dia,len),c( dia,-dia,  0),dir,cw)
    
    #Face 4 Lower left triangle
    drawTriangle(pos,offset,c(-dia,-dia,  0),c(-dia, dia,  0),c(-dia,-dia,len),dir,cw)
    drawTriangle(pos,offset,c(-dia,-dia,len),c(-dia, dia,len),c(-dia, dia,  0),dir,cw)
    
    
    #Top face 1
    drawTriangle(pos,offset,c(-dia, dia,len),c( dia, dia,len),c( dia,-dia,len),dir,cw)
    drawTriangle(pos,offset,c( dia,-dia,len),c(-dia,-dia,len),c(-dia, dia,len),dir,cw)
    
    return()  
  }
  
  #if we don't have a cross section, then draw a single facet
  #draw_gene_to_stl(pos,cs,len,dia)
}


