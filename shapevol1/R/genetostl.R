



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


rotvert <- function(vert,dir="X"){
  if(dir == "X")
    vert<-c(vert[3],vert[1],vert[2])
  if(dir == "Y")
    vert<-c(vert[2],vert[3],vert[1])
  
  return(vert)
}


drawCStoSTL <- function(inpos,cs,len,dia,dir="Z",offset=c(0,0,0),verbose = T){
  
  if(is.data.frame(inpos)){
    if(nrow(inpos) != 1){
      message(sprintf("ERROR! bad number of rows (%d) in first argument of drawCStoSTL",nrow(inpos)))
      return()
    }
    else{
      if(verbose)message(sprintf("inside drawCStoSTL Setting pos to %0.0f,%0.0f,%0.0f",inpos$X[1],inpos$Y[1],inpos$Z[1]))
      cat(sprintf("#inside drawCStoSTL Setting pos to %0.0f,%0.0f,%0.0f\n",inpos$X[1],inpos$Y[1],inpos$Z[1]))
      pos <- c(inpos$X[1],inpos$Y[1],inpos$Z[1])
      cat(sprintf("#inside drawCStoSTL pos is now %0.0f,%0.0f,%0.0f; len is %0.0f; dia is %0.0f\n",pos[1],pos[2],pos[3],len,dia))
    }
  }
  else{
    pos<-inpos
  }
  
  if(cs == "Square"){
        
        #Bottom face 1
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos + offset + rotvert(c(-dia, dia,  0),dir)))  
        cat(printvertex(pos + offset + rotvert(c( dia, dia,  0),dir)))
        cat(printvertex(pos + offset + rotvert(c( dia,-dia,  0),dir)))
        cat("endloop\nendfacet\n")    
        #Bottom face 1
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos + offset + rotvert(c( dia,-dia,  0),dir)))  
        cat(printvertex(pos + offset + rotvert(c(-dia,-dia,  0),dir)))
        cat(printvertex(pos + offset + rotvert(c(-dia, dia,  0),dir)))
        cat("endloop\nendfacet\n")    
    
    
        #Face 1 Lower left triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos + offset + rotvert(c(-dia, dia,  0),dir)))  
        cat(printvertex(pos + offset + rotvert(c( dia, dia,  0),dir)))
        cat(printvertex(pos + offset + rotvert(c(-dia, dia,len),dir)))
        cat("endloop\nendfacet\n")    
        
        #upper right triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos + offset + rotvert(c(-dia, dia,len),dir)))  
        cat(printvertex(pos + offset + rotvert(c( dia, dia,len),dir)))
        cat(printvertex(pos + offset + rotvert(c( dia, dia,  0),dir)))
        cat("endloop\nendfacet\n")    
        
        #Face 2 Lower left triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos + offset + rotvert(c( dia,-dia,  0),dir)))  
        cat(printvertex(pos + offset + rotvert(c( dia, dia,  0),dir)))
        cat(printvertex(pos + offset + rotvert(c( dia,-dia,len),dir)))
        cat("endloop\nendfacet\n")    
        
        #upper right triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos + offset + rotvert(c( dia,-dia,len),dir)))  
        cat(printvertex(pos + offset + rotvert(c( dia, dia,len),dir)))
        cat(printvertex(pos + offset + rotvert(c( dia, dia,  0),dir)))
        cat("endloop\nendfacet\n")    
        
        #Face 3 Lower left triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos + offset + rotvert(c(-dia,-dia,  0),dir)))  
        cat(printvertex(pos + offset + rotvert(c( dia,-dia,  0),dir)))
        cat(printvertex(pos + offset + rotvert(c(-dia,-dia,len),dir)))
        cat("endloop\nendfacet\n")    
        
        #upper right triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos + offset + rotvert(c(-dia,-dia,len),dir)))  
        cat(printvertex(pos + offset + rotvert(c( dia,-dia,len),dir)))
        cat(printvertex(pos + offset + rotvert(c( dia,-dia,  0),dir)))
        cat("endloop\nendfacet\n")    
        
  
        #Face 4 Lower left triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos + offset + rotvert(c(-dia,-dia,  0),dir)))  
        cat(printvertex(pos + offset + rotvert(c(-dia, dia,  0),dir)))
        cat(printvertex(pos + offset + rotvert(c(-dia,-dia,len),dir)))
        cat("endloop\nendfacet\n")    
        
        #upper right triangle
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos + offset + rotvert(c(-dia,-dia,len),dir)))  
        cat(printvertex(pos + offset + rotvert(c(-dia, dia,len),dir)))
        cat(printvertex(pos + offset + rotvert(c(-dia, dia,  0),dir)))
        cat("endloop\nendfacet\n")    
        
        
        #Top face 1
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos + offset + rotvert(c(-dia, dia,  len),dir)))  
        cat(printvertex(pos + offset + rotvert(c( dia, dia,  len),dir)))
        cat(printvertex(pos + offset + rotvert(c( dia,-dia,  len),dir)))
        cat("endloop\nendfacet\n")    
        #Top face 2
        cat("facet normal 0 0 0\nouter loop\n")
        cat(printvertex(pos + offset + rotvert(c( dia,-dia,  len),dir)))  
        cat(printvertex(pos + offset + rotvert(c(-dia,-dia,  len),dir)))
        cat(printvertex(pos + offset + rotvert(c(-dia, dia,  len),dir)))
        cat("endloop\nendfacet\n")    
        
    return()  
  }
  
  #if we don't have a cross section, then draw a single facet
  #draw_gene_to_stl(pos,cs,len,dia)
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
    
    message(sprintf("Iteration %d, position = %0.2f,%0.2f,%0.2f",iterations,pos[1],pos[2],pos[3]))
    
    cs <- gene[gene$att == "Cross section",]
    if(nrow(cs) == 1){
      message(sprintf("Cross section is %s",cs$valtyp[1]))
    }
    else{
      message("Multiple cross sections!")
    }
    active_cs <- cs$valtyp[1]
    
    
    ls <- gene[gene$att == "Length",]
    if(nrow(ls) == 1){
      message(sprintf("Length is %0.2f",as.numeric(ls$valtyp[1])))
    }
    else{
      message("Multiple Lengths!")
    }
    active_len <- as.numeric(ls$valtyp[1])
    
    
    ds <- gene[gene$att == "Diameter",]
    message(sprintf("DIA,  ds has %d rows ",nrow(ds)))
    ds <- ds[ds$start <= pos[3] & ds$stop >= pos[3],] # TODO: Identify which dimension this applies!
    message(sprintf("STST, ds has %d rows ",nrow(ds)))
    ds <- ds[ds$dom == max(ds$dom),]
    message(sprintf("DOM,  ds has %d rows ",nrow(ds)))
    if(nrow(ds) == 1){
      message(sprintf("Diameter is %0.2f",as.numeric(ds$valtyp[1])))
    }
    else{
      message("OOPS - found %d Diameters!",nrow(ds))
    }
    active_dia <- as.numeric(ds$valtyp[1])
  
    # CALCULATE THE DIRECTION:
    dirs <- gene[gene$att == "DirectionX" | gene$att == "DirectionY" | gene$att == "DirectionZ", ]
    message(sprintf("Found %d direction genes",nrow(dirs)))
    dirs <- dirs[as.numeric(dirs$valtyp)>0,]
    message(sprintf("Found %d nonzero direction genes, direction is %s",nrow(dirs),dirs$att))
    
    active_dir = "N"
    if(dirs$att[1] == "DirectionX") active_dir = "X"
    if(dirs$att[1] == "DirectionY") active_dir = "Y"
    if(dirs$att[1] == "DirectionZ") active_dir = "Z"
    message(sprintf("Active Direction is %s",active_dir))
    
    #Calculate this iteration's nodes
    
    #Draw what we have
    #draw_gene_to_stl(pos,active_cs,active_len,active_dia)
    drawCStoSTL(pos,active_cs,active_len,active_dia,dir = active_dir)
    
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
genetostlfile2 <- function(fn="shape.stl",gene,pos=c(0,0,0),runlim=1000){
  
  running <- T
  
  message(sprintf("writing stl file to %s",fn))
  
  sink(file = fn)
  cat("solid Exported from Blender-2.80 (sub 75)\n")
  
  iterations<-0
  #Scan the gene for a shape and size
  while (running){
    
    message(sprintf("Iteration %d, position = %0.2f,%0.2f,%0.2f",iterations,pos[1],pos[2],pos[3]))
    
    
    #take a copy of the current position set: 
    oldpos <- pos
    
    #TODO: check for active genes - but since all genes are active at present, this is unnecessary
    
    #GET THE CURRENT CROSS SECTION
    cs <- gene[gene$att == "Cross section",]
    if(nrow(cs) == 1){
      message(sprintf("Cross section is %s",cs$valtyp[1]))
    }
    else{
      message("Multiple cross sections!")
    }
    active_cs <- cs$valtyp[1]
    
    #GET THE CURRENT LENGTH
    ##TODO: Check that the length is active!
    ls <- gene[gene$att == "Length",]
    if(nrow(ls) != 1){
    }
    else{
      message("Multiple Lengths!")
    }
    message(sprintf("Length is %0.2f",as.numeric(ls$valtyp[1])))
    active_len <- as.numeric(ls$valtyp[1])
    
    #GET THE CURRENT DIAMETER
    ds <- gene[gene$att == "Diameter",]
    message(sprintf("DIA,  ds has %d rows ",nrow(ds)))
    ds <- ds[ds$start <= pos[3] & ds$stop >= pos[3],] # TODO: Identify which dimension this applies!
    message(sprintf("STST, ds has %d rows ",nrow(ds)))
    ds <- ds[ds$dom == max(ds$dom),]
    message(sprintf("DOM,  ds has %d rows ",nrow(ds)))
    if(nrow(ds) == 1){
      message(sprintf("Diameter is %0.2f",as.numeric(ds$valtyp[1])))
    }
    else{
      message("Multiple Diameters!")
    }
    active_dia <- as.numeric(ds$valtyp[1])
    
    
    # CALCULATE THE DIRECTION:
    # Assume that the start/stop values *only* apply to the direction in which the gene operates...??
    #Xdimension
    dirsX <- gene[str_detect(gene$att,".X") & gene$start<= pos[1] & gene$stop >= pos[1], ]
    message(sprintf("Found %d X direction genes, pos is %d,%d,%d",nrow(dirsX),pos[1],pos[2],pos[3]))
    #Ydimension
    dirsY <- gene[str_detect(gene$att,".Y") & gene$start<= pos[2] & gene$stop >= pos[2], ]
    message(sprintf("Found %d Y direction genes, pos is %d,%d,%d",nrow(dirsY),pos[1],pos[2],pos[3]))
    #Zdimension
    dirsZ <- gene[str_detect(gene$att,".Z") & gene$start<= pos[3] & gene$stop >= pos[3], ]
    message(sprintf("Found %d Z direction genes, pos is %d,%d,%d",nrow(dirsZ),pos[1],pos[2],pos[3]))
    dirs <- rbind(dirsX,dirsY,dirsZ)
    
    active_dir = "N"
    posnext <- pos
    if(nrow(dirs)>0){
      #Select the dominant direction:
      message(sprintf("Max dominance is %0.0f: found %d genes",max(dirs$dom),nrow(dirs)))
      dirs <- dirs[dirs$dom == max(dirs$dom),]
      message(sprintf("after dominance pruning, %d genes remain",nrow(dirs)))
      
      #Prune any zero-valued directions: 
      dirs <- dirs[as.numeric(dirs$valtyp)!=0,]
      message(sprintf("Found %d nonzero direction genes, direction is %s",nrow(dirs),dirs$att[1]))
      
      if(nrow(dirs)>0){
        for(dd in 1:nrow(dirs)){
        
          message(sprintf("Processing active gene %d, name is %s",dd,dirs$att[dd]))
          
          if(str_detect(dirs$att[dd],".X") & dirs$start[dd]<= pos[1] & dirs$stop[dd] >= pos[1]){
            active_dir <- "X" 
            vts <- as.numeric(dirs$valtyp[dd])
            posnext[1] <-posnext[1]+active_len
          } 
          if(str_detect(dirs$att[dd],".Y") & dirs$start[dd]<= pos[2] & dirs$stop[dd] >= pos[2]){
            active_dir <- "Y" 
            vts <- as.numeric(dirs$valtyp[dd])
            posnext[2] <-posnext[2]+active_len
          } 
          if(str_detect(dirs$att[dd],".Z") & dirs$start[dd]<= pos[3] & dirs$stop[dd] >= pos[3]){
            active_dir <- "Z" 
            vts <- as.numeric(dirs$valtyp[dd])
            posnext[3] <-posnext[3]+active_len
          } 
          message(sprintf("Active Direction is %s",active_dir))
          
          #Draw what we have
          #draw_gene_to_stl(pos,active_cs,active_len,active_dia)
          if(active_dir != "N"){
            drawCStoSTL(pos,active_cs,active_len*vts,active_dia,active_dir)
          }
        }
      }
    }
    
    if(active_dir == "N"){
      message("No active directions remain!")
      break
    }
    #Update posn
    pos <- posnext
    #if(str_detect(dirs$att[1],".X")) pos[1] <- pos[1]+active_len
    #if(str_detect(dirs$att[1],".Y")) pos[2] <- pos[2]+active_len
    #if(str_detect(dirs$att[1],".Z")) pos[3] <- pos[3]+active_len
    
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
    
    message(sprintf("Running is %d, iterations = %d, runlim = %d\n",running,iterations,runlim))
  }
  
  cat("solid Exported from Blender-2.80 (sub 75)\n")
  sink()
}


printpos<- function(pos,msg){
  
  message(msg)
  for(pp in 1:nrow(pos))
    message(sprintf("%02d: x=%0.0f, y=%0.0f, z=%0.0f",pp,pos$X[pp],pos$Y[pp],pos$Z[pp]))
  
  
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
genetostlfile3 <- function(fn="shape.stl",gene,pos=spos(0,0,0),runlim=1000,comments=F){
  
  running <- T
  
  message(sprintf("writing stl file to %s",fn))
  
  sink(file = fn)
  cat("solid Exported from Blender-2.80 (sub 75)\n")
  
  iterations<-0
  #Scan the gene for a shape and size
  while (running){
    
    #message(sprintf("Iteration %d, position = %0.2f,%0.2f,%0.2f",iterations,pos[1],pos[2],pos[3]))
    message(sprintf("Iteration %d, %d positions to parse:",iterations,nrow(pos)))
    if(comments)cat(sprintf("\n#Iteration %d, %d positions to parse\n",iterations,nrow(pos)))
    for(px in 1:nrow(pos)){
      message(sprintf("\t\t%0.0f,%0.0f,%0.0f",pos$X[px],pos$Y[px],pos$Z[px]))
    }
    
    
    #take a copy of the current position set: 
    oldpos <- pos
    
    #TODO: check for active genes - but since all genes are active at present, this is unnecessary
    
    #create the empty posnext:
    posnext <- NULL
    
    for(pp in 1:nrow(pos)){ 
      #GET THE CURRENT CROSS SECTION
      cs <- gene[gene$att == "Cross section",]
      if(nrow(cs) == 1){
        message(sprintf("Cross section is %s",cs$valtyp[1]))
      }
      else{
        message("Multiple cross sections!")
      }
      active_cs <- cs$valtyp[1]
      
      #GET THE CURRENT LENGTH
      ##TODO: Check that the length is active!
      ls <- gene[gene$att == "Length",]
      if(nrow(ls) != 1){
      }
      else{
        message("Multiple Lengths!")
      }
      message(sprintf("Length is %0.2f",as.numeric(ls$valtyp[1])))
      active_len <- as.numeric(ls$valtyp[1])
      
      #GET THE CURRENT DIAMETER
      ds <- gene[gene$att == "Diameter",]
      message(sprintf("DIA,  ds has %d rows ",nrow(ds)))
      ds <- ds[ds$start <= pos$Z[pp] & ds$stop >= pos$Z[pp],] # TODO: Identify which dimension this applies!
      message(sprintf("STST, ds has %d rows ",nrow(ds)))
      ds <- ds[ds$dom == max(ds$dom),]
      message(sprintf("DOM,  ds has %d rows ",nrow(ds)))
      if(nrow(ds) == 1){
        message(sprintf("Diameter is %0.2f",as.numeric(ds$valtyp[1])))
      }
      else{
        message("Multiple Diameters!")
      }
      active_dia <- as.numeric(ds$valtyp[1])
    
    
      message(sprintf("Parsing position %d: %0.0f,%0.0f, %0.0f",pp,pos$X[pp],pos$Y[pp],pos$Z[pp]))
      
      # CALCULATE THE DIRECTION:
      # Assume that the start/stop values *only* apply to the direction in which the gene operates...??
      #Xdimension
      dirsX <- gene[str_detect(gene$att,".X") & gene$start<= pos$X[pp] & gene$stop >= pos$X[pp], ]
      message(sprintf("Found %d X direction genes, pos is %d,%d,%d",nrow(dirsX),pos$X[pp],pos$Y[pp],pos$Z[pp]))
      #Ydimension
      dirsY <- gene[str_detect(gene$att,".Y") & gene$start<= pos$Y[pp] & gene$stop >= pos$Y[pp], ]
      message(sprintf("Found %d Y direction genes, pos is %d,%d,%d",nrow(dirsY),pos$X[pp],pos$Y[pp],pos$Z[pp]))
      #Zdimension
      dirsZ <- gene[str_detect(gene$att,".Z") & gene$start<= pos$Z[pp] & gene$stop >= pos$Z[pp], ]
      message(sprintf("Found %d Z direction genes, pos is %d,%d,%d",nrow(dirsZ),pos$X[pp],pos$Y[pp],pos$Z[pp]))
      dirs <- rbind(dirsX,dirsY,dirsZ)
      
      active_dir = "N"
      if(nrow(dirs)>0){
        #Select the dominant direction:
        message(sprintf("Max dominance is %0.0f: found %d genes",max(dirs$dom),nrow(dirs)))
        dirs <- dirs[dirs$dom == max(dirs$dom),]
        message(sprintf("after dominance pruning, %d genes remain",nrow(dirs)))
        
        #Prune any zero-valued directions: 
        dirs <- dirs[as.numeric(dirs$valtyp)!=0,]
        message(sprintf("Found %d nonzero direction genes, direction is %s",nrow(dirs),dirs$att[1]))
        
        if(nrow(dirs)>0){
          pc <- 1
          for(dd in 1:nrow(dirs)){
            
            message(sprintf("Processing active gene %d, name is %s",dd,dirs$att[dd]))
            
            if(str_detect(dirs$att[dd],".X") & dirs$start[dd]<= pos$X[pp] & dirs$stop[dd] >=  pos$X[pp] ){
              active_dir <- "X" 
              vts <- as.numeric(dirs$valtyp[dd])
              #posnext$X[pc] <-posnext$X[pc]+active_len
              posnext <- addpos(pos$X[pp]+(active_len*vts),pos$Y[pp],pos$Z[pp],posnext)
              printpos(posnext,"posnext is now:")
            } 
            if(str_detect(dirs$att[dd],".Y") & dirs$start[dd]<= pos$Y[pp] & dirs$stop[dd] >=  pos$Y[pp]){
              active_dir <- "Y" 
              vts <- as.numeric(dirs$valtyp[dd])
              ##posnext[2] <-posnext[2]+active_len
              posnext <- addpos(pos$X[pp],pos$Y[pp]+(active_len*vts),pos$Z[pp],posnext)
              printpos(posnext,"posnext is now:")
            } 
            if(str_detect(dirs$att[dd],".Z") & dirs$start[dd]<= pos$Z[pp] & dirs$stop[dd] >= pos$Z[pp]){
              active_dir <- "Z" 
              vts <- as.numeric(dirs$valtyp[dd])
              #posnext[3] <-posnext[3]+active_len
              posnext <- addpos(pos$X[pp],pos$Y[pp],pos$Z[pp]+(active_len*vts),posnext)
              printpos(posnext,"posnext is now:")
            } 
            message(sprintf("Active Direction is %s",active_dir))
            
            #Draw what we have
            #draw_gene_to_stl(pos,active_cs,active_len,active_dia)
            if(active_dir != "N"){
              message(sprintf("Drawing a segment at pos %0.0f,%0.0f,%0.0f. active_len=%0.0f, vts=%0.0f",
                              pos$X[pp],pos$Y[pp],pos$Z[pp],active_len,vts))
              drawCStoSTL(pos[pp,],active_cs,active_len*vts,active_dia,active_dir)
            }
          }
        }
      }
    }
    
    if(active_dir == "N"){
      message("No active directions remain!")
      break
    }
    #Update posn
    #TODO: Potentially need to check whether any positions in pos have already been seen!
    pos <- posnext
    #if(str_detect(dirs$att[1],".X")) pos[1] <- pos[1]+active_len
    #if(str_detect(dirs$att[1],".Y")) pos[2] <- pos[2]+active_len
    #if(str_detect(dirs$att[1],".Z")) pos[3] <- pos[3]+active_len
    
    #update status
    # for(dd in 1:nrow(dirs)){
    #   if(dirs$att[dd] == "DirectionX")
    #     if(pos[1] > dirs$stop[dd]){
    #       message("Reached DirectionX stop");running<-F}
    #   if(dirs$att[dd] == "DirectionY")
    #     if(pos[2] > dirs$stop[dd]){
    #       message("Reached DirectionY stop");running<-F}
    #   if(dirs$att[dd] == "DirectionZ")
    #     if(pos[3] > dirs$stop[dd]){
    #       message("Reached DirectionZ stop");running<-F}
    # }
    
    # Limit infinite runs
    if(iterations > runlim){message("finished");running<-F}
    iterations <- iterations + 1
    
    message(sprintf("Running is %d, iterations = %d, runlim = %d\n",running,iterations,runlim))
  }
  
  cat("solid Exported from Blender-2.80 (sub 75)\n")
  sink()
}
