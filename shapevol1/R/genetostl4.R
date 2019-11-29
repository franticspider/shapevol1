


#' Check whether a position is within a range (if the range exists)
#' 
#' @param gene the group of genes under consideration,
#' @param rx a regular expression for detecting the right attribute
#' @param pos a real number indicating the current position of the development
#' @return a boolean value indicating whether the gene is in this range
#' @examples
#' inrange <- checklimits(genegroup,".X",33)
checklimits <- function(gene,rx="X",pos){
  
  #Check whethe there is a restriction for this regex
  
  #dir <- gene[str_detect(gene$att,rx),]
  #dir <- gene[attDir(gene$att,rx),]
  dir <- gene[str_sub(gene$att,str_length(gene$att),str_length(gene$att)) == rx,]

  if(nrow(dir)>0){
    
    dirinlim <- dir[dir$start<= pos & dir$stop >= pos, ]
    if(nrow(dirinlim)==0)
      return(FALSE)
    else
      return(TRUE)
  }
  else
    return (TRUE)
}




#' Find the dominant gene at a given position
#' 
#' @param pos a data frame with a single row and fields X, Y and Z
#' @param group a group of genes
#' @return dominance value as an integer
#' @examples
#' domgene <- domInZone(data.frame(X=0,Y=1,Z=2),genegroup1)
domInZone <- function(pos,group){
  
  #We only want (for now) The list of dominances for *position* variables
  #group <- group[str_detect(group$att,".(X|Y|Z)"),]
  #group <- group[attDir(group$att,"X") | attDir(group$att,"Y") | attDir(group$att,"Z"),]
  gx <- group[str_sub(group$att,str_length(group$att),str_length(group$att)) == "X",]
  gy <- group[str_sub(group$att,str_length(group$att),str_length(group$att)) == "Y",]
  gz <- group[str_sub(group$att,str_length(group$att),str_length(group$att)) == "Z",]
  group <- rbind(gx,gy,gz)
  
  #Get the list of dominances: 
  doms <- unique(group$dom)
  doms <- doms[order(doms,decreasing = T)]
  
  
  for(dd in doms){
    #browser()
    inzone <- T
    gene <- group[group$dom == dd,]
    
    #check for X limits:
    inzone <- inzone & checklimits(gene,"X",pos$X)
    
    #check for Y limits: 
    inzone <- inzone & checklimits(gene,"Y",pos$Y)
    
    #check for Z limits: 
    inzone <- inzone & checklimits(gene,"Z",pos$Z)
    
    if(inzone)
      return(dd)
  }
  
  message("Nothing to process for this position")
  return (NA)
}


#TODO make this work for dataframes, not just individual rows.
attDir <- function (att,dd){
  
  if(str_sub(att,str_length(att),str_length(att))==dd)
    return (TRUE)
  else
    return (FALSE)
  
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
genetostlfile4 <- function(fn="shape.stl",gene,pos=spos(0,0,0),runlim=1000,comments=F,debug=F){
  
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
    
    #create the empty posnext to hold the next set of positions:
    posnext <- NULL
    
    #browser()
    for(pp in 1:nrow(pos)){ 
      
      #################################################################
      # GET THE CURRENT SHAPE ATTRIBUTES
      # todo: check the zoning is working properly
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
      if(nrow(ls) == 1){
        
      }
      else{
        message(sprintf("Multiple Lengths! %d rows found",nrow(ls)))
        browser()
        for(ll in 1:nrow(ls)){
          message(sprintf("%d: Att = %s, Val = %s",ll,ls$att[ll],ls$valtyp[ll]))
        }
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
      #####################################################################
      
      message(sprintf("Parsing position %d: %0.0f,%0.0f, %0.0f",pp,pos$X[pp],pos$Y[pp],pos$Z[pp]))
      
      # CALCULATE THE DIRECTION:
      # Assume that the start/stop values *only* apply to the direction in which the gene operates...??
      #Xdimension
      #dirsX <- gene[str_detect(gene$att,".X") & gene$start<= pos$X[pp] & gene$stop >= pos$X[pp], ]
      #message(sprintf("Found %d X direction genes, pos is %d,%d,%d",nrow(dirsX),pos$X[pp],pos$Y[pp],pos$Z[pp]))
      #Ydimension
      #dirsY <- gene[str_detect(gene$att,".Y") & gene$start<= pos$Y[pp] & gene$stop >= pos$Y[pp], ]
      #message(sprintf("Found %d Y direction genes, pos is %d,%d,%d",nrow(dirsY),pos$X[pp],pos$Y[pp],pos$Z[pp]))
      #Zdimension
      #dirsZ <- gene[str_detect(gene$att,".Z") & gene$start<= pos$Z[pp] & gene$stop >= pos$Z[pp], ]
      #message(sprintf("Found %d Z direction genes, pos is %d,%d,%d",nrow(dirsZ),pos$X[pp],pos$Y[pp],pos$Z[pp]))
      #dirs <- rbind(dirsX,dirsY,dirsZ)
      
      #browser()
      
      domval <- domInZone(pos[pp,],gene)
      #browser()
      
      active_dir = "N"
      #if(nrow(dirs)>0){
      if(!is.na(domval)){
        dirs <- gene[gene$dom == domval,]
        
        ##Use zero-valued directions to check the spatial scope of the genes...
        #dv <- unique(dirs$dom,)
        #dv <- dv[order(dv,decreasing = T)]
        ## Starting with the highest, go through the dominance levels until we find one 
        ## where the current position is "in zone"
        #for(dd in dv){
        #  ddirs <- dirs[dirs$dom == dd,]
        #}
        
        #Select the dominant direction:
        message(sprintf("Max dominance is %0.0f: found %d genes",max(dirs$dom),nrow(dirs)))
        dirs <- dirs[dirs$dom == max(dirs$dom),]
        message(sprintf("after dominance pruning, %d genes remain",nrow(dirs)))
        
        #Get directions from the non-zero valued attributes
        dirs <- dirs[as.numeric(dirs$valtyp)!=0,]
        message(sprintf("Found %d nonzero direction genes, direction is %s",nrow(dirs),dirs$att[1]))
        if(debug)browser()
        if(nrow(dirs)>0){
          pc <- 1
          for(dd in 1:nrow(dirs)){
            
            message(sprintf("Processing active gene %d, name is %s",dd,dirs$att[dd]))
            
            #browser()
            
            
            #if(str_detect(dirs$att[dd],"^X") & dirs$start[dd]<= pos$X[pp] & dirs$stop[dd] >=  pos$X[pp] ){
            if(attDir(dirs$att[dd],"X") & dirs$start[dd]<= pos$X[pp] & dirs$stop[dd] >=  pos$X[pp] ){
              active_dir <- "X" 
              vts <- as.numeric(dirs$valtyp[dd])
              #posnext$X[pc] <-posnext$X[pc]+active_len
              posnext <- addpos(pos$X[pp]+(active_len*vts),pos$Y[pp],pos$Z[pp],posnext)
              printpos(posnext,"posnext is now:")
            } 
            #if(str_detect(dirs$att[dd],"^Y") & dirs$start[dd]<= pos$Y[pp] & dirs$stop[dd] >=  pos$Y[pp]){
            if(attDir(dirs$att[dd],"Y") & dirs$start[dd]<= pos$Y[pp] & dirs$stop[dd] >=  pos$Y[pp] ){
              active_dir <- "Y" 
              vts <- as.numeric(dirs$valtyp[dd])
              ##posnext[2] <-posnext[2]+active_len
              posnext <- addpos(pos$X[pp],pos$Y[pp]+(active_len*vts),pos$Z[pp],posnext)
              printpos(posnext,"posnext is now:")
            } 
            #if(str_detect(dirs$att[dd],"^Z") & dirs$start[dd]<= pos$Z[pp] & dirs$stop[dd] >= pos$Z[pp]){
            if(attDir(dirs$att[dd],"Z") & dirs$start[dd]<= pos$Z[pp] & dirs$stop[dd] >=  pos$Z[pp] ){
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
    
    #if(active_dir == "N"){
    if(is.null(posnext)){
      if(debug)browser()
      message("No active positions to parse!")
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
    if(iterations > runlim){
      browser()
      message("finished")
      running<-F
    }
    iterations <- iterations + 1
    
    message(sprintf("Running is %d, iterations = %d, runlim = %d\n",running,iterations,runlim))
  }
  
  cat("solid Exported from Blender-2.80 (sub 75)\n")
  sink()
}