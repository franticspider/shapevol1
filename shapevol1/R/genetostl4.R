


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
domInZone <- function(pos,group,verbose=F){
  
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
  
  if(verbose)message("Nothing to process for this position")
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
genetostlfile4 <- function(fn="shape.stl",gene,pos=spos(0,0,0),offset=c(0,0,0),runlim=1000,comments=F,verbose = F,debug=F,solo=T){
  
  running <- T

  seenpos<-pos
    
  if(verbose)message(sprintf("writing stl file to %s",fn))
  
  if(solo){
    sink(file = fn)
    cat("solid Exported from Blender-2.80 (sub 75)\n")
  }
  
  iterations<-0
  firstblock<-T
  #Scan the gene for a shape and size
  while (running){
    
    if(verbose)message(sprintf("Iteration %d, %d positions to parse:",iterations,nrow(pos)))
    if(comments)cat(sprintf("\n#Iteration %d, %d positions to parse\n",iterations,nrow(pos)))
    if(verbose){
      for(px in 1:nrow(pos)){
        message(sprintf("\t\t%0.0f,%0.0f,%0.0f",pos$X[px],pos$Y[px],pos$Z[px]))
      }
    }
    
    #create the empty posnext to hold the next set of positions:
    posnext <- NULL
    
    if(debug)browser()
    for(pp in 1:nrow(pos)){ 
      
      #################################################################
      # GET THE CURRENT SHAPE ATTRIBUTES
      # todo: check the zoning is working properly
      cs <- gene[gene$att == "Cross section",]
      if(verbose){
        if(nrow(cs) == 1){
          message(sprintf("Cross section is %s",cs$valtyp[1]))
        }
        else{
          message("Multiple cross sections!")
        }
      }
      active_cs <- cs$valtyp[1]
      
      #GET THE CURRENT LENGTH
      ##TODO: Check that the length is active!
      ls <- gene[gene$att == "Length",]
      if(nrow(ls) == 1){
        
      }
      else{
        message(sprintf("Multiple Lengths! %d rows found",nrow(ls)))
        #      browser()
        for(ll in 1:nrow(ls)){
          message(sprintf("%d: Att = %s, Val = %s",ll,ls$att[ll],ls$valtyp[ll]))
        }
      }
      if(verbose)message(sprintf("Length is %0.2f",as.numeric(ls$valtyp[1])))
      active_len <- as.numeric(ls$valtyp[1])
      
      #GET THE CURRENT DIAMETER
      ds <- gene[gene$att == "Diameter",]
      if(verbose)message(sprintf("DIA,  ds has %d rows ",nrow(ds)))
      ds <- ds[ds$start <= pos$Z[pp] & ds$stop >= pos$Z[pp],] # TODO: Identify which dimension this applies!
      if(verbose)message(sprintf("STST, ds has %d rows ",nrow(ds)))
      ds <- ds[ds$dom == max(ds$dom),]
      if(verbose)message(sprintf("DOM,  ds has %d rows ",nrow(ds)))
      if(nrow(ds) == 1){
        if(verbose)message(sprintf("Diameter is %0.2f",as.numeric(ds$valtyp[1])))
      }
      else{
        if(verbose)message("Multiple Diameters!")
      }
      active_dia <- as.numeric(ds$valtyp[1])
      #####################################################################
      
      if(verbose)message(sprintf("Parsing position %d: %0.0f,%0.0f, %0.0f",pp,pos$X[pp],pos$Y[pp],pos$Z[pp]))
      
      #Create the start block
      if(firstblock){
        drawCStoSTLv2(pos[pp,],active_cs,active_len*vts,active_dia,"Z",offset,verbose,initonly=T)
        firstblock<-F
      }
      
      domval <- domInZone(pos[pp,],gene)
      #browser()
      
      active_dir = "N"
      #if(nrow(dirs)>0){
      if(!is.na(domval)){
        dirs <- gene[gene$dom == domval,]
        
        #Select the dominant direction:
        if(verbose)message(sprintf("Max dominance is %0.0f: found %d genes",max(dirs$dom),nrow(dirs)))
        dirs <- dirs[dirs$dom == max(dirs$dom),]
        if(verbose)message(sprintf("after dominance pruning, %d genes remain",nrow(dirs)))
        
        #Get directions from the non-zero valued attributes
        dirs <- dirs[as.numeric(dirs$valtyp)!=0,]
        if(verbose)message(sprintf("Found %d nonzero direction genes, direction is %s",nrow(dirs),dirs$att[1]))
        if(debug)browser()
        if(nrow(dirs)>0){
          pc <- 1
          for(dd in 1:nrow(dirs)){
            
            if(verbose)message(sprintf("Processing active gene %d, name is %s",dd,dirs$att[dd]))
            
            #
            if(debug)
              browser()
            
            #if(str_detect(dirs$att[dd],"^X") & dirs$start[dd]<= pos$X[pp] & dirs$stop[dd] >=  pos$X[pp] ){
            if(attDir(dirs$att[dd],"X") & dirs$start[dd]<= pos$X[pp] & dirs$stop[dd] >=  pos$X[pp] ){
              active_dir <- "X" 
              vts <- as.numeric(dirs$valtyp[dd])

              posnext <- addpos(pos$X[pp]+(active_len*vts),pos$Y[pp],pos$Z[pp],posnext,seenpos)
              seenpos <- addpos(pos$X[pp]+(active_len*vts),pos$Y[pp],pos$Z[pp],seenpos)
              if(verbose)printpos(posnext,"posnext is now:")
            } 
            #if(str_detect(dirs$att[dd],"^Y") & dirs$start[dd]<= pos$Y[pp] & dirs$stop[dd] >=  pos$Y[pp]){
            if(attDir(dirs$att[dd],"Y") & dirs$start[dd]<= pos$Y[pp] & dirs$stop[dd] >=  pos$Y[pp] ){
              active_dir <- "Y" 
              vts <- as.numeric(dirs$valtyp[dd])
              ##posnext[2] <-posnext[2]+active_len
              posnext <- addpos(pos$X[pp],pos$Y[pp]+(active_len*vts),pos$Z[pp],posnext,seenpos)
              seenpos <- addpos(pos$X[pp],pos$Y[pp]+(active_len*vts),pos$Z[pp],seenpos)
              if(verbose)printpos(posnext,"posnext is now:")
            } 
            #if(str_detect(dirs$att[dd],"^Z") & dirs$start[dd]<= pos$Z[pp] & dirs$stop[dd] >= pos$Z[pp]){
            if(attDir(dirs$att[dd],"Z") & dirs$start[dd]<= pos$Z[pp] & dirs$stop[dd] >=  pos$Z[pp] ){
              active_dir <- "Z" 
              vts <- as.numeric(dirs$valtyp[dd])
              #posnext[3] <-posnext[3]+active_len

              posnext <- addpos(pos$X[pp],pos$Y[pp],pos$Z[pp]+(active_len*vts),posnext,seenpos)
              seenpos <- addpos(pos$X[pp],pos$Y[pp],pos$Z[pp]+(active_len*vts),seenpos)
              if(verbose)printpos(posnext,"posnext is now:")
            } 
            if(verbose)message(sprintf("Active Direction is %s",active_dir))
            
            #Draw what we have
            #draw_gene_to_stl(pos,active_cs,active_len,active_dia)
            if(active_dir != "N"){
              if(verbose)message(sprintf("Drawing a segment at pos %0.0f,%0.0f,%0.0f. active_len=%0.0f, vts=%0.0f",
                              pos$X[pp],pos$Y[pp],pos$Z[pp],active_len,vts))
              drawCStoSTLv2(pos[pp,],active_cs,active_len*vts,active_dia,active_dir,offset,verbose)
            }
          }
        }
      }
    }
    
    #if(active_dir == "N"){
    if(is.null(posnext)){
      if(debug)browser()
      if(verbose)message("No active positions to parse!")
      break
    }
    pos <- posnext
    
    # Limit infinite runs
    if(iterations > runlim){
      if(debug)browser()
      message("finished (iterations > runlim)")
      running<-F
    }
    iterations <- iterations + 1
    
    if(verbose)message(sprintf("Running is %d, iterations = %d, runlim = %d\n",running,iterations,runlim))
  }
  
  if(solo){
    cat("endsolid Exported from Blender-2.80 (sub 75)\n")
    sink()
  }
  
  #create a return data object
  pdata <- list()
  pdata$iterations <- iterations
  pdata$positions <- seenpos
  
  return(pdata)
  
}
