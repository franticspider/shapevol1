

#' standard activation function
act.fct <- function (x) 
{
  1/(1 + exp(-x))
}

#' initialise a NEAT network
#' 
#' @param inodes the input nodes
#' @return a boolean value indicating whether the gene is in this range
#' @examples
#' nn <- init.neat(c("x","y"))
init.neat <- function(inodes,templatenetwork=NULL){
  
  
  if(is.null(templatenetwork)){
    tconns <- data.frame(nin = c("x","y","1"),nout = c("1","1","i"),w=runif(3),stringsAsFactors = F)
    tnodes <- data.frame(node = c("1","i"), parsed = c(T,T), layer= c(1,2), rank= c(1,1),stringsAsFactors = F)
    tpn <- list()
  
    tpn$conns <- tconns
    tpn$nodes <- tnodes
    tpn$inodes <- inodes
  }
  else{
    tpn <- templatenetwork
    tpn$conns$w <- runif(nrow(tpn$conns),min=-10,max = 10)
  }
  
  return(tpn)

}


mut.addconnection <- function(net){
  
  # 
}



#' mutate a NEAT network
#' 
#' @param inodes the input nodes
#' @return a boolean value indicating whether the gene is in this range
mut.net <- function(net,noderate=0.0,wtstep= 0.05, mp = NULL, midx = NULL, verbose = T){
  if(is.null(mp))mp <- runif(1,0,1)

  if(mp<noderate){            
    if(verbose)message(sprintf("  node mutation (mp = %f, noderate = %f, midx=%d, w = %f)",mp,noderate,midx,net$conns$w[midx])) 
  }
  else{
    if(is.null(midx))midx <- sample(1:length(net$conns$w),size=1)
    oldval <- net$conns$w[midx]    
    net$conns$w[midx] <- max(-10,min(10,net$conns$w[midx]+sample(c(wtstep,-wtstep),1)))
    if(verbose) message(sprintf("  WEIGHT mutation (mp = %f, noderate = %f, midx=%d, was = %f now = %f)",mp,noderate,midx,oldval,net$conns$w[midx]))   
  }
  
  return(net)
}





#' Make a hidden nodes list from a dataframe of connections
#' 
proc.neat <- function(neat,ivals,summarize=F,verbose=T){

  output <- neat$nodes
  output$ival <- 0
  output$oval <- 0
  
  for(layer in 1:max(neat$nodes$layer)){
    nd <- output[output$layer == layer,]
    if(verbose)message(sprintf("processing layer %d, found %d nodes",layer,nrow(nd)))
    for(nn in 1:nrow(nd)){
      oidx <- match(nd$node[nn],output$node)
      con <- neat$conns[neat$conns$nout == nd$node[nn],]
      if(verbose)message(sprintf("  index is %d, node is %s, found %d connections",oidx,nd$node[nn],nrow(con)))
      for(cc in 1:nrow(con)){
        #oidx <- match(con$nout[cc],output$node)
        if(con$nin[cc] %in% neat$inodes){
          if(verbose)message(sprintf("  processing input node %s to %s",con$nin[cc],con$nout[cc]))
          vidx <- match(con$nin[cc],neat$inodes)
          output$ival[oidx] <- output$ival[oidx] + (ivals[vidx] * con$w[cc])
        }
        else{
          if(verbose)message(sprintf("  processing hidden node %s to %s",con$nin[cc],con$nout[cc]))
          vidx <- match(con$nin[cc],output$node)
          if(verbose)message(sprintf("    vidx = %d, output$oval[vidx] = %f",vidx,output$oval[vidx]))
          output$ival[oidx] <- output$ival[oidx] + (output$oval[vidx] * con$w[cc])
        }
      }
      output$oval[oidx] <- act.fct(output$ival[oidx])
      if(verbose)message(sprintf("  row %d: act.fun(%f) = %f",oidx,output$ival[oidx],output$oval[oidx]))
    }
  }
    
  if(summarize){
    #TODO: deal with differnt output node names
    return(output$oval[output$node == "i"])
  }else{
    return(output)   
  }
}



#' Make a hidden nodes list from a dataframe of connections
#' 
getHnodes <- function(clc,inodes,onodes = NULL){
  nodes <- unique(c(clc$nin,clc$nout))
  nodes <- data.frame(c("node" = nodes),stringsAsFactors = F)
  nodes$parsed <- F
  colnames(nodes) <- c("node","parsed")
  rownames(nodes) <- NULL
  hnodes <- nodes[!(nodes$node %in% c(inodes)),]
  return (hnodes)
}


#' Calculate the number of 'layers' in a NEAT network
#' 
#' 
#' 
calclayers<-function(clc,inodes,onodes,hnodes = NULL,verbose=F){
  
  #CALCLAYERS function
  clc$parsed <- F
  clc$layer <- 0
  layer <- 1
  
  # We could pass in this list if we had it and do `if(is.null(hnodes))`
  if(is.null(hnodes)){
    hnodes <- getHnodes(clc,inodes)
  }
  if(verbose)print(hnodes)
  
  #nodes$parsed[!(nodes$node %in% hnodes$node)] <- T
  
  hnodes$layer <- 0
  hnodes$rank <- 0
  
  while(!all(hnodes$parsed)){
    if(verbose)message(sprintf("\nparsing layer %d",layer))
    
    #all parsed nodes:
    #pn  <- unique(c(nodes$node[nodes$parsed],hnodes$node[hnodes$parsed]))
    pn  <- unique(c( inodes,                  hnodes$node[hnodes$parsed]))
    
    #all unparsed hidden nodes:
    hn <- hnodes[!hnodes$parsed,]
    if(verbose){
      message("hidden nodes are")
      message(hn)
    }
    
    # Rank is the vertical position in the network (processing l to r) - used for plotting
    rank <- 1
    
    #go through each unparsed hidden node
    for(hh in 1:nrow(hn)){
      
      #find all the connections where the output node is the current node
      cc<- clc[clc$nout == hn$node[hh],]
      if(verbose){
        message(sprintf("node %s: cc input nodes for this output are",hn$node[hh]))
        message(cc$nin)
        message("parsed nodes are")
        message(pn)
      }
      
      #Get the parsed nodes: 
      if(all(cc$nin %in% pn)){
        if(verbose)message(sprintf("Found all inputs for node %s",hn$node[hh]))
        
        clc$layer[clc$nout == hn$node[hh]]<-layer
        clc$parsed[clc$nout == hn$node[hh]]<-T
        hnodes$parsed[hnodes$node == hn$node[hh]]<- T
        
        hnodes$layer[hnodes$node == hn$node[hh]] <- layer
        hnodes$rank[hnodes$node == hn$node[hh]] <- rank
        rank <- rank + 1
        if(verbose){
          message("parsed hnodes are")
          message(hnodes$node[hnodes$parsed])
        }
      }
      else{
        if(verbose)message(sprintf("Unparsed inputs remain for node %s",hn$node[hh]))
      }
    }
    
    layer <- layer + 1
    
    if(layer > 10)break
  }
  
  nn <- list()
  nn$conns <- clc
  nn$nodes <- hnodes
  nn$inodes <- inodes
  
  #CALCLAYERS returns
  return(nn)
}


oddrank <- function(nodes,layer){
 
  nodes <- nodes[nodes$layer == layer,]
  
  topr <- max(nodes$rank)
  
  return ((topr%%2)/2)
}


#' plot a NEAT network
#' @param 
plot.neat <- function(pconns,inodes=NULL,onodes,showaxes=T,verbose=F){
  
  
  if(is.null(inodes))
    inodes <- pconns$inodes
  
  #ispc<- ydim[2]/(length(inodes)-1)
  pmargin <- 0.2
  
  plot(NA,xlim=c(-pmargin,max(pconns$nodes$layer)+pmargin),ylim=c(1-pmargin,max(pconns$nodes$rank,length(inodes))+pmargin+0.5),axes=showaxes,
       xlab="",ylab="")
  
  totlayers <- max(pconns$nodes$layer)

  
  
  #EDGES
  
  for(ee in 1:nrow(pconns$conns)){
    if(pconns$conns$nin[ee] %in% inodes){
      x0 <- 0
      idx0 <- match(pconns$conns$nin[ee],inodes)
      y0 <- idx0 
    }
    else{
      idx0 <- match(pconns$conns$nin[ee],pconns$nodes$node)
      x0 <- pconns$nodes$layer[idx0]
      y0 <- pconns$nodes$rank[idx0] + oddrank(pconns$nodes,x0)
    }
    if(is.na(idx0))message(sprintf("Bad idx0 for row %d",ee))
    else{
    
      idx1 <- match(pconns$conns$nout[ee],pconns$nodes$node)
      if(is.na(idx1))message(sprintf("Bad idx1 for row %d",ee))
      else{
        x1 <- pconns$nodes$layer[idx1]
        y1 <- pconns$nodes$rank[idx1] + oddrank(pconns$nodes,x1)
        
        if(verbose)message(sprintf("Parsing row %d: %s to %s x0= %0.1f, y0= %0.1f, x1= %0.1f, y1=%0.1f",
                        ee,
                        pconns$conns$nin[ee],
                        pconns$conns$nout[ee],
                        x0,y0,x1,y1))
        segments(x0=x0,y0=y0,x1=x1,y1=y1)
      }
    }
  }
  
  #NODES
  
  #Draw the input nodes
  for(rr in 1:length(inodes)){
    #draw.circle(x=0,y=rr,radius=0.2,col="red",border="green")
    points(x=0,y=rr,cex=5,pch=19,col="white")
    points(x=0,y=rr,cex=5)
    text(x=0,y=rr,labels = inodes[rr])
  }
  
  
  #Draw the nodes
  for (rr in 1:nrow(pconns$nodes)){
    x <- pconns$nodes$layer[rr]
    y <- pconns$nodes$rank[rr] + oddrank(pconns$nodes,x)
    points(x=x,y=y,cex=5,pch=19,col="white")
    points(x=x,y=y,cex=5)
    text(x=x,y=y,labels = pconns$nodes$node[rr])
  }
  
}



