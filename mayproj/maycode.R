




#' Add a node to a dataframe of nodes
#' 
#' @param nodes the dataframe of nodes
#' @param n the unique index
#' @param xpos the x position
#' @param ypos the y poosition
#' @param level the number of nodes from the foundation node
#' @param slevel the number of nodes from the/a stimulus
#' @param type the node type
#' @return the node dataframe with the new node added
addnode <- function(nodes,n=0,xpos=0,ypos=0,level=-1,slevel=-1,type="U"){
  
  newrow <- data.frame(
    n = n,
    x = xpos ,
    y= ypos, 
    level = level, 
    type = type, 
    slevel = slevel,    # the 'stimulus level' a node has - zero means stimulus is adjacent
    stringsAsFactors = F)
  
  if(is.null(nodes))
    nodes <- newrow
  else
    nodes <- rbind(nodes,newrow, stringsAsFactors = F)
  
  return(nodes)
  
}




#' Add an edge to a dataframe of edges
#' 
#' @param edges the dataframe of edges
#' @param nodes the dataframe of nodes
#' @param n the node to generate the edge from
#' @param dir the direction to add the edge
#' @param dist the distance to look for neighbouring nodes
#' @return the node dataframe with the new node added
addedge <- function(edges=NULL,nodes,n,dir,dist){
  
  #Identify the node to connect to: 
  n1 <- nodes[nodes$n==n,]
  
  if(nrow(n1)!=1){
    message(sprintf("ERROR: found %d rows for node %d",nrow(n1),n))
    return(NULL)
  }
  
  found <- F
  if(dir == "W"){
    n2 = nodes[nodes$x == (n1$x[1]-dist) & nodes$y == n1$y[1],]
    if(nrow(n2)!=1){
      message(sprintf("ERROR: found %d rows for neighbour node",nrow(n2),n))
      return(NULL)
    }
    else{
      found = T
    }
  }
  if(dir == "S"){
    n2 = nodes[nodes$x == (n1$x[1]) & nodes$y == (n1$y[1]-dist),]
    if(nrow(n2)!=1){
      message(sprintf("ERROR: found %d rows for neighbour node",nrow(n2),n))
      return(NULL)
    }
    else{
      found = T
    }
  }
  if(dir == "SW"){
    n2 = nodes[nodes$x == (n1$x[1]-dist) & nodes$y == (n1$y[1]-dist),]
    if(nrow(n2)!=1){
      message(sprintf("ERROR: found %d rows for neighbour node",nrow(n2),n))
      return(NULL)
    }
    else{
      found = T
    }
  }
  
  if(dir == "SEh"){#to SouthEast on a hexagon grid
    #ostep <- dist
    n2 <- nodes[nodes$x == (n1$x[1]+(dist/2)) & abs( nodes$y  - (n1$y[1]-(dist*sqrt(3)/2)) )<(dist*0.1),]
    if(nrow(n2)!=1){
      message(sprintf("ERROR NEh: found %d rows for neighbour node",nrow(n2),n))
      #return(NULL)
    }
    else{
      found = T
    }
  }
  
  if(dir == "SWh"){#to SouthEast on a hexagon grid
    #ostep <- dist
    n2 <- nodes[nodes$x == (n1$x[1]-(dist/2)) & abs( nodes$y  - (n1$y[1]-(dist*sqrt(3)/2)) )<(dist*0.1),]
    if(nrow(n2)!=1){
      message(sprintf("ERROR NEh: found %d rows for neighbour node",nrow(n2),n))
      #return(NULL)
    }
    else{
      found = T
    }
  }
  
  if(found){
    newrow <- data.frame(from = n1$n[1], to = n2$n[1], type = "U")
    
    if(is.null(edges))
      edges <- newrow
    else
      edges <- rbind(edges,newrow, stringsAsFactors = F)
  }
  
  return(edges)
}



#' Generate a regular network of nodes and edges
#' 
#' @param xlim the x range of the network 
#' @param ylim the y range of the network
#' @param ostep the spacing of the nodes
#' @return the constructed network
makenet <- function(xlim=c(-100,100),ylim=c(0,200),ostep=20){

  # generate the nodes: 
  nodes <- NULL
  edges <- NULL
  n = 1
  e = 1
  offset = F
  for(yy in seq(ylim[1],ylim[2],ostep)){
    offset <- !offset
    for(xx in seq(xlim[1],xlim[2],ostep)){
      #if (offset){
      #  nodes <- addnode(nodes,n=n,xpos=xx,ypos=yy)
      #}
      #else{
      #  nodes <- addnode(nodes,n=n,xpos=(xx-(ostep/2)),ypos=yy)
      #}
      nodes <- addnode(nodes,n=n,xpos=xx,ypos=yy)
      
      #horizontal edges: 
      if(xx>xlim[1])
        edges <- addedge(edges,nodes,n,dir="W",ostep)
      #vertical edges: 
      if(yy>ylim[1])
        edges <- addedge(edges,nodes,n,dir="S",ostep)
      #vertical edges: 
      if(xx>xlim[1] & yy>ylim[1])
        edges <- addedge(edges,nodes,n,dir="SW",ostep)
      
      
      #increment the node number
      n = n+1
    }
  }
  
  net <- list()
  net$n <- nodes
  net$e <- edges
  
  return(net)
}



#' Generate a regular hexagonal network of nodes and edges
#' 
#' @param xlim the x range of the network 
#' @param ylim the y range of the network
#' @param ostep the spacing of the nodes
#' @return the constructed network
makehexnet <- function(xlim=c(-100,100),ylim=c(0,200),ostep=20){
  
  # generate the nodes: 
  nodes <- NULL
  edges <- NULL
  n = 1
  e = 1
  offset = F
  
  overt = ostep * sqrt(3)/2
  
  for(yy in seq(ylim[1],ylim[2],overt)){
    offset <- !offset
    for(xx in seq(xlim[1],xlim[2],ostep)){
      if (offset){
        #nodes <- addnode(nodes,n=n,xpos=xx,ypos=yy)
        xpos <- xx
      }
      else{
        xpos <- xx-(ostep/2)
      }
      nodes <- addnode(nodes,n=n,xpos=xpos,ypos=yy)
      #nodes <- addnode(nodes,n=n,xpos=xx,ypos=yy)
      
      #horizontal edges: 
      if(xx>xlim[1]){
        edges <- addedge(edges,nodes,n,dir="W",ostep)
      }
      #right-leaning edges: 
      if(yy>ylim[1] & xpos<xlim[2]){
        edges <- addedge(edges,nodes,n,dir="SEh",ostep) 
      }
      
      if(yy>ylim[1] & xpos>xlim[1]){
        edges <- addedge(edges,nodes,n,dir="SWh",ostep) 
      }
      
      #else{
      #  
      #}
      #if(yy>ylim[1])
      #  edges <- addedge(edges,nodes,n,dir="S",ostep)
      #vertical edges: 
      #if(xx>xlim[1] & yy>ylim[1])
      #  edges <- addedge(edges,nodes,n,dir="SW",ostep)
      
      
      #increment the node number
      n = n+1
    }
  }
  
  net <- list()
  net$n <- nodes
  net$e <- edges
  
  return(net)
}





#' Set node type within a region
#' 
#' @param net the network
#' @param x0 the min x value
#' @param y0 the min y value
#' @param x1 the max x value
#' @param y1 the max y value
#' @return the constructed network
setnodetype <- function(net,x0,y0,x1,y1,type="F"){
  net$n$type[net$n$x>x0 & net$n$x<x1 & net$n$y>y0 & net$n$y<y1] <- type
  return(net)
}


#' Draw the state of a network, with optional stimuli and inhibs
#' 
#' @param net the network
#' @param stim dataframe of stimuli
#' @param inhib dataframe of inhibs
#' @param xlim x plot range
#' @param ylim y plot range
plotnet <- function(pnet,stim=NULL,inhib=NULL,xlim=NULL,ylim=c(0,200),pcols=NULL,newplot=T){
  nodes <- pnet$n
  edges <- pnet$e
  
  if(is.null(ylim))ylim=range(nodes$y)
  if(is.null(xlim))xlim=range(nodes$x)
  
  if(newplot)
    plot(NA,xlim=xlim,ylim=ylim,asp=T,xlab="x",ylab="y")
  
  # Draw the inhibition regions
  if(!is.null(inhib) & newplot){
    rect(xleft = inhib$x0, ybottom = inhib$y0, xright = inhib$x1, ytop = inhib$y1, col="darkcyan",lty=0)
  }
  
  # Draw the edges
  if(!is.null(edges)){
    for(ee in 1:nrow(edges)){
      n0 <- nodes[nodes$n == edges$from[ee],]
      n1 <- nodes[nodes$n == edges$to[ee],]
      segments(x0=n0$x,x1=n1$x,y0=n0$y,y1=n1$y,lwd = 2,col="grey80")
    }
  }
  
  # Draw the nodes
  un <- nodes[nodes$type=="U",]
  points(x=un$x,y=un$y,pch=20,cex=2,col="grey80")
  fn <- nodes[nodes$type=="F",]
  points(x=fn$x,y=fn$y,pch=20,cex=2,col="black")
  sn <- nodes[nodes$type=="S",]
  points(x=sn$x,y=sn$y,pch=1,cex=1.5,col="red",lwd=3)
  xn <- nodes[nodes$type=="X",]
  points(x=xn$x,y=xn$y,pch=20,cex=2,col="darkcyan")
  
  # Draw the stimulus 
  if(!is.null(stim))
    points(x=stim$x,y=stim$y,pch=20,cex=2,col="red")
  
  #Draw the propagation now: 
  if(max(nodes$level)>-1){
    if(is.null(pcols))
      pcols = rainbow(max(nodes$level))
    for(pp in 1:max(nodes$level)){
      pn <- nodes[nodes$level==pp & nodes$type!="F",]
      if(nrow(pn)>0)
        points(x=pn$x,y=pn$y,pch=20,col=pcols[pp])
    }
  }
  
  #points(x = runif(50,xlim[1],xlim[2]),y=runif(50,ylim[1],ylim[2]))
  points(x=xlim[2]*10,y=ylim[2]*10)
}




#' Update the state of all nodes in the network (stimuli etc detected elsewhere)
#' 
#' @param network the network
#' @return the updated network
propagate <- function(network){
  
  edges <- network$e
  
  if(max(network$n$level)==-1){
    sn <- network$n[network$n$type=="F",]
    sl <- 1
    
    pe <- edges[edges$from %in% sn$n | edges$to %in% sn$n,]
    newn <- unique(c(pe$from,pe$to))
    newn <- newn[!(newn %in% sn$n)]
    
    network$n$level[network$n$n %in% newn & network$n$level == -1 & network$n$type != "X"] <- sl
  }
  else{
    sn <- network$n[network$n$level==max(network$n$level),]
    sl <- max(sn$level)+1
    #message(sprintf("adding level %d ",sl))
    
    pe <- edges[edges$from %in% sn$n | edges$to %in% sn$n,]
    newn <- unique(c(pe$from,pe$to))
    newn <- newn[!(newn %in% sn$n)]
    
    network$n$level[network$n$n %in% newn & network$n$level == -1 & network$n$type != "X"] <- sl
    network$n$type[network$n$n %in% newn & network$n$level == -1 & network$n$type != "X"] <- "S"
    
    message(sprintf("Adding slevel %d to nodes %d",sl,network$n$n[network$n$n %in% newn & network$n$level == -1]))
  }
  
  return(network)
}





#' Check for stimuli in a region around a network level
#' 
#' @param net the network
#' @param stim the stimuli
#' @param level the level to check
#' @param odist the distance threshold for stimuli
#' @param verbose whether to print diagnostics
#' @return the updated network
checkstim <- function(net,stim,level,odist=20,verbose = T){
  
  if(verbose)message(sprintf("Checking stim at %0.2f,%0.2f",stim$x,stim$y))
  
  found <- F
  nodes <- net$n[net$n$level == level,]
  if(nrow(nodes)>0){
    for(nn in 1:nrow(nodes)){
      edist <- sqrt((nodes$x[nn]-stim$x)^2 + (nodes$y[nn]-stim$y)^2)
      if(edist<odist){
        message(sprintf("Found stimulus at level %d",level))
        found <- T
        
        net$n$slevel[net$n$n == nodes$n[nn]] = 0
        net$n$type[net$n$n == nodes$n[nn]] = "S"
        
      }
    }
  }
  
  return(net)
}




#' propagate stimulus info around a network
#' 
#' @param net the network
#' @param verbose whether to print diagnostics
#' @return the updated network
propstim <- function(net,verbose=T){
  
  # get the current maximum level
  msl <- max(net$n$slevel)
  
  # find all nodes at this level
  sn <- net$n[net$n$slevel == msl,]
  fl <- min(net$n$level[net$n$slevel == msl])
  
  msl <- msl+1
  message(sprintf("adding stimulus level %d ",msl))
  
  edges <- net$e
  
  # get everything connected to the current slevel nodes: 
  pe <- edges[edges$from %in% sn$n | edges$to %in% sn$n,]
  newn <- unique(c(pe$from,pe$to))
  newn <- newn[!(newn %in% sn$n)]
  
  # Might need to check what the level is to set this: 
  net$n$slevel[
    net$n$n %in% newn & 
      net$n$level == fl-1 & 
      net$n$slevel == -1 ] <- msl
  net$n$type[
    net$n$n %in% newn & 
      net$n$level == fl-1 & 
      net$n$slevel == msl ] <- "S"
  
  
  return(net)
}




#' run generative growth - the main function for generative networks
#' 
#' @param netlist list of networks, one for each foundation
#' @param stim the stimuli
#' @param inhib the inhibs
#' @param nsteps the max iterations
#' @param xlim x plot limits
#' @param ylim y plot limits
#' @param pcols colors for plotting
#' @param verbose whether to print diagnostics
#' @return the updated network
rungrowth <- function(netlist, stim = NULL, inhib=NULL, odist=20,  nsteps = NULL, xlim, ylim, pcols = NULL){
  #net <- innet
  #nsteps <- 15
  if(is.null(pcols))  pcols  <- rainbow(n=nsteps)
  
  newplot<-T
  for(ll in 1:length(netlist)){
    net <- netlist[[ll]]
    plotnet(net,stim,inhib,xlim=xlim,ylim=ylim,pcols = pcols,newplot = newplot)
    newplot <-F
  }
  
  stimfound <- rep(F,length(netlist))
  for(ppp in 1:nsteps){
    newplot<-T
    #for(ll in 1:length(netlist)){
      net <- netlist[[ppp]]
      net <- propagate(net)
      if(stimfound[ll]){
        message("Running propstim")
        net <- propstim(net)
      }
      else{
        net <- checkstim(net,stim,ppp,odist = odist)
        if("S" %in% net$n$type){
          message("Setting S")
          stimfound[ll] <- T
        }
      }
      plotnet(net,stim,inhib,xlim,ylim,pcols,newplot = newplot)
      newplot <-F
      
      netlist[[ppp+1]] <- net
    #}
  }
  return (netlist)
}
