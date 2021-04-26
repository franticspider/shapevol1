

# This code was inspired by OpenConstructor


VECTOR.len <- function(nodes,n1,n2){
  x <- nodes$x[n1]-nodes$x[n2]
  y <- nodes$y[n1]-nodes$y[n2]
  
  return(sqrt((x*x)+(y*y)))
}



clearForce <- function(nodes){
  nodes$fx <- 0
  nodes$fy <- 0
  return(nodes)
}


# Simply adds the gravity value to the y vector
gForce <- function(nodes, gg= -0.02){
  #nodes$fx <- 0
  nodes$fy <- nodes$fy + gg
  return(nodes)
}


# Friction ("f") opposes velocity...
fForce <- function(nodes, ff = 0.137){
  
  for(nn in 1:nrow(nodes)){
    if(nodes$type[nn] == "S"){
      #//mass.f.add(VECTOR.mul(mass.v, -1 * MODEL.instance.f()));
      nodes$fx[nn] = nodes$fx[nn] + (nodes$vx[nn] * -ff)
      nodes$fy[nn] = nodes$fy[nn] + (nodes$vy[nn] * -ff)
    }
  }
  
  return(nodes)
}




kForce <- function(net,  kk =6.25){
  
  edges <- net$e
  nodes <- net$n
  #message("Looping through edges")
  for(ee in 1:nrow(edges)){
    
    nf <- match(edges$from[ee],nodes$n) # index of node 'from'
    nt <- match(edges$to[ee],nodes$n)   # index of node 'to'
    
    #// hoo"k"e's law (springiness)
    #var ds = VECTOR.sub(spr.m2.s, spr.m1.s);
    # m1 and m2 are the nodes that the edge connects; s is the position vector - so we mean nodes.x and nodes.y
    dsx <- nodes$x[nt] - nodes$x[nf]
    dsy <- nodes$y[nt] - nodes$y[nf]
    
    #var length = VECTOR.mag(ds); // distance between m1 and m2
    length <- sqrt((dsx*dsx)+(dsy*dsy))
    
    #var dh = VECTOR.hat(ds);     // unit direction from m1 to m2
    # var length is NOT used in the javascript - 
    # but it is calculated again *within* the VECTOR.hat function, so we're good doing this: 
    dhx <- dsx/length
    dhy <- dsy/length
    
    #// hooke's law:  F=kX
    #// here, positive magnitude = inward (attraction to other mass)
    #var fMag = MODEL.instance.k() * (length - spr.restlength());
    edges$fMag[ee] <- kk * (length-restlength)
    # For our purposes, we record fMag for each edge as this is whether the spring is in tension or compression
    # Note springiness is bound to oscillate though!
    
    #NB! spr.m1 *references* the mass m1 because of the way javascript works
    #spr.m1.f.add(VECTOR.mul(dh, fMag));
    #spr.m2.f.add(VECTOR.mul(dh, -fMag));
    #message(sprintf("nf = %d, nt = %d",nf,nt))
    # So the force added here is the unit vector dh times fMag - makes sense
    nodes$fx[nf] <- nodes$fx[nf] + (dhx * edges$fMag[ee])
    nodes$fy[nf] <- nodes$fy[nf] + (dhy * edges$fMag[ee])
    
    nodes$fx[nt] <- nodes$fx[nt] - (dhx * edges$fMag[ee])
    nodes$fy[nt] <- nodes$fy[nt] - (dhy * edges$fMag[ee])
    
  }
  
  net$n <- nodes
  net$e <- edges
  
  return(net)
}





integrate <- function(nodes,dt,mm=1){
  #var state = { dt: dt }
  #MODEL.instance.masses.forEach(function(mass) {
  for(nn in 1:nrow(nodes)){
    if(nodes$type[nn] == "S"){
      #// F=ma -> a=F/m
      #// Euler's method - acceleration is force / mass
      # TODO: consider free and fixed masses...
      #if (mass.isFreeMass())
      #{
      #// The order is important.  We must update each accumulator
      #// using the value calculated last frame (not this frame).
      #// Therefore we must add s += v before calculating the new v,
      #// etc.
      # position is updated by velocity
      #mass.s.add(VECTOR.mul(mass.v, this.dt));
      #Update position: 
      nodes$x[nn] <- nodes$x[nn] + (nodes$vx[nn] * dt)
      nodes$y[nn] <- nodes$y[nn] + (nodes$vy[nn] * dt)
      
      # velocity is updated by acceleration
      #mass.v.add(VECTOR.mul(mass.a, this.dt));
      #Update velocity: 
      nodes$vx[nn] <- nodes$vx[nn] + (nodes$ax[nn] * dt)
      nodes$vy[nn] <- nodes$vy[nn] + (nodes$ay[nn] * dt)
      
      # acceleration is updated via forces
      #mass.a = VECTOR.div(mass.f, mass.m());//m is 1(kg)!
      #Update acceleration
      nodes$ax[nn] <- nodes$fx[nn] / mm
      nodes$ay[nn] <- nodes$fy[nn] / mm
      
      #}
      #}, state);
    }
  }
  return(nodes)
}


updateModel <- function(gn, ff=0.137, kk=6.25, dt=0.001666667, ni=300){
  for(dd in 1:ni){
    gn$n <- clearForce(gn$n)
    gn$n <- gForce(gn$n)
    gn$n <- fForce(gn$n,ff)
    gn <-   kForce(gn,kk)
    gn$n <- integrate(gn$n,dt)
  }
  return(gn)
}




plotForces <- function(gnn){
  
  nodes <- gnn$n
  edges <- gnn$e
  for(ee in 1:nrow(edges)){
    if(abs(edges$fMag[ee])>1e-30){#0.000000000001){
      #message(sprintf("gn$e$fMag[%d] = %0.5f",ee,gn$e$fMag[ee]))
      if(edges$fMag[ee]>1e-30){
        n0 <- nodes[nodes$n == edges$from[ee],]
        n1 <- nodes[nodes$n == edges$to[ee],]
        segments(x0=n0$x,x1=n1$x,y0=n0$y,y1=n1$y,lwd = 3,col="red")  
      }else{
        n0 <- nodes[nodes$n == edges$from[ee],]
        n1 <- nodes[nodes$n == edges$to[ee],]
        segments(x0=n0$x,x1=n1$x,y0=n0$y,y1=n1$y,lwd = 3,col="blue")  
      }
    }
  }
}

plotForceMap <- function(nodes,edges){
  gnn <- list()
  gnn$n <- nodes
  gnn$e <- edges
  plotForces(gnn)
  
}


