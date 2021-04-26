#include <Rcpp.h>
using namespace Rcpp;

//TODO: will need to do this in proper C++ file..
//#define DEFAULT_MASS (1.0)


/*clearForce <- function(nodes){
 nodes$fx <- 0
 nodes$fy <- 0
 return(nodes)
}*/
// [[Rcpp::export]]
void rcclearForce(DataFrame ndf){
  NumericVector fx = ndf["fx"];
  NumericVector fy = ndf["fy"];
  
  for(int ii=0;ii<fx.length();ii++){
    fx[ii]=0.0;
    fy[ii]=0.0;
  }
}


// [[Rcpp::export]]
void rcgForce(DataFrame df, double gg=-0.02){
  NumericVector x = df["fy"];
  x = x + gg;
}


/*
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
 }*/
// [[Rcpp::export]]
void rcfForce(DataFrame df, double ff = 0.137){
  //Access the three columns we need: 
  CharacterVector type = df["type"];
  NumericVector fx = df["fx"];
  NumericVector fy = df["fy"];
  NumericVector vx = df["vx"];
  NumericVector vy = df["vy"];
  
  //Set up iterators
  CharacterVector::iterator itype = type.begin();
  NumericVector::iterator ify = fy.begin();
  NumericVector::iterator ivx = vx.begin();
  NumericVector::iterator ivy = vy.begin();
  for(NumericVector::iterator ifx = fx.begin(); ifx != fx.end(); ifx++) {
    if(*itype == "S"){
      *ifx = *ifx + (*ivx * -ff);
      *ify = *ify + (*ivy * -ff);
    }
    itype++;
    ify++;
    ivx++;
    ivy++;
  }
}


//NB do NOT export because R needs to +1 to the index and C doesn't!
int matchnode(NumericVector nn, int eeval){
  for(int idx = 0;idx < nn.length();idx++){
    if((int) nn[idx]== (int) eeval)
      return idx;
  }
  Rprintf("ERROR: Can't find edge node %d in node list!\n",eeval);
  return 0; //TODO: should find a better way to flag an error than this!
}


// [[Rcpp::export]]
void rckForce(DataFrame ndf, DataFrame edf, double restlength, double kk = 6.25){
  
  //edges <- net$e
  //nodes <- net$n
  NumericVector efrom = edf["from"];
  NumericVector eto = edf["to"];
  NumericVector efmag = edf["fMag"];
  
  NumericVector noden = ndf["n"];
  NumericVector nodex = ndf["x"];
  NumericVector nodey = ndf["y"];
  NumericVector nodefx = ndf["fx"];
  NumericVector nodefy = ndf["fy"];
  
  //for(ee in 1:nrow(edges)){
  for(int ee = 0;ee < efrom.length();ee++){
    
    //nf <- match(edges$from[ee],nodes$n) # index of node 'from'
    //nt <- match(edges$to[ee],nodes$n)   # index of node 'to'
    int nf = matchnode(noden,(int) efrom[ee]);
    int nt = matchnode(noden,(int) eto[ee]);
    
    //#// hoo"k"e's law (springiness)
    //#var ds = VECTOR.sub(spr.m2.s, spr.m1.s);
    //# m1 and m2 are the nodes that the edge connects; s is the position vector - so we mean nodes.x and nodes.y
    //dsx <- nodes$x[nt] - nodes$x[nf]
    //dsy <- nodes$y[nt] - nodes$y[nf]
    double dsx = nodex[nt] - nodex[nf];
    double dsy = nodey[nt] - nodey[nf];
    
    //#var length = VECTOR.mag(ds); // distance between m1 and m2
    //length <- sqrt((dsx*dsx)+(dsy*dsy))
    double length =  sqrt((dsx*dsx)+(dsy*dsy));
    
    //dhx <- dsx/length
    //dhy <- dsy/length
    double dhx = dsx/length;
    double dhy = dsy/length;
    
    //#// hooke's law:  F=kX
    //#// here, positive magnitude = inward (attraction to other mass)
    //edges$fMag[ee] <- kk * (length-restlength)
    efmag[ee] = kk * (length-restlength);
    
    //# So the force added here is the unit vector dh times fMag - makes sense
    //nodes$fx[nf] <- nodes$fx[nf] + (dhx * edges$fMag[ee])
    //nodes$fy[nf] <- nodes$fy[nf] + (dhy * edges$fMag[ee])
    nodefx[nf] += dhx * efmag[ee];
    nodefy[nf] += dhy * efmag[ee];
    
    //nodes$fx[nt] <- nodes$fx[nt] - (dhx * edges$fMag[ee])
    //nodes$fy[nt] <- nodes$fy[nt] - (dhy * edges$fMag[ee])
    nodefx[nt] -= dhx * efmag[ee];
    nodefy[nt] -= dhy * efmag[ee];
    
  }
}


// [[Rcpp::export]]
void rcintegrate(DataFrame ndf, double dt, double mm=1.0, int move = 1){
  
  CharacterVector ntype = ndf["type"];
  
  NumericVector noden = ndf["n"];
  NumericVector nx = ndf["x"];
  NumericVector ny = ndf["y"];
  NumericVector nfx = ndf["fx"];
  NumericVector nfy = ndf["fy"];
  NumericVector nax = ndf["ax"];
  NumericVector nay = ndf["ay"];
  NumericVector nvx = ndf["vx"];
  NumericVector nvy = ndf["vy"];
  
  //for(nn in 1:nrow(nodes)){
  for(int nn=0;nn<ntype.length();nn++){
    //if(nodes$type[nn] == "S"){
    if(ntype[nn] == "S"){
      //#// F=ma -> a=F/m
      //#// Euler's method - acceleration is force / mass
      //# TODO: consider free and fixed masses...
      //#if (mass.isFreeMass())
      //#{
      //#// The order is important.  We must update each accumulator
      //#// using the value calculated last frame (not this frame).
      //#// Therefore we must add s += v before calculating the new v,
      //#// etc.
      //# position is updated by velocity
      //#mass.s.add(VECTOR.mul(mass.v, this.dt));
      //#Update position: 
      //nodes$x[nn] <- nodes$x[nn] + (nodes$vx[nn] * dt)
      //nodes$y[nn] <- nodes$y[nn] + (nodes$vy[nn] * dt)
      if(move==1){
        nx[nn] += nvx[nn] * dt;
        ny[nn] += nvy[nn] * dt;
      }
      //# velocity is updated by acceleration
      //#mass.v.add(VECTOR.mul(mass.a, this.dt));
      //#Update velocity: 
      //nodes$vx[nn] <- nodes$vx[nn] + (nodes$ax[nn] * dt)
      //nodes$vy[nn] <- nodes$vy[nn] + (nodes$ay[nn] * dt)
      nvx[nn] += nax[nn] * dt;
      nvy[nn] += nay[nn] * dt;
      
      //# acceleration is updated via forces
      // f = ma, so a = f/m
      //#mass.a = VECTOR.div(mass.f, mass.m());//m is 1(kg)!
      //#Update acceleration
      //nodes$ax[nn] <- nodes$fx[nn] / mm
      //nodes$ay[nn] <- nodes$fy[nn] / mm
      nax[nn] = nfx[nn]/mm;
      nay[nn] = nfy[nn]/mm;
    }
  }
  //return(nodes)
}


//updateModel <- function(gn, ff=0.137, kk=6.25, dt=0.001666667, ni=300){
// [[Rcpp::export]]
void rcupdateModel(DataFrame gnn, DataFrame gne, double restlength, double ff=0.137, double kk=6.25, double dt=0.001666667, int ni=300, double mm = 1.0, int move = 1){
  //for(dd in 1:ni){
  for(int dd=0;dd<ni;dd++){
    rcclearForce(gnn);
    
    //gn$n <- gForce(gn$n)
    rcgForce(gnn);
    
    //gn$n <- fForce(gn$n,ff)
    rcfForce(gnn,ff);
    
    //gn <-   kForce(gn,kk)
    rckForce(gnn,gne,restlength,kk);
    
    //gn$n <- integrate(gn$n,dt)
    rcintegrate(gnn,dt,mm,move);
  }
  //return(gn)
}




// Testing ideas only... leaving here for reference (for now)
void trymatch(DataFrame ndf, DataFrame edf){
  Rprintf("Trying out rcpp match\n");
  
  IntegerVector efrom = as<IntegerVector>(edf["from"]);
  //IntegerVector eto = edf["to"];
  IntegerVector noden = as<IntegerVector>(ndf["n"]);
  
  for(int ii=0;ii<3;ii++){
    
    //nf <- match(edges$from[ee],nodes$n) # index of node 'from'
    //nt <- match(edges$to[ee],nodes$n)   # index of node 'to'
    // MATCH DOESN'T SEEM TO WORK HOW I NEED IT TO...
    //int nf = match(efrom[ii],ndf);
    //int nt = match(eto[ii],ndf);
    int idx=0;
    for(IntegerVector::iterator inoden = noden.begin(); inoden != noden.end(); inoden++) {
      if(*inoden==efrom[ii]){
        Rprintf("Found!\n");
        //idx = std::distance(noden.begin(), inoden);
        break;
      }
      idx++;
    }
    
    Rprintf("idx = %d, efrom[idx] = %d, ndf[idx] = %d\n",idx,efrom[ii],noden[idx]);
    // Rprintf("nt = %d, eto[ii] = %d, ndf[nt] = %d\n",nf,eto[ii],ndf[nt]);
    for(idx=0;idx<noden.length();idx++){
      if(noden[idx]==efrom[ii])
        break;
    }
    Rprintf("idx = %d, efrom[idx] = %d, ndf[idx] = %d\n",idx,efrom[ii],noden[idx]);
  }
}