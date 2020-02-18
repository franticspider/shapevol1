




#' execute a novelty search run
#' 
#' @param geneset the 'seed' shape,
#' @param popsize the population size
#' @param ngen the number of generations
#' @param elite the number of highest ranking individuals that will be copied to the next generation
#' @param lambda the number of highest ranking individuals that will be added to the archive each generation
#' @param seed the random number seed
#' @param save whether to save the gene data to a file
#' @param verbose whether to generate messages while processing
#' @return a copy of the input geneset with the mutation
#' @examples
#' mutant <- domut(3,1,2,1,geneset)
novsearch <- function(geneset,popsize=100,ngen=100,elite=5,lambda=2,seed=436,fnroot="ns",rwalk=F,save=F,verbose=T){
  
  #source("../shapevol1/R/domut.R")
  #require(stringr)
  
  # Initialize population
  #popsize <- 100
  #elite <- 5
  #lambda <- 2
  poslim <- 350
  #ngen <- 100
  
  set.seed(seed)
  gpop <- list()
  ppop <- list()
  npop <- list()
  
  if(!rwalk){
    archive <- list()
    alen <- 1
  }
  
  fn <- sprintf("%s_gen001.stl",fnroot)  
  sink(file = fn)
  cat("solid Exported from Blender-2.80 (sub 75)\n")
  xpos <- -poslim
  ypos <- -poslim
  for(pp in 1:popsize){
    gpop[[pp]] <- mutate.gene(geneset)
    ppop[[pp]] <- genetostlfile4   (sprintf("tmp.stl"),gpop[[pp]],pos=spos(0, 0,0),offset=c(xpos,ypos,0),runlim=100,solo=F)
    xpos<-xpos + 50
    if(xpos == poslim){
      xpos <- -poslim
      ypos <- ypos + 50 
    }
  }
  cat("solid Exported from Blender-2.80 (sub 75)\n")
  sink()
  
  
  dhits <- matrix(nrow=popsize,ncol=ngen)
  
  # Iterate
  for(nn in 1:ngen){
    if(verbose)message(sprintf("Processing generation %d of %d",nn,ngen))
    
    for(pp in 1:popsize){
      npop[[pp]] <- getNSvals(ppop[[pp]])
    }
    for(pp in 1:popsize){
      if(lambda>0)
        dpop[pp] <- meankdist(c(npop,archive),pp)  #NSdist(npop[[1]],npop[[pp]])
      else
        dpop[pp] <- meankdist(npop,pp)  #NSdist(npop[[1]],npop[[pp]])
    } 
    
    #Get distance to NSseed and store in dhits
    dhits[pp,] <- 0
    for(pp in 1:popsize){
      dhits[pp,nn] <- NSdist(NSseed,npop[[pp]])
    }
    
    nextgen <- list()
    
    if(rwalk){
      for(pp in 1:popsize){
        nextgen[[pp]] <- mutate.gene(gpop[[pp]])
      }
    }
    else{
      rank <- order(dpop,decreasing = T)
      
      for(ee in 1:elite){
        nextgen[[ee]] <- gpop[[rank[ee]]]
      }
      
      #we only need to keep an archive of the NS values - not the genome... - although we may need to keep track of the genomes in future
      if(lambda>0){
        for(aa in 1:lambda){
          archive[[alen]] <- npop[[rank[aa]]]
          alen <- alen + 1
        }
      }
      
      for(pp in (elite+1):popsize){
        tourney <- sample(popsize,2)
        winner <- tourney[1]
        if(dpop[tourney[1]] < dpop[tourney[2]] )
          winner <- tourney[2]
        
        nextgen[[pp]] <- mutate.gene(gpop[[winner]])
      } 
    }
    
    gpop <- nextgen
    
    fn <- sprintf("%s_gen%03d.stl",fnroot,nn)  
    sink(file = fn)
    cat("solid Exported from Blender-2.80 (sub 75)\n")
    xpos <- -poslim
    ypos <- -poslim
    for(pp in 1:popsize){
      ppop[[pp]] <- genetostlfile4   ("tmp.stl",gpop[[pp]],pos=spos(0, 0,0),offset=c(xpos,ypos,0),runlim=100,solo=F)
      xpos<-xpos + 50
      if(xpos == poslim){
        xpos <- -poslim
        ypos <- ypos + 50 
      }
    }
    cat("solid Exported from Blender-2.80 (sub 75)\n")
    sink()
    
    #record the genomes - everything else can be generated from these!
    if(save){
      save(gpop,file=sprintf("%s_gpop%d.Rdata",fnroot,nn))
    }
    
  }
  
  return(dhits)

}