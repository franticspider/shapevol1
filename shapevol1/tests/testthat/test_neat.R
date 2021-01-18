
context("Neuro Evolution functions")


tconns <- data.frame(nin = c("x","y","1","1","x","2"),nout = c("1","1","2","i","2","i"),w=c(0.45,0.35,0.1,0.9,0.2,0.4),stringsAsFactors = F)
tnodes <- data.frame(node = c("1","2","i"), parsed = c(T,T,T), layer= c(1,2,3), rank= c(1,1,1),stringsAsFactors = F)
inputnames <- c("x","y")


test_that("nn parses input values correctly",{
  xval <- 3
  yval <- 4
  
  proc <- tnodes
  
  proc$ival <- 0
  proc$oval <- 0
  
  # Node 1 has both the input values only
  proc$ival[1] <- (xval * tconns$w[1]) + (yval *tconns$w[2])
  proc$oval[1] <- act.fct(proc$ival[1])
  
  #Node 2 is a mix of input and hidden outputs
  proc$ival[2] <- (proc$oval[1] * tconns$w[3]) + (xval * tconns$w[5]) 
  proc$oval[2] <- act.fct(proc$ival[2])
  
  #Node 3 is the output node
  proc$ival[3] <- (proc$oval[1] * tconns$w[4]) + (proc$oval[2] * tconns$w[6]) 
  proc$oval[3] <- act.fct(proc$ival[3])
  
  
  
  tnp <- proc.neat(list(conns=tconns,nodes=tnodes,inodes=c("x","y")),c(xval,yval))
  
  #node inputs
  expect_equal(proc$ival[1],tnp$ival[1])
  expect_equal(proc$ival[2],tnp$ival[2])
  expect_equal(proc$ival[3],tnp$ival[3])
  
  #intermediate results: 
  expect_equal(proc$oval[1],tnp$oval[1])
  expect_equal(proc$oval[2],tnp$oval[2])  
  
  #final result: 
  expect_equal(proc$oval[3],tnp$oval[3])
  
})


test_that("nn parses hidden layers correctly",{
  
  
  
})




test_that("hidden nodes can be set using getHnodes", {
  hn <- getHnodes(tconns,inputnames)
  
  expect_equal(hn$node[1],"1")
  expect_equal(hn$node[2],"2")
  expect_equal(hn$node[3],"i")
  
})


test_that("act.fct works ok",{
  
  r1 <- act.fct(0.9)
  
  expect_equal(r1,0.7109495)
  expect_equal(act.fct(-100),3.720076e-44,tolerance = 0.00002)
  expect_equal(act.fct(-10) ,4.539787e-05,tolerance = 0.00002)
  expect_equal(act.fct(-1)  ,0.2689414,tolerance = 0.00002)
  expect_equal(act.fct(-0.5),0.3775407,tolerance = 0.00002)
  expect_equal(act.fct(0)   ,0.5,tolerance = 0.00002)
  expect_equal(act.fct(0.5) ,0.6224593,tolerance = 0.00002)
  expect_equal(act.fct(1)   ,0.7310586,tolerance = 0.00002)
  expect_equal(act.fct(10)  ,0.9999546,tolerance = 0.00002)
  expect_equal(act.fct(100) ,1,tolerance = 0.00002)
  
})
