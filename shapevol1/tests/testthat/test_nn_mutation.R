context("Mutation of neural networks")


#Define a simple nn for testing mutations:  
inodes <- c("x","y")
onodes <- c("i")
set.seed(345)
testnn <- init.neat(inodes)



test_that("weight mutations work correctly",{
  
  mutant <- mut.net(testnn,noderate = 0,mp = 1,midx = 2)
  
  expect_equal(mutant$conns$w[2],0.2247640, tolerance=1e-3)
  expect_equal(mutant$conns$w[1],testnn$conns$w[1])
  expect_equal(mutant$conns$w[3],testnn$conns$w[3])
})




test_that("node mutations work correctly",{
  
  mutant <- mut.net(testnn,noderate = 0)
  
  expect_equal(nrow(mutant$nodes),3)
  
  
})