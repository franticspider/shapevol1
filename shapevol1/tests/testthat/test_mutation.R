context("Mutation of gene sets")

#Define a geneset for testing mutations:  
gene01 <- sgene("Cross section","Square",status = T,start=-40,stop=40,dom=1)
gene02 <- sgene("Length",       5       ,status = T,start=-40,stop=40,dom=1)
gene03 <- sgene("Diameter",     1       ,status = T,start=-40,stop=40,dom=0)
group1 <- rbind(gene01,gene02,gene03)

gene04 <- sgene("X_1X",          0,status = T,start=0,stop=1,dom=50)
gene05 <- sgene("Y_1Y",          0,status = T,start=0,stop=1,dom=50)
gene06 <- sgene("Z_1Z",          1,status = T,start=0,stop=5,dom=50)
group2 <- rbind(gene04,gene05,gene06)

#Table top:
gene07 <- sgene("T1_2X",          1,status = T,start=0,stop=5,dom=48)
gene08 <- sgene("T2_2Y",          1,status = T,start=0,stop=10,dom=48)
gene09 <- sgene("T1_2Z",          0,status = T,start=0,stop=15,dom=48)

#y axis leg and end strut 
gene10 <- sgene("T3_2X",          1,status = T,start=0,stop=1,dom=47)
gene11 <- sgene("T3_2Y",          0,status = T,start=15,stop=16,dom=47)
gene12 <- sgene("T3_2Z",          -1,status = T,start=6,stop=10,dom=47)

#x axis leg and end strut
gene13 <- sgene("T4_2X",          0,status = T,start=10,stop=11,dom=46)
gene14 <- sgene("T4_2Y",          1,status = T,start=0,stop=4,dom=46)
gene15 <- sgene("T4_2Z",          -1,status = T,start=6,stop=10,dom=46)

# Table edge along far y axis
gene16 <- sgene("T5_2X",          0,status = T,start=10,stop=11,dom=45)
gene17 <- sgene("T5_2Y",          1,status = T,start=4,stop=14,dom=45)
gene18 <- sgene("T5_2Z",          0,status = T,start=6,stop=10,dom=45)

# Table edge along far x axis
gene19 <- sgene("T6_2X",          1,status = T,start=5,stop=9,dom=44)
gene20 <- sgene("T6_2Y",          0,status = T,start=11,stop=15,dom=44)
gene21 <- sgene("T6_2Z",          0,status = T,start=6,stop=10,dom=44)

# Lower portions of the remaining three legs
gene22 <- sgene("L3_2X",          0,status = T,start= 0,stop= 30,dom=43)
gene23 <- sgene("L3_2Y",          0,status = T,start=0,stop=15,dom=43)
gene24 <- sgene("L3_2Z",          -1,status = T,start= 1,stop=12,dom=43)

group3 <- rbind(gene07,gene08,gene09
                ,gene10,gene11,gene12
                ,gene13,gene14,gene15
                ,gene16,gene17,gene18
                ,gene19,gene20,gene21
                ,gene22,gene23,gene24
)

geneset <- rbind(group1,group2,group3)



test_that("mutations occur within the correct geneset rows",{
  #test that either end of the mutatable genes is accessible:
  expect_equal(getrowno(1,1),4)
  expect_equal(getrowno(1,3),6)
  
  expect_equal(getrowno(8,1),25)
  expect_equal(getrowno(8,3),27)
  
})




test_that("specified mutations occur at the correct place",{
  
  skip("New structure has beein implemented")
  #valtyp mutation
  mutant <- domut(8,3,1,geneset)
  expect_equal(mutant$valtyp[27],0)
  
})