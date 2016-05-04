sg.int<-function(g,...,lower,upper)
  
{ require("SparseGrid")

 lower<-floor(lower) ##this ensures that the lower bound values are all rounded down to whole numbers
 
 upper<-ceiling(upper) ##this ensures that the upper bound values are all rounded up to whole numbers

 if (any(lower>upper)) stop("lower must be smaller than upper")
 ##this if statement ensures that no one lower value can be greater than any one upper value

 gridss<-as.matrix(expand.grid(seq(lower[1],upper[1]-1,by=1),seq(lower[2],upper[2]-1,by=1)))
 
 sp.grid <- createIntegrationGrid('KPU', dimension=2,k=5)

 nodes<-gridss[1,]+sp.grid$nodes

 weights<-sp.grid$weights

 for (i in 2:nrow(gridss))

 {
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes)  

    weights<-c(weights,sp.grid$weights)

  }

  gx.sp <- apply(nodes, 1, g,...)
  val.sp <- gx.sp %*%weights
  val.sp
}

##Here I am installing the SparseGrid package to make sure I can work with this library.
install.packages("SparseGrid")
library(SparseGrid)

##My example function
g <-function(x){
  return(4*x + 5)
}

##Attempting the integration itself with my newly defined g
theLower <- c(1,2)
theUpper<- c(10,11)
sg.int(g, lower=theLower, upper=theUpper)

###########Changing the dimensions

##I am now checking the help files for SparseGrid
?SparseGrid

##my attempt at allowing for more dimensions
sg.int.dim<-function(g,dim,lower,upper)
  
{ require("SparseGrid")
  
  lower<-floor(lower) ##this ensures that the lower bound values are all rounded down to whole numbers
  
  upper<-ceiling(upper) ##this ensures that the upper bound values are all rounded up to whole numbers
  
  if (any(lower>upper)) stop("lower must be smaller than upper")
  ##this if statement ensures that no one lower value can be greater than any one upper value
  
  ##this if statement serves as a reminder to the user to input a number of dimensions that
  ##equals the number of elements in the lower and upper bound vectors
  if(dim != c(length(lower)) | dim != c(length(upper))){
    stop("Make sure to check that number of dimensions equals the amount of elements in
         the lower bound and upper bound vectors!")
  }
  
  ##myGrid and gridss helps me to ensure as many possibilities as possible for
  ##changing the dimensions
  myGrid <- function(i){
    seq(lower[i], upper[i]-1, by=1)
  }
  
  gridss<-as.matrix(expand.grid(lapply(1:dim, myGrid)))
  
  ##ensure for the createIntegrationGrid that it can accept any number of dimensions
  sp.grid <- createIntegrationGrid('KPU', dimension=dim,k=5) 
  
  nodes<-gridss[1,]+sp.grid$nodes
  
  weights<-sp.grid$weights
  
  for (i in 2:nrow(gridss))
  {
    nodes<-rbind(nodes,gridss[i,]+sp.grid$nodes)  
    
    weights<-c(weights,sp.grid$weights)
  }
  
  gx.sp <- apply(nodes, 1, g)
  val.sp <- gx.sp %*%weights
  val.sp
}

##This includes 4 elements
lower_example <- c(1:4)
upper_example<- c(4:7)

##This includes 3 elements
myLower <- c(1:3)
myUpper <- c(4:6)

##checking that I can change the dimensions
sg.int.dim(g, dim=3,lower=lower_example, upper=upper_example)
##This proved the if statement work because the dimensions was fewer than both vectors.

sg.int.dim(g, dim=3, lower=myLower, upper=upper_example)
##This proved the same thing as above that even if the dimensions matches the number
##of elements in the lower bound, not matching the number in the upper bound example
##returns the error message.

sg.int.dim(g, dim=3, lower=myLower, upper=myUpper)
##This example outputs an actual answer, showing the number of dimensions can be changed

sg.int.dim(g, dim=4, lower=lower_example, upper=upper_example)


#######Writing tests for testthat
library("testthat")

context("Correct Inputs and Outputs")

test_that("User Input", {
  myfunction <- function(x){
    return(3*x^2) ##This is the test function I am using
  }
  expect_error(sg.int.dim(g=myfunction, dim=2, lower=2, upper=4),
         "Make sure to check that number of dimensions equals the amount of elements in
         the lower bound and upper bound vectors!") ##this verifies the error message the user will recieve if
  ##they make this error
  
  expect_error(sg.int.dim(g=myfunction, dim=1, lower=5, upper=4),
               "lower must be smaller than upper")
  
 })

