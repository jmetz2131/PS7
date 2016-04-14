install.packages("SparseGrid")
library(SparseGrid)
install.packages("testthat")
library(testthat)
library(microbenchmark)
install.packages("parallel")
library(parallel)
install.packages("cubature")
library(cubature)
##I'm using these libraries to prepare for actually changing the 
##function to include things parallel processing and to measure gains in speed


##Defining g. I made g a simple linear function of 4x+5 and checked that it works by making x=2, which
##appropriately outputted 13.
g <-function(x){
  return(4*x + 5)
}
g(2)

sg.int<-function(g,...,lower,upper)
  
{ require("SparseGrid")

 lower<-floor(lower) ##vector of the lower bounds and it will needs to accept any number
 #based on the number of dimensionality. You should ensure that length of lower=length of upper
 

 upper<-ceiling(upper)

 if (any(lower>upper)) stop("lower must be smaller than upper")

 gridss<-as.matrix(expand.grid(seq(lower[1],upper[1]-1,by=1),seq(lower[2],upper[2]-1,by=1)))

##From what I can see, I will change the dimensions of the function here by changing the dimension argument to
##equal something other than 2.
 sp.grid <- createIntegrationGrid( 'KPU', dimension=2, k=5 )

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

##I am now checking the help files for SparseGrid
?SparseGrid

#I just figured out that the entirety of the code here relates to the sg.int function itself.
#I also know that I will have to define g outside of the sg.int function to make sg.int actually
#work

##Attempting the integration itself with my newly defined g
sg.int(g, lower=0, upper=4)


