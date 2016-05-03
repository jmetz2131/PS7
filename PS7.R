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

##Attempting the integration itself with my newly defined g
lower_example <- c(1:4)
upper_example<- c(10:13)
sg.int(g, lower=lower_example, upper=upper_example)

######Changing the dimensions
##I am now checking the help files for SparseGrid
?SparseGrid

##my attempt at allowing for more dimensions
createSparseGrid()

