sg.int<-function(g,...,lower,upper)


{ require("SparseGrid")

 lower<-floor(lower)

 upper<-ceiling(upper)

 if (any(lower>upper)) stop("lower must be smaller than upper")

 gridss<-as.matrix(expand.grid(seq(lower[1],upper[1]-1,by=1),seq(lower[2],upper[2]-1,by=1)))

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

#I just figured out that the entirety of the code here relates to the sg.int function itself.
#I also know that I will have to define g outside of the sg.int function to make sg.int actually
#work

##Defining g. I made g a simple linear function of 4x+5 and checked that it works by making x=2, which
##appropriately outputted 13.
g <-function(x){
  return(4*x + 5)
}
g(2)

##Attempting the integration itself with my newly defined g
sg.int(g, lower=0, upper=4)

