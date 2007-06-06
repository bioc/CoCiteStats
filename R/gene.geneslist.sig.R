gene.geneslist.sig <-
function(gene, geneslist, paperLens = paperLen(), n.resamp=100)
 {
   numPapers = length(paperLens$counts)

   n <- length(geneslist)
   gene.geneslist.stat <- gene.geneslist.statistic(gene, geneslist, 
                                paperLens)
   gene.geneslist.stat <- sapply(gene.geneslist.stat,function(x) x)
    
   gene.geneslist.stat.null <- list()
   length(gene.geneslist.stat.null) <- n.resamp
    
   gN = ls(humanLLMappingsLL2PMID)
   for (i in 1:n.resamp) {
      geneslist.null <- sample(gN, n, replace=FALSE)
      gene.geneslist.stat.null[[i]] <- gene.geneslist.statistic(gene,
                geneslist.null, paperLens)
   }
    
   temp <- lapply(gene.geneslist.stat.null, function(x) 
                  gene.geneslist.stat < sapply(x, function(y) y))
    
   temp1 <- temp[[1]]
    
   for ( i in 2:length(temp)) 
       temp1 <- temp1 + temp[[i]]
    
   return(list(statistic=t(gene.geneslist.stat), 
            pval=apply(temp1,1, function(x) x/n.resamp)))
}

