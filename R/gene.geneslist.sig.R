"gene.geneslist.sig" <-
function(gene, geneslist, numPapers, PaperLen, n.resamp=100)
 {
   require("humanLLMappings")
   n <- length(geneslist)
    gene.geneslist.stat <- gene.geneslist.statistic(gene, geneslist, 
                                numPapers, PaperLen)
    gene.geneslist.stat <- sapply(gene.geneslist.stat,function(x) x)
    
    gene.geneslist.stat.null <- list()
    length(gene.geneslist.stat.null) <- n.resamp
    
    for (i in 1:n.resamp)
    {
       print(i)
       geneslist.null <- sample(ls(humanLLMappingsLL2PMID),n, replace=FALSE)
       gene.geneslist.stat.null[[i]] <- gene.geneslist.statistic(gene,
                geneslist.null, numPapers, PaperLen)
    }
    
    temp <- lapply(gene.geneslist.stat.null, function(x) 
                  gene.geneslist.stat < sapply(x, function(y) y))
    
    temp1 <- temp[[1]]
    
    for ( i in 2:length(temp))
    {
       temp1 <- temp1 + temp[[i]]
    }   
    
    return(list(statistic=t(gene.geneslist.stat), pval=apply(temp1,1, function(x) x/n.resamp)))
  }

