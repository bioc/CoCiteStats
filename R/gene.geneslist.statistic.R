"gene.geneslist.statistic" <-
function(gene, geneslist, numPapers, PaperLen)
{
   gene.geneslist.stat <- list()
   n <- length(geneslist)
   length(gene.geneslist.stat) <- n
   names(gene.geneslist.stat) <- geneslist
   
   for ( i in geneslist)
    {
      gene.geneslist.stat[[i]] <- gene.gene.statistic(gene, i,numPapers,PaperLen)
     }
    
    temp <- NULL
    length(temp) <- 4
    for ( i in names(gene.geneslist.stat[[1]]))
     temp[[i]] <- apply(sapply(gene.geneslist.stat, function(x) x[[i]]),1,sum)
    return(temp) 
 }

