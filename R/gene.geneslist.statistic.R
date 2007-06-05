"gene.geneslist.statistic" <-
function(gene, geneslist, paperLens)
{
   numPapers = length(paperLens)
   n <- length(geneslist)
   gene.geneslist.stat <- vector("list", length=n)
   names(gene.geneslist.stat) <- geneslist
   
   for ( i in geneslist)
    {
      gene.geneslist.stat[[i]] <- gene.gene.statistic(gene, i,
             paperLens)
     }
    
    temp <- NULL
    for ( i in names(gene.geneslist.stat[[1]]))
     temp[[i]] <- apply(sapply(gene.geneslist.stat, function(x) x[[i]]),1,sum)
    return(temp) 
 }

