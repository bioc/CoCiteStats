"gene.geneslist.statistic" <-
function(gene, geneslist, paperLens = paperLen())
{
   ans = lapply(geneslist, function(x) gene.gene.statistic(gene, x, paperLens)) 
    
   lapply(names(ans[[1]]), function(y) 
            apply(sapply(ans, function(x) x[[y]]),1,sum))
}

