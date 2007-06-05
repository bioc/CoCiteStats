"gene.gene.statistic" <-
function(g1, g2, paperLens)
{
   numPapers = length(paperLen)
   ## no adjustment
   twT <- twowayTable(g1, g2, weights=FALSE, numPapers)
   original <- twTStats(twT)
   
   ## Gene size adjusted
   twT <- actorAdjTable(twT)
   gs <- twTStats(twT)
   
   ## Paper size adjusted
   twT <- twowayTable(g1, g2, weights=TRUE, paperLens)
   ps <- twTStats(twT)
   
   ## both
   twT <- actorAdjTable(twT)
   both <- twTStats(twT)
 
   return(list(original=original, gs=gs, ps=ps, both=both))
 }

