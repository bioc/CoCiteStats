"gene.gene.statistic" <-
function(g1, g2, numPapers, PaperLen)
{
   ## no adjustment
   twT <- twowayTable(g1, g2, weights=FALSE, numPapers)
   original <- twTStats(twT)
   
   ## Gene size adjusted
   twT <- actorAdjTable(twT)
   gs <- twTStats(twT)
   
   ## Paper size adjusted
   twT <- twowayTable(g1, g2, weights=TRUE, numPapers, PaperLen)
   ps <- twTStats(twT)
   
   ## both
   twT <- actorAdjTable(twT)
   both <- twTStats(twT)
 
   return(list(original=original, gs=gs, ps=ps, both=both))
 }

