twowayTable <- function (g1, g2, weights = TRUE, numPapers, PaperLen) 
{
    if (missing(numPapers)) 
        numPapers = length(unique(unlist(eapply(
              humanLLMappingsLL2PMID,function(x) x))))
    if (weights==TRUE & missing(PaperLen))
      {
         PaperLen = paperLen(ls(humanLLMappingsLL2PMID))$Counts
      }   
    
    wh = paperLen(c(g1, g2))
    g1pp = wh$papers[[g1]]
    g2pp = wh$papers[[g2]]
    ##FIXME: defensive programming as string NA's seem to appear at times
    ina = is.na(g1pp) | g1pp == "NA"
    g1pp = g1pp[!ina]
    ina = is.na(g2pp) | g2pp == "NA"
    g2pp = g2pp[!is.na(g2pp)]
    matches = intersect(g1pp, g2pp)
    unions = union(g1pp, g2pp)
    just1 = setdiff(g1pp, g2pp)
    just2 = setdiff(g2pp, g1pp)
    if (weights) {
        n11 = sum(1/wh$Counts[matches])
        n12 = sum(1/wh$Counts[just1])
        n21 = sum(1/wh$Counts[just2])
        n22 = sum(1/PaperLen[!(names(PaperLen) %in% unions)])
       }
    else {
        n11 = length(matches)
        n12 = length(just1)
        n21 = length(just2)
        n22 = numPapers - n11 - n12 - n21
    }
    c(n11 = n11, n12 = n12, n21 = n21, n22 = n22)
}

