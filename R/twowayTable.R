twowayTable <- function (g1, g2, weights = TRUE, paperLens) 
{
    if (missing(paperLens))  
      paperLens <- unlist(eapply(humanLLMappingsPMID2LL, length))

    numPapers = length(paperLens)
    
    wh = paperLens[c(g1, g2)]
    if (!length(wh$papers))  # no papers found
      return(c(n11=0, n12=0, n21=0, n22=numPapers))
    g1pp = wh$papers[[g1]]
    g2pp = wh$papers[[g2]]
    ina = is.na(g1pp) 
    g1pp = g1pp[!ina]
    ina = is.na(g2pp) 
    g2pp = g2pp[!ina]
    matches = intersect(g1pp, g2pp)
    unions = union(g1pp, g2pp)
    just1 = setdiff(g1pp, g2pp)
    just2 = setdiff(g2pp, g1pp)
    if (weights) {
        n11 = sum(1/wh$Counts[matches])
        n12 = sum(1/wh$Counts[just1])
        n21 = sum(1/wh$Counts[just2])
        n22 = sum(1/paperLens[!(names(paperLens) %in% unions)])
       }
    else {
        n11 = length(matches)
        n12 = length(just1)
        n21 = length(just2)
        n22 = numPapers - n11 - n12 - n21
    }
    c(n11 = n11, n12 = n12, n21 = n21, n22 = n22)
}

