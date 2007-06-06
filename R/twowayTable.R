twowayTable <- function (g1, g2, weights = TRUE, paperLens = paperLen()) 
{
    numPapers = length(paperLens$counts)
    
    g1pp = paperLens$papers[[g1, exact=TRUE]]
    g2pp = paperLens$papers[[g2, exact=TRUE]]

    if ( is.null(g1pp) || is.null(g2pp) )  # no papers found
      return(c(n11=0, n12=0, n21=0, n22=numPapers))
    matches = intersect(g1pp, g2pp)
    unions = union(g1pp, g2pp)
    just1 = setdiff(g1pp, g2pp)
    just2 = setdiff(g2pp, g1pp)
    if (weights) {
        n11 = sum(1/paperLens$counts[matches])
        n12 = sum(1/paperLens$counts[just1])
        n21 = sum(1/paperLens$counts[just2])
        n22 = sum(1/paperLens$counts[!(names(paperLens$counts) %in% unions)])
       }
    else {
        n11 = length(matches)
        n12 = length(just1)
        n21 = length(just2)
        n22 = numPapers - n11 - n12 - n21
    }
    c(n11 = n11, n12 = n12, n21 = n21, n22 = n22)
}

