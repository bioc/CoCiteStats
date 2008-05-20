paperLen <- function (x) 
{
    tT = toTable(org.Hs.egPMID)
    papersByLL = split(tT[,2], tT[,1])
    if( !missing(x) ) papersByLL = papersByLL[x]
    if (length(papersByLL) == 0) 
        return(list(counts = numeric(0), papers = numeric(0)))
    paperCts = nhit(revmap(org.Hs.egPMID))
    if( !missing(x) )
	paperCts = paperCts[unique(unlist(papersByLL, use.names=FALSE))]
    return(list(counts = paperCts, papers = papersByLL))
}
