paperLen <- function (x) 
{
    if(missing(x)) 
       x = Lkeys(org.Hs.egPMID) 

    papersByLL = mget(x, org.Hs.egPMID, ifnotfound = NA)
    papers = unique(unlist(papersByLL))
    inap = is.na(papers) | papers == "NA"
    if (any(inap)) 
        papers = papers[!inap]
    if (length(papers) == 0) 
        return(list(counts = numeric(0), papers = numeric(0)))
    paperForLL = mget(papers, revmap(org.Hs.egPMID))
    paperCts = sapply(paperForLL, length)
    return(list(counts = paperCts, papers = papersByLL))
}
