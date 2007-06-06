paperLen <- function (x) 
{
    if(missing(x)) 
       x = ls(humanLLMappingsLL2PMID) 

    papersByLL = mget(x, humanLLMappingsLL2PMID, ifnotfound = NA)
    papers = unique(unlist(papersByLL))
    inap = is.na(papers) | papers == "NA"
    if (any(inap)) 
        papers = papers[!inap]
    if (length(papers) == 0) 
        return(list(counts = numeric(0), papers = numeric(0)))
    paperForLL = mget(papers, humanLLMappingsPMID2LL)
    paperCts = sapply(paperForLL, length)
    return(list(counts = paperCts, papers = papersByLL))
}
