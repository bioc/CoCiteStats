"paperLen" <-
function (X) 
{
    require("humanLLMappings") || stop("can't match without data")
    papersByLL = mget(X, humanLLMappingsLL2PMID, ifnotfound = NA)
    papers = unique(unlist(papersByLL))
    inap = is.na(papers)
    if (any(inap)) 
        papers = papers[!inap]
    if (length(papers) == 0) 
        return(list(Counts = numeric(0), papers = numeric(0)))
    paperForLL = mget(papers, humanLLMappingsPMID2LL)
    paperCts = sapply(paperForLL, length)
    return(list(Counts = paperCts, papers = papersByLL))
}
