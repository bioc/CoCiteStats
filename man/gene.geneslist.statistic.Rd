\name{gene.geneslist.statistic}
\alias{gene.geneslist.statistic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gene.geneslist.statistic(gene, geneslist, numPapers, PaperLen)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gene}{ ~~Describe \code{gene} here~~ }
  \item{geneslist}{ ~~Describe \code{geneslist} here~~ }
  \item{numPapers}{ ~~Describe \code{numPapers} here~~ }
  \item{PaperLen}{ ~~Describe \code{PaperLen} here~~ }
}
\details{
  ~~ If necessary, more details than the __description__  above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(gene, geneslist, numPapers, PaperLen)
{
   gene.geneslist.stat <- list()
   n <- length(geneslist)
   length(gene.geneslist.stat) <- n
   names(gene.geneslist.stat) <- geneslist
   
   for ( i in geneslist)
    {
      gene.geneslist.stat[[i]] <- gene.gene.statistic(gene, i,numPapers,PaperLen)
     }
    
    temp <- NULL
    length(temp) <- 4
    for ( i in names(gene.geneslist.stat[[1]]))
     temp[[i]] <- apply(sapply(gene.geneslist.stat, function(x) x[[i]]),1,sum)
    return(temp) 
 }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
