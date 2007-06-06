\name{gene.gene.statistic}
\alias{gene.gene.statistic}
\title{ Compute gene-gene statistics }
\description{
 Computes gene gene statistics.
}
\usage{
gene.gene.statistic(g1, g2, paperLens = paperLen())
}
\arguments{
  \item{g1}{The Entrez Gene identifier for one of the genes. }
  \item{g2}{The Entrez Gene identifier for the other gene. }
  \item{paperLens}{ A vector with the 
   number of citations for each paper.  }
}
\details{
For the two genes identified by their Entrez IDs a number
of two-way table statistics, i.e. those computed via \code{\link{twTStats}},
are returned, as are their gene and paper size adjusted variants.
}
\value{
A list with entries
\item{original} The output of \code{\link{twTStats}} on the observed data.
\item{gs}  The output of \code{\link{twTStats}} on the data scaled for
 gene size.
\item{ps} The output of \code{\link{twTStats}} on the data scaled for
 paper size.
\item{both} The output of \code{\link{twTStats}} on the data scaled for
 both paper and gene size.
}
\reference{ Testing Gene Associations Using Co-citation, by B. Ding and
R. Gentleman. Bioconductor Technical Report, 2004  }
\author{ B. Ding and R. Gentleman}
\seealso{ \code{\link{twowayTable}} }
\examples{

 g1 = "10"  #Entrez ID for gene 1
 g2 = "101"  #Entrez ID for gene 2
 pLens = paperLen()
 gene.gene.statistic(g1, g2, pLens)

}
\keyword{ manip }
