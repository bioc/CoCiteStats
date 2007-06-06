\name{gene.geneslist.sig}
\alias{gene.geneslist.sig}
\title{Compute 3 two-way table statistics and p-values, 
with 4 different adjustments, for PubMed co-citation}
\description{
This function calculates Concordance, Jaccard's index and 
 Hubert's $\Gamma$ with 
no adjustment, adjusting for paper size (PS), adjusting for 
gene size (GS) and both, to evaluate the significance of 
co-citation of a gene of interest and a gene list
}
\usage{
gene.geneslist.sig(gene, geneslist, paperLens = paperLen(), n.resamp=100)
}

\arguments{
  \item{gene}{ The Entrez Gene ID for the gene of interest. }
  \item{geneslist}{The list of Entrez Gene IDs for genes with which the 
   co-citation of the gene of interest is to be evaluated.}
  \item{paperLens}{The sizes of the PubMed papers for consideration. }
  \item{n.resamp}{Number of resampling for generating empirical p-values.}
}
\details{

}
\value{
  Statistics and resampling p-values for all 3 two-way tables along with the 
  4 adjustments for \code{gene} and \code{geneslist} based on 
  \code{n.resamp} resamplings.
}
\references{Testing Gene Associations Using Co-citation, by B. Ding and
R. Gentleman. Bioconductor Technical Report, 2004}
\author{Beiying Ding}

\seealso{\code{\link{actorAdjTable}},\code{\link{paperLen}}, 
   \code{\link{twTStats}}, \code{\link{twowayTable}} }

\examples{
 
gene <- "705"
geneslist <- "7216"

gene.geneslist.sig(gene, geneslist, n.resamp=50)

}
\keyword{manip}

