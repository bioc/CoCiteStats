\name{gene.geneslist.statistic}
\alias{gene.geneslist.statistic}
\title{A function to compute association measures between a gene of interest
and a list of genes. }
\description{
 Whether or not a gene has an association with another gene, or a set of 
genes is measured using co-citation in PubMed as a basis for measuring
that association.
}
\usage{
gene.geneslist.statistic(gene, geneslist, numPapers, PaperLen)
}
\arguments{
  \item{gene}{The LocusLink identifier for the gene of interest. }
  \item{geneslist}{A vector of LocusLink identifiers for the set of genes. }
  \item{numPapers}{The number of papers.}
  \item{PaperLen}{A vector of length \code{numPapers} containing the 
    number of citations for each paper. }
}
\details{
To be filled in later.
}
\value{
To be filled in later.
}
\references{ The Springer Book. }
\author{B. Ding }

\seealso{ \code{\link{twoWayTable}}}
\examples{
}
\keyword{ manip }
