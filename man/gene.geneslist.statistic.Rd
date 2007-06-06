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
gene.geneslist.statistic(gene, geneslist, paperLens = paperLen())
}
\arguments{
  \item{gene}{The Entrez Gene ID for the gene of interest. }
  \item{geneslist}{A vector of Entrez Gene ID for the set of genes. }
  \item{paperLens}{A vector containing the number of genes cited by each 
   paper.}
}
\details{
 
}
\value{
To be filled in later.
}
\reference{ Testing Gene Associations Using Co-citation, by B. Ding and
R. Gentleman. Bioconductor Technical Report, 2004} 

\author{R. Gentleman }

\seealso{ \code{\link{twowayTable}}, \code{link{gene.gene.statistic}}}
\examples{
  g1 = "101"
  gl = c("10014", "10015", "10016", "10017", "10018")
  pL = paperLen()
  s1 = gene.geneslist.statistic(g1, gl, pL)
  s1
}
\keyword{ manip }
