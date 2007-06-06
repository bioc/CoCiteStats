\name{twowayTable}
\alias{twowayTable}

\title{ Compute a two way co-citation table for 2 genes. }
\description{
  This function computes a two way table for comparing co-citation, in
  PubMed for the two input genes. The values in the table can be
  adjusted according to either the paper size or the gene size.
}
\usage{
twowayTable(g1, g2, weights = TRUE, paperLens=paperLen())
}

\arguments{
  \item{g1}{The EntrezGene identifier for gene 1.}
  \item{g2}{The EntrezGene identifier for gene 2.}
  \item{weights}{\code{TRUE} or \code{FALSE} indicating whether paper
  size weights should be used. }
  \item{paperLens}{A vector containing the 
   number of genes each paper refers to, or cites.}
}
\details{
  To determine the association between two genes one can use
  co-citation in the medical literature. When \code{weights} is 
  \code{FALSE} this function computes the
  number of papers that cite only gene 1, only gene 2, both and
  neither. 

  By default, we use the humanLLMappings package to define the set of
  papers that are used in the computations.  For other organisms, or for more
  restricted sets of papers the user will need to supply the
  vector \code{paperLens} explicitly.

  One can consider papers which cite many genes to be less informative than
  those that cite only a few genes.  If \code{weights} is \code{TRUE}
  (the default) then papers are weighted by the inverse of the number 
  of citations.
}
\value{
  A vector of length four, with entries \code{n11}, \code{n12},
  \code{n21} and \code{n22}. These correspond to the number of papers
  that cite both genes, the number that cite only gene 1, the number
  that cite only gene 2, and the total number of papers minus those
  counted in \code{n11}, \code{n21}, \code{n12}, or in the default case
  the weighted versions of these quantities.
}

\author{R. Gentleman}
\seealso{\code{\link{paperLen}}, \code{\link{twTStats}} }
\examples{

  pL = paperLen()
  twowayTable("10", "100", paperLens=pL)
  twowayTable("10", "100", FALSE, paperLens=pL)
}
\keyword{manip}

