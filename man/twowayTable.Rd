\name{twowayTable}
\alias{twowayTable}

\title{ Compute a two way co-citation table for 2 genes. }
\description{
  This function computes a two way table for comparing co-citation, in
  PubMed for the two input genes. The values in the table can be
  adjusted according to either the paper size or the gene size.
}
\usage{
twowayTable(g1, g2, weights = TRUE, numPapers, PaperLen)
}

\arguments{
  \item{g1}{The EntrezGene identifier for gene 1.}
  \item{g2}{The EntrezGene identifier for gene 2.}
  \item{weights}{\code{TRUE} or \code{FALSE} indicating whether paper
  size weights should be used. }
  \item{numPapers}{The total number of papers under consideration.}
  \item{PaperLen}{A vector of length \code{numPapers} containing the 
   number of citations each paper makes.}
}
\details{
  To determine the association between two genes one can use
  co-citation in the medical literature. This function computes the
  number of papers that cite only gene 1, only gene 2, both and
  neither. These are then returned as the entries in a two way table.
}
\value{
  A vector of length four, with entries \code{n11}, \code{n12},
  \code{n21} and \code{n22}. These correspond to the number of papers
  that cite both genes, the number that cite only gene 1, the number
  that cite only gene 2, and the total number of papers minus those
  counted in \code{n11}, \code{n21}, \code{n12}.
}

\author{R. Gentleman}
\seealso{\code{\link{paperLen}}, \code{\link{twTStats}} }
\examples{

  twowayTable("10", "100")
  twowayTable("10", "100", FALSE)

}
\keyword{manip}

