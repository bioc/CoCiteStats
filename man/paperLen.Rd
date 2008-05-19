\name{paperLen}
\alias{paperLen}
\title{Find the number of papers cited}
\description{
  The set of papers that cite the input Entrez Gene identifiers are
  found, and for each of these the number of genes cited in that
  paper is computed and returned.
}
\usage{
paperLen(x)
}

\arguments{
  \item{x}{A vector of Entrez Gene identifiers. }
}
\details{
 This function first finds the set of unique PMIDs associated with the
 input set of Entrez Gene IDS. Then for each PMID it finds the number of
 Entrez Gene identifiers associated with that paper. The function uses
 different sets of variable mappings from the \pkg{org.Hs.eg.db}
 package.

 If \code{x} is missing then all Entrez gene identifiers in the
 org.Hs.egPMID are used.

}
\value{
  \item{counts }{For each paper the number of Entrez Gene identifiers
  referred to.}
  \item{papers }{A list of the same length as \code{x}, each element
  contains the papers that refer to the corresponding Entrez Gene identifier.}
}
\author{R. Gentleman}

\seealso{\code{\link{twTStats}}}
\examples{
  ans = paperLen(c("10", "1001"))
  ans$counts
  ans$papers
}
\keyword{manip }

