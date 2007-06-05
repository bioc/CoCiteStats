\name{paperLen}
\alias{paperLen}
\title{Find the number of papers cited}
\description{
  The set of papers that are cite the input Entrez Gene identifiers are
  found, and for each of these the number of genes referred to is found
  and returned.
}
\usage{
paperLen(X)
}

\arguments{
  \item{X}{A vector of Entrez Gene identifiers. }
}
\details{
 This function first finds the set of unique PMIDs associated with the
 input set of Entrez Gene IDS. Then for each PMID it finds the number of
 Entrez Gene identifiers associated with that paper. The function uses
 different sets of variable mappings from the \pkg{humanLLMappings}
 package.
}
\value{
  \item{Counts }{For each paper the number of Entrez Gene identifiers
  referred to.}
  \item{papers }{A list of the same length as \code{X}, each element
  contains the papers that refer to the corresponding Entrez Gene identifier.}
}
\author{R. Gentleman}

\seealso{\code{\link{twTStats}}}
\examples{
  paperLen(c("10", "1001"))
}
\keyword{manip }

