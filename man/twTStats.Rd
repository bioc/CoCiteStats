\name{twTStats}
\alias{twTStats}
\title{Computes 3 two way table statistics. }
\description{
  For two way tables based on co-citations we use three different test
  statistic, the Concordance, the Jaccard index and Hubert's Gamma.
}
\usage{
twTStats(twT)
}

\arguments{
  \item{twT}{A two way table, as produced by \code{twowayTable}. }
}
\details{
  The Conordance is simply the n11 entry. The Jaccard index is the n11
  entry divided by the sum of n11, n12, and n21. Hubert's Gamma is
  slightly more complicated.
}
\value{
  \item{Concordance }{The concordance statistic.}
  \item{Jaccard}{The Jaccard index.}
  \item{Hubert}{Hubert's Gamma}
}
\references{Testing Gene Associations Using Co-citation, by B. Ding and
R. Gentleman. Bioconductor Technical Report, 2004}
\author{R. Gentleman}

\seealso{\code{\link{paperLen}}, \code{\link{twowayTable}} }
\examples{
  tw1 = twowayTable("10", "101", FALSE)
  twTStats(tw1)

}

\keyword{manip }

