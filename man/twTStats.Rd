\name{twTStats}
\alias{twTStats}
\title{Computes 3 two way table statistics. }
\description{
  For two way tables based on co-citations four different test
  statistics are reported, the odds ratio, the Concordance, the Jaccard index 
  and Hubert's Gamma.
}
\usage{
twTStats(twT)
}

\arguments{
  \item{twT}{A two way table, as produced by \code{twowayTable}. }
}
\details{
  The entries in the presumed 2 by 2 table are labeled n11, n12, n21, n22,
  corresponding to the entries in the first row first column, first row 
  second column etc. The odds ratio is the product of n11 and n22 divided 
  by the product of n12 and n21.
  The Conordance is simply the n11 entry. The Jaccard index is the n11
  entry divided by the sum of n11, n12, and n21. Hubert's Gamma is
  slightly more complicated.
}
\value{
  \item{Concordance }{The concordance statistic.}
  \item{Jaccard}{The Jaccard index.}
  \item{Hubert}{Hubert's Gamma}
  \item{OddsRatio}{The odds ratio.}
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

