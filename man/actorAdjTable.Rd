\name{actorAdjTable}
\alias{actorAdjTable}
\title{Compute actor size adjustment on a two way table}
\description{
  When two objects are related through a bipartite graph it is
  sometimes appropriate to carry out special adjustments. One of the
  adjustments is called actor size adjustment. In this case the counts
  are adjusted according to how often the objects are referenced.
}
\usage{
actorAdjTable(twT, eps = 1e-08)
}

\arguments{
  \item{twT}{ A two way table as produced by \code{\link{twowayTable}}. }
  \item{eps}{A small quantity used to assess approximate equality.}
}
\details{
 Need more.
}
\value{
  An adjusted two way table, with elements named \code{u11},
  \code{u12}, \code{u21} and \code{u22}.
}
\references{Testing Gene Associations Using Co-citation, by B. Ding and
R. Gentleman. Bioconductor Technical Report, 2004}
\author{R. Gentleman}

\seealso{\code{\link{paperLen}}, \code{\link{twowayTable}} }

\examples{
  tw1 = twowayTable("10", "100", FALSE)
  actorAdjTable(tw1)

}
\keyword{manip}

