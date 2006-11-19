\name{MAQCsubset}
\docType{data}
\alias{afxsubRMAES}
\alias{afxsubRMA}
\alias{afxsub}
\title{Experimental Data Package: MAQCsubset}
\description{selected data from the MAQC project (Nature Biotechnology, Sept. 2006)}
\usage{
data(MAQCsubset)
}
\format{
  The format is:
  An \code{ExpressionSetObject} with covariates:
\itemize{
\item \code{site}: from cel
\item \code{samp}: rna src/mixture code
\item \code{repl}: replicate
}
}

\note{\code{afxsubRMA} is an \code{exprSet} (deprecated)
and \code{afxsub} is an \code{AffyBatch}.
}

\examples{
  data(MAQCsubset)
}
\keyword{datasets}

