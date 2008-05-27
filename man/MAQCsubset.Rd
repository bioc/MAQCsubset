\name{MAQCsubset}
\docType{data}
\alias{afxsubRMAES}
\alias{afxsubRMA}
\alias{ilmMAQCsubR}
\alias{afxsub}
\alias{MAQCsubset}
\title{Experimental Data Package: MAQCsubset}
\description{selected data from the MAQC project (Nature Biotechnology, Sept. 2006)}
\usage{
data(afxsubRMAES)
data(afxsubRMA)
data(afxsub)
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
and \code{afxsub} is an \code{AffyBatch}.  \code{afxsubRMAES}
is a proper ExpressionSet instance.

ilmMAQCsubR is the result of applying lumiR to the files
in the vicinity of GEO GSM122901 with filename suffixes matching
those of the sampleNames in the set.
}

\examples{
  data(afxsubRMAES)
}
\keyword{datasets}

