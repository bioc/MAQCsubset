\name{gnfCerebHi}
\alias{gnfCerebHi}
\alias{gnfCerebLow}
\docType{data}
\title{Gene lists for hi or low abundance in cerebellum according
to Novartis GNF symatlas}
\description{Data frames with gene lists for hi or low abundance in cerebellum according
to Novartis GNF symatlas}
\usage{
data(gnfCerebHi)
data(gnfCerebLow)
}
\details{
The 
symatlas.gnf.org database was searched using the gcrma version of
human gene atlas for genes having expression in cerebellum 
at least 3 times (or at most 1/3 times) median
expression over all organs surveyed.  The resulting
gene lists were intersected with genes present on GE codelink
(version used in MAQCsubset) and hgu95a.
}
\value{
data.frame instances with columns providing gene name, affy probe set identifier,
codelink probe identifier, illuminaHumanv1 identifier.
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
data(gnfCerebHi)
gnfCerebHi[1:3,]
}
\keyword{ models }
