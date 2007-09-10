\name{gehMAQCsubDef}
\docType{data}
\alias{gehMAQCsubDef}
\alias{gehSubRaw}
\title{Excerpt from GE Codelink array contributions to MAQC}
\description{ Excerpt from GE Codelink contributions to MAQC}
\usage{
data(gehSubRaw)
data(gehMAQCsubDef)
}
\details{
\code{gehSubRaw} is a \code{codelink::Codelink} instance based on reading
the raw GEO files:
"GSM123122\_GEH\_1\_A1.TXT" "GSM123123\_GEH\_1\_A2.TXT" "GSM123127\_GEH\_1\_B1.TXT"
"GSM123128\_GEH\_1\_B2.TXT" "GSM123132\_GEH\_1\_C1.TXT" "GSM123133\_GEH\_1\_C2.TXT"
"GSM123137\_GEH\_1\_D1.TXT" "GSM123138\_GEH\_1\_D2.TXT"

\code{gehMAQCsubDef} is an \code{ExpressionSet} instance based on
default background correction and normalization of the \code{codelink}
package.  The original feature names include duplicates; these
were made unique by \code{\link{make.names}} with \code{unique=TRUE}.
}
%\value{
%}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
data(gehMAQCsubDef)
gehMAQCsubDef
}
\keyword{ models }
