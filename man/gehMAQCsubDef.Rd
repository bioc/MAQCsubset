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
"GSM123122_GEH_1_A1.TXT" "GSM123123_GEH_1_A2.TXT" "GSM123127_GEH_1_B1.TXT"
"GSM123128_GEH_1_B2.TXT" "GSM123132_GEH_1_C1.TXT" "GSM123133_GEH_1_C2.TXT"
"GSM123137_GEH_1_D1.TXT" "GSM123138_GEH_1_D2.TXT"

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
