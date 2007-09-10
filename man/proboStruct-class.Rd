\name{proboStruct-class}
\docType{class}
\alias{proboStruct-class}
\alias{lines,proboStruct-method}
\alias{plot,proboStruct-method}
\alias{show,proboStruct-method}

\title{Class "proboStruct" }
\description{	 structure for managing proboscis plot data }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("proboStruct", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
    \item{\code{call}:}{Object of class \code{"call"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
Class \code{"\linkS4class{AssayData}"}, by class "list", distance 2.
}
\section{Methods}{
plot
}
\references{ 
For Figure 2 of Shippy et al., Using RNA sample titrations... (Nat Biotech,
24(9):1123-1131, Sep 2006)
}
\author{ V Carey <stvjc@channing.harvard.edu> }
\note{ The proboscis plot shows how the 
probability of self-consistent monotone titration  (SCMT)
varies with the spiked difference in concentrations of
two mRNA preparations in an MAQC dataset.}
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%	or \code{\linkS4class{CLASSNAME}} for links to other classes
%}
\examples{
data(afxsubRMAES)
NN1 = proboscis(afxsubRMAES)
plot(NN1)
showClass("proboStruct")
}
\keyword{classes}
