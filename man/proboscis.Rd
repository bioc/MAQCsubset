\name{proboscis}
\alias{proboscis}
\title{Produce a plot similar to Figure 2 of the Shippy MAQC paper (PMID 16964226).  }
\description{  Produce a plot similar to Figure 2 of the Shippy MAQC paper (PMID 16964226).
}
\usage{
proboscis(es, site=1, ABp=0.001, CDp=0.01, mmrad=100)
}
\arguments{
  \item{es}{ \code{\link[Biobase:ExpressionSet-class]{ExpressionSet}} instance with MAQC assay results}
  \item{site}{ numeric code -- site to be assessed}
  \item{ABp}{ ABp -- p-value threshold to declare concentration of gene in
sample A to be different from ehe concentration in sample B}
  \item{CDp}{ CDp -- p-value threshold to declare concentration of gene in
sample C to be different from the concentration in sample D}
  \item{mmrad}{ numeric radius of the moving mean used to smooth the proportions
differentially expressed }
}
\details{
Figure 2 of the Shippy paper consists of a collection of plots of
estimated probabilities of self-consistent monotone titration -- briefly,
samples are such that A has 100\% USRNA, B has 100\% Ambion brain,
C has 75\% USRNA+25\% brain, D has 25\% USRNA, 75\% brain.  Self-consistent
monotone titration holds for gene g if microarray measures for that gene
satisfy A > C > D > B or B > C > D > A.  The estimated probability functions
look like a creature sticking its nose over a wall, thus the name of this function.
}
\value{
an instance of \code{\linkS4class{proboStruct}}, for which a plot and lines method are available.
}
\references{PMID 16964226  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
data(afxsubRMAES)
NN2 = proboscis(afxsubRMAES, site=2)
plot(NN2)
}
\keyword{ models }
