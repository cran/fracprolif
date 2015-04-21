\name{ca1d.erlotinib}
\alias{ca1d.erlotinib}
\title{CA1d cells in 16 micromolar erlotinib}

\description{
CA1d cells were plated on a 96-well plate and imaged on a BD Pathway 855 every 10 minutes for 92 hours on 11/22/2010. The image stack was analyzed manually and observed cell lifespans were recorded. Each cell lifespan is defined by a start event (mitosis) and an end event (mitosis, death or end of experiment). Start events are shared among siblings (sister cells) and assigned an identifier comprised of the frame number and a letter. End events are assigned an identifier of frame number and a letter if mitotic. In some cases a mitotic end event identifier corresponds to another start event identifier (i.e. cell progeny). Events are converted to time units based on the frame acquisition rates (sampling interval); cell birth time and lifespan are expressed in hours.

\enumerate{
      
    \item When a mitotic event occurs, the daughter cells are each assigned the next rows available in a \code{\link[base:data.frame]{data.frame}}.
    \item The frame number of the event is recorded in the \code{Birth.frame} column of the rows assigned in (1).
    \item The event from (1) is given an alphabetic identifier not previously used within the same \code{Birth.frame}.
    \item The last frame a cell is observed in is recorded in the \code{End.frame}. 
    \item If the last frame is a mitotic event, assign letter as (3) in \code{End.ID}.
    \item If the cell is present in the last frame of the image stack, the column \code{End.of.Expt} is marked \code{TRUE}, otherwise it is marked \code{FALSE}.
    \item If a cell is observed to die, this is recorded in the column \code{Death} as a \code{TRUE}, otherwise as \code{FALSE}.
    \item Create \code{Birthtime} column in hours by multiplying \code{Birth.frame} by 1/5.
    \item Create \code{Lifespan} column in hours by multiplying \code{(End.frame - Birth.frame)} by 1/5.
}

Information about the cell line used (CA1d) can be found in the following reference: \cite{Fred R. Miller, Herbert D. Soule, Larty Tait, Robert J. Pauley, Sandra R. Wolman, Peter J. Dawson, and Gloria H. Heppner. Xenograft Model of Progressive Human Proliferative Breast Disease.
JNCI J Natl Cancer Inst (1993) 85(21): 1725-1732 doi:10.1093/jnci/85.21.1725}

}

\usage{
  data(ca1d.erlotinib)
}
\value{
   A data frame of lifespan CA1d observations in the presence of 16 micromolar erlotinib.
}
\author{ Darren Tyson, Shawn Garbett }

\examples{
  data(ca1d.erlotinib)
}




