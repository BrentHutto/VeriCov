% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covV.R
\name{covV}
\alias{covV}
\title{Computes sufficiency coverage and consistency parameters 
by Ragin (R), Haesebrouck (H) and Veri (V) methods}
\usage{
covV(expr, out, dat)
}
\arguments{
\item{expr}{Expression in SOP or DNF form, in quotes}

\item{out}{Name of the outcome variable, in quotes}

\item{dat}{The calibrated data}
}
\value{
InclR C. Ragin's inclusion (consistency) metric

InclR C. Ragin's inclusion (consistency) metric

InclR C. Ragin's inclusion (consistency) metric

InclH T. Haesebrouck's inclusion (consistency) metric

InclV F. Veri's inclusion (consistency) metric

CovV F. Veri's summary coverage metric

QualV F. Veri's Qualitative coverage metric (fs calibrated)

QuantV F. Veri's Quantitative coverage metric

typ.N Number of typical cases

typ.X Sum of typical cases membership (x)

dcvN Number of deviant cases coverage

dcv.X Sum of deviant cases coverage membership (x)

iir.N Number of individually irrelevant cases

iir.X Sum of individually irrelevant cases membership (x)

ir.N Number of individually irrelevant cases

ir.X Sum of irrelevant cases membership (x)

dcd.N Number of deviant cases consistency in degree

dcd.X Sum of deviant cases consistency in degree membership (x)

dck.N Number of deviant cases consistency in kind

dck.X Sum of deviant cases consistency in kind membership (x)

dck.XY Sum of deviant cases consistency in kind MIN(x,y) values
}
\description{
Computes sufficiency coverage and consistency parameters 
by Ragin (R), Haesebrouck (H) and Veri (V) methods
}
