#' Computes sufficiency coverage and consistency parameters 
#' by Ragin (R), Haesebrouck (H) and Veri (V) methods

#' @param  expr Expression in SOP or DNF form, in quotes
#' @param  out  Name of the outcome variable, in quotes
#' @param  dat  The calibrated data

#' @return InclR C. Ragin's inclusion (consistency) metric
#' @return InclR C. Ragin's inclusion (consistency) metric
#' @return InclR C. Ragin's inclusion (consistency) metric
#' @return InclH T. Haesebrouck's inclusion (consistency) metric
#' @return InclV F. Veri's inclusion (consistency) metric
#' @return CovV F. Veri's summary coverage metric
#' @return QualV F. Veri's Qualitative coverage metric (fs calibrated)
#' @return QuantV F. Veri's Quantitative coverage metric
#' @return typ.N Number of typical cases
#' @return typ.X Sum of typical cases membership (x) 
#' @return dcvN Number of deviant cases coverage
#' @return dcv.X Sum of deviant cases coverage membership (x) 
#' @return iir.N Number of individually irrelevant cases
#' @return iir.X Sum of individually irrelevant cases membership (x) 
#' @return ir.N Number of individually irrelevant cases
#' @return ir.X Sum of irrelevant cases membership (x) 
#' @return dcd.N Number of deviant cases consistency in degree
#' @return dcd.X Sum of deviant cases consistency in degree membership (x) 
#' @return dck.N Number of deviant cases consistency in kind
#' @return dck.X Sum of deviant cases consistency in kind membership (x) 
#' @return dck.XY Sum of deviant cases consistency in kind MIN(x,y) values
#` @return ambig.N Number of Veri ambiguous cases 
#` @return ambig.X Sum of Veri ambiguous cases membership (x)
#` @return unamb.N Number of Veri unambiguous cases
#` @return unamb.X Sum of of Veri unambiguous cases membership (x)
#` @return incon.N Number of Veri inconsistent cases 
#` @return incon.X Sum of Veri inconsistent cases membership (x)
#` @return incon.XY Sum of Veri inconsistent cases MIN(x,y) values

#' @export

covV <- function(expr, out, dat) 
  {
  y <- admisc::compute(out,dat)
  x <- admisc::compute(expr,dat)
  
  typ <- (y>0.5 & x>0.5 & y>=x)

  dcv <- (y>0.5 & x<0.5)
  dcd <- (x>0.5 & y>0.5 & y<x)

  iir <- (x<0.5 & y<0.5 & y>=x)
  ir  <- (x<0.5 & y<0.5 & y<x)

  dck <- (x>0.5 & y<0.5)
  
  unamb <- (x<=y & x>=(1-y))
  ambig <- (x<=y & x<(1-y))
  incon <- (y<x)
    
  rwQuant <- sum(typ) / sum(typ|dcv|iir)
  fsQuant <- QCA::calibrate(rwQuant, logistic=TRUE,
                                     thresholds="e=0.1,c=0.5,i=0.9")
                                
  Qual <- ifelse( sum(typ)==0, 0, sum(x[typ])/sum(y[typ]))
  
  covS <- sqrt((Qual+fsQuant)/2)*sqrt(min(Qual, fsQuant))

  InclV <- (sum(pmin(x, y)) - 
            sum(pmax(3*(pmin(x,y,(1-y))-x) + pmin(x,y,(1-y)), 0))) / sum(x)

  InclH <- sum(pmin(x, y)) / sum(pmin(x, y) + sqrt(pmax(x - y, 0)*x))
  
  PRI <- (sum(pmin(x,y))-sum(pmin(x,y,(1-y)))) / (sum(x)-sum(pmin(x,y,(1-y))))
  InclR <- sum(pmin(x, y)) / sum(x)
  CovR <- sum(pmin(y, x)) / sum(y)

  return(as.data.frame(list('condition'=expr,
                            'InclR'=round(InclR, 3),
                            'CovR'=round(CovR, 3),
                            'PRI'=round(PRI, 3),
                            'InclH'=round(InclH, 3),
                            'QuantV'=round(fsQuant, 3),
                            'QualV'=round(Qual, 3),
                            'CovV'=round(covS, 3),
                            'InclV'=round(InclV, 3),
                            'typ'=list('N'=sum(typ), 
                                       'X'=sum(x[typ])),
                            'dcv'=list('N'=sum(dcv), 
                                       'X'=sum(x[dcv])),
                            'iir'=list('N'=sum(iir), 
                                       'X'=sum(x[iir])),
                            'ir'=list('N'=sum(ir), 
                                      'X'=sum(x[ir])),
                            'dcd'=list('N'=sum(dcd), 
                                       'X'=sum(x[dcd])),
                            'dck'=list('N'=sum(dck), 
                                       'X'=sum(x[dck]), 
                                       'XY'=sum(pmin(x[dck],y[dck]))),
                            'ambig'=list('N'=sum(ambig), 
                                         'X'=sum(x[ambig])),
                            'unamb'=list('N'=sum(unamb), 
                                         'X'=sum(x[unamb])),
                            'incon'=list('N'=sum(incon), 
                                         'X'=sum(x[incon]), 
                                         'XY'=sum(pmin(x[dck],y[dck])))
                                         )))
  }
