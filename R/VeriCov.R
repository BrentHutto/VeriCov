#' Computes sufficiency coverage and consistency parameters 
#' by Ragin (R), Haesebrouck (H) and Veri (V) methods

#' @param  model Either QCA minimized model object or one or more SOP expressions in quotes
#' @param  out  Name of the outcome variable, in quotes
#' @param  dat  The calibrated data

#' @param jitter Passed to XYplot function
#' @param enhance Passed to XYplot function
#' @param sol is the solution number (for model objects)
#' @param tablerows is 'terms', 'solution' or 'both'
#' @param tablecols is 'none', 'Ragin', 'Veri', 'both' or 'details'
#' @param plot is FALSE, TRUE or 'Veri'

#' @export

VeriCov <- function(model, out, dat, jitter=TRUE, enhance=FALSE, sol=1,
                    tablerows='both', tablecols='Veri', plot=FALSE ) 
  {
  vp <- (tolower(plot)=='veri')
  
  # Check input for quotes around SOP
  if (grepl('\"',deparse(model)[[1]])) {
          expr <- as.character(model)
          lastrow <- 'solution_formula'
          if (missing(tablerows)) { tablerows <- 'terms' }
  
  # Otherwise, input is is a model
      } else {
          expr <- as.character(model$solution[[sol]])
          lastrow <- 'model_solution'
          if (missing(tablerows)) { tablerows <- 'both' }
          if (missing(dat)) {
              if (is.data.frame(out)) {
                  dat <- out
                  out <- as.character(model$tt$call$outcome) 
              }
          }
  }
  
details <- c("typ.N", "typ.X", 
             "dcv.N", "dcv.X", 
             "iir.N", "iir.X", 
             "ir.N", "ir.X", 
             "dcd.N", "dcd.X", 
             "dck.N", "dck.X", "dck.XY", 
             "ambig.N", "ambig.X", 
             "unamb.N", "unamb.X", 
             "incon.N", "incon.X", "incon.XY")
  
  # Use lappply() with rbind() to create one row for every SOP
  res1 <- as.data.frame(do.call(rbind,lapply(expr, covV, out, dat)))
  res1 <- res1[,c("condition", 
                  "InclR", "CovR", "PRI", 
                  "InclH",
                  "QuantV", "QualV", 
                  "CovV", "InclV",
                  details)]

  # Use paste() to create a DNF and put it on a Solution row
  res2 <- as.data.frame(covV(paste(expr,collapse='+'), out, dat))
  res2 <- res2[,c("condition", 
                  "InclR", "CovR", "PRI", 
                  "InclH",
                  "QuantV", "QualV", 
                  "CovV", "InclV",
                  details)]
  res2$condition <- lastrow
  
  # Put both SOP row(s) and Solution row into return value, optionally plot both
  if (tolower(tablerows)=='both') {
    res <- rbind(res1,res2)
    if (plot != FALSE) {
        invisible(lapply(expr, MYplot, out, dat, jitter, enhance, Veri=vp))
        invisible(MYplot(paste(expr,collapse='+'), 
                         out, dat, jitter, enhance, Veri=vp, model=TRUE))
        }
    }
  # Put SOP row(s) into return value, optionally plot SOP row(s)
  else if (tolower(tablerows)=='terms') {
    res <- res1
    if (plot != FALSE) {
        invisible(lapply(expr, MYplot, out, dat, jitter, enhance, Veri=vp))
        }
  }
  # Put a Solution row into return value, optionally plot Solution
  else if (tolower(tablerows)=='solution') {
    res <- res2
    if (plot != FALSE) {
        invisible(MYplot(paste(expr,collapse='+'), 
                         out, dat, jitter, enhance, Veri=vp, model=TRUE))
        }
  }
  
  row.names(res) <- res$condition

  if (tolower(tablecols)=='veri') {res <- res[, c(5,6,7,8,9)]} 
      else if (tolower(tablecols)=='both') {res <- res[, c(2,3,4,5,6,7,8,9)]}
      else if (tolower(tablecols)=='ragin') {res <- res[, c(2,3,4)]}
      else if (tolower(tablecols)=='details') {res <- res[, -1]}

  if (tolower(tablecols)=='' | tolower(tablecols)=='none') {
          invisible()
      } else {
          return(structure(format(res, digits=3, nsmall=3), 
                 class = c("data.frame", "VeriCov")))
      }
}
