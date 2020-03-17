#' Annotates Dusa's XYplot of fsQCA with Inclusion and Coverage measures
#' using various methods (C, Ragin, T. Haesebrouck and F. Veri)

#' @param expr Expression in SOP or DNF form, in quotes
#' @param out Name of the outcome variable, in quotes
#' @param dat The calibrated data
#' @param jitter Passed to XYplot function
#' @param Veri TRUE to outline Ambiguous and Non-Ambiguous cases areas on plot
#' @param enhance Passed to XYplot function
#' @param model Passed to XYplot function

#' @export

MYplot <- function(expr, out, dat, 
                   jitter=TRUE, enhance=FALSE, Veri=TRUE, model=FALSE)
  {
  resR <- as.data.frame(covV( expr, out, dat ))[,c("InclR", "CovR", "PRI")]
  resH <- as.data.frame(covV( expr, out, dat ))[,"InclH"]
  resV <- as.data.frame(covV( expr, out, dat ))[,c("QuantV", "QualV", 
                                                   "CovV", "InclV")]
  stats <- cbind(resR, resH, resV)
  
  stats[is.na(stats)] <- 0

  XYplot(expr, out, dat, jitter=jitter, enhance=enhance, model=model)

  if (Veri) {
    box(col='white')
    box(col='black', bty='l')
    abline(0, 1, col = "white")
    abline(v = 0.5, lty = 1, col = "white")
    abline(h = 0.5, lty = 1, col = "white")

    segments(x0=0.5,y0=-0.015,x1=0.5,y1=1.015,col='grey',lty=2)
    segments(y0=0.5,x0=-0.015,y1=0.5,x1=1.015,col='grey',lty=2)
    segments(x0=1.015,y0=-0.015,x1=1.015,y1=1.015,col='grey',lwd=1,lty=2)
    segments(x0=-0.015,y0=-0.015,x1=1.015,y1=-0.015,col='grey',lwd=1,lty=2)

    segments(x0=-0.015,y0=1.015,x1=0.5,y1=0.5,col="green4",lwd=1.5,lty=1)
    segments(x0=1.015,y0=1.015,x1=0.5,y1=0.5,col="green4",lwd=1.5,lty=1)
    segments(x0=-0.015,y0=1.015,x1=1.015,y1=1.015,col="green4",lwd=1.5,lty=1)

    segments(x0=-0.015,y0=1.005,x1=0.495,y1=0.495,col="red2",lwd=1.5,lty=1)
    segments(x0=-0.015,y0=-0.015,x1=0.495,y1=0.495,col="red2",lwd=1.5,lty=1)
    segments(x0=-0.015,y0=-0.015,x1=-0.015,y1=1.005,col="red2",lwd=1.5,lty=1)
    }

  whiteout <- paste(rep("\u2588", 60), collapse="")
  mtext(whiteout, col='white', at = 0, adj = 0, line=1.3)
  mtext(whiteout, col='white', at = 0, adj = 0, line=0.05)

  mtext(paste(c("InclR:", " PRI:", " InclH:", " InclV:"),
              c(sprintf("%.3f", stats[c(1,3,4,8)])), collapse = " "),
                at = -0.02, adj = 0, cex=0.8, line=1)

  mtext(paste(c("CovR:", " CovV:", " QuantV:", " QualV:"),
              c(sprintf("%.3f", stats[c(2,5,6,7)])), collapse = ""),
                at = -0.02, adj = 0, cex=0.8, line=0)
  }
