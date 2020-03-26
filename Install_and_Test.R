library(devtools)
library(roxygen2)
library(QCA)

document()
setwd("..")
install("VeriCov")

library(VeriCov)

fsKA

modKA <- minimize(fsKA, type='fs', outcome='SUPRA', conditions="REGIO,CONF,HMC,GENP",incl.cut=0.9)

VeriCov(modKA, "SUPRA", fsKA, plot='Veri', tablecols='both')
