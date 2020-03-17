library(devtools)

fsMaggetti <- read.csv("data-raw/Maggetti.csv",row.name=1)

fsKA <- read.csv("data-raw/KA.csv",row.name=1)

fsVis <- read.csv("data-raw/Vis.csv",row.name=1)

use_data(fsMaggetti)
use_data(fsKA)
use_data(fsVis)

