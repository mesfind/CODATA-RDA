# to install jrGgplot2 package
install.packages("drat")
drat::addRepo("jr-packages")
install.packages("jrGgplot2", dependencies = TRUE)

library(jrGgplot2)
library(ggplot2)

data(Beauty,package = "jrGgplot2")
