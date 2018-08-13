# to install jrGgplot2 package
install.packages("drat")
drat::addRepo("jr-packages")
install.packages("jrGgplot2", dependencies = TRUE)

library(jrGgplot2)
library(ggplot2)

data(Beauty,package = "jrGgplot2")

g <-  ggplot(data = Beauty)

g1 <- g + geom_point(aes(x = age, y = beauty))


g + geom_point(aes(x = age, y = beauty, color=gender))

g + geom_point(aes(x = age,
                   y = beauty,
                   alpha=evaluation,
                   color=gender))
#why the difference ??
g + geom_point(aes(x = age, y = beauty, color=tenured))

g + geom_point(aes(x = age, y =beauty,
                   color=factor(tenured)))

# Box plots 
# update from practical sheet

g + geom_boxplot(aes(x = gender,
                     y = beauty, 
                     color=tenured))
## OK
g + geom_boxplot(aes(x = gender,
                     y =beauty,
                   color=factor(tenured)))

# Bar plot
g + geom_bar(aes(x=factor(tenured)))

# First, we round ages to the nearest
# decade:
  
Beauty$dec = factor(signif(Beauty$age, 1))
g = ggplot(data = Beauty)

g + geom_bar(aes(x= gender, fill=dec))

g + geom_bar(aes(x= gender, fill=dec),
                 position="stack")

g + geom_bar(aes(x=gender, fill=dec), position = "dodge")




             