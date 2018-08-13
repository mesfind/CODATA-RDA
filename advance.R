# to install jrGgplot2 package
# install.packages("drat")
# drat::addRepo("jr-packages")
# install.packages("jrGgplot2", dependencies = TRUE)

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


data(bond, package = "jrGgplot2")

g = ggplot(data = bond,
           mapping = aes(x = Kills,
                         y = Alcohol_Units))
# print the first layer of our plot
g

# Add layers to our plot
(g = ggplot(bond, aes(Actor, Alcohol_Units)))
(g1 =g + geom_boxplot())

g + geom_violin()

g + geom_violin()+
  geom_boxplot()

g + geom_violin()+
  geom_boxplot()+
  geom_point()
# place a color on the box plot
# Box plot aesthetic by fill
(g2 = g + geom_boxplot(aes(fill=Actor)))

# Box plot aesthetic by group
(g + geom_boxplot(aes(group=Actor)))

# Box plot aesthetic by color
(g + geom_boxplot(aes(color=Actor)))

# Box plot aesthetic by weight
(g + geom_boxplot(aes(weight=Kills)))
             