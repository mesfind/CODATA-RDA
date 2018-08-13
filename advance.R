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

# install.packages("ggrepel")

library(ggrepel)
library(tidyverse)
data(bond, package = "jrGgplot2")

ggplot(bond, aes(Kills, Alcohol_Units))+
  geom_point() +
  geom_text_repel(aes(label=Actor))


ggplot(bond, aes(Kills, Alcohol_Units, color=Alcohol))+
  geom_point() +
  geom_text_repel(aes(label=Actor))

just_connery = bond[bond$Actor=="Connery",]
ggplot(bond, aes(Kills, Alcohol_Units, color=Alcohol, group=Actor)) +
         geom_point()+
         geom_text_repel(data = just_connery,
                         mapping = aes(x=Kills,
                                       y = Alcohol_Units,
                                       label=Actor))



ggplot(bond, aes(Kills, Alcohol_Units, color=Alcohol, group=Actor)) +
  geom_point()+
  geom_text_repel(mapping = aes(x=Kills,
                                y = Alcohol_Units,
                                label=Actor))

#statics
d = data.frame(x=1:50, y=1:50, z=0:9)
head(d)
tail(d)

ggplot(d, aes(x,y))+
  geom_point()

ggplot(d, aes(x,y, color=z))+
  geom_point()

#changing the color 
ggplot(d, aes(x,y, color=factor(z)))+
  geom_point()

# this line of code couldn't change the color of the plot
ggplot(d, aes(x,y))+
  geom_point(aes(color="blue"))


ggplot(d, aes(x,y, color=as.character(z)))+
  geom_point(alpha=0.5)

ggplot(d, aes(x,y, color=factor(z), size=z, shape=factor(z)))+
  geom_point(alpha=0.5)


ggplot(bond,aes(Alcohol_Units,Kills))+
  stat_smooth(method=lm,se=FALSE)

ggplot(bond,aes(Alcohol_Units,Kills))+
  stat_smooth(aes(color=Nationality),se=FALSE)


ggplot(bond,aes(Alcohol_Units,Kills))+
  stat_smooth(method=lm,aes(color=Nationality),se=FALSE)



ggplot(bond, aes(Actor, Alcohol_Units))+
  stat_summary(geom = "point",fun.y = mean)


ggplot(bond, aes(Actor, Alcohol_Units))+
  stat_summary(geom = "point",fun.y = function(i){max(i) -min(i)})

## Standard error function
std_err = function(i)
  dt(0.975, length(i) - 1) * sd(i) / sqrt(length(i))
ggplot(bond, aes(x = Actor, y = Alcohol_Units)) +
  stat_summary(fun.ymin = function(i) mean(i) - std_err(i),
               fun.ymax = function(i) mean(i) + std_err(i),
               colour = "steelblue", geom = "errorbar",
               width = 0.2, lwd = 2) +
  ylim(c(0, 20))


install.packages("hrbrthemes")
install.packages("plotly")
install.packages("ggridges")
library(plotly)
library(ggridges)
library(hrbrthemes)

data(movies, package = "ggplot2movies")

colnames(movies)

(g = ggplot(movies, aes(length))+ xlim(0,200) +
    geom_histogram(aes(y =..density..), binwidth = 3))


g + facet_grid(Comedy ~ .)



g + facet_grid(. ~ Animation)



g + facet_grid(Comedy ~ Animation)


movies$decade = floor(movies$year/10)*10
ggplot(movies, aes(x=length))+
  geom_histogram()+
  facet_wrap(~ decade, ncol = 6)


# Ridge plots
library(ggridges)

ggplot(movies,
       aes(length, year, group=year, height = ..density..)) +
  geom_density_ridges(scale=10, alpha=0.7) +
  theme_ridges(grid = FALSE)+
  scale_x_log10(limits = c(1, 500),
                breaks = c(1, 10, 100, 1000),
                expand = c(0.01, 0)) +
  scale_y_reverse(breaks = seq(2000, 1900, by = -20),
                  expand = c(0.01, 0))

# Axis Scales

data(movies, package = "ggplot2movies")
known_budget = movies[!is.na(movies), ]

h = ggplot(known_budget, aes(y = length)) + 
  ylim(0, 500)
h1 = h + 
  geom_point(aes(budget), alpha = 0.2)

h1

h2 = h + geom_point(aes(log10(budget)), alpha = 0.2)

h2




df = overplot_data(n=20000)             

(h <- ggplot(df) + 
    geom_point(aes(x,y)))


h + geom_jitter(aes(x,y))

h + stat_density2d(aes(x,y, fill=..density..),
                   contour=FALSE, geom="tile")
h + stat_bin2d(aes(x,y, fill=..density..),
                   contour=FALSE, geom="tile")

h + stat_binhex(aes(x,y, fill=..density..),
               contour=FALSE, geom="tile")

  