###These are the colours used on the IEB poster template
#Green
evpgreen<-rgb(94,159,91,max=255)
#Orange
evporange<-rgb(215,120,40,max=255)
#Combine both into a palette, you can add more colours to this set if you need more than two colours in a plot
evpalette<-palette(c(evpgreen, evporange))

#Add this palette to your ggplot figure by adding this line
#+scale_color_manual(values=evpalette)#

###Example using the iris dataset
library(ggplot2)
ggplot(iris[iris$Species%in%c("setosa", "virginica"),],aes(x=Sepal.Length, y=Sepal.Width, col=Species))+
  geom_point(size=3)+scale_color_manual(values=evpalette)
