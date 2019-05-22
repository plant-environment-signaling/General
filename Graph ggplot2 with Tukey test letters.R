# This script is to analyse data and create a graph whith a Tukey test as post hoc (after Anova)
# Always set the working directory. This file has been annoated with texts, using the hashtag.
# The whole file is designed not designed to run as a continous script because of outlier checks

# Read a csv file
setwd("C:/Users/Oskam007/OneDrive - Universiteit Utrecht/Werk/Experiments/2019/Hydroponics/Hydroponics try-out")
Data<-read.csv("2019_04_25 Hydroponics tryout.csv", sep = ";")

#Load required libraries
library("car")
library("multcompView")
library("RColorBrewer")
library("ggpubr")
library("plyr")

# View your data
View(Data)

# Fit data to linear model and inspect for outliers
hypo24<-lm(Data$hyponasty24~Data$treatment)
plot(hypo24)

#Remove outliers (check residuals vs factor levels to see in wich group they are etc), 
#and view your data to see if the deletion was succesfull
subhypo24<-Data[-c(34, 45,38),]
View(subhypo24)


# Fit data without outliers to linear model
subhypo24lin<-lm(subhypo24$hyponasty24~subhypo24$treatment)
plot(subhypo24lin)

# Check homogeneity of variances
# Levene's test, via car package (carData required)
# a Pr (P) value higher than 0.05 is desired. You do not want significant variances within your groups
# Check also without outliers

leveneTest(hyponasty24 ~ treatment, data = subhypo24)
leveneTest(hyponasty24 ~ treatment, data = hypo24)

# Perform anova
model=lm( subhypo24$hyponasty24 ~ subhypo24$treatment )
ANOVA=aov(model)

#TWO WAYS TO create a graph, first via normal boxplot, second part is via ggplot2.
# I prefer ggplot2, so everything is silenced using hashtags.

# PART 1 Perform post-hoc test, if you get an error message, I added the StringAsFactors=FALSE, remove that first
#TUKEY <- TukeyHSD(x=ANOVA, 'subhypo24$treatment', conf.level=0.95, stringsAsFactors=FALSE )

# Tuckey test representation :
#plot(TUKEY , las=1 , col="brown" )

# I need to group the treatments that are not different each other together.
#generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  #Tukey.levels <- TUKEY[[variable]][,4]
  #Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  #Tukey.labels$treatment=rownames(Tukey.labels)
  #Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  #return(Tukey.labels)
#}

# Apply the function on my dataset
#LABELS=generate_label_df(TUKEY , "subhypo24$treatment")

# Lay-out of the graph: A panel of colors to draw each group with the same color :
#my_colors=c( rgb(143,199,74,maxColorValue = 255),rgb(242,104,34,maxColorValue = 255), rgb(111,145,202,maxColorValue = 255),rgb(254,188,18,maxColorValue = 255) , rgb(74,132,54,maxColorValue = 255),rgb(236,33,39,maxColorValue = 255),rgb(165,103,40,maxColorValue = 255))

# Draw the basic boxplot
#a=boxplot(subhypo24$hyponasty24 ~ subhypo24$treatment , ylim=c(min(subhypo24$hyponasty24) , 1.1*max(subhypo24$hyponasty24)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="Hyponastic response (?? petiole angle (°))" , main="")
# I want to write the letter over each box. Over is how high I want to write it.
#over=0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
#text( c(1:nlevels(subhypo24$treatment)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )


#PART 2, using ggplot2
# Make boxplot of trait by treatment
# Do the posthoc test based on the anova, again, added StringAsFactors=FALSE because of errors
TUKEY <- TukeyHSD(x=ANOVA, 'subhypo24$treatment', conf.level=0.95, stringsAsFactors=FALSE, ordered = FALSE )

# Create the function to generate the labels
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

# Actually generate the labels
labels<- generate_label_df (TUKEY, 'subhypo24$treatment')
# Rename columns for merging
names(labels)<- c('Letters','conditions')

# Obtain letter position for y axis using means
yvalue<-aggregate(.~treatment, data=subhypo24, mean)

# Merge dataframes, treatment is the original from the full data, conditions is the one you just created above
final<- merge(labels,yvalue,  by= "treatment", "conditions") 

#Creating the graph using ggplot2, you store it, so that you can add extra layers
bphyponasty24<- ggboxplot(subhypo24, x = "treatment", y = "hyponasty24",bxp.errorbar = TRUE,bxp.errorbar.width = 0.2,
                          ylim = c(-10,30),
                          order = c("WL", "FR", "FRt_WL", "LBL", "FR_LBL", "FRt_LBL"),
                          main = "Hyponastic response (24h), outliers removed", ylab = "Hyponastic response (?? petiole angle (°))", xlab = "Treatment") + geom_boxplot()+
  geom_text( data = final, aes(x = conditions, y= hyponasty24, label = Letters, vjust=-7,hjust=0.5)) 

# Adding the extra layers and printing the graph in your plot screen
final <- bphyponasty24 + theme(legend.position = "none") +  geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.1)

#To save the picture automatically, this line will be improved (jitter sometimes interferes with Tukey letters)
ggsave(filename = "Hyponasty tukey hydroponics.png",plot = final,width = 7,height = 5,dpi = 600)
 