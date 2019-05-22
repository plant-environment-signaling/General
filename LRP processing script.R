setwd("C:/Data/Dropbox/Werk-Utrecht/results/18-04-28 dellap hyhhy5 Col-0 results")
library(xlsx)
library(stats)


LRPdata = read.xlsx("18-06-07_LRP_data.xlsx", 2)
#this selects the second sheet in the excel sheet

#below the stages are grouped into the normal groups and added
LRPdata$stage12 = LRPdata$stage.I+LRPdata$stage.II
LRPdata$stage34 = LRPdata$stage.III+LRPdata$stage.IV
LRPdata$stage56 = LRPdata$stage.V+LRPdata$stage.VI
LRPdata$stage7E = LRPdata$stage.VII+LRPdata$Emerged

#below the fraction of total LRP is calculated
LRPdata$propstage12 = LRPdata$stage12/LRPdata$total
LRPdata$propstage34 = LRPdata$stage34/LRPdata$total
LRPdata$propstage56 = LRPdata$stage56/LRPdata$total
LRPdata$propstage7E = LRPdata$stage7E/LRPdata$total


tl = grepl(x= LRPdata$genotype, pattern = "Col-0") #I select the 1st pattern
genotype1=LRPdata[tl,] #and use the selection to get data

tl = grepl(x= LRPdata$genotype, pattern = "dellap") #I select the 2nd pattern
genotype2=LRPdata[tl,] #and use the selection to get data

tl = grepl(x= LRPdata$genotype, pattern = "hy5hyh") #I select the 3rd pattern
genotype3=LRPdata[tl,] #and use the selection to get data

#_______________________________________________________________________________________

#for plotting we calculate the mean and SEM separately
d.mean12 <- tapply(LRPdata$propstage12, list(LRPdata$genotype, LRPdata$treatment), mean)
# R does not have a function for standard error, 
# but you caneasily make your own functions in R
sem12 <- function(x) sd(x)/length(x)^0.5 
d.sem12 <- tapply(LRPdata$propstage12, list(LRPdata$genotype, LRPdata$treatment), sem12)
# inspect output
d.mean12
d.sem12
# we need to transpose the table for good plotting
d.mean12 <- t(d.mean12)
d.sem12 <- t(d.sem12)

d.mean34 <- tapply(LRPdata$propstage34, list(LRPdata$genotype, LRPdata$treatment), mean)
sem34 <- function(x) sd(x)/length(x)^0.5 
d.sem34 <- tapply(LRPdata$propstage34, list(LRPdata$genotype, LRPdata$treatment), sem34)
d.mean34
d.sem34
d.mean34 <- t(d.mean34)
d.sem34 <- t(d.sem34)

d.mean56 <- tapply(LRPdata$propstage56, list(LRPdata$genotype, LRPdata$treatment), mean)
sem56 <- function(x) sd(x)/length(x)^0.5 
d.sem56 <- tapply(LRPdata$propstage56, list(LRPdata$genotype, LRPdata$treatment), sem56)
d.mean56
d.sem56
d.mean56 <- t(d.mean56)
d.sem56 <- t(d.sem56)

d.mean7E <- tapply(LRPdata$propstage7E, list(LRPdata$genotype, LRPdata$treatment), mean)
sem7E <- function(x) sd(x)/length(x)^0.5 
d.sem7E <- tapply(LRPdata$propstage7E, list(LRPdata$genotype, LRPdata$treatment), sem7E)
d.mean7E
d.sem7E
d.mean7E <- t(d.mean7E)
d.sem7E <- t(d.sem7E)


jpeg(filename = "LRPstages-fractions_barplot.jpg",
     width = 2200, height = 700, units = "px", pointsize = 36, 
     quality = 97, type = "windows")
par(mfrow = c(1,4)) #this makes a two figure graphical window
bar12 <- barplot(height = d.mean12, beside = TRUE, col=c("blueviolet", "chartreuse"), 
                      ylim=c(0,0.8), space=c(0.5,1.25), border='black', main = "stage 1+2")
# to place error bars in R, you actually draw arrows
# specifiy the starting (x,y) and ending coordinates (x,y),
# specifiy arrows on poth side (code=3) and the angle of the arrow tips (90)
arrows(bar12, d.mean12-d.sem12, bar12, d.mean12+d.sem12, angle = 90,
       code = 3, length = 0.03, lwd=1, col='black')
legend(1.2,0.8, c("WL","WL+FR"),
       fill = c("blueviolet", "chartreuse"), cex = 1)

bar34 <- barplot(height = d.mean34, beside = TRUE, col=c("blueviolet", "chartreuse"), 
                 ylim=c(0,0.8),  space=c(0.5,1.25), border='black', main = "stage 3+4")
# to place error bars in R, you actually draw arrows
# specifiy the starting (x,y) and ending coordinates (x,y),
# specifiy arrows on poth side (code=3) and the angle of the arrow tips (90)
arrows(bar34, d.mean34-d.sem34, bar34, d.mean34+d.sem34, angle = 90,
       code = 3, length = 0.03, lwd=1, col='black')

bar56 <- barplot(height = d.mean56, beside = TRUE, col=c("blueviolet", "chartreuse"), 
                 ylim=c(0,0.8), space=c(0.5,1.25), border='black', main = "stage 5+6")
# to place error bars in R, you actually draw arrows
# specifiy the starting (x,y) and ending coordinates (x,y),
# specifiy arrows on poth side (code=3) and the angle of the arrow tips (90)
arrows(bar56, d.mean56-d.sem56, bar56, d.mean56+d.sem56, angle = 90,
       code = 3, length = 0.03, lwd=1, col='black')

bar7E <- barplot(height = d.mean7E, beside = TRUE, col=c("blueviolet", "chartreuse"), 
                 ylim=c(0,0.8), space=c(0.5,1.25), border='black', main = "stage 7+E")
# to place error bars in R, you actually draw arrows
# specifiy the starting (x,y) and ending coordinates (x,y),
# specifiy arrows on poth side (code=3) and the angle of the arrow tips (90)
arrows(bar7E, d.mean7E-d.sem7E, bar7E, d.mean7E+d.sem7E, angle = 90,
       code = 3, length = 0.03, lwd=1, col='black')

dev.off()

#And now the stats
library(xlsx)


#we have genotype and treatment variables that might interact
#that's why we want a two-way anova
model = aov(propstage12 ~ genotype * treatment, data=LRPdata)
#with a linear model we look at the response (lrdensity) vs the factors treatment and genotype
tresult = anova(model)#we make the ANOVA table
#now to write away the table
twb = createWorkbook(type="xlsx") #create an excel workbook
tsheet = createSheet(twb,"stage 1+2 ANOVA") #create sheet
addDataFrame(tresult,tsheet) #add the frame to the excel workbook

#saveWorkbook(twb, file = "test2.xlsx") #save the workbook as an excel file

tresult = TukeyHSD(model, which = c("treatment","genotype","genotype:treatment"), ordered= FALSE)
#this gives significance for the two way anova
tframea = as.data.frame(tresult$treatment)
tframeb = as.data.frame(tresult$genotype)
tframec= as.data.frame(tresult$`genotype:treatment`)
#by using the $ sign you can coerce the Tukey output into a data frame
addDataFrame(tframea, sheet = tsheet, startRow = 7)
#then we can add the data frame to the workbook
addDataFrame(tframeb, sheet = tsheet, startRow = 10)
addDataFrame(tframec, sheet = tsheet, startRow = 13)

#saveWorkbook(twb, file = "test2.xlsx") #save the workbook as an excel file

tframe = as.data.frame(pairwise.t.test(genotype1$propstage12,genotype1$treatment)$p.value)
#lable = c()
addDataFrame(tframe, sheet = tsheet, startRow = 20)

tframe = as.data.frame(pairwise.t.test(genotype1$propstage34,genotype1$treatment)$p.value)
addDataFrame(tframe, sheet = tsheet, startRow = 22)
tframe = as.data.frame(pairwise.t.test(genotype1$propstage56,genotype1$treatment)$p.value)
addDataFrame(tframe, sheet = tsheet, startRow = 24)
tframe = as.data.frame(pairwise.t.test(genotype1$propstage7E,genotype1$treatment)$p.value)
addDataFrame(tframe, sheet = tsheet, startRow = 26)

saveWorkbook(twb, file = "test3.xlsx") #save the workbook as an excel file


#windows(9,6,16)
jpeg(filename = "LR_Density_2wayANOVAmodel-plot.jpg",
     width = 900, height = 900, units = "px", pointsize = 26, 
     quality = 97, type = "windows")
par(mfrow=c(2,2))
#dit plot het anova model
plot(model)
#
dev.off()


jpeg(filename = "LR_Density_interaction-plot.jpg",
     width = 900, height = 600, units = "px", pointsize = 26, 
     quality = 97, type = "windows")
interaction.plot(mainroots$genotype, mainroots$treatment, mainroots$lrdensity)
#this plots the interaction of the two-way
dev.off()

#_______________________________________________________________________________________

#Now we're doing the total stages
#for plotting we calculate the mean and SEM separately
d.mean12 <- tapply(LRPdata$stage12, list(LRPdata$genotype, LRPdata$treatment), mean)
# R does not have a function for standard error, 
# but you caneasily make your own functions in R
sem12 <- function(x) sd(x)/length(x)^0.5 
d.sem12 <- tapply(LRPdata$stage12, list(LRPdata$genotype, LRPdata$treatment), sem12)
# inspect output
d.mean12
d.sem12
# we need to transpose the table for good plotting
d.mean12 <- t(d.mean12)
d.sem12 <- t(d.sem12)

d.mean34 <- tapply(LRPdata$stage34, list(LRPdata$genotype, LRPdata$treatment), mean)
sem34 <- function(x) sd(x)/length(x)^0.5 
d.sem34 <- tapply(LRPdata$stage34, list(LRPdata$genotype, LRPdata$treatment), sem34)
d.mean34
d.sem34
d.mean34 <- t(d.mean34)
d.sem34 <- t(d.sem34)

d.mean56 <- tapply(LRPdata$stage56, list(LRPdata$genotype, LRPdata$treatment), mean)
sem56 <- function(x) sd(x)/length(x)^0.5 
d.sem56 <- tapply(LRPdata$stage56, list(LRPdata$genotype, LRPdata$treatment), sem56)
d.mean56
d.sem56
d.mean56 <- t(d.mean56)
d.sem56 <- t(d.sem56)

d.mean7E <- tapply(LRPdata$stage7E, list(LRPdata$genotype, LRPdata$treatment), mean)
sem7E <- function(x) sd(x)/length(x)^0.5 
d.sem7E <- tapply(LRPdata$stage7E, list(LRPdata$genotype, LRPdata$treatment), sem7E)
d.mean7E
d.sem7E
d.mean7E <- t(d.mean7E)
d.sem7E <- t(d.sem7E)


jpeg(filename = "LRPstages-total_barplot.jpg",
     width = 2200, height = 700, units = "px", pointsize = 36, 
     quality = 97, type = "windows")
par(mfrow = c(1,4)) #this makes a two figure graphical window
bar12 <- barplot(height = d.mean12, beside = TRUE, col=c("blueviolet", "chartreuse"), 
                 ylim=c(0,15), space=c(0.5,1.25), border='black', main = "stage 1+2")
# to place error bars in R, you actually draw arrows
# specifiy the starting (x,y) and ending coordinates (x,y),
# specifiy arrows on poth side (code=3) and the angle of the arrow tips (90)
arrows(bar12, d.mean12-d.sem12, bar12, d.mean12+d.sem12, angle = 90,
       code = 3, length = 0.03, lwd=1, col='black')
legend(1.2,15, c("WL","WL+FR"),
       fill = c("blueviolet", "chartreuse"), cex = 1)

bar34 <- barplot(height = d.mean34, beside = TRUE, col=c("blueviolet", "chartreuse"), 
                 ylim=c(0,15),  space=c(0.5,1.25), border='black', main = "stage 3+4")
# to place error bars in R, you actually draw arrows
# specifiy the starting (x,y) and ending coordinates (x,y),
# specifiy arrows on poth side (code=3) and the angle of the arrow tips (90)
arrows(bar34, d.mean34-d.sem34, bar34, d.mean34+d.sem34, angle = 90,
       code = 3, length = 0.03, lwd=1, col='black')

bar56 <- barplot(height = d.mean56, beside = TRUE, col=c("blueviolet", "chartreuse"), 
                 ylim=c(0,15), space=c(0.5,1.25), border='black', main = "stage 5+6")
# to place error bars in R, you actually draw arrows
# specifiy the starting (x,y) and ending coordinates (x,y),
# specifiy arrows on poth side (code=3) and the angle of the arrow tips (90)
arrows(bar56, d.mean56-d.sem56, bar56, d.mean56+d.sem56, angle = 90,
       code = 3, length = 0.03, lwd=1, col='black')

bar7E <- barplot(height = d.mean7E, beside = TRUE, col=c("blueviolet", "chartreuse"), 
                 ylim=c(0,15), space=c(0.5,1.25), border='black', main = "stage 7+E")
# to place error bars in R, you actually draw arrows
# specifiy the starting (x,y) and ending coordinates (x,y),
# specifiy arrows on poth side (code=3) and the angle of the arrow tips (90)
arrows(bar7E, d.mean7E-d.sem7E, bar7E, d.mean7E+d.sem7E, angle = 90,
       code = 3, length = 0.03, lwd=1, col='black')

dev.off()
