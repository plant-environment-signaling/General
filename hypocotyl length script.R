setwd("D:/Dropbox/Werk-Utrecht/results/18-07-11 pifQ phenotyping")
library(xlsx)
hypocotyl_length = read.xlsx("hypocotyl length 02-07-18.xlsx",2)#filename and sheet nr.


#for plotting we calculate the mean and SEM separately
d.mean <- tapply(hypocotyl_length$length, list(hypocotyl_length$genotype, hypocotyl_length$treatment), mean)
# R does not have a function for standard error, 
# but you caneasily make your own functions in R
sem <- function(x) sd(x)/length(x)^0.5 
d.sem <- tapply(hypocotyl_length$length, list(hypocotyl_length$genotype, hypocotyl_length$treatment), sem)
# inspect output
d.mean
d.sem
# we need to transpose the table for good plotting
d.mean <- t(d.mean)
d.sem <- t(d.sem)


jpeg(filename = "hypocotyl_length_barplot.jpg",
     width = 400, height = 700, units = "px", pointsize = 26, 
     quality = 97, type = "windows")
barCenters <- barplot(height = d.mean, beside = TRUE, col=c("blueviolet", "chartreuse"),
                      ylab = "hypocotyl length (mm)",
                      ylim=c(0,max(d.mean+d.sem+0.5)), width=0.5, space=c(0.25,1.25), border='black')
# to place error bars in R, you actually draw arrows
# specifiy the starting (x,y) and ending coordinates (x,y),
# specifiy arrows on poth side (code=3) and the angle of the arrow tips (90)
arrows(barCenters, d.mean-d.sem, barCenters, d.mean+d.sem, angle = 90,
       code = 3, length = 0.03, lwd=1, col='black')
legend(1,max(d.mean+d.sem+0.5), c("WL","WL+FR"),
       fill = c("blueviolet", "chartreuse"), cex = 0.75)

dev.off()

#And now the stats

#however we have genotype and treatment variables that might interact
#that's why we want a two-way anova
model = aov(length ~ genotype * treatment, data=hypocotyl_length)
#with a linear model we look at the response (lrdensity) vs the factors treatment and genotype
tresult = anova(model)#we make the ANOVA table
#now to write away the table
twb = createWorkbook(type="xlsx") #create an excel workbook
tsheet = createSheet(twb,"hypocotyl_length_2-way ANOVA") #create sheet
addDataFrame(tresult,tsheet) #add the frame to the excel workbook

tresult = TukeyHSD(model, which = c("treatment","genotype","genotype:treatment"), ordered= FALSE)
#this gives significance for the two way anova
tframea = as.data.frame(tresult$treatment)
tframeb = as.data.frame(tresult$genotype)
tframec= as.data.frame(tresult$`genotype:treatment`)
#by using the $ sign you can coerce the Tukey output into a data frame
addDataFrame(tframea, sheet = tsheet, startRow = 7)
#then we can add the data frame to the workbook
addDataFrame(tframeb, sheet = tsheet, startRow = 10)
addDataFrame(tframec, sheet = tsheet, startRow = 15)

#saveWorkbook(twb, file = "test2.xlsx") #save the workbook as an excel file

saveWorkbook(twb, file = "hypocotyl_length_2-WAY-ANOVA-stats.xlsx") #save the workbook as an excel file


#windows(9,6,16)
jpeg(filename = "hypocotyl_length_2wayANOVAmodel-plot.jpg",
     width = 900, height = 900, units = "px", pointsize = 26, 
     quality = 97, type = "windows")
par(mfrow=c(2,2))
#dit plot het anova model
plot(model)
#
dev.off()


jpeg(filename = "hypocotyl_length_interaction-plot.jpg",
     width = 900, height = 600, units = "px", pointsize = 26, 
     quality = 97, type = "windows")
interaction.plot(hypocotyl_length$genotype, hypocotyl_length$treatment, hypocotyl_length$length)
#this plots the interaction of the two-way
dev.off()

