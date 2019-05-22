setwd("E:\\Dropbox\\Werk-Utrecht\\results\\19-04-17 bbox 20,21") #always check this, use two slashes
rootdata = read.csv("19-04-30_rawrootdata.csv") #the filename of the smartroot output
#Make sure there is never a comma in the filename of the image!!!!
head(rootdata)

mainroots=rootdata[rootdata$root_order==0,] 
#I select from the root order only the ones with a "0" and put them in mainroots
head(mainroots)

LRroots=rootdata[rootdata$root_order==1,] 
#I select from the root order only the ones with a "1" and put them in LRroots
head(LRroots)

#werkwijze#2 deze werkwijze rockt, maar je kan geen typo's maken in je originele data

tl=strsplit(as.vector(mainroots$root_name),"_") #here it separates genotype and treatment
#we splitten de string op de underscore, gooien hem naar een temp table (tl)

mainroots$genotype <- sapply(tl, function(x)x[1])
#Dan gooien we via sapply de 1e kolom naar het genotype

c2 <- sapply(tl, function(x)x[2])
#Dan de tweede kolom naar een temp c2 variable

mainroots$treatment <- gsub("[1234567890]", "", c2)
#en dan selecteren we de getallen en vervangen we ze door niks

trimws(mainroots$genotype,c("both"))
trimws(mainroots$treatment,c("both"))

mainroots$genotype #is all ok?
mainroots$treatment #is all ok?

mainroots$lrdensity = (mainroots$n_child / mainroots$length)
#calculate the lrdensity

tl = grepl(x= mainroots$genotype, pattern = "Col-0") #I select the 1st pattern
genotype1=mainroots[tl,] #and use the selection to get data

tl = grepl(x= mainroots$genotype, pattern = "bbx20") #I select the 2nd pattern
genotype2=mainroots[tl,] #and use the selection to get data

tl = grepl(x= mainroots$genotype, pattern = "bbx21") #I select the 3rd pattern
genotype3=mainroots[tl,] #and use the selection to get data

tl = grepl(x= mainroots$genotype, pattern = "bbxdbl2021") #I select the 4th pattern
genotype4=mainroots[tl,] #and use the selection to get data

#tl = grepl(x= mainroots$genotype, pattern = "100nM-PAC") #I select the 5th pattern
#genotype5=mainroots[tl,] #and use the selection to get data

#________________________________________________________________________________

#Now we're repeating earlier code to get laterals  
tl=strsplit(as.vector(LRroots$parent_name),"_")
#we splitten de string op de underscore, gooien hem naar een temp table (tl)

LRroots$genotype <- sapply(tl, function(x)x[1])
#Dan gooien we via sapply de 1e kolom naar het genotype

c2 <- sapply(tl, function(x)x[2])
#Dan de tweede kolom naar een temp c2 variable

LRroots$treatment <- gsub("[1234567890]", "", c2)
#en dan selecteren we de getallen en vervangen we ze door niks

trimws(LRroots$genotype,c("both"))
trimws(LRroots$treatment,c("both"))

LRroots$genotype #is all ok?
LRroots$treatment #is all ok?


LRroots$lengthmm = (LRroots$length * 10)
#convert length to mm

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
LRrootangles=LRroots[LRroots$insertion_angle>10,] 
# use this if you want to only include lateral roots where the angles are above a certain limit
LRrootangles=LRrootangles[LRrootangles$lengthmm >0.33,] 
# use this if you want to only include lateral roots where the lentgth is above a certain limit
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


tl = grepl(x= LRroots$genotype, pattern = "Col-0") #I select the 1st pattern
LRgenotype1=LRroots[tl,] #and use the selection to get data

tl = grepl(x= LRroots$genotype, pattern = "bbx20") #I select the 2nd pattern
LRgenotype2=LRroots[tl,] #and use the selection to get data

tl = grepl(x= LRroots$genotype, pattern = "bbx21") #I select the 3rd pattern
LRgenotype3=LRroots[tl,] #and use the selection to get data

tl = grepl(x= LRroots$genotype, pattern = "bbxdbl2021") #I select the 3rd pattern
LRgenotype4=LRroots[tl,] #and use the selection to get data

#tl = grepl(x= LRroots$genotype, pattern = "100nM-PAC") #I select the 3rd pattern
#LRgenotype5=LRroots[tl,] #and use the selection to get data

tl = grepl(x= LRrootangles$genotype, pattern = "Col-0") #I select the 1st pattern
LRanglesgenotype1=LRrootangles[tl,] #and use the selection to get data

tl = grepl(x= LRrootangles$genotype, pattern = "bbx20") #I select the 2nd pattern
LRanglesgenotype2=LRrootangles[tl,] #and use the selection to get data

tl = grepl(x= LRrootangles$genotype, pattern = "bbx21") #I select the 3rd pattern
LRanglesgenotype3=LRrootangles[tl,] #and use the selection to get data

tl = grepl(x= LRrootangles$genotype, pattern = "bbxdbl2021") #I select the 3rd pattern
LRanglesgenotype4=LRrootangles[tl,] #and use the selection to get data

#____________________________________________________________________________________

#now we write the data to an excel file for graphpad or sumpin like that
library(xlsx)
twb = createWorkbook(type="xlsx") #create an excel workbook


tsheet = createSheet(twb,"genotype1_mainroots") #create sheet
addDataFrame(genotype1,tsheet) #add the frame to the excel workbook

tsheet = createSheet(twb,"genotype1_lateralroots") #create sheet
addDataFrame(LRgenotype1,tsheet) #add the frame to the excel workbook

tsheet = createSheet(twb,"genotype1_lateralrootangles") #create sheet
addDataFrame(LRanglesgenotype1,tsheet) #add the frame to the excel workbook


tsheet = createSheet(twb,"genotype2_mainroots") #create sheet
addDataFrame(genotype2,tsheet) #add the frame to the excel workbook

tsheet = createSheet(twb,"genotype2_lateralroots") #create sheet
addDataFrame(LRgenotype2,tsheet) #add the frame to the excel workbook

tsheet = createSheet(twb,"genotype2_lateralrootangles") #create sheet
addDataFrame(LRanglesgenotype2,tsheet) #add the frame to the excel workbook


tsheet = createSheet(twb,"genotype3_mainroots") #create sheet
addDataFrame(genotype3,tsheet) #add the frame to the excel workbook

tsheet = createSheet(twb,"genotype3_lateralroots") #create sheet
addDataFrame(LRgenotype3,tsheet) #add the frame to the excel workbook

tsheet = createSheet(twb,"genotype3_lateralrootangles") #create sheet
addDataFrame(LRanglesgenotype3,tsheet) #add the frame to the excel workbook


tsheet = createSheet(twb,"genotype4_mainroots") #create sheet
addDataFrame(genotype4,tsheet) #add the frame to the excel workbook

tsheet = createSheet(twb,"genotype4_lateralroots") #create sheet
addDataFrame(LRgenotype4,tsheet) #add the frame to the excel workbook

tsheet = createSheet(twb,"genotype4_lateralrootangles") #create sheet
addDataFrame(LRanglesgenotype4,tsheet) #add the frame to the excel workbook


tsheet = createSheet(twb,"genotype5_mainroots") #create sheet
addDataFrame(genotype5,tsheet) #add the frame to the excel workbook

tsheet = createSheet(twb,"genotype5_lateralroots") #create sheet
addDataFrame(LRgenotype5,tsheet) #add the frame to the excel workbook

tsheet = createSheet(twb,"genotype1_lateralrootangles") #create sheet
addDataFrame(LRanglesgenotype5,tsheet) #add the frame to the excel workbook

saveWorkbook(twb, file = "19-05-02_processed_data_and-angles.xlsx") #save the workbook as an excel file


#_________________________________________________________________________________

#for plotting we calculate the mean and SEM separately
d.mean <- tapply(mainroots$lrdensity, list(mainroots$genotype, mainroots$treatment), mean)
# R does not have a function for standard error, 
# but you caneasily make your own functions in R
sem <- function(x) sd(x)/length(x)^0.5 
d.sem <- tapply(mainroots$lrdensity, list(mainroots$genotype, mainroots$treatment), sem)
# inspect output
d.mean
d.sem
# we need to transpose the table for good plotting
d.mean <- t(d.mean)
d.sem <- t(d.sem)


#And now the stats
library(xlsx)

#lrdensityanova = aov(lrdensity  ~ genotype, data=mainroots) #this does a one-way anova
#summary (lrdensityanova)
#however we have genotype and treatment variables that might interact
#that's why we want a two-way anova
model = aov(lrdensity ~ genotype * treatment, data=mainroots)
#with a linear model we look at the response (lrdensity) vs the factors treatment and genotype
tresult = anova(model)#we make the ANOVA table
#now to write away the table
twb = createWorkbook(type="xlsx") #create an excel workbook
tsheet = createSheet(twb,"LR_Density_2-way ANOVA") #create sheet
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
addDataFrame(tframec, sheet = tsheet, startRow = 20)
addDataFrame(d.mean, sheet = tsheet, startRow = 55)
addDataFrame(d.sem, sheet = tsheet, startRow = 60)

#saveWorkbook(twb, file = "test3.xlsx") #save the workbook as an excel file

#saveWorkbook(twb, file = "LR-Dens_2-WAY-ANOVA-stats.xlsx") #save the workbook as an excel file



#____________________________________________________________________________


#for plotting we calculate the mean and SEM separately
d.mean <- tapply(mainroots$length, list(mainroots$genotype, mainroots$treatment), mean)
# R does not have a function for standard error, 
# but you caneasily make your own functions in R
sem <- function(x) sd(x)/length(x)^0.5 
d.sem <- tapply(mainroots$length, list(mainroots$genotype, mainroots$treatment), sem)
# inspect output
d.mean
d.sem
# we need to transpose the table for good plotting
d.mean <- t(d.mean)
d.sem <- t(d.sem)


#lrdensityanova = aov(lrdensity  ~ genotype, data=mainroots) #this does a one-way anova
#summary (lrdensityanova)
#however we have genotype and treatment variables that might interact
#that's why we want a two-way anova
model = aov(length ~ genotype * treatment, data=mainroots)
#with a linear model we look at the response (lrdensity) vs the factors treatment and genotype
tresult = anova(model)#we make the ANOVA table
#now to write away the table
#twb = createWorkbook(type="xlsx") #create an excel workbook
tsheet = createSheet(twb,"MRlength_2-way ANOVA") #create sheet
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
addDataFrame(tframec, sheet = tsheet, startRow = 20)
addDataFrame(d.mean, sheet = tsheet, startRow = 55)
addDataFrame(d.sem, sheet = tsheet, startRow = 60)

#saveWorkbook(twb, file = "test3.xlsx") #save the workbook as an excel file
#saveWorkbook(twb, file = "MR-length_2-WAY-ANOVA-stats.xlsx") #save the workbook as an excel file


#____________________________________________________________________________


#for plotting we calculate the mean and SEM separately
d.mean <- tapply(mainroots$child_density, list(mainroots$genotype, mainroots$treatment), mean)
# R does not have a function for standard error, 
# but you caneasily make your own functions in R
sem <- function(x) sd(x)/length(x)^0.5 
d.sem <- tapply(mainroots$child_density, list(mainroots$genotype, mainroots$treatment), sem)
# inspect output
d.mean
d.sem
# we need to transpose the table for good plotting
d.mean <- t(d.mean)
d.sem <- t(d.sem)

#lrdensityanova = aov(lrdensity  ~ genotype, data=mainroots) #this does a one-way anova
#summary (lrdensityanova)
#however we have genotype and treatment variables that might interact
#that's why we want a two-way anova
model = aov(length ~ genotype * treatment, data=mainroots)
#with a linear model we look at the response (lrdensity) vs the factors treatment and genotype
tresult = anova(model)#we make the ANOVA table
#now to write away the table
#twb = createWorkbook(type="xlsx") #create an excel workbook
tsheet = createSheet(twb,"child_density_2-way ANOVA") #create sheet
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
addDataFrame(tframec, sheet = tsheet, startRow = 20)
addDataFrame(d.mean, sheet = tsheet, startRow = 55)
addDataFrame(d.sem, sheet = tsheet, startRow = 60)

#saveWorkbook(twb, file = "test4.xlsx") #save the workbook as an excel file
#saveWorkbook(twb, file = "child_density_2-WAY-ANOVA-stats.xlsx") #save the workbook as an excel file

#_________________________________________________________________

#for plotting we calculate the mean and SEM separately
d.mean <- tapply(LRrootangles$insertion_angle, list(LRrootangles$genotype, LRrootangles$treatment), mean)
# R does not have a function for standard error, 
# but you caneasily make your own functions in R
sem <- function(x) sd(x)/length(x)^0.5 
d.sem <- tapply(LRrootangles$insertion_angle, list(LRrootangles$genotype, LRrootangles$treatment), sem)
# inspect output
d.mean
d.sem
# we need to transpose the table for good plotting
d.mean <- t(d.mean)
d.sem <- t(d.sem)


#lrdensityanova = aov(lrdensity  ~ genotype, data=LRroots) #this does a one-way anova
#summary (lrdensityanova)
#however we have genotype and treatment variables that might interact
#that's why we want a two-way anova
model = aov(insertion_angle ~ genotype * treatment, data=LRrootangles)
#with a linear model we look at the response (lrdensity) vs the factors treatment and genotype
tresult = anova(model)#we make the ANOVA table
#now to write away the table
#twb = createWorkbook(type="xlsx") #create an excel workbook
tsheet = createSheet(twb,"LRangles_2-way ANOVA") #create sheet
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
addDataFrame(tframec, sheet = tsheet, startRow = 20)
addDataFrame(d.mean, sheet = tsheet, startRow = 55)
addDataFrame(d.sem, sheet = tsheet, startRow = 60)

#__________________

#for plotting we calculate the mean and SEM separately
d.mean <- tapply(LRrootangles$lengthmm, list(LRrootangles$genotype, LRrootangles$treatment), mean)
# R does not have a function for standard error, 
# but you caneasily make your own functions in R
sem <- function(x) sd(x)/length(x)^0.5 
d.sem <- tapply(LRrootangles$lengthmm, list(LRrootangles$genotype, LRrootangles$treatment), sem)
# inspect output
d.mean
d.sem
# we need to transpose the table for good plotting
d.mean <- t(d.mean)
d.sem <- t(d.sem)


#lrdensityanova = aov(lrdensity  ~ genotype, data=LRroots) #this does a one-way anova
#summary (lrdensityanova)
#however we have genotype and treatment variables that might interact
#that's why we want a two-way anova
model = aov(lengthmm ~ genotype * treatment, data=LRrootangles)
#with a linear model we look at the response (lrdensity) vs the factors treatment and genotype
tresult = anova(model)#we make the ANOVA table
#now to write away the table
#twb = createWorkbook(type="xlsx") #create an excel workbook
tsheet = createSheet(twb,"LRlength_2-way ANOVA") #create sheet
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
addDataFrame(tframec, sheet = tsheet, startRow = 20)
addDataFrame(d.mean, sheet = tsheet, startRow = 55)
addDataFrame(d.sem, sheet = tsheet, startRow = 60)


#saveWorkbook(twb, file = "test4.xlsx") #save the workbook as an excel file
saveWorkbook(twb, file = "19-05-02-WAY-ANOVA-stats_and-angles.xlsx") #save the workbook as an excel file

