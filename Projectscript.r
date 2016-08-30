
#Prediction of Health expenditure of a country
#Jai Kiran Duvvu, Jyothirmayi Panda, Sneha Vaishnavi Gandham

install.packages('reshape2')
install.packages('DMwR')
install.packages('ggplot2')
install.packages('randomForest')

library(DMwR)
library(reshape2)
library(ggplot2)
library(randomForest)
#############################################################################################################

file1<-read.csv("D:/Kdd/Project/Urban Population.csv", stringsAsFactors = FALSE)


file1<-subset(file1, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file1)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")
which(is.na(file1))
indx<-which(is.na(file1), arr.ind = TRUE)
file1[indx]<-rowMeans(file1[5:16], na.rm=TRUE)[indx[,"row"]]

file1<-melt(file1)
file1<-subset(file1, select=c(0:2,5:6))
colnames(file1)[3:4]<-c("Year", "Urban Population")
write.csv(file1, "Urban Population.csv", row.names = FALSE)

#==============================================================================================

file2<-read.csv("D:/Kdd/Project/Total Population.csv", stringsAsFactors = FALSE)


file2<-subset(file2, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file2)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file2))  
indx2<-which(is.na(file2), arr.ind = TRUE)
file2[indx2]<-rowMeans(file2[5:16], na.rm=TRUE)[indx2[,"row"]]

file2<-melt(file2)
file2<-subset(file2, select=c(0:2,5:6))
colnames(file2)[3:4]<-c("Year", "Total Population")
write.csv(file2, "Total Population.csv", row.names = FALSE)

#=============================================================================================

file3<-read.csv("D:/Kdd/Project/Births Attended by Skilled staff.csv", stringsAsFactors = FALSE)


file3<-subset(file3, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file3)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")
which(is.na(file3))
indx3<-which(is.na(file3), arr.ind = TRUE)
file3[indx3]<-rowMeans(file3[5:16], na.rm=TRUE)[indx3[,"row"]]

file3<-melt(file3)
file3<-subset(file3, select=c(0:2,5:6))
colnames(file3)[3:4]<-c("Year", "Births by skilled staff")
write.csv(file3, "Births Attended by Skilled staff.csv", row.names = FALSE)

#######################################################################################

file4<-read.csv("D:/Kdd/Project/Male Life expectancy.csv", stringsAsFactors = FALSE)


file4<-subset(file4, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file4)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file4))  
indx4<-which(is.na(file4), arr.ind = TRUE)
file4[indx4]<-rowMeans(file4[5:16], na.rm=TRUE)[indx4[,"row"]]

file4<-melt(file4)
file4<-subset(file4, select=c(0:2,5:6))
colnames(file4)[3:4]<-c("Year", "Male Life expectancy")
write.csv(file4, "Male Life expectancy.csv", row.names = FALSE)

#=============================================================================================

file5<-read.csv("D:/Kdd/Project/Female Life expectancy.csv", stringsAsFactors = FALSE)

file5<-subset(file5, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file5)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file5))  
indx5<-which(is.na(file5), arr.ind = TRUE)
file5[indx5]<-rowMeans(file5[5:16], na.rm=TRUE)[indx5[,"row"]]

file5<-melt(file5)
file5<-subset(file5, select=c(0:2,5:6))
colnames(file5)[3:4]<-c("Year", "Female Life expectancy")
write.csv(file5, "Female Life expectancy.csv", row.names = FALSE)

#==============================================================================================

file6<-read.csv("D:/Kdd/Project/Fertility Rate.csv", stringsAsFactors = FALSE)


file6<-subset(file6, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file6)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file6))  
indx6<-which(is.na(file6), arr.ind = TRUE)
file6[indx6]<-rowMeans(file6[5:16], na.rm=TRUE)[indx6[,"row"]]

file6<-melt(file6)
file6<-subset(file6, select=c(0:2,5:6))
colnames(file6)[3:4]<-c("Year", "Fertility Rate")
head(file6)
write.csv(file6, "Fertility Rate.csv", row.names = FALSE)

#=============================================================================================

file7<-read.csv("D:/Kdd/Project/Adolescent Fertility rate.csv", stringsAsFactors = FALSE)

file7<-subset(file7, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file7)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file7))  
indx7<-which(is.na(file7), arr.ind = TRUE)
file7[indx7]<-rowMeans(file7[5:16], na.rm=TRUE)[indx7[,"row"]]

file7<-melt(file7)
file7<-subset(file7, select=c(0:2,5:6))
colnames(file7)[3:4]<-c("Year", "Adolescent Fertility Rate")
write.csv(file7, "Adolescent Fertility Rate.csv", row.names = FALSE)

#==============================================================================================

file8<-read.csv("D:/Kdd/Project/Crude Death Rate.csv", stringsAsFactors = FALSE)

file8<-subset(file8, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file8)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file8))  
indx8<-which(is.na(file8), arr.ind = TRUE)
file8[indx8]<-rowMeans(file8[5:16], na.rm=TRUE)[indx8[,"row"]]

file8<-melt(file8)
file8<-subset(file8, select=c(0:2,5:6))
colnames(file8)[3:4]<-c("Year", "Crude Death Rate")
write.csv(file8, "Crude Death Rate.csv", row.names = FALSE)

#=============================================================================================

file9<-read.csv("D:/Kdd/Project/Crude Birth Rate.csv", stringsAsFactors = FALSE)
head(file9)

file9<-subset(file9, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file9)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file9))  
indx9<-which(is.na(file9), arr.ind = TRUE)
file9[indx9]<-rowMeans(file9[5:16], na.rm=TRUE)[indx9[,"row"]]

file9<-melt(file9)
file9<-subset(file9, select=c(0:2,5:6))
colnames(file9)[3:4]<-c("Year", "Crude Birth Rate")
write.csv(file9, "Crude Birth Rate.csv", row.names = FALSE)

#==============================================================================================

file10<-read.csv("D:/Kdd/Project/Infant Mortality Rate.csv", stringsAsFactors = FALSE)

file10<-subset(file10, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file10)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file10))  
indx10<-which(is.na(file10), arr.ind = TRUE)
file10[indx10]<-rowMeans(file10[5:16], na.rm=TRUE)[indx10[,"row"]]

file10<-melt(file10)
file10<-subset(file10, select=c(0:2,5:6))
colnames(file10)[3:4]<-c("Year", "Infant Mortality Rate")
write.csv(file10, "Infant Mortality Rate.csv", row.names = FALSE)

#=============================================================================================

file11<-read.csv("D:/Kdd/Project/Immunization.csv", stringsAsFactors = FALSE)

file11<-subset(file11, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file11)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file11))  
indx11<-which(is.na(file11), arr.ind = TRUE)
file11[indx11]<-rowMeans(file11[5:16], na.rm=TRUE)[indx11[,"row"]]

file11<-melt(file11)
file11<-subset(file11, select=c(0:2,5:6))
colnames(file11)[3:4]<-c("Year", "Immunization")
write.csv(file11, "Immunization.csv", row.names = FALSE)

#==============================================================================================

file12<-read.csv("D:/Kdd/Project/Incidence of TB.csv", stringsAsFactors = FALSE)


file12<-subset(file12, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file12)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file12))  
indx12<-which(is.na(file12), arr.ind = TRUE)
file12[indx12]<-rowMeans(file12[5:16], na.rm=TRUE)[indx12[,"row"]]

file12<-melt(file12)
file12<-subset(file12, select=c(0:2,5:6))
colnames(file12)[3:4]<-c("Year", "Incidence of TB")
write.csv(file12, "Incidence of TB.csv", row.names = FALSE)

#=============================================================================================

file13<-read.csv("D:/Kdd/Project/Improved sanitation facilities.csv", stringsAsFactors = FALSE)

file13<-subset(file13, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file13)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file13))  
indx13<-which(is.na(file13), arr.ind = TRUE)
file13[indx13]<-rowMeans(file13[5:16], na.rm=TRUE)[indx13[,"row"]]

file13<-melt(file13)
file13<-subset(file13, select=c(0:2,5:6))
colnames(file13)[3:4]<-c("Year", "Improved sanitation facilities")
write.csv(file13, "Improved sanitation facilities.csv", row.names = FALSE)

#==============================================================================================

file14<-read.csv("D:/Kdd/Project/Improved water source.csv", stringsAsFactors = FALSE)

file14<-subset(file14, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file14)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file14))  
indx14<-which(is.na(file14), arr.ind = TRUE)
file14[indx14]<-rowMeans(file14[5:16], na.rm=TRUE)[indx14[,"row"]]

file14<-melt(file14)
file14<-subset(file14, select=c(0:2,5:6))
colnames(file14)[3:4]<-c("Year", "Improved water source")
write.csv(file14, "Improved water source.csv", row.names = FALSE)

#=============================================================================================

file15<-read.csv("D:/Kdd/Project/Health Expenditure.csv", stringsAsFactors = FALSE)


file15<-subset(file15, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file15)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file15))  
indx15<-which(is.na(file15), arr.ind = TRUE)
file15[indx15]<-rowMeans(file15[5:16], na.rm=TRUE)[indx15[,"row"]]

file15<-melt(file15)
file15<-subset(file15, select=c(0:2,5:6))
colnames(file15)[3:4]<-c("Year", "Health Expenditure")
write.csv(file15, "Health Expenditure.csv", row.names = FALSE)

#==============================================================================================

file16<-read.csv("D:/Kdd/Project/Prevalence of Anemia.csv", stringsAsFactors = FALSE)

file16<-subset(file16, select=c(0:4, 48,49,50,51,52,53,54,55,56,57,58,59))

colnames(file16)[5:16]<-c("2003", "2004", "2005", "2006" , "2007", "2008", "2009", "2010","2011", "2012", "2013" ,"2014")

which(is.na(file16))  
indx16<-which(is.na(file16), arr.ind = TRUE)
file16[indx16]<-rowMeans(file16[5:16], na.rm=TRUE)[indx16[,"row"]]

file16<-melt(file16)
file16<-subset(file16, select=c(0:2,5:6))
colnames(file16)[3:4]<-c("Year", "Prevalence of Anemia")
write.csv(file16, "Prevalence of Anemia.csv", row.names = FALSE)

############################################################################################
multmerge=function(mypath)
{
  filenames=list.files(path=mypath, full.names = TRUE)
  datalist=lapply(filenames, function(x){read.csv(file=x, header=T, stringsAsFactors = FALSE)})
  Reduce(function(x,y){merge(x,y)}, datalist)
}

total=multmerge("D:/R directory/Data")

head(total)


###############################################################################################

total$Country.Name[total$Country.Name == 'Egypt, Arab Rep.'] = 'Egypt'
total$Country.Name[total$Country.Name == 'Congo,Dem. Rep.'] = 'Democratic Republic of the Congo'
total$Country.Name[total$Country.Name == 'Gambia, The'] = 'Gambia'
total$Country.Name[total$Country.Name == 'Macedonia, FYR'] = 'Macedonia'
total$Country.Name[total$Country.Name == 'Yemen, Rep.'] = 'Yemen'
total$Country.Name[total$Country.Name == 'Bahamas, The'] = 'Bahamas'
total$Country.Name[total$Country.Name == 'Venezuela, RB'] = 'Venezuela'
total$Country.Name[total$Country.Name == 'Korea, Rep.'] = 'South Korea'
total$Country.Name[total$Country.Name == 'Iran, Islamic Rep.'] = 'Iran'
total$Country.Name[total$Country.Name == 'Arab World'] = 'Palestine'
total$Country.Name[total$Country.Name == 'Caribbean small states'] = 'Anguilla'
total$Country.Name[total$Country.Name == 'Central Europe and the Baltics'] = 'Slovakia'
total$Country.Name[total$Country.Name == 'East Asia & Pacific (all income levels)'] = 'Korea, Dem.'
total$Country.Name[total$Country.Name == 'East Asia & Pacific (developing only)'] = 'Taiwan'
total$Country.Name[total$Country.Name == 'Fragile and conflict affected situations'] = 'Guinea Bassau'

total1<-subset(total, !(total$Country.Name =="Euro area" | total$Country.Name =="European Union" | 
                          total$Country.Name == "Europe & Central Asia (all income levels)" | total$Country.Name=="Europe & Central Asia (developing only)" | 
                          total$Country.Name=="Heavily indebted poor countries (HIPC)" | 
                          total$Country.Name == "High income" | total$Country.Name=="Latin America & Caribbean (all income levels)" | 
                          total$Country.Name=="Middle East & North Africa (all income levels)" | 
                          total$Country.Name=="Middle East & North Africa (developing only)" | total$Country.Name=="North America" | 
                          total$Country.Name=="Pacific island small states" | total$Country.Name=="Sub-Saharan Africa (all income levels)" | 
                          total$Country.Name=="Sub-Saharan Africa (developing only)" |
                          total$Country.Name=="High income: nonOECD" | total$Country.Name=="High income: OECD" |
                          total$Country.Name=="Latin America & Caribbean (developing only)" | total$Country.Name == "Least developed countries: UN classification"|
                          total$Country.Name=="Lower middle income" | total$Country.Name=="Low income" |
                          total$Country.Name=="Low & middle income" | total$Country.Name=="Middle income" |
                          total$Country.Name=="Not classified" | total$Country.Name=="OECD members" |
                          total$Country.Name=="Other small states" | total$Country.Name=="Small states" |
                          total$Country.Name=="South Asia" | total$Country.Name=="Upper middle income" | 
                          total$Country.Name=="World"))

write.csv(total1, "Analysis Dataset.csv", row.names = FALSE)

##############################################################################################


total<-total1[rowSums(is.na(total1))<4, ]
df1<-total[!is.na(total$Health.Expenditure),]



df1$HA[df1$Health.Expenditure<=5]<-"Low"
df1$HA[df1$Health.Expenditure>5 & df1$Health.Expenditure<=8]<-"Medium"
df1$HA[df1$Health.Expenditure>8 & df1$Health.Expenditure<=10]<-"High"
df1$HA[df1$Health.Expenditure>10]<-"Very High"

df1$HA<-factor(df1$HA, labels=c("Low", "Medium", "High", "Very High"))

write.csv(df1, "Analysis Dataset.csv", row.names = FALSE)

################################################################################################

#to check correlation, we remove NA values first.

df2<-subset(df1, select=c(3:20))
df3<-knnImputation(df2, k=5)

df4<-subset(df1, select=c(0:2))
df5<-cbind(df4, df3)

write.csv(df5, "Analysis Dataset.csv", row.names = FALSE)
df5<-read.csv("D:/R directory/Analysis Dataset2.csv", stringsAsFactors = FALSE)

##########################################################################################################

#Outlier handling:

boxplot(df5$Adolescent.Fertility.Rate)
boxplot(df5$Fertility.Rate)
boxplot(df5$Crude.Birth.Rate)
boxplot(df5$Crude.Death.Rate)
boxplot(df5$Female.Life.expectancy)
boxplot(df5$Male.Life.expectancy)
boxplot(df5$Improved.sanitation.facilities)
boxplot(df5$Improved.water.source)
boxplot(df5$Incidence.of.TB)
boxplot(df5$Health.Expenditure)
boxplot(df5$Urban.Population)
boxplot(df5$Total.Population)
boxplot(df5$Infant.Mortality.Rate)
boxplot(df5$Births.by.skilled.staff)
boxplot(df5$Prevalence.of.Anemia)
boxplot(df5$Immunization)

df5$Adolescent.Fertility.Rate[df5$Adolescent.Fertility.Rate>=173]<-NA
df5$Fertility.Rate[df5$Fertility.Rate>=8]<-NA
df5$Crude.Death.Rate[df5$Crude.Death.Rate>=16]<-NA
df5$Female.Life.expectancy[df5$Female.Life.expectancy<=45]<-NA
df5$Male.Life.expectancy[df5$Male.Life.expectancy<=43]<-NA
df5$Improved.water.source[df5$Improved.water.source<=52]<-NA
df5$Incidence.of.TB[df5$Incidence.of.TB>=470]<-NA
df5$Health.Expenditure[df5$Health.Expenditure>=14]<-NA
df5$Infant.Mortality.Rate[df5$Infant.Mortality.Rate>=105]<-NA
df5$Births.by.skilled.staff[df5$Births.by.skilled.staff<=20]<-NA
df5$Immunization[df5$Immunization<=60]<-NA

df2<-subset(df5, select=c(3:20))
df3<-knnImputation(df2, k=5)

df4<-subset(df1, select=c(0:2))
df5<-cbind(df4, df3)

#########################################################################################################
#Clustering

df<-read.csv("D:/R directory/Analysis Dataset.csv", stringsAsFactors = FALSE)
# Summarise data
summary(df)

# Subset the attitude data
dat = df[,c(5,15)]

# Plot subset data
plot(dat, main = "% of favourable responses to births by skilled staff and infant mortality rate", pch =20, cex =2)

# Perform K-Means with 2 clusters
set.seed(7)
km1 = kmeans(dat, 2, nstart=100)

# Plot results
plot(dat, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)

# Check for the optimal number of clusters given the data

mydata <- dat
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

# Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(7)
km2 = kmeans(dat, 6, nstart=100)

# Examine the result of the clustering algorithm
km2

# Plot results
plot(dat, col =(km2$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)

km3 = kmeans(dat, 8, nstart=100)
plot(dat, col =(km3$cluster +1) , main="K-Means result with 8 clusters", pch=20, cex=2)

##########################################################################################################
sub <- sample(nrow(df5), floor(nrow(df5) * 0.9))
training2 <- df5[sub, ]
testing2 <- df5[-sub, ]
testing3<-df5[-sub,]
testing<-subset(testing2, select=c(0:9 , 11:19))
training<-subset(training2, select=c(0:9, 11:20))
head(testing1)


########################################################################################################

fit2<-randomForest(as.factor(HA)~Year+ Adolescent.Fertility.Rate + Births.by.skilled.staff + Crude.Birth.Rate + Crude.Death.Rate + Female.Life.expectancy + Fertility.Rate + Immunization + Improved.sanitation.facilities + Improved.water.source + Incidence.of.TB + Infant.Mortality.Rate + Male.Life.expectancy + Prevalence.of.Anemia + Total.Population + Urban.Population, data=training2, importance=TRUE, ntree=4000)
prediction<-predict(fit2, newdata = testing)
result<-cbind(testing, prediction)
head(result)

test_error<-sum(prediction!=testing3$HA)
test_error/nrow(testing3)

#######################################################################################################


write.csv(result, "Predicted.csv", row.names = FALSE)

####################################################################################################


