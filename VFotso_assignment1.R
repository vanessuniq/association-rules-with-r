# UMGC DATA 630
# Assignment 1
# Written by Vanessa Fotso

#load libraries
library("arules")
library("arulesViz")

#Load the Student Alcohol consumption data
alcohol<-read.csv(file="student-mat.csv", head=TRUE, sep=";", as.is = FALSE)

#run the Str and summary commands to acquaint ourself with the data
str(alcohol)
summary(alcohol)

#graph and review daily alcohol consumption

hist(alcohol$Dalc)

#Discretize numerical variables and add labels for supervised learning
alcohol$age<-factor(alcohol$age)
alcohol$Medu <-factor(alcohol$Medu)
alcohol$Fedu <-factor(alcohol$Fedu)
alcohol$traveltime <-factor(alcohol$traveltime)
alcohol$studytime <-factor(alcohol$studytime, labels = c("<2hrs", "2 to 5 hrs","5 to 10 hrs","over 10 hrs"))
alcohol$famrel <-factor(alcohol$famrel, labels = c("very bad", "bad","fair","good","very good"))
alcohol$failures <-factor(alcohol$failures)
alcohol$freetime <-factor(alcohol$freetime, labels = c("very low", "low","medium","high","very high"))
alcohol$goout <-factor(alcohol$goout, labels = c("very low", "low","medium","high","very high"))
alcohol$Dalc <-factor(alcohol$Dalc, labels = c("very low", "low","medium","high","very high"))
alcohol$Walc <-factor(alcohol$Walc, labels = c("very low", "low","medium","high","very high"))
alcohol$health <-factor(alcohol$health, labels = c("very bad", "bad","fair","good","very good"))
alcohol$G1<-discretize(alcohol$G1, method="interval", breaks=5)
alcohol$G2<-discretize(alcohol$G2, method="interval", breaks=5)
alcohol$G3<-discretize(alcohol$G3, method="interval", breaks=5)
alcohol$absences<-discretize(alcohol$absences, method="fixed", breaks=c(0, 5, 10, 15, 75))

# Structure and summary of the discretized data
str(alcohol)
summary(alcohol)


#Run apriori method rules to get rules for low daily alcohol use 
alclow<-apriori(alcohol, parameter= list(supp=0.35, conf=0.8, minlen=2), appearance=list(rhs=c("Dalc=very low", "Dalc=low"), default="lhs"))
alclow

#remove the redundant rules and display the remaining rules
rules.sorted <- sort(alclow, by="lift")
inspect(rules.sorted)
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
summary(rules.pruned)

# preview the top 5 rules by lift 
inspect(head(sort(rules.pruned, by="lift")),n=5)

# reduce the number of rules by changing the min-supp to 0.3
alclow<-apriori(alcohol, parameter= list(supp=0.35, conf=0.8, minlen=2), appearance=list(rhs=c("Dalc=very low", "Dalc=low"), default="lhs"))
alclow

#Graph the data
plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))


#Run apriori method rules to get rules for high daily alcohol use 
alchigh<-apriori(alcohol, parameter= list(supp=0.05, conf=0.1, minlen=2), appearance=list(rhs=c("Dalc=very high", "Dalc=high","Dalc=medium"), default="lhs"))
alchigh


# view the 3 generated rules by lift 
inspect(head(sort(rules.pruned1, by="lift")))



#Graph the data
plot(rules.pruned1, method="paracoord", control=list(reorder=TRUE))

# End Script
