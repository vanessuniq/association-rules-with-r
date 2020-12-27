# 1. Install and load backages
#install arules package only once
install.packages("arules")
library("arules")
#install arulesViz package only once
install.packages("TSP")
install.packages("data.table")
install.packages("arulesViz")
library("arulesViz")

# 2. Read the CSV file.  
# Change the file path to the file location on your hard drive.
setwd("F:/Datasets")
bank <- read.csv("Bank.csv", head =TRUE, sep=",", as.is=FALSE)
head(bank)
str(bank)

# 3. Preprocessing
#Remove id variable
bank$id<-NULL
head(bank)

#discretization
bank$age<-discretize(bank$age, method="frequency", breaks=6)
bank$income<-discretize(bank$income, method="frequency", breaks=6)
#factor
bank$children<-factor(bank$children)
summary(bank$children)

# 4.Run the method with default parameters
rules<-apriori(bank)
rules
inspect(rules)
inspect(rules[1:10])
inspect(rules[10:20])
# 5. different suport and confidence values
rules <- apriori(bank, parameter= list(supp=0.4, conf=0.7))
inspect(rules)
rules <- apriori(bank, parameter= list(supp=0.4, conf=0.7, minlen=2))
#6. display other measures
interestMeasure(rules, c("support", "chiSquare", "confidence", "conviction", "cosine", "coverage", "leverage", "lift", "oddsRatio"), bank)
rules<-apriori(bank, parameter= list(supp=0.1, conf=0.8, minlen=2), appearance=list(rhs=c("pep=NO", "pep=YES"), default="lhs"))
inspect(rules)

# 7. Prunning rules
#Sort the rules by lift
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
#Remove the redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1
subset.matrix
redundant
which(redundant)
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
summary(rules.pruned)

# 8. Plot the rules
plot(rules.pruned)
plot(rules.pruned, method="graph", control=list(type="items"))
plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))
plot(rules.pruned, method = "grouped")
plot(rules.pruned, method="matrix", measure=c("lift", "confidence"))
