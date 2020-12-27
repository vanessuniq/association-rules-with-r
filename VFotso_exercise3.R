# UMGC DATA 630
# Exericse 3
# Written by Vanessa Fotso

#arules packge is already installed
# install the TSP, data.table and aruleViz packages
install.packages("TSP")
install.packages("data.table")
install.packages("arulesViz")


# load packages in memory
library("arules")
library("arulesViz")



# load the data and save into the credit data frame variable
credit <- read.csv(file="CreditApproval1.csv", head=TRUE, sep = ",", as.is = FALSE)

# preview the data frame credit
head(credit)

# Display the data structure
str(credit)

# Data Preprocessing

# remove the irrelevant key values
credit$Key <- NULL
# Data preview after dropping the key column
head(credit)

# running k-means discretization method on age, debt, yearsemployed, creditscore, and income variables
credit$Age <- discretize(credit$Age, method = "cluster", breaks = 6)
summary(credit$Age)

credit$Debt <- discretize(credit$Debt, method = "cluster", breaks = 6)
summary(credit$Debt)

credit$YearsEmployed <- discretize(credit$YearsEmployed, method = "cluster", breaks = 4)
summary(credit$YearsEmployed)

credit$CreditScore <- discretize(credit$CreditScore, method = "cluster", breaks = 4)
summary(credit$CreditScore)

credit$Income <- discretize(credit$Income, method = "cluster", breaks = 6)
summary(credit$Income)

# running Factor function on zipcode variable
credit$Zipcode <- factor(credit$Zipcode)
summary(credit$Zipcode)

# Run the method with default parameters and save it into the variable rules
rules <- apriori(credit)
rules
# inspect the first 10 rules
inspect(rules[1:10])

#  run the method with 2 different combinations of confidence, support and min length values

# first run:
rules <- apriori(credit, parameter= list(supp=0.4, conf=0.7, minlen=2))
rules
inspect(rules[1:10])

#second run:
rules <- apriori(credit, parameter= list(supp=0.5, conf=0.8, minlen=2))
rules
inspect(rules[1:10])
summary(rules)

# rules with "+" or "-" on the right hand side
rules<-apriori(credit, parameter= list(supp=0.2, conf=0.8, minlen=2), appearance=list(rhs=c("class=-", "class=+"), default="lhs"))
rules
inspect(rules[1:10])


# Pruning rules
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

#Remove the redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

# visualize the rules
plot(rules.pruned, method="paracoord", control = list(reorder = TRUE))

# End Script