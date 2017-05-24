#Market Baseket Analysis
library(arules)
library(arulesViz)
getwd()

#convert data to transaction format


trans = read.transactions("C:/Users/User/Documents/R-Programming_Project/R_Market_Basket/Online Retail_RV.csv", format = "single", sep = ",", cols = c("InvoiceNo", "Description"), rm.duplicates = TRUE)

inspect(trans)
shopping_rule <- apriori(trans)
shopping_rule<-apriori(trans,parameter = list(supp= .01, conf = .8 ))
inspect(shopping_rule[1:20])

shopping_rule<-apriori(trans,parameter = list(supp= .01, conf = .95 ))
inspect(shopping_rule[1:20])

#Remove Redundant Rules
#See which rules is duplicated

Check_Dup<-is.redundant((trans,parameter = list(supp= .01, conf = .95 )))

summary(Check_Dup)

#Remove duplicate Rule
shopping_rule <- shopping_rule[!Check_Dup]

shopping_rule

#Sort rules
inspect(shopping_rule)
shopping_rule<- sort(shopping_rule, by = "lift")
inspect(shopping_rule [1:20])

#Discover a specific product
#Eg.TOAST ITS - FAIRY FLOWER
Fairy_rule <- apriori(trans,parameter = list(supp= .00001, conf = .70 ) , appearance = list(default = "rhs", lhs = "TOAST ITS - FAIRY FLOWER")) #right hand side of rule will be default and left side is our product
# reduce the support level and conf. level if there is no rules 

inspect(Fairy_rule)

Fairy_dup <- is.redundant(Fairy_rule)
Fairy_dup

#plotting the fairy rules
plot(Fairy_rule , method = "graph")

#plotting shopping rules
plot(shopping_rule[1:20], method = "graph" , measure = "support", shading = "lift", interactive = TRUE)

