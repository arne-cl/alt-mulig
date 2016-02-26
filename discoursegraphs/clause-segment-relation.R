# TODO: have a look at this website, esp. "Conditional Inference Tree"
# http://www.statmethods.net/advstats/cart.html

library(rpart)
data("kyphosis")
fit <- rpart(Kyphosis ~ Age + Number + Start,
             method="class", data=kyphosis)
printcp(fit)
plotcp(fit)
summary(fit)
plot(fit)
text(fit, use.n=TRUE, all=TRUE, cex=.8)

library(randomForest)
fit <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fit) # view results
importance(fit) # importance of each predictor 

clause_table <- read.table("~/repos/alt-mulig/discoursegraphs/clause-segment-relation.csv", sep=',', header = T, row.names = 1)
class(clause_table$segment)
clause_fit <- rpart(clause ~ segment + relation, data=clause_table)
plot(clause_fit)
text(clause_fit, all=T, cex=1, use.n=T)

fit2 <-rpart(relation ~ segment + clause, data=clause_table)
pdf("~/repos/alt-mulig/discoursegraphs/fit.pdf")
plot(fit2)
text(fit2,all=T, cex=1, fancy=T)
dev.off()

rf <- randomForest(clause ~ segment + relation, data=clause_table)
print(rf)

rf2 <-randomForest(relation ~ segment + clause, data=clause_table)
print(rf2)
