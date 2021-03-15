credit.dataset <- read.csv("swiss_credit_dataset.csv", header=TRUE, sep = ",")

class(credit.dataset)

str(credit.dataset)

sum(is.na(credit.dataset))

# metodă pentru transformarea datelor
to.factors <- function(dataset, variables){
  for (variable in variables){
    dataset[[variable]] <- as.factor(dataset[[variable]])
  }
  return(dataset)
}

# selectarea datelor pe care vrem să le transformăm
categorical.vars <- c('credit.rating', 'account.balance', 'previous.credit.payment.status',
                      'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 'residence.duration', 'current.assets',
                      'other.credits', 'apartment.type', 'bank.credits', 'occupation',
                      'dependents', 'telephone', 'foreign.worker')

# transformarea datelor
credit.dataset <- to.factors(dataset=credit.dataset,variables = categorical.vars)

str(credit.dataset)

# install.packages(c("pastecs","gmodels","gridExtra","ggplot2", "car"))

library(pastecs)
library(gmodels)
library(gridExtra)
library(ggplot2)

# analizarea variabilelor numerice
get.numeric.variable.stats <- function(indep.var, detailed=FALSE){
  options(scipen=100)
  options(digits=2)
  if (detailed){
    var.stats <- stat.desc(indep.var)
  }else{
    var.stats <- summary(indep.var)
  }
  
  dataset <- data.frame(round(as.numeric(var.stats),2))
  colnames(dataset) <- deparse(substitute(indep.var))
  rownames(dataset) <- names(var.stats)
  
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  grid.table(t(dataset))
}

# vizualizare
# histograms\density
visualize.distribution <- function(indep.var){
  pl1 <- qplot(indep.var, geom="histogram",
               fill=I('gray'), binwidth=5,
               col=I('black'))+ theme_bw()
  pl2 <- qplot(indep.var, geom="density",
               fill=I('gray'), binwidth=5,
               col=I('black'))+ theme_bw()
  
  grid.arrange(pl1,pl2, ncol=2)
}

# box plots
visualize.boxplot <- function(indep.var, dep.var){
  pl1 <- qplot(factor(0),indep.var, geom="boxplot",
               xlab = deparse(substitute(indep.var)),
               ylab="values") + theme_bw()
  pl2 <- qplot(dep.var,indep.var,geom="boxplot",
               xlab = deparse(substitute(dep.var)),
               ylab = deparse(substitute(indep.var))) + theme_bw()
  
  grid.arrange(pl1,pl2, ncol=2)
}

# analizarea variabilelor categorice
get.categorical.variable.stats <- function(indep.var){
  
  feature.name = deparse(substitute(indep.var))
  dataframe1 <- data.frame(table(indep.var))
  colnames(dataframe1) <- c(feature.name, "Frequency")
  dataframe2 <- data.frame(prop.table(table(indep.var)))
  colnames(dataframe2) <- c(feature.name, "Proportion")
  
  dataset <- merge(
    dataframe1, dataframe2, by = feature.name
  )
  ndf <- dataset[order(-dataset$Frequency),]
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  grid.table(ndf)
}

# generare tabel contingent
get.contingency.table <- function(dep.var, indep.var,
                                  stat.tests=F){
  if(stat.tests == F){
    CrossTable(dep.var, indep.var, digits=1,
               prop.r=F, prop.t=F, prop.chisq=F)
  }else{
    CrossTable(dep.var, indep.var, digits=1,
               prop.r=F, prop.t=F, prop.chisq=F,
               chisq=T, fisher=T)
  }
}

# vizualizare
# barcharts
visualize.barchart <- function(indep.var){
  qplot(indep.var, geom="bar",
        fill=I('gray'), col=I('black'),
        xlab = deparse(substitute(indep.var))) + theme_bw()
}


# mosaic plots
visualize.contingency.table <- function(dep.var, indep.var){
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  mosaicplot(dep.var ~ indep.var, color=T,
             main = "Contingency table plot")
}

## Analizarea dataset-ului

attach(credit.dataset)

get.categorical.variable.stats(credit.rating)

visualize.barchart(credit.rating)

get.categorical.variable.stats(account.balance)

visualize.barchart(account.balance)

library(car)

new.account.balance<- recode(account.balance,"1=1;2=2;3=3;4=3")

credit.dataset$account.balance<- new.account.balance

get.contingency.table(credit.rating,new.account.balance,
                      stat.tests = T)

visualize.contingency.table(credit.rating,new.account.balance)

get.numeric.variable.stats(credit.duration.months)

visualize.distribution(credit.duration.months)

visualize.boxplot(credit.duration.months,credit.rating)

get.categorical.variable.stats(previous.credit.payment.status)

visualize.barchart(previous.credit.payment.status)


new.previous.credit.payment.status<- recode(previous.credit.payment.status,
                                            "0=1;1=1;2=2;3=3;4=3")
credit.dataset$previous.credit.payment.status<-
  new.previous.credit.payment.status

get.contingency.table(credit.rating,new.previous.credit.payment.status)

get.categorical.variable.stats(credit.purpose)

visualize.barchart(credit.purpose)

new.credit.purpose <- recode(credit.purpose,
                             "0=4;1=1;2=2;3=3;4=3;5=3;6=3;7=4;8=4;9=4;10=4")

credit.dataset$credit.purpose<- new.credit.purpose

get.contingency.table(credit.rating,new.credit.purpose)

get.numeric.variable.stats(credit.amount)

visualize.distribution(credit.amount)

visualize.boxplot(credit.amount,credit.rating)

new.savings<- recode(savings,"1=1;2=2;3=3;4=3;5=4")

credit.dataset$savings<-new.savings

get.contingency.table(credit.rating,new.savings)

# feature: employment.duration - rescrie clase si updateaza data frame-ul
new.employment.duration <- recode(employment.duration,
                                  "1=1;2=1;3=2;4=3;5=4")
credit.dataset$employment.duration <- new.employment.duration

# tabel contingent
get.contingency.table(credit.rating, new.employment.duration)


get.contingency.table(credit.rating,installment.rate,stat.tests = TRUE)

# feature: marital.status - rescrie clase si updateaza data frame-ul
new.marital.status <- recode(marital.status, "1=1;2=1;3=3;4=4")
credit.dataset$marital.status <- new.marital.status

# contingency table
get.contingency.table(credit.rating, new.marital.status)


# feature: guarantor - rescrie clase si updateaza data frame-ul
new.guarantor <- recode(guarantor, "1=1;2=2;3=2")
credit.dataset$guarantor <- new.guarantor

# efectuează testele statistice
fisher.test(credit.rating, new.guarantor)
chisq.test(credit.rating, new.guarantor)

# efectuează testele statistice pentru residence.duration
fisher.test(credit.rating, residence.duration)
chisq.test(credit.rating, residence.duration)

# efectuează testele statistice pentru current.assets
fisher.test(credit.rating, current.assets)
chisq.test(credit.rating, current.assets)

get.numeric.variable.stats(age)

visualize.distribution(age)

visualize.boxplot(age,credit.rating)

# feature: other.credits - rescrie clase si updateaza data frame-ul
new.other.credits <- recode(other.credits, "1=1;2=1;3=2")
credit.dataset$other.credits <- new.other.credits

# efectuează testele statistice
fisher.test(credit.rating, new.other.credits)
chisq.test(credit.rating, new.other.credits)

# efectuează testele statistice pentru apartment.type
fisher.test(credit.rating, apartment.type)
chisq.test(credit.rating, apartment.type)

# feature: bank.credits - rescrie clase si updateaza data frame-ul
new.bank.credits <- recode(bank.credits, "1=1;2=2;3=2;4=2")
credit.dataset$bank.credits <- new.bank.credits

# efectuează testele statistice
fisher.test(credit.rating, new.bank.credits)
chisq.test(credit.rating, new.bank.credits)

# efectuează testele statistice pentru occupation
fisher.test(credit.rating, occupation)
chisq.test(credit.rating, occupation)

# efectuează testele statistice pentru dependents
fisher.test(credit.rating, dependents)
chisq.test(credit.rating, dependents)

# efectuează testele statistice pentru telephone
fisher.test(credit.rating, telephone)
chisq.test(credit.rating, telephone)


# efectuează testele statistice pentru foreign.worker
fisher.test(credit.rating, foreign.worker)
chisq.test(credit.rating, foreign.worker)

write.csv(file='swiss_credit_dataset_final.csv',x=credit.dataset,row.names = F)
