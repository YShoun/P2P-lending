# # # # # # # # # # # # # #
# PHO Yong Shoun - CA660) #
# # # # # # # # # # # # # #
library(dplyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(e1071)
library(gtable)


stat_original <- read.csv("Finaldataset_Lending_club.csv", sep= ",", header = TRUE)
stat_data <- stat_original
#### Exploratory Data ANalysis ####
# Goal during EDA is to develop an understanding of your data. 
dim(stat_data)
names(stat_data)
str(stat_data)
summary(stat_data)

str(stat_data)
levels(stat_data$term)
levels(stat_data$grade)
levels(stat_data$home_ownership)
levels(stat_data$verification_status)
levels(stat_data$loan_status)
levels(stat_data$purpose)

freq_grade<- as.data.frame(round(prop.table(table(stat_data$grade))*100, digits = 2))
freq_grade.barplot <- ggplot(data=freq_grade, aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), vjust=-0.5, color="black", size=3.5)+
  ggtitle("Grade frequency")+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
freq_grade.barplot

ggplot(stat_data, aes(x=int_rate,fill=grade))+
  geom_density(alpha = 0.3) + 
  theme(legend.position="none") +
  xlab("Interest rate") + 
  ggtitle("Interest rate by Borrower's Grade") + 
  facet_grid(grade ~ ., scales = "free")

# Default prop in each grade
stat_data.default <- stat_data[stat_data$loan_status == "Default" | stat_data$loan_status == "Charged Off"| stat_data$loan_status == "Late (16-30 days)" | stat_data$loan_status == "Late (31-120 days)",]

a = table(stat_data.default$grade)# /dim(stat_data.default)[1]
b = table(stat_data$grade)# /dim(stat_data)[1]

a/b*100

plot_grad <- function(grade) {
  loan_data_tmp <- stat_data[stat_data$grade == grade,]
  
  ## Aggregating up total loans by purpose ##
  freq_purpose.grade <- as.data.frame(round(prop.table(table(loan_data_tmp$purpose))*100, digits = 2))
  freq_purpose.barplot.grade <- ggplot(data=freq_purpose.grade, aes(x=reorder(Var1, -Freq), y=Freq)) +
    geom_bar(stat="identity", fill="steelblue")+
    theme(axis.text.x = element_text(angle=30, vjust=0.6))+
    geom_text(aes(label=Freq), vjust=-0.5, color="black", size=3.5)+
    ggtitle("Purpose frequency")+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  ## Aggregating up by loan amount ##
  loan_amnt_tmp <- loan_data_tmp$loan_amnt
  loan_amnt_hist <- qplot(loan_amnt_tmp, fill = I("dodgerblue4"), 
                          alpha = I(0.7), col = I("grey29")) + xlab("Loan Amount") + ylab("Count") + 
    geom_vline(aes(xintercept = mean(loan_amnt_tmp)), 
               color = "dodgerblue4", 
               linetype = "dashed", 
               size = 2) +
    annotate("text", x = Inf, y = Inf, 
             label = sprintf("\n Mean: %s  \n Average Deviation: %s   \n Skewness: %s   \n Kurtosis: %s   ",
                             round(mean(loan_amnt_tmp)),
                             round(mean(abs(loan_amnt_tmp-mean(loan_amnt_tmp)))),
                             round(skewness(loan_amnt_tmp),2),
                             round(kurtosis(loan_amnt_tmp),2)), 
             vjust = 1, hjust = 1)
  
  plot_grid(loan_amnt_hist, freq_purpose.barplot.grade,
            hjust = -0.5, vjust = 1.5, scale = 1,
            ncol = 2, nrow = 1)
}

plot_grad("A")
plot_grad("B")
plot_grad("C")
plot_grad("D")
plot_grad("E")
plot_grad("G")



#### Pearson Coefficient ####
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library("ggpubr")

# eRemove NA in columns:
stat_data <- na.omit(stat_data)

#categorical into numerical : sub-grade
sub_grade <- as.character(stat_data$sub_grade)
sub_grade <- as.numeric(stat_data$sub_grade)

Pearson.subgrade <- data.frame(sub_grade,
                 stat_data$int_rate,
                 stat_data$loan_amnt,
                 stat_data$annual_inc,
                 stat_data$emp_length,
                 stat_data$inq_last_6mths,
                 stat_data$revol_util,
                 stat_data$open_acc,
                 stat_data$mths_since_last_delinq,
                 stat_data$dti,
                 stat_data$installment)
Pearson.subgrade.pearson <- cor(as.matrix(Pearson.subgrade), method = "pearson")
Pearson.subgrade.spearman <- cor(as.matrix(Pearson.subgrade), method = "spearman")


# grade correlation:
grade <- as.character(stat_data$grade)
grade <- as.numeric(stat_data$grade)

Pearson.grade <- data.frame(grade,
                            stat_data$int_rate,
                            stat_data$loan_amnt,
                            stat_data$annual_inc,
                            stat_data$emp_length,
                            stat_data$inq_last_6mths,
                            stat_data$revol_util,
                            stat_data$open_acc,
                            stat_data$mths_since_last_delinq,
                            stat_data$dti,
                            stat_data$installment)

Pearson.grade.pearson <- cor(as.matrix(Pearson.grade), method = "pearson")
Pearson.grade.spearman <- cor(as.matrix(Pearson.grade), method = "spearman")


#### Biserial Correlation ####
library(data.table)

setDT(stat_data)[, c(levels(stat_data$home_ownership), "home_ownership") := 
            c(lapply(levels(home_ownership), function(x) as.integer(x == home_ownership)), .(NULL))]

setDT(stat_data)[, c(levels(stat_data$purpose), "purpose") := 
                                       c(lapply(levels(purpose), function(x) as.integer(x == purpose)), .(NULL))]
stat_data <- na.omit(stat_data)

Binary.pearson.subgrade <- data.frame(sub_grade,
                                      stat_data$MORTGAGE,
                                      stat_data$OWN,
                                      stat_data$RENT,
                                      stat_data$credit_card,
                                      stat_data$debt_consolidation)

Binary.pearson.subgrade.person <- cor(as.matrix(Binary.pearson.subgrade), method = "pearson")



#### CHI square test preparation ####
library(MASS)
stat_data.chi <- stat_original

# creating new level
levels(stat_data.chi$loan_status) <- c(levels(stat_data.chi$loan_status), "bad")
levels(stat_data.chi$loan_status) <- c(levels(stat_data.chi$loan_status), "good") 

#replacing level
stat_data.chi$loan_status[stat_data.chi$loan_status == "Late (31-120 days)"] <- "bad"
stat_data.chi$loan_status[stat_data.chi$loan_status == "Late (16-30 days)"] <- "bad"
stat_data.chi$loan_status[stat_data.chi$loan_status == "Default"] <- "bad"
stat_data.chi$loan_status[stat_data.chi$loan_status == "Charged Off"] <- "bad"
stat_data.chi$loan_status[stat_data.chi$loan_status == "In Grace Period"] <- "good"
stat_data.chi$loan_status[stat_data.chi$loan_status == "Fully Paid"] <- "good"

table(stat_data.chi$loan_status)

# updateing level
stat_data.chi$loan_status <- factor(stat_data.chi$loan_status)  
levels(stat_data.chi$loan_status)


## grade 
chisquare <- function(grade1) {
  # separating grade 
  grade_temp1 <- stat_data.chi[stat_data.chi$grade == grade1,]
  # update grade factor
  grade_temp1$grade <- factor(grade_temp1$grade)  
  levels(grade_temp1$grade)
  #get contigency table
  h_cont <- table(grade_temp1$grade,grade_temp1$loan_status)
  #print percentage
  print(round(prop.table(h_cont)*100,2))
  chisq.test(h_cont)
}

chisquare("A")
chisquare("B")
chisquare("C")
chisquare("D")
chisquare("E")
chisquare("F")
## P-value ≤ α: The variables have a statistically significant association (Reject H0) 
####If the p-value is less than or equal to the significance level, you reject the null hypothesis and conclude that there is a statistically significant association between the variables. #

## P-value > α: Cannot conclude that the variables are associated (Fail to reject H0) 
####If the p-value is larger than the significance level, you fail to reject the null hypothesis because there is not enough evidence to conclude that the variables are associated. 

## purpose 
chisquare2 <- function(purpose1) {
  # separating purpose 
  purpose1_temp1 <- stat_data.chi[stat_data.chi$purpose == purpose1,]
  # update purpose factor
  purpose1_temp1$purpose <- factor(purpose1_temp1$purpose)  
  levels(purpose1_temp1$purpose)
  #get contigency table
  h_cont <- table(purpose1_temp1$purpose,purpose1_temp1$loan_status)
  #print percentage
  print(round(prop.table(h_cont)*100,2))
  chisq.test(h_cont)
}

chisquare2("car")
# separating purpose 
purpose1_temp1 <- stat_data.chi[stat_data.chi$purpose == "car",]
# update purpose factor
purpose1_temp1$purpose <- factor(purpose1_temp1$purpose)  
levels(purpose1_temp1$purpose)
#get contigency table
h_cont <- table(purpose1_temp1$purpose,purpose1_temp1$loan_status)
#print percentage
print(round(prop.table(h_cont)*100,2))
chisq.test(h_cont)

chisquare2("credit_card")
chisquare2("debt_consolidation")
chisquare2("home_improvement")
chisquare2("house")
chisquare2("major_purchase")
chisquare2("medical")
chisquare2("moving")
chisquare2("vacation")
chisquare2("wedding")
chisquare2("renewable_energy")
chisquare2("small_business")
chisquare2("other")

#### Mean & SD ####
stat_data.t <- stat_original

# creating new level
levels(stat_data.t$loan_status) <- c(levels(stat_data.t$loan_status), "bad")
levels(stat_data.t$loan_status) <- c(levels(stat_data.t$loan_status), "good") 

#replacing level
stat_data.t$loan_status[stat_data.t$loan_status == "Late (31-120 days)"] <- "bad"
stat_data.t$loan_status[stat_data.t$loan_status == "Late (16-30 days)"] <- "bad"
stat_data.t$loan_status[stat_data.t$loan_status == "Default"] <- "bad"
stat_data.t$loan_status[stat_data.t$loan_status == "Charged Off"] <- "bad"
stat_data.t$loan_status[stat_data.t$loan_status == "In Grace Period"] <- "good"
stat_data.t$loan_status[stat_data.t$loan_status == "Fully Paid"] <- "good"

table(stat_data.t$loan_status)

# updateing level
stat_data.t$loan_status <- factor(stat_data.t$loan_status)  
levels(stat_data.t$loan_status)


#separate good and bad borrowers
stat_data.t.bad <- stat_data.t[stat_data.t$loan_status == "bad",]
stat_data.t.good <- stat_data.t[stat_data.t$loan_status == "good",]

# Mean and sd
c(mean(stat_data.t$int_rate),sd(stat_data.t$int_rate),mean(stat_data.t.bad$int_rate),sd(stat_data.t.bad$int_rate), mean(stat_data.t.good$int_rate),sd(stat_data.t.good$int_rate))
#t test
t.test(stat_data.t$int_rate ~ stat_data.t$loan_status, var.equal=TRUE)

# Mean and sd
c(mean(stat_data.t$loan_amnt),sd(stat_data.t$loan_amnt),mean(stat_data.t.bad$loan_amnt),sd(stat_data.t.bad$loan_amnt), mean(stat_data.t.good$loan_amnt),sd(stat_data.t.good$loan_amnt))
#t test
t.test(stat_data.t$loan_amnt ~ stat_data.t$loan_status, var.equal=TRUE)

# Mean and sd
c(mean(stat_data.t$annual_inc),sd(stat_data.t$annual_inc),mean(stat_data.t.bad$annual_inc),sd(stat_data.t.bad$annual_inc), mean(stat_data.t.good$annual_inc),sd(stat_data.t.good$annual_inc))
#t test
t.test(stat_data.t$annual_inc ~ stat_data.t$loan_status, var.equal=TRUE)

# Mean and sd *
c(mean(stat_data.t$emp_length, na.rm=T),sd(stat_data.t$emp_length, na.rm=T),mean(stat_data.t.bad$emp_length, na.rm=T),sd(stat_data.t.bad$emp_length, na.rm=T), mean(stat_data.t.good$emp_length, na.rm=T),sd(stat_data.t.good$emp_length, na.rm=T))
#t test
t.test(stat_data.t$emp_length ~ stat_data.t$loan_status, var.equal=TRUE)

# Mean and sd
c(mean(stat_data.t$inq_last_6mths),sd(stat_data.t$inq_last_6mths),mean(stat_data.t.bad$inq_last_6mths),sd(stat_data.t.bad$inq_last_6mths), mean(stat_data.t.good$inq_last_6mths),sd(stat_data.t.good$inq_last_6mths))
#t test
t.test(stat_data.t$inq_last_6mths ~ stat_data.t$loan_status, var.equal=TRUE)

# Mean and sd *
c(mean(stat_data.t$revol_util, na.rm=T),sd(stat_data.t$revol_util, na.rm=T),mean(stat_data.t.bad$revol_util, na.rm=T),sd(stat_data.t.bad$revol_util, na.rm=T), mean(stat_data.t.good$revol_util, na.rm=T),sd(stat_data.t.good$revol_util, na.rm=T))
#t test
t.test(stat_data.t$revol_util ~ stat_data.t$loan_status, var.equal=TRUE)

# Mean and sd
c(mean(stat_data.t$open_acc),sd(stat_data.t$open_acc),mean(stat_data.t.bad$open_acc),sd(stat_data.t.bad$open_acc), mean(stat_data.t.good$open_acc),sd(stat_data.t.good$open_acc))
#t test
t.test(stat_data.t$open_acc ~ stat_data.t$loan_status, var.equal=TRUE)

# Mean and sd *
c(mean(stat_data.t$mths_since_last_delinq, na.rm=T),sd(stat_data.t$mths_since_last_delinq, na.rm=T),mean(stat_data.t.bad$mths_since_last_delinq, na.rm=T),sd(stat_data.t.bad$mths_since_last_delinq, na.rm=T), mean(stat_data.t.good$mths_since_last_delinq, na.rm=T),sd(stat_data.t.good$mths_since_last_delinq, na.rm=T))
#t test
t.test(stat_data.t$mths_since_last_delinq ~ stat_data.t$loan_status, var.equal=TRUE)

# Mean and sd *
c(mean(stat_data.t$dti, na.rm=T),sd(stat_data.t$dti, na.rm=T),mean(stat_data.t.bad$dti, na.rm=T),sd(stat_data.t.bad$dti, na.rm=T), mean(stat_data.t.good$dti, na.rm=T),sd(stat_data.t.good$dti, na.rm=T))
#t test
t.test(stat_data.t$dti ~ stat_data.t$loan_status, var.equal=TRUE)

# Mean and sd
c(mean(stat_data.t$installment),sd(stat_data.t$installment),mean(stat_data.t.bad$installment),sd(stat_data.t.bad$installment), mean(stat_data.t.good$installment),sd(stat_data.t.good$installment))
#t test
t.test(stat_data.t$installment ~ stat_data.t$loan_status, var.equal=TRUE)


## if null value

stat_data$int_rate
stat_data$loan_amnt
stat_data$annual_inc
stat_data$emp_length
stat_data$inq_last_6mths
stat_data$revol_util
stat_data$open_acc
stat_data$mths_since_last_delinq
stat_data$dti
stat_data$installment
