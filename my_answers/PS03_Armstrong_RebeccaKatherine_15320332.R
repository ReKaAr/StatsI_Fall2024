#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages

fp<-file.path("C:", "Users", "isabel", "Documents", "GitHub", "StatsI_Fall2024", "problemSets", "PS03")
install.packages("stargazer")
library(stargazer)
#Question 1(1), load data and run regression where output is voteshare and explanatory is difflog
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

reg<-lm(inc.sub$difflog~inc.sub$voteshare)
regtable<-summary(reg)
stargazer(reg, type="latex",title="Regression Summary: Outcome VoteShare, Explanatory DiffLog", out = file.path(fp, "pso3q1regsum.tex"))
#Question 1(2), make a scatterplot of this regression and add the regression line
scpl<-plot(inc.sub$voteshare, inc.sub$difflog)
abline(reg, col = "red", lwd = 1)
#Question 1(3), save the residuals in a separate object
res_reg<-residuals(reg)
resreg1sum<-head(res_reg)
stargazer(resreg1sum, type="latex", title="Regression Residuals: Outcome Voteshare, Explanatory Difflog", out = file.path(fp, "ps03q1res.tex"))
#Question 1(4), writing the prediction equation (Below is copied from latex pdf)
print("The prediction equation for the regression measuring voteshare as the outcome variable and difflog as the explanatory variable is as follows: 
      Voteshare = B0 + difflogB1.   
      There is no error term as it is a prediction equation thus we do not know if there is any unexplained variance, 
      and no use of 'i' as we are not measuring the impact of an individual observation",type="latex")

#Question 2(1), run a regression with output presvote and explanatory difflog
reg2<-lm(inc.sub$difflog~inc.sub$presvote)
regtable2<-summary(reg2)
stargazer(reg2, type="latex",title="Regression Summary: Outcome Presvote, Explanatory: DiffLog", out = file.path(fp, "pso3q2regsum.tex"))

#Question 2(2), make a scatterplot of this regression and add regression line
scpl2<-plot(inc.sub$presvote, inc.sub$difflog)
abline(reg2, col = "red", lwd = 1)
#Question 2(3), save the residuals in a separate object
res_reg2<-residuals(reg2)
resreg2sum<-head(res_reg2)
stargazer(resreg2sum,type="latex", title="Regression Residuals: Outcome Presvote, Explanatory Difflog", out = file.path(fp, "ps03q2res.tex"))

#Question 2(4), prediction equation (as copied from latex pdf):

#Question 3(1) run regression where outcome is voteshare and explanatory is presvote
reg3<-lm(inc.sub$presvote~inc.sub$voteshare)
regtable3<-summary(reg3)
stargazer(reg3, type="latex",title="Regression Summary: Outcome VoteShare, Explanatory Presvote", out = file.path(fp, "pso3q3regsum.tex"))
#Question 3(2), make a scatterplot of this regression and add the regression line
scpl3<-plot(inc.sub$voteshare, inc.sub$presvote)
abline(reg3, col = "red", lwd = 1)
#Question 3(add), save the residuals in a separate object NOT ASKED IN QUESTION, JUST IN CASE!
res_reg3<-residuals(reg3)
stargazer(head(res_reg3),type="latex", title="Regression Residuals: Outcome Voteshare, Explanatory Presvote", out = "ps03q3res.tex")

#Question 3(3) Write the prediction equation (copied from latex)

#Question 4(1) run regression with outcome being q1 residuals and explanatory being q2 residuals
reg4<-lm(res_reg2~res_reg)
regtable4<-summary(reg4)
stargazer(reg4, type="latex",title="Regression Summary: Outcome Q1 Residuals, Explanatory Q2 Residuals", out = file.path(fp, "pso3q4regsum.tex"))
#Question 4(2) make a scatterplot of this regression and add the regression line
scpl4<-plot(res_reg, res_reg2)
abline(reg4, col = "red", lwd = 1)

#Question 4(3) write the prediction equation (below copied from latex)

#Question 5(1) regression with outcome being voteshare and explanatory being difflog and presvote
reg5 <- lm(inc.sub$voteshare ~ inc.sub$difflog + inc.sub$presvote)
stargazer(reg5, type="latex",title="Regression Summary: Outcome Voteshare, Explanatory difflog and presvote", out = file.path(fp, "pso3q5regsum.tex"))

