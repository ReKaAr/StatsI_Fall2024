#PS04 Rebecca Katherine Armstrong, 15320332

#Installing Packages prior to starting
install.packages("stargazer")
library(stargazer)
install.packages("car")
library(car)
data(Prestige)
help(Prestige)
fp<-file.path("C:", "Users", "isabel", "Documents", "GitHub", "StatsI_Fall2024", "problemSets", "PS04")

#Question 1(a) 
#variable professional = 1 if type="prof", else professional=0
professional<-ifelse(Prestige$type=="prof",1,0)
prtab<-table(professional)
prtab<-as.data.frame(prtab)
stargazer(prtab, type="latex",title="Professional Variable Table", out = file.path(fp, "ps04q1table.tex"))

#Question 1(b)
#Linear model with prestige as outcome variable, and explanatory variables being income, professional and interaction of two.
presreg<-lm(Prestige$prestige~Prestige$income*professional)
stargazer(presreg, type="latex",title="Linear Model with Outcome Variable: 
Prestige and Explanatory Variables: Income, Professional, 
and interaction of Income and Professional", 
out = file.path(fp, "ps04q1reg.tex"))

#Question 1(f) 
#effect of $1000 increase on prestige score for professional occupations?
cf<-coef(presreg)
meinc<-(cf["Prestige$income"] + cf["Prestige$income:professional"])
meinc
stargazer(meinc, type="latex",title="Marginal Effect of income when the variable takes the value of  $1", out = file.path(fp, "ps04q1one$.tex"))
meinc2<-meinc*1000
meinc2
stargazer(meinc2, type="latex",title="Change in Prestige associated with a $1000 increase in income", out = file.path(fp, "ps04q1thous$.tex"))
        

#Question 1(g)
#effect of changing professional from 0-1 when income is $6000?
chngprof<-cf["professional"]+cf["Prestige$income:professional"]*6000
chngprof
stargazer(chngprof, type="latex",title="Marginal Effect of professional jobs when income is six thousand dollars", out = file.path(fp, "ps04q1sixthous$.tex"))


#Question 2(a)
#Inputting data
lawn_cf<-0.042
lawn_se<-0.016
adjlawn_cf<-0.042
adjlawn_se<-0.013
n_lawn<-30
n_adjlawn<-76
int_cf<-0.302
int_se<-0.011
rsqr<-0.094
n<-131
#Calculating T-Statistics and degrees of freedom
lawn_tstat<-lawn_cf/lawn_se
adjlawn_tstat<-adjlawn_cf/adjlawn_se
int_tstat<-int_cf/int_se
df_lawn=n_lawn-1
df_adjlawn=n_adjlawn-1
df_int<-n-1
#Creating p-value function
p_value<-function(pval) {
  if(pval>0.05){
    cat("P Value is:", pval, "The p value is greater than 0.05, 
    therefore we fail to reject the null hypothesis")
       }
  else {
  cat("P value is:", pval, "The p value is less than 0.05, thus 
      there is sufficient statistical evidence to reject the null 
      hypothesis")
  }
}
#Measuring p-values
p_value(p_lawn)
p_value(p_adjlawn)
p_value(p_int)
