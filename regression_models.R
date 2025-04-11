library(stargazer)
library(lmtest)
library(tidyverse)
library(fixest)

df <- read.csv('hack_level_regression_data.csv')

df2<-filter(df,year<2021)

m1 = feols(
  disclosed_binary ~ in_company_first5pctile | company + year +hacker, 
  data=df2, cluster = c("company","hacker"))
m2 = feols(
  disclosed_binary ~ in_company_first10pctile | company + year+hacker, 
  data=df2, cluster = c("company",'hacker'))
m3 = feols(
  disclosed_binary ~ in_company_first20pctile | company + year+hacker, 
  data=df2, cluster = c("company","hacker"))
m4 = femlm(
  disclosed_binary ~ in_company_first5pctile| company +year, 
  data=df2,  cluster = c("company","hacker"),family = 'logit')
m5 = femlm(
  disclosed_binary ~ in_company_first10pctile| company +year, 
  data=df2, cluster = c("company","hacker"),family = 'logit')
m6 = femlm(
  disclosed_binary ~ in_company_first20pctile | company  +year, 
  data=df2, cluster = c("company","hacker"),
  family = 'logit')
#etable(m2,m3,m5,m6,digits=3,fitstat=c('r2','pr2','n'),tex=T)
etable(m1, m2,m3,m4,m5,m6)