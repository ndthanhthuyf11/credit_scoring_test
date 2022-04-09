library(ISLR)
library(tidyverse)
library(tidyselect)
library(pastecs)
library(tibble)
library(data.table)

df <- read.csv(".\\dataset\\mortgage.csv")
View(df)
#head(data)
str(df)

#descriptive
is.na(df$LTV_time)
#No missing values
df_describe <- summary(df)
view(df_describe)
#NA of LTV value: 

#detecting NA observations: 
na_rate <- function(x) {x %>% is.na() %>% sum() / length(x)}
sapply(df, na_rate) %>% round(2)

#replace NA by mean: 
df$LTV_time = ifelse(is.na(df$LTV_time),
                     ave(df$LTV_time, FUN = function (x)mean(x, na.rm = TRUE)),
                     df$LTV_time)
#stat.desc(df)

##Split dataset
d = sort(sample(nrow(df), nrow(df)*.6))
#select training sample
train <- df[d,]
test <- df[-d,]
#train <- subset(train,select)

##regression analysis

explanatory_vars <- c("balance_time", "LTV_time", "interest_rate_time", "uer_time")
# <- glm(payoff_time~ LTV_time + uer_time + balance_time + balance_orig_time,data=df,family=binomial)
m <- glm(payoff_time ~ .,data=train %>% select(-id, -time, -orig_time, -first_time, -mat_time, -payoff_time),family=binomial())
summary(m)

# invoke stepwise regression based on AIC 
m_step <- step(m, direction = "both", trace = FALSE)
summary(m_step)

# generate VIF 
vif(m, merge_coef = TRUE)

#get results of terms in regression
g<-predict(m,type='terms',test)

#function to pick top 3 reasons
#works by sorting coefficient terms in equation
# and selecting top 3 in sort for each loan scored
ftopk<- function(x,top=3){
  res=names(x)[order(x, decreasing = TRUE)][1:top]
  paste(res,collapse=";",sep="")
}
# Application of the function using the top 3 rows
topk=apply(g,1,ftopk,top=3)
#add reason list to scored tets sample
test<-cbind(test, topk)
view(topk)