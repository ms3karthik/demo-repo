#Read raw data
data = read.csv("~/2021 Surveys/Seller/C2C Panel Survey/C2C Driver Analysis Nov v1.csv")
str(data)
data[sapply(data, is.character)] = lapply(data[sapply(data, is.character)], as.factor)
str(data)
library(dplyr)
library(party)
library(MASS)
data$NPS_Bucket = as.ordered(data$NPS_Bucket)
head(data$NPS_Bucket)
#data = filter(data, hCountry == 1)
#data = filter(data, hCountry == 2)
data = filter(data, hCountry == 3)
set.seed(1000)
str(data)

#ordered logit model for Q12
m_q12 <- polr(NPS_Bucket~Q12_1+Q12_2+Q12_3+Q12_4+Q12_5+Q12_6+Q12_7+Q12_8+Q12_9+Q12_10+Q12_11+Q12_12+Q12_13+Q12_14+Q12_15+Q12_16+Q12_17, data = data, Hess=TRUE)
summary(m_q12)
ctable <- coef(summary(m_q12))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
ci <- confint(m_q12)
exp(coef(m_q12))
q1_exp = exp(cbind(OR = coef(m_q12), ci))
q1_exp
q1_df = as.data.frame(q1_exp)

#ordered logit model for Q13
m_q13 <- polr(NPS_Bucket~Q13_1+Q13_2+Q13_3+Q13_4+Q13_5+Q13_6+Q13_7+Q13_8+Q13_9+Q13_10+Q13_11, data = data, Hess=TRUE)
summary(m_q13)
ctable <- coef(summary(m_q13))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
ci <- confint(m_q13)
exp(coef(m_q13))
q1_exp = exp(cbind(OR = coef(m_q13), ci))
q1_exp
q1_df = as.data.frame(q1_exp)

RF = cforest(q1~q5r1+q5r2+q5r3+q5r4+q5r5+q5r6+q5r7+q5r8, data = data, controls = cforest_unbiased(ntree = 1000))
varimp = varimp(RF, conditional = TRUE)
barplot(varimp, ylab = "Variable Importance", las = 2)
