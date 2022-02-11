# 1.1
# load data

# Q: Which 2 sample t-test to use, welch t-test or equal variance t-test?
```{r,echo=T,comment=" "}
lead_df <- read.csv("/Users/amrit/Documents/Courses/Applied Statistics & Experimental Design/Assignments/A3/lead_study.csv")
var(lead_df[lead_df$SEX==1,]$IQ)
var(lead_df[lead_df$SEX==2,]$IQ)
## The variance of above two groups are not equal, therefore we consider the Welch t-test

welch_t_test_results <- with(lead_df, t.test(IQ[SEX==1],IQ[SEX==2],var.equal = T))
welch_t_test_results

```

1.7
summary(aov(IQ ~ factor(GROUP), data=lead_df))

1.8
# From the p-value we deduce that two or more groups don't have the same mean. Therefore
# there might be an association between lead exposure and IQ

1.9 
# Variance check
var(lead_df[lead_df$GROUP==1,]$IQ)
var(lead_df[lead_df$GROUP==2,]$IQ)
var(lead_df[lead_df$GROUP==3,]$IQ)
# Variances are not equal 

1.10
group_1_IQ <- lead_df[lead_df$GROUP == 1,]$IQ
group_2_IQ <- lead_df[lead_df$GROUP == 2,]$IQ
group_3_IQ <- lead_df[lead_df$GROUP == 3,]$IQ

t.test(group_1_IQ,group_2_IQ, var.equal = F)$p.value
t.test(group_2_IQ,group_3_IQ, var.equal = F)$p.value
t.test(group_3_IQ,group_1_IQ, var.equal = F)$p.value

1.11
# Upon bonferroni correction, we know that the significance level for each 
# comparison would be 0.016667. As the p-value for the comparison between group 1 and group 2
# is less than the significance level after bonferroni correction we reject the null hypothesis.
# This agrees with our conclusion in Q8 that there might be an assoication between lead and IQ.

1.12
# Q: What all does descriptive statistics cover?
men_df <- lead_df[lead_df$SEX==1,]
women_df <- lead_df[lead_df$SEX==2,]

men_mean=with(men_df,tapply(IQ,GROUP,mean))
men_sd=with(men_df,tapply(IQ,GROUP,sd))
men_sample_length=with(men_df,tapply(IQ,GROUP,length))
men_descriptive_stats = data.frame(men_mean,men_sd,men_sample_length)
men_descriptive_stats

women_mean=with(women_df,tapply(IQ,GROUP,mean))
women_sd=with(women_df,tapply(IQ,GROUP,sd))
women_sample_length=with(women_df,tapply(IQ,GROUP,length))
women_descriptive_stats = data.frame(women_mean,women_sd,women_sample_length)
women_descriptive_stats

1.13

summary(aov(IQ ~ factor(GROUP), data=men_df))
summary(aov(IQ ~ factor(GROUP), data=women_df))


1.14

No association between lead and IQ in men (disagreeing with Q8 and Q11), whereas there is an association between lead and IQ in women (agreeing with Q8 & Q11)


1.15

print("Pariwise comparison for Men")
men_group_1_IQ <- men_df[men_df$GROUP == 1,]$IQ
men_group_2_IQ <- men_df[men_df$GROUP == 2,]$IQ
men_group_3_IQ <- men_df[men_df$GROUP == 3,]$IQ

t.test(men_group_1_IQ,men_group_2_IQ, var.equal = F)$p.value
t.test(men_group_2_IQ,men_group_3_IQ, var.equal = F)$p.value
t.test(men_group_3_IQ,men_group_1_IQ, var.equal = F)$p.value


print("Pariwise comparison for Women")
women_group_1_IQ <- women_df[women_df$GROUP == 1,]$IQ
women_group_2_IQ <- women_df[women_df$GROUP == 2,]$IQ
women_group_3_IQ <- women_df[women_df$GROUP == 3,]$IQ

t.test(women_group_1_IQ,women_group_2_IQ, var.equal = F)$p.value
t.test(women_group_2_IQ,women_group_3_IQ, var.equal = F)$p.value
t.test(women_group_3_IQ,women_group_1_IQ, var.equal = F)$p.value

#1.16
