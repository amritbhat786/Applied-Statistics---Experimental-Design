# 1.1

temperature_df <- read.csv("./data/temperature_experiment.csv")

## Creating a box plot to see 
boxplot(split(temperature_df$output,temperature_df$temp))

mean=with(temperature_df,tapply(output,temp,mean))
sd=with(temperature_df,tapply(output,temp,sd))
sample_length=with(temperature_df,tapply(output,temp,length))
descriptive_stats = data.frame(mean,sd,sample_length)
descriptive_stats
z_statistic=(mean[1]-mean[2])/sqrt(sum(sd^2/sample_length))
data.frame(z_statistic,p=round(2*(1-pnorm(abs(z_statistic))),4))


# 1.2

Yes, we reject the null hypothesis

# 1.3


# 1.4
welch_t_test_results <- with(temperature_df,
     t.test(output[temp=="60"],output[temp=="75"],var.equal=F,alternative="less"))
welch_t_test_results

print("Test statistic: ")
welch_t_test_results$statistic

print("p-value:")
welch_t_test_results$p.value



# 1.5
equal_variance_t_test_results <- with(temperature_df,
     t.test(output[temp=="60"],output[temp=="75"],var.equal=T,alternative="less"))
equal_variance_t_test_results

print("Test statistic: ")
equal_variance_t_test_results$statistic

print("p-value:")
equal_variance_t_test_results$p.value

# 1.6
hist(temperature_df[temperature_df$temp == "60",]$output)
hist(temperature_df[temperature_df$temp == "75",]$output)
# As variance for both samples are not equal, either the large sample z test or Welch t-test is relevant for our use case


# 1.7
se=sqrt(sum(sd^2/sample_length))
z.05=qnorm(0.975)
lower = mean[1]-mean[2]-z.05*se
upper = mean[1]-mean[2]+z.05*se
lower
upper
print(paste(" 95% confidence interval for the difference between mean output using the large-sample (lower,upper): (",lower,",",upper,")"))

# 1.8 
welch_t_test_results <- with(temperature_df,
                             t.test(output[temp=="60"],output[temp=="75"],var.equal=F,alternative="two.sided"))
welch_t_test_results$conf.int

#1.9
equal_variance_t_test_results <- with(temperature_df,
                                      t.test(output[temp=="60"],output[temp=="75"],var.equal=T,alternative="two.sided"))
equal_variance_t_test_results$conf.int

#1.10


# 2.1
defects_df <- read.csv("/Users/amrit/Documents/Courses/Applied Statistics & Experimental Design/Assignments/A2/data/defects.csv")
# As we are hypothesizing for an estimate's value within the same sample, we can use the 1 sample t-test

# 2.2
def_A <- defects_df[defects_df$Method == "A",]$Weight
def_B <- defects_df[defects_df$Method == "B",]$Weight
def_C <- defects_df[defects_df$Method == "C",]$Weight
def_D <- defects_df[defects_df$Method == "D",]$Weight

t.test(def_A,def_B,var.equal = T)$p.value
t.test(def_A,def_C,var.equal = T)$p.value
t.test(def_A,def_D,var.equal = T)$p.value
t.test(def_B,def_D,var.equal = T)$p.value
t.test(def_C,def_D,var.equal = T)$p.value
t.test(def_B,def_C,var.equal = T)$p.value



# 2.6

t.test(def_A,def_B,var.equal = T)$p.value/6
t.test(def_A,def_C,var.equal = T)$p.value/6
t.test(def_A,def_D,var.equal = T)$p.value/6
t.test(def_B,def_D,var.equal = T)$p.value/6
t.test(def_C,def_D,var.equal = T)$p.value/6
t.test(def_B,def_C,var.equal = T)$p.value/6
