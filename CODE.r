library(dslabs)
library(tidyverse)


# sourcing functions (IMPORTANT!)
source("Functions.r")


# DATA WRANGLING
# admission data UC Berkeley from 1973
data <- dslabs::admissions %>% 
  mutate(admitted = admitted/100) %>%  # transforming admitted in percent
  mutate(admitted_total = round(admitted * applicants))


# calculating total admisson rate (not gender-grouped)
admissionrate_total <- sum(data$admitted_total) / sum(data$applicants)


# admission rate / gender
table1 <- data %>% 
  group_by(gender) %>% 
  summarise(p = sum(admitted_total) / sum(applicants), n = sum(applicants))




# Scenario 1: H0 false (H1 true)
# testing power for 
# 1. change in n_applicants / effect size stays the same (0.10)
vec_applicants <- seq(from = 10, to = 1500, by = 5)


# setting seed before each simulation 
# generating data frame, containing a vector of power-values for each simulation, 
# corresponding to a specific test (here: Z-Test)
set.seed(0911) 
power1.1.1_z_test <- data.frame(
  power = unlist(sapply(vec_applicants, function(i) {
  simulation_testing(matrix1 = dataset_sim(n_sets = 1000,
                                           p = 0.4,
                                           n_applicants = i),
                     matrix2 = dataset_sim(n_sets = 1000,
                                           p = 0.5,
                                           n_applicants = i),
                     NullHyp = FALSE)[5]
  
})), 
  object = "power1.1.1_z_test", # label containing information about scenario,
  i = vec_applicants)           # test and observed metric (power or size)
                                # "i" is a vector, either n_applicants or effect
                                # size, as we test the power as a function of 
                                # those variables


set.seed(0911)
power1.1.1_chisq <- data.frame(
  power =unlist(sapply(vec_applicants, function(i) {
    simulation_testing(matrix1 = dataset_sim(n_sets = 1000,
                                             p = 0.4,
                                             n_applicants = i),
                       matrix2 = dataset_sim(n_sets = 1000,
                                             p = 0.5,
                                             n_applicants = i),
                       NullHyp = FALSE)[6]
    
  })), 
  object = "power1.1.1_chisq",
  i = vec_applicants)




set.seed(0911)
power1.1.2_z_test <- data.frame(
  power = unlist(sapply(vec_applicants, function(i) {
  simulation_testing(matrix1 = dataset_sim(n_sets = 1000,
                                           p = 0.3,
                                           n_applicants = i),
                     matrix2 = dataset_sim(n_sets = 1000,
                                           p = 0.6,
                                           n_applicants = i),
                     NullHyp = FALSE)[5]
  
})),
  object = "power1.1.2_z_test",
  i = vec_applicants)

set.seed(0911)
power1.1.2_chisq <- data.frame(
  power = unlist(sapply(vec_applicants, function(i) {
  simulation_testing(matrix1 = dataset_sim(n_sets = 1000,
                                           p = 0.3,
                                           n_applicants = i),
                     matrix2 = dataset_sim(n_sets = 1000,
                                           p = 0.6,
                                           n_applicants = i),
                     NullHyp = FALSE)[6]
  
})),
  object = "power1.1.2_chisq",
  i = vec_applicants)


# Scenario results:
#1.1.1 d = 0.1
result1.1.1 <- c(power1.1.1_z_test$power[power1.1.1_z_test$i == 300],
                 power1.1.1_chisq$power[power1.1.1_chisq$i == 300])

#1.1.1 d = 0.3
result1.1.2 <- c(power1.1.2_z_test$power[power1.1.2_z_test$i == 300],
                 power1.1.2_chisq$power[power1.1.2_chisq$i == 300])




df1.1 <- rbind(power1.1.1_z_test, #binding together the computed data frames for
               power1.1.1_chisq,  # the purpose of plotting via ggplot()
               power1.1.2_z_test,
               power1.1.2_chisq)


df1.1 %>% #plotting power as a function of the sample size
  ggplot(aes(x = i, y = power, color = object))+
  geom_line()+
  geom_hline(yintercept = 0.90, linetype = "dashed")+
  scale_y_continuous(breaks = sort(c(seq(0, 1, by=0.5), c(0.75, 0.9))))+
  geom_hline(yintercept = 0.75, linetype = "dashed")+
  geom_vline(xintercept = 300, color= "red", linetype = "dashed")+
  facet_wrap(~object)+
  xlab("number of applicants / n")+
  ylab("statistical power")+
  scale_color_manual(values = c("purple", "seagreen3", "purple", "seagreen3"))
  
  ggsave("1.1.png") #saving image to workspace

# calculating and plotting difference between the power of Z-test and Chisq-test,
#as a function of n
  
# data frames containing the difference, the effect size (delta) and the sample
# size used for the simulation
diff1 <- data.frame(diff = (power1.1.1_z_test$power - power1.1.1_chisq$power),
                    delta = 0.1,
                    i = vec_applicants)


diff2 <-  data.frame(diff = (power1.1.2_z_test$power - power1.1.2_chisq$power),
                     delta = 0.3,
                     i = vec_applicants)


df1.1_diff <- rbind(diff1, diff2)

df1.1_diff %>% 
  ggplot(aes(x = i, y = diff))+
  geom_line(color = "red")+
  facet_wrap(~delta)+
  ylim(c(-0.01, 0.2))+
  ylab("difference (power z test - power chisq)")+
  xlab("number of applicants / n")+
  geom_smooth(size=0.5, color = "blue", linetype="dashed", se=TRUE, alpha = 0.1)

ggsave("1.1.diff.png")



# minimum sample size needed to conduct a power of 90%
min(power1.1.1_z_test$i[power1.1.1_z_test$power > 0.9]) # 1.1.1 / d=0.1
min(power1.1.2_z_test$i[power1.1.2_z_test$power > 0.9]) # 1.1.2 / d=0.3





# Simulation procedure, creation of data frames and plotting for the remaining
# scenarios follows the same procedure as for scenario 1.1 - 
# commenting is therefore reduced to a minimum as comments in scenario 1.1 
# shall be able to explain the following code

# 2. effect size with fixed number of applicants (=100)
vec_femaleproportion_1.2 <- seq(from= 0.1, to = 0.5, by = 0.005)
delta1.2 <- vec_femaleproportion_1.2- 0.1


set.seed(0911)
power1.2_z_test <- data.frame(
  power = unlist(sapply(vec_femaleproportion_1.2, function(i) {
  simulation_testing(matrix1 = dataset_sim(n_sets = 1000,
                                           p = 0.1,
                                           n_applicants = 100),
                     matrix2 = dataset_sim(n_sets = 1000,
                                           p = i, 
                                           n_applicants = 100),
                     NullHyp = FALSE)[5]
  
})),
  object = "power1.2_z_test",
  effect_size = delta1.2)



set.seed(0911)
power1.2_chisq <- data.frame(
  power = unlist(sapply(vec_femaleproportion_1.2, function(i) {
  simulation_testing(matrix1 = dataset_sim(n_sets = 1000,
                                           p = 0.1,
                                           n_applicants = 100),
                     matrix2 = dataset_sim(n_sets = 1000,
                                           p = i,
                                           n_applicants = 100),
                     NullHyp = FALSE)[6]
  
})),
  object = "power1.2_chisq",
  effect_size = delta1.2)



# results 1.2 (d = 0.2)
result1.2 <- c(power1.2_z_test$power[41], power1.2_chisq$power[41])


df1.2 <- rbind(power1.2_z_test, power1.2_chisq)


df1.2 %>% 
  ggplot(aes(x = effect_size, y = power, color = object))+
  geom_line()+
  xlab("effect size (d)")+
  ylab("statistical power")+
  geom_hline(yintercept = 0.90, linetype = "dashed")+
  geom_hline(yintercept = 0.75, linetype = "dashed")+
  scale_y_continuous(breaks = sort(c(seq(0, 1, by=0.5), c(0.75, 0.9))))+
  geom_vline(xintercept = 0.2, colour = "red", linetype = "dashed")+
  facet_wrap(~object)+
  scale_color_manual(values = c("purple", "seagreen3"))
  
ggsave("1.2.png")



diff_1.2 <- data.frame(
  diff = (power1.2_z_test$power - power1.2_chisq$power),
  i = delta1.2)

diff_1.2 %>% # EDIT!
  ggplot(aes(x = i, y = diff))+
  geom_line(color = "red")+
  ylim(c(-0.01, 0.3))+
  ylab("difference (power z test - power chisq)")+
  xlab("effect size")
  


ggsave("1.2.diff.png")


min(power1.2_z_test$effect_size[power1.2_z_test$power > 0.75]) # 1.1.1 / d=0.1
min(power1.2_chisq$effect_size[power1.2_chisq$power > 0.75]) 

min(power1.2_z_test$effect_size[power1.2_z_test$power > 0.9]) # 1.1.1 / d=0.1
min(power1.2_chisq$effect_size[power1.2_chisq$power > 0.9]) 

  

# 3. 
# effect size is 0.1, 
# number of male applicants is fixed (=100),
# number of female applicants changed
vec_applicants2  <- seq(from = 10, to = 2500, by = 10)

set.seed(0911)
power1.3_z_test <- data.frame(
  power = unlist(sapply(vec_applicants2, function(i) {
  simulation_testing(matrix1 = dataset_sim(n_sets = 1000,
                                           p = 0.4,
                                           n_applicants = 200),
                     matrix2 = dataset_sim(n_sets = 1000,
                                           p = 0.5,
                                           n_applicants = i),
                     NullHyp = FALSE)[5]
  
})),
  object = "power1.3_z_test",
  i = vec_applicants2)



set.seed(0911)
power1.3_chisq <- data.frame(
  power = unlist(sapply(vec_applicants2, function(i) {
  simulation_testing(matrix1 = dataset_sim(n_sets = 1000,
                                           p = 0.4,
                                           n_applicants = 200),
                     matrix2 = dataset_sim(n_sets = 1000,
                                           p = 0.5,
                                           n_applicants = i),
                     NullHyp = FALSE)[6]
  
})),
  object = "power1.3_chisq",
  i = vec_applicants2)

result1.3 <- c(power1.3_z_test$power[power1.3_z_test$i == 800], 
                power1.3_chisq$power[power1.3_chisq$i == 800])


df1.3 <- rbind(power1.3_z_test, power1.3_chisq)

df1.3 %>% # checking plotting!!!! 
  ggplot(aes(x = i, y = power, color = object))+
  geom_line()+
  geom_hline(yintercept = 0.90, linetype = "dashed")+
  geom_hline(yintercept = 0.75, linetype = "dashed")+
  scale_y_continuous(breaks = sort(c(seq(0, 1, by=0.5), c(0.75, 0.9))))+
  facet_wrap(~object)+
  xlab("number of applicants / n")+
  ylab("statistical power")+
  geom_vline(xintercept = 800, colour = "red", linetype = "dashed")+
  scale_color_manual(values = c("purple", "seagreen3"))

ggsave("1.3.png")

diff_1.3. <- data.frame(
                    diff = (power1.3_z_test$power - power1.3_chisq$power),
                    i = vec_applicants2)

diff_1.3. %>% 
  ggplot(aes(x = i, y = diff))+
  geom_line(colour = "red")+
  ylim(c(-0.01, 0.2))+
  ylab("difference (power z test - power chisq)")+
  xlab("number of applicants / n")+
  geom_smooth(size=0.5, color = "blue", linetype="dashed", se=TRUE, alpha = 0.1)


ggsave("1.3.diff.png")

# Scenario 2: H0 is true
# size as function of n_applicants

set.seed(0911)
size2.1_z_test <- data.frame(size = unlist(sapply(vec_applicants, function(i) {
  simulation_testing(matrix1 = dataset_sim(1000,
                                           p = 0.2,
                                           n_applicants = i),
                     matrix2 = dataset_sim(1000,
                                           p = 0.2,
                                           n_applicants = i),
                     NullHyp = TRUE)[3]
})),
                          object = "size2.1_z_test",
                          i = vec_applicants)



set.seed(0911)
size2.1_chisq <- data.frame(size = unlist(sapply(vec_applicants, function(i) {
  simulation_testing(matrix1 = dataset_sim(1000,
                                           p = 0.2,
                                           n_applicants = i),
                     matrix2 = dataset_sim(1000, p = 0.2, n_applicants = i),
                     NullHyp = TRUE)[4]
})),
                             object = "size2.1_chisq",
                             i = vec_applicants)



result2.1 <- c(size2.1_z_test$size[size2.1_z_test$i == 300],
               size2.1_chisq$size[size2.1_chisq$i == 300])



df2.1 <- rbind(size2.1_z_test, size2.1_chisq)



df2.1 %>% 
  ggplot(aes(x = i, y = size, color = object))+
  geom_line()+
  geom_smooth(color = "blue")+
  facet_wrap(~object)+
  xlab("number of applicants / n")+
  ylab("statistical size")+
  ylim(c(-0.01, 0.1))+
  geom_vline(xintercept = 300, colour = "red", linetype = "dashed")+
  scale_color_manual(values = c("purple", "seagreen3"))
  


ggsave("2.1.png")

diff_2.1. <- data.frame(
  diff = (size2.1_z_test$size - size2.1_chisq$size),
  i = vec_applicants)

diff_2.1. %>% 
  ggplot(aes(x = i, y = diff, color = "red"))+
  geom_line()+
  ylab("difference (size z test - size chisq)")+
  xlab("number of applicants / n")+
  guides(color = "none")+
  geom_smooth(size=0.5, color = "blue", linetype="dashed", se=TRUE, alpha = 0.1)


ggsave("2.1.diff.png")



##### Table outputs for report ####
# Table Scenarios
table_scenario1 <- matrix(nrow = 5, ncol=5)
rownames(table_scenario1) <- c("1.1.1", "1.1.2", "1.2", "1.3", "2.1")
colnames(table_scenario1) <- c("p male", "p female", "effect size", "n male", "n female")
table_scenario1[,1] <-c(0.4, 0.3, 0.1, 0.4, 0.2)
table_scenario1[,2] <- c(0.5, 0.6, 0.2, 0.5, 0.2)
table_scenario1[,3] <- c(0.1, 0.3, 0.2 , 0.1, 0)
table_scenario1[,4] <- c(300, 300, 100, 200, 300)
table_scenario1[,5] <- c(300, 300, 100, 800, 300)

#Table results
table_results <- matrix(nrow = 2, ncol = 5)
rownames(table_results) <- c("Z-Test", "ChiSq-Test")
colnames(table_results) <- c("1.1.1", "1.1.2", 1.2, 1.3, 2.1)
table_results[1,] <- c(0.729, 1, 0.95, 0.727, 0.054)
table_results[2,] <- c(0.729, 1, 0.95, 0.725, 0.053)
