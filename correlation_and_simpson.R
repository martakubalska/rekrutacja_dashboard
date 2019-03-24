library(MASS)
library(ggplot2)
library(dplyr)

#----------------------------------------------------SIMPSON PARADOX----------------------------------------------------

samples = 50
r = 0.85


data = mvrnorm(n=samples, mu=c(10, 10), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
data <- data.frame(data)
group <- rep("set1",50)
data <- cbind(data,group)


r = 0.8

data1 = mvrnorm(n=samples, mu=c(13, 8), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
data1 <- data.frame(data1)
group <- rep("set2",50)
data1 <- cbind(data1,group)



r = 0.9

data2 = mvrnorm(n=samples, mu=c(16, 6), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
data2 <- data.frame(data2)
group <- rep("set3",50)
data2 <- cbind(data2,group)



simpson <- rbind(data,data1, data2)

write.csv(simpson, "simpson.csv")
remove(list=ls())
simpson <- read.csv("simpson.csv", header = TRUE)[,-1]

#Wykres
ggplot(simpson, aes(x=X1, y=X2, color = group, shape = group)) + geom_point(size = 2) + 
  geom_smooth(method=lm, se=FALSE, color = "black") + scale_color_manual(values=c('red2', 'dodgerblue2', "gray50")) + 
  theme(legend.position="none")


ggplot(simpson, aes(x=X1, y=X2)) + geom_point(size = 2) + 
  geom_smooth(method=lm, se=FALSE, color = "black") + scale_color_manual(values=c('red2', 'dodgerblue2', 'gray50')) + 
  geom_smooth(aes(color = group), method=lm, se=FALSE) + theme(legend.position="none")


example <- read.csv2("simpson_example.csv", header = TRUE)
example <- data.frame(example)
example$rownum <- c("Total", "Group1", "Group2", "Group3")
example <-example %>% as_tibble() %>% tibble::column_to_rownames("rownum") 
example
