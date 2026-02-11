#### Week 03 Normal Distribution
mu <- 26.4
sd <- 5.8
#### Problem Section 1
p1.0_Problems <- 0
#1.1
p1.1 <- qnorm(0.975, mean = mu, sd = sd); p1.1
#1.2
p1.2 <- 1 - pnorm(21, mean = mu, sd = sd); p1.2
#1.3
p1.3 <- dnorm(32.8, mean = 26.4, sd = 5.8)
#1.4
p1.4 <- pnorm(22, mean = mu, sd = sd) - pnorm(18, mean = mu, sd = sd);p1.4
#1.5
p1.5 <- dnorm(26.4, mean = mu, sd = sd)
#1.6
p1.6_lower <- mu - 2*sd
p1.6_upper <- mu + 2*sd
p1.6_answer <- c(p1.6_lower, p1.6_upper)
#1.7
p1.7_lower <- qnorm(0.025, mean = mu, sd = sd)
p1.7_upper <- qnorm(0.975, mean = mu, sd = sd)
p1.7_answer <- c(p1.7_lower, p1.7_upper)
#1.8
p1.8 <- (35 -mu) / sd; p1.8
#### Problem Section 2
p2.0_Problems <- 0
#2.1
p2.1 <- dnorm(1.25, mean = 0, sd = 1)
#2.2
p2.2 <- dnorm(-0.33, mean = 0, sd = 1)
#2.3
p2.3 <- dnorm(2.9, mean = 0, sd = 1)
#2.4
p2.4 <- dnorm(2.5, mean = 0, sd = 1)
#2.5
p2.5 <- dnorm(-0.9, mean = 0, sd = 1)
#2.6
p2.6 <- 1 - pnorm(-1, mean = 0, sd = 1)
#2.7
p2.7 <- pnorm(3.5, mean = 0, sd = 1) - pnorm(1.4, mean = 0, sd = 1)
#2.8
p2.8 <- qnorm(0.10, mean = 0, sd = 1)
#2.9
p2.9 <- qnorm(0.95, mean = 0, sd = 1)
#2.10
p2.10 <- qnorm(0.05, mean = 0, sd = 1)
#### Problem Section 3
p3.0_Problems <- 0
#3.1
p3.1 <- pnorm(4948, mean = 4313, sd = 583)
#3.2
p3.2 <- pnorm(5513, mean = 5261, sd = 807)
#3.3
p3.3 <- qnorm(0.05, mean = 4313, sd = 583)
#3.4
p3.4 <- qnorm(0.90, mean = 5261, sd = 807)
#3.5
p3.5_male_score <- (4460 - 4313) / 583
#3.6
p3.6_Female_score <- (5105 - 5261) / 807
#3.7
# I dont know if the female data is correct due to the negative but the woman had a better score too her group
#3.8
p3.8 <- pnorm(5000, mean = 4313, sd = 583) - pnorm(4500, mean = 4313, sd = 583)




