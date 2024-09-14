library('forecast')
library('tidyverse')
library('latex2exp')

interest <- read.csv('interest.csv')

interest.vec <- c(interest[,1])

sample.mean <- mean(interest.vec)

#COMPUTING MEAN-CORRECTED St
S_t <- interest.vec - sample.mean

#PLOTTING THE ACF OF St
plot <- ggAcf(S_t, lag.max = 20)
print(plot)

#PLOTTING THE PACF OF St
plot <- ggPacf(S_t, lag.max = 20)
print(plot)

#CREATING DATA-FRAME TO GET LAG 
time <- 1:302

S_t.df <- data.frame(time, S_t)

lag.interest <- lag(S_t.df, 1)
lag.interest <- lag.interest %>% rename('S_(t-1)' = 'S_t', 'time lag' = 'time')
S_t.df['S_(t-1)'] <- lag.interest$`S_(t-1)`
S_t.df2 <- S_t.df[2:nrow(S_t.df), ]

#PLOTTING SCATTERPLOT TO SEE CORRELATION BETWEEN VALUES
ggplot(S_t.df[2:nrow(S_t.df),], aes(x=`S_(t-1)`, y= `S_t`)) +
  geom_point(color = 'blue')

#PERFORMING LINEAR REGRESSION TO GET PSI
l_reg <- lm(S_t.df2$S_t ~ 0 + S_t.df2$`S_(t-1)`)
l_reg2 <- lm(S_t.df2$S_t ~ S_t.df2$`S_(t-1)` -1)

psi <- l_reg$coefficients
psi <- 0.9908014 

#OBTAINING SIGMA 
Z.sigma2 <- var(S_t.df2$S_t - psi*S_t.df2$`S_(t-1)`) 
Z.sigma <- sqrt(Z.sigma2)
mean.wn <- mean(S_t.df2$S_t - psi*S_t.df2$`S_(t-1)`)

#PROVING THAT THE RIGHT VALUES HAVE BEEN FOUND
arima(S_t.df$S_t, order=c(1,0,0) ,include.mean = FALSE)

simulate.z <- rnorm(301, mean=0,sd = Z.sigma2)

S_t.sim <- psi*S_t.df2$`S_(t-1)` + simulate.z

S_t.samplepath1 <- S_t.df2$S_t

for (i in seq(0,50,1)){
  S_t.samplepath1[302+i] <- psi*S_t.samplepath1[(301+i)] + rnorm(1,mean=0,sd=Z.sigma)
}

df.samplepath <- tibble(S_t.samplepath1)
time <- 1:352
plot <- ggplot(data = df.samplepath, aes(x=time,y=S_t.samplepath1, colour = (time<=301))) + 
  geom_line() 

print(plot)

S_t.samplepath2 <- S_t.df2$S_t

for (i in seq(0,50,1)){
  S_t.samplepath2[302+i] <- psi*S_t.samplepath2[(301+i)] + rnorm(1,mean=0,sd=Z.sigma)
}

df.samplepath['samplepath2'] <- S_t.samplepath2

S_t.samplepath3 <- S_t.df2$S_t

for (i in seq(0,50,1)){
  S_t.samplepath3[302+i] <- psi*S_t.samplepath3[(301+i)] + rnorm(1,mean=0,sd=Z.sigma)
}

df.samplepath['samplepath3'] <- S_t.samplepath3

S_t.samplepath4 <- S_t.df2$S_t

for (i in seq(0,50,1)){
  S_t.samplepath4[302+i] <- psi*S_t.samplepath4[(301+i)] + rnorm(1,mean=0,sd=Z.sigma)
}

df.samplepath['samplepath4'] <- S_t.samplepath4


#FIX THE BELOW
plot <- ggplot(data = df.samplepath, aes(x=time,y=S_t.samplepath1, colour = (time<=301))) + 
  geom_line(aes(colour = "orginal path")) + 
  geom_line(aes(y=samplepath2, colour = "sample path 2")) +
  geom_line(aes(y=samplepath3, colour = "sample path 3")) +
  geom_line(aes(y=S_t.samplepath1, colour = "sample path 1")) + 
  geom_line(aes(y=S_t.samplepath4, colour="sample path 4")) +
  scale_color_manual("",
                     breaks = c("sample path 1", "sample path 2", "sample path 3", "sample path 4","original path"),
                     values = c("sample path 2" = "cyan", "sample path 3" = "royal blue", "sample path 3" = "red" , "sample path 1" = "orange1")) +
  labs(y=TeX('$S_{t}$'), x = "time")

print(plot)


#FINDING THE RESIDUALS

#METHOD 1 OF COMPUTING RESIDUALS
residuals <- S_t.df2$S_t - psi*S_t.df2$`S_(t-1)`
#METHOD 2 OF COMPUTING RESIDUALS
res <- residuals(l_reg)
ggAcf(res)
#METHOD 3 OF COMPUTING REDIDUALS
residuals.2 <- resid(l_reg)
ggAcf(residuals.2)

#CREATE DATA FRAME CONTAINING RESIDUALS
df.residuals <- tibble(residuals)

#RESIDUAL PLOTS 
plot <- ggplot(data=df.residuals, aes(x=1:nrow(df.residuals),y=residuals)) +
  geom_line(color = 'royalblue') + 
  labs(x = "Time", y = "Residuals")
plot(residuals)
print(plot)

plot <- ggplot( data = df.residuals, aes(x=1:nrow(df.residuals), y=residuals)) + 
  geom_point(colour = "royalblue") + 
  labs(x="Time", y = "Residuals")

print(plot)


plot <- ggAcf(residuals, lag.max=301)
print(plot)

plot <- ggAcf(residuals, lag.max = 30)
print(plot)

#LJUNG-BOX TEST OF RESDIUALS
Box.test(residuals, type="Ljung",lag=25)



interest <- read_csv("interest.csv")

interest.vec <- c(interest[,1])

sample.mean <- mean(interest.vec$I)


S_t <- interest.vec$I - sample.mean

training.set <- S_t[1:201]
test.set <- S_t[202:302]

acvf <- acf(training.set, type = "covariance", lag.max = 20)

acvf.matrix <- toeplitz(c(acvf$acf[1:20]))
acvf.sol <- acvf$acf[2:21]

a.solution <- solve(acvf.matrix,acvf$acf[2:21])

#3.84 lag 1

#now with the best linear predictor found. One can you to find the best predicted values

for (i in seq(202,302,1)){
  training.set[i] <- a.solution %*% rev(c(training.set[(i-20):(i-1)]))
}

to.test <- tibble("predictions" = training.set[202:302], "test" = test.set)


ggplot(to.test, aes(x=1:101)) +
  geom_line(aes(y = predictions, colour='predictions')) + 
  geom_line(aes(y=test.set, colour = "test.set")) +
  geom_line(aes(y=0, colour = "mean")) +
  scale_colour_manual("", 
                      breaks = c("predictions","test.set","mean"),
                      values = c("predictions"="royalblue", "test.set"="orange1", "mean"="green")) +
  labs(x="Time after month 201", y=TeX("$S_t$"))

for (i in seq(202,302,1)){
  training.set[i] <- psi * training.set[i-1]
}


s <- 0

best.predictors <- training.set[202:302]
real.results <- test.set

for (j in seq(1,101,1)){
  difference <- (best.predictors[j] - real.results[j])
  difference <- difference^2
  s <- s + difference
}

s2 <- 0

for(j in seq(1,101,1)){
  s2 <- s2 + real.results[j]^2
}

# BELOW SHOWS THAT THE NAIVE APPROACH PROVIDES BETTER RESULTS. 
mse <- s/101
mse.naive <- s2/101
mse.naive <- (1/101)*sum(real.results^2)

sprintf("Mean Square for Best-Predictor: %f. ", mse)
sprintf("Mean Square for Naive Approach: %f. ", mse.naive)


#### task 5

acvf.0 <- (Z.sigma2)/(1-(psi*psi))

acvf.ar1 <- function(phi,sigma,h){
  ((sigma^2)*phi^(abs(h)))/(1-(phi^(2)))
}

theoretical.acvf <- NULL
theoretical.acvf[1] <- acvf.0

for (i in seq(1,20)){
  theoretical.acvf[i+1] <- acvf.ar1(psi,Z.sigma,i)
}

theoryacvf.matrix <- toeplitz(theoretical.acvf[1:20])
acvf.vec <- theoretical.acvf[2:21]

theory.asol <- solve(theoryacvf.matrix,acvf.vec)
theory.asol2 <- solve(theoryacvf.matrix, -acvf.vec)

training.set <- S_t[1:201]
test.set <- S_t[202:302]

for (i in seq(202,302,1)){
  training.set[i] <- theory.asol2 %*% rev(c(training.set[(i-20):(i-1)]))
}

for (i in seq(202,302,1)){
  s <- 0
  for (j in seq(1,20,1)){
    s <- s + theory.asol2[j]*training.set[i-j]
  }
  training.set[i] <- s
}

train.predictions <- training.set[202:302]

to.test['parametric predictions']<- train.predictions

to.test <- tibble("predictions" = train.predictions, "test" = test.set)

ggplot(to.test, aes(x=1:101)) + 
  geom_line(aes(y=predictions)) +
  geom_line(aes(y=test))

ggplot(to.test, aes(x=1:101)) +
  geom_line(aes(y = predictions, colour='predictions')) + 
  geom_line(aes(y=test, colour = "test")) +
  geom_line(aes(y= `parametric predictions`, colour="parametric predictions")) +
  geom_line(aes(y=0, colour="mean")) +
  scale_colour_manual("", 
                      breaks = c("predictions","test", "parametric predictions","mean"),
                      values = c("predictions"="royalblue", "test"="orange1", "parametric predictions" = "red", "mean" = "green")) +
  labs(x="Time", y=TeX("$S_t"))


s.theory <- 0

best.predictors <- training.set[202:302]
real.results <- test.set

for (j in seq(1,101,1)){
  difference <- (best.predictors[j] - real.results[j])
  difference <- difference^2
  s <- s + difference
}

mse.theory <- s/101

