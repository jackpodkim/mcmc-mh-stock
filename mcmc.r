getwd()
#import sets 
spy = read.csv('jack/JHU/7.Computational_stat/group/SPY.csv', header=T)
qqq = read.csv('jack/JHU/7.Computational_stat/group/QQQ.csv', header=T)


#look at sets
plot(as.Date(spy$Date), spy$Close )
plot(as.Date(qqq$Date), qqq$Close )

# hist((spy$Close-mean(spy$Close))/mean(spy$Close))
# hist((spy$Close-median(spy$Close))/median(spy$Close))

hist((log(spy$Close)-mean(log(spy$Close)))/mean(log(spy$Close)))
hist((log(qqq$Close)-mean(log(qqq$Close)))/mean(log(qqq$Close)), bin=30)

spy$log.mean.adj = (log(spy$Close)-mean(log(spy$Close)))/mean(log(spy$Close))
qqq$log.mean.adj = (log(qqq$Close)-mean(log(qqq$Close)))/mean(log(qqq$Close))


#get log return
log.return = log(spy$Close[1:(nrow(spy)-1)]/spy$Close[2:nrow(spy)]) # ln(t[i+1] / t[i])
spy$log.return = c(0, log.return)
log.return = log(qqq$Close[1:(nrow(qqq)-1)]/qqq$Close[2:nrow(qqq)]) # ln(t[i+1] / t[i])
qqq$log.return = c(0, log.return)

length(spy$log.return)


colnames(spy)
colnames(qqq)


hist(spy$log.return)
hist(qqq$log.return)
hist(spy$log.mean.adj)
hist(qqq$log.mean.adj)

########


# proposal density.... need to estimate
# log return density estimate
d = density(spy$log.return, bw='sj')
plot(d)
shapiro.test(spy$log.return)
plot(qnorm(ppoints(length(spy$log.return))), spy$log.return[order(spy$log.return)])
qqline(spy$log.return, col='red') # not normal distribution, very light tails

# density destimation
density.est.func = function(data){
    d = density(data, bw='sj') # density estimation, bandwidth=sj
    return(approxfun(d))
}

# SPY
par(mfrow=c(2,1))
d = density(spy$log.return, bw='sj')
plot(d)
test = sample(spy$log.return, 50)
prop.d = density.est.func(spy$log.return)
points(test, prop.d(test), col='red', cex=5)

d = density(spy$log.mean.adj, bw='sj')
plot(d)
test = sample(spy$log.mean.adj, 50)
prop.d = density.est.func(spy$log.mean.adj)
points(test, prop.d(test), col='red', cex=5)

# QQQ
par(mfrow=c(2,1))
d = density(qqq$log.return, bw='sj')
plot(d)
test = sample(qqq$log.return, 50)
prop.d = density.est.func(qqq$log.return)
points(test, prop.d(test), col='red', cex=5)

d = density(qqq$log.mean.adj, bw='sj')
plot(d)
test = sample(qqq$log.mean.adj, 50)
prop.d = density.est.func(qqq$log.mean.adj)
points(test, prop.d(test), col='red', cex=5)


### MH 

mh = function(x, min, max){
  x.t1 = runif(1, min, max) 
  r = min(1, prop.d(x.t1)/prop.d(x))
  if(r<1){
    ifelse(runif(1)<r, return(x.t1), return(x))
  }
  return(x.t1)
}

cusum_ = function(x){
  n = length(x)
  theta_hat = mean(x)
  pre_cusum = rep(0, n)
  for(i in 1:n){
    pre_cusum[i] = (x[i]-theta_hat)
  }
  return(cumsum(pre_cusum))
} # for cusum graph

# mixing test
mc.iter = 2000
mc = numeric(mc.iter)


### use SPY as sample
# 1. starting values (low 5%, mid, top95%), prop = log.return
st.val = unname(quantile(spy$log.return, c(0.01, 0.50, 0.99)))
sp.min = min(spy$log.return)
sp.max = max(spy$log.return)
prop.d = density.est.func(spy$log.return)
accept.rate = 0

accept.rate/mc.iter 

par(mfrow=c(3,1))

for(st in st.val){
  accept.rate = 0
  mc[1] = st # initial value
  for(i in 2:mc.iter){
      mc[i] = mh(mc[i-1], sp.min, sp.max)
      if(mc[i]!=mc[i-1]) accept.rate=accept.rate+1
  }
  accept.rate = accept.rate/mc.iter
  plot(mc, type='l', main=paste('start value= ', st, ' acceptance= ', accept.rate))
  plot(cusum_(mc), main=paste('start value= ', st, ' acceptance= ', accept.rate))
  acf(mc, lag.max=mc.iter, main=paste('start value= ', st, ' acceptance= ', accept.rate))
}



# 2. different proposals
# mixing test
mc.iter = 2000
mc = numeric(mc.iter)

# normal
prop.d = function(x) rnorm(1,x,1)

accept.rate = 0
mc[1] = 0 # initial value
sp.min = min(spy$log.return)
sp.max = max(spy$log.return)
for(i in 2:mc.iter){
    mc[i] = mh(mc[i-1], sp.min, sp.max)
    if(mc[i]!=mc[i-1]) accept.rate=accept.rate+1
}
accept.rate = accept.rate/mc.iter # 0.36

par(mfrow=c(3,1))
plot(mc, type='l', main=paste('proposal= normal, acceptance= ', accept.rate))
plot(cusum_(mc), main=paste('proposal= normal, acceptance= ', accept.rate))
acf(mc, lag.max=mc.iter, main=paste('proposal= normal, acceptance= ', accept.rate))

# log return
prop.d = density.est.func(spy$log.return)

accept.rate = 0
mc[1] = 0 # initial value
sp.min = min(spy$log.return)
sp.max = max(spy$log.return)
for(i in 2:mc.iter){
    mc[i] = mh(mc[i-1], sp.min, sp.max)
    if(mc[i]!=mc[i-1]) accept.rate=accept.rate+1
}
accept.rate = accept.rate/mc.iter # 0.36

par(mfrow=c(3,1))
plot(mc, type='l', main=paste('proposal= log return, acceptance= ', accept.rate))
plot(cusum_(mc), main=paste('proposal= log return, acceptance= ', accept.rate))
acf(mc, lag.max=mc.iter, main=paste('proposal= log return, acceptance= ', accept.rate))


# mean adj
prop.d = density.est.func(spy$log.mean.adj)

accept.rate = 0
mc[1] = 0 # initial value
sp.min = min(spy$log.mean.adj)
sp.max = max(spy$log.mean.adj)
for(i in 2:mc.iter){
    mc[i] = mh(mc[i-1], sp.min, sp.max)
    if(mc[i]!=mc[i-1]) accept.rate=accept.rate+1
}
accept.rate = accept.rate/mc.iter # 0.36

par(mfrow=c(3,1))
plot(mc, type='l', main=paste('proposal= log mean adj, acceptance= ', accept.rate))
plot(cusum_(mc), main=paste('proposal= log mean adj, acceptance= ', accept.rate))
acf(mc, lag.max=mc.iter, main=paste('proposal= log mean adj, acceptance= ', accept.rate))

### QQQ
# mixing test
mc.iter = 2000
mc = numeric(mc.iter)

# normal
prop.d = function(x) rnorm(1,x,1)

accept.rate = 0
mc[1] = 0 # initial value
sp.min = min(qqq$log.return)
sp.max = max(qqq$log.return)
for(i in 2:mc.iter){
    mc[i] = mh(mc[i-1], sp.min, sp.max)
    if(mc[i]!=mc[i-1]) accept.rate=accept.rate+1
}
accept.rate = accept.rate/mc.iter # 0.36

par(mfrow=c(3,1))
plot(mc, type='l', main=paste('proposal= normal, acceptance= ', accept.rate))
plot(cusum_(mc), main=paste('proposal= normal, acceptance= ', accept.rate))
acf(mc, lag.max=mc.iter, main=paste('proposal= normal, acceptance= ', accept.rate))

# log return
prop.d = density.est.func(qqq$log.return)

accept.rate = 0
mc[1] = 0 # initial value
sp.min = min(qqq$log.return)
sp.max = max(qqq$log.return)
for(i in 2:mc.iter){
    mc[i] = mh(mc[i-1], sp.min, sp.max)
    if(mc[i]!=mc[i-1]) accept.rate=accept.rate+1
}
accept.rate = accept.rate/mc.iter # 0.36

par(mfrow=c(3,1))
plot(mc, type='l', main=paste('proposal= log return, acceptance= ', accept.rate))
plot(cusum_(mc), main=paste('proposal= log return, acceptance= ', accept.rate))
acf(mc, lag.max=mc.iter, main=paste('proposal= log return, acceptance= ', accept.rate))


# mean adj
prop.d = density.est.func(qqq$log.mean.adj)

accept.rate = 0
mc[1] = 0 # initial value
sp.min = min(qqq$log.mean.adj)
sp.max = max(qqq$log.mean.adj)
for(i in 2:mc.iter){
    mc[i] = mh(mc[i-1], sp.min, sp.max)
    if(mc[i]!=mc[i-1]) accept.rate=accept.rate+1
}
accept.rate = accept.rate/mc.iter # 0.36

par(mfrow=c(3,1))
plot(mc, type='l', main=paste('proposal= log mean adj, acceptance= ', accept.rate))
plot(cusum_(mc), main=paste('proposal= log mean adj, acceptance= ', accept.rate))
acf(mc, lag.max=mc.iter, main=paste('proposal= log mean adj, acceptance= ', accept.rate))


# (r=risk free rate, t=time steps, z=random portion, Z~N(0,1))
# S = s0 * exp((rf-(sd^2/2))*t + sd*t^0.5*z)
# std = std of log(S)
# therefore, log transform the price

rf = 43.36/1000 # 10year treasury current 4.336%)
sp.sd = sd(log(spy$Close))
n = 100 #number of times we want to try MH
mc.iter = 2000
mc = numeric(mc.iter)

# brownian black scholes
S.t = function(s0,z,day,pred.len){
    delta.t = day/pred.len
    return(s0 * exp((rf-(sp.sd^2/2))*delta.t + sp.sd*delta.t^0.5*z))
}


# prediction
MC.SIM = function(ticker, price.data, burn.in.bool, pred.days){
  RMSE=numeric(n)
  ndata = length(price.data)
  # par(mfrow=c(1,1))
  for (k in 1:n){
    # pred.days = 252
    for(i in 2:mc.iter){
      mc[i] = mh(mc[i-1], sp.min, sp.max)
      if(mc[i]!=mc[i-1]) accept.rate=accept.rate+1
    }
    for(sim in 1:pred.days){
      s = numeric(pred.days)
      for(i in 1:pred.days){
        if(burn.in.bool==FALSE){
          s[i] = S.t(price.data[ndata-pred.days], mc[i],i, pred.days)
        }
        else{
          s[i] = S.t(price.data[ndata-pred.days], mc[i+length(mc)-pred.days],i, pred.days)
        }
      }      
    }
    RMSE[k]=sqrt(mean((price.data[(ndata-pred.days+1):ndata] - s)^2))
  }
  # xlim last 2 years
  plot(price.data,  xlim=c(ndata-252*2, ndata), main=paste(ticker, ' | Burn-in: ', burn.in.bool, ' | Prediction Days: ', pred.days, ' | RMSE: ', mean(RMSE)))
  lines(((ndata-pred.days)+1):ndata, s,col='red')
  print(paste("burn-in = ", burn.in.bool, " | Average RMSE predictions: ", mean(RMSE)))
  return(list(ticker=ticker, burn.in=burn.in.bool, pred.days=pred.days, rmse=mean(RMSE)))
}


#####
##### main loop
#####

result = data.frame(list(ticker=NULL, burn.in=NULL, pred.days=NULL, RMSE=NULL))
# 1,2,4,12 (year, 6mo, 3mo, 1mo)
tmp = c(1,2,4,12)
par(mfrow=c(4,2)) # 8 graphs


### MH parameters (proposal density, density sample min/max) needs to be set as global param
### SPY
# normal density
prop.d = function(x) rnorm(1,x,1)
sp.min = min(spy$log.return)
sp.max = max(spy$log.return)
for(div in tmp){
  result = rbind(result, MC.SIM('SPY', spy$Close, FALSE, 252/div))
  result = rbind(result, MC.SIM('SPY', spy$Close, TRUE, 252/div))
}
# dev.copy(png, filename='7.Computational_stat/group/spy_norm.png', width=800, height=1200)
# dev.off()


# estimated density on log return
prop.d = density.est.func(spy$log.return)
sp.min = min(spy$log.return)
sp.max = max(spy$log.return)
for(div in tmp){
  result = rbind(result, MC.SIM('SPY', spy$Close, FALSE, 252/div))
  result = rbind(result, MC.SIM('SPY', spy$Close, TRUE, 252/div))
}
# dev.copy(png, filename='7.Computational_stat/group/spy_lr.png', width=800, height=1200)
# dev.off()

# estimated density on log mean adjusted
prop.d = density.est.func(spy$log.mean.adj)
sp.min = min(spy$log.mean.adj)
sp.max = max(spy$log.mean.adj)
for(div in tmp){
  result = rbind(result, MC.SIM('SPY', spy$Close, FALSE, 252/div))
  result = rbind(result, MC.SIM('SPY', spy$Close, TRUE, 252/div))
}
# dev.copy(png, filename='7.Computational_stat/group/spy_ma.png', width=800, height=1200)
# dev.off()

### QQQ
# normal density
prop.d = function(x) rnorm(1,x,1)
sp.min = min(qqq$log.return)
sp.max = max(qqq$log.return)
for(div in tmp){
  result = rbind(result, MC.SIM('QQQ', qqq$Close, FALSE, 252/div))
  result = rbind(result, MC.SIM('QQQ', qqq$Close, TRUE, 252/div))
}
# dev.copy(png, filename='7.Computational_stat/group/qqq_norm.png', width=800, height=1200)
# dev.off()

# estimated density on log return
prop.d = density.est.func(qqq$log.return)
sp.min = min(qqq$log.return)
sp.max = max(qqq$log.return)
for(div in tmp){
  result = rbind(result, MC.SIM('QQQ', qqq$Close, FALSE, 252/div))
  result = rbind(result, MC.SIM('QQQ', qqq$Close, TRUE, 252/div))
}
# dev.copy(png, filename='7.Computational_stat/group/qqq_lr.png', width=800, height=1200)
# dev.off()

# estimated density on log mean adjusted
prop.d = density.est.func(qqq$log.mean.adj)
sp.min = min(qqq$log.mean.adj)
sp.max = max(qqq$log.mean.adj)
for(div in tmp){
  result = rbind(result, MC.SIM('QQQ', qqq$Close, FALSE, 252/div))
  result = rbind(result, MC.SIM('QQQ', qqq$Close, TRUE, 252/div))
}
# dev.copy(png, filename='7.Computational_stat/group/qqq_ma.png', width=800, height=1200)
# dev.off()

result$proposal = c(rep('normal', 8), rep('log return', 8), rep('mean adj', 8), rep('normal', 8), rep('log return', 8), rep('mean adj', 8))

result
# write.csv(result, "7.Computational_stat/group/result.csv") 



# burn-in
# sum((result[result$burn.in==TRUE, 4]-result[result$burn.in==FALSE, 4])^2)^0.5 / var(result$rmse)
result[result$burn.in==TRUE, 4]-result[result$burn.in==FALSE, 4]
sum((result[result$burn.in==TRUE, 4]-result[result$burn.in==FALSE, 4])>0)/(nrow(result)/2) # 0.4583333

sum(result[result$burn.in==TRUE, 4]-result[result$burn.in==FALSE, 4])
mean(result$rmse)

summary(lm(rmse~., data=result)) # pred.days p=4.02e-09

aov(lm(rmse~., data=result))

result

# proposal dist
aggregate(.~pred.days, result[result$ticker=='SPY',], min) # log return
aggregate(.~pred.days, result[result$ticker=='QQQ',], min) # log return

aggregate(.~pred.days, result[result$ticker=='SPY',], max) # normal
aggregate(.~pred.days, result[result$ticker=='QQQ',], max) # normal

result


