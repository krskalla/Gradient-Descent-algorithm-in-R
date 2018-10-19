# Gradient-Descent-algorithm-in-R
Compare Gradient Descent with OLS

```{r}
#==============================Batch Gradient descent algorithm========================
N = 1000  # Number of records
ncol=5 # Number of independent varoables.
x = matrix(rnorm(N*ncol,0,1),ncol=ncol) # Generating independent variables.
y = rnorm(N)*0.5 # Genarating dependent variable.
#plot(x,y) # Observing the parrettern between one independent and one dependent variable.
iters = 10000 # Number of iterations.
theta = rep(0.5,ncol) # Coefficient values.
alpha=1 # Learning rate.
cost = function(act_y,exp_y) # Defining cost dunction.
{
  return(1/(2*N)*sum((exp_y-act_y)^2))
}

batch_gd<-function(x,y,iters,theta,alpha)
{
  theta = matrix(theta,ncol=1)
  for(i in 1:iters)
  {
    y_hat = x%*%theta
    cost_hist[i] = cost(y,y_hat)
    theta = theta-alpha*(1/length(y))*t(x)%*%y_hat
  }
  return(list(cost_hist,theta))
}
out= batch_gd(x,y,iters=iters,theta=theta,alpha=alpha)

plot(out[[1]],type="l")


#==============================mini-Batch  & stochatic Gradient descent algorithm========================


N = 1000
ncol=5
x = matrix(rnorm(N*ncol,0,1),ncol=ncol)
y = rnorm(N)*0.5
#plot(x,y)
#iters = 1000
theta = rep(0.5,ncol)
alpha=0.005

# If mini_match_size=1, then SGD performs, otherwise mini-batch performs.
mini_match_size = 2

cost = function(act_y,exp_y)
{
  return(1/(2*N)*sum((exp_y-act_y)^2))
}

steps = seq(0,N,mini_match_size)
steps = unique(c(steps,N))
cost_hist =NULL
batch_gd<-function(x,y,iters,theta,alpha)
{
  theta = matrix(theta,ncol=1)
#  for(i in 1:10)
 # {
    for(j in 1:(length(steps)-1))
    {
      pos = (steps[j]+1):(steps[j+1])
      y_hat = x[pos,]%*%theta
      cost_hist = c(cost_hist,cost(y[pos],y_hat))
      if(ncol>1)
      {theta = theta-alpha*(1/length(y))*t(x[pos,])%*%y_hat}else{
        theta = theta-alpha*(1/length(y))*t(x[pos,])*y_hat
      }

    }
  #}
  return(list(cost_hist,theta))
}
out= batch_gd(x,y,iters=iters,theta=theta,alpha=alpha)

plot(out[[1]],type="l")

```
