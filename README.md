# Gradient-Descent-algorithm-in-R
Compare Gradient Descent with OLS
```{r}

```{r}
# Loading Gradient descent algorithm.
library(gradDescent)

# Definng cost function.
cost_f<-function(x,y,theta)
{
 return(1/2*(sum((x%*%theta-y)^2)))
}

# Model for optimal estimates.
linear_reg<-function(x,y,theta=NULL,learn=0.05,epsilon=0.00001,niter=5000)
{
  if(!is.matrix(x))
  x = as.matrix(x)
  
  if(!is.matrix(y))
  y = as.matrix(y)

  if(is.null(theta))
  {
     theta = matrix(rnorm(ncol(x)),nrow=ncol(x))
  }else{
    if(ncol(theta)!=1)
    theta<-matrix(theta,ncol=1)
  }

  theta_hist <- list()
  SSE<-cost_f(x,y,theta)
   
  for (i in 2:(niter+1))
  {
    error = (x %*% theta) -y
    delta = t(x) %*% error
    theta = theta-learn*(1/nrow(x))*delta
    SSE<-c(SSE,cost_f(x,y,theta))
    theta_hist[[i]]<-theta
    if((SSE[i-1]-SSE[i])<=epsilon)
    break
  }
  return(list(theta=round(theta[,1],4),SSE=SSE,theta_hist=theta_hist,Total_number_of_iterations=length(theta_hist)+1))
}

# Preparing mock up data.
x1 = rnorm(n = 10000, mean = 0, sd = 1)
x2 = rnorm(n = 10000, mean = 10, sd = 1)
x3 = rnorm(n = 10000, mean = 0, sd = 10)
x4 = rnorm(n = 10000, mean = 200, sd = 100)
x5 = rnorm(n = 10000, mean = 5, sd = 5)

# Scaling Independent variables.
x =scale(as.matrix( data.frame(x1,x2,x3, x4,x5)))
# Scaling depenadent variables.
y =scale(as.matrix( rnorm(n = 10000, mean = 0, sd = 1),ncol=1))

# Passing required parameters to the function.
out=linear_reg(x=cbind(Int=1,x),y=y,theta=NULL,learn=0.1,epsilon=0.000001,niter=5000)
# Above function provides Optimized estimates, Sum of Squares of error and theta eatimates for all the iterationss

out[[1]]
            [,1]
Int -0.001390472
x1  -0.009274992
x2  -0.020559408
x3  -0.003080449
x4  -0.029031844
x5  -0.018961002

df=data.frame(cbind(x,y=y))
names(df)[ncol(df)]<-"y"

m2<-lm(y~.,data=df)
> round(coef(m2),4)
(Intercept)          x1          x2          x3          x4          x5 
   0.000000   -0.006319    0.004955    0.011333    0.014451   -0.023478

library(gradDescent)

m1=GD(df,alpha=0.05,maxIter=5000,seed=NULL)
> round(m1,6)
     [,1]      [,2]     [,3]     [,4]     [,5]      [,6]
[1,]    0 -0.006319 0.004955 0.011333 0.014451 -0.023478

```
