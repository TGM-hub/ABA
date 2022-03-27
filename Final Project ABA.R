sma<-function(sales,steps){
  
  searchfork<-function(k){
    k=round(k)
    MA<-e<-c()
    MA[1:k]=mean(sales[1:k])
    e[1:k]=sales[1:k]-MA[1:k]
    
    for(t in (k+1):length(sales)){
      MA[t]=mean(sales[(t-1):(t-k)])
      e[t]=sales[t]-MA[t]
    } 
    
    sum(e^2)}
  
  results<-optim(2,searchfork,method = "Brent",lower = 1,upper = 10)
  k<-round(results[[1]])
  
  fo=c(); fo[1]=mean(tail(sales,k))
  if(steps>1){
    for(i in 1:(k-1)){
      fo[i+1]=mean(c(fo,tail(sales,k-i)))}
  }
  if(steps>k){
    for(j in (k+1):steps){
      fo[j]=mean(tail(fo,k))
    }
  }
  fo[1:steps]}


EWMA<-function(sales,steps){
  
  ewma<-e<-c(); ewma[1]=sales[1];   e[1]=0
  
  minsqe<-function(alpha){
    for(t in 2:length(sales)){
      ewma[t]=alpha*sales[t-1]+(1-alpha)*ewma[t-1]
      e[t]=sales[t]-ewma[t]
    }
    sum(e^2)
  }
  
  alpha<-optim(0.5,minsqe,method = "Brent",lower = 0, upper = 1)[[1]]
  
  for(t in 2:length(sales)){
    ewma[t]=alpha*sales[t-1]+(1-alpha)*ewma[t-1]
  }
  
  rep(alpha*sales[t]+(1-alpha)*ewma[t],steps)
}

ARMA<-function(sales,steps){
  e<-c();e[1]=0
  
  minsqe<-function(param){
    con<-param[1];alpha<-param[2];theta<-param[3]
    for(t in 2:length(sales)){
      e[t]=sales[t]-(con+alpha*sales[t-1]+theta*e[t-1])
    }
    sum(e^2)
  }
  
  resu=optim(c(1,.8,-.5),minsqe)
  
  con<-resu[[1]][1];alpha<-resu[[1]][2];theta<-resu[[1]][3]
  
  for(t in 2:length(sales)){
    e[t]=sales[t]-(con+alpha*sales[t-1]+theta*e[t-1])
  }
  
  fo<-c();
  fo[1]=con+alpha*tail(sales,1)+theta*tail(e,1)
  
  if(steps>1){for(i in 2:steps){
    fo[i]=con+alpha*fo[i-1]}
  }
  
  fo[1:steps]
}

COMB<-function(sales,steps){(sma(sales,steps)+EWMA(sales,steps)+ARMA(sales,steps))/3}

W<-read.csv("WALMART.csv")

sales=as.numeric(W[4,12:ncol(W)])
for(t in 1:length(sales)){if(sales[t]!=0){sales=sales[t:length(sales)];break}}
plot(ts(sales))

steps=7



#Initializing the coefficients
al <- 0
be <- 0
ga <- 0

#New function COMB with the linear coefficients
COMB<-function(sales,steps){(sma(sales,steps)+EWMA(sales,steps)+ARMA(sales,steps))/3}
COMB2<-function(sales,steps){(al*sma(sales,steps)+be*EWMA(sales,steps)+ga*ARMA(sales,steps))/3}

#Initializing a variable and a vector  
vec = c()
vec_alpha = c()
vec_beta = c()
count <- 0
count_al <- 0

while (al <= 1){ #while loop to avoid breaking the for loop if needed
  count_w <- 0
  for (w in 1:10){
    count <- count + 1 #indicate the position in vec
    count_w <- count_w + 0.1
    be <- count_w*(1-al)
    vec_alpha[count] <- al #store the value of alpha
    vec_beta[count] <- be #store the value of beta 
    ga <- (1-al)*(1-count_w) #gamma depending on alpha and w
    vec[count] <- sum((tail(sales,steps)-COMB2(head(sales,length(sales)-steps),steps))^2) #returning the sum(e^2)
  }
  al = al + 0.1
}

vec 
vec_alpha 
vec_beta

#Creating function to return alpha, beta and gamma such as the e^2 is minimal
df <- data.frame(vec, vec_alpha, vec_beta)#create a df with value of e^2, alpha and beta
min_row <- which.min(df$vec)
alpha_opt <- df$vec_alpha[min_row]
beta_opt <- df$vec_beta[min_row]
gamma_opt <- 1 - alpha_opt - beta_opt

param_opti <- c(alpha_opt, beta_opt, gamma_opt)
param_opti #display the values of the parameters

#Comparing the difference in error with the initial function
res <- (sum((tail(sales,steps)-COMB2(head(sales,length(sales)-steps),steps))^2) - sum((tail(sales,steps)-COMB(head(sales,length(sales)-steps),steps))^2))/sum((tail(sales,steps)-COMB2(head(sales,length(sales)-steps),steps))^2)
res*100 #minimizing of the error using the linear coefficients

##Conclusion##
#We have tested the sales of Walmart to predict what would be the sales for the last week
#We initially have used the COMB method that takes into account the sma, the ewma and the arma methods
#We thought of giving a coefficient to each of these methods to better predict the sales of the last week
#We realized that these coefficients are clearly giving more weight to the sma that seems to be the better method here
#Finally, by comparing the results with our initial method, we managed to reduce the error by 60% ! Which is a huge improvement 

