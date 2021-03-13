options(digits = 2)
underlying_asset=function(P0,u,d,t)
{
  P=c()
  P[1]=P0
  X=matrix(0,nrow=t,ncol=t)
  for(a in 1:t)
  {
    for(b in 1:t){
      if(a-b>0){X[a,b]=0}
      else{X[a,b]=P0*u^(b-a)*d^(a-1)}
    }
    
  }
  return(X)
}


underlying_asset=underlying_asset(1,1.1,1/1.1 ,53)
underlying_asset=round(underlying_asset,2)
underlying_asset
#-------------------------------------------------------------

#options(digits = 3)
#onedown=function(P0,u,d,t){X=matrix(0,nrow=t,ncol=t)
#X[,ncol(X)]=pmax(0,1-underlying_asset[,ncol(underlying_asset)])

#for(j in (t-1):1){
 # for(i in 1:j){
  #  X[i,j]=X[i,j+1]*u +X[i+1,j+1]*d
  #}
#}
#return(X)}

#onedown=onedown(1,0.476,0.524,53)
#onedown

options(digits = 4)
onedown=function(P0,u,d,t){X=matrix(0,nrow=t,ncol=t)
X[,ncol(X)]=pmax(0,1-underlying_asset[,ncol(underlying_asset)])
for(j in (t-1):1){
  for(i in 1:j){X[i,j]=pmax(X[i,j+1]*u+X[i+1,j+1]*d,(1-underlying_asset[i,j]))}
  
}

return(X)}
onedown=onedown(1,0.476,0.524,53)
onedown

M1=matrix(0,nrow=53,ncol=53)
for(a in 52:1){
  for(b in 1:a){if(as.integer(onedown[b,a])==as.integer((1-as.integer(underlying_asset[b,a])))){M1[b,a]=onedown[b,a]}}
}
M1
M1=c(M1)
M1


twodown=matrix(0,nrow=53,ncol=53)
twodown[,53]=onedown[,53]
for(a in 52:1){
  for(b in 1:a){
    twodown[b,a]=pmax(twodown[b,a+1]*0.476+twodown[b+1,a+1]*0.524,onedown[b,a]+(1-underlying_asset[b,a]))
  }
}
twodown



M2=matrix(0,nrow=53,ncol=53)
for(a in 52:1){
  for(b in 1:a){if(as.integer(twodown[b,a])==as.integer(onedown[b,a])+(1-as.integer(underlying_asset[b,a]))){M2[b,a]=twodown[b,a]}}
}
M2
M2=c(M2)
M2
max(M2)#twodown's highest exercise price


threedown=matrix(0,nrow=53,ncol=53)
threedown[,1:51]=twodown[,1:51]
for(a in 51:1){
  for(b in 1:a){
    threedown[b,a]=pmax(threedown[b,a+1]*0.476+threedown[b+1,a+1]*0.524,twodown[b,a]+(1-underlying_asset[b,a]))
  }
}
threedown

M3=matrix(0,nrow=53,ncol=53)
for(a in 52:1){
  for(b in 1:a){if(as.integer(threedown[b,a])==as.integer(twodown[b,a])+(1-as.integer(underlying_asset[b,a]))){M3[b,a]=threedown[b,a]}}
}
M3
M3=c(M3)
max(M3)#threedown's highest exercise price


fourdown=matrix(0,nrow=53,ncol=53)
fourdown[,1:50]=threedown[,1:50]
for(a in 50:1){
  for(b in 1:a){
    fourdown[b,a]=pmax(fourdown[b,a+1]*0.476+fourdown[b+1,a+1]*0.524,threedown[b,a]+(1-underlying_asset[b,a]))
  }
}
fourdown

M4=matrix(0,nrow=53,ncol=53)
for(a in 52:1){
  for(b in 1:a){if(as.integer(fourdown[b,a])==as.integer(fourdown[b,a])+(1-as.integer(underlying_asset[b,a]))){M4[b,a]=fourdown[b,a]}}
}
M4
M4=c(M4)
max(M4)#fourdown's highest exercise price
