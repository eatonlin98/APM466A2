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


underlying_asset=underlying_asset(1,1.1,1/1.1 ,52)
underlying_asset=round(underlying_asset,2)
underlying_asset
#-------------------------------------------------------
install.packages("spray")
library(spray)
install.packages("Rmpfr")
library(Rmpfr)


oneup=function(P0,u,d,t){X=matrix(0,nrow=t,ncol=t)
X[,ncol(X)]=pmax(0,underlying_asset[,ncol(underlying_asset)]-P0)

for(j in (t-1):1){
  for(i in 1:j){
    X[i,j]=X[i,j+1]*u +X[i+1,j+1]*d
  }
}
return(X)}

oneup=oneup(1,0.5,0.5,52)
oneup

options(digits = 2)
twoup=function(P0,pu,pd,t){X=matrix(0,nrow=t,ncol=t)
M1=matrix(0,nrow=t,ncol=t)
  X[,ncol(X)]=pmax(0,underlying_asset[,ncol(underlying_asset)]-P0)
  M1[,ncol(M1)]=pmax(0,0)
  for( a in (t-1):1){
    for( b in (1:a)){
      if(((X[b,a+1]*pu+X[b+1,a+1]*pd)-(oneup[b,a]+(underlying_asset[b,a]-1))<0)){ M1[b,a]=oneup[b,a]+(underlying_asset[b,a]-1)}
      else{M1[b,a]=0}
      X[b,a]=pmax(X[b,a+1]*pu+X[b+1,a+1]*pd,oneup[b,a]+(underlying_asset[b,a]-1))
      
    }
  }
output=list(X=X,M1=M1)
  return(output)}


twoup=twoup(1,0.5,0.5,52)
twoup
twoup_price=twoup$X
twoup_M1=twoup$M1



M11=matrix(0,nrow=52,ncol=52)
for (i in 1:52){
  for (j in 1:52){if(twoup_M1[j,i]>0){ for(a in i:52){M11[,a:52]=twoup_price[,a:52]}}
  }
}


M11
ls1=c()
k=1
for (i in 1:52){
  for (j in 1:52){if(M11[j,i]>0){ls1[k]=i}
    k=k+1
  }
}
ls1=na.omit(ls1)
ls1=length(min(ls1):52)
ls1



threeup=function(P0,pu,pd,t){X=matrix(0,nrow=t,ncol=t)
M2=matrix(0,nrow=t,ncol=t)
X[,(ncol(X)-1):ncol(X)]=twoup_price[,(ncol(twoup_price)-1):ncol(twoup_price)]
M2[,ncol(M2)]=pmax(0,0)
for( a in (t-ls1):1){
  for( b in (1:a)){
    if(((X[b,a+1]*pu+X[b+1,a+1]*pd)-(twoup_price[b,a]+(underlying_asset[b,a]-1))<0)){ M2[b,a]=twoup_price[b,a]+(underlying_asset[b,a]-1)}
    else{M2[b,a]=0}
    X[b,a]=pmax(X[b,a+1]*pu+X[b+1,a+1]*pd,twoup_price[b,a]+(underlying_asset[b,a]-1))
    
  }
}
output=list(X=X,M2=M2)
return(output)}
threeup=threeup(1,0.5,0.5,52)
threeup
threeup_price=threeup$X
threeup_M2=threeup$M2



M2=matrix(0,nrow=52,ncol=52)
for (i in 1:52){
  for (j in 1:52){if(threeup_M2[j,i]>0){ for(a in i:52){M2[,a:52]=threeup_price[,a:52]}}
  }
}


M2
ls2=c()
k=1
for (i in 1:52){
  for (j in 1:52){if(M2[j,i]>0){ls2[k]=i}
    k=k+1
  }
}
ls2=na.omit(ls2)
ls2=length(min(ls2):52)
ls2


fourup=function(P0,pu,pd,t){X=matrix(0,nrow=t,ncol=t)
M3=matrix(0,nrow=t,ncol=t)
X[,(ncol(X)-2):ncol(X)]=threeup_price[,(ncol(threeup_price)-2):ncol(threeup_price)]
M3[,ncol(M3)]=pmax(0,0)
for( a in (t-3):1){
  for( b in (1:a)){
    if(((X[b,a+1]*pu+X[b+1,a+1]*pd)-(threeup_price[b,a]+(underlying_asset[b,a]-1))<0)){ M3[b,a]=threeup_price[b,a]+(underlying_asset[b,a]-1)}
    else{M3[b,a]=0}
    X[b,a]=pmax(X[b,a+1]*pu+X[b+1,a+1]*pd,threeup_price[b,a]+(underlying_asset[b,a]-1))
    
  }
}
output=list(X=X,M3=M3)
return(output)}
fourup=fourup(1,0.5,0.5,52)
fourup
















options(digits = 2)




onedown=function(P0,u,d,t){X=matrix(0,nrow=t,ncol=t)
X[,ncol(X)]=pmax(0,1-underlying_asset[,ncol(underlying_asset)])

for(j in (t-1):1){
  for(i in 1:j){
    X[i,j]=X[i,j+1]*u +X[i+1,j+1]*d
  }
}
return(X)}

onedown=onedown(1,0.5,0.5,52)
onedown=round(onedown,2)
onedown



twodown=function(P0,pu,pd,t){X=matrix(0,nrow=t,ncol=t)
M1=matrix(0,nrow=t,ncol=t)
X[,ncol(X)]=pmax(0,1-underlying_asset[,ncol(underlying_asset)])
M1[,ncol(M1)]=pmax(0,0)
for( a in (t-1):1){
  for( b in (1:a)){
    if(((X[b,a+1]*pu+X[b+1,a+1]*pd)-(onedown[b,a]+(1-underlying_asset[b,a]))<0)){ M1[b,a]=onedown[b,a]+(1-underlying_asset[b,a])}
    else{M1[b,a]=0}
    X[b,a]=pmax(X[b,a+1]*pu+X[b+1,a+1]*pd,onedown[b,a]+(1-underlying_asset[b,a]))
    
  }
}
output=list(X=X,M1=M1)
return(output)}


twodown=twodown(1,0.5,0.5,52)

#format(twodown, digits = 1) 
twodown
twodown_price=twodown$X
twodown_M1=twodown$M1



length(which(twodown_M1[,3]>0))>0
M11=matrix(0,nrow=52,ncol=52)
for (i in 1:52){
  for (j in 1:52){if(twodown_M1[j,i]>0){ for(a in i:52){M11[,a]=twodown_price[,a]}}
  }
}


M11
ls11=c()
k=1
for (i in 1:52){
  for (j in 1:52){if(M11[j,i]>0){ls11[k]=i}
    k=k+1
  }
}
ls11=na.omit(ls11)
ls11=length(min(ls11):52)
ls11

threedown=function(P0,pu,pd,t){X=matrix(0,nrow=t,ncol=t)
M2=matrix(0,nrow=t,ncol=t)
X[,(ncol(X)-44):ncol(X)]=twodown_price[,(ncol(twodown_price)-44):ncol(twodown_price)]
M2[,ncol(M2)]=pmax(0,0)
for( a in (t-45):1){
  for( b in (1:a)){
    if(((X[b,a+1]*pu+X[b+1,a+1]*pd)-(twodown_price[b,a]+(1-underlying_asset[b,a]))<0)){ M2[b,a]=twodown_price[b,a]+(1-underlying_asset[b,a])}
    else{M2[b,a]=0}
    X[b,a]=pmax(X[b,a+1]*pu+X[b+1,a+1]*pd,twodown_price[b,a]+(1-underlying_asset[b,a]))
    
  }
}
output=list(X=X,M2=M2)
return(output)}
threedown=threedown(1,0.5,0.5,52)
threedown
threedown_price=threedown$X
threedown_M2=threedown$M2


length(which(threedown_M2[,3]>0))>0
M22=matrix(0,nrow=52,ncol=52)
for (i in 1:52){
  for (j in 1:52){if(threedown_M2[j,i]>0){ for(a in i:52){M22[,a]=threedown_price[,a]}}
  }
}


M22
ls22=c()
k=1
for (i in 1:52){
  for (j in 1:52){if(M22[j,i]>0){ls22[k]=i}
    k=k+1
  }
}
ls22=na.omit(ls22)
ls22
identical(ls22,NULL)
ls22=length(min(ls22):52)
ls22

fourdown=function(P0,pu,pd,t){X=matrix(0,nrow=t,ncol=t)
M3=matrix(0,nrow=t,ncol=t)
X[,(ncol(X)-48):ncol(X)]=threedown_price[,(ncol(threedown_price)-48):ncol(threedown_price)]
M3[,ncol(M3)]=pmax(0,0)
for( a in (t-49):1){
  for( b in (1:a)){
    if(((X[b,a+1]*pu+X[b+1,a+1]*pd)-(threedown_price[b,a]+(1-underlying_asset[b,a]))<0)){ M3[b,a]=threedown_price[b,a]+(1-underlying_asset[b,a])}
    else{M3[b,a]=0}
    X[b,a]=pmax(X[b,a+1]*pu+X[b+1,a+1]*pd,threedown_price[b,a]+(1-underlying_asset[b,a]))
    
  }
}
output=list(X=X,M3=M3)
return(output)}
fourdown=fourdown(1,0.5,0.5,52)
fourdown
