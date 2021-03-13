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
#-------------------------------------------------------

oneup=function(P0,u,d,t){X=matrix(0,nrow=t,ncol=t)
X[,ncol(X)]=pmax(0,underlying_asset[,ncol(underlying_asset)]-P0)
for(j in (t-1):1){
  for(i in 1:j){X[i,j]=pmax(X[i,j+1]*u+X[i+1,j+1]*d,(underlying_asset[i,j]-1))}
  
}

return(X)}
oneup(1,0.476,0.524,53)

oneup=oneup(1,0.476,0.524,53)




options(digits = 2)
twoup=function(P0,pu,pd,t){X=matrix(0,nrow=t,ncol=t)
M1=matrix(0,nrow=t,ncol=t)
X[,ncol(X)]=pmax(0,underlying_asset[,ncol(underlying_asset)]-P0)
M1[,ncol(M1)]=pmax(0,0)
for( a in (t-1):1){
  for( b in (1:a)){
    if(((X[b,a+1]*pu+X[b+1,a+1]*pd)-(as.integer(oneup[b,a])+(underlying_asset[b,a]-1))<0)){ M1[b,a]=oneup[b,a]+(underlying_asset[b,a]-1)}
    else{M1[b,a]=0}
    X[b,a]=pmax(X[b,a+1]*pu+X[b+1,a+1]*pd,oneup[b,a]+(underlying_asset[b,a]-1))
    
  }
}
output=list(X=X,M1=M1)
return(output)}


twoup=twoup(1,0.476,0.524,53)
twoup
twoup_price=twoup$X
twoup_M1=twoup$M1

twoup_v=c()
i=1
for( a in 52:1){
  for( b in (1:a)){
    if(((twoup_price[b,a+1]*0.476+twoup_price[b+1,a+1]*0.524)-(oneup[b,a]+(underlying_asset[b,a]-1))<0)){twoup_v[i]=oneup[b,a]+(underlying_asset[b,a]-1)}
    i=i+1
  }}
twoup_v
twoup_v=na.omit(twoup_v)
twoup_v





threeup=function(P0,pu,pd,t){X=matrix(0,nrow=t,ncol=t)
M2=matrix(0,nrow=t,ncol=t)
X[,53]=twoup_price[,53]
M2[,ncol(M2)]=pmax(0,0)
for( a in (52:1)){
  for( b in (1:a)){
    if(((twoup_price[b,a+1]*pu+twoup_price[b+1,a+1]*pd)-(oneup[b,a]+(underlying_asset[b,a]-1))<0)){X[b,a]=twoup_price[b,a]}
    else{
      X[b,a]=pmax(X[b,a+1]*pu+X[b+1,a+1]*pd,twoup_price[b,a]+(underlying_asset[b,a]-1))}
  }
}
output=list(X=X,M2=M2)
return(output)}
threeup=threeup(1,0.476,0.524,53)
threeup
threeup_price=threeup$X
threeup_M2=threeup$M2

threeup_v=c()
i=1
for( a in 52:1){
  for( b in (1:a)){
    if(((threeup_price[b,a+1]*0.476+threeup_price[b+1,a+1]*0.524)-(twoup_price[b,a]+(underlying_asset[b,a]-1))<0)){threeup_v[i]=twoup_price[b,a]+(underlying_asset[b,a]-1)}
    i=i+1
  }}
threeup_v
sort(threeup_v)







twoup_M1[,53]=twoup_price[,53]
for( a in (52:1)){
  for( b in (1:a)){
    threeup_M2[b,a]=twoup_M1[b,a]}}
threeup_M2
for( a in (52:1)){
  for( b in (1:a)){
    if(as.integer(threeup_M2[b,a]==0)){threeup_M2[b,a]=max(threeup_M2[b,a+1]*0.476+threeup_M2[b+1,a+1]*0.524,threeup_price[b,a]+(underlying_asset[b,a]-1))}}}
threeup_M2

fourup=threeup_M2
fourup_price=fourup

fourup_v=c()
i=1
for( a in 52:1){
  for( b in (1:a)){
    if(((fourup_price[b,a+1]*0.476+fourup_price[b+1,a+1]*0.524)-(threeup_price[b,a]+(underlying_asset[b,a]-1))<0)){fourup_v[i]=threeup_price[b,a]+(underlying_asset[b,a]-1)}
    i=i+1
  }}
fourup_v
sort(threeup_v)
