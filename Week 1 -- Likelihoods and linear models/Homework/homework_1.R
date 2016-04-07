#devtools::install_github("nwfsc-assess/geostatistical_deltaGLMM")
#Install Libraries
library(SpatialDeltaGLMM)
library(leaflet)
library(ggplot2)
library(TMB)
setwd('C:/Users/jrfried/Documents/2016_Spatio-temporal_models/Week 1 -- Likelihoods and linear models/Homework/')

#Load Data
data( EBS_pollock_data )
data = EBS_pollock_data

#Plot Counts
ggplot(data) + 
  geom_point(aes(x=long,y=lat,color=catch+1)) + 
  scale_color_gradient(low='white',high='blue',trans='log',name="Log Catch + 1")


##Prep Data for TMB
X = cbind( "Intercept"=rep(1,length(data$catch)) )

# Step 0 -- make and compile template file
compile( "homework_1.cpp" )

# Step 1 -- divide into partitions
K = 10
Partition_i = sample( x=1:K, size=length(data$catch), replace=TRUE )
PredNLL_k = rep(NA, K)

# Step 2 --Loop through partitions
for(k in 1:K){
  for (mod in c(0,1)) {
  dyn.load( dynlib("homework_1") )
  Params = list("b_j"=rep(0,ncol(X)), "theta_z"=c(0,0))
  
  Data = list( "y_i"=data$catch, "X_ij"=X, predTF_i=ifelse(Partition_i==k,1,0),model=mod )
  Obj = MakeADFun( data=Data, parameters=Params, DLL="homework_1")
  Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr )
  Report = Obj$report()
  PredNLL_k[k] = Report$pred_jnll
}
}

# log-Predictive probability per datum
mean( PredNLL_k / table(Partition_i) )


