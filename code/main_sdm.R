# Run from the public-package root: Rscript code/main_sdm.R
suppressPackageStartupMessages({library(data.table);library(splm);library(spdep);library(MASS)})
d<-fread('data/city_level_data.csv');w<-fread('data/spatial_weights.csv')
codes<-w$city_id;W<-as.matrix(w[,-1]);dimnames(W)<-list(as.character(codes),as.character(codes))
stopifnot(nrow(d)==2810,nrow(W)==281,max(abs(rowSums(W)-1))<1e-12,all(diag(W)==0),!anyNA(d))
d[,city_id:=as.character(city_id)];setorder(d,city_id,year)
stopifnot(identical(sort(unique(d$city_id)),as.character(codes)))
R <- 5000L
eigenvalues <- eigen(W,only.values=TRUE)$values
stopifnot(max(abs(Im(eigenvalues)))<1e-8)
eigenvalues <- Re(eigenvalues)
rho_lower <- 1/min(eigenvalues)
rho_upper <- 1/max(eigenvalues)
stopifnot(rho_lower<0,rho_upper>0)
controls<-c('Impervious','UR','lnPGDP','Open','lnTech','ER','lnFixed','DZ','NDVI','Temperature','Precipitation','WindSpeed')
policies<-c('aq_phase1_post','aq_phase2_post','post_inspection','post_inspection_lag1','post_air_inspection','inspection_current_year','inspection_air_current_year')
for(v in c('Shrinkage_pop',controls,policies)) {
 d[,paste0('W_',v):=as.numeric(W %*% .SD[[1]]),by=year,.SDcols=v]
}
impact<-function(par) {
 S<-solve(diag(nrow(W))-par[1]*W,par[2]*diag(nrow(W))+par[3]*W)
 direct<-mean(diag(S));total<-mean(rowSums(S))
 c(direct=direct,indirect=total-direct,total=total)
}
dir.create('Output',showWarnings=FALSE);results<-list();coefficients<-list();diagnostics<-list()
for(model in c('primary','policy')) for(y in c('lnPM1','lnPM2_5','lnPM10')) {
 x<-c('Shrinkage_pop',controls,if(model=='policy')policies)
 formula<-reformulate(c(x,paste0('W_',x)),response=y)
 fit<-splm::spml(formula,data=d,index=c('city_id','year'),listw=spdep::mat2listw(W,style='W'),model='within',effect='twoways',lag=TRUE,spatial.error='none')
 b<-coef(fit);V<-as.matrix(vcov(fit));dimnames(V)<-list(names(b),names(b))
 terms<-c('lambda','Shrinkage_pop','W_Shrinkage_pop');point<-impact(b[terms])
 set.seed(20240615)
 draws <- MASS::mvrnorm(R,mu=b[terms],Sigma=V[terms,terms])
 sim <- t(apply(draws,1,function(z) {
  if(z[1]>rho_lower && z[1]<rho_upper) impact(z) else rep(NA_real_,3)
 }))
 n_rejected <- sum(!complete.cases(sim))
 sim <- sim[complete.cases(sim),,drop=FALSE]
 stopifnot(nrow(sim)>1,all(is.finite(sim)))
 se <- apply(sim,2,sd)
 lower <- apply(sim,2,quantile,0.025)
 upper <- apply(sim,2,quantile,0.975)
 ans<-data.table(model,outcome=y,effect=names(point),estimate=as.numeric(point),standard_error=se,p_value=2*pnorm(abs(point/se),lower.tail=FALSE),lower=lower,upper=upper,draws_requested=R,n_rejected=n_rejected,draws_used=nrow(sim),rho_lower=rho_lower,rho_upper=rho_upper)
 ans[,`:=`(percent=100*expm1(estimate),percent_lower=100*expm1(lower),percent_upper=100*expm1(upper))]
 key<-paste(model,y,sep='_');results[[key]]<-ans
 coefficients[[key]]<-data.table(model,outcome=y,term=names(b),estimate=as.numeric(b),standard_error=sqrt(diag(V)))
 yy<-d[[y]]-ave(d[[y]],d$city_id)-ave(d[[y]],d$year)+mean(d[[y]])
 diagnostics[[key]]<-data.table(model,outcome=y,N=nrow(d),within_r2=1-sum(residuals(fit)^2)/sum(yy^2),draws_requested=R,n_rejected=n_rejected,draws_used=nrow(sim),rho_lower=rho_lower,rho_upper=rho_upper)
 cat('Completed',key,'; accepted',nrow(sim),'of',R,'; rejected',n_rejected,'\n');fwrite(rbindlist(results),'Output/sdm_impacts.csv')
}
fwrite(rbindlist(coefficients),'Output/sdm_coefficients.csv')
fwrite(rbindlist(diagnostics),'Output/sdm_diagnostics.csv')