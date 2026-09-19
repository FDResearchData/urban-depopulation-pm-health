# Run from the public-package root: Rscript Code/health_analysis.R
d <- read.csv('Data/health_inputs.csv',stringsAsFactors=FALSE)
reference <- read.csv('Data/health_results.csv',stringsAsFactors=FALSE)
stopifnot(nrow(d)==7306,!anyNA(d),length(unique(d$city_id))==281)
paf <- function(pm) {
 z <- pmax(pm-d$c0,0)
 rr <- exp(d$theta*log1p(z/d$alpha)*plogis((z-d$mu)/d$nu))
 1-1/rr
}
contrast <- paf(d$pm25_no_depopulation)-paf(d$pm25_observed)
for (s in c('mortality_rate','mortality_lower','mortality_upper'))
 d[[s]] <- d$population*d[[s]]*contrast
city <- aggregate(d[c('mortality_rate','mortality_lower','mortality_upper')],d['city_id'],sum)
names(city)[2:4] <- c('deaths_avoided','lower','upper')
check <- merge(city,reference,by='city_id',suffixes=c('_new','_published'))
for (s in c('deaths_avoided','lower','upper'))
 stopifnot(max(abs(check[[paste0(s,'_new')]]-check[[paste0(s,'_published')]]))<1e-6)
national <- data.frame(deaths_avoided=sum(city$deaths_avoided),lower=sum(city$lower),upper=sum(city$upper))
stopifnot(abs(national$deaths_avoided-27315.6396159532)<1e-6)
dir.create('Output',showWarnings=FALSE)
write.csv(city,'Output/health_city_results.csv',row.names=FALSE)
write.csv(national,'Output/health_national_results.csv',row.names=FALSE)
print(national)