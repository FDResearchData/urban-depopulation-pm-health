# Run from the public-package root: Rscript Code/fig2.R
suppressPackageStartupMessages({library(data.table);library(ggplot2);library(patchwork)})
d <- fread('Data/source_data/fig2.csv')
dir.create('Output',showWarnings=FALSE)
labels <- c('Exclude Northeast','Exclude BTH','Exclude YRD','Exclude Guangdong','Exclude BTH + YRD','Province-by-year FE','Region-by-year FE','No time-varying covariates')
pm_titles <- list(expression(PM[1]),expression(PM[2.5]),expression(PM[10]))
setnames(d,c('model','group','specification','estimate','lower','upper'),c('figure_row','impact_type','specification_label','percent_effect','percent_ci_low','percent_ci_high'))
d[,impact_type:=factor(impact_type,levels=c('Direct','Connected-city','Total'))]
d[figure_row=='Robustness checks',y_pos:=(8:1)[match(specification_label,labels)]]
cols <- c('Direct'='#3E6F89','Connected-city'='#D28A48','Total'='#756BA8')
base <- theme_classic(base_size=8,base_family='Helvetica')+theme(
 panel.border=element_rect(colour='#B8B8B8',fill=NA,linewidth=.22),
 axis.line=element_blank(),axis.ticks=element_line(colour='#707070',linewidth=.25),
 axis.text=element_text(size=10,colour='#303030'),plot.title=element_text(hjust=.5,size=10),
 plot.tag=element_text(size=10,face='bold'),plot.tag.position=c(0,1),
 plot.margin=margin(6,4,6,4),legend.position='none')
main <- d[figure_row!='Robustness checks'];rr <- d[figure_row=='Robustness checks']
mlim <- c(floor(min(main$percent_ci_low)/5)*5,ceiling(max(main$percent_ci_high)/5)*5)
rlim <- c(floor(min(rr$percent_ci_low)/5)*5,ceiling(max(0,rr$percent_ci_high)/5)*5+2)
main_panels <- lapply(1:6,function(k){
 dt<-d[panel==letters[k]]
 ggplot(dt,aes(impact_type,percent_effect,ymin=percent_ci_low,ymax=percent_ci_high,colour=impact_type,shape=impact_type))+
 geom_hline(yintercept=0,colour='#707070',linewidth=.34,linetype='22')+
 geom_point(size=2.8,alpha=.65)+geom_errorbar(width=.185,linewidth=.69)+
 scale_colour_manual(values=cols)+scale_shape_manual(values=c(16,15,17))+
 scale_y_continuous(limits=mlim,breaks=seq(ceiling(mlim[1]/10)*10,0,10))+
 labs(x=NULL,y=NULL,
 title=pm_titles[[(k-1)%%3+1]],tag=NULL)+base+
 annotate("text",x=-Inf,y=Inf,label=letters[k],hjust=0,vjust=-1.05,size=3.9,fontface="bold",family="Helvetica")+coord_cartesian(clip="off")+
 theme(axis.text.x=element_text(size=9),axis.title.y=element_text(size=10,margin=margin(r=1)),
 axis.text.y=if(k %in% c(1,4)) element_text(size=10) else element_blank(),
 axis.ticks.y=if(k %in% c(1,4)) element_line(colour="#707070",linewidth=.25) else element_blank())+
 (if(k %in% c(1,4)) annotation_custom(grid::textGrob(
 "Estimated change in\nparticulate concentration (%)",x=grid::unit(0,"npc")-grid::unit(12.5,"mm"),
 y=.5,rot=90,gp=grid::gpar(fontfamily="Helvetica",fontsize=10))) else NULL)
})
robust_panels <- lapply(1:3,function(j){
 dt<-d[panel==letters[j+6]]
 ggplot(dt,aes(percent_effect,y_pos,xmin=percent_ci_low,xmax=percent_ci_high))+
 geom_vline(xintercept=0,colour='#707070',linewidth=.34,linetype='22')+
 geom_errorbar(orientation='y',width=0,linewidth=.5,colour=cols[['Connected-city']])+
 geom_point(size=2.3,shape=15,colour=cols[['Connected-city']])+
 scale_x_continuous(limits=rlim,breaks=seq(ceiling(rlim[1]/10)*10,0,10))+
 scale_y_continuous(limits=c(.4,8.6),breaks=8:1,labels=labels,expand=expansion(0))+
 labs(x='Connected-city estimate (%)',y=NULL,title=pm_titles[[j]],tag=NULL)+base+
 annotate("text",x=-Inf,y=Inf,label=letters[j+6],hjust=0,vjust=-1.05,size=3.9,fontface="bold",family="Helvetica")+coord_cartesian(clip="off")+
 theme(axis.text.y=if(j==1) element_text(size=9) else element_blank(),axis.title.x=element_text(size=10),axis.ticks.y=element_blank())
})
fig <- wrap_plots(c(main_panels,robust_panels),ncol=3,nrow=3,byrow=TRUE,heights=c(.9,.9,1.2))

ggsave('Output/fig2.png',fig,width=240,height=232,units='mm',dpi=300,device=ragg::agg_png,bg='white')
