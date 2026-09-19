# Run from the public-package root: Rscript code/fig3.R
suppressPackageStartupMessages({library(data.table);library(ggplot2);library(patchwork)})
d <- fread('data/fig3.csv')
dir.create('Output',showWarnings=FALSE)
cols <- c('Direct'='#3E6F89','Local'='#3E6F89','Connected-city'='#D28A48','Total'='#756BA8')
shapes <- c('Direct'=16,'Local'=16,'Connected-city'=15,'Total'=17)
base <- theme_classic(base_family='Helvetica',base_size=10)+theme(panel.border=element_rect(colour='#B8B8B8',fill=NA,linewidth=.22),axis.line=element_blank(),axis.text=element_text(colour='#303030'),legend.title=element_blank(),legend.position='bottom',plot.margin=margin(14,5,5,5),strip.background=element_blank(),strip.text=element_text(face='plain'))
letter <- function(p,l) p+annotate('text',x=-Inf,y=Inf,label=l,hjust=0,vjust=-1.05,size=3.9,fontface='bold')+coord_cartesian(clip='off')
d[panel %in% c('a','b'),group:=factor(group,levels=c('Direct','Connected-city','Total'))]
impact <- function(l,ylabel) letter(ggplot(d[panel==l],aes(group,estimate,colour=group,shape=group))+
 geom_hline(yintercept=0,linetype='22',colour='#707070',linewidth=.34)+
 geom_point(size=2.85)+geom_errorbar(aes(ymin=lower,ymax=upper),width=.17,linewidth=.64)+
 scale_x_discrete(limits=c('Direct','Connected-city','Total'))+scale_colour_manual(values=cols)+scale_shape_manual(values=shapes)+labs(x=NULL,y=ylabel)+base+theme(legend.position='none'),l)
a<-impact('a','Impact (percentage points)'); b<-impact('b','Impact (Theil index)')
coefplot<-function(l,order) {
 z<-d[panel==l];z[,outcome:=factor(outcome,levels=rev(order))]
 letter(ggplot(z,aes(estimate,outcome,colour=group,shape=group))+
 geom_vline(xintercept=0,linetype='22',colour='#707070',linewidth=.34)+
 geom_errorbar(aes(xmin=lower,xmax=upper),orientation='y',position=position_dodge(.48),width=.16,linewidth=.69)+
 geom_point(position=position_dodge(.48),size=2.8)+scale_colour_manual(values=cols,breaks=c('Local','Connected-city'))+scale_shape_manual(values=shapes,breaks=c('Local','Connected-city'))+
 scale_y_discrete(labels=function(x) {x<-gsub('SO2','SO₂',x);x<-gsub('NOx','NOₓ',x);x<-gsub('Industrial soot emissions','Industrial soot\nemissions',x);x<-gsub('Industrial air-emission composite','Industrial air-emission\ncomposite',x);x<-gsub('High-emission firm entry','High-emission\nfirm entry',x);gsub('Emission-weighted entry','Emission-weighted\nentry',x)})+
 labs(x='Coefficient (95% interval)',y=NULL)+base+theme(axis.text.y=element_text(hjust=1),legend.box.spacing=grid::unit(1,'pt')),l)
}
cc<-coefplot('c',c('SO2 emissions','NOx emissions','Industrial soot emissions','Industrial air-emission composite'))
e<-coefplot('e',c('High-emission firm entry','Emission-weighted entry'))
z<-d[panel=='d'];z[,outcome:=factor(outcome,levels=c('SO[2]','NO[x]',"'Industrial soot'","'Air-emission composite'"))]
means<-z[,.(estimate=mean(estimate),ymin=min(y)-.5,ymax=max(y)+.5),by=.(group,outcome)]
dd<-ggplot(z,aes(estimate,y,colour=group))+geom_vline(xintercept=0,linetype='22',colour='#707070',linewidth=.34)+geom_point(size=1.2)+
 geom_segment(data=means,aes(x=estimate,xend=estimate,y=ymin,yend=ymax),linetype='22',linewidth=.5)+
 geom_text(data=means,aes(x=estimate,y=ymax+2,label=sprintf('Mean %.2f',estimate)),size=3)+
 facet_wrap(~outcome,nrow=1,scales='free_x',labeller=label_parsed)+
 scale_colour_manual(values=c('Depopulating cities'='#3E6F89','Highly connected non-depopulating cities'='#D28A48'))+
 labs(x='Change in industrial emissions (million kg)',y=NULL)+base+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
# a/b first row, c/e second row, d across the bottom.
dd<-dd+labs(tag='d')+theme(plot.tag=element_text(face='bold',size=11),plot.tag.position=c(0,1))
fig <- (a|b)/(cc|e)/dd+plot_layout(heights=c(1,1.1,1.3))

ggsave('Output/fig3.png',fig,width=260,height=250,units='mm',dpi=300,device=ragg::agg_png,bg='white')
