# Run from the public-package root: Rscript Code/fig4.R
suppressPackageStartupMessages({library(data.table);library(ggplot2);library(patchwork);library(sf)})
d <- fread('Data/source_data/fig4.csv')
dir.create('Output',showWarnings=FALSE)
crs <- '+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +datum=WGS84 +units=m +no_defs'
city_boundary <- sf::st_read('Data/city_boundaries.gpkg',quiet=TRUE)
city_boundary$city_id <- sprintf('%06d',as.integer(as.character(city_boundary$city_id)))
city_boundary <- sf::st_transform(city_boundary,crs)
join_boundary <- function(values) {
 values <- as.data.frame(values);values$city_id<-sprintf('%06d',as.integer(values$city_id))
 ix<-match(values$city_id,city_boundary$city_id)
 sf::st_sf(values,geometry=sf::st_geometry(city_boundary)[ix])
}
map_layers <- function() list(
 ggmapcn::geom_mapcn(admin_level='province',crs=crs,color='#5A5A5A',fill=NA,linewidth=.31),
 ggmapcn::geom_boundary_cn(crs=crs,mainland_color='#171717',mainland_size=.58,coastline_color='#171717',coastline_size=.58),
 coord_sf(crs=crs,expand=FALSE,datum=NA))

cols <- c('Direct'='#3E6F89','Local'='#3E6F89','Connected-city'='#D28A48','Total'='#756BA8')
shapes <- c('Direct'=16,'Local'=16,'Connected-city'=15,'Total'=17)
base <- theme_classic(base_family='Helvetica',base_size=10)+theme(panel.border=element_rect(colour='#B8B8B8',fill=NA,linewidth=.22),axis.line=element_blank(),axis.text=element_text(colour='#303030'),legend.title=element_blank(),legend.position='bottom',plot.margin=margin(14,5,5,5),strip.background=element_blank(),strip.text=element_text(face='plain'))
letter <- function(p,l) p+annotate('text',x=-Inf,y=Inf,label=l,hjust=0,vjust=-1.05,size=3.9,fontface='bold')+coord_cartesian(clip='off')
mapplot <- function(l,title,palette) {
 z<-d[panel==l];ranges<-unique(z[,.(group,estimate)]);lev<-z[,.(v=mean(estimate)),by=group][order(v)]$group
 z[,group:=factor(group,levels=lev)];z<-join_boundary(z)
 p<-ggplot(z)+geom_sf(aes(fill=group),colour='white',linewidth=.08)+map_layers()+
 scale_fill_manual(values=setNames(palette(length(lev)),lev),breaks=setdiff(lev,c('0','Zero')))+labs(fill=title)+
 theme_void(base_family='Helvetica')+theme(legend.position='inside',legend.position.inside=c(.01,.13),legend.justification=c(0,0),legend.text=element_text(size=10),legend.title=element_text(size=10),plot.margin=margin(12,4,4,4))
 p+labs(tag=l)+theme(plot.tag=element_text(face='bold',size=11),plot.tag.position=c(.02,.98))
}
a<-mapplot('a','Depopulation-associated\nmean PM₂.₅ change',colorRampPalette(c('#084594','#EFF3FF')))
b<-mapplot('b','Modeled fewer deaths',colorRampPalette(c('#FEE8C8','#B30000')))
barplot<-function(l) {
 z<-d[panel==l];if(l=='g') z<-z[order(match(group,c('Depopulating','Non-depopulating: zero connected exposure','Non-depopulating: low positive connected exposure','Non-depopulating: high positive connected exposure')))];z[,label:=ifelse(is.na(label)|label=='',group,label)]
 z[,label:=gsub('\\n','\n',label,fixed=TRUE)]
 z[,label:=factor(label,levels=rev(unique(label)))];z[,colour:=ifelse(grepl('^Depopulating',group),'#D28A48','#4E9094')]
 if(l=='g') z[,colour:=fcase(grepl('Depopulating',group),'#D28A48',grepl('Zero|zero',group),'#C5E2E0',grepl('Low|low',group),'#79B7B3',default='#286F73')]
 share<-l %in% c('d','e');z[,pct:=if(share) estimate else 100*estimate/27315.6396159532]
 p<-ggplot(z,aes(estimate,label))+geom_col(aes(fill=colour),width=.4)+scale_fill_identity()+
 labs(x=if(share)'Share of national estimate (%)' else 'Modeled fewer deaths',y=if(l=='c')'Age group' else NULL)+base+theme(legend.position='none',axis.text.y=element_text(hjust=1),axis.title.y=element_text(margin=margin(r=1)))
 if(!share)p<-p+geom_errorbar(aes(xmin=lower,xmax=upper),orientation='y',width=.2,linewidth=.8)
 if(l!='c')p<-p+geom_text(aes(x=ifelse(is.na(upper),estimate,upper),label=sprintf('%.1f%%',pct)),hjust=-.2,size=3.3)+scale_x_continuous(expand=expansion(mult=c(0,.25)))
 letter(p,l)
}
c<-barplot('c');dd<-barplot('d');e<-barplot('e');f<-barplot('f');g<-barplot('g')
# d/e/f/g category spacing is set by heights proportional to category counts.
# Explicit two-column lower section avoids matching the bottom of f and g.
left<-c/f+plot_layout(heights=c(13,2));right<-dd/e/g+plot_layout(heights=c(2,3,4))
fig<-(a|b)/(left|right)+plot_layout(heights=c(1.15,1.5))

ggsave('Output/fig4.png',fig,width=260,height=270,units='mm',dpi=300,device=ragg::agg_png,bg='white')
