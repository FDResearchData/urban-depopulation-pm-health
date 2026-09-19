# Run from the public-package root: Rscript Code/fig1.R
suppressPackageStartupMessages({library(data.table);library(ggplot2);library(patchwork);library(sf)})
d <- fread('Data/source_data/fig1.csv')
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

a <- d[panel=='a']; setnames(a,c('estimate','group','marker'),c('population_change_percent','plot_value','ever_shrinkage_pop'))
map <- join_boundary(a)
marker_xy <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(map)))
a[,`:=`(marker_x=marker_xy[,1],marker_y=marker_xy[,2])]
map$plot_value <- factor(map$plot_value,levels=c('< -10','-10 to < -5','-5 to < 0','0 to < 5','5 to < 10','>= 10'))
b <- d[panel=='b']; setnames(b,c('year','estimate'),c('Year','shrinkage_pop_sum'))
c <- d[panel=='c']; setnames(c,c('year','estimate','group','outcome'),c('年份','mean_concentration','status','pollutant_label'))
cols <- c('#2F6480','#6F9CB4','#BDD1DC','#E8B7AD','#CA7868','#8F3B32')
base <- theme_classic(base_family='Helvetica',base_size=9)+theme(
 panel.border=element_rect(colour='#B8B8B8',fill=NA,linewidth=.22),axis.line=element_blank(),
 axis.ticks=element_line(colour='#707070',linewidth=.25),axis.text=element_text(size=11,colour='#303030'),
 axis.title=element_text(size=11),strip.text=element_text(size=11),strip.background=element_blank(),
 legend.text=element_text(size=11),legend.title=element_text(size=11),plot.margin=margin(12,4,6,4))
p1a <- ggplot(map)+geom_sf(aes(fill=plot_value),colour='#B5B5B5',linewidth=.11)+
 map_layers()+
 geom_point(data=a[ever_shrinkage_pop==TRUE],aes(marker_x,marker_y),inherit.aes=FALSE,shape=21,size=2.0,stroke=.4,fill='white',colour='#1F1F1F')+
 scale_fill_manual(values=cols,drop=FALSE,na.value='#EEEEEE')+
 labs(fill='Resident population change (%)')+theme_void(base_family='Helvetica')+theme(legend.position='inside',legend.position.inside=c(.022,.105),legend.justification=c(0,0),legend.background=element_rect(fill='white',colour=NA))+
 theme(legend.text=element_text(size=11),legend.title=element_text(size=11),legend.key.height=grid::unit(4,'mm'),plot.margin=margin(12,4,6,4))
p1a <- p1a + ggspatial::annotation_north_arrow(
 location='tl',which_north='grid',width=grid::unit(.8,'cm'),height=grid::unit(.8,'cm'),
 pad_x=grid::unit(.22,'cm'),pad_y=grid::unit(.22,'cm'),
 style=ggspatial::north_arrow_orienteering(line_width=.5,text_size=9,text_family='Helvetica'))+
 ggspatial::annotation_scale(location='bl',style='ticks',unit_category='metric',plot_unit='m',
 width_hint=.34,line_width=.8,line_col='#303030',text_col='#202020',
 height=grid::unit(2,'mm'),pad_x=grid::unit(2,'mm'),pad_y=grid::unit(2,'mm'),
 text_pad=grid::unit(1.2,'mm'),text_cex=.9,text_family='Helvetica')
p1b <- ggplot(b,aes(Year,shrinkage_pop_sum))+geom_line(colour='#3E6F89',linewidth=.65)+geom_point(colour='#3E6F89',size=1.45)+
 scale_x_continuous(breaks=c(2011,2014,2017,2020))+scale_y_continuous(breaks=scales::pretty_breaks(4),expand=expansion(mult=c(.04,.12)))+
 labs(x=NULL,y='Depopulating cities')+base
c[,status:=factor(status,levels=c('Depopulating','Other'))]
c[,pollutant_label:=factor(pollutant_label,levels=c('PM[1]','PM[2.5]','PM[10]'))]
p1c <- ggplot(c,aes(年份,mean_concentration,colour=status,linetype=status))+geom_line(linewidth=.60)+
 facet_wrap(~pollutant_label,nrow=1,scales='free_y',labeller=label_parsed)+
 scale_colour_manual(values=c(Depopulating='#3E6F89',Other='#D28A48'))+
 scale_linetype_manual(values=c(Depopulating='solid',Other='22'))+
 scale_x_continuous(breaks=c(2011,2014,2017,2020),expand=expansion(mult=c(.06,.06)))+
 labs(x=NULL,y=expression(mu*g~m^{-3}),colour=NULL,linetype=NULL)+base+
 theme(axis.text.x=element_text(angle=45,hjust=1),legend.position='bottom',legend.margin=margin(0,0,0,0),panel.spacing.x=grid::unit(3,'mm'))
# Position every letter against its first panel bounding box, with identical offsets.
letter <- function(p,label) {
 g <- ggplotGrob(p); ids<-which(grepl('^panel',g$layout$name)); top<-min(g$layout$t[ids]); left<-min(g$layout$l[ids])
 gtable::gtable_add_grob(g,grid::textGrob(label,x=grid::unit(0,'npc'),y=grid::unit(1,'npc')+grid::unit(3,'mm'),just=c('left','bottom'),gp=grid::gpar(fontfamily='Helvetica',fontsize=13.1,fontface='bold')),t=top,l=left,clip='off')
}
fig <- wrap_elements(full=letter(p1a,'a')) | (wrap_elements(full=letter(p1b,'b')) / wrap_elements(full=letter(p1c,'c')))
fig <- fig+plot_layout(widths=c(.57,.43))

ggsave('Output/fig1.png',fig,width=280,height=185,units='mm',dpi=300,device=ragg::agg_png,bg='white')
