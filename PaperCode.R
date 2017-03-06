#Coordinate Grid Transformation
library(ggplot2)
library(gstat)
library(sp)
library(maptools)
library(rgdal)
library(RPostgreSQL)
library(DBI)
library(jsonlite)
library(RColorBrewer)
library(sp)
library(rgeos)

dbport=5435
if (exists('con')){
  dbDisconnect(con)
}
con <- dbConnect(PostgreSQL(), user= "citypulse", password="qPoRD3Zk", dbname="routedb", port=dbport, host="localhost")
mydsn="PG:dbname='routedb'  host=localhost user=citypulse password=qPoRD3Zk port=5435"
#ogrListLayers(mydsn)

getNearestNodeId <- function(con,x,y){
  sql = paste("select id from ways_vertices_pgr order by st_distance(the_geom, st_setsrid(ST_GeomFromText('Point(", x," ",y, ")'), 4326)) limit 1", sep="")
  response = dbSendQuery(con,sql)
  way=dbFetch(response,n=-1)
  return (way$id[1]);
}
getRoutingWayGeneric <- function(con,trg, src){
  print(paste(trg,src,goal))
  sql = paste("SELECT *, st_astext(the_geom) as geometrytext FROM ways JOIN(",
              "SELECT * FROM pgr_astar( 'SELECT gid as id, source, target, cost, reverse_cost, x1, y1, x2, y2 FROM ways'",
              ",",trg,",",src,",false",")) AS route ON ways.gid = edge", sep=" ")
  
  response = dbSendQuery(con,sql)
  way=dbFetch(response,n=-1)
  return (way);
}

full_x_min = 10.068
full_x_max = 10.35585
full_y_min = 56.08001
full_y_max = 56.252
full_x_delta = full_x_max-full_x_min
full_y_delta = full_y_max-full_y_min
x_min=full_x_min +full_x_delta*0.45 
x_max=full_x_max -full_x_delta*0.50 
y_min=full_y_min +full_y_delta*0.50 
y_max=full_y_max -full_y_delta*0.45

temp_table_name= 'temp_osm_table'
#Drop
str_drop_view=paste('drop view ', temp_table_name)
dbSendQuery(con, str_drop_view)
#Create
sql=paste('SELECT * FROM ways WHERE the_geom && ST_MakeEnvelope(',x_min,',', y_min,',', x_max,',',y_max,', 4326);')
str_create_view = paste("CREATE view ",temp_table_name," AS", sql)
dbSendQuery(con, str_create_view)
way_network = readOGR(dsn=mydsn,layer=temp_table_name, verbose = TRUE,stringsAsFactors=FALSE)
plot(way_network)
way_network@data[c('gid','source','target','cost','osm_id','one_way')]

way_network_fortify <- merge(fortify(way_network),as.data.frame(way_network), by.x="id", by.y=0)

one_way_legend <- c('Bi-Directional','Uni-Directional Lane','One Way Street')
way_network_fortify$one_way[way_network_fortify$one_way == 0] <- one_way_legend[1]
way_network_fortify$one_way[way_network_fortify$one_way == 1] <- one_way_legend[2]
way_network_fortify$one_way[way_network_fortify$one_way == 2] <- one_way_legend[3]

one_way_legend_colors <- c('#000000','#FF0000','#0000FF')#,brewer.pal(5,"Set3"))

gg <-ggplot(way_network_fortify)+
  geom_path(aes(x=long,y=lat,group=group,colour=one_way))+ 
  scale_colour_manual(name = "Street Type",values = one_way_legend_colors)+
  theme_bw()+ theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle = 45, hjust = .9))+
  theme(axis.text.y=element_text(angle = 45, vjust = .5))+
  xlim(min=10.195,max=10.215)
gg
#ggsave('./figures/one_way_coordinates.pdf',gg,width = 14,height=14 ,units='cm')

way_network_fortify$arrow <- way_network_fortify$one_way
way_network_fortify$arrow[way_network_fortify$arrow == one_way_legend[1]] <- 0
way_network_fortify$arrow[way_network_fortify$arrow == one_way_legend[2]] <- 30
way_network_fortify$arrow[way_network_fortify$arrow == one_way_legend[1]] <- 30


gg2 <-ggplot()+
  geom_path(data=way_network_fortify,aes(x=long,y=lat,group=group,colour=one_way)
            ,arrow = arrow(angle = 30, length = unit(0.1, "cm"), ends = "last", type = "closed")
  )+ 
  scale_colour_manual(name = "Street Type",values = one_way_legend_colors)+
  theme_bw()+ theme(legend.position="bottom")+
  theme(axis.text.y=element_text(angle = 45, vjust = .5))+
  coord_cartesian(xlim =  c(10.197, 10.2125), ylim=c(56.1660,56.1740))+
  xlab("Longitude")+  ylab("Latitude")
gg2
#ggsave('./figures/one_way_coordinates_2.pdf',gg2,width = 14,height=14 ,units='cm')

dat_orig <- read.table(text="   Location  symbol  lat   long
Origin   1    56.170    10.204
                       Destination      8    56.174     10.198", header=TRUE, stringsAsFactors=FALSE)
dat <- dat_orig


gg3 <-ggplot()+
  geom_path(data=way_network_fortify,aes(x=long,y=lat,group=group,colour=one_way)
            ,arrow = arrow(angle = 30, length = unit(0.1, "cm"), ends = "last", type = "closed")
  )+ 
  geom_point(data=dat,aes(x=long,y=lat), size=3,color='#00ff00')+
  geom_text(data=dat,aes(x=long,y=lat,label=Location))+
  scale_colour_manual(name = "Street Type",values = one_way_legend_colors)+
  theme_bw()+ theme(legend.position="bottom")+
  #theme(axis.text.y=element_text(angle = 45, vjust = .5))+
  coord_cartesian(xlim =  c(10.197, 10.2125), ylim=c(56.1660,56.1740))+
  xlab("Longitude")+  ylab("Latitude")
gg3

gg_origin <-ggplot()+
  geom_path(data=way_network_fortify,aes(x=long,y=lat,group=group,colour=one_way)
            ,arrow = arrow(angle = 30, length = unit(0.1, "cm"), ends = "last", type = "closed")
  )+ 
  geom_point(data=dat,aes(x=long,y=lat), size=5,color='#00ff00')+
  geom_text(data=dat,aes(x=long,y=lat,label=Location),nudge_x=-0.0002, nudge_y=-0.0002,size=5)+
  scale_colour_manual(name = "Street Type",values = one_way_legend_colors)+
  theme_bw()+ theme(legend.position="bottom")+
  #theme(axis.text.y=element_text(angle = 45, vjust = .5))+
  coord_cartesian(xlim =  c(10.202, 10.206), ylim=c(56.168,56.172))+
  xlab("Longitude")+  ylab("Latitude")
gg_origin

sql="WITH
    pt as (SELECT 'SRID=4326;POINT(10.204 56.170)'::geometry as geo), 
line as (SELECT st_astext(the_geom),gid,source,target,the_geom from temp_osm_table)
SELECT
ST_x(ST_ClosestPoint(line.the_geom,pt.geo)) As cp_line_pt_x, ST_y(ST_ClosestPoint(line.the_geom,pt.geo)) As cp_line_pt_y,
ST_Distance(ST_ClosestPoint(line.the_geom,pt.geo),pt.geo) as distance,
line.*
from line,pt
ORDER BY distance ASC
"
queryData <- dbSendQuery(con, sql)
data_select <- fetch(queryData, n=-1)
#data_select
dsplot <- data_select[1,c('distance','cp_line_pt_x','cp_line_pt_y','gid','source','target')]
dsplot$label=paste('ClosestPointOnEdge', dsplot$gid)
gg_nearest <- gg_origin+
  geom_text(data=dsplot,aes(x=cp_line_pt_x,y=cp_line_pt_y,label=label),nudge_x=0.0002, nudge_y=0.0002,size=5)+
  geom_point(data=dsplot,aes(x=cp_line_pt_x,y=cp_line_pt_y), color="#00AAAA", size=5)
gg_nearest
#ggsave('./figures/closest_point.pdf',gg_nearest,width = 14,height=14 ,units='cm')
#ggsave('./figures/closest_point_s.pdf',gg_nearest,width = 12,height=12 ,units='cm')

nearest_edge_gid = dsplot$gid
nearest_edge_point_x = dsplot$cp_line_pt_x
nearest_edge_point_y = dsplot$cp_line_pt_y

sql_cut_edge = paste("With ",
                     "cutgeo as",
                     "(SELECT st_intersection(st_buffer('SRID=4326;POINT(",nearest_edge_point_x,"",nearest_edge_point_y,")'::geometry,0.00001), the_geom) as intersection", 
                     "from ways where ways.gid=",dsplot$gid,"),",
                     "origline as (SELECT the_geom as geo from ways where ways.gid=",dsplot$gid,"), ",
                     "cutpoint as (SELECT st_buffer('SRID=4326;POINT(",nearest_edge_point_x,"",nearest_edge_point_y,")'::geometry,0.00001) as geo)",
                     "Select st_astext(cutgeo.intersection),st_astext(cutpoint.geo), st_astext(origline.geo)  from cutgeo,origline,cutpoint")
queryData_cutEdge <- dbSendQuery(con, sql_cut_edge)
data_select_cut_edge <- fetch(queryData_cutEdge, n=-1)
data_select_cut_edge
p1 =  readWKT(data_select_cut_edge[2])
l1 =  readWKT(data_select_cut_edge[3])
c1 =  readWKT(data_select_cut_edge[1])
plot(l1)
plot(c1,add=TRUE,col='red')#,add=true,color='red')
plot(p1,add=TRUE)
l1_coords = l1@lines[[1]]@Lines[[1]]@coords
l1_vector <- l1_coords[1,]-l1_coords[nrow(l1_coords),]
ortho_coordinates = data.frame(c(nearest_edge_point_x-l1_vector['y'],nearest_edge_point_x,nearest_edge_point_x+l1_vector['y']*0.001),
                               c(nearest_edge_point_y+l1_vector['x'],nearest_edge_point_y,nearest_edge_point_y-l1_vector['x']*0.001))
colnames(ortho_coordinates)=c('x','y')
ortho_wkt= paste0('SRID=4326;LINESTRING(',ortho_coordinates[1,1]," ",ortho_coordinates[1,2],",",ortho_coordinates[2,1]," ",ortho_coordinates[2,2],",",ortho_coordinates[3,1]," ",ortho_coordinates[3,2],')')
#ortho_wkt



sql_cut_edge = paste0("With ",
                      "cutgeo as ",
                      "(SELECT st_Difference(the_geom,'",ortho_wkt,"'::geometry) as intersection ", 
                      "from ways where ways.gid=",dsplot$gid,"), ",
                      "origline as (SELECT the_geom as geo from ways where ways.gid=",dsplot$gid,"), ",
                      "ortholine as (SELECT '",ortho_wkt,"'::geometry as geo) ", 
                      "Select st_astext(cutgeo.intersection),st_astext(ortholine.geo), st_astext(origline.geo), st_length(cutgeo.intersection::geography) from cutgeo,origline,ortholine")
queryData_cutEdge <- dbSendQuery(con, sql_cut_edge)
data_select_cut_edge <- fetch(queryData_cutEdge, n=-1)
#data_select_cut_edge
c2 =  readWKT(data_select_cut_edge[1])
p2 =  readWKT(data_select_cut_edge[2])
l2 =  readWKT(data_select_cut_edge[3])
length = data_select_cut_edge[4]
#length
plot(l2)
plot(c2,add=TRUE,col='red')#,add=true,color='red')
plot(p2,add=TRUE,col='purple')

line_1=Line(c2@lines[[1]]@Lines[[1]]@coords)
line_2=Line(c2@lines[[1]]@Lines[[2]]@coords)
lines_1 = Lines(list(line_1),ID='a')
lines_2 = Lines(list(line_2),ID='b')
line_cut=p2@lines[[1]]
line_cut@ID='c'
slines=SpatialLines(list(lines_1,lines_2,line_cut))
slengthdf <- SpatialLinesDataFrame(slines,data.frame(z=c(c('a','b','c')),row.names = c('a','b','c')))
proj4string(slengthdf)<-CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
slengthdf@data[,c('length')] <- SpatialLinesLengths(spTransform(slengthdf, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")))
#plot(spTransform(slengthdf, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")))
slengthdf@data
fortify_slengthdf <- fortify(slengthdf)
fortify_slengthdf <-merge(fortify(slengthdf),as.data.frame(slengthdf), by.x="id", by.y=0)
fortify_slengthdf$group <- as.character(fortify_slengthdf$group)
fortify_slengthdf$group[fortify_slengthdf$group == 'a.1'] <- paste0(nearest_edge_gid,'a')
fortify_slengthdf$group[fortify_slengthdf$group == 'b.1'] <- paste0(nearest_edge_gid,'b')
fortify_slengthdf$group[fortify_slengthdf$group == 'c.1'] <- 'cut_edge'
fortify_slengthdf$group <- as.factor(fortify_slengthdf$group)

gg_cut  = ggplot(fortify_slengthdf,aes(x=long, y= lat)) + 
  geom_path(aes(group=group,color=group),arrow=arrow())+#+geom_text(label='a')
  scale_colour_manual(name = "LINESTRING",values = c('#FF0000','#FFAA00','#FF00FF'))+
  theme_bw()+ theme(legend.position="bottom")+
  theme(axis.text.x=element_text(angle = 45, hjust = .9))+
  theme(axis.text.y=element_text(angle = 45, vjust = .5))
gg_cut
ggsave('./figures/closest_point_cut.pdf',gg_cut,width = 14,height=14 ,units='cm')
ggsave('./figures/closest_point_cut_s.pdf',gg_cut,width = 12,height=12 ,units='cm')




library(igraph)
library(plyr)
raw_graph <- way_network@data[,c('source','target','gid','one_way','length_m','x1','x2','y1','y2')]
mutuals = raw_graph[raw_graph$one_way==0,]
colnames(mutuals)<-c("target","source","gid","one_way","length_m","x2","x1","y2","y1")
mutuals=mutuals[,c("source", "target",   "gid", "one_way",  "length_m",       "x1",       "x2",       "y1",       "y2")]
raw_graph_mut <- rbind(raw_graph,mutuals)

graph <- graph.data.frame(raw_graph_mut, directed=TRUE)
E(graph)$color <- as.character(E(graph)$one_way)
E(graph)$color <- revalue(E(graph)$color, c('0'='#000000','1'='#0000FF','2'='#FF0000'))
#E(graph)$arrow.mode<-(E(graph)$one_way>0)


#plot(graph,vertex.size=1, vertex.label=NA, edge.arrow.width=.7,edge.arrow.size=.7)

selected_edge_ids = as.character(c(39250,52740,39251,35655,39252,18625, 39253,7592,39254))
selected_graph = subgraph.edges(graph=graph, eids=which(is.element(E(graph)$gid,selected_edge_ids)), delete.vertices = TRUE)

selected_graph$layout<-layout_components#layout.fruchterman.reingold
V(selected_graph)$color='#FFFFFF'

osp1= SpatialPoints(cbind(10.204,56.170 ))
osp2= SpatialPoints(cbind(10.2041048, 56.170))
osp3= SpatialPoints(cbind(10.2042958457167, 56.1700789954532))
osp1a= SpatialPoints(cbind(10.2042958457167, 56.1700789954532))
osp2a= SpatialPoints(cbind(10.2042958457167, 56.1704178))
osp3a= SpatialPoints(cbind(10.204323, 56.1699773))

d1= spDistsN1(osp1,osp1a,longlat = TRUE)*1000
d2 = spDistsN1(osp2,osp2a,longlat = TRUE)*1000
d3 = spDistsN1(osp3,osp3a,longlat = TRUE)*1000

extended_graph <- graph + vertex("Origin", color='green')
extended_graph <- extended_graph + vertex("Cutpoint",  color='green')
extended_graph <- extended_graph +edge("Origin", "Cutpoint",one_way=0,color='#000000',  label="StartEdge", length_m=d1,gid='StartEdge',x1=10.204,x2=10.2042958457167,y1=56.170,y2=56.1700789954532)
extended_graph <- extended_graph +edge("26405","Cutpoint",one_way=2,color='#FF0000',  label ='39252a',length_m=d2,gid='39252a',x1=10.2041048,x2=10.2042958457167,y1=56.170,y2=56.1704178)
extended_graph <- extended_graph +edge("Cutpoint","17770",one_way=2,color='#FF0000',  label = '39252b',length_m=d3,gid='39252b',x1=10.2042958457167,x2=10.204323,y1=56.1700789954532,y2=56.1699773)
E(extended_graph)$weight=E(extended_graph)$length_m


head = head_of(extended_graph,E(extended_graph))
tail = tail_of(extended_graph,E(extended_graph))
x2s = E(extended_graph)$x2
y2s = E(extended_graph)$y2

#Should be replaced with an apply version, sorry for the for-clause. 
for (i in 1:length(head)){#slowFor
  name = head[i]$name
  id = which(V(extended_graph)$name==name)
  V(extended_graph)[id]$x=x2s[i]
  V(extended_graph)[id]$y=y2s[i]
  name = tail[i]$name
  id = which(V(extended_graph)$name==name)
  V(extended_graph)[id]$x=x2s[i]
  V(extended_graph)[id]$y=y2s[i]
}
#V(extended_graph)[[1]]
#plot(extended_graph)


selected_edge_ids_extended = as.character(c(39250,52740,39251,35655,39252,18625, 39253,7592,39254,'StartEdge','39252a','39252b'))
selected_graph_extended = subgraph.edges(graph=extended_graph, eids=which(is.element(E(extended_graph)$gid,selected_edge_ids_extended)), delete.vertices = TRUE)
l <- layout.fruchterman.reingold(selected_graph_extended) 

plot.igraph(selected_graph_extended, vertex.size=10,layout=l,asp=.5)
plot.igraph(selected_graph, vertex.size=10,layout=l[1:10,],asp=.5)


origin_id = which(names(V(extended_graph))=="Origin")
destination_id = which(names(V(extended_graph))=="18051")
shop = shortest.paths(extended_graph,origin_id, destination_id)#, output="both")
shop2 = shortest.paths(extended_graph, destination_id,origin_id)#, output="both")
#shop
#shop2
all_shop = all_shortest_paths(extended_graph,origin_id)
#all_shop$nrgeo
a1 = all_shop[[1]][[1]]
all_shortest_distances = igraph::distances(extended_graph,origin_id)
#colnames(all_shortest_distances)==V(extended_graph)$name

o= SpatialPoints(cbind(10.204, 56.170))
proj4string(o) <- CRS("+init=epsg:4326")
p1 = SpatialPoints(cbind(V(extended_graph)$x, V(extended_graph)$y))
proj4string(p1) <- CRS("+init=epsg:4326")
plot(o)
eucldists = spDistsN1(p1,o, longlat=TRUE)*1000
length(all_shortest_distances)
both_dists = rbind(all_shortest_distances,eucldists)
factors = 1/both_dists[2,]*both_dists[1,]
results = data.frame(t(rbind(both_dists,factors)))

results$v_name=row.names(results)
results$x = V(extended_graph)$x
results$y = V(extended_graph)$y

results_filter1=results[!(is.infinite(results$factors)),]
results_filter=results_filter1[results_filter1$factors>1,]
colnames(results_filter)=c('shortest_path','euclidean','Coefficient','v_name','lat','lon')
g4_1 = ggplot(results_filter,aes(Coefficient))+stat_ecdf()
g4_1
#ggsave('./figures/Coefficient_half.pdf',g4_1,width = 7,height=7 ,units='cm')


g4_2 <-ggplot()+
  geom_path(data=way_network_fortify,aes(x=long,y=lat,group=group))+
  geom_point(data=results_filter,aes(lat,lon,colour=Coefficient),size=5)+
  scale_colour_gradient( low='blue',high="red",limits=c(1, max(results_filter$Coefficient)))+
  geom_point(data=dat[1,],aes(x=long,y=lat), size=3,color='#00ff00')+
  geom_text(data=dat[1,],aes(x=long,y=lat,label=Location),nudge_x=-0.0002, nudge_y=-0.0002,size=5)+
  #scale_colour_manual(name = "Street Type",values = one_way_legend_colors)+
  theme_bw()+ theme(legend.position="bottom")+
  #theme(axis.text.y=element_text(angle = 45, vjust = .5))+
  coord_cartesian(xlim =  c(10.197, 10.2125), ylim=c(56.1660,56.1740))+
  xlab("Longitude")+  ylab("Latitude")
g4_2
#ggsave('./figures/Coefficient_map.pdf',g4_2,width = 14,height=14 ,units='cm')
#spatstat::idw


