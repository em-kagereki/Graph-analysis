
library(neo4r)
library(dplyr)
library(purrr)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(ggtext)
library(ggplot2)
library(plyr)
library(extrafont)
library(igraph)
library(ggbump)
library(ggraph)
library(magrittr)

####
#### 1. Connection
####
connect <- neo4j_api$new(url = "http://localhost:7474", 
                         db = "Tutorial", 
                         user = "neo4j", 
                         password = "Tutorial")

disease_genes <- 'merge (n1:Disease {name: row.Disease, identifier: row.Disease_ID})
merge (n2:Gene {name: row.Gene, identifier: row.Gene_ID})
merge (n1)-[r:Disease_Gene {type: row.Relation}]-> (n2);'
load_csv(url = "file:///disease_genes.csv", 
         con = connect, header = TRUE, periodic_commit = 50, 
         as = "row", on_load = disease_genes)

compound_disease <- 'merge (n1:Compound {name: row.Compound, identifier: row.Compound_ID})
merge (n2:Disease {name: row.Disease, identifier: row.Disease_ID})
merge (n1)-[r:Compound_Disease {type: row.Relation}]-> (n2);'
load_csv(url = "file:///compound_disease.csv", 
         con = connect, header = TRUE, periodic_commit = 50, 
         as = "row", on_load = compound_disease)

compound_gene <- 'merge (n1:Compound {name: row.Compound, identifier: row.Compound_ID})
merge (n2:Gene {name: row.Gene, identifier: row.Gene_ID})
merge (n1)-[r:Compound_Gene {type: row.Relation}]-> (n2);'
load_csv(url = "file:///compound_gene.csv", 
         con = connect, header = TRUE, periodic_commit = 50, 
         as = "row", on_load = compound_gene)

disease_anatomy <- 'merge (n1:Disease {name: row.Disease, identifier: row.Disease_ID})
merge (n2:Anatomy {name: row.Anatomy, identifier: row.Anatomy_ID})
merge (n1)-[r:Disease_Anatomy {type: row.Relation}]-> (n2);'
load_csv(url = "file:///disease_anatomy.csv", 
         con = connect, header = TRUE, periodic_commit = 50, 
         as = "row", on_load = disease_anatomy)

gene_anatomy <- 'merge (n1:Anatomy {name: row.Anatomy, identifier: row.Anatomy_ID})
merge (n2:Gene {name: row.Gene, identifier: row.Gene_ID})
merge (n1)-[r:Anatomy_Gene {type: row.Relation}]-> (n2);'
load_csv(url = "file:///gene_anatomy.csv", 
         con = connect, header = TRUE, periodic_commit = 50, 
         as = "row", on_load = gene_anatomy)

gene_bioprocess <- 'merge (n1:Gene {name: row.Gene, identifier: row.Gene_ID})
merge (n2:BiologicalProcess {name: row.BiologicalProcess, identifier: row.BiologicalProcess_ID})
merge (n1)-[r:Gene_BiologicalProcess {type: row.Relation}]-> (n2);'

load_csv(url = "file:///gene_bioprocess.csv", 
         con = connect, header = TRUE, periodic_commit = 50, 
         as = "row", on_load = gene_bioprocess)

gene_pathway <- 'merge (n1:Gene {name: row.Gene, identifier: row.Gene_ID})
merge (n2:Pathway {name: row.Pathway, identifier: row.Pathway_ID})
merge (n1)-[r:Gene_Pathway {type: row.Relation}]-> (n2);'

load_csv(url = "file:///gene_pathway.csv", 
         con = connect, header = TRUE, periodic_commit = 50, 
         as = "row", on_load = gene_pathway)
####
####2. Create the nodes
####

'match (n:Gene)
set n:Hetionet_Nodes;' %>% 
  call_neo4j(connect)
'match (n:Compound)
set n:Hetionet_Nodes;'%>% 
  call_neo4j(connect)
'match (n:Pathway)
set n:Hetionet_Nodes;'%>% 
  call_neo4j(connect)
'match (n:Anatomy)
set n:Hetionet_Nodes;'%>% 
  call_neo4j(connect)
'match (n:BiologicalProcess)
set n:Hetionet_Nodes;'%>%  
  call_neo4j(connect)
'match (n:Disease)
set n:Hetionet_Nodes;'%>% 
  call_neo4j(connect)

####
#### 3. Create new node properties to specify node type - e.g., Gene, Compound, Pathway, Anatomy, etc.
####

'match (n:Gene)
set n.type = "Gene";' %>% 
  call_neo4j(connect)
'match (n:Compound)
set n.type = "Compound";'%>%  
  call_neo4j(connect)
'match (n:Pathway)
set n.type = "Pathway";'%>% 
  call_neo4j(connect)
'match (n:Anatomy)
set n.type = "Anatomy";'%>% 
  call_neo4j(connect)
'match (n:BiologicalProcess)
set n.type = "BiologicalProcess";'%>% 
  call_neo4j(connect)
'match (n:Disease)
set n.type = "Disease";'%>% 
  call_neo4j(connect)

####
####4. Graph statistics
####

## Total connections:
nNodes <-'match (n:Hetionet_Nodes)
return count(n) as total_number_of_nodes;' %>%
  call_neo4j(connect)
nNodes <-nNodes$`total_number_of_nodes`[[1]] # Extract the element from the tibble!!!

## Total genes:
nGenes<-'match (n:Gene)
return count(n) as total_number_of_genes;' %>%
  call_neo4j(connect)
nGenes <-nGenes$total_number_of_genes[[1]] 

## Count Compound nodes:
nCompound<-'match (n:Compound)
return count(n) as total_number_of_compounds;' %>%
  call_neo4j(connect)
nCompound <-nCompound$total_number_of_compounds[[1]] 

## Count Disease nodes:
nDisease<-'match (n:Disease)
return count(n) as total_number_of_diseases;' %>%
  call_neo4j(connect)
nDisease <-nDisease$total_number_of_diseases[[1]] 

## Count BiologicalProcess nodes:
nBiologicalProcess<-'match (n:BiologicalProcess)
return count(n) as total_number_of_BiologicalProcess;' %>%
  call_neo4j(connect)
nBiologicalProcess <-nBiologicalProcess$total_number_of_BiologicalProcess[[1]]

## Count Pathway nodes:
nPathway<-'match (n:Pathway)
return count(n) as total_number_of_Pathway;' %>%
  call_neo4j(connect)
nPathway <-nPathway$total_number_of_Pathway[[1]]

## Extract the Edges
Edges<-'match (:Hetionet_Nodes)-[r]-(:Hetionet_Nodes) return r;' %>%
  call_neo4j(connect)
Edges <-Edges$r 
write.csv(Edges, "Edges.csv")

## Extract the Nodes
Nodes <- "match (node1)-[r]-(node2) return node1.name,node2.name" %>%
  call_neo4j(connect)
Nodes <-cbind(Nodes$node1.name,Nodes$node2.name)

write.csv(Nodes, "Nodes.csv")  

## Total number of relations:
nRelationships<-'match (:Hetionet_Nodes)-[r]-(:Hetionet_Nodes)
return count(r);' %>%
  call_neo4j(connect)
nRelationships <-nRelationships$`count(r)`[[1]] 

## Total number of relations between Genes and Diseases:
gRd<-'match (:Gene)-[r]-(:Disease)
return count(r);' %>%
  call_neo4j(connect)
gRd <-gRd$`count(r)`[[1]] 

## Total number of relations between Compounds and Diseases:
cRd<-'match (:Compound)-[r]-(:Disease)
return count(r)' %>%
  call_neo4j(connect)
cRd <-cRd$`count(r)`[[1]] 

## Total number of relations between Disease and Anatomy:
dRa<-'match (:Disease)-[r]-(:Anatomy)
return count(r)' %>%
  call_neo4j(connect)
dRa <-dRa$`count(r)`[[1]] 

## Total number of relations between Anatomy and Gene:
aRg<-'match (:Anatomy)-[r]-(:Gene)
return count(r)' %>%
  call_neo4j(connect)
aRg <-aRg$`count(r)`[[1]] 

## Total number of relations between Gene and BiologicalProcess:
gRb<-'match (:Gene)-[r]-(:BiologicalProcess)
return count(r)' %>%
  call_neo4j(connect)
gRb <-gRb$`count(r)`[[1]] 

## Total number of relations between Gene and Pathway:
gRp<-'match (:Gene)-[r]-(:Pathway)
return count(r)' %>%
  call_neo4j(connect)
gRp <-gRp$`count(r)`[[1]] 

Aspects <- c('Nodes','Genes','Pathway','nBiologicalProcess','Relationships','nDisease','nCompound','gRd','dRa','aRg','gRb','gRp')
Statistics<- c(nNodes,nGenes,nPathway,nBiologicalProcess,nRelationships,nDisease,nCompound,gRd,dRa,aRg,gRb,gRp)
basicStats<-as.data.frame(cbind(Aspects,Statistics))
basicStats$Statistics<-as.numeric(basicStats$Statistics)
write.csv(basicStats, "basicStats.csv")



##  List the individual nodes

Genes<-'MATCH (n:Gene)
RETURN n.name;' %>% 
  call_neo4j(connect)
Genes<-data.frame(Genes)
Genes$name<-c("Gene")

Compound<-'MATCH (n:Compound)
RETURN n.name;' %>% 
  call_neo4j(connect)
Compound<-data.frame(Compound)
Compound$name<-c("Compound")

Pathway<-'MATCH (n:Pathway)
RETURN n.name;' %>% 
  call_neo4j(connect)
Pathway<-data.frame(Pathway)
Pathway$name<-c("Pathway")

Anatomy<-'MATCH (n:Anatomy)
RETURN n.name;' %>% 
  call_neo4j(connect)
Anatomy<-data.frame(Anatomy)
Anatomy$name<-c("Anatomy")

BiologicalProcess<-'MATCH (n:BiologicalProcess)
RETURN n.name;' %>% 
  call_neo4j(connect)
BiologicalProcess<-data.frame(BiologicalProcess)
BiologicalProcess$name<-c("BiologicalProcess")

Disease<-'MATCH (n:Disease)
RETURN n.name;' %>% 
  call_neo4j(connect)
Disease<-data.frame(Disease)
Disease$name<-c("Disease")

nodeList<-rbind(Genes,Compound,Pathway,Anatomy,BiologicalProcess,Disease)
names(nodeList)[1] <- "name"
names(nodeList)[2] <- "Category"

write.csv(nodeList,"nodeList.csv")

####
### 5 Calculate global graph stats
###

# Centrality disctribution

Stats<-"CALL gds.degree.stats({
  nodeProjection: 'Nodes',
  relationshipProjection: ['Disease_Gene', 'Disease_Anatomy', 'Compound_Gene','Anatomy_Gene','Gene_BiologicalProcess','Gene_Pathway']
  })
YIELD centralityDistribution
RETURN centralityDistribution"
Stats<-call_neo4j(Stats ,connect)
Stats<-data.frame(Stats$centralityDistribution)
write.csv(Stats,"Stats.csv")

## Betweeness
bet<-"call gds.betweenness.stream({
  nodeProjection: 'Nodes',
  relationshipProjection: ['Disease_Gene', 'Disease_Anatomy', 'Compound_Gene', 'Anatomy_Gene', 'Gene_BiologicalProcess', 'Gene_Pathway']
})
YIELD nodeId, score
return gds.util.asNode(nodeId).name AS name, gds.util.asNode(nodeId).type AS nodeType ,score;"
between<-call_neo4j(bet,connect)
between<-cbind(between$name,between$score)
names(between)[1] <- "name"
names(between)[2] <- "betweenness"
betweenMerge<-merge(between,nodeList, by="name")

write.csv(betweenMerge, "between.csv")

## Centrality
allCent<-'match (node1)-[r]-(node2)
return node1.name,count(r);'
allCent<-call_neo4j(allCent,connect)
allCent<-cbind(allCent$node1.name,allCent$`count(r)`)
names(allCent)[1] <- "name"
names(allCent)[2] <- "totalDegree"

inCent<-'match (node1)<-[r]-(node2)
return node1.name,count(r);'
inCent<-call_neo4j(inCent,connect)
inCent<-cbind(inCent$node1.name,inCent$`count(r)`)
names(inCent)[1] <- "name"
names(inCent)[2] <- "inDegree"

outCent<-'match (node1)-[r]->(node2)
return node1.name,count(r);'
outCent<-call_neo4j(outCent,connect)
outCent<-cbind(outCent$node1.name,outCent$`count(r)`)
names(outCent)[1] <- "name"
names(outCent)[2] <- "outDegree"

AllCentrality<-merge(x=nodeList, y=allCent, by="name", all.x = TRUE)
AllCentrality<-merge(x=AllCentrality, y=outCent, by="name", all.x = TRUE)
AllCentrality<-merge(x=AllCentrality, y=inCent, by="name", all.x = TRUE)

centrality<-"call gds.degree.stream({
  nodeProjection: 'Nodes',
  relationshipProjection: ['Disease_Gene', 'Disease_Anatomy', 'Compound_Gene','Anatomy_Gene','Gene_BiologicalProcess','Gene_Pathway']
  })
YIELD nodeId, score
return gds.util.asNode(nodeId).name AS name, score
order by score desc"
centrality<-call_neo4j(centrality,connect)
centrality<-cbind(centrality$name,centrality$score)
names(centrality)[1] <- "name"
names(centrality)[2] <- "centrality"
centralMerge<-merge(centrality,nodeList, by="name") 
centralMerge<-mutate(centralMerge, centralPercent=ntile(centralMerge$centrality,100))
write.csv(centralMerge, "centrality.csv")

## Closeness
close<-"call gds.alpha.closeness.stream({
  nodeProjection: 'Nodes',
  relationshipProjection: ['Disease_Gene', 'Disease_Anatomy', 'Compound_Gene', 'Anatomy_Gene', 'Gene_BiologicalProcess', 'Gene_Pathway']
  })
YIELD nodeId, centrality
return gds.util.asNode(nodeId).name AS name, gds.util.asNode(nodeId).type AS nodeType, centrality;"
closeness<-call_neo4j(close,connect)
closeness<-cbind(closeness$name,closeness$centrality)
names(closeness)[1] <- "name"
names(closeness)[2] <- "closeness"
closenessMerge<-merge(closeness,nodeList, by="name") 
closenessMerge<-mutate(closenessMerge, closenessPercent=ntile(closenessMerge$closeness,100))
write.csv(closenessMerge, "closeness.csv")



###
### 6.  LINK PREDICTION
###

## Link prediction for the compound based on the disease using Common neighbors
common<-'MATCH (n1:Disease)
MATCH (n2:Compound)
WHERE NOT (n2)--(n1)
RETURN n1.name, n2.name, gds.alpha.linkprediction.commonNeighbors(n1, n2) AS commonNeighbors_score
order by commonNeighbors_score desc;'
common<-call_neo4j(common,connect)
common<-cbind(common$n1.name,common$n2.name,common$commonNeighbors_score)
names(common)[1] <- "Disease"
names(common)[2] <- "Name"
common<- common %>% 
  filter(value>0) 
write.csv(common, "diseaseDrugRank.csv")


## Link Prediction : Compound and Anatomy using Common neighbors

siteDrugRank<-'MATCH (n1:Anatomy)
MATCH (n2:Compound)
WHERE NOT (n2)--(n1)
RETURN n1.name, n2.name, gds.alpha.linkprediction.commonNeighbors(n1, n2) AS commonNeighbors_score
order by commonNeighbors_score desc;'
siteDrugRank<-call_neo4j(siteDrugRank,connect)
siteDrugRank<-cbind(siteDrugRank$n1.name,siteDrugRank$n2.name,siteDrugRank$commonNeighbors_score)
names(siteDrugRank)[1] <- "Site"
names(siteDrugRank)[2] <- "Compound"
names(siteDrugRank)[3] <- "Value"
siteDrugRank<- siteDrugRank %>% 
  select(Site, Compound,Value) %>% 
  filter(Value>0) %>% 
  group_by(Site) %>% 
  mutate(rank = order(Value, decreasing=TRUE))
write.csv(siteDrugRank, "siteDrugRank.csv")


siteDrugRank2<-siteDrugRank %>% 
  mutate(Name=Compound,siteDrugLink=Value) %>% 
  select(Name, Site, siteDrugLink)

diseaseDrugRank2<-common %>% 
  mutate(diseaseDrugLink=value) %>% 
  select(Name, Disease,diseaseDrugLink)

presentLinks<-merge(siteDrugRank2,diseaseDrugRank2, by="Name")
allLinks<-merge(siteDrugRank2,diseaseDrugRank2, by="Name", all = TRUE)
  

### 
### 
###
all<-read.csv("Data/all.csv")
all2 <- all[ which(all$Degree>10 & all$Degree<1000), ]
loadfonts(device = "win")
mu <- ddply(all2, "Aspect", summarise, grp.mean=median(Degree))
png("Data/all.png", width = 12, height = 7, units = 'in', res = 600)
ggplot(all2, aes(x=Degree, color=Aspect)) +
  geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Aspect),
             linetype="dashed") +
  xlim(NA, 100)+ 
  labs(x = NULL)+
  theme_classic()+
  labs(
    title = "<b style = 'color:#1381B0;  font-size:38px';> Density plot of degree centrality</b><br>
    <span style = 'font-size:14pt'>The in-degree, out degree and total degree distribution for all the nodes. This plot excludes nodes with degrees below 10 and above 1000</span>",
    x = "Number of connections",
    y = "Distribution</span>")+
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)),
    axis.title.x = element_textbox_simple(
      width = NULL,
      padding = margin(4, 4, 4, 4),
      margin = margin(4, 0, 0, 0),
      linetype = 1,
      r = grid::unit(8, "pt")
      # fill = "azure1"
    ),
    axis.title.y = element_textbox_simple(
      hjust = 0,
      orientation = "left-rotated",
      minwidth = unit(1, "in"),
      maxwidth = unit(2, "in"),
      padding = margin(4, 4, 2, 4),
      margin = margin(0, 0, 2, 0)
      #fill = "lightsteelblue1"
    ))
dev.off()


centralMerge<- read.csv("Data/centrality.csv")
centralMerge$centrality<-scales::rescale(centralMerge$centrality)
centralMerge2<-centralMerge %>% 
  mutate(rank = dense_rank(centrality)) %>% 
  head(5)
centralMerge2<-centralMerge %>%
  group_by(Category) %>%
  mutate(rank = dense_rank(centrality))

topCen<- centralMerge %>% 
  filter(Category=="Gene") %>% 
  filter(centralPercent==100) %>% 
  mutate(name2=name) %>% 
  select(name,name2)
centralMerge<-merge(centralMerge,topCen, by="name",all.x = TRUE)
png("Data/centrality.png", width = 12, height = 7, units = 'in', res = 600)
ggplot(centralMerge, aes(x = factor(1), y = centrality)) +
  geom_boxplot(width = 0.9, fill = "white") +
  geom_jitter(aes(color = Category, shape = Category), 
              width = 0.4, size = 2) + 
  labs(x = NULL)+
  theme_classic()+
  labs(
    title = "<b style = 'color:#1381B0; font-size:38px';>Degree Centrality</b><br>
    <span style = 'font-size:14pt'>Only genes in the top 1 percent bin  are labeled.</span>",
    x = "All points",
    y = "Centrality (scale (0-1)</span>"
  )+ 
  geom_text_repel(
    aes(label = name2),
    size = 3,
    force        = 0.7,
    nudge_x      = 0.5,
    direction    = "y",
    hjust        = 1.3,
    segment.size = 0.2,max.overlaps = Inf
    #  box.padding = unit(0.4, "lines"),
    #  point.padding = unit(0.1, "lines")
  )+
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
      # fill = "cornsilk"
    ),
    axis.title.x = element_textbox_simple(
      width = NULL,
      padding = margin(4, 4, 4, 4),
      margin = margin(4, 0, 0, 0),
      linetype = 1,
      r = grid::unit(8, "pt")
      # fill = "azure1"
    ),
    axis.title.y = element_textbox_simple(
      hjust = 0,
      orientation = "left-rotated",
      minwidth = unit(1, "in"),
      maxwidth = unit(2, "in"),
      padding = margin(4, 4, 2, 4),
      margin = margin(0, 0, 2, 0)
      #fill = "lightsteelblue1"
    ))
dev.off()



betweenMerge<- read.csv("Data/between.csv") 
betweenMerge<-betweenMerge%>% 
  mutate(betweenPercent=ntile(betweenMerge$betweenness,100))
centralMerge<- read.csv("Data/centrality.csv")
betweenAndcentrality <-merge(betweenMerge,centralMerge)
topbetweenAndcentrality<-betweenAndcentrality %>% 
  filter(Category=="Gene",betweenPercent==100,centralPercent==100) %>% 
  mutate(name2=name) %>% 
  select(name,name2)
betweenAndcentrality$centrality<-scales::rescale(betweenAndcentrality$centrality)
betweenAndcentrality$betweenness<-scales::rescale(betweenAndcentrality$betweenness)
betweenAndcentrality<-merge(x=betweenAndcentrality,y=topbetweenAndcentrality, by="name", all.x=TRUE)
betweenAndcentrality<-betweenAndcentrality%>% 
  mutate(Class=ifelse(is.na(name2), "Less", "Top 1 percent"))

#head(betweenAndcentrality)
png("Data/betweenAndcentrality.png", width = 12, height = 7, units = 'in', res = 600)
ggplot(betweenAndcentrality, aes(x=betweenness, y=centrality, shape=Category, color=Category)) +
  geom_point() + 
  theme_classic()+
  labs(x = NULL)+
  labs(
    title = "<b style = 'color:#1381B0;  font-size:38px';>Influential connectors</b><br>
    <span style = 'font-size:12pt'>Centrality and betweenness  scatter plot to 
identify  nodes that have many and influential linkages. Only genes within the top percent in both are labeled.</span>",
    x = "Betweenness centrality",
    y = "Centrality (scale (0-1)</span>"
  )+ 
  geom_mark_hull(aes(fill = Class,filter = Class != 'Less'),show.legend=FALSE,  
                 fill = "white",
                 con.colour = "black",
                 con.size = 0.5,
                 con.linetype = 1) +
  geom_mark_rect(aes(fill = name2, label = name2,
                     filter = name2 != ''),show.legend=FALSE,
                 expand = unit(0.5,"mm"),
                 radius = unit(0.5, "mm"),
                 con.type = "straight",)+
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
      # fill = "cornsilk"
    ),
    axis.title.x = element_textbox_simple(
      width = NULL,
      padding = margin(4, 4, 4, 4),
      margin = margin(4, 0, 0, 0),
      linetype = 1,
      r = grid::unit(8, "pt")
      # fill = "azure1"
    ),
    axis.title.y = element_textbox_simple(
      hjust = 0,
      orientation = "left-rotated",
      minwidth = unit(1, "in"),
      maxwidth = unit(2, "in"),
      padding = margin(4, 4, 2, 4),
      margin = margin(0, 0, 2, 0)
      #fill = "lightsteelblue1"
    ))+ 
  theme(legend.position="bottom")
dev.off()

# https://www.r-bloggers.com/2018/03/another-game-of-thrones-network-analysis-this-time-with-tidygraph-and-ggraph/
#### 
### betweenAndclose
# https://www.researchgate.net/figure/Closeness-centrality-and-betweenness-centrality-scatter-plot-of-the-network_fig2_239605090

betweenMerge<- read.csv("Data/between.csv")
closenessMerge <- read.csv("Data/closeness.csv")

betweenAndclose<-merge(closenessMerge,betweenMerge, by="name") %>% 
  mutate(Category = Category.x)
betweenAndclose$betweenness<-scales::rescale(betweenAndclose$betweenness)

betweenAndclose$closeness<-scales::rescale(betweenAndclose$closeness)

betweenMerge<- read.csv("Data/between.csv")
TopbetweenMerge <-betweenMerge %>% 
  filter(Category=="Gene") %>% 
  mutate(betweenPercent=ntile(betweenness,100)) %>% 
  filter(betweenPercent==100) %>% 
  mutate(Between=name) %>% 
  select(name,Between)

closenessMerge <- read.csv("Data/closeness.csv")
TopclosenessMerge <-closenessMerge %>% 
  filter(Category=="Gene") %>% 
  mutate(closePercent=ntile(closeness,100)) %>% 
  filter(closePercent==100) %>% 
  mutate(close=name) %>% 
  select(name,close)

both<-merge(x=TopbetweenMerge, y=TopclosenessMerge, all=TRUE) %>% 
  select(name,Between,close) %>% 
  drop_na() %>% 
  mutate(name2=name) %>% 
  select(-Between,-close)

betweenAndclose<-merge(x=betweenAndclose, y=both, by="name", all.x=TRUE)
betweenAndclose<-betweenAndclose%>% 
      mutate(Class=ifelse(is.na(name2), "Less", "Top 1 percent"))

png("Data/betweenAndclose.png", width = 12, height = 7, units = 'in', res = 600)
ggplot(betweenAndclose, aes(x=betweenness, y=closeness, shape=Category, color=Category)) +
  geom_point()+
  theme_classic()+
  labs(
    title = "<b style = 'color:#1381B0;  font-size:38px';> Information flow influencers</b><br>
    <span style = 'font-size:14pt'>Scatter plot of Closeness vs Betweenness  to 
identify  nodes that support distribution of information 
effectively throughout the network.</span>",
    x = "Betweenness (scale (0-1)",
    y = "Closeness (scale (0-1)</span>"
  )+
  geom_mark_hull(aes(fill = Class,filter = Class != 'Less'),show.legend=FALSE,  
                 fill = "white",
                 con.colour = "black",
                 con.size = 0.5,
                 con.linetype = 1) +
  geom_mark_rect(aes(fill = name2, label = name2,
                     filter = name2 != ''),show.legend=FALSE,
                 expand = unit(0.5,"mm"),
                 radius = unit(0.5, "mm"))+
  theme(
    #plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
    ),
    axis.title.x = element_textbox_simple(
      width = NULL,
      padding = margin(4, 4, 4, 4),
      margin = margin(4, 0, 0, 0),
      linetype = 1,
      r = grid::unit(8, "pt")
    ),
    axis.title.y = element_textbox_simple(
      hjust = 0,
      orientation = "left-rotated",
      minwidth = unit(1, "in"),
      maxwidth = unit(2, "in"),
      padding = margin(4, 4, 2, 4),
      margin = margin(0, 0, 2, 0)
    ))+ 
  theme(legend.position="bottom")


dev.off()



toplist<-read.csv("Data/toplist.csv")%>% 
  select(-X) %>% 
  drop_na()
png("Data/subgraph.png", width = 12, height = 7, units = 'in', res = 600)
subgraph = igraph::graph.data.frame(toplist, directed=T)
set.seed(1)
ggraph(subgraph, layout = 'kk') + 
  geom_edge_link(aes(colour = node1.name))+
  geom_node_point( colour = "black")+ 
  theme(axis.title = element_blank())+
  theme_classic()+
  labs(
    title = "<b style = 'color:#1381B0;  font-size:38px';>Subset of the network of the top influential genes</b><br>
    <span style = 'font-size:20pt'>Visualization of the network formed by the four genes 
    ranked to have the highest degree centrality, betweenness and closeness centrality.</span>",
    x = "",
    y = "" 
  )+
  
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
    ),
    axis.title.x = element_textbox_simple(
      width = NULL
      
    ),
    axis.title.y = element_textbox_simple(
      hjust = 0,
      orientation = "left-rotated",
      minwidth = unit(1, "in"),
      maxwidth = unit(2, "in"),
      padding = margin(4, 4, 2, 4),
      margin = margin(0, 0, 2, 0)
    ))

dev.off()



siteDrugRank<-read.csv("Data/siteDrugRank.csv")
siteDrugRank<-siteDrugRank %>% 
  filter(rank<11)
png("Data/siteDrugRank.png", width = 12, height = 7, units = 'in', res = 600)
ggplot(siteDrugRank, aes(x = Site, y = Compound, group = Compound)) +
  geom_point(aes(color = rank, size = desc(rank)))+
  geom_line(aes(color = rank), size = 2)+
  labs(
    fill="Rank",
    title = "<b style = 'color:#1381B0;  font-size:34px';>Potential linkages between the compound and site based on shared genes.
</b><br>
    <span style = 'font-size:20pt'> Potential linkages with highest common neighbor scores (46 out of 90)</span>",
    x = "Anatomical site",
    y = "Compound" 
  )+
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
    ),
    axis.title.x = element_textbox_simple(
      width = NULL
      ),
    axis.title.y = element_textbox_simple(
      hjust = 0,
      orientation = "left-rotated",
      minwidth = unit(1, "in"),
      maxwidth = unit(2, "in"),
      padding = margin(4, 4, 2, 4),
      margin = margin(0, 0, 2, 0)
    ))
dev.off()


##Disease Drug Rank
diseaseDrugRank<-read.csv("Data/diseaseDrugRank.csv")
siteDrugRank<-read.csv("Data/siteDrugRank.csv")
siteDrugRank2<-siteDrugRank %>% 
  mutate(Name=Compound,siteDrugLink=Value) %>% 
  select(Name, Site, siteDrugLink)
diseaseDrugRank2<-diseaseDrugRank %>% 
  mutate(diseaseDrugLink=value) %>% 
  select(Name, Disease,diseaseDrugLink)
presentLinks<-merge(siteDrugRank2,diseaseDrugRank2, by="Name") %>% 
  mutate(Name2=ifelse(diseaseDrugLink>1,Name,NA))
png("Data/presentLinks.png", width = 12, height = 7, units = 'in', res = 600)
ggplot(presentLinks, aes(x = siteDrugLink, y = diseaseDrugLink)) +
  geom_point(aes(color = Site)) +
  facet_grid(~ Site)+
  theme_bw(base_size = 15) + theme(legend.position = "bottom") +
  geom_text_repel(
    aes(label = Name2),
    size = 4,
    box.padding = unit(0.25, "lines"),
    point.padding = unit(0.2, "lines"))+
  labs(
    title = "<b style = 'color:#1381B0;  font-size:38px';>Predicting site specific compounds.</b><br>
    <span style = 'font-size:20pt'>Correlation between link prediction scores of BTC-compound
    and Site-Compound </span>",
    x = "Link Prediction score: Anatomic site",
    y = "Link Prediction score: Compound" 
  )+
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
    ),
    axis.title.x = element_textbox_simple(
      width = NULL
    ),
    axis.title.y = element_textbox_simple(
      hjust = 0,
      orientation = "left-rotated",
      minwidth = unit(1, "in"),
      maxwidth = unit(2, "in"),
      padding = margin(4, 4, 2, 4),
      margin = margin(0, 0, 2, 0)
    ))
dev.off()



## Class membership


library(tidyverse)
communitiesTop<-read.csv("communities.csv") 
nodeList<-read.csv("nodeList.csv")
communitiesTop<-merge(x=communitiesTop, y=nodeList, by="name",all.x = TRUE)
communitiesNumber<-communitiesTop %>% 
  group_by(communityId) %>% 
  tally()

communitiesWithone<-communitiesNumber %>% 
  filter(n==1) %>% 
  tally()


communitiesTop3<- merge(y=communitiesTop, x=communitiesNumber,by="communityId",all.x=TRUE) %>% 
  drop_na() %>% 
  rename( Membership=n, Nodes=Category)
tab<-tableplot(communitiesTop3, select = c( Membership, Nodes), sortCol = Membership)
tableSave(tab, filename = "frequency.png", width = 12, height = 8, fontsize = 12, legend.lines = 7)
