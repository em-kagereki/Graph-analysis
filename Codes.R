setwd("C:/Users/Admin/Documents/KM - Winter 2022/tutorial/B00867154/Original Data")


library(neo4r)
library(dplyr)
library(purrr)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(ggtext)

# Note that play_movies is only available for versions >= 0.1.3 

connect <- neo4j_api$new(url = "http://localhost:7474", 
                         db = "Tutorial", 
                         user = "neo4j", 
                         password = "Tutorial")
##
## READ THE FILES
##
## THis code is modified from: https://github.com/neo4j-rstats/neo4r
# I changed the on_load_query name to read accordingly

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

#Create a universal label (Hetionet_Nodes) to all nodes
#This will help with calling GDS functions for graph analysis
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

#Create new node properties to specify node type - e.g., Gene, Compound, Pathway, Anatomy, etc.
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


# Graph statistics

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

setwd("C:/Users/Admin/Documents/KM - Winter 2022/tutorial/B00867154/Data")

Aspects <- c('Nodes','Genes','Pathway','nBiologicalProcess','Relationships','nDisease','nCompound','gRd','dRa','aRg','gRb','gRp')
Statistics<- c(nNodes,nGenes,nPathway,nBiologicalProcess,nRelationships,nDisease,nCompound,gRd,dRa,aRg,gRb,gRp)
basicStats<-as.data.frame(cbind(Aspects,Statistics))
basicStats$Statistics<-as.numeric(basicStats$Statistics)
write.csv(basicStats, "basicStats.csv")

library(igraph)
library(magrittr)
library(neo4r)

## PART 2: List the individual nodes

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


## Part 3: Global graph stats

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
### Here I will Use the query ro calculate in degree, outdegree and all

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




### LINK PREDICTION
## Link prediction for the compound based on the disease
# Common neighbors
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

siteDrugRank<-read.csv("diseaseDrugRank.csv")

## Link Prediction : Anatomy
# Common neighbors
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
  

ggplot(allLinks, aes(x = diseaseDrugLink, y = siteDrugLink)) +
     geom_point(aes(color = Site))










## Adamic Adar
common4<-'MATCH (n1:Disease)
MATCH (n2:Compound)
RETURN n1.name, n2.name, gds.alpha.linkprediction.adamicAdar(n1, n2) AS AdamicAdar_Score
order by AdamicAdar_Score desc;'
common4<-call_neo4j(common4,connect)
common4<-cbind(common4$n1.name,common4$n2.name,common4$AdamicAdar_Score)
names(common4)[1] <- "disease"
names(common4)[2] <- "Name"
common4<- common4 %>% 
  select(Name, value) %>% 
  mutate(Category = "Compound") %>% 
  filter(value>0)



## Then  the Pathway
common3<-'MATCH (n1:Disease)
MATCH (n2:Pathway)
RETURN n1.name, n2.name, gds.alpha.linkprediction.adamicAdar(n1, n2) AS AdamicAdar_Score
order by AdamicAdar_Score desc
limit 100'
common3<-call_neo4j(common3,connect)
common3<-cbind(common3$n1.name,common3$n2.name,common3$AdamicAdar_Score)
names(common3)[1] <- "disease"
names(common3)[2] <- "Name"
common3<- common3 %>% 
  select(Name, value) %>% 
  mutate(Category = "Pathway")%>% 
  filter(value>0)

## Then the biological processes
common4<-'MATCH (n1:Disease)
MATCH (n2:BiologicalProcess)
RETURN n1.name, n2.name, gds.alpha.linkprediction.adamicAdar(n1, n2) AS AdamicAdar_Score
order by AdamicAdar_Score desc;'
common4<-call_neo4j(common4,connect)
common4<-cbind(common4$n1.name,common4$n2.name,common4$AdamicAdar_Score)
names(common4)[1] <- "disease"
names(common4)[2] <- "Name"
common4<- common4 %>% 
  select(Name, value) %>% 
  mutate(Category = "Biological Process") %>% 
  filter(value>0)
#head(common4)


data<-rbind(common,common3, common4)

write.csv(data,"linkCompound.csv")



## Link Prediction
#### Common Neighbours: neighbouring nodes in common is indicative of potential relation between a Disease and a Compound
## Link prediction for the compound
common4<-'MATCH (n1:Anatomy)
MATCH (n2:Compound)
WHERE NOT (n2)--(n1)
RETURN n1.name, n2.name, gds.alpha.linkprediction.commonNeighbors(n1, n2) AS commonNeighbors_score
order by commonNeighbors_score desc
limit 100;'
common4<-call_neo4j(common4,connect)
common4<-cbind(common4$n1.name,common4$n2.name,common4$commonNeighbors_score)
names(common4)[1] <- "Site"
names(common4)[2] <- "name"
common4<- common4 %>% 
  select(Site,name, value) %>% 
  filter(value>0)
common4<-merge(common4, nodeList, by ="name")

## Then  the Pathway
common5<-'MATCH (n1:Anatomy)
MATCH (n2:Pathway)
RETURN n1.name, n2.name, gds.alpha.linkprediction.adamicAdar(n1, n2) AS AdamicAdar_Score
order by AdamicAdar_Score desc
limit 100'
common5<-call_neo4j(common5,connect)
common5<-cbind(common5$n1.name,common5$n2.name,common5$AdamicAdar_Score)
names(common5)[1] <- "Site"
names(common5)[2] <- "name"
common5<- common5 %>% 
  select(Site,name, value) %>% 
  filter(value>0)
common5<-merge(common5, nodeList, by ="name")

## Then the biological processes
common6<-'MATCH (n1:Anatomy)
MATCH (n2:BiologicalProcess)
RETURN n1.name, n2.name, gds.alpha.linkprediction.adamicAdar(n1, n2) AS AdamicAdar_Score
order by AdamicAdar_Score desc;'
common6<-call_neo4j(common6,connect)
common6<-cbind(common6$n1.name,common6$n2.name,common6$AdamicAdar_Score)
names(common6)[1] <- "Site"
names(common6)[2] <- "name"
common6<- common6 %>% 
  select(Site,name, value) %>% 
  filter(value>0)
common6<-merge(common6, nodeList, by ="name")
data2<-rbind(common4,common5, common6)
write.csv(data2, "linkBiologicalProcess.csv")


names(data2)[3] <- "AnatomyLInk"
names(data)[2] <- "DiseaseLInk"


data <-rename(data, Name = DiseaseLInk)
data <-rename(data, DiseaseLInk = value)

data2 <-rename(data2, Name = AnatomyLInk)
data2 <-rename(data2, AnatomyLInk = value)

dataComplete<-merge(x = data, y = data2, by = "Name", all = TRUE)
dataComplete<-dataComplete[complete.cases(dataComplete), ]


dataComplete<-merge(x = data, y = data2, by = "Name", all = TRUE)
dataComplete<-dataComplete[complete.cases(dataComplete), ]

drugs <- filter(data2, Category=='Compound') %>% 
  select(name,Site,value)
library(tidyr)
drugsWide<-spread(drugs, key = name, value = value)
drugsWide[is.na(drugsWide)] <- 0
names(drugsWide)[1] <- "group"
row.names(drugsWide) <- drugsWide$group
drugsWide <- as_tibble(drugsWide,rescale)


drugsWide<-drugsWide %>% 
  mutate_each(funs(rescale), -group) %>%
  ggradar()



library(tidyr)
drugsWide<-spread(drugs, key = Name, value = AnatomyLInk) %>% 
  select(-Category)
row.names(drugsWide) <- drugsWide$Site
drugsWide<- drugsWide %>% select(-Site)
ggradar(drugsWide)


student1_data <- drugsWide[c("liver","lymph node"), ]
student1_data[is.na(student1_data)] <- 0
radarchart(student1_data)


drugsWide<-spread(drugs, key = name, value = value) %>% 
  select(-Category)

## LInk Prediction : Similarity
jarc<-'match (g:Gene)--(:Disease)
with collect(g.name) as GenesAssociatedWithDisease
match (n1:Gene)--(n2:Pathway)
where n1.name in GenesAssociatedWithDisease
with n1, collect(id(n2)) as Pathway1, GenesAssociatedWithDisease
match (n3:Gene)--(n4:Pathway)
where not n3.name in GenesAssociatedWithDisease
with n1, Pathway1, n3, collect(id(n4)) as Pathway2
return n1.name, n3.name, gds.alpha.similarity.jaccard(Pathway1, Pathway2) as similarity
order by similarity desc;'
jarc<-call_neo4j(jarc,connect)
jarc<-cbind(jarc$n1.name,jarc$n3.name,jarc$similarity)



library(ggplot2)
library(ggradar)
suppressPackageStartupMessages(library(dplyr))
library(scales)

mtcars %>%
  tibble::rownames_to_column( var = "group" ) %>%
  mutate_each(across(rescale), -group) %>%
  tail(4) %>% select(1:10) -> mtcars_radar

dat %>% 
  mutate_each(funs(rescale), -group) %>%
  ggradar()

ggradar(mtcars_radar)


betweenMerge<- read.csv("between.csv")
centralMerge<- read.csv("centrality.csv")
betweenAndcentrality <-merge(betweenMerge,centralMerge)
closeness <- read.csv("closeness.csv") %>% 
  dplyr::select(name,closeness)
da<-merge(betweenAndcentrality,closeness, by="name")

bet_list <- split(da$betweenness, da$Category)
cen_list <- split(da$centrality, da$Category)
clos_list <- split(da$closeness, da$Category)
## The Original plot
inline_plot <- data.frame(Node = c("BiologicalProcess", "Compound", "Gene","Pathway"), Plot = "", mpg_hist = "",
                          mpg_line1 = "", mpg_line2 = "",
                          mpg_points1 = "", mpg_points2 = "", mpg_poly = "")
inline_plot %>%
  kbl(booktabs = TRUE) %>%
  kable_paper(full_width = TRUE) %>%
  column_spec(2, image = spec_boxplot(mpg_list))%>%
  column_spec(3, image = spec_hist(mpg_list)) %>%
  column_spec(4, image = spec_plot(mpg_list, same_lim = TRUE)) %>%
  column_spec(5, image = spec_plot(mpg_list, same_lim = FALSE)) %>%
  column_spec(6, image = spec_plot(mpg_list, type = "p")) %>%
  column_spec(7, image = spec_plot(mpg_list, disp_list, type = "p")) %>%
  column_spec(8, image = spec_plot(mpg_list, polymin = 5))



## Modifications


inline_plot <- data.frame(Node = c("BiologicalProcess", "Compound", "Gene","Pathway"), Plot = "", 
                          Highest = "")
inline_plot %>%
  kbl(booktabs = TRUE) %>%
  kable_paper(full_width = TRUE) %>%
  column_spec(2, image = spec_boxplot(bet_list))%>%
  column_spec(3, image = spec_hist(cen_list,150,150)) 




coef_table <- data.frame(
  Variables = c("Biological Process", "Compound", "Gene", "Pathway"),
  Coefficients = c(confint(lm(da[ which(da$Category=='BiologicalProcess'), ]$closeness ~ 1, da), level=0.95)[1],
                   confint(lm(da[ which(da$Category=='Compound'), ]$closeness ~ 1, da), level=0.95)[1],
                   confint(lm(da[ which(da$Category=='Gene'), ]$closeness ~ 1, da), level=0.95)[1],
                   confint(lm(da[ which(da$Category=='Pathway'), ]$closeness ~ 1, da), level=0.95)[1]),
  Conf.Lower = c(mean(da[ which(da$Category=='BiologicalProcess'), ]$closeness),
                 mean(da[ which(da$Category=='Compound'), ]$closeness), 
                 mean(da[ which(da$Category=='Gene'), ]$closeness),
                 mean(da[ which(da$Category=='Pathway'), ]$closeness)),
  Conf.Higher = c(confint(lm(da[ which(da$Category=='BiologicalProcess'), ]$closeness ~ 1, da), level=0.95)[1],
                  confint(lm(da[ which(da$Category=='Compound'), ]$closeness ~ 1, da), level=0.95)[1],
                  confint(lm(da[ which(da$Category=='Gene'), ]$closeness ~ 1, da), level=0.95)[1],
                  confint(lm(da[ which(da$Category=='Pathway'), ]$closeness ~ 1, da), level=0.95)[1])
) 

data.frame(
  Variable = coef_table$Variables,
  Visualization = ""
) %>%
  kbl(booktabs = T) %>%
  kable_classic(full_width = FALSE) %>%
  column_spec(2, image = spec_pointrange(
    x = coef_table$Coefficients, 
    xmin = coef_table$Conf.Lower, 
    xmax = coef_table$Conf.Higher, 
    vline = 1)
  )

library(dplyr)
closeness <- read.csv("closeness.csv")
geneClose<-closeness[ which(closeness$Category=='Gene'), ] %>% 
  mutate(rank = dense_rank(desc(closeness))) %>% 
  head(5)

geneClose$name


set.seed(1234)
wdata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58))
)
head(wdata, 4)

library("dplyr")
mu <- wdata %>% 
  group_by(sex) %>%
  summarise(grp.mean = mean(weight))
mu




inD<-AllCentrality %>% 
  select(name,inDegree) %>% 
  rename(Degree = inDegree) %>% 
  mutate(Aspect ="In degree") %>% 
  mutate( Degree= replace_na(Degree, 0))

  outD<-AllCentrality %>% 
    select(name,outDegree) %>% 
    rename(Degree = outDegree) %>% 
    mutate(Aspect ="Out degree") %>% 
    mutate( Degree= replace_na(Degree, 0))

  allD<-AllCentrality %>% 
    select(name,totalDegree) %>% 
    rename(Degree = totalDegree) %>% 
    mutate(Aspect ="Total degree") %>% 
    mutate( Degree= replace_na(Degree, 0))
  
all<-rbind(outD,inD,allD)  
  

a <- ggplot(all, aes(x = Degree))


a + geom_freqpoly( aes(color = Aspect, linetype = Aspect),
                   bins = 10, size = 1.5) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#00FF00"))

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

write.csv(all,"all.csv")


query <- "match  (node1)-[r]-(node2) return node1.name,node2.name"
triplist <- call_neo4j(query, connect)
library(igraph)
subgraph = graph.data.frame(triplist, directed=T)
plot(subgraph)
write.csv(triplist,"triplist.csv")

setwd("C:/Users/Admin/Documents/KM - Winter 2022/tutorial")

ddd<-read.csv("data/triplist.csv") %>% 
  select(-X) %>% 
  drop_na()

subgraph = graph.data.frame(ddd, directed=T)
plot(subgraph)
#return node1.name,count(r)

ddd<-data.frame(triplist$node1.name,triplist$node2.name)
ddd<-ddd %>% 
  drop_na()


ggraph(ddd,layout="lgl")+
  geom_edge_link(width=0.1,colour="grey")+
  geom_node_point(col="black",size=3.7)+
  theme_graph()


setwd("C:/Users/Admin/Documents/KM - Winter 2022/tutorial")




library(tidyverse)
library(igraph)
library(ggraph)


## Query for the most influential gener

influentialGenes <- "match  (node1)-[r]-(node2) where node1.name='UBC' 
XOR node1.name='SHC1' XOR node1.name='MYC' XOR node1.name='FYN' 
XOR node1.name='NUP85' AND node2.name='bile tract disease' return node1.name,node2.name"
toplist <- call_neo4j(influentialGenes, connect)
toplist<-data.frame(toplist$node1.name,toplist$node2.name)
write.csv(toplist,"toplist.csv")

subgraph = graph.data.frame(toplist, directed=T)
subgraph<-simplify(subgraph)



ggraph(subgraph, layout = 'kk') + 
  geom_edge_link(aes(colour = node1.name)) + 
  geom_node_point()

library(tidygraph)
dolphin <- tbl_graph(nodes = Nodes, edges = Edges, directed = TRUE)


## Graph layouts
## https://www.data-imaginist.com/2017/ggraph-introduction-layouts/


dd<-'MATCH (n)--(r) RETURN n.name AS from, r.name AS to;' %>% 
  call_neo4j(connect)

nodes = data.frame(id=unique(c(dd$from, dd$to)))
nodes$label = nodes$id

edges<-data.frame(dd)
names(edges)[1] <- "x"
names(edges)[2] <- "y"


### LInk Prediction

# // Link prediction #1: Common Neighbours
# // Query below calculates shared neighbours between Disease and Compound
# // Having many neighbouring nodes in common is indicative of potential relation between a Disease and a Compound
# // Note that the query below will only match Compound nodes which DO NOT HAVE A DIRECT RELATION with the Disease node MATCH (n1:Disease)
MATCH (n2:Compound)
WHERE NOT (n2)--(n1)
RETURN n1.name, n2.name, gds.alpha.linkprediction.commonNeighbors(n1, n2) AS commonNeighbors_score
order by commonNeighbors_score desc
limit 100