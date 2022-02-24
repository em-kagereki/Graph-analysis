// Cypher queries for link prediction
// Link prediction algorithms are usually used to quantify indirect relations between unconnected nodes
// The greater the link prediction score, the greater the potential of forming a relation between the given unconnected nodes

// Link prediction #1: Common Neighbours
// Query below calculates shared neighbours between Disease and Compound
// Having many neighbouring nodes in common is indicative of potential relation between a Disease and a Compound
// Note that the query below will only match Compound nodes which DO NOT HAVE A DIRECT RELATION with the Disease node
MATCH (n1:Disease)
MATCH (n2:Compound)
WHERE NOT (n2)--(n1)
RETURN n1.name, n2.name, gds.alpha.linkprediction.commonNeighbors(n1, n2) AS commonNeighbors_score
order by commonNeighbors_score desc
limit 100

// Link prediction #2: Common Neighbours
// Query below calculates shared neighbours between Disease and BiologicalProcess
// Having many neighbouring nodes in common is indicative of potential relation between a Disease and a BiologicalProcess
// Since Disease and BiologicalProcess nodes do not have a direct relationship with each other, no additional WHERE conditions are required
MATCH (n1:Disease)
MATCH (n2:BiologicalProcess)
RETURN n1.name, n2.name, gds.alpha.linkprediction.commonNeighbors(n1, n2) AS commonNeighbors_score
order by commonNeighbors_score desc
limit 100

// Link prediction #3: Adamic Adar
// Query below computes link prediction between Compound and Disease nodes
// Note that the query below will only match Compound nodes which DO NOT HAVE A DIRECT RELATION with the Disease node
MATCH (n1:Disease)
MATCH (n2:Compound)
WHERE NOT (n2)--(n1)
RETURN n1.name, n2.name, gds.alpha.linkprediction.adamicAdar(n1, n2) AS AdamicAdar_Score
order by AdamicAdar_Score desc
limit 100

// Link prediction #4: Adamic Adar
// Query below computes link prediction between Disease and Pathway nodes
// Since Disease and Pathway nodes do not have a direct relationship with each other, no additional WHERE conditions are required
MATCH (n1:Disease)
MATCH (n2:Pathway)
RETURN n1.name, n2.name, gds.alpha.linkprediction.adamicAdar(n1, n2) AS AdamicAdar_Score
order by AdamicAdar_Score desc
limit 100


// Link prediction #5: Jaccard Similarity
// Query below calculates similarity between nodes based on common relations with other neighbouring nodes
// While Jaccard Similarity isn't considered a 'link prediction' algorithm, however high similarity between nodes indicates that there are many shared connections between nodes
// For example, if two Gene nodes (n1 and n3) have similar relations with neighbouring Pathway nodes are assumed to have similar functions based on their high similarity scores
// If Gene (n1) is associated with Disease Z, then Gene (n3) is hypothesized to be associated with Disease Z
// Query below applies Jaccard Similarity function to calculate similarity between Gene nodes based on shared relations with Pathway nodes
// Note that this query first identifies Gene nodes which have a direct relation to Disease nodes, then identifies Gene nodes which DO NOT have a direct relation to Disease nodes
// The final score indicates similarity of two Gene nodes based on shared relations with Pathway nodes, whereby (n1) is a Gene that has a direct relation with the Disease node
// and (n3) is a Gene node that does NOT have a direct relation with the Disease node
// If similarity scores are high, we can hypothesize that Genes (n1) and (n3) participate in similar genetic functions, and may also be associated with the Disease (hypothesized association)
match (g:Gene)--(:Disease)
with collect(g.name) as GenesAssociatedWithDisease
match (n1:Gene)--(n2:Pathway)
where n1.name in GenesAssociatedWithDisease
with n1, collect(id(n2)) as Pathway1, GenesAssociatedWithDisease
match (n3:Gene)--(n4:Pathway)
where not n3.name in GenesAssociatedWithDisease
with n1, Pathway1, n3, collect(id(n4)) as Pathway2
return n1.name, n3.name, gds.alpha.similarity.jaccard(Pathway1, Pathway2) as similarity
order by similarity desc;

// Link prediction #6: Jaccard Similarity
// Query below also applies Jaccard Similarity but instead focuses on Gene--BiologicalProcess relations
match (g:Gene)--(:Disease)
with collect(g.name) as GenesAssociatedWithDisease
match (n1:Gene)--(n2:BiologicalProcess)
where n1.name in GenesAssociatedWithDisease
with n1, collect(id(n2)) as BiologicalProcess1, GenesAssociatedWithDisease
match (n3:Gene)--(n4:BiologicalProcess)
where not n3.name in GenesAssociatedWithDisease
with n1, BiologicalProcess1, n3, collect(id(n4)) as BiologicalProcess2
return n1.name, n3.name, gds.alpha.similarity.jaccard(BiologicalProcess1, BiologicalProcess2) as similarity
order by similarity desc;
