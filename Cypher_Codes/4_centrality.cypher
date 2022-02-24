// Cypher query to compute degree centrality of all graph nodes
// Results are limited to top 100 nodes and include the 'type' of node (i.e., Gene, Anatomy, Compound, etc.)
// NOTE: cross-check relationshipProjection with Relationship Types in your graph
// If Compound_Disease is not a relationship in your graph, remove it from relationshipProjection
call gds.degree.stream({
  nodeProjection: 'Hetionet_Nodes',
  relationshipProjection: ['Disease_Gene', 'Disease_Anatomy', 'Compound_Gene', 'Anatomy_Gene', 'Gene_BiologicalProcess', 'Gene_Pathway', 'Compound_Disease']
  })
YIELD nodeId, score
return gds.util.asNode(nodeId).name AS name, gds.util.asNode(nodeId).type AS nodeType ,score
order by score desc limit 100

// Cypher query to compute degree betweenness centrality of all graph nodes
// Results are limited to top 100 nodes and include the 'type' of node (i.e., Gene, Anatomy, Compound, etc.)
// NOTE: cross-check relationshipProjection with Relationship Types in your graph
// If Compound_Disease is not a relationship in your graph, remove it from relationshipProjection
call gds.betweenness.stream({
  nodeProjection: 'Hetionet_Nodes',
  relationshipProjection: ['Disease_Gene', 'Disease_Anatomy', 'Compound_Gene', 'Anatomy_Gene', 'Gene_BiologicalProcess', 'Gene_Pathway', 'Compound_Disease']
  })
YIELD nodeId, score
return gds.util.asNode(nodeId).name AS name, gds.util.asNode(nodeId).type AS nodeType ,score
order by score desc limit 100;

// Cypher query to compute degree cloeseness centrality of all graph nodes
// Results are limited to top 100 nodes and include the 'type' of node (i.e., Gene, Anatomy, Compound, etc.)
// NOTE: cross-check relationshipProjection with Relationship Types in your graph
// If Compound_Disease is not a relationship in your graph, remove it from relationshipProjection
call gds.alpha.closeness.stream({
  nodeProjection: 'Hetionet_Nodes',
  relationshipProjection: ['Disease_Gene', 'Disease_Anatomy', 'Compound_Gene', 'Anatomy_Gene', 'Gene_BiologicalProcess', 'Gene_Pathway', 'Compound_Disease']
  })
YIELD nodeId, centrality
return gds.util.asNode(nodeId).name AS name, gds.util.asNode(nodeId).type AS nodeType, centrality
order by centrality desc limit 100

// Cypher query to compute degree cloeseness centrality of all graph nodes
// Results are limited to top 100 nodes and include the 'type' of node (i.e., Gene, Anatomy, Compound, etc.)
// NOTE: cross-check relationshipProjection with Relationship Types in your graph
// If Compound_Disease is not a relationship in your graph, remove it from relationshipProjection
call gds.pageRank.stream({
  nodeProjection: 'Hetionet_Nodes',
  relationshipProjection: ['Disease_Gene', 'Disease_Anatomy', 'Compound_Gene', 'Anatomy_Gene', 'Gene_BiologicalProcess', 'Gene_Pathway', 'Compound_Disease']
  })
YIELD nodeId, score
return gds.util.asNode(nodeId).name AS name, gds.util.asNode(nodeId).type AS nodeType ,score
order by score desc limit 100
