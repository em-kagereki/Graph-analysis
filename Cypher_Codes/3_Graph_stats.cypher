//Cypher query to visualize graph schema
CALL db.schema.visualization YIELD nodes, relationships

//Queries to count number of nodes and relaitons in the graph

// Count total number of nodes
match (n:Hetionet_Nodes)
return count(n) as total_number_of_nodes;

// Count Gene nodes
match (n:Gene)
return count(n) as total_number_of_genes;

// Count Compound nodes
match (n:Compound)
return count(n) as total_number_of_compounds;

// Count Disease nodes
match (n:Disease)
return count(n) as total_number_of_diseases;

// Count BiologicalProcess nodes
match (n:BiologicalProcess)
return count(n) as total_number_of_BiologicalProcess;

// Count Pathway nodes
match (n:Pathway)
return count(n) as total_number_of_Pathway;

// Count total number of relations
match (:Hetionet_Nodes)-[r]-(:Hetionet_Nodes)
return count(r);

// Count total number of relations between Genes and Diseases
match (:Gene)-[r]-(:Disease)
return count(r);

// Count total number of relations between Compounds and Diseases
match (:Compound)-[r]-(:Disease)
return count(r);

// Count total number of relations between Disease and Anatomy
match (:Disease)-[r]-(:Anatomy)
return count(r);

// Count total number of relations between Anatomy and Gene
match (:Anatomy)-[r]-(:Gene)
return count(r);

// Count total number of relations between Gene and BiologicalProcess
match (:Gene)-[r]-(:BiologicalProcess)
return count(r);

// Count total number of relations between Gene and Pathway
match (:Gene)-[r]-(:Pathway)
return count(r);
