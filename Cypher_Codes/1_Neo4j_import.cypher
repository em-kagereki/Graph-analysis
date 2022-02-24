// Scripts to import data into Neo4j

// (1) import disease_gene.csv
load csv with headers from "file:///disease_genes.csv" as row
merge (n1:Disease {name: row.Disease, identifier: row.Disease_ID})
merge (n2:Gene {name: row.Gene, identifier: row.Gene_ID})
merge (n1)-[r:Disease_Gene {type: row.Relation}]-> (n2);

// (2) import compound_disease.csv
load csv with headers from "file:///compound_disease.csv" as row
merge (n1:Compound {name: row.Compound, identifier: row.Compound_ID})
merge (n2:Disease {name: row.Disease, identifier: row.Disease_ID})
merge (n1)-[r:Compound_Disease {type: row.Relation}]-> (n2);

// (3) import compound_gene.csv
load csv with headers from "file:///compound_gene.csv" as row
merge (n1:Compound {name: row.Compound, identifier: row.Compound_ID})
merge (n2:Gene {name: row.Gene, identifier: row.Gene_ID})
merge (n1)-[r:Compound_Gene {type: row.Relation}]-> (n2);

// (4) import disease_anatomy.csv
load csv with headers from "file:///disease_anatomy.csv" as row
merge (n1:Disease {name: row.Disease, identifier: row.Disease_ID})
merge (n2:Anatomy {name: row.Anatomy, identifier: row.Anatomy_ID})
merge (n1)-[r:Disease_Anatomy {type: row.Relation}]-> (n2);

// (5) import gene_anatomy.csv
load csv with headers from "file:///gene_anatomy.csv" as row
merge (n1:Anatomy {name: row.Anatomy, identifier: row.Anatomy_ID})
merge (n2:Gene {name: row.Gene, identifier: row.Gene_ID})
merge (n1)-[r:Anatomy_Gene {type: row.Relation}]-> (n2);

// (6) import gene_bioprocess.csv
load csv with headers from "file:///gene_bioprocess.csv" as row
merge (n1:Gene {name: row.Gene, identifier: row.Gene_ID})
merge (n2:BiologicalProcess {name: row.BiologicalProcess, identifier: row.BiologicalProcess_ID})
merge (n1)-[r:Gene_BiologicalProcess {type: row.Relation}]-> (n2);

// (7) import gene_pathway.csv
load csv with headers from "file:///gene_pathway.csv" as row
merge (n1:Gene {name: row.Gene, identifier: row.Gene_ID})
merge (n2:Pathway {name: row.Pathway, identifier: row.Pathway_ID})
merge (n1)-[r:Gene_Pathway {type: row.Relation}]-> (n2);
