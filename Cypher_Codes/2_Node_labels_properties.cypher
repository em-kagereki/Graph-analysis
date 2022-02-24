// Create a universal label (Hetionet_Nodes) to all nodes
//This will help with calling GDS functions for graph analysis
match (n:Gene)
set n:Hetionet_Nodes;
match (n:Compound)
set n:Hetionet_Nodes;
match (n:Pathway)
set n:Hetionet_Nodes;
match (n:Anatomy)
set n:Hetionet_Nodes;
match (n:BiologicalProcess)
set n:Hetionet_Nodes;
match (n:Disease)
set n:Hetionet_Nodes;

// Create new node properties to specify node type - e.g., Gene, Compound, Pathway, Anatomy, etc.
match (n:Gene)
set n.type = 'Gene';
match (n:Compound)
set n.type = 'Compound';
match (n:Pathway)
set n.type = 'Pathway';
match (n:Anatomy)
set n.type = 'Anatomy';
match (n:BiologicalProcess)
set n.type = 'BiologicalProcess';
match (n:Disease)
set n.type = 'Disease';
