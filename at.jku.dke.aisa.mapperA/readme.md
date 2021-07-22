# Mapping Method A
Mapping method A generates with the help of SPARQL a set of Prolog predicates from the RDFS/SHACL schema 
and a SPARQL query for each predicate. 
The result of executing the SPARQL query gives the facts for the respective predicate. 
These facts are written to a file and that file can be loaded into Prolog.

## How to start the mapper
1. Add all prefixes, data and shacl files, which should be mapped, to the input folder.
2. Start Jena Fuseki with any of the given configurations (e.g.: AISA-fuseki-server-mem.bat) or by configuring the jena fuseki server in Eclipse.
3. Run Shacl2PrologLauncher in Eclipse.
4. The Output can be found in the output folder.

Note: Jena Fuseki 3.17 does not work, rather use the given version 3.16 or try with the latest.\
Note: In case you want to restart the mapping, make sure to close the Jena Fuseki server first if started manually.

## Input Files
There are 3 input files which are used for mapping: the shacl schema, the data and the prefixes.\
KG-Schema can be defined in multiple RDFS/SHACL files which will be unionend into the Schema-Named-Graph in the KG. 
There must not be an overlap between these files, i.e., every SHACL shape must be defined in exactly one RDFS/SHACL file, otherwise definitions in blank nodes get duplicated.


### Shacl Schema
The shacl schema is uploaded to the Jena Fuseki sever at runtime. The shacl schema is used to create a shacl graph and parse shacl shapes. The graph and the shapes are 
needed for creation of the SPARQL queries and the Prolog facts.\
All shacl schema files which are in the /input/schema/ folder will be uploaded to the Jena Fuseki server and be used for mapping.\
Example file: /input/schema/donlon-shacl.ttl
### Data
The data is uploaded to the Jena Fuseki server at runtime. The data will be later retrieved by the SPARQL queries and the results processed to Prolog facts.\
All data files which are in the /input/data/ folder will be uploaded to the Jena Fuseki server and be used for mapping.\
Example file: /input/data/donlon-data.ttl
### Prefixes
The prefix files should contain all prefixes which are used in the shacl schema and in the data file.
These prefixes are mainly required to replace the URIs in the resulting fact files by their keys.\
Example file: /input/prefixes.ttl

## Output Files
The mapper outputs 2 different files, which can be found in the output folder: the sparql queries and the Prolog facts.

### SPARQL queries
The SPARQL queries are generated at runtime, saved to a file and executed.
The results of the query execution are used for Prolog facts generation.\
Example file: /output/queries.sparql
### Prolog facts
The Prolog facts are generated and saved to a file at runtime by processing the results of the SPARQL queries and using the prefix mapping defined in the prefix file.\
Example file: /output/facts.pl