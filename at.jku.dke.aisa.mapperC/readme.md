# Mapping Method C
The relevant part of the Knowledge Graphs is replicated in the main-memory RDF database of SWI-Prolog. The actual mapping is then formulated as Prolog 
rules with the RDF quadruples in the RDF database as input.

## How to start the mapper
1. Add all prefixes, data and shacl files, which should be mapped, to the input folder.
2. Start Jena Fuseki with any of the given configurations (e.g.: AISA-fuseki-server-mem.bat) or by configuring the jena fuseki server in Eclipse.
3. Run Shacl2PrologLauncher in Eclipse.
4. The Output can be found in the output folder.

Note: Jena Fuseki 3.17 does not work, rather use the given version 3.16 or try with the latest.\
Note: In case you want to restart the mapping, make sure to close the Jena Fuseki server first if started manually.

## Input Files
The shacl schema is uploaded to the Jena Fuseki sever at runtime. The shacl schema is used to create a shacl graph and parse shacl shapes. The graph and the shapes are 
needed for creation the Prolog module.\
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
The mapper outputs a file with Prolog rules and a file with the instance data, which can be found in the output folder.

### Prolog rules
The Prolog rules are generated and saved to a file at runtime by using the given shacl schema and the prefix mapping defined in the prefix file.\
Example file: /output/facts.pl
### Dataset
The instance data is saved to a file and loaded from the Prolog rules file at runtime in Prolog.
Example file: /output/dataset.trig
### Performance result
At the end of the execution of the mapping, the execution time is saved to a file.
Next to the overall execution time and the execution time of separate parts of the mapping, also the number of data copies and the mapping variant is saved to the performance result.
The number of data copies can be changed in the Shacl2PrologLauncher if required. (Default value=1)
Example file: /output/performance_results.csv

## How to start the performance tests
1. Go to Eclipse File->Export->Runnable Jar File.
2. Chose the main class of the mapping variant and the dedicated name (MapperA.jar|MapperB.jar|MappperC.jar)
 and select library handling 'Package required libraries into generated JAR'.
3. Export the jar file into the project folder of the respective mapping variant. (e.g.: /AISA-KG-Prolog-Mapper/at.jku.dke.aisa.mapperA/MapperA.jar)
3. Start start_performance_test.bat.
4. The results can be found in the output folder of the dedicated mapping variant.
