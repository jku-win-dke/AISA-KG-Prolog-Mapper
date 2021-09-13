# Knowledge Graph System

## How to start the knowledge graph system
1. Start Jena Fuseki with any of the given configurations (e.g.: AISA-fuseki-server-mem.bat) or by configuring the jena fuseki server in Eclipse.
3. Run one of the sample projects (at.jku.dke.aisa.kg.sample.adsb.KGSystem1, at.jku.dke.aisa.kg.sample.adsb.KGSystem2, at.jku.dke.aisa.kg.sample.prolog.KGSystem)
4. The Output can be found in the fileoutput folder of the sample project, which was executed.

Note: Jena Fuseki 3.17 does not work, rather use the given version 3.16 or try with the latest.\

## Dependencies within the projects
The repository consists of the following projects and dependencies, which can be found in the respective .classpath files:
* at.jku.dke.aisa.kg.sample.adsb --> has a dependency on -> at.jku.dke.aisa.kg
* at.jku.dke.aisa.kg.sample.prolog --> has a dependency on ->at.jku.dke.aisa.kg
* at.jku.dke.aisa.kg --> has a dependency on -> at.jku.dke.aisa.mapperC
* at.jku.dke.aisa.mapperA
* at.jku.dke.aisa.mapperB
* at.jku.dke.aisa.mapperC

Additionally, the installation of swipl is required, which can be downloaded from https://www.swi-prolog.org/download/stable. 
The default installation path "C:/Program Files/swipl" should be chosen. 
After installation, the system environment variable must be set for the bin folder of swipl (Variable "Path" with vaLue "C:\Program Files\swipl\bin").