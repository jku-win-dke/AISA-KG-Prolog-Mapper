package at.jku.dke.aisa.mapperA;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;

import org.apache.jena.graph.Graph;
import org.apache.jena.rdfconnection.RDFConnectionFactory;
import org.apache.jena.rdfconnection.RDFConnectionFuseki;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.shacl.Shapes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Main class to map shacl shapes and data to sparql queries and Prolog facts.
 * An additional prefix file is used for the prefix mapping of the facts.
 * 
 * Input files: shacl shapes, data and prefixes
 * Output files: sparql queries, Prolog facts
 */
public class Shacl2PrologLauncher {

	private static final Logger LOGGER = LoggerFactory.getLogger(Shacl2PrologLauncher.class);

	private final static String LOCALHOST_3030 = "http://localhost:3030/test/";
	
	private final static String SHACL_SCHEMA = "input/donlon-shacl.ttl";
	private final static String DATA_FILE = "input/donlon-data.ttl";
	
//	private final static String SHACL_SCHEMA = "input/plain-shacl.ttl";
//	private final static String DATA_FILE = "input/plain-data.ttl";
	
	private final static String PREFIXES_FILE = "input/prefixes.ttl";
	
	private static final String DATA_GRAPH_NAME = "https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml";
	private final static String GRAPH_NAME = "https://github.com/jku-win-dke/aisa/graphs/schema";
	
	private final static String SPARQL_FILE = "output/donlon-queries.sparql";
	private final static String FACTS_FILE = "output/donlon-facts.pl";
	
	public static void main(String[] args) {
		
		// can be configured to start jena fuseki server otherwise just start jena fuseki manually
//		ProcessBuilder builder = new ProcessBuilder("cmd", "/c", this.floraBatchFileInWorkingDir);
//		builder.directory( new File(this.workingDir) );
//		builder.redirectErrorStream(true); // so we can ignore the error stream
// 		builder.start();
//		Process process;
//		process = builder.start();
//		process.destroy();
		
		// jena fuseki setup
		RDFConnectionFuseki fuseki = RDFConnectionFactory.connectFuseki(LOCALHOST_3030);
		fuseki.loadDataset(SHACL_SCHEMA);
		fuseki.load(GRAPH_NAME, SHACL_SCHEMA);
		fuseki.load(DATA_GRAPH_NAME, DATA_FILE);
		
		// load shacl schema from local file
		Graph shapesGraph = RDFDataMgr.loadGraph(SHACL_SCHEMA);
		Shapes shapes = Shapes.parse(shapesGraph);
		
		// create KnowledgeGraphClasses and KnowledgeGraphProperties
		Mapper mapper = new Mapper(shapesGraph, shapes, PREFIXES_FILE, GRAPH_NAME);
		
		// create SPARQL file
		File sparqlFile = createFile(SPARQL_FILE);
		try(PrintWriter printWriter = new PrintWriter(sparqlFile)) {
		
			mapper.generateSPARQLQueries(printWriter);
				
		} catch (FileNotFoundException e) {
			String message = String.format("File %s could not be found.", SPARQL_FILE);
			LOGGER.debug(message);
		} finally {
			LOGGER.debug("Shacl2Sparql mapping completed. Result can be found in " + SPARQL_FILE);
		}
			
		// create facts file
		File factsFile = createFile(FACTS_FILE);
		try(PrintWriter printWriter = new PrintWriter(factsFile)) {
		
			printStaticContent(printWriter);
			mapper.printPrefixRegistration(printWriter);
			mapper.printRDFMeta(printWriter);
			
			QueryExecuter queryExecuter = new QueryExecuter(SPARQL_FILE, fuseki, mapper, printWriter);
			queryExecuter.execute();
				
		} catch (FileNotFoundException e) {
			String message = String.format("File %s could not be found.", FACTS_FILE);
			LOGGER.debug(message);
		} finally {
			LOGGER.debug("SPARQL2Prolog mapping completed. Result can be found in " + FACTS_FILE);
		}
	}

	/**
	 * Print static content needed to execute the file in Prolog.
	 * 
	 * e.g.: modules used in Prolog or flags
	 */
	private static void printStaticContent(PrintWriter printWriter) {
		printWriter.println("/* use the new RDF-DB library ");
		printWriter.println("https://www.swi-prolog.org/pldoc/man?section=semweb-rdf11 */");
		printWriter.println(":- use_module(library(semweb/rdf11)).");
		printWriter.println();
		printWriter.println("/* for writing/reading RDF files in Turtle format we use the Turtle library");
		printWriter.println("https://www.swi-prolog.org/pldoc/man?section=turtle */");
		printWriter.println(":- use_module(library(semweb/turtle)). ");
		printWriter.println();
		printWriter.println("/* do not output bindings for anonymous variables (e.g., _X) in query results ");
		printWriter.println("https://www.swi-prolog.org/pldoc/man?section=flags#flag:toplevel_print_anon */");
		printWriter.println(":- set_prolog_flag(toplevel_print_anon, false).");
		printWriter.println();
		printWriter.println("/* PREFIX HANDLING ");
		printWriter.println("see: https://www.swi-prolog.org/pldoc/doc/_SWI_/library/semweb/rdf_prefixes.pl");
		printWriter.println("*/");
		printWriter.println();
		printWriter.println("/* declare namespace prefixes - in addition to predeclared ones ");
		printWriter.println("https://www.swi-prolog.org/pldoc/doc_for?object=rdf_register_prefix/2");
		printWriter.println("*/");
	}

	/**
	 * Creates a new File and deletes old content if the File already exists.
	 * @return
	 */
	private static File createFile(String name) {
		File sparqlFile = new File(name);
		try {
			if(!sparqlFile.createNewFile()) {
				// delete old content of generated file
				new PrintWriter(name).close();
			}
		} catch (FileNotFoundException e) {
			String message = String.format("File %s coult not be found.", name);
			LOGGER.debug(message);
		} catch (IOException e) {
			String message = String.format("Error when creating %s.", name);
			LOGGER.debug(message);
		}
		return sparqlFile;
	}
}
