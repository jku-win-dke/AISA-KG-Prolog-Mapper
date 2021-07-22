package at.jku.dke.aisa.mapperA;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;

import org.apache.jena.graph.Graph;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdfconnection.RDFConnectionFactory;
import org.apache.jena.rdfconnection.RDFConnectionFuseki;
import org.apache.jena.shacl.Shapes;
import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
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

	private static final int NUMBER_OF_DATA_COPIES = 1;
	private static final String INPUT_DATA = "input/data";
	private static final String INPUT_SCHEMA = "input/schema";

	private static final Logger LOGGER = LoggerFactory.getLogger(Shacl2PrologLauncher.class);

	private final static String LOCALHOST_3030 = "http://localhost:3030/test/";
	
	private final static String PREFIXES_FILE = "input/prefixes.ttl";
	
	private final static String SCHEMA_GRAPH_NAME = "https://github.com/jku-win-dke/aisa/graphs/schema";
//	private final static String DATA_GRAPH_NAMESPACE = "https://github.com/jku-win-dke/aisa/graphs/data";
	private final static String DATA_GRAPH_NAMESPACE = "https://github.com/jku-win-dke/aisa/graphs";//BN
	
	private final static String SPARQL_FILE = "output/queries.sparql";
	private final static String FACTS_FILE = "output/facts.pl";
	
	public static void main(String[] args) {
		
		// can be configured to start jena fuseki server otherwise just start jena fuseki manually
//		ProcessBuilder builder = new ProcessBuilder("cmd", "/c", this.floraBatchFileInWorkingDir);
//		builder.directory( new File(this.workingDir) );
//		builder.redirectErrorStream(true); // so we can ignore the error stream
// 		builder.start();
//		Process process;
//		process = builder.start();
//		process.destroy();
		
		long startTime = System.currentTimeMillis();
		
		// jena fuseki setup
		RDFConnectionFuseki fuseki = RDFConnectionFactory.connectFuseki(LOCALHOST_3030);

		long time_one = System.currentTimeMillis();
		
		File dir = new File(INPUT_SCHEMA);     
		File[] files = dir.listFiles();
		for (int i = 0; i < files.length; i++) {
		  File file = files[i];
		  fuseki.load(SCHEMA_GRAPH_NAME, file.getAbsolutePath());
		}
		
		long time_two = System.currentTimeMillis();
		
		for(int j = 0 ; j<NUMBER_OF_DATA_COPIES ; j++) {
			File dir2 = new File(INPUT_DATA);     
			File[] files2 = dir2.listFiles();
			for (int i = 0; i < files2.length; i++) {
			  File file2 = files2[i];
//			  fuseki.load(DATA_GRAPH_NAMESPACE + "/" + j + "/" + file2.getName(), file2.getAbsolutePath());
			  fuseki.load(DATA_GRAPH_NAMESPACE + "/" + j + "_" + file2.getName(), file2.getAbsolutePath());//BN
			}
		}
		
		long time_three = System.currentTimeMillis();
		
		Model model = fuseki.fetch(SCHEMA_GRAPH_NAME);
		Graph shapesGraph = model.getGraph();
		Shapes shapes = Shapes.parse(shapesGraph);
		
		long time_four = System.currentTimeMillis();
		
		// create KnowledgeGraphClasses and KnowledgeGraphProperties
		Mapper mapper = new Mapper(shapesGraph, shapes, PREFIXES_FILE, SCHEMA_GRAPH_NAME);
		
		long time_five = System.currentTimeMillis();
		
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
			
		long time_six = System.currentTimeMillis();
		
		// create facts file
		File factsFile = createFile(FACTS_FILE);
		try(PrintWriter printWriter = new PrintWriter(factsFile)) {
		
			mapper.printStaticContent(printWriter);
			mapper.printPrefixRegistration(printWriter);
			mapper.printRDFMeta(printWriter);
			
			QueryExecuter queryExecuter = new QueryExecuter(SPARQL_FILE, fuseki, mapper, printWriter);
			queryExecuter.execute();
			
			mapper.printInheritanceRules(printWriter);
			
		} catch (FileNotFoundException e) {
			String message = String.format("File %s could not be found.", FACTS_FILE);
			LOGGER.debug(message);
		} finally {
			LOGGER.debug("SPARQL2Prolog mapping completed. Result can be found in " + FACTS_FILE);
		}
		
        long endTime = System.currentTimeMillis();
        
        long timeElapsed = endTime - startTime;
        System.out.println("Jena Fuseki connection establishment: " + (time_one - startTime));
        System.out.println("Loading shacl schema files: " + (time_two - time_one));
        System.out.println("Loading data files: " + (time_three - time_two));
        System.out.println("Fetching shacl schema: " + (time_four - time_three));
        System.out.println("Creating KnowledgeGraphClasses and KnowledgeGraphProperties: " + (time_five - time_four));
        System.out.println("Creating SPARQL file: " + (time_six - time_five));
        System.out.println("Executing SPARQL queries and creating Prolog facts: " + (endTime - time_six));
        System.out.println();
        System.out.println("Execution time in milliseconds: " + timeElapsed);
        
		Query q1 = 
			    new Query( 
				"consult", 
				new Term[] {new Atom("C:\\Users\\neumayr\\git\\AISA-KG-Prolog-Mapper\\at.jku.dke.aisa.mapperA\\output\\program.pl")} 
			    );
		System.out.println( "consult " + (q1.hasSolution() ? "succeeded" : "failed"));
		new Query("run").hasSolution();
		
		fuseki.load("http://ex.org/new", "output/output.ttl");

        
        
        

	}

	/**
	 * Creates a new File and deletes old content if the File already exists.
	 * @return
	 */
	private static File createFile(String name) {
		File file = new File(name);
		try {
			if(!file.createNewFile()) {
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
		return file;
	}
}
