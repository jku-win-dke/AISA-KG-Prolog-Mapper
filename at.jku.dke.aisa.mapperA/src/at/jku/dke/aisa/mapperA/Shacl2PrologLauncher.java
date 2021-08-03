package at.jku.dke.aisa.mapperA;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;

import org.apache.jena.graph.Graph;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdfconnection.RDFConnectionFactory;
import org.apache.jena.rdfconnection.RDFConnectionFuseki;
import org.apache.jena.shacl.Shapes;
import org.jpl7.Query;
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

	private static final String PERFORMANCE__RESULTS_CSV = "output/performance_results.csv";
	private static final int NUMBER_OF_DATA_COPIES = 10;
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
		
		System.out.println(args);
		
		// Start Jena Fuseki manually and close it after execution.
		// Running Jena Fuseki at runtime works, however, the performance decreases heavily. 
		
//		Process process = null;
//		try {
//			process = startJenaFuseki();
//		} catch (IOException e2) {
//			// TODO Auto-generated catch block
//			e2.printStackTrace();
//		}
		
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
		
		long time_seven = System.currentTimeMillis();

        
		new Query("consult('output/program.pl')").hasSolution();
		
		long time_eight = System.currentTimeMillis();
		
		new Query("run").hasSolution();
		
		long time_nine = System.currentTimeMillis();
		
		new Query("save").hasSolution();

		long time_ten = System.currentTimeMillis();


		
		
		fuseki.load("http://ex.org/new", "output/output.ttl");

        long endTime = System.currentTimeMillis();        
        long timeElapsed = endTime - startTime;
        
//		process.destroy();
//		System.out.println(process.exitValue());
//		
//		byte[] bytes = null;
//		try {
//			bytes = new byte[process.getInputStream().available()];
//			process.getInputStream().read(bytes);
//		} catch (IOException e2) {
//			// TODO Auto-generated catch block
//			e2.printStackTrace();
//		}
//		System.out.println(new String(bytes));

        
        System.out.println("Jena Fuseki connection establishment: " + (time_one - startTime));
        System.out.println("Loading shacl schema files: " + (time_two - time_one));
        System.out.println("Loading data files: " + (time_three - time_two));
        System.out.println("Fetching shacl schema: " + (time_four - time_three));
        System.out.println("Creating KnowledgeGraphClasses and KnowledgeGraphProperties: " + (time_five - time_four));
        System.out.println("Creating SPARQL file: " + (time_six - time_five));
        System.out.println("Executing SPARQL queries and creating Prolog facts: " + (time_seven - time_six));
        System.out.println("Consult Program: " + (time_eight - time_seven));
        System.out.println("Invoke run/0 in Prolog: " + (time_nine - time_eight));
        System.out.println("Invoke save/0 in Prolog: " + (time_ten - time_nine));
        System.out.println("Load saved results to Fuseki: " + (endTime - time_ten));
        System.out.println();
        System.out.println("Execution time in milliseconds: " + timeElapsed);        
        
        
        fuseki.fetch("http://ex.org/new").write(System.out, "TURTLE");
        

        /* in ein Log-File schreiben und dann in z.B. Excel auswerten */
        System.out.println(
        		System.currentTimeMillis()
        		+ ";A"
        		+ ";" + NUMBER_OF_DATA_COPIES
        		+ ";" + (time_one - startTime)
           		+ ";" + (time_two - time_one)
           		+ ";" + (time_three - time_two)
           		+ ";" + (time_four - time_three)
           		+ ";" + (time_five - time_four)
           		+ ";" + (time_six - time_five)
           		+ ";" + (time_seven - time_six)
           		+ ";" + (time_eight - time_seven)
           		+ ";" + (time_nine - time_eight)
           		+ ";" + (time_ten - time_nine)
           		+ ";" + timeElapsed);
        
        File performance_results_csv_file = new File(PERFORMANCE__RESULTS_CSV);
        boolean isFileNewlyCreated = false;
        try {
        	isFileNewlyCreated = performance_results_csv_file.createNewFile();
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
        try(PrintWriter pw = new PrintWriter(new FileOutputStream(performance_results_csv_file, true))) {
            if(isFileNewlyCreated) {
            	pw.println(
            			"Current time milliseconds" //
            			+ ";" + "Mapping Variant" //
            			+ ";" + "Number of data copies" //
            			+ ";" + "Jena Fuseki connection establishment" //
            			+ ";" + "Loading shacl schema files" //
            			+ ";" + "Loading data files" //
            			+ ";" + "Fetching shacl schema" //
            			+ ";" + "Creating KnowledgeGraphClasses and KnowledgeGraphProperties" //
            			+ ";" + "Creating SPARQL file" //
            			+ ";" + "Executing SPARQL queries and creating Prolog facts" //
            			+ ";" + "Consult Program" //
            			+ ";" + "Invoke run/0 in Prolog" //
            			+ ";" + "Invoke save/0 in Prolog" //
            			+ ";" + "Load saved results to Fuseki" //
            			+ ";" + "Execution time in milliseconds" //
            			);
            }
        	pw.println(System.currentTimeMillis()
            		+ ";A"
            		+ ";" + NUMBER_OF_DATA_COPIES
            		+ ";" + (time_one - startTime)
               		+ ";" + (time_two - time_one)
               		+ ";" + (time_three - time_two)
               		+ ";" + (time_four - time_three)
               		+ ";" + (time_five - time_four)
               		+ ";" + (time_six - time_five)
               		+ ";" + (time_seven - time_six)
               		+ ";" + (time_eight - time_seven)
               		+ ";" + (time_nine - time_eight)
               		+ ";" + (time_ten - time_nine)
               		+ ";" + timeElapsed);
        } catch (FileNotFoundException e) {
			String message = String.format("File %s could not be found.", PERFORMANCE__RESULTS_CSV);
			LOGGER.debug(message);
		}
	}

//	private static Process startJenaFuseki() throws IOException {
//		// can be configured to start jena fuseki server otherwise just start jena fuseki manually
//		ProcessBuilder builder = new ProcessBuilder("java", "-Xmx1200M", "-jar", "fuseki-server.jar", "--config", "AISA-config-mem.ttl");
//		builder.directory( new File("apache-jena-fuseki-3.16.0") );
//		builder.redirectErrorStream(true); // so we can ignore the error stream
//		
//		Process process = builder.start();
//		
//		return process;
//	}

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
