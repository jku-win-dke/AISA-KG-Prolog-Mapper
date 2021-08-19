package at.jku.dke.aisa.mapperB;

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
	
	private static final int JENA_FUSEKI_PORT = 3030;
	private static final String JENA_FUSEKI_PATH = "/test/sparql";
	
	private static final int NUMBER_OF_DATA_COPIES = 1;
	private static final String INPUT_DATA = "input/data";
	private static final String INPUT_SCHEMA = "input/schema";

	private static final Logger LOGGER = LoggerFactory.getLogger(Shacl2PrologLauncher.class);

	private final static String LOCALHOST_3030 = "http://localhost:3030/test/";
	
	private final static String PREFIXES_FILE = "input/prefixes.ttl";
	
	private final static String SCHEMA_GRAPH_NAME = "https://github.com/jku-win-dke/aisa/graphs/schema";
	private final static String DATA_GRAPH_NAMESPACE = "https://github.com/jku-win-dke/aisa/graphs";
	
	private final static String PROLOG_FILE = "output/sparql.pl";
	
	public static void main(String[] args) {
		
		int data_copies = NUMBER_OF_DATA_COPIES;
		
		if(args != null && args.length > 0) {
			data_copies = Integer.parseInt(args[0]);
		}
		System.out.println("Number of data copies: " + data_copies);
		
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
		
		for(int j = 0 ; j<data_copies ; j++) {
			File dir2 = new File(INPUT_DATA);     
			File[] files2 = dir2.listFiles();
			for (int i = 0; i < files2.length; i++) {
			  File file2 = files2[i];
			  fuseki.load(DATA_GRAPH_NAMESPACE + "/" + j + "_" + file2.getName(), file2.getAbsolutePath());
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
		File sparqlFile = createFile(PROLOG_FILE);
		try(PrintWriter printWriter = new PrintWriter(sparqlFile)) {
			
			mapper.printStaticContent(printWriter);
			mapper.generatePrologModulesForQuerying(printWriter, JENA_FUSEKI_PORT, JENA_FUSEKI_PATH);
			mapper.generateStaticContent(printWriter);
			mapper.generatePrologRules(printWriter);
			mapper.printInheritanceRules(printWriter);
				
		} catch (FileNotFoundException e) {
			String message = String.format("File %s could not be found.", PROLOG_FILE);
			LOGGER.debug(message);
		} finally {
			LOGGER.debug("Shacl2Sparql mapping completed. Result can be found in " + PROLOG_FILE);
		}
			
		long time_six = System.currentTimeMillis();

		new Query("consult('output/program.pl')").hasSolution();
		
		long time_seven = System.currentTimeMillis();
		
		new Query("run").hasSolution();
		
		long time_eight = System.currentTimeMillis();
		
		new Query("save").hasSolution();

		long time_nine = System.currentTimeMillis();
		
		fuseki.load("http://ex.org/new", "output/output.ttl");

        long endTime = System.currentTimeMillis();        
        long timeElapsed = endTime - startTime;
        
        System.out.println("Jena Fuseki connection establishment: " + (time_one - startTime));
        System.out.println("Loading shacl schema files: " + (time_two - time_one));
        System.out.println("Loading data files: " + (time_three - time_two));
        System.out.println("Fetching shacl schema: " + (time_four - time_three));
        System.out.println("Creating KnowledgeGraphClasses and KnowledgeGraphProperties: " + (time_five - time_four));
        System.out.println("Creating Prolog file with embedded SPARQL queries: " + (time_six - time_five));
        System.out.println("Consult Program: " + (time_seven - time_six));
        System.out.println("Invoke run/0 in Prolog: " + (time_eight - time_seven));
        System.out.println("Invoke save/0 in Prolog: " + (time_nine - time_eight));
        System.out.println("Load saved results to Fuseki: " + (endTime - time_nine));
        System.out.println();
        System.out.println("Execution time in milliseconds: " + timeElapsed);        
        
        
        fuseki.fetch("http://ex.org/new").write(System.out, "TURTLE");
        
        File performance_results_csv_file = new File(PERFORMANCE__RESULTS_CSV);
        boolean isFileNewlyCreated = false;
        try {
        	isFileNewlyCreated = performance_results_csv_file.createNewFile();
		} catch (IOException e1) {
			String message = String.format("File %s could not be found.", PERFORMANCE__RESULTS_CSV);
			LOGGER.debug(message);
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
            			+ ";" + "Creating Prolog file with embedded SPARQL queries" //
            			+ ";" + "Consult Program" //
            			+ ";" + "Invoke run/0 in Prolog" //
            			+ ";" + "Invoke save/0 in Prolog" //
            			+ ";" + "Load saved results to Fuseki" //
            			+ ";" + "Execution time in milliseconds" //
            			);
            }
        	pw.println(System.currentTimeMillis()
            		+ ";B"
            		+ ";" + data_copies
            		+ ";" + (time_one - startTime)
               		+ ";" + (time_two - time_one)
               		+ ";" + (time_three - time_two)
               		+ ";" + (time_four - time_three)
               		+ ";" + (time_five - time_four)
               		+ ";" + (time_six - time_five)
               		+ ";" + (time_seven - time_six)
               		+ ";" + (time_eight - time_seven)
               		+ ";" + (time_nine - time_eight)
               		+ ";" + timeElapsed);
        } catch (FileNotFoundException e) {
			String message = String.format("File %s could not be found.", PERFORMANCE__RESULTS_CSV);
			LOGGER.debug(message);
		}
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
