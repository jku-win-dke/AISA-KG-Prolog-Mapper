package at.jku.dke.aisa.mapperC;

import java.io.PrintWriter;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdfconnection.RDFConnectionFuseki;

/**
 * Takes the SPARQL file as input and executes each query separately.
 * After query execution the resultSet is forwarded to the Mapper to create the Prolog facts.
 */
public class QueryExecuter {
	
	private String sparqlFile;
	private RDFConnectionFuseki fuseki;
	private Mapper mapper;
	private PrintWriter printWriter;
	
	public QueryExecuter(String sparqlFile, RDFConnectionFuseki fuseki, Mapper mapper, PrintWriter printWriter) {
		this.sparqlFile = sparqlFile;
		this.fuseki = fuseki;
		this.mapper = mapper;
		this.printWriter = printWriter;
	}

	/**
	 * Executes all queries from the SPARQL file.
	 */
	public void execute() {
		String[] queries = readQueries();
	    for (int i = 0; i < queries.length; i++) {
	    	KnowledgeGraphClass schemaForGeneratingFacts = mapper.getKnowledgeGraphClasses().get(i);
	    	printWriter.println(schemaForGeneratingFacts.generateComment());
	    	executeQuery(queries[i], schemaForGeneratingFacts);
	    	printWriter.println();
	    }
	}

	/**
	 * Reads all queries from the SPARQL file and saves them separately.
	 * @return
	 */
	private String[] readQueries() {
	    String[] queries = null;
	    try {
	        Path sqlPath = FileSystems.getDefault().getPath(sparqlFile);
	        String sql = new String(Files.readAllBytes(sqlPath));
	        queries = sql.split("\\r\\n\\r\\n\\r\\n|\\n\\n\\n|\\n\\n\\r\\n");
	    } catch (Exception e) {
	        System.out.println("ERROR: Failed to read sparql file.");
	    }
	    return queries; 
	}

	/**
	 * Executes the given query and forwards the result to the mapper to create the Prolog facts.
	 * 
	 * @param query
	 * @param schemaForGeneratingFacts
	 */
	private void executeQuery(String query, KnowledgeGraphClass schemaForGeneratingFacts) {
		try(QueryExecution queryExecution = fuseki.query(query)) {
			ResultSet resultSet = queryExecution.execSelect();
//			boolean printSchema = resultSet.hasNext();
//			if(printSchema) {
//				printWriter.println(schemaForGeneratingFacts.generateComment());
//			}
			while(resultSet.hasNext()) {
				QuerySolution querySolution = resultSet.nextSolution();
				printWriter.println(mapper.generateFact(querySolution, schemaForGeneratingFacts));
			}
//			if(printSchema) {
//				printWriter.println();
//			}
		}
	}
}
