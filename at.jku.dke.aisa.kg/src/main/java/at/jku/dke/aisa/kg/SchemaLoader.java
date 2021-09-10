package at.jku.dke.aisa.kg;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;

import org.apache.jena.graph.Graph;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.shacl.Shapes;
import org.jpl7.Query;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import at.jku.dke.aisa.mapperC.Mapper;

public class SchemaLoader extends AbstractSingleRunModule {


	private final static String PREFIXES_FILE = "resources/prefixes.ttl";
//	private final static String FACTS_FILE = "output/facts.pl";
	private static final Logger LOGGER = LoggerFactory.getLogger(SchemaLoader.class);
	
	public SchemaLoader() {
		super("schema");
	}
	
	public String getModuleIri() {
		return GLOBAL.SCHEMA_GRAPH_NAME ;		
	}
	
	protected void doInit() {
		File dir = new File(getInputPath(""));     
		File[] files = dir.listFiles();
		for (int i = 0; i < files.length; i++) {
		  File file = files[i];
		  kg.con.load(this.getModuleIri(), file.getAbsolutePath());
		}
		
		
		Model model = kg.con.fetch(getModuleIri());
		Graph shapesGraph = model.getGraph();
		Shapes shapes = Shapes.parse(shapesGraph);
		
		// create KnowledgeGraphClasses and KnowledgeGraphProperties
		Mapper mapper = new Mapper(shapesGraph, shapes, PREFIXES_FILE, getModuleIri());
		
		
		// create facts file
		File factsFile = createFile(getOutputPath("facts.pl"));
		try(PrintWriter printWriter = new PrintWriter(factsFile)) {
		
			mapper.printStaticContent(printWriter);
			mapper.printPrefixRegistration(printWriter);
//			mapper.printLoadDataSet(printWriter);
			mapper.printRDFMeta(printWriter);
			mapper.printSubClassOfRules(printWriter, getModuleIri());
			
			mapper.generateFactRule(printWriter);
			
			mapper.printInheritanceRules(printWriter);
			
		} catch (FileNotFoundException e) {
			String message = String.format("File %s could not be found.", getOutputPath("facts.pl"));
			LOGGER.debug(message);
		} finally {
			LOGGER.debug("SPARQL2Prolog mapping completed. Result can be found in " + getOutputPath("facts.pl"));
		}
        
		new Query("consult('" + getOutputPath("facts.pl") + "')").hasSolution();
		
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
