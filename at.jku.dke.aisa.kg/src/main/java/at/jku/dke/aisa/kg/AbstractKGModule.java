package at.jku.dke.aisa.kg;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Consumer;

import org.apache.jena.query.ParameterizedSparqlString;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.shared.PrefixMapping;

public abstract class AbstractKGModule implements KGModule {



	/** the local name of the module and its named graph */
	private final String module;
	
	/** is set to logical time of KG at the beginning of every transaction */
	private long logicalTime; 
	private long physicalTime; 
	private RDFConnection con;
	KGModuleSystem kg;
	private PrefixMapping prefixes;
	
	public AbstractKGModule(String name) {
		this.module = name;
		this.logicalTime = System.currentTimeMillis();
	}
	
	public final void register(KGModuleSystem kg) {
		this.kg = kg;
		this.con = kg.getConnection();
		this.prefixes = kg.getPrefixes();
	}
	
	protected final void resetTimestamp() {
		logicalTime = kg.getLogicalTime();
		physicalTime = System.currentTimeMillis();
	}
		
	public final String getName() {
		return this.module;
	}
	
	public String getModuleIri() {
		return GLOBAL.NS_GRAPHS + module ;
	}
	
	public final long getLogicalTime() {
		return logicalTime;
	}
	
	public final long getPhysicalTime() {
		return physicalTime;
	}
	
	public ParameterizedSparqlString preprocessSparql(String sparql) {
		ParameterizedSparqlString pss = new ParameterizedSparqlString(sparql);
		pss.setIri("MODULE",getModuleIri());
		pss.setLiteral("TIMESTAMP",getLogicalTime());
		pss.setNsPrefixes(prefixes);
		return pss;		
	}
	
	public final void init() {
		resetTimestamp();
		try {
			createOutputFolderIfNotExists();
		} catch (IOException e) {
			e.printStackTrace();
		}
		ParameterizedSparqlString pss = preprocessSparql("""
				INSERT DATA { GRAPH ?MODULE { 
				?MODULE a aisa:Module;
						aisa:creationTime ?TIMESTAMP; 
					    aisa:moduleClass ?MODULE_CLASS. } }
			""");
		pss.setLiteral("MODULE_CLASS", this.getClass().toString());
		executeSparqlUpdate(pss);	
		
		doInit();
		
		kg.copyFromKgToFileAndProlog(getModuleIri(),getName());
//		kg.copyFromKgToFileAndProlog(getModuleIri(),getOutputPath(getName()+".ttl"));
			
	}
	
	/** 
	 * doInit() is called from init() to facilitate custom content per module 
	 * per default doInit does nothing, can be overridden when necessary */
	protected void doInit() {};
	
	private final String getInputFolder() {
		return GLOBAL.FILEINPUT_PATH + "/" + getName();
	}
	
	private final String getOutputFolder() {
		return GLOBAL.FILEOUTPUT_PATH + "/" + getName();
	}

	public String getOutputPath(String filename) {
		return getOutputFolder() + "/" + filename;
	}
	
	public String getInputPath(String filename) {
		return getInputFolder() + "/" + filename;
	}
	
	public void createOutputFolderIfNotExists() throws IOException {
		Path moduleDir = Paths.get(getOutputFolder());
		if(Files.notExists(moduleDir)) {
			Files.createDirectory(moduleDir);
		}
	}
	
	public void log(String str) {
//		System.out.println(str);
	}
	
	public final void executeSparqlSelect(
			ParameterizedSparqlString sparql, 
			Consumer<QuerySolution> rowAction) 
	{	
		log("AbstractModule:executeSparqlSelect \n" + sparql.toString());
		con.querySelect(sparql.asQuery(),rowAction);
	}
	
	public final 
	Model executeSparqlConstruct(ParameterizedSparqlString sparql) {
		log("AbstractModule:executeSparqlConstruct \n" + sparql.toString());
		QueryExecution ex = con.query(sparql.asQuery());
		return ex.execConstruct();
	}
	
	public final boolean executeSparqlAsk(ParameterizedSparqlString sparql) {
		log("AbstractModule:executeSparqlAsk \n" + sparql.toString());
		QueryExecution ex = con.query(sparql.asQuery());
		return ex.execAsk();
	}
	
	public final void executeSparqlUpdate(ParameterizedSparqlString sparql) {
		log("AbstractModule:executeSparqlUpdate \n" + sparql.toString());
		con.update(sparql.asUpdate());
	}
	
	public final Model fetchFromKG(String graphUri) {
		log("AbstractModule:fetchFromKG> " + graphUri);
		Model m = con.fetch(graphUri);
		return m;
	}
	
	public final void loadGraphToKG(String graphUri, Model m) {
		log("AbstractModule:loadGraphToKG> " + graphUri + " (" + m.size()+ " Statements)");
		con.load(graphUri,m);
	}	
	
	public PrintWriter getPrintWriter(String filenametemplate) throws IOException {
		String filepath = getOutputPath(filenametemplate);
		return new PrintWriter(Files.newOutputStream(Paths.get(filepath)));
	}
	

	

	
}
