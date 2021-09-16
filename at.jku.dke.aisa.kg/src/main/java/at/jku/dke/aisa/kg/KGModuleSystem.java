package at.jku.dke.aisa.kg;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

import org.apache.commons.io.FileUtils;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.shared.PrefixMapping;
import org.jpl7.Query;
import org.jpl7.Term;

public class KGModuleSystem {


	ArrayList<KGModule> moduleList; //in the order they were registered
	Map<String,KGModule> modules;
	Map<String,SingleRunModule> staticModules;
	Map<String,InternalModule> internalModules;
	Map<String,ExternalModule> externalModules;
	RDFConnection con;
	PrefixMapping prefixes;
	long logicalTime;

	public KGModuleSystem(RDFConnection con, PrefixMapping prefixes) {
		moduleList = new ArrayList<KGModule>();
		modules = new HashMap<String,KGModule>();
		staticModules = new HashMap<String,SingleRunModule>();
		internalModules = new HashMap<String,InternalModule>();
		externalModules = new HashMap<String,ExternalModule>();
		this.con = con;
		this.prefixes = prefixes;
		
		new Query("consult('"+ GLOBAL.INIT_PROLOG_PROGRAM + "')").hasSolution();
	
	}
	
	public void register(KGModule mod) throws Exception  {
		
		if(modules.containsKey(mod.getName()))
			throw new Exception("Module with name" + mod.getName() + " already registered");
		
		moduleList.add(mod);
		
		modules.put(mod.getName(),mod);
		mod.register(this);

		if(mod instanceof ExternalModule)
			externalModules.put(mod.getName(),(ExternalModule) mod);
		else if(mod instanceof InternalModule)
			internalModules.put(mod.getName(),(InternalModule) mod);
		else if(mod instanceof SingleRunModule)
			staticModules.put(mod.getName(),(SingleRunModule) mod);
		else throw new Exception("Every model must be one of ExternalModule, InternalModule, StaticModule");
	}
	
	public void initAllModules() {
		for (KGModule mod : moduleList) {
			mod.init();
		}		
	}
	
	public RDFConnection getConnection() {
		return con;
	}
	
	public PrefixMapping getPrefixes() {
		return prefixes;
	}
	
	public void cleanKG() {
		System.out.println("Clean KG ...");
		con.update("""
				DELETE { GRAPH ?g { ?s ?p ?o } }
				WHERE { GRAPH ?g { ?s ?p ?o } };
		
				DELETE { ?s ?p ?o }
				WHERE { ?s ?p ?o }
				""");
	} 
	
	public void cleanOutputFolders() throws IOException {
		System.out.println("Clean Output Folders ...");
		FileUtils.cleanDirectory(new File(GLOBAL.FILEOUTPUT_PATH));
	}
	
	public void startConsoleApplication(InputStream in, PrintStream out) {
		Scanner scanner = new Scanner(in);
        out.print("Your commands: > ");
        while (true){
        	logicalTime++;
            String s = scanner.next();
            if(s.equals("exit")) break;
            else if(s.equals("run")) {
            	String mod = scanner.next();
            	if(internalModules.containsKey(mod))
            		internalModules.get(mod).run();
            	else
            		out.println("There is no internal module '"+mod+"'");
            }
            else if(s.equals("start")) {
            	String mod = scanner.next();
            	if(externalModules.containsKey(mod))
            		externalModules.get(mod).exportInput();
            	else
            		out.println("There is no external module '"+mod+"' to start");
            }
            else if(s.equals("finish")) {
            	String mod = scanner.next();
            	if(externalModules.containsKey(mod))
            		externalModules.get(mod).importResults();
            	else
            		out.println("There is no external module '"+mod+"' to finish");
            }
            else {
            	out.println("Wrong input: "+ s);
            	out.println("type one of the following:");
            	out.println("   exit");
            	out.println("   run <internal module>");
            	out.println("   start <external module>");
            	out.println("   finish <external module>");
            }
            System.out.print("> ");            
        }
        System.out.println("Bye...");
        scanner.close(); 
	}
	
	public long getLogicalTime() {
		return logicalTime;
	}

	public void setLogicalTime(long logicalTime) {
		this.logicalTime = logicalTime;
	}
	
	public void setLogicalTimeToNow() {
		this.logicalTime = System.currentTimeMillis();
	}
	
	
	/* should be called after finishing/committing a named graph */
	public void copyFromKgToFileAndProlog(String graphIri, String ttlFile) {
		Model model = con.fetch(graphIri);			
		copyFromKgToProlog(model, graphIri);
		
		try(OutputStream fileOut = Files.newOutputStream(Paths.get(ttlFile))) {
			RDFDataMgr.write(fileOut, model, Lang.TTL);
		} catch (IOException e) { e.printStackTrace(); }

	}
	
	/* should be called for replicating a non-finished named graph */
	public void copyFromKgToProlog(String graphIri) {
		Model model = con.fetch(graphIri);			
		copyFromKgToProlog(model, graphIri);		
	}
	
	
	/** replicate a named graph from the KG in Prolog-RDF-DB
	 * 
	 *  it is assumed that the named graph is committed and does not change anymore
	 *  
	 *  also writes a copy in TTL format to the file system
	 *  */
	public void copyFromKgToProlog(Model model, String graphIri) {
		
		try(OutputStream fileOut = Files.newOutputStream(Paths.get(GLOBAL.TEMPFILE_PATH_REPLICATE))) {
			RDFDataMgr.write(fileOut, model, Lang.RDFXML);
		} catch (IOException e) { e.printStackTrace(); }
		
		new Query("rdf_load('" + GLOBAL.TEMPFILE_PATH_REPLICATE + "',[graph('"+graphIri+"')])").hasSolution();
	}
	
	/** replicate a named graph from Prolog-RDF-DB in the KG 
	 * 
	 * it is assumed that the named graph is committed and does not change anymore
	 * */
	public void copyFromPrologToKg(String graphIri) {
		new Query("rdf_save('" + GLOBAL.TEMPFILE_PATH_REPLICATE + "',[graph('"+graphIri+"')])").hasSolution();
		con.load(graphIri, GLOBAL.TEMPFILE_PATH_REPLICATE); 	
	}


	
}
