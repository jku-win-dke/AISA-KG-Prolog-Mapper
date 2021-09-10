package at.jku.dke.aisa.kg;

import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.jena.query.ParameterizedSparqlString;

public abstract class AbstractMultipleRunModule extends AbstractKGModule implements MultipleRunModule {

	int turn = 0;
	private long previousInvocationTime = 0;
	
	public final void setPreviousInvocationTime(long time) {
		previousInvocationTime = time;
	}
	
	public AbstractMultipleRunModule(String name) {
		super(name);
	}
	
	protected void incTurn() {
		turn++;		
	}

	public final int getTurn() {
		return turn;
	}	

	public String getTurnIri() {
		return "http://aisa-project.eu/graphs/" + getName() + "-" + getTurn() ;
	}
	
	public long getPreviousInvocationTime() {
		return previousInvocationTime;
	}

	public String getOutputPath(String filenametemplate) {
		String filename = filenametemplate.replace("$TURN$", ""+getTurn());
		return super.getOutputPath(filename);
	}
	
	public String getInputPath(String filenametemplate) {
		String filename = filenametemplate.replace("$TURN$", ""+getTurn());
		return super.getInputPath(filename);
	}
		
	final void initTurn() {
		incTurn();

		ParameterizedSparqlString pss_previoustime = preprocessSparql("""
				SELECT (COALESCE(MAX(?time),0) AS ?t) 
				WHERE { GRAPH ?turn {?turn  aisa:invocationTime ?time; aisa:module ?MODULE . } }
			""");
		executeSparqlSelect(pss_previoustime,
				qs->setPreviousInvocationTime(qs.getLiteral("t").getLong()));
		log(getName() + " previousTime: " + previousInvocationTime );	
		
		executeSparqlUpdate(preprocessSparql("""
				INSERT DATA { GRAPH ?TURN { 
					?TURN a aisa:Turn;
						aisa:invocationTime ?TIMESTAMP;
						aisa:module ?MODULE;
						aisa:turnNo ?TURN_NO. 
				} }
			    """));
	}
	
	/** A new-data-graph is committed by setting a 'logical' commitTime (which is the start-time of the 'transaction'), 
	 * for internal modules invocationTime and commitTime are the same 
	 * for external modules a turn consists of an export and an import transaction, invocationTime=exportTime, commitTime=importTime*/
	final void commitTurn() {
		executeSparqlUpdate(preprocessSparql("""
				INSERT DATA { GRAPH ?TURN { ?TURN aisa:commitTime ?TIMESTAMP. } }
			"""));
	}
		
	public ParameterizedSparqlString preprocessSparql(String sparql) {
		String str = extendWithGraphPatterns(sparql);
		ParameterizedSparqlString pss = super.preprocessSparql(str);
		pss.setIri("TURN",getTurnIri());
		pss.setLiteral("PREV_TIME",getPreviousInvocationTime());
		pss.setLiteral("TURN_NO",getTurn());
		return pss;
	}

	
	public String extendWithGraphPatterns(String sparql) {
		Matcher matcher1 = Pattern.compile("GRAPH\\s+\\?(G\\d*)\\_(\\w+)\\_(\\w+)\\s+\\{").matcher(sparql);
		HashMap<String,String> specialGraphVars = new HashMap<String,String>();
		while ( matcher1.find() ) {
			String moduleName = matcher1.group(2);
			String mode = matcher1.group(3);
			String moduleIri;
			String graphVar = matcher1.group(1)+ "_" + moduleName + "_" + mode;
			if(specialGraphVars.containsKey(graphVar))
				continue;
			if(! kg.modules.containsKey(moduleName)) 
				throw new RuntimeException(moduleName + "does not exists");
			moduleIri = kg.modules.get(moduleName).getModuleIri();
			/* always restrict to the given module */
			String replacement = 
					"GRAPH ?" + graphVar + " {?" + graphVar + " aisa:module <" + moduleIri + ">; \n"
	                  + " aisa:commitTime ?time_" + graphVar + ".\n"
	                  + "   FILTER ( ?time_" + graphVar + " < " + getLogicalTime() + ") \n";
			if(mode.equals("all")) 
				replacement += "";
			else if(mode.equals("new")) 
				replacement += "  FILTER ( ?time_" + graphVar + " > " + getPreviousInvocationTime() + ") \n";
			else if(mode.equals("old")) 
				replacement += "  FILTER ( ?time_" + graphVar + " < " + getPreviousInvocationTime() + ") \n";
			else
				throw new RuntimeException("Unknown SpecialGraphVars-Mode: " + mode);
			specialGraphVars.put(graphVar, replacement);
		}
		matcher1.reset();
		StringBuffer sb = new StringBuffer();
		while ( matcher1.find() ) { 
			String graphVar = matcher1.group(1)+ "_"  +  matcher1.group(2) + "_" +  matcher1.group(3);
			matcher1.appendReplacement( sb, specialGraphVars.get(graphVar));
		}
		matcher1.appendTail( sb );
		return sb.toString();
	} 
	
}
