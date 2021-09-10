package at.jku.dke.aisa.kg.sample.adsb;

import org.apache.jena.rdfconnection.RDFConnection;

import at.jku.dke.aisa.kg.AbstractInternalModule;
import at.jku.dke.aisa.kg.InternalModule;

/**
 *     
 * */
public final class ADSBProcessor1 extends AbstractInternalModule implements InternalModule  {
	
	RDFConnection adsbCon;
	
	public ADSBProcessor1() {
		super("adsbP1");
	}

	/* Override the log method per engine to easily specify the level of logging per engine 
	 * Having the log method is in development/debugging mode of the engine/module
	 * */
//	public void log(String str) {
//		System.out.println(str);
//	}
	
	public void doInit() {}

	public void doRun() {
		
		executeSparqlUpdate(preprocessSparql("""
				INSERT { GRAPH ?TURN { 
					[] aisa:state ?state; 
						rdf:type aisa:LaggingState; 
						aisa:lag ?lag. } } 
				WHERE { GRAPH ?G12_adsb_new { 
					?state adsb:requestTime ?rtime;
						adsb:hasTimePosition ?ptime. }
					FILTER (?rtime > ?ptime)
					BIND ((?rtime - ?ptime) AS ?lag)  }  
				"""));
		
		executeSparqlSelect(preprocessSparql("""
				SELECT ?state ?lag ?flight
				WHERE { GRAPH ?TURN { [] aisa:state ?state; aisa:lag ?lag. }
					{ GRAPH ?g { ?state adsb:flight ?flight. } }
				}
				"""),
				qs -> System.out.println(
						"Time lag of " + qs.getLiteral("lag").getInt() 
						+ " of flight " + qs.getResource("flight").getLocalName() 
						+ " with state " + qs.getResource("state").getLocalName() + "."));
		
		System.out.println("adsbP1 finished");
		
	}
}
