package at.jku.dke.aisa.kg;

import org.apache.jena.query.ParameterizedSparqlString;

public abstract class AbstractInternalModule extends AbstractMultipleRunModule implements InternalModule {
	
	public AbstractInternalModule(String name) {
		super(name);
	}
	
	public final void run() {
		resetTimestamp();
		initTurn();
		doRun2();
		commitTurn();	
		
		ParameterizedSparqlString pss = preprocessSparql("""
				INSERT DATA { GRAPH ?TURN { ?TURN aisa:durationInMs ?DURATION. } }
			""");
		pss.setLiteral("DURATION",System.currentTimeMillis() - getPhysicalTime());
		executeSparqlUpdate(pss);
		
		/* note that named graphs from Prolog get replicated back to Prolog so that the metadata added here is also 
		 * available in Prolog */
		kg.copyFromKgToFileAndProlog(getTurnIri(),getOutputPath(getTurnName()+".ttl"));
		
	}
	
	/* used to add additional code in subclasses before or after doRun*/
	protected void doRun2() {
		doRun();
	}
	
	abstract protected void doRun();
	
	
	
}
