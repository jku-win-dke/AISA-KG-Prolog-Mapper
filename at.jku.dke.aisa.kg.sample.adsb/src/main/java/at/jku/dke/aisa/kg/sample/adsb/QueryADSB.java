package at.jku.dke.aisa.kg.sample.adsb;

import at.jku.dke.aisa.kg.AbstractInternalModule;
import at.jku.dke.aisa.kg.InternalModule;

public class QueryADSB extends AbstractInternalModule implements InternalModule  {
	
	public QueryADSB() {
		super("qadsb");
	}
	
	public void doRun() {
		executeSparqlSelect(
			preprocessSparql("""
				SELECT 
					( COUNT(DISTINCT(?states)) AS ?noOfStates ) 
					( COUNT(DISTINCT(?states_all)) AS ?noOfStates_all ) 
				WHERE { 
					{ GRAPH ?G_adsb_new { ?states a adsb:FlightState. }  }
					UNION 
					{ GRAPH ?G_adsb_all { ?states_all a adsb:FlightState. } } }
				"""),
			qs -> System.out.println("" + qs.getLiteral("noOfStates").getInt() + "/"
			   		+qs.getLiteral("noOfStates_all").getInt()
			   		+" flight states added since last invocation.")
		);
	}
	
}
