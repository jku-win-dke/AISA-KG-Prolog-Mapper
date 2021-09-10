package at.jku.dke.aisa.kg.sample.adsb;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdfconnection.RDFConnection;

import at.jku.dke.aisa.kg.AbstractInternalModule;
import at.jku.dke.aisa.kg.GLOBAL;
import at.jku.dke.aisa.kg.InternalModule;

/**
 *     
 * */
public final class FlightPairs extends AbstractInternalModule implements InternalModule  {
	
	RDFConnection adsbCon;
		
	public FlightPairs() {
		super("pairs");
	}

	double calculateDistance(float lat1, float long1, float lat2, float long2) {
		return Math.sqrt(Math.pow(Math.abs(long1 - long2), 2) + Math.pow(Math.abs(long1 - long2),2));
	}
	
	public void doRun() {		
	
		executeSparqlUpdate(
			preprocessSparql("""
				INSERT { GRAPH ?TURN { 
					  ?iri  a  aisa:PairOfFlightStates;
						aisa:flight1  ?f1; aisa:state1 ?s1;
						aisa:flight2  ?f2; aisa:state2 ?s2;
						adsb:requestTime ?time.
				 } } 
				WHERE {
					GRAPH ?G_adsb_new { 
				    	?f1  a  adsb:Flight;  adsb:hasAircraftIcao24  ?f1ID.
				    	?f2  a  adsb:Flight;  adsb:hasAircraftIcao24  ?f2ID.
				    	?s1  adsb:flight  ?f1; adsb:requestTime ?time.
				    	?s2  adsb:flight  ?f2; adsb:requestTime ?time.
				    	
				    FILTER ( ?f1ID > ?f2ID )
				    BIND( (IRI(CONCAT("$AISAR$",?f1ID,"_",?f2ID,"_",STR(?time)))) AS ?iri)
					}
				}
				""".replace("$AISAR$",GLOBAL.NS_AISAR)));
			
		Model m = ModelFactory.createDefaultModel();
		
		executeSparqlSelect(
			preprocessSparql("""
				SELECT ?pair ?lat1 ?long1 ?lat2 ?long2 
				WHERE {
					GRAPH ?TURN {
						?pair aisa:state1 ?s1; aisa:state2 ?s2.
					}
					GRAPH ?G_adsb_new { 
				    	?s1  adsb:hasLatitude  ?lat1;  adsb:hasLongitude ?long1.
				    	?s2  adsb:hasLatitude  ?lat2;  adsb:hasLongitude ?long2.
					}
				}
				"""),
			qs -> m.add(
						qs.getResource("pair"),
						m.createProperty(GLOBAL.NS_AISA,"distance"),
						m.createTypedLiteral(
							
							calculateDistance(
								qs.getLiteral("lat1").getFloat(),
								qs.getLiteral("long1").getFloat(),
								qs.getLiteral("lat2").getFloat(),
								qs.getLiteral("long2").getFloat())
							
						)));

		loadGraphToKG(getTurnIri(), m);
		
		System.out.println("pairs: " + m.size() + " distances between flights inserted");
		
	}

}
