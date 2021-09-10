package at.jku.dke.aisa.kg.sample.adsb;

import org.apache.jena.rdfconnection.RDFConnection;

import at.jku.dke.aisa.kg.AbstractInternalModule;
import at.jku.dke.aisa.kg.InternalModule;

/**
 *     
 * */
public final class ADSBProcessor2 extends AbstractInternalModule implements InternalModule  {
	
	RDFConnection adsbCon;
	
	public ADSBProcessor2() {
		super("adsbP2");
	}

	public void doRun() {
		
		executeSparqlUpdate(preprocessSparql("""
				INSERT { GRAPH ?TURN { ?flight a aisa:IncomingFlight; aisa:firstRequestTime ?time. } } 
				WHERE { 
					SELECT ?flight (MIN(?rtime) AS ?time)
					WHERE {
						GRAPH ?G_adsb_new { 
								?state adsb:requestTime ?rtime;
									adsb:flight ?flight. }
						MINUS {
							GRAPH ?G_adsb_old { ?flight a adsb:Flight. } 
						}
					}					
					GROUP BY ?flight 
				} 
				"""));
		
		executeSparqlSelect(preprocessSparql("""
				SELECT DISTINCT ?icao ?country ?time
				WHERE { 
					GRAPH ?TURN { ?flight aisa:firstRequestTime ?time. }
					GRAPH ?g { 
						?flight adsb:hasAircarftIcao24 ?icao; 
							adsb:hasOriginCountry ?country. 
					} 
				}
				"""),
				qs -> System.out.println(
						"State of aircraft " + qs.getLiteral("icao").getString() 
						+ " from " + qs.getLiteral("country").getString() 
						+ " first retrieved at " + qs.getLiteral("time").getInt() + "."));
		
	}

}
