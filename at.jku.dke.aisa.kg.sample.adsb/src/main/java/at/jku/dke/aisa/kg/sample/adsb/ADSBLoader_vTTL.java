package at.jku.dke.aisa.kg.sample.adsb;

import org.apache.jena.query.ParameterizedSparqlString;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.rdfconnection.RDFConnectionFactory;
import org.apache.jena.riot.RDFDataMgr;

import at.jku.dke.aisa.kg.*;

/**
 * Like ADSBLoader but reading the data from a directory of Turtle-Files.
 * The default.ttl is loaded into the default graph of the KG and the other files are loaded as named graphs 
 *     
 * */
public final class ADSBLoader_vTTL extends AbstractInternalModule implements InternalModule  {
	
	RDFConnection adsbCon;

	public ADSBLoader_vTTL() {
		super("adsb");
	}
	
	public void doInit() {		
	       adsbCon = RDFConnectionFactory.connect(
	    		   RDFDataMgr.loadDataset(getInputPath("ttl/default.ttl")));
		}
		
	public void doRun() {
		
		String graphName = "g"+(getTurn()-1);
		
		String inputGraphUri = GLOBAL.NS_GRAPHS + graphName;
		
		System.out.println(getName() + " reads: " + graphName + ".ttl");

		ResultSet rs = adsbCon.query("SELECT ?t WHERE { <"+inputGraphUri+"> "
				+ " <" + GLOBAL.NS_ADSB + "requestTime> ?t. }").execSelect();
		
		long requestTime = rs.next().getLiteral("t").getLong();
		System.out.println(getName() + " - Request time: " + requestTime);
		
		Model m = RDFDataMgr.loadModel("fileinput/adsb/ttl/" + graphName + ".ttl");
		loadGraphToKG(getTurnIri(),m);
		
		/* add the request time to each FlightState */		
		ParameterizedSparqlString pss = preprocessSparql("""
				WITH ?TURN 
				INSERT { ?flight adsb:requestTime ?REQUEST_TIME. } 
				WHERE { ?flight a adsb:FlightState. } 
				""");
		pss.setLiteral("REQUEST_TIME", requestTime);
		executeSparqlUpdate(pss);
		
	}

}
