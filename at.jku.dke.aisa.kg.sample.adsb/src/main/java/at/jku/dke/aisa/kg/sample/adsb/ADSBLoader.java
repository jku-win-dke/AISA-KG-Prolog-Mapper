package at.jku.dke.aisa.kg.sample.adsb;

import org.apache.jena.query.ParameterizedSparqlString;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.rdfconnection.RDFConnectionFactory;
import org.apache.jena.riot.RDFDataMgr;

import at.jku.dke.aisa.kg.*;

/**
 * The ADBSLoader imports flight positions from a dataset contained in file input.trig
 * File input.trig was generated from json-data retrieved from opensky-network.org
 *   every 10 seconds for a bounding box surrounding Austria on Feb 09 2021 from 19:14:00 to 19:24:50
 *   and transformed to RDF based on code developed by students in a practical course under the supervision 
 *   of Sebastian Gruber. The 59 named graphs are ordered by retrieval time with a distance of 10 seconds.   
 * The ADBSLoader imports one named graph per turn, hence, the real time distance between turns is 10 seconds.
 * 
 * To make tests more interesting (especially with ADSBProcessor2) I have deleted one flight and state 
 *  from graph:g0 and another one from graph:g1
 *  
 * 
 * The file input.trig is contained in the module's sub-folder /fileinput/adsb/
 *   with "adsb" being the module's name which is hard-coded in the constructor.
 * The module name being hard-coded in the constructor means that there is only
 *   a single instance of the ADSBLoader in one KG system. 
 *     
 * */
public final class ADSBLoader extends AbstractInternalModule implements InternalModule  {
	
	RDFConnection adsbCon;
	
	public ADSBLoader() {
		super("adsb");
	}
		
	public void doInit() {		
       adsbCon = RDFConnectionFactory.connect(
    		   RDFDataMgr.loadDataset(getInputPath("input.trig")));
	}

	public void doRun() {
		String inputGraphUri = GLOBAL.NS_GRAPHS + "g"+(getTurn()-1);
		
		System.out.println(getName() + " reads: " + inputGraphUri);

		ResultSet rs = adsbCon.query("SELECT ?t WHERE { <"+inputGraphUri+"> "
				+ " <" + GLOBAL.NS_ADSB + "requestTime> ?t. }").execSelect();
		
		long requestTime = rs.next().getLiteral("t").getLong();
		System.out.println(getName() + " - Request time: " + requestTime);
		
		Model m = adsbCon.fetch(inputGraphUri);
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
