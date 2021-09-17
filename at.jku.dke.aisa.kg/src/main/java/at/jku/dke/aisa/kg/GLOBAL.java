package at.jku.dke.aisa.kg;

import org.apache.jena.query.DatasetFactory;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.rdfconnection.RDFConnectionFactory;
import org.apache.jena.rdfconnection.RDFConnectionFuseki;
import org.apache.jena.rdfconnection.RDFConnectionRemoteBuilder;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.shared.PrefixMapping;

public final class GLOBAL {

	public final static String DATASET_URL = "http://localhost:3030/aisakg";
	public final static String UPDATE_ENDPOINT = "/update";
	public final static String QUERY_ENDPOINT = "/sparql";
	public final static String GRAPHSTOREPROTOCOL_ENDPOINT = "/data";
	
	public final static String NS_XS =    "http://www.w3.org/2001/XMLSchema#";
	public final static String NS_RDF =   "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
	public final static String NS_RDFS =  "http://www.w3.org/2000/01/rdf-schema#";
	public final static String NS_GRAPHS ="http://aisa-project.eu/graphs/";
	public final static String NS_AISA =  "http://aisa-project.eu/vocab#";
	public final static String NS_AISAR = "http://aisa-project.eu/resources#";
	public final static String NS_ADSB =  "http://aisa-project.eu/adsb#";
	
	public final static String SCHEMA_GRAPH_NAME = "https://github.com/jku-win-dke/aisa/graphs/schema";
	public final static String DATA_GRAPH_NAMESPACE = "https://github.com/jku-win-dke/aisa/graphs";
	
	public static final String INIT_PROLOG_PROGRAM = "resources/global.pl";
	public static final String FILEOUTPUT_PATH = "fileoutput";
	public static final String FILEINPUT_PATH = "fileinput";
	public static final String TEMPFILE_PATH_REPLICATE = FILEOUTPUT_PATH + "/tempreplicate.rdf";
	public static final String NAMED_GRAPH_FOLDER = FILEOUTPUT_PATH + "/_NG_";

	
	private final static String PREFIXES_FILE = "resources/prefixes.ttl";

	/* load prefix mappings from PREFIXES_FILE and override it with hard-coded Namespace Prefixes */
	public static PrefixMapping getPrefixMapping() {
		PrefixMapping prefixes = RDFDataMgr.loadGraph(PREFIXES_FILE).getPrefixMapping();

		prefixes.setNsPrefix("xs", NS_XS);
		prefixes.setNsPrefix("rdf", NS_RDF);
		prefixes.setNsPrefix("rdfs", NS_RDFS);
		prefixes.setNsPrefix("aisa", NS_AISA);
		prefixes.setNsPrefix("graphs", NS_GRAPHS);
		prefixes.setNsPrefix("adsb", NS_ADSB);
		prefixes.setNsPrefix("aisar", NS_AISAR);
		return prefixes;
	}
	
    public static RDFConnectionFuseki getLocalFusekiConnection() {
		RDFConnectionRemoteBuilder builder = RDFConnectionFuseki.create()
	            .destination(GLOBAL.DATASET_URL)
	            .queryEndpoint(GLOBAL.QUERY_ENDPOINT)
	            .gspEndpoint(GLOBAL.GRAPHSTOREPROTOCOL_ENDPOINT)
	            .updateEndpoint(GLOBAL.UPDATE_ENDPOINT);
	    RDFConnectionFuseki con = (RDFConnectionFuseki)builder.build();
	    return con;
    }
    
    public static RDFConnection getNewDatasetConnection() {
    	return RDFConnectionFactory.connect(DatasetFactory.create());
    }
}