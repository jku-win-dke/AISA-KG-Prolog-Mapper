import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdfconnection.RDFConnection;
import org.apache.jena.rdfconnection.RDFConnectionFactory;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;

/* splits a Trig-File (fileinput/adsb/input.trig) into Turtle files (fileinput/adsb/ttl/*.ttl) */
public class TrigToTTL {

	public static void main(String[] args) {
		RDFConnection con = RDFConnectionFactory.connect(
	    		   RDFDataMgr.loadDataset("fileinput/adsb/input.trig"));		
	
		Model defaultModel = con.fetch();
		defaultModel.getNsPrefixMap();
		
		try(OutputStream fileOut = Files.newOutputStream(Paths.get("fileinput/adsb/ttl/default.ttl"))) {
			RDFDataMgr.write(fileOut, defaultModel, Lang.TTL);
		} catch (IOException e) { e.printStackTrace(); }
		
		for(int i=0;i<60;i++) {
			Model ng = con.fetch("http://aisa-project.eu/graphs/g"+i);
			ng.setNsPrefixes(defaultModel.getNsPrefixMap());
			try(OutputStream ngOut = Files.newOutputStream(Paths.get("fileinput/adsb/ttl/g"+i+".ttl"))) {
				RDFDataMgr.write(ngOut, ng, Lang.TTL);
			} catch (IOException e) { e.printStackTrace(); }
		}

	}

}
