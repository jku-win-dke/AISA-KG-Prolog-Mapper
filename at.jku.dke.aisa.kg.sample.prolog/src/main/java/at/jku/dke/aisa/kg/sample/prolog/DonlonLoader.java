package at.jku.dke.aisa.kg.sample.prolog;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.riot.RDFDataMgr;

import at.jku.dke.aisa.kg.AbstractInternalModule;
import at.jku.dke.aisa.kg.InternalModule;

/* The donlon loader loads the same data every round */
public final class DonlonLoader extends AbstractInternalModule implements InternalModule  {
	
	public DonlonLoader() {
		super("donlon");
	}		

	public void doInit() {
		Model m = RDFDataMgr.loadModel(getInputPath("donlon-data.ttl"));
		loadGraphToKG(getModuleIri(),m);		
	}
	
	public void doRun() {
		Model m = RDFDataMgr.loadModel(getInputPath("donlon-data"+getTurn()+".ttl"));
		loadGraphToKG(getTurnIri(),m);		
	}

}
