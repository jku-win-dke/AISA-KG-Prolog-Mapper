package at.jku.dke.aisa.kg.sample.prolog;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.riot.RDFDataMgr;

import at.jku.dke.aisa.kg.*;

/* The donlon loader loads the same data every round */
public final class FixmLoader extends AbstractInternalModule implements InternalModule  {
	
	public FixmLoader() {
		super("fixm");
	}		

	public void doRun() {
		Model m = RDFDataMgr.loadModel(getInputPath("FIXM_EDDF-VHHH.ttl"));
		loadGraphToKG(getTurnIri(),m);		
	}

}
