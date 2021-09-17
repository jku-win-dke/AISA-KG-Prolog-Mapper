package at.jku.dke.aisa.kg;

import org.jpl7.Query;

public class PrologModule extends AbstractInternalModule  {

	public PrologModule(String name)  {
		super(name);
	}
	
	/* Prolog modules have their named graphs replicated directly before and after 
	 * being run so that metadata management can take place solely in the KG.
	 * The method is final to avoid this crucial replication being overwritten */
	protected final void doRun2() {
		kg.copyFromKgToProlog(getTurnIri());		
		doRun();
		kg.copyFromPrologToKg(getTurnIri());
	}

	
	/* By default, a Prolog module is implemented by a Prolog program (fileinput/<modulename>/program.pl) 
	 * which gets consulted by the Prolog engine during initialization.
	 * It is assumed that Prolog program defines itself to use the module's name */
	public void doInit() {
		System.out.println("DO INIT: " + getName() + "-" + getLogicalTime());
		
		new Query("consult('"+ getInputPath("program.pl") + "')").hasSolution();
	}

	
	/* A standard Prolog module is implemented by a Prolog program (program.pl) 
	 * which gets consulted by the Prolog engine during initialization. */
	public void doRun() {
		System.out.println("DO RUN: " + getName() + "-" + getTurn() + "-" + getLogicalTime());

		new Query("set_current_graph('"+ getTurnIri() +"')").hasSolution();
		new Query(getName()+":run").hasSolution();		
		
	}
	
}
