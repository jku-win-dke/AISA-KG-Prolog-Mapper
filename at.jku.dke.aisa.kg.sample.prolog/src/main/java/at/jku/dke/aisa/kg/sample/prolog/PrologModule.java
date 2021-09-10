package at.jku.dke.aisa.kg.sample.prolog;


import org.jpl7.Query;

import at.jku.dke.aisa.kg.AbstractPrologModule;

public final class PrologModule extends AbstractPrologModule   {
	
	
	public PrologModule(String name) {
		super(name);
	}
	
	public void doInit() {
		System.out.println("DO INIT: " + getName() + "-" + getTurn() + "-" + getLogicalTime());
		
		new Query("consult('"+ getInputPath("program.pl") + "')").hasSolution();

	}

	public void doRun() {
		System.out.println("DO RUN: " + getName() + "-" + getTurn() + "-" + getLogicalTime());

		new Query("set_current_graph('"+ getTurnIri() +"')").hasSolution();
		new Query(getName()+":run").hasSolution();		
		
	}
}
