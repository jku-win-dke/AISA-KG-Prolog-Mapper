package at.jku.dke.aisa.kg.sample.adsb;


import org.jpl7.Query;

import at.jku.dke.aisa.kg.AbstractPrologModule;

/**
 *     
 * */
public final class PrologModule extends AbstractPrologModule   {
	
	
	public PrologModule() {
		super("prolog");
	}
	
	public void doInit() {
		System.out.println("DO INIT: " + getName() + "-" + getTurn() + "-" + getLogicalTime());
	}

	public void doRun() {
		System.out.println("DO RUN: " + getName() + "-" + getTurn() + "-" + getLogicalTime());

		new Query("run('"+ getTurnIri() +"',"+ getTurn() +")").hasSolution();
		
	}
}
