package at.jku.dke.aisa.kg.sample.adsb;

import at.jku.dke.aisa.kg.AbstractSingleRunModule;
import at.jku.dke.aisa.kg.SingleRunModule;

public class SampleStaticModule extends AbstractSingleRunModule implements SingleRunModule {

	public SampleStaticModule(String name) {
		super(name);
	}

	@Override
	protected
	void doInit() {
		System.out.println("DO INIT: " + getName() + "-" + getLogicalTime());		
	}

	
	
	
}
