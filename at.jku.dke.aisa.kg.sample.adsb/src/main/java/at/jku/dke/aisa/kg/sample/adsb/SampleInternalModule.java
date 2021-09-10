package at.jku.dke.aisa.kg.sample.adsb;

import at.jku.dke.aisa.kg.AbstractInternalModule;

public class SampleInternalModule extends AbstractInternalModule {

	public SampleInternalModule(String name) {
		super(name);
	}

	@Override
	protected void doRun() {
		System.out.println("DO RUN: " + getName() + "-" + getTurn() + "-" + getLogicalTime());
	}

	@Override
	protected void doInit() {
		System.out.println("DO INIT: " + getName() + "-" + getTurn() + "-" + getLogicalTime());
	}

}
