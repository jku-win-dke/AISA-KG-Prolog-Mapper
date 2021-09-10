package at.jku.dke.aisa.kg.sample.adsb;

import at.jku.dke.aisa.kg.AbstractExternalModule;

public class SampleExternalModule extends AbstractExternalModule {

	public SampleExternalModule(String name) {
		super(name);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected
	void doInit() {
		System.out.println("DO INIT: " + getName() + "-" + getTurn() + "-" + getLogicalTime());
	}

	
	@Override
	protected
	void doExportInput() {
		System.out.println("DO START: " + getName() + "-" + getTurn() + "-" + getLogicalTime());
	}

	@Override
	protected
	void doImportResults() {
		System.out.println("DO FINISH: " + getName() + "-" + getTurn() + "-" + getLogicalTime());

	}


}
