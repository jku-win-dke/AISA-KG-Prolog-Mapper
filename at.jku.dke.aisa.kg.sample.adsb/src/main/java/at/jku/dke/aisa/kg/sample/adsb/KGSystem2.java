package at.jku.dke.aisa.kg.sample.adsb;

import at.jku.dke.aisa.kg.GLOBAL;
import at.jku.dke.aisa.kg.KGModuleSystem;

public class KGSystem2 {

	public KGSystem2() {}

	public static void main(String[] args) throws Exception {

		KGModuleSystem kg = 
				new KGModuleSystem(
//						GLOBAL.getNewDatasetConnection(),
						GLOBAL.getLocalFusekiConnection(),						
						GLOBAL.getPrefixMapping()
						);
		long time = 0;
		kg.setLogicalTime(time++);
		kg.cleanKG();
		kg.cleanOutputFolders();

		
		kg.setLogicalTime(time++);
		kg.register(new SampleStaticModule("ModuleA"));
		kg.register(new SampleStaticModule("ModuleB"));
		kg.register(new SampleExternalModule("ExternalA"));
		kg.register(new SampleExternalModule("ExternalB"));
		kg.register(new SampleInternalModule("InternalA"));
		kg.register(new SampleInternalModule("InternalB"));
		kg.register(new ADSBLoader()); //module name: adsb
		kg.register(new QueryADSB());  //module name: qadsb
		kg.register(new ADSBProcessor1());  //module name: adsbP1
		kg.register(new ADSBProcessor2());  //module name: adsbP2
		kg.register(new FlightPairs());  //module name: pairs
		kg.register(new PerformanceReport());  //module name: pairs
		
		kg.setLogicalTime(time++);
		kg.initAllModules();
			
		kg.startConsoleApplication(System.in, System.out);		
		
	}
	
}
