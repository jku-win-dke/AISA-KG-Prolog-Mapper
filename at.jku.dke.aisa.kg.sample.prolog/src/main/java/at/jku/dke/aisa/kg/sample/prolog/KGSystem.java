package at.jku.dke.aisa.kg.sample.prolog;

import at.jku.dke.aisa.kg.GLOBAL;
import at.jku.dke.aisa.kg.KGModuleSystem;
import at.jku.dke.aisa.kg.PerformanceReport;
import at.jku.dke.aisa.kg.SchemaLoader;
import at.jku.dke.aisa.kg.PrologModule;

public class KGSystem {

	public KGSystem() {}

	public static void main(String[] args) throws Exception {

		KGModuleSystem kg = new KGModuleSystem(
						GLOBAL.getLocalFusekiConnection(),						
						GLOBAL.getPrefixMapping() );
		
		long time = 0;
		
		kg.setLogicalTime(time++);
		
		/* delete all triples and quadruples from the KG */
		kg.cleanKG();
		
		/* delete output files from previous runs */
		kg.cleanOutputFolders();
				
		/* create KG modules (in Java) */
		SchemaLoader schema = new SchemaLoader();
		PrologModule prolog1 = new PrologModule("prolog1");
		PrologModule prolog2 = new PrologModule("prolog2");
		DonlonLoader donlon = new DonlonLoader();
		FixmLoader fixm = new FixmLoader();
		PerformanceReport report = new PerformanceReport();
				
		kg.setLogicalTime(time++);

		/* register the modules with the KG system (still only in Java) */		
		kg.register(schema); 
		kg.register(fixm);
		kg.register(donlon); 
		kg.register(prolog1); 
		kg.register(prolog2); 
		kg.register(report); 
		
		kg.setLogicalTime(time++);

		/* initialize all modules registerd with the KG system
		 * - this calls the init() method of every module, which creates a named graph representing the module in the KG
		 * - for Prolog modules, the init-method let the already running Prolog instance
		 *     consult the respective program.pl (which has to specify the module name as part of program.pl)
		 *  
		 *  */		
		kg.initAllModules();
			
		for(int i=0; i<5; i++) {
			kg.setLogicalTime(time++);
			donlon.run();
			kg.setLogicalTime(time++);
			fixm.run();
			kg.setLogicalTime(time++);
			prolog1.run();
			kg.setLogicalTime(time++);
			prolog2.run();
		}
		
		kg.setLogicalTime(time++);
		report.run();

		
	}
	
}
