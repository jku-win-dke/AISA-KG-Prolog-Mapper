package at.jku.dke.aisa.kg;

/** Subclasses only implement doStartTurn(), export to the file system, and finishTurn(), i.e. import from the file system 
 * 
 * for external modules a turn consists of two transactions: 1. export/checkOut, 2. import/checkIn
 * */
public abstract class AbstractExternalModule extends AbstractMultipleRunModule implements ExternalModule {

	public AbstractExternalModule(String name) {
		super(name);
	}

	/**  */
	public final void exportInput() {
		resetTimestamp();
		initTurn();
		doExportInput();		
	}

	abstract protected void doExportInput();
	
	public final void importResults() {
		resetTimestamp();
		doImportResults();	
		commitTurn();
		
		kg.copyFromKgToFileAndProlog(getTurnIri(),getTurnName());
//		kg.copyFromKgToFileAndProlog(getTurnIri(),getOutputPath(getTurnName()+".ttl"));
	}
	
	abstract protected void doImportResults();	

}
