package at.jku.dke.aisa.kg;

public abstract class AbstractPrologModule extends AbstractInternalModule  {

	public AbstractPrologModule(String name)  {
		super(name);
	}
	
	/* Prolog modules have their named graphs replicated directly before and after 
	 * being run so that metadata management can take place solely in the KG */
	protected void doRun2() {
		kg.copyFromKgToProlog(getTurnIri());
		
		doRun();
		kg.copyFromPrologToKg(getTurnIri());
	}
	
}
