package at.jku.dke.aisa.kg;

public interface KGModule {
	
	public String getName();
	
	public void init();
	
	public void register(KGModuleSystem kg);
	
	public String getModuleIri();

}
