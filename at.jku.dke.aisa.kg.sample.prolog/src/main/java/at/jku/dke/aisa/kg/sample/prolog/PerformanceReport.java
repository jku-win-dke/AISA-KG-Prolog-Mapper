package at.jku.dke.aisa.kg.sample.prolog;

import java.io.IOException;
import java.io.PrintWriter;

import at.jku.dke.aisa.kg.AbstractInternalModule;
import at.jku.dke.aisa.kg.InternalModule;

public class PerformanceReport extends AbstractInternalModule implements InternalModule  {
	
	public PerformanceReport() {
		super("report");
	}
	
	public void doRun() {
		
		try (PrintWriter out = getPrintWriter("report_$TURN$.csv")) {
			out.println("module,minTime,maxTime,avgTime,minCount,maxCount,avgCount");	
			executeSparqlSelect(
				preprocessSparql("""
					SELECT ?module ?turnNo ?duration (count(*) AS ?noOfTriples)
					WHERE {
						GRAPH ?g { ?g aisa:durationInMs ?duration; aisa:turnNo ?turnNo; aisa:module ?module. ?s ?p ?o.}
					}
					GROUP BY ?module ?turnNo ?duration
					ORDER BY ?module ?turnNo
					"""),
				qs -> System.out.println("" 
						+ qs.getResource("module").getLocalName() + ", "
				   		+ qs.getLiteral("turnNo").getInt() + ", "
				   		+ qs.getLiteral("duration").getInt() + " ms, "
				   		+ qs.getLiteral("noOfTriples").getInt() + " triples.")
			);
	    } catch (IOException x) {
	  	      System.err.println(x);
	  	}
		
		try (PrintWriter out = getPrintWriter("aggregated_report_$TURN$.csv")) {

			out.println("module,minTime,maxTime,avgTime,minCount,maxCount,avgCount");	
			
			executeSparqlSelect(
				preprocessSparql("""
					SELECT ?module 
						(MIN(?duration) AS ?minTime) (MAX(?duration) AS ?maxTime) (AVG(?duration) AS ?avgTime) 
						(MIN(?noOfTriples) AS ?minCount) (MAX(?noOfTriples) AS ?maxCount) (AVG(?noOfTriples) AS ?avgCount)
					WHERE {
						SELECT ?module ?turnNo ?duration (count(*) AS ?noOfTriples)
						WHERE {
							GRAPH ?g { 
								?g aisa:durationInMs ?duration; 
								aisa:turnNo ?turnNo; 
								aisa:module ?module. 
								?s ?p ?o.
							}
						}
						GROUP BY ?module ?turnNo ?duration
					}
					GROUP BY ?module
					"""),
				qs -> { 
					String module = qs.getResource("module").getLocalName();
					int minTime = qs.getLiteral("minTime").getInt();
					int maxTime = qs.getLiteral("maxTime").getInt();
					int avgTime = qs.getLiteral("avgTime").getInt();
					int minCount = qs.getLiteral("minCount").getInt();
					int maxCount = qs.getLiteral("maxCount").getInt();
					int avgCount = qs.getLiteral("avgCount").getInt();
					
					System.out.println(module + ", " + minTime + " ms (min), " + maxTime + " ms (max), "
				   		+ avgTime + " ms (avg), "+ minCount + " triples (min), "+ maxCount + " triples (max), "
				   		+ avgCount + " triples (avg)."); 
					out.println(module+","+minTime+","+maxTime+","+avgTime+","+minCount+","+maxCount+","+avgCount);	
				}
			);
		
	    } catch (IOException x) {
  	      System.err.println(x);
  	    }
		
	}	
	
}
