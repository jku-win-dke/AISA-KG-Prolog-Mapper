package at.jku.dke.aisa.mapperA;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.shacl.Shapes;
import org.apache.jena.shacl.parser.Shape;
import org.apache.jena.shared.PrefixMapping;
import org.apache.jena.util.iterator.ExtendedIterator;

/**
 * The main task of the Mapper is to create SPARQL queries and Prolog facts.
 * 
 * Takes shacl graph, parsed shacl shapes, prefixes and the graph name as input.
 * For each target shape as input a KnowledgeGraphClass is created and for each property of the shape a KnowledgeGraphProperty.
 * KnowledgeGraphClass and KnowledgeGraphProperty are used for easier creation of SPARQL queries and Prolog facts.
 */
public class Mapper {

	private Graph shaclGraph;
	private Shapes shapes;
	private PrefixMapping prefixes;
	private Map<String, String> orderedPrefixes;
	private String graphName;
	private List<KnowledgeGraphClass> knowledgeGraphClasses = new ArrayList<>();
	private List<Node> superClasses = new ArrayList<>();
	
	/**
	 * Creates the mapper and all required variables for later use.
	 * @param shaclGraph
	 * @param shapes
	 * @param prefixesFile
	 * @param graphName
	 */
	public Mapper(Graph shaclGraph, Shapes shapes, String prefixesFile, String graphName) {
		this.shaclGraph = shaclGraph;
		this.shapes = shapes;
		this.prefixes = RDFDataMgr.loadGraph(prefixesFile).getPrefixMapping();
		this.orderedPrefixes = orderPrefixes(prefixes.getNsPrefixMap());
		this.graphName = graphName;
		findSuperClasses();
		generateClassesAndProperties();
	}

	/**
	 * Takes all target shapes of the shacl graph as input and generates the respective knowledge graph classes and properties.
	 * This method is called at creation time of the mapper.
	 */
	private void generateClassesAndProperties() {
		for(Shape rootShape : shapes.getTargetShapes()) {
			boolean isSuperClass = superClasses.contains(rootShape.getShapeNode());
			KnowledgeGraphClass knowledgeGraphClass = new KnowledgeGraphClass(shaclGraph, rootShape, isSuperClass, graphName);
			knowledgeGraphClasses.add(knowledgeGraphClass);
		}
	}
	
	/**
	 * Prints the sparql queries with the given PrintWriter.
	 * @param printWriter
	 */
	public void generateSPARQLQueries(PrintWriter printWriter) {
		for(KnowledgeGraphClass knowledgeGraphClass : knowledgeGraphClasses) {
			printWriter.print(generatePrefixes());
			printWriter.println(knowledgeGraphClass.getSPARQLQuery() + '\n');
		}
	}

	/**
	 * For each query the prefixes are generated.
	 * 
	 * e.g.: PREFIX  xsd:  <http://www.w3.org/2001/XMLSchema#>
	 * 
	 */
	private String generatePrefixes() {
		String prefixes = "";
		for(Entry<String, String> entry : orderedPrefixes.entrySet()) {
			prefixes += "PREFIX " + entry.getKey() + ": <" + entry.getValue() + ">" + '\n';
		}
		prefixes += '\n';
		return prefixes;
	}
	
	/**
	 * Takes the querySolution as input and creates Prolog facts out of it.
	 * For generation of the Prolog facts, the dedicated shacl schema is required to use information like minCount/maxCount, subClasses, etc. 
	 * 
	 * @param querySolution
	 * @param schemaForGeneratingFacts
	 * @return
	 */
	public String generateFact(QuerySolution querySolution, KnowledgeGraphClass schemaForGeneratingFacts) {
		System.out.println(querySolution);

		String fact = schemaForGeneratingFacts.predicateName + "(";
		fact += resolvePrefixMapping(querySolution.get("?graph")) != null ? resolvePrefixMapping(querySolution.get("?graph")) : ("\'" + querySolution.get("?graph") + "\'");
		fact += resolvePrefixMapping(querySolution.get("?" + schemaForGeneratingFacts.predicateName)) != null ? (", " +resolvePrefixMapping(querySolution.get("?" + schemaForGeneratingFacts.predicateName))) : (", \'" + querySolution.get("?" + schemaForGeneratingFacts.predicateName) + "\'");
		for(KnowledgeGraphProperty property : schemaForGeneratingFacts.getKnowledgeGraphProperties()) {
			if(property.maxCount == 0) {
				RDFNode node = querySolution.get("?" + property.getName() + "Concat");
				if(node != null) {
					fact += ", [";
					String[] listItems = node.toString().split(",");
					for(int i = 0; i<listItems.length; i++) {
						String nodeWithPrefixMapping = node != null ? resolvePrefixMapping(listItems[i]) : null;
						if(nodeWithPrefixMapping != null) {
							fact += nodeWithPrefixMapping;
						} else {
							fact += "\'" + listItems[i] + "\'";
						}
						if(i != listItems.length-1) {
							fact += ", ";
						}
					}
					fact += "]";
				} else {
					fact += ", []";
				}
			} else {
				RDFNode node = querySolution.get("?" + property.getName());
				String nodeWithPrefixMapping = node != null ? resolvePrefixMapping(node) : null;
				if(node != null) {
					if(node.isLiteral()) {
						if(node.toString().startsWith("nil:")) {
							fact += ", nil(\'" + node.toString().substring(new String("nil:").length()) + "\')";							
						} else if(node.toString().startsWith("val:")) {
							fact += ", val(\'" + node.toString().substring(new String("val:").length()) + "\')";
						} else if(node.toString().startsWith("xval:")) {
							String[] xval = node.toString().split(":");
							fact += ", "+ xval[0] + "(\'" + xval[1] + "\',\'" + xval[2] + "\')";
						}
					} else {
						if(nodeWithPrefixMapping != null) {
							fact += ", " + nodeWithPrefixMapping;
						} else {
							fact += ", \'" + node + "\'";
						}
					}
				} else {
					fact += ", \'$null$\'";
				}
			}
		}
		fact += ").";
		return fact;
	}
	
	/**
	 * Replaces the URI of the prefix mapping from a list item with the key.
	 * 
	 * e.g.: s1:'AHY_EADH_PERMIT'
	 * 
	 * @param listItem
	 * @return
	 */
	private String resolvePrefixMapping(String listItem) {
		for(Entry<String, String> prefixMapping : orderedPrefixes.entrySet()) {
			if(listItem.startsWith(prefixMapping.getValue())) {
				return prefixMapping.getKey() + ":\'" + listItem.substring(prefixMapping.getValue().length()) + "\'";
			}
		}
		return null;
	}

	/**
	 * Replaces the URI of the prefix mapping from a RDFNode (graph) with the key.
	 * 
	 * e.g.: g1:'EA_AIP_DS_FULL_20170701.xml'
	 * 
	 * @param node
	 * @return
	 */
	private String resolvePrefixMapping(RDFNode node) {
		for(Entry<String, String> prefixMapping : orderedPrefixes.entrySet()) {
			if(node.toString().startsWith(prefixMapping.getValue())) {
				return prefixMapping.getKey() + ":\'" + node.toString().substring(prefixMapping.getValue().length()) + "\'";
			}
		}
		return null;
	}

	/**
	 * Orders the given prefixes from the prefix file by the length of the URI in descending order.
	 * @param nsPrefixMap
	 * @return
	 */
	private Map<String, String> orderPrefixes(Map<String, String> nsPrefixMap) {
		Map<String, String> sortedMap = nsPrefixMap.entrySet().stream()
		        .sorted((a, b) -> Integer.compare(b.getValue().length(), a.getValue().length()))
		     .collect(Collectors.toMap(Map.Entry::getKey,
		                Map.Entry::getValue, (oldValue, newValue) -> oldValue, LinkedHashMap::new));
        return sortedMap;
	}

	/**
	 * Find all shacl shapes, which have sub classes
	 */
	private void findSuperClasses() {
		ExtendedIterator<Triple> superClassesTriples = shaclGraph.find(null, ShaclUtil.rdfsSubClassOfAsNode(), null);
		while(superClassesTriples.hasNext()) {
			Node superClass = superClassesTriples.next().getObject();
			if(!superClasses.contains(superClass)) {
				superClasses.add(superClass);
			}
		}
	}
	
	/**
	 * Returns all knowledgeGraphClasses.
	 * @return
	 */
	public List<KnowledgeGraphClass> getKnowledgeGraphClasses() {
		return this.knowledgeGraphClasses;
	}

	/**
	 * Prints rdf_register_prefix for all required prefixes defined in the prefix file.
	 * 
	 * e.g.: :- rdf_register_prefix(sh,'http://www.w3.org/ns/shacl#').
	 * 
	 * @param printWriter
	 */
	public void printPrefixRegistration(PrintWriter printWriter) {
		for(Entry<String, String> entry : orderedPrefixes.entrySet()) {
			printWriter.println(":- rdf_register_prefix(" + entry.getKey() + ",\'" + entry.getValue() + "\').");
		}
		printWriter.println();
	}

	/**
	 * Creates rdf_meta rule for each KnowledgeGraphClass.
	 * 
	 * e.g.: :- rdf_meta
	 *	  % city(Graph, City, Name?, Annotation*)
	 *	  city(r,r,t,t)
	 * 
	 * @param printWriter
	 */
	public void printRDFMeta(PrintWriter printWriter) {
		printWriter.println("/* for prefix handling: declare predicates that have RDF terms as arguments");
		printWriter.println("see: https://www.swi-prolog.org/pldoc/man?predicate=rdf_meta/1");
		printWriter.println("*/");
		printWriter.print(":- rdf_meta"); 
		for(int i = 0; i < knowledgeGraphClasses.size(); i++) {
			printWriter.println();
			printWriter.println("  " + knowledgeGraphClasses.get(i).generateComment());
			String properties = "";
			for(int j = 0; j<knowledgeGraphClasses.get(i).getKnowledgeGraphProperties().size(); j++) {
				properties += ",t";
			}
			if(i == 0 ) {
				printWriter.println("  " + knowledgeGraphClasses.get(i).predicateName + "(r,r" + properties + ")");				
			} else {
				printWriter.println("  ," + knowledgeGraphClasses.get(i).predicateName + "(r,r" + properties + ")");
			}
		}
		printWriter.println(".");
		printWriter.println();
	}
}
