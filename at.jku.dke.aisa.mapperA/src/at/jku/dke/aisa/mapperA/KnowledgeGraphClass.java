package at.jku.dke.aisa.mapperA;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang3.StringUtils;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.shacl.engine.Target;
import org.apache.jena.shacl.parser.PropertyShape;
import org.apache.jena.shacl.parser.Shape;
import org.apache.jena.shared.PrefixMapping;
import org.apache.jena.util.iterator.ExtendedIterator;

/**
 * Depicts a NodeShape of the shacl graph.
 * For each PropertyShape of the given shape, a KnowledgeGraphProperty is created which is linked to this KnowledgeGraphClass.
 * Creates the respective SPARQL query at creation time and the Prolog schema comment.
 */
public class KnowledgeGraphClass {

	private Graph shaclGraph;
	public Shape rootShape;
	private boolean isSuperClass;
	private KnowledgeGraphClass isSubClassOf;
	private String graphName;
	private List<KnowledgeGraphProperty> knowledgeGraphProperties = new ArrayList<>();
	
	private String sparqlQuery;
	
	// created by generateNamesTargetsPrefixes()
	public String predicateName;
	private String nameOfTargets;
	private String nameOfTargetsWithShortPrefix;
	
	/**
	 * Creates the KnowledgeGraphClass and respective helper variables which are required for later use.
	 * 
	 * @param shaclGraph
	 * @param rootShape
	 * @param isSuperClass
	 * @param graphName
	 */
	public KnowledgeGraphClass(Graph shaclGraph, Shape rootShape, boolean isSuperClass, String graphName) {
		this.shaclGraph = shaclGraph;
		this.rootShape = rootShape;
		this.isSuperClass = isSuperClass;
		this.graphName = graphName;
		
		this.predicateName = getNameOfTargets().substring(0,1).toLowerCase() + getNameOfTargets().substring(1);
		this.nameOfTargets = getNameOfTargets();
		this.nameOfTargetsWithShortPrefix = getNameOfTargetsWithPrefixShort();
		
		createProperties();
		generateSPARQLQuery();
	}
	
	/**
	 * Creates for each property of the shacl shapes a KnowledgeGraphProperty which is linked to this KnowledgeGraphClass.
	 */
	private void createProperties() {
		List<PropertyShape> properties = orderPropertyShapes(rootShape.getPropertyShapes());
		for(PropertyShape property : properties) {
			KnowledgeGraphProperty knowledgeGraphProperty = new KnowledgeGraphProperty(this, property);
			knowledgeGraphProperties.add(knowledgeGraphProperty);
		}
	}
	
	/**
	 * Generates the SPARQL query of this KnowledgeGraphClass.
	 */
	private void generateSPARQLQuery() {
		sparqlQuery = generatePrefixes();
		sparqlQuery += generateSelect();
		sparqlQuery += generateWhere();
	}

	/**
	 * Returns the SPARQL query of this KnowledgeGraphClass.
	 * 
	 * @return
	 */
	public String getSPARQLQuery() {
		return this.sparqlQuery;
	}
	
	/**
	 * For each query the prefixes are generated.
	 * 
	 * e.g.: PREFIX  xsd:  <http://www.w3.org/2001/XMLSchema#>
	 * 
	 */
	private String generatePrefixes() {
		String prefixes = "";
		PrefixMapping prefixMapping = shaclGraph.getPrefixMapping();
		Map<String, String> prefix = prefixMapping.getNsPrefixMap();
		for(Entry<String, String> entry : prefix.entrySet()) {
			prefixes += "PREFIX " + entry.getKey() + ": <" + entry.getValue() + ">" + '\n';
		}
		prefixes += '\n';
		return prefixes;
	}
	
	/**
	 * Generates the select part of the SPARQL query.
	 * 
	 * @return
	 */
	private String generateSelect() {
		String select = "SELECT ?graph ?" + predicateName;
		for(KnowledgeGraphProperty property : knowledgeGraphProperties) {
			select += " " + property.getSelectFragment();
		}
		select += '\n';
		return select;
	}
	
	/**
	 * Generates the where part of the SPARQL query.
	 * 
	 * @return
	 */
	private String generateWhere() {
		String where = "WHERE" + '\n';
		if(isSuperClass) {
			where += " {" + '\n';
			where += "  GRAPH <" + graphName + "> {" +'\n'
					+ "    ?SUBCLASS rdfs:subClassOf* " + nameOfTargetsWithShortPrefix + " ." + '\n'
					+ "  }" + '\n';
		}
		
		where += "  { GRAPH ?graph" + '\n';
		where += "    {" + '\n';
		
		if(isSuperClass) {
			where += "      ?" + predicateName + " " + getShapeType() + " ?SUBCLASS ." + '\n';
		} else {
			where += "      ?" + predicateName + " " + getShapeType() + " " + nameOfTargetsWithShortPrefix + " ." + '\n';
		}
		boolean groupByNeeded = false;
		String groupBy = "";
		for(KnowledgeGraphProperty knowledgeGraphPropery : knowledgeGraphProperties) {
			where += knowledgeGraphPropery.getWhereFragment();
			if(knowledgeGraphPropery.maxCount == 0) {
				groupByNeeded = true;
			}
			if(knowledgeGraphPropery.maxCount == 1) {
				groupBy += " ?" + knowledgeGraphPropery.getName();
			}
		}
		
		where += "    }" + '\n';
		where += "  }" + '\n';
		
		
		if(isSuperClass) {
			where += "}" + '\n';
		}
		
		if(groupByNeeded) {
			where += "GROUP BY ?graph ?" + predicateName + groupBy + '\n';
		}

		return where;
	}

	/**
	 * Generates the Prolog schema comment of this KnowledgeGraphClass.
	 * 
	 * e.g.: % city(Graph, City, Name?, Annotation*)
	 * 
	 * @return
	 */
	public String generateComment() {
		String comment = "% " + predicateName + "(Graph, " + StringUtils.capitalize(predicateName);
		for(KnowledgeGraphProperty knowledgeGraphProperty : knowledgeGraphProperties) {
			comment += ", " + StringUtils.capitalize(knowledgeGraphProperty.getName() + knowledgeGraphProperty.getCardinality());
		}
		comment += ")";
		return comment;
	}
	
	/**
	 * Returns the shape type of this KnowledgeGraphClass.
	 * 
	 * e.g.: rdf:type
	 * 
	 * @return
	 */
	private String getShapeType() {
		String result = "";
		ExtendedIterator<Triple> tripleIterator = rootShape.getShapeGraph().find(rootShape.getShapeNode(), null, ShaclUtil.shaclNodeShapeAsNode());
		while(tripleIterator.hasNext()) {
			result += tripleIterator.next().getPredicate().toString(rootShape.getShapeGraph().getPrefixMapping()); //
		}
		return result;
	}
	
	/**
	 * The PropertyShapes of a RootShape will be returned in the right order according to sh:order in shacl.
	 * 
	 * Returns a list of PropertyShapes with the right order.
	 * 
	 * @param unorderedList
	 * @return
	 */
	private List<PropertyShape> orderPropertyShapes(Collection <PropertyShape> unorderedList) {
		List<PropertyShape> orderedList = new ArrayList<>();
		unorderedList.forEach(p ->
			orderedList.add(p)
		);
		
		Collections.sort(orderedList, new Comparator<PropertyShape>() {

			@Override
			public int compare(PropertyShape o1, PropertyShape o2) {
				
				int o1Order = Integer.MAX_VALUE;
				int o2Order = Integer.MAX_VALUE;
				
				ExtendedIterator<Triple> o1Iterator = o1.getShapeGraph().find(o1.getShapeNode(), ShaclUtil.shaclOrderAsNode(), null);
				while(o1Iterator.hasNext()) {
					o1Order = Integer.parseInt(o1Iterator.next().getObject().getLiteralValue().toString());
				}
				
				ExtendedIterator<Triple> o2Iterator = o2.getShapeGraph().find(o2.getShapeNode(), ShaclUtil.shaclOrderAsNode(), null);
				while(o2Iterator.hasNext()) {
					o2Order = Integer.parseInt(o2Iterator.next().getObject().getLiteralValue().toString());
				}
				 
				if(o1Order > o2Order) {
					return 1;
				} else if(o1Order < o2Order) {
					return -1;
				} else {
					return 0;
				}
			}
			
		});

		return orderedList;
	}
	
	/**
	 * Return only name from a given target.
	 * 
	 * e.g.: input target: targetClass[<http://example.org/person>]
	 * returns: person
	 * 
	 * @param shape
	 * @return
	 */
	private String getNameOfTargets() {
		String nameOfTargets = "";
		for(Target target : rootShape.getTargets()) {
			nameOfTargets += target.getObject().getLocalName();
		}
		return nameOfTargets;
	}
	
	/**
	 * Returns the name of the node including the prefix mapping.
	 * 
	 * e.g.: x:person
	 * 
	 * @param node
	 * @param rootShape
	 * @return
	 */
	private String getPrefixMapping(Node node, Shape rootShape) {
		return node.toString(rootShape.getShapeGraph().getPrefixMapping());
	}
	
	/**
	 * Returns the name of the target with short prefix of this KnowledgeGraphClass.
	 * 
	 * e.g.: aixm:Point
	 * 
	 * @return
	 */
	private String getNameOfTargetsWithPrefixShort() {
		String target = "";
		Collection<Target> targetCollection = rootShape.getTargets();
		for(Target t : targetCollection) {
			 target += getPrefixMapping(t.getObject(), rootShape);
		}
		return target;
	}

	/**
	 * Returns the knowledgeGraphProperties of this KnowledgeGraphClass.
	 * 
	 * @return
	 */
	public List<KnowledgeGraphProperty> getKnowledgeGraphProperties() {
		return this.knowledgeGraphProperties;
	}
}
