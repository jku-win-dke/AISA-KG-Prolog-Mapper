package at.jku.dke.aisa.mapperA;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.shacl.engine.Target;
import org.apache.jena.shacl.parser.PropertyShape;
import org.apache.jena.shacl.parser.Shape;
import org.apache.jena.util.iterator.ExtendedIterator;

/**
 * Depicts a NodeShape of the shacl graph.
 * For each PropertyShape of the given shape, a KnowledgeGraphProperty is created which is linked to this KnowledgeGraphClass.
 * Creates the respective SPARQL query at creation time and the Prolog schema comment.
 */
public class KnowledgeGraphClass {

	private Graph shaclGraph;
	public Shape rootShape;
	public boolean isSuperClass;
	public boolean isSubClass;
	private String graphName;
	private List<KnowledgeGraphProperty> knowledgeGraphProperties = new ArrayList<>();
	
	private String sparqlQuery;
	
	// created by generateNamesTargetsPrefixes()
	public String predicateName;
	private String nameOfTargets;
	private String nameOfTargetsWithShortPrefix;
	
	Map<String, String> orderedPrefixes;

	/**
	 * Creates the KnowledgeGraphClass and respective helper variables which are required for later use.
	 * 
	 * @param shaclGraph
	 * @param rootShape
	 * @param isSuperClass
	 * @param graphName
	 * @param orderedPrefixes 
	 * @param subClassOfSuperClass 
	 */
	public KnowledgeGraphClass(Graph shaclGraph, Shape rootShape, boolean isSuperClass, String graphName, Map<String, String> orderedPrefixes) {
		this.shaclGraph = shaclGraph;
		this.rootShape = rootShape;
		this.isSuperClass = isSuperClass;
		this.graphName = graphName;
		
		this.predicateName = getNameOfTargets().substring(0,1).toLowerCase() + getNameOfTargets().substring(1);
		this.nameOfTargets = getNameOfTargets();
		this.nameOfTargetsWithShortPrefix = getNameOfTargetsWithPrefixShort();
		
		this.orderedPrefixes = orderedPrefixes;
		
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
		sparqlQuery = generateSelect() //
					+ generateWhere();
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
			if(knowledgeGraphPropery.maxCount > 1 || knowledgeGraphPropery.maxCount == -1) {
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
		String comment = "% " + getNameOfTargetsWithPrefixShortAndUnderScore() + "(Graph, " + StringUtils.capitalize(predicateName);
		for(KnowledgeGraphProperty knowledgeGraphProperty : knowledgeGraphProperties) {
			comment += ", " + StringUtils.capitalize(knowledgeGraphProperty.getName() + knowledgeGraphProperty.getCardinality());
		}
		comment += ")";
		return comment;
	}
	
	/**
	 * Generates the Prolog rule of this KnowledgeGraphClass.
	 * 
	 * e.g.: city(Graph, City, Name, Annotation)
	 * 
	 * @return
	 */
	public String generatePrologRule(String joinedName) {
		String rule = getNameOfTargetsWithPrefixShortAndUnderScore() + "(Graph, " + (joinedName == null ? StringUtils.capitalize(predicateName) : joinedName);
		for(KnowledgeGraphProperty knowledgeGraphProperty : knowledgeGraphProperties) {
			if(knowledgeGraphProperty.maxCount > 1 || knowledgeGraphProperty.maxCount == -1) {
				rule += ", " + StringUtils.capitalize(knowledgeGraphProperty.getName()) + "List";
			} else {
				rule += ", " + StringUtils.capitalize(knowledgeGraphProperty.getName());
			}
		}
		rule += ")";
		return rule;
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
		String prefixMapping = node.toString(rootShape.getShapeGraph().getPrefixMapping());
		return node.toString().equals(prefixMapping) ? "<" + node.toString() + ">" : prefixMapping;
	}
	
	/**
	 * Returns the name of the target with short prefix of this KnowledgeGraphClass.
	 * 
	 * e.g.: aixm:Point
	 * 
	 * @return
	 */
	public String getNameOfTargetsWithPrefixShort() {
		String target = "";
		Collection<Target> targetCollection = rootShape.getTargets();
		for(Target t : targetCollection) {
			 target += getPrefixMapping(t.getObject(), rootShape);
		}
		return target;
	}
	
	/**
	 * Returns the name of the target with short prefix of this KnowledgeGraphClass.
	 * 
	 * e.g.: aixm:Point
	 * 
	 * @return
	 */
	public String getNameOfTargetsWithPrefixShortAndUnderScore() {
		String target = "";
		Collection<Target> targetCollection = rootShape.getTargets();
		for(Target t : targetCollection) {
			target += getPrefixMapping(t.getObject(), rootShape);
		}
		if(target.startsWith("<http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#")) {
			String[] pathSplitted = target.split("#");
			return "fixm_" + pathSplitted[1].substring(0, pathSplitted[1].length()-1);
		} else if(target.startsWith("<http://www.aisa-project.eu/vocabulary/aixm_5-1-1#")) { 
			String[] pathSplitted = target.split("#");
			return "aixm_" + pathSplitted[1].substring(0, pathSplitted[1].length()-1);	
		} else if(target.startsWith("<http://www.opengis.net/gml/3.2#")) {
			String[] pathSplitted = target.split("gml/3.2#");
			return "gml_" + pathSplitted[1].substring(0, pathSplitted[1].length()-1);
		} else if(target.startsWith("<http://www.aisa-project.eu/vocabulary/plain#")) {
			String[] pathSplitted = target.split("plain#");
			return "plain_" + pathSplitted[1].substring(0, pathSplitted[1].length()-1);			
		}
			
		String[] split = target.split(":");
		return split[0] + "_" + split[1];
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
