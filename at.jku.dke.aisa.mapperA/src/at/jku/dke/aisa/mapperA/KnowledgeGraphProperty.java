package at.jku.dke.aisa.mapperA;

import org.apache.jena.graph.Node;
import org.apache.jena.graph.Triple;
import org.apache.jena.shacl.parser.PropertyShape;
import org.apache.jena.shacl.parser.Shape;
import org.apache.jena.sparql.path.Path;
import org.apache.jena.util.iterator.ExtendedIterator;

/**
 * Depicts a PropertyShape, which belongs to a KnowledgeGraphClass / target shape of the shacl graph.
 *
 */
public class KnowledgeGraphProperty {

	private KnowledgeGraphClass knowledgeGraphClass;
	private PropertyShape property;
	private String name;
	public int minCount;
	public int maxCount;
	private String nameOfPathWithShortPrefix;
	private boolean isShaclClass;
	
	private String selectFragment;
	private String whereFragment;
	
	/**
	 * Takes the respective KnowledgeGraphClass and PropertyShape as input and creates all required variables for later use.
	 * 
	 * @param knowledgeGraphClass
	 * @param property
	 */
	public KnowledgeGraphProperty(KnowledgeGraphClass knowledgeGraphClass, PropertyShape property) {
		this.knowledgeGraphClass = knowledgeGraphClass;
		this.property = property;
		this.name = getNameOfPath(property.getPath());
		this.minCount = getMinCount();
		this.maxCount = getMaxCount();
		this.nameOfPathWithShortPrefix = getNameOfPathWithShortPrefix();
		this.isShaclClass = checkIfPropertyIsShaclClass();
		
		createSelectFragment();
		createWhereFragment();
	}

	/**
	 * returns the name of the property.
	 * 
	 * e.g.: city
	 * 
	 * @return
	 */
	public String getName() {
		return this.name;
	}
	
	/**
	 * Creates select fragment, which is used later for SPARQL query generation.
	 * 
	 * e.g.: (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat)
	 */
	private void createSelectFragment() {
		if(maxCount == 0) {
			this.selectFragment = "(GROUP_CONCAT(DISTINCT ?" + name + ";SEPARATOR=\",\") AS ?" + name + "Concat)";
		} else {
			this.selectFragment = "?" + name;
		}
	}

	/**
	 * Returns the in the constructor created select fragment of the property.
	 * 
	 * @return
	 */
	public String getSelectFragment() {
		return this.selectFragment;
	}
	
	
	/**
	 * Creates the where fragment, which is used later for SPARQL query generation.
	 * 
	 * e.g.: OPTIONAL {?city aixm:annotation ?annotation .}
	 */
	private void createWhereFragment() {
		if(isShaclClass && minCount == 1) {
			whereFragment = "      ?" + knowledgeGraphClass.predicateName + " " + nameOfPathWithShortPrefix + " ?" + name + " ." + '\n';
		} else if(isShaclClass && minCount == 0) {
			whereFragment = "      OPTIONAL {?" + knowledgeGraphClass.predicateName + " " + nameOfPathWithShortPrefix + " ?" + name + " ." + "}" + '\n';
		} else if(minCount == 1) {
			whereFragment = "      ?" + knowledgeGraphClass.predicateName + " " + nameOfPathWithShortPrefix + "  ?_" + name + " ." + '\n' //
					+ "        {" + '\n' //
					+ "          {" + '\n' //
					+ "            ?_" + name + " rdf:value ?" + name + "Value ." +'\n' //
					+ "            FILTER ( NOT EXISTS {?_" + name + " aixm:uom ?" + name + "UoM})" + '\n' //
					+ "            BIND(concat('val:',?" + name + "Value) AS ?" + name + ")" + '\n' //
					+ "          }" + '\n' //
					+ "		     UNION" + '\n' //
					+ "		     {" + '\n' //
					+ "            ?_" + name + "\n" //
					+ "              rdf:value ?" + name + "Value ;" + '\n' //
					+ "              aixm:uom ?" + name + "UoM ." + '\n' //
					+ "              BIND(concat('xval:',STR(?" + name + "Value),':',?" + name + "UoM) AS ?" + name + ")" + '\n' //
					+ "          }" + '\n' //
					+ "          UNION" + '\n' //
					+ "          {" + '\n' //
					+ "		       ?_" + name + "  aixm:nilReason ?" + name + "NilReason ." + '\n'
					+ "		       BIND(concat('nil:',?" + name + "NilReason) AS ?" + name + ")" + '\n' //
					+ "		   }" + '\n' //
					+ "      }" + '\n'; //
		} else {
			whereFragment = "      OPTIONAL { ?" + knowledgeGraphClass.predicateName + " " + nameOfPathWithShortPrefix + " ?_" + name + " ." + '\n' //
							+ "        {" + '\n' //
							+ "          {" + '\n' //
							+ "            ?_" + name + " rdf:value ?" + name + "Value ." +'\n' //
							+ "            FILTER ( NOT EXISTS {?_" + name + " aixm:uom ?" + name + "UoM})" + '\n' //
							+ "            BIND(concat('val:',?" + name + "Value) AS ?" + name + ")" + '\n' //
							+ "          }" + '\n' //
							+ "            UNION" + '\n' //
							+ "          {" + '\n' //
							+ "            ?_" + name + "\n" //
							+ "              rdf:value ?" + name + "Value ;" + '\n' //
							+ "              aixm:uom ?" + name + "UoM ." + '\n' //
							+ "            BIND(concat('xval:',STR(?" + name + "Value),':',?" + name + "UoM) AS ?" + name + ")" + '\n' //
							+ "          }" + '\n' //
							+ "            UNION" + '\n' //
							+ "          {" + '\n' //
							+ "           ?_" + name + "  aixm:nilReason ?" + name + "NilReason ." + '\n'
							+ "           BIND(concat('nil:',?" + name + "NilReason) AS ?" + name + ")" + '\n' //
							+ "          }" + '\n' //
							+ "        }" + '\n' //
							+ "      }" + '\n';

		}
	}
	
	/**
	 * Returns the in the constructor created where fragment of the property.
	 * 
	 * @return
	 */
	public String getWhereFragment() {
		return this.whereFragment;
	}
	
	/**
	 * Returns the sh:maxCount of a ProperyShape.
	 * 
	 * @return
	 */
	private int getMaxCount() {
		int max = 0;
		ExtendedIterator<Triple> triplesWithMaxCount = property.getShapeGraph().find(property.getShapeNode(), ShaclUtil.shaclMaxOrderAsNode(), null);
		while(triplesWithMaxCount.hasNext()) {
			Triple maxCount = triplesWithMaxCount.next();
			max = Integer.parseInt(maxCount.getObject().getLiteralValue().toString());
		}
		return max;
	}
	
	/**
	 * Returns the sh:minCount of a PropertyShape.
	 * 
	 * @param property
	 * @return
	 */
	private int getMinCount() {
		int min = 0;
		ExtendedIterator<Triple> triplesWithMinCount = property.getShapeGraph().find(property.getShapeNode(), ShaclUtil.shaclMinOrderAsNode(), null);
		while(triplesWithMinCount.hasNext()) {
			Triple minCount = triplesWithMinCount.next();
			min = Integer.parseInt(minCount.getObject().getLiteralValue().toString());
		}
		return min;
	}
	
	/**
	 * Returns the cardinality symbol for the given property.
	 * This method is used for the Prolog schema comment generation.
	 * 
	 * e.g.: % city(Graph, City, Name?, Annotation*)
	 * 
	 * @return
	 */
	public String getCardinality() {
		if(minCount == 0 && maxCount == 0) {
			return "*";
		} else if(minCount == 1 && maxCount == 1) {
			return "";
		} else if(minCount == 0 && maxCount == 1) {
			return "?";
		} else if(minCount == 1 && maxCount == 0) {
			return "+";
		}
		return "";
	}
	
	/**
	 * Return only name from a given path.
	 * 
	 * e.g.: input path: <http://example.org/gender>
	 * returns: gender
	 * 
	 * @param path
	 * @return
	 */
	private String getNameOfPath(Path path) {
		if(path.toString().contains("aixm_5-1-1#")) {
			String[] pathSplitted = path.toString().split("#");
			return pathSplitted[1].substring(0, pathSplitted[1].length()-1);
		} else if(path.toString().startsWith("<http://www.opengis.net/gml/3.2#")) {
			String[] pathSplitted = path.toString().split("gml/3.2#");
			return pathSplitted[1].substring(0, pathSplitted[1].length()-1);
		}
		String[] pathSplitted = path.toString().split("/");
		return pathSplitted[pathSplitted.length-1].substring(0, pathSplitted[pathSplitted.length-1].length()-1);
	}
	
	/**
	 * Checks if this property is a shacl class. 
	 * Shacl classes are handled differently from literals when generating the where fragment.
	 * 
	 * @return
	 */
	private boolean checkIfPropertyIsShaclClass() {
		ExtendedIterator<Triple> classProperties = property.getShapeGraph().find(property.getShapeNode(), ShaclUtil.shaclClassAsNode(), null);			
			while(classProperties.hasNext()) {
				return true;
			}
		return false;
	}

	/**
	 * Returns the name of the path of this property with its short prefix mapping.
	 * 
	 * e.g.: aixm:City
	 * 
	 * @return
	 */
	private String getNameOfPathWithShortPrefix() {
		String path = "";
		ExtendedIterator<Triple> iterator = property.getShapeGraph().find(property.getShapeNode(), ShaclUtil.shaclPathAsNode(), null);
		while(iterator.hasNext()) {
			Triple t = iterator.next();
			path += getPrefixMapping(t.getObject(), knowledgeGraphClass.rootShape);
		}
		return path;
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
}
