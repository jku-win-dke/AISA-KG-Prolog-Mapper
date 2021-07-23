package at.jku.dke.aisa.mapperC;

import org.apache.jena.graph.Node;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;

/**
 * Helper to create shacl resources and nodes used for finding Subjects,Properties,Objects in the shape graphs.
 * 
 * e.g: property.getShapeGraph().find(property.getShapeNode(), ShaclUtil.shaclOrderAsNode(), null);
 * returns: all triples with the matching pattern.
 * 
 * Especially useful for finding sh:order of a property.
 */
public class ShaclUtil {

	public static Resource NODE_SHAPE_RESOURCE = ResourceFactory.createResource("http://www.w3.org/ns/shacl#NodeShape");
	
	public static Resource SHACL_ORDER_RESOURCE = ResourceFactory.createResource("http://www.w3.org/ns/shacl#order");
	
	public static Resource SHACL_PATH_RESOURCE = ResourceFactory.createResource("http://www.w3.org/ns/shacl#path");
	
	public static Resource SHACL_MAXCOUNT_RESOURCE = ResourceFactory.createResource("http://www.w3.org/ns/shacl#maxCount");
	
	public static Resource SHACL_MINCOUNT_RESOURCE = ResourceFactory.createResource("http://www.w3.org/ns/shacl#minCount");
	
	public static Resource SHACL_NODE_RESOURCE = ResourceFactory.createResource("http://www.w3.org/ns/shacl#node");
	
	public static Resource SHACL_CLASS_RESOURCE = ResourceFactory.createResource("http://www.w3.org/ns/shacl#class");
	
	public static Resource RDFS_SUBCLASSOF_RESOURCE = ResourceFactory.createResource("http://www.w3.org/2000/01/rdf-schema#subClassOf");
	
	public static Node shaclNodeShapeAsNode() {
		return NODE_SHAPE_RESOURCE.asNode();
	}
	
	public static Node shaclOrderAsNode() {
		return SHACL_ORDER_RESOURCE.asNode();
	}
	
	public static Node shaclPathAsNode() {
		return SHACL_PATH_RESOURCE.asNode();
	}
	
	public static Node shaclMaxOrderAsNode() {
		return SHACL_MAXCOUNT_RESOURCE.asNode();
	}
	
	public static Node shaclMinOrderAsNode() {
		return SHACL_MINCOUNT_RESOURCE.asNode();
	}
	
	public static Node shaclNodeAsNode() {
		return SHACL_NODE_RESOURCE.asNode();
	}
	
	public static Node shaclClassAsNode() {
		return SHACL_CLASS_RESOURCE.asNode();
	}
	
	public static Node rdfsSubClassOfAsNode() {
		return RDFS_SUBCLASSOF_RESOURCE.asNode();
	}
}
