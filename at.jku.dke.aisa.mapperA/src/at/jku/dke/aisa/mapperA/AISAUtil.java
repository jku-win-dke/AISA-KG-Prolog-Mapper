package at.jku.dke.aisa.mapperA;

import org.apache.jena.graph.Node;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;

/**
 * Helper to create aisa resources and nodes used for finding different types of NodeShapes in the shape graphs.
 * 
 * e.g: shaclGraph.find(null, null, AISAUtil.objectNodeShapeAsNode());
 * returns: all triples with the matching pattern.
 * 
 */
public class AISAUtil {
	
	public static Resource OBJECT_NODE_SHAPE = ResourceFactory.createResource("http://www.aisa-project.eu/vocabulary/aixm_5-1-1#ObjectNodeShape");

	public static Resource CODE_LIST_NODE_SHAPE = ResourceFactory.createResource("http://www.aisa-project.eu/vocabulary/aixm_5-1-1#CodeListNodeShape");

	public static Resource BASIC_ELEMENT_NODE_SHAPE = ResourceFactory.createResource("http://www.aisa-project.eu/vocabulary/aixm_5-1-1#BasicElementNodeShape");
	
	public static Resource DATA_TYPE_NODE_SHAPE = ResourceFactory.createResource("http://www.aisa-project.eu/vocabulary/aixm_5-1-1#DataTypeNodeShape");
	
	public static Resource FEATURE_NODE_SHAPE = ResourceFactory.createResource("http://www.aisa-project.eu/vocabulary/aixm_5-1-1#FeatureNodeShape");

	public static Resource PLAIN_NODE_SHAPE = ResourceFactory.createResource("http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#PlainNodeShape");
	
	public static Resource ENUMERATION_NODE_SHAPE = ResourceFactory.createResource("http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#EnumerationNodeShape");
	
	public static Resource CHOICE_NODE_SHAPE = ResourceFactory.createResource("http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#ChoiceNodeShape");
	
	public static Node objectNodeShapeAsNode() {
		return OBJECT_NODE_SHAPE.asNode();
	}
	
	public static Node codeListNodeShapeAsNode() {
		return CODE_LIST_NODE_SHAPE.asNode();
	}
	
	public static Node basicElementNodeShapeAsNode() {
		return BASIC_ELEMENT_NODE_SHAPE.asNode();
	}
	
	public static Node dataTypeNodeShapeAsNode() {
		return DATA_TYPE_NODE_SHAPE.asNode();
	}
	
	public static Node featureNodeShapeAsNode() {
		return FEATURE_NODE_SHAPE.asNode();
	}
	
	public static Node plainNodeShapeAsNode() {
		return PLAIN_NODE_SHAPE.asNode();
	}
	
	public static Node enumerationNodeShapeAsNode() {
		return ENUMERATION_NODE_SHAPE.asNode();
	}
	
	public static Node choiceNodeShapeAsNode() {
		return CHOICE_NODE_SHAPE.asNode();
	}
}
