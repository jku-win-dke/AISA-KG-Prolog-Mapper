:- module(prolog1,[]).  % the Prolog module name must be the same as the KG module name

% Test the SHACL2Prolog Mapping  
testMapping(TimePeriod,Graph) :- 
	gml_TimePeriod(Graph, TimePeriod, _,_).

run :- 
  forall( testMapping(X,G), insert_rdf( X, 'http://ex.org/in', G ) ),
  forall( rdf(_,_,_,G), insert_rdf( G, rdf:type, 'http://ex.org/Graph' ) ) .
