:- module(prolog2,[]).  % the Prolog module name must be the same as the KG module name

airportorg_annotation(Graph,AirportHeliportResponsibilityOrganisation, Annotation) :- 
  aixm_AirportHeliportResponsibilityOrganisation_Combined(Graph, AirportHeliportResponsibilityOrganisation, Annotations, _,_,_,_), member(Annotation, Annotations).
  
run :- 
  forall( airportorg_annotation(_,O,A), insert_rdf(O,'http://ex.org/annotatedBy',A) ),
  forall( airportorg_annotation(G,O,_), insert_rdf(O,'http://ex.org/mentionedIn',G) ) .