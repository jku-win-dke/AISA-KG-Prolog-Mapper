:- [facts].

airportorg_annotation(Graph,AirportHeliportResponsibilityOrganisation, Annotation) :- 
  aixm_AirportHeliportResponsibilityOrganisation_Combined(Graph, AirportHeliportResponsibilityOrganisation, Annotations, _,_,_,_), member(Annotation, Annotations).
  
run :- 
  forall( airportorg_annotation(_,O,A), rdf_assert(O,'http://ex.org/annotatedBy',A,'http://ex.org/output') ),
  forall( airportorg_annotation(G,O,_), rdf_assert(O,'http://ex.org/mentionedIn',G,'http://ex.org/output') ) 
  .
  
save :- 
  rdf_save_turtle( 'C:/Users/neumayr/git/AISA-KG-Prolog-Mapper/at.jku.dke.aisa.mapperA/output/output.ttl', [graph('http://ex.org/output')] ).