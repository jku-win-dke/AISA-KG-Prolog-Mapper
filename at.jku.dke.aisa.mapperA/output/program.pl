:- [facts].

airportorg_annotation(AirportHeliportResponsibilityOrganisation, Annotation) :- 
  aixm_AirportHeliportResponsibilityOrganisation_Combined(_, AirportHeliportResponsibilityOrganisation, Annotations, _,_,_,_), member(Annotation, Annotations).
  
run :- 
  forall( airportorg_annotation(O,A), rdf_assert(O,'http://ex.org/annotatedBy',A,'http://ex.org/output') ),
  rdf_save_turtle( 'C:/Users/neumayr/git/AISA-KG-Prolog-Mapper/at.jku.dke.aisa.mapperA/output/output.ttl', [graph('http://ex.org/output')] ).