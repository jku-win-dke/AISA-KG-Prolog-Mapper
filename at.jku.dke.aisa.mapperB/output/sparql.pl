/* use the new RDF-DB library 
https://www.swi-prolog.org/pldoc/man?section=semweb-rdf11 */
:- use_module(library(semweb/rdf11)).

/* for writing/reading RDF files in Turtle format we use the Turtle library
https://www.swi-prolog.org/pldoc/man?section=turtle */
:- use_module(library(semweb/turtle)). 

/* do not output bindings for anonymous variables (e.g., _X) in query results 
https://www.swi-prolog.org/pldoc/man?section=flags#flag:toplevel_print_anon */
:- set_prolog_flag(toplevel_print_anon, false).

:- use_module(library(semweb/sparql_client)).
:- sparql_set_server([host(localhost),port(3030),path('/test/sparql')]).

convert(ConcatenatedString,ListOfAtoms) :-
  ConcatenatedString = literal(XConcatenatedString),
  split_string(XConcatenatedString, ',', '', ListOfStrings),
  maplist(string_atom, ListOfStrings, ListOfAtoms).

convert(Null,[]) :-
  Null = '$null$'.

string_atom(X,Y) :- atom_string(Y,X).

% fixm_PostalAddress(Graph, PostalAddress, AdministrativeArea?, PostalCode?, DeliveryPoint?, CountryCode?, CountryName?, City?)

fixm_PostalAddress(Graph, PostalAddress, AdministrativeArea, PostalCode, DeliveryPoint, CountryCode, CountryName, City) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?postalAddress ?administrativeArea ?postalCode ?deliveryPoint ?countryCode ?countryName ?city
WHERE
  { GRAPH ?graph
    {
      ?postalAddress rdf:type fixm:PostalAddress .
      OPTIONAL { ?postalAddress fixm:administrativeArea ?_administrativeArea .
        {
          {
            ?_administrativeArea rdf:value ?administrativeAreaValue .
            FILTER ( NOT EXISTS {?_administrativeArea (aixm:uom | fixm:uom | plain:uom) ?administrativeAreaUoM})
            BIND(concat(\'val:\',?administrativeAreaValue) AS ?administrativeArea)
          }
            UNION
          {
            ?_administrativeArea
              rdf:value ?administrativeAreaValue ;
              (aixm:uom | fixm:uom | plain:uom) ?administrativeAreaUoM .
            BIND(concat(\'xval:\',STR(?administrativeAreaValue),\':\',?administrativeAreaUoM) AS ?administrativeArea)
          }
            UNION
          {
           ?_administrativeArea  aixm:nilReason ?administrativeAreaNilReason .
           BIND(concat(\'nil:\',?administrativeAreaNilReason) AS ?administrativeArea)
          }
        }
      }
      OPTIONAL { ?postalAddress fixm:postalCode ?_postalCode .
        {
          {
            ?_postalCode rdf:value ?postalCodeValue .
            FILTER ( NOT EXISTS {?_postalCode (aixm:uom | fixm:uom | plain:uom) ?postalCodeUoM})
            BIND(concat(\'val:\',?postalCodeValue) AS ?postalCode)
          }
            UNION
          {
            ?_postalCode
              rdf:value ?postalCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?postalCodeUoM .
            BIND(concat(\'xval:\',STR(?postalCodeValue),\':\',?postalCodeUoM) AS ?postalCode)
          }
            UNION
          {
           ?_postalCode  aixm:nilReason ?postalCodeNilReason .
           BIND(concat(\'nil:\',?postalCodeNilReason) AS ?postalCode)
          }
        }
      }
      OPTIONAL { ?postalAddress fixm:deliveryPoint ?_deliveryPoint .
        {
          {
            ?_deliveryPoint rdf:value ?deliveryPointValue .
            FILTER ( NOT EXISTS {?_deliveryPoint (aixm:uom | fixm:uom | plain:uom) ?deliveryPointUoM})
            BIND(concat(\'val:\',?deliveryPointValue) AS ?deliveryPoint)
          }
            UNION
          {
            ?_deliveryPoint
              rdf:value ?deliveryPointValue ;
              (aixm:uom | fixm:uom | plain:uom) ?deliveryPointUoM .
            BIND(concat(\'xval:\',STR(?deliveryPointValue),\':\',?deliveryPointUoM) AS ?deliveryPoint)
          }
            UNION
          {
           ?_deliveryPoint  aixm:nilReason ?deliveryPointNilReason .
           BIND(concat(\'nil:\',?deliveryPointNilReason) AS ?deliveryPoint)
          }
        }
      }
      OPTIONAL { ?postalAddress fixm:countryCode ?_countryCode .
        {
          {
            ?_countryCode rdf:value ?countryCodeValue .
            FILTER ( NOT EXISTS {?_countryCode (aixm:uom | fixm:uom | plain:uom) ?countryCodeUoM})
            BIND(concat(\'val:\',?countryCodeValue) AS ?countryCode)
          }
            UNION
          {
            ?_countryCode
              rdf:value ?countryCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?countryCodeUoM .
            BIND(concat(\'xval:\',STR(?countryCodeValue),\':\',?countryCodeUoM) AS ?countryCode)
          }
            UNION
          {
           ?_countryCode  aixm:nilReason ?countryCodeNilReason .
           BIND(concat(\'nil:\',?countryCodeNilReason) AS ?countryCode)
          }
        }
      }
      OPTIONAL { ?postalAddress fixm:countryName ?_countryName .
        {
          {
            ?_countryName rdf:value ?countryNameValue .
            FILTER ( NOT EXISTS {?_countryName (aixm:uom | fixm:uom | plain:uom) ?countryNameUoM})
            BIND(concat(\'val:\',?countryNameValue) AS ?countryName)
          }
            UNION
          {
            ?_countryName
              rdf:value ?countryNameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?countryNameUoM .
            BIND(concat(\'xval:\',STR(?countryNameValue),\':\',?countryNameUoM) AS ?countryName)
          }
            UNION
          {
           ?_countryName  aixm:nilReason ?countryNameNilReason .
           BIND(concat(\'nil:\',?countryNameNilReason) AS ?countryName)
          }
        }
      }
      OPTIONAL { ?postalAddress fixm:city ?_city .
        {
          {
            ?_city rdf:value ?cityValue .
            FILTER ( NOT EXISTS {?_city (aixm:uom | fixm:uom | plain:uom) ?cityUoM})
            BIND(concat(\'val:\',?cityValue) AS ?city)
          }
            UNION
          {
            ?_city
              rdf:value ?cityValue ;
              (aixm:uom | fixm:uom | plain:uom) ?cityUoM .
            BIND(concat(\'xval:\',STR(?cityValue),\':\',?cityUoM) AS ?city)
          }
            UNION
          {
           ?_city  aixm:nilReason ?cityNilReason .
           BIND(concat(\'nil:\',?cityNilReason) AS ?city)
          }
        }
      }
    }
  }

      '
,row(Graph,PostalAddress,AdministrativeArea,PostalCode,DeliveryPoint,CountryCode,CountryName,City),[]).

% fixm_NavigationCapabilities(Graph, NavigationCapabilities, OtherNavigationCapabilities?, PerformanceBasedCode*, NavigationCode*)

fixm_NavigationCapabilities(Graph, NavigationCapabilities, OtherNavigationCapabilities, PerformanceBasedCodeList, NavigationCodeList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?navigationCapabilities ?otherNavigationCapabilities (GROUP_CONCAT(DISTINCT ?performanceBasedCode;SEPARATOR=",") AS ?performanceBasedCodeConcat) (GROUP_CONCAT(DISTINCT ?navigationCode;SEPARATOR=",") AS ?navigationCodeConcat)
WHERE
  { GRAPH ?graph
    {
      ?navigationCapabilities rdf:type fixm:NavigationCapabilities .
      OPTIONAL { ?navigationCapabilities fixm:otherNavigationCapabilities ?_otherNavigationCapabilities .
        {
          {
            ?_otherNavigationCapabilities rdf:value ?otherNavigationCapabilitiesValue .
            FILTER ( NOT EXISTS {?_otherNavigationCapabilities (aixm:uom | fixm:uom | plain:uom) ?otherNavigationCapabilitiesUoM})
            BIND(concat(\'val:\',?otherNavigationCapabilitiesValue) AS ?otherNavigationCapabilities)
          }
            UNION
          {
            ?_otherNavigationCapabilities
              rdf:value ?otherNavigationCapabilitiesValue ;
              (aixm:uom | fixm:uom | plain:uom) ?otherNavigationCapabilitiesUoM .
            BIND(concat(\'xval:\',STR(?otherNavigationCapabilitiesValue),\':\',?otherNavigationCapabilitiesUoM) AS ?otherNavigationCapabilities)
          }
            UNION
          {
           ?_otherNavigationCapabilities  aixm:nilReason ?otherNavigationCapabilitiesNilReason .
           BIND(concat(\'nil:\',?otherNavigationCapabilitiesNilReason) AS ?otherNavigationCapabilities)
          }
        }
      }
      OPTIONAL { ?navigationCapabilities fixm:performanceBasedCode ?_performanceBasedCode .
        {
          {
            ?_performanceBasedCode rdf:value ?performanceBasedCodeValue .
            FILTER ( NOT EXISTS {?_performanceBasedCode (aixm:uom | fixm:uom | plain:uom) ?performanceBasedCodeUoM})
            BIND(concat(\'val:\',?performanceBasedCodeValue) AS ?performanceBasedCode)
          }
            UNION
          {
            ?_performanceBasedCode
              rdf:value ?performanceBasedCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?performanceBasedCodeUoM .
            BIND(concat(\'xval:\',STR(?performanceBasedCodeValue),\':\',?performanceBasedCodeUoM) AS ?performanceBasedCode)
          }
            UNION
          {
           ?_performanceBasedCode  aixm:nilReason ?performanceBasedCodeNilReason .
           BIND(concat(\'nil:\',?performanceBasedCodeNilReason) AS ?performanceBasedCode)
          }
        }
      }
      OPTIONAL { ?navigationCapabilities fixm:navigationCode ?_navigationCode .
        {
          {
            ?_navigationCode rdf:value ?navigationCodeValue .
            FILTER ( NOT EXISTS {?_navigationCode (aixm:uom | fixm:uom | plain:uom) ?navigationCodeUoM})
            BIND(concat(\'val:\',?navigationCodeValue) AS ?navigationCode)
          }
            UNION
          {
            ?_navigationCode
              rdf:value ?navigationCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?navigationCodeUoM .
            BIND(concat(\'xval:\',STR(?navigationCodeValue),\':\',?navigationCodeUoM) AS ?navigationCode)
          }
            UNION
          {
           ?_navigationCode  aixm:nilReason ?navigationCodeNilReason .
           BIND(concat(\'nil:\',?navigationCodeNilReason) AS ?navigationCode)
          }
        }
      }
    }
  }
GROUP BY ?graph ?navigationCapabilities ?otherNavigationCapabilities

      '
,row(Graph,NavigationCapabilities,OtherNavigationCapabilities,PerformanceBasedCodeConcat,NavigationCodeConcat),[]), convert(PerformanceBasedCodeConcat,PerformanceBasedCodeList), convert(NavigationCodeConcat,NavigationCodeList).

% fixm_GroundspeedRange(Graph, GroundspeedRange, LowerSpeed?, UpperSpeed?)

fixm_GroundspeedRange(Graph, GroundspeedRange, LowerSpeed, UpperSpeed) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?groundspeedRange ?lowerSpeed ?upperSpeed
WHERE
  { GRAPH ?graph
    {
      ?groundspeedRange rdf:type fixm:GroundspeedRange .
      OPTIONAL { ?groundspeedRange fixm:lowerSpeed ?_lowerSpeed .
        {
          {
            ?_lowerSpeed rdf:value ?lowerSpeedValue .
            FILTER ( NOT EXISTS {?_lowerSpeed (aixm:uom | fixm:uom | plain:uom) ?lowerSpeedUoM})
            BIND(concat(\'val:\',?lowerSpeedValue) AS ?lowerSpeed)
          }
            UNION
          {
            ?_lowerSpeed
              rdf:value ?lowerSpeedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?lowerSpeedUoM .
            BIND(concat(\'xval:\',STR(?lowerSpeedValue),\':\',?lowerSpeedUoM) AS ?lowerSpeed)
          }
            UNION
          {
           ?_lowerSpeed  aixm:nilReason ?lowerSpeedNilReason .
           BIND(concat(\'nil:\',?lowerSpeedNilReason) AS ?lowerSpeed)
          }
        }
      }
      OPTIONAL { ?groundspeedRange fixm:upperSpeed ?_upperSpeed .
        {
          {
            ?_upperSpeed rdf:value ?upperSpeedValue .
            FILTER ( NOT EXISTS {?_upperSpeed (aixm:uom | fixm:uom | plain:uom) ?upperSpeedUoM})
            BIND(concat(\'val:\',?upperSpeedValue) AS ?upperSpeed)
          }
            UNION
          {
            ?_upperSpeed
              rdf:value ?upperSpeedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?upperSpeedUoM .
            BIND(concat(\'xval:\',STR(?upperSpeedValue),\':\',?upperSpeedUoM) AS ?upperSpeed)
          }
            UNION
          {
           ?_upperSpeed  aixm:nilReason ?upperSpeedNilReason .
           BIND(concat(\'nil:\',?upperSpeedNilReason) AS ?upperSpeed)
          }
        }
      }
    }
  }

      '
,row(Graph,GroundspeedRange,LowerSpeed,UpperSpeed),[]).

% aixm_Note(Graph, Note, PropertyName?, Purpose?, TranslatedNote*)

aixm_Note(Graph, Note, PropertyName, Purpose, TranslatedNoteList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?note ?propertyName ?purpose (GROUP_CONCAT(DISTINCT ?translatedNote;SEPARATOR=",") AS ?translatedNoteConcat)
WHERE
  { GRAPH ?graph
    {
      ?note rdf:type aixm:Note .
      OPTIONAL { ?note aixm:propertyName ?_propertyName .
        {
          {
            ?_propertyName rdf:value ?propertyNameValue .
            FILTER ( NOT EXISTS {?_propertyName (aixm:uom | fixm:uom | plain:uom) ?propertyNameUoM})
            BIND(concat(\'val:\',?propertyNameValue) AS ?propertyName)
          }
            UNION
          {
            ?_propertyName
              rdf:value ?propertyNameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?propertyNameUoM .
            BIND(concat(\'xval:\',STR(?propertyNameValue),\':\',?propertyNameUoM) AS ?propertyName)
          }
            UNION
          {
           ?_propertyName  aixm:nilReason ?propertyNameNilReason .
           BIND(concat(\'nil:\',?propertyNameNilReason) AS ?propertyName)
          }
        }
      }
      OPTIONAL { ?note aixm:purpose ?_purpose .
        {
          {
            ?_purpose rdf:value ?purposeValue .
            FILTER ( NOT EXISTS {?_purpose (aixm:uom | fixm:uom | plain:uom) ?purposeUoM})
            BIND(concat(\'val:\',?purposeValue) AS ?purpose)
          }
            UNION
          {
            ?_purpose
              rdf:value ?purposeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?purposeUoM .
            BIND(concat(\'xval:\',STR(?purposeValue),\':\',?purposeUoM) AS ?purpose)
          }
            UNION
          {
           ?_purpose  aixm:nilReason ?purposeNilReason .
           BIND(concat(\'nil:\',?purposeNilReason) AS ?purpose)
          }
        }
      }
      OPTIONAL {?note aixm:translatedNote ?translatedNote .}
    }
  }
GROUP BY ?graph ?note ?propertyName ?purpose

      '
,row(Graph,Note,PropertyName,Purpose,TranslatedNoteConcat),[]), convert(TranslatedNoteConcat,TranslatedNoteList).

% fixm_Pointout(Graph, Pointout, OriginatingUnit?, ReceivingUnit*)

fixm_Pointout(Graph, Pointout, OriginatingUnit, ReceivingUnitList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?pointout ?originatingUnit (GROUP_CONCAT(DISTINCT ?receivingUnit;SEPARATOR=",") AS ?receivingUnitConcat)
WHERE
  { GRAPH ?graph
    {
      ?pointout rdf:type fixm:Pointout .
      OPTIONAL {?pointout fixm:originatingUnit ?originatingUnit .}
      OPTIONAL {?pointout fixm:receivingUnit ?receivingUnit .}
    }
  }
GROUP BY ?graph ?pointout ?originatingUnit

      '
,row(Graph,Pointout,OriginatingUnit,ReceivingUnitConcat),[]), convert(ReceivingUnitConcat,ReceivingUnitList).

% fixm_VerticalRange(Graph, VerticalRange, LowerBound?, UpperBound?)

fixm_VerticalRange(Graph, VerticalRange, LowerBound, UpperBound) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?verticalRange ?lowerBound ?upperBound
WHERE
  { GRAPH ?graph
    {
      ?verticalRange rdf:type fixm:VerticalRange .
      OPTIONAL { ?verticalRange fixm:lowerBound ?_lowerBound .
        {
          {
            ?_lowerBound rdf:value ?lowerBoundValue .
            FILTER ( NOT EXISTS {?_lowerBound (aixm:uom | fixm:uom | plain:uom) ?lowerBoundUoM})
            BIND(concat(\'val:\',?lowerBoundValue) AS ?lowerBound)
          }
            UNION
          {
            ?_lowerBound
              rdf:value ?lowerBoundValue ;
              (aixm:uom | fixm:uom | plain:uom) ?lowerBoundUoM .
            BIND(concat(\'xval:\',STR(?lowerBoundValue),\':\',?lowerBoundUoM) AS ?lowerBound)
          }
            UNION
          {
           ?_lowerBound  aixm:nilReason ?lowerBoundNilReason .
           BIND(concat(\'nil:\',?lowerBoundNilReason) AS ?lowerBound)
          }
        }
      }
      OPTIONAL { ?verticalRange fixm:upperBound ?_upperBound .
        {
          {
            ?_upperBound rdf:value ?upperBoundValue .
            FILTER ( NOT EXISTS {?_upperBound (aixm:uom | fixm:uom | plain:uom) ?upperBoundUoM})
            BIND(concat(\'val:\',?upperBoundValue) AS ?upperBound)
          }
            UNION
          {
            ?_upperBound
              rdf:value ?upperBoundValue ;
              (aixm:uom | fixm:uom | plain:uom) ?upperBoundUoM .
            BIND(concat(\'xval:\',STR(?upperBoundValue),\':\',?upperBoundUoM) AS ?upperBound)
          }
            UNION
          {
           ?_upperBound  aixm:nilReason ?upperBoundNilReason .
           BIND(concat(\'nil:\',?upperBoundNilReason) AS ?upperBound)
          }
        }
      }
    }
  }

      '
,row(Graph,VerticalRange,LowerBound,UpperBound),[]).

% fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel?, EstimatedTime?, Constraint*)

fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel, EstimatedTime, ConstraintList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?expandedRoutePoint ?estimatedLevel ?estimatedTime (GROUP_CONCAT(DISTINCT ?constraint;SEPARATOR=",") AS ?constraintConcat)
WHERE
  { GRAPH ?graph
    {
      ?expandedRoutePoint rdf:type fixm:ExpandedRoutePoint .
      OPTIONAL { ?expandedRoutePoint fixm:estimatedLevel ?_estimatedLevel .
        {
          {
            ?_estimatedLevel rdf:value ?estimatedLevelValue .
            FILTER ( NOT EXISTS {?_estimatedLevel (aixm:uom | fixm:uom | plain:uom) ?estimatedLevelUoM})
            BIND(concat(\'val:\',?estimatedLevelValue) AS ?estimatedLevel)
          }
            UNION
          {
            ?_estimatedLevel
              rdf:value ?estimatedLevelValue ;
              (aixm:uom | fixm:uom | plain:uom) ?estimatedLevelUoM .
            BIND(concat(\'xval:\',STR(?estimatedLevelValue),\':\',?estimatedLevelUoM) AS ?estimatedLevel)
          }
            UNION
          {
           ?_estimatedLevel  aixm:nilReason ?estimatedLevelNilReason .
           BIND(concat(\'nil:\',?estimatedLevelNilReason) AS ?estimatedLevel)
          }
        }
      }
      OPTIONAL { ?expandedRoutePoint fixm:estimatedTime ?_estimatedTime .
        {
          {
            ?_estimatedTime rdf:value ?estimatedTimeValue .
            FILTER ( NOT EXISTS {?_estimatedTime (aixm:uom | fixm:uom | plain:uom) ?estimatedTimeUoM})
            BIND(concat(\'val:\',?estimatedTimeValue) AS ?estimatedTime)
          }
            UNION
          {
            ?_estimatedTime
              rdf:value ?estimatedTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?estimatedTimeUoM .
            BIND(concat(\'xval:\',STR(?estimatedTimeValue),\':\',?estimatedTimeUoM) AS ?estimatedTime)
          }
            UNION
          {
           ?_estimatedTime  aixm:nilReason ?estimatedTimeNilReason .
           BIND(concat(\'nil:\',?estimatedTimeNilReason) AS ?estimatedTime)
          }
        }
      }
      OPTIONAL {?expandedRoutePoint fixm:constraint ?constraint .}
    }
  }
GROUP BY ?graph ?expandedRoutePoint ?estimatedLevel ?estimatedTime

      '
,row(Graph,ExpandedRoutePoint,EstimatedLevel,EstimatedTime,ConstraintConcat),[]), convert(ConstraintConcat,ConstraintList).

% aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)

aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?elevatedSurface ?elevation ?geoidUndulation ?verticalDatum ?verticalAccuracy
WHERE
  { GRAPH ?graph
    {
      ?elevatedSurface rdf:type aixm:ElevatedSurface .
      OPTIONAL { ?elevatedSurface aixm:elevation ?_elevation .
        {
          {
            ?_elevation rdf:value ?elevationValue .
            FILTER ( NOT EXISTS {?_elevation (aixm:uom | fixm:uom | plain:uom) ?elevationUoM})
            BIND(concat(\'val:\',?elevationValue) AS ?elevation)
          }
            UNION
          {
            ?_elevation
              rdf:value ?elevationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?elevationUoM .
            BIND(concat(\'xval:\',STR(?elevationValue),\':\',?elevationUoM) AS ?elevation)
          }
            UNION
          {
           ?_elevation  aixm:nilReason ?elevationNilReason .
           BIND(concat(\'nil:\',?elevationNilReason) AS ?elevation)
          }
        }
      }
      OPTIONAL { ?elevatedSurface aixm:geoidUndulation ?_geoidUndulation .
        {
          {
            ?_geoidUndulation rdf:value ?geoidUndulationValue .
            FILTER ( NOT EXISTS {?_geoidUndulation (aixm:uom | fixm:uom | plain:uom) ?geoidUndulationUoM})
            BIND(concat(\'val:\',?geoidUndulationValue) AS ?geoidUndulation)
          }
            UNION
          {
            ?_geoidUndulation
              rdf:value ?geoidUndulationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?geoidUndulationUoM .
            BIND(concat(\'xval:\',STR(?geoidUndulationValue),\':\',?geoidUndulationUoM) AS ?geoidUndulation)
          }
            UNION
          {
           ?_geoidUndulation  aixm:nilReason ?geoidUndulationNilReason .
           BIND(concat(\'nil:\',?geoidUndulationNilReason) AS ?geoidUndulation)
          }
        }
      }
      OPTIONAL { ?elevatedSurface aixm:verticalDatum ?_verticalDatum .
        {
          {
            ?_verticalDatum rdf:value ?verticalDatumValue .
            FILTER ( NOT EXISTS {?_verticalDatum (aixm:uom | fixm:uom | plain:uom) ?verticalDatumUoM})
            BIND(concat(\'val:\',?verticalDatumValue) AS ?verticalDatum)
          }
            UNION
          {
            ?_verticalDatum
              rdf:value ?verticalDatumValue ;
              (aixm:uom | fixm:uom | plain:uom) ?verticalDatumUoM .
            BIND(concat(\'xval:\',STR(?verticalDatumValue),\':\',?verticalDatumUoM) AS ?verticalDatum)
          }
            UNION
          {
           ?_verticalDatum  aixm:nilReason ?verticalDatumNilReason .
           BIND(concat(\'nil:\',?verticalDatumNilReason) AS ?verticalDatum)
          }
        }
      }
      OPTIONAL { ?elevatedSurface aixm:verticalAccuracy ?_verticalAccuracy .
        {
          {
            ?_verticalAccuracy rdf:value ?verticalAccuracyValue .
            FILTER ( NOT EXISTS {?_verticalAccuracy (aixm:uom | fixm:uom | plain:uom) ?verticalAccuracyUoM})
            BIND(concat(\'val:\',?verticalAccuracyValue) AS ?verticalAccuracy)
          }
            UNION
          {
            ?_verticalAccuracy
              rdf:value ?verticalAccuracyValue ;
              (aixm:uom | fixm:uom | plain:uom) ?verticalAccuracyUoM .
            BIND(concat(\'xval:\',STR(?verticalAccuracyValue),\':\',?verticalAccuracyUoM) AS ?verticalAccuracy)
          }
            UNION
          {
           ?_verticalAccuracy  aixm:nilReason ?verticalAccuracyNilReason .
           BIND(concat(\'nil:\',?verticalAccuracyNilReason) AS ?verticalAccuracy)
          }
        }
      }
    }
  }

      '
,row(Graph,ElevatedSurface,Elevation,GeoidUndulation,VerticalDatum,VerticalAccuracy),[]).

% fixm_Dimensions(Graph, Dimensions, Height?, Length?, Width?)

fixm_Dimensions(Graph, Dimensions, Height, Length, Width) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?dimensions ?height ?length ?width
WHERE
  { GRAPH ?graph
    {
      ?dimensions rdf:type fixm:Dimensions .
      OPTIONAL { ?dimensions fixm:height ?_height .
        {
          {
            ?_height rdf:value ?heightValue .
            FILTER ( NOT EXISTS {?_height (aixm:uom | fixm:uom | plain:uom) ?heightUoM})
            BIND(concat(\'val:\',?heightValue) AS ?height)
          }
            UNION
          {
            ?_height
              rdf:value ?heightValue ;
              (aixm:uom | fixm:uom | plain:uom) ?heightUoM .
            BIND(concat(\'xval:\',STR(?heightValue),\':\',?heightUoM) AS ?height)
          }
            UNION
          {
           ?_height  aixm:nilReason ?heightNilReason .
           BIND(concat(\'nil:\',?heightNilReason) AS ?height)
          }
        }
      }
      OPTIONAL { ?dimensions fixm:length ?_length .
        {
          {
            ?_length rdf:value ?lengthValue .
            FILTER ( NOT EXISTS {?_length (aixm:uom | fixm:uom | plain:uom) ?lengthUoM})
            BIND(concat(\'val:\',?lengthValue) AS ?length)
          }
            UNION
          {
            ?_length
              rdf:value ?lengthValue ;
              (aixm:uom | fixm:uom | plain:uom) ?lengthUoM .
            BIND(concat(\'xval:\',STR(?lengthValue),\':\',?lengthUoM) AS ?length)
          }
            UNION
          {
           ?_length  aixm:nilReason ?lengthNilReason .
           BIND(concat(\'nil:\',?lengthNilReason) AS ?length)
          }
        }
      }
      OPTIONAL { ?dimensions fixm:width ?_width .
        {
          {
            ?_width rdf:value ?widthValue .
            FILTER ( NOT EXISTS {?_width (aixm:uom | fixm:uom | plain:uom) ?widthUoM})
            BIND(concat(\'val:\',?widthValue) AS ?width)
          }
            UNION
          {
            ?_width
              rdf:value ?widthValue ;
              (aixm:uom | fixm:uom | plain:uom) ?widthUoM .
            BIND(concat(\'xval:\',STR(?widthValue),\':\',?widthUoM) AS ?width)
          }
            UNION
          {
           ?_width  aixm:nilReason ?widthNilReason .
           BIND(concat(\'nil:\',?widthNilReason) AS ?width)
          }
        }
      }
    }
  }

      '
,row(Graph,Dimensions,Height,Length,Width),[]).

% fixm_StandPositionAndTime(Graph, StandPositionAndTime, StandName?, StandTime?, TerminalName?)

fixm_StandPositionAndTime(Graph, StandPositionAndTime, StandName, StandTime, TerminalName) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?standPositionAndTime ?standName ?standTime ?terminalName
WHERE
  { GRAPH ?graph
    {
      ?standPositionAndTime rdf:type fixm:StandPositionAndTime .
      OPTIONAL { ?standPositionAndTime fixm:standName ?_standName .
        {
          {
            ?_standName rdf:value ?standNameValue .
            FILTER ( NOT EXISTS {?_standName (aixm:uom | fixm:uom | plain:uom) ?standNameUoM})
            BIND(concat(\'val:\',?standNameValue) AS ?standName)
          }
            UNION
          {
            ?_standName
              rdf:value ?standNameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?standNameUoM .
            BIND(concat(\'xval:\',STR(?standNameValue),\':\',?standNameUoM) AS ?standName)
          }
            UNION
          {
           ?_standName  aixm:nilReason ?standNameNilReason .
           BIND(concat(\'nil:\',?standNameNilReason) AS ?standName)
          }
        }
      }
      OPTIONAL {?standPositionAndTime fixm:standTime ?standTime .}
      OPTIONAL { ?standPositionAndTime fixm:terminalName ?_terminalName .
        {
          {
            ?_terminalName rdf:value ?terminalNameValue .
            FILTER ( NOT EXISTS {?_terminalName (aixm:uom | fixm:uom | plain:uom) ?terminalNameUoM})
            BIND(concat(\'val:\',?terminalNameValue) AS ?terminalName)
          }
            UNION
          {
            ?_terminalName
              rdf:value ?terminalNameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?terminalNameUoM .
            BIND(concat(\'xval:\',STR(?terminalNameValue),\':\',?terminalNameUoM) AS ?terminalName)
          }
            UNION
          {
           ?_terminalName  aixm:nilReason ?terminalNameNilReason .
           BIND(concat(\'nil:\',?terminalNameNilReason) AS ?terminalName)
          }
        }
      }
    }
  }

      '
,row(Graph,StandPositionAndTime,StandName,StandTime,TerminalName),[]).

% fixm_RouteSegment(Graph, RouteSegment, Airway?, RoutePoint?)

fixm_RouteSegment(Graph, RouteSegment, Airway, RoutePoint) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?routeSegment ?airway ?routePoint
WHERE
  { GRAPH ?graph
    {
      ?routeSegment rdf:type fixm:RouteSegment .
      OPTIONAL { ?routeSegment fixm:airway ?_airway .
        {
          {
            ?_airway rdf:value ?airwayValue .
            FILTER ( NOT EXISTS {?_airway (aixm:uom | fixm:uom | plain:uom) ?airwayUoM})
            BIND(concat(\'val:\',?airwayValue) AS ?airway)
          }
            UNION
          {
            ?_airway
              rdf:value ?airwayValue ;
              (aixm:uom | fixm:uom | plain:uom) ?airwayUoM .
            BIND(concat(\'xval:\',STR(?airwayValue),\':\',?airwayUoM) AS ?airway)
          }
            UNION
          {
           ?_airway  aixm:nilReason ?airwayNilReason .
           BIND(concat(\'nil:\',?airwayNilReason) AS ?airway)
          }
        }
      }
      OPTIONAL {?routeSegment fixm:routePoint ?routePoint .}
    }
  }

      '
,row(Graph,RouteSegment,Airway,RoutePoint),[]).

% aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator?, Flight*, Aircraft*, Weather*, SubCondition*)

aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator, FlightList, AircraftList, WeatherList, SubConditionList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?conditionCombination ?logicalOperator (GROUP_CONCAT(DISTINCT ?flight;SEPARATOR=",") AS ?flightConcat) (GROUP_CONCAT(DISTINCT ?aircraft;SEPARATOR=",") AS ?aircraftConcat) (GROUP_CONCAT(DISTINCT ?weather;SEPARATOR=",") AS ?weatherConcat) (GROUP_CONCAT(DISTINCT ?subCondition;SEPARATOR=",") AS ?subConditionConcat)
WHERE
  { GRAPH ?graph
    {
      ?conditionCombination rdf:type aixm:ConditionCombination .
      OPTIONAL { ?conditionCombination aixm:logicalOperator ?_logicalOperator .
        {
          {
            ?_logicalOperator rdf:value ?logicalOperatorValue .
            FILTER ( NOT EXISTS {?_logicalOperator (aixm:uom | fixm:uom | plain:uom) ?logicalOperatorUoM})
            BIND(concat(\'val:\',?logicalOperatorValue) AS ?logicalOperator)
          }
            UNION
          {
            ?_logicalOperator
              rdf:value ?logicalOperatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?logicalOperatorUoM .
            BIND(concat(\'xval:\',STR(?logicalOperatorValue),\':\',?logicalOperatorUoM) AS ?logicalOperator)
          }
            UNION
          {
           ?_logicalOperator  aixm:nilReason ?logicalOperatorNilReason .
           BIND(concat(\'nil:\',?logicalOperatorNilReason) AS ?logicalOperator)
          }
        }
      }
      OPTIONAL {?conditionCombination aixm:flight ?flight .}
      OPTIONAL {?conditionCombination aixm:aircraft ?aircraft .}
      OPTIONAL {?conditionCombination aixm:weather ?weather .}
      OPTIONAL {?conditionCombination aixm:subCondition ?subCondition .}
    }
  }
GROUP BY ?graph ?conditionCombination ?logicalOperator

      '
,row(Graph,ConditionCombination,LogicalOperator,FlightConcat,AircraftConcat,WeatherConcat,SubConditionConcat),[]), convert(FlightConcat,FlightList), convert(AircraftConcat,AircraftList), convert(WeatherConcat,WeatherList), convert(SubConditionConcat,SubConditionList).

% aixm_SurfaceContaminationLayer(Graph, SurfaceContaminationLayer, LayerOrder?, Type?, Extent*, Annotation*)

aixm_SurfaceContaminationLayer(Graph, SurfaceContaminationLayer, LayerOrder, Type, ExtentList, AnnotationList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?surfaceContaminationLayer ?layerOrder ?type (GROUP_CONCAT(DISTINCT ?extent;SEPARATOR=",") AS ?extentConcat) (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat)
WHERE
  { GRAPH ?graph
    {
      ?surfaceContaminationLayer rdf:type aixm:SurfaceContaminationLayer .
      OPTIONAL { ?surfaceContaminationLayer aixm:layerOrder ?_layerOrder .
        {
          {
            ?_layerOrder rdf:value ?layerOrderValue .
            FILTER ( NOT EXISTS {?_layerOrder (aixm:uom | fixm:uom | plain:uom) ?layerOrderUoM})
            BIND(concat(\'val:\',?layerOrderValue) AS ?layerOrder)
          }
            UNION
          {
            ?_layerOrder
              rdf:value ?layerOrderValue ;
              (aixm:uom | fixm:uom | plain:uom) ?layerOrderUoM .
            BIND(concat(\'xval:\',STR(?layerOrderValue),\':\',?layerOrderUoM) AS ?layerOrder)
          }
            UNION
          {
           ?_layerOrder  aixm:nilReason ?layerOrderNilReason .
           BIND(concat(\'nil:\',?layerOrderNilReason) AS ?layerOrder)
          }
        }
      }
      OPTIONAL { ?surfaceContaminationLayer aixm:type ?_type .
        {
          {
            ?_type rdf:value ?typeValue .
            FILTER ( NOT EXISTS {?_type (aixm:uom | fixm:uom | plain:uom) ?typeUoM})
            BIND(concat(\'val:\',?typeValue) AS ?type)
          }
            UNION
          {
            ?_type
              rdf:value ?typeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?typeUoM .
            BIND(concat(\'xval:\',STR(?typeValue),\':\',?typeUoM) AS ?type)
          }
            UNION
          {
           ?_type  aixm:nilReason ?typeNilReason .
           BIND(concat(\'nil:\',?typeNilReason) AS ?type)
          }
        }
      }
      OPTIONAL {?surfaceContaminationLayer aixm:extent ?extent .}
      OPTIONAL {?surfaceContaminationLayer aixm:annotation ?annotation .}
    }
  }
GROUP BY ?graph ?surfaceContaminationLayer ?layerOrder ?type

      '
,row(Graph,SurfaceContaminationLayer,LayerOrder,Type,ExtentConcat,AnnotationConcat),[]), convert(ExtentConcat,ExtentList), convert(AnnotationConcat,AnnotationList).

% fixm_Organization(Graph, Organization, Name?, OtherOrganization?, Contact?)

fixm_Organization(Graph, Organization, Name, OtherOrganization, Contact) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?organization ?name ?otherOrganization ?contact
WHERE
  { GRAPH ?graph
    {
      ?organization rdf:type fixm:Organization .
      OPTIONAL { ?organization fixm:name ?_name .
        {
          {
            ?_name rdf:value ?nameValue .
            FILTER ( NOT EXISTS {?_name (aixm:uom | fixm:uom | plain:uom) ?nameUoM})
            BIND(concat(\'val:\',?nameValue) AS ?name)
          }
            UNION
          {
            ?_name
              rdf:value ?nameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?nameUoM .
            BIND(concat(\'xval:\',STR(?nameValue),\':\',?nameUoM) AS ?name)
          }
            UNION
          {
           ?_name  aixm:nilReason ?nameNilReason .
           BIND(concat(\'nil:\',?nameNilReason) AS ?name)
          }
        }
      }
      OPTIONAL { ?organization fixm:otherOrganization ?_otherOrganization .
        {
          {
            ?_otherOrganization rdf:value ?otherOrganizationValue .
            FILTER ( NOT EXISTS {?_otherOrganization (aixm:uom | fixm:uom | plain:uom) ?otherOrganizationUoM})
            BIND(concat(\'val:\',?otherOrganizationValue) AS ?otherOrganization)
          }
            UNION
          {
            ?_otherOrganization
              rdf:value ?otherOrganizationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?otherOrganizationUoM .
            BIND(concat(\'xval:\',STR(?otherOrganizationValue),\':\',?otherOrganizationUoM) AS ?otherOrganization)
          }
            UNION
          {
           ?_otherOrganization  aixm:nilReason ?otherOrganizationNilReason .
           BIND(concat(\'nil:\',?otherOrganizationNilReason) AS ?otherOrganization)
          }
        }
      }
      OPTIONAL {?organization fixm:contact ?contact .}
    }
  }

      '
,row(Graph,Organization,Name,OtherOrganization,Contact),[]).

% aixm_OrganisationAuthorityAssociation(Graph, OrganisationAuthorityAssociation, Type?, Annotation*, TheOrganisationAuthority)

aixm_OrganisationAuthorityAssociation(Graph, OrganisationAuthorityAssociation, Type, AnnotationList, TheOrganisationAuthority) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?organisationAuthorityAssociation ?type (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat) ?theOrganisationAuthority
WHERE
  { GRAPH ?graph
    {
      ?organisationAuthorityAssociation rdf:type aixm:OrganisationAuthorityAssociation .
      OPTIONAL { ?organisationAuthorityAssociation aixm:type ?_type .
        {
          {
            ?_type rdf:value ?typeValue .
            FILTER ( NOT EXISTS {?_type (aixm:uom | fixm:uom | plain:uom) ?typeUoM})
            BIND(concat(\'val:\',?typeValue) AS ?type)
          }
            UNION
          {
            ?_type
              rdf:value ?typeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?typeUoM .
            BIND(concat(\'xval:\',STR(?typeValue),\':\',?typeUoM) AS ?type)
          }
            UNION
          {
           ?_type  aixm:nilReason ?typeNilReason .
           BIND(concat(\'nil:\',?typeNilReason) AS ?type)
          }
        }
      }
      OPTIONAL {?organisationAuthorityAssociation aixm:annotation ?annotation .}
      ?organisationAuthorityAssociation aixm:theOrganisationAuthority ?theOrganisationAuthority .
    }
  }
GROUP BY ?graph ?organisationAuthorityAssociation ?type ?theOrganisationAuthority

      '
,row(Graph,OrganisationAuthorityAssociation,Type,AnnotationConcat,TheOrganisationAuthority),[]), convert(AnnotationConcat,AnnotationList).

% aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)

aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?elevatedPoint ?elevation ?geoidUndulation ?verticalDatum ?verticalAccuracy
WHERE
  { GRAPH ?graph
    {
      ?elevatedPoint rdf:type aixm:ElevatedPoint .
      OPTIONAL { ?elevatedPoint aixm:elevation ?_elevation .
        {
          {
            ?_elevation rdf:value ?elevationValue .
            FILTER ( NOT EXISTS {?_elevation (aixm:uom | fixm:uom | plain:uom) ?elevationUoM})
            BIND(concat(\'val:\',?elevationValue) AS ?elevation)
          }
            UNION
          {
            ?_elevation
              rdf:value ?elevationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?elevationUoM .
            BIND(concat(\'xval:\',STR(?elevationValue),\':\',?elevationUoM) AS ?elevation)
          }
            UNION
          {
           ?_elevation  aixm:nilReason ?elevationNilReason .
           BIND(concat(\'nil:\',?elevationNilReason) AS ?elevation)
          }
        }
      }
      OPTIONAL { ?elevatedPoint aixm:geoidUndulation ?_geoidUndulation .
        {
          {
            ?_geoidUndulation rdf:value ?geoidUndulationValue .
            FILTER ( NOT EXISTS {?_geoidUndulation (aixm:uom | fixm:uom | plain:uom) ?geoidUndulationUoM})
            BIND(concat(\'val:\',?geoidUndulationValue) AS ?geoidUndulation)
          }
            UNION
          {
            ?_geoidUndulation
              rdf:value ?geoidUndulationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?geoidUndulationUoM .
            BIND(concat(\'xval:\',STR(?geoidUndulationValue),\':\',?geoidUndulationUoM) AS ?geoidUndulation)
          }
            UNION
          {
           ?_geoidUndulation  aixm:nilReason ?geoidUndulationNilReason .
           BIND(concat(\'nil:\',?geoidUndulationNilReason) AS ?geoidUndulation)
          }
        }
      }
      OPTIONAL { ?elevatedPoint aixm:verticalDatum ?_verticalDatum .
        {
          {
            ?_verticalDatum rdf:value ?verticalDatumValue .
            FILTER ( NOT EXISTS {?_verticalDatum (aixm:uom | fixm:uom | plain:uom) ?verticalDatumUoM})
            BIND(concat(\'val:\',?verticalDatumValue) AS ?verticalDatum)
          }
            UNION
          {
            ?_verticalDatum
              rdf:value ?verticalDatumValue ;
              (aixm:uom | fixm:uom | plain:uom) ?verticalDatumUoM .
            BIND(concat(\'xval:\',STR(?verticalDatumValue),\':\',?verticalDatumUoM) AS ?verticalDatum)
          }
            UNION
          {
           ?_verticalDatum  aixm:nilReason ?verticalDatumNilReason .
           BIND(concat(\'nil:\',?verticalDatumNilReason) AS ?verticalDatum)
          }
        }
      }
      OPTIONAL { ?elevatedPoint aixm:verticalAccuracy ?_verticalAccuracy .
        {
          {
            ?_verticalAccuracy rdf:value ?verticalAccuracyValue .
            FILTER ( NOT EXISTS {?_verticalAccuracy (aixm:uom | fixm:uom | plain:uom) ?verticalAccuracyUoM})
            BIND(concat(\'val:\',?verticalAccuracyValue) AS ?verticalAccuracy)
          }
            UNION
          {
            ?_verticalAccuracy
              rdf:value ?verticalAccuracyValue ;
              (aixm:uom | fixm:uom | plain:uom) ?verticalAccuracyUoM .
            BIND(concat(\'xval:\',STR(?verticalAccuracyValue),\':\',?verticalAccuracyUoM) AS ?verticalAccuracy)
          }
            UNION
          {
           ?_verticalAccuracy  aixm:nilReason ?verticalAccuracyNilReason .
           BIND(concat(\'nil:\',?verticalAccuracyNilReason) AS ?verticalAccuracy)
          }
        }
      }
    }
  }

      '
,row(Graph,ElevatedPoint,Elevation,GeoidUndulation,VerticalDatum,VerticalAccuracy),[]).

% fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel?)

fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?efplPoint4D ?flightLevel
WHERE
  { GRAPH ?graph
    {
      ?efplPoint4D rdf:type fixm:EfplPoint4D .
      OPTIONAL {?efplPoint4D fixm:flightLevel ?flightLevel .}
    }
  }

      '
,row(Graph,EfplPoint4D,FlightLevel),[]).

% fixm_AircraftOperator(Graph, AircraftOperator, OperatingOrganization?, OperatorCategory?)

fixm_AircraftOperator(Graph, AircraftOperator, OperatingOrganization, OperatorCategory) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?aircraftOperator ?operatingOrganization ?operatorCategory
WHERE
  { GRAPH ?graph
    {
      ?aircraftOperator rdf:type fixm:AircraftOperator .
      OPTIONAL { ?aircraftOperator fixm:operatingOrganization ?_operatingOrganization .
        {
          {
            ?_operatingOrganization rdf:value ?operatingOrganizationValue .
            FILTER ( NOT EXISTS {?_operatingOrganization (aixm:uom | fixm:uom | plain:uom) ?operatingOrganizationUoM})
            BIND(concat(\'val:\',?operatingOrganizationValue) AS ?operatingOrganization)
          }
            UNION
          {
            ?_operatingOrganization
              rdf:value ?operatingOrganizationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?operatingOrganizationUoM .
            BIND(concat(\'xval:\',STR(?operatingOrganizationValue),\':\',?operatingOrganizationUoM) AS ?operatingOrganization)
          }
            UNION
          {
           ?_operatingOrganization  aixm:nilReason ?operatingOrganizationNilReason .
           BIND(concat(\'nil:\',?operatingOrganizationNilReason) AS ?operatingOrganization)
          }
        }
      }
      OPTIONAL { ?aircraftOperator fixm:operatorCategory ?_operatorCategory .
        {
          {
            ?_operatorCategory rdf:value ?operatorCategoryValue .
            FILTER ( NOT EXISTS {?_operatorCategory (aixm:uom | fixm:uom | plain:uom) ?operatorCategoryUoM})
            BIND(concat(\'val:\',?operatorCategoryValue) AS ?operatorCategory)
          }
            UNION
          {
            ?_operatorCategory
              rdf:value ?operatorCategoryValue ;
              (aixm:uom | fixm:uom | plain:uom) ?operatorCategoryUoM .
            BIND(concat(\'xval:\',STR(?operatorCategoryValue),\':\',?operatorCategoryUoM) AS ?operatorCategory)
          }
            UNION
          {
           ?_operatorCategory  aixm:nilReason ?operatorCategoryNilReason .
           BIND(concat(\'nil:\',?operatorCategoryNilReason) AS ?operatorCategory)
          }
        }
      }
    }
  }

      '
,row(Graph,AircraftOperator,OperatingOrganization,OperatorCategory),[]).

% gml_Point(Graph, Point)

gml_Point(Graph, Point) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?point
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* gml:Point .
  }
  { GRAPH ?graph
    {
      ?point rdf:type ?SUBCLASS .
    }
  }
}

      '
,row(Graph,Point),[]).

% fixm_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair)

fixm_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?efplTrajectoryRoutePair
WHERE
  { GRAPH ?graph
    {
      ?efplTrajectoryRoutePair rdf:type fixm:EfplTrajectoryRoutePair .
    }
  }

      '
,row(Graph,EfplTrajectoryRoutePair),[]).

% fixm_RoutePoint(Graph, RoutePoint, Constraint*)

fixm_RoutePoint(Graph, RoutePoint, ConstraintList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?routePoint (GROUP_CONCAT(DISTINCT ?constraint;SEPARATOR=",") AS ?constraintConcat)
WHERE
  { GRAPH ?graph
    {
      ?routePoint rdf:type fixm:RoutePoint .
      OPTIONAL {?routePoint fixm:constraint ?constraint .}
    }
  }
GROUP BY ?graph ?routePoint

      '
,row(Graph,RoutePoint,ConstraintConcat),[]), convert(ConstraintConcat,ConstraintList).

% fixm_BeaconCodeAssignment(Graph, BeaconCodeAssignment, CurrentBeaconCode?, PreviousBeaconCode?, ReassignedBeaconCode?, ReassigningUnit?)

fixm_BeaconCodeAssignment(Graph, BeaconCodeAssignment, CurrentBeaconCode, PreviousBeaconCode, ReassignedBeaconCode, ReassigningUnit) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?beaconCodeAssignment ?currentBeaconCode ?previousBeaconCode ?reassignedBeaconCode ?reassigningUnit
WHERE
  { GRAPH ?graph
    {
      ?beaconCodeAssignment rdf:type fixm:BeaconCodeAssignment .
      OPTIONAL { ?beaconCodeAssignment fixm:currentBeaconCode ?_currentBeaconCode .
        {
          {
            ?_currentBeaconCode rdf:value ?currentBeaconCodeValue .
            FILTER ( NOT EXISTS {?_currentBeaconCode (aixm:uom | fixm:uom | plain:uom) ?currentBeaconCodeUoM})
            BIND(concat(\'val:\',?currentBeaconCodeValue) AS ?currentBeaconCode)
          }
            UNION
          {
            ?_currentBeaconCode
              rdf:value ?currentBeaconCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?currentBeaconCodeUoM .
            BIND(concat(\'xval:\',STR(?currentBeaconCodeValue),\':\',?currentBeaconCodeUoM) AS ?currentBeaconCode)
          }
            UNION
          {
           ?_currentBeaconCode  aixm:nilReason ?currentBeaconCodeNilReason .
           BIND(concat(\'nil:\',?currentBeaconCodeNilReason) AS ?currentBeaconCode)
          }
        }
      }
      OPTIONAL { ?beaconCodeAssignment fixm:previousBeaconCode ?_previousBeaconCode .
        {
          {
            ?_previousBeaconCode rdf:value ?previousBeaconCodeValue .
            FILTER ( NOT EXISTS {?_previousBeaconCode (aixm:uom | fixm:uom | plain:uom) ?previousBeaconCodeUoM})
            BIND(concat(\'val:\',?previousBeaconCodeValue) AS ?previousBeaconCode)
          }
            UNION
          {
            ?_previousBeaconCode
              rdf:value ?previousBeaconCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?previousBeaconCodeUoM .
            BIND(concat(\'xval:\',STR(?previousBeaconCodeValue),\':\',?previousBeaconCodeUoM) AS ?previousBeaconCode)
          }
            UNION
          {
           ?_previousBeaconCode  aixm:nilReason ?previousBeaconCodeNilReason .
           BIND(concat(\'nil:\',?previousBeaconCodeNilReason) AS ?previousBeaconCode)
          }
        }
      }
      OPTIONAL { ?beaconCodeAssignment fixm:reassignedBeaconCode ?_reassignedBeaconCode .
        {
          {
            ?_reassignedBeaconCode rdf:value ?reassignedBeaconCodeValue .
            FILTER ( NOT EXISTS {?_reassignedBeaconCode (aixm:uom | fixm:uom | plain:uom) ?reassignedBeaconCodeUoM})
            BIND(concat(\'val:\',?reassignedBeaconCodeValue) AS ?reassignedBeaconCode)
          }
            UNION
          {
            ?_reassignedBeaconCode
              rdf:value ?reassignedBeaconCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?reassignedBeaconCodeUoM .
            BIND(concat(\'xval:\',STR(?reassignedBeaconCodeValue),\':\',?reassignedBeaconCodeUoM) AS ?reassignedBeaconCode)
          }
            UNION
          {
           ?_reassignedBeaconCode  aixm:nilReason ?reassignedBeaconCodeNilReason .
           BIND(concat(\'nil:\',?reassignedBeaconCodeNilReason) AS ?reassignedBeaconCode)
          }
        }
      }
      OPTIONAL {?beaconCodeAssignment fixm:reassigningUnit ?reassigningUnit .}
    }
  }

      '
,row(Graph,BeaconCodeAssignment,CurrentBeaconCode,PreviousBeaconCode,ReassignedBeaconCode,ReassigningUnit),[]).

% fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile*, DescentProfile*)

fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfileList, DescentProfileList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?flightPerformanceData (GROUP_CONCAT(DISTINCT ?climbProfile;SEPARATOR=",") AS ?climbProfileConcat) (GROUP_CONCAT(DISTINCT ?descentProfile;SEPARATOR=",") AS ?descentProfileConcat)
WHERE
  { GRAPH ?graph
    {
      ?flightPerformanceData rdf:type fixm:FlightPerformanceData .
      OPTIONAL {?flightPerformanceData fixm:climbProfile ?climbProfile .}
      OPTIONAL {?flightPerformanceData fixm:descentProfile ?descentProfile .}
    }
  }
GROUP BY ?graph ?flightPerformanceData

      '
,row(Graph,FlightPerformanceData,ClimbProfileConcat,DescentProfileConcat),[]), convert(ClimbProfileConcat,ClimbProfileList), convert(DescentProfileConcat,DescentProfileList).

% fixm_ExpandedRoute(Graph, ExpandedRoute, RoutePoint*)

fixm_ExpandedRoute(Graph, ExpandedRoute, RoutePointList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?expandedRoute (GROUP_CONCAT(DISTINCT ?routePoint;SEPARATOR=",") AS ?routePointConcat)
WHERE
  { GRAPH ?graph
    {
      ?expandedRoute rdf:type fixm:ExpandedRoute .
      OPTIONAL {?expandedRoute fixm:routePoint ?routePoint .}
    }
  }
GROUP BY ?graph ?expandedRoute

      '
,row(Graph,ExpandedRoute,RoutePointConcat),[]), convert(RoutePointConcat,RoutePointList).

% fixm_RouteConstraintOrPreference(Graph, RouteConstraintOrPreference, ConstraintType?)

fixm_RouteConstraintOrPreference(Graph, RouteConstraintOrPreference, ConstraintType) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?routeConstraintOrPreference ?constraintType
WHERE
  { GRAPH ?graph
    {
      ?routeConstraintOrPreference rdf:type fixm:RouteConstraintOrPreference .
      OPTIONAL { ?routeConstraintOrPreference fixm:constraintType ?_constraintType .
        {
          {
            ?_constraintType rdf:value ?constraintTypeValue .
            FILTER ( NOT EXISTS {?_constraintType (aixm:uom | fixm:uom | plain:uom) ?constraintTypeUoM})
            BIND(concat(\'val:\',?constraintTypeValue) AS ?constraintType)
          }
            UNION
          {
            ?_constraintType
              rdf:value ?constraintTypeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?constraintTypeUoM .
            BIND(concat(\'xval:\',STR(?constraintTypeValue),\':\',?constraintTypeUoM) AS ?constraintType)
          }
            UNION
          {
           ?_constraintType  aixm:nilReason ?constraintTypeNilReason .
           BIND(concat(\'nil:\',?constraintTypeNilReason) AS ?constraintType)
          }
        }
      }
    }
  }

      '
,row(Graph,RouteConstraintOrPreference,ConstraintType),[]).

% fixm_DeclarationText(Graph, DeclarationText, Compliance?, Consignor?, Shipper?)

fixm_DeclarationText(Graph, DeclarationText, Compliance, Consignor, Shipper) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?declarationText ?compliance ?consignor ?shipper
WHERE
  { GRAPH ?graph
    {
      ?declarationText rdf:type fixm:DeclarationText .
      OPTIONAL { ?declarationText fixm:compliance ?_compliance .
        {
          {
            ?_compliance rdf:value ?complianceValue .
            FILTER ( NOT EXISTS {?_compliance (aixm:uom | fixm:uom | plain:uom) ?complianceUoM})
            BIND(concat(\'val:\',?complianceValue) AS ?compliance)
          }
            UNION
          {
            ?_compliance
              rdf:value ?complianceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?complianceUoM .
            BIND(concat(\'xval:\',STR(?complianceValue),\':\',?complianceUoM) AS ?compliance)
          }
            UNION
          {
           ?_compliance  aixm:nilReason ?complianceNilReason .
           BIND(concat(\'nil:\',?complianceNilReason) AS ?compliance)
          }
        }
      }
      OPTIONAL { ?declarationText fixm:consignor ?_consignor .
        {
          {
            ?_consignor rdf:value ?consignorValue .
            FILTER ( NOT EXISTS {?_consignor (aixm:uom | fixm:uom | plain:uom) ?consignorUoM})
            BIND(concat(\'val:\',?consignorValue) AS ?consignor)
          }
            UNION
          {
            ?_consignor
              rdf:value ?consignorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?consignorUoM .
            BIND(concat(\'xval:\',STR(?consignorValue),\':\',?consignorUoM) AS ?consignor)
          }
            UNION
          {
           ?_consignor  aixm:nilReason ?consignorNilReason .
           BIND(concat(\'nil:\',?consignorNilReason) AS ?consignor)
          }
        }
      }
      OPTIONAL { ?declarationText fixm:shipper ?_shipper .
        {
          {
            ?_shipper rdf:value ?shipperValue .
            FILTER ( NOT EXISTS {?_shipper (aixm:uom | fixm:uom | plain:uom) ?shipperUoM})
            BIND(concat(\'val:\',?shipperValue) AS ?shipper)
          }
            UNION
          {
            ?_shipper
              rdf:value ?shipperValue ;
              (aixm:uom | fixm:uom | plain:uom) ?shipperUoM .
            BIND(concat(\'xval:\',STR(?shipperValue),\':\',?shipperUoM) AS ?shipper)
          }
            UNION
          {
           ?_shipper  aixm:nilReason ?shipperNilReason .
           BIND(concat(\'nil:\',?shipperNilReason) AS ?shipper)
          }
        }
      }
    }
  }

      '
,row(Graph,DeclarationText,Compliance,Consignor,Shipper),[]).

% fixm_EstimatedElapsedTime(Graph, EstimatedElapsedTime, ElapsedTime?, Location?)

fixm_EstimatedElapsedTime(Graph, EstimatedElapsedTime, ElapsedTime, Location) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?estimatedElapsedTime ?elapsedTime ?location
WHERE
  { GRAPH ?graph
    {
      ?estimatedElapsedTime rdf:type fixm:EstimatedElapsedTime .
      OPTIONAL { ?estimatedElapsedTime fixm:elapsedTime ?_elapsedTime .
        {
          {
            ?_elapsedTime rdf:value ?elapsedTimeValue .
            FILTER ( NOT EXISTS {?_elapsedTime (aixm:uom | fixm:uom | plain:uom) ?elapsedTimeUoM})
            BIND(concat(\'val:\',?elapsedTimeValue) AS ?elapsedTime)
          }
            UNION
          {
            ?_elapsedTime
              rdf:value ?elapsedTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?elapsedTimeUoM .
            BIND(concat(\'xval:\',STR(?elapsedTimeValue),\':\',?elapsedTimeUoM) AS ?elapsedTime)
          }
            UNION
          {
           ?_elapsedTime  aixm:nilReason ?elapsedTimeNilReason .
           BIND(concat(\'nil:\',?elapsedTimeNilReason) AS ?elapsedTime)
          }
        }
      }
      OPTIONAL { ?estimatedElapsedTime fixm:location ?_location .
        {
          {
            ?_location rdf:value ?locationValue .
            FILTER ( NOT EXISTS {?_location (aixm:uom | fixm:uom | plain:uom) ?locationUoM})
            BIND(concat(\'val:\',?locationValue) AS ?location)
          }
            UNION
          {
            ?_location
              rdf:value ?locationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?locationUoM .
            BIND(concat(\'xval:\',STR(?locationValue),\':\',?locationUoM) AS ?location)
          }
            UNION
          {
           ?_location  aixm:nilReason ?locationNilReason .
           BIND(concat(\'nil:\',?locationNilReason) AS ?location)
          }
        }
      }
    }
  }

      '
,row(Graph,EstimatedElapsedTime,ElapsedTime,Location),[]).

% fixm_ReportedTime(Graph, ReportedTime, Provenance?, Time?)

fixm_ReportedTime(Graph, ReportedTime, Provenance, Time) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?reportedTime ?provenance ?time
WHERE
  { GRAPH ?graph
    {
      ?reportedTime rdf:type fixm:ReportedTime .
      OPTIONAL {?reportedTime fixm:provenance ?provenance .}
      OPTIONAL { ?reportedTime fixm:time ?_time .
        {
          {
            ?_time rdf:value ?timeValue .
            FILTER ( NOT EXISTS {?_time (aixm:uom | fixm:uom | plain:uom) ?timeUoM})
            BIND(concat(\'val:\',?timeValue) AS ?time)
          }
            UNION
          {
            ?_time
              rdf:value ?timeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?timeUoM .
            BIND(concat(\'xval:\',STR(?timeValue),\':\',?timeUoM) AS ?time)
          }
            UNION
          {
           ?_time  aixm:nilReason ?timeNilReason .
           BIND(concat(\'nil:\',?timeNilReason) AS ?time)
          }
        }
      }
    }
  }

      '
,row(Graph,ReportedTime,Provenance,Time),[]).

% fixm_GeographicLocation(Graph, GeographicLocation, Pos*, SrsName?)

fixm_GeographicLocation(Graph, GeographicLocation, PosList, SrsName) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?geographicLocation (GROUP_CONCAT(DISTINCT ?pos;SEPARATOR=",") AS ?posConcat) ?srsName
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:GeographicLocation .
  }
  { GRAPH ?graph
    {
      ?geographicLocation rdf:type ?SUBCLASS .
      OPTIONAL { ?geographicLocation fixm:pos ?_pos .
        {
          {
            ?_pos rdf:value ?posValue .
            FILTER ( NOT EXISTS {?_pos (aixm:uom | fixm:uom | plain:uom) ?posUoM})
            BIND(concat(\'val:\',?posValue) AS ?pos)
          }
            UNION
          {
            ?_pos
              rdf:value ?posValue ;
              (aixm:uom | fixm:uom | plain:uom) ?posUoM .
            BIND(concat(\'xval:\',STR(?posValue),\':\',?posUoM) AS ?pos)
          }
            UNION
          {
           ?_pos  aixm:nilReason ?posNilReason .
           BIND(concat(\'nil:\',?posNilReason) AS ?pos)
          }
        }
      }
      OPTIONAL { ?geographicLocation fixm:srsName ?_srsName .
        {
          {
            ?_srsName rdf:value ?srsNameValue .
            FILTER ( NOT EXISTS {?_srsName (aixm:uom | fixm:uom | plain:uom) ?srsNameUoM})
            BIND(concat(\'val:\',?srsNameValue) AS ?srsName)
          }
            UNION
          {
            ?_srsName
              rdf:value ?srsNameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?srsNameUoM .
            BIND(concat(\'xval:\',STR(?srsNameValue),\':\',?srsNameUoM) AS ?srsName)
          }
            UNION
          {
           ?_srsName  aixm:nilReason ?srsNameNilReason .
           BIND(concat(\'nil:\',?srsNameNilReason) AS ?srsName)
          }
        }
      }
    }
  }
}
GROUP BY ?graph ?geographicLocation ?srsName

      '
,row(Graph,GeographicLocation,PosConcat,SrsName),[]), convert(PosConcat,PosList).

% aixm_LinguisticNote(Graph, LinguisticNote, Note?)

aixm_LinguisticNote(Graph, LinguisticNote, Note) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?linguisticNote ?note
WHERE
  { GRAPH ?graph
    {
      ?linguisticNote rdf:type aixm:LinguisticNote .
      OPTIONAL { ?linguisticNote aixm:note ?_note .
        {
          {
            ?_note rdf:value ?noteValue .
            FILTER ( NOT EXISTS {?_note (aixm:uom | fixm:uom | plain:uom) ?noteUoM})
            BIND(concat(\'val:\',?noteValue) AS ?note)
          }
            UNION
          {
            ?_note
              rdf:value ?noteValue ;
              (aixm:uom | fixm:uom | plain:uom) ?noteUoM .
            BIND(concat(\'xval:\',STR(?noteValue),\':\',?noteUoM) AS ?note)
          }
            UNION
          {
           ?_note  aixm:nilReason ?noteNilReason .
           BIND(concat(\'nil:\',?noteNilReason) AS ?note)
          }
        }
      }
    }
  }

      '
,row(Graph,LinguisticNote,Note),[]).

% aixm_Meteorology(Graph, Meteorology, FlightConditions?, Visibility?, VisibilityInterpretation?, RunwayVisualRange?, RunwayVisualRangeInterpretation?, Annotation*)

aixm_Meteorology(Graph, Meteorology, FlightConditions, Visibility, VisibilityInterpretation, RunwayVisualRange, RunwayVisualRangeInterpretation, AnnotationList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?meteorology ?flightConditions ?visibility ?visibilityInterpretation ?runwayVisualRange ?runwayVisualRangeInterpretation (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat)
WHERE
  { GRAPH ?graph
    {
      ?meteorology rdf:type aixm:Meteorology .
      OPTIONAL { ?meteorology aixm:flightConditions ?_flightConditions .
        {
          {
            ?_flightConditions rdf:value ?flightConditionsValue .
            FILTER ( NOT EXISTS {?_flightConditions (aixm:uom | fixm:uom | plain:uom) ?flightConditionsUoM})
            BIND(concat(\'val:\',?flightConditionsValue) AS ?flightConditions)
          }
            UNION
          {
            ?_flightConditions
              rdf:value ?flightConditionsValue ;
              (aixm:uom | fixm:uom | plain:uom) ?flightConditionsUoM .
            BIND(concat(\'xval:\',STR(?flightConditionsValue),\':\',?flightConditionsUoM) AS ?flightConditions)
          }
            UNION
          {
           ?_flightConditions  aixm:nilReason ?flightConditionsNilReason .
           BIND(concat(\'nil:\',?flightConditionsNilReason) AS ?flightConditions)
          }
        }
      }
      OPTIONAL { ?meteorology aixm:visibility ?_visibility .
        {
          {
            ?_visibility rdf:value ?visibilityValue .
            FILTER ( NOT EXISTS {?_visibility (aixm:uom | fixm:uom | plain:uom) ?visibilityUoM})
            BIND(concat(\'val:\',?visibilityValue) AS ?visibility)
          }
            UNION
          {
            ?_visibility
              rdf:value ?visibilityValue ;
              (aixm:uom | fixm:uom | plain:uom) ?visibilityUoM .
            BIND(concat(\'xval:\',STR(?visibilityValue),\':\',?visibilityUoM) AS ?visibility)
          }
            UNION
          {
           ?_visibility  aixm:nilReason ?visibilityNilReason .
           BIND(concat(\'nil:\',?visibilityNilReason) AS ?visibility)
          }
        }
      }
      OPTIONAL { ?meteorology aixm:visibilityInterpretation ?_visibilityInterpretation .
        {
          {
            ?_visibilityInterpretation rdf:value ?visibilityInterpretationValue .
            FILTER ( NOT EXISTS {?_visibilityInterpretation (aixm:uom | fixm:uom | plain:uom) ?visibilityInterpretationUoM})
            BIND(concat(\'val:\',?visibilityInterpretationValue) AS ?visibilityInterpretation)
          }
            UNION
          {
            ?_visibilityInterpretation
              rdf:value ?visibilityInterpretationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?visibilityInterpretationUoM .
            BIND(concat(\'xval:\',STR(?visibilityInterpretationValue),\':\',?visibilityInterpretationUoM) AS ?visibilityInterpretation)
          }
            UNION
          {
           ?_visibilityInterpretation  aixm:nilReason ?visibilityInterpretationNilReason .
           BIND(concat(\'nil:\',?visibilityInterpretationNilReason) AS ?visibilityInterpretation)
          }
        }
      }
      OPTIONAL { ?meteorology aixm:runwayVisualRange ?_runwayVisualRange .
        {
          {
            ?_runwayVisualRange rdf:value ?runwayVisualRangeValue .
            FILTER ( NOT EXISTS {?_runwayVisualRange (aixm:uom | fixm:uom | plain:uom) ?runwayVisualRangeUoM})
            BIND(concat(\'val:\',?runwayVisualRangeValue) AS ?runwayVisualRange)
          }
            UNION
          {
            ?_runwayVisualRange
              rdf:value ?runwayVisualRangeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?runwayVisualRangeUoM .
            BIND(concat(\'xval:\',STR(?runwayVisualRangeValue),\':\',?runwayVisualRangeUoM) AS ?runwayVisualRange)
          }
            UNION
          {
           ?_runwayVisualRange  aixm:nilReason ?runwayVisualRangeNilReason .
           BIND(concat(\'nil:\',?runwayVisualRangeNilReason) AS ?runwayVisualRange)
          }
        }
      }
      OPTIONAL { ?meteorology aixm:runwayVisualRangeInterpretation ?_runwayVisualRangeInterpretation .
        {
          {
            ?_runwayVisualRangeInterpretation rdf:value ?runwayVisualRangeInterpretationValue .
            FILTER ( NOT EXISTS {?_runwayVisualRangeInterpretation (aixm:uom | fixm:uom | plain:uom) ?runwayVisualRangeInterpretationUoM})
            BIND(concat(\'val:\',?runwayVisualRangeInterpretationValue) AS ?runwayVisualRangeInterpretation)
          }
            UNION
          {
            ?_runwayVisualRangeInterpretation
              rdf:value ?runwayVisualRangeInterpretationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?runwayVisualRangeInterpretationUoM .
            BIND(concat(\'xval:\',STR(?runwayVisualRangeInterpretationValue),\':\',?runwayVisualRangeInterpretationUoM) AS ?runwayVisualRangeInterpretation)
          }
            UNION
          {
           ?_runwayVisualRangeInterpretation  aixm:nilReason ?runwayVisualRangeInterpretationNilReason .
           BIND(concat(\'nil:\',?runwayVisualRangeInterpretationNilReason) AS ?runwayVisualRangeInterpretation)
          }
        }
      }
      OPTIONAL {?meteorology aixm:annotation ?annotation .}
    }
  }
GROUP BY ?graph ?meteorology ?flightConditions ?visibility ?visibilityInterpretation ?runwayVisualRange ?runwayVisualRangeInterpretation

      '
,row(Graph,Meteorology,FlightConditions,Visibility,VisibilityInterpretation,RunwayVisualRange,RunwayVisualRangeInterpretation,AnnotationConcat),[]), convert(AnnotationConcat,AnnotationList).

% fixm_PointRange(Graph, PointRange, LateralRange?, VerticalRange?, TemporalRange?)

fixm_PointRange(Graph, PointRange, LateralRange, VerticalRange, TemporalRange) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?pointRange ?lateralRange ?verticalRange ?temporalRange
WHERE
  { GRAPH ?graph
    {
      ?pointRange rdf:type fixm:PointRange .
      OPTIONAL {?pointRange fixm:lateralRange ?lateralRange .}
      OPTIONAL {?pointRange fixm:verticalRange ?verticalRange .}
      OPTIONAL {?pointRange fixm:temporalRange ?temporalRange .}
    }
  }

      '
,row(Graph,PointRange,LateralRange,VerticalRange,TemporalRange),[]).

% aixm_City(Graph, City, Name?, Annotation*)

aixm_City(Graph, City, Name, AnnotationList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?city ?name (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat)
WHERE
  { GRAPH ?graph
    {
      ?city rdf:type aixm:City .
      OPTIONAL { ?city aixm:name ?_name .
        {
          {
            ?_name rdf:value ?nameValue .
            FILTER ( NOT EXISTS {?_name (aixm:uom | fixm:uom | plain:uom) ?nameUoM})
            BIND(concat(\'val:\',?nameValue) AS ?name)
          }
            UNION
          {
            ?_name
              rdf:value ?nameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?nameUoM .
            BIND(concat(\'xval:\',STR(?nameValue),\':\',?nameUoM) AS ?name)
          }
            UNION
          {
           ?_name  aixm:nilReason ?nameNilReason .
           BIND(concat(\'nil:\',?nameNilReason) AS ?name)
          }
        }
      }
      OPTIONAL {?city aixm:annotation ?annotation .}
    }
  }
GROUP BY ?graph ?city ?name

      '
,row(Graph,City,Name,AnnotationConcat),[]), convert(AnnotationConcat,AnnotationList).

% aixm_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role?, TheOrganisationAuthority)

aixm_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role, TheOrganisationAuthority) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?airportHeliportResponsibilityOrganisation ?role ?theOrganisationAuthority
WHERE
  { GRAPH ?graph
    {
      ?airportHeliportResponsibilityOrganisation rdf:type aixm:AirportHeliportResponsibilityOrganisation .
      OPTIONAL { ?airportHeliportResponsibilityOrganisation aixm:role ?_role .
        {
          {
            ?_role rdf:value ?roleValue .
            FILTER ( NOT EXISTS {?_role (aixm:uom | fixm:uom | plain:uom) ?roleUoM})
            BIND(concat(\'val:\',?roleValue) AS ?role)
          }
            UNION
          {
            ?_role
              rdf:value ?roleValue ;
              (aixm:uom | fixm:uom | plain:uom) ?roleUoM .
            BIND(concat(\'xval:\',STR(?roleValue),\':\',?roleUoM) AS ?role)
          }
            UNION
          {
           ?_role  aixm:nilReason ?roleNilReason .
           BIND(concat(\'nil:\',?roleNilReason) AS ?role)
          }
        }
      }
      ?airportHeliportResponsibilityOrganisation aixm:theOrganisationAuthority ?theOrganisationAuthority .
    }
  }

      '
,row(Graph,AirportHeliportResponsibilityOrganisation,Role,TheOrganisationAuthority),[]).

% fixm_AirspeedRange(Graph, AirspeedRange, LowerSpeed?, UpperSpeed?)

fixm_AirspeedRange(Graph, AirspeedRange, LowerSpeed, UpperSpeed) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?airspeedRange ?lowerSpeed ?upperSpeed
WHERE
  { GRAPH ?graph
    {
      ?airspeedRange rdf:type fixm:AirspeedRange .
      OPTIONAL { ?airspeedRange fixm:lowerSpeed ?_lowerSpeed .
        {
          {
            ?_lowerSpeed rdf:value ?lowerSpeedValue .
            FILTER ( NOT EXISTS {?_lowerSpeed (aixm:uom | fixm:uom | plain:uom) ?lowerSpeedUoM})
            BIND(concat(\'val:\',?lowerSpeedValue) AS ?lowerSpeed)
          }
            UNION
          {
            ?_lowerSpeed
              rdf:value ?lowerSpeedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?lowerSpeedUoM .
            BIND(concat(\'xval:\',STR(?lowerSpeedValue),\':\',?lowerSpeedUoM) AS ?lowerSpeed)
          }
            UNION
          {
           ?_lowerSpeed  aixm:nilReason ?lowerSpeedNilReason .
           BIND(concat(\'nil:\',?lowerSpeedNilReason) AS ?lowerSpeed)
          }
        }
      }
      OPTIONAL { ?airspeedRange fixm:upperSpeed ?_upperSpeed .
        {
          {
            ?_upperSpeed rdf:value ?upperSpeedValue .
            FILTER ( NOT EXISTS {?_upperSpeed (aixm:uom | fixm:uom | plain:uom) ?upperSpeedUoM})
            BIND(concat(\'val:\',?upperSpeedValue) AS ?upperSpeed)
          }
            UNION
          {
            ?_upperSpeed
              rdf:value ?upperSpeedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?upperSpeedUoM .
            BIND(concat(\'xval:\',STR(?upperSpeedValue),\':\',?upperSpeedUoM) AS ?upperSpeed)
          }
            UNION
          {
           ?_upperSpeed  aixm:nilReason ?upperSpeedNilReason .
           BIND(concat(\'nil:\',?upperSpeedNilReason) AS ?upperSpeed)
          }
        }
      }
    }
  }

      '
,row(Graph,AirspeedRange,LowerSpeed,UpperSpeed),[]).

% fixm_RankedTrajectory(Graph, RankedTrajectory, Identifier?, MaximumAcceptableDelay?, AssignedIndicator?, RouteTrajectoryPair?)

fixm_RankedTrajectory(Graph, RankedTrajectory, Identifier, MaximumAcceptableDelay, AssignedIndicator, RouteTrajectoryPair) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?rankedTrajectory ?identifier ?maximumAcceptableDelay ?assignedIndicator ?routeTrajectoryPair
WHERE
  { GRAPH ?graph
    {
      ?rankedTrajectory rdf:type fixm:RankedTrajectory .
      OPTIONAL { ?rankedTrajectory fixm:identifier ?_identifier .
        {
          {
            ?_identifier rdf:value ?identifierValue .
            FILTER ( NOT EXISTS {?_identifier (aixm:uom | fixm:uom | plain:uom) ?identifierUoM})
            BIND(concat(\'val:\',?identifierValue) AS ?identifier)
          }
            UNION
          {
            ?_identifier
              rdf:value ?identifierValue ;
              (aixm:uom | fixm:uom | plain:uom) ?identifierUoM .
            BIND(concat(\'xval:\',STR(?identifierValue),\':\',?identifierUoM) AS ?identifier)
          }
            UNION
          {
           ?_identifier  aixm:nilReason ?identifierNilReason .
           BIND(concat(\'nil:\',?identifierNilReason) AS ?identifier)
          }
        }
      }
      OPTIONAL { ?rankedTrajectory fixm:maximumAcceptableDelay ?_maximumAcceptableDelay .
        {
          {
            ?_maximumAcceptableDelay rdf:value ?maximumAcceptableDelayValue .
            FILTER ( NOT EXISTS {?_maximumAcceptableDelay (aixm:uom | fixm:uom | plain:uom) ?maximumAcceptableDelayUoM})
            BIND(concat(\'val:\',?maximumAcceptableDelayValue) AS ?maximumAcceptableDelay)
          }
            UNION
          {
            ?_maximumAcceptableDelay
              rdf:value ?maximumAcceptableDelayValue ;
              (aixm:uom | fixm:uom | plain:uom) ?maximumAcceptableDelayUoM .
            BIND(concat(\'xval:\',STR(?maximumAcceptableDelayValue),\':\',?maximumAcceptableDelayUoM) AS ?maximumAcceptableDelay)
          }
            UNION
          {
           ?_maximumAcceptableDelay  aixm:nilReason ?maximumAcceptableDelayNilReason .
           BIND(concat(\'nil:\',?maximumAcceptableDelayNilReason) AS ?maximumAcceptableDelay)
          }
        }
      }
      OPTIONAL { ?rankedTrajectory fixm:assignedIndicator ?_assignedIndicator .
        {
          {
            ?_assignedIndicator rdf:value ?assignedIndicatorValue .
            FILTER ( NOT EXISTS {?_assignedIndicator (aixm:uom | fixm:uom | plain:uom) ?assignedIndicatorUoM})
            BIND(concat(\'val:\',?assignedIndicatorValue) AS ?assignedIndicator)
          }
            UNION
          {
            ?_assignedIndicator
              rdf:value ?assignedIndicatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?assignedIndicatorUoM .
            BIND(concat(\'xval:\',STR(?assignedIndicatorValue),\':\',?assignedIndicatorUoM) AS ?assignedIndicator)
          }
            UNION
          {
           ?_assignedIndicator  aixm:nilReason ?assignedIndicatorNilReason .
           BIND(concat(\'nil:\',?assignedIndicatorNilReason) AS ?assignedIndicator)
          }
        }
      }
      OPTIONAL {?rankedTrajectory fixm:routeTrajectoryPair ?routeTrajectoryPair .}
    }
  }

      '
,row(Graph,RankedTrajectory,Identifier,MaximumAcceptableDelay,AssignedIndicator,RouteTrajectoryPair),[]).

% fixm_TrajectoryPointRole(Graph, TrajectoryPointRole, BottomOfClimb?, BottomOfDescent?, BoundaryPoint?, FromGATToOAT?, FromIFRToVFR?, FromOATToGat?, FromVFRToIFR?, TopOfClimb?, TopOfDescent?)

fixm_TrajectoryPointRole(Graph, TrajectoryPointRole, BottomOfClimb, BottomOfDescent, BoundaryPoint, FromGATToOAT, FromIFRToVFR, FromOATToGat, FromVFRToIFR, TopOfClimb, TopOfDescent) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?trajectoryPointRole ?bottomOfClimb ?bottomOfDescent ?boundaryPoint ?fromGATToOAT ?fromIFRToVFR ?fromOATToGat ?fromVFRToIFR ?topOfClimb ?topOfDescent
WHERE
  { GRAPH ?graph
    {
      ?trajectoryPointRole rdf:type fixm:TrajectoryPointRole .
      OPTIONAL { ?trajectoryPointRole fixm:bottomOfClimb ?_bottomOfClimb .
        {
          {
            ?_bottomOfClimb rdf:value ?bottomOfClimbValue .
            FILTER ( NOT EXISTS {?_bottomOfClimb (aixm:uom | fixm:uom | plain:uom) ?bottomOfClimbUoM})
            BIND(concat(\'val:\',?bottomOfClimbValue) AS ?bottomOfClimb)
          }
            UNION
          {
            ?_bottomOfClimb
              rdf:value ?bottomOfClimbValue ;
              (aixm:uom | fixm:uom | plain:uom) ?bottomOfClimbUoM .
            BIND(concat(\'xval:\',STR(?bottomOfClimbValue),\':\',?bottomOfClimbUoM) AS ?bottomOfClimb)
          }
            UNION
          {
           ?_bottomOfClimb  aixm:nilReason ?bottomOfClimbNilReason .
           BIND(concat(\'nil:\',?bottomOfClimbNilReason) AS ?bottomOfClimb)
          }
        }
      }
      OPTIONAL { ?trajectoryPointRole fixm:bottomOfDescent ?_bottomOfDescent .
        {
          {
            ?_bottomOfDescent rdf:value ?bottomOfDescentValue .
            FILTER ( NOT EXISTS {?_bottomOfDescent (aixm:uom | fixm:uom | plain:uom) ?bottomOfDescentUoM})
            BIND(concat(\'val:\',?bottomOfDescentValue) AS ?bottomOfDescent)
          }
            UNION
          {
            ?_bottomOfDescent
              rdf:value ?bottomOfDescentValue ;
              (aixm:uom | fixm:uom | plain:uom) ?bottomOfDescentUoM .
            BIND(concat(\'xval:\',STR(?bottomOfDescentValue),\':\',?bottomOfDescentUoM) AS ?bottomOfDescent)
          }
            UNION
          {
           ?_bottomOfDescent  aixm:nilReason ?bottomOfDescentNilReason .
           BIND(concat(\'nil:\',?bottomOfDescentNilReason) AS ?bottomOfDescent)
          }
        }
      }
      OPTIONAL { ?trajectoryPointRole fixm:boundaryPoint ?_boundaryPoint .
        {
          {
            ?_boundaryPoint rdf:value ?boundaryPointValue .
            FILTER ( NOT EXISTS {?_boundaryPoint (aixm:uom | fixm:uom | plain:uom) ?boundaryPointUoM})
            BIND(concat(\'val:\',?boundaryPointValue) AS ?boundaryPoint)
          }
            UNION
          {
            ?_boundaryPoint
              rdf:value ?boundaryPointValue ;
              (aixm:uom | fixm:uom | plain:uom) ?boundaryPointUoM .
            BIND(concat(\'xval:\',STR(?boundaryPointValue),\':\',?boundaryPointUoM) AS ?boundaryPoint)
          }
            UNION
          {
           ?_boundaryPoint  aixm:nilReason ?boundaryPointNilReason .
           BIND(concat(\'nil:\',?boundaryPointNilReason) AS ?boundaryPoint)
          }
        }
      }
      OPTIONAL { ?trajectoryPointRole fixm:fromGATToOAT ?_fromGATToOAT .
        {
          {
            ?_fromGATToOAT rdf:value ?fromGATToOATValue .
            FILTER ( NOT EXISTS {?_fromGATToOAT (aixm:uom | fixm:uom | plain:uom) ?fromGATToOATUoM})
            BIND(concat(\'val:\',?fromGATToOATValue) AS ?fromGATToOAT)
          }
            UNION
          {
            ?_fromGATToOAT
              rdf:value ?fromGATToOATValue ;
              (aixm:uom | fixm:uom | plain:uom) ?fromGATToOATUoM .
            BIND(concat(\'xval:\',STR(?fromGATToOATValue),\':\',?fromGATToOATUoM) AS ?fromGATToOAT)
          }
            UNION
          {
           ?_fromGATToOAT  aixm:nilReason ?fromGATToOATNilReason .
           BIND(concat(\'nil:\',?fromGATToOATNilReason) AS ?fromGATToOAT)
          }
        }
      }
      OPTIONAL { ?trajectoryPointRole fixm:fromIFRToVFR ?_fromIFRToVFR .
        {
          {
            ?_fromIFRToVFR rdf:value ?fromIFRToVFRValue .
            FILTER ( NOT EXISTS {?_fromIFRToVFR (aixm:uom | fixm:uom | plain:uom) ?fromIFRToVFRUoM})
            BIND(concat(\'val:\',?fromIFRToVFRValue) AS ?fromIFRToVFR)
          }
            UNION
          {
            ?_fromIFRToVFR
              rdf:value ?fromIFRToVFRValue ;
              (aixm:uom | fixm:uom | plain:uom) ?fromIFRToVFRUoM .
            BIND(concat(\'xval:\',STR(?fromIFRToVFRValue),\':\',?fromIFRToVFRUoM) AS ?fromIFRToVFR)
          }
            UNION
          {
           ?_fromIFRToVFR  aixm:nilReason ?fromIFRToVFRNilReason .
           BIND(concat(\'nil:\',?fromIFRToVFRNilReason) AS ?fromIFRToVFR)
          }
        }
      }
      OPTIONAL { ?trajectoryPointRole fixm:fromOATToGat ?_fromOATToGat .
        {
          {
            ?_fromOATToGat rdf:value ?fromOATToGatValue .
            FILTER ( NOT EXISTS {?_fromOATToGat (aixm:uom | fixm:uom | plain:uom) ?fromOATToGatUoM})
            BIND(concat(\'val:\',?fromOATToGatValue) AS ?fromOATToGat)
          }
            UNION
          {
            ?_fromOATToGat
              rdf:value ?fromOATToGatValue ;
              (aixm:uom | fixm:uom | plain:uom) ?fromOATToGatUoM .
            BIND(concat(\'xval:\',STR(?fromOATToGatValue),\':\',?fromOATToGatUoM) AS ?fromOATToGat)
          }
            UNION
          {
           ?_fromOATToGat  aixm:nilReason ?fromOATToGatNilReason .
           BIND(concat(\'nil:\',?fromOATToGatNilReason) AS ?fromOATToGat)
          }
        }
      }
      OPTIONAL { ?trajectoryPointRole fixm:fromVFRToIFR ?_fromVFRToIFR .
        {
          {
            ?_fromVFRToIFR rdf:value ?fromVFRToIFRValue .
            FILTER ( NOT EXISTS {?_fromVFRToIFR (aixm:uom | fixm:uom | plain:uom) ?fromVFRToIFRUoM})
            BIND(concat(\'val:\',?fromVFRToIFRValue) AS ?fromVFRToIFR)
          }
            UNION
          {
            ?_fromVFRToIFR
              rdf:value ?fromVFRToIFRValue ;
              (aixm:uom | fixm:uom | plain:uom) ?fromVFRToIFRUoM .
            BIND(concat(\'xval:\',STR(?fromVFRToIFRValue),\':\',?fromVFRToIFRUoM) AS ?fromVFRToIFR)
          }
            UNION
          {
           ?_fromVFRToIFR  aixm:nilReason ?fromVFRToIFRNilReason .
           BIND(concat(\'nil:\',?fromVFRToIFRNilReason) AS ?fromVFRToIFR)
          }
        }
      }
      OPTIONAL { ?trajectoryPointRole fixm:topOfClimb ?_topOfClimb .
        {
          {
            ?_topOfClimb rdf:value ?topOfClimbValue .
            FILTER ( NOT EXISTS {?_topOfClimb (aixm:uom | fixm:uom | plain:uom) ?topOfClimbUoM})
            BIND(concat(\'val:\',?topOfClimbValue) AS ?topOfClimb)
          }
            UNION
          {
            ?_topOfClimb
              rdf:value ?topOfClimbValue ;
              (aixm:uom | fixm:uom | plain:uom) ?topOfClimbUoM .
            BIND(concat(\'xval:\',STR(?topOfClimbValue),\':\',?topOfClimbUoM) AS ?topOfClimb)
          }
            UNION
          {
           ?_topOfClimb  aixm:nilReason ?topOfClimbNilReason .
           BIND(concat(\'nil:\',?topOfClimbNilReason) AS ?topOfClimb)
          }
        }
      }
      OPTIONAL { ?trajectoryPointRole fixm:topOfDescent ?_topOfDescent .
        {
          {
            ?_topOfDescent rdf:value ?topOfDescentValue .
            FILTER ( NOT EXISTS {?_topOfDescent (aixm:uom | fixm:uom | plain:uom) ?topOfDescentUoM})
            BIND(concat(\'val:\',?topOfDescentValue) AS ?topOfDescent)
          }
            UNION
          {
            ?_topOfDescent
              rdf:value ?topOfDescentValue ;
              (aixm:uom | fixm:uom | plain:uom) ?topOfDescentUoM .
            BIND(concat(\'xval:\',STR(?topOfDescentValue),\':\',?topOfDescentUoM) AS ?topOfDescent)
          }
            UNION
          {
           ?_topOfDescent  aixm:nilReason ?topOfDescentNilReason .
           BIND(concat(\'nil:\',?topOfDescentNilReason) AS ?topOfDescent)
          }
        }
      }
    }
  }

      '
,row(Graph,TrajectoryPointRole,BottomOfClimb,BottomOfDescent,BoundaryPoint,FromGATToOAT,FromIFRToVFR,FromOATToGat,FromVFRToIFR,TopOfClimb,TopOfDescent),[]).

% fixm_CommunicationCapabilities(Graph, CommunicationCapabilities, OtherCommunicationCapabilities?, OtherDataLinkCapabilities?, DataLinkCode*, SelectiveCallingCode?, CommunicationCode*)

fixm_CommunicationCapabilities(Graph, CommunicationCapabilities, OtherCommunicationCapabilities, OtherDataLinkCapabilities, DataLinkCodeList, SelectiveCallingCode, CommunicationCodeList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?communicationCapabilities ?otherCommunicationCapabilities ?otherDataLinkCapabilities (GROUP_CONCAT(DISTINCT ?dataLinkCode;SEPARATOR=",") AS ?dataLinkCodeConcat) ?selectiveCallingCode (GROUP_CONCAT(DISTINCT ?communicationCode;SEPARATOR=",") AS ?communicationCodeConcat)
WHERE
  { GRAPH ?graph
    {
      ?communicationCapabilities rdf:type fixm:CommunicationCapabilities .
      OPTIONAL { ?communicationCapabilities fixm:otherCommunicationCapabilities ?_otherCommunicationCapabilities .
        {
          {
            ?_otherCommunicationCapabilities rdf:value ?otherCommunicationCapabilitiesValue .
            FILTER ( NOT EXISTS {?_otherCommunicationCapabilities (aixm:uom | fixm:uom | plain:uom) ?otherCommunicationCapabilitiesUoM})
            BIND(concat(\'val:\',?otherCommunicationCapabilitiesValue) AS ?otherCommunicationCapabilities)
          }
            UNION
          {
            ?_otherCommunicationCapabilities
              rdf:value ?otherCommunicationCapabilitiesValue ;
              (aixm:uom | fixm:uom | plain:uom) ?otherCommunicationCapabilitiesUoM .
            BIND(concat(\'xval:\',STR(?otherCommunicationCapabilitiesValue),\':\',?otherCommunicationCapabilitiesUoM) AS ?otherCommunicationCapabilities)
          }
            UNION
          {
           ?_otherCommunicationCapabilities  aixm:nilReason ?otherCommunicationCapabilitiesNilReason .
           BIND(concat(\'nil:\',?otherCommunicationCapabilitiesNilReason) AS ?otherCommunicationCapabilities)
          }
        }
      }
      OPTIONAL { ?communicationCapabilities fixm:otherDataLinkCapabilities ?_otherDataLinkCapabilities .
        {
          {
            ?_otherDataLinkCapabilities rdf:value ?otherDataLinkCapabilitiesValue .
            FILTER ( NOT EXISTS {?_otherDataLinkCapabilities (aixm:uom | fixm:uom | plain:uom) ?otherDataLinkCapabilitiesUoM})
            BIND(concat(\'val:\',?otherDataLinkCapabilitiesValue) AS ?otherDataLinkCapabilities)
          }
            UNION
          {
            ?_otherDataLinkCapabilities
              rdf:value ?otherDataLinkCapabilitiesValue ;
              (aixm:uom | fixm:uom | plain:uom) ?otherDataLinkCapabilitiesUoM .
            BIND(concat(\'xval:\',STR(?otherDataLinkCapabilitiesValue),\':\',?otherDataLinkCapabilitiesUoM) AS ?otherDataLinkCapabilities)
          }
            UNION
          {
           ?_otherDataLinkCapabilities  aixm:nilReason ?otherDataLinkCapabilitiesNilReason .
           BIND(concat(\'nil:\',?otherDataLinkCapabilitiesNilReason) AS ?otherDataLinkCapabilities)
          }
        }
      }
      OPTIONAL { ?communicationCapabilities fixm:dataLinkCode ?_dataLinkCode .
        {
          {
            ?_dataLinkCode rdf:value ?dataLinkCodeValue .
            FILTER ( NOT EXISTS {?_dataLinkCode (aixm:uom | fixm:uom | plain:uom) ?dataLinkCodeUoM})
            BIND(concat(\'val:\',?dataLinkCodeValue) AS ?dataLinkCode)
          }
            UNION
          {
            ?_dataLinkCode
              rdf:value ?dataLinkCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?dataLinkCodeUoM .
            BIND(concat(\'xval:\',STR(?dataLinkCodeValue),\':\',?dataLinkCodeUoM) AS ?dataLinkCode)
          }
            UNION
          {
           ?_dataLinkCode  aixm:nilReason ?dataLinkCodeNilReason .
           BIND(concat(\'nil:\',?dataLinkCodeNilReason) AS ?dataLinkCode)
          }
        }
      }
      OPTIONAL { ?communicationCapabilities fixm:selectiveCallingCode ?_selectiveCallingCode .
        {
          {
            ?_selectiveCallingCode rdf:value ?selectiveCallingCodeValue .
            FILTER ( NOT EXISTS {?_selectiveCallingCode (aixm:uom | fixm:uom | plain:uom) ?selectiveCallingCodeUoM})
            BIND(concat(\'val:\',?selectiveCallingCodeValue) AS ?selectiveCallingCode)
          }
            UNION
          {
            ?_selectiveCallingCode
              rdf:value ?selectiveCallingCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?selectiveCallingCodeUoM .
            BIND(concat(\'xval:\',STR(?selectiveCallingCodeValue),\':\',?selectiveCallingCodeUoM) AS ?selectiveCallingCode)
          }
            UNION
          {
           ?_selectiveCallingCode  aixm:nilReason ?selectiveCallingCodeNilReason .
           BIND(concat(\'nil:\',?selectiveCallingCodeNilReason) AS ?selectiveCallingCode)
          }
        }
      }
      OPTIONAL { ?communicationCapabilities fixm:communicationCode ?_communicationCode .
        {
          {
            ?_communicationCode rdf:value ?communicationCodeValue .
            FILTER ( NOT EXISTS {?_communicationCode (aixm:uom | fixm:uom | plain:uom) ?communicationCodeUoM})
            BIND(concat(\'val:\',?communicationCodeValue) AS ?communicationCode)
          }
            UNION
          {
            ?_communicationCode
              rdf:value ?communicationCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?communicationCodeUoM .
            BIND(concat(\'xval:\',STR(?communicationCodeValue),\':\',?communicationCodeUoM) AS ?communicationCode)
          }
            UNION
          {
           ?_communicationCode  aixm:nilReason ?communicationCodeNilReason .
           BIND(concat(\'nil:\',?communicationCodeNilReason) AS ?communicationCode)
          }
        }
      }
    }
  }
GROUP BY ?graph ?communicationCapabilities ?otherCommunicationCapabilities ?otherDataLinkCapabilities ?selectiveCallingCode

      '
,row(Graph,CommunicationCapabilities,OtherCommunicationCapabilities,OtherDataLinkCapabilities,DataLinkCodeConcat,SelectiveCallingCode,CommunicationCodeConcat),[]), convert(DataLinkCodeConcat,DataLinkCodeList), convert(CommunicationCodeConcat,CommunicationCodeList).

% fixm_Dinghy(Graph, Dinghy, Quantity?, TotalCapacity?, Covered?, Colour?)

fixm_Dinghy(Graph, Dinghy, Quantity, TotalCapacity, Covered, Colour) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?dinghy ?quantity ?totalCapacity ?covered ?colour
WHERE
  { GRAPH ?graph
    {
      ?dinghy rdf:type fixm:Dinghy .
      OPTIONAL { ?dinghy fixm:quantity ?_quantity .
        {
          {
            ?_quantity rdf:value ?quantityValue .
            FILTER ( NOT EXISTS {?_quantity (aixm:uom | fixm:uom | plain:uom) ?quantityUoM})
            BIND(concat(\'val:\',?quantityValue) AS ?quantity)
          }
            UNION
          {
            ?_quantity
              rdf:value ?quantityValue ;
              (aixm:uom | fixm:uom | plain:uom) ?quantityUoM .
            BIND(concat(\'xval:\',STR(?quantityValue),\':\',?quantityUoM) AS ?quantity)
          }
            UNION
          {
           ?_quantity  aixm:nilReason ?quantityNilReason .
           BIND(concat(\'nil:\',?quantityNilReason) AS ?quantity)
          }
        }
      }
      OPTIONAL { ?dinghy fixm:totalCapacity ?_totalCapacity .
        {
          {
            ?_totalCapacity rdf:value ?totalCapacityValue .
            FILTER ( NOT EXISTS {?_totalCapacity (aixm:uom | fixm:uom | plain:uom) ?totalCapacityUoM})
            BIND(concat(\'val:\',?totalCapacityValue) AS ?totalCapacity)
          }
            UNION
          {
            ?_totalCapacity
              rdf:value ?totalCapacityValue ;
              (aixm:uom | fixm:uom | plain:uom) ?totalCapacityUoM .
            BIND(concat(\'xval:\',STR(?totalCapacityValue),\':\',?totalCapacityUoM) AS ?totalCapacity)
          }
            UNION
          {
           ?_totalCapacity  aixm:nilReason ?totalCapacityNilReason .
           BIND(concat(\'nil:\',?totalCapacityNilReason) AS ?totalCapacity)
          }
        }
      }
      OPTIONAL { ?dinghy fixm:covered ?_covered .
        {
          {
            ?_covered rdf:value ?coveredValue .
            FILTER ( NOT EXISTS {?_covered (aixm:uom | fixm:uom | plain:uom) ?coveredUoM})
            BIND(concat(\'val:\',?coveredValue) AS ?covered)
          }
            UNION
          {
            ?_covered
              rdf:value ?coveredValue ;
              (aixm:uom | fixm:uom | plain:uom) ?coveredUoM .
            BIND(concat(\'xval:\',STR(?coveredValue),\':\',?coveredUoM) AS ?covered)
          }
            UNION
          {
           ?_covered  aixm:nilReason ?coveredNilReason .
           BIND(concat(\'nil:\',?coveredNilReason) AS ?covered)
          }
        }
      }
      OPTIONAL { ?dinghy fixm:colour ?_colour .
        {
          {
            ?_colour rdf:value ?colourValue .
            FILTER ( NOT EXISTS {?_colour (aixm:uom | fixm:uom | plain:uom) ?colourUoM})
            BIND(concat(\'val:\',?colourValue) AS ?colour)
          }
            UNION
          {
            ?_colour
              rdf:value ?colourValue ;
              (aixm:uom | fixm:uom | plain:uom) ?colourUoM .
            BIND(concat(\'xval:\',STR(?colourValue),\':\',?colourUoM) AS ?colour)
          }
            UNION
          {
           ?_colour  aixm:nilReason ?colourNilReason .
           BIND(concat(\'nil:\',?colourNilReason) AS ?colour)
          }
        }
      }
    }
  }

      '
,row(Graph,Dinghy,Quantity,TotalCapacity,Covered,Colour),[]).

% aixm_ContactInformation(Graph, ContactInformation, Name?, Title?, Annotation*, NetworkNode*, Address*, PhoneFax*)

aixm_ContactInformation(Graph, ContactInformation, Name, Title, AnnotationList, NetworkNodeList, AddressList, PhoneFaxList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?contactInformation ?name ?title (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat) (GROUP_CONCAT(DISTINCT ?networkNode;SEPARATOR=",") AS ?networkNodeConcat) (GROUP_CONCAT(DISTINCT ?address;SEPARATOR=",") AS ?addressConcat) (GROUP_CONCAT(DISTINCT ?phoneFax;SEPARATOR=",") AS ?phoneFaxConcat)
WHERE
  { GRAPH ?graph
    {
      ?contactInformation rdf:type aixm:ContactInformation .
      OPTIONAL { ?contactInformation aixm:name ?_name .
        {
          {
            ?_name rdf:value ?nameValue .
            FILTER ( NOT EXISTS {?_name (aixm:uom | fixm:uom | plain:uom) ?nameUoM})
            BIND(concat(\'val:\',?nameValue) AS ?name)
          }
            UNION
          {
            ?_name
              rdf:value ?nameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?nameUoM .
            BIND(concat(\'xval:\',STR(?nameValue),\':\',?nameUoM) AS ?name)
          }
            UNION
          {
           ?_name  aixm:nilReason ?nameNilReason .
           BIND(concat(\'nil:\',?nameNilReason) AS ?name)
          }
        }
      }
      OPTIONAL { ?contactInformation aixm:title ?_title .
        {
          {
            ?_title rdf:value ?titleValue .
            FILTER ( NOT EXISTS {?_title (aixm:uom | fixm:uom | plain:uom) ?titleUoM})
            BIND(concat(\'val:\',?titleValue) AS ?title)
          }
            UNION
          {
            ?_title
              rdf:value ?titleValue ;
              (aixm:uom | fixm:uom | plain:uom) ?titleUoM .
            BIND(concat(\'xval:\',STR(?titleValue),\':\',?titleUoM) AS ?title)
          }
            UNION
          {
           ?_title  aixm:nilReason ?titleNilReason .
           BIND(concat(\'nil:\',?titleNilReason) AS ?title)
          }
        }
      }
      OPTIONAL {?contactInformation aixm:annotation ?annotation .}
      OPTIONAL {?contactInformation aixm:networkNode ?networkNode .}
      OPTIONAL {?contactInformation aixm:address ?address .}
      OPTIONAL {?contactInformation aixm:phoneFax ?phoneFax .}
    }
  }
GROUP BY ?graph ?contactInformation ?name ?title

      '
,row(Graph,ContactInformation,Name,Title,AnnotationConcat,NetworkNodeConcat,AddressConcat,PhoneFaxConcat),[]), convert(AnnotationConcat,AnnotationList), convert(NetworkNodeConcat,NetworkNodeList), convert(AddressConcat,AddressList), convert(PhoneFaxConcat,PhoneFaxList).

% fixm_PlannedReportingPosition(Graph, PlannedReportingPosition, Position?, PositionAltitude?, PositionEstimatedTime?)

fixm_PlannedReportingPosition(Graph, PlannedReportingPosition, Position, PositionAltitude, PositionEstimatedTime) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?plannedReportingPosition ?position ?positionAltitude ?positionEstimatedTime
WHERE
  { GRAPH ?graph
    {
      ?plannedReportingPosition rdf:type fixm:PlannedReportingPosition .
      OPTIONAL {?plannedReportingPosition fixm:position ?position .}
      OPTIONAL { ?plannedReportingPosition fixm:positionAltitude ?_positionAltitude .
        {
          {
            ?_positionAltitude rdf:value ?positionAltitudeValue .
            FILTER ( NOT EXISTS {?_positionAltitude (aixm:uom | fixm:uom | plain:uom) ?positionAltitudeUoM})
            BIND(concat(\'val:\',?positionAltitudeValue) AS ?positionAltitude)
          }
            UNION
          {
            ?_positionAltitude
              rdf:value ?positionAltitudeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?positionAltitudeUoM .
            BIND(concat(\'xval:\',STR(?positionAltitudeValue),\':\',?positionAltitudeUoM) AS ?positionAltitude)
          }
            UNION
          {
           ?_positionAltitude  aixm:nilReason ?positionAltitudeNilReason .
           BIND(concat(\'nil:\',?positionAltitudeNilReason) AS ?positionAltitude)
          }
        }
      }
      OPTIONAL { ?plannedReportingPosition fixm:positionEstimatedTime ?_positionEstimatedTime .
        {
          {
            ?_positionEstimatedTime rdf:value ?positionEstimatedTimeValue .
            FILTER ( NOT EXISTS {?_positionEstimatedTime (aixm:uom | fixm:uom | plain:uom) ?positionEstimatedTimeUoM})
            BIND(concat(\'val:\',?positionEstimatedTimeValue) AS ?positionEstimatedTime)
          }
            UNION
          {
            ?_positionEstimatedTime
              rdf:value ?positionEstimatedTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?positionEstimatedTimeUoM .
            BIND(concat(\'xval:\',STR(?positionEstimatedTimeValue),\':\',?positionEstimatedTimeUoM) AS ?positionEstimatedTime)
          }
            UNION
          {
           ?_positionEstimatedTime  aixm:nilReason ?positionEstimatedTimeNilReason .
           BIND(concat(\'nil:\',?positionEstimatedTimeNilReason) AS ?positionEstimatedTime)
          }
        }
      }
    }
  }

      '
,row(Graph,PlannedReportingPosition,Position,PositionAltitude,PositionEstimatedTime),[]).

% fixm_SignificantPoint(Graph, SignificantPoint)

fixm_SignificantPoint(Graph, SignificantPoint) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?significantPoint
WHERE
  { GRAPH ?graph
    {
      ?significantPoint rdf:type fixm:SignificantPoint .
    }
  }

      '
,row(Graph,SignificantPoint),[]).

% fixm_SupplementalData(Graph, SupplementalData, FuelEndurance?, PersonsOnBoard?, PilotInCommand?)

fixm_SupplementalData(Graph, SupplementalData, FuelEndurance, PersonsOnBoard, PilotInCommand) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?supplementalData ?fuelEndurance ?personsOnBoard ?pilotInCommand
WHERE
  { GRAPH ?graph
    {
      ?supplementalData rdf:type fixm:SupplementalData .
      OPTIONAL { ?supplementalData fixm:fuelEndurance ?_fuelEndurance .
        {
          {
            ?_fuelEndurance rdf:value ?fuelEnduranceValue .
            FILTER ( NOT EXISTS {?_fuelEndurance (aixm:uom | fixm:uom | plain:uom) ?fuelEnduranceUoM})
            BIND(concat(\'val:\',?fuelEnduranceValue) AS ?fuelEndurance)
          }
            UNION
          {
            ?_fuelEndurance
              rdf:value ?fuelEnduranceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?fuelEnduranceUoM .
            BIND(concat(\'xval:\',STR(?fuelEnduranceValue),\':\',?fuelEnduranceUoM) AS ?fuelEndurance)
          }
            UNION
          {
           ?_fuelEndurance  aixm:nilReason ?fuelEnduranceNilReason .
           BIND(concat(\'nil:\',?fuelEnduranceNilReason) AS ?fuelEndurance)
          }
        }
      }
      OPTIONAL { ?supplementalData fixm:personsOnBoard ?_personsOnBoard .
        {
          {
            ?_personsOnBoard rdf:value ?personsOnBoardValue .
            FILTER ( NOT EXISTS {?_personsOnBoard (aixm:uom | fixm:uom | plain:uom) ?personsOnBoardUoM})
            BIND(concat(\'val:\',?personsOnBoardValue) AS ?personsOnBoard)
          }
            UNION
          {
            ?_personsOnBoard
              rdf:value ?personsOnBoardValue ;
              (aixm:uom | fixm:uom | plain:uom) ?personsOnBoardUoM .
            BIND(concat(\'xval:\',STR(?personsOnBoardValue),\':\',?personsOnBoardUoM) AS ?personsOnBoard)
          }
            UNION
          {
           ?_personsOnBoard  aixm:nilReason ?personsOnBoardNilReason .
           BIND(concat(\'nil:\',?personsOnBoardNilReason) AS ?personsOnBoard)
          }
        }
      }
      OPTIONAL {?supplementalData fixm:pilotInCommand ?pilotInCommand .}
    }
  }

      '
,row(Graph,SupplementalData,FuelEndurance,PersonsOnBoard,PilotInCommand),[]).

% fixm_DangerousGoods(Graph, DangerousGoods, GuidebookNumber?, OnboardLocation?, HandlingInformation?, AircraftLimitation?, AirWayBill?, Shipment?, PackageGroup*, ShippingInformation?)

fixm_DangerousGoods(Graph, DangerousGoods, GuidebookNumber, OnboardLocation, HandlingInformation, AircraftLimitation, AirWayBill, Shipment, PackageGroupList, ShippingInformation) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?dangerousGoods ?guidebookNumber ?onboardLocation ?handlingInformation ?aircraftLimitation ?airWayBill ?shipment (GROUP_CONCAT(DISTINCT ?packageGroup;SEPARATOR=",") AS ?packageGroupConcat) ?shippingInformation
WHERE
  { GRAPH ?graph
    {
      ?dangerousGoods rdf:type fixm:DangerousGoods .
      OPTIONAL { ?dangerousGoods fixm:guidebookNumber ?_guidebookNumber .
        {
          {
            ?_guidebookNumber rdf:value ?guidebookNumberValue .
            FILTER ( NOT EXISTS {?_guidebookNumber (aixm:uom | fixm:uom | plain:uom) ?guidebookNumberUoM})
            BIND(concat(\'val:\',?guidebookNumberValue) AS ?guidebookNumber)
          }
            UNION
          {
            ?_guidebookNumber
              rdf:value ?guidebookNumberValue ;
              (aixm:uom | fixm:uom | plain:uom) ?guidebookNumberUoM .
            BIND(concat(\'xval:\',STR(?guidebookNumberValue),\':\',?guidebookNumberUoM) AS ?guidebookNumber)
          }
            UNION
          {
           ?_guidebookNumber  aixm:nilReason ?guidebookNumberNilReason .
           BIND(concat(\'nil:\',?guidebookNumberNilReason) AS ?guidebookNumber)
          }
        }
      }
      OPTIONAL { ?dangerousGoods fixm:onboardLocation ?_onboardLocation .
        {
          {
            ?_onboardLocation rdf:value ?onboardLocationValue .
            FILTER ( NOT EXISTS {?_onboardLocation (aixm:uom | fixm:uom | plain:uom) ?onboardLocationUoM})
            BIND(concat(\'val:\',?onboardLocationValue) AS ?onboardLocation)
          }
            UNION
          {
            ?_onboardLocation
              rdf:value ?onboardLocationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?onboardLocationUoM .
            BIND(concat(\'xval:\',STR(?onboardLocationValue),\':\',?onboardLocationUoM) AS ?onboardLocation)
          }
            UNION
          {
           ?_onboardLocation  aixm:nilReason ?onboardLocationNilReason .
           BIND(concat(\'nil:\',?onboardLocationNilReason) AS ?onboardLocation)
          }
        }
      }
      OPTIONAL {?dangerousGoods fixm:handlingInformation ?handlingInformation .}
      OPTIONAL { ?dangerousGoods fixm:aircraftLimitation ?_aircraftLimitation .
        {
          {
            ?_aircraftLimitation rdf:value ?aircraftLimitationValue .
            FILTER ( NOT EXISTS {?_aircraftLimitation (aixm:uom | fixm:uom | plain:uom) ?aircraftLimitationUoM})
            BIND(concat(\'val:\',?aircraftLimitationValue) AS ?aircraftLimitation)
          }
            UNION
          {
            ?_aircraftLimitation
              rdf:value ?aircraftLimitationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?aircraftLimitationUoM .
            BIND(concat(\'xval:\',STR(?aircraftLimitationValue),\':\',?aircraftLimitationUoM) AS ?aircraftLimitation)
          }
            UNION
          {
           ?_aircraftLimitation  aixm:nilReason ?aircraftLimitationNilReason .
           BIND(concat(\'nil:\',?aircraftLimitationNilReason) AS ?aircraftLimitation)
          }
        }
      }
      OPTIONAL { ?dangerousGoods fixm:airWayBill ?_airWayBill .
        {
          {
            ?_airWayBill rdf:value ?airWayBillValue .
            FILTER ( NOT EXISTS {?_airWayBill (aixm:uom | fixm:uom | plain:uom) ?airWayBillUoM})
            BIND(concat(\'val:\',?airWayBillValue) AS ?airWayBill)
          }
            UNION
          {
            ?_airWayBill
              rdf:value ?airWayBillValue ;
              (aixm:uom | fixm:uom | plain:uom) ?airWayBillUoM .
            BIND(concat(\'xval:\',STR(?airWayBillValue),\':\',?airWayBillUoM) AS ?airWayBill)
          }
            UNION
          {
           ?_airWayBill  aixm:nilReason ?airWayBillNilReason .
           BIND(concat(\'nil:\',?airWayBillNilReason) AS ?airWayBill)
          }
        }
      }
      OPTIONAL { ?dangerousGoods fixm:shipment ?_shipment .
        {
          {
            ?_shipment rdf:value ?shipmentValue .
            FILTER ( NOT EXISTS {?_shipment (aixm:uom | fixm:uom | plain:uom) ?shipmentUoM})
            BIND(concat(\'val:\',?shipmentValue) AS ?shipment)
          }
            UNION
          {
            ?_shipment
              rdf:value ?shipmentValue ;
              (aixm:uom | fixm:uom | plain:uom) ?shipmentUoM .
            BIND(concat(\'xval:\',STR(?shipmentValue),\':\',?shipmentUoM) AS ?shipment)
          }
            UNION
          {
           ?_shipment  aixm:nilReason ?shipmentNilReason .
           BIND(concat(\'nil:\',?shipmentNilReason) AS ?shipment)
          }
        }
      }
      OPTIONAL {?dangerousGoods fixm:packageGroup ?packageGroup .}
      OPTIONAL {?dangerousGoods fixm:shippingInformation ?shippingInformation .}
    }
  }
GROUP BY ?graph ?dangerousGoods ?guidebookNumber ?onboardLocation ?handlingInformation ?aircraftLimitation ?airWayBill ?shipment ?shippingInformation

      '
,row(Graph,DangerousGoods,GuidebookNumber,OnboardLocation,HandlingInformation,AircraftLimitation,AirWayBill,Shipment,PackageGroupConcat,ShippingInformation),[]), convert(PackageGroupConcat,PackageGroupList).

% fixm_DangerousGoodsPackageGroup(Graph, DangerousGoodsPackageGroup, ShipmentDimensions?, DangerousGoodsPackage*, ShipmentUseIndicator?)

fixm_DangerousGoodsPackageGroup(Graph, DangerousGoodsPackageGroup, ShipmentDimensions, DangerousGoodsPackageList, ShipmentUseIndicator) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?dangerousGoodsPackageGroup ?shipmentDimensions (GROUP_CONCAT(DISTINCT ?dangerousGoodsPackage;SEPARATOR=",") AS ?dangerousGoodsPackageConcat) ?shipmentUseIndicator
WHERE
  { GRAPH ?graph
    {
      ?dangerousGoodsPackageGroup rdf:type fixm:DangerousGoodsPackageGroup .
      OPTIONAL {?dangerousGoodsPackageGroup fixm:shipmentDimensions ?shipmentDimensions .}
      OPTIONAL {?dangerousGoodsPackageGroup fixm:dangerousGoodsPackage ?dangerousGoodsPackage .}
      OPTIONAL { ?dangerousGoodsPackageGroup fixm:shipmentUseIndicator ?_shipmentUseIndicator .
        {
          {
            ?_shipmentUseIndicator rdf:value ?shipmentUseIndicatorValue .
            FILTER ( NOT EXISTS {?_shipmentUseIndicator (aixm:uom | fixm:uom | plain:uom) ?shipmentUseIndicatorUoM})
            BIND(concat(\'val:\',?shipmentUseIndicatorValue) AS ?shipmentUseIndicator)
          }
            UNION
          {
            ?_shipmentUseIndicator
              rdf:value ?shipmentUseIndicatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?shipmentUseIndicatorUoM .
            BIND(concat(\'xval:\',STR(?shipmentUseIndicatorValue),\':\',?shipmentUseIndicatorUoM) AS ?shipmentUseIndicator)
          }
            UNION
          {
           ?_shipmentUseIndicator  aixm:nilReason ?shipmentUseIndicatorNilReason .
           BIND(concat(\'nil:\',?shipmentUseIndicatorNilReason) AS ?shipmentUseIndicator)
          }
        }
      }
    }
  }
GROUP BY ?graph ?dangerousGoodsPackageGroup ?shipmentDimensions ?shipmentUseIndicator

      '
,row(Graph,DangerousGoodsPackageGroup,ShipmentDimensions,DangerousGoodsPackageConcat,ShipmentUseIndicator),[]), convert(DangerousGoodsPackageConcat,DangerousGoodsPackageList).

% fixm_OfftrackDistance(Graph, OfftrackDistance, Distance?, Direction?)

fixm_OfftrackDistance(Graph, OfftrackDistance, Distance, Direction) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?offtrackDistance ?distance ?direction
WHERE
  { GRAPH ?graph
    {
      ?offtrackDistance rdf:type fixm:OfftrackDistance .
      OPTIONAL { ?offtrackDistance fixm:distance ?_distance .
        {
          {
            ?_distance rdf:value ?distanceValue .
            FILTER ( NOT EXISTS {?_distance (aixm:uom | fixm:uom | plain:uom) ?distanceUoM})
            BIND(concat(\'val:\',?distanceValue) AS ?distance)
          }
            UNION
          {
            ?_distance
              rdf:value ?distanceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?distanceUoM .
            BIND(concat(\'xval:\',STR(?distanceValue),\':\',?distanceUoM) AS ?distance)
          }
            UNION
          {
           ?_distance  aixm:nilReason ?distanceNilReason .
           BIND(concat(\'nil:\',?distanceNilReason) AS ?distance)
          }
        }
      }
      OPTIONAL { ?offtrackDistance fixm:direction ?_direction .
        {
          {
            ?_direction rdf:value ?directionValue .
            FILTER ( NOT EXISTS {?_direction (aixm:uom | fixm:uom | plain:uom) ?directionUoM})
            BIND(concat(\'val:\',?directionValue) AS ?direction)
          }
            UNION
          {
            ?_direction
              rdf:value ?directionValue ;
              (aixm:uom | fixm:uom | plain:uom) ?directionUoM .
            BIND(concat(\'xval:\',STR(?directionValue),\':\',?directionUoM) AS ?direction)
          }
            UNION
          {
           ?_direction  aixm:nilReason ?directionNilReason .
           BIND(concat(\'nil:\',?directionNilReason) AS ?direction)
          }
        }
      }
    }
  }

      '
,row(Graph,OfftrackDistance,Distance,Direction),[]).

% fixm_Handoff(Graph, Handoff, ReceivingUnit?, TransferringUnit?, CoordinationStatus?)

fixm_Handoff(Graph, Handoff, ReceivingUnit, TransferringUnit, CoordinationStatus) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?handoff ?receivingUnit ?transferringUnit ?coordinationStatus
WHERE
  { GRAPH ?graph
    {
      ?handoff rdf:type fixm:Handoff .
      OPTIONAL {?handoff fixm:receivingUnit ?receivingUnit .}
      OPTIONAL {?handoff fixm:transferringUnit ?transferringUnit .}
      OPTIONAL {?handoff fixm:coordinationStatus ?coordinationStatus .}
    }
  }

      '
,row(Graph,Handoff,ReceivingUnit,TransferringUnit,CoordinationStatus),[]).

% fixm_TrajectoryChange(Graph, TrajectoryChange, ConstrainedAirspace?, SpecialActivityAirspace?)

fixm_TrajectoryChange(Graph, TrajectoryChange, ConstrainedAirspace, SpecialActivityAirspace) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?trajectoryChange ?constrainedAirspace ?specialActivityAirspace
WHERE
  { GRAPH ?graph
    {
      ?trajectoryChange rdf:type fixm:TrajectoryChange .
      OPTIONAL { ?trajectoryChange fixm:constrainedAirspace ?_constrainedAirspace .
        {
          {
            ?_constrainedAirspace rdf:value ?constrainedAirspaceValue .
            FILTER ( NOT EXISTS {?_constrainedAirspace (aixm:uom | fixm:uom | plain:uom) ?constrainedAirspaceUoM})
            BIND(concat(\'val:\',?constrainedAirspaceValue) AS ?constrainedAirspace)
          }
            UNION
          {
            ?_constrainedAirspace
              rdf:value ?constrainedAirspaceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?constrainedAirspaceUoM .
            BIND(concat(\'xval:\',STR(?constrainedAirspaceValue),\':\',?constrainedAirspaceUoM) AS ?constrainedAirspace)
          }
            UNION
          {
           ?_constrainedAirspace  aixm:nilReason ?constrainedAirspaceNilReason .
           BIND(concat(\'nil:\',?constrainedAirspaceNilReason) AS ?constrainedAirspace)
          }
        }
      }
      OPTIONAL { ?trajectoryChange fixm:specialActivityAirspace ?_specialActivityAirspace .
        {
          {
            ?_specialActivityAirspace rdf:value ?specialActivityAirspaceValue .
            FILTER ( NOT EXISTS {?_specialActivityAirspace (aixm:uom | fixm:uom | plain:uom) ?specialActivityAirspaceUoM})
            BIND(concat(\'val:\',?specialActivityAirspaceValue) AS ?specialActivityAirspace)
          }
            UNION
          {
            ?_specialActivityAirspace
              rdf:value ?specialActivityAirspaceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?specialActivityAirspaceUoM .
            BIND(concat(\'xval:\',STR(?specialActivityAirspaceValue),\':\',?specialActivityAirspaceUoM) AS ?specialActivityAirspace)
          }
            UNION
          {
           ?_specialActivityAirspace  aixm:nilReason ?specialActivityAirspaceNilReason .
           BIND(concat(\'nil:\',?specialActivityAirspaceNilReason) AS ?specialActivityAirspace)
          }
        }
      }
    }
  }

      '
,row(Graph,TrajectoryChange,ConstrainedAirspace,SpecialActivityAirspace),[]).

% fixm_ContactInformation(Graph, ContactInformation, Name?, Title?, OnlineContact?, PhoneFax?, Address?)

fixm_ContactInformation(Graph, ContactInformation, Name, Title, OnlineContact, PhoneFax, Address) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?contactInformation ?name ?title ?onlineContact ?phoneFax ?address
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:ContactInformation .
  }
  { GRAPH ?graph
    {
      ?contactInformation rdf:type ?SUBCLASS .
      OPTIONAL { ?contactInformation fixm:name ?_name .
        {
          {
            ?_name rdf:value ?nameValue .
            FILTER ( NOT EXISTS {?_name (aixm:uom | fixm:uom | plain:uom) ?nameUoM})
            BIND(concat(\'val:\',?nameValue) AS ?name)
          }
            UNION
          {
            ?_name
              rdf:value ?nameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?nameUoM .
            BIND(concat(\'xval:\',STR(?nameValue),\':\',?nameUoM) AS ?name)
          }
            UNION
          {
           ?_name  aixm:nilReason ?nameNilReason .
           BIND(concat(\'nil:\',?nameNilReason) AS ?name)
          }
        }
      }
      OPTIONAL { ?contactInformation fixm:title ?_title .
        {
          {
            ?_title rdf:value ?titleValue .
            FILTER ( NOT EXISTS {?_title (aixm:uom | fixm:uom | plain:uom) ?titleUoM})
            BIND(concat(\'val:\',?titleValue) AS ?title)
          }
            UNION
          {
            ?_title
              rdf:value ?titleValue ;
              (aixm:uom | fixm:uom | plain:uom) ?titleUoM .
            BIND(concat(\'xval:\',STR(?titleValue),\':\',?titleUoM) AS ?title)
          }
            UNION
          {
           ?_title  aixm:nilReason ?titleNilReason .
           BIND(concat(\'nil:\',?titleNilReason) AS ?title)
          }
        }
      }
      OPTIONAL {?contactInformation fixm:onlineContact ?onlineContact .}
      OPTIONAL {?contactInformation fixm:phoneFax ?phoneFax .}
      OPTIONAL {?contactInformation fixm:address ?address .}
    }
  }
}

      '
,row(Graph,ContactInformation,Name,Title,OnlineContact,PhoneFax,Address),[]).

% aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator?, Name?, LocationIndicatorICAO?, DesignatorIATA?, Type?, CertifiedICAO?, PrivateUse?, ControlType?, FieldElevation?, FieldElevationAccuracy?, VerticalDatum?, MagneticVariation?, MagneticVariationAccuracy?, DateMagneticVariation?, MagneticVariationChange?, ReferenceTemperature?, AltimeterCheckLocation?, SecondaryPowerSupply?, WindDirectionIndicator?, LandingDirectionIndicator?, TransitionAltitude?, TransitionLevel?, LowestTemperature?, Abandoned?, CertificationDate?, CertificationExpirationDate?, Contact*, Annotation*, ARP?, AltimeterSource*, Contaminant*, ServedCity*, ResponsibleOrganisation?, AviationBoundary?, Availability*)

aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, ContactList, AnnotationList, ARP, AltimeterSourceList, ContaminantList, ServedCityList, ResponsibleOrganisation, AviationBoundary, AvailabilityList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?airportHeliportTimeSlice ?designator ?name ?locationIndicatorICAO ?designatorIATA ?type ?certifiedICAO ?privateUse ?controlType ?fieldElevation ?fieldElevationAccuracy ?verticalDatum ?magneticVariation ?magneticVariationAccuracy ?dateMagneticVariation ?magneticVariationChange ?referenceTemperature ?altimeterCheckLocation ?secondaryPowerSupply ?windDirectionIndicator ?landingDirectionIndicator ?transitionAltitude ?transitionLevel ?lowestTemperature ?abandoned ?certificationDate ?certificationExpirationDate (GROUP_CONCAT(DISTINCT ?contact;SEPARATOR=",") AS ?contactConcat) (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat) ?ARP (GROUP_CONCAT(DISTINCT ?altimeterSource;SEPARATOR=",") AS ?altimeterSourceConcat) (GROUP_CONCAT(DISTINCT ?contaminant;SEPARATOR=",") AS ?contaminantConcat) (GROUP_CONCAT(DISTINCT ?servedCity;SEPARATOR=",") AS ?servedCityConcat) ?responsibleOrganisation ?aviationBoundary (GROUP_CONCAT(DISTINCT ?availability;SEPARATOR=",") AS ?availabilityConcat)
WHERE
  { GRAPH ?graph
    {
      ?airportHeliportTimeSlice rdf:type aixm:AirportHeliportTimeSlice .
      OPTIONAL { ?airportHeliportTimeSlice aixm:designator ?_designator .
        {
          {
            ?_designator rdf:value ?designatorValue .
            FILTER ( NOT EXISTS {?_designator (aixm:uom | fixm:uom | plain:uom) ?designatorUoM})
            BIND(concat(\'val:\',?designatorValue) AS ?designator)
          }
            UNION
          {
            ?_designator
              rdf:value ?designatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?designatorUoM .
            BIND(concat(\'xval:\',STR(?designatorValue),\':\',?designatorUoM) AS ?designator)
          }
            UNION
          {
           ?_designator  aixm:nilReason ?designatorNilReason .
           BIND(concat(\'nil:\',?designatorNilReason) AS ?designator)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:name ?_name .
        {
          {
            ?_name rdf:value ?nameValue .
            FILTER ( NOT EXISTS {?_name (aixm:uom | fixm:uom | plain:uom) ?nameUoM})
            BIND(concat(\'val:\',?nameValue) AS ?name)
          }
            UNION
          {
            ?_name
              rdf:value ?nameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?nameUoM .
            BIND(concat(\'xval:\',STR(?nameValue),\':\',?nameUoM) AS ?name)
          }
            UNION
          {
           ?_name  aixm:nilReason ?nameNilReason .
           BIND(concat(\'nil:\',?nameNilReason) AS ?name)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:locationIndicatorICAO ?_locationIndicatorICAO .
        {
          {
            ?_locationIndicatorICAO rdf:value ?locationIndicatorICAOValue .
            FILTER ( NOT EXISTS {?_locationIndicatorICAO (aixm:uom | fixm:uom | plain:uom) ?locationIndicatorICAOUoM})
            BIND(concat(\'val:\',?locationIndicatorICAOValue) AS ?locationIndicatorICAO)
          }
            UNION
          {
            ?_locationIndicatorICAO
              rdf:value ?locationIndicatorICAOValue ;
              (aixm:uom | fixm:uom | plain:uom) ?locationIndicatorICAOUoM .
            BIND(concat(\'xval:\',STR(?locationIndicatorICAOValue),\':\',?locationIndicatorICAOUoM) AS ?locationIndicatorICAO)
          }
            UNION
          {
           ?_locationIndicatorICAO  aixm:nilReason ?locationIndicatorICAONilReason .
           BIND(concat(\'nil:\',?locationIndicatorICAONilReason) AS ?locationIndicatorICAO)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:designatorIATA ?_designatorIATA .
        {
          {
            ?_designatorIATA rdf:value ?designatorIATAValue .
            FILTER ( NOT EXISTS {?_designatorIATA (aixm:uom | fixm:uom | plain:uom) ?designatorIATAUoM})
            BIND(concat(\'val:\',?designatorIATAValue) AS ?designatorIATA)
          }
            UNION
          {
            ?_designatorIATA
              rdf:value ?designatorIATAValue ;
              (aixm:uom | fixm:uom | plain:uom) ?designatorIATAUoM .
            BIND(concat(\'xval:\',STR(?designatorIATAValue),\':\',?designatorIATAUoM) AS ?designatorIATA)
          }
            UNION
          {
           ?_designatorIATA  aixm:nilReason ?designatorIATANilReason .
           BIND(concat(\'nil:\',?designatorIATANilReason) AS ?designatorIATA)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:type ?_type .
        {
          {
            ?_type rdf:value ?typeValue .
            FILTER ( NOT EXISTS {?_type (aixm:uom | fixm:uom | plain:uom) ?typeUoM})
            BIND(concat(\'val:\',?typeValue) AS ?type)
          }
            UNION
          {
            ?_type
              rdf:value ?typeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?typeUoM .
            BIND(concat(\'xval:\',STR(?typeValue),\':\',?typeUoM) AS ?type)
          }
            UNION
          {
           ?_type  aixm:nilReason ?typeNilReason .
           BIND(concat(\'nil:\',?typeNilReason) AS ?type)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:certifiedICAO ?_certifiedICAO .
        {
          {
            ?_certifiedICAO rdf:value ?certifiedICAOValue .
            FILTER ( NOT EXISTS {?_certifiedICAO (aixm:uom | fixm:uom | plain:uom) ?certifiedICAOUoM})
            BIND(concat(\'val:\',?certifiedICAOValue) AS ?certifiedICAO)
          }
            UNION
          {
            ?_certifiedICAO
              rdf:value ?certifiedICAOValue ;
              (aixm:uom | fixm:uom | plain:uom) ?certifiedICAOUoM .
            BIND(concat(\'xval:\',STR(?certifiedICAOValue),\':\',?certifiedICAOUoM) AS ?certifiedICAO)
          }
            UNION
          {
           ?_certifiedICAO  aixm:nilReason ?certifiedICAONilReason .
           BIND(concat(\'nil:\',?certifiedICAONilReason) AS ?certifiedICAO)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:privateUse ?_privateUse .
        {
          {
            ?_privateUse rdf:value ?privateUseValue .
            FILTER ( NOT EXISTS {?_privateUse (aixm:uom | fixm:uom | plain:uom) ?privateUseUoM})
            BIND(concat(\'val:\',?privateUseValue) AS ?privateUse)
          }
            UNION
          {
            ?_privateUse
              rdf:value ?privateUseValue ;
              (aixm:uom | fixm:uom | plain:uom) ?privateUseUoM .
            BIND(concat(\'xval:\',STR(?privateUseValue),\':\',?privateUseUoM) AS ?privateUse)
          }
            UNION
          {
           ?_privateUse  aixm:nilReason ?privateUseNilReason .
           BIND(concat(\'nil:\',?privateUseNilReason) AS ?privateUse)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:controlType ?_controlType .
        {
          {
            ?_controlType rdf:value ?controlTypeValue .
            FILTER ( NOT EXISTS {?_controlType (aixm:uom | fixm:uom | plain:uom) ?controlTypeUoM})
            BIND(concat(\'val:\',?controlTypeValue) AS ?controlType)
          }
            UNION
          {
            ?_controlType
              rdf:value ?controlTypeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?controlTypeUoM .
            BIND(concat(\'xval:\',STR(?controlTypeValue),\':\',?controlTypeUoM) AS ?controlType)
          }
            UNION
          {
           ?_controlType  aixm:nilReason ?controlTypeNilReason .
           BIND(concat(\'nil:\',?controlTypeNilReason) AS ?controlType)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:fieldElevation ?_fieldElevation .
        {
          {
            ?_fieldElevation rdf:value ?fieldElevationValue .
            FILTER ( NOT EXISTS {?_fieldElevation (aixm:uom | fixm:uom | plain:uom) ?fieldElevationUoM})
            BIND(concat(\'val:\',?fieldElevationValue) AS ?fieldElevation)
          }
            UNION
          {
            ?_fieldElevation
              rdf:value ?fieldElevationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?fieldElevationUoM .
            BIND(concat(\'xval:\',STR(?fieldElevationValue),\':\',?fieldElevationUoM) AS ?fieldElevation)
          }
            UNION
          {
           ?_fieldElevation  aixm:nilReason ?fieldElevationNilReason .
           BIND(concat(\'nil:\',?fieldElevationNilReason) AS ?fieldElevation)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:fieldElevationAccuracy ?_fieldElevationAccuracy .
        {
          {
            ?_fieldElevationAccuracy rdf:value ?fieldElevationAccuracyValue .
            FILTER ( NOT EXISTS {?_fieldElevationAccuracy (aixm:uom | fixm:uom | plain:uom) ?fieldElevationAccuracyUoM})
            BIND(concat(\'val:\',?fieldElevationAccuracyValue) AS ?fieldElevationAccuracy)
          }
            UNION
          {
            ?_fieldElevationAccuracy
              rdf:value ?fieldElevationAccuracyValue ;
              (aixm:uom | fixm:uom | plain:uom) ?fieldElevationAccuracyUoM .
            BIND(concat(\'xval:\',STR(?fieldElevationAccuracyValue),\':\',?fieldElevationAccuracyUoM) AS ?fieldElevationAccuracy)
          }
            UNION
          {
           ?_fieldElevationAccuracy  aixm:nilReason ?fieldElevationAccuracyNilReason .
           BIND(concat(\'nil:\',?fieldElevationAccuracyNilReason) AS ?fieldElevationAccuracy)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:verticalDatum ?_verticalDatum .
        {
          {
            ?_verticalDatum rdf:value ?verticalDatumValue .
            FILTER ( NOT EXISTS {?_verticalDatum (aixm:uom | fixm:uom | plain:uom) ?verticalDatumUoM})
            BIND(concat(\'val:\',?verticalDatumValue) AS ?verticalDatum)
          }
            UNION
          {
            ?_verticalDatum
              rdf:value ?verticalDatumValue ;
              (aixm:uom | fixm:uom | plain:uom) ?verticalDatumUoM .
            BIND(concat(\'xval:\',STR(?verticalDatumValue),\':\',?verticalDatumUoM) AS ?verticalDatum)
          }
            UNION
          {
           ?_verticalDatum  aixm:nilReason ?verticalDatumNilReason .
           BIND(concat(\'nil:\',?verticalDatumNilReason) AS ?verticalDatum)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:magneticVariation ?_magneticVariation .
        {
          {
            ?_magneticVariation rdf:value ?magneticVariationValue .
            FILTER ( NOT EXISTS {?_magneticVariation (aixm:uom | fixm:uom | plain:uom) ?magneticVariationUoM})
            BIND(concat(\'val:\',?magneticVariationValue) AS ?magneticVariation)
          }
            UNION
          {
            ?_magneticVariation
              rdf:value ?magneticVariationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?magneticVariationUoM .
            BIND(concat(\'xval:\',STR(?magneticVariationValue),\':\',?magneticVariationUoM) AS ?magneticVariation)
          }
            UNION
          {
           ?_magneticVariation  aixm:nilReason ?magneticVariationNilReason .
           BIND(concat(\'nil:\',?magneticVariationNilReason) AS ?magneticVariation)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:magneticVariationAccuracy ?_magneticVariationAccuracy .
        {
          {
            ?_magneticVariationAccuracy rdf:value ?magneticVariationAccuracyValue .
            FILTER ( NOT EXISTS {?_magneticVariationAccuracy (aixm:uom | fixm:uom | plain:uom) ?magneticVariationAccuracyUoM})
            BIND(concat(\'val:\',?magneticVariationAccuracyValue) AS ?magneticVariationAccuracy)
          }
            UNION
          {
            ?_magneticVariationAccuracy
              rdf:value ?magneticVariationAccuracyValue ;
              (aixm:uom | fixm:uom | plain:uom) ?magneticVariationAccuracyUoM .
            BIND(concat(\'xval:\',STR(?magneticVariationAccuracyValue),\':\',?magneticVariationAccuracyUoM) AS ?magneticVariationAccuracy)
          }
            UNION
          {
           ?_magneticVariationAccuracy  aixm:nilReason ?magneticVariationAccuracyNilReason .
           BIND(concat(\'nil:\',?magneticVariationAccuracyNilReason) AS ?magneticVariationAccuracy)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:dateMagneticVariation ?_dateMagneticVariation .
        {
          {
            ?_dateMagneticVariation rdf:value ?dateMagneticVariationValue .
            FILTER ( NOT EXISTS {?_dateMagneticVariation (aixm:uom | fixm:uom | plain:uom) ?dateMagneticVariationUoM})
            BIND(concat(\'val:\',?dateMagneticVariationValue) AS ?dateMagneticVariation)
          }
            UNION
          {
            ?_dateMagneticVariation
              rdf:value ?dateMagneticVariationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?dateMagneticVariationUoM .
            BIND(concat(\'xval:\',STR(?dateMagneticVariationValue),\':\',?dateMagneticVariationUoM) AS ?dateMagneticVariation)
          }
            UNION
          {
           ?_dateMagneticVariation  aixm:nilReason ?dateMagneticVariationNilReason .
           BIND(concat(\'nil:\',?dateMagneticVariationNilReason) AS ?dateMagneticVariation)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:magneticVariationChange ?_magneticVariationChange .
        {
          {
            ?_magneticVariationChange rdf:value ?magneticVariationChangeValue .
            FILTER ( NOT EXISTS {?_magneticVariationChange (aixm:uom | fixm:uom | plain:uom) ?magneticVariationChangeUoM})
            BIND(concat(\'val:\',?magneticVariationChangeValue) AS ?magneticVariationChange)
          }
            UNION
          {
            ?_magneticVariationChange
              rdf:value ?magneticVariationChangeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?magneticVariationChangeUoM .
            BIND(concat(\'xval:\',STR(?magneticVariationChangeValue),\':\',?magneticVariationChangeUoM) AS ?magneticVariationChange)
          }
            UNION
          {
           ?_magneticVariationChange  aixm:nilReason ?magneticVariationChangeNilReason .
           BIND(concat(\'nil:\',?magneticVariationChangeNilReason) AS ?magneticVariationChange)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:referenceTemperature ?_referenceTemperature .
        {
          {
            ?_referenceTemperature rdf:value ?referenceTemperatureValue .
            FILTER ( NOT EXISTS {?_referenceTemperature (aixm:uom | fixm:uom | plain:uom) ?referenceTemperatureUoM})
            BIND(concat(\'val:\',?referenceTemperatureValue) AS ?referenceTemperature)
          }
            UNION
          {
            ?_referenceTemperature
              rdf:value ?referenceTemperatureValue ;
              (aixm:uom | fixm:uom | plain:uom) ?referenceTemperatureUoM .
            BIND(concat(\'xval:\',STR(?referenceTemperatureValue),\':\',?referenceTemperatureUoM) AS ?referenceTemperature)
          }
            UNION
          {
           ?_referenceTemperature  aixm:nilReason ?referenceTemperatureNilReason .
           BIND(concat(\'nil:\',?referenceTemperatureNilReason) AS ?referenceTemperature)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:altimeterCheckLocation ?_altimeterCheckLocation .
        {
          {
            ?_altimeterCheckLocation rdf:value ?altimeterCheckLocationValue .
            FILTER ( NOT EXISTS {?_altimeterCheckLocation (aixm:uom | fixm:uom | plain:uom) ?altimeterCheckLocationUoM})
            BIND(concat(\'val:\',?altimeterCheckLocationValue) AS ?altimeterCheckLocation)
          }
            UNION
          {
            ?_altimeterCheckLocation
              rdf:value ?altimeterCheckLocationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?altimeterCheckLocationUoM .
            BIND(concat(\'xval:\',STR(?altimeterCheckLocationValue),\':\',?altimeterCheckLocationUoM) AS ?altimeterCheckLocation)
          }
            UNION
          {
           ?_altimeterCheckLocation  aixm:nilReason ?altimeterCheckLocationNilReason .
           BIND(concat(\'nil:\',?altimeterCheckLocationNilReason) AS ?altimeterCheckLocation)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:secondaryPowerSupply ?_secondaryPowerSupply .
        {
          {
            ?_secondaryPowerSupply rdf:value ?secondaryPowerSupplyValue .
            FILTER ( NOT EXISTS {?_secondaryPowerSupply (aixm:uom | fixm:uom | plain:uom) ?secondaryPowerSupplyUoM})
            BIND(concat(\'val:\',?secondaryPowerSupplyValue) AS ?secondaryPowerSupply)
          }
            UNION
          {
            ?_secondaryPowerSupply
              rdf:value ?secondaryPowerSupplyValue ;
              (aixm:uom | fixm:uom | plain:uom) ?secondaryPowerSupplyUoM .
            BIND(concat(\'xval:\',STR(?secondaryPowerSupplyValue),\':\',?secondaryPowerSupplyUoM) AS ?secondaryPowerSupply)
          }
            UNION
          {
           ?_secondaryPowerSupply  aixm:nilReason ?secondaryPowerSupplyNilReason .
           BIND(concat(\'nil:\',?secondaryPowerSupplyNilReason) AS ?secondaryPowerSupply)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:windDirectionIndicator ?_windDirectionIndicator .
        {
          {
            ?_windDirectionIndicator rdf:value ?windDirectionIndicatorValue .
            FILTER ( NOT EXISTS {?_windDirectionIndicator (aixm:uom | fixm:uom | plain:uom) ?windDirectionIndicatorUoM})
            BIND(concat(\'val:\',?windDirectionIndicatorValue) AS ?windDirectionIndicator)
          }
            UNION
          {
            ?_windDirectionIndicator
              rdf:value ?windDirectionIndicatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?windDirectionIndicatorUoM .
            BIND(concat(\'xval:\',STR(?windDirectionIndicatorValue),\':\',?windDirectionIndicatorUoM) AS ?windDirectionIndicator)
          }
            UNION
          {
           ?_windDirectionIndicator  aixm:nilReason ?windDirectionIndicatorNilReason .
           BIND(concat(\'nil:\',?windDirectionIndicatorNilReason) AS ?windDirectionIndicator)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:landingDirectionIndicator ?_landingDirectionIndicator .
        {
          {
            ?_landingDirectionIndicator rdf:value ?landingDirectionIndicatorValue .
            FILTER ( NOT EXISTS {?_landingDirectionIndicator (aixm:uom | fixm:uom | plain:uom) ?landingDirectionIndicatorUoM})
            BIND(concat(\'val:\',?landingDirectionIndicatorValue) AS ?landingDirectionIndicator)
          }
            UNION
          {
            ?_landingDirectionIndicator
              rdf:value ?landingDirectionIndicatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?landingDirectionIndicatorUoM .
            BIND(concat(\'xval:\',STR(?landingDirectionIndicatorValue),\':\',?landingDirectionIndicatorUoM) AS ?landingDirectionIndicator)
          }
            UNION
          {
           ?_landingDirectionIndicator  aixm:nilReason ?landingDirectionIndicatorNilReason .
           BIND(concat(\'nil:\',?landingDirectionIndicatorNilReason) AS ?landingDirectionIndicator)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:transitionAltitude ?_transitionAltitude .
        {
          {
            ?_transitionAltitude rdf:value ?transitionAltitudeValue .
            FILTER ( NOT EXISTS {?_transitionAltitude (aixm:uom | fixm:uom | plain:uom) ?transitionAltitudeUoM})
            BIND(concat(\'val:\',?transitionAltitudeValue) AS ?transitionAltitude)
          }
            UNION
          {
            ?_transitionAltitude
              rdf:value ?transitionAltitudeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?transitionAltitudeUoM .
            BIND(concat(\'xval:\',STR(?transitionAltitudeValue),\':\',?transitionAltitudeUoM) AS ?transitionAltitude)
          }
            UNION
          {
           ?_transitionAltitude  aixm:nilReason ?transitionAltitudeNilReason .
           BIND(concat(\'nil:\',?transitionAltitudeNilReason) AS ?transitionAltitude)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:transitionLevel ?_transitionLevel .
        {
          {
            ?_transitionLevel rdf:value ?transitionLevelValue .
            FILTER ( NOT EXISTS {?_transitionLevel (aixm:uom | fixm:uom | plain:uom) ?transitionLevelUoM})
            BIND(concat(\'val:\',?transitionLevelValue) AS ?transitionLevel)
          }
            UNION
          {
            ?_transitionLevel
              rdf:value ?transitionLevelValue ;
              (aixm:uom | fixm:uom | plain:uom) ?transitionLevelUoM .
            BIND(concat(\'xval:\',STR(?transitionLevelValue),\':\',?transitionLevelUoM) AS ?transitionLevel)
          }
            UNION
          {
           ?_transitionLevel  aixm:nilReason ?transitionLevelNilReason .
           BIND(concat(\'nil:\',?transitionLevelNilReason) AS ?transitionLevel)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:lowestTemperature ?_lowestTemperature .
        {
          {
            ?_lowestTemperature rdf:value ?lowestTemperatureValue .
            FILTER ( NOT EXISTS {?_lowestTemperature (aixm:uom | fixm:uom | plain:uom) ?lowestTemperatureUoM})
            BIND(concat(\'val:\',?lowestTemperatureValue) AS ?lowestTemperature)
          }
            UNION
          {
            ?_lowestTemperature
              rdf:value ?lowestTemperatureValue ;
              (aixm:uom | fixm:uom | plain:uom) ?lowestTemperatureUoM .
            BIND(concat(\'xval:\',STR(?lowestTemperatureValue),\':\',?lowestTemperatureUoM) AS ?lowestTemperature)
          }
            UNION
          {
           ?_lowestTemperature  aixm:nilReason ?lowestTemperatureNilReason .
           BIND(concat(\'nil:\',?lowestTemperatureNilReason) AS ?lowestTemperature)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:abandoned ?_abandoned .
        {
          {
            ?_abandoned rdf:value ?abandonedValue .
            FILTER ( NOT EXISTS {?_abandoned (aixm:uom | fixm:uom | plain:uom) ?abandonedUoM})
            BIND(concat(\'val:\',?abandonedValue) AS ?abandoned)
          }
            UNION
          {
            ?_abandoned
              rdf:value ?abandonedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?abandonedUoM .
            BIND(concat(\'xval:\',STR(?abandonedValue),\':\',?abandonedUoM) AS ?abandoned)
          }
            UNION
          {
           ?_abandoned  aixm:nilReason ?abandonedNilReason .
           BIND(concat(\'nil:\',?abandonedNilReason) AS ?abandoned)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:certificationDate ?_certificationDate .
        {
          {
            ?_certificationDate rdf:value ?certificationDateValue .
            FILTER ( NOT EXISTS {?_certificationDate (aixm:uom | fixm:uom | plain:uom) ?certificationDateUoM})
            BIND(concat(\'val:\',?certificationDateValue) AS ?certificationDate)
          }
            UNION
          {
            ?_certificationDate
              rdf:value ?certificationDateValue ;
              (aixm:uom | fixm:uom | plain:uom) ?certificationDateUoM .
            BIND(concat(\'xval:\',STR(?certificationDateValue),\':\',?certificationDateUoM) AS ?certificationDate)
          }
            UNION
          {
           ?_certificationDate  aixm:nilReason ?certificationDateNilReason .
           BIND(concat(\'nil:\',?certificationDateNilReason) AS ?certificationDate)
          }
        }
      }
      OPTIONAL { ?airportHeliportTimeSlice aixm:certificationExpirationDate ?_certificationExpirationDate .
        {
          {
            ?_certificationExpirationDate rdf:value ?certificationExpirationDateValue .
            FILTER ( NOT EXISTS {?_certificationExpirationDate (aixm:uom | fixm:uom | plain:uom) ?certificationExpirationDateUoM})
            BIND(concat(\'val:\',?certificationExpirationDateValue) AS ?certificationExpirationDate)
          }
            UNION
          {
            ?_certificationExpirationDate
              rdf:value ?certificationExpirationDateValue ;
              (aixm:uom | fixm:uom | plain:uom) ?certificationExpirationDateUoM .
            BIND(concat(\'xval:\',STR(?certificationExpirationDateValue),\':\',?certificationExpirationDateUoM) AS ?certificationExpirationDate)
          }
            UNION
          {
           ?_certificationExpirationDate  aixm:nilReason ?certificationExpirationDateNilReason .
           BIND(concat(\'nil:\',?certificationExpirationDateNilReason) AS ?certificationExpirationDate)
          }
        }
      }
      OPTIONAL {?airportHeliportTimeSlice aixm:contact ?contact .}
      OPTIONAL {?airportHeliportTimeSlice aixm:annotation ?annotation .}
      OPTIONAL {?airportHeliportTimeSlice aixm:ARP ?ARP .}
      OPTIONAL {?airportHeliportTimeSlice aixm:altimeterSource ?altimeterSource .}
      OPTIONAL {?airportHeliportTimeSlice aixm:contaminant ?contaminant .}
      OPTIONAL {?airportHeliportTimeSlice aixm:servedCity ?servedCity .}
      OPTIONAL {?airportHeliportTimeSlice aixm:responsibleOrganisation ?responsibleOrganisation .}
      OPTIONAL {?airportHeliportTimeSlice aixm:aviationBoundary ?aviationBoundary .}
      OPTIONAL {?airportHeliportTimeSlice aixm:availability ?availability .}
    }
  }
GROUP BY ?graph ?airportHeliportTimeSlice ?designator ?name ?locationIndicatorICAO ?designatorIATA ?type ?certifiedICAO ?privateUse ?controlType ?fieldElevation ?fieldElevationAccuracy ?verticalDatum ?magneticVariation ?magneticVariationAccuracy ?dateMagneticVariation ?magneticVariationChange ?referenceTemperature ?altimeterCheckLocation ?secondaryPowerSupply ?windDirectionIndicator ?landingDirectionIndicator ?transitionAltitude ?transitionLevel ?lowestTemperature ?abandoned ?certificationDate ?certificationExpirationDate ?ARP ?responsibleOrganisation ?aviationBoundary

      '
,row(Graph,AirportHeliportTimeSlice,Designator,Name,LocationIndicatorICAO,DesignatorIATA,Type,CertifiedICAO,PrivateUse,ControlType,FieldElevation,FieldElevationAccuracy,VerticalDatum,MagneticVariation,MagneticVariationAccuracy,DateMagneticVariation,MagneticVariationChange,ReferenceTemperature,AltimeterCheckLocation,SecondaryPowerSupply,WindDirectionIndicator,LandingDirectionIndicator,TransitionAltitude,TransitionLevel,LowestTemperature,Abandoned,CertificationDate,CertificationExpirationDate,ContactConcat,AnnotationConcat,ARP,AltimeterSourceConcat,ContaminantConcat,ServedCityConcat,ResponsibleOrganisation,AviationBoundary,AvailabilityConcat),[]), convert(ContactConcat,ContactList), convert(AnnotationConcat,AnnotationList), convert(AltimeterSourceConcat,AltimeterSourceList), convert(ContaminantConcat,ContaminantList), convert(ServedCityConcat,ServedCityList), convert(AvailabilityConcat,AvailabilityList).

% fixm_Point4D(Graph, Point4D, Altitude?, Time?, PointRange?)

fixm_Point4D(Graph, Point4D, Altitude, Time, PointRange) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?point4D ?altitude ?time ?pointRange
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:Point4D .
  }
  { GRAPH ?graph
    {
      ?point4D rdf:type ?SUBCLASS .
      OPTIONAL { ?point4D fixm:altitude ?_altitude .
        {
          {
            ?_altitude rdf:value ?altitudeValue .
            FILTER ( NOT EXISTS {?_altitude (aixm:uom | fixm:uom | plain:uom) ?altitudeUoM})
            BIND(concat(\'val:\',?altitudeValue) AS ?altitude)
          }
            UNION
          {
            ?_altitude
              rdf:value ?altitudeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?altitudeUoM .
            BIND(concat(\'xval:\',STR(?altitudeValue),\':\',?altitudeUoM) AS ?altitude)
          }
            UNION
          {
           ?_altitude  aixm:nilReason ?altitudeNilReason .
           BIND(concat(\'nil:\',?altitudeNilReason) AS ?altitude)
          }
        }
      }
      OPTIONAL { ?point4D fixm:time ?_time .
        {
          {
            ?_time rdf:value ?timeValue .
            FILTER ( NOT EXISTS {?_time (aixm:uom | fixm:uom | plain:uom) ?timeUoM})
            BIND(concat(\'val:\',?timeValue) AS ?time)
          }
            UNION
          {
            ?_time
              rdf:value ?timeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?timeUoM .
            BIND(concat(\'xval:\',STR(?timeValue),\':\',?timeUoM) AS ?time)
          }
            UNION
          {
           ?_time  aixm:nilReason ?timeNilReason .
           BIND(concat(\'nil:\',?timeNilReason) AS ?time)
          }
        }
      }
      OPTIONAL {?point4D fixm:pointRange ?pointRange .}
    }
  }
}

      '
,row(Graph,Point4D,Altitude,Time,PointRange),[]).

% fixm_AbstractRoutePoint(Graph, AbstractRoutePoint, AirTrafficType?, DelayAtPoint?, FlightRules?, Point?, ClearanceLimit?)

fixm_AbstractRoutePoint(Graph, AbstractRoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?abstractRoutePoint ?airTrafficType ?delayAtPoint ?flightRules ?point ?clearanceLimit
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:AbstractRoutePoint .
  }
  { GRAPH ?graph
    {
      ?abstractRoutePoint rdf:type ?SUBCLASS .
      OPTIONAL { ?abstractRoutePoint fixm:airTrafficType ?_airTrafficType .
        {
          {
            ?_airTrafficType rdf:value ?airTrafficTypeValue .
            FILTER ( NOT EXISTS {?_airTrafficType (aixm:uom | fixm:uom | plain:uom) ?airTrafficTypeUoM})
            BIND(concat(\'val:\',?airTrafficTypeValue) AS ?airTrafficType)
          }
            UNION
          {
            ?_airTrafficType
              rdf:value ?airTrafficTypeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?airTrafficTypeUoM .
            BIND(concat(\'xval:\',STR(?airTrafficTypeValue),\':\',?airTrafficTypeUoM) AS ?airTrafficType)
          }
            UNION
          {
           ?_airTrafficType  aixm:nilReason ?airTrafficTypeNilReason .
           BIND(concat(\'nil:\',?airTrafficTypeNilReason) AS ?airTrafficType)
          }
        }
      }
      OPTIONAL { ?abstractRoutePoint fixm:delayAtPoint ?_delayAtPoint .
        {
          {
            ?_delayAtPoint rdf:value ?delayAtPointValue .
            FILTER ( NOT EXISTS {?_delayAtPoint (aixm:uom | fixm:uom | plain:uom) ?delayAtPointUoM})
            BIND(concat(\'val:\',?delayAtPointValue) AS ?delayAtPoint)
          }
            UNION
          {
            ?_delayAtPoint
              rdf:value ?delayAtPointValue ;
              (aixm:uom | fixm:uom | plain:uom) ?delayAtPointUoM .
            BIND(concat(\'xval:\',STR(?delayAtPointValue),\':\',?delayAtPointUoM) AS ?delayAtPoint)
          }
            UNION
          {
           ?_delayAtPoint  aixm:nilReason ?delayAtPointNilReason .
           BIND(concat(\'nil:\',?delayAtPointNilReason) AS ?delayAtPoint)
          }
        }
      }
      OPTIONAL { ?abstractRoutePoint fixm:flightRules ?_flightRules .
        {
          {
            ?_flightRules rdf:value ?flightRulesValue .
            FILTER ( NOT EXISTS {?_flightRules (aixm:uom | fixm:uom | plain:uom) ?flightRulesUoM})
            BIND(concat(\'val:\',?flightRulesValue) AS ?flightRules)
          }
            UNION
          {
            ?_flightRules
              rdf:value ?flightRulesValue ;
              (aixm:uom | fixm:uom | plain:uom) ?flightRulesUoM .
            BIND(concat(\'xval:\',STR(?flightRulesValue),\':\',?flightRulesUoM) AS ?flightRules)
          }
            UNION
          {
           ?_flightRules  aixm:nilReason ?flightRulesNilReason .
           BIND(concat(\'nil:\',?flightRulesNilReason) AS ?flightRules)
          }
        }
      }
      OPTIONAL {?abstractRoutePoint fixm:point ?point .}
      OPTIONAL { ?abstractRoutePoint fixm:clearanceLimit ?_clearanceLimit .
        {
          {
            ?_clearanceLimit rdf:value ?clearanceLimitValue .
            FILTER ( NOT EXISTS {?_clearanceLimit (aixm:uom | fixm:uom | plain:uom) ?clearanceLimitUoM})
            BIND(concat(\'val:\',?clearanceLimitValue) AS ?clearanceLimit)
          }
            UNION
          {
            ?_clearanceLimit
              rdf:value ?clearanceLimitValue ;
              (aixm:uom | fixm:uom | plain:uom) ?clearanceLimitUoM .
            BIND(concat(\'xval:\',STR(?clearanceLimitValue),\':\',?clearanceLimitUoM) AS ?clearanceLimit)
          }
            UNION
          {
           ?_clearanceLimit  aixm:nilReason ?clearanceLimitNilReason .
           BIND(concat(\'nil:\',?clearanceLimitNilReason) AS ?clearanceLimit)
          }
        }
      }
    }
  }
}

      '
,row(Graph,AbstractRoutePoint,AirTrafficType,DelayAtPoint,FlightRules,Point,ClearanceLimit),[]).

% aixm_Ridge(Graph, Ridge, Side?, Distance?, Depth?, Annotation*)

aixm_Ridge(Graph, Ridge, Side, Distance, Depth, AnnotationList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?ridge ?side ?distance ?depth (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat)
WHERE
  { GRAPH ?graph
    {
      ?ridge rdf:type aixm:Ridge .
      OPTIONAL { ?ridge aixm:side ?_side .
        {
          {
            ?_side rdf:value ?sideValue .
            FILTER ( NOT EXISTS {?_side (aixm:uom | fixm:uom | plain:uom) ?sideUoM})
            BIND(concat(\'val:\',?sideValue) AS ?side)
          }
            UNION
          {
            ?_side
              rdf:value ?sideValue ;
              (aixm:uom | fixm:uom | plain:uom) ?sideUoM .
            BIND(concat(\'xval:\',STR(?sideValue),\':\',?sideUoM) AS ?side)
          }
            UNION
          {
           ?_side  aixm:nilReason ?sideNilReason .
           BIND(concat(\'nil:\',?sideNilReason) AS ?side)
          }
        }
      }
      OPTIONAL { ?ridge aixm:distance ?_distance .
        {
          {
            ?_distance rdf:value ?distanceValue .
            FILTER ( NOT EXISTS {?_distance (aixm:uom | fixm:uom | plain:uom) ?distanceUoM})
            BIND(concat(\'val:\',?distanceValue) AS ?distance)
          }
            UNION
          {
            ?_distance
              rdf:value ?distanceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?distanceUoM .
            BIND(concat(\'xval:\',STR(?distanceValue),\':\',?distanceUoM) AS ?distance)
          }
            UNION
          {
           ?_distance  aixm:nilReason ?distanceNilReason .
           BIND(concat(\'nil:\',?distanceNilReason) AS ?distance)
          }
        }
      }
      OPTIONAL { ?ridge aixm:depth ?_depth .
        {
          {
            ?_depth rdf:value ?depthValue .
            FILTER ( NOT EXISTS {?_depth (aixm:uom | fixm:uom | plain:uom) ?depthUoM})
            BIND(concat(\'val:\',?depthValue) AS ?depth)
          }
            UNION
          {
            ?_depth
              rdf:value ?depthValue ;
              (aixm:uom | fixm:uom | plain:uom) ?depthUoM .
            BIND(concat(\'xval:\',STR(?depthValue),\':\',?depthUoM) AS ?depth)
          }
            UNION
          {
           ?_depth  aixm:nilReason ?depthNilReason .
           BIND(concat(\'nil:\',?depthNilReason) AS ?depth)
          }
        }
      }
      OPTIONAL {?ridge aixm:annotation ?annotation .}
    }
  }
GROUP BY ?graph ?ridge ?side ?distance ?depth

      '
,row(Graph,Ridge,Side,Distance,Depth,AnnotationConcat),[]), convert(AnnotationConcat,AnnotationList).

% fixm_DepartureActivityTimes(Graph, DepartureActivityTimes, BoardingTime?, DeIcingTime?, GroundHandlingTime?, StartupTime?)

fixm_DepartureActivityTimes(Graph, DepartureActivityTimes, BoardingTime, DeIcingTime, GroundHandlingTime, StartupTime) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?departureActivityTimes ?boardingTime ?deIcingTime ?groundHandlingTime ?startupTime
WHERE
  { GRAPH ?graph
    {
      ?departureActivityTimes rdf:type fixm:DepartureActivityTimes .
      OPTIONAL {?departureActivityTimes fixm:boardingTime ?boardingTime .}
      OPTIONAL {?departureActivityTimes fixm:deIcingTime ?deIcingTime .}
      OPTIONAL {?departureActivityTimes fixm:groundHandlingTime ?groundHandlingTime .}
      OPTIONAL {?departureActivityTimes fixm:startupTime ?startupTime .}
    }
  }

      '
,row(Graph,DepartureActivityTimes,BoardingTime,DeIcingTime,GroundHandlingTime,StartupTime),[]).

% fixm_EnRouteDiversion(Graph, EnRouteDiversion, DiversionRecoveryInformation?)

fixm_EnRouteDiversion(Graph, EnRouteDiversion, DiversionRecoveryInformation) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?enRouteDiversion ?diversionRecoveryInformation
WHERE
  { GRAPH ?graph
    {
      ?enRouteDiversion rdf:type fixm:EnRouteDiversion .
      OPTIONAL { ?enRouteDiversion fixm:diversionRecoveryInformation ?_diversionRecoveryInformation .
        {
          {
            ?_diversionRecoveryInformation rdf:value ?diversionRecoveryInformationValue .
            FILTER ( NOT EXISTS {?_diversionRecoveryInformation (aixm:uom | fixm:uom | plain:uom) ?diversionRecoveryInformationUoM})
            BIND(concat(\'val:\',?diversionRecoveryInformationValue) AS ?diversionRecoveryInformation)
          }
            UNION
          {
            ?_diversionRecoveryInformation
              rdf:value ?diversionRecoveryInformationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?diversionRecoveryInformationUoM .
            BIND(concat(\'xval:\',STR(?diversionRecoveryInformationValue),\':\',?diversionRecoveryInformationUoM) AS ?diversionRecoveryInformation)
          }
            UNION
          {
           ?_diversionRecoveryInformation  aixm:nilReason ?diversionRecoveryInformationNilReason .
           BIND(concat(\'nil:\',?diversionRecoveryInformationNilReason) AS ?diversionRecoveryInformation)
          }
        }
      }
    }
  }

      '
,row(Graph,EnRouteDiversion,DiversionRecoveryInformation),[]).

% fixm_ActualSpeed(Graph, ActualSpeed, Calculated?, PilotReported?, Surveillance?)

fixm_ActualSpeed(Graph, ActualSpeed, Calculated, PilotReported, Surveillance) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?actualSpeed ?calculated ?pilotReported ?surveillance
WHERE
  { GRAPH ?graph
    {
      ?actualSpeed rdf:type fixm:ActualSpeed .
      OPTIONAL { ?actualSpeed fixm:calculated ?_calculated .
        {
          {
            ?_calculated rdf:value ?calculatedValue .
            FILTER ( NOT EXISTS {?_calculated (aixm:uom | fixm:uom | plain:uom) ?calculatedUoM})
            BIND(concat(\'val:\',?calculatedValue) AS ?calculated)
          }
            UNION
          {
            ?_calculated
              rdf:value ?calculatedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?calculatedUoM .
            BIND(concat(\'xval:\',STR(?calculatedValue),\':\',?calculatedUoM) AS ?calculated)
          }
            UNION
          {
           ?_calculated  aixm:nilReason ?calculatedNilReason .
           BIND(concat(\'nil:\',?calculatedNilReason) AS ?calculated)
          }
        }
      }
      OPTIONAL { ?actualSpeed fixm:pilotReported ?_pilotReported .
        {
          {
            ?_pilotReported rdf:value ?pilotReportedValue .
            FILTER ( NOT EXISTS {?_pilotReported (aixm:uom | fixm:uom | plain:uom) ?pilotReportedUoM})
            BIND(concat(\'val:\',?pilotReportedValue) AS ?pilotReported)
          }
            UNION
          {
            ?_pilotReported
              rdf:value ?pilotReportedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?pilotReportedUoM .
            BIND(concat(\'xval:\',STR(?pilotReportedValue),\':\',?pilotReportedUoM) AS ?pilotReported)
          }
            UNION
          {
           ?_pilotReported  aixm:nilReason ?pilotReportedNilReason .
           BIND(concat(\'nil:\',?pilotReportedNilReason) AS ?pilotReported)
          }
        }
      }
      OPTIONAL { ?actualSpeed fixm:surveillance ?_surveillance .
        {
          {
            ?_surveillance rdf:value ?surveillanceValue .
            FILTER ( NOT EXISTS {?_surveillance (aixm:uom | fixm:uom | plain:uom) ?surveillanceUoM})
            BIND(concat(\'val:\',?surveillanceValue) AS ?surveillance)
          }
            UNION
          {
            ?_surveillance
              rdf:value ?surveillanceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?surveillanceUoM .
            BIND(concat(\'xval:\',STR(?surveillanceValue),\':\',?surveillanceUoM) AS ?surveillance)
          }
            UNION
          {
           ?_surveillance  aixm:nilReason ?surveillanceNilReason .
           BIND(concat(\'nil:\',?surveillanceNilReason) AS ?surveillance)
          }
        }
      }
    }
  }

      '
,row(Graph,ActualSpeed,Calculated,PilotReported,Surveillance),[]).

% fixm_FlightEmergency(Graph, FlightEmergency, ActionTaken?, EmergencyDescription?, Originator?, OtherInformation?, Phase?, Contact?)

fixm_FlightEmergency(Graph, FlightEmergency, ActionTaken, EmergencyDescription, Originator, OtherInformation, Phase, Contact) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?flightEmergency ?actionTaken ?emergencyDescription ?originator ?otherInformation ?phase ?contact
WHERE
  { GRAPH ?graph
    {
      ?flightEmergency rdf:type fixm:FlightEmergency .
      OPTIONAL { ?flightEmergency fixm:actionTaken ?_actionTaken .
        {
          {
            ?_actionTaken rdf:value ?actionTakenValue .
            FILTER ( NOT EXISTS {?_actionTaken (aixm:uom | fixm:uom | plain:uom) ?actionTakenUoM})
            BIND(concat(\'val:\',?actionTakenValue) AS ?actionTaken)
          }
            UNION
          {
            ?_actionTaken
              rdf:value ?actionTakenValue ;
              (aixm:uom | fixm:uom | plain:uom) ?actionTakenUoM .
            BIND(concat(\'xval:\',STR(?actionTakenValue),\':\',?actionTakenUoM) AS ?actionTaken)
          }
            UNION
          {
           ?_actionTaken  aixm:nilReason ?actionTakenNilReason .
           BIND(concat(\'nil:\',?actionTakenNilReason) AS ?actionTaken)
          }
        }
      }
      OPTIONAL { ?flightEmergency fixm:emergencyDescription ?_emergencyDescription .
        {
          {
            ?_emergencyDescription rdf:value ?emergencyDescriptionValue .
            FILTER ( NOT EXISTS {?_emergencyDescription (aixm:uom | fixm:uom | plain:uom) ?emergencyDescriptionUoM})
            BIND(concat(\'val:\',?emergencyDescriptionValue) AS ?emergencyDescription)
          }
            UNION
          {
            ?_emergencyDescription
              rdf:value ?emergencyDescriptionValue ;
              (aixm:uom | fixm:uom | plain:uom) ?emergencyDescriptionUoM .
            BIND(concat(\'xval:\',STR(?emergencyDescriptionValue),\':\',?emergencyDescriptionUoM) AS ?emergencyDescription)
          }
            UNION
          {
           ?_emergencyDescription  aixm:nilReason ?emergencyDescriptionNilReason .
           BIND(concat(\'nil:\',?emergencyDescriptionNilReason) AS ?emergencyDescription)
          }
        }
      }
      OPTIONAL {?flightEmergency fixm:originator ?originator .}
      OPTIONAL { ?flightEmergency fixm:otherInformation ?_otherInformation .
        {
          {
            ?_otherInformation rdf:value ?otherInformationValue .
            FILTER ( NOT EXISTS {?_otherInformation (aixm:uom | fixm:uom | plain:uom) ?otherInformationUoM})
            BIND(concat(\'val:\',?otherInformationValue) AS ?otherInformation)
          }
            UNION
          {
            ?_otherInformation
              rdf:value ?otherInformationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?otherInformationUoM .
            BIND(concat(\'xval:\',STR(?otherInformationValue),\':\',?otherInformationUoM) AS ?otherInformation)
          }
            UNION
          {
           ?_otherInformation  aixm:nilReason ?otherInformationNilReason .
           BIND(concat(\'nil:\',?otherInformationNilReason) AS ?otherInformation)
          }
        }
      }
      OPTIONAL { ?flightEmergency fixm:phase ?_phase .
        {
          {
            ?_phase rdf:value ?phaseValue .
            FILTER ( NOT EXISTS {?_phase (aixm:uom | fixm:uom | plain:uom) ?phaseUoM})
            BIND(concat(\'val:\',?phaseValue) AS ?phase)
          }
            UNION
          {
            ?_phase
              rdf:value ?phaseValue ;
              (aixm:uom | fixm:uom | plain:uom) ?phaseUoM .
            BIND(concat(\'xval:\',STR(?phaseValue),\':\',?phaseUoM) AS ?phase)
          }
            UNION
          {
           ?_phase  aixm:nilReason ?phaseNilReason .
           BIND(concat(\'nil:\',?phaseNilReason) AS ?phase)
          }
        }
      }
      OPTIONAL {?flightEmergency fixm:contact ?contact .}
    }
  }

      '
,row(Graph,FlightEmergency,ActionTaken,EmergencyDescription,Originator,OtherInformation,Phase,Contact),[]).

% fixm_Flight(Graph, Flight, ControllingUnit?, Extensions*, FlightFiler?, Gufi?, Remarks?, AircraftDescription?, DangerousGoods*, RankedTrajectories*, RouteToRevisedDestination?, Negotiating?, Agreed?, Arrival?, Departure?, Emergency?, RadioCommunicationFailure?, EnRoute?, Operator?, EnRouteDiversion?, FlightType?, FlightStatus?, Originator?, SupplementalData?, FlightIdentification?, SpecialHandling*)

fixm_Flight(Graph, Flight, ControllingUnit, ExtensionsList, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoodsList, RankedTrajectoriesList, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandlingList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?flight ?controllingUnit (GROUP_CONCAT(DISTINCT ?extensions;SEPARATOR=",") AS ?extensionsConcat) ?flightFiler ?gufi ?remarks ?aircraftDescription (GROUP_CONCAT(DISTINCT ?dangerousGoods;SEPARATOR=",") AS ?dangerousGoodsConcat) (GROUP_CONCAT(DISTINCT ?rankedTrajectories;SEPARATOR=",") AS ?rankedTrajectoriesConcat) ?routeToRevisedDestination ?negotiating ?agreed ?arrival ?departure ?emergency ?radioCommunicationFailure ?enRoute ?operator ?enRouteDiversion ?flightType ?flightStatus ?originator ?supplementalData ?flightIdentification (GROUP_CONCAT(DISTINCT ?specialHandling;SEPARATOR=",") AS ?specialHandlingConcat)
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:Flight .
  }
  { GRAPH ?graph
    {
      ?flight rdf:type ?SUBCLASS .
      OPTIONAL {?flight fixm:controllingUnit ?controllingUnit .}
      OPTIONAL {?flight fixm:extensions ?extensions .}
      OPTIONAL { ?flight fixm:flightFiler ?_flightFiler .
        {
          {
            ?_flightFiler rdf:value ?flightFilerValue .
            FILTER ( NOT EXISTS {?_flightFiler (aixm:uom | fixm:uom | plain:uom) ?flightFilerUoM})
            BIND(concat(\'val:\',?flightFilerValue) AS ?flightFiler)
          }
            UNION
          {
            ?_flightFiler
              rdf:value ?flightFilerValue ;
              (aixm:uom | fixm:uom | plain:uom) ?flightFilerUoM .
            BIND(concat(\'xval:\',STR(?flightFilerValue),\':\',?flightFilerUoM) AS ?flightFiler)
          }
            UNION
          {
           ?_flightFiler  aixm:nilReason ?flightFilerNilReason .
           BIND(concat(\'nil:\',?flightFilerNilReason) AS ?flightFiler)
          }
        }
      }
      OPTIONAL { ?flight fixm:gufi ?_gufi .
        {
          {
            ?_gufi rdf:value ?gufiValue .
            FILTER ( NOT EXISTS {?_gufi (aixm:uom | fixm:uom | plain:uom) ?gufiUoM})
            BIND(concat(\'val:\',?gufiValue) AS ?gufi)
          }
            UNION
          {
            ?_gufi
              rdf:value ?gufiValue ;
              (aixm:uom | fixm:uom | plain:uom) ?gufiUoM .
            BIND(concat(\'xval:\',STR(?gufiValue),\':\',?gufiUoM) AS ?gufi)
          }
            UNION
          {
           ?_gufi  aixm:nilReason ?gufiNilReason .
           BIND(concat(\'nil:\',?gufiNilReason) AS ?gufi)
          }
        }
      }
      OPTIONAL { ?flight fixm:remarks ?_remarks .
        {
          {
            ?_remarks rdf:value ?remarksValue .
            FILTER ( NOT EXISTS {?_remarks (aixm:uom | fixm:uom | plain:uom) ?remarksUoM})
            BIND(concat(\'val:\',?remarksValue) AS ?remarks)
          }
            UNION
          {
            ?_remarks
              rdf:value ?remarksValue ;
              (aixm:uom | fixm:uom | plain:uom) ?remarksUoM .
            BIND(concat(\'xval:\',STR(?remarksValue),\':\',?remarksUoM) AS ?remarks)
          }
            UNION
          {
           ?_remarks  aixm:nilReason ?remarksNilReason .
           BIND(concat(\'nil:\',?remarksNilReason) AS ?remarks)
          }
        }
      }
      OPTIONAL {?flight fixm:aircraftDescription ?aircraftDescription .}
      OPTIONAL {?flight fixm:dangerousGoods ?dangerousGoods .}
      OPTIONAL {?flight fixm:rankedTrajectories ?rankedTrajectories .}
      OPTIONAL {?flight fixm:routeToRevisedDestination ?routeToRevisedDestination .}
      OPTIONAL {?flight fixm:negotiating ?negotiating .}
      OPTIONAL {?flight fixm:agreed ?agreed .}
      OPTIONAL {?flight fixm:arrival ?arrival .}
      OPTIONAL {?flight fixm:departure ?departure .}
      OPTIONAL {?flight fixm:emergency ?emergency .}
      OPTIONAL {?flight fixm:radioCommunicationFailure ?radioCommunicationFailure .}
      OPTIONAL {?flight fixm:enRoute ?enRoute .}
      OPTIONAL {?flight fixm:operator ?operator .}
      OPTIONAL {?flight fixm:enRouteDiversion ?enRouteDiversion .}
      OPTIONAL { ?flight fixm:flightType ?_flightType .
        {
          {
            ?_flightType rdf:value ?flightTypeValue .
            FILTER ( NOT EXISTS {?_flightType (aixm:uom | fixm:uom | plain:uom) ?flightTypeUoM})
            BIND(concat(\'val:\',?flightTypeValue) AS ?flightType)
          }
            UNION
          {
            ?_flightType
              rdf:value ?flightTypeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?flightTypeUoM .
            BIND(concat(\'xval:\',STR(?flightTypeValue),\':\',?flightTypeUoM) AS ?flightType)
          }
            UNION
          {
           ?_flightType  aixm:nilReason ?flightTypeNilReason .
           BIND(concat(\'nil:\',?flightTypeNilReason) AS ?flightType)
          }
        }
      }
      OPTIONAL {?flight fixm:flightStatus ?flightStatus .}
      OPTIONAL { ?flight fixm:originator ?_originator .
        {
          {
            ?_originator rdf:value ?originatorValue .
            FILTER ( NOT EXISTS {?_originator (aixm:uom | fixm:uom | plain:uom) ?originatorUoM})
            BIND(concat(\'val:\',?originatorValue) AS ?originator)
          }
            UNION
          {
            ?_originator
              rdf:value ?originatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?originatorUoM .
            BIND(concat(\'xval:\',STR(?originatorValue),\':\',?originatorUoM) AS ?originator)
          }
            UNION
          {
           ?_originator  aixm:nilReason ?originatorNilReason .
           BIND(concat(\'nil:\',?originatorNilReason) AS ?originator)
          }
        }
      }
      OPTIONAL {?flight fixm:supplementalData ?supplementalData .}
      OPTIONAL {?flight fixm:flightIdentification ?flightIdentification .}
      OPTIONAL { ?flight fixm:specialHandling ?_specialHandling .
        {
          {
            ?_specialHandling rdf:value ?specialHandlingValue .
            FILTER ( NOT EXISTS {?_specialHandling (aixm:uom | fixm:uom | plain:uom) ?specialHandlingUoM})
            BIND(concat(\'val:\',?specialHandlingValue) AS ?specialHandling)
          }
            UNION
          {
            ?_specialHandling
              rdf:value ?specialHandlingValue ;
              (aixm:uom | fixm:uom | plain:uom) ?specialHandlingUoM .
            BIND(concat(\'xval:\',STR(?specialHandlingValue),\':\',?specialHandlingUoM) AS ?specialHandling)
          }
            UNION
          {
           ?_specialHandling  aixm:nilReason ?specialHandlingNilReason .
           BIND(concat(\'nil:\',?specialHandlingNilReason) AS ?specialHandling)
          }
        }
      }
    }
  }
}
GROUP BY ?graph ?flight ?controllingUnit ?flightFiler ?gufi ?remarks ?aircraftDescription ?routeToRevisedDestination ?negotiating ?agreed ?arrival ?departure ?emergency ?radioCommunicationFailure ?enRoute ?operator ?enRouteDiversion ?flightType ?flightStatus ?originator ?supplementalData ?flightIdentification

      '
,row(Graph,Flight,ControllingUnit,ExtensionsConcat,FlightFiler,Gufi,Remarks,AircraftDescription,DangerousGoodsConcat,RankedTrajectoriesConcat,RouteToRevisedDestination,Negotiating,Agreed,Arrival,Departure,Emergency,RadioCommunicationFailure,EnRoute,Operator,EnRouteDiversion,FlightType,FlightStatus,Originator,SupplementalData,FlightIdentification,SpecialHandlingConcat),[]), convert(ExtensionsConcat,ExtensionsList), convert(DangerousGoodsConcat,DangerousGoodsList), convert(RankedTrajectoriesConcat,RankedTrajectoriesList), convert(SpecialHandlingConcat,SpecialHandlingList).

% aixm_PropertiesWithSchedule(Graph, PropertiesWithSchedule, Annotation*, SpecialDateAuthority*, TimeInterval*)

aixm_PropertiesWithSchedule(Graph, PropertiesWithSchedule, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?propertiesWithSchedule (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat) (GROUP_CONCAT(DISTINCT ?specialDateAuthority;SEPARATOR=",") AS ?specialDateAuthorityConcat) (GROUP_CONCAT(DISTINCT ?timeInterval;SEPARATOR=",") AS ?timeIntervalConcat)
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* aixm:PropertiesWithSchedule .
  }
  { GRAPH ?graph
    {
      ?propertiesWithSchedule rdf:type ?SUBCLASS .
      OPTIONAL {?propertiesWithSchedule aixm:annotation ?annotation .}
      OPTIONAL {?propertiesWithSchedule aixm:specialDateAuthority ?specialDateAuthority .}
      OPTIONAL {?propertiesWithSchedule aixm:timeInterval ?timeInterval .}
    }
  }
}
GROUP BY ?graph ?propertiesWithSchedule

      '
,row(Graph,PropertiesWithSchedule,AnnotationConcat,SpecialDateAuthorityConcat,TimeIntervalConcat),[]), convert(AnnotationConcat,AnnotationList), convert(SpecialDateAuthorityConcat,SpecialDateAuthorityList), convert(TimeIntervalConcat,TimeIntervalList).

% gml_Surface(Graph, Surface, Patch+)

gml_Surface(Graph, Surface, PatchList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?surface (GROUP_CONCAT(DISTINCT ?patch;SEPARATOR=",") AS ?patchConcat)
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* gml:Surface .
  }
  { GRAPH ?graph
    {
      ?surface rdf:type ?SUBCLASS .
      ?surface aixm:patch ?patch .
    }
  }
}
GROUP BY ?graph ?surface

      '
,row(Graph,Surface,PatchConcat),[]), convert(PatchConcat,PatchList).

% fixm_ClearedFlightInformation(Graph, ClearedFlightInformation, ClearedFlightLevel?, ClearedSpeed?, Heading?, OfftrackClearance?, RateOfClimbDescend?, DirectRouting?)

fixm_ClearedFlightInformation(Graph, ClearedFlightInformation, ClearedFlightLevel, ClearedSpeed, Heading, OfftrackClearance, RateOfClimbDescend, DirectRouting) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?clearedFlightInformation ?clearedFlightLevel ?clearedSpeed ?heading ?offtrackClearance ?rateOfClimbDescend ?directRouting
WHERE
  { GRAPH ?graph
    {
      ?clearedFlightInformation rdf:type fixm:ClearedFlightInformation .
      OPTIONAL { ?clearedFlightInformation fixm:clearedFlightLevel ?_clearedFlightLevel .
        {
          {
            ?_clearedFlightLevel rdf:value ?clearedFlightLevelValue .
            FILTER ( NOT EXISTS {?_clearedFlightLevel (aixm:uom | fixm:uom | plain:uom) ?clearedFlightLevelUoM})
            BIND(concat(\'val:\',?clearedFlightLevelValue) AS ?clearedFlightLevel)
          }
            UNION
          {
            ?_clearedFlightLevel
              rdf:value ?clearedFlightLevelValue ;
              (aixm:uom | fixm:uom | plain:uom) ?clearedFlightLevelUoM .
            BIND(concat(\'xval:\',STR(?clearedFlightLevelValue),\':\',?clearedFlightLevelUoM) AS ?clearedFlightLevel)
          }
            UNION
          {
           ?_clearedFlightLevel  aixm:nilReason ?clearedFlightLevelNilReason .
           BIND(concat(\'nil:\',?clearedFlightLevelNilReason) AS ?clearedFlightLevel)
          }
        }
      }
      OPTIONAL { ?clearedFlightInformation fixm:clearedSpeed ?_clearedSpeed .
        {
          {
            ?_clearedSpeed rdf:value ?clearedSpeedValue .
            FILTER ( NOT EXISTS {?_clearedSpeed (aixm:uom | fixm:uom | plain:uom) ?clearedSpeedUoM})
            BIND(concat(\'val:\',?clearedSpeedValue) AS ?clearedSpeed)
          }
            UNION
          {
            ?_clearedSpeed
              rdf:value ?clearedSpeedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?clearedSpeedUoM .
            BIND(concat(\'xval:\',STR(?clearedSpeedValue),\':\',?clearedSpeedUoM) AS ?clearedSpeed)
          }
            UNION
          {
           ?_clearedSpeed  aixm:nilReason ?clearedSpeedNilReason .
           BIND(concat(\'nil:\',?clearedSpeedNilReason) AS ?clearedSpeed)
          }
        }
      }
      OPTIONAL { ?clearedFlightInformation fixm:heading ?_heading .
        {
          {
            ?_heading rdf:value ?headingValue .
            FILTER ( NOT EXISTS {?_heading (aixm:uom | fixm:uom | plain:uom) ?headingUoM})
            BIND(concat(\'val:\',?headingValue) AS ?heading)
          }
            UNION
          {
            ?_heading
              rdf:value ?headingValue ;
              (aixm:uom | fixm:uom | plain:uom) ?headingUoM .
            BIND(concat(\'xval:\',STR(?headingValue),\':\',?headingUoM) AS ?heading)
          }
            UNION
          {
           ?_heading  aixm:nilReason ?headingNilReason .
           BIND(concat(\'nil:\',?headingNilReason) AS ?heading)
          }
        }
      }
      OPTIONAL {?clearedFlightInformation fixm:offtrackClearance ?offtrackClearance .}
      OPTIONAL { ?clearedFlightInformation fixm:rateOfClimbDescend ?_rateOfClimbDescend .
        {
          {
            ?_rateOfClimbDescend rdf:value ?rateOfClimbDescendValue .
            FILTER ( NOT EXISTS {?_rateOfClimbDescend (aixm:uom | fixm:uom | plain:uom) ?rateOfClimbDescendUoM})
            BIND(concat(\'val:\',?rateOfClimbDescendValue) AS ?rateOfClimbDescend)
          }
            UNION
          {
            ?_rateOfClimbDescend
              rdf:value ?rateOfClimbDescendValue ;
              (aixm:uom | fixm:uom | plain:uom) ?rateOfClimbDescendUoM .
            BIND(concat(\'xval:\',STR(?rateOfClimbDescendValue),\':\',?rateOfClimbDescendUoM) AS ?rateOfClimbDescend)
          }
            UNION
          {
           ?_rateOfClimbDescend  aixm:nilReason ?rateOfClimbDescendNilReason .
           BIND(concat(\'nil:\',?rateOfClimbDescendNilReason) AS ?rateOfClimbDescend)
          }
        }
      }
      OPTIONAL {?clearedFlightInformation fixm:directRouting ?directRouting .}
    }
  }

      '
,row(Graph,ClearedFlightInformation,ClearedFlightLevel,ClearedSpeed,Heading,OfftrackClearance,RateOfClimbDescend,DirectRouting),[]).

% fixm_TrajectoryRoutePair(Graph, TrajectoryRoutePair, Trajectory?, Route?)

fixm_TrajectoryRoutePair(Graph, TrajectoryRoutePair, Trajectory, Route) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?trajectoryRoutePair ?trajectory ?route
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:TrajectoryRoutePair .
  }
  { GRAPH ?graph
    {
      ?trajectoryRoutePair rdf:type ?SUBCLASS .
      OPTIONAL {?trajectoryRoutePair fixm:trajectory ?trajectory .}
      OPTIONAL {?trajectoryRoutePair fixm:route ?route .}
    }
  }
}

      '
,row(Graph,TrajectoryRoutePair,Trajectory,Route),[]).

% fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit?, UpstreamUnit?, BoundaryCrossingProposed?, BoundaryCrossingCoordinated?, Handoff?, UnitBoundaryIndicator?)

fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit, UpstreamUnit, BoundaryCrossingProposed, BoundaryCrossingCoordinated, Handoff, UnitBoundaryIndicator) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?unitBoundary ?downstreamUnit ?upstreamUnit ?boundaryCrossingProposed ?boundaryCrossingCoordinated ?handoff ?unitBoundaryIndicator
WHERE
  { GRAPH ?graph
    {
      ?unitBoundary rdf:type fixm:UnitBoundary .
      OPTIONAL {?unitBoundary fixm:downstreamUnit ?downstreamUnit .}
      OPTIONAL {?unitBoundary fixm:upstreamUnit ?upstreamUnit .}
      OPTIONAL {?unitBoundary fixm:boundaryCrossingProposed ?boundaryCrossingProposed .}
      OPTIONAL {?unitBoundary fixm:boundaryCrossingCoordinated ?boundaryCrossingCoordinated .}
      OPTIONAL {?unitBoundary fixm:handoff ?handoff .}
      OPTIONAL { ?unitBoundary fixm:unitBoundaryIndicator ?_unitBoundaryIndicator .
        {
          {
            ?_unitBoundaryIndicator rdf:value ?unitBoundaryIndicatorValue .
            FILTER ( NOT EXISTS {?_unitBoundaryIndicator (aixm:uom | fixm:uom | plain:uom) ?unitBoundaryIndicatorUoM})
            BIND(concat(\'val:\',?unitBoundaryIndicatorValue) AS ?unitBoundaryIndicator)
          }
            UNION
          {
            ?_unitBoundaryIndicator
              rdf:value ?unitBoundaryIndicatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?unitBoundaryIndicatorUoM .
            BIND(concat(\'xval:\',STR(?unitBoundaryIndicatorValue),\':\',?unitBoundaryIndicatorUoM) AS ?unitBoundaryIndicator)
          }
            UNION
          {
           ?_unitBoundaryIndicator  aixm:nilReason ?unitBoundaryIndicatorNilReason .
           BIND(concat(\'nil:\',?unitBoundaryIndicatorNilReason) AS ?unitBoundaryIndicator)
          }
        }
      }
    }
  }

      '
,row(Graph,UnitBoundary,DownstreamUnit,UpstreamUnit,BoundaryCrossingProposed,BoundaryCrossingCoordinated,Handoff,UnitBoundaryIndicator),[]).

% aixm_SurfaceContamination(Graph, SurfaceContamination, ObservationTime?, Depth?, FrictionCoefficient?, FrictionEstimation?, FrictionDevice?, ObscuredLights?, FurtherClearanceTime?, FurtherTotalClearance?, NextObservationTime?, Proportion?, CriticalRidge*, Annotation*, Layer*)

aixm_SurfaceContamination(Graph, SurfaceContamination, ObservationTime, Depth, FrictionCoefficient, FrictionEstimation, FrictionDevice, ObscuredLights, FurtherClearanceTime, FurtherTotalClearance, NextObservationTime, Proportion, CriticalRidgeList, AnnotationList, LayerList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?surfaceContamination ?observationTime ?depth ?frictionCoefficient ?frictionEstimation ?frictionDevice ?obscuredLights ?furtherClearanceTime ?furtherTotalClearance ?nextObservationTime ?proportion (GROUP_CONCAT(DISTINCT ?criticalRidge;SEPARATOR=",") AS ?criticalRidgeConcat) (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat) (GROUP_CONCAT(DISTINCT ?layer;SEPARATOR=",") AS ?layerConcat)
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* aixm:SurfaceContamination .
  }
  { GRAPH ?graph
    {
      ?surfaceContamination rdf:type ?SUBCLASS .
      OPTIONAL { ?surfaceContamination aixm:observationTime ?_observationTime .
        {
          {
            ?_observationTime rdf:value ?observationTimeValue .
            FILTER ( NOT EXISTS {?_observationTime (aixm:uom | fixm:uom | plain:uom) ?observationTimeUoM})
            BIND(concat(\'val:\',?observationTimeValue) AS ?observationTime)
          }
            UNION
          {
            ?_observationTime
              rdf:value ?observationTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?observationTimeUoM .
            BIND(concat(\'xval:\',STR(?observationTimeValue),\':\',?observationTimeUoM) AS ?observationTime)
          }
            UNION
          {
           ?_observationTime  aixm:nilReason ?observationTimeNilReason .
           BIND(concat(\'nil:\',?observationTimeNilReason) AS ?observationTime)
          }
        }
      }
      OPTIONAL { ?surfaceContamination aixm:depth ?_depth .
        {
          {
            ?_depth rdf:value ?depthValue .
            FILTER ( NOT EXISTS {?_depth (aixm:uom | fixm:uom | plain:uom) ?depthUoM})
            BIND(concat(\'val:\',?depthValue) AS ?depth)
          }
            UNION
          {
            ?_depth
              rdf:value ?depthValue ;
              (aixm:uom | fixm:uom | plain:uom) ?depthUoM .
            BIND(concat(\'xval:\',STR(?depthValue),\':\',?depthUoM) AS ?depth)
          }
            UNION
          {
           ?_depth  aixm:nilReason ?depthNilReason .
           BIND(concat(\'nil:\',?depthNilReason) AS ?depth)
          }
        }
      }
      OPTIONAL { ?surfaceContamination aixm:frictionCoefficient ?_frictionCoefficient .
        {
          {
            ?_frictionCoefficient rdf:value ?frictionCoefficientValue .
            FILTER ( NOT EXISTS {?_frictionCoefficient (aixm:uom | fixm:uom | plain:uom) ?frictionCoefficientUoM})
            BIND(concat(\'val:\',?frictionCoefficientValue) AS ?frictionCoefficient)
          }
            UNION
          {
            ?_frictionCoefficient
              rdf:value ?frictionCoefficientValue ;
              (aixm:uom | fixm:uom | plain:uom) ?frictionCoefficientUoM .
            BIND(concat(\'xval:\',STR(?frictionCoefficientValue),\':\',?frictionCoefficientUoM) AS ?frictionCoefficient)
          }
            UNION
          {
           ?_frictionCoefficient  aixm:nilReason ?frictionCoefficientNilReason .
           BIND(concat(\'nil:\',?frictionCoefficientNilReason) AS ?frictionCoefficient)
          }
        }
      }
      OPTIONAL { ?surfaceContamination aixm:frictionEstimation ?_frictionEstimation .
        {
          {
            ?_frictionEstimation rdf:value ?frictionEstimationValue .
            FILTER ( NOT EXISTS {?_frictionEstimation (aixm:uom | fixm:uom | plain:uom) ?frictionEstimationUoM})
            BIND(concat(\'val:\',?frictionEstimationValue) AS ?frictionEstimation)
          }
            UNION
          {
            ?_frictionEstimation
              rdf:value ?frictionEstimationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?frictionEstimationUoM .
            BIND(concat(\'xval:\',STR(?frictionEstimationValue),\':\',?frictionEstimationUoM) AS ?frictionEstimation)
          }
            UNION
          {
           ?_frictionEstimation  aixm:nilReason ?frictionEstimationNilReason .
           BIND(concat(\'nil:\',?frictionEstimationNilReason) AS ?frictionEstimation)
          }
        }
      }
      OPTIONAL { ?surfaceContamination aixm:frictionDevice ?_frictionDevice .
        {
          {
            ?_frictionDevice rdf:value ?frictionDeviceValue .
            FILTER ( NOT EXISTS {?_frictionDevice (aixm:uom | fixm:uom | plain:uom) ?frictionDeviceUoM})
            BIND(concat(\'val:\',?frictionDeviceValue) AS ?frictionDevice)
          }
            UNION
          {
            ?_frictionDevice
              rdf:value ?frictionDeviceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?frictionDeviceUoM .
            BIND(concat(\'xval:\',STR(?frictionDeviceValue),\':\',?frictionDeviceUoM) AS ?frictionDevice)
          }
            UNION
          {
           ?_frictionDevice  aixm:nilReason ?frictionDeviceNilReason .
           BIND(concat(\'nil:\',?frictionDeviceNilReason) AS ?frictionDevice)
          }
        }
      }
      OPTIONAL { ?surfaceContamination aixm:obscuredLights ?_obscuredLights .
        {
          {
            ?_obscuredLights rdf:value ?obscuredLightsValue .
            FILTER ( NOT EXISTS {?_obscuredLights (aixm:uom | fixm:uom | plain:uom) ?obscuredLightsUoM})
            BIND(concat(\'val:\',?obscuredLightsValue) AS ?obscuredLights)
          }
            UNION
          {
            ?_obscuredLights
              rdf:value ?obscuredLightsValue ;
              (aixm:uom | fixm:uom | plain:uom) ?obscuredLightsUoM .
            BIND(concat(\'xval:\',STR(?obscuredLightsValue),\':\',?obscuredLightsUoM) AS ?obscuredLights)
          }
            UNION
          {
           ?_obscuredLights  aixm:nilReason ?obscuredLightsNilReason .
           BIND(concat(\'nil:\',?obscuredLightsNilReason) AS ?obscuredLights)
          }
        }
      }
      OPTIONAL { ?surfaceContamination aixm:furtherClearanceTime ?_furtherClearanceTime .
        {
          {
            ?_furtherClearanceTime rdf:value ?furtherClearanceTimeValue .
            FILTER ( NOT EXISTS {?_furtherClearanceTime (aixm:uom | fixm:uom | plain:uom) ?furtherClearanceTimeUoM})
            BIND(concat(\'val:\',?furtherClearanceTimeValue) AS ?furtherClearanceTime)
          }
            UNION
          {
            ?_furtherClearanceTime
              rdf:value ?furtherClearanceTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?furtherClearanceTimeUoM .
            BIND(concat(\'xval:\',STR(?furtherClearanceTimeValue),\':\',?furtherClearanceTimeUoM) AS ?furtherClearanceTime)
          }
            UNION
          {
           ?_furtherClearanceTime  aixm:nilReason ?furtherClearanceTimeNilReason .
           BIND(concat(\'nil:\',?furtherClearanceTimeNilReason) AS ?furtherClearanceTime)
          }
        }
      }
      OPTIONAL { ?surfaceContamination aixm:furtherTotalClearance ?_furtherTotalClearance .
        {
          {
            ?_furtherTotalClearance rdf:value ?furtherTotalClearanceValue .
            FILTER ( NOT EXISTS {?_furtherTotalClearance (aixm:uom | fixm:uom | plain:uom) ?furtherTotalClearanceUoM})
            BIND(concat(\'val:\',?furtherTotalClearanceValue) AS ?furtherTotalClearance)
          }
            UNION
          {
            ?_furtherTotalClearance
              rdf:value ?furtherTotalClearanceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?furtherTotalClearanceUoM .
            BIND(concat(\'xval:\',STR(?furtherTotalClearanceValue),\':\',?furtherTotalClearanceUoM) AS ?furtherTotalClearance)
          }
            UNION
          {
           ?_furtherTotalClearance  aixm:nilReason ?furtherTotalClearanceNilReason .
           BIND(concat(\'nil:\',?furtherTotalClearanceNilReason) AS ?furtherTotalClearance)
          }
        }
      }
      OPTIONAL { ?surfaceContamination aixm:nextObservationTime ?_nextObservationTime .
        {
          {
            ?_nextObservationTime rdf:value ?nextObservationTimeValue .
            FILTER ( NOT EXISTS {?_nextObservationTime (aixm:uom | fixm:uom | plain:uom) ?nextObservationTimeUoM})
            BIND(concat(\'val:\',?nextObservationTimeValue) AS ?nextObservationTime)
          }
            UNION
          {
            ?_nextObservationTime
              rdf:value ?nextObservationTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?nextObservationTimeUoM .
            BIND(concat(\'xval:\',STR(?nextObservationTimeValue),\':\',?nextObservationTimeUoM) AS ?nextObservationTime)
          }
            UNION
          {
           ?_nextObservationTime  aixm:nilReason ?nextObservationTimeNilReason .
           BIND(concat(\'nil:\',?nextObservationTimeNilReason) AS ?nextObservationTime)
          }
        }
      }
      OPTIONAL { ?surfaceContamination aixm:proportion ?_proportion .
        {
          {
            ?_proportion rdf:value ?proportionValue .
            FILTER ( NOT EXISTS {?_proportion (aixm:uom | fixm:uom | plain:uom) ?proportionUoM})
            BIND(concat(\'val:\',?proportionValue) AS ?proportion)
          }
            UNION
          {
            ?_proportion
              rdf:value ?proportionValue ;
              (aixm:uom | fixm:uom | plain:uom) ?proportionUoM .
            BIND(concat(\'xval:\',STR(?proportionValue),\':\',?proportionUoM) AS ?proportion)
          }
            UNION
          {
           ?_proportion  aixm:nilReason ?proportionNilReason .
           BIND(concat(\'nil:\',?proportionNilReason) AS ?proportion)
          }
        }
      }
      OPTIONAL {?surfaceContamination aixm:criticalRidge ?criticalRidge .}
      OPTIONAL {?surfaceContamination aixm:annotation ?annotation .}
      OPTIONAL {?surfaceContamination aixm:layer ?layer .}
    }
  }
}
GROUP BY ?graph ?surfaceContamination ?observationTime ?depth ?frictionCoefficient ?frictionEstimation ?frictionDevice ?obscuredLights ?furtherClearanceTime ?furtherTotalClearance ?nextObservationTime ?proportion

      '
,row(Graph,SurfaceContamination,ObservationTime,Depth,FrictionCoefficient,FrictionEstimation,FrictionDevice,ObscuredLights,FurtherClearanceTime,FurtherTotalClearance,NextObservationTime,Proportion,CriticalRidgeConcat,AnnotationConcat,LayerConcat),[]), convert(CriticalRidgeConcat,CriticalRidgeList), convert(AnnotationConcat,AnnotationList), convert(LayerConcat,LayerList).

% fixm_MeteorologicalData(Graph, MeteorologicalData, Temperature?, WindDirection?, WindSpeed?)

fixm_MeteorologicalData(Graph, MeteorologicalData, Temperature, WindDirection, WindSpeed) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?meteorologicalData ?temperature ?windDirection ?windSpeed
WHERE
  { GRAPH ?graph
    {
      ?meteorologicalData rdf:type fixm:MeteorologicalData .
      OPTIONAL { ?meteorologicalData fixm:temperature ?_temperature .
        {
          {
            ?_temperature rdf:value ?temperatureValue .
            FILTER ( NOT EXISTS {?_temperature (aixm:uom | fixm:uom | plain:uom) ?temperatureUoM})
            BIND(concat(\'val:\',?temperatureValue) AS ?temperature)
          }
            UNION
          {
            ?_temperature
              rdf:value ?temperatureValue ;
              (aixm:uom | fixm:uom | plain:uom) ?temperatureUoM .
            BIND(concat(\'xval:\',STR(?temperatureValue),\':\',?temperatureUoM) AS ?temperature)
          }
            UNION
          {
           ?_temperature  aixm:nilReason ?temperatureNilReason .
           BIND(concat(\'nil:\',?temperatureNilReason) AS ?temperature)
          }
        }
      }
      OPTIONAL { ?meteorologicalData fixm:windDirection ?_windDirection .
        {
          {
            ?_windDirection rdf:value ?windDirectionValue .
            FILTER ( NOT EXISTS {?_windDirection (aixm:uom | fixm:uom | plain:uom) ?windDirectionUoM})
            BIND(concat(\'val:\',?windDirectionValue) AS ?windDirection)
          }
            UNION
          {
            ?_windDirection
              rdf:value ?windDirectionValue ;
              (aixm:uom | fixm:uom | plain:uom) ?windDirectionUoM .
            BIND(concat(\'xval:\',STR(?windDirectionValue),\':\',?windDirectionUoM) AS ?windDirection)
          }
            UNION
          {
           ?_windDirection  aixm:nilReason ?windDirectionNilReason .
           BIND(concat(\'nil:\',?windDirectionNilReason) AS ?windDirection)
          }
        }
      }
      OPTIONAL { ?meteorologicalData fixm:windSpeed ?_windSpeed .
        {
          {
            ?_windSpeed rdf:value ?windSpeedValue .
            FILTER ( NOT EXISTS {?_windSpeed (aixm:uom | fixm:uom | plain:uom) ?windSpeedUoM})
            BIND(concat(\'val:\',?windSpeedValue) AS ?windSpeed)
          }
            UNION
          {
            ?_windSpeed
              rdf:value ?windSpeedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?windSpeedUoM .
            BIND(concat(\'xval:\',STR(?windSpeedValue),\':\',?windSpeedUoM) AS ?windSpeed)
          }
            UNION
          {
           ?_windSpeed  aixm:nilReason ?windSpeedNilReason .
           BIND(concat(\'nil:\',?windSpeedNilReason) AS ?windSpeed)
          }
        }
      }
    }
  }

      '
,row(Graph,MeteorologicalData,Temperature,WindDirection,WindSpeed),[]).

% aixm_OrganisationAuthority(Graph, OrganisationAuthority, TimeSlice*)

aixm_OrganisationAuthority(Graph, OrganisationAuthority, TimeSliceList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?organisationAuthority (GROUP_CONCAT(DISTINCT ?timeSlice;SEPARATOR=",") AS ?timeSliceConcat)
WHERE
  { GRAPH ?graph
    {
      ?organisationAuthority rdf:type aixm:OrganisationAuthority .
      OPTIONAL {?organisationAuthority aixm:timeSlice ?timeSlice .}
    }
  }
GROUP BY ?graph ?organisationAuthority

      '
,row(Graph,OrganisationAuthority,TimeSliceConcat),[]), convert(TimeSliceConcat,TimeSliceList).

% fixm_TelephoneContact(Graph, TelephoneContact, Voice?, Facimile?)

fixm_TelephoneContact(Graph, TelephoneContact, Voice, Facimile) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?telephoneContact ?voice ?facimile
WHERE
  { GRAPH ?graph
    {
      ?telephoneContact rdf:type fixm:TelephoneContact .
      OPTIONAL { ?telephoneContact fixm:voice ?_voice .
        {
          {
            ?_voice rdf:value ?voiceValue .
            FILTER ( NOT EXISTS {?_voice (aixm:uom | fixm:uom | plain:uom) ?voiceUoM})
            BIND(concat(\'val:\',?voiceValue) AS ?voice)
          }
            UNION
          {
            ?_voice
              rdf:value ?voiceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?voiceUoM .
            BIND(concat(\'xval:\',STR(?voiceValue),\':\',?voiceUoM) AS ?voice)
          }
            UNION
          {
           ?_voice  aixm:nilReason ?voiceNilReason .
           BIND(concat(\'nil:\',?voiceNilReason) AS ?voice)
          }
        }
      }
      OPTIONAL { ?telephoneContact fixm:facimile ?_facimile .
        {
          {
            ?_facimile rdf:value ?facimileValue .
            FILTER ( NOT EXISTS {?_facimile (aixm:uom | fixm:uom | plain:uom) ?facimileUoM})
            BIND(concat(\'val:\',?facimileValue) AS ?facimile)
          }
            UNION
          {
            ?_facimile
              rdf:value ?facimileValue ;
              (aixm:uom | fixm:uom | plain:uom) ?facimileUoM .
            BIND(concat(\'xval:\',STR(?facimileValue),\':\',?facimileUoM) AS ?facimile)
          }
            UNION
          {
           ?_facimile  aixm:nilReason ?facimileNilReason .
           BIND(concat(\'nil:\',?facimileNilReason) AS ?facimile)
          }
        }
      }
    }
  }

      '
,row(Graph,TelephoneContact,Voice,Facimile),[]).

% fixm_ShippingInformation(Graph, ShippingInformation, AerodromeOfLoading?, AerodromeOfUnloading?, DangerousGoodsScreeningLocation?, DepartureCountry?, DestinationCountry?, OriginCountry?, ShipmentAuthorizations?, SubsidiaryHazardClassAndDivision?, SupplementaryInformation?, TransferAerodromes*, DeclarationText?, Consignee?, Shipper?)

fixm_ShippingInformation(Graph, ShippingInformation, AerodromeOfLoading, AerodromeOfUnloading, DangerousGoodsScreeningLocation, DepartureCountry, DestinationCountry, OriginCountry, ShipmentAuthorizations, SubsidiaryHazardClassAndDivision, SupplementaryInformation, TransferAerodromesList, DeclarationText, Consignee, Shipper) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?shippingInformation ?aerodromeOfLoading ?aerodromeOfUnloading ?dangerousGoodsScreeningLocation ?departureCountry ?destinationCountry ?originCountry ?shipmentAuthorizations ?subsidiaryHazardClassAndDivision ?supplementaryInformation (GROUP_CONCAT(DISTINCT ?transferAerodromes;SEPARATOR=",") AS ?transferAerodromesConcat) ?declarationText ?consignee ?shipper
WHERE
  { GRAPH ?graph
    {
      ?shippingInformation rdf:type fixm:ShippingInformation .
      OPTIONAL {?shippingInformation fixm:aerodromeOfLoading ?aerodromeOfLoading .}
      OPTIONAL {?shippingInformation fixm:aerodromeOfUnloading ?aerodromeOfUnloading .}
      OPTIONAL { ?shippingInformation fixm:dangerousGoodsScreeningLocation ?_dangerousGoodsScreeningLocation .
        {
          {
            ?_dangerousGoodsScreeningLocation rdf:value ?dangerousGoodsScreeningLocationValue .
            FILTER ( NOT EXISTS {?_dangerousGoodsScreeningLocation (aixm:uom | fixm:uom | plain:uom) ?dangerousGoodsScreeningLocationUoM})
            BIND(concat(\'val:\',?dangerousGoodsScreeningLocationValue) AS ?dangerousGoodsScreeningLocation)
          }
            UNION
          {
            ?_dangerousGoodsScreeningLocation
              rdf:value ?dangerousGoodsScreeningLocationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?dangerousGoodsScreeningLocationUoM .
            BIND(concat(\'xval:\',STR(?dangerousGoodsScreeningLocationValue),\':\',?dangerousGoodsScreeningLocationUoM) AS ?dangerousGoodsScreeningLocation)
          }
            UNION
          {
           ?_dangerousGoodsScreeningLocation  aixm:nilReason ?dangerousGoodsScreeningLocationNilReason .
           BIND(concat(\'nil:\',?dangerousGoodsScreeningLocationNilReason) AS ?dangerousGoodsScreeningLocation)
          }
        }
      }
      OPTIONAL { ?shippingInformation fixm:departureCountry ?_departureCountry .
        {
          {
            ?_departureCountry rdf:value ?departureCountryValue .
            FILTER ( NOT EXISTS {?_departureCountry (aixm:uom | fixm:uom | plain:uom) ?departureCountryUoM})
            BIND(concat(\'val:\',?departureCountryValue) AS ?departureCountry)
          }
            UNION
          {
            ?_departureCountry
              rdf:value ?departureCountryValue ;
              (aixm:uom | fixm:uom | plain:uom) ?departureCountryUoM .
            BIND(concat(\'xval:\',STR(?departureCountryValue),\':\',?departureCountryUoM) AS ?departureCountry)
          }
            UNION
          {
           ?_departureCountry  aixm:nilReason ?departureCountryNilReason .
           BIND(concat(\'nil:\',?departureCountryNilReason) AS ?departureCountry)
          }
        }
      }
      OPTIONAL { ?shippingInformation fixm:destinationCountry ?_destinationCountry .
        {
          {
            ?_destinationCountry rdf:value ?destinationCountryValue .
            FILTER ( NOT EXISTS {?_destinationCountry (aixm:uom | fixm:uom | plain:uom) ?destinationCountryUoM})
            BIND(concat(\'val:\',?destinationCountryValue) AS ?destinationCountry)
          }
            UNION
          {
            ?_destinationCountry
              rdf:value ?destinationCountryValue ;
              (aixm:uom | fixm:uom | plain:uom) ?destinationCountryUoM .
            BIND(concat(\'xval:\',STR(?destinationCountryValue),\':\',?destinationCountryUoM) AS ?destinationCountry)
          }
            UNION
          {
           ?_destinationCountry  aixm:nilReason ?destinationCountryNilReason .
           BIND(concat(\'nil:\',?destinationCountryNilReason) AS ?destinationCountry)
          }
        }
      }
      OPTIONAL { ?shippingInformation fixm:originCountry ?_originCountry .
        {
          {
            ?_originCountry rdf:value ?originCountryValue .
            FILTER ( NOT EXISTS {?_originCountry (aixm:uom | fixm:uom | plain:uom) ?originCountryUoM})
            BIND(concat(\'val:\',?originCountryValue) AS ?originCountry)
          }
            UNION
          {
            ?_originCountry
              rdf:value ?originCountryValue ;
              (aixm:uom | fixm:uom | plain:uom) ?originCountryUoM .
            BIND(concat(\'xval:\',STR(?originCountryValue),\':\',?originCountryUoM) AS ?originCountry)
          }
            UNION
          {
           ?_originCountry  aixm:nilReason ?originCountryNilReason .
           BIND(concat(\'nil:\',?originCountryNilReason) AS ?originCountry)
          }
        }
      }
      OPTIONAL { ?shippingInformation fixm:shipmentAuthorizations ?_shipmentAuthorizations .
        {
          {
            ?_shipmentAuthorizations rdf:value ?shipmentAuthorizationsValue .
            FILTER ( NOT EXISTS {?_shipmentAuthorizations (aixm:uom | fixm:uom | plain:uom) ?shipmentAuthorizationsUoM})
            BIND(concat(\'val:\',?shipmentAuthorizationsValue) AS ?shipmentAuthorizations)
          }
            UNION
          {
            ?_shipmentAuthorizations
              rdf:value ?shipmentAuthorizationsValue ;
              (aixm:uom | fixm:uom | plain:uom) ?shipmentAuthorizationsUoM .
            BIND(concat(\'xval:\',STR(?shipmentAuthorizationsValue),\':\',?shipmentAuthorizationsUoM) AS ?shipmentAuthorizations)
          }
            UNION
          {
           ?_shipmentAuthorizations  aixm:nilReason ?shipmentAuthorizationsNilReason .
           BIND(concat(\'nil:\',?shipmentAuthorizationsNilReason) AS ?shipmentAuthorizations)
          }
        }
      }
      OPTIONAL { ?shippingInformation fixm:subsidiaryHazardClassAndDivision ?_subsidiaryHazardClassAndDivision .
        {
          {
            ?_subsidiaryHazardClassAndDivision rdf:value ?subsidiaryHazardClassAndDivisionValue .
            FILTER ( NOT EXISTS {?_subsidiaryHazardClassAndDivision (aixm:uom | fixm:uom | plain:uom) ?subsidiaryHazardClassAndDivisionUoM})
            BIND(concat(\'val:\',?subsidiaryHazardClassAndDivisionValue) AS ?subsidiaryHazardClassAndDivision)
          }
            UNION
          {
            ?_subsidiaryHazardClassAndDivision
              rdf:value ?subsidiaryHazardClassAndDivisionValue ;
              (aixm:uom | fixm:uom | plain:uom) ?subsidiaryHazardClassAndDivisionUoM .
            BIND(concat(\'xval:\',STR(?subsidiaryHazardClassAndDivisionValue),\':\',?subsidiaryHazardClassAndDivisionUoM) AS ?subsidiaryHazardClassAndDivision)
          }
            UNION
          {
           ?_subsidiaryHazardClassAndDivision  aixm:nilReason ?subsidiaryHazardClassAndDivisionNilReason .
           BIND(concat(\'nil:\',?subsidiaryHazardClassAndDivisionNilReason) AS ?subsidiaryHazardClassAndDivision)
          }
        }
      }
      OPTIONAL { ?shippingInformation fixm:supplementaryInformation ?_supplementaryInformation .
        {
          {
            ?_supplementaryInformation rdf:value ?supplementaryInformationValue .
            FILTER ( NOT EXISTS {?_supplementaryInformation (aixm:uom | fixm:uom | plain:uom) ?supplementaryInformationUoM})
            BIND(concat(\'val:\',?supplementaryInformationValue) AS ?supplementaryInformation)
          }
            UNION
          {
            ?_supplementaryInformation
              rdf:value ?supplementaryInformationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?supplementaryInformationUoM .
            BIND(concat(\'xval:\',STR(?supplementaryInformationValue),\':\',?supplementaryInformationUoM) AS ?supplementaryInformation)
          }
            UNION
          {
           ?_supplementaryInformation  aixm:nilReason ?supplementaryInformationNilReason .
           BIND(concat(\'nil:\',?supplementaryInformationNilReason) AS ?supplementaryInformation)
          }
        }
      }
      OPTIONAL { ?shippingInformation fixm:transferAerodromes ?_transferAerodromes .
        {
          {
            ?_transferAerodromes rdf:value ?transferAerodromesValue .
            FILTER ( NOT EXISTS {?_transferAerodromes (aixm:uom | fixm:uom | plain:uom) ?transferAerodromesUoM})
            BIND(concat(\'val:\',?transferAerodromesValue) AS ?transferAerodromes)
          }
            UNION
          {
            ?_transferAerodromes
              rdf:value ?transferAerodromesValue ;
              (aixm:uom | fixm:uom | plain:uom) ?transferAerodromesUoM .
            BIND(concat(\'xval:\',STR(?transferAerodromesValue),\':\',?transferAerodromesUoM) AS ?transferAerodromes)
          }
            UNION
          {
           ?_transferAerodromes  aixm:nilReason ?transferAerodromesNilReason .
           BIND(concat(\'nil:\',?transferAerodromesNilReason) AS ?transferAerodromes)
          }
        }
      }
      OPTIONAL {?shippingInformation fixm:declarationText ?declarationText .}
      OPTIONAL {?shippingInformation fixm:consignee ?consignee .}
      OPTIONAL {?shippingInformation fixm:shipper ?shipper .}
    }
  }
GROUP BY ?graph ?shippingInformation ?aerodromeOfLoading ?aerodromeOfUnloading ?dangerousGoodsScreeningLocation ?departureCountry ?destinationCountry ?originCountry ?shipmentAuthorizations ?subsidiaryHazardClassAndDivision ?supplementaryInformation ?declarationText ?consignee ?shipper

      '
,row(Graph,ShippingInformation,AerodromeOfLoading,AerodromeOfUnloading,DangerousGoodsScreeningLocation,DepartureCountry,DestinationCountry,OriginCountry,ShipmentAuthorizations,SubsidiaryHazardClassAndDivision,SupplementaryInformation,TransferAerodromesConcat,DeclarationText,Consignee,Shipper),[]), convert(TransferAerodromesConcat,TransferAerodromesList).

% aixm_AirportHeliportContamination(Graph, AirportHeliportContamination)

aixm_AirportHeliportContamination(Graph, AirportHeliportContamination) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?airportHeliportContamination
WHERE
  { GRAPH ?graph
    {
      ?airportHeliportContamination rdf:type aixm:AirportHeliportContamination .
    }
  }

      '
,row(Graph,AirportHeliportContamination),[]).

% fixm_OtherInformation(Graph, OtherInformation, ReplacementFlightPlanIndicator?, RunwayVisualRange?)

fixm_OtherInformation(Graph, OtherInformation, ReplacementFlightPlanIndicator, RunwayVisualRange) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?otherInformation ?replacementFlightPlanIndicator ?runwayVisualRange
WHERE
  { GRAPH ?graph
    {
      ?otherInformation rdf:type fixm:OtherInformation .
      OPTIONAL { ?otherInformation fixm:replacementFlightPlanIndicator ?_replacementFlightPlanIndicator .
        {
          {
            ?_replacementFlightPlanIndicator rdf:value ?replacementFlightPlanIndicatorValue .
            FILTER ( NOT EXISTS {?_replacementFlightPlanIndicator (aixm:uom | fixm:uom | plain:uom) ?replacementFlightPlanIndicatorUoM})
            BIND(concat(\'val:\',?replacementFlightPlanIndicatorValue) AS ?replacementFlightPlanIndicator)
          }
            UNION
          {
            ?_replacementFlightPlanIndicator
              rdf:value ?replacementFlightPlanIndicatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?replacementFlightPlanIndicatorUoM .
            BIND(concat(\'xval:\',STR(?replacementFlightPlanIndicatorValue),\':\',?replacementFlightPlanIndicatorUoM) AS ?replacementFlightPlanIndicator)
          }
            UNION
          {
           ?_replacementFlightPlanIndicator  aixm:nilReason ?replacementFlightPlanIndicatorNilReason .
           BIND(concat(\'nil:\',?replacementFlightPlanIndicatorNilReason) AS ?replacementFlightPlanIndicator)
          }
        }
      }
      OPTIONAL { ?otherInformation fixm:runwayVisualRange ?_runwayVisualRange .
        {
          {
            ?_runwayVisualRange rdf:value ?runwayVisualRangeValue .
            FILTER ( NOT EXISTS {?_runwayVisualRange (aixm:uom | fixm:uom | plain:uom) ?runwayVisualRangeUoM})
            BIND(concat(\'val:\',?runwayVisualRangeValue) AS ?runwayVisualRange)
          }
            UNION
          {
            ?_runwayVisualRange
              rdf:value ?runwayVisualRangeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?runwayVisualRangeUoM .
            BIND(concat(\'xval:\',STR(?runwayVisualRangeValue),\':\',?runwayVisualRangeUoM) AS ?runwayVisualRange)
          }
            UNION
          {
           ?_runwayVisualRange  aixm:nilReason ?runwayVisualRangeNilReason .
           BIND(concat(\'nil:\',?runwayVisualRangeNilReason) AS ?runwayVisualRange)
          }
        }
      }
    }
  }

      '
,row(Graph,OtherInformation,ReplacementFlightPlanIndicator,RunwayVisualRange),[]).

% fixm_DinghyColour(Graph, DinghyColour)

fixm_DinghyColour(Graph, DinghyColour) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?dinghyColour
WHERE
  { GRAPH ?graph
    {
      ?dinghyColour rdf:type fixm:DinghyColour .
    }
  }

      '
,row(Graph,DinghyColour),[]).

% fixm_CpdlcConnection(Graph, CpdlcConnection, ReceivingUnitFrequency?, AtnLogonParameters?, SendCpldcIndicator?, ConnectionStatus?, FrequencyUsage?, Fans1ALogonParameters?)

fixm_CpdlcConnection(Graph, CpdlcConnection, ReceivingUnitFrequency, AtnLogonParameters, SendCpldcIndicator, ConnectionStatus, FrequencyUsage, Fans1ALogonParameters) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?cpdlcConnection ?receivingUnitFrequency ?atnLogonParameters ?sendCpldcIndicator ?connectionStatus ?frequencyUsage ?fans1ALogonParameters
WHERE
  { GRAPH ?graph
    {
      ?cpdlcConnection rdf:type fixm:CpdlcConnection .
      OPTIONAL { ?cpdlcConnection fixm:receivingUnitFrequency ?_receivingUnitFrequency .
        {
          {
            ?_receivingUnitFrequency rdf:value ?receivingUnitFrequencyValue .
            FILTER ( NOT EXISTS {?_receivingUnitFrequency (aixm:uom | fixm:uom | plain:uom) ?receivingUnitFrequencyUoM})
            BIND(concat(\'val:\',?receivingUnitFrequencyValue) AS ?receivingUnitFrequency)
          }
            UNION
          {
            ?_receivingUnitFrequency
              rdf:value ?receivingUnitFrequencyValue ;
              (aixm:uom | fixm:uom | plain:uom) ?receivingUnitFrequencyUoM .
            BIND(concat(\'xval:\',STR(?receivingUnitFrequencyValue),\':\',?receivingUnitFrequencyUoM) AS ?receivingUnitFrequency)
          }
            UNION
          {
           ?_receivingUnitFrequency  aixm:nilReason ?receivingUnitFrequencyNilReason .
           BIND(concat(\'nil:\',?receivingUnitFrequencyNilReason) AS ?receivingUnitFrequency)
          }
        }
      }
      OPTIONAL { ?cpdlcConnection fixm:atnLogonParameters ?_atnLogonParameters .
        {
          {
            ?_atnLogonParameters rdf:value ?atnLogonParametersValue .
            FILTER ( NOT EXISTS {?_atnLogonParameters (aixm:uom | fixm:uom | plain:uom) ?atnLogonParametersUoM})
            BIND(concat(\'val:\',?atnLogonParametersValue) AS ?atnLogonParameters)
          }
            UNION
          {
            ?_atnLogonParameters
              rdf:value ?atnLogonParametersValue ;
              (aixm:uom | fixm:uom | plain:uom) ?atnLogonParametersUoM .
            BIND(concat(\'xval:\',STR(?atnLogonParametersValue),\':\',?atnLogonParametersUoM) AS ?atnLogonParameters)
          }
            UNION
          {
           ?_atnLogonParameters  aixm:nilReason ?atnLogonParametersNilReason .
           BIND(concat(\'nil:\',?atnLogonParametersNilReason) AS ?atnLogonParameters)
          }
        }
      }
      OPTIONAL { ?cpdlcConnection fixm:sendCpldcIndicator ?_sendCpldcIndicator .
        {
          {
            ?_sendCpldcIndicator rdf:value ?sendCpldcIndicatorValue .
            FILTER ( NOT EXISTS {?_sendCpldcIndicator (aixm:uom | fixm:uom | plain:uom) ?sendCpldcIndicatorUoM})
            BIND(concat(\'val:\',?sendCpldcIndicatorValue) AS ?sendCpldcIndicator)
          }
            UNION
          {
            ?_sendCpldcIndicator
              rdf:value ?sendCpldcIndicatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?sendCpldcIndicatorUoM .
            BIND(concat(\'xval:\',STR(?sendCpldcIndicatorValue),\':\',?sendCpldcIndicatorUoM) AS ?sendCpldcIndicator)
          }
            UNION
          {
           ?_sendCpldcIndicator  aixm:nilReason ?sendCpldcIndicatorNilReason .
           BIND(concat(\'nil:\',?sendCpldcIndicatorNilReason) AS ?sendCpldcIndicator)
          }
        }
      }
      OPTIONAL { ?cpdlcConnection fixm:connectionStatus ?_connectionStatus .
        {
          {
            ?_connectionStatus rdf:value ?connectionStatusValue .
            FILTER ( NOT EXISTS {?_connectionStatus (aixm:uom | fixm:uom | plain:uom) ?connectionStatusUoM})
            BIND(concat(\'val:\',?connectionStatusValue) AS ?connectionStatus)
          }
            UNION
          {
            ?_connectionStatus
              rdf:value ?connectionStatusValue ;
              (aixm:uom | fixm:uom | plain:uom) ?connectionStatusUoM .
            BIND(concat(\'xval:\',STR(?connectionStatusValue),\':\',?connectionStatusUoM) AS ?connectionStatus)
          }
            UNION
          {
           ?_connectionStatus  aixm:nilReason ?connectionStatusNilReason .
           BIND(concat(\'nil:\',?connectionStatusNilReason) AS ?connectionStatus)
          }
        }
      }
      OPTIONAL { ?cpdlcConnection fixm:frequencyUsage ?_frequencyUsage .
        {
          {
            ?_frequencyUsage rdf:value ?frequencyUsageValue .
            FILTER ( NOT EXISTS {?_frequencyUsage (aixm:uom | fixm:uom | plain:uom) ?frequencyUsageUoM})
            BIND(concat(\'val:\',?frequencyUsageValue) AS ?frequencyUsage)
          }
            UNION
          {
            ?_frequencyUsage
              rdf:value ?frequencyUsageValue ;
              (aixm:uom | fixm:uom | plain:uom) ?frequencyUsageUoM .
            BIND(concat(\'xval:\',STR(?frequencyUsageValue),\':\',?frequencyUsageUoM) AS ?frequencyUsage)
          }
            UNION
          {
           ?_frequencyUsage  aixm:nilReason ?frequencyUsageNilReason .
           BIND(concat(\'nil:\',?frequencyUsageNilReason) AS ?frequencyUsage)
          }
        }
      }
      OPTIONAL { ?cpdlcConnection fixm:fans1ALogonParameters ?_fans1ALogonParameters .
        {
          {
            ?_fans1ALogonParameters rdf:value ?fans1ALogonParametersValue .
            FILTER ( NOT EXISTS {?_fans1ALogonParameters (aixm:uom | fixm:uom | plain:uom) ?fans1ALogonParametersUoM})
            BIND(concat(\'val:\',?fans1ALogonParametersValue) AS ?fans1ALogonParameters)
          }
            UNION
          {
            ?_fans1ALogonParameters
              rdf:value ?fans1ALogonParametersValue ;
              (aixm:uom | fixm:uom | plain:uom) ?fans1ALogonParametersUoM .
            BIND(concat(\'xval:\',STR(?fans1ALogonParametersValue),\':\',?fans1ALogonParametersUoM) AS ?fans1ALogonParameters)
          }
            UNION
          {
           ?_fans1ALogonParameters  aixm:nilReason ?fans1ALogonParametersNilReason .
           BIND(concat(\'nil:\',?fans1ALogonParametersNilReason) AS ?fans1ALogonParameters)
          }
        }
      }
    }
  }

      '
,row(Graph,CpdlcConnection,ReceivingUnitFrequency,AtnLogonParameters,SendCpldcIndicator,ConnectionStatus,FrequencyUsage,Fans1ALogonParameters),[]).

% aixm_TelephoneContact(Graph, TelephoneContact, Voice?, Facsimile?)

aixm_TelephoneContact(Graph, TelephoneContact, Voice, Facsimile) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?telephoneContact ?voice ?facsimile
WHERE
  { GRAPH ?graph
    {
      ?telephoneContact rdf:type aixm:TelephoneContact .
      OPTIONAL { ?telephoneContact aixm:voice ?_voice .
        {
          {
            ?_voice rdf:value ?voiceValue .
            FILTER ( NOT EXISTS {?_voice (aixm:uom | fixm:uom | plain:uom) ?voiceUoM})
            BIND(concat(\'val:\',?voiceValue) AS ?voice)
          }
            UNION
          {
            ?_voice
              rdf:value ?voiceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?voiceUoM .
            BIND(concat(\'xval:\',STR(?voiceValue),\':\',?voiceUoM) AS ?voice)
          }
            UNION
          {
           ?_voice  aixm:nilReason ?voiceNilReason .
           BIND(concat(\'nil:\',?voiceNilReason) AS ?voice)
          }
        }
      }
      OPTIONAL { ?telephoneContact aixm:facsimile ?_facsimile .
        {
          {
            ?_facsimile rdf:value ?facsimileValue .
            FILTER ( NOT EXISTS {?_facsimile (aixm:uom | fixm:uom | plain:uom) ?facsimileUoM})
            BIND(concat(\'val:\',?facsimileValue) AS ?facsimile)
          }
            UNION
          {
            ?_facsimile
              rdf:value ?facsimileValue ;
              (aixm:uom | fixm:uom | plain:uom) ?facsimileUoM .
            BIND(concat(\'xval:\',STR(?facsimileValue),\':\',?facsimileUoM) AS ?facsimile)
          }
            UNION
          {
           ?_facsimile  aixm:nilReason ?facsimileNilReason .
           BIND(concat(\'nil:\',?facsimileNilReason) AS ?facsimile)
          }
        }
      }
    }
  }

      '
,row(Graph,TelephoneContact,Voice,Facsimile),[]).

% fixm_Route(Graph, Route, AirfileRouteStartTime?, FlightDuration?, InitialCruisingSpeed?, InitialFlightRules?, RequestedAltitude?, RouteText?, EstimatedElapsedTime*, ExpandedRoute?, ClimbSchedule?, DescentSchedule?, Segment*)

fixm_Route(Graph, Route, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTimeList, ExpandedRoute, ClimbSchedule, DescentSchedule, SegmentList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?route ?airfileRouteStartTime ?flightDuration ?initialCruisingSpeed ?initialFlightRules ?requestedAltitude ?routeText (GROUP_CONCAT(DISTINCT ?estimatedElapsedTime;SEPARATOR=",") AS ?estimatedElapsedTimeConcat) ?expandedRoute ?climbSchedule ?descentSchedule (GROUP_CONCAT(DISTINCT ?segment;SEPARATOR=",") AS ?segmentConcat)
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:Route .
  }
  { GRAPH ?graph
    {
      ?route rdf:type ?SUBCLASS .
      OPTIONAL { ?route fixm:airfileRouteStartTime ?_airfileRouteStartTime .
        {
          {
            ?_airfileRouteStartTime rdf:value ?airfileRouteStartTimeValue .
            FILTER ( NOT EXISTS {?_airfileRouteStartTime (aixm:uom | fixm:uom | plain:uom) ?airfileRouteStartTimeUoM})
            BIND(concat(\'val:\',?airfileRouteStartTimeValue) AS ?airfileRouteStartTime)
          }
            UNION
          {
            ?_airfileRouteStartTime
              rdf:value ?airfileRouteStartTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?airfileRouteStartTimeUoM .
            BIND(concat(\'xval:\',STR(?airfileRouteStartTimeValue),\':\',?airfileRouteStartTimeUoM) AS ?airfileRouteStartTime)
          }
            UNION
          {
           ?_airfileRouteStartTime  aixm:nilReason ?airfileRouteStartTimeNilReason .
           BIND(concat(\'nil:\',?airfileRouteStartTimeNilReason) AS ?airfileRouteStartTime)
          }
        }
      }
      OPTIONAL { ?route fixm:flightDuration ?_flightDuration .
        {
          {
            ?_flightDuration rdf:value ?flightDurationValue .
            FILTER ( NOT EXISTS {?_flightDuration (aixm:uom | fixm:uom | plain:uom) ?flightDurationUoM})
            BIND(concat(\'val:\',?flightDurationValue) AS ?flightDuration)
          }
            UNION
          {
            ?_flightDuration
              rdf:value ?flightDurationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?flightDurationUoM .
            BIND(concat(\'xval:\',STR(?flightDurationValue),\':\',?flightDurationUoM) AS ?flightDuration)
          }
            UNION
          {
           ?_flightDuration  aixm:nilReason ?flightDurationNilReason .
           BIND(concat(\'nil:\',?flightDurationNilReason) AS ?flightDuration)
          }
        }
      }
      OPTIONAL { ?route fixm:initialCruisingSpeed ?_initialCruisingSpeed .
        {
          {
            ?_initialCruisingSpeed rdf:value ?initialCruisingSpeedValue .
            FILTER ( NOT EXISTS {?_initialCruisingSpeed (aixm:uom | fixm:uom | plain:uom) ?initialCruisingSpeedUoM})
            BIND(concat(\'val:\',?initialCruisingSpeedValue) AS ?initialCruisingSpeed)
          }
            UNION
          {
            ?_initialCruisingSpeed
              rdf:value ?initialCruisingSpeedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?initialCruisingSpeedUoM .
            BIND(concat(\'xval:\',STR(?initialCruisingSpeedValue),\':\',?initialCruisingSpeedUoM) AS ?initialCruisingSpeed)
          }
            UNION
          {
           ?_initialCruisingSpeed  aixm:nilReason ?initialCruisingSpeedNilReason .
           BIND(concat(\'nil:\',?initialCruisingSpeedNilReason) AS ?initialCruisingSpeed)
          }
        }
      }
      OPTIONAL { ?route fixm:initialFlightRules ?_initialFlightRules .
        {
          {
            ?_initialFlightRules rdf:value ?initialFlightRulesValue .
            FILTER ( NOT EXISTS {?_initialFlightRules (aixm:uom | fixm:uom | plain:uom) ?initialFlightRulesUoM})
            BIND(concat(\'val:\',?initialFlightRulesValue) AS ?initialFlightRules)
          }
            UNION
          {
            ?_initialFlightRules
              rdf:value ?initialFlightRulesValue ;
              (aixm:uom | fixm:uom | plain:uom) ?initialFlightRulesUoM .
            BIND(concat(\'xval:\',STR(?initialFlightRulesValue),\':\',?initialFlightRulesUoM) AS ?initialFlightRules)
          }
            UNION
          {
           ?_initialFlightRules  aixm:nilReason ?initialFlightRulesNilReason .
           BIND(concat(\'nil:\',?initialFlightRulesNilReason) AS ?initialFlightRules)
          }
        }
      }
      OPTIONAL { ?route fixm:requestedAltitude ?_requestedAltitude .
        {
          {
            ?_requestedAltitude rdf:value ?requestedAltitudeValue .
            FILTER ( NOT EXISTS {?_requestedAltitude (aixm:uom | fixm:uom | plain:uom) ?requestedAltitudeUoM})
            BIND(concat(\'val:\',?requestedAltitudeValue) AS ?requestedAltitude)
          }
            UNION
          {
            ?_requestedAltitude
              rdf:value ?requestedAltitudeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?requestedAltitudeUoM .
            BIND(concat(\'xval:\',STR(?requestedAltitudeValue),\':\',?requestedAltitudeUoM) AS ?requestedAltitude)
          }
            UNION
          {
           ?_requestedAltitude  aixm:nilReason ?requestedAltitudeNilReason .
           BIND(concat(\'nil:\',?requestedAltitudeNilReason) AS ?requestedAltitude)
          }
        }
      }
      OPTIONAL { ?route fixm:routeText ?_routeText .
        {
          {
            ?_routeText rdf:value ?routeTextValue .
            FILTER ( NOT EXISTS {?_routeText (aixm:uom | fixm:uom | plain:uom) ?routeTextUoM})
            BIND(concat(\'val:\',?routeTextValue) AS ?routeText)
          }
            UNION
          {
            ?_routeText
              rdf:value ?routeTextValue ;
              (aixm:uom | fixm:uom | plain:uom) ?routeTextUoM .
            BIND(concat(\'xval:\',STR(?routeTextValue),\':\',?routeTextUoM) AS ?routeText)
          }
            UNION
          {
           ?_routeText  aixm:nilReason ?routeTextNilReason .
           BIND(concat(\'nil:\',?routeTextNilReason) AS ?routeText)
          }
        }
      }
      OPTIONAL {?route fixm:estimatedElapsedTime ?estimatedElapsedTime .}
      OPTIONAL {?route fixm:expandedRoute ?expandedRoute .}
      OPTIONAL {?route fixm:climbSchedule ?climbSchedule .}
      OPTIONAL {?route fixm:descentSchedule ?descentSchedule .}
      OPTIONAL {?route fixm:segment ?segment .}
    }
  }
}
GROUP BY ?graph ?route ?airfileRouteStartTime ?flightDuration ?initialCruisingSpeed ?initialFlightRules ?requestedAltitude ?routeText ?expandedRoute ?climbSchedule ?descentSchedule

      '
,row(Graph,Route,AirfileRouteStartTime,FlightDuration,InitialCruisingSpeed,InitialFlightRules,RequestedAltitude,RouteText,EstimatedElapsedTimeConcat,ExpandedRoute,ClimbSchedule,DescentSchedule,SegmentConcat),[]), convert(EstimatedElapsedTimeConcat,EstimatedElapsedTimeList), convert(SegmentConcat,SegmentList).

% fixm_Person(Graph, Person, Name?, Contact?)

fixm_Person(Graph, Person, Name, Contact) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?person ?name ?contact
WHERE
  { GRAPH ?graph
    {
      ?person rdf:type fixm:Person .
      OPTIONAL { ?person fixm:name ?_name .
        {
          {
            ?_name rdf:value ?nameValue .
            FILTER ( NOT EXISTS {?_name (aixm:uom | fixm:uom | plain:uom) ?nameUoM})
            BIND(concat(\'val:\',?nameValue) AS ?name)
          }
            UNION
          {
            ?_name
              rdf:value ?nameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?nameUoM .
            BIND(concat(\'xval:\',STR(?nameValue),\':\',?nameUoM) AS ?name)
          }
            UNION
          {
           ?_name  aixm:nilReason ?nameNilReason .
           BIND(concat(\'nil:\',?nameNilReason) AS ?name)
          }
        }
      }
      OPTIONAL {?person fixm:contact ?contact .}
    }
  }

      '
,row(Graph,Person,Name,Contact),[]).

% fixm_EfplFlight(Graph, EfplFlight, IfplId?, TotalEstimatedElapsedTime?, AerodromesOfDestination?, EfplSpecialHandling?, EfplFiledTrajectory?, EfplAcceptedTrajectory?, OtherInformation?, FlightPerformanceData?)

fixm_EfplFlight(Graph, EfplFlight, IfplId, TotalEstimatedElapsedTime, AerodromesOfDestination, EfplSpecialHandling, EfplFiledTrajectory, EfplAcceptedTrajectory, OtherInformation, FlightPerformanceData) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?efplFlight ?ifplId ?totalEstimatedElapsedTime ?aerodromesOfDestination ?efplSpecialHandling ?efplFiledTrajectory ?efplAcceptedTrajectory ?otherInformation ?flightPerformanceData
WHERE
  { GRAPH ?graph
    {
      ?efplFlight rdf:type fixm:EfplFlight .
      OPTIONAL { ?efplFlight fixm:ifplId ?_ifplId .
        {
          {
            ?_ifplId rdf:value ?ifplIdValue .
            FILTER ( NOT EXISTS {?_ifplId (aixm:uom | fixm:uom | plain:uom) ?ifplIdUoM})
            BIND(concat(\'val:\',?ifplIdValue) AS ?ifplId)
          }
            UNION
          {
            ?_ifplId
              rdf:value ?ifplIdValue ;
              (aixm:uom | fixm:uom | plain:uom) ?ifplIdUoM .
            BIND(concat(\'xval:\',STR(?ifplIdValue),\':\',?ifplIdUoM) AS ?ifplId)
          }
            UNION
          {
           ?_ifplId  aixm:nilReason ?ifplIdNilReason .
           BIND(concat(\'nil:\',?ifplIdNilReason) AS ?ifplId)
          }
        }
      }
      OPTIONAL { ?efplFlight fixm:totalEstimatedElapsedTime ?_totalEstimatedElapsedTime .
        {
          {
            ?_totalEstimatedElapsedTime rdf:value ?totalEstimatedElapsedTimeValue .
            FILTER ( NOT EXISTS {?_totalEstimatedElapsedTime (aixm:uom | fixm:uom | plain:uom) ?totalEstimatedElapsedTimeUoM})
            BIND(concat(\'val:\',?totalEstimatedElapsedTimeValue) AS ?totalEstimatedElapsedTime)
          }
            UNION
          {
            ?_totalEstimatedElapsedTime
              rdf:value ?totalEstimatedElapsedTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?totalEstimatedElapsedTimeUoM .
            BIND(concat(\'xval:\',STR(?totalEstimatedElapsedTimeValue),\':\',?totalEstimatedElapsedTimeUoM) AS ?totalEstimatedElapsedTime)
          }
            UNION
          {
           ?_totalEstimatedElapsedTime  aixm:nilReason ?totalEstimatedElapsedTimeNilReason .
           BIND(concat(\'nil:\',?totalEstimatedElapsedTimeNilReason) AS ?totalEstimatedElapsedTime)
          }
        }
      }
      OPTIONAL {?efplFlight fixm:aerodromesOfDestination ?aerodromesOfDestination .}
      OPTIONAL { ?efplFlight fixm:efplSpecialHandling ?_efplSpecialHandling .
        {
          {
            ?_efplSpecialHandling rdf:value ?efplSpecialHandlingValue .
            FILTER ( NOT EXISTS {?_efplSpecialHandling (aixm:uom | fixm:uom | plain:uom) ?efplSpecialHandlingUoM})
            BIND(concat(\'val:\',?efplSpecialHandlingValue) AS ?efplSpecialHandling)
          }
            UNION
          {
            ?_efplSpecialHandling
              rdf:value ?efplSpecialHandlingValue ;
              (aixm:uom | fixm:uom | plain:uom) ?efplSpecialHandlingUoM .
            BIND(concat(\'xval:\',STR(?efplSpecialHandlingValue),\':\',?efplSpecialHandlingUoM) AS ?efplSpecialHandling)
          }
            UNION
          {
           ?_efplSpecialHandling  aixm:nilReason ?efplSpecialHandlingNilReason .
           BIND(concat(\'nil:\',?efplSpecialHandlingNilReason) AS ?efplSpecialHandling)
          }
        }
      }
      OPTIONAL {?efplFlight fixm:efplFiledTrajectory ?efplFiledTrajectory .}
      OPTIONAL {?efplFlight fixm:efplAcceptedTrajectory ?efplAcceptedTrajectory .}
      OPTIONAL {?efplFlight fixm:otherInformation ?otherInformation .}
      OPTIONAL {?efplFlight fixm:flightPerformanceData ?flightPerformanceData .}
    }
  }

      '
,row(Graph,EfplFlight,IfplId,TotalEstimatedElapsedTime,AerodromesOfDestination,EfplSpecialHandling,EfplFiledTrajectory,EfplAcceptedTrajectory,OtherInformation,FlightPerformanceData),[]).

% fixm_Originator(Graph, Originator)

fixm_Originator(Graph, Originator) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?originator
WHERE
  { GRAPH ?graph
    {
      ?originator rdf:type fixm:Originator .
    }
  }

      '
,row(Graph,Originator),[]).

% fixm_FlightStatus(Graph, FlightStatus, AirborneHold?, Airfile?, Accepted?, FlightCycle?, MissedApproach?, Suspended?)

fixm_FlightStatus(Graph, FlightStatus, AirborneHold, Airfile, Accepted, FlightCycle, MissedApproach, Suspended) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?flightStatus ?airborneHold ?airfile ?accepted ?flightCycle ?missedApproach ?suspended
WHERE
  { GRAPH ?graph
    {
      ?flightStatus rdf:type fixm:FlightStatus .
      OPTIONAL { ?flightStatus fixm:airborneHold ?_airborneHold .
        {
          {
            ?_airborneHold rdf:value ?airborneHoldValue .
            FILTER ( NOT EXISTS {?_airborneHold (aixm:uom | fixm:uom | plain:uom) ?airborneHoldUoM})
            BIND(concat(\'val:\',?airborneHoldValue) AS ?airborneHold)
          }
            UNION
          {
            ?_airborneHold
              rdf:value ?airborneHoldValue ;
              (aixm:uom | fixm:uom | plain:uom) ?airborneHoldUoM .
            BIND(concat(\'xval:\',STR(?airborneHoldValue),\':\',?airborneHoldUoM) AS ?airborneHold)
          }
            UNION
          {
           ?_airborneHold  aixm:nilReason ?airborneHoldNilReason .
           BIND(concat(\'nil:\',?airborneHoldNilReason) AS ?airborneHold)
          }
        }
      }
      OPTIONAL { ?flightStatus fixm:airfile ?_airfile .
        {
          {
            ?_airfile rdf:value ?airfileValue .
            FILTER ( NOT EXISTS {?_airfile (aixm:uom | fixm:uom | plain:uom) ?airfileUoM})
            BIND(concat(\'val:\',?airfileValue) AS ?airfile)
          }
            UNION
          {
            ?_airfile
              rdf:value ?airfileValue ;
              (aixm:uom | fixm:uom | plain:uom) ?airfileUoM .
            BIND(concat(\'xval:\',STR(?airfileValue),\':\',?airfileUoM) AS ?airfile)
          }
            UNION
          {
           ?_airfile  aixm:nilReason ?airfileNilReason .
           BIND(concat(\'nil:\',?airfileNilReason) AS ?airfile)
          }
        }
      }
      OPTIONAL { ?flightStatus fixm:accepted ?_accepted .
        {
          {
            ?_accepted rdf:value ?acceptedValue .
            FILTER ( NOT EXISTS {?_accepted (aixm:uom | fixm:uom | plain:uom) ?acceptedUoM})
            BIND(concat(\'val:\',?acceptedValue) AS ?accepted)
          }
            UNION
          {
            ?_accepted
              rdf:value ?acceptedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?acceptedUoM .
            BIND(concat(\'xval:\',STR(?acceptedValue),\':\',?acceptedUoM) AS ?accepted)
          }
            UNION
          {
           ?_accepted  aixm:nilReason ?acceptedNilReason .
           BIND(concat(\'nil:\',?acceptedNilReason) AS ?accepted)
          }
        }
      }
      OPTIONAL { ?flightStatus fixm:flightCycle ?_flightCycle .
        {
          {
            ?_flightCycle rdf:value ?flightCycleValue .
            FILTER ( NOT EXISTS {?_flightCycle (aixm:uom | fixm:uom | plain:uom) ?flightCycleUoM})
            BIND(concat(\'val:\',?flightCycleValue) AS ?flightCycle)
          }
            UNION
          {
            ?_flightCycle
              rdf:value ?flightCycleValue ;
              (aixm:uom | fixm:uom | plain:uom) ?flightCycleUoM .
            BIND(concat(\'xval:\',STR(?flightCycleValue),\':\',?flightCycleUoM) AS ?flightCycle)
          }
            UNION
          {
           ?_flightCycle  aixm:nilReason ?flightCycleNilReason .
           BIND(concat(\'nil:\',?flightCycleNilReason) AS ?flightCycle)
          }
        }
      }
      OPTIONAL { ?flightStatus fixm:missedApproach ?_missedApproach .
        {
          {
            ?_missedApproach rdf:value ?missedApproachValue .
            FILTER ( NOT EXISTS {?_missedApproach (aixm:uom | fixm:uom | plain:uom) ?missedApproachUoM})
            BIND(concat(\'val:\',?missedApproachValue) AS ?missedApproach)
          }
            UNION
          {
            ?_missedApproach
              rdf:value ?missedApproachValue ;
              (aixm:uom | fixm:uom | plain:uom) ?missedApproachUoM .
            BIND(concat(\'xval:\',STR(?missedApproachValue),\':\',?missedApproachUoM) AS ?missedApproach)
          }
            UNION
          {
           ?_missedApproach  aixm:nilReason ?missedApproachNilReason .
           BIND(concat(\'nil:\',?missedApproachNilReason) AS ?missedApproach)
          }
        }
      }
      OPTIONAL { ?flightStatus fixm:suspended ?_suspended .
        {
          {
            ?_suspended rdf:value ?suspendedValue .
            FILTER ( NOT EXISTS {?_suspended (aixm:uom | fixm:uom | plain:uom) ?suspendedUoM})
            BIND(concat(\'val:\',?suspendedValue) AS ?suspended)
          }
            UNION
          {
            ?_suspended
              rdf:value ?suspendedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?suspendedUoM .
            BIND(concat(\'xval:\',STR(?suspendedValue),\':\',?suspendedUoM) AS ?suspended)
          }
            UNION
          {
           ?_suspended  aixm:nilReason ?suspendedNilReason .
           BIND(concat(\'nil:\',?suspendedNilReason) AS ?suspended)
          }
        }
      }
    }
  }

      '
,row(Graph,FlightStatus,AirborneHold,Airfile,Accepted,FlightCycle,MissedApproach,Suspended),[]).

% fixm_IdentifiedUnitReference(Graph, IdentifiedUnitReference, UnitIdentifier?)

fixm_IdentifiedUnitReference(Graph, IdentifiedUnitReference, UnitIdentifier) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?identifiedUnitReference ?unitIdentifier
WHERE
  { GRAPH ?graph
    {
      ?identifiedUnitReference rdf:type fixm:IdentifiedUnitReference .
      OPTIONAL { ?identifiedUnitReference fixm:unitIdentifier ?_unitIdentifier .
        {
          {
            ?_unitIdentifier rdf:value ?unitIdentifierValue .
            FILTER ( NOT EXISTS {?_unitIdentifier (aixm:uom | fixm:uom | plain:uom) ?unitIdentifierUoM})
            BIND(concat(\'val:\',?unitIdentifierValue) AS ?unitIdentifier)
          }
            UNION
          {
            ?_unitIdentifier
              rdf:value ?unitIdentifierValue ;
              (aixm:uom | fixm:uom | plain:uom) ?unitIdentifierUoM .
            BIND(concat(\'xval:\',STR(?unitIdentifierValue),\':\',?unitIdentifierUoM) AS ?unitIdentifier)
          }
            UNION
          {
           ?_unitIdentifier  aixm:nilReason ?unitIdentifierNilReason .
           BIND(concat(\'nil:\',?unitIdentifierNilReason) AS ?unitIdentifier)
          }
        }
      }
    }
  }

      '
,row(Graph,IdentifiedUnitReference,UnitIdentifier),[]).

% fixm_Radionuclide(Graph, Radionuclide, PhysicalChemicalForm?, RadionuclideId?, RadionuclideName?, LowDispersibleMaterialIndicator?, Activity?, SpecialFormIndicator?)

fixm_Radionuclide(Graph, Radionuclide, PhysicalChemicalForm, RadionuclideId, RadionuclideName, LowDispersibleMaterialIndicator, Activity, SpecialFormIndicator) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?radionuclide ?physicalChemicalForm ?radionuclideId ?radionuclideName ?lowDispersibleMaterialIndicator ?activity ?specialFormIndicator
WHERE
  { GRAPH ?graph
    {
      ?radionuclide rdf:type fixm:Radionuclide .
      OPTIONAL { ?radionuclide fixm:physicalChemicalForm ?_physicalChemicalForm .
        {
          {
            ?_physicalChemicalForm rdf:value ?physicalChemicalFormValue .
            FILTER ( NOT EXISTS {?_physicalChemicalForm (aixm:uom | fixm:uom | plain:uom) ?physicalChemicalFormUoM})
            BIND(concat(\'val:\',?physicalChemicalFormValue) AS ?physicalChemicalForm)
          }
            UNION
          {
            ?_physicalChemicalForm
              rdf:value ?physicalChemicalFormValue ;
              (aixm:uom | fixm:uom | plain:uom) ?physicalChemicalFormUoM .
            BIND(concat(\'xval:\',STR(?physicalChemicalFormValue),\':\',?physicalChemicalFormUoM) AS ?physicalChemicalForm)
          }
            UNION
          {
           ?_physicalChemicalForm  aixm:nilReason ?physicalChemicalFormNilReason .
           BIND(concat(\'nil:\',?physicalChemicalFormNilReason) AS ?physicalChemicalForm)
          }
        }
      }
      OPTIONAL { ?radionuclide fixm:radionuclideId ?_radionuclideId .
        {
          {
            ?_radionuclideId rdf:value ?radionuclideIdValue .
            FILTER ( NOT EXISTS {?_radionuclideId (aixm:uom | fixm:uom | plain:uom) ?radionuclideIdUoM})
            BIND(concat(\'val:\',?radionuclideIdValue) AS ?radionuclideId)
          }
            UNION
          {
            ?_radionuclideId
              rdf:value ?radionuclideIdValue ;
              (aixm:uom | fixm:uom | plain:uom) ?radionuclideIdUoM .
            BIND(concat(\'xval:\',STR(?radionuclideIdValue),\':\',?radionuclideIdUoM) AS ?radionuclideId)
          }
            UNION
          {
           ?_radionuclideId  aixm:nilReason ?radionuclideIdNilReason .
           BIND(concat(\'nil:\',?radionuclideIdNilReason) AS ?radionuclideId)
          }
        }
      }
      OPTIONAL { ?radionuclide fixm:radionuclideName ?_radionuclideName .
        {
          {
            ?_radionuclideName rdf:value ?radionuclideNameValue .
            FILTER ( NOT EXISTS {?_radionuclideName (aixm:uom | fixm:uom | plain:uom) ?radionuclideNameUoM})
            BIND(concat(\'val:\',?radionuclideNameValue) AS ?radionuclideName)
          }
            UNION
          {
            ?_radionuclideName
              rdf:value ?radionuclideNameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?radionuclideNameUoM .
            BIND(concat(\'xval:\',STR(?radionuclideNameValue),\':\',?radionuclideNameUoM) AS ?radionuclideName)
          }
            UNION
          {
           ?_radionuclideName  aixm:nilReason ?radionuclideNameNilReason .
           BIND(concat(\'nil:\',?radionuclideNameNilReason) AS ?radionuclideName)
          }
        }
      }
      OPTIONAL { ?radionuclide fixm:lowDispersibleMaterialIndicator ?_lowDispersibleMaterialIndicator .
        {
          {
            ?_lowDispersibleMaterialIndicator rdf:value ?lowDispersibleMaterialIndicatorValue .
            FILTER ( NOT EXISTS {?_lowDispersibleMaterialIndicator (aixm:uom | fixm:uom | plain:uom) ?lowDispersibleMaterialIndicatorUoM})
            BIND(concat(\'val:\',?lowDispersibleMaterialIndicatorValue) AS ?lowDispersibleMaterialIndicator)
          }
            UNION
          {
            ?_lowDispersibleMaterialIndicator
              rdf:value ?lowDispersibleMaterialIndicatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?lowDispersibleMaterialIndicatorUoM .
            BIND(concat(\'xval:\',STR(?lowDispersibleMaterialIndicatorValue),\':\',?lowDispersibleMaterialIndicatorUoM) AS ?lowDispersibleMaterialIndicator)
          }
            UNION
          {
           ?_lowDispersibleMaterialIndicator  aixm:nilReason ?lowDispersibleMaterialIndicatorNilReason .
           BIND(concat(\'nil:\',?lowDispersibleMaterialIndicatorNilReason) AS ?lowDispersibleMaterialIndicator)
          }
        }
      }
      OPTIONAL { ?radionuclide fixm:activity ?_activity .
        {
          {
            ?_activity rdf:value ?activityValue .
            FILTER ( NOT EXISTS {?_activity (aixm:uom | fixm:uom | plain:uom) ?activityUoM})
            BIND(concat(\'val:\',?activityValue) AS ?activity)
          }
            UNION
          {
            ?_activity
              rdf:value ?activityValue ;
              (aixm:uom | fixm:uom | plain:uom) ?activityUoM .
            BIND(concat(\'xval:\',STR(?activityValue),\':\',?activityUoM) AS ?activity)
          }
            UNION
          {
           ?_activity  aixm:nilReason ?activityNilReason .
           BIND(concat(\'nil:\',?activityNilReason) AS ?activity)
          }
        }
      }
      OPTIONAL { ?radionuclide fixm:specialFormIndicator ?_specialFormIndicator .
        {
          {
            ?_specialFormIndicator rdf:value ?specialFormIndicatorValue .
            FILTER ( NOT EXISTS {?_specialFormIndicator (aixm:uom | fixm:uom | plain:uom) ?specialFormIndicatorUoM})
            BIND(concat(\'val:\',?specialFormIndicatorValue) AS ?specialFormIndicator)
          }
            UNION
          {
            ?_specialFormIndicator
              rdf:value ?specialFormIndicatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?specialFormIndicatorUoM .
            BIND(concat(\'xval:\',STR(?specialFormIndicatorValue),\':\',?specialFormIndicatorUoM) AS ?specialFormIndicator)
          }
            UNION
          {
           ?_specialFormIndicator  aixm:nilReason ?specialFormIndicatorNilReason .
           BIND(concat(\'nil:\',?specialFormIndicatorNilReason) AS ?specialFormIndicator)
          }
        }
      }
    }
  }

      '
,row(Graph,Radionuclide,PhysicalChemicalForm,RadionuclideId,RadionuclideName,LowDispersibleMaterialIndicator,Activity,SpecialFormIndicator),[]).

% aixm_OnlineContact(Graph, OnlineContact, Network?, Linkage?, Protocol?, EMail?)

aixm_OnlineContact(Graph, OnlineContact, Network, Linkage, Protocol, EMail) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?onlineContact ?network ?linkage ?protocol ?eMail
WHERE
  { GRAPH ?graph
    {
      ?onlineContact rdf:type aixm:OnlineContact .
      OPTIONAL { ?onlineContact aixm:network ?_network .
        {
          {
            ?_network rdf:value ?networkValue .
            FILTER ( NOT EXISTS {?_network (aixm:uom | fixm:uom | plain:uom) ?networkUoM})
            BIND(concat(\'val:\',?networkValue) AS ?network)
          }
            UNION
          {
            ?_network
              rdf:value ?networkValue ;
              (aixm:uom | fixm:uom | plain:uom) ?networkUoM .
            BIND(concat(\'xval:\',STR(?networkValue),\':\',?networkUoM) AS ?network)
          }
            UNION
          {
           ?_network  aixm:nilReason ?networkNilReason .
           BIND(concat(\'nil:\',?networkNilReason) AS ?network)
          }
        }
      }
      OPTIONAL { ?onlineContact aixm:linkage ?_linkage .
        {
          {
            ?_linkage rdf:value ?linkageValue .
            FILTER ( NOT EXISTS {?_linkage (aixm:uom | fixm:uom | plain:uom) ?linkageUoM})
            BIND(concat(\'val:\',?linkageValue) AS ?linkage)
          }
            UNION
          {
            ?_linkage
              rdf:value ?linkageValue ;
              (aixm:uom | fixm:uom | plain:uom) ?linkageUoM .
            BIND(concat(\'xval:\',STR(?linkageValue),\':\',?linkageUoM) AS ?linkage)
          }
            UNION
          {
           ?_linkage  aixm:nilReason ?linkageNilReason .
           BIND(concat(\'nil:\',?linkageNilReason) AS ?linkage)
          }
        }
      }
      OPTIONAL { ?onlineContact aixm:protocol ?_protocol .
        {
          {
            ?_protocol rdf:value ?protocolValue .
            FILTER ( NOT EXISTS {?_protocol (aixm:uom | fixm:uom | plain:uom) ?protocolUoM})
            BIND(concat(\'val:\',?protocolValue) AS ?protocol)
          }
            UNION
          {
            ?_protocol
              rdf:value ?protocolValue ;
              (aixm:uom | fixm:uom | plain:uom) ?protocolUoM .
            BIND(concat(\'xval:\',STR(?protocolValue),\':\',?protocolUoM) AS ?protocol)
          }
            UNION
          {
           ?_protocol  aixm:nilReason ?protocolNilReason .
           BIND(concat(\'nil:\',?protocolNilReason) AS ?protocol)
          }
        }
      }
      OPTIONAL { ?onlineContact aixm:eMail ?_eMail .
        {
          {
            ?_eMail rdf:value ?eMailValue .
            FILTER ( NOT EXISTS {?_eMail (aixm:uom | fixm:uom | plain:uom) ?eMailUoM})
            BIND(concat(\'val:\',?eMailValue) AS ?eMail)
          }
            UNION
          {
            ?_eMail
              rdf:value ?eMailValue ;
              (aixm:uom | fixm:uom | plain:uom) ?eMailUoM .
            BIND(concat(\'xval:\',STR(?eMailValue),\':\',?eMailUoM) AS ?eMail)
          }
            UNION
          {
           ?_eMail  aixm:nilReason ?eMailNilReason .
           BIND(concat(\'nil:\',?eMailNilReason) AS ?eMail)
          }
        }
      }
    }
  }

      '
,row(Graph,OnlineContact,Network,Linkage,Protocol,EMail),[]).

% fixm_StructuredPostalAddress(Graph, StructuredPostalAddress)

fixm_StructuredPostalAddress(Graph, StructuredPostalAddress) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?structuredPostalAddress
WHERE
  { GRAPH ?graph
    {
      ?structuredPostalAddress rdf:type fixm:StructuredPostalAddress .
    }
  }

      '
,row(Graph,StructuredPostalAddress),[]).

% fixm_AircraftPosition(Graph, AircraftPosition, Altitude?, Position?, PositionTime?, Track?, ActualSpeed?, NextPosition?, ReportSource?, FollowingPosition?)

fixm_AircraftPosition(Graph, AircraftPosition, Altitude, Position, PositionTime, Track, ActualSpeed, NextPosition, ReportSource, FollowingPosition) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?aircraftPosition ?altitude ?position ?positionTime ?track ?actualSpeed ?nextPosition ?reportSource ?followingPosition
WHERE
  { GRAPH ?graph
    {
      ?aircraftPosition rdf:type fixm:AircraftPosition .
      OPTIONAL { ?aircraftPosition fixm:altitude ?_altitude .
        {
          {
            ?_altitude rdf:value ?altitudeValue .
            FILTER ( NOT EXISTS {?_altitude (aixm:uom | fixm:uom | plain:uom) ?altitudeUoM})
            BIND(concat(\'val:\',?altitudeValue) AS ?altitude)
          }
            UNION
          {
            ?_altitude
              rdf:value ?altitudeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?altitudeUoM .
            BIND(concat(\'xval:\',STR(?altitudeValue),\':\',?altitudeUoM) AS ?altitude)
          }
            UNION
          {
           ?_altitude  aixm:nilReason ?altitudeNilReason .
           BIND(concat(\'nil:\',?altitudeNilReason) AS ?altitude)
          }
        }
      }
      OPTIONAL {?aircraftPosition fixm:position ?position .}
      OPTIONAL { ?aircraftPosition fixm:positionTime ?_positionTime .
        {
          {
            ?_positionTime rdf:value ?positionTimeValue .
            FILTER ( NOT EXISTS {?_positionTime (aixm:uom | fixm:uom | plain:uom) ?positionTimeUoM})
            BIND(concat(\'val:\',?positionTimeValue) AS ?positionTime)
          }
            UNION
          {
            ?_positionTime
              rdf:value ?positionTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?positionTimeUoM .
            BIND(concat(\'xval:\',STR(?positionTimeValue),\':\',?positionTimeUoM) AS ?positionTime)
          }
            UNION
          {
           ?_positionTime  aixm:nilReason ?positionTimeNilReason .
           BIND(concat(\'nil:\',?positionTimeNilReason) AS ?positionTime)
          }
        }
      }
      OPTIONAL { ?aircraftPosition fixm:track ?_track .
        {
          {
            ?_track rdf:value ?trackValue .
            FILTER ( NOT EXISTS {?_track (aixm:uom | fixm:uom | plain:uom) ?trackUoM})
            BIND(concat(\'val:\',?trackValue) AS ?track)
          }
            UNION
          {
            ?_track
              rdf:value ?trackValue ;
              (aixm:uom | fixm:uom | plain:uom) ?trackUoM .
            BIND(concat(\'xval:\',STR(?trackValue),\':\',?trackUoM) AS ?track)
          }
            UNION
          {
           ?_track  aixm:nilReason ?trackNilReason .
           BIND(concat(\'nil:\',?trackNilReason) AS ?track)
          }
        }
      }
      OPTIONAL {?aircraftPosition fixm:actualSpeed ?actualSpeed .}
      OPTIONAL {?aircraftPosition fixm:nextPosition ?nextPosition .}
      OPTIONAL { ?aircraftPosition fixm:reportSource ?_reportSource .
        {
          {
            ?_reportSource rdf:value ?reportSourceValue .
            FILTER ( NOT EXISTS {?_reportSource (aixm:uom | fixm:uom | plain:uom) ?reportSourceUoM})
            BIND(concat(\'val:\',?reportSourceValue) AS ?reportSource)
          }
            UNION
          {
            ?_reportSource
              rdf:value ?reportSourceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?reportSourceUoM .
            BIND(concat(\'xval:\',STR(?reportSourceValue),\':\',?reportSourceUoM) AS ?reportSource)
          }
            UNION
          {
           ?_reportSource  aixm:nilReason ?reportSourceNilReason .
           BIND(concat(\'nil:\',?reportSourceNilReason) AS ?reportSource)
          }
        }
      }
      OPTIONAL {?aircraftPosition fixm:followingPosition ?followingPosition .}
    }
  }

      '
,row(Graph,AircraftPosition,Altitude,Position,PositionTime,Track,ActualSpeed,NextPosition,ReportSource,FollowingPosition),[]).

% aixm_AirportHeliportUsage(Graph, AirportHeliportUsage, Operation?)

aixm_AirportHeliportUsage(Graph, AirportHeliportUsage, Operation) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?airportHeliportUsage ?operation
WHERE
  { GRAPH ?graph
    {
      ?airportHeliportUsage rdf:type aixm:AirportHeliportUsage .
      OPTIONAL { ?airportHeliportUsage aixm:operation ?_operation .
        {
          {
            ?_operation rdf:value ?operationValue .
            FILTER ( NOT EXISTS {?_operation (aixm:uom | fixm:uom | plain:uom) ?operationUoM})
            BIND(concat(\'val:\',?operationValue) AS ?operation)
          }
            UNION
          {
            ?_operation
              rdf:value ?operationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?operationUoM .
            BIND(concat(\'xval:\',STR(?operationValue),\':\',?operationUoM) AS ?operation)
          }
            UNION
          {
           ?_operation  aixm:nilReason ?operationNilReason .
           BIND(concat(\'nil:\',?operationNilReason) AS ?operation)
          }
        }
      }
    }
  }

      '
,row(Graph,AirportHeliportUsage,Operation),[]).

% aixm_Timesheet(Graph, Timesheet, TimeReference?, StartDate?, EndDate?, Day?, DayTil?, StartTime?, StartEvent?, StartTimeRelativeEvent?, StartEventInterpretation?, EndTime?, EndEvent?, EndTimeRelativeEvent?, EndEventInterpretation?, DaylightSavingAdjust?, Excluded?, Annotation*)

aixm_Timesheet(Graph, Timesheet, TimeReference, StartDate, EndDate, Day, DayTil, StartTime, StartEvent, StartTimeRelativeEvent, StartEventInterpretation, EndTime, EndEvent, EndTimeRelativeEvent, EndEventInterpretation, DaylightSavingAdjust, Excluded, AnnotationList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?timesheet ?timeReference ?startDate ?endDate ?day ?dayTil ?startTime ?startEvent ?startTimeRelativeEvent ?startEventInterpretation ?endTime ?endEvent ?endTimeRelativeEvent ?endEventInterpretation ?daylightSavingAdjust ?excluded (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat)
WHERE
  { GRAPH ?graph
    {
      ?timesheet rdf:type aixm:Timesheet .
      OPTIONAL { ?timesheet aixm:timeReference ?_timeReference .
        {
          {
            ?_timeReference rdf:value ?timeReferenceValue .
            FILTER ( NOT EXISTS {?_timeReference (aixm:uom | fixm:uom | plain:uom) ?timeReferenceUoM})
            BIND(concat(\'val:\',?timeReferenceValue) AS ?timeReference)
          }
            UNION
          {
            ?_timeReference
              rdf:value ?timeReferenceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?timeReferenceUoM .
            BIND(concat(\'xval:\',STR(?timeReferenceValue),\':\',?timeReferenceUoM) AS ?timeReference)
          }
            UNION
          {
           ?_timeReference  aixm:nilReason ?timeReferenceNilReason .
           BIND(concat(\'nil:\',?timeReferenceNilReason) AS ?timeReference)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:startDate ?_startDate .
        {
          {
            ?_startDate rdf:value ?startDateValue .
            FILTER ( NOT EXISTS {?_startDate (aixm:uom | fixm:uom | plain:uom) ?startDateUoM})
            BIND(concat(\'val:\',?startDateValue) AS ?startDate)
          }
            UNION
          {
            ?_startDate
              rdf:value ?startDateValue ;
              (aixm:uom | fixm:uom | plain:uom) ?startDateUoM .
            BIND(concat(\'xval:\',STR(?startDateValue),\':\',?startDateUoM) AS ?startDate)
          }
            UNION
          {
           ?_startDate  aixm:nilReason ?startDateNilReason .
           BIND(concat(\'nil:\',?startDateNilReason) AS ?startDate)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:endDate ?_endDate .
        {
          {
            ?_endDate rdf:value ?endDateValue .
            FILTER ( NOT EXISTS {?_endDate (aixm:uom | fixm:uom | plain:uom) ?endDateUoM})
            BIND(concat(\'val:\',?endDateValue) AS ?endDate)
          }
            UNION
          {
            ?_endDate
              rdf:value ?endDateValue ;
              (aixm:uom | fixm:uom | plain:uom) ?endDateUoM .
            BIND(concat(\'xval:\',STR(?endDateValue),\':\',?endDateUoM) AS ?endDate)
          }
            UNION
          {
           ?_endDate  aixm:nilReason ?endDateNilReason .
           BIND(concat(\'nil:\',?endDateNilReason) AS ?endDate)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:day ?_day .
        {
          {
            ?_day rdf:value ?dayValue .
            FILTER ( NOT EXISTS {?_day (aixm:uom | fixm:uom | plain:uom) ?dayUoM})
            BIND(concat(\'val:\',?dayValue) AS ?day)
          }
            UNION
          {
            ?_day
              rdf:value ?dayValue ;
              (aixm:uom | fixm:uom | plain:uom) ?dayUoM .
            BIND(concat(\'xval:\',STR(?dayValue),\':\',?dayUoM) AS ?day)
          }
            UNION
          {
           ?_day  aixm:nilReason ?dayNilReason .
           BIND(concat(\'nil:\',?dayNilReason) AS ?day)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:dayTil ?_dayTil .
        {
          {
            ?_dayTil rdf:value ?dayTilValue .
            FILTER ( NOT EXISTS {?_dayTil (aixm:uom | fixm:uom | plain:uom) ?dayTilUoM})
            BIND(concat(\'val:\',?dayTilValue) AS ?dayTil)
          }
            UNION
          {
            ?_dayTil
              rdf:value ?dayTilValue ;
              (aixm:uom | fixm:uom | plain:uom) ?dayTilUoM .
            BIND(concat(\'xval:\',STR(?dayTilValue),\':\',?dayTilUoM) AS ?dayTil)
          }
            UNION
          {
           ?_dayTil  aixm:nilReason ?dayTilNilReason .
           BIND(concat(\'nil:\',?dayTilNilReason) AS ?dayTil)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:startTime ?_startTime .
        {
          {
            ?_startTime rdf:value ?startTimeValue .
            FILTER ( NOT EXISTS {?_startTime (aixm:uom | fixm:uom | plain:uom) ?startTimeUoM})
            BIND(concat(\'val:\',?startTimeValue) AS ?startTime)
          }
            UNION
          {
            ?_startTime
              rdf:value ?startTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?startTimeUoM .
            BIND(concat(\'xval:\',STR(?startTimeValue),\':\',?startTimeUoM) AS ?startTime)
          }
            UNION
          {
           ?_startTime  aixm:nilReason ?startTimeNilReason .
           BIND(concat(\'nil:\',?startTimeNilReason) AS ?startTime)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:startEvent ?_startEvent .
        {
          {
            ?_startEvent rdf:value ?startEventValue .
            FILTER ( NOT EXISTS {?_startEvent (aixm:uom | fixm:uom | plain:uom) ?startEventUoM})
            BIND(concat(\'val:\',?startEventValue) AS ?startEvent)
          }
            UNION
          {
            ?_startEvent
              rdf:value ?startEventValue ;
              (aixm:uom | fixm:uom | plain:uom) ?startEventUoM .
            BIND(concat(\'xval:\',STR(?startEventValue),\':\',?startEventUoM) AS ?startEvent)
          }
            UNION
          {
           ?_startEvent  aixm:nilReason ?startEventNilReason .
           BIND(concat(\'nil:\',?startEventNilReason) AS ?startEvent)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:startTimeRelativeEvent ?_startTimeRelativeEvent .
        {
          {
            ?_startTimeRelativeEvent rdf:value ?startTimeRelativeEventValue .
            FILTER ( NOT EXISTS {?_startTimeRelativeEvent (aixm:uom | fixm:uom | plain:uom) ?startTimeRelativeEventUoM})
            BIND(concat(\'val:\',?startTimeRelativeEventValue) AS ?startTimeRelativeEvent)
          }
            UNION
          {
            ?_startTimeRelativeEvent
              rdf:value ?startTimeRelativeEventValue ;
              (aixm:uom | fixm:uom | plain:uom) ?startTimeRelativeEventUoM .
            BIND(concat(\'xval:\',STR(?startTimeRelativeEventValue),\':\',?startTimeRelativeEventUoM) AS ?startTimeRelativeEvent)
          }
            UNION
          {
           ?_startTimeRelativeEvent  aixm:nilReason ?startTimeRelativeEventNilReason .
           BIND(concat(\'nil:\',?startTimeRelativeEventNilReason) AS ?startTimeRelativeEvent)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:startEventInterpretation ?_startEventInterpretation .
        {
          {
            ?_startEventInterpretation rdf:value ?startEventInterpretationValue .
            FILTER ( NOT EXISTS {?_startEventInterpretation (aixm:uom | fixm:uom | plain:uom) ?startEventInterpretationUoM})
            BIND(concat(\'val:\',?startEventInterpretationValue) AS ?startEventInterpretation)
          }
            UNION
          {
            ?_startEventInterpretation
              rdf:value ?startEventInterpretationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?startEventInterpretationUoM .
            BIND(concat(\'xval:\',STR(?startEventInterpretationValue),\':\',?startEventInterpretationUoM) AS ?startEventInterpretation)
          }
            UNION
          {
           ?_startEventInterpretation  aixm:nilReason ?startEventInterpretationNilReason .
           BIND(concat(\'nil:\',?startEventInterpretationNilReason) AS ?startEventInterpretation)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:endTime ?_endTime .
        {
          {
            ?_endTime rdf:value ?endTimeValue .
            FILTER ( NOT EXISTS {?_endTime (aixm:uom | fixm:uom | plain:uom) ?endTimeUoM})
            BIND(concat(\'val:\',?endTimeValue) AS ?endTime)
          }
            UNION
          {
            ?_endTime
              rdf:value ?endTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?endTimeUoM .
            BIND(concat(\'xval:\',STR(?endTimeValue),\':\',?endTimeUoM) AS ?endTime)
          }
            UNION
          {
           ?_endTime  aixm:nilReason ?endTimeNilReason .
           BIND(concat(\'nil:\',?endTimeNilReason) AS ?endTime)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:endEvent ?_endEvent .
        {
          {
            ?_endEvent rdf:value ?endEventValue .
            FILTER ( NOT EXISTS {?_endEvent (aixm:uom | fixm:uom | plain:uom) ?endEventUoM})
            BIND(concat(\'val:\',?endEventValue) AS ?endEvent)
          }
            UNION
          {
            ?_endEvent
              rdf:value ?endEventValue ;
              (aixm:uom | fixm:uom | plain:uom) ?endEventUoM .
            BIND(concat(\'xval:\',STR(?endEventValue),\':\',?endEventUoM) AS ?endEvent)
          }
            UNION
          {
           ?_endEvent  aixm:nilReason ?endEventNilReason .
           BIND(concat(\'nil:\',?endEventNilReason) AS ?endEvent)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:endTimeRelativeEvent ?_endTimeRelativeEvent .
        {
          {
            ?_endTimeRelativeEvent rdf:value ?endTimeRelativeEventValue .
            FILTER ( NOT EXISTS {?_endTimeRelativeEvent (aixm:uom | fixm:uom | plain:uom) ?endTimeRelativeEventUoM})
            BIND(concat(\'val:\',?endTimeRelativeEventValue) AS ?endTimeRelativeEvent)
          }
            UNION
          {
            ?_endTimeRelativeEvent
              rdf:value ?endTimeRelativeEventValue ;
              (aixm:uom | fixm:uom | plain:uom) ?endTimeRelativeEventUoM .
            BIND(concat(\'xval:\',STR(?endTimeRelativeEventValue),\':\',?endTimeRelativeEventUoM) AS ?endTimeRelativeEvent)
          }
            UNION
          {
           ?_endTimeRelativeEvent  aixm:nilReason ?endTimeRelativeEventNilReason .
           BIND(concat(\'nil:\',?endTimeRelativeEventNilReason) AS ?endTimeRelativeEvent)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:endEventInterpretation ?_endEventInterpretation .
        {
          {
            ?_endEventInterpretation rdf:value ?endEventInterpretationValue .
            FILTER ( NOT EXISTS {?_endEventInterpretation (aixm:uom | fixm:uom | plain:uom) ?endEventInterpretationUoM})
            BIND(concat(\'val:\',?endEventInterpretationValue) AS ?endEventInterpretation)
          }
            UNION
          {
            ?_endEventInterpretation
              rdf:value ?endEventInterpretationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?endEventInterpretationUoM .
            BIND(concat(\'xval:\',STR(?endEventInterpretationValue),\':\',?endEventInterpretationUoM) AS ?endEventInterpretation)
          }
            UNION
          {
           ?_endEventInterpretation  aixm:nilReason ?endEventInterpretationNilReason .
           BIND(concat(\'nil:\',?endEventInterpretationNilReason) AS ?endEventInterpretation)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:daylightSavingAdjust ?_daylightSavingAdjust .
        {
          {
            ?_daylightSavingAdjust rdf:value ?daylightSavingAdjustValue .
            FILTER ( NOT EXISTS {?_daylightSavingAdjust (aixm:uom | fixm:uom | plain:uom) ?daylightSavingAdjustUoM})
            BIND(concat(\'val:\',?daylightSavingAdjustValue) AS ?daylightSavingAdjust)
          }
            UNION
          {
            ?_daylightSavingAdjust
              rdf:value ?daylightSavingAdjustValue ;
              (aixm:uom | fixm:uom | plain:uom) ?daylightSavingAdjustUoM .
            BIND(concat(\'xval:\',STR(?daylightSavingAdjustValue),\':\',?daylightSavingAdjustUoM) AS ?daylightSavingAdjust)
          }
            UNION
          {
           ?_daylightSavingAdjust  aixm:nilReason ?daylightSavingAdjustNilReason .
           BIND(concat(\'nil:\',?daylightSavingAdjustNilReason) AS ?daylightSavingAdjust)
          }
        }
      }
      OPTIONAL { ?timesheet aixm:excluded ?_excluded .
        {
          {
            ?_excluded rdf:value ?excludedValue .
            FILTER ( NOT EXISTS {?_excluded (aixm:uom | fixm:uom | plain:uom) ?excludedUoM})
            BIND(concat(\'val:\',?excludedValue) AS ?excluded)
          }
            UNION
          {
            ?_excluded
              rdf:value ?excludedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?excludedUoM .
            BIND(concat(\'xval:\',STR(?excludedValue),\':\',?excludedUoM) AS ?excluded)
          }
            UNION
          {
           ?_excluded  aixm:nilReason ?excludedNilReason .
           BIND(concat(\'nil:\',?excludedNilReason) AS ?excluded)
          }
        }
      }
      OPTIONAL {?timesheet aixm:annotation ?annotation .}
    }
  }
GROUP BY ?graph ?timesheet ?timeReference ?startDate ?endDate ?day ?dayTil ?startTime ?startEvent ?startTimeRelativeEvent ?startEventInterpretation ?endTime ?endEvent ?endTimeRelativeEvent ?endEventInterpretation ?daylightSavingAdjust ?excluded

      '
,row(Graph,Timesheet,TimeReference,StartDate,EndDate,Day,DayTil,StartTime,StartEvent,StartTimeRelativeEvent,StartEventInterpretation,EndTime,EndEvent,EndTimeRelativeEvent,EndEventInterpretation,DaylightSavingAdjust,Excluded,AnnotationConcat),[]), convert(AnnotationConcat,AnnotationList).

% gml_SurfacePatch(Graph, SurfacePatch)

gml_SurfacePatch(Graph, SurfacePatch) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?surfacePatch
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* gml:SurfacePatch .
  }
  { GRAPH ?graph
    {
      ?surfacePatch rdf:type ?SUBCLASS .
    }
  }
}

      '
,row(Graph,SurfacePatch),[]).

% fixm_MultiTime(Graph, MultiTime, Actual?, Estimated?)

fixm_MultiTime(Graph, MultiTime, Actual, Estimated) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?multiTime ?actual ?estimated
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:MultiTime .
  }
  { GRAPH ?graph
    {
      ?multiTime rdf:type ?SUBCLASS .
      OPTIONAL {?multiTime fixm:actual ?actual .}
      OPTIONAL {?multiTime fixm:estimated ?estimated .}
    }
  }
}

      '
,row(Graph,MultiTime,Actual,Estimated),[]).

% aixm_FlightCharacteristic(Graph, FlightCharacteristic, Type?, Rule?, Status?, Military?, Origin?, Purpose?, Annotation*)

aixm_FlightCharacteristic(Graph, FlightCharacteristic, Type, Rule, Status, Military, Origin, Purpose, AnnotationList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?flightCharacteristic ?type ?rule ?status ?military ?origin ?purpose (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat)
WHERE
  { GRAPH ?graph
    {
      ?flightCharacteristic rdf:type aixm:FlightCharacteristic .
      OPTIONAL { ?flightCharacteristic aixm:type ?_type .
        {
          {
            ?_type rdf:value ?typeValue .
            FILTER ( NOT EXISTS {?_type (aixm:uom | fixm:uom | plain:uom) ?typeUoM})
            BIND(concat(\'val:\',?typeValue) AS ?type)
          }
            UNION
          {
            ?_type
              rdf:value ?typeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?typeUoM .
            BIND(concat(\'xval:\',STR(?typeValue),\':\',?typeUoM) AS ?type)
          }
            UNION
          {
           ?_type  aixm:nilReason ?typeNilReason .
           BIND(concat(\'nil:\',?typeNilReason) AS ?type)
          }
        }
      }
      OPTIONAL { ?flightCharacteristic aixm:rule ?_rule .
        {
          {
            ?_rule rdf:value ?ruleValue .
            FILTER ( NOT EXISTS {?_rule (aixm:uom | fixm:uom | plain:uom) ?ruleUoM})
            BIND(concat(\'val:\',?ruleValue) AS ?rule)
          }
            UNION
          {
            ?_rule
              rdf:value ?ruleValue ;
              (aixm:uom | fixm:uom | plain:uom) ?ruleUoM .
            BIND(concat(\'xval:\',STR(?ruleValue),\':\',?ruleUoM) AS ?rule)
          }
            UNION
          {
           ?_rule  aixm:nilReason ?ruleNilReason .
           BIND(concat(\'nil:\',?ruleNilReason) AS ?rule)
          }
        }
      }
      OPTIONAL { ?flightCharacteristic aixm:status ?_status .
        {
          {
            ?_status rdf:value ?statusValue .
            FILTER ( NOT EXISTS {?_status (aixm:uom | fixm:uom | plain:uom) ?statusUoM})
            BIND(concat(\'val:\',?statusValue) AS ?status)
          }
            UNION
          {
            ?_status
              rdf:value ?statusValue ;
              (aixm:uom | fixm:uom | plain:uom) ?statusUoM .
            BIND(concat(\'xval:\',STR(?statusValue),\':\',?statusUoM) AS ?status)
          }
            UNION
          {
           ?_status  aixm:nilReason ?statusNilReason .
           BIND(concat(\'nil:\',?statusNilReason) AS ?status)
          }
        }
      }
      OPTIONAL { ?flightCharacteristic aixm:military ?_military .
        {
          {
            ?_military rdf:value ?militaryValue .
            FILTER ( NOT EXISTS {?_military (aixm:uom | fixm:uom | plain:uom) ?militaryUoM})
            BIND(concat(\'val:\',?militaryValue) AS ?military)
          }
            UNION
          {
            ?_military
              rdf:value ?militaryValue ;
              (aixm:uom | fixm:uom | plain:uom) ?militaryUoM .
            BIND(concat(\'xval:\',STR(?militaryValue),\':\',?militaryUoM) AS ?military)
          }
            UNION
          {
           ?_military  aixm:nilReason ?militaryNilReason .
           BIND(concat(\'nil:\',?militaryNilReason) AS ?military)
          }
        }
      }
      OPTIONAL { ?flightCharacteristic aixm:origin ?_origin .
        {
          {
            ?_origin rdf:value ?originValue .
            FILTER ( NOT EXISTS {?_origin (aixm:uom | fixm:uom | plain:uom) ?originUoM})
            BIND(concat(\'val:\',?originValue) AS ?origin)
          }
            UNION
          {
            ?_origin
              rdf:value ?originValue ;
              (aixm:uom | fixm:uom | plain:uom) ?originUoM .
            BIND(concat(\'xval:\',STR(?originValue),\':\',?originUoM) AS ?origin)
          }
            UNION
          {
           ?_origin  aixm:nilReason ?originNilReason .
           BIND(concat(\'nil:\',?originNilReason) AS ?origin)
          }
        }
      }
      OPTIONAL { ?flightCharacteristic aixm:purpose ?_purpose .
        {
          {
            ?_purpose rdf:value ?purposeValue .
            FILTER ( NOT EXISTS {?_purpose (aixm:uom | fixm:uom | plain:uom) ?purposeUoM})
            BIND(concat(\'val:\',?purposeValue) AS ?purpose)
          }
            UNION
          {
            ?_purpose
              rdf:value ?purposeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?purposeUoM .
            BIND(concat(\'xval:\',STR(?purposeValue),\':\',?purposeUoM) AS ?purpose)
          }
            UNION
          {
           ?_purpose  aixm:nilReason ?purposeNilReason .
           BIND(concat(\'nil:\',?purposeNilReason) AS ?purpose)
          }
        }
      }
      OPTIONAL {?flightCharacteristic aixm:annotation ?annotation .}
    }
  }
GROUP BY ?graph ?flightCharacteristic ?type ?rule ?status ?military ?origin ?purpose

      '
,row(Graph,FlightCharacteristic,Type,Rule,Status,Military,Origin,Purpose,AnnotationConcat),[]), convert(AnnotationConcat,AnnotationList).

% fixm_Provenance(Graph, Provenance, Timestamp?, Centre?, Source?, System?)

fixm_Provenance(Graph, Provenance, Timestamp, Centre, Source, System) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?provenance ?timestamp ?centre ?source ?system
WHERE
  { GRAPH ?graph
    {
      ?provenance rdf:type fixm:Provenance .
      OPTIONAL { ?provenance fixm:timestamp ?_timestamp .
        {
          {
            ?_timestamp rdf:value ?timestampValue .
            FILTER ( NOT EXISTS {?_timestamp (aixm:uom | fixm:uom | plain:uom) ?timestampUoM})
            BIND(concat(\'val:\',?timestampValue) AS ?timestamp)
          }
            UNION
          {
            ?_timestamp
              rdf:value ?timestampValue ;
              (aixm:uom | fixm:uom | plain:uom) ?timestampUoM .
            BIND(concat(\'xval:\',STR(?timestampValue),\':\',?timestampUoM) AS ?timestamp)
          }
            UNION
          {
           ?_timestamp  aixm:nilReason ?timestampNilReason .
           BIND(concat(\'nil:\',?timestampNilReason) AS ?timestamp)
          }
        }
      }
      OPTIONAL { ?provenance fixm:centre ?_centre .
        {
          {
            ?_centre rdf:value ?centreValue .
            FILTER ( NOT EXISTS {?_centre (aixm:uom | fixm:uom | plain:uom) ?centreUoM})
            BIND(concat(\'val:\',?centreValue) AS ?centre)
          }
            UNION
          {
            ?_centre
              rdf:value ?centreValue ;
              (aixm:uom | fixm:uom | plain:uom) ?centreUoM .
            BIND(concat(\'xval:\',STR(?centreValue),\':\',?centreUoM) AS ?centre)
          }
            UNION
          {
           ?_centre  aixm:nilReason ?centreNilReason .
           BIND(concat(\'nil:\',?centreNilReason) AS ?centre)
          }
        }
      }
      OPTIONAL { ?provenance fixm:source ?_source .
        {
          {
            ?_source rdf:value ?sourceValue .
            FILTER ( NOT EXISTS {?_source (aixm:uom | fixm:uom | plain:uom) ?sourceUoM})
            BIND(concat(\'val:\',?sourceValue) AS ?source)
          }
            UNION
          {
            ?_source
              rdf:value ?sourceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?sourceUoM .
            BIND(concat(\'xval:\',STR(?sourceValue),\':\',?sourceUoM) AS ?source)
          }
            UNION
          {
           ?_source  aixm:nilReason ?sourceNilReason .
           BIND(concat(\'nil:\',?sourceNilReason) AS ?source)
          }
        }
      }
      OPTIONAL { ?provenance fixm:system ?_system .
        {
          {
            ?_system rdf:value ?systemValue .
            FILTER ( NOT EXISTS {?_system (aixm:uom | fixm:uom | plain:uom) ?systemUoM})
            BIND(concat(\'val:\',?systemValue) AS ?system)
          }
            UNION
          {
            ?_system
              rdf:value ?systemValue ;
              (aixm:uom | fixm:uom | plain:uom) ?systemUoM .
            BIND(concat(\'xval:\',STR(?systemValue),\':\',?systemUoM) AS ?system)
          }
            UNION
          {
           ?_system  aixm:nilReason ?systemNilReason .
           BIND(concat(\'nil:\',?systemNilReason) AS ?system)
          }
        }
      }
    }
  }

      '
,row(Graph,Provenance,Timestamp,Centre,Source,System),[]).

% aixm_AirportHeliport(Graph, AirportHeliport, TimeSlice*)

aixm_AirportHeliport(Graph, AirportHeliport, TimeSliceList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?airportHeliport (GROUP_CONCAT(DISTINCT ?timeSlice;SEPARATOR=",") AS ?timeSliceConcat)
WHERE
  { GRAPH ?graph
    {
      ?airportHeliport rdf:type aixm:AirportHeliport .
      OPTIONAL {?airportHeliport aixm:timeSlice ?timeSlice .}
    }
  }
GROUP BY ?graph ?airportHeliport

      '
,row(Graph,AirportHeliport,TimeSliceConcat),[]), convert(TimeSliceConcat,TimeSliceList).

% fixm_TrajectoryPoint(Graph, TrajectoryPoint, AltimeterSetting?, PredictedAirspeed?, PredictedGroundspeed?, MetData?, Point?, TrajectoryChange*, TrajectoryChangeType*, ReferencePoint?)

fixm_TrajectoryPoint(Graph, TrajectoryPoint, AltimeterSetting, PredictedAirspeed, PredictedGroundspeed, MetData, Point, TrajectoryChangeList, TrajectoryChangeTypeList, ReferencePoint) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?trajectoryPoint ?altimeterSetting ?predictedAirspeed ?predictedGroundspeed ?metData ?point (GROUP_CONCAT(DISTINCT ?trajectoryChange;SEPARATOR=",") AS ?trajectoryChangeConcat) (GROUP_CONCAT(DISTINCT ?trajectoryChangeType;SEPARATOR=",") AS ?trajectoryChangeTypeConcat) ?referencePoint
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:TrajectoryPoint .
  }
  { GRAPH ?graph
    {
      ?trajectoryPoint rdf:type ?SUBCLASS .
      OPTIONAL { ?trajectoryPoint fixm:altimeterSetting ?_altimeterSetting .
        {
          {
            ?_altimeterSetting rdf:value ?altimeterSettingValue .
            FILTER ( NOT EXISTS {?_altimeterSetting (aixm:uom | fixm:uom | plain:uom) ?altimeterSettingUoM})
            BIND(concat(\'val:\',?altimeterSettingValue) AS ?altimeterSetting)
          }
            UNION
          {
            ?_altimeterSetting
              rdf:value ?altimeterSettingValue ;
              (aixm:uom | fixm:uom | plain:uom) ?altimeterSettingUoM .
            BIND(concat(\'xval:\',STR(?altimeterSettingValue),\':\',?altimeterSettingUoM) AS ?altimeterSetting)
          }
            UNION
          {
           ?_altimeterSetting  aixm:nilReason ?altimeterSettingNilReason .
           BIND(concat(\'nil:\',?altimeterSettingNilReason) AS ?altimeterSetting)
          }
        }
      }
      OPTIONAL { ?trajectoryPoint fixm:predictedAirspeed ?_predictedAirspeed .
        {
          {
            ?_predictedAirspeed rdf:value ?predictedAirspeedValue .
            FILTER ( NOT EXISTS {?_predictedAirspeed (aixm:uom | fixm:uom | plain:uom) ?predictedAirspeedUoM})
            BIND(concat(\'val:\',?predictedAirspeedValue) AS ?predictedAirspeed)
          }
            UNION
          {
            ?_predictedAirspeed
              rdf:value ?predictedAirspeedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?predictedAirspeedUoM .
            BIND(concat(\'xval:\',STR(?predictedAirspeedValue),\':\',?predictedAirspeedUoM) AS ?predictedAirspeed)
          }
            UNION
          {
           ?_predictedAirspeed  aixm:nilReason ?predictedAirspeedNilReason .
           BIND(concat(\'nil:\',?predictedAirspeedNilReason) AS ?predictedAirspeed)
          }
        }
      }
      OPTIONAL { ?trajectoryPoint fixm:predictedGroundspeed ?_predictedGroundspeed .
        {
          {
            ?_predictedGroundspeed rdf:value ?predictedGroundspeedValue .
            FILTER ( NOT EXISTS {?_predictedGroundspeed (aixm:uom | fixm:uom | plain:uom) ?predictedGroundspeedUoM})
            BIND(concat(\'val:\',?predictedGroundspeedValue) AS ?predictedGroundspeed)
          }
            UNION
          {
            ?_predictedGroundspeed
              rdf:value ?predictedGroundspeedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?predictedGroundspeedUoM .
            BIND(concat(\'xval:\',STR(?predictedGroundspeedValue),\':\',?predictedGroundspeedUoM) AS ?predictedGroundspeed)
          }
            UNION
          {
           ?_predictedGroundspeed  aixm:nilReason ?predictedGroundspeedNilReason .
           BIND(concat(\'nil:\',?predictedGroundspeedNilReason) AS ?predictedGroundspeed)
          }
        }
      }
      OPTIONAL {?trajectoryPoint fixm:metData ?metData .}
      OPTIONAL {?trajectoryPoint fixm:point ?point .}
      OPTIONAL {?trajectoryPoint fixm:trajectoryChange ?trajectoryChange .}
      OPTIONAL { ?trajectoryPoint fixm:trajectoryChangeType ?_trajectoryChangeType .
        {
          {
            ?_trajectoryChangeType rdf:value ?trajectoryChangeTypeValue .
            FILTER ( NOT EXISTS {?_trajectoryChangeType (aixm:uom | fixm:uom | plain:uom) ?trajectoryChangeTypeUoM})
            BIND(concat(\'val:\',?trajectoryChangeTypeValue) AS ?trajectoryChangeType)
          }
            UNION
          {
            ?_trajectoryChangeType
              rdf:value ?trajectoryChangeTypeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?trajectoryChangeTypeUoM .
            BIND(concat(\'xval:\',STR(?trajectoryChangeTypeValue),\':\',?trajectoryChangeTypeUoM) AS ?trajectoryChangeType)
          }
            UNION
          {
           ?_trajectoryChangeType  aixm:nilReason ?trajectoryChangeTypeNilReason .
           BIND(concat(\'nil:\',?trajectoryChangeTypeNilReason) AS ?trajectoryChangeType)
          }
        }
      }
      OPTIONAL {?trajectoryPoint fixm:referencePoint ?referencePoint .}
    }
  }
}
GROUP BY ?graph ?trajectoryPoint ?altimeterSetting ?predictedAirspeed ?predictedGroundspeed ?metData ?point ?referencePoint

      '
,row(Graph,TrajectoryPoint,AltimeterSetting,PredictedAirspeed,PredictedGroundspeed,MetData,Point,TrajectoryChangeConcat,TrajectoryChangeTypeConcat,ReferencePoint),[]), convert(TrajectoryChangeConcat,TrajectoryChangeList), convert(TrajectoryChangeTypeConcat,TrajectoryChangeTypeList).

% fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier?, DistanceFromTakeOff?, EfplEstimatedSpeed?, ElapsedTime?, GrossWeight?, TrajectoryPointType?, TrajectoryPointRole?, InboundSegment?)

fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier, DistanceFromTakeOff, EfplEstimatedSpeed, ElapsedTime, GrossWeight, TrajectoryPointType, TrajectoryPointRole, InboundSegment) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?efplTrajectoryPoint ?aerodromeIdentifier ?distanceFromTakeOff ?efplEstimatedSpeed ?elapsedTime ?grossWeight ?trajectoryPointType ?trajectoryPointRole ?inboundSegment
WHERE
  { GRAPH ?graph
    {
      ?efplTrajectoryPoint rdf:type fixm:EfplTrajectoryPoint .
      OPTIONAL {?efplTrajectoryPoint fixm:aerodromeIdentifier ?aerodromeIdentifier .}
      OPTIONAL { ?efplTrajectoryPoint fixm:distanceFromTakeOff ?_distanceFromTakeOff .
        {
          {
            ?_distanceFromTakeOff rdf:value ?distanceFromTakeOffValue .
            FILTER ( NOT EXISTS {?_distanceFromTakeOff (aixm:uom | fixm:uom | plain:uom) ?distanceFromTakeOffUoM})
            BIND(concat(\'val:\',?distanceFromTakeOffValue) AS ?distanceFromTakeOff)
          }
            UNION
          {
            ?_distanceFromTakeOff
              rdf:value ?distanceFromTakeOffValue ;
              (aixm:uom | fixm:uom | plain:uom) ?distanceFromTakeOffUoM .
            BIND(concat(\'xval:\',STR(?distanceFromTakeOffValue),\':\',?distanceFromTakeOffUoM) AS ?distanceFromTakeOff)
          }
            UNION
          {
           ?_distanceFromTakeOff  aixm:nilReason ?distanceFromTakeOffNilReason .
           BIND(concat(\'nil:\',?distanceFromTakeOffNilReason) AS ?distanceFromTakeOff)
          }
        }
      }
      OPTIONAL { ?efplTrajectoryPoint fixm:efplEstimatedSpeed ?_efplEstimatedSpeed .
        {
          {
            ?_efplEstimatedSpeed rdf:value ?efplEstimatedSpeedValue .
            FILTER ( NOT EXISTS {?_efplEstimatedSpeed (aixm:uom | fixm:uom | plain:uom) ?efplEstimatedSpeedUoM})
            BIND(concat(\'val:\',?efplEstimatedSpeedValue) AS ?efplEstimatedSpeed)
          }
            UNION
          {
            ?_efplEstimatedSpeed
              rdf:value ?efplEstimatedSpeedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?efplEstimatedSpeedUoM .
            BIND(concat(\'xval:\',STR(?efplEstimatedSpeedValue),\':\',?efplEstimatedSpeedUoM) AS ?efplEstimatedSpeed)
          }
            UNION
          {
           ?_efplEstimatedSpeed  aixm:nilReason ?efplEstimatedSpeedNilReason .
           BIND(concat(\'nil:\',?efplEstimatedSpeedNilReason) AS ?efplEstimatedSpeed)
          }
        }
      }
      OPTIONAL { ?efplTrajectoryPoint fixm:elapsedTime ?_elapsedTime .
        {
          {
            ?_elapsedTime rdf:value ?elapsedTimeValue .
            FILTER ( NOT EXISTS {?_elapsedTime (aixm:uom | fixm:uom | plain:uom) ?elapsedTimeUoM})
            BIND(concat(\'val:\',?elapsedTimeValue) AS ?elapsedTime)
          }
            UNION
          {
            ?_elapsedTime
              rdf:value ?elapsedTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?elapsedTimeUoM .
            BIND(concat(\'xval:\',STR(?elapsedTimeValue),\':\',?elapsedTimeUoM) AS ?elapsedTime)
          }
            UNION
          {
           ?_elapsedTime  aixm:nilReason ?elapsedTimeNilReason .
           BIND(concat(\'nil:\',?elapsedTimeNilReason) AS ?elapsedTime)
          }
        }
      }
      OPTIONAL { ?efplTrajectoryPoint fixm:grossWeight ?_grossWeight .
        {
          {
            ?_grossWeight rdf:value ?grossWeightValue .
            FILTER ( NOT EXISTS {?_grossWeight (aixm:uom | fixm:uom | plain:uom) ?grossWeightUoM})
            BIND(concat(\'val:\',?grossWeightValue) AS ?grossWeight)
          }
            UNION
          {
            ?_grossWeight
              rdf:value ?grossWeightValue ;
              (aixm:uom | fixm:uom | plain:uom) ?grossWeightUoM .
            BIND(concat(\'xval:\',STR(?grossWeightValue),\':\',?grossWeightUoM) AS ?grossWeight)
          }
            UNION
          {
           ?_grossWeight  aixm:nilReason ?grossWeightNilReason .
           BIND(concat(\'nil:\',?grossWeightNilReason) AS ?grossWeight)
          }
        }
      }
      OPTIONAL { ?efplTrajectoryPoint fixm:trajectoryPointType ?_trajectoryPointType .
        {
          {
            ?_trajectoryPointType rdf:value ?trajectoryPointTypeValue .
            FILTER ( NOT EXISTS {?_trajectoryPointType (aixm:uom | fixm:uom | plain:uom) ?trajectoryPointTypeUoM})
            BIND(concat(\'val:\',?trajectoryPointTypeValue) AS ?trajectoryPointType)
          }
            UNION
          {
            ?_trajectoryPointType
              rdf:value ?trajectoryPointTypeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?trajectoryPointTypeUoM .
            BIND(concat(\'xval:\',STR(?trajectoryPointTypeValue),\':\',?trajectoryPointTypeUoM) AS ?trajectoryPointType)
          }
            UNION
          {
           ?_trajectoryPointType  aixm:nilReason ?trajectoryPointTypeNilReason .
           BIND(concat(\'nil:\',?trajectoryPointTypeNilReason) AS ?trajectoryPointType)
          }
        }
      }
      OPTIONAL {?efplTrajectoryPoint fixm:trajectoryPointRole ?trajectoryPointRole .}
      OPTIONAL {?efplTrajectoryPoint fixm:inboundSegment ?inboundSegment .}
    }
  }

      '
,row(Graph,EfplTrajectoryPoint,AerodromeIdentifier,DistanceFromTakeOff,EfplEstimatedSpeed,ElapsedTime,GrossWeight,TrajectoryPointType,TrajectoryPointRole,InboundSegment),[]).

% fixm_Temperatures(Graph, Temperatures, ControlTemperature?, EmergencyTemperature?, FlashpointTemperature?)

fixm_Temperatures(Graph, Temperatures, ControlTemperature, EmergencyTemperature, FlashpointTemperature) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?temperatures ?controlTemperature ?emergencyTemperature ?flashpointTemperature
WHERE
  { GRAPH ?graph
    {
      ?temperatures rdf:type fixm:Temperatures .
      OPTIONAL { ?temperatures fixm:controlTemperature ?_controlTemperature .
        {
          {
            ?_controlTemperature rdf:value ?controlTemperatureValue .
            FILTER ( NOT EXISTS {?_controlTemperature (aixm:uom | fixm:uom | plain:uom) ?controlTemperatureUoM})
            BIND(concat(\'val:\',?controlTemperatureValue) AS ?controlTemperature)
          }
            UNION
          {
            ?_controlTemperature
              rdf:value ?controlTemperatureValue ;
              (aixm:uom | fixm:uom | plain:uom) ?controlTemperatureUoM .
            BIND(concat(\'xval:\',STR(?controlTemperatureValue),\':\',?controlTemperatureUoM) AS ?controlTemperature)
          }
            UNION
          {
           ?_controlTemperature  aixm:nilReason ?controlTemperatureNilReason .
           BIND(concat(\'nil:\',?controlTemperatureNilReason) AS ?controlTemperature)
          }
        }
      }
      OPTIONAL { ?temperatures fixm:emergencyTemperature ?_emergencyTemperature .
        {
          {
            ?_emergencyTemperature rdf:value ?emergencyTemperatureValue .
            FILTER ( NOT EXISTS {?_emergencyTemperature (aixm:uom | fixm:uom | plain:uom) ?emergencyTemperatureUoM})
            BIND(concat(\'val:\',?emergencyTemperatureValue) AS ?emergencyTemperature)
          }
            UNION
          {
            ?_emergencyTemperature
              rdf:value ?emergencyTemperatureValue ;
              (aixm:uom | fixm:uom | plain:uom) ?emergencyTemperatureUoM .
            BIND(concat(\'xval:\',STR(?emergencyTemperatureValue),\':\',?emergencyTemperatureUoM) AS ?emergencyTemperature)
          }
            UNION
          {
           ?_emergencyTemperature  aixm:nilReason ?emergencyTemperatureNilReason .
           BIND(concat(\'nil:\',?emergencyTemperatureNilReason) AS ?emergencyTemperature)
          }
        }
      }
      OPTIONAL { ?temperatures fixm:flashpointTemperature ?_flashpointTemperature .
        {
          {
            ?_flashpointTemperature rdf:value ?flashpointTemperatureValue .
            FILTER ( NOT EXISTS {?_flashpointTemperature (aixm:uom | fixm:uom | plain:uom) ?flashpointTemperatureUoM})
            BIND(concat(\'val:\',?flashpointTemperatureValue) AS ?flashpointTemperature)
          }
            UNION
          {
            ?_flashpointTemperature
              rdf:value ?flashpointTemperatureValue ;
              (aixm:uom | fixm:uom | plain:uom) ?flashpointTemperatureUoM .
            BIND(concat(\'xval:\',STR(?flashpointTemperatureValue),\':\',?flashpointTemperatureUoM) AS ?flashpointTemperature)
          }
            UNION
          {
           ?_flashpointTemperature  aixm:nilReason ?flashpointTemperatureNilReason .
           BIND(concat(\'nil:\',?flashpointTemperatureNilReason) AS ?flashpointTemperature)
          }
        }
      }
    }
  }

      '
,row(Graph,Temperatures,ControlTemperature,EmergencyTemperature,FlashpointTemperature),[]).

% fixm_TrajectorySegment(Graph, TrajectorySegment, SegmentIdentifier?, SegmentType?)

fixm_TrajectorySegment(Graph, TrajectorySegment, SegmentIdentifier, SegmentType) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?trajectorySegment ?segmentIdentifier ?segmentType
WHERE
  { GRAPH ?graph
    {
      ?trajectorySegment rdf:type fixm:TrajectorySegment .
      OPTIONAL { ?trajectorySegment fixm:segmentIdentifier ?_segmentIdentifier .
        {
          {
            ?_segmentIdentifier rdf:value ?segmentIdentifierValue .
            FILTER ( NOT EXISTS {?_segmentIdentifier (aixm:uom | fixm:uom | plain:uom) ?segmentIdentifierUoM})
            BIND(concat(\'val:\',?segmentIdentifierValue) AS ?segmentIdentifier)
          }
            UNION
          {
            ?_segmentIdentifier
              rdf:value ?segmentIdentifierValue ;
              (aixm:uom | fixm:uom | plain:uom) ?segmentIdentifierUoM .
            BIND(concat(\'xval:\',STR(?segmentIdentifierValue),\':\',?segmentIdentifierUoM) AS ?segmentIdentifier)
          }
            UNION
          {
           ?_segmentIdentifier  aixm:nilReason ?segmentIdentifierNilReason .
           BIND(concat(\'nil:\',?segmentIdentifierNilReason) AS ?segmentIdentifier)
          }
        }
      }
      OPTIONAL { ?trajectorySegment fixm:segmentType ?_segmentType .
        {
          {
            ?_segmentType rdf:value ?segmentTypeValue .
            FILTER ( NOT EXISTS {?_segmentType (aixm:uom | fixm:uom | plain:uom) ?segmentTypeUoM})
            BIND(concat(\'val:\',?segmentTypeValue) AS ?segmentType)
          }
            UNION
          {
            ?_segmentType
              rdf:value ?segmentTypeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?segmentTypeUoM .
            BIND(concat(\'xval:\',STR(?segmentTypeValue),\':\',?segmentTypeUoM) AS ?segmentType)
          }
            UNION
          {
           ?_segmentType  aixm:nilReason ?segmentTypeNilReason .
           BIND(concat(\'nil:\',?segmentTypeNilReason) AS ?segmentType)
          }
        }
      }
    }
  }

      '
,row(Graph,TrajectorySegment,SegmentIdentifier,SegmentType),[]).

% fixm_RunwayPositionAndTime(Graph, RunwayPositionAndTime, RunwayName?, RunwayTime?)

fixm_RunwayPositionAndTime(Graph, RunwayPositionAndTime, RunwayName, RunwayTime) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?runwayPositionAndTime ?runwayName ?runwayTime
WHERE
  { GRAPH ?graph
    {
      ?runwayPositionAndTime rdf:type fixm:RunwayPositionAndTime .
      OPTIONAL { ?runwayPositionAndTime fixm:runwayName ?_runwayName .
        {
          {
            ?_runwayName rdf:value ?runwayNameValue .
            FILTER ( NOT EXISTS {?_runwayName (aixm:uom | fixm:uom | plain:uom) ?runwayNameUoM})
            BIND(concat(\'val:\',?runwayNameValue) AS ?runwayName)
          }
            UNION
          {
            ?_runwayName
              rdf:value ?runwayNameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?runwayNameUoM .
            BIND(concat(\'xval:\',STR(?runwayNameValue),\':\',?runwayNameUoM) AS ?runwayName)
          }
            UNION
          {
           ?_runwayName  aixm:nilReason ?runwayNameNilReason .
           BIND(concat(\'nil:\',?runwayNameNilReason) AS ?runwayName)
          }
        }
      }
      OPTIONAL {?runwayPositionAndTime fixm:runwayTime ?runwayTime .}
    }
  }

      '
,row(Graph,RunwayPositionAndTime,RunwayName,RunwayTime),[]).

% fixm_Feature(Graph, Feature, Provenance?)

fixm_Feature(Graph, Feature, Provenance) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?feature ?provenance
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:Feature .
  }
  { GRAPH ?graph
    {
      ?feature rdf:type ?SUBCLASS .
      OPTIONAL {?feature fixm:provenance ?provenance .}
    }
  }
}

      '
,row(Graph,Feature,Provenance),[]).

% fixm_FlightIdentification(Graph, FlightIdentification, AircraftIdentification?, MajorCarrierIdentifier?, MarketingCarrierFlightIdentifier*)

fixm_FlightIdentification(Graph, FlightIdentification, AircraftIdentification, MajorCarrierIdentifier, MarketingCarrierFlightIdentifierList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?flightIdentification ?aircraftIdentification ?majorCarrierIdentifier (GROUP_CONCAT(DISTINCT ?marketingCarrierFlightIdentifier;SEPARATOR=",") AS ?marketingCarrierFlightIdentifierConcat)
WHERE
  { GRAPH ?graph
    {
      ?flightIdentification rdf:type fixm:FlightIdentification .
      OPTIONAL { ?flightIdentification fixm:aircraftIdentification ?_aircraftIdentification .
        {
          {
            ?_aircraftIdentification rdf:value ?aircraftIdentificationValue .
            FILTER ( NOT EXISTS {?_aircraftIdentification (aixm:uom | fixm:uom | plain:uom) ?aircraftIdentificationUoM})
            BIND(concat(\'val:\',?aircraftIdentificationValue) AS ?aircraftIdentification)
          }
            UNION
          {
            ?_aircraftIdentification
              rdf:value ?aircraftIdentificationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?aircraftIdentificationUoM .
            BIND(concat(\'xval:\',STR(?aircraftIdentificationValue),\':\',?aircraftIdentificationUoM) AS ?aircraftIdentification)
          }
            UNION
          {
           ?_aircraftIdentification  aixm:nilReason ?aircraftIdentificationNilReason .
           BIND(concat(\'nil:\',?aircraftIdentificationNilReason) AS ?aircraftIdentification)
          }
        }
      }
      OPTIONAL { ?flightIdentification fixm:majorCarrierIdentifier ?_majorCarrierIdentifier .
        {
          {
            ?_majorCarrierIdentifier rdf:value ?majorCarrierIdentifierValue .
            FILTER ( NOT EXISTS {?_majorCarrierIdentifier (aixm:uom | fixm:uom | plain:uom) ?majorCarrierIdentifierUoM})
            BIND(concat(\'val:\',?majorCarrierIdentifierValue) AS ?majorCarrierIdentifier)
          }
            UNION
          {
            ?_majorCarrierIdentifier
              rdf:value ?majorCarrierIdentifierValue ;
              (aixm:uom | fixm:uom | plain:uom) ?majorCarrierIdentifierUoM .
            BIND(concat(\'xval:\',STR(?majorCarrierIdentifierValue),\':\',?majorCarrierIdentifierUoM) AS ?majorCarrierIdentifier)
          }
            UNION
          {
           ?_majorCarrierIdentifier  aixm:nilReason ?majorCarrierIdentifierNilReason .
           BIND(concat(\'nil:\',?majorCarrierIdentifierNilReason) AS ?majorCarrierIdentifier)
          }
        }
      }
      OPTIONAL { ?flightIdentification fixm:marketingCarrierFlightIdentifier ?_marketingCarrierFlightIdentifier .
        {
          {
            ?_marketingCarrierFlightIdentifier rdf:value ?marketingCarrierFlightIdentifierValue .
            FILTER ( NOT EXISTS {?_marketingCarrierFlightIdentifier (aixm:uom | fixm:uom | plain:uom) ?marketingCarrierFlightIdentifierUoM})
            BIND(concat(\'val:\',?marketingCarrierFlightIdentifierValue) AS ?marketingCarrierFlightIdentifier)
          }
            UNION
          {
            ?_marketingCarrierFlightIdentifier
              rdf:value ?marketingCarrierFlightIdentifierValue ;
              (aixm:uom | fixm:uom | plain:uom) ?marketingCarrierFlightIdentifierUoM .
            BIND(concat(\'xval:\',STR(?marketingCarrierFlightIdentifierValue),\':\',?marketingCarrierFlightIdentifierUoM) AS ?marketingCarrierFlightIdentifier)
          }
            UNION
          {
           ?_marketingCarrierFlightIdentifier  aixm:nilReason ?marketingCarrierFlightIdentifierNilReason .
           BIND(concat(\'nil:\',?marketingCarrierFlightIdentifierNilReason) AS ?marketingCarrierFlightIdentifier)
          }
        }
      }
    }
  }
GROUP BY ?graph ?flightIdentification ?aircraftIdentification ?majorCarrierIdentifier

      '
,row(Graph,FlightIdentification,AircraftIdentification,MajorCarrierIdentifier,MarketingCarrierFlightIdentifierConcat),[]), convert(MarketingCarrierFlightIdentifierConcat,MarketingCarrierFlightIdentifierList).

% fixm_LastContact(Graph, LastContact, ContactFrequency?, LastContactTime?, LastContactUnit?, Position?)

fixm_LastContact(Graph, LastContact, ContactFrequency, LastContactTime, LastContactUnit, Position) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?lastContact ?contactFrequency ?lastContactTime ?lastContactUnit ?position
WHERE
  { GRAPH ?graph
    {
      ?lastContact rdf:type fixm:LastContact .
      OPTIONAL { ?lastContact fixm:contactFrequency ?_contactFrequency .
        {
          {
            ?_contactFrequency rdf:value ?contactFrequencyValue .
            FILTER ( NOT EXISTS {?_contactFrequency (aixm:uom | fixm:uom | plain:uom) ?contactFrequencyUoM})
            BIND(concat(\'val:\',?contactFrequencyValue) AS ?contactFrequency)
          }
            UNION
          {
            ?_contactFrequency
              rdf:value ?contactFrequencyValue ;
              (aixm:uom | fixm:uom | plain:uom) ?contactFrequencyUoM .
            BIND(concat(\'xval:\',STR(?contactFrequencyValue),\':\',?contactFrequencyUoM) AS ?contactFrequency)
          }
            UNION
          {
           ?_contactFrequency  aixm:nilReason ?contactFrequencyNilReason .
           BIND(concat(\'nil:\',?contactFrequencyNilReason) AS ?contactFrequency)
          }
        }
      }
      OPTIONAL { ?lastContact fixm:lastContactTime ?_lastContactTime .
        {
          {
            ?_lastContactTime rdf:value ?lastContactTimeValue .
            FILTER ( NOT EXISTS {?_lastContactTime (aixm:uom | fixm:uom | plain:uom) ?lastContactTimeUoM})
            BIND(concat(\'val:\',?lastContactTimeValue) AS ?lastContactTime)
          }
            UNION
          {
            ?_lastContactTime
              rdf:value ?lastContactTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?lastContactTimeUoM .
            BIND(concat(\'xval:\',STR(?lastContactTimeValue),\':\',?lastContactTimeUoM) AS ?lastContactTime)
          }
            UNION
          {
           ?_lastContactTime  aixm:nilReason ?lastContactTimeNilReason .
           BIND(concat(\'nil:\',?lastContactTimeNilReason) AS ?lastContactTime)
          }
        }
      }
      OPTIONAL { ?lastContact fixm:lastContactUnit ?_lastContactUnit .
        {
          {
            ?_lastContactUnit rdf:value ?lastContactUnitValue .
            FILTER ( NOT EXISTS {?_lastContactUnit (aixm:uom | fixm:uom | plain:uom) ?lastContactUnitUoM})
            BIND(concat(\'val:\',?lastContactUnitValue) AS ?lastContactUnit)
          }
            UNION
          {
            ?_lastContactUnit
              rdf:value ?lastContactUnitValue ;
              (aixm:uom | fixm:uom | plain:uom) ?lastContactUnitUoM .
            BIND(concat(\'xval:\',STR(?lastContactUnitValue),\':\',?lastContactUnitUoM) AS ?lastContactUnit)
          }
            UNION
          {
           ?_lastContactUnit  aixm:nilReason ?lastContactUnitNilReason .
           BIND(concat(\'nil:\',?lastContactUnitNilReason) AS ?lastContactUnit)
          }
        }
      }
      OPTIONAL {?lastContact fixm:position ?position .}
    }
  }

      '
,row(Graph,LastContact,ContactFrequency,LastContactTime,LastContactUnit,Position),[]).

% fixm_ElapsedTimeLocation(Graph, ElapsedTimeLocation)

fixm_ElapsedTimeLocation(Graph, ElapsedTimeLocation) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?elapsedTimeLocation
WHERE
  { GRAPH ?graph
    {
      ?elapsedTimeLocation rdf:type fixm:ElapsedTimeLocation .
    }
  }

      '
,row(Graph,ElapsedTimeLocation),[]).

% aixm_Surface(Graph, Surface, HorizontalAccuracy?, Annotation*)

aixm_Surface(Graph, Surface, HorizontalAccuracy, AnnotationList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?surface ?horizontalAccuracy (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat)
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* aixm:Surface .
  }
  { GRAPH ?graph
    {
      ?surface rdf:type ?SUBCLASS .
      OPTIONAL { ?surface aixm:horizontalAccuracy ?_horizontalAccuracy .
        {
          {
            ?_horizontalAccuracy rdf:value ?horizontalAccuracyValue .
            FILTER ( NOT EXISTS {?_horizontalAccuracy (aixm:uom | fixm:uom | plain:uom) ?horizontalAccuracyUoM})
            BIND(concat(\'val:\',?horizontalAccuracyValue) AS ?horizontalAccuracy)
          }
            UNION
          {
            ?_horizontalAccuracy
              rdf:value ?horizontalAccuracyValue ;
              (aixm:uom | fixm:uom | plain:uom) ?horizontalAccuracyUoM .
            BIND(concat(\'xval:\',STR(?horizontalAccuracyValue),\':\',?horizontalAccuracyUoM) AS ?horizontalAccuracy)
          }
            UNION
          {
           ?_horizontalAccuracy  aixm:nilReason ?horizontalAccuracyNilReason .
           BIND(concat(\'nil:\',?horizontalAccuracyNilReason) AS ?horizontalAccuracy)
          }
        }
      }
      OPTIONAL {?surface aixm:annotation ?annotation .}
    }
  }
}
GROUP BY ?graph ?surface ?horizontalAccuracy

      '
,row(Graph,Surface,HorizontalAccuracy,AnnotationConcat),[]), convert(AnnotationConcat,AnnotationList).

% gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition)

gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?timePeriod ?beginPosition ?endPosition
WHERE
  { GRAPH ?graph
    {
      ?timePeriod rdf:type gml:TimePeriod .
      ?timePeriod gml:beginPosition  ?_beginPosition .
        {
          {
            ?_beginPosition rdf:value ?beginPositionValue .
            FILTER ( NOT EXISTS {?_beginPosition (aixm:uom | fixm:uom | plain:uom) ?beginPositionUoM})
            BIND(concat(\'val:\',?beginPositionValue) AS ?beginPosition)
          }
		     UNION
		     {
            ?_beginPosition
              rdf:value ?beginPositionValue ;
              (aixm:uom | fixm:uom | plain:uom) ?beginPositionUoM .
              BIND(concat(\'xval:\',STR(?beginPositionValue),\':\',?beginPositionUoM) AS ?beginPosition)
          }
          UNION
          {
		       ?_beginPosition  aixm:nilReason ?beginPositionNilReason .
		       BIND(concat(\'nil:\',?beginPositionNilReason) AS ?beginPosition)
		   }
      }
      ?timePeriod gml:endPosition  ?_endPosition .
        {
          {
            ?_endPosition rdf:value ?endPositionValue .
            FILTER ( NOT EXISTS {?_endPosition (aixm:uom | fixm:uom | plain:uom) ?endPositionUoM})
            BIND(concat(\'val:\',?endPositionValue) AS ?endPosition)
          }
		     UNION
		     {
            ?_endPosition
              rdf:value ?endPositionValue ;
              (aixm:uom | fixm:uom | plain:uom) ?endPositionUoM .
              BIND(concat(\'xval:\',STR(?endPositionValue),\':\',?endPositionUoM) AS ?endPosition)
          }
          UNION
          {
		       ?_endPosition  aixm:nilReason ?endPositionNilReason .
		       BIND(concat(\'nil:\',?endPositionNilReason) AS ?endPosition)
		   }
      }
    }
  }

      '
,row(Graph,TimePeriod,BeginPosition,EndPosition),[]).

% fixm_AircraftCapabilities(Graph, AircraftCapabilities, Survival?, Communication?, Navigation?, Surveillance?, StandardCapabilities?)

fixm_AircraftCapabilities(Graph, AircraftCapabilities, Survival, Communication, Navigation, Surveillance, StandardCapabilities) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?aircraftCapabilities ?survival ?communication ?navigation ?surveillance ?standardCapabilities
WHERE
  { GRAPH ?graph
    {
      ?aircraftCapabilities rdf:type fixm:AircraftCapabilities .
      OPTIONAL {?aircraftCapabilities fixm:survival ?survival .}
      OPTIONAL {?aircraftCapabilities fixm:communication ?communication .}
      OPTIONAL {?aircraftCapabilities fixm:navigation ?navigation .}
      OPTIONAL {?aircraftCapabilities fixm:surveillance ?surveillance .}
      OPTIONAL { ?aircraftCapabilities fixm:standardCapabilities ?_standardCapabilities .
        {
          {
            ?_standardCapabilities rdf:value ?standardCapabilitiesValue .
            FILTER ( NOT EXISTS {?_standardCapabilities (aixm:uom | fixm:uom | plain:uom) ?standardCapabilitiesUoM})
            BIND(concat(\'val:\',?standardCapabilitiesValue) AS ?standardCapabilities)
          }
            UNION
          {
            ?_standardCapabilities
              rdf:value ?standardCapabilitiesValue ;
              (aixm:uom | fixm:uom | plain:uom) ?standardCapabilitiesUoM .
            BIND(concat(\'xval:\',STR(?standardCapabilitiesValue),\':\',?standardCapabilitiesUoM) AS ?standardCapabilities)
          }
            UNION
          {
           ?_standardCapabilities  aixm:nilReason ?standardCapabilitiesNilReason .
           BIND(concat(\'nil:\',?standardCapabilitiesNilReason) AS ?standardCapabilities)
          }
        }
      }
    }
  }

      '
,row(Graph,AircraftCapabilities,Survival,Communication,Navigation,Surveillance,StandardCapabilities),[]).

% fixm_SpeedSchedule(Graph, SpeedSchedule, InitialSpeed?, SubsequentSpeed?)

fixm_SpeedSchedule(Graph, SpeedSchedule, InitialSpeed, SubsequentSpeed) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?speedSchedule ?initialSpeed ?subsequentSpeed
WHERE
  { GRAPH ?graph
    {
      ?speedSchedule rdf:type fixm:SpeedSchedule .
      OPTIONAL { ?speedSchedule fixm:initialSpeed ?_initialSpeed .
        {
          {
            ?_initialSpeed rdf:value ?initialSpeedValue .
            FILTER ( NOT EXISTS {?_initialSpeed (aixm:uom | fixm:uom | plain:uom) ?initialSpeedUoM})
            BIND(concat(\'val:\',?initialSpeedValue) AS ?initialSpeed)
          }
            UNION
          {
            ?_initialSpeed
              rdf:value ?initialSpeedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?initialSpeedUoM .
            BIND(concat(\'xval:\',STR(?initialSpeedValue),\':\',?initialSpeedUoM) AS ?initialSpeed)
          }
            UNION
          {
           ?_initialSpeed  aixm:nilReason ?initialSpeedNilReason .
           BIND(concat(\'nil:\',?initialSpeedNilReason) AS ?initialSpeed)
          }
        }
      }
      OPTIONAL { ?speedSchedule fixm:subsequentSpeed ?_subsequentSpeed .
        {
          {
            ?_subsequentSpeed rdf:value ?subsequentSpeedValue .
            FILTER ( NOT EXISTS {?_subsequentSpeed (aixm:uom | fixm:uom | plain:uom) ?subsequentSpeedUoM})
            BIND(concat(\'val:\',?subsequentSpeedValue) AS ?subsequentSpeed)
          }
            UNION
          {
            ?_subsequentSpeed
              rdf:value ?subsequentSpeedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?subsequentSpeedUoM .
            BIND(concat(\'xval:\',STR(?subsequentSpeedValue),\':\',?subsequentSpeedUoM) AS ?subsequentSpeed)
          }
            UNION
          {
           ?_subsequentSpeed  aixm:nilReason ?subsequentSpeedNilReason .
           BIND(concat(\'nil:\',?subsequentSpeedNilReason) AS ?subsequentSpeed)
          }
        }
      }
    }
  }

      '
,row(Graph,SpeedSchedule,InitialSpeed,SubsequentSpeed),[]).

% aixm_OrganisationAuthorityTimeSlice(Graph, OrganisationAuthorityTimeSlice, Name?, Designator?, Type?, Military?, Annotation*, Contact*, RelatedOrganisationAuthority*)

aixm_OrganisationAuthorityTimeSlice(Graph, OrganisationAuthorityTimeSlice, Name, Designator, Type, Military, AnnotationList, ContactList, RelatedOrganisationAuthorityList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?organisationAuthorityTimeSlice ?name ?designator ?type ?military (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat) (GROUP_CONCAT(DISTINCT ?contact;SEPARATOR=",") AS ?contactConcat) (GROUP_CONCAT(DISTINCT ?relatedOrganisationAuthority;SEPARATOR=",") AS ?relatedOrganisationAuthorityConcat)
WHERE
  { GRAPH ?graph
    {
      ?organisationAuthorityTimeSlice rdf:type aixm:OrganisationAuthorityTimeSlice .
      OPTIONAL { ?organisationAuthorityTimeSlice aixm:name ?_name .
        {
          {
            ?_name rdf:value ?nameValue .
            FILTER ( NOT EXISTS {?_name (aixm:uom | fixm:uom | plain:uom) ?nameUoM})
            BIND(concat(\'val:\',?nameValue) AS ?name)
          }
            UNION
          {
            ?_name
              rdf:value ?nameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?nameUoM .
            BIND(concat(\'xval:\',STR(?nameValue),\':\',?nameUoM) AS ?name)
          }
            UNION
          {
           ?_name  aixm:nilReason ?nameNilReason .
           BIND(concat(\'nil:\',?nameNilReason) AS ?name)
          }
        }
      }
      OPTIONAL { ?organisationAuthorityTimeSlice aixm:designator ?_designator .
        {
          {
            ?_designator rdf:value ?designatorValue .
            FILTER ( NOT EXISTS {?_designator (aixm:uom | fixm:uom | plain:uom) ?designatorUoM})
            BIND(concat(\'val:\',?designatorValue) AS ?designator)
          }
            UNION
          {
            ?_designator
              rdf:value ?designatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?designatorUoM .
            BIND(concat(\'xval:\',STR(?designatorValue),\':\',?designatorUoM) AS ?designator)
          }
            UNION
          {
           ?_designator  aixm:nilReason ?designatorNilReason .
           BIND(concat(\'nil:\',?designatorNilReason) AS ?designator)
          }
        }
      }
      OPTIONAL { ?organisationAuthorityTimeSlice aixm:type ?_type .
        {
          {
            ?_type rdf:value ?typeValue .
            FILTER ( NOT EXISTS {?_type (aixm:uom | fixm:uom | plain:uom) ?typeUoM})
            BIND(concat(\'val:\',?typeValue) AS ?type)
          }
            UNION
          {
            ?_type
              rdf:value ?typeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?typeUoM .
            BIND(concat(\'xval:\',STR(?typeValue),\':\',?typeUoM) AS ?type)
          }
            UNION
          {
           ?_type  aixm:nilReason ?typeNilReason .
           BIND(concat(\'nil:\',?typeNilReason) AS ?type)
          }
        }
      }
      OPTIONAL { ?organisationAuthorityTimeSlice aixm:military ?_military .
        {
          {
            ?_military rdf:value ?militaryValue .
            FILTER ( NOT EXISTS {?_military (aixm:uom | fixm:uom | plain:uom) ?militaryUoM})
            BIND(concat(\'val:\',?militaryValue) AS ?military)
          }
            UNION
          {
            ?_military
              rdf:value ?militaryValue ;
              (aixm:uom | fixm:uom | plain:uom) ?militaryUoM .
            BIND(concat(\'xval:\',STR(?militaryValue),\':\',?militaryUoM) AS ?military)
          }
            UNION
          {
           ?_military  aixm:nilReason ?militaryNilReason .
           BIND(concat(\'nil:\',?militaryNilReason) AS ?military)
          }
        }
      }
      OPTIONAL {?organisationAuthorityTimeSlice aixm:annotation ?annotation .}
      OPTIONAL {?organisationAuthorityTimeSlice aixm:contact ?contact .}
      OPTIONAL {?organisationAuthorityTimeSlice aixm:relatedOrganisationAuthority ?relatedOrganisationAuthority .}
    }
  }
GROUP BY ?graph ?organisationAuthorityTimeSlice ?name ?designator ?type ?military

      '
,row(Graph,OrganisationAuthorityTimeSlice,Name,Designator,Type,Military,AnnotationConcat,ContactConcat,RelatedOrganisationAuthorityConcat),[]), convert(AnnotationConcat,AnnotationList), convert(ContactConcat,ContactList), convert(RelatedOrganisationAuthorityConcat,RelatedOrganisationAuthorityList).

% fixm_EnRoute(Graph, EnRoute, AlternateAerodrome*, FleetPrioritization?, BoundaryCrossings*, CpdlcConnection?, BeaconCodeAssignment?, Cleared?, ControlElement*, Pointout?, Position?)

fixm_EnRoute(Graph, EnRoute, AlternateAerodromeList, FleetPrioritization, BoundaryCrossingsList, CpdlcConnection, BeaconCodeAssignment, Cleared, ControlElementList, Pointout, Position) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?enRoute (GROUP_CONCAT(DISTINCT ?alternateAerodrome;SEPARATOR=",") AS ?alternateAerodromeConcat) ?fleetPrioritization (GROUP_CONCAT(DISTINCT ?boundaryCrossings;SEPARATOR=",") AS ?boundaryCrossingsConcat) ?cpdlcConnection ?beaconCodeAssignment ?cleared (GROUP_CONCAT(DISTINCT ?controlElement;SEPARATOR=",") AS ?controlElementConcat) ?pointout ?position
WHERE
  { GRAPH ?graph
    {
      ?enRoute rdf:type fixm:EnRoute .
      OPTIONAL {?enRoute fixm:alternateAerodrome ?alternateAerodrome .}
      OPTIONAL { ?enRoute fixm:fleetPrioritization ?_fleetPrioritization .
        {
          {
            ?_fleetPrioritization rdf:value ?fleetPrioritizationValue .
            FILTER ( NOT EXISTS {?_fleetPrioritization (aixm:uom | fixm:uom | plain:uom) ?fleetPrioritizationUoM})
            BIND(concat(\'val:\',?fleetPrioritizationValue) AS ?fleetPrioritization)
          }
            UNION
          {
            ?_fleetPrioritization
              rdf:value ?fleetPrioritizationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?fleetPrioritizationUoM .
            BIND(concat(\'xval:\',STR(?fleetPrioritizationValue),\':\',?fleetPrioritizationUoM) AS ?fleetPrioritization)
          }
            UNION
          {
           ?_fleetPrioritization  aixm:nilReason ?fleetPrioritizationNilReason .
           BIND(concat(\'nil:\',?fleetPrioritizationNilReason) AS ?fleetPrioritization)
          }
        }
      }
      OPTIONAL {?enRoute fixm:boundaryCrossings ?boundaryCrossings .}
      OPTIONAL {?enRoute fixm:cpdlcConnection ?cpdlcConnection .}
      OPTIONAL {?enRoute fixm:beaconCodeAssignment ?beaconCodeAssignment .}
      OPTIONAL {?enRoute fixm:cleared ?cleared .}
      OPTIONAL { ?enRoute fixm:controlElement ?_controlElement .
        {
          {
            ?_controlElement rdf:value ?controlElementValue .
            FILTER ( NOT EXISTS {?_controlElement (aixm:uom | fixm:uom | plain:uom) ?controlElementUoM})
            BIND(concat(\'val:\',?controlElementValue) AS ?controlElement)
          }
            UNION
          {
            ?_controlElement
              rdf:value ?controlElementValue ;
              (aixm:uom | fixm:uom | plain:uom) ?controlElementUoM .
            BIND(concat(\'xval:\',STR(?controlElementValue),\':\',?controlElementUoM) AS ?controlElement)
          }
            UNION
          {
           ?_controlElement  aixm:nilReason ?controlElementNilReason .
           BIND(concat(\'nil:\',?controlElementNilReason) AS ?controlElement)
          }
        }
      }
      OPTIONAL {?enRoute fixm:pointout ?pointout .}
      OPTIONAL {?enRoute fixm:position ?position .}
    }
  }
GROUP BY ?graph ?enRoute ?fleetPrioritization ?cpdlcConnection ?beaconCodeAssignment ?cleared ?pointout ?position

      '
,row(Graph,EnRoute,AlternateAerodromeConcat,FleetPrioritization,BoundaryCrossingsConcat,CpdlcConnection,BeaconCodeAssignment,Cleared,ControlElementConcat,Pointout,Position),[]), convert(AlternateAerodromeConcat,AlternateAerodromeList), convert(BoundaryCrossingsConcat,BoundaryCrossingsList), convert(ControlElementConcat,ControlElementList).

% fixm_FlightLevel(Graph, FlightLevel, Level?, Unit?)

fixm_FlightLevel(Graph, FlightLevel, Level, Unit) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?flightLevel ?level ?unit
WHERE
  { GRAPH ?graph
    {
      ?flightLevel rdf:type fixm:FlightLevel .
      OPTIONAL { ?flightLevel fixm:level ?_level .
        {
          {
            ?_level rdf:value ?levelValue .
            FILTER ( NOT EXISTS {?_level (aixm:uom | fixm:uom | plain:uom) ?levelUoM})
            BIND(concat(\'val:\',?levelValue) AS ?level)
          }
            UNION
          {
            ?_level
              rdf:value ?levelValue ;
              (aixm:uom | fixm:uom | plain:uom) ?levelUoM .
            BIND(concat(\'xval:\',STR(?levelValue),\':\',?levelUoM) AS ?level)
          }
            UNION
          {
           ?_level  aixm:nilReason ?levelNilReason .
           BIND(concat(\'nil:\',?levelNilReason) AS ?level)
          }
        }
      }
      OPTIONAL { ?flightLevel fixm:unit ?_unit .
        {
          {
            ?_unit rdf:value ?unitValue .
            FILTER ( NOT EXISTS {?_unit (aixm:uom | fixm:uom | plain:uom) ?unitUoM})
            BIND(concat(\'val:\',?unitValue) AS ?unit)
          }
            UNION
          {
            ?_unit
              rdf:value ?unitValue ;
              (aixm:uom | fixm:uom | plain:uom) ?unitUoM .
            BIND(concat(\'xval:\',STR(?unitValue),\':\',?unitUoM) AS ?unit)
          }
            UNION
          {
           ?_unit  aixm:nilReason ?unitNilReason .
           BIND(concat(\'nil:\',?unitNilReason) AS ?unit)
          }
        }
      }
    }
  }

      '
,row(Graph,FlightLevel,Level,Unit),[]).

% fixm_LateralOfftrack(Graph, LateralOfftrack, OfftrackDistance*, OfftrackReason?)

fixm_LateralOfftrack(Graph, LateralOfftrack, OfftrackDistanceList, OfftrackReason) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?lateralOfftrack (GROUP_CONCAT(DISTINCT ?offtrackDistance;SEPARATOR=",") AS ?offtrackDistanceConcat) ?offtrackReason
WHERE
  { GRAPH ?graph
    {
      ?lateralOfftrack rdf:type fixm:LateralOfftrack .
      OPTIONAL {?lateralOfftrack fixm:offtrackDistance ?offtrackDistance .}
      OPTIONAL { ?lateralOfftrack fixm:offtrackReason ?_offtrackReason .
        {
          {
            ?_offtrackReason rdf:value ?offtrackReasonValue .
            FILTER ( NOT EXISTS {?_offtrackReason (aixm:uom | fixm:uom | plain:uom) ?offtrackReasonUoM})
            BIND(concat(\'val:\',?offtrackReasonValue) AS ?offtrackReason)
          }
            UNION
          {
            ?_offtrackReason
              rdf:value ?offtrackReasonValue ;
              (aixm:uom | fixm:uom | plain:uom) ?offtrackReasonUoM .
            BIND(concat(\'xval:\',STR(?offtrackReasonValue),\':\',?offtrackReasonUoM) AS ?offtrackReason)
          }
            UNION
          {
           ?_offtrackReason  aixm:nilReason ?offtrackReasonNilReason .
           BIND(concat(\'nil:\',?offtrackReasonNilReason) AS ?offtrackReason)
          }
        }
      }
    }
  }
GROUP BY ?graph ?lateralOfftrack ?offtrackReason

      '
,row(Graph,LateralOfftrack,OfftrackDistanceConcat,OfftrackReason),[]), convert(OfftrackDistanceConcat,OfftrackDistanceList).

% fixm_TemporalRange(Graph, TemporalRange, Earliest?, Latest?)

fixm_TemporalRange(Graph, TemporalRange, Earliest, Latest) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?temporalRange ?earliest ?latest
WHERE
  { GRAPH ?graph
    {
      ?temporalRange rdf:type fixm:TemporalRange .
      OPTIONAL { ?temporalRange fixm:earliest ?_earliest .
        {
          {
            ?_earliest rdf:value ?earliestValue .
            FILTER ( NOT EXISTS {?_earliest (aixm:uom | fixm:uom | plain:uom) ?earliestUoM})
            BIND(concat(\'val:\',?earliestValue) AS ?earliest)
          }
            UNION
          {
            ?_earliest
              rdf:value ?earliestValue ;
              (aixm:uom | fixm:uom | plain:uom) ?earliestUoM .
            BIND(concat(\'xval:\',STR(?earliestValue),\':\',?earliestUoM) AS ?earliest)
          }
            UNION
          {
           ?_earliest  aixm:nilReason ?earliestNilReason .
           BIND(concat(\'nil:\',?earliestNilReason) AS ?earliest)
          }
        }
      }
      OPTIONAL { ?temporalRange fixm:latest ?_latest .
        {
          {
            ?_latest rdf:value ?latestValue .
            FILTER ( NOT EXISTS {?_latest (aixm:uom | fixm:uom | plain:uom) ?latestUoM})
            BIND(concat(\'val:\',?latestValue) AS ?latest)
          }
            UNION
          {
            ?_latest
              rdf:value ?latestValue ;
              (aixm:uom | fixm:uom | plain:uom) ?latestUoM .
            BIND(concat(\'xval:\',STR(?latestValue),\':\',?latestUoM) AS ?latest)
          }
            UNION
          {
           ?_latest  aixm:nilReason ?latestNilReason .
           BIND(concat(\'nil:\',?latestNilReason) AS ?latest)
          }
        }
      }
    }
  }

      '
,row(Graph,TemporalRange,Earliest,Latest),[]).

% fixm_Aircraft(Graph, Aircraft, AircraftColours?, AircraftQuantity?, EngineType?, AircraftAddress?, Capabilities?, Registration?, AircraftType?, WakeTurbulence?, AircraftPerformance?)

fixm_Aircraft(Graph, Aircraft, AircraftColours, AircraftQuantity, EngineType, AircraftAddress, Capabilities, Registration, AircraftType, WakeTurbulence, AircraftPerformance) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?aircraft ?aircraftColours ?aircraftQuantity ?engineType ?aircraftAddress ?capabilities ?registration ?aircraftType ?wakeTurbulence ?aircraftPerformance
WHERE
  { GRAPH ?graph
    {
      ?aircraft rdf:type fixm:Aircraft .
      OPTIONAL { ?aircraft fixm:aircraftColours ?_aircraftColours .
        {
          {
            ?_aircraftColours rdf:value ?aircraftColoursValue .
            FILTER ( NOT EXISTS {?_aircraftColours (aixm:uom | fixm:uom | plain:uom) ?aircraftColoursUoM})
            BIND(concat(\'val:\',?aircraftColoursValue) AS ?aircraftColours)
          }
            UNION
          {
            ?_aircraftColours
              rdf:value ?aircraftColoursValue ;
              (aixm:uom | fixm:uom | plain:uom) ?aircraftColoursUoM .
            BIND(concat(\'xval:\',STR(?aircraftColoursValue),\':\',?aircraftColoursUoM) AS ?aircraftColours)
          }
            UNION
          {
           ?_aircraftColours  aixm:nilReason ?aircraftColoursNilReason .
           BIND(concat(\'nil:\',?aircraftColoursNilReason) AS ?aircraftColours)
          }
        }
      }
      OPTIONAL { ?aircraft fixm:aircraftQuantity ?_aircraftQuantity .
        {
          {
            ?_aircraftQuantity rdf:value ?aircraftQuantityValue .
            FILTER ( NOT EXISTS {?_aircraftQuantity (aixm:uom | fixm:uom | plain:uom) ?aircraftQuantityUoM})
            BIND(concat(\'val:\',?aircraftQuantityValue) AS ?aircraftQuantity)
          }
            UNION
          {
            ?_aircraftQuantity
              rdf:value ?aircraftQuantityValue ;
              (aixm:uom | fixm:uom | plain:uom) ?aircraftQuantityUoM .
            BIND(concat(\'xval:\',STR(?aircraftQuantityValue),\':\',?aircraftQuantityUoM) AS ?aircraftQuantity)
          }
            UNION
          {
           ?_aircraftQuantity  aixm:nilReason ?aircraftQuantityNilReason .
           BIND(concat(\'nil:\',?aircraftQuantityNilReason) AS ?aircraftQuantity)
          }
        }
      }
      OPTIONAL { ?aircraft fixm:engineType ?_engineType .
        {
          {
            ?_engineType rdf:value ?engineTypeValue .
            FILTER ( NOT EXISTS {?_engineType (aixm:uom | fixm:uom | plain:uom) ?engineTypeUoM})
            BIND(concat(\'val:\',?engineTypeValue) AS ?engineType)
          }
            UNION
          {
            ?_engineType
              rdf:value ?engineTypeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?engineTypeUoM .
            BIND(concat(\'xval:\',STR(?engineTypeValue),\':\',?engineTypeUoM) AS ?engineType)
          }
            UNION
          {
           ?_engineType  aixm:nilReason ?engineTypeNilReason .
           BIND(concat(\'nil:\',?engineTypeNilReason) AS ?engineType)
          }
        }
      }
      OPTIONAL { ?aircraft fixm:aircraftAddress ?_aircraftAddress .
        {
          {
            ?_aircraftAddress rdf:value ?aircraftAddressValue .
            FILTER ( NOT EXISTS {?_aircraftAddress (aixm:uom | fixm:uom | plain:uom) ?aircraftAddressUoM})
            BIND(concat(\'val:\',?aircraftAddressValue) AS ?aircraftAddress)
          }
            UNION
          {
            ?_aircraftAddress
              rdf:value ?aircraftAddressValue ;
              (aixm:uom | fixm:uom | plain:uom) ?aircraftAddressUoM .
            BIND(concat(\'xval:\',STR(?aircraftAddressValue),\':\',?aircraftAddressUoM) AS ?aircraftAddress)
          }
            UNION
          {
           ?_aircraftAddress  aixm:nilReason ?aircraftAddressNilReason .
           BIND(concat(\'nil:\',?aircraftAddressNilReason) AS ?aircraftAddress)
          }
        }
      }
      OPTIONAL {?aircraft fixm:capabilities ?capabilities .}
      OPTIONAL { ?aircraft fixm:registration ?_registration .
        {
          {
            ?_registration rdf:value ?registrationValue .
            FILTER ( NOT EXISTS {?_registration (aixm:uom | fixm:uom | plain:uom) ?registrationUoM})
            BIND(concat(\'val:\',?registrationValue) AS ?registration)
          }
            UNION
          {
            ?_registration
              rdf:value ?registrationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?registrationUoM .
            BIND(concat(\'xval:\',STR(?registrationValue),\':\',?registrationUoM) AS ?registration)
          }
            UNION
          {
           ?_registration  aixm:nilReason ?registrationNilReason .
           BIND(concat(\'nil:\',?registrationNilReason) AS ?registration)
          }
        }
      }
      OPTIONAL { ?aircraft fixm:aircraftType ?_aircraftType .
        {
          {
            ?_aircraftType rdf:value ?aircraftTypeValue .
            FILTER ( NOT EXISTS {?_aircraftType (aixm:uom | fixm:uom | plain:uom) ?aircraftTypeUoM})
            BIND(concat(\'val:\',?aircraftTypeValue) AS ?aircraftType)
          }
            UNION
          {
            ?_aircraftType
              rdf:value ?aircraftTypeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?aircraftTypeUoM .
            BIND(concat(\'xval:\',STR(?aircraftTypeValue),\':\',?aircraftTypeUoM) AS ?aircraftType)
          }
            UNION
          {
           ?_aircraftType  aixm:nilReason ?aircraftTypeNilReason .
           BIND(concat(\'nil:\',?aircraftTypeNilReason) AS ?aircraftType)
          }
        }
      }
      OPTIONAL { ?aircraft fixm:wakeTurbulence ?_wakeTurbulence .
        {
          {
            ?_wakeTurbulence rdf:value ?wakeTurbulenceValue .
            FILTER ( NOT EXISTS {?_wakeTurbulence (aixm:uom | fixm:uom | plain:uom) ?wakeTurbulenceUoM})
            BIND(concat(\'val:\',?wakeTurbulenceValue) AS ?wakeTurbulence)
          }
            UNION
          {
            ?_wakeTurbulence
              rdf:value ?wakeTurbulenceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?wakeTurbulenceUoM .
            BIND(concat(\'xval:\',STR(?wakeTurbulenceValue),\':\',?wakeTurbulenceUoM) AS ?wakeTurbulence)
          }
            UNION
          {
           ?_wakeTurbulence  aixm:nilReason ?wakeTurbulenceNilReason .
           BIND(concat(\'nil:\',?wakeTurbulenceNilReason) AS ?wakeTurbulence)
          }
        }
      }
      OPTIONAL { ?aircraft fixm:aircraftPerformance ?_aircraftPerformance .
        {
          {
            ?_aircraftPerformance rdf:value ?aircraftPerformanceValue .
            FILTER ( NOT EXISTS {?_aircraftPerformance (aixm:uom | fixm:uom | plain:uom) ?aircraftPerformanceUoM})
            BIND(concat(\'val:\',?aircraftPerformanceValue) AS ?aircraftPerformance)
          }
            UNION
          {
            ?_aircraftPerformance
              rdf:value ?aircraftPerformanceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?aircraftPerformanceUoM .
            BIND(concat(\'xval:\',STR(?aircraftPerformanceValue),\':\',?aircraftPerformanceUoM) AS ?aircraftPerformance)
          }
            UNION
          {
           ?_aircraftPerformance  aixm:nilReason ?aircraftPerformanceNilReason .
           BIND(concat(\'nil:\',?aircraftPerformanceNilReason) AS ?aircraftPerformance)
          }
        }
      }
    }
  }

      '
,row(Graph,Aircraft,AircraftColours,AircraftQuantity,EngineType,AircraftAddress,Capabilities,Registration,AircraftType,WakeTurbulence,AircraftPerformance),[]).

% fixm_OnlineContact(Graph, OnlineContact, Email?)

fixm_OnlineContact(Graph, OnlineContact, Email) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?onlineContact ?email
WHERE
  { GRAPH ?graph
    {
      ?onlineContact rdf:type fixm:OnlineContact .
      OPTIONAL { ?onlineContact fixm:email ?_email .
        {
          {
            ?_email rdf:value ?emailValue .
            FILTER ( NOT EXISTS {?_email (aixm:uom | fixm:uom | plain:uom) ?emailUoM})
            BIND(concat(\'val:\',?emailValue) AS ?email)
          }
            UNION
          {
            ?_email
              rdf:value ?emailValue ;
              (aixm:uom | fixm:uom | plain:uom) ?emailUoM .
            BIND(concat(\'xval:\',STR(?emailValue),\':\',?emailUoM) AS ?email)
          }
            UNION
          {
           ?_email  aixm:nilReason ?emailNilReason .
           BIND(concat(\'nil:\',?emailNilReason) AS ?email)
          }
        }
      }
    }
  }

      '
,row(Graph,OnlineContact,Email),[]).

% fixm_AirspaceConstraint(Graph, AirspaceConstraint, AirspaceControlledEntryTime?, ConstrainedAirspace?)

fixm_AirspaceConstraint(Graph, AirspaceConstraint, AirspaceControlledEntryTime, ConstrainedAirspace) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?airspaceConstraint ?airspaceControlledEntryTime ?constrainedAirspace
WHERE
  { GRAPH ?graph
    {
      ?airspaceConstraint rdf:type fixm:AirspaceConstraint .
      OPTIONAL { ?airspaceConstraint fixm:airspaceControlledEntryTime ?_airspaceControlledEntryTime .
        {
          {
            ?_airspaceControlledEntryTime rdf:value ?airspaceControlledEntryTimeValue .
            FILTER ( NOT EXISTS {?_airspaceControlledEntryTime (aixm:uom | fixm:uom | plain:uom) ?airspaceControlledEntryTimeUoM})
            BIND(concat(\'val:\',?airspaceControlledEntryTimeValue) AS ?airspaceControlledEntryTime)
          }
            UNION
          {
            ?_airspaceControlledEntryTime
              rdf:value ?airspaceControlledEntryTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?airspaceControlledEntryTimeUoM .
            BIND(concat(\'xval:\',STR(?airspaceControlledEntryTimeValue),\':\',?airspaceControlledEntryTimeUoM) AS ?airspaceControlledEntryTime)
          }
            UNION
          {
           ?_airspaceControlledEntryTime  aixm:nilReason ?airspaceControlledEntryTimeNilReason .
           BIND(concat(\'nil:\',?airspaceControlledEntryTimeNilReason) AS ?airspaceControlledEntryTime)
          }
        }
      }
      OPTIONAL { ?airspaceConstraint fixm:constrainedAirspace ?_constrainedAirspace .
        {
          {
            ?_constrainedAirspace rdf:value ?constrainedAirspaceValue .
            FILTER ( NOT EXISTS {?_constrainedAirspace (aixm:uom | fixm:uom | plain:uom) ?constrainedAirspaceUoM})
            BIND(concat(\'val:\',?constrainedAirspaceValue) AS ?constrainedAirspace)
          }
            UNION
          {
            ?_constrainedAirspace
              rdf:value ?constrainedAirspaceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?constrainedAirspaceUoM .
            BIND(concat(\'xval:\',STR(?constrainedAirspaceValue),\':\',?constrainedAirspaceUoM) AS ?constrainedAirspace)
          }
            UNION
          {
           ?_constrainedAirspace  aixm:nilReason ?constrainedAirspaceNilReason .
           BIND(concat(\'nil:\',?constrainedAirspaceNilReason) AS ?constrainedAirspace)
          }
        }
      }
    }
  }

      '
,row(Graph,AirspaceConstraint,AirspaceControlledEntryTime,ConstrainedAirspace),[]).

% fixm_TimeSequence(Graph, TimeSequence, Approval?, Begin?, End?, Ready?, Request?)

fixm_TimeSequence(Graph, TimeSequence, Approval, Begin, End, Ready, Request) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?timeSequence ?approval ?begin ?end ?ready ?request
WHERE
  { GRAPH ?graph
    {
      ?timeSequence rdf:type fixm:TimeSequence .
      OPTIONAL {?timeSequence fixm:approval ?approval .}
      OPTIONAL {?timeSequence fixm:begin ?begin .}
      OPTIONAL {?timeSequence fixm:end ?end .}
      OPTIONAL {?timeSequence fixm:ready ?ready .}
      OPTIONAL {?timeSequence fixm:request ?request .}
    }
  }

      '
,row(Graph,TimeSequence,Approval,Begin,End,Ready,Request),[]).

% fixm_AdditionalHandlingInformation(Graph, AdditionalHandlingInformation, ResponsibleAgent?)

fixm_AdditionalHandlingInformation(Graph, AdditionalHandlingInformation, ResponsibleAgent) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?additionalHandlingInformation ?responsibleAgent
WHERE
  { GRAPH ?graph
    {
      ?additionalHandlingInformation rdf:type fixm:AdditionalHandlingInformation .
      OPTIONAL { ?additionalHandlingInformation fixm:responsibleAgent ?_responsibleAgent .
        {
          {
            ?_responsibleAgent rdf:value ?responsibleAgentValue .
            FILTER ( NOT EXISTS {?_responsibleAgent (aixm:uom | fixm:uom | plain:uom) ?responsibleAgentUoM})
            BIND(concat(\'val:\',?responsibleAgentValue) AS ?responsibleAgent)
          }
            UNION
          {
            ?_responsibleAgent
              rdf:value ?responsibleAgentValue ;
              (aixm:uom | fixm:uom | plain:uom) ?responsibleAgentUoM .
            BIND(concat(\'xval:\',STR(?responsibleAgentValue),\':\',?responsibleAgentUoM) AS ?responsibleAgent)
          }
            UNION
          {
           ?_responsibleAgent  aixm:nilReason ?responsibleAgentNilReason .
           BIND(concat(\'nil:\',?responsibleAgentNilReason) AS ?responsibleAgent)
          }
        }
      }
    }
  }

      '
,row(Graph,AdditionalHandlingInformation,ResponsibleAgent),[]).

% fixm_AtcUnitReference(Graph, AtcUnitReference, SectorIdentifier?, Delegated?)

fixm_AtcUnitReference(Graph, AtcUnitReference, SectorIdentifier, Delegated) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?atcUnitReference ?sectorIdentifier ?delegated
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:AtcUnitReference .
  }
  { GRAPH ?graph
    {
      ?atcUnitReference rdf:type ?SUBCLASS .
      OPTIONAL { ?atcUnitReference fixm:sectorIdentifier ?_sectorIdentifier .
        {
          {
            ?_sectorIdentifier rdf:value ?sectorIdentifierValue .
            FILTER ( NOT EXISTS {?_sectorIdentifier (aixm:uom | fixm:uom | plain:uom) ?sectorIdentifierUoM})
            BIND(concat(\'val:\',?sectorIdentifierValue) AS ?sectorIdentifier)
          }
            UNION
          {
            ?_sectorIdentifier
              rdf:value ?sectorIdentifierValue ;
              (aixm:uom | fixm:uom | plain:uom) ?sectorIdentifierUoM .
            BIND(concat(\'xval:\',STR(?sectorIdentifierValue),\':\',?sectorIdentifierUoM) AS ?sectorIdentifier)
          }
            UNION
          {
           ?_sectorIdentifier  aixm:nilReason ?sectorIdentifierNilReason .
           BIND(concat(\'nil:\',?sectorIdentifierNilReason) AS ?sectorIdentifier)
          }
        }
      }
      OPTIONAL { ?atcUnitReference fixm:delegated ?_delegated .
        {
          {
            ?_delegated rdf:value ?delegatedValue .
            FILTER ( NOT EXISTS {?_delegated (aixm:uom | fixm:uom | plain:uom) ?delegatedUoM})
            BIND(concat(\'val:\',?delegatedValue) AS ?delegated)
          }
            UNION
          {
            ?_delegated
              rdf:value ?delegatedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?delegatedUoM .
            BIND(concat(\'xval:\',STR(?delegatedValue),\':\',?delegatedUoM) AS ?delegated)
          }
            UNION
          {
           ?_delegated  aixm:nilReason ?delegatedNilReason .
           BIND(concat(\'nil:\',?delegatedNilReason) AS ?delegated)
          }
        }
      }
    }
  }
}

      '
,row(Graph,AtcUnitReference,SectorIdentifier,Delegated),[]).

% fixm_Extension(Graph, Extension)

fixm_Extension(Graph, Extension) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?extension
WHERE
  { GRAPH ?graph
    {
      ?extension rdf:type fixm:Extension .
    }
  }

      '
,row(Graph,Extension),[]).

% fixm_SurveillanceCapabilities(Graph, SurveillanceCapabilities, OtherSurveillanceCapabilities?, SurveillanceCode*)

fixm_SurveillanceCapabilities(Graph, SurveillanceCapabilities, OtherSurveillanceCapabilities, SurveillanceCodeList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?surveillanceCapabilities ?otherSurveillanceCapabilities (GROUP_CONCAT(DISTINCT ?surveillanceCode;SEPARATOR=",") AS ?surveillanceCodeConcat)
WHERE
  { GRAPH ?graph
    {
      ?surveillanceCapabilities rdf:type fixm:SurveillanceCapabilities .
      OPTIONAL { ?surveillanceCapabilities fixm:otherSurveillanceCapabilities ?_otherSurveillanceCapabilities .
        {
          {
            ?_otherSurveillanceCapabilities rdf:value ?otherSurveillanceCapabilitiesValue .
            FILTER ( NOT EXISTS {?_otherSurveillanceCapabilities (aixm:uom | fixm:uom | plain:uom) ?otherSurveillanceCapabilitiesUoM})
            BIND(concat(\'val:\',?otherSurveillanceCapabilitiesValue) AS ?otherSurveillanceCapabilities)
          }
            UNION
          {
            ?_otherSurveillanceCapabilities
              rdf:value ?otherSurveillanceCapabilitiesValue ;
              (aixm:uom | fixm:uom | plain:uom) ?otherSurveillanceCapabilitiesUoM .
            BIND(concat(\'xval:\',STR(?otherSurveillanceCapabilitiesValue),\':\',?otherSurveillanceCapabilitiesUoM) AS ?otherSurveillanceCapabilities)
          }
            UNION
          {
           ?_otherSurveillanceCapabilities  aixm:nilReason ?otherSurveillanceCapabilitiesNilReason .
           BIND(concat(\'nil:\',?otherSurveillanceCapabilitiesNilReason) AS ?otherSurveillanceCapabilities)
          }
        }
      }
      OPTIONAL { ?surveillanceCapabilities fixm:surveillanceCode ?_surveillanceCode .
        {
          {
            ?_surveillanceCode rdf:value ?surveillanceCodeValue .
            FILTER ( NOT EXISTS {?_surveillanceCode (aixm:uom | fixm:uom | plain:uom) ?surveillanceCodeUoM})
            BIND(concat(\'val:\',?surveillanceCodeValue) AS ?surveillanceCode)
          }
            UNION
          {
            ?_surveillanceCode
              rdf:value ?surveillanceCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?surveillanceCodeUoM .
            BIND(concat(\'xval:\',STR(?surveillanceCodeValue),\':\',?surveillanceCodeUoM) AS ?surveillanceCode)
          }
            UNION
          {
           ?_surveillanceCode  aixm:nilReason ?surveillanceCodeNilReason .
           BIND(concat(\'nil:\',?surveillanceCodeNilReason) AS ?surveillanceCode)
          }
        }
      }
    }
  }
GROUP BY ?graph ?surveillanceCapabilities ?otherSurveillanceCapabilities

      '
,row(Graph,SurveillanceCapabilities,OtherSurveillanceCapabilities,SurveillanceCodeConcat),[]), convert(SurveillanceCodeConcat,SurveillanceCodeList).

% fixm_Trajectory(Graph, Trajectory, TrajectoryPoint*)

fixm_Trajectory(Graph, Trajectory, TrajectoryPointList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?trajectory (GROUP_CONCAT(DISTINCT ?trajectoryPoint;SEPARATOR=",") AS ?trajectoryPointConcat)
WHERE
  { GRAPH ?graph
    {
      ?trajectory rdf:type fixm:Trajectory .
      OPTIONAL {?trajectory fixm:trajectoryPoint ?trajectoryPoint .}
    }
  }
GROUP BY ?graph ?trajectory

      '
,row(Graph,Trajectory,TrajectoryPointConcat),[]), convert(TrajectoryPointConcat,TrajectoryPointList).

% aixm_AltimeterSourceTimeSlice(Graph, AltimeterSourceTimeSlice, IsRemote?, IsPrimary?, Availability*, Annotation*)

aixm_AltimeterSourceTimeSlice(Graph, AltimeterSourceTimeSlice, IsRemote, IsPrimary, AvailabilityList, AnnotationList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?altimeterSourceTimeSlice ?isRemote ?isPrimary (GROUP_CONCAT(DISTINCT ?availability;SEPARATOR=",") AS ?availabilityConcat) (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat)
WHERE
  { GRAPH ?graph
    {
      ?altimeterSourceTimeSlice rdf:type aixm:AltimeterSourceTimeSlice .
      OPTIONAL { ?altimeterSourceTimeSlice aixm:isRemote ?_isRemote .
        {
          {
            ?_isRemote rdf:value ?isRemoteValue .
            FILTER ( NOT EXISTS {?_isRemote (aixm:uom | fixm:uom | plain:uom) ?isRemoteUoM})
            BIND(concat(\'val:\',?isRemoteValue) AS ?isRemote)
          }
            UNION
          {
            ?_isRemote
              rdf:value ?isRemoteValue ;
              (aixm:uom | fixm:uom | plain:uom) ?isRemoteUoM .
            BIND(concat(\'xval:\',STR(?isRemoteValue),\':\',?isRemoteUoM) AS ?isRemote)
          }
            UNION
          {
           ?_isRemote  aixm:nilReason ?isRemoteNilReason .
           BIND(concat(\'nil:\',?isRemoteNilReason) AS ?isRemote)
          }
        }
      }
      OPTIONAL { ?altimeterSourceTimeSlice aixm:isPrimary ?_isPrimary .
        {
          {
            ?_isPrimary rdf:value ?isPrimaryValue .
            FILTER ( NOT EXISTS {?_isPrimary (aixm:uom | fixm:uom | plain:uom) ?isPrimaryUoM})
            BIND(concat(\'val:\',?isPrimaryValue) AS ?isPrimary)
          }
            UNION
          {
            ?_isPrimary
              rdf:value ?isPrimaryValue ;
              (aixm:uom | fixm:uom | plain:uom) ?isPrimaryUoM .
            BIND(concat(\'xval:\',STR(?isPrimaryValue),\':\',?isPrimaryUoM) AS ?isPrimary)
          }
            UNION
          {
           ?_isPrimary  aixm:nilReason ?isPrimaryNilReason .
           BIND(concat(\'nil:\',?isPrimaryNilReason) AS ?isPrimary)
          }
        }
      }
      OPTIONAL {?altimeterSourceTimeSlice aixm:availability ?availability .}
      OPTIONAL {?altimeterSourceTimeSlice aixm:annotation ?annotation .}
    }
  }
GROUP BY ?graph ?altimeterSourceTimeSlice ?isRemote ?isPrimary

      '
,row(Graph,AltimeterSourceTimeSlice,IsRemote,IsPrimary,AvailabilityConcat,AnnotationConcat),[]), convert(AvailabilityConcat,AvailabilityList), convert(AnnotationConcat,AnnotationList).

% aixm_Point(Graph, Point, HorizontalAccuracy?, Annotation*)

aixm_Point(Graph, Point, HorizontalAccuracy, AnnotationList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?point ?horizontalAccuracy (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat)
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* aixm:Point .
  }
  { GRAPH ?graph
    {
      ?point rdf:type ?SUBCLASS .
      OPTIONAL { ?point aixm:horizontalAccuracy ?_horizontalAccuracy .
        {
          {
            ?_horizontalAccuracy rdf:value ?horizontalAccuracyValue .
            FILTER ( NOT EXISTS {?_horizontalAccuracy (aixm:uom | fixm:uom | plain:uom) ?horizontalAccuracyUoM})
            BIND(concat(\'val:\',?horizontalAccuracyValue) AS ?horizontalAccuracy)
          }
            UNION
          {
            ?_horizontalAccuracy
              rdf:value ?horizontalAccuracyValue ;
              (aixm:uom | fixm:uom | plain:uom) ?horizontalAccuracyUoM .
            BIND(concat(\'xval:\',STR(?horizontalAccuracyValue),\':\',?horizontalAccuracyUoM) AS ?horizontalAccuracy)
          }
            UNION
          {
           ?_horizontalAccuracy  aixm:nilReason ?horizontalAccuracyNilReason .
           BIND(concat(\'nil:\',?horizontalAccuracyNilReason) AS ?horizontalAccuracy)
          }
        }
      }
      OPTIONAL {?point aixm:annotation ?annotation .}
    }
  }
}
GROUP BY ?graph ?point ?horizontalAccuracy

      '
,row(Graph,Point,HorizontalAccuracy,AnnotationConcat),[]), convert(AnnotationConcat,AnnotationList).

% aixm_AircraftCharacteristic(Graph, AircraftCharacteristic, Type?, Engine?, NumberEngine?, TypeAircraftICAO?, AircraftLandingCategory?, WingSpan?, WingSpanInterpretation?, ClassWingSpan?, Weight?, WeightInterpretation?, Passengers?, PassengersInterpretation?, Speed?, SpeedInterpretation?, WakeTurbulence?, NavigationEquipment?, NavigationSpecification?, VerticalSeparationCapability?, AntiCollisionAndSeparationEquipment?, CommunicationEquipment?, SurveillanceEquipment?, Annotation*)

aixm_AircraftCharacteristic(Graph, AircraftCharacteristic, Type, Engine, NumberEngine, TypeAircraftICAO, AircraftLandingCategory, WingSpan, WingSpanInterpretation, ClassWingSpan, Weight, WeightInterpretation, Passengers, PassengersInterpretation, Speed, SpeedInterpretation, WakeTurbulence, NavigationEquipment, NavigationSpecification, VerticalSeparationCapability, AntiCollisionAndSeparationEquipment, CommunicationEquipment, SurveillanceEquipment, AnnotationList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?aircraftCharacteristic ?type ?engine ?numberEngine ?typeAircraftICAO ?aircraftLandingCategory ?wingSpan ?wingSpanInterpretation ?classWingSpan ?weight ?weightInterpretation ?passengers ?passengersInterpretation ?speed ?speedInterpretation ?wakeTurbulence ?navigationEquipment ?navigationSpecification ?verticalSeparationCapability ?antiCollisionAndSeparationEquipment ?communicationEquipment ?surveillanceEquipment (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat)
WHERE
  { GRAPH ?graph
    {
      ?aircraftCharacteristic rdf:type aixm:AircraftCharacteristic .
      OPTIONAL { ?aircraftCharacteristic aixm:type ?_type .
        {
          {
            ?_type rdf:value ?typeValue .
            FILTER ( NOT EXISTS {?_type (aixm:uom | fixm:uom | plain:uom) ?typeUoM})
            BIND(concat(\'val:\',?typeValue) AS ?type)
          }
            UNION
          {
            ?_type
              rdf:value ?typeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?typeUoM .
            BIND(concat(\'xval:\',STR(?typeValue),\':\',?typeUoM) AS ?type)
          }
            UNION
          {
           ?_type  aixm:nilReason ?typeNilReason .
           BIND(concat(\'nil:\',?typeNilReason) AS ?type)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:engine ?_engine .
        {
          {
            ?_engine rdf:value ?engineValue .
            FILTER ( NOT EXISTS {?_engine (aixm:uom | fixm:uom | plain:uom) ?engineUoM})
            BIND(concat(\'val:\',?engineValue) AS ?engine)
          }
            UNION
          {
            ?_engine
              rdf:value ?engineValue ;
              (aixm:uom | fixm:uom | plain:uom) ?engineUoM .
            BIND(concat(\'xval:\',STR(?engineValue),\':\',?engineUoM) AS ?engine)
          }
            UNION
          {
           ?_engine  aixm:nilReason ?engineNilReason .
           BIND(concat(\'nil:\',?engineNilReason) AS ?engine)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:numberEngine ?_numberEngine .
        {
          {
            ?_numberEngine rdf:value ?numberEngineValue .
            FILTER ( NOT EXISTS {?_numberEngine (aixm:uom | fixm:uom | plain:uom) ?numberEngineUoM})
            BIND(concat(\'val:\',?numberEngineValue) AS ?numberEngine)
          }
            UNION
          {
            ?_numberEngine
              rdf:value ?numberEngineValue ;
              (aixm:uom | fixm:uom | plain:uom) ?numberEngineUoM .
            BIND(concat(\'xval:\',STR(?numberEngineValue),\':\',?numberEngineUoM) AS ?numberEngine)
          }
            UNION
          {
           ?_numberEngine  aixm:nilReason ?numberEngineNilReason .
           BIND(concat(\'nil:\',?numberEngineNilReason) AS ?numberEngine)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:typeAircraftICAO ?_typeAircraftICAO .
        {
          {
            ?_typeAircraftICAO rdf:value ?typeAircraftICAOValue .
            FILTER ( NOT EXISTS {?_typeAircraftICAO (aixm:uom | fixm:uom | plain:uom) ?typeAircraftICAOUoM})
            BIND(concat(\'val:\',?typeAircraftICAOValue) AS ?typeAircraftICAO)
          }
            UNION
          {
            ?_typeAircraftICAO
              rdf:value ?typeAircraftICAOValue ;
              (aixm:uom | fixm:uom | plain:uom) ?typeAircraftICAOUoM .
            BIND(concat(\'xval:\',STR(?typeAircraftICAOValue),\':\',?typeAircraftICAOUoM) AS ?typeAircraftICAO)
          }
            UNION
          {
           ?_typeAircraftICAO  aixm:nilReason ?typeAircraftICAONilReason .
           BIND(concat(\'nil:\',?typeAircraftICAONilReason) AS ?typeAircraftICAO)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:aircraftLandingCategory ?_aircraftLandingCategory .
        {
          {
            ?_aircraftLandingCategory rdf:value ?aircraftLandingCategoryValue .
            FILTER ( NOT EXISTS {?_aircraftLandingCategory (aixm:uom | fixm:uom | plain:uom) ?aircraftLandingCategoryUoM})
            BIND(concat(\'val:\',?aircraftLandingCategoryValue) AS ?aircraftLandingCategory)
          }
            UNION
          {
            ?_aircraftLandingCategory
              rdf:value ?aircraftLandingCategoryValue ;
              (aixm:uom | fixm:uom | plain:uom) ?aircraftLandingCategoryUoM .
            BIND(concat(\'xval:\',STR(?aircraftLandingCategoryValue),\':\',?aircraftLandingCategoryUoM) AS ?aircraftLandingCategory)
          }
            UNION
          {
           ?_aircraftLandingCategory  aixm:nilReason ?aircraftLandingCategoryNilReason .
           BIND(concat(\'nil:\',?aircraftLandingCategoryNilReason) AS ?aircraftLandingCategory)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:wingSpan ?_wingSpan .
        {
          {
            ?_wingSpan rdf:value ?wingSpanValue .
            FILTER ( NOT EXISTS {?_wingSpan (aixm:uom | fixm:uom | plain:uom) ?wingSpanUoM})
            BIND(concat(\'val:\',?wingSpanValue) AS ?wingSpan)
          }
            UNION
          {
            ?_wingSpan
              rdf:value ?wingSpanValue ;
              (aixm:uom | fixm:uom | plain:uom) ?wingSpanUoM .
            BIND(concat(\'xval:\',STR(?wingSpanValue),\':\',?wingSpanUoM) AS ?wingSpan)
          }
            UNION
          {
           ?_wingSpan  aixm:nilReason ?wingSpanNilReason .
           BIND(concat(\'nil:\',?wingSpanNilReason) AS ?wingSpan)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:wingSpanInterpretation ?_wingSpanInterpretation .
        {
          {
            ?_wingSpanInterpretation rdf:value ?wingSpanInterpretationValue .
            FILTER ( NOT EXISTS {?_wingSpanInterpretation (aixm:uom | fixm:uom | plain:uom) ?wingSpanInterpretationUoM})
            BIND(concat(\'val:\',?wingSpanInterpretationValue) AS ?wingSpanInterpretation)
          }
            UNION
          {
            ?_wingSpanInterpretation
              rdf:value ?wingSpanInterpretationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?wingSpanInterpretationUoM .
            BIND(concat(\'xval:\',STR(?wingSpanInterpretationValue),\':\',?wingSpanInterpretationUoM) AS ?wingSpanInterpretation)
          }
            UNION
          {
           ?_wingSpanInterpretation  aixm:nilReason ?wingSpanInterpretationNilReason .
           BIND(concat(\'nil:\',?wingSpanInterpretationNilReason) AS ?wingSpanInterpretation)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:classWingSpan ?_classWingSpan .
        {
          {
            ?_classWingSpan rdf:value ?classWingSpanValue .
            FILTER ( NOT EXISTS {?_classWingSpan (aixm:uom | fixm:uom | plain:uom) ?classWingSpanUoM})
            BIND(concat(\'val:\',?classWingSpanValue) AS ?classWingSpan)
          }
            UNION
          {
            ?_classWingSpan
              rdf:value ?classWingSpanValue ;
              (aixm:uom | fixm:uom | plain:uom) ?classWingSpanUoM .
            BIND(concat(\'xval:\',STR(?classWingSpanValue),\':\',?classWingSpanUoM) AS ?classWingSpan)
          }
            UNION
          {
           ?_classWingSpan  aixm:nilReason ?classWingSpanNilReason .
           BIND(concat(\'nil:\',?classWingSpanNilReason) AS ?classWingSpan)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:weight ?_weight .
        {
          {
            ?_weight rdf:value ?weightValue .
            FILTER ( NOT EXISTS {?_weight (aixm:uom | fixm:uom | plain:uom) ?weightUoM})
            BIND(concat(\'val:\',?weightValue) AS ?weight)
          }
            UNION
          {
            ?_weight
              rdf:value ?weightValue ;
              (aixm:uom | fixm:uom | plain:uom) ?weightUoM .
            BIND(concat(\'xval:\',STR(?weightValue),\':\',?weightUoM) AS ?weight)
          }
            UNION
          {
           ?_weight  aixm:nilReason ?weightNilReason .
           BIND(concat(\'nil:\',?weightNilReason) AS ?weight)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:weightInterpretation ?_weightInterpretation .
        {
          {
            ?_weightInterpretation rdf:value ?weightInterpretationValue .
            FILTER ( NOT EXISTS {?_weightInterpretation (aixm:uom | fixm:uom | plain:uom) ?weightInterpretationUoM})
            BIND(concat(\'val:\',?weightInterpretationValue) AS ?weightInterpretation)
          }
            UNION
          {
            ?_weightInterpretation
              rdf:value ?weightInterpretationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?weightInterpretationUoM .
            BIND(concat(\'xval:\',STR(?weightInterpretationValue),\':\',?weightInterpretationUoM) AS ?weightInterpretation)
          }
            UNION
          {
           ?_weightInterpretation  aixm:nilReason ?weightInterpretationNilReason .
           BIND(concat(\'nil:\',?weightInterpretationNilReason) AS ?weightInterpretation)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:passengers ?_passengers .
        {
          {
            ?_passengers rdf:value ?passengersValue .
            FILTER ( NOT EXISTS {?_passengers (aixm:uom | fixm:uom | plain:uom) ?passengersUoM})
            BIND(concat(\'val:\',?passengersValue) AS ?passengers)
          }
            UNION
          {
            ?_passengers
              rdf:value ?passengersValue ;
              (aixm:uom | fixm:uom | plain:uom) ?passengersUoM .
            BIND(concat(\'xval:\',STR(?passengersValue),\':\',?passengersUoM) AS ?passengers)
          }
            UNION
          {
           ?_passengers  aixm:nilReason ?passengersNilReason .
           BIND(concat(\'nil:\',?passengersNilReason) AS ?passengers)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:passengersInterpretation ?_passengersInterpretation .
        {
          {
            ?_passengersInterpretation rdf:value ?passengersInterpretationValue .
            FILTER ( NOT EXISTS {?_passengersInterpretation (aixm:uom | fixm:uom | plain:uom) ?passengersInterpretationUoM})
            BIND(concat(\'val:\',?passengersInterpretationValue) AS ?passengersInterpretation)
          }
            UNION
          {
            ?_passengersInterpretation
              rdf:value ?passengersInterpretationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?passengersInterpretationUoM .
            BIND(concat(\'xval:\',STR(?passengersInterpretationValue),\':\',?passengersInterpretationUoM) AS ?passengersInterpretation)
          }
            UNION
          {
           ?_passengersInterpretation  aixm:nilReason ?passengersInterpretationNilReason .
           BIND(concat(\'nil:\',?passengersInterpretationNilReason) AS ?passengersInterpretation)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:speed ?_speed .
        {
          {
            ?_speed rdf:value ?speedValue .
            FILTER ( NOT EXISTS {?_speed (aixm:uom | fixm:uom | plain:uom) ?speedUoM})
            BIND(concat(\'val:\',?speedValue) AS ?speed)
          }
            UNION
          {
            ?_speed
              rdf:value ?speedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?speedUoM .
            BIND(concat(\'xval:\',STR(?speedValue),\':\',?speedUoM) AS ?speed)
          }
            UNION
          {
           ?_speed  aixm:nilReason ?speedNilReason .
           BIND(concat(\'nil:\',?speedNilReason) AS ?speed)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:speedInterpretation ?_speedInterpretation .
        {
          {
            ?_speedInterpretation rdf:value ?speedInterpretationValue .
            FILTER ( NOT EXISTS {?_speedInterpretation (aixm:uom | fixm:uom | plain:uom) ?speedInterpretationUoM})
            BIND(concat(\'val:\',?speedInterpretationValue) AS ?speedInterpretation)
          }
            UNION
          {
            ?_speedInterpretation
              rdf:value ?speedInterpretationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?speedInterpretationUoM .
            BIND(concat(\'xval:\',STR(?speedInterpretationValue),\':\',?speedInterpretationUoM) AS ?speedInterpretation)
          }
            UNION
          {
           ?_speedInterpretation  aixm:nilReason ?speedInterpretationNilReason .
           BIND(concat(\'nil:\',?speedInterpretationNilReason) AS ?speedInterpretation)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:wakeTurbulence ?_wakeTurbulence .
        {
          {
            ?_wakeTurbulence rdf:value ?wakeTurbulenceValue .
            FILTER ( NOT EXISTS {?_wakeTurbulence (aixm:uom | fixm:uom | plain:uom) ?wakeTurbulenceUoM})
            BIND(concat(\'val:\',?wakeTurbulenceValue) AS ?wakeTurbulence)
          }
            UNION
          {
            ?_wakeTurbulence
              rdf:value ?wakeTurbulenceValue ;
              (aixm:uom | fixm:uom | plain:uom) ?wakeTurbulenceUoM .
            BIND(concat(\'xval:\',STR(?wakeTurbulenceValue),\':\',?wakeTurbulenceUoM) AS ?wakeTurbulence)
          }
            UNION
          {
           ?_wakeTurbulence  aixm:nilReason ?wakeTurbulenceNilReason .
           BIND(concat(\'nil:\',?wakeTurbulenceNilReason) AS ?wakeTurbulence)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:navigationEquipment ?_navigationEquipment .
        {
          {
            ?_navigationEquipment rdf:value ?navigationEquipmentValue .
            FILTER ( NOT EXISTS {?_navigationEquipment (aixm:uom | fixm:uom | plain:uom) ?navigationEquipmentUoM})
            BIND(concat(\'val:\',?navigationEquipmentValue) AS ?navigationEquipment)
          }
            UNION
          {
            ?_navigationEquipment
              rdf:value ?navigationEquipmentValue ;
              (aixm:uom | fixm:uom | plain:uom) ?navigationEquipmentUoM .
            BIND(concat(\'xval:\',STR(?navigationEquipmentValue),\':\',?navigationEquipmentUoM) AS ?navigationEquipment)
          }
            UNION
          {
           ?_navigationEquipment  aixm:nilReason ?navigationEquipmentNilReason .
           BIND(concat(\'nil:\',?navigationEquipmentNilReason) AS ?navigationEquipment)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:navigationSpecification ?_navigationSpecification .
        {
          {
            ?_navigationSpecification rdf:value ?navigationSpecificationValue .
            FILTER ( NOT EXISTS {?_navigationSpecification (aixm:uom | fixm:uom | plain:uom) ?navigationSpecificationUoM})
            BIND(concat(\'val:\',?navigationSpecificationValue) AS ?navigationSpecification)
          }
            UNION
          {
            ?_navigationSpecification
              rdf:value ?navigationSpecificationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?navigationSpecificationUoM .
            BIND(concat(\'xval:\',STR(?navigationSpecificationValue),\':\',?navigationSpecificationUoM) AS ?navigationSpecification)
          }
            UNION
          {
           ?_navigationSpecification  aixm:nilReason ?navigationSpecificationNilReason .
           BIND(concat(\'nil:\',?navigationSpecificationNilReason) AS ?navigationSpecification)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:verticalSeparationCapability ?_verticalSeparationCapability .
        {
          {
            ?_verticalSeparationCapability rdf:value ?verticalSeparationCapabilityValue .
            FILTER ( NOT EXISTS {?_verticalSeparationCapability (aixm:uom | fixm:uom | plain:uom) ?verticalSeparationCapabilityUoM})
            BIND(concat(\'val:\',?verticalSeparationCapabilityValue) AS ?verticalSeparationCapability)
          }
            UNION
          {
            ?_verticalSeparationCapability
              rdf:value ?verticalSeparationCapabilityValue ;
              (aixm:uom | fixm:uom | plain:uom) ?verticalSeparationCapabilityUoM .
            BIND(concat(\'xval:\',STR(?verticalSeparationCapabilityValue),\':\',?verticalSeparationCapabilityUoM) AS ?verticalSeparationCapability)
          }
            UNION
          {
           ?_verticalSeparationCapability  aixm:nilReason ?verticalSeparationCapabilityNilReason .
           BIND(concat(\'nil:\',?verticalSeparationCapabilityNilReason) AS ?verticalSeparationCapability)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:antiCollisionAndSeparationEquipment ?_antiCollisionAndSeparationEquipment .
        {
          {
            ?_antiCollisionAndSeparationEquipment rdf:value ?antiCollisionAndSeparationEquipmentValue .
            FILTER ( NOT EXISTS {?_antiCollisionAndSeparationEquipment (aixm:uom | fixm:uom | plain:uom) ?antiCollisionAndSeparationEquipmentUoM})
            BIND(concat(\'val:\',?antiCollisionAndSeparationEquipmentValue) AS ?antiCollisionAndSeparationEquipment)
          }
            UNION
          {
            ?_antiCollisionAndSeparationEquipment
              rdf:value ?antiCollisionAndSeparationEquipmentValue ;
              (aixm:uom | fixm:uom | plain:uom) ?antiCollisionAndSeparationEquipmentUoM .
            BIND(concat(\'xval:\',STR(?antiCollisionAndSeparationEquipmentValue),\':\',?antiCollisionAndSeparationEquipmentUoM) AS ?antiCollisionAndSeparationEquipment)
          }
            UNION
          {
           ?_antiCollisionAndSeparationEquipment  aixm:nilReason ?antiCollisionAndSeparationEquipmentNilReason .
           BIND(concat(\'nil:\',?antiCollisionAndSeparationEquipmentNilReason) AS ?antiCollisionAndSeparationEquipment)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:communicationEquipment ?_communicationEquipment .
        {
          {
            ?_communicationEquipment rdf:value ?communicationEquipmentValue .
            FILTER ( NOT EXISTS {?_communicationEquipment (aixm:uom | fixm:uom | plain:uom) ?communicationEquipmentUoM})
            BIND(concat(\'val:\',?communicationEquipmentValue) AS ?communicationEquipment)
          }
            UNION
          {
            ?_communicationEquipment
              rdf:value ?communicationEquipmentValue ;
              (aixm:uom | fixm:uom | plain:uom) ?communicationEquipmentUoM .
            BIND(concat(\'xval:\',STR(?communicationEquipmentValue),\':\',?communicationEquipmentUoM) AS ?communicationEquipment)
          }
            UNION
          {
           ?_communicationEquipment  aixm:nilReason ?communicationEquipmentNilReason .
           BIND(concat(\'nil:\',?communicationEquipmentNilReason) AS ?communicationEquipment)
          }
        }
      }
      OPTIONAL { ?aircraftCharacteristic aixm:surveillanceEquipment ?_surveillanceEquipment .
        {
          {
            ?_surveillanceEquipment rdf:value ?surveillanceEquipmentValue .
            FILTER ( NOT EXISTS {?_surveillanceEquipment (aixm:uom | fixm:uom | plain:uom) ?surveillanceEquipmentUoM})
            BIND(concat(\'val:\',?surveillanceEquipmentValue) AS ?surveillanceEquipment)
          }
            UNION
          {
            ?_surveillanceEquipment
              rdf:value ?surveillanceEquipmentValue ;
              (aixm:uom | fixm:uom | plain:uom) ?surveillanceEquipmentUoM .
            BIND(concat(\'xval:\',STR(?surveillanceEquipmentValue),\':\',?surveillanceEquipmentUoM) AS ?surveillanceEquipment)
          }
            UNION
          {
           ?_surveillanceEquipment  aixm:nilReason ?surveillanceEquipmentNilReason .
           BIND(concat(\'nil:\',?surveillanceEquipmentNilReason) AS ?surveillanceEquipment)
          }
        }
      }
      OPTIONAL {?aircraftCharacteristic aixm:annotation ?annotation .}
    }
  }
GROUP BY ?graph ?aircraftCharacteristic ?type ?engine ?numberEngine ?typeAircraftICAO ?aircraftLandingCategory ?wingSpan ?wingSpanInterpretation ?classWingSpan ?weight ?weightInterpretation ?passengers ?passengersInterpretation ?speed ?speedInterpretation ?wakeTurbulence ?navigationEquipment ?navigationSpecification ?verticalSeparationCapability ?antiCollisionAndSeparationEquipment ?communicationEquipment ?surveillanceEquipment

      '
,row(Graph,AircraftCharacteristic,Type,Engine,NumberEngine,TypeAircraftICAO,AircraftLandingCategory,WingSpan,WingSpanInterpretation,ClassWingSpan,Weight,WeightInterpretation,Passengers,PassengersInterpretation,Speed,SpeedInterpretation,WakeTurbulence,NavigationEquipment,NavigationSpecification,VerticalSeparationCapability,AntiCollisionAndSeparationEquipment,CommunicationEquipment,SurveillanceEquipment,AnnotationConcat),[]), convert(AnnotationConcat,AnnotationList).

% aixm_PostalAddress(Graph, PostalAddress, DeliveryPoint?, City?, AdministrativeArea?, PostalCode?, Country?)

aixm_PostalAddress(Graph, PostalAddress, DeliveryPoint, City, AdministrativeArea, PostalCode, Country) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?postalAddress ?deliveryPoint ?city ?administrativeArea ?postalCode ?country
WHERE
  { GRAPH ?graph
    {
      ?postalAddress rdf:type aixm:PostalAddress .
      OPTIONAL { ?postalAddress aixm:deliveryPoint ?_deliveryPoint .
        {
          {
            ?_deliveryPoint rdf:value ?deliveryPointValue .
            FILTER ( NOT EXISTS {?_deliveryPoint (aixm:uom | fixm:uom | plain:uom) ?deliveryPointUoM})
            BIND(concat(\'val:\',?deliveryPointValue) AS ?deliveryPoint)
          }
            UNION
          {
            ?_deliveryPoint
              rdf:value ?deliveryPointValue ;
              (aixm:uom | fixm:uom | plain:uom) ?deliveryPointUoM .
            BIND(concat(\'xval:\',STR(?deliveryPointValue),\':\',?deliveryPointUoM) AS ?deliveryPoint)
          }
            UNION
          {
           ?_deliveryPoint  aixm:nilReason ?deliveryPointNilReason .
           BIND(concat(\'nil:\',?deliveryPointNilReason) AS ?deliveryPoint)
          }
        }
      }
      OPTIONAL { ?postalAddress aixm:city ?_city .
        {
          {
            ?_city rdf:value ?cityValue .
            FILTER ( NOT EXISTS {?_city (aixm:uom | fixm:uom | plain:uom) ?cityUoM})
            BIND(concat(\'val:\',?cityValue) AS ?city)
          }
            UNION
          {
            ?_city
              rdf:value ?cityValue ;
              (aixm:uom | fixm:uom | plain:uom) ?cityUoM .
            BIND(concat(\'xval:\',STR(?cityValue),\':\',?cityUoM) AS ?city)
          }
            UNION
          {
           ?_city  aixm:nilReason ?cityNilReason .
           BIND(concat(\'nil:\',?cityNilReason) AS ?city)
          }
        }
      }
      OPTIONAL { ?postalAddress aixm:administrativeArea ?_administrativeArea .
        {
          {
            ?_administrativeArea rdf:value ?administrativeAreaValue .
            FILTER ( NOT EXISTS {?_administrativeArea (aixm:uom | fixm:uom | plain:uom) ?administrativeAreaUoM})
            BIND(concat(\'val:\',?administrativeAreaValue) AS ?administrativeArea)
          }
            UNION
          {
            ?_administrativeArea
              rdf:value ?administrativeAreaValue ;
              (aixm:uom | fixm:uom | plain:uom) ?administrativeAreaUoM .
            BIND(concat(\'xval:\',STR(?administrativeAreaValue),\':\',?administrativeAreaUoM) AS ?administrativeArea)
          }
            UNION
          {
           ?_administrativeArea  aixm:nilReason ?administrativeAreaNilReason .
           BIND(concat(\'nil:\',?administrativeAreaNilReason) AS ?administrativeArea)
          }
        }
      }
      OPTIONAL { ?postalAddress aixm:postalCode ?_postalCode .
        {
          {
            ?_postalCode rdf:value ?postalCodeValue .
            FILTER ( NOT EXISTS {?_postalCode (aixm:uom | fixm:uom | plain:uom) ?postalCodeUoM})
            BIND(concat(\'val:\',?postalCodeValue) AS ?postalCode)
          }
            UNION
          {
            ?_postalCode
              rdf:value ?postalCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?postalCodeUoM .
            BIND(concat(\'xval:\',STR(?postalCodeValue),\':\',?postalCodeUoM) AS ?postalCode)
          }
            UNION
          {
           ?_postalCode  aixm:nilReason ?postalCodeNilReason .
           BIND(concat(\'nil:\',?postalCodeNilReason) AS ?postalCode)
          }
        }
      }
      OPTIONAL { ?postalAddress aixm:country ?_country .
        {
          {
            ?_country rdf:value ?countryValue .
            FILTER ( NOT EXISTS {?_country (aixm:uom | fixm:uom | plain:uom) ?countryUoM})
            BIND(concat(\'val:\',?countryValue) AS ?country)
          }
            UNION
          {
            ?_country
              rdf:value ?countryValue ;
              (aixm:uom | fixm:uom | plain:uom) ?countryUoM .
            BIND(concat(\'xval:\',STR(?countryValue),\':\',?countryUoM) AS ?country)
          }
            UNION
          {
           ?_country  aixm:nilReason ?countryNilReason .
           BIND(concat(\'nil:\',?countryNilReason) AS ?country)
          }
        }
      }
    }
  }

      '
,row(Graph,PostalAddress,DeliveryPoint,City,AdministrativeArea,PostalCode,Country),[]).

% fixm_DangerousGoodsPackage(Graph, DangerousGoodsPackage, DangerousGoodsQuantity?, PackageDimensions?, PackingInstructionNumber?, ProductName?, ProperShippingName?, ReportableQuantity?, SupplementaryInformation?, TechnicalName?, TypeOfPackaging?, UnNumber?, DangerousGoodsLimitation?, ShipmentType?, AllPackedInOne?, CompatibilityGroup?, ShipmentDimensions?, MarinePollutantIndicator?, RadioactiveMaterials?, HazardClass?, PackingGroup?, Temperatures?, OverpackIndicator?, SubsidiaryHazardClass*)

fixm_DangerousGoodsPackage(Graph, DangerousGoodsPackage, DangerousGoodsQuantity, PackageDimensions, PackingInstructionNumber, ProductName, ProperShippingName, ReportableQuantity, SupplementaryInformation, TechnicalName, TypeOfPackaging, UnNumber, DangerousGoodsLimitation, ShipmentType, AllPackedInOne, CompatibilityGroup, ShipmentDimensions, MarinePollutantIndicator, RadioactiveMaterials, HazardClass, PackingGroup, Temperatures, OverpackIndicator, SubsidiaryHazardClassList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?dangerousGoodsPackage ?dangerousGoodsQuantity ?packageDimensions ?packingInstructionNumber ?productName ?properShippingName ?reportableQuantity ?supplementaryInformation ?technicalName ?typeOfPackaging ?unNumber ?dangerousGoodsLimitation ?shipmentType ?allPackedInOne ?compatibilityGroup ?shipmentDimensions ?marinePollutantIndicator ?radioactiveMaterials ?hazardClass ?packingGroup ?temperatures ?overpackIndicator (GROUP_CONCAT(DISTINCT ?subsidiaryHazardClass;SEPARATOR=",") AS ?subsidiaryHazardClassConcat)
WHERE
  { GRAPH ?graph
    {
      ?dangerousGoodsPackage rdf:type fixm:DangerousGoodsPackage .
      OPTIONAL { ?dangerousGoodsPackage fixm:dangerousGoodsQuantity ?_dangerousGoodsQuantity .
        {
          {
            ?_dangerousGoodsQuantity rdf:value ?dangerousGoodsQuantityValue .
            FILTER ( NOT EXISTS {?_dangerousGoodsQuantity (aixm:uom | fixm:uom | plain:uom) ?dangerousGoodsQuantityUoM})
            BIND(concat(\'val:\',?dangerousGoodsQuantityValue) AS ?dangerousGoodsQuantity)
          }
            UNION
          {
            ?_dangerousGoodsQuantity
              rdf:value ?dangerousGoodsQuantityValue ;
              (aixm:uom | fixm:uom | plain:uom) ?dangerousGoodsQuantityUoM .
            BIND(concat(\'xval:\',STR(?dangerousGoodsQuantityValue),\':\',?dangerousGoodsQuantityUoM) AS ?dangerousGoodsQuantity)
          }
            UNION
          {
           ?_dangerousGoodsQuantity  aixm:nilReason ?dangerousGoodsQuantityNilReason .
           BIND(concat(\'nil:\',?dangerousGoodsQuantityNilReason) AS ?dangerousGoodsQuantity)
          }
        }
      }
      OPTIONAL {?dangerousGoodsPackage fixm:packageDimensions ?packageDimensions .}
      OPTIONAL { ?dangerousGoodsPackage fixm:packingInstructionNumber ?_packingInstructionNumber .
        {
          {
            ?_packingInstructionNumber rdf:value ?packingInstructionNumberValue .
            FILTER ( NOT EXISTS {?_packingInstructionNumber (aixm:uom | fixm:uom | plain:uom) ?packingInstructionNumberUoM})
            BIND(concat(\'val:\',?packingInstructionNumberValue) AS ?packingInstructionNumber)
          }
            UNION
          {
            ?_packingInstructionNumber
              rdf:value ?packingInstructionNumberValue ;
              (aixm:uom | fixm:uom | plain:uom) ?packingInstructionNumberUoM .
            BIND(concat(\'xval:\',STR(?packingInstructionNumberValue),\':\',?packingInstructionNumberUoM) AS ?packingInstructionNumber)
          }
            UNION
          {
           ?_packingInstructionNumber  aixm:nilReason ?packingInstructionNumberNilReason .
           BIND(concat(\'nil:\',?packingInstructionNumberNilReason) AS ?packingInstructionNumber)
          }
        }
      }
      OPTIONAL { ?dangerousGoodsPackage fixm:productName ?_productName .
        {
          {
            ?_productName rdf:value ?productNameValue .
            FILTER ( NOT EXISTS {?_productName (aixm:uom | fixm:uom | plain:uom) ?productNameUoM})
            BIND(concat(\'val:\',?productNameValue) AS ?productName)
          }
            UNION
          {
            ?_productName
              rdf:value ?productNameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?productNameUoM .
            BIND(concat(\'xval:\',STR(?productNameValue),\':\',?productNameUoM) AS ?productName)
          }
            UNION
          {
           ?_productName  aixm:nilReason ?productNameNilReason .
           BIND(concat(\'nil:\',?productNameNilReason) AS ?productName)
          }
        }
      }
      OPTIONAL { ?dangerousGoodsPackage fixm:properShippingName ?_properShippingName .
        {
          {
            ?_properShippingName rdf:value ?properShippingNameValue .
            FILTER ( NOT EXISTS {?_properShippingName (aixm:uom | fixm:uom | plain:uom) ?properShippingNameUoM})
            BIND(concat(\'val:\',?properShippingNameValue) AS ?properShippingName)
          }
            UNION
          {
            ?_properShippingName
              rdf:value ?properShippingNameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?properShippingNameUoM .
            BIND(concat(\'xval:\',STR(?properShippingNameValue),\':\',?properShippingNameUoM) AS ?properShippingName)
          }
            UNION
          {
           ?_properShippingName  aixm:nilReason ?properShippingNameNilReason .
           BIND(concat(\'nil:\',?properShippingNameNilReason) AS ?properShippingName)
          }
        }
      }
      OPTIONAL { ?dangerousGoodsPackage fixm:reportableQuantity ?_reportableQuantity .
        {
          {
            ?_reportableQuantity rdf:value ?reportableQuantityValue .
            FILTER ( NOT EXISTS {?_reportableQuantity (aixm:uom | fixm:uom | plain:uom) ?reportableQuantityUoM})
            BIND(concat(\'val:\',?reportableQuantityValue) AS ?reportableQuantity)
          }
            UNION
          {
            ?_reportableQuantity
              rdf:value ?reportableQuantityValue ;
              (aixm:uom | fixm:uom | plain:uom) ?reportableQuantityUoM .
            BIND(concat(\'xval:\',STR(?reportableQuantityValue),\':\',?reportableQuantityUoM) AS ?reportableQuantity)
          }
            UNION
          {
           ?_reportableQuantity  aixm:nilReason ?reportableQuantityNilReason .
           BIND(concat(\'nil:\',?reportableQuantityNilReason) AS ?reportableQuantity)
          }
        }
      }
      OPTIONAL { ?dangerousGoodsPackage fixm:supplementaryInformation ?_supplementaryInformation .
        {
          {
            ?_supplementaryInformation rdf:value ?supplementaryInformationValue .
            FILTER ( NOT EXISTS {?_supplementaryInformation (aixm:uom | fixm:uom | plain:uom) ?supplementaryInformationUoM})
            BIND(concat(\'val:\',?supplementaryInformationValue) AS ?supplementaryInformation)
          }
            UNION
          {
            ?_supplementaryInformation
              rdf:value ?supplementaryInformationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?supplementaryInformationUoM .
            BIND(concat(\'xval:\',STR(?supplementaryInformationValue),\':\',?supplementaryInformationUoM) AS ?supplementaryInformation)
          }
            UNION
          {
           ?_supplementaryInformation  aixm:nilReason ?supplementaryInformationNilReason .
           BIND(concat(\'nil:\',?supplementaryInformationNilReason) AS ?supplementaryInformation)
          }
        }
      }
      OPTIONAL { ?dangerousGoodsPackage fixm:technicalName ?_technicalName .
        {
          {
            ?_technicalName rdf:value ?technicalNameValue .
            FILTER ( NOT EXISTS {?_technicalName (aixm:uom | fixm:uom | plain:uom) ?technicalNameUoM})
            BIND(concat(\'val:\',?technicalNameValue) AS ?technicalName)
          }
            UNION
          {
            ?_technicalName
              rdf:value ?technicalNameValue ;
              (aixm:uom | fixm:uom | plain:uom) ?technicalNameUoM .
            BIND(concat(\'xval:\',STR(?technicalNameValue),\':\',?technicalNameUoM) AS ?technicalName)
          }
            UNION
          {
           ?_technicalName  aixm:nilReason ?technicalNameNilReason .
           BIND(concat(\'nil:\',?technicalNameNilReason) AS ?technicalName)
          }
        }
      }
      OPTIONAL { ?dangerousGoodsPackage fixm:typeOfPackaging ?_typeOfPackaging .
        {
          {
            ?_typeOfPackaging rdf:value ?typeOfPackagingValue .
            FILTER ( NOT EXISTS {?_typeOfPackaging (aixm:uom | fixm:uom | plain:uom) ?typeOfPackagingUoM})
            BIND(concat(\'val:\',?typeOfPackagingValue) AS ?typeOfPackaging)
          }
            UNION
          {
            ?_typeOfPackaging
              rdf:value ?typeOfPackagingValue ;
              (aixm:uom | fixm:uom | plain:uom) ?typeOfPackagingUoM .
            BIND(concat(\'xval:\',STR(?typeOfPackagingValue),\':\',?typeOfPackagingUoM) AS ?typeOfPackaging)
          }
            UNION
          {
           ?_typeOfPackaging  aixm:nilReason ?typeOfPackagingNilReason .
           BIND(concat(\'nil:\',?typeOfPackagingNilReason) AS ?typeOfPackaging)
          }
        }
      }
      OPTIONAL { ?dangerousGoodsPackage fixm:unNumber ?_unNumber .
        {
          {
            ?_unNumber rdf:value ?unNumberValue .
            FILTER ( NOT EXISTS {?_unNumber (aixm:uom | fixm:uom | plain:uom) ?unNumberUoM})
            BIND(concat(\'val:\',?unNumberValue) AS ?unNumber)
          }
            UNION
          {
            ?_unNumber
              rdf:value ?unNumberValue ;
              (aixm:uom | fixm:uom | plain:uom) ?unNumberUoM .
            BIND(concat(\'xval:\',STR(?unNumberValue),\':\',?unNumberUoM) AS ?unNumber)
          }
            UNION
          {
           ?_unNumber  aixm:nilReason ?unNumberNilReason .
           BIND(concat(\'nil:\',?unNumberNilReason) AS ?unNumber)
          }
        }
      }
      OPTIONAL { ?dangerousGoodsPackage fixm:dangerousGoodsLimitation ?_dangerousGoodsLimitation .
        {
          {
            ?_dangerousGoodsLimitation rdf:value ?dangerousGoodsLimitationValue .
            FILTER ( NOT EXISTS {?_dangerousGoodsLimitation (aixm:uom | fixm:uom | plain:uom) ?dangerousGoodsLimitationUoM})
            BIND(concat(\'val:\',?dangerousGoodsLimitationValue) AS ?dangerousGoodsLimitation)
          }
            UNION
          {
            ?_dangerousGoodsLimitation
              rdf:value ?dangerousGoodsLimitationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?dangerousGoodsLimitationUoM .
            BIND(concat(\'xval:\',STR(?dangerousGoodsLimitationValue),\':\',?dangerousGoodsLimitationUoM) AS ?dangerousGoodsLimitation)
          }
            UNION
          {
           ?_dangerousGoodsLimitation  aixm:nilReason ?dangerousGoodsLimitationNilReason .
           BIND(concat(\'nil:\',?dangerousGoodsLimitationNilReason) AS ?dangerousGoodsLimitation)
          }
        }
      }
      OPTIONAL { ?dangerousGoodsPackage fixm:shipmentType ?_shipmentType .
        {
          {
            ?_shipmentType rdf:value ?shipmentTypeValue .
            FILTER ( NOT EXISTS {?_shipmentType (aixm:uom | fixm:uom | plain:uom) ?shipmentTypeUoM})
            BIND(concat(\'val:\',?shipmentTypeValue) AS ?shipmentType)
          }
            UNION
          {
            ?_shipmentType
              rdf:value ?shipmentTypeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?shipmentTypeUoM .
            BIND(concat(\'xval:\',STR(?shipmentTypeValue),\':\',?shipmentTypeUoM) AS ?shipmentType)
          }
            UNION
          {
           ?_shipmentType  aixm:nilReason ?shipmentTypeNilReason .
           BIND(concat(\'nil:\',?shipmentTypeNilReason) AS ?shipmentType)
          }
        }
      }
      OPTIONAL {?dangerousGoodsPackage fixm:allPackedInOne ?allPackedInOne .}
      OPTIONAL { ?dangerousGoodsPackage fixm:compatibilityGroup ?_compatibilityGroup .
        {
          {
            ?_compatibilityGroup rdf:value ?compatibilityGroupValue .
            FILTER ( NOT EXISTS {?_compatibilityGroup (aixm:uom | fixm:uom | plain:uom) ?compatibilityGroupUoM})
            BIND(concat(\'val:\',?compatibilityGroupValue) AS ?compatibilityGroup)
          }
            UNION
          {
            ?_compatibilityGroup
              rdf:value ?compatibilityGroupValue ;
              (aixm:uom | fixm:uom | plain:uom) ?compatibilityGroupUoM .
            BIND(concat(\'xval:\',STR(?compatibilityGroupValue),\':\',?compatibilityGroupUoM) AS ?compatibilityGroup)
          }
            UNION
          {
           ?_compatibilityGroup  aixm:nilReason ?compatibilityGroupNilReason .
           BIND(concat(\'nil:\',?compatibilityGroupNilReason) AS ?compatibilityGroup)
          }
        }
      }
      OPTIONAL {?dangerousGoodsPackage fixm:shipmentDimensions ?shipmentDimensions .}
      OPTIONAL { ?dangerousGoodsPackage fixm:marinePollutantIndicator ?_marinePollutantIndicator .
        {
          {
            ?_marinePollutantIndicator rdf:value ?marinePollutantIndicatorValue .
            FILTER ( NOT EXISTS {?_marinePollutantIndicator (aixm:uom | fixm:uom | plain:uom) ?marinePollutantIndicatorUoM})
            BIND(concat(\'val:\',?marinePollutantIndicatorValue) AS ?marinePollutantIndicator)
          }
            UNION
          {
            ?_marinePollutantIndicator
              rdf:value ?marinePollutantIndicatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?marinePollutantIndicatorUoM .
            BIND(concat(\'xval:\',STR(?marinePollutantIndicatorValue),\':\',?marinePollutantIndicatorUoM) AS ?marinePollutantIndicator)
          }
            UNION
          {
           ?_marinePollutantIndicator  aixm:nilReason ?marinePollutantIndicatorNilReason .
           BIND(concat(\'nil:\',?marinePollutantIndicatorNilReason) AS ?marinePollutantIndicator)
          }
        }
      }
      OPTIONAL {?dangerousGoodsPackage fixm:radioactiveMaterials ?radioactiveMaterials .}
      OPTIONAL { ?dangerousGoodsPackage fixm:hazardClass ?_hazardClass .
        {
          {
            ?_hazardClass rdf:value ?hazardClassValue .
            FILTER ( NOT EXISTS {?_hazardClass (aixm:uom | fixm:uom | plain:uom) ?hazardClassUoM})
            BIND(concat(\'val:\',?hazardClassValue) AS ?hazardClass)
          }
            UNION
          {
            ?_hazardClass
              rdf:value ?hazardClassValue ;
              (aixm:uom | fixm:uom | plain:uom) ?hazardClassUoM .
            BIND(concat(\'xval:\',STR(?hazardClassValue),\':\',?hazardClassUoM) AS ?hazardClass)
          }
            UNION
          {
           ?_hazardClass  aixm:nilReason ?hazardClassNilReason .
           BIND(concat(\'nil:\',?hazardClassNilReason) AS ?hazardClass)
          }
        }
      }
      OPTIONAL { ?dangerousGoodsPackage fixm:packingGroup ?_packingGroup .
        {
          {
            ?_packingGroup rdf:value ?packingGroupValue .
            FILTER ( NOT EXISTS {?_packingGroup (aixm:uom | fixm:uom | plain:uom) ?packingGroupUoM})
            BIND(concat(\'val:\',?packingGroupValue) AS ?packingGroup)
          }
            UNION
          {
            ?_packingGroup
              rdf:value ?packingGroupValue ;
              (aixm:uom | fixm:uom | plain:uom) ?packingGroupUoM .
            BIND(concat(\'xval:\',STR(?packingGroupValue),\':\',?packingGroupUoM) AS ?packingGroup)
          }
            UNION
          {
           ?_packingGroup  aixm:nilReason ?packingGroupNilReason .
           BIND(concat(\'nil:\',?packingGroupNilReason) AS ?packingGroup)
          }
        }
      }
      OPTIONAL {?dangerousGoodsPackage fixm:temperatures ?temperatures .}
      OPTIONAL { ?dangerousGoodsPackage fixm:overpackIndicator ?_overpackIndicator .
        {
          {
            ?_overpackIndicator rdf:value ?overpackIndicatorValue .
            FILTER ( NOT EXISTS {?_overpackIndicator (aixm:uom | fixm:uom | plain:uom) ?overpackIndicatorUoM})
            BIND(concat(\'val:\',?overpackIndicatorValue) AS ?overpackIndicator)
          }
            UNION
          {
            ?_overpackIndicator
              rdf:value ?overpackIndicatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?overpackIndicatorUoM .
            BIND(concat(\'xval:\',STR(?overpackIndicatorValue),\':\',?overpackIndicatorUoM) AS ?overpackIndicator)
          }
            UNION
          {
           ?_overpackIndicator  aixm:nilReason ?overpackIndicatorNilReason .
           BIND(concat(\'nil:\',?overpackIndicatorNilReason) AS ?overpackIndicator)
          }
        }
      }
      OPTIONAL { ?dangerousGoodsPackage fixm:subsidiaryHazardClass ?_subsidiaryHazardClass .
        {
          {
            ?_subsidiaryHazardClass rdf:value ?subsidiaryHazardClassValue .
            FILTER ( NOT EXISTS {?_subsidiaryHazardClass (aixm:uom | fixm:uom | plain:uom) ?subsidiaryHazardClassUoM})
            BIND(concat(\'val:\',?subsidiaryHazardClassValue) AS ?subsidiaryHazardClass)
          }
            UNION
          {
            ?_subsidiaryHazardClass
              rdf:value ?subsidiaryHazardClassValue ;
              (aixm:uom | fixm:uom | plain:uom) ?subsidiaryHazardClassUoM .
            BIND(concat(\'xval:\',STR(?subsidiaryHazardClassValue),\':\',?subsidiaryHazardClassUoM) AS ?subsidiaryHazardClass)
          }
            UNION
          {
           ?_subsidiaryHazardClass  aixm:nilReason ?subsidiaryHazardClassNilReason .
           BIND(concat(\'nil:\',?subsidiaryHazardClassNilReason) AS ?subsidiaryHazardClass)
          }
        }
      }
    }
  }
GROUP BY ?graph ?dangerousGoodsPackage ?dangerousGoodsQuantity ?packageDimensions ?packingInstructionNumber ?productName ?properShippingName ?reportableQuantity ?supplementaryInformation ?technicalName ?typeOfPackaging ?unNumber ?dangerousGoodsLimitation ?shipmentType ?allPackedInOne ?compatibilityGroup ?shipmentDimensions ?marinePollutantIndicator ?radioactiveMaterials ?hazardClass ?packingGroup ?temperatures ?overpackIndicator

      '
,row(Graph,DangerousGoodsPackage,DangerousGoodsQuantity,PackageDimensions,PackingInstructionNumber,ProductName,ProperShippingName,ReportableQuantity,SupplementaryInformation,TechnicalName,TypeOfPackaging,UnNumber,DangerousGoodsLimitation,ShipmentType,AllPackedInOne,CompatibilityGroup,ShipmentDimensions,MarinePollutantIndicator,RadioactiveMaterials,HazardClass,PackingGroup,Temperatures,OverpackIndicator,SubsidiaryHazardClassConcat),[]), convert(SubsidiaryHazardClassConcat,SubsidiaryHazardClassList).

% fixm_LastPositionReport(Graph, LastPositionReport, DeterminationMethod?, Position?, TimeAtPosition?)

fixm_LastPositionReport(Graph, LastPositionReport, DeterminationMethod, Position, TimeAtPosition) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?lastPositionReport ?determinationMethod ?position ?timeAtPosition
WHERE
  { GRAPH ?graph
    {
      ?lastPositionReport rdf:type fixm:LastPositionReport .
      OPTIONAL { ?lastPositionReport fixm:determinationMethod ?_determinationMethod .
        {
          {
            ?_determinationMethod rdf:value ?determinationMethodValue .
            FILTER ( NOT EXISTS {?_determinationMethod (aixm:uom | fixm:uom | plain:uom) ?determinationMethodUoM})
            BIND(concat(\'val:\',?determinationMethodValue) AS ?determinationMethod)
          }
            UNION
          {
            ?_determinationMethod
              rdf:value ?determinationMethodValue ;
              (aixm:uom | fixm:uom | plain:uom) ?determinationMethodUoM .
            BIND(concat(\'xval:\',STR(?determinationMethodValue),\':\',?determinationMethodUoM) AS ?determinationMethod)
          }
            UNION
          {
           ?_determinationMethod  aixm:nilReason ?determinationMethodNilReason .
           BIND(concat(\'nil:\',?determinationMethodNilReason) AS ?determinationMethod)
          }
        }
      }
      OPTIONAL {?lastPositionReport fixm:position ?position .}
      OPTIONAL { ?lastPositionReport fixm:timeAtPosition ?_timeAtPosition .
        {
          {
            ?_timeAtPosition rdf:value ?timeAtPositionValue .
            FILTER ( NOT EXISTS {?_timeAtPosition (aixm:uom | fixm:uom | plain:uom) ?timeAtPositionUoM})
            BIND(concat(\'val:\',?timeAtPositionValue) AS ?timeAtPosition)
          }
            UNION
          {
            ?_timeAtPosition
              rdf:value ?timeAtPositionValue ;
              (aixm:uom | fixm:uom | plain:uom) ?timeAtPositionUoM .
            BIND(concat(\'xval:\',STR(?timeAtPositionValue),\':\',?timeAtPositionUoM) AS ?timeAtPosition)
          }
            UNION
          {
           ?_timeAtPosition  aixm:nilReason ?timeAtPositionNilReason .
           BIND(concat(\'nil:\',?timeAtPositionNilReason) AS ?timeAtPosition)
          }
        }
      }
    }
  }

      '
,row(Graph,LastPositionReport,DeterminationMethod,Position,TimeAtPosition),[]).

% aixm_AltimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus?)

aixm_AltimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?altimeterSourceStatus ?operationalStatus
WHERE
  { GRAPH ?graph
    {
      ?altimeterSourceStatus rdf:type aixm:AltimeterSourceStatus .
      OPTIONAL { ?altimeterSourceStatus aixm:operationalStatus ?_operationalStatus .
        {
          {
            ?_operationalStatus rdf:value ?operationalStatusValue .
            FILTER ( NOT EXISTS {?_operationalStatus (aixm:uom | fixm:uom | plain:uom) ?operationalStatusUoM})
            BIND(concat(\'val:\',?operationalStatusValue) AS ?operationalStatus)
          }
            UNION
          {
            ?_operationalStatus
              rdf:value ?operationalStatusValue ;
              (aixm:uom | fixm:uom | plain:uom) ?operationalStatusUoM .
            BIND(concat(\'xval:\',STR(?operationalStatusValue),\':\',?operationalStatusUoM) AS ?operationalStatus)
          }
            UNION
          {
           ?_operationalStatus  aixm:nilReason ?operationalStatusNilReason .
           BIND(concat(\'nil:\',?operationalStatusNilReason) AS ?operationalStatus)
          }
        }
      }
    }
  }

      '
,row(Graph,AltimeterSourceStatus,OperationalStatus),[]).

% fixm_DangerousGoodsDimensions(Graph, DangerousGoodsDimensions, GrossWeight?, NetWeight?, Volume?)

fixm_DangerousGoodsDimensions(Graph, DangerousGoodsDimensions, GrossWeight, NetWeight, Volume) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?dangerousGoodsDimensions ?grossWeight ?netWeight ?volume
WHERE
  { GRAPH ?graph
    {
      ?dangerousGoodsDimensions rdf:type fixm:DangerousGoodsDimensions .
      OPTIONAL { ?dangerousGoodsDimensions fixm:grossWeight ?_grossWeight .
        {
          {
            ?_grossWeight rdf:value ?grossWeightValue .
            FILTER ( NOT EXISTS {?_grossWeight (aixm:uom | fixm:uom | plain:uom) ?grossWeightUoM})
            BIND(concat(\'val:\',?grossWeightValue) AS ?grossWeight)
          }
            UNION
          {
            ?_grossWeight
              rdf:value ?grossWeightValue ;
              (aixm:uom | fixm:uom | plain:uom) ?grossWeightUoM .
            BIND(concat(\'xval:\',STR(?grossWeightValue),\':\',?grossWeightUoM) AS ?grossWeight)
          }
            UNION
          {
           ?_grossWeight  aixm:nilReason ?grossWeightNilReason .
           BIND(concat(\'nil:\',?grossWeightNilReason) AS ?grossWeight)
          }
        }
      }
      OPTIONAL { ?dangerousGoodsDimensions fixm:netWeight ?_netWeight .
        {
          {
            ?_netWeight rdf:value ?netWeightValue .
            FILTER ( NOT EXISTS {?_netWeight (aixm:uom | fixm:uom | plain:uom) ?netWeightUoM})
            BIND(concat(\'val:\',?netWeightValue) AS ?netWeight)
          }
            UNION
          {
            ?_netWeight
              rdf:value ?netWeightValue ;
              (aixm:uom | fixm:uom | plain:uom) ?netWeightUoM .
            BIND(concat(\'xval:\',STR(?netWeightValue),\':\',?netWeightUoM) AS ?netWeight)
          }
            UNION
          {
           ?_netWeight  aixm:nilReason ?netWeightNilReason .
           BIND(concat(\'nil:\',?netWeightNilReason) AS ?netWeight)
          }
        }
      }
      OPTIONAL { ?dangerousGoodsDimensions fixm:volume ?_volume .
        {
          {
            ?_volume rdf:value ?volumeValue .
            FILTER ( NOT EXISTS {?_volume (aixm:uom | fixm:uom | plain:uom) ?volumeUoM})
            BIND(concat(\'val:\',?volumeValue) AS ?volume)
          }
            UNION
          {
            ?_volume
              rdf:value ?volumeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?volumeUoM .
            BIND(concat(\'xval:\',STR(?volumeValue),\':\',?volumeUoM) AS ?volume)
          }
            UNION
          {
           ?_volume  aixm:nilReason ?volumeNilReason .
           BIND(concat(\'nil:\',?volumeNilReason) AS ?volume)
          }
        }
      }
    }
  }

      '
,row(Graph,DangerousGoodsDimensions,GrossWeight,NetWeight,Volume),[]).

% fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules?)

fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?efplRoute ?efplFlightRules
WHERE
  { GRAPH ?graph
    {
      ?efplRoute rdf:type fixm:EfplRoute .
      OPTIONAL { ?efplRoute fixm:efplFlightRules ?_efplFlightRules .
        {
          {
            ?_efplFlightRules rdf:value ?efplFlightRulesValue .
            FILTER ( NOT EXISTS {?_efplFlightRules (aixm:uom | fixm:uom | plain:uom) ?efplFlightRulesUoM})
            BIND(concat(\'val:\',?efplFlightRulesValue) AS ?efplFlightRules)
          }
            UNION
          {
            ?_efplFlightRules
              rdf:value ?efplFlightRulesValue ;
              (aixm:uom | fixm:uom | plain:uom) ?efplFlightRulesUoM .
            BIND(concat(\'xval:\',STR(?efplFlightRulesValue),\':\',?efplFlightRulesUoM) AS ?efplFlightRules)
          }
            UNION
          {
           ?_efplFlightRules  aixm:nilReason ?efplFlightRulesNilReason .
           BIND(concat(\'nil:\',?efplFlightRulesNilReason) AS ?efplFlightRules)
          }
        }
      }
    }
  }

      '
,row(Graph,EfplRoute,EfplFlightRules),[]).

% fixm_CoordinationStatus(Graph, CoordinationStatus, AbrogationReason?, CoordinationStatus?, NonStandardCommunicationReason?, ReleaseConditions?)

fixm_CoordinationStatus(Graph, CoordinationStatus, AbrogationReason, CoordinationStatus, NonStandardCommunicationReason, ReleaseConditions) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?coordinationStatus ?abrogationReason ?coordinationStatus ?nonStandardCommunicationReason ?releaseConditions
WHERE
  { GRAPH ?graph
    {
      ?coordinationStatus rdf:type fixm:CoordinationStatus .
      OPTIONAL { ?coordinationStatus fixm:abrogationReason ?_abrogationReason .
        {
          {
            ?_abrogationReason rdf:value ?abrogationReasonValue .
            FILTER ( NOT EXISTS {?_abrogationReason (aixm:uom | fixm:uom | plain:uom) ?abrogationReasonUoM})
            BIND(concat(\'val:\',?abrogationReasonValue) AS ?abrogationReason)
          }
            UNION
          {
            ?_abrogationReason
              rdf:value ?abrogationReasonValue ;
              (aixm:uom | fixm:uom | plain:uom) ?abrogationReasonUoM .
            BIND(concat(\'xval:\',STR(?abrogationReasonValue),\':\',?abrogationReasonUoM) AS ?abrogationReason)
          }
            UNION
          {
           ?_abrogationReason  aixm:nilReason ?abrogationReasonNilReason .
           BIND(concat(\'nil:\',?abrogationReasonNilReason) AS ?abrogationReason)
          }
        }
      }
      OPTIONAL { ?coordinationStatus fixm:coordinationStatus ?_coordinationStatus .
        {
          {
            ?_coordinationStatus rdf:value ?coordinationStatusValue .
            FILTER ( NOT EXISTS {?_coordinationStatus (aixm:uom | fixm:uom | plain:uom) ?coordinationStatusUoM})
            BIND(concat(\'val:\',?coordinationStatusValue) AS ?coordinationStatus)
          }
            UNION
          {
            ?_coordinationStatus
              rdf:value ?coordinationStatusValue ;
              (aixm:uom | fixm:uom | plain:uom) ?coordinationStatusUoM .
            BIND(concat(\'xval:\',STR(?coordinationStatusValue),\':\',?coordinationStatusUoM) AS ?coordinationStatus)
          }
            UNION
          {
           ?_coordinationStatus  aixm:nilReason ?coordinationStatusNilReason .
           BIND(concat(\'nil:\',?coordinationStatusNilReason) AS ?coordinationStatus)
          }
        }
      }
      OPTIONAL { ?coordinationStatus fixm:nonStandardCommunicationReason ?_nonStandardCommunicationReason .
        {
          {
            ?_nonStandardCommunicationReason rdf:value ?nonStandardCommunicationReasonValue .
            FILTER ( NOT EXISTS {?_nonStandardCommunicationReason (aixm:uom | fixm:uom | plain:uom) ?nonStandardCommunicationReasonUoM})
            BIND(concat(\'val:\',?nonStandardCommunicationReasonValue) AS ?nonStandardCommunicationReason)
          }
            UNION
          {
            ?_nonStandardCommunicationReason
              rdf:value ?nonStandardCommunicationReasonValue ;
              (aixm:uom | fixm:uom | plain:uom) ?nonStandardCommunicationReasonUoM .
            BIND(concat(\'xval:\',STR(?nonStandardCommunicationReasonValue),\':\',?nonStandardCommunicationReasonUoM) AS ?nonStandardCommunicationReason)
          }
            UNION
          {
           ?_nonStandardCommunicationReason  aixm:nilReason ?nonStandardCommunicationReasonNilReason .
           BIND(concat(\'nil:\',?nonStandardCommunicationReasonNilReason) AS ?nonStandardCommunicationReason)
          }
        }
      }
      OPTIONAL { ?coordinationStatus fixm:releaseConditions ?_releaseConditions .
        {
          {
            ?_releaseConditions rdf:value ?releaseConditionsValue .
            FILTER ( NOT EXISTS {?_releaseConditions (aixm:uom | fixm:uom | plain:uom) ?releaseConditionsUoM})
            BIND(concat(\'val:\',?releaseConditionsValue) AS ?releaseConditions)
          }
            UNION
          {
            ?_releaseConditions
              rdf:value ?releaseConditionsValue ;
              (aixm:uom | fixm:uom | plain:uom) ?releaseConditionsUoM .
            BIND(concat(\'xval:\',STR(?releaseConditionsValue),\':\',?releaseConditionsUoM) AS ?releaseConditions)
          }
            UNION
          {
           ?_releaseConditions  aixm:nilReason ?releaseConditionsNilReason .
           BIND(concat(\'nil:\',?releaseConditionsNilReason) AS ?releaseConditions)
          }
        }
      }
    }
  }

      '
,row(Graph,CoordinationStatus,AbrogationReason,CoordinationStatus,NonStandardCommunicationReason,ReleaseConditions),[]).

% fixm_BoundaryCrossing(Graph, BoundaryCrossing, Altitude?, CrossingPoint?, CrossingSpeed?, CrossingTime?, Offtrack?, AltitudeInTransition?)

fixm_BoundaryCrossing(Graph, BoundaryCrossing, Altitude, CrossingPoint, CrossingSpeed, CrossingTime, Offtrack, AltitudeInTransition) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?boundaryCrossing ?altitude ?crossingPoint ?crossingSpeed ?crossingTime ?offtrack ?altitudeInTransition
WHERE
  { GRAPH ?graph
    {
      ?boundaryCrossing rdf:type fixm:BoundaryCrossing .
      OPTIONAL { ?boundaryCrossing fixm:altitude ?_altitude .
        {
          {
            ?_altitude rdf:value ?altitudeValue .
            FILTER ( NOT EXISTS {?_altitude (aixm:uom | fixm:uom | plain:uom) ?altitudeUoM})
            BIND(concat(\'val:\',?altitudeValue) AS ?altitude)
          }
            UNION
          {
            ?_altitude
              rdf:value ?altitudeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?altitudeUoM .
            BIND(concat(\'xval:\',STR(?altitudeValue),\':\',?altitudeUoM) AS ?altitude)
          }
            UNION
          {
           ?_altitude  aixm:nilReason ?altitudeNilReason .
           BIND(concat(\'nil:\',?altitudeNilReason) AS ?altitude)
          }
        }
      }
      OPTIONAL {?boundaryCrossing fixm:crossingPoint ?crossingPoint .}
      OPTIONAL { ?boundaryCrossing fixm:crossingSpeed ?_crossingSpeed .
        {
          {
            ?_crossingSpeed rdf:value ?crossingSpeedValue .
            FILTER ( NOT EXISTS {?_crossingSpeed (aixm:uom | fixm:uom | plain:uom) ?crossingSpeedUoM})
            BIND(concat(\'val:\',?crossingSpeedValue) AS ?crossingSpeed)
          }
            UNION
          {
            ?_crossingSpeed
              rdf:value ?crossingSpeedValue ;
              (aixm:uom | fixm:uom | plain:uom) ?crossingSpeedUoM .
            BIND(concat(\'xval:\',STR(?crossingSpeedValue),\':\',?crossingSpeedUoM) AS ?crossingSpeed)
          }
            UNION
          {
           ?_crossingSpeed  aixm:nilReason ?crossingSpeedNilReason .
           BIND(concat(\'nil:\',?crossingSpeedNilReason) AS ?crossingSpeed)
          }
        }
      }
      OPTIONAL { ?boundaryCrossing fixm:crossingTime ?_crossingTime .
        {
          {
            ?_crossingTime rdf:value ?crossingTimeValue .
            FILTER ( NOT EXISTS {?_crossingTime (aixm:uom | fixm:uom | plain:uom) ?crossingTimeUoM})
            BIND(concat(\'val:\',?crossingTimeValue) AS ?crossingTime)
          }
            UNION
          {
            ?_crossingTime
              rdf:value ?crossingTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?crossingTimeUoM .
            BIND(concat(\'xval:\',STR(?crossingTimeValue),\':\',?crossingTimeUoM) AS ?crossingTime)
          }
            UNION
          {
           ?_crossingTime  aixm:nilReason ?crossingTimeNilReason .
           BIND(concat(\'nil:\',?crossingTimeNilReason) AS ?crossingTime)
          }
        }
      }
      OPTIONAL {?boundaryCrossing fixm:offtrack ?offtrack .}
      OPTIONAL { ?boundaryCrossing fixm:altitudeInTransition ?_altitudeInTransition .
        {
          {
            ?_altitudeInTransition rdf:value ?altitudeInTransitionValue .
            FILTER ( NOT EXISTS {?_altitudeInTransition (aixm:uom | fixm:uom | plain:uom) ?altitudeInTransitionUoM})
            BIND(concat(\'val:\',?altitudeInTransitionValue) AS ?altitudeInTransition)
          }
            UNION
          {
            ?_altitudeInTransition
              rdf:value ?altitudeInTransitionValue ;
              (aixm:uom | fixm:uom | plain:uom) ?altitudeInTransitionUoM .
            BIND(concat(\'xval:\',STR(?altitudeInTransitionValue),\':\',?altitudeInTransitionUoM) AS ?altitudeInTransition)
          }
            UNION
          {
           ?_altitudeInTransition  aixm:nilReason ?altitudeInTransitionNilReason .
           BIND(concat(\'nil:\',?altitudeInTransitionNilReason) AS ?altitudeInTransition)
          }
        }
      }
    }
  }

      '
,row(Graph,BoundaryCrossing,Altitude,CrossingPoint,CrossingSpeed,CrossingTime,Offtrack,AltitudeInTransition),[]).

% fixm_IcaoAerodromeReference(Graph, IcaoAerodromeReference, Code?)

fixm_IcaoAerodromeReference(Graph, IcaoAerodromeReference, Code) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?icaoAerodromeReference ?code
WHERE
  { GRAPH ?graph
    {
      ?icaoAerodromeReference rdf:type fixm:IcaoAerodromeReference .
      OPTIONAL { ?icaoAerodromeReference fixm:code ?_code .
        {
          {
            ?_code rdf:value ?codeValue .
            FILTER ( NOT EXISTS {?_code (aixm:uom | fixm:uom | plain:uom) ?codeUoM})
            BIND(concat(\'val:\',?codeValue) AS ?code)
          }
            UNION
          {
            ?_code
              rdf:value ?codeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?codeUoM .
            BIND(concat(\'xval:\',STR(?codeValue),\':\',?codeUoM) AS ?code)
          }
            UNION
          {
           ?_code  aixm:nilReason ?codeNilReason .
           BIND(concat(\'nil:\',?codeNilReason) AS ?code)
          }
        }
      }
    }
  }

      '
,row(Graph,IcaoAerodromeReference,Code),[]).

% fixm_RadioCommunicationFailure(Graph, RadioCommunicationFailure, RadioFailureRemarks?, RemainingComCapability?, Contact?)

fixm_RadioCommunicationFailure(Graph, RadioCommunicationFailure, RadioFailureRemarks, RemainingComCapability, Contact) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?radioCommunicationFailure ?radioFailureRemarks ?remainingComCapability ?contact
WHERE
  { GRAPH ?graph
    {
      ?radioCommunicationFailure rdf:type fixm:RadioCommunicationFailure .
      OPTIONAL { ?radioCommunicationFailure fixm:radioFailureRemarks ?_radioFailureRemarks .
        {
          {
            ?_radioFailureRemarks rdf:value ?radioFailureRemarksValue .
            FILTER ( NOT EXISTS {?_radioFailureRemarks (aixm:uom | fixm:uom | plain:uom) ?radioFailureRemarksUoM})
            BIND(concat(\'val:\',?radioFailureRemarksValue) AS ?radioFailureRemarks)
          }
            UNION
          {
            ?_radioFailureRemarks
              rdf:value ?radioFailureRemarksValue ;
              (aixm:uom | fixm:uom | plain:uom) ?radioFailureRemarksUoM .
            BIND(concat(\'xval:\',STR(?radioFailureRemarksValue),\':\',?radioFailureRemarksUoM) AS ?radioFailureRemarks)
          }
            UNION
          {
           ?_radioFailureRemarks  aixm:nilReason ?radioFailureRemarksNilReason .
           BIND(concat(\'nil:\',?radioFailureRemarksNilReason) AS ?radioFailureRemarks)
          }
        }
      }
      OPTIONAL { ?radioCommunicationFailure fixm:remainingComCapability ?_remainingComCapability .
        {
          {
            ?_remainingComCapability rdf:value ?remainingComCapabilityValue .
            FILTER ( NOT EXISTS {?_remainingComCapability (aixm:uom | fixm:uom | plain:uom) ?remainingComCapabilityUoM})
            BIND(concat(\'val:\',?remainingComCapabilityValue) AS ?remainingComCapability)
          }
            UNION
          {
            ?_remainingComCapability
              rdf:value ?remainingComCapabilityValue ;
              (aixm:uom | fixm:uom | plain:uom) ?remainingComCapabilityUoM .
            BIND(concat(\'xval:\',STR(?remainingComCapabilityValue),\':\',?remainingComCapabilityUoM) AS ?remainingComCapability)
          }
            UNION
          {
           ?_remainingComCapability  aixm:nilReason ?remainingComCapabilityNilReason .
           BIND(concat(\'nil:\',?remainingComCapabilityNilReason) AS ?remainingComCapability)
          }
        }
      }
      OPTIONAL {?radioCommunicationFailure fixm:contact ?contact .}
    }
  }

      '
,row(Graph,RadioCommunicationFailure,RadioFailureRemarks,RemainingComCapability,Contact),[]).

% aixm_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus?, Warning?, Usage*)

aixm_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus, Warning, UsageList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?airportHeliportAvailability ?operationalStatus ?warning (GROUP_CONCAT(DISTINCT ?usage;SEPARATOR=",") AS ?usageConcat)
WHERE
  { GRAPH ?graph
    {
      ?airportHeliportAvailability rdf:type aixm:AirportHeliportAvailability .
      OPTIONAL { ?airportHeliportAvailability aixm:operationalStatus ?_operationalStatus .
        {
          {
            ?_operationalStatus rdf:value ?operationalStatusValue .
            FILTER ( NOT EXISTS {?_operationalStatus (aixm:uom | fixm:uom | plain:uom) ?operationalStatusUoM})
            BIND(concat(\'val:\',?operationalStatusValue) AS ?operationalStatus)
          }
            UNION
          {
            ?_operationalStatus
              rdf:value ?operationalStatusValue ;
              (aixm:uom | fixm:uom | plain:uom) ?operationalStatusUoM .
            BIND(concat(\'xval:\',STR(?operationalStatusValue),\':\',?operationalStatusUoM) AS ?operationalStatus)
          }
            UNION
          {
           ?_operationalStatus  aixm:nilReason ?operationalStatusNilReason .
           BIND(concat(\'nil:\',?operationalStatusNilReason) AS ?operationalStatus)
          }
        }
      }
      OPTIONAL { ?airportHeliportAvailability aixm:warning ?_warning .
        {
          {
            ?_warning rdf:value ?warningValue .
            FILTER ( NOT EXISTS {?_warning (aixm:uom | fixm:uom | plain:uom) ?warningUoM})
            BIND(concat(\'val:\',?warningValue) AS ?warning)
          }
            UNION
          {
            ?_warning
              rdf:value ?warningValue ;
              (aixm:uom | fixm:uom | plain:uom) ?warningUoM .
            BIND(concat(\'xval:\',STR(?warningValue),\':\',?warningUoM) AS ?warning)
          }
            UNION
          {
           ?_warning  aixm:nilReason ?warningNilReason .
           BIND(concat(\'nil:\',?warningNilReason) AS ?warning)
          }
        }
      }
      OPTIONAL {?airportHeliportAvailability aixm:usage ?usage .}
    }
  }
GROUP BY ?graph ?airportHeliportAvailability ?operationalStatus ?warning

      '
,row(Graph,AirportHeliportAvailability,OperationalStatus,Warning,UsageConcat),[]), convert(UsageConcat,UsageList).

% fixm_FlightArrival(Graph, FlightArrival, ApproachFix?, ApproachTime?, ArrivalAerodrome?, ArrivalAerodromeAlternate*, ArrivalAerodromeOriginal?, ArrivalFix?, ArrivalFixTime?, ArrivalFleetPrioritization?, ArrivalSequenceNumber?, EarliestInBlockTime?, FiledRevisedDestinationAerodrome?, FiledRevisedDestinationStar?, RunwayPositionAndTime?, StandardInstrumentArrival?, StandPositionAndTime?, LandingLimits?)

fixm_FlightArrival(Graph, FlightArrival, ApproachFix, ApproachTime, ArrivalAerodrome, ArrivalAerodromeAlternateList, ArrivalAerodromeOriginal, ArrivalFix, ArrivalFixTime, ArrivalFleetPrioritization, ArrivalSequenceNumber, EarliestInBlockTime, FiledRevisedDestinationAerodrome, FiledRevisedDestinationStar, RunwayPositionAndTime, StandardInstrumentArrival, StandPositionAndTime, LandingLimits) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?flightArrival ?approachFix ?approachTime ?arrivalAerodrome (GROUP_CONCAT(DISTINCT ?arrivalAerodromeAlternate;SEPARATOR=",") AS ?arrivalAerodromeAlternateConcat) ?arrivalAerodromeOriginal ?arrivalFix ?arrivalFixTime ?arrivalFleetPrioritization ?arrivalSequenceNumber ?earliestInBlockTime ?filedRevisedDestinationAerodrome ?filedRevisedDestinationStar ?runwayPositionAndTime ?standardInstrumentArrival ?standPositionAndTime ?landingLimits
WHERE
  { GRAPH ?graph
    {
      ?flightArrival rdf:type fixm:FlightArrival .
      OPTIONAL {?flightArrival fixm:approachFix ?approachFix .}
      OPTIONAL {?flightArrival fixm:approachTime ?approachTime .}
      OPTIONAL {?flightArrival fixm:arrivalAerodrome ?arrivalAerodrome .}
      OPTIONAL {?flightArrival fixm:arrivalAerodromeAlternate ?arrivalAerodromeAlternate .}
      OPTIONAL {?flightArrival fixm:arrivalAerodromeOriginal ?arrivalAerodromeOriginal .}
      OPTIONAL {?flightArrival fixm:arrivalFix ?arrivalFix .}
      OPTIONAL {?flightArrival fixm:arrivalFixTime ?arrivalFixTime .}
      OPTIONAL { ?flightArrival fixm:arrivalFleetPrioritization ?_arrivalFleetPrioritization .
        {
          {
            ?_arrivalFleetPrioritization rdf:value ?arrivalFleetPrioritizationValue .
            FILTER ( NOT EXISTS {?_arrivalFleetPrioritization (aixm:uom | fixm:uom | plain:uom) ?arrivalFleetPrioritizationUoM})
            BIND(concat(\'val:\',?arrivalFleetPrioritizationValue) AS ?arrivalFleetPrioritization)
          }
            UNION
          {
            ?_arrivalFleetPrioritization
              rdf:value ?arrivalFleetPrioritizationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?arrivalFleetPrioritizationUoM .
            BIND(concat(\'xval:\',STR(?arrivalFleetPrioritizationValue),\':\',?arrivalFleetPrioritizationUoM) AS ?arrivalFleetPrioritization)
          }
            UNION
          {
           ?_arrivalFleetPrioritization  aixm:nilReason ?arrivalFleetPrioritizationNilReason .
           BIND(concat(\'nil:\',?arrivalFleetPrioritizationNilReason) AS ?arrivalFleetPrioritization)
          }
        }
      }
      OPTIONAL { ?flightArrival fixm:arrivalSequenceNumber ?_arrivalSequenceNumber .
        {
          {
            ?_arrivalSequenceNumber rdf:value ?arrivalSequenceNumberValue .
            FILTER ( NOT EXISTS {?_arrivalSequenceNumber (aixm:uom | fixm:uom | plain:uom) ?arrivalSequenceNumberUoM})
            BIND(concat(\'val:\',?arrivalSequenceNumberValue) AS ?arrivalSequenceNumber)
          }
            UNION
          {
            ?_arrivalSequenceNumber
              rdf:value ?arrivalSequenceNumberValue ;
              (aixm:uom | fixm:uom | plain:uom) ?arrivalSequenceNumberUoM .
            BIND(concat(\'xval:\',STR(?arrivalSequenceNumberValue),\':\',?arrivalSequenceNumberUoM) AS ?arrivalSequenceNumber)
          }
            UNION
          {
           ?_arrivalSequenceNumber  aixm:nilReason ?arrivalSequenceNumberNilReason .
           BIND(concat(\'nil:\',?arrivalSequenceNumberNilReason) AS ?arrivalSequenceNumber)
          }
        }
      }
      OPTIONAL { ?flightArrival fixm:earliestInBlockTime ?_earliestInBlockTime .
        {
          {
            ?_earliestInBlockTime rdf:value ?earliestInBlockTimeValue .
            FILTER ( NOT EXISTS {?_earliestInBlockTime (aixm:uom | fixm:uom | plain:uom) ?earliestInBlockTimeUoM})
            BIND(concat(\'val:\',?earliestInBlockTimeValue) AS ?earliestInBlockTime)
          }
            UNION
          {
            ?_earliestInBlockTime
              rdf:value ?earliestInBlockTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?earliestInBlockTimeUoM .
            BIND(concat(\'xval:\',STR(?earliestInBlockTimeValue),\':\',?earliestInBlockTimeUoM) AS ?earliestInBlockTime)
          }
            UNION
          {
           ?_earliestInBlockTime  aixm:nilReason ?earliestInBlockTimeNilReason .
           BIND(concat(\'nil:\',?earliestInBlockTimeNilReason) AS ?earliestInBlockTime)
          }
        }
      }
      OPTIONAL {?flightArrival fixm:filedRevisedDestinationAerodrome ?filedRevisedDestinationAerodrome .}
      OPTIONAL { ?flightArrival fixm:filedRevisedDestinationStar ?_filedRevisedDestinationStar .
        {
          {
            ?_filedRevisedDestinationStar rdf:value ?filedRevisedDestinationStarValue .
            FILTER ( NOT EXISTS {?_filedRevisedDestinationStar (aixm:uom | fixm:uom | plain:uom) ?filedRevisedDestinationStarUoM})
            BIND(concat(\'val:\',?filedRevisedDestinationStarValue) AS ?filedRevisedDestinationStar)
          }
            UNION
          {
            ?_filedRevisedDestinationStar
              rdf:value ?filedRevisedDestinationStarValue ;
              (aixm:uom | fixm:uom | plain:uom) ?filedRevisedDestinationStarUoM .
            BIND(concat(\'xval:\',STR(?filedRevisedDestinationStarValue),\':\',?filedRevisedDestinationStarUoM) AS ?filedRevisedDestinationStar)
          }
            UNION
          {
           ?_filedRevisedDestinationStar  aixm:nilReason ?filedRevisedDestinationStarNilReason .
           BIND(concat(\'nil:\',?filedRevisedDestinationStarNilReason) AS ?filedRevisedDestinationStar)
          }
        }
      }
      OPTIONAL {?flightArrival fixm:runwayPositionAndTime ?runwayPositionAndTime .}
      OPTIONAL { ?flightArrival fixm:standardInstrumentArrival ?_standardInstrumentArrival .
        {
          {
            ?_standardInstrumentArrival rdf:value ?standardInstrumentArrivalValue .
            FILTER ( NOT EXISTS {?_standardInstrumentArrival (aixm:uom | fixm:uom | plain:uom) ?standardInstrumentArrivalUoM})
            BIND(concat(\'val:\',?standardInstrumentArrivalValue) AS ?standardInstrumentArrival)
          }
            UNION
          {
            ?_standardInstrumentArrival
              rdf:value ?standardInstrumentArrivalValue ;
              (aixm:uom | fixm:uom | plain:uom) ?standardInstrumentArrivalUoM .
            BIND(concat(\'xval:\',STR(?standardInstrumentArrivalValue),\':\',?standardInstrumentArrivalUoM) AS ?standardInstrumentArrival)
          }
            UNION
          {
           ?_standardInstrumentArrival  aixm:nilReason ?standardInstrumentArrivalNilReason .
           BIND(concat(\'nil:\',?standardInstrumentArrivalNilReason) AS ?standardInstrumentArrival)
          }
        }
      }
      OPTIONAL {?flightArrival fixm:standPositionAndTime ?standPositionAndTime .}
      OPTIONAL { ?flightArrival fixm:landingLimits ?_landingLimits .
        {
          {
            ?_landingLimits rdf:value ?landingLimitsValue .
            FILTER ( NOT EXISTS {?_landingLimits (aixm:uom | fixm:uom | plain:uom) ?landingLimitsUoM})
            BIND(concat(\'val:\',?landingLimitsValue) AS ?landingLimits)
          }
            UNION
          {
            ?_landingLimits
              rdf:value ?landingLimitsValue ;
              (aixm:uom | fixm:uom | plain:uom) ?landingLimitsUoM .
            BIND(concat(\'xval:\',STR(?landingLimitsValue),\':\',?landingLimitsUoM) AS ?landingLimits)
          }
            UNION
          {
           ?_landingLimits  aixm:nilReason ?landingLimitsNilReason .
           BIND(concat(\'nil:\',?landingLimitsNilReason) AS ?landingLimits)
          }
        }
      }
    }
  }
GROUP BY ?graph ?flightArrival ?approachFix ?approachTime ?arrivalAerodrome ?arrivalAerodromeOriginal ?arrivalFix ?arrivalFixTime ?arrivalFleetPrioritization ?arrivalSequenceNumber ?earliestInBlockTime ?filedRevisedDestinationAerodrome ?filedRevisedDestinationStar ?runwayPositionAndTime ?standardInstrumentArrival ?standPositionAndTime ?landingLimits

      '
,row(Graph,FlightArrival,ApproachFix,ApproachTime,ArrivalAerodrome,ArrivalAerodromeAlternateConcat,ArrivalAerodromeOriginal,ArrivalFix,ArrivalFixTime,ArrivalFleetPrioritization,ArrivalSequenceNumber,EarliestInBlockTime,FiledRevisedDestinationAerodrome,FiledRevisedDestinationStar,RunwayPositionAndTime,StandardInstrumentArrival,StandPositionAndTime,LandingLimits),[]), convert(ArrivalAerodromeAlternateConcat,ArrivalAerodromeAlternateList).

% fixm_RadioactiveMaterial(Graph, RadioactiveMaterial, CriticalitySafetyIndex?, TransportIndex?, FissileExceptedIndicator?, Category?, Radionuclide?)

fixm_RadioactiveMaterial(Graph, RadioactiveMaterial, CriticalitySafetyIndex, TransportIndex, FissileExceptedIndicator, Category, Radionuclide) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?radioactiveMaterial ?criticalitySafetyIndex ?transportIndex ?fissileExceptedIndicator ?category ?radionuclide
WHERE
  { GRAPH ?graph
    {
      ?radioactiveMaterial rdf:type fixm:RadioactiveMaterial .
      OPTIONAL { ?radioactiveMaterial fixm:criticalitySafetyIndex ?_criticalitySafetyIndex .
        {
          {
            ?_criticalitySafetyIndex rdf:value ?criticalitySafetyIndexValue .
            FILTER ( NOT EXISTS {?_criticalitySafetyIndex (aixm:uom | fixm:uom | plain:uom) ?criticalitySafetyIndexUoM})
            BIND(concat(\'val:\',?criticalitySafetyIndexValue) AS ?criticalitySafetyIndex)
          }
            UNION
          {
            ?_criticalitySafetyIndex
              rdf:value ?criticalitySafetyIndexValue ;
              (aixm:uom | fixm:uom | plain:uom) ?criticalitySafetyIndexUoM .
            BIND(concat(\'xval:\',STR(?criticalitySafetyIndexValue),\':\',?criticalitySafetyIndexUoM) AS ?criticalitySafetyIndex)
          }
            UNION
          {
           ?_criticalitySafetyIndex  aixm:nilReason ?criticalitySafetyIndexNilReason .
           BIND(concat(\'nil:\',?criticalitySafetyIndexNilReason) AS ?criticalitySafetyIndex)
          }
        }
      }
      OPTIONAL { ?radioactiveMaterial fixm:transportIndex ?_transportIndex .
        {
          {
            ?_transportIndex rdf:value ?transportIndexValue .
            FILTER ( NOT EXISTS {?_transportIndex (aixm:uom | fixm:uom | plain:uom) ?transportIndexUoM})
            BIND(concat(\'val:\',?transportIndexValue) AS ?transportIndex)
          }
            UNION
          {
            ?_transportIndex
              rdf:value ?transportIndexValue ;
              (aixm:uom | fixm:uom | plain:uom) ?transportIndexUoM .
            BIND(concat(\'xval:\',STR(?transportIndexValue),\':\',?transportIndexUoM) AS ?transportIndex)
          }
            UNION
          {
           ?_transportIndex  aixm:nilReason ?transportIndexNilReason .
           BIND(concat(\'nil:\',?transportIndexNilReason) AS ?transportIndex)
          }
        }
      }
      OPTIONAL { ?radioactiveMaterial fixm:fissileExceptedIndicator ?_fissileExceptedIndicator .
        {
          {
            ?_fissileExceptedIndicator rdf:value ?fissileExceptedIndicatorValue .
            FILTER ( NOT EXISTS {?_fissileExceptedIndicator (aixm:uom | fixm:uom | plain:uom) ?fissileExceptedIndicatorUoM})
            BIND(concat(\'val:\',?fissileExceptedIndicatorValue) AS ?fissileExceptedIndicator)
          }
            UNION
          {
            ?_fissileExceptedIndicator
              rdf:value ?fissileExceptedIndicatorValue ;
              (aixm:uom | fixm:uom | plain:uom) ?fissileExceptedIndicatorUoM .
            BIND(concat(\'xval:\',STR(?fissileExceptedIndicatorValue),\':\',?fissileExceptedIndicatorUoM) AS ?fissileExceptedIndicator)
          }
            UNION
          {
           ?_fissileExceptedIndicator  aixm:nilReason ?fissileExceptedIndicatorNilReason .
           BIND(concat(\'nil:\',?fissileExceptedIndicatorNilReason) AS ?fissileExceptedIndicator)
          }
        }
      }
      OPTIONAL { ?radioactiveMaterial fixm:category ?_category .
        {
          {
            ?_category rdf:value ?categoryValue .
            FILTER ( NOT EXISTS {?_category (aixm:uom | fixm:uom | plain:uom) ?categoryUoM})
            BIND(concat(\'val:\',?categoryValue) AS ?category)
          }
            UNION
          {
            ?_category
              rdf:value ?categoryValue ;
              (aixm:uom | fixm:uom | plain:uom) ?categoryUoM .
            BIND(concat(\'xval:\',STR(?categoryValue),\':\',?categoryUoM) AS ?category)
          }
            UNION
          {
           ?_category  aixm:nilReason ?categoryNilReason .
           BIND(concat(\'nil:\',?categoryNilReason) AS ?category)
          }
        }
      }
      OPTIONAL {?radioactiveMaterial fixm:radionuclide ?radionuclide .}
    }
  }

      '
,row(Graph,RadioactiveMaterial,CriticalitySafetyIndex,TransportIndex,FissileExceptedIndicator,Category,Radionuclide),[]).

% fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled?, Initial?)

fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled, Initial) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?extendedMultiTime ?controlled ?initial
WHERE
  { GRAPH ?graph
    {
      ?extendedMultiTime rdf:type fixm:ExtendedMultiTime .
      OPTIONAL {?extendedMultiTime fixm:controlled ?controlled .}
      OPTIONAL {?extendedMultiTime fixm:initial ?initial .}
    }
  }

      '
,row(Graph,ExtendedMultiTime,Controlled,Initial),[]).

% fixm_ControlElement(Graph, ControlElement)

fixm_ControlElement(Graph, ControlElement) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?controlElement
WHERE
  { GRAPH ?graph
    {
      ?controlElement rdf:type fixm:ControlElement .
    }
  }

      '
,row(Graph,ControlElement),[]).

% fixm_AerodromesOfDestination(Graph, AerodromesOfDestination, AerodromeOfDestination?, Alternate1?, Alternate2?, FiledRevisedDestinationAerodrome?)

fixm_AerodromesOfDestination(Graph, AerodromesOfDestination, AerodromeOfDestination, Alternate1, Alternate2, FiledRevisedDestinationAerodrome) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?aerodromesOfDestination ?aerodromeOfDestination ?alternate1 ?alternate2 ?filedRevisedDestinationAerodrome
WHERE
  { GRAPH ?graph
    {
      ?aerodromesOfDestination rdf:type fixm:AerodromesOfDestination .
      OPTIONAL {?aerodromesOfDestination fixm:aerodromeOfDestination ?aerodromeOfDestination .}
      OPTIONAL {?aerodromesOfDestination fixm:alternate1 ?alternate1 .}
      OPTIONAL {?aerodromesOfDestination fixm:alternate2 ?alternate2 .}
      OPTIONAL {?aerodromesOfDestination fixm:filedRevisedDestinationAerodrome ?filedRevisedDestinationAerodrome .}
    }
  }

      '
,row(Graph,AerodromesOfDestination,AerodromeOfDestination,Alternate1,Alternate2,FiledRevisedDestinationAerodrome),[]).

% fixm_AllPackedInOne(Graph, AllPackedInOne, NumberOfPackages?, QValue?)

fixm_AllPackedInOne(Graph, AllPackedInOne, NumberOfPackages, QValue) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?allPackedInOne ?numberOfPackages ?qValue
WHERE
  { GRAPH ?graph
    {
      ?allPackedInOne rdf:type fixm:AllPackedInOne .
      OPTIONAL { ?allPackedInOne fixm:numberOfPackages ?_numberOfPackages .
        {
          {
            ?_numberOfPackages rdf:value ?numberOfPackagesValue .
            FILTER ( NOT EXISTS {?_numberOfPackages (aixm:uom | fixm:uom | plain:uom) ?numberOfPackagesUoM})
            BIND(concat(\'val:\',?numberOfPackagesValue) AS ?numberOfPackages)
          }
            UNION
          {
            ?_numberOfPackages
              rdf:value ?numberOfPackagesValue ;
              (aixm:uom | fixm:uom | plain:uom) ?numberOfPackagesUoM .
            BIND(concat(\'xval:\',STR(?numberOfPackagesValue),\':\',?numberOfPackagesUoM) AS ?numberOfPackages)
          }
            UNION
          {
           ?_numberOfPackages  aixm:nilReason ?numberOfPackagesNilReason .
           BIND(concat(\'nil:\',?numberOfPackagesNilReason) AS ?numberOfPackages)
          }
        }
      }
      OPTIONAL { ?allPackedInOne fixm:qValue ?_qValue .
        {
          {
            ?_qValue rdf:value ?qValueValue .
            FILTER ( NOT EXISTS {?_qValue (aixm:uom | fixm:uom | plain:uom) ?qValueUoM})
            BIND(concat(\'val:\',?qValueValue) AS ?qValue)
          }
            UNION
          {
            ?_qValue
              rdf:value ?qValueValue ;
              (aixm:uom | fixm:uom | plain:uom) ?qValueUoM .
            BIND(concat(\'xval:\',STR(?qValueValue),\':\',?qValueUoM) AS ?qValue)
          }
            UNION
          {
           ?_qValue  aixm:nilReason ?qValueNilReason .
           BIND(concat(\'nil:\',?qValueNilReason) AS ?qValue)
          }
        }
      }
    }
  }

      '
,row(Graph,AllPackedInOne,NumberOfPackages,QValue),[]).

% aixm_AltimeterSource(Graph, AltimeterSource, TimeSlice*)

aixm_AltimeterSource(Graph, AltimeterSource, TimeSliceList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?altimeterSource (GROUP_CONCAT(DISTINCT ?timeSlice;SEPARATOR=",") AS ?timeSliceConcat)
WHERE
  { GRAPH ?graph
    {
      ?altimeterSource rdf:type aixm:AltimeterSource .
      OPTIONAL {?altimeterSource aixm:timeSlice ?timeSlice .}
    }
  }
GROUP BY ?graph ?altimeterSource

      '
,row(Graph,AltimeterSource,TimeSliceConcat),[]), convert(TimeSliceConcat,TimeSliceList).

% fixm_SurvivalCapabilities(Graph, SurvivalCapabilities, SurvivalEquipmentRemarks?, DinghyInformation?, EmergencyRadioCode*, LifeJacketCode*, SurvivalEquipmentCode*)

fixm_SurvivalCapabilities(Graph, SurvivalCapabilities, SurvivalEquipmentRemarks, DinghyInformation, EmergencyRadioCodeList, LifeJacketCodeList, SurvivalEquipmentCodeList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?survivalCapabilities ?survivalEquipmentRemarks ?dinghyInformation (GROUP_CONCAT(DISTINCT ?emergencyRadioCode;SEPARATOR=",") AS ?emergencyRadioCodeConcat) (GROUP_CONCAT(DISTINCT ?lifeJacketCode;SEPARATOR=",") AS ?lifeJacketCodeConcat) (GROUP_CONCAT(DISTINCT ?survivalEquipmentCode;SEPARATOR=",") AS ?survivalEquipmentCodeConcat)
WHERE
  { GRAPH ?graph
    {
      ?survivalCapabilities rdf:type fixm:SurvivalCapabilities .
      OPTIONAL { ?survivalCapabilities fixm:survivalEquipmentRemarks ?_survivalEquipmentRemarks .
        {
          {
            ?_survivalEquipmentRemarks rdf:value ?survivalEquipmentRemarksValue .
            FILTER ( NOT EXISTS {?_survivalEquipmentRemarks (aixm:uom | fixm:uom | plain:uom) ?survivalEquipmentRemarksUoM})
            BIND(concat(\'val:\',?survivalEquipmentRemarksValue) AS ?survivalEquipmentRemarks)
          }
            UNION
          {
            ?_survivalEquipmentRemarks
              rdf:value ?survivalEquipmentRemarksValue ;
              (aixm:uom | fixm:uom | plain:uom) ?survivalEquipmentRemarksUoM .
            BIND(concat(\'xval:\',STR(?survivalEquipmentRemarksValue),\':\',?survivalEquipmentRemarksUoM) AS ?survivalEquipmentRemarks)
          }
            UNION
          {
           ?_survivalEquipmentRemarks  aixm:nilReason ?survivalEquipmentRemarksNilReason .
           BIND(concat(\'nil:\',?survivalEquipmentRemarksNilReason) AS ?survivalEquipmentRemarks)
          }
        }
      }
      OPTIONAL {?survivalCapabilities fixm:dinghyInformation ?dinghyInformation .}
      OPTIONAL { ?survivalCapabilities fixm:emergencyRadioCode ?_emergencyRadioCode .
        {
          {
            ?_emergencyRadioCode rdf:value ?emergencyRadioCodeValue .
            FILTER ( NOT EXISTS {?_emergencyRadioCode (aixm:uom | fixm:uom | plain:uom) ?emergencyRadioCodeUoM})
            BIND(concat(\'val:\',?emergencyRadioCodeValue) AS ?emergencyRadioCode)
          }
            UNION
          {
            ?_emergencyRadioCode
              rdf:value ?emergencyRadioCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?emergencyRadioCodeUoM .
            BIND(concat(\'xval:\',STR(?emergencyRadioCodeValue),\':\',?emergencyRadioCodeUoM) AS ?emergencyRadioCode)
          }
            UNION
          {
           ?_emergencyRadioCode  aixm:nilReason ?emergencyRadioCodeNilReason .
           BIND(concat(\'nil:\',?emergencyRadioCodeNilReason) AS ?emergencyRadioCode)
          }
        }
      }
      OPTIONAL { ?survivalCapabilities fixm:lifeJacketCode ?_lifeJacketCode .
        {
          {
            ?_lifeJacketCode rdf:value ?lifeJacketCodeValue .
            FILTER ( NOT EXISTS {?_lifeJacketCode (aixm:uom | fixm:uom | plain:uom) ?lifeJacketCodeUoM})
            BIND(concat(\'val:\',?lifeJacketCodeValue) AS ?lifeJacketCode)
          }
            UNION
          {
            ?_lifeJacketCode
              rdf:value ?lifeJacketCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?lifeJacketCodeUoM .
            BIND(concat(\'xval:\',STR(?lifeJacketCodeValue),\':\',?lifeJacketCodeUoM) AS ?lifeJacketCode)
          }
            UNION
          {
           ?_lifeJacketCode  aixm:nilReason ?lifeJacketCodeNilReason .
           BIND(concat(\'nil:\',?lifeJacketCodeNilReason) AS ?lifeJacketCode)
          }
        }
      }
      OPTIONAL { ?survivalCapabilities fixm:survivalEquipmentCode ?_survivalEquipmentCode .
        {
          {
            ?_survivalEquipmentCode rdf:value ?survivalEquipmentCodeValue .
            FILTER ( NOT EXISTS {?_survivalEquipmentCode (aixm:uom | fixm:uom | plain:uom) ?survivalEquipmentCodeUoM})
            BIND(concat(\'val:\',?survivalEquipmentCodeValue) AS ?survivalEquipmentCode)
          }
            UNION
          {
            ?_survivalEquipmentCode
              rdf:value ?survivalEquipmentCodeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?survivalEquipmentCodeUoM .
            BIND(concat(\'xval:\',STR(?survivalEquipmentCodeValue),\':\',?survivalEquipmentCodeUoM) AS ?survivalEquipmentCode)
          }
            UNION
          {
           ?_survivalEquipmentCode  aixm:nilReason ?survivalEquipmentCodeNilReason .
           BIND(concat(\'nil:\',?survivalEquipmentCodeNilReason) AS ?survivalEquipmentCode)
          }
        }
      }
    }
  }
GROUP BY ?graph ?survivalCapabilities ?survivalEquipmentRemarks ?dinghyInformation

      '
,row(Graph,SurvivalCapabilities,SurvivalEquipmentRemarks,DinghyInformation,EmergencyRadioCodeConcat,LifeJacketCodeConcat,SurvivalEquipmentCodeConcat),[]), convert(EmergencyRadioCodeConcat,EmergencyRadioCodeList), convert(LifeJacketCodeConcat,LifeJacketCodeList), convert(SurvivalEquipmentCodeConcat,SurvivalEquipmentCodeList).

% fixm_DirectRouting(Graph, DirectRouting, From?, To?)

fixm_DirectRouting(Graph, DirectRouting, From, To) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?directRouting ?from ?to
WHERE
  { GRAPH ?graph
    {
      ?directRouting rdf:type fixm:DirectRouting .
      OPTIONAL {?directRouting fixm:from ?from .}
      OPTIONAL {?directRouting fixm:to ?to .}
    }
  }

      '
,row(Graph,DirectRouting,From,To),[]).

% fixm_TargetMultiTime(Graph, TargetMultiTime, Target?)

fixm_TargetMultiTime(Graph, TargetMultiTime, Target) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?targetMultiTime ?target
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:TargetMultiTime .
  }
  { GRAPH ?graph
    {
      ?targetMultiTime rdf:type ?SUBCLASS .
      OPTIONAL {?targetMultiTime fixm:target ?target .}
    }
  }
}

      '
,row(Graph,TargetMultiTime,Target),[]).

% fixm_AircraftType(Graph, AircraftType)

fixm_AircraftType(Graph, AircraftType) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?aircraftType
WHERE
  { GRAPH ?graph
    {
      ?aircraftType rdf:type fixm:AircraftType .
    }
  }

      '
,row(Graph,AircraftType),[]).

% fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome?, DepartureFix?, DepartureFixTime?, DepartureFleetPrioritization?, DepartureSlot?, EarliestOffBlockTime?, OffBlockReadyTime?, RunwayPositionAndTime?, StandardInstrumentDeparture?, StandPositionAndTime?, TakeoffAlternateAerodrome*, TakeoffWeight?, DepartureTimes?)

fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodromeList, TakeoffWeight, DepartureTimes) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?flightDeparture ?departureAerodrome ?departureFix ?departureFixTime ?departureFleetPrioritization ?departureSlot ?earliestOffBlockTime ?offBlockReadyTime ?runwayPositionAndTime ?standardInstrumentDeparture ?standPositionAndTime (GROUP_CONCAT(DISTINCT ?takeoffAlternateAerodrome;SEPARATOR=",") AS ?takeoffAlternateAerodromeConcat) ?takeoffWeight ?departureTimes
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:FlightDeparture .
  }
  { GRAPH ?graph
    {
      ?flightDeparture rdf:type ?SUBCLASS .
      OPTIONAL {?flightDeparture fixm:departureAerodrome ?departureAerodrome .}
      OPTIONAL {?flightDeparture fixm:departureFix ?departureFix .}
      OPTIONAL {?flightDeparture fixm:departureFixTime ?departureFixTime .}
      OPTIONAL { ?flightDeparture fixm:departureFleetPrioritization ?_departureFleetPrioritization .
        {
          {
            ?_departureFleetPrioritization rdf:value ?departureFleetPrioritizationValue .
            FILTER ( NOT EXISTS {?_departureFleetPrioritization (aixm:uom | fixm:uom | plain:uom) ?departureFleetPrioritizationUoM})
            BIND(concat(\'val:\',?departureFleetPrioritizationValue) AS ?departureFleetPrioritization)
          }
            UNION
          {
            ?_departureFleetPrioritization
              rdf:value ?departureFleetPrioritizationValue ;
              (aixm:uom | fixm:uom | plain:uom) ?departureFleetPrioritizationUoM .
            BIND(concat(\'xval:\',STR(?departureFleetPrioritizationValue),\':\',?departureFleetPrioritizationUoM) AS ?departureFleetPrioritization)
          }
            UNION
          {
           ?_departureFleetPrioritization  aixm:nilReason ?departureFleetPrioritizationNilReason .
           BIND(concat(\'nil:\',?departureFleetPrioritizationNilReason) AS ?departureFleetPrioritization)
          }
        }
      }
      OPTIONAL { ?flightDeparture fixm:departureSlot ?_departureSlot .
        {
          {
            ?_departureSlot rdf:value ?departureSlotValue .
            FILTER ( NOT EXISTS {?_departureSlot (aixm:uom | fixm:uom | plain:uom) ?departureSlotUoM})
            BIND(concat(\'val:\',?departureSlotValue) AS ?departureSlot)
          }
            UNION
          {
            ?_departureSlot
              rdf:value ?departureSlotValue ;
              (aixm:uom | fixm:uom | plain:uom) ?departureSlotUoM .
            BIND(concat(\'xval:\',STR(?departureSlotValue),\':\',?departureSlotUoM) AS ?departureSlot)
          }
            UNION
          {
           ?_departureSlot  aixm:nilReason ?departureSlotNilReason .
           BIND(concat(\'nil:\',?departureSlotNilReason) AS ?departureSlot)
          }
        }
      }
      OPTIONAL { ?flightDeparture fixm:earliestOffBlockTime ?_earliestOffBlockTime .
        {
          {
            ?_earliestOffBlockTime rdf:value ?earliestOffBlockTimeValue .
            FILTER ( NOT EXISTS {?_earliestOffBlockTime (aixm:uom | fixm:uom | plain:uom) ?earliestOffBlockTimeUoM})
            BIND(concat(\'val:\',?earliestOffBlockTimeValue) AS ?earliestOffBlockTime)
          }
            UNION
          {
            ?_earliestOffBlockTime
              rdf:value ?earliestOffBlockTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?earliestOffBlockTimeUoM .
            BIND(concat(\'xval:\',STR(?earliestOffBlockTimeValue),\':\',?earliestOffBlockTimeUoM) AS ?earliestOffBlockTime)
          }
            UNION
          {
           ?_earliestOffBlockTime  aixm:nilReason ?earliestOffBlockTimeNilReason .
           BIND(concat(\'nil:\',?earliestOffBlockTimeNilReason) AS ?earliestOffBlockTime)
          }
        }
      }
      OPTIONAL {?flightDeparture fixm:offBlockReadyTime ?offBlockReadyTime .}
      OPTIONAL {?flightDeparture fixm:runwayPositionAndTime ?runwayPositionAndTime .}
      OPTIONAL { ?flightDeparture fixm:standardInstrumentDeparture ?_standardInstrumentDeparture .
        {
          {
            ?_standardInstrumentDeparture rdf:value ?standardInstrumentDepartureValue .
            FILTER ( NOT EXISTS {?_standardInstrumentDeparture (aixm:uom | fixm:uom | plain:uom) ?standardInstrumentDepartureUoM})
            BIND(concat(\'val:\',?standardInstrumentDepartureValue) AS ?standardInstrumentDeparture)
          }
            UNION
          {
            ?_standardInstrumentDeparture
              rdf:value ?standardInstrumentDepartureValue ;
              (aixm:uom | fixm:uom | plain:uom) ?standardInstrumentDepartureUoM .
            BIND(concat(\'xval:\',STR(?standardInstrumentDepartureValue),\':\',?standardInstrumentDepartureUoM) AS ?standardInstrumentDeparture)
          }
            UNION
          {
           ?_standardInstrumentDeparture  aixm:nilReason ?standardInstrumentDepartureNilReason .
           BIND(concat(\'nil:\',?standardInstrumentDepartureNilReason) AS ?standardInstrumentDeparture)
          }
        }
      }
      OPTIONAL {?flightDeparture fixm:standPositionAndTime ?standPositionAndTime .}
      OPTIONAL {?flightDeparture fixm:takeoffAlternateAerodrome ?takeoffAlternateAerodrome .}
      OPTIONAL { ?flightDeparture fixm:takeoffWeight ?_takeoffWeight .
        {
          {
            ?_takeoffWeight rdf:value ?takeoffWeightValue .
            FILTER ( NOT EXISTS {?_takeoffWeight (aixm:uom | fixm:uom | plain:uom) ?takeoffWeightUoM})
            BIND(concat(\'val:\',?takeoffWeightValue) AS ?takeoffWeight)
          }
            UNION
          {
            ?_takeoffWeight
              rdf:value ?takeoffWeightValue ;
              (aixm:uom | fixm:uom | plain:uom) ?takeoffWeightUoM .
            BIND(concat(\'xval:\',STR(?takeoffWeightValue),\':\',?takeoffWeightUoM) AS ?takeoffWeight)
          }
            UNION
          {
           ?_takeoffWeight  aixm:nilReason ?takeoffWeightNilReason .
           BIND(concat(\'nil:\',?takeoffWeightNilReason) AS ?takeoffWeight)
          }
        }
      }
      OPTIONAL {?flightDeparture fixm:departureTimes ?departureTimes .}
    }
  }
}
GROUP BY ?graph ?flightDeparture ?departureAerodrome ?departureFix ?departureFixTime ?departureFleetPrioritization ?departureSlot ?earliestOffBlockTime ?offBlockReadyTime ?runwayPositionAndTime ?standardInstrumentDeparture ?standPositionAndTime ?takeoffWeight ?departureTimes

      '
,row(Graph,FlightDeparture,DepartureAerodrome,DepartureFix,DepartureFixTime,DepartureFleetPrioritization,DepartureSlot,EarliestOffBlockTime,OffBlockReadyTime,RunwayPositionAndTime,StandardInstrumentDeparture,StandPositionAndTime,TakeoffAlternateAerodromeConcat,TakeoffWeight,DepartureTimes),[]), convert(TakeoffAlternateAerodromeConcat,TakeoffAlternateAerodromeList).

% fixm_AerodromeReference(Graph, AerodromeReference)

fixm_AerodromeReference(Graph, AerodromeReference) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?aerodromeReference
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* fixm:AerodromeReference .
  }
  { GRAPH ?graph
    {
      ?aerodromeReference rdf:type ?SUBCLASS .
    }
  }
}

      '
,row(Graph,AerodromeReference),[]).

% fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime?, TaxiTime?)

fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime, TaxiTime) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?efplFlightDeparture ?estimatedOffBlockTime ?taxiTime
WHERE
  { GRAPH ?graph
    {
      ?efplFlightDeparture rdf:type fixm:EfplFlightDeparture .
      OPTIONAL { ?efplFlightDeparture fixm:estimatedOffBlockTime ?_estimatedOffBlockTime .
        {
          {
            ?_estimatedOffBlockTime rdf:value ?estimatedOffBlockTimeValue .
            FILTER ( NOT EXISTS {?_estimatedOffBlockTime (aixm:uom | fixm:uom | plain:uom) ?estimatedOffBlockTimeUoM})
            BIND(concat(\'val:\',?estimatedOffBlockTimeValue) AS ?estimatedOffBlockTime)
          }
            UNION
          {
            ?_estimatedOffBlockTime
              rdf:value ?estimatedOffBlockTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?estimatedOffBlockTimeUoM .
            BIND(concat(\'xval:\',STR(?estimatedOffBlockTimeValue),\':\',?estimatedOffBlockTimeUoM) AS ?estimatedOffBlockTime)
          }
            UNION
          {
           ?_estimatedOffBlockTime  aixm:nilReason ?estimatedOffBlockTimeNilReason .
           BIND(concat(\'nil:\',?estimatedOffBlockTimeNilReason) AS ?estimatedOffBlockTime)
          }
        }
      }
      OPTIONAL { ?efplFlightDeparture fixm:taxiTime ?_taxiTime .
        {
          {
            ?_taxiTime rdf:value ?taxiTimeValue .
            FILTER ( NOT EXISTS {?_taxiTime (aixm:uom | fixm:uom | plain:uom) ?taxiTimeUoM})
            BIND(concat(\'val:\',?taxiTimeValue) AS ?taxiTime)
          }
            UNION
          {
            ?_taxiTime
              rdf:value ?taxiTimeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?taxiTimeUoM .
            BIND(concat(\'xval:\',STR(?taxiTimeValue),\':\',?taxiTimeUoM) AS ?taxiTime)
          }
            UNION
          {
           ?_taxiTime  aixm:nilReason ?taxiTimeNilReason .
           BIND(concat(\'nil:\',?taxiTimeNilReason) AS ?taxiTime)
          }
        }
      }
    }
  }

      '
,row(Graph,EfplFlightDeparture,EstimatedOffBlockTime,TaxiTime),[]).

% aixm_UsageCondition(Graph, UsageCondition, Type?, PriorPermission?, Selection?, Annotation*, Contact*)

aixm_UsageCondition(Graph, UsageCondition, Type, PriorPermission, Selection, AnnotationList, ContactList) :-
  sparql_query(
      '
PREFIX s2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#>
PREFIX s1: <https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#>
PREFIX g2: <https://github.com/aixm/donlon/blob/master/digitalNOTAM/>
PREFIX fixm: <http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#>
PREFIX aixm: <http://www.aisa-project.eu/vocabulary/aixm_5-1-1#>
PREFIX plain: <http://www.aisa-project.eu/vocabulary/plain#>
PREFIX g1: <https://github.com/aixm/donlon/blob/master/>
PREFIX graph: <https://github.com/jku-win-dke/aisa/graphs/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX event: <http://www.aixm.aero/schema/5.1/event#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX gml: <http://www.opengis.net/gml/3.2#>
PREFIX file: <https://www.jena.com/plain#>
PREFIX sh: <http://www.w3.org/ns/shacl#>
PREFIX uuid: <uuid:>


SELECT ?graph ?usageCondition ?type ?priorPermission ?selection (GROUP_CONCAT(DISTINCT ?annotation;SEPARATOR=",") AS ?annotationConcat) (GROUP_CONCAT(DISTINCT ?contact;SEPARATOR=",") AS ?contactConcat)
WHERE
 {
  GRAPH <https://github.com/jku-win-dke/aisa/graphs/schema> {
    ?SUBCLASS rdfs:subClassOf* aixm:UsageCondition .
  }
  { GRAPH ?graph
    {
      ?usageCondition rdf:type ?SUBCLASS .
      OPTIONAL { ?usageCondition aixm:type ?_type .
        {
          {
            ?_type rdf:value ?typeValue .
            FILTER ( NOT EXISTS {?_type (aixm:uom | fixm:uom | plain:uom) ?typeUoM})
            BIND(concat(\'val:\',?typeValue) AS ?type)
          }
            UNION
          {
            ?_type
              rdf:value ?typeValue ;
              (aixm:uom | fixm:uom | plain:uom) ?typeUoM .
            BIND(concat(\'xval:\',STR(?typeValue),\':\',?typeUoM) AS ?type)
          }
            UNION
          {
           ?_type  aixm:nilReason ?typeNilReason .
           BIND(concat(\'nil:\',?typeNilReason) AS ?type)
          }
        }
      }
      OPTIONAL { ?usageCondition aixm:priorPermission ?_priorPermission .
        {
          {
            ?_priorPermission rdf:value ?priorPermissionValue .
            FILTER ( NOT EXISTS {?_priorPermission (aixm:uom | fixm:uom | plain:uom) ?priorPermissionUoM})
            BIND(concat(\'val:\',?priorPermissionValue) AS ?priorPermission)
          }
            UNION
          {
            ?_priorPermission
              rdf:value ?priorPermissionValue ;
              (aixm:uom | fixm:uom | plain:uom) ?priorPermissionUoM .
            BIND(concat(\'xval:\',STR(?priorPermissionValue),\':\',?priorPermissionUoM) AS ?priorPermission)
          }
            UNION
          {
           ?_priorPermission  aixm:nilReason ?priorPermissionNilReason .
           BIND(concat(\'nil:\',?priorPermissionNilReason) AS ?priorPermission)
          }
        }
      }
      OPTIONAL {?usageCondition aixm:selection ?selection .}
      OPTIONAL {?usageCondition aixm:annotation ?annotation .}
      OPTIONAL {?usageCondition aixm:contact ?contact .}
    }
  }
}
GROUP BY ?graph ?usageCondition ?type ?priorPermission ?selection

      '
,row(Graph,UsageCondition,Type,PriorPermission,Selection,AnnotationConcat,ContactConcat),[]), convert(AnnotationConcat,AnnotationList), convert(ContactConcat,ContactList).

fixm_ExpandedRoutePoint_Combined(Graph, ExpandedRoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit, EstimatedLevel, EstimatedTime, ConstraintList) :-
  fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel, EstimatedTime, ConstraintList),
  fixm_AbstractRoutePoint(Graph, ExpandedRoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit) .

aixm_ElevatedSurface_Combined(Graph, ElevatedSurface, PatchList, HorizontalAccuracy, AnnotationList, HorizontalAccuracy, AnnotationList, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy),
  aixm_Surface_Combined(Graph,ElevatedSurface, PatchList, HorizontalAccuracy, AnnotationList) .

aixm_ConditionCombination_Combined(Graph, ConditionCombination, AnnotationList, SpecialDateAuthorityList, TimeIntervalList, LogicalOperator, FlightList, AircraftList, WeatherList, SubConditionList) :-
  aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator, FlightList, AircraftList, WeatherList, SubConditionList),
  aixm_PropertiesWithSchedule(Graph, ConditionCombination, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) .

aixm_ElevatedPoint_Combined(Graph, ElevatedPoint, HorizontalAccuracy, AnnotationList, HorizontalAccuracy, AnnotationList, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy),
  aixm_Point_Combined(Graph,ElevatedPoint, HorizontalAccuracy, AnnotationList) .

fixm_EfplPoint4D_Combined(Graph, EfplPoint4D, PosList, SrsName, Altitude, Time, PointRange, Altitude, Time, PointRange, FlightLevel) :-
  fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel),
  fixm_Point4D_Combined(Graph,EfplPoint4D, PosList, SrsName, Altitude, Time, PointRange) .

fixm_EfplTrajectoryRoutePair_Combined(Graph, EfplTrajectoryRoutePair, Trajectory, Route) :-
  fixm_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair),
  fixm_TrajectoryRoutePair(Graph, EfplTrajectoryRoutePair, Trajectory, Route) .

fixm_RoutePoint_Combined(Graph, RoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit, ConstraintList) :-
  fixm_RoutePoint(Graph, RoutePoint, ConstraintList),
  fixm_AbstractRoutePoint(Graph, RoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit) .

aixm_AirportHeliportResponsibilityOrganisation_Combined(Graph, AirportHeliportResponsibilityOrganisation, AnnotationList, SpecialDateAuthorityList, TimeIntervalList, Role, TheOrganisationAuthority) :-
  aixm_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role, TheOrganisationAuthority),
  aixm_PropertiesWithSchedule(Graph, AirportHeliportResponsibilityOrganisation, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) .

fixm_DangerousGoods_Combined(Graph, DangerousGoods, Provenance, GuidebookNumber, OnboardLocation, HandlingInformation, AircraftLimitation, AirWayBill, Shipment, PackageGroupList, ShippingInformation) :-
  fixm_DangerousGoods(Graph, DangerousGoods, GuidebookNumber, OnboardLocation, HandlingInformation, AircraftLimitation, AirWayBill, Shipment, PackageGroupList, ShippingInformation),
  fixm_Feature(Graph, DangerousGoods, Provenance) .

fixm_Point4D_Combined(Graph, Point4D, PosList, SrsName, Altitude, Time, PointRange) :-
  fixm_Point4D(Graph, Point4D, Altitude, Time, PointRange),
  fixm_GeographicLocation(Graph, Point4D, PosList, SrsName) .

fixm_FlightEmergency_Combined(Graph, FlightEmergency, Provenance, ActionTaken, EmergencyDescription, Originator, OtherInformation, Phase, Contact) :-
  fixm_FlightEmergency(Graph, FlightEmergency, ActionTaken, EmergencyDescription, Originator, OtherInformation, Phase, Contact),
  fixm_Feature(Graph, FlightEmergency, Provenance) .

fixm_Flight_Combined(Graph, Flight, Provenance, ControllingUnit, ExtensionsList, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoodsList, RankedTrajectoriesList, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandlingList) :-
  fixm_Flight(Graph, Flight, ControllingUnit, ExtensionsList, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoodsList, RankedTrajectoriesList, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandlingList),
  fixm_Feature(Graph, Flight, Provenance) .

gml_Surface_Combined(Graph, Surface, PatchList) :-
  gml_Surface(Graph, Surface, PatchList),
  gml_SurfacePatch(Graph, Surface) .

fixm_UnitBoundary_Combined(Graph, UnitBoundary, SectorIdentifier, Delegated, DownstreamUnit, UpstreamUnit, BoundaryCrossingProposed, BoundaryCrossingCoordinated, Handoff, UnitBoundaryIndicator) :-
  fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit, UpstreamUnit, BoundaryCrossingProposed, BoundaryCrossingCoordinated, Handoff, UnitBoundaryIndicator),
  fixm_AtcUnitReference(Graph, UnitBoundary, SectorIdentifier, Delegated) .

aixm_AirportHeliportContamination_Combined(Graph, AirportHeliportContamination, ObservationTime, Depth, FrictionCoefficient, FrictionEstimation, FrictionDevice, ObscuredLights, FurtherClearanceTime, FurtherTotalClearance, NextObservationTime, Proportion, CriticalRidgeList, AnnotationList, LayerList) :-
  aixm_AirportHeliportContamination(Graph, AirportHeliportContamination),
  aixm_SurfaceContamination(Graph, AirportHeliportContamination, ObservationTime, Depth, FrictionCoefficient, FrictionEstimation, FrictionDevice, ObscuredLights, FurtherClearanceTime, FurtherTotalClearance, NextObservationTime, Proportion, CriticalRidgeList, AnnotationList, LayerList) .

aixm_TelephoneContact_Combined(Graph, TelephoneContact, AnnotationList, SpecialDateAuthorityList, TimeIntervalList, Voice, Facsimile) :-
  aixm_TelephoneContact(Graph, TelephoneContact, Voice, Facsimile),
  aixm_PropertiesWithSchedule(Graph, TelephoneContact, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) .

fixm_Route_Combined(Graph, Route, Provenance, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTimeList, ExpandedRoute, ClimbSchedule, DescentSchedule, SegmentList) :-
  fixm_Route(Graph, Route, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTimeList, ExpandedRoute, ClimbSchedule, DescentSchedule, SegmentList),
  fixm_Feature(Graph, Route, Provenance) .

fixm_EfplFlight_Combined(Graph, EfplFlight, Provenance, ControllingUnit, ExtensionsList, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoodsList, RankedTrajectoriesList, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandlingList, ControllingUnit, ExtensionsList, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoodsList, RankedTrajectoriesList, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandlingList, IfplId, TotalEstimatedElapsedTime, AerodromesOfDestination, EfplSpecialHandling, EfplFiledTrajectory, EfplAcceptedTrajectory, OtherInformation, FlightPerformanceData) :-
  fixm_EfplFlight(Graph, EfplFlight, IfplId, TotalEstimatedElapsedTime, AerodromesOfDestination, EfplSpecialHandling, EfplFiledTrajectory, EfplAcceptedTrajectory, OtherInformation, FlightPerformanceData),
  fixm_Flight_Combined(Graph,EfplFlight, Provenance, ControllingUnit, ExtensionsList, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoodsList, RankedTrajectoriesList, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandlingList) .

fixm_FlightStatus_Combined(Graph, FlightStatus, Provenance, AirborneHold, Airfile, Accepted, FlightCycle, MissedApproach, Suspended) :-
  fixm_FlightStatus(Graph, FlightStatus, AirborneHold, Airfile, Accepted, FlightCycle, MissedApproach, Suspended),
  fixm_Feature(Graph, FlightStatus, Provenance) .

fixm_IdentifiedUnitReference_Combined(Graph, IdentifiedUnitReference, SectorIdentifier, Delegated, UnitIdentifier) :-
  fixm_IdentifiedUnitReference(Graph, IdentifiedUnitReference, UnitIdentifier),
  fixm_AtcUnitReference(Graph, IdentifiedUnitReference, SectorIdentifier, Delegated) .

aixm_OnlineContact_Combined(Graph, OnlineContact, AnnotationList, SpecialDateAuthorityList, TimeIntervalList, Network, Linkage, Protocol, EMail) :-
  aixm_OnlineContact(Graph, OnlineContact, Network, Linkage, Protocol, EMail),
  aixm_PropertiesWithSchedule(Graph, OnlineContact, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) .

fixm_StructuredPostalAddress_Combined(Graph, StructuredPostalAddress, Name, Title, OnlineContact, PhoneFax, Address) :-
  fixm_StructuredPostalAddress(Graph, StructuredPostalAddress),
  fixm_ContactInformation(Graph, StructuredPostalAddress, Name, Title, OnlineContact, PhoneFax, Address) .

fixm_AircraftPosition_Combined(Graph, AircraftPosition, Provenance, Altitude, Position, PositionTime, Track, ActualSpeed, NextPosition, ReportSource, FollowingPosition) :-
  fixm_AircraftPosition(Graph, AircraftPosition, Altitude, Position, PositionTime, Track, ActualSpeed, NextPosition, ReportSource, FollowingPosition),
  fixm_Feature(Graph, AircraftPosition, Provenance) .

aixm_AirportHeliportUsage_Combined(Graph, AirportHeliportUsage, Type, PriorPermission, Selection, AnnotationList, ContactList, Operation) :-
  aixm_AirportHeliportUsage(Graph, AirportHeliportUsage, Operation),
  aixm_UsageCondition(Graph, AirportHeliportUsage, Type, PriorPermission, Selection, AnnotationList, ContactList) .

fixm_EfplTrajectoryPoint_Combined(Graph, EfplTrajectoryPoint, AltimeterSetting, PredictedAirspeed, PredictedGroundspeed, MetData, Point, TrajectoryChangeList, TrajectoryChangeTypeList, ReferencePoint, AerodromeIdentifier, DistanceFromTakeOff, EfplEstimatedSpeed, ElapsedTime, GrossWeight, TrajectoryPointType, TrajectoryPointRole, InboundSegment) :-
  fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier, DistanceFromTakeOff, EfplEstimatedSpeed, ElapsedTime, GrossWeight, TrajectoryPointType, TrajectoryPointRole, InboundSegment),
  fixm_TrajectoryPoint(Graph, EfplTrajectoryPoint, AltimeterSetting, PredictedAirspeed, PredictedGroundspeed, MetData, Point, TrajectoryChangeList, TrajectoryChangeTypeList, ReferencePoint) .

fixm_LastContact_Combined(Graph, LastContact, Provenance, ContactFrequency, LastContactTime, LastContactUnit, Position) :-
  fixm_LastContact(Graph, LastContact, ContactFrequency, LastContactTime, LastContactUnit, Position),
  fixm_Feature(Graph, LastContact, Provenance) .

aixm_Surface_Combined(Graph, Surface, PatchList, PatchList, HorizontalAccuracy, AnnotationList) :-
  aixm_Surface(Graph, Surface, HorizontalAccuracy, AnnotationList),
  gml_Surface_Combined(Graph,Surface, PatchList) .

fixm_EnRoute_Combined(Graph, EnRoute, Provenance, AlternateAerodromeList, FleetPrioritization, BoundaryCrossingsList, CpdlcConnection, BeaconCodeAssignment, Cleared, ControlElementList, Pointout, Position) :-
  fixm_EnRoute(Graph, EnRoute, AlternateAerodromeList, FleetPrioritization, BoundaryCrossingsList, CpdlcConnection, BeaconCodeAssignment, Cleared, ControlElementList, Pointout, Position),
  fixm_Feature(Graph, EnRoute, Provenance) .

fixm_Aircraft_Combined(Graph, Aircraft, Provenance, AircraftColours, AircraftQuantity, EngineType, AircraftAddress, Capabilities, Registration, AircraftType, WakeTurbulence, AircraftPerformance) :-
  fixm_Aircraft(Graph, Aircraft, AircraftColours, AircraftQuantity, EngineType, AircraftAddress, Capabilities, Registration, AircraftType, WakeTurbulence, AircraftPerformance),
  fixm_Feature(Graph, Aircraft, Provenance) .

fixm_Extension_Combined(Graph, Extension, Provenance) :-
  fixm_Extension(Graph, Extension),
  fixm_Feature(Graph, Extension, Provenance) .

aixm_Point_Combined(Graph, Point, HorizontalAccuracy, AnnotationList) :-
  aixm_Point(Graph, Point, HorizontalAccuracy, AnnotationList),
  gml_Point(Graph, Point) .

aixm_PostalAddress_Combined(Graph, PostalAddress, AnnotationList, SpecialDateAuthorityList, TimeIntervalList, DeliveryPoint, City, AdministrativeArea, PostalCode, Country) :-
  aixm_PostalAddress(Graph, PostalAddress, DeliveryPoint, City, AdministrativeArea, PostalCode, Country),
  aixm_PropertiesWithSchedule(Graph, PostalAddress, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) .

aixm_AltimeterSourceStatus_Combined(Graph, AltimeterSourceStatus, AnnotationList, SpecialDateAuthorityList, TimeIntervalList, OperationalStatus) :-
  aixm_AltimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus),
  aixm_PropertiesWithSchedule(Graph, AltimeterSourceStatus, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) .

fixm_EfplRoute_Combined(Graph, EfplRoute, Provenance, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTimeList, ExpandedRoute, ClimbSchedule, DescentSchedule, SegmentList, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTimeList, ExpandedRoute, ClimbSchedule, DescentSchedule, SegmentList, EfplFlightRules) :-
  fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules),
  fixm_Route_Combined(Graph,EfplRoute, Provenance, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTimeList, ExpandedRoute, ClimbSchedule, DescentSchedule, SegmentList) .

fixm_IcaoAerodromeReference_Combined(Graph, IcaoAerodromeReference, Code) :-
  fixm_IcaoAerodromeReference(Graph, IcaoAerodromeReference, Code),
  fixm_AerodromeReference(Graph, IcaoAerodromeReference) .

fixm_RadioCommunicationFailure_Combined(Graph, RadioCommunicationFailure, Provenance, RadioFailureRemarks, RemainingComCapability, Contact) :-
  fixm_RadioCommunicationFailure(Graph, RadioCommunicationFailure, RadioFailureRemarks, RemainingComCapability, Contact),
  fixm_Feature(Graph, RadioCommunicationFailure, Provenance) .

aixm_AirportHeliportAvailability_Combined(Graph, AirportHeliportAvailability, AnnotationList, SpecialDateAuthorityList, TimeIntervalList, OperationalStatus, Warning, UsageList) :-
  aixm_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus, Warning, UsageList),
  aixm_PropertiesWithSchedule(Graph, AirportHeliportAvailability, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) .

fixm_FlightArrival_Combined(Graph, FlightArrival, Provenance, ApproachFix, ApproachTime, ArrivalAerodrome, ArrivalAerodromeAlternateList, ArrivalAerodromeOriginal, ArrivalFix, ArrivalFixTime, ArrivalFleetPrioritization, ArrivalSequenceNumber, EarliestInBlockTime, FiledRevisedDestinationAerodrome, FiledRevisedDestinationStar, RunwayPositionAndTime, StandardInstrumentArrival, StandPositionAndTime, LandingLimits) :-
  fixm_FlightArrival(Graph, FlightArrival, ApproachFix, ApproachTime, ArrivalAerodrome, ArrivalAerodromeAlternateList, ArrivalAerodromeOriginal, ArrivalFix, ArrivalFixTime, ArrivalFleetPrioritization, ArrivalSequenceNumber, EarliestInBlockTime, FiledRevisedDestinationAerodrome, FiledRevisedDestinationStar, RunwayPositionAndTime, StandardInstrumentArrival, StandPositionAndTime, LandingLimits),
  fixm_Feature(Graph, FlightArrival, Provenance) .

fixm_ExtendedMultiTime_Combined(Graph, ExtendedMultiTime, Actual, Estimated, Target, Target, Controlled, Initial) :-
  fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled, Initial),
  fixm_TargetMultiTime_Combined(Graph,ExtendedMultiTime, Actual, Estimated, Target) .

fixm_TargetMultiTime_Combined(Graph, TargetMultiTime, Actual, Estimated, Target) :-
  fixm_TargetMultiTime(Graph, TargetMultiTime, Target),
  fixm_MultiTime(Graph, TargetMultiTime, Actual, Estimated) .

fixm_FlightDeparture_Combined(Graph, FlightDeparture, Provenance, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodromeList, TakeoffWeight, DepartureTimes) :-
  fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodromeList, TakeoffWeight, DepartureTimes),
  fixm_Feature(Graph, FlightDeparture, Provenance) .

fixm_EfplFlightDeparture_Combined(Graph, EfplFlightDeparture, Provenance, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodromeList, TakeoffWeight, DepartureTimes, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodromeList, TakeoffWeight, DepartureTimes, EstimatedOffBlockTime, TaxiTime) :-
  fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime, TaxiTime),
  fixm_FlightDeparture_Combined(Graph,EfplFlightDeparture, Provenance, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodromeList, TakeoffWeight, DepartureTimes) .
