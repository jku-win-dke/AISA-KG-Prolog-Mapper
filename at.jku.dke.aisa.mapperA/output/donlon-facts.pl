/* use the new RDF-DB library 
https://www.swi-prolog.org/pldoc/man?section=semweb-rdf11 */
:- use_module(library(semweb/rdf11)).

/* for writing/reading RDF files in Turtle format we use the Turtle library
https://www.swi-prolog.org/pldoc/man?section=turtle */
:- use_module(library(semweb/turtle)). 

/* do not output bindings for anonymous variables (e.g., _X) in query results 
https://www.swi-prolog.org/pldoc/man?section=flags#flag:toplevel_print_anon */
:- set_prolog_flag(toplevel_print_anon, false).

/* PREFIX HANDLING 
see: https://www.swi-prolog.org/pldoc/doc/_SWI_/library/semweb/rdf_prefixes.pl
*/

/* declare namespace prefixes - in addition to predeclared ones 
https://www.swi-prolog.org/pldoc/doc_for?object=rdf_register_prefix/2
*/
:- rdf_register_prefix(s2,'https://github.com/aixm/donlon/blob/master/digitalNOTAM/DN_AD.CLS_except_special_flights.xml#').
:- rdf_register_prefix(s1,'https://github.com/aixm/donlon/blob/master/EA_AIP_DS_FULL_20170701.xml#').
:- rdf_register_prefix(g2,'https://github.com/aixm/donlon/blob/master/digitalNOTAM/').
:- rdf_register_prefix(aixm,'http://www.aisa-project.eu/vocabulary/aixm_5-1-1#').
:- rdf_register_prefix(g1,'https://github.com/aixm/donlon/blob/master/').
:- rdf_register_prefix(graph,'https://github.com/jku-win-dke/aisa/graphs/').
:- rdf_register_prefix(rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_prefix(event,'http://www.aixm.aero/schema/5.1/event#').
:- rdf_register_prefix(rdfs,'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_prefix(xsd,'http://www.w3.org/2001/XMLSchema#').
:- rdf_register_prefix(gml,'http://www.opengis.net/gml/3.2#').
:- rdf_register_prefix(sh,'http://www.w3.org/ns/shacl#').
:- rdf_register_prefix(uuid,'uuid:').

/* for prefix handling: declare predicates that have RDF terms as arguments
see: https://www.swi-prolog.org/pldoc/man?predicate=rdf_meta/1
*/
:- rdf_meta
  % airportHeliportContamination(Graph, AirportHeliportContamination)
  airportHeliportContamination(r,r)

  % city(Graph, City, Name?, Annotation*)
  ,city(r,r,t,t)

  % airportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role?, TheOrganisationAuthority)
  ,airportHeliportResponsibilityOrganisation(r,r,t,t)

  % note(Graph, Note, PropertyName?, Purpose?, TranslatedNote*)
  ,note(r,r,t,t,t)

  % telephoneContact(Graph, TelephoneContact, Voice?, Facsimile?)
  ,telephoneContact(r,r,t,t)

  % contactInformation(Graph, ContactInformation, Name?, Title?, Annotation*, NetworkNode*, Address*, PhoneFax*)
  ,contactInformation(r,r,t,t,t,t,t,t)

  % onlineContact(Graph, OnlineContact, Network?, Linkage?, Protocol?, EMail?)
  ,onlineContact(r,r,t,t,t,t)

  % elevatedSurface(Graph, ElevatedSurface, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)
  ,elevatedSurface(r,r,t,t,t,t)

  % altimeterSourceTimeSlice(Graph, AltimeterSourceTimeSlice, IsRemote?, IsPrimary?, Availability*, Annotation*)
  ,altimeterSourceTimeSlice(r,r,t,t,t,t)

  % point(Graph, Point, HorizontalAccuracy?, Annotation*)
  ,point(r,r,t,t)

  % postalAddress(Graph, PostalAddress, DeliveryPoint?, City?, AdministrativeArea?, PostalCode?, Country?)
  ,postalAddress(r,r,t,t,t,t,t)

  % aircraftCharacteristic(Graph, AircraftCharacteristic, Type?, Engine?, NumberEngine?, TypeAircraftICAO?, AircraftLandingCategory?, WingSpan?, WingSpanInterpretation?, ClassWingSpan?, Weight?, WeightInterpretation?, Passengers?, PassengersInterpretation?, Speed?, SpeedInterpretation?, WakeTurbulence?, NavigationEquipment?, NavigationSpecification?, VerticalSeparationCapability?, AntiCollisionAndSeparationEquipment?, CommunicationEquipment?, SurveillanceEquipment?, Annotation*)
  ,aircraftCharacteristic(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % airportHeliportUsage(Graph, AirportHeliportUsage, Operation?)
  ,airportHeliportUsage(r,r,t)

  % conditionCombination(Graph, ConditionCombination, LogicalOperator?, Flight*, Aircraft*, Weather*, SubCondition*)
  ,conditionCombination(r,r,t,t,t,t,t)

  % surfaceContaminationLayer(Graph, SurfaceContaminationLayer, LayerOrder?, Type?, Extent*, Annotation*)
  ,surfaceContaminationLayer(r,r,t,t,t,t)

  % timesheet(Graph, Timesheet, TimeReference?, StartDate?, EndDate?, Day?, DayTil?, StartTime?, StartEvent?, StartTimeRelativeEvent?, StartEventInterpretation?, EndTime?, EndEvent?, EndTimeRelativeEvent?, EndEventInterpretation?, DaylightSavingAdjust?, Excluded?, Annotation*)
  ,timesheet(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % organisationAuthorityAssociation(Graph, OrganisationAuthorityAssociation, Type?, Annotation*, TheOrganisationAuthority)
  ,organisationAuthorityAssociation(r,r,t,t,t)

  % surfacePatch(Graph, SurfacePatch)
  ,surfacePatch(r,r)

  % altimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus?)
  ,altimeterSourceStatus(r,r,t)

  % elevatedPoint(Graph, ElevatedPoint, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)
  ,elevatedPoint(r,r,t,t,t,t)

  % airportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator?, Name?, LocationIndicatorICAO?, DesignatorIATA?, Type?, CertifiedICAO?, PrivateUse?, ControlType?, FieldElevation?, FieldElevationAccuracy?, VerticalDatum?, MagneticVariation?, MagneticVariationAccuracy?, DateMagneticVariation?, MagneticVariationChange?, ReferenceTemperature?, AltimeterCheckLocation?, SecondaryPowerSupply?, WindDirectionIndicator?, LandingDirectionIndicator?, TransitionAltitude?, TransitionLevel?, LowestTemperature?, Abandoned?, CertificationDate?, CertificationExpirationDate?, Contact*, Annotation*, ARP?, AltimeterSource*, Contaminant*, ServedCity*, ResponsibleOrganisation?, AviationBoundary?, Availability*)
  ,airportHeliportTimeSlice(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % flightCharacteristic(Graph, FlightCharacteristic, Type?, Rule?, Status?, Military?, Origin?, Purpose?, Annotation*)
  ,flightCharacteristic(r,r,t,t,t,t,t,t,t)

  % ridge(Graph, Ridge, Side?, Distance?, Depth?, Annotation*)
  ,ridge(r,r,t,t,t,t)

  % airportHeliport(Graph, AirportHeliport, TimeSlice*)
  ,airportHeliport(r,r,t)

  % airportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus?, Warning?, Usage*)
  ,airportHeliportAvailability(r,r,t,t,t)

  % point(Graph, Point)
  ,point(r,r)

  % propertiesWithSchedule(Graph, PropertiesWithSchedule, Annotation*, SpecialDateAuthority*, TimeInterval*)
  ,propertiesWithSchedule(r,r,t,t,t)

  % altimeterSource(Graph, AltimeterSource, TimeSlice*)
  ,altimeterSource(r,r,t)

  % surface(Graph, Surface, Patch+)
  ,surface(r,r,t)

  % surfaceContamination(Graph, SurfaceContamination, ObservationTime?, Depth?, FrictionCoefficient?, FrictionEstimation?, FrictionDevice?, ObscuredLights?, FurtherClearanceTime?, FurtherTotalClearance?, NextObservationTime?, Proportion?, CriticalRidge*, Annotation*, Layer*)
  ,surfaceContamination(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % organisationAuthority(Graph, OrganisationAuthority, TimeSlice*)
  ,organisationAuthority(r,r,t)

  % surface(Graph, Surface, HorizontalAccuracy?, Annotation*)
  ,surface(r,r,t,t)

  % timePeriod(Graph, TimePeriod, BeginPosition, EndPosition)
  ,timePeriod(r,r,t,t)

  % organisationAuthorityTimeSlice(Graph, OrganisationAuthorityTimeSlice, Name?, Designator?, Type?, Military?, Annotation*, Contact*, RelatedOrganisationAuthority*)
  ,organisationAuthorityTimeSlice(r,r,t,t,t,t,t,t,t)

  % linguisticNote(Graph, LinguisticNote, Note?)
  ,linguisticNote(r,r,t)

  % meteorology(Graph, Meteorology, FlightConditions?, Visibility?, VisibilityInterpretation?, RunwayVisualRange?, RunwayVisualRangeInterpretation?, Annotation*)
  ,meteorology(r,r,t,t,t,t,t,t)

  % usageCondition(Graph, UsageCondition, Type?, PriorPermission?, Selection?, Annotation*, Contact*)
  ,usageCondition(r,r,t,t,t,t,t)
.

% airportHeliportContamination(Graph, AirportHeliportContamination)

% city(Graph, City, Name?, Annotation*)
city(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'ID_110', val('DONLON'), []).

% airportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role?, TheOrganisationAuthority)
airportHeliportResponsibilityOrganisation(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'A-a72cfd3a', val('OPERATE'), uuid:'74efb6ba-a52a-46c0-a16b-03860d356882').

% note(Graph, Note, PropertyName?, Purpose?, TranslatedNote*)
note(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'n003', val('aRP'), '$null$', [s1:'ln003']).
note(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'n002', val('fieldElevation'), '$null$', [s1:'ln002']).

% telephoneContact(Graph, TelephoneContact, Voice?, Facsimile?)

% contactInformation(Graph, ContactInformation, Name?, Title?, Annotation*, NetworkNode*, Address*, PhoneFax*)

% onlineContact(Graph, OnlineContact, Network?, Linkage?, Protocol?, EMail?)

% elevatedSurface(Graph, ElevatedSurface, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)

% altimeterSourceTimeSlice(Graph, AltimeterSourceTimeSlice, IsRemote?, IsPrimary?, Availability*, Annotation*)

% point(Graph, Point, HorizontalAccuracy?, Annotation*)
point(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'elpoint1EADH', '$null$', []).

% postalAddress(Graph, PostalAddress, DeliveryPoint?, City?, AdministrativeArea?, PostalCode?, Country?)

% aircraftCharacteristic(Graph, AircraftCharacteristic, Type?, Engine?, NumberEngine?, TypeAircraftICAO?, AircraftLandingCategory?, WingSpan?, WingSpanInterpretation?, ClassWingSpan?, Weight?, WeightInterpretation?, Passengers?, PassengersInterpretation?, Speed?, SpeedInterpretation?, WakeTurbulence?, NavigationEquipment?, NavigationSpecification?, VerticalSeparationCapability?, AntiCollisionAndSeparationEquipment?, CommunicationEquipment?, SurveillanceEquipment?, Annotation*)

% airportHeliportUsage(Graph, AirportHeliportUsage, Operation?)
airportHeliportUsage(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_14', '$null$').
airportHeliportUsage(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'AHU_EADH_PERMIT', '$null$').

% conditionCombination(Graph, ConditionCombination, LogicalOperator?, Flight*, Aircraft*, Weather*, SubCondition*)
conditionCombination(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_20', val('NONE'), [s2:'ID_ACT_21'], [], [], []).
conditionCombination(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_15', val('OR'), [], [], [], [s2:'ID_ACT_16', s2:'ID_ACT_20', s2:'ID_ACT_18']).
conditionCombination(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_16', val('NONE'), [s2:'ID_ACT_17'], [], [], []).
conditionCombination(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'agtayyat', val('NONE'), [s1:'F_yastadyt'], [], [], []).
conditionCombination(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_18', val('NONE'), [s2:'ID_ACT_19'], [], [], []).

% surfaceContaminationLayer(Graph, SurfaceContaminationLayer, LayerOrder?, Type?, Extent*, Annotation*)

% timesheet(Graph, Timesheet, TimeReference?, StartDate?, EndDate?, Day?, DayTil?, StartTime?, StartEvent?, StartTimeRelativeEvent?, StartEventInterpretation?, EndTime?, EndEvent?, EndTimeRelativeEvent?, EndEventInterpretation?, DaylightSavingAdjust?, Excluded?, Annotation*)

% organisationAuthorityAssociation(Graph, OrganisationAuthorityAssociation, Type?, Annotation*, TheOrganisationAuthority)

% surfacePatch(Graph, SurfacePatch)

% altimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus?)

% elevatedPoint(Graph, ElevatedPoint, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)
elevatedPoint(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'elpoint1EADH', '$null$', '$null$', '$null$', '$null$').

% airportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator?, Name?, LocationIndicatorICAO?, DesignatorIATA?, Type?, CertifiedICAO?, PrivateUse?, ControlType?, FieldElevation?, FieldElevationAccuracy?, VerticalDatum?, MagneticVariation?, MagneticVariationAccuracy?, DateMagneticVariation?, MagneticVariationChange?, ReferenceTemperature?, AltimeterCheckLocation?, SecondaryPowerSupply?, WindDirectionIndicator?, LandingDirectionIndicator?, TransitionAltitude?, TransitionLevel?, LowestTemperature?, Abandoned?, CertificationDate?, CertificationExpirationDate?, Contact*, Annotation*, ARP?, AltimeterSource*, Contaminant*, ServedCity*, ResponsibleOrganisation?, AviationBoundary?, Availability*)
airportHeliportTimeSlice(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_11', '$null$', '$null$', '$null$', val('ysdf'), '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], [], '$null$', [], [], [s2:'city1', s2:'city2'], '$null$', '$null$', [s2:'ID_ACT_13']).
airportHeliportTimeSlice(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'AHP_EADH', val('EADH'), val('DONLON/DOWNTOWN HELIPORT'), val('EADH'), nil('unknown'), '$null$', nil('unknown'), '$null$', val('CIVIL'), xval('18','M'), xval('0.5','M'), '$null$', '$null$', '$null$', val('1990'), '$null$', xval('21.0','C'), '$null$', '$null$', '$null$', '$null$', xval('3500','FT'), '$null$', '$null$', '$null$', nil('unknown'), '$null$', [], [s1:'n003', s1:'n002'], s1:'elpoint1EADH', [], [], [s1:'ID_110'], s1:'A-a72cfd3a', '$null$', [s1:'AHY_EADH_PERMIT']).

% flightCharacteristic(Graph, FlightCharacteristic, Type?, Rule?, Status?, Military?, Origin?, Purpose?, Annotation*)
flightCharacteristic(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_21', '$null$', '$null$', val('OTHER'), '$null$', '$null$', '$null$', []).
flightCharacteristic(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'F_yastadyt', '$null$', val('VFR'), '$null$', '$null$', '$null$', '$null$', []).
flightCharacteristic(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_17', '$null$', '$null$', '$null$', '$null$', val('HOME_BASED'), '$null$', []).
flightCharacteristic(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_19', '$null$', '$null$', val('SAR'), '$null$', '$null$', '$null$', []).

% ridge(Graph, Ridge, Side?, Distance?, Depth?, Annotation*)

% airportHeliport(Graph, AirportHeliport, TimeSlice*)
airportHeliport(g1:'EA_AIP_DS_FULL_20170701.xml', uuid:'dd062d88-3e64-4a5d-bebd-89476db9ebea', [s1:'AHP_EADH', s2:'ID_ACT_11']).

% airportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus?, Warning?, Usage*)
airportHeliportAvailability(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'AHY_EADH_PERMIT', val('NORMAL'), '$null$', [s1:'AHU_EADH_PERMIT']).
airportHeliportAvailability(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_13', val('CLOSED'), '$null$', [s2:'ID_ACT_14']).

% point(Graph, Point)
point(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'elpoint1EADH').

% propertiesWithSchedule(Graph, PropertiesWithSchedule, Annotation*, SpecialDateAuthority*, TimeInterval*)
propertiesWithSchedule(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_13', [], [], []).
propertiesWithSchedule(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_15', [], [], []).
propertiesWithSchedule(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_16', [], [], []).
propertiesWithSchedule(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_18', [], [], []).
propertiesWithSchedule(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'A-a72cfd3a', [s1:'n002'], [], []).
propertiesWithSchedule(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'agtayyat', [], [], []).
propertiesWithSchedule(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'AHY_EADH_PERMIT', [], [], []).
propertiesWithSchedule(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_20', [], [], []).

% altimeterSource(Graph, AltimeterSource, TimeSlice*)

% surface(Graph, Surface, Patch+)

% surfaceContamination(Graph, SurfaceContamination, ObservationTime?, Depth?, FrictionCoefficient?, FrictionEstimation?, FrictionDevice?, ObscuredLights?, FurtherClearanceTime?, FurtherTotalClearance?, NextObservationTime?, Proportion?, CriticalRidge*, Annotation*, Layer*)

% organisationAuthority(Graph, OrganisationAuthority, TimeSlice*)
organisationAuthority(g1:'EA_AIP_DS_FULL_20170701.xml', uuid:'74efb6ba-a52a-46c0-a16b-03860d356882', []).

% surface(Graph, Surface, HorizontalAccuracy?, Annotation*)

% timePeriod(Graph, TimePeriod, BeginPosition, EndPosition)
timePeriod(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'IDE_ACT_23', '$null$', '$null$').
timePeriod(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_12', '$null$', '$null$').
timePeriod(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'IDE_ACT_24', '$null$', '$null$').

% organisationAuthorityTimeSlice(Graph, OrganisationAuthorityTimeSlice, Name?, Designator?, Type?, Military?, Annotation*, Contact*, RelatedOrganisationAuthority*)

% linguisticNote(Graph, LinguisticNote, Note?)
linguisticNote(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'ln002', val('Geoid undulation at ELEV PSN is 9 M')).
linguisticNote(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'ln003', val('Heliport reference point site at heliport: geometric centre of TLOF, Direction and distance from (city): Donlon	downtown, east shore of Donlon river')).

% meteorology(Graph, Meteorology, FlightConditions?, Visibility?, VisibilityInterpretation?, RunwayVisualRange?, RunwayVisualRangeInterpretation?, Annotation*)

% usageCondition(Graph, UsageCondition, Type?, PriorPermission?, Selection?, Annotation*, Contact*)
usageCondition(g1:'EA_AIP_DS_FULL_20170701.xml', s2:'ID_ACT_14', val('PERMIT'), '$null$', s2:'ID_ACT_15', [], []).
usageCondition(g1:'EA_AIP_DS_FULL_20170701.xml', s1:'AHU_EADH_PERMIT', val('PERMIT'), '$null$', s1:'agtayyat', [s1:'annotation1'], []).

