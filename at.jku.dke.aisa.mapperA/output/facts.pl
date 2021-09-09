% :- module(a,[]).

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
:- rdf_register_prefix(fixm,'http://www.aisa-project.eu/vocabulary/fixm_3-0-1_sesar#').
:- rdf_register_prefix(aixm,'http://www.aisa-project.eu/vocabulary/aixm_5-1-1#').
:- rdf_register_prefix(plain,'http://www.aisa-project.eu/vocabulary/plain#').
:- rdf_register_prefix(g1,'https://github.com/aixm/donlon/blob/master/').
:- rdf_register_prefix(graph,'https://github.com/jku-win-dke/aisa/graphs/').
:- rdf_register_prefix(rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_prefix(event,'http://www.aixm.aero/schema/5.1/event#').
:- rdf_register_prefix(rdfs,'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_prefix(xsd,'http://www.w3.org/2001/XMLSchema#').
:- rdf_register_prefix(gml,'http://www.opengis.net/gml/3.2#').
:- rdf_register_prefix(file,'https://www.jena.com/plain#').
:- rdf_register_prefix(sh,'http://www.w3.org/ns/shacl#').
:- rdf_register_prefix(uuid,'uuid:').

/* for prefix handling: declare predicates that have RDF terms as arguments
see: https://www.swi-prolog.org/pldoc/man?predicate=rdf_meta/1
*/
:- rdf_meta
  % fixm_NavigationCapabilities(Graph, NavigationCapabilities, OtherNavigationCapabilities?, PerformanceBasedCode*, NavigationCode*)
  fixm_NavigationCapabilities(r,r,t,t,t)

  % aixm_Note(Graph, Note, PropertyName?, Purpose?, TranslatedNote*)
  ,aixm_Note(r,r,t,t,t)

  % fixm_VerticalRange(Graph, VerticalRange, LowerBound?, UpperBound?)
  ,fixm_VerticalRange(r,r,t,t)

  % plain_Performance(Graph, Performance, RateOfDescent, PerformanceAltitude, RateOfClimb, TrueAirSpeed, FlightIdentification*, MachNumber)
  ,plain_Performance(r,r,t,t,t,t,t,t)

  % aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)
  ,aixm_ElevatedSurface(r,r,t,t,t,t)

  % plain_AircraftType(Graph, AircraftType, Aircraft*, IcaoModelIdentifier)
  ,plain_AircraftType(r,r,t,t)

  % fixm_StandPositionAndTime(Graph, StandPositionAndTime, StandName?, StandTime?, TerminalName?)
  ,fixm_StandPositionAndTime(r,r,t,t,t)

  % aixm_OrganisationAuthorityAssociation(Graph, OrganisationAuthorityAssociation, Type?, Annotation*, TheOrganisationAuthority)
  ,aixm_OrganisationAuthorityAssociation(r,r,t,t,t)

  % fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel?)
  ,fixm_EfplPoint4D(r,r,t)

  % fixm_AircraftOperator(Graph, AircraftOperator, OperatingOrganization?, OperatorCategory?)
  ,fixm_AircraftOperator(r,r,t,t)

  % fixm_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair)
  ,fixm_EfplTrajectoryRoutePair(r,r)

  % fixm_RoutePoint(Graph, RoutePoint, Constraint*)
  ,fixm_RoutePoint(r,r,t)

  % fixm_BeaconCodeAssignment(Graph, BeaconCodeAssignment, CurrentBeaconCode?, PreviousBeaconCode?, ReassignedBeaconCode?, ReassigningUnit?)
  ,fixm_BeaconCodeAssignment(r,r,t,t,t,t)

  % fixm_RouteConstraintOrPreference(Graph, RouteConstraintOrPreference, ConstraintType?)
  ,fixm_RouteConstraintOrPreference(r,r,t)

  % plain_FlightLevel(Graph, FlightLevel, Unit, EfplPoint4D+, Level)
  ,plain_FlightLevel(r,r,t,t,t)

  % fixm_EstimatedElapsedTime(Graph, EstimatedElapsedTime, ElapsedTime?, Location?)
  ,fixm_EstimatedElapsedTime(r,r,t,t)

  % fixm_ReportedTime(Graph, ReportedTime, Provenance?, Time?)
  ,fixm_ReportedTime(r,r,t,t)

  % fixm_GeographicLocation(Graph, GeographicLocation, Pos*, SrsName?)
  ,fixm_GeographicLocation(r,r,t,t)

  % aixm_LinguisticNote(Graph, LinguisticNote, Note?)
  ,aixm_LinguisticNote(r,r,t)

  % fixm_PointRange(Graph, PointRange, LateralRange?, VerticalRange?, TemporalRange?)
  ,fixm_PointRange(r,r,t,t,t)

  % aixm_City(Graph, City, Name?, Annotation*)
  ,aixm_City(r,r,t,t)

  % fixm_AirspeedRange(Graph, AirspeedRange, LowerSpeed?, UpperSpeed?)
  ,fixm_AirspeedRange(r,r,t,t)

  % plain_AirspaceVolume(Graph, AirspaceVolume, MaximumLimit, MaximumLimitReference, Width, MinimumLimit, UpperLimit, LowerLimit, AirspaceGeometryComponent+, UpperLimitReference, LowerLimitReference, MinimumLimitReference)
  ,plain_AirspaceVolume(r,r,t,t,t,t,t,t,t,t,t,t)

  % fixm_RankedTrajectory(Graph, RankedTrajectory, Identifier?, MaximumAcceptableDelay?, AssignedIndicator?, RouteTrajectoryPair?)
  ,fixm_RankedTrajectory(r,r,t,t,t,t)

  % fixm_TrajectoryPointRole(Graph, TrajectoryPointRole, BottomOfClimb?, BottomOfDescent?, BoundaryPoint?, FromGATToOAT?, FromIFRToVFR?, FromOATToGat?, FromVFRToIFR?, TopOfClimb?, TopOfDescent?)
  ,fixm_TrajectoryPointRole(r,r,t,t,t,t,t,t,t,t,t)

  % fixm_CommunicationCapabilities(Graph, CommunicationCapabilities, OtherCommunicationCapabilities?, OtherDataLinkCapabilities?, DataLinkCode*, SelectiveCallingCode?, CommunicationCode*)
  ,fixm_CommunicationCapabilities(r,r,t,t,t,t,t)

  % fixm_Dinghy(Graph, Dinghy, Quantity?, TotalCapacity?, Covered?, Colour?)
  ,fixm_Dinghy(r,r,t,t,t,t)

  % aixm_ContactInformation(Graph, ContactInformation, Name?, Title?, Annotation*, NetworkNode*, Address*, PhoneFax*)
  ,aixm_ContactInformation(r,r,t,t,t,t,t,t)

  % fixm_DangerousGoods(Graph, DangerousGoods, GuidebookNumber?, OnboardLocation?, HandlingInformation?, AircraftLimitation?, AirWayBill?, Shipment?, PackageGroup*, ShippingInformation?)
  ,fixm_DangerousGoods(r,r,t,t,t,t,t,t,t,t)

  % fixm_Handoff(Graph, Handoff, ReceivingUnit?, TransferringUnit?, CoordinationStatus?)
  ,fixm_Handoff(r,r,t,t,t)

  % plain_AerodromesOfDestination(Graph, AerodromesOfDestination, EfplFlight*)
  ,plain_AerodromesOfDestination(r,r,t)

  % fixm_ContactInformation(Graph, ContactInformation, Name?, Title?, OnlineContact?, PhoneFax?, Address?)
  ,fixm_ContactInformation(r,r,t,t,t,t,t)

  % aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator?, Name?, LocationIndicatorICAO?, DesignatorIATA?, Type?, CertifiedICAO?, PrivateUse?, ControlType?, FieldElevation?, FieldElevationAccuracy?, VerticalDatum?, MagneticVariation?, MagneticVariationAccuracy?, DateMagneticVariation?, MagneticVariationChange?, ReferenceTemperature?, AltimeterCheckLocation?, SecondaryPowerSupply?, WindDirectionIndicator?, LandingDirectionIndicator?, TransitionAltitude?, TransitionLevel?, LowestTemperature?, Abandoned?, CertificationDate?, CertificationExpirationDate?, Contact*, Annotation*, ARP?, AltimeterSource*, Contaminant*, ServedCity*, ResponsibleOrganisation?, AviationBoundary?, Availability*)
  ,aixm_AirportHeliportTimeSlice(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % aixm_Ridge(Graph, Ridge, Side?, Distance?, Depth?, Annotation*)
  ,aixm_Ridge(r,r,t,t,t,t)

  % plain_ElevatedPoint(Graph, ElevatedPoint, Navaid*, Elevation, AirportHeliport+)
  ,plain_ElevatedPoint(r,r,t,t,t)

  % fixm_EnRouteDiversion(Graph, EnRouteDiversion, DiversionRecoveryInformation?)
  ,fixm_EnRouteDiversion(r,r,t)

  % plain_SectorExitConditions(Graph, SectorExitConditions, FlightIdentification*, ExitFL)
  ,plain_SectorExitConditions(r,r,t,t)

  % plain_AircraftPosition(Graph, AircraftPosition, FlightLevel, ActualSpeed, Course, Longitude, Altitude, Track, Latitude, Speed, FlightIdentification*, SrsName, Position, PositionTime)
  ,plain_AircraftPosition(r,r,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_ActualSpeed(Graph, ActualSpeed, Calculated?, PilotReported?, Surveillance?)
  ,fixm_ActualSpeed(r,r,t,t,t)

  % plain_FuturePosition(Graph, FuturePosition, TimeStamp, Probability, PredictedAltitude, FlightIdentification+, Latitude, Longitude, WaypointName)
  ,plain_FuturePosition(r,r,t,t,t,t,t,t,t)

  % fixm_FlightEmergency(Graph, FlightEmergency, ActionTaken?, EmergencyDescription?, Originator?, OtherInformation?, Phase?, Contact?)
  ,fixm_FlightEmergency(r,r,t,t,t,t,t,t)

  % aixm_PropertiesWithSchedule(Graph, PropertiesWithSchedule, Annotation*, SpecialDateAuthority*, TimeInterval*)
  ,aixm_PropertiesWithSchedule(r,r,t,t,t)

  % fixm_TrajectoryRoutePair(Graph, TrajectoryRoutePair, Trajectory?, Route?)
  ,fixm_TrajectoryRoutePair(r,r,t,t)

  % fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit?, UpstreamUnit?, BoundaryCrossingProposed?, BoundaryCrossingCoordinated?, Handoff?, UnitBoundaryIndicator?)
  ,fixm_UnitBoundary(r,r,t,t,t,t,t,t)

  % plain_List(Graph, List, Assumed, FlightIdentification*)
  ,plain_List(r,r,t,t)

  % aixm_SurfaceContamination(Graph, SurfaceContamination, ObservationTime?, Depth?, FrictionCoefficient?, FrictionEstimation?, FrictionDevice?, ObscuredLights?, FurtherClearanceTime?, FurtherTotalClearance?, NextObservationTime?, Proportion?, CriticalRidge*, Annotation*, Layer*)
  ,aixm_SurfaceContamination(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_MeteorologicalData(Graph, MeteorologicalData, Temperature?, WindDirection?, WindSpeed?)
  ,fixm_MeteorologicalData(r,r,t,t,t)

  % aixm_OrganisationAuthority(Graph, OrganisationAuthority, TimeSlice*)
  ,aixm_OrganisationAuthority(r,r,t)

  % fixm_TelephoneContact(Graph, TelephoneContact, Voice?, Facimile?)
  ,fixm_TelephoneContact(r,r,t,t)

  % plain_DME(Graph, DME, Frequency, NavaidEquipment*, Type)
  ,plain_DME(r,r,t,t,t)

  % plain_Weather(Graph, Weather, WindDirection, Temperature, Precipitation, WindSpeed, Thunder, Altitude*, Icing, CloudCoverage)
  ,plain_Weather(r,r,t,t,t,t,t,t,t,t)

  % plain_Point(Graph, Point, SignificantPoint+, HorizontalAccuracy)
  ,plain_Point(r,r,t,t)

  % aixm_AirportHeliportContamination(Graph, AirportHeliportContamination)
  ,aixm_AirportHeliportContamination(r,r)

  % plain_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus, AirportHeliport+, Warning)
  ,plain_AirportHeliportAvailability(r,r,t,t,t)

  % fixm_OtherInformation(Graph, OtherInformation, ReplacementFlightPlanIndicator?, RunwayVisualRange?)
  ,fixm_OtherInformation(r,r,t,t)

  % aixm_TelephoneContact(Graph, TelephoneContact, Voice?, Facsimile?)
  ,aixm_TelephoneContact(r,r,t,t)

  % plain_SignificantPointInAirspace(Graph, SignificantPointInAirspace, Type, RelativeLocation)
  ,plain_SignificantPointInAirspace(r,r,t,t)

  % plain_NavaidComponent(Graph, NavaidComponent, CollocationGroup, Navaid*)
  ,plain_NavaidComponent(r,r,t,t)

  % fixm_Originator(Graph, Originator)
  ,fixm_Originator(r,r)

  % fixm_FlightStatus(Graph, FlightStatus, AirborneHold?, Airfile?, Accepted?, FlightCycle?, MissedApproach?, Suspended?)
  ,fixm_FlightStatus(r,r,t,t,t,t,t,t)

  % fixm_IdentifiedUnitReference(Graph, IdentifiedUnitReference, UnitIdentifier?)
  ,fixm_IdentifiedUnitReference(r,r,t)

  % plain_AirspaceGeometryComponent(Graph, AirspaceGeometryComponent, Airspace+)
  ,plain_AirspaceGeometryComponent(r,r,t)

  % fixm_Radionuclide(Graph, Radionuclide, PhysicalChemicalForm?, RadionuclideId?, RadionuclideName?, LowDispersibleMaterialIndicator?, Activity?, SpecialFormIndicator?)
  ,fixm_Radionuclide(r,r,t,t,t,t,t,t)

  % aixm_OnlineContact(Graph, OnlineContact, Network?, Linkage?, Protocol?, EMail?)
  ,aixm_OnlineContact(r,r,t,t,t,t)

  % plain_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, TrajectoryPointType, Trajectory*)
  ,plain_EfplTrajectoryPoint(r,r,t,t)

  % fixm_StructuredPostalAddress(Graph, StructuredPostalAddress)
  ,fixm_StructuredPostalAddress(r,r)

  % aixm_AirportHeliportUsage(Graph, AirportHeliportUsage, Operation?)
  ,aixm_AirportHeliportUsage(r,r,t)

  % plain_Meteorology(Graph, Meteorology, ConditionCombination+, RunwayVisualRangeInterpretation, Visibility, RunwayVisualRange, VisibilityInterpretation, FlightConditions)
  ,plain_Meteorology(r,r,t,t,t,t,t,t)

  % gml_SurfacePatch(Graph, SurfacePatch)
  ,gml_SurfacePatch(r,r)

  % aixm_FlightCharacteristic(Graph, FlightCharacteristic, Type?, Rule?, Status?, Military?, Origin?, Purpose?, Annotation*)
  ,aixm_FlightCharacteristic(r,r,t,t,t,t,t,t,t)

  % aixm_AirportHeliport(Graph, AirportHeliport, TimeSlice*)
  ,aixm_AirportHeliport(r,r,t)

  % fixm_TrajectoryPoint(Graph, TrajectoryPoint, AltimeterSetting?, PredictedAirspeed?, PredictedGroundspeed?, MetData?, Point?, TrajectoryChange*, TrajectoryChangeType*, ReferencePoint?)
  ,fixm_TrajectoryPoint(r,r,t,t,t,t,t,t,t,t)

  % fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier?, DistanceFromTakeOff?, EfplEstimatedSpeed?, ElapsedTime?, GrossWeight?, TrajectoryPointType?, TrajectoryPointRole?, InboundSegment?)
  ,fixm_EfplTrajectoryPoint(r,r,t,t,t,t,t,t,t,t)

  % fixm_RunwayPositionAndTime(Graph, RunwayPositionAndTime, RunwayName?, RunwayTime?)
  ,fixm_RunwayPositionAndTime(r,r,t,t)

  % fixm_Feature(Graph, Feature, Provenance?)
  ,fixm_Feature(r,r,t)

  % fixm_ElapsedTimeLocation(Graph, ElapsedTimeLocation)
  ,fixm_ElapsedTimeLocation(r,r)

  % plain_EfplPoint4D(Graph, EfplPoint4D, EfplTrajectoryPoint*, FlightLevel, Time, SrsName, Pos)
  ,plain_EfplPoint4D(r,r,t,t,t,t,t)

  % aixm_Surface(Graph, Surface, HorizontalAccuracy?, Annotation*)
  ,aixm_Surface(r,r,t,t)

  % gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition)
  ,gml_TimePeriod(r,r,t,t)

  % fixm_AircraftCapabilities(Graph, AircraftCapabilities, Survival?, Communication?, Navigation?, Surveillance?, StandardCapabilities?)
  ,fixm_AircraftCapabilities(r,r,t,t,t,t,t)

  % fixm_EnRoute(Graph, EnRoute, AlternateAerodrome*, FleetPrioritization?, BoundaryCrossings*, CpdlcConnection?, BeaconCodeAssignment?, Cleared?, ControlElement*, Pointout?, Position?)
  ,fixm_EnRoute(r,r,t,t,t,t,t,t,t,t,t)

  % fixm_TemporalRange(Graph, TemporalRange, Earliest?, Latest?)
  ,fixm_TemporalRange(r,r,t,t)

  % plain_City(Graph, City, AirportHeliport+, Name)
  ,plain_City(r,r,t,t)

  % fixm_Aircraft(Graph, Aircraft, AircraftColours?, AircraftQuantity?, EngineType?, AircraftAddress?, Capabilities?, Registration?, AircraftType?, WakeTurbulence?, AircraftPerformance?)
  ,fixm_Aircraft(r,r,t,t,t,t,t,t,t,t,t)

  % fixm_OnlineContact(Graph, OnlineContact, Email?)
  ,fixm_OnlineContact(r,r,t)

  % fixm_TimeSequence(Graph, TimeSequence, Approval?, Begin?, End?, Ready?, Request?)
  ,fixm_TimeSequence(r,r,t,t,t,t,t)

  % fixm_AdditionalHandlingInformation(Graph, AdditionalHandlingInformation, ResponsibleAgent?)
  ,fixm_AdditionalHandlingInformation(r,r,t)

  % fixm_Extension(Graph, Extension)
  ,fixm_Extension(r,r)

  % fixm_SurveillanceCapabilities(Graph, SurveillanceCapabilities, OtherSurveillanceCapabilities?, SurveillanceCode*)
  ,fixm_SurveillanceCapabilities(r,r,t,t)

  % fixm_Trajectory(Graph, Trajectory, TrajectoryPoint*)
  ,fixm_Trajectory(r,r,t)

  % aixm_AltimeterSourceTimeSlice(Graph, AltimeterSourceTimeSlice, IsRemote?, IsPrimary?, Availability*, Annotation*)
  ,aixm_AltimeterSourceTimeSlice(r,r,t,t,t,t)

  % aixm_PostalAddress(Graph, PostalAddress, DeliveryPoint?, City?, AdministrativeArea?, PostalCode?, Country?)
  ,aixm_PostalAddress(r,r,t,t,t,t,t)

  % plain_AircraftOperator(Graph, AircraftOperator, EfplFlight*, OperatorCategory)
  ,plain_AircraftOperator(r,r,t,t)

  % fixm_LastPositionReport(Graph, LastPositionReport, DeterminationMethod?, Position?, TimeAtPosition?)
  ,fixm_LastPositionReport(r,r,t,t,t)

  % aixm_AltimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus?)
  ,aixm_AltimeterSourceStatus(r,r,t)

  % fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules?)
  ,fixm_EfplRoute(r,r,t)

  % fixm_CoordinationStatus(Graph, CoordinationStatus, AbrogationReason?, CoordinationStatus?, NonStandardCommunicationReason?, ReleaseConditions?)
  ,fixm_CoordinationStatus(r,r,t,t,t,t)

  % fixm_BoundaryCrossing(Graph, BoundaryCrossing, Altitude?, CrossingPoint?, CrossingSpeed?, CrossingTime?, Offtrack?, AltitudeInTransition?)
  ,fixm_BoundaryCrossing(r,r,t,t,t,t,t,t)

  % plain_AirportHeliport(Graph, AirportHeliport, Navaid*, TransitionLevel, Name, Designator, DesignatorIATA, MagneticVariation, LocationIndicatorICAO, FieldElevation, TransitionAltitude)
  ,plain_AirportHeliport(r,r,t,t,t,t,t,t,t,t,t)

  % fixm_RadioCommunicationFailure(Graph, RadioCommunicationFailure, RadioFailureRemarks?, RemainingComCapability?, Contact?)
  ,fixm_RadioCommunicationFailure(r,r,t,t,t)

  % aixm_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus?, Warning?, Usage*)
  ,aixm_AirportHeliportAvailability(r,r,t,t,t)

  % fixm_RadioactiveMaterial(Graph, RadioactiveMaterial, CriticalitySafetyIndex?, TransportIndex?, FissileExceptedIndicator?, Category?, Radionuclide?)
  ,fixm_RadioactiveMaterial(r,r,t,t,t,t,t)

  % fixm_ControlElement(Graph, ControlElement)
  ,fixm_ControlElement(r,r)

  % fixm_AllPackedInOne(Graph, AllPackedInOne, NumberOfPackages?, QValue?)
  ,fixm_AllPackedInOne(r,r,t,t)

  % aixm_AltimeterSource(Graph, AltimeterSource, TimeSlice*)
  ,aixm_AltimeterSource(r,r,t)

  % plain_DesignatedPoint(Graph, DesignatedPoint, Name, Type, Designator, SignificantPoint*)
  ,plain_DesignatedPoint(r,r,t,t,t,t)

  % fixm_DirectRouting(Graph, DirectRouting, From?, To?)
  ,fixm_DirectRouting(r,r,t,t)

  % fixm_TargetMultiTime(Graph, TargetMultiTime, Target?)
  ,fixm_TargetMultiTime(r,r,t)

  % plain_AircraftPairs(Graph, AircraftPairs, VarAltitude, MinimumDistance, ProbabilityOfSI, ConflictCode, Screening, FlightIdentification+, DistanceToMD, SituationOfInterest, Conflict, VarVerticalRate, TimeToMD, VarGS)
  ,plain_AircraftPairs(r,r,t,t,t,t,t,t,t,t,t,t,t,t)

  % plain_EfplRoute(Graph, EfplRoute, RouteText, EfplFlightRules, EfplTrajectoryRoutePair*)
  ,plain_EfplRoute(r,r,t,t,t)

  % fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome?, DepartureFix?, DepartureFixTime?, DepartureFleetPrioritization?, DepartureSlot?, EarliestOffBlockTime?, OffBlockReadyTime?, RunwayPositionAndTime?, StandardInstrumentDeparture?, StandPositionAndTime?, TakeoffAlternateAerodrome*, TakeoffWeight?, DepartureTimes?)
  ,fixm_FlightDeparture(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_AerodromeReference(Graph, AerodromeReference)
  ,fixm_AerodromeReference(r,r)

  % fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime?, TaxiTime?)
  ,fixm_EfplFlightDeparture(r,r,t,t)

  % fixm_PostalAddress(Graph, PostalAddress, AdministrativeArea?, PostalCode?, DeliveryPoint?, CountryCode?, CountryName?, City?)
  ,fixm_PostalAddress(r,r,t,t,t,t,t,t)

  % fixm_GroundspeedRange(Graph, GroundspeedRange, LowerSpeed?, UpperSpeed?)
  ,fixm_GroundspeedRange(r,r,t,t)

  % fixm_Pointout(Graph, Pointout, OriginatingUnit?, ReceivingUnit*)
  ,fixm_Pointout(r,r,t,t)

  % fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel?, EstimatedTime?, Constraint*)
  ,fixm_ExpandedRoutePoint(r,r,t,t,t)

  % plain_Aircraft(Graph, Aircraft, EfplFlight*, AircraftAddress, AircraftPerformance, Registration, AircraftQuantity, WakeTurbulence)
  ,plain_Aircraft(r,r,t,t,t,t,t,t)

  % fixm_Dimensions(Graph, Dimensions, Height?, Length?, Width?)
  ,fixm_Dimensions(r,r,t,t,t)

  % fixm_RouteSegment(Graph, RouteSegment, Airway?, RoutePoint?)
  ,fixm_RouteSegment(r,r,t,t)

  % aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator?, Flight*, Aircraft*, Weather*, SubCondition*)
  ,aixm_ConditionCombination(r,r,t,t,t,t,t)

  % aixm_SurfaceContaminationLayer(Graph, SurfaceContaminationLayer, LayerOrder?, Type?, Extent*, Annotation*)
  ,aixm_SurfaceContaminationLayer(r,r,t,t,t,t)

  % plain_Altitude(Graph, Altitude, FlightLevel, AltitudeInterpretation, Route+)
  ,plain_Altitude(r,r,t,t,t)

  % fixm_Organization(Graph, Organization, Name?, OtherOrganization?, Contact?)
  ,fixm_Organization(r,r,t,t,t)

  % aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)
  ,aixm_ElevatedPoint(r,r,t,t,t,t)

  % gml_Point(Graph, Point)
  ,gml_Point(r,r)

  % plain_ConditionCombination(Graph, ConditionCombination, UsageCondition*)
  ,plain_ConditionCombination(r,r,t)

  % plain_AircraftAction(Graph, AircraftAction, ExecutionOfRequests, InitialCall, FrequencyTransfer, FlightIdentification+)
  ,plain_AircraftAction(r,r,t,t,t,t)

  % plain_Trajectory(Graph, Trajectory, EfplTrajectoryRoutePair*)
  ,plain_Trajectory(r,r,t)

  % fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile*, DescentProfile*)
  ,fixm_FlightPerformanceData(r,r,t,t)

  % fixm_ExpandedRoute(Graph, ExpandedRoute, RoutePoint*)
  ,fixm_ExpandedRoute(r,r,t)

  % plain_DirectRouting(Graph, DirectRouting, From, ClearedFlightInformation*, To)
  ,plain_DirectRouting(r,r,t,t,t)

  % plain_CoordinationStatus(Graph, CoordinationStatus, ReleaseConditions, NonStandardCommunicationReason, CoordinationStatus, AbrogationReason)
  ,plain_CoordinationStatus(r,r,t,t,t,t)

  % fixm_DeclarationText(Graph, DeclarationText, Compliance?, Consignor?, Shipper?)
  ,fixm_DeclarationText(r,r,t,t,t)

  % aixm_Meteorology(Graph, Meteorology, FlightConditions?, Visibility?, VisibilityInterpretation?, RunwayVisualRange?, RunwayVisualRangeInterpretation?, Annotation*)
  ,aixm_Meteorology(r,r,t,t,t,t,t,t)

  % aixm_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role?, TheOrganisationAuthority)
  ,aixm_AirportHeliportResponsibilityOrganisation(r,r,t,t)

  % plain_Organization(Graph, Organization, AircraftOperator*, Name)
  ,plain_Organization(r,r,t,t)

  % fixm_PlannedReportingPosition(Graph, PlannedReportingPosition, Position?, PositionAltitude?, PositionEstimatedTime?)
  ,fixm_PlannedReportingPosition(r,r,t,t,t)

  % fixm_SignificantPoint(Graph, SignificantPoint)
  ,fixm_SignificantPoint(r,r)

  % fixm_SupplementalData(Graph, SupplementalData, FuelEndurance?, PersonsOnBoard?, PilotInCommand?)
  ,fixm_SupplementalData(r,r,t,t,t)

  % plain_Datalink(Graph, Datalink, EquipmentDegradation, InitialCall, FlightIdentification+, FrequencyChange, AssumedBySkyguide)
  ,plain_Datalink(r,r,t,t,t,t,t)

  % plain_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role, AirportHeliport*)
  ,plain_AirportHeliportResponsibilityOrganisation(r,r,t,t)

  % fixm_DangerousGoodsPackageGroup(Graph, DangerousGoodsPackageGroup, ShipmentDimensions?, DangerousGoodsPackage*, ShipmentUseIndicator?)
  ,fixm_DangerousGoodsPackageGroup(r,r,t,t,t)

  % plain_Time(Graph, Time, SequenceNumber, FlightIdentification+, TimeOfPrediction)
  ,plain_Time(r,r,t,t,t)

  % fixm_OfftrackDistance(Graph, OfftrackDistance, Distance?, Direction?)
  ,fixm_OfftrackDistance(r,r,t,t)

  % plain_Navaid(Graph, Navaid, Purpose, Type, SignificantPoint*, Designator, FlightChecked, Name)
  ,plain_Navaid(r,r,t,t,t,t,t,t)

  % fixm_TrajectoryChange(Graph, TrajectoryChange, ConstrainedAirspace?, SpecialActivityAirspace?)
  ,fixm_TrajectoryChange(r,r,t,t)

  % fixm_Point4D(Graph, Point4D, Altitude?, Time?, PointRange?)
  ,fixm_Point4D(r,r,t,t,t)

  % fixm_AbstractRoutePoint(Graph, AbstractRoutePoint, AirTrafficType?, DelayAtPoint?, FlightRules?, Point?, ClearanceLimit?)
  ,fixm_AbstractRoutePoint(r,r,t,t,t,t,t)

  % fixm_DepartureActivityTimes(Graph, DepartureActivityTimes, BoardingTime?, DeIcingTime?, GroundHandlingTime?, StartupTime?)
  ,fixm_DepartureActivityTimes(r,r,t,t,t,t)

  % plain_Waypoints(Graph, Waypoints, Longitude, Latitude, WaypointName, Route+)
  ,plain_Waypoints(r,r,t,t,t,t)

  % fixm_Flight(Graph, Flight, ControllingUnit?, Extensions*, FlightFiler?, Gufi?, Remarks?, AircraftDescription?, DangerousGoods*, RankedTrajectories*, RouteToRevisedDestination?, Negotiating?, Agreed?, Arrival?, Departure?, Emergency?, RadioCommunicationFailure?, EnRoute?, Operator?, EnRouteDiversion?, FlightType?, FlightStatus?, Originator?, SupplementalData?, FlightIdentification?, SpecialHandling*)
  ,fixm_Flight(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % gml_Surface(Graph, Surface, Patch+)
  ,gml_Surface(r,r,t)

  % fixm_ClearedFlightInformation(Graph, ClearedFlightInformation, ClearedFlightLevel?, ClearedSpeed?, Heading?, OfftrackClearance?, RateOfClimbDescend?, DirectRouting?)
  ,fixm_ClearedFlightInformation(r,r,t,t,t,t,t,t)

  % plain_Route(Graph, Route, Eastbound, RequestedAltitude, AtsRoute, Westbound, Routing, Southbound, Remarks, Northbound, CoordinationPoint, FlightIdentification+, RouteText)
  ,plain_Route(r,r,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_ShippingInformation(Graph, ShippingInformation, AerodromeOfLoading?, AerodromeOfUnloading?, DangerousGoodsScreeningLocation?, DepartureCountry?, DestinationCountry?, OriginCountry?, ShipmentAuthorizations?, SubsidiaryHazardClassAndDivision?, SupplementaryInformation?, TransferAerodromes*, DeclarationText?, Consignee?, Shipper?)
  ,fixm_ShippingInformation(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_DinghyColour(Graph, DinghyColour)
  ,fixm_DinghyColour(r,r)

  % fixm_CpdlcConnection(Graph, CpdlcConnection, ReceivingUnitFrequency?, AtnLogonParameters?, SendCpldcIndicator?, ConnectionStatus?, FrequencyUsage?, Fans1ALogonParameters?)
  ,fixm_CpdlcConnection(r,r,t,t,t,t,t,t)

  % fixm_Route(Graph, Route, AirfileRouteStartTime?, FlightDuration?, InitialCruisingSpeed?, InitialFlightRules?, RequestedAltitude?, RouteText?, EstimatedElapsedTime*, ExpandedRoute?, ClimbSchedule?, DescentSchedule?, Segment*)
  ,fixm_Route(r,r,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_Person(Graph, Person, Name?, Contact?)
  ,fixm_Person(r,r,t,t)

  % plain_EfplFlight(Graph, EfplFlight, FlightType)
  ,plain_EfplFlight(r,r,t)

  % fixm_EfplFlight(Graph, EfplFlight, IfplId?, TotalEstimatedElapsedTime?, AerodromesOfDestination?, EfplSpecialHandling?, EfplFiledTrajectory?, EfplAcceptedTrajectory?, OtherInformation?, FlightPerformanceData?)
  ,fixm_EfplFlight(r,r,t,t,t,t,t,t,t,t)

  % plain_TrafficSituation(Graph, TrafficSituation, Sector, FlightIdentification+)
  ,plain_TrafficSituation(r,r,t,t)

  % plain_IcaoAerodromeReference(Graph, IcaoAerodromeReference, EfplFlightDeparture*, AerodromesOfDestination*, Code)
  ,plain_IcaoAerodromeReference(r,r,t,t,t)

  % plain_ContactInformation(Graph, ContactInformation, UsageCondition*, Title, Name)
  ,plain_ContactInformation(r,r,t,t,t)

  % plain_string(Graph, String, WhiteSpace)
  ,plain_string(r,r,t)

  % plain_ClearedFlightInformation(Graph, ClearedFlightInformation, ClearedFlightLevel, FlightIdentification*, RateOfClimbDescent, OfftrackClearance, ClearedSpeed, Heading)
  ,plain_ClearedFlightInformation(r,r,t,t,t,t,t,t)

  % fixm_AircraftPosition(Graph, AircraftPosition, Altitude?, Position?, PositionTime?, Track?, ActualSpeed?, NextPosition?, ReportSource?, FollowingPosition?)
  ,fixm_AircraftPosition(r,r,t,t,t,t,t,t,t,t)

  % plain_NavaidEquipment(Graph, NavaidEquipment, Mobile, FlightChecked, Designator, NavaidComponent*, Name)
  ,plain_NavaidEquipment(r,r,t,t,t,t,t)

  % plain_Airspace(Graph, Airspace, Name, ControlType, Type, LocalType, UpperLowerSeparation, Designator, SignificantPointInAirspace+)
  ,plain_Airspace(r,r,t,t,t,t,t,t,t)

  % aixm_Timesheet(Graph, Timesheet, TimeReference?, StartDate?, EndDate?, Day?, DayTil?, StartTime?, StartEvent?, StartTimeRelativeEvent?, StartEventInterpretation?, EndTime?, EndEvent?, EndTimeRelativeEvent?, EndEventInterpretation?, DaylightSavingAdjust?, Excluded?, Annotation*)
  ,aixm_Timesheet(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % plain_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair, EfplFlight*)
  ,plain_EfplTrajectoryRoutePair(r,r,t)

  % fixm_MultiTime(Graph, MultiTime, Actual?, Estimated?)
  ,fixm_MultiTime(r,r,t,t)

  % fixm_Provenance(Graph, Provenance, Timestamp?, Centre?, Source?, System?)
  ,fixm_Provenance(r,r,t,t,t,t)

  % fixm_Temperatures(Graph, Temperatures, ControlTemperature?, EmergencyTemperature?, FlashpointTemperature?)
  ,fixm_Temperatures(r,r,t,t,t)

  % fixm_TrajectorySegment(Graph, TrajectorySegment, SegmentIdentifier?, SegmentType?)
  ,fixm_TrajectorySegment(r,r,t,t)

  % fixm_FlightIdentification(Graph, FlightIdentification, AircraftIdentification?, MajorCarrierIdentifier?, MarketingCarrierFlightIdentifier*)
  ,fixm_FlightIdentification(r,r,t,t,t)

  % fixm_LastContact(Graph, LastContact, ContactFrequency?, LastContactTime?, LastContactUnit?, Position?)
  ,fixm_LastContact(r,r,t,t,t,t)

  % plain_VOR(Graph, VOR, Type, Frequency, NavaidEquipment*)
  ,plain_VOR(r,r,t,t,t)

  % plain_AircraftCharacteristic(Graph, AircraftCharacteristic, VerticalSeparationCapability, Weight, AircraftLandingCategory, ConditionCombination*, Speed, WakeTurbulence, TypeAircraftICAO)
  ,plain_AircraftCharacteristic(r,r,t,t,t,t,t,t,t)

  % plain_EfplFlightDeparture(Graph, EfplFlightDeparture, EfplFlight*, EstimatedOffBlockTime)
  ,plain_EfplFlightDeparture(r,r,t,t)

  % fixm_SpeedSchedule(Graph, SpeedSchedule, InitialSpeed?, SubsequentSpeed?)
  ,fixm_SpeedSchedule(r,r,t,t)

  % aixm_OrganisationAuthorityTimeSlice(Graph, OrganisationAuthorityTimeSlice, Name?, Designator?, Type?, Military?, Annotation*, Contact*, RelatedOrganisationAuthority*)
  ,aixm_OrganisationAuthorityTimeSlice(r,r,t,t,t,t,t,t,t)

  % plain_AirportHeliportUsage(Graph, AirportHeliportUsage, AirportHeliportAvailability*, Operation)
  ,plain_AirportHeliportUsage(r,r,t,t)

  % fixm_FlightLevel(Graph, FlightLevel, Level?, Unit?)
  ,fixm_FlightLevel(r,r,t,t)

  % plain_VerticalRate(Graph, VerticalRate, VerticalRate, FlightIdentification*)
  ,plain_VerticalRate(r,r,t,t)

  % fixm_LateralOfftrack(Graph, LateralOfftrack, OfftrackDistance*, OfftrackReason?)
  ,fixm_LateralOfftrack(r,r,t,t)

  % fixm_AirspaceConstraint(Graph, AirspaceConstraint, AirspaceControlledEntryTime?, ConstrainedAirspace?)
  ,fixm_AirspaceConstraint(r,r,t,t)

  % plain_SignificantPoint(Graph, SignificantPoint, SignificantPointInAirspace+)
  ,plain_SignificantPoint(r,r,t)

  % fixm_AtcUnitReference(Graph, AtcUnitReference, SectorIdentifier?, Delegated?)
  ,fixm_AtcUnitReference(r,r,t,t)

  % aixm_Point(Graph, Point, HorizontalAccuracy?, Annotation*)
  ,aixm_Point(r,r,t,t)

  % aixm_AircraftCharacteristic(Graph, AircraftCharacteristic, Type?, Engine?, NumberEngine?, TypeAircraftICAO?, AircraftLandingCategory?, WingSpan?, WingSpanInterpretation?, ClassWingSpan?, Weight?, WeightInterpretation?, Passengers?, PassengersInterpretation?, Speed?, SpeedInterpretation?, WakeTurbulence?, NavigationEquipment?, NavigationSpecification?, VerticalSeparationCapability?, AntiCollisionAndSeparationEquipment?, CommunicationEquipment?, SurveillanceEquipment?, Annotation*)
  ,aixm_AircraftCharacteristic(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_DangerousGoodsPackage(Graph, DangerousGoodsPackage, DangerousGoodsQuantity?, PackageDimensions?, PackingInstructionNumber?, ProductName?, ProperShippingName?, ReportableQuantity?, SupplementaryInformation?, TechnicalName?, TypeOfPackaging?, UnNumber?, DangerousGoodsLimitation?, ShipmentType?, AllPackedInOne?, CompatibilityGroup?, ShipmentDimensions?, MarinePollutantIndicator?, RadioactiveMaterials?, HazardClass?, PackingGroup?, Temperatures?, OverpackIndicator?, SubsidiaryHazardClass*)
  ,fixm_DangerousGoodsPackage(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_DangerousGoodsDimensions(Graph, DangerousGoodsDimensions, GrossWeight?, NetWeight?, Volume?)
  ,fixm_DangerousGoodsDimensions(r,r,t,t,t)

  % plain_Timesheet(Graph, Timesheet, Excluded, EndTime, AirspaceActivation*, Day, DaylightSavingAdjust, EndDate, TimeReference, StartDate, StartTime)
  ,plain_Timesheet(r,r,t,t,t,t,t,t,t,t,t)

  % plain_AirspaceActivation(Graph, AirspaceActivation, Airspace*, Status, Activity)
  ,plain_AirspaceActivation(r,r,t,t,t)

  % fixm_IcaoAerodromeReference(Graph, IcaoAerodromeReference, Code?)
  ,fixm_IcaoAerodromeReference(r,r,t)

  % fixm_FlightArrival(Graph, FlightArrival, ApproachFix?, ApproachTime?, ArrivalAerodrome?, ArrivalAerodromeAlternate*, ArrivalAerodromeOriginal?, ArrivalFix?, ArrivalFixTime?, ArrivalFleetPrioritization?, ArrivalSequenceNumber?, EarliestInBlockTime?, FiledRevisedDestinationAerodrome?, FiledRevisedDestinationStar?, RunwayPositionAndTime?, StandardInstrumentArrival?, StandPositionAndTime?, LandingLimits?)
  ,fixm_FlightArrival(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled?, Initial?)
  ,fixm_ExtendedMultiTime(r,r,t,t)

  % fixm_AerodromesOfDestination(Graph, AerodromesOfDestination, AerodromeOfDestination?, Alternate1?, Alternate2?, FiledRevisedDestinationAerodrome?)
  ,fixm_AerodromesOfDestination(r,r,t,t,t,t)

  % plain_NDB(Graph, NDB, Frequency, NavaidEquipment*)
  ,plain_NDB(r,r,t,t)

  % fixm_SurvivalCapabilities(Graph, SurvivalCapabilities, SurvivalEquipmentRemarks?, DinghyInformation?, EmergencyRadioCode*, LifeJacketCode*, SurvivalEquipmentCode*)
  ,fixm_SurvivalCapabilities(r,r,t,t,t,t,t)

  % plain_FlightIdentification(Graph, FlightIdentification, EfplFlight+, Callsign, AircraftIdentification)
  ,plain_FlightIdentification(r,r,t,t,t)

  % fixm_AircraftType(Graph, AircraftType)
  ,fixm_AircraftType(r,r)

  % plain_Speed(Graph, Speed, GroundSpeed, FlightIdentification+)
  ,plain_Speed(r,r,t,t)

  % plain_UsageCondition(Graph, UsageCondition, AirportHeliportUsage*, Type, PriorPermission)
  ,plain_UsageCondition(r,r,t,t,t)

  % plain_Surface(Graph, Surface, AirspaceVolume+)
  ,plain_Surface(r,r,t)

  % aixm_UsageCondition(Graph, UsageCondition, Type?, PriorPermission?, Selection?, Annotation*, Contact*)
  ,aixm_UsageCondition(r,r,t,t,t,t,t)
.

% fixm_NavigationCapabilities(Graph, NavigationCapabilities, OtherNavigationCapabilities?, PerformanceBasedCode*, NavigationCode*)
fixm_NavigationCapabilities(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', val("RNVD1E2A1"^^xsd:'string'), [val("B1"^^xsd:'string'), val("A1"^^xsd:'string'), val("D1"^^xsd:'string'), val("L1"^^xsd:'string'), val("O1"^^xsd:'string'), val("S1"^^xsd:'string'), val("C1"^^xsd:'string'), val("S2"^^xsd:'string')], [val("I"^^xsd:'string'), val("A"^^xsd:'string'), val("D"^^xsd:'string'), val("W"^^xsd:'string'), val("X"^^xsd:'string'), val("G"^^xsd:'string')]).

% aixm_Note(Graph, Note, PropertyName?, Purpose?, TranslatedNote*)
aixm_Note(graph:'0_donlon-data.ttl', s1:'n002', val("fieldElevation"^^xsd:'string'), '$null$', [s1:'ln002']).
aixm_Note(graph:'0_donlon-data.ttl', s1:'n003', val("aRP"^^xsd:'string'), '$null$', [s1:'ln003']).

% fixm_VerticalRange(Graph, VerticalRange, LowerBound?, UpperBound?)

% plain_Performance(Graph, Performance, RateOfDescent, PerformanceAltitude, RateOfClimb, TrueAirSpeed, FlightIdentification*, MachNumber)

% aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)

% plain_AircraftType(Graph, AircraftType, Aircraft*, IcaoModelIdentifier)
plain_AircraftType(graph:'0_UML_all.ttl', file:'aircraftType', [], val("B738"^^xsd:'string')).
plain_AircraftType(graph:'0_UML_all.ttl', file:'type2', [], val("B734"^^xsd:'string')).
plain_AircraftType(graph:'0_UML_all.ttl', file:'Type2', [], val("A320"^^xsd:'string')).
plain_AircraftType(graph:'0_UML_all.ttl', file:'Type1', [], val("A320"^^xsd:'string')).
plain_AircraftType(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ID_011', [], val("B738"^^xsd:'string')).

% fixm_StandPositionAndTime(Graph, StandPositionAndTime, StandName?, StandTime?, TerminalName?)

% aixm_OrganisationAuthorityAssociation(Graph, OrganisationAuthorityAssociation, Type?, Annotation*, TheOrganisationAuthority)

% fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel?)
fixm_EfplPoint4D(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034').
fixm_EfplPoint4D(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040').
fixm_EfplPoint4D(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031').
fixm_EfplPoint4D(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025').
fixm_EfplPoint4D(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037').

% fixm_AircraftOperator(Graph, AircraftOperator, OperatingOrganization?, OperatorCategory?)
fixm_AircraftOperator(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val("AIR_CARRIER"^^xsd:'string')).

% fixm_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair)
fixm_EfplTrajectoryRoutePair(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008').

% fixm_RoutePoint(Graph, RoutePoint, Constraint*)

% fixm_BeaconCodeAssignment(Graph, BeaconCodeAssignment, CurrentBeaconCode?, PreviousBeaconCode?, ReassignedBeaconCode?, ReassigningUnit?)

% fixm_RouteConstraintOrPreference(Graph, RouteConstraintOrPreference, ConstraintType?)

% plain_FlightLevel(Graph, FlightLevel, Unit, EfplPoint4D+, Level)

% fixm_EstimatedElapsedTime(Graph, EstimatedElapsedTime, ElapsedTime?, Location?)

% fixm_ReportedTime(Graph, ReportedTime, Provenance?, Time?)

% fixm_GeographicLocation(Graph, GeographicLocation, Pos*, SrsName?)
fixm_GeographicLocation(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', [], val("urn:ogc:def:crs:EPSG::4326"^^xsd:'string')).
fixm_GeographicLocation(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', [], val("urn:ogc:def:crs:EPSG::4326"^^xsd:'string')).
fixm_GeographicLocation(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', [], val("urn:ogc:def:crs:EPSG::4326"^^xsd:'string')).
fixm_GeographicLocation(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', [val(50.03330555555556^^xsd:'decimal'), val(8.570455555555556^^xsd:'decimal')], val("urn:ogc:def:crs:EPSG::4326"^^xsd:'string')).
fixm_GeographicLocation(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', [], val("urn:ogc:def:crs:EPSG::4326"^^xsd:'string')).

% aixm_LinguisticNote(Graph, LinguisticNote, Note?)
aixm_LinguisticNote(graph:'0_donlon-data.ttl', s1:'ln002', val("Geoid undulation at ELEV PSN is 9 M"^^xsd:'string')).
aixm_LinguisticNote(graph:'0_donlon-data.ttl', s1:'ln003', val("Heliport reference point site at heliport: geometric centre of TLOF, Direction and distance from (city): Donlon	downtown, east shore of Donlon river"^^xsd:'string')).

% fixm_PointRange(Graph, PointRange, LateralRange?, VerticalRange?, TemporalRange?)

% aixm_City(Graph, City, Name?, Annotation*)
aixm_City(graph:'0_donlon-data.ttl', s1:'ID_110', val("DONLON"^^xsd:'string'), []).

% fixm_AirspeedRange(Graph, AirspeedRange, LowerSpeed?, UpperSpeed?)

% plain_AirspaceVolume(Graph, AirspaceVolume, MaximumLimit, MaximumLimitReference, Width, MinimumLimit, UpperLimit, LowerLimit, AirspaceGeometryComponent+, UpperLimitReference, LowerLimitReference, MinimumLimitReference)

% fixm_RankedTrajectory(Graph, RankedTrajectory, Identifier?, MaximumAcceptableDelay?, AssignedIndicator?, RouteTrajectoryPair?)

% fixm_TrajectoryPointRole(Graph, TrajectoryPointRole, BottomOfClimb?, BottomOfDescent?, BoundaryPoint?, FromGATToOAT?, FromIFRToVFR?, FromOATToGat?, FromVFRToIFR?, TopOfClimb?, TopOfDescent?)
fixm_TrajectoryPointRole(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', val("false"^^xsd:'string'), val("false"^^xsd:'string'), '$null$', '$null$', '$null$', '$null$', '$null$', val("false"^^xsd:'string'), val("false"^^xsd:'string')).

% fixm_CommunicationCapabilities(Graph, CommunicationCapabilities, OtherCommunicationCapabilities?, OtherDataLinkCapabilities?, DataLinkCode*, SelectiveCallingCode?, CommunicationCode*)
fixm_CommunicationCapabilities(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', '$null$', val("SVM"^^xsd:'string'), [val("J4"^^xsd:'string'), val("J5"^^xsd:'string'), val("J3"^^xsd:'string')], val("EHJM"^^xsd:'string'), [val("E3"^^xsd:'string'), val("H"^^xsd:'string'), val("E2"^^xsd:'string'), val("M1"^^xsd:'string'), val("Y"^^xsd:'string')]).

% fixm_Dinghy(Graph, Dinghy, Quantity?, TotalCapacity?, Covered?, Colour?)

% aixm_ContactInformation(Graph, ContactInformation, Name?, Title?, Annotation*, NetworkNode*, Address*, PhoneFax*)

% fixm_DangerousGoods(Graph, DangerousGoods, GuidebookNumber?, OnboardLocation?, HandlingInformation?, AircraftLimitation?, AirWayBill?, Shipment?, PackageGroup*, ShippingInformation?)

% fixm_Handoff(Graph, Handoff, ReceivingUnit?, TransferringUnit?, CoordinationStatus?)

% plain_AerodromesOfDestination(Graph, AerodromesOfDestination, EfplFlight*)
plain_AerodromesOfDestination(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ID_007', []).

% fixm_ContactInformation(Graph, ContactInformation, Name?, Title?, OnlineContact?, PhoneFax?, Address?)

% aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator?, Name?, LocationIndicatorICAO?, DesignatorIATA?, Type?, CertifiedICAO?, PrivateUse?, ControlType?, FieldElevation?, FieldElevationAccuracy?, VerticalDatum?, MagneticVariation?, MagneticVariationAccuracy?, DateMagneticVariation?, MagneticVariationChange?, ReferenceTemperature?, AltimeterCheckLocation?, SecondaryPowerSupply?, WindDirectionIndicator?, LandingDirectionIndicator?, TransitionAltitude?, TransitionLevel?, LowestTemperature?, Abandoned?, CertificationDate?, CertificationExpirationDate?, Contact*, Annotation*, ARP?, AltimeterSource*, Contaminant*, ServedCity*, ResponsibleOrganisation?, AviationBoundary?, Availability*)
aixm_AirportHeliportTimeSlice(graph:'0_donlon-data.ttl', s1:'AHP_EADH', val("EADH"^^xsd:'string'), val("DONLON/DOWNTOWN HELIPORT"^^xsd:'string'), val("EADH"^^xsd:'string'), nil("unknown"^^xsd:'string'), '$null$', nil("unknown"^^xsd:'string'), '$null$', val("CIVIL"^^xsd:'string'), xval("18"^^xsd:'string',"M"^^xsd:'string'), xval("0.5"^^xsd:'string',"M"^^xsd:'string'), '$null$', val(-3.0^^xsd:'decimal'), val(1.0^^xsd:'decimal'), val("1990"^^xsd:'string'), val(0.03^^xsd:'decimal'), xval(21.0^^xsd:'decimal',"C"^^xsd:'string'), '$null$', '$null$', '$null$', '$null$', xval("3500"^^xsd:'string',"FT"^^xsd:'string'), '$null$', '$null$', '$null$', nil("unknown"^^xsd:'string'), '$null$', [], [s1:'n003', s1:'n002'], s1:'elpoint1EADH', [], [], [s1:'ID_110'], s1:'A-a72cfd3a', '$null$', [s1:'AHY_EADH_PERMIT']).
aixm_AirportHeliportTimeSlice(graph:'0_donlon-data.ttl', s2:'ID_ACT_11', '$null$', '$null$', '$null$', val("ysdf"^^xsd:'string'), '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], [], '$null$', [], [], [s2:'city1', s2:'city2'], '$null$', '$null$', [s2:'ID_ACT_13']).

% aixm_Ridge(Graph, Ridge, Side?, Distance?, Depth?, Annotation*)

% plain_ElevatedPoint(Graph, ElevatedPoint, Navaid*, Elevation, AirportHeliport+)

% fixm_EnRouteDiversion(Graph, EnRouteDiversion, DiversionRecoveryInformation?)

% plain_SectorExitConditions(Graph, SectorExitConditions, FlightIdentification*, ExitFL)
plain_SectorExitConditions(graph:'0_UML_all.ttl', file:'ext1', [], xval("355"^^xsd:'string',"FL"^^xsd:'string')).
plain_SectorExitConditions(graph:'0_UML_all.ttl', file:'ext2', [], xval("355"^^xsd:'string',"FL"^^xsd:'string')).
plain_SectorExitConditions(graph:'0_UML_all.ttl', file:'sectorExitConditions', [], xval(380^^xsd:'decimal',"FL"^^xsd:'string')).

% plain_AircraftPosition(Graph, AircraftPosition, FlightLevel, ActualSpeed, Course, Longitude, Altitude, Track, Latitude, Speed, FlightIdentification*, SrsName, Position, PositionTime)

% fixm_ActualSpeed(Graph, ActualSpeed, Calculated?, PilotReported?, Surveillance?)

% plain_FuturePosition(Graph, FuturePosition, TimeStamp, Probability, PredictedAltitude, FlightIdentification+, Latitude, Longitude, WaypointName)

% fixm_FlightEmergency(Graph, FlightEmergency, ActionTaken?, EmergencyDescription?, Originator?, OtherInformation?, Phase?, Contact?)

% aixm_PropertiesWithSchedule(Graph, PropertiesWithSchedule, Annotation*, SpecialDateAuthority*, TimeInterval*)
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s2:'ID_ACT_20', [], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s2:'ID_ACT_18', [], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s1:'AHY_EADH_PERMIT', [], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s1:'A-a72cfd3a', [s1:'n002'], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s1:'agtayyat', [], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s2:'ID_ACT_13', [], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s2:'ID_ACT_16', [], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s2:'ID_ACT_15', [], [], []).

% fixm_TrajectoryRoutePair(Graph, TrajectoryRoutePair, Trajectory?, Route?)
fixm_TrajectoryRoutePair(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020').

% fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit?, UpstreamUnit?, BoundaryCrossingProposed?, BoundaryCrossingCoordinated?, Handoff?, UnitBoundaryIndicator?)

% plain_List(Graph, List, Assumed, FlightIdentification*)
plain_List(graph:'0_UML_all.ttl', file:'list', val("YES"^^xsd:'string'), []).

% aixm_SurfaceContamination(Graph, SurfaceContamination, ObservationTime?, Depth?, FrictionCoefficient?, FrictionEstimation?, FrictionDevice?, ObscuredLights?, FurtherClearanceTime?, FurtherTotalClearance?, NextObservationTime?, Proportion?, CriticalRidge*, Annotation*, Layer*)

% fixm_MeteorologicalData(Graph, MeteorologicalData, Temperature?, WindDirection?, WindSpeed?)
fixm_MeteorologicalData(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', xval(4.3^^xsd:'decimal',"CELSIUS"^^xsd:'string'), xval(248.0^^xsd:'decimal',"DEGREES"^^xsd:'string'), xval(10.41^^xsd:'decimal',"METERS_PER_SECOND"^^xsd:'string')).

% aixm_OrganisationAuthority(Graph, OrganisationAuthority, TimeSlice*)
aixm_OrganisationAuthority(graph:'0_donlon-data.ttl', uuid:'74efb6ba-a52a-46c0-a16b-03860d356882', []).

% fixm_TelephoneContact(Graph, TelephoneContact, Voice?, Facimile?)

% plain_DME(Graph, DME, Frequency, NavaidEquipment*, Type)

% plain_Weather(Graph, Weather, WindDirection, Temperature, Precipitation, WindSpeed, Thunder, Altitude*, Icing, CloudCoverage)
plain_Weather(graph:'0_UML_all.ttl', file:'weather', xval("000"^^xsd:'string',"DEGREES"^^xsd:'string'), xval(-57^^xsd:'decimal',"CELSIUS"^^xsd:'string'), val("NO"^^xsd:'string'), xval(55^^xsd:'decimal',"MILES_PER_HOUR"^^xsd:'string'), val("NO"^^xsd:'string'), [], val("NO"^^xsd:'string'), val("SKC"^^xsd:'string')).

% plain_Point(Graph, Point, SignificantPoint+, HorizontalAccuracy)

% aixm_AirportHeliportContamination(Graph, AirportHeliportContamination)

% plain_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus, AirportHeliport+, Warning)

% fixm_OtherInformation(Graph, OtherInformation, ReplacementFlightPlanIndicator?, RunwayVisualRange?)
fixm_OtherInformation(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', '$null$', xval(0.075^^xsd:'decimal',"KILOMETERS"^^xsd:'string')).

% aixm_TelephoneContact(Graph, TelephoneContact, Voice?, Facsimile?)

% plain_SignificantPointInAirspace(Graph, SignificantPointInAirspace, Type, RelativeLocation)
plain_SignificantPointInAirspace(graph:'0_UML_all.ttl', 'http://localhost:3030/test/entry_PUNSA', val("ENTRY"^^xsd:'string'), val("BORDER"^^xsd:'string')).

% plain_NavaidComponent(Graph, NavaidComponent, CollocationGroup, Navaid*)
plain_NavaidComponent(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#HOC_VOR', val(1^^xsd:'unsignedint'), []).
plain_NavaidComponent(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#SPR_DME', val(1^^xsd:'unsignedint'), []).
plain_NavaidComponent(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#FRI_VOR', val(1^^xsd:'unsignedint'), []).
plain_NavaidComponent(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ZUE_DME', val(1^^xsd:'unsignedint'), []).
plain_NavaidComponent(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#TRA_VOR', val(1^^xsd:'unsignedint'), []).
plain_NavaidComponent(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#GVA_DME', val(1^^xsd:'unsignedint'), []).
plain_NavaidComponent(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#FRI_DME', val(1^^xsd:'unsignedint'), []).
plain_NavaidComponent(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#TRA_DME', val(1^^xsd:'unsignedint'), []).
plain_NavaidComponent(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#SPR_VOR', val(1^^xsd:'unsignedint'), []).
plain_NavaidComponent(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#GVA_VOR', val(1^^xsd:'unsignedint'), []).
plain_NavaidComponent(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#HOC_DME', val(1^^xsd:'unsignedint'), []).
plain_NavaidComponent(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ZUE_VOR', val(1^^xsd:'unsignedint'), []).

% fixm_Originator(Graph, Originator)

% fixm_FlightStatus(Graph, FlightStatus, AirborneHold?, Airfile?, Accepted?, FlightCycle?, MissedApproach?, Suspended?)

% fixm_IdentifiedUnitReference(Graph, IdentifiedUnitReference, UnitIdentifier?)

% plain_AirspaceGeometryComponent(Graph, AirspaceGeometryComponent, Airspace+)

% fixm_Radionuclide(Graph, Radionuclide, PhysicalChemicalForm?, RadionuclideId?, RadionuclideName?, LowDispersibleMaterialIndicator?, Activity?, SpecialFormIndicator?)

% aixm_OnlineContact(Graph, OnlineContact, Network?, Linkage?, Protocol?, EMail?)

% plain_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, TrajectoryPointType, Trajectory*)
plain_EfplTrajectoryPoint(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#P2', val(0^^xsd:'integer'), []).
plain_EfplTrajectoryPoint(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#P1', val(0^^xsd:'integer'), []).
plain_EfplTrajectoryPoint(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#P4', val(0^^xsd:'integer'), []).
plain_EfplTrajectoryPoint(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#P3', val(0^^xsd:'integer'), []).
plain_EfplTrajectoryPoint(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#P6', val(0^^xsd:'integer'), []).
plain_EfplTrajectoryPoint(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#P5', val(0^^xsd:'integer'), []).
plain_EfplTrajectoryPoint(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#P8', val(0^^xsd:'integer'), []).
plain_EfplTrajectoryPoint(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#P7', val(0^^xsd:'integer'), []).
plain_EfplTrajectoryPoint(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#P10', val(0^^xsd:'integer'), []).
plain_EfplTrajectoryPoint(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#P9', val(0^^xsd:'integer'), []).

% fixm_StructuredPostalAddress(Graph, StructuredPostalAddress)

% aixm_AirportHeliportUsage(Graph, AirportHeliportUsage, Operation?)
aixm_AirportHeliportUsage(graph:'0_donlon-data.ttl', s2:'ID_ACT_14', '$null$').
aixm_AirportHeliportUsage(graph:'0_donlon-data.ttl', s1:'AHU_EADH_PERMIT', '$null$').

% plain_Meteorology(Graph, Meteorology, ConditionCombination+, RunwayVisualRangeInterpretation, Visibility, RunwayVisualRange, VisibilityInterpretation, FlightConditions)

% gml_SurfacePatch(Graph, SurfacePatch)
gml_SurfacePatch(graph:'0_UML_all.ttl', file:'ID1').
gml_SurfacePatch(graph:'0_UML_all.ttl', file:'ID12').

% aixm_FlightCharacteristic(Graph, FlightCharacteristic, Type?, Rule?, Status?, Military?, Origin?, Purpose?, Annotation*)
aixm_FlightCharacteristic(graph:'0_donlon-data.ttl', s2:'ID_ACT_19', '$null$', '$null$', val("SAR"^^xsd:'string'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'0_donlon-data.ttl', s2:'ID_ACT_17', '$null$', '$null$', '$null$', '$null$', val("HOME_BASED"^^xsd:'string'), '$null$', []).
aixm_FlightCharacteristic(graph:'0_donlon-data.ttl', s1:'F_yastadyt', '$null$', val("VFR"^^xsd:'string'), '$null$', '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'0_donlon-data.ttl', s2:'ID_ACT_21', '$null$', '$null$', val("OTHER"^^xsd:'string'), '$null$', '$null$', '$null$', []).

% aixm_AirportHeliport(Graph, AirportHeliport, TimeSlice*)
aixm_AirportHeliport(graph:'0_donlon-data.ttl', uuid:'dd062d88-3e64-4a5d-bebd-89476db9ebea', [s1:'AHP_EADH', s2:'ID_ACT_11']).

% fixm_TrajectoryPoint(Graph, TrajectoryPoint, AltimeterSetting?, PredictedAirspeed?, PredictedGroundspeed?, MetData?, Point?, TrajectoryChange*, TrajectoryChangeType*, ReferencePoint?)
fixm_TrajectoryPoint(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', [], [], '$null$').
fixm_TrajectoryPoint(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', '$null$', xval(0.0^^xsd:'decimal',"KILOMETRES_PER_HOUR"^^xsd:'string'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', [], [], '$null$').
fixm_TrajectoryPoint(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', [], [], '$null$').
fixm_TrajectoryPoint(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', [], [], '$null$').
fixm_TrajectoryPoint(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', [], [], '$null$').

% fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier?, DistanceFromTakeOff?, EfplEstimatedSpeed?, ElapsedTime?, GrossWeight?, TrajectoryPointType?, TrajectoryPointRole?, InboundSegment?)
fixm_EfplTrajectoryPoint(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', xval(250.151^^xsd:'decimal',"KILOMETERS"^^xsd:'string'), '$null$', val("P0DT0H24M3S"^^xsd:'string'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', xval(3.727^^xsd:'decimal',"KILOMETERS"^^xsd:'string'), '$null$', val("P0DT0H1M4S"^^xsd:'string'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', xval(0.0^^xsd:'decimal',"KILOMETERS"^^xsd:'string'), '$null$', val("P0DT0H0M0S"^^xsd:'string'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', xval(0.0^^xsd:'decimal',"KILOMETERS"^^xsd:'string'), xval(0.0^^xsd:'decimal',"MACH"^^xsd:'string'), val("P0DT0H0M0S"^^xsd:'string'), xval(458849.0^^xsd:'decimal',"KILOGRAMS"^^xsd:'string'), val(0^^xsd:'integer'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$').
fixm_EfplTrajectoryPoint(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', xval(261.039^^xsd:'decimal',"KILOMETERS"^^xsd:'string'), '$null$', val("P0DT0H24M48S"^^xsd:'string'), '$null$', '$null$', '$null$', '$null$').

% fixm_RunwayPositionAndTime(Graph, RunwayPositionAndTime, RunwayName?, RunwayTime?)

% fixm_Feature(Graph, Feature, Provenance?)
fixm_Feature(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$').
fixm_Feature(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$').
fixm_Feature(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$').
fixm_Feature(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$').

% fixm_ElapsedTimeLocation(Graph, ElapsedTimeLocation)

% plain_EfplPoint4D(Graph, EfplPoint4D, EfplTrajectoryPoint*, FlightLevel, Time, SrsName, Pos)

% aixm_Surface(Graph, Surface, HorizontalAccuracy?, Annotation*)

% gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition)
gml_TimePeriod(graph:'0_donlon-data.ttl', s2:'IDE_ACT_24', val(date_time(2018, 5, 11, 10, 0, 0, 0)^^xsd:'dateTime'), val(date_time(2018, 5, 11, 18, 0, 0, 0)^^xsd:'dateTime')).
gml_TimePeriod(graph:'0_donlon-data.ttl', s2:'IDE_ACT_23', val(date_time(2018, 5, 11, 10, 0, 0, 0)^^xsd:'dateTime'), val(date_time(2018, 5, 11, 18, 0, 0, 0)^^xsd:'dateTime')).
gml_TimePeriod(graph:'0_donlon-data.ttl', s2:'ID_ACT_12', val(date_time(2018, 5, 11, 10, 0, 0, 0)^^xsd:'dateTime'), val(date_time(2018, 5, 11, 18, 0, 0, 0)^^xsd:'dateTime')).
gml_TimePeriod(graph:'0_donlon-data.ttl', s1:'vtnull0', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_donlon-data.ttl', s1:'ltnull0', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltLUKOM', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtOBEDU', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtVENAT', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtLUPOL', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltARNOT', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltCERVI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtAKITO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtLASAT', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtRESIA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtRIXUV', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltKUDIS', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtROREM', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtAOSTA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltGARLO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltDEGAD', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtLEVSI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtLOKTA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltRAVED', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtSALEV', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltPUNSA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtSUREP', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltGOLEB', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltnull0_spr', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtESEVA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBIVLO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtVEDOK', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltRESIA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtROBAS', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtVALBU', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltPELOG', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtALAGO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBIBOT', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltKUKEV', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltROMGA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtLUTIX', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltNEMOS', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltETOXU', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtOBAKI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltHEUSE', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtTORPA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtMIRGU', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtKOGAS', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltOBAKI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtSOSAL', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtDEGES', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtVADEM', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBASGO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltnull0', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtRINLI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltRISOR', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltTUROM', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltGILIR', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtOSDOV', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtLADOL', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtABESI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBATUB', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltMIRGU', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltSUREP', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtLAMUR', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltARBOS', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtNEMOS', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltUSUDO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtKUDIS', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtSAFFA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltGATPI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltLAMUR', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtMINGA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtGARLO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltUTAVO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltMOSIT', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtVANAS', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltVEBIT', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtTUROM', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtRONAG', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtLUKOM', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtOLASO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBANKO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltSIROD', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltARTAG', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltARLES', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltABESI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBALIR', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltROLSA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltIDREK', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtROMGA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtHEUSE', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltTUPOV', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltNATLI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtUNTAD', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtETOXU', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBALIR', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltCANNE', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltVALBU', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtUTAVO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtARLES', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBEMKI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtELMUR', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltSOPER', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtPELAD', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtCANNE', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltOMETO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtnull0_hoc', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltASLEG', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtMOSIT', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtATMAX', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltAOSTA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtPEPAG', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtRAUTA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtABARI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltRAUTA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtALINE', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltRIPUS', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltLUSIL', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltPEPAG', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtGUDAX', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtPUNSA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltGERSA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtOMIDO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtGAMSA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltAMRUP', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltTERSI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltTINOX', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBADEP', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBATUB', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltLEVSI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltGAMSA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtMONIN', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltNUNRI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtDETRI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtRAVED', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtTELNO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtVALAV', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltUNTAD', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtPEVIT', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltDORAP', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtRISOR', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtAKASU', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltNESES', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtGERSA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtARBOS', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltNATOR', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltIBINI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtIXOGU', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtMADEB', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtAKABI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltVENAT', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltMOROK', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtTIRSO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtKUKEV', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltLUTIX', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltKOGAS', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltIXOGU', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtTINOX', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltMOGTI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtAMEXO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltnull0_tra', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtROMIR', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltMONIN', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtNUNRI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtLAPAG', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltABREG', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltODINA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBEGAR', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', file:'AVT1', val(date_time(2019, 7, 4, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtODINA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltMINGA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtIBINI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltNAXAV', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtGARMO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltRONAG', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltDETRI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltINTEG', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltnav1901_spr', val(date_time(2010, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtMOGTI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltATMAX', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltKUSAM', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltDOMIL', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltGARMO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtUBIMA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBIBOT', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltKUNOD', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltESEVA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltURIGI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBENOT', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtDEGAD', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtnull0', val(date_time(2020, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltVADEM', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltSALEV', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBIBAN', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBAMEV', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#nav2501_tra', val(date_time(2020, 1, 19, 8, 38, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#nav2501_fri', val(date_time(2020, 1, 19, 8, 38, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtVEBIT', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtSHARA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtDILEB', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBIKBI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBERSU', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltTORPA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtTERSI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtOMASI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtADISO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', file:'AFLT1', val(date_time(2019, 7, 4, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltVEDOK', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltnav2501:fri', val(date_time(2010, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBIKBI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltKONOL', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtUTABA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtnull0_gva_vor', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltOLBEN', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtMOBLO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtGILIR', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtSUVEL', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltRIXUV', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtNAXAV', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtSOPER', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#nav1901_zue', val(date_time(2020, 1, 19, 8, 38, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltUMTEX', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtCERVI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtGOLEB', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtULMES', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltVADAR', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltOMIDO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtROLSA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtXEBIX', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtnull0_tra', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltDILEB', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtKONOL', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBETEN', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtUSUDO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltINVOP', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtLORLO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltVANAS', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtLUSIL', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltPEVIT', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltDOPIL', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltOBEDU', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltMADEB', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtnull0_fri', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltnav2501_tra', val(date_time(2010, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltROMIR', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtOSDER', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltORSUD', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtKORED', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltPERAK', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#gla2701', val(date_time(2020, 1, 22, 8, 38, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltKUBOM', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltLAPAG', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBASGO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtKUSAM', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtTUPOV', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtPELOG', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltLUPOL', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#nav1901_spr', val(date_time(2020, 1, 19, 8, 38, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltOMASI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltLOKTA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltRINLI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltnull0_zue', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtAMRUP', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltMANEG', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtPERAK', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltVALAV', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtMOMUK', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtARTAG', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltOBUBI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtEVANO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltDITON', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtLUMEL', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtNATOR', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtINVOP', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtDITON', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltnav2501_hoc', val(date_time(2010, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtDOPIL', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltELMEM', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltROTOS', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtELMEM', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtOLBEN', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtINTEG', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltROBAS', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltUBIMA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtABREG', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltnull0_fri', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vlt1901', val(date_time(2020, 1, 19, 8, 38, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltOSDOV', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltSUXAN', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#tgla2701', val(date_time(2010, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltMOBLO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltGUDAX', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtARDED', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtSUXAN', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBERSU', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtKESEX', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltTIRSO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBODAN', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtIDREK', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltTIRUL', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtOBUBI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltTELNO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtATPED', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltKORED', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtLASUN', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtMOROK', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltVEROX', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtTIRUL', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltOLASO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtGATPI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtDOMIL', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltULMES', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltEVANO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltALAGO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtSIROD', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtUMTEX', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBETEN', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltABARI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltPELAD', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltROREM', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBEMKI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#nav1901_gva', val(date_time(2020, 1, 19, 8, 38, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltnav1901_zue', val(date_time(2010, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBAMEV', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtURIGI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltAMEXO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltKELIP', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', file:'AVT2', val(date_time(2019, 7, 4, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtORSUD', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltNEGRA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBEGAR', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltALINE', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltMOLUS', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBANKO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltAKITO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltSOSON', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltOSDER', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltSHARA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltPIXOS', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBADEP', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtNESES', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltAKABI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtKUNOD', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtPIXOS', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtROTOS', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltnav1901_gva', val(date_time(2010, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltMOREG', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltMOMUK', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtOMETO', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltnull0_hoc', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBIBAN', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltXEBIX', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltARDED', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltADISO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltATPED', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vt_punsa_entry', val(date_time(2019, 7, 4, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtMOLUS', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtKUBOM', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtMANEG', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtEDOPI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltLASAT', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltELMUR', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBIVLO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtPIMOT', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltUTABA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltDEGES', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtNEGRA', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltLUMEL', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtASLEG', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltEDOPI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtNATLI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtSONGI', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltBODAN', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltPIMOT', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtnull0_spr', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#lt_punsa_entry', val(date_time(2019, 7, 4, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtSOSON', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtKELIP', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltLASUN', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltnull0_gva_dme', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#nav2501_hoc', val(date_time(2020, 1, 19, 8, 38, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtRIPUS', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtMOREG', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltAKASU', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltLADOL', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtBENOT', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtARNOT', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtVADAR', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltKESEX', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltSUVEL', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtVEROX', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltSAFFA', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtnull0_zue', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#vtDORAP', val(date_time(2017, 7, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltSOSAL', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', file:'AFLT2', val(date_time(2019, 7, 4, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltSONGI', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#lt1901', val(date_time(2010, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).
gml_TimePeriod(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ltLORLO', val(date_time(2009, 1, 1, 0, 0, 0, 0)^^xsd:'dateTime'), indeterminate("unknown"^^xsd:'string')).

% fixm_AircraftCapabilities(Graph, AircraftCapabilities, Survival?, Communication?, Navigation?, Surveillance?, StandardCapabilities?)
fixm_AircraftCapabilities(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$').

% fixm_EnRoute(Graph, EnRoute, AlternateAerodrome*, FleetPrioritization?, BoundaryCrossings*, CpdlcConnection?, BeaconCodeAssignment?, Cleared?, ControlElement*, Pointout?, Position?)

% fixm_TemporalRange(Graph, TemporalRange, Earliest?, Latest?)

% plain_City(Graph, City, AirportHeliport+, Name)

% fixm_Aircraft(Graph, Aircraft, AircraftColours?, AircraftQuantity?, EngineType?, AircraftAddress?, Capabilities?, Registration?, AircraftType?, WakeTurbulence?, AircraftPerformance?)
fixm_Aircraft(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$', val(1^^xsd:'integer'), '$null$', val("3C65A1"^^xsd:'string'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', val("DAIMA"^^xsd:'string'), '$null$', val("J"^^xsd:'string'), val("C"^^xsd:'string')).

% fixm_OnlineContact(Graph, OnlineContact, Email?)

% fixm_TimeSequence(Graph, TimeSequence, Approval?, Begin?, End?, Ready?, Request?)

% fixm_AdditionalHandlingInformation(Graph, AdditionalHandlingInformation, ResponsibleAgent?)

% fixm_Extension(Graph, Extension)

% fixm_SurveillanceCapabilities(Graph, SurveillanceCapabilities, OtherSurveillanceCapabilities?, SurveillanceCode*)
fixm_SurveillanceCapabilities(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$', [val("B1"^^xsd:'string'), val("D1"^^xsd:'string'), val("L"^^xsd:'string')]).

% fixm_Trajectory(Graph, Trajectory, TrajectoryPoint*)
fixm_Trajectory(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022']).

% aixm_AltimeterSourceTimeSlice(Graph, AltimeterSourceTimeSlice, IsRemote?, IsPrimary?, Availability*, Annotation*)

% aixm_PostalAddress(Graph, PostalAddress, DeliveryPoint?, City?, AdministrativeArea?, PostalCode?, Country?)

% plain_AircraftOperator(Graph, AircraftOperator, EfplFlight*, OperatorCategory)
plain_AircraftOperator(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ID_005', [], val("AIR_CARRIER"^^xsd:'string')).

% fixm_LastPositionReport(Graph, LastPositionReport, DeterminationMethod?, Position?, TimeAtPosition?)

% aixm_AltimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus?)

% fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules?)
fixm_EfplRoute(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', val("IFR"^^xsd:'string')).

% fixm_CoordinationStatus(Graph, CoordinationStatus, AbrogationReason?, CoordinationStatus?, NonStandardCommunicationReason?, ReleaseConditions?)

% fixm_BoundaryCrossing(Graph, BoundaryCrossing, Altitude?, CrossingPoint?, CrossingSpeed?, CrossingTime?, Offtrack?, AltitudeInTransition?)

% plain_AirportHeliport(Graph, AirportHeliport, Navaid*, TransitionLevel, Name, Designator, DesignatorIATA, MagneticVariation, LocationIndicatorICAO, FieldElevation, TransitionAltitude)

% fixm_RadioCommunicationFailure(Graph, RadioCommunicationFailure, RadioFailureRemarks?, RemainingComCapability?, Contact?)

% aixm_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus?, Warning?, Usage*)
aixm_AirportHeliportAvailability(graph:'0_donlon-data.ttl', s1:'AHY_EADH_PERMIT', val("NORMAL"^^xsd:'string'), '$null$', [s1:'AHU_EADH_PERMIT']).
aixm_AirportHeliportAvailability(graph:'0_donlon-data.ttl', s2:'ID_ACT_13', val("CLOSED"^^xsd:'string'), '$null$', [s2:'ID_ACT_14']).

% fixm_RadioactiveMaterial(Graph, RadioactiveMaterial, CriticalitySafetyIndex?, TransportIndex?, FissileExceptedIndicator?, Category?, Radionuclide?)

% fixm_ControlElement(Graph, ControlElement)

% fixm_AllPackedInOne(Graph, AllPackedInOne, NumberOfPackages?, QValue?)

% aixm_AltimeterSource(Graph, AltimeterSource, TimeSlice*)

% plain_DesignatedPoint(Graph, DesignatedPoint, Name, Type, Designator, SignificantPoint*)

% fixm_DirectRouting(Graph, DirectRouting, From?, To?)

% fixm_TargetMultiTime(Graph, TargetMultiTime, Target?)

% plain_AircraftPairs(Graph, AircraftPairs, VarAltitude, MinimumDistance, ProbabilityOfSI, ConflictCode, Screening, FlightIdentification+, DistanceToMD, SituationOfInterest, Conflict, VarVerticalRate, TimeToMD, VarGS)

% plain_EfplRoute(Graph, EfplRoute, RouteText, EfplFlightRules, EfplTrajectoryRoutePair*)
plain_EfplRoute(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ID_020', val("*MLG3 *MLG4 MLG MGA BLN ANZAN NASOS INTAX CJN BRITO LARDA RONNY TOPTU GONUP TOU GAI MAKIL DITEV MEN NINUN MEZIN LATAM OTROT ETREK LUXAN ARKOX LTP GIPNO NAVLA SOPLO OMASI GAMSA DOLUP ENORU POLON ASLUX ETUBO RUDKA REWED ORHIM GOVIK MNS ABKOM PASUD MOSON KOMOG RUBAG GUBON FK"^^xsd:'string'), val("IFR"^^xsd:'string'), []).

% fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome?, DepartureFix?, DepartureFixTime?, DepartureFleetPrioritization?, DepartureSlot?, EarliestOffBlockTime?, OffBlockReadyTime?, RunwayPositionAndTime?, StandardInstrumentDeparture?, StandPositionAndTime?, TakeoffAlternateAerodrome*, TakeoffWeight?, DepartureTimes?)
fixm_FlightDeparture(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], '$null$', '$null$').

% fixm_AerodromeReference(Graph, AerodromeReference)
fixm_AerodromeReference(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018').
fixm_AerodromeReference(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027').
fixm_AerodromeReference(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016').
fixm_AerodromeReference(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019').

% fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime?, TaxiTime?)
fixm_EfplFlightDeparture(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', val(date_time(2016, 4, 27, 10, 0, 0, 0)^^xsd:'dateTime'), val("P0DT0H16M0S"^^xsd:'string')).

% fixm_PostalAddress(Graph, PostalAddress, AdministrativeArea?, PostalCode?, DeliveryPoint?, CountryCode?, CountryName?, City?)

% fixm_GroundspeedRange(Graph, GroundspeedRange, LowerSpeed?, UpperSpeed?)

% fixm_Pointout(Graph, Pointout, OriginatingUnit?, ReceivingUnit*)

% fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel?, EstimatedTime?, Constraint*)

% plain_Aircraft(Graph, Aircraft, EfplFlight*, AircraftAddress, AircraftPerformance, Registration, AircraftQuantity, WakeTurbulence)

% fixm_Dimensions(Graph, Dimensions, Height?, Length?, Width?)

% fixm_RouteSegment(Graph, RouteSegment, Airway?, RoutePoint?)

% aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator?, Flight*, Aircraft*, Weather*, SubCondition*)
aixm_ConditionCombination(graph:'0_donlon-data.ttl', s2:'ID_ACT_16', val("NONE"^^xsd:'string'), [s2:'ID_ACT_17'], [], [], []).
aixm_ConditionCombination(graph:'0_donlon-data.ttl', s1:'agtayyat', val("NONE"^^xsd:'string'), [s1:'F_yastadyt'], [], [], []).
aixm_ConditionCombination(graph:'0_donlon-data.ttl', s2:'ID_ACT_15', val("OR"^^xsd:'string'), [], [], [], [s2:'ID_ACT_16', s2:'ID_ACT_20', s2:'ID_ACT_18']).
aixm_ConditionCombination(graph:'0_donlon-data.ttl', s2:'ID_ACT_18', val("NONE"^^xsd:'string'), [s2:'ID_ACT_19'], [], [], []).
aixm_ConditionCombination(graph:'0_donlon-data.ttl', s2:'ID_ACT_20', val("NONE"^^xsd:'string'), [s2:'ID_ACT_21'], [], [], []).

% aixm_SurfaceContaminationLayer(Graph, SurfaceContaminationLayer, LayerOrder?, Type?, Extent*, Annotation*)

% plain_Altitude(Graph, Altitude, FlightLevel, AltitudeInterpretation, Route+)

% fixm_Organization(Graph, Organization, Name?, OtherOrganization?, Contact?)
fixm_Organization(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_017', val("DLH"^^xsd:'string'), '$null$', '$null$').

% aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)
aixm_ElevatedPoint(graph:'0_donlon-data.ttl', s1:'elpoint1EADH', '$null$', '$null$', '$null$', '$null$').

% gml_Point(Graph, Point)
gml_Point(graph:'0_donlon-data.ttl', s1:'elpoint1EADH').

% plain_ConditionCombination(Graph, ConditionCombination, UsageCondition*)
plain_ConditionCombination(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#AHS1', []).

% plain_AircraftAction(Graph, AircraftAction, ExecutionOfRequests, InitialCall, FrequencyTransfer, FlightIdentification+)

% plain_Trajectory(Graph, Trajectory, EfplTrajectoryRoutePair*)
plain_Trajectory(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ID_021', []).

% fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile*, DescentProfile*)
fixm_FlightPerformanceData(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029'], ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035']).

% fixm_ExpandedRoute(Graph, ExpandedRoute, RoutePoint*)

% plain_DirectRouting(Graph, DirectRouting, From, ClearedFlightInformation*, To)
plain_DirectRouting(graph:'0_UML_all.ttl', file:'CFIDR1', val("47.74063864746494 9.533319521486275"^^xsd:'string'), [], val("47.78439985761241 9.69784142364791 "^^xsd:'string')).

% plain_CoordinationStatus(Graph, CoordinationStatus, ReleaseConditions, NonStandardCommunicationReason, CoordinationStatus, AbrogationReason)

% fixm_DeclarationText(Graph, DeclarationText, Compliance?, Consignor?, Shipper?)

% aixm_Meteorology(Graph, Meteorology, FlightConditions?, Visibility?, VisibilityInterpretation?, RunwayVisualRange?, RunwayVisualRangeInterpretation?, Annotation*)

% aixm_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role?, TheOrganisationAuthority)
aixm_AirportHeliportResponsibilityOrganisation(graph:'0_donlon-data.ttl', s1:'A-a72cfd3a', val("OPERATE"^^xsd:'string'), uuid:'74efb6ba-a52a-46c0-a16b-03860d356882').

% plain_Organization(Graph, Organization, AircraftOperator*, Name)
plain_Organization(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ID_017', [], val("AFL"^^xsd:'string')).

% fixm_PlannedReportingPosition(Graph, PlannedReportingPosition, Position?, PositionAltitude?, PositionEstimatedTime?)

% fixm_SignificantPoint(Graph, SignificantPoint)

% fixm_SupplementalData(Graph, SupplementalData, FuelEndurance?, PersonsOnBoard?, PilotInCommand?)

% plain_Datalink(Graph, Datalink, EquipmentDegradation, InitialCall, FlightIdentification+, FrequencyChange, AssumedBySkyguide)

% plain_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role, AirportHeliport*)
plain_AirportHeliportResponsibilityOrganisation(graph:'0_UML_all.ttl', file:'airportHeliportResponsibilityOrganisation', val("OTHER"^^xsd:'string'), []).

% fixm_DangerousGoodsPackageGroup(Graph, DangerousGoodsPackageGroup, ShipmentDimensions?, DangerousGoodsPackage*, ShipmentUseIndicator?)

% plain_Time(Graph, Time, SequenceNumber, FlightIdentification+, TimeOfPrediction)

% fixm_OfftrackDistance(Graph, OfftrackDistance, Distance?, Direction?)

% plain_Navaid(Graph, Navaid, Purpose, Type, SignificantPoint*, Designator, FlightChecked, Name)

% fixm_TrajectoryChange(Graph, TrajectoryChange, ConstrainedAirspace?, SpecialActivityAirspace?)

% fixm_Point4D(Graph, Point4D, Altitude?, Time?, PointRange?)
fixm_Point4D(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', '$null$', '$null$').

% fixm_AbstractRoutePoint(Graph, AbstractRoutePoint, AirTrafficType?, DelayAtPoint?, FlightRules?, Point?, ClearanceLimit?)

% fixm_DepartureActivityTimes(Graph, DepartureActivityTimes, BoardingTime?, DeIcingTime?, GroundHandlingTime?, StartupTime?)

% plain_Waypoints(Graph, Waypoints, Longitude, Latitude, WaypointName, Route+)

% fixm_Flight(Graph, Flight, ControllingUnit?, Extensions*, FlightFiler?, Gufi?, Remarks?, AircraftDescription?, DangerousGoods*, RankedTrajectories*, RouteToRevisedDestination?, Negotiating?, Agreed?, Arrival?, Departure?, Emergency?, RadioCommunicationFailure?, EnRoute?, Operator?, EnRouteDiversion?, FlightType?, FlightStatus?, Originator?, SupplementalData?, FlightIdentification?, SpecialHandling*)
fixm_Flight(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', [], '$null$', '$null$', val("TCAS"^^xsd:'string'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', [], [], '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val("SCHEDULED"^^xsd:'string'), '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', []).

% gml_Surface(Graph, Surface, Patch+)

% fixm_ClearedFlightInformation(Graph, ClearedFlightInformation, ClearedFlightLevel?, ClearedSpeed?, Heading?, OfftrackClearance?, RateOfClimbDescend?, DirectRouting?)

% plain_Route(Graph, Route, Eastbound, RequestedAltitude, AtsRoute, Westbound, Routing, Southbound, Remarks, Northbound, CoordinationPoint, FlightIdentification+, RouteText)

% fixm_ShippingInformation(Graph, ShippingInformation, AerodromeOfLoading?, AerodromeOfUnloading?, DangerousGoodsScreeningLocation?, DepartureCountry?, DestinationCountry?, OriginCountry?, ShipmentAuthorizations?, SubsidiaryHazardClassAndDivision?, SupplementaryInformation?, TransferAerodromes*, DeclarationText?, Consignee?, Shipper?)

% fixm_DinghyColour(Graph, DinghyColour)

% fixm_CpdlcConnection(Graph, CpdlcConnection, ReceivingUnitFrequency?, AtnLogonParameters?, SendCpldcIndicator?, ConnectionStatus?, FrequencyUsage?, Fans1ALogonParameters?)

% fixm_Route(Graph, Route, AirfileRouteStartTime?, FlightDuration?, InitialCruisingSpeed?, InitialFlightRules?, RequestedAltitude?, RouteText?, EstimatedElapsedTime*, ExpandedRoute?, ClimbSchedule?, DescentSchedule?, Segment*)
fixm_Route(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$', '$null$', '$null$', '$null$', '$null$', val("N0491F350 SULUS8G SULUS UZ650 ERETO UZ715 KULOK UL984 OKG L984 DOPOV T46 DOKEL N871 POLON Z169 GERVI P851 ABERO/K0887F350 P851 RAVOK Z860 TOBLO/K0880F350 B365 BANIP/K0865F330 B365 OLUPI B923 BEKAS B142 SIVKO/K0898F390 G3 AKB A360 BLH A110 TDK A124 RULAD/K0878S1190 A460 KCA L888 LEBAK/K0882S1250 L888 PEXUN B213 WFX B330 POU R473 SIERA"^^xsd:'string'), [], '$null$', '$null$', '$null$', []).

% fixm_Person(Graph, Person, Name?, Contact?)

% plain_EfplFlight(Graph, EfplFlight, FlightType)
plain_EfplFlight(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ID_002', val("SCHEDULED"^^xsd:'string')).

% fixm_EfplFlight(Graph, EfplFlight, IfplId?, TotalEstimatedElapsedTime?, AerodromesOfDestination?, EfplSpecialHandling?, EfplFiledTrajectory?, EfplAcceptedTrajectory?, OtherInformation?, FlightPerformanceData?)
fixm_EfplFlight(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', val("P0Y0M0DT10H15M0S"^^xsd:'string'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009').

% plain_TrafficSituation(Graph, TrafficSituation, Sector, FlightIdentification+)

% plain_IcaoAerodromeReference(Graph, IcaoAerodromeReference, EfplFlightDeparture*, AerodromesOfDestination*, Code)
plain_IcaoAerodromeReference(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ID_016', [], [], val("LEMG"^^xsd:'string')).
plain_IcaoAerodromeReference(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ID_018', [], [], val("UUEE"^^xsd:'string')).

% plain_ContactInformation(Graph, ContactInformation, UsageCondition*, Title, Name)
plain_ContactInformation(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#AHC1', [], val("AIR_NAVIGATION_SERVICE_PROVIDER"^^xsd:'string'), val("SKYGUIDE"^^xsd:'string')).

% plain_string(Graph, String, WhiteSpace)

% plain_ClearedFlightInformation(Graph, ClearedFlightInformation, ClearedFlightLevel, FlightIdentification*, RateOfClimbDescent, OfftrackClearance, ClearedSpeed, Heading)

% fixm_AircraftPosition(Graph, AircraftPosition, Altitude?, Position?, PositionTime?, Track?, ActualSpeed?, NextPosition?, ReportSource?, FollowingPosition?)

% plain_NavaidEquipment(Graph, NavaidEquipment, Mobile, FlightChecked, Designator, NavaidComponent*, Name)

% plain_Airspace(Graph, Airspace, Name, ControlType, Type, LocalType, UpperLowerSeparation, Designator, SignificantPointInAirspace+)

% aixm_Timesheet(Graph, Timesheet, TimeReference?, StartDate?, EndDate?, Day?, DayTil?, StartTime?, StartEvent?, StartTimeRelativeEvent?, StartEventInterpretation?, EndTime?, EndEvent?, EndTimeRelativeEvent?, EndEventInterpretation?, DaylightSavingAdjust?, Excluded?, Annotation*)

% plain_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair, EfplFlight*)
plain_EfplTrajectoryRoutePair(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ID_008', []).

% fixm_MultiTime(Graph, MultiTime, Actual?, Estimated?)

% fixm_Provenance(Graph, Provenance, Timestamp?, Centre?, Source?, System?)

% fixm_Temperatures(Graph, Temperatures, ControlTemperature?, EmergencyTemperature?, FlashpointTemperature?)

% fixm_TrajectorySegment(Graph, TrajectorySegment, SegmentIdentifier?, SegmentType?)

% fixm_FlightIdentification(Graph, FlightIdentification, AircraftIdentification?, MajorCarrierIdentifier?, MarketingCarrierFlightIdentifier*)
fixm_FlightIdentification(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', val("DLH796"^^xsd:'string'), '$null$', []).

% fixm_LastContact(Graph, LastContact, ContactFrequency?, LastContactTime?, LastContactUnit?, Position?)

% plain_VOR(Graph, VOR, Type, Frequency, NavaidEquipment*)

% plain_AircraftCharacteristic(Graph, AircraftCharacteristic, VerticalSeparationCapability, Weight, AircraftLandingCategory, ConditionCombination*, Speed, WakeTurbulence, TypeAircraftICAO)
plain_AircraftCharacteristic(graph:'0_UML_all.ttl', 'aero:airm:1.0.0:ContextualModel:ATMBusinessTerms:AircraftCharacteristic', val("RVSM"^^xsd:'string'), xval(242000^^xsd:'decimal',"KG"^^xsd:'string'), val("C"^^xsd:'string'), [], xval(475^^xsd:'decimal',"KT"^^xsd:'string'), val("HIGH"^^xsd:'string'), val("A333"^^xsd:'string')).

% plain_EfplFlightDeparture(Graph, EfplFlightDeparture, EfplFlight*, EstimatedOffBlockTime)
plain_EfplFlightDeparture(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#ID_006', [], val(date_time(2019, 7, 3, 22, 9, 0, 0)^^xsd:'dateTime')).

% fixm_SpeedSchedule(Graph, SpeedSchedule, InitialSpeed?, SubsequentSpeed?)

% aixm_OrganisationAuthorityTimeSlice(Graph, OrganisationAuthorityTimeSlice, Name?, Designator?, Type?, Military?, Annotation*, Contact*, RelatedOrganisationAuthority*)

% plain_AirportHeliportUsage(Graph, AirportHeliportUsage, AirportHeliportAvailability*, Operation)
plain_AirportHeliportUsage(graph:'0_UML_all.ttl', 'https://www.plain.aero/releases/SESAR_Ext-1.0/RDF.xml#AHU1', [], val("ALL"^^xsd:'string')).

% fixm_FlightLevel(Graph, FlightLevel, Level?, Unit?)
fixm_FlightLevel(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025', val(11^^xsd:'integer'), val("S"^^xsd:'string')).
fixm_FlightLevel(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040', val(12256^^xsd:'integer'), val("SM"^^xsd:'string')).
fixm_FlightLevel(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031', val(110^^xsd:'integer'), val("SM"^^xsd:'string')).
fixm_FlightLevel(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037', val(12802^^xsd:'integer'), val("SM"^^xsd:'string')).
fixm_FlightLevel(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034', val(381^^xsd:'integer'), val("SM"^^xsd:'string')).

% plain_VerticalRate(Graph, VerticalRate, VerticalRate, FlightIdentification*)

% fixm_LateralOfftrack(Graph, LateralOfftrack, OfftrackDistance*, OfftrackReason?)

% fixm_AirspaceConstraint(Graph, AirspaceConstraint, AirspaceControlledEntryTime?, ConstrainedAirspace?)

% plain_SignificantPoint(Graph, SignificantPoint, SignificantPointInAirspace+)

% fixm_AtcUnitReference(Graph, AtcUnitReference, SectorIdentifier?, Delegated?)

% aixm_Point(Graph, Point, HorizontalAccuracy?, Annotation*)
aixm_Point(graph:'0_donlon-data.ttl', s1:'elpoint1EADH', '$null$', []).

% aixm_AircraftCharacteristic(Graph, AircraftCharacteristic, Type?, Engine?, NumberEngine?, TypeAircraftICAO?, AircraftLandingCategory?, WingSpan?, WingSpanInterpretation?, ClassWingSpan?, Weight?, WeightInterpretation?, Passengers?, PassengersInterpretation?, Speed?, SpeedInterpretation?, WakeTurbulence?, NavigationEquipment?, NavigationSpecification?, VerticalSeparationCapability?, AntiCollisionAndSeparationEquipment?, CommunicationEquipment?, SurveillanceEquipment?, Annotation*)

% fixm_DangerousGoodsPackage(Graph, DangerousGoodsPackage, DangerousGoodsQuantity?, PackageDimensions?, PackingInstructionNumber?, ProductName?, ProperShippingName?, ReportableQuantity?, SupplementaryInformation?, TechnicalName?, TypeOfPackaging?, UnNumber?, DangerousGoodsLimitation?, ShipmentType?, AllPackedInOne?, CompatibilityGroup?, ShipmentDimensions?, MarinePollutantIndicator?, RadioactiveMaterials?, HazardClass?, PackingGroup?, Temperatures?, OverpackIndicator?, SubsidiaryHazardClass*)

% fixm_DangerousGoodsDimensions(Graph, DangerousGoodsDimensions, GrossWeight?, NetWeight?, Volume?)

% plain_Timesheet(Graph, Timesheet, Excluded, EndTime, AirspaceActivation*, Day, DaylightSavingAdjust, EndDate, TimeReference, StartDate, StartTime)
plain_Timesheet(graph:'0_UML_all.ttl', file:'t_interval', val("NO"^^xsd:'string'), val("00:00"^^xsd:'string'), [], val("ANY"^^xsd:'string'), val("NO"^^xsd:'string'), val("01-10"^^xsd:'string'), val("UTC"^^xsd:'string'), val("01-05"^^xsd:'string'), val("00:00"^^xsd:'string')).
plain_Timesheet(graph:'0_UML_all.ttl', file:'t_interval2', val("NO"^^xsd:'string'), val("00:00"^^xsd:'string'), [], val("ANY"^^xsd:'string'), val("NO"^^xsd:'string'), val("01-10"^^xsd:'string'), val("UTC"^^xsd:'string'), val("01-05"^^xsd:'string'), val("00:00"^^xsd:'string')).

% plain_AirspaceActivation(Graph, AirspaceActivation, Airspace*, Status, Activity)
plain_AirspaceActivation(graph:'0_UML_all.ttl', file:'ACTV', [], val("ACTIVE"^^xsd:'string'), val("MILOPS"^^xsd:'string')).
plain_AirspaceActivation(graph:'0_UML_all.ttl', file:'ACTV2', [], val("ACTIVE"^^xsd:'string'), val("MILOPS"^^xsd:'string')).

% fixm_IcaoAerodromeReference(Graph, IcaoAerodromeReference, Code?)
fixm_IcaoAerodromeReference(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', val("VHHH"^^xsd:'string')).
fixm_IcaoAerodromeReference(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', val("EDDF"^^xsd:'string')).
fixm_IcaoAerodromeReference(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', val("EDDF"^^xsd:'string')).
fixm_IcaoAerodromeReference(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', val("ZGGG"^^xsd:'string')).

% fixm_FlightArrival(Graph, FlightArrival, ApproachFix?, ApproachTime?, ArrivalAerodrome?, ArrivalAerodromeAlternate*, ArrivalAerodromeOriginal?, ArrivalFix?, ArrivalFixTime?, ArrivalFleetPrioritization?, ArrivalSequenceNumber?, EarliestInBlockTime?, FiledRevisedDestinationAerodrome?, FiledRevisedDestinationStar?, RunwayPositionAndTime?, StandardInstrumentArrival?, StandPositionAndTime?, LandingLimits?)

% fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled?, Initial?)

% fixm_AerodromesOfDestination(Graph, AerodromesOfDestination, AerodromeOfDestination?, Alternate1?, Alternate2?, FiledRevisedDestinationAerodrome?)
fixm_AerodromesOfDestination(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', '$null$', '$null$').

% plain_NDB(Graph, NDB, Frequency, NavaidEquipment*)

% fixm_SurvivalCapabilities(Graph, SurvivalCapabilities, SurvivalEquipmentRemarks?, DinghyInformation?, EmergencyRadioCode*, LifeJacketCode*, SurvivalEquipmentCode*)

% plain_FlightIdentification(Graph, FlightIdentification, EfplFlight+, Callsign, AircraftIdentification)

% fixm_AircraftType(Graph, AircraftType)
fixm_AircraftType(graph:'0_FIXM_EDDF-VHHH-data.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_011').

% plain_Speed(Graph, Speed, GroundSpeed, FlightIdentification+)

% plain_UsageCondition(Graph, UsageCondition, AirportHeliportUsage*, Type, PriorPermission)

% plain_Surface(Graph, Surface, AirspaceVolume+)

% aixm_UsageCondition(Graph, UsageCondition, Type?, PriorPermission?, Selection?, Annotation*, Contact*)
aixm_UsageCondition(graph:'0_donlon-data.ttl', s1:'AHU_EADH_PERMIT', val("PERMIT"^^xsd:'string'), '$null$', s1:'agtayyat', [s1:'annotation1'], []).
aixm_UsageCondition(graph:'0_donlon-data.ttl', s2:'ID_ACT_14', val("PERMIT"^^xsd:'string'), '$null$', s2:'ID_ACT_15', [], []).

aixm_ElevatedSurface_Combined(Graph, ElevatedSurface, PatchList, HorizontalAccuracy, AnnotationList, HorizontalAccuracy, AnnotationList, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy),
  aixm_Surface_Combined(Graph,ElevatedSurface, PatchList, HorizontalAccuracy, AnnotationList) .

fixm_EfplPoint4D_Combined(Graph, EfplPoint4D, PosList, SrsName, Altitude, Time, PointRange, Altitude, Time, PointRange, FlightLevel) :-
  fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel),
  fixm_Point4D_Combined(Graph,EfplPoint4D, PosList, SrsName, Altitude, Time, PointRange) .

fixm_EfplTrajectoryRoutePair_Combined(Graph, EfplTrajectoryRoutePair, Trajectory, Route) :-
  fixm_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair),
  fixm_TrajectoryRoutePair(Graph, EfplTrajectoryRoutePair, Trajectory, Route) .

fixm_RoutePoint_Combined(Graph, RoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit, ConstraintList) :-
  fixm_RoutePoint(Graph, RoutePoint, ConstraintList),
  fixm_AbstractRoutePoint(Graph, RoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit) .

fixm_DangerousGoods_Combined(Graph, DangerousGoods, Provenance, GuidebookNumber, OnboardLocation, HandlingInformation, AircraftLimitation, AirWayBill, Shipment, PackageGroupList, ShippingInformation) :-
  fixm_DangerousGoods(Graph, DangerousGoods, GuidebookNumber, OnboardLocation, HandlingInformation, AircraftLimitation, AirWayBill, Shipment, PackageGroupList, ShippingInformation),
  fixm_Feature(Graph, DangerousGoods, Provenance) .

fixm_FlightEmergency_Combined(Graph, FlightEmergency, Provenance, ActionTaken, EmergencyDescription, Originator, OtherInformation, Phase, Contact) :-
  fixm_FlightEmergency(Graph, FlightEmergency, ActionTaken, EmergencyDescription, Originator, OtherInformation, Phase, Contact),
  fixm_Feature(Graph, FlightEmergency, Provenance) .

fixm_UnitBoundary_Combined(Graph, UnitBoundary, SectorIdentifier, Delegated, DownstreamUnit, UpstreamUnit, BoundaryCrossingProposed, BoundaryCrossingCoordinated, Handoff, UnitBoundaryIndicator) :-
  fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit, UpstreamUnit, BoundaryCrossingProposed, BoundaryCrossingCoordinated, Handoff, UnitBoundaryIndicator),
  fixm_AtcUnitReference(Graph, UnitBoundary, SectorIdentifier, Delegated) .

aixm_AirportHeliportContamination_Combined(Graph, AirportHeliportContamination, ObservationTime, Depth, FrictionCoefficient, FrictionEstimation, FrictionDevice, ObscuredLights, FurtherClearanceTime, FurtherTotalClearance, NextObservationTime, Proportion, CriticalRidgeList, AnnotationList, LayerList) :-
  aixm_AirportHeliportContamination(Graph, AirportHeliportContamination),
  aixm_SurfaceContamination(Graph, AirportHeliportContamination, ObservationTime, Depth, FrictionCoefficient, FrictionEstimation, FrictionDevice, ObscuredLights, FurtherClearanceTime, FurtherTotalClearance, NextObservationTime, Proportion, CriticalRidgeList, AnnotationList, LayerList) .

aixm_TelephoneContact_Combined(Graph, TelephoneContact, AnnotationList, SpecialDateAuthorityList, TimeIntervalList, Voice, Facsimile) :-
  aixm_TelephoneContact(Graph, TelephoneContact, Voice, Facsimile),
  aixm_PropertiesWithSchedule(Graph, TelephoneContact, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) .

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

aixm_AirportHeliportUsage_Combined(Graph, AirportHeliportUsage, Type, PriorPermission, Selection, AnnotationList, ContactList, Operation) :-
  aixm_AirportHeliportUsage(Graph, AirportHeliportUsage, Operation),
  aixm_UsageCondition(Graph, AirportHeliportUsage, Type, PriorPermission, Selection, AnnotationList, ContactList) .

fixm_EfplTrajectoryPoint_Combined(Graph, EfplTrajectoryPoint, AltimeterSetting, PredictedAirspeed, PredictedGroundspeed, MetData, Point, TrajectoryChangeList, TrajectoryChangeTypeList, ReferencePoint, AerodromeIdentifier, DistanceFromTakeOff, EfplEstimatedSpeed, ElapsedTime, GrossWeight, TrajectoryPointType, TrajectoryPointRole, InboundSegment) :-
  fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier, DistanceFromTakeOff, EfplEstimatedSpeed, ElapsedTime, GrossWeight, TrajectoryPointType, TrajectoryPointRole, InboundSegment),
  fixm_TrajectoryPoint(Graph, EfplTrajectoryPoint, AltimeterSetting, PredictedAirspeed, PredictedGroundspeed, MetData, Point, TrajectoryChangeList, TrajectoryChangeTypeList, ReferencePoint) .

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

aixm_PostalAddress_Combined(Graph, PostalAddress, AnnotationList, SpecialDateAuthorityList, TimeIntervalList, DeliveryPoint, City, AdministrativeArea, PostalCode, Country) :-
  aixm_PostalAddress(Graph, PostalAddress, DeliveryPoint, City, AdministrativeArea, PostalCode, Country),
  aixm_PropertiesWithSchedule(Graph, PostalAddress, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) .

aixm_AltimeterSourceStatus_Combined(Graph, AltimeterSourceStatus, AnnotationList, SpecialDateAuthorityList, TimeIntervalList, OperationalStatus) :-
  aixm_AltimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus),
  aixm_PropertiesWithSchedule(Graph, AltimeterSourceStatus, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) .

fixm_EfplRoute_Combined(Graph, EfplRoute, Provenance, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTimeList, ExpandedRoute, ClimbSchedule, DescentSchedule, SegmentList, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTimeList, ExpandedRoute, ClimbSchedule, DescentSchedule, SegmentList, EfplFlightRules) :-
  fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules),
  fixm_Route_Combined(Graph,EfplRoute, Provenance, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTimeList, ExpandedRoute, ClimbSchedule, DescentSchedule, SegmentList) .

fixm_RadioCommunicationFailure_Combined(Graph, RadioCommunicationFailure, Provenance, RadioFailureRemarks, RemainingComCapability, Contact) :-
  fixm_RadioCommunicationFailure(Graph, RadioCommunicationFailure, RadioFailureRemarks, RemainingComCapability, Contact),
  fixm_Feature(Graph, RadioCommunicationFailure, Provenance) .

aixm_AirportHeliportAvailability_Combined(Graph, AirportHeliportAvailability, AnnotationList, SpecialDateAuthorityList, TimeIntervalList, OperationalStatus, Warning, UsageList) :-
  aixm_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus, Warning, UsageList),
  aixm_PropertiesWithSchedule(Graph, AirportHeliportAvailability, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) .

fixm_TargetMultiTime_Combined(Graph, TargetMultiTime, Actual, Estimated, Target) :-
  fixm_TargetMultiTime(Graph, TargetMultiTime, Target),
  fixm_MultiTime(Graph, TargetMultiTime, Actual, Estimated) .

fixm_FlightDeparture_Combined(Graph, FlightDeparture, Provenance, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodromeList, TakeoffWeight, DepartureTimes) :-
  fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodromeList, TakeoffWeight, DepartureTimes),
  fixm_Feature(Graph, FlightDeparture, Provenance) .

fixm_EfplFlightDeparture_Combined(Graph, EfplFlightDeparture, Provenance, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodromeList, TakeoffWeight, DepartureTimes, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodromeList, TakeoffWeight, DepartureTimes, EstimatedOffBlockTime, TaxiTime) :-
  fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime, TaxiTime),
  fixm_FlightDeparture_Combined(Graph,EfplFlightDeparture, Provenance, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodromeList, TakeoffWeight, DepartureTimes) .

fixm_ExpandedRoutePoint_Combined(Graph, ExpandedRoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit, EstimatedLevel, EstimatedTime, ConstraintList) :-
  fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel, EstimatedTime, ConstraintList),
  fixm_AbstractRoutePoint(Graph, ExpandedRoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit) .

aixm_ConditionCombination_Combined(Graph, ConditionCombination, AnnotationList, SpecialDateAuthorityList, TimeIntervalList, LogicalOperator, FlightList, AircraftList, WeatherList, SubConditionList) :-
  aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator, FlightList, AircraftList, WeatherList, SubConditionList),
  aixm_PropertiesWithSchedule(Graph, ConditionCombination, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) .

aixm_ElevatedPoint_Combined(Graph, ElevatedPoint, HorizontalAccuracy, AnnotationList, HorizontalAccuracy, AnnotationList, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy),
  aixm_Point_Combined(Graph,ElevatedPoint, HorizontalAccuracy, AnnotationList) .

aixm_AirportHeliportResponsibilityOrganisation_Combined(Graph, AirportHeliportResponsibilityOrganisation, AnnotationList, SpecialDateAuthorityList, TimeIntervalList, Role, TheOrganisationAuthority) :-
  aixm_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role, TheOrganisationAuthority),
  aixm_PropertiesWithSchedule(Graph, AirportHeliportResponsibilityOrganisation, AnnotationList, SpecialDateAuthorityList, TimeIntervalList) .

fixm_Point4D_Combined(Graph, Point4D, PosList, SrsName, Altitude, Time, PointRange) :-
  fixm_Point4D(Graph, Point4D, Altitude, Time, PointRange),
  fixm_GeographicLocation(Graph, Point4D, PosList, SrsName) .

fixm_Flight_Combined(Graph, Flight, Provenance, ControllingUnit, ExtensionsList, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoodsList, RankedTrajectoriesList, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandlingList) :-
  fixm_Flight(Graph, Flight, ControllingUnit, ExtensionsList, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoodsList, RankedTrajectoriesList, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandlingList),
  fixm_Feature(Graph, Flight, Provenance) .

gml_Surface_Combined(Graph, Surface, PatchList) :-
  gml_Surface(Graph, Surface, PatchList),
  gml_SurfacePatch(Graph, Surface) .

fixm_Route_Combined(Graph, Route, Provenance, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTimeList, ExpandedRoute, ClimbSchedule, DescentSchedule, SegmentList) :-
  fixm_Route(Graph, Route, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTimeList, ExpandedRoute, ClimbSchedule, DescentSchedule, SegmentList),
  fixm_Feature(Graph, Route, Provenance) .

fixm_EfplFlight_Combined(Graph, EfplFlight, Provenance, ControllingUnit, ExtensionsList, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoodsList, RankedTrajectoriesList, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandlingList, ControllingUnit, ExtensionsList, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoodsList, RankedTrajectoriesList, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandlingList, IfplId, TotalEstimatedElapsedTime, AerodromesOfDestination, EfplSpecialHandling, EfplFiledTrajectory, EfplAcceptedTrajectory, OtherInformation, FlightPerformanceData) :-
  fixm_EfplFlight(Graph, EfplFlight, IfplId, TotalEstimatedElapsedTime, AerodromesOfDestination, EfplSpecialHandling, EfplFiledTrajectory, EfplAcceptedTrajectory, OtherInformation, FlightPerformanceData),
  fixm_Flight_Combined(Graph,EfplFlight, Provenance, ControllingUnit, ExtensionsList, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoodsList, RankedTrajectoriesList, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandlingList) .

fixm_AircraftPosition_Combined(Graph, AircraftPosition, Provenance, Altitude, Position, PositionTime, Track, ActualSpeed, NextPosition, ReportSource, FollowingPosition) :-
  fixm_AircraftPosition(Graph, AircraftPosition, Altitude, Position, PositionTime, Track, ActualSpeed, NextPosition, ReportSource, FollowingPosition),
  fixm_Feature(Graph, AircraftPosition, Provenance) .

fixm_LastContact_Combined(Graph, LastContact, Provenance, ContactFrequency, LastContactTime, LastContactUnit, Position) :-
  fixm_LastContact(Graph, LastContact, ContactFrequency, LastContactTime, LastContactUnit, Position),
  fixm_Feature(Graph, LastContact, Provenance) .

aixm_Point_Combined(Graph, Point, HorizontalAccuracy, AnnotationList) :-
  aixm_Point(Graph, Point, HorizontalAccuracy, AnnotationList),
  gml_Point(Graph, Point) .

fixm_IcaoAerodromeReference_Combined(Graph, IcaoAerodromeReference, Code) :-
  fixm_IcaoAerodromeReference(Graph, IcaoAerodromeReference, Code),
  fixm_AerodromeReference(Graph, IcaoAerodromeReference) .

fixm_FlightArrival_Combined(Graph, FlightArrival, Provenance, ApproachFix, ApproachTime, ArrivalAerodrome, ArrivalAerodromeAlternateList, ArrivalAerodromeOriginal, ArrivalFix, ArrivalFixTime, ArrivalFleetPrioritization, ArrivalSequenceNumber, EarliestInBlockTime, FiledRevisedDestinationAerodrome, FiledRevisedDestinationStar, RunwayPositionAndTime, StandardInstrumentArrival, StandPositionAndTime, LandingLimits) :-
  fixm_FlightArrival(Graph, FlightArrival, ApproachFix, ApproachTime, ArrivalAerodrome, ArrivalAerodromeAlternateList, ArrivalAerodromeOriginal, ArrivalFix, ArrivalFixTime, ArrivalFleetPrioritization, ArrivalSequenceNumber, EarliestInBlockTime, FiledRevisedDestinationAerodrome, FiledRevisedDestinationStar, RunwayPositionAndTime, StandardInstrumentArrival, StandPositionAndTime, LandingLimits),
  fixm_Feature(Graph, FlightArrival, Provenance) .

fixm_ExtendedMultiTime_Combined(Graph, ExtendedMultiTime, Actual, Estimated, Target, Target, Controlled, Initial) :-
  fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled, Initial),
  fixm_TargetMultiTime_Combined(Graph,ExtendedMultiTime, Actual, Estimated, Target) .

