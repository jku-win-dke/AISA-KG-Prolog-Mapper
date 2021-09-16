% :- module(c,[]).

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

:- rdf_load('dataset.trig') .

/* for prefix handling: declare predicates that have RDF terms as arguments
see: https://www.swi-prolog.org/pldoc/man?predicate=rdf_meta/1
*/
:- rdf_meta
  subClassOf(r,r)

  % fixm_PostalAddress(Graph, PostalAddress, AdministrativeArea, PostalCode, DeliveryPoint, CountryCode, CountryName, City)
  ,fixm_PostalAddress(r,r,t,t,t,t,t,t)

  % fixm_NavigationCapabilities(Graph, NavigationCapabilities, OtherNavigationCapabilities, PerformanceBasedCode, NavigationCode)
  ,fixm_NavigationCapabilities(r,r,t,t,t)

  % fixm_GroundspeedRange(Graph, GroundspeedRange, LowerSpeed, UpperSpeed)
  ,fixm_GroundspeedRange(r,r,t,t)

  % aixm_Note(Graph, Note, PropertyName, Purpose, TranslatedNote)
  ,aixm_Note(r,r,t,t,t)

  % fixm_Pointout(Graph, Pointout, OriginatingUnit, ReceivingUnit)
  ,fixm_Pointout(r,r,t,t)

  % fixm_VerticalRange(Graph, VerticalRange, LowerBound, UpperBound)
  ,fixm_VerticalRange(r,r,t,t)

  % fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel, EstimatedTime, Constraint)
  ,fixm_ExpandedRoutePoint(r,r,t,t,t)

  % aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy)
  ,aixm_ElevatedSurface(r,r,t,t,t,t)

  % fixm_Dimensions(Graph, Dimensions, Height, Length, Width)
  ,fixm_Dimensions(r,r,t,t,t)

  % fixm_StandPositionAndTime(Graph, StandPositionAndTime, StandName, StandTime, TerminalName)
  ,fixm_StandPositionAndTime(r,r,t,t,t)

  % fixm_RouteSegment(Graph, RouteSegment, Airway, RoutePoint)
  ,fixm_RouteSegment(r,r,t,t)

  % aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator, Flight, Aircraft, Weather, SubCondition)
  ,aixm_ConditionCombination(r,r,t,t,t,t,t)

  % aixm_SurfaceContaminationLayer(Graph, SurfaceContaminationLayer, LayerOrder, Type, Extent, Annotation)
  ,aixm_SurfaceContaminationLayer(r,r,t,t,t,t)

  % fixm_Organization(Graph, Organization, Name, OtherOrganization, Contact)
  ,fixm_Organization(r,r,t,t,t)

  % aixm_OrganisationAuthorityAssociation(Graph, OrganisationAuthorityAssociation, Type, Annotation, TheOrganisationAuthority)
  ,aixm_OrganisationAuthorityAssociation(r,r,t,t,t)

  % aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy)
  ,aixm_ElevatedPoint(r,r,t,t,t,t)

  % fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel)
  ,fixm_EfplPoint4D(r,r,t)

  % fixm_AircraftOperator(Graph, AircraftOperator, OperatingOrganization, OperatorCategory)
  ,fixm_AircraftOperator(r,r,t,t)

  % gml_Point(Graph, Point)
  ,gml_Point(r,r)

  % fixm_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair)
  ,fixm_EfplTrajectoryRoutePair(r,r)

  % fixm_RoutePoint(Graph, RoutePoint, Constraint)
  ,fixm_RoutePoint(r,r,t)

  % fixm_BeaconCodeAssignment(Graph, BeaconCodeAssignment, CurrentBeaconCode, PreviousBeaconCode, ReassignedBeaconCode, ReassigningUnit)
  ,fixm_BeaconCodeAssignment(r,r,t,t,t,t)

  % fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile)
  ,fixm_FlightPerformanceData(r,r,t,t)

  % fixm_ExpandedRoute(Graph, ExpandedRoute, RoutePoint)
  ,fixm_ExpandedRoute(r,r,t)

  % fixm_RouteConstraintOrPreference(Graph, RouteConstraintOrPreference, ConstraintType)
  ,fixm_RouteConstraintOrPreference(r,r,t)

  % fixm_DeclarationText(Graph, DeclarationText, Compliance, Consignor, Shipper)
  ,fixm_DeclarationText(r,r,t,t,t)

  % fixm_EstimatedElapsedTime(Graph, EstimatedElapsedTime, ElapsedTime, Location)
  ,fixm_EstimatedElapsedTime(r,r,t,t)

  % fixm_ReportedTime(Graph, ReportedTime, Provenance, Time)
  ,fixm_ReportedTime(r,r,t,t)

  % fixm_GeographicLocation(Graph, GeographicLocation, Pos, SrsName)
  ,fixm_GeographicLocation(r,r,t,t)

  % aixm_LinguisticNote(Graph, LinguisticNote, Note)
  ,aixm_LinguisticNote(r,r,t)

  % aixm_Meteorology(Graph, Meteorology, FlightConditions, Visibility, VisibilityInterpretation, RunwayVisualRange, RunwayVisualRangeInterpretation, Annotation)
  ,aixm_Meteorology(r,r,t,t,t,t,t,t)

  % fixm_PointRange(Graph, PointRange, LateralRange, VerticalRange, TemporalRange)
  ,fixm_PointRange(r,r,t,t,t)

  % aixm_City(Graph, City, Name, Annotation)
  ,aixm_City(r,r,t,t)

  % aixm_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role, TheOrganisationAuthority)
  ,aixm_AirportHeliportResponsibilityOrganisation(r,r,t,t)

  % fixm_AirspeedRange(Graph, AirspeedRange, LowerSpeed, UpperSpeed)
  ,fixm_AirspeedRange(r,r,t,t)

  % fixm_RankedTrajectory(Graph, RankedTrajectory, Identifier, MaximumAcceptableDelay, AssignedIndicator, RouteTrajectoryPair)
  ,fixm_RankedTrajectory(r,r,t,t,t,t)

  % fixm_TrajectoryPointRole(Graph, TrajectoryPointRole, BottomOfClimb, BottomOfDescent, BoundaryPoint, FromGATToOAT, FromIFRToVFR, FromOATToGat, FromVFRToIFR, TopOfClimb, TopOfDescent)
  ,fixm_TrajectoryPointRole(r,r,t,t,t,t,t,t,t,t,t)

  % fixm_CommunicationCapabilities(Graph, CommunicationCapabilities, OtherCommunicationCapabilities, OtherDataLinkCapabilities, DataLinkCode, SelectiveCallingCode, CommunicationCode)
  ,fixm_CommunicationCapabilities(r,r,t,t,t,t,t)

  % fixm_Dinghy(Graph, Dinghy, Quantity, TotalCapacity, Covered, Colour)
  ,fixm_Dinghy(r,r,t,t,t,t)

  % aixm_ContactInformation(Graph, ContactInformation, Name, Title, Annotation, NetworkNode, Address, PhoneFax)
  ,aixm_ContactInformation(r,r,t,t,t,t,t,t)

  % fixm_PlannedReportingPosition(Graph, PlannedReportingPosition, Position, PositionAltitude, PositionEstimatedTime)
  ,fixm_PlannedReportingPosition(r,r,t,t,t)

  % fixm_SignificantPoint(Graph, SignificantPoint)
  ,fixm_SignificantPoint(r,r)

  % fixm_SupplementalData(Graph, SupplementalData, FuelEndurance, PersonsOnBoard, PilotInCommand)
  ,fixm_SupplementalData(r,r,t,t,t)

  % fixm_DangerousGoods(Graph, DangerousGoods, GuidebookNumber, OnboardLocation, HandlingInformation, AircraftLimitation, AirWayBill, Shipment, PackageGroup, ShippingInformation)
  ,fixm_DangerousGoods(r,r,t,t,t,t,t,t,t,t)

  % fixm_DangerousGoodsPackageGroup(Graph, DangerousGoodsPackageGroup, ShipmentDimensions, DangerousGoodsPackage, ShipmentUseIndicator)
  ,fixm_DangerousGoodsPackageGroup(r,r,t,t,t)

  % fixm_OfftrackDistance(Graph, OfftrackDistance, Distance, Direction)
  ,fixm_OfftrackDistance(r,r,t,t)

  % fixm_Handoff(Graph, Handoff, ReceivingUnit, TransferringUnit, CoordinationStatus)
  ,fixm_Handoff(r,r,t,t,t)

  % fixm_TrajectoryChange(Graph, TrajectoryChange, ConstrainedAirspace, SpecialActivityAirspace)
  ,fixm_TrajectoryChange(r,r,t,t)

  % fixm_ContactInformation(Graph, ContactInformation, Name, Title, OnlineContact, PhoneFax, Address)
  ,fixm_ContactInformation(r,r,t,t,t,t,t)

  % aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability)
  ,aixm_AirportHeliportTimeSlice(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_Point4D(Graph, Point4D, Altitude, Time, PointRange)
  ,fixm_Point4D(r,r,t,t,t)

  % fixm_AbstractRoutePoint(Graph, AbstractRoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit)
  ,fixm_AbstractRoutePoint(r,r,t,t,t,t,t)

  % aixm_Ridge(Graph, Ridge, Side, Distance, Depth, Annotation)
  ,aixm_Ridge(r,r,t,t,t,t)

  % fixm_DepartureActivityTimes(Graph, DepartureActivityTimes, BoardingTime, DeIcingTime, GroundHandlingTime, StartupTime)
  ,fixm_DepartureActivityTimes(r,r,t,t,t,t)

  % fixm_EnRouteDiversion(Graph, EnRouteDiversion, DiversionRecoveryInformation)
  ,fixm_EnRouteDiversion(r,r,t)

  % fixm_ActualSpeed(Graph, ActualSpeed, Calculated, PilotReported, Surveillance)
  ,fixm_ActualSpeed(r,r,t,t,t)

  % fixm_FlightEmergency(Graph, FlightEmergency, ActionTaken, EmergencyDescription, Originator, OtherInformation, Phase, Contact)
  ,fixm_FlightEmergency(r,r,t,t,t,t,t,t)

  % fixm_Flight(Graph, Flight, ControllingUnit, Extensions, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoods, RankedTrajectories, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandling)
  ,fixm_Flight(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % aixm_PropertiesWithSchedule(Graph, PropertiesWithSchedule, Annotation, SpecialDateAuthority, TimeInterval)
  ,aixm_PropertiesWithSchedule(r,r,t,t,t)

  % gml_Surface(Graph, Surface, Patch)
  ,gml_Surface(r,r,t)

  % fixm_ClearedFlightInformation(Graph, ClearedFlightInformation, ClearedFlightLevel, ClearedSpeed, Heading, OfftrackClearance, RateOfClimbDescend, DirectRouting)
  ,fixm_ClearedFlightInformation(r,r,t,t,t,t,t,t)

  % fixm_TrajectoryRoutePair(Graph, TrajectoryRoutePair, Trajectory, Route)
  ,fixm_TrajectoryRoutePair(r,r,t,t)

  % fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit, UpstreamUnit, BoundaryCrossingProposed, BoundaryCrossingCoordinated, Handoff, UnitBoundaryIndicator)
  ,fixm_UnitBoundary(r,r,t,t,t,t,t,t)

  % aixm_SurfaceContamination(Graph, SurfaceContamination, ObservationTime, Depth, FrictionCoefficient, FrictionEstimation, FrictionDevice, ObscuredLights, FurtherClearanceTime, FurtherTotalClearance, NextObservationTime, Proportion, CriticalRidge, Annotation, Layer)
  ,aixm_SurfaceContamination(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_MeteorologicalData(Graph, MeteorologicalData, Temperature, WindDirection, WindSpeed)
  ,fixm_MeteorologicalData(r,r,t,t,t)

  % aixm_OrganisationAuthority(Graph, OrganisationAuthority, TimeSlice)
  ,aixm_OrganisationAuthority(r,r,t)

  % fixm_TelephoneContact(Graph, TelephoneContact, Voice, Facimile)
  ,fixm_TelephoneContact(r,r,t,t)

  % fixm_ShippingInformation(Graph, ShippingInformation, AerodromeOfLoading, AerodromeOfUnloading, DangerousGoodsScreeningLocation, DepartureCountry, DestinationCountry, OriginCountry, ShipmentAuthorizations, SubsidiaryHazardClassAndDivision, SupplementaryInformation, TransferAerodromes, DeclarationText, Consignee, Shipper)
  ,fixm_ShippingInformation(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % aixm_AirportHeliportContamination(Graph, AirportHeliportContamination)
  ,aixm_AirportHeliportContamination(r,r)

  % fixm_OtherInformation(Graph, OtherInformation, ReplacementFlightPlanIndicator, RunwayVisualRange)
  ,fixm_OtherInformation(r,r,t,t)

  % fixm_DinghyColour(Graph, DinghyColour)
  ,fixm_DinghyColour(r,r)

  % fixm_CpdlcConnection(Graph, CpdlcConnection, ReceivingUnitFrequency, AtnLogonParameters, SendCpldcIndicator, ConnectionStatus, FrequencyUsage, Fans1ALogonParameters)
  ,fixm_CpdlcConnection(r,r,t,t,t,t,t,t)

  % aixm_TelephoneContact(Graph, TelephoneContact, Voice, Facsimile)
  ,aixm_TelephoneContact(r,r,t,t)

  % fixm_Route(Graph, Route, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTime, ExpandedRoute, ClimbSchedule, DescentSchedule, Segment)
  ,fixm_Route(r,r,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_Person(Graph, Person, Name, Contact)
  ,fixm_Person(r,r,t,t)

  % fixm_EfplFlight(Graph, EfplFlight, IfplId, TotalEstimatedElapsedTime, AerodromesOfDestination, EfplSpecialHandling, EfplFiledTrajectory, EfplAcceptedTrajectory, OtherInformation, FlightPerformanceData)
  ,fixm_EfplFlight(r,r,t,t,t,t,t,t,t,t)

  % fixm_Originator(Graph, Originator)
  ,fixm_Originator(r,r)

  % fixm_FlightStatus(Graph, FlightStatus, AirborneHold, Airfile, Accepted, FlightCycle, MissedApproach, Suspended)
  ,fixm_FlightStatus(r,r,t,t,t,t,t,t)

  % fixm_IdentifiedUnitReference(Graph, IdentifiedUnitReference, UnitIdentifier)
  ,fixm_IdentifiedUnitReference(r,r,t)

  % fixm_Radionuclide(Graph, Radionuclide, PhysicalChemicalForm, RadionuclideId, RadionuclideName, LowDispersibleMaterialIndicator, Activity, SpecialFormIndicator)
  ,fixm_Radionuclide(r,r,t,t,t,t,t,t)

  % aixm_OnlineContact(Graph, OnlineContact, Network, Linkage, Protocol, EMail)
  ,aixm_OnlineContact(r,r,t,t,t,t)

  % fixm_StructuredPostalAddress(Graph, StructuredPostalAddress)
  ,fixm_StructuredPostalAddress(r,r)

  % fixm_AircraftPosition(Graph, AircraftPosition, Altitude, Position, PositionTime, Track, ActualSpeed, NextPosition, ReportSource, FollowingPosition)
  ,fixm_AircraftPosition(r,r,t,t,t,t,t,t,t,t)

  % aixm_AirportHeliportUsage(Graph, AirportHeliportUsage, Operation)
  ,aixm_AirportHeliportUsage(r,r,t)

  % aixm_Timesheet(Graph, Timesheet, TimeReference, StartDate, EndDate, Day, DayTil, StartTime, StartEvent, StartTimeRelativeEvent, StartEventInterpretation, EndTime, EndEvent, EndTimeRelativeEvent, EndEventInterpretation, DaylightSavingAdjust, Excluded, Annotation)
  ,aixm_Timesheet(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % gml_SurfacePatch(Graph, SurfacePatch)
  ,gml_SurfacePatch(r,r)

  % fixm_MultiTime(Graph, MultiTime, Actual, Estimated)
  ,fixm_MultiTime(r,r,t,t)

  % aixm_FlightCharacteristic(Graph, FlightCharacteristic, Type, Rule, Status, Military, Origin, Purpose, Annotation)
  ,aixm_FlightCharacteristic(r,r,t,t,t,t,t,t,t)

  % fixm_Provenance(Graph, Provenance, Timestamp, Centre, Source, System)
  ,fixm_Provenance(r,r,t,t,t,t)

  % aixm_AirportHeliport(Graph, AirportHeliport, TimeSlice)
  ,aixm_AirportHeliport(r,r,t)

  % fixm_TrajectoryPoint(Graph, TrajectoryPoint, AltimeterSetting, PredictedAirspeed, PredictedGroundspeed, MetData, Point, TrajectoryChange, TrajectoryChangeType, ReferencePoint)
  ,fixm_TrajectoryPoint(r,r,t,t,t,t,t,t,t,t)

  % fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier, DistanceFromTakeOff, EfplEstimatedSpeed, ElapsedTime, GrossWeight, TrajectoryPointType, TrajectoryPointRole, InboundSegment)
  ,fixm_EfplTrajectoryPoint(r,r,t,t,t,t,t,t,t,t)

  % fixm_Temperatures(Graph, Temperatures, ControlTemperature, EmergencyTemperature, FlashpointTemperature)
  ,fixm_Temperatures(r,r,t,t,t)

  % fixm_TrajectorySegment(Graph, TrajectorySegment, SegmentIdentifier, SegmentType)
  ,fixm_TrajectorySegment(r,r,t,t)

  % fixm_RunwayPositionAndTime(Graph, RunwayPositionAndTime, RunwayName, RunwayTime)
  ,fixm_RunwayPositionAndTime(r,r,t,t)

  % fixm_Feature(Graph, Feature, Provenance)
  ,fixm_Feature(r,r,t)

  % fixm_FlightIdentification(Graph, FlightIdentification, AircraftIdentification, MajorCarrierIdentifier, MarketingCarrierFlightIdentifier)
  ,fixm_FlightIdentification(r,r,t,t,t)

  % fixm_LastContact(Graph, LastContact, ContactFrequency, LastContactTime, LastContactUnit, Position)
  ,fixm_LastContact(r,r,t,t,t,t)

  % fixm_ElapsedTimeLocation(Graph, ElapsedTimeLocation)
  ,fixm_ElapsedTimeLocation(r,r)

  % aixm_Surface(Graph, Surface, HorizontalAccuracy, Annotation)
  ,aixm_Surface(r,r,t,t)

  % gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition)
  ,gml_TimePeriod(r,r,t,t)

  % fixm_AircraftCapabilities(Graph, AircraftCapabilities, Survival, Communication, Navigation, Surveillance, StandardCapabilities)
  ,fixm_AircraftCapabilities(r,r,t,t,t,t,t)

  % fixm_SpeedSchedule(Graph, SpeedSchedule, InitialSpeed, SubsequentSpeed)
  ,fixm_SpeedSchedule(r,r,t,t)

  % aixm_OrganisationAuthorityTimeSlice(Graph, OrganisationAuthorityTimeSlice, Name, Designator, Type, Military, Annotation, Contact, RelatedOrganisationAuthority)
  ,aixm_OrganisationAuthorityTimeSlice(r,r,t,t,t,t,t,t,t)

  % fixm_EnRoute(Graph, EnRoute, AlternateAerodrome, FleetPrioritization, BoundaryCrossings, CpdlcConnection, BeaconCodeAssignment, Cleared, ControlElement, Pointout, Position)
  ,fixm_EnRoute(r,r,t,t,t,t,t,t,t,t,t)

  % fixm_FlightLevel(Graph, FlightLevel, Level, Unit)
  ,fixm_FlightLevel(r,r,t,t)

  % fixm_LateralOfftrack(Graph, LateralOfftrack, OfftrackDistance, OfftrackReason)
  ,fixm_LateralOfftrack(r,r,t,t)

  % fixm_TemporalRange(Graph, TemporalRange, Earliest, Latest)
  ,fixm_TemporalRange(r,r,t,t)

  % fixm_Aircraft(Graph, Aircraft, AircraftColours, AircraftQuantity, EngineType, AircraftAddress, Capabilities, Registration, AircraftType, WakeTurbulence, AircraftPerformance)
  ,fixm_Aircraft(r,r,t,t,t,t,t,t,t,t,t)

  % fixm_OnlineContact(Graph, OnlineContact, Email)
  ,fixm_OnlineContact(r,r,t)

  % fixm_AirspaceConstraint(Graph, AirspaceConstraint, AirspaceControlledEntryTime, ConstrainedAirspace)
  ,fixm_AirspaceConstraint(r,r,t,t)

  % fixm_TimeSequence(Graph, TimeSequence, Approval, Begin, End, Ready, Request)
  ,fixm_TimeSequence(r,r,t,t,t,t,t)

  % fixm_AdditionalHandlingInformation(Graph, AdditionalHandlingInformation, ResponsibleAgent)
  ,fixm_AdditionalHandlingInformation(r,r,t)

  % fixm_AtcUnitReference(Graph, AtcUnitReference, SectorIdentifier, Delegated)
  ,fixm_AtcUnitReference(r,r,t,t)

  % fixm_Extension(Graph, Extension)
  ,fixm_Extension(r,r)

  % fixm_SurveillanceCapabilities(Graph, SurveillanceCapabilities, OtherSurveillanceCapabilities, SurveillanceCode)
  ,fixm_SurveillanceCapabilities(r,r,t,t)

  % fixm_Trajectory(Graph, Trajectory, TrajectoryPoint)
  ,fixm_Trajectory(r,r,t)

  % aixm_AltimeterSourceTimeSlice(Graph, AltimeterSourceTimeSlice, IsRemote, IsPrimary, Availability, Annotation)
  ,aixm_AltimeterSourceTimeSlice(r,r,t,t,t,t)

  % aixm_Point(Graph, Point, HorizontalAccuracy, Annotation)
  ,aixm_Point(r,r,t,t)

  % aixm_AircraftCharacteristic(Graph, AircraftCharacteristic, Type, Engine, NumberEngine, TypeAircraftICAO, AircraftLandingCategory, WingSpan, WingSpanInterpretation, ClassWingSpan, Weight, WeightInterpretation, Passengers, PassengersInterpretation, Speed, SpeedInterpretation, WakeTurbulence, NavigationEquipment, NavigationSpecification, VerticalSeparationCapability, AntiCollisionAndSeparationEquipment, CommunicationEquipment, SurveillanceEquipment, Annotation)
  ,aixm_AircraftCharacteristic(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % aixm_PostalAddress(Graph, PostalAddress, DeliveryPoint, City, AdministrativeArea, PostalCode, Country)
  ,aixm_PostalAddress(r,r,t,t,t,t,t)

  % fixm_DangerousGoodsPackage(Graph, DangerousGoodsPackage, DangerousGoodsQuantity, PackageDimensions, PackingInstructionNumber, ProductName, ProperShippingName, ReportableQuantity, SupplementaryInformation, TechnicalName, TypeOfPackaging, UnNumber, DangerousGoodsLimitation, ShipmentType, AllPackedInOne, CompatibilityGroup, ShipmentDimensions, MarinePollutantIndicator, RadioactiveMaterials, HazardClass, PackingGroup, Temperatures, OverpackIndicator, SubsidiaryHazardClass)
  ,fixm_DangerousGoodsPackage(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_LastPositionReport(Graph, LastPositionReport, DeterminationMethod, Position, TimeAtPosition)
  ,fixm_LastPositionReport(r,r,t,t,t)

  % aixm_AltimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus)
  ,aixm_AltimeterSourceStatus(r,r,t)

  % fixm_DangerousGoodsDimensions(Graph, DangerousGoodsDimensions, GrossWeight, NetWeight, Volume)
  ,fixm_DangerousGoodsDimensions(r,r,t,t,t)

  % fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules)
  ,fixm_EfplRoute(r,r,t)

  % fixm_CoordinationStatus(Graph, CoordinationStatus, AbrogationReason, CoordinationStatus, NonStandardCommunicationReason, ReleaseConditions)
  ,fixm_CoordinationStatus(r,r,t,t,t,t)

  % fixm_BoundaryCrossing(Graph, BoundaryCrossing, Altitude, CrossingPoint, CrossingSpeed, CrossingTime, Offtrack, AltitudeInTransition)
  ,fixm_BoundaryCrossing(r,r,t,t,t,t,t,t)

  % fixm_IcaoAerodromeReference(Graph, IcaoAerodromeReference, Code)
  ,fixm_IcaoAerodromeReference(r,r,t)

  % fixm_RadioCommunicationFailure(Graph, RadioCommunicationFailure, RadioFailureRemarks, RemainingComCapability, Contact)
  ,fixm_RadioCommunicationFailure(r,r,t,t,t)

  % aixm_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus, Warning, Usage)
  ,aixm_AirportHeliportAvailability(r,r,t,t,t)

  % fixm_FlightArrival(Graph, FlightArrival, ApproachFix, ApproachTime, ArrivalAerodrome, ArrivalAerodromeAlternate, ArrivalAerodromeOriginal, ArrivalFix, ArrivalFixTime, ArrivalFleetPrioritization, ArrivalSequenceNumber, EarliestInBlockTime, FiledRevisedDestinationAerodrome, FiledRevisedDestinationStar, RunwayPositionAndTime, StandardInstrumentArrival, StandPositionAndTime, LandingLimits)
  ,fixm_FlightArrival(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_RadioactiveMaterial(Graph, RadioactiveMaterial, CriticalitySafetyIndex, TransportIndex, FissileExceptedIndicator, Category, Radionuclide)
  ,fixm_RadioactiveMaterial(r,r,t,t,t,t,t)

  % fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled, Initial)
  ,fixm_ExtendedMultiTime(r,r,t,t)

  % fixm_ControlElement(Graph, ControlElement)
  ,fixm_ControlElement(r,r)

  % fixm_AerodromesOfDestination(Graph, AerodromesOfDestination, AerodromeOfDestination, Alternate1, Alternate2, FiledRevisedDestinationAerodrome)
  ,fixm_AerodromesOfDestination(r,r,t,t,t,t)

  % fixm_AllPackedInOne(Graph, AllPackedInOne, NumberOfPackages, QValue)
  ,fixm_AllPackedInOne(r,r,t,t)

  % aixm_AltimeterSource(Graph, AltimeterSource, TimeSlice)
  ,aixm_AltimeterSource(r,r,t)

  % fixm_SurvivalCapabilities(Graph, SurvivalCapabilities, SurvivalEquipmentRemarks, DinghyInformation, EmergencyRadioCode, LifeJacketCode, SurvivalEquipmentCode)
  ,fixm_SurvivalCapabilities(r,r,t,t,t,t,t)

  % fixm_DirectRouting(Graph, DirectRouting, From, To)
  ,fixm_DirectRouting(r,r,t,t)

  % fixm_TargetMultiTime(Graph, TargetMultiTime, Target)
  ,fixm_TargetMultiTime(r,r,t)

  % fixm_AircraftType(Graph, AircraftType)
  ,fixm_AircraftType(r,r)

  % fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes)
  ,fixm_FlightDeparture(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_AerodromeReference(Graph, AerodromeReference)
  ,fixm_AerodromeReference(r,r)

  % fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime, TaxiTime)
  ,fixm_EfplFlightDeparture(r,r,t,t)

  % aixm_UsageCondition(Graph, UsageCondition, Type, PriorPermission, Selection, Annotation, Contact)
  ,aixm_UsageCondition(r,r,t,t,t,t,t)
.

subClassOf(X,Y) :-
  rdf(X,rdfs:subClassOf,Y,'https://github.com/jku-win-dke/aisa/graphs/schema') .

subClassOf(X,X) :-
  rdf(_,rdf:type,X,_) .

subClassOf(X,Y) :-
  rdf(X,rdfs:subClassOf,Z,'https://github.com/jku-win-dke/aisa/graphs/schema'),
  subClassOf(Z,Y) .

fixm_PostalAddress(Graph, PostalAddress, AdministrativeArea, PostalCode, DeliveryPoint, CountryCode, CountryName, City) :-
  rdf(PostalAddress,rdf:type,fixm:'PostalAddress',Graph)
  ,(
    ( AdministrativeArea='$null$',
      \+ rdf( PostalAddress,fixm:'administrativeArea',_AdministrativeArea,Graph )
    );
  ( rdf( PostalAddress,fixm:'administrativeArea',AdministrativeAreaNode,Graph )),
      (
        (
          rdf(AdministrativeAreaNode,rdf:value,AdministrativeAreaValue,Graph),
         \+ ( rdf( AdministrativeAreaNode, aixm:uom, _AdministrativeAreaUOM, Graph ); rdf( AdministrativeAreaNode, fixm:uom, _AdministrativeAreaUOM, Graph ); rdf( AdministrativeAreaNode, plain:uom, _AdministrativeAreaUOM, Graph ) ),
          AdministrativeArea=val(AdministrativeAreaValue)
        );
        (
          rdf( AdministrativeAreaNode,rdf:value,AdministrativeAreaValue,Graph ),
          ( rdf( AdministrativeAreaNode, aixm:uom, AdministrativeAreaUOM, Graph ); rdf( AdministrativeAreaNode, fixm:uom, AdministrativeAreaUOM, Graph ); rdf( AdministrativeAreaNode, plain:uom, AdministrativeAreaUOM, Graph ) ),
          AdministrativeArea=xval(AdministrativeAreaValue,AdministrativeAreaUOM)
        );
        (
          rdf( AdministrativeAreaNode,aixm:nilReason, AdministrativeAreaNilReason, Graph ),
          AdministrativeArea=nil(AdministrativeAreaNilReason)
        );
        (
          rdf( AdministrativeAreaNode,gml:indeterminatePosition, AdministrativeAreaIndeterminate, Graph ),
          AdministrativeArea=indeterminate(AdministrativeAreaIndeterminate)
        )
      )
  )
  ,(
    ( PostalCode='$null$',
      \+ rdf( PostalAddress,fixm:'postalCode',_PostalCode,Graph )
    );
  ( rdf( PostalAddress,fixm:'postalCode',PostalCodeNode,Graph )),
      (
        (
          rdf(PostalCodeNode,rdf:value,PostalCodeValue,Graph),
         \+ ( rdf( PostalCodeNode, aixm:uom, _PostalCodeUOM, Graph ); rdf( PostalCodeNode, fixm:uom, _PostalCodeUOM, Graph ); rdf( PostalCodeNode, plain:uom, _PostalCodeUOM, Graph ) ),
          PostalCode=val(PostalCodeValue)
        );
        (
          rdf( PostalCodeNode,rdf:value,PostalCodeValue,Graph ),
          ( rdf( PostalCodeNode, aixm:uom, PostalCodeUOM, Graph ); rdf( PostalCodeNode, fixm:uom, PostalCodeUOM, Graph ); rdf( PostalCodeNode, plain:uom, PostalCodeUOM, Graph ) ),
          PostalCode=xval(PostalCodeValue,PostalCodeUOM)
        );
        (
          rdf( PostalCodeNode,aixm:nilReason, PostalCodeNilReason, Graph ),
          PostalCode=nil(PostalCodeNilReason)
        );
        (
          rdf( PostalCodeNode,gml:indeterminatePosition, PostalCodeIndeterminate, Graph ),
          PostalCode=indeterminate(PostalCodeIndeterminate)
        )
      )
  )
  ,(
    ( DeliveryPoint='$null$',
      \+ rdf( PostalAddress,fixm:'deliveryPoint',_DeliveryPoint,Graph )
    );
  ( rdf( PostalAddress,fixm:'deliveryPoint',DeliveryPointNode,Graph )),
      (
        (
          rdf(DeliveryPointNode,rdf:value,DeliveryPointValue,Graph),
         \+ ( rdf( DeliveryPointNode, aixm:uom, _DeliveryPointUOM, Graph ); rdf( DeliveryPointNode, fixm:uom, _DeliveryPointUOM, Graph ); rdf( DeliveryPointNode, plain:uom, _DeliveryPointUOM, Graph ) ),
          DeliveryPoint=val(DeliveryPointValue)
        );
        (
          rdf( DeliveryPointNode,rdf:value,DeliveryPointValue,Graph ),
          ( rdf( DeliveryPointNode, aixm:uom, DeliveryPointUOM, Graph ); rdf( DeliveryPointNode, fixm:uom, DeliveryPointUOM, Graph ); rdf( DeliveryPointNode, plain:uom, DeliveryPointUOM, Graph ) ),
          DeliveryPoint=xval(DeliveryPointValue,DeliveryPointUOM)
        );
        (
          rdf( DeliveryPointNode,aixm:nilReason, DeliveryPointNilReason, Graph ),
          DeliveryPoint=nil(DeliveryPointNilReason)
        );
        (
          rdf( DeliveryPointNode,gml:indeterminatePosition, DeliveryPointIndeterminate, Graph ),
          DeliveryPoint=indeterminate(DeliveryPointIndeterminate)
        )
      )
  )
  ,(
    ( CountryCode='$null$',
      \+ rdf( PostalAddress,fixm:'countryCode',_CountryCode,Graph )
    );
  ( rdf( PostalAddress,fixm:'countryCode',CountryCodeNode,Graph )),
      (
        (
          rdf(CountryCodeNode,rdf:value,CountryCodeValue,Graph),
         \+ ( rdf( CountryCodeNode, aixm:uom, _CountryCodeUOM, Graph ); rdf( CountryCodeNode, fixm:uom, _CountryCodeUOM, Graph ); rdf( CountryCodeNode, plain:uom, _CountryCodeUOM, Graph ) ),
          CountryCode=val(CountryCodeValue)
        );
        (
          rdf( CountryCodeNode,rdf:value,CountryCodeValue,Graph ),
          ( rdf( CountryCodeNode, aixm:uom, CountryCodeUOM, Graph ); rdf( CountryCodeNode, fixm:uom, CountryCodeUOM, Graph ); rdf( CountryCodeNode, plain:uom, CountryCodeUOM, Graph ) ),
          CountryCode=xval(CountryCodeValue,CountryCodeUOM)
        );
        (
          rdf( CountryCodeNode,aixm:nilReason, CountryCodeNilReason, Graph ),
          CountryCode=nil(CountryCodeNilReason)
        );
        (
          rdf( CountryCodeNode,gml:indeterminatePosition, CountryCodeIndeterminate, Graph ),
          CountryCode=indeterminate(CountryCodeIndeterminate)
        )
      )
  )
  ,(
    ( CountryName='$null$',
      \+ rdf( PostalAddress,fixm:'countryName',_CountryName,Graph )
    );
  ( rdf( PostalAddress,fixm:'countryName',CountryNameNode,Graph )),
      (
        (
          rdf(CountryNameNode,rdf:value,CountryNameValue,Graph),
         \+ ( rdf( CountryNameNode, aixm:uom, _CountryNameUOM, Graph ); rdf( CountryNameNode, fixm:uom, _CountryNameUOM, Graph ); rdf( CountryNameNode, plain:uom, _CountryNameUOM, Graph ) ),
          CountryName=val(CountryNameValue)
        );
        (
          rdf( CountryNameNode,rdf:value,CountryNameValue,Graph ),
          ( rdf( CountryNameNode, aixm:uom, CountryNameUOM, Graph ); rdf( CountryNameNode, fixm:uom, CountryNameUOM, Graph ); rdf( CountryNameNode, plain:uom, CountryNameUOM, Graph ) ),
          CountryName=xval(CountryNameValue,CountryNameUOM)
        );
        (
          rdf( CountryNameNode,aixm:nilReason, CountryNameNilReason, Graph ),
          CountryName=nil(CountryNameNilReason)
        );
        (
          rdf( CountryNameNode,gml:indeterminatePosition, CountryNameIndeterminate, Graph ),
          CountryName=indeterminate(CountryNameIndeterminate)
        )
      )
  )
  ,(
    ( City='$null$',
      \+ rdf( PostalAddress,fixm:'city',_City,Graph )
    );
  ( rdf( PostalAddress,fixm:'city',CityNode,Graph )),
      (
        (
          rdf(CityNode,rdf:value,CityValue,Graph),
         \+ ( rdf( CityNode, aixm:uom, _CityUOM, Graph ); rdf( CityNode, fixm:uom, _CityUOM, Graph ); rdf( CityNode, plain:uom, _CityUOM, Graph ) ),
          City=val(CityValue)
        );
        (
          rdf( CityNode,rdf:value,CityValue,Graph ),
          ( rdf( CityNode, aixm:uom, CityUOM, Graph ); rdf( CityNode, fixm:uom, CityUOM, Graph ); rdf( CityNode, plain:uom, CityUOM, Graph ) ),
          City=xval(CityValue,CityUOM)
        );
        (
          rdf( CityNode,aixm:nilReason, CityNilReason, Graph ),
          City=nil(CityNilReason)
        );
        (
          rdf( CityNode,gml:indeterminatePosition, CityIndeterminate, Graph ),
          City=indeterminate(CityIndeterminate)
        )
      )
  ) .

fixm_NavigationCapabilities(Graph, NavigationCapabilities, OtherNavigationCapabilities, PerformanceBasedCode, NavigationCode) :-
  rdf(NavigationCapabilities,rdf:type,fixm:'NavigationCapabilities',Graph)
  ,(
    ( OtherNavigationCapabilities='$null$',
      \+ rdf( NavigationCapabilities,fixm:'otherNavigationCapabilities',_OtherNavigationCapabilities,Graph )
    );
  ( rdf( NavigationCapabilities,fixm:'otherNavigationCapabilities',OtherNavigationCapabilitiesNode,Graph )),
      (
        (
          rdf(OtherNavigationCapabilitiesNode,rdf:value,OtherNavigationCapabilitiesValue,Graph),
         \+ ( rdf( OtherNavigationCapabilitiesNode, aixm:uom, _OtherNavigationCapabilitiesUOM, Graph ); rdf( OtherNavigationCapabilitiesNode, fixm:uom, _OtherNavigationCapabilitiesUOM, Graph ); rdf( OtherNavigationCapabilitiesNode, plain:uom, _OtherNavigationCapabilitiesUOM, Graph ) ),
          OtherNavigationCapabilities=val(OtherNavigationCapabilitiesValue)
        );
        (
          rdf( OtherNavigationCapabilitiesNode,rdf:value,OtherNavigationCapabilitiesValue,Graph ),
          ( rdf( OtherNavigationCapabilitiesNode, aixm:uom, OtherNavigationCapabilitiesUOM, Graph ); rdf( OtherNavigationCapabilitiesNode, fixm:uom, OtherNavigationCapabilitiesUOM, Graph ); rdf( OtherNavigationCapabilitiesNode, plain:uom, OtherNavigationCapabilitiesUOM, Graph ) ),
          OtherNavigationCapabilities=xval(OtherNavigationCapabilitiesValue,OtherNavigationCapabilitiesUOM)
        );
        (
          rdf( OtherNavigationCapabilitiesNode,aixm:nilReason, OtherNavigationCapabilitiesNilReason, Graph ),
          OtherNavigationCapabilities=nil(OtherNavigationCapabilitiesNilReason)
        );
        (
          rdf( OtherNavigationCapabilitiesNode,gml:indeterminatePosition, OtherNavigationCapabilitiesIndeterminate, Graph ),
          OtherNavigationCapabilities=indeterminate(OtherNavigationCapabilitiesIndeterminate)
        )
      )
  )
  ,findall(A, rdf(NavigationCapabilities,fixm:'performanceBasedCode',A,Graph), PerformanceBasedCode)
  ,findall(A, rdf(NavigationCapabilities,fixm:'navigationCode',A,Graph), NavigationCode) .

fixm_GroundspeedRange(Graph, GroundspeedRange, LowerSpeed, UpperSpeed) :-
  rdf(GroundspeedRange,rdf:type,fixm:'GroundspeedRange',Graph)
  ,(
    ( LowerSpeed='$null$',
      \+ rdf( GroundspeedRange,fixm:'lowerSpeed',_LowerSpeed,Graph )
    );
  ( rdf( GroundspeedRange,fixm:'lowerSpeed',LowerSpeedNode,Graph )),
      (
        (
          rdf(LowerSpeedNode,rdf:value,LowerSpeedValue,Graph),
         \+ ( rdf( LowerSpeedNode, aixm:uom, _LowerSpeedUOM, Graph ); rdf( LowerSpeedNode, fixm:uom, _LowerSpeedUOM, Graph ); rdf( LowerSpeedNode, plain:uom, _LowerSpeedUOM, Graph ) ),
          LowerSpeed=val(LowerSpeedValue)
        );
        (
          rdf( LowerSpeedNode,rdf:value,LowerSpeedValue,Graph ),
          ( rdf( LowerSpeedNode, aixm:uom, LowerSpeedUOM, Graph ); rdf( LowerSpeedNode, fixm:uom, LowerSpeedUOM, Graph ); rdf( LowerSpeedNode, plain:uom, LowerSpeedUOM, Graph ) ),
          LowerSpeed=xval(LowerSpeedValue,LowerSpeedUOM)
        );
        (
          rdf( LowerSpeedNode,aixm:nilReason, LowerSpeedNilReason, Graph ),
          LowerSpeed=nil(LowerSpeedNilReason)
        );
        (
          rdf( LowerSpeedNode,gml:indeterminatePosition, LowerSpeedIndeterminate, Graph ),
          LowerSpeed=indeterminate(LowerSpeedIndeterminate)
        )
      )
  )
  ,(
    ( UpperSpeed='$null$',
      \+ rdf( GroundspeedRange,fixm:'upperSpeed',_UpperSpeed,Graph )
    );
  ( rdf( GroundspeedRange,fixm:'upperSpeed',UpperSpeedNode,Graph )),
      (
        (
          rdf(UpperSpeedNode,rdf:value,UpperSpeedValue,Graph),
         \+ ( rdf( UpperSpeedNode, aixm:uom, _UpperSpeedUOM, Graph ); rdf( UpperSpeedNode, fixm:uom, _UpperSpeedUOM, Graph ); rdf( UpperSpeedNode, plain:uom, _UpperSpeedUOM, Graph ) ),
          UpperSpeed=val(UpperSpeedValue)
        );
        (
          rdf( UpperSpeedNode,rdf:value,UpperSpeedValue,Graph ),
          ( rdf( UpperSpeedNode, aixm:uom, UpperSpeedUOM, Graph ); rdf( UpperSpeedNode, fixm:uom, UpperSpeedUOM, Graph ); rdf( UpperSpeedNode, plain:uom, UpperSpeedUOM, Graph ) ),
          UpperSpeed=xval(UpperSpeedValue,UpperSpeedUOM)
        );
        (
          rdf( UpperSpeedNode,aixm:nilReason, UpperSpeedNilReason, Graph ),
          UpperSpeed=nil(UpperSpeedNilReason)
        );
        (
          rdf( UpperSpeedNode,gml:indeterminatePosition, UpperSpeedIndeterminate, Graph ),
          UpperSpeed=indeterminate(UpperSpeedIndeterminate)
        )
      )
  ) .

aixm_Note(Graph, Note, PropertyName, Purpose, TranslatedNote) :-
  rdf(Note,rdf:type,aixm:'Note',Graph)
  ,(
    ( PropertyName='$null$',
      \+ rdf( Note,aixm:'propertyName',_PropertyName,Graph )
    );
  ( rdf( Note,aixm:'propertyName',PropertyNameNode,Graph )),
      (
        (
          rdf(PropertyNameNode,rdf:value,PropertyNameValue,Graph),
         \+ ( rdf( PropertyNameNode, aixm:uom, _PropertyNameUOM, Graph ); rdf( PropertyNameNode, fixm:uom, _PropertyNameUOM, Graph ); rdf( PropertyNameNode, plain:uom, _PropertyNameUOM, Graph ) ),
          PropertyName=val(PropertyNameValue)
        );
        (
          rdf( PropertyNameNode,rdf:value,PropertyNameValue,Graph ),
          ( rdf( PropertyNameNode, aixm:uom, PropertyNameUOM, Graph ); rdf( PropertyNameNode, fixm:uom, PropertyNameUOM, Graph ); rdf( PropertyNameNode, plain:uom, PropertyNameUOM, Graph ) ),
          PropertyName=xval(PropertyNameValue,PropertyNameUOM)
        );
        (
          rdf( PropertyNameNode,aixm:nilReason, PropertyNameNilReason, Graph ),
          PropertyName=nil(PropertyNameNilReason)
        );
        (
          rdf( PropertyNameNode,gml:indeterminatePosition, PropertyNameIndeterminate, Graph ),
          PropertyName=indeterminate(PropertyNameIndeterminate)
        )
      )
  )
  ,(
    ( Purpose='$null$',
      \+ rdf( Note,aixm:'purpose',_Purpose,Graph )
    );
  ( rdf( Note,aixm:'purpose',PurposeNode,Graph )),
      (
        (
          rdf(PurposeNode,rdf:value,PurposeValue,Graph),
         \+ ( rdf( PurposeNode, aixm:uom, _PurposeUOM, Graph ); rdf( PurposeNode, fixm:uom, _PurposeUOM, Graph ); rdf( PurposeNode, plain:uom, _PurposeUOM, Graph ) ),
          Purpose=val(PurposeValue)
        );
        (
          rdf( PurposeNode,rdf:value,PurposeValue,Graph ),
          ( rdf( PurposeNode, aixm:uom, PurposeUOM, Graph ); rdf( PurposeNode, fixm:uom, PurposeUOM, Graph ); rdf( PurposeNode, plain:uom, PurposeUOM, Graph ) ),
          Purpose=xval(PurposeValue,PurposeUOM)
        );
        (
          rdf( PurposeNode,aixm:nilReason, PurposeNilReason, Graph ),
          Purpose=nil(PurposeNilReason)
        );
        (
          rdf( PurposeNode,gml:indeterminatePosition, PurposeIndeterminate, Graph ),
          Purpose=indeterminate(PurposeIndeterminate)
        )
      )
  )
  ,findall(A, rdf(Note,aixm:'translatedNote',A,Graph), TranslatedNote) .

fixm_Pointout(Graph, Pointout, OriginatingUnit, ReceivingUnit) :-
  rdf(Pointout,rdf:type,fixm:'Pointout',Graph)
  ,( ( OriginatingUnit='$null$',
    \+ rdf( Pointout,fixm:'originatingUnit', _OriginatingUnit, Graph  )
   ; rdf(Pointout,fixm:'originatingUnit', OriginatingUnit, Graph ) )
  )
  ,findall(A, rdf(Pointout,fixm:'receivingUnit',A,Graph), ReceivingUnit) .

fixm_VerticalRange(Graph, VerticalRange, LowerBound, UpperBound) :-
  rdf(VerticalRange,rdf:type,fixm:'VerticalRange',Graph)
  ,(
    ( LowerBound='$null$',
      \+ rdf( VerticalRange,fixm:'lowerBound',_LowerBound,Graph )
    );
  ( rdf( VerticalRange,fixm:'lowerBound',LowerBoundNode,Graph )),
      (
        (
          rdf(LowerBoundNode,rdf:value,LowerBoundValue,Graph),
         \+ ( rdf( LowerBoundNode, aixm:uom, _LowerBoundUOM, Graph ); rdf( LowerBoundNode, fixm:uom, _LowerBoundUOM, Graph ); rdf( LowerBoundNode, plain:uom, _LowerBoundUOM, Graph ) ),
          LowerBound=val(LowerBoundValue)
        );
        (
          rdf( LowerBoundNode,rdf:value,LowerBoundValue,Graph ),
          ( rdf( LowerBoundNode, aixm:uom, LowerBoundUOM, Graph ); rdf( LowerBoundNode, fixm:uom, LowerBoundUOM, Graph ); rdf( LowerBoundNode, plain:uom, LowerBoundUOM, Graph ) ),
          LowerBound=xval(LowerBoundValue,LowerBoundUOM)
        );
        (
          rdf( LowerBoundNode,aixm:nilReason, LowerBoundNilReason, Graph ),
          LowerBound=nil(LowerBoundNilReason)
        );
        (
          rdf( LowerBoundNode,gml:indeterminatePosition, LowerBoundIndeterminate, Graph ),
          LowerBound=indeterminate(LowerBoundIndeterminate)
        )
      )
  )
  ,(
    ( UpperBound='$null$',
      \+ rdf( VerticalRange,fixm:'upperBound',_UpperBound,Graph )
    );
  ( rdf( VerticalRange,fixm:'upperBound',UpperBoundNode,Graph )),
      (
        (
          rdf(UpperBoundNode,rdf:value,UpperBoundValue,Graph),
         \+ ( rdf( UpperBoundNode, aixm:uom, _UpperBoundUOM, Graph ); rdf( UpperBoundNode, fixm:uom, _UpperBoundUOM, Graph ); rdf( UpperBoundNode, plain:uom, _UpperBoundUOM, Graph ) ),
          UpperBound=val(UpperBoundValue)
        );
        (
          rdf( UpperBoundNode,rdf:value,UpperBoundValue,Graph ),
          ( rdf( UpperBoundNode, aixm:uom, UpperBoundUOM, Graph ); rdf( UpperBoundNode, fixm:uom, UpperBoundUOM, Graph ); rdf( UpperBoundNode, plain:uom, UpperBoundUOM, Graph ) ),
          UpperBound=xval(UpperBoundValue,UpperBoundUOM)
        );
        (
          rdf( UpperBoundNode,aixm:nilReason, UpperBoundNilReason, Graph ),
          UpperBound=nil(UpperBoundNilReason)
        );
        (
          rdf( UpperBoundNode,gml:indeterminatePosition, UpperBoundIndeterminate, Graph ),
          UpperBound=indeterminate(UpperBoundIndeterminate)
        )
      )
  ) .

fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel, EstimatedTime, Constraint) :-
  rdf(ExpandedRoutePoint,rdf:type,fixm:'ExpandedRoutePoint',Graph)
  ,(
    ( EstimatedLevel='$null$',
      \+ rdf( ExpandedRoutePoint,fixm:'estimatedLevel',_EstimatedLevel,Graph )
    );
  ( rdf( ExpandedRoutePoint,fixm:'estimatedLevel',EstimatedLevelNode,Graph )),
      (
        (
          rdf(EstimatedLevelNode,rdf:value,EstimatedLevelValue,Graph),
         \+ ( rdf( EstimatedLevelNode, aixm:uom, _EstimatedLevelUOM, Graph ); rdf( EstimatedLevelNode, fixm:uom, _EstimatedLevelUOM, Graph ); rdf( EstimatedLevelNode, plain:uom, _EstimatedLevelUOM, Graph ) ),
          EstimatedLevel=val(EstimatedLevelValue)
        );
        (
          rdf( EstimatedLevelNode,rdf:value,EstimatedLevelValue,Graph ),
          ( rdf( EstimatedLevelNode, aixm:uom, EstimatedLevelUOM, Graph ); rdf( EstimatedLevelNode, fixm:uom, EstimatedLevelUOM, Graph ); rdf( EstimatedLevelNode, plain:uom, EstimatedLevelUOM, Graph ) ),
          EstimatedLevel=xval(EstimatedLevelValue,EstimatedLevelUOM)
        );
        (
          rdf( EstimatedLevelNode,aixm:nilReason, EstimatedLevelNilReason, Graph ),
          EstimatedLevel=nil(EstimatedLevelNilReason)
        );
        (
          rdf( EstimatedLevelNode,gml:indeterminatePosition, EstimatedLevelIndeterminate, Graph ),
          EstimatedLevel=indeterminate(EstimatedLevelIndeterminate)
        )
      )
  )
  ,(
    ( EstimatedTime='$null$',
      \+ rdf( ExpandedRoutePoint,fixm:'estimatedTime',_EstimatedTime,Graph )
    );
  ( rdf( ExpandedRoutePoint,fixm:'estimatedTime',EstimatedTimeNode,Graph )),
      (
        (
          rdf(EstimatedTimeNode,rdf:value,EstimatedTimeValue,Graph),
         \+ ( rdf( EstimatedTimeNode, aixm:uom, _EstimatedTimeUOM, Graph ); rdf( EstimatedTimeNode, fixm:uom, _EstimatedTimeUOM, Graph ); rdf( EstimatedTimeNode, plain:uom, _EstimatedTimeUOM, Graph ) ),
          EstimatedTime=val(EstimatedTimeValue)
        );
        (
          rdf( EstimatedTimeNode,rdf:value,EstimatedTimeValue,Graph ),
          ( rdf( EstimatedTimeNode, aixm:uom, EstimatedTimeUOM, Graph ); rdf( EstimatedTimeNode, fixm:uom, EstimatedTimeUOM, Graph ); rdf( EstimatedTimeNode, plain:uom, EstimatedTimeUOM, Graph ) ),
          EstimatedTime=xval(EstimatedTimeValue,EstimatedTimeUOM)
        );
        (
          rdf( EstimatedTimeNode,aixm:nilReason, EstimatedTimeNilReason, Graph ),
          EstimatedTime=nil(EstimatedTimeNilReason)
        );
        (
          rdf( EstimatedTimeNode,gml:indeterminatePosition, EstimatedTimeIndeterminate, Graph ),
          EstimatedTime=indeterminate(EstimatedTimeIndeterminate)
        )
      )
  )
  ,findall(A, rdf(ExpandedRoutePoint,fixm:'constraint',A,Graph), Constraint) .

aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  rdf(ElevatedSurface,rdf:type,aixm:'ElevatedSurface',Graph)
  ,(
    ( Elevation='$null$',
      \+ rdf( ElevatedSurface,aixm:'elevation',_Elevation,Graph )
    );
  ( rdf( ElevatedSurface,aixm:'elevation',ElevationNode,Graph )),
      (
        (
          rdf(ElevationNode,rdf:value,ElevationValue,Graph),
         \+ ( rdf( ElevationNode, aixm:uom, _ElevationUOM, Graph ); rdf( ElevationNode, fixm:uom, _ElevationUOM, Graph ); rdf( ElevationNode, plain:uom, _ElevationUOM, Graph ) ),
          Elevation=val(ElevationValue)
        );
        (
          rdf( ElevationNode,rdf:value,ElevationValue,Graph ),
          ( rdf( ElevationNode, aixm:uom, ElevationUOM, Graph ); rdf( ElevationNode, fixm:uom, ElevationUOM, Graph ); rdf( ElevationNode, plain:uom, ElevationUOM, Graph ) ),
          Elevation=xval(ElevationValue,ElevationUOM)
        );
        (
          rdf( ElevationNode,aixm:nilReason, ElevationNilReason, Graph ),
          Elevation=nil(ElevationNilReason)
        );
        (
          rdf( ElevationNode,gml:indeterminatePosition, ElevationIndeterminate, Graph ),
          Elevation=indeterminate(ElevationIndeterminate)
        )
      )
  )
  ,(
    ( GeoidUndulation='$null$',
      \+ rdf( ElevatedSurface,aixm:'geoidUndulation',_GeoidUndulation,Graph )
    );
  ( rdf( ElevatedSurface,aixm:'geoidUndulation',GeoidUndulationNode,Graph )),
      (
        (
          rdf(GeoidUndulationNode,rdf:value,GeoidUndulationValue,Graph),
         \+ ( rdf( GeoidUndulationNode, aixm:uom, _GeoidUndulationUOM, Graph ); rdf( GeoidUndulationNode, fixm:uom, _GeoidUndulationUOM, Graph ); rdf( GeoidUndulationNode, plain:uom, _GeoidUndulationUOM, Graph ) ),
          GeoidUndulation=val(GeoidUndulationValue)
        );
        (
          rdf( GeoidUndulationNode,rdf:value,GeoidUndulationValue,Graph ),
          ( rdf( GeoidUndulationNode, aixm:uom, GeoidUndulationUOM, Graph ); rdf( GeoidUndulationNode, fixm:uom, GeoidUndulationUOM, Graph ); rdf( GeoidUndulationNode, plain:uom, GeoidUndulationUOM, Graph ) ),
          GeoidUndulation=xval(GeoidUndulationValue,GeoidUndulationUOM)
        );
        (
          rdf( GeoidUndulationNode,aixm:nilReason, GeoidUndulationNilReason, Graph ),
          GeoidUndulation=nil(GeoidUndulationNilReason)
        );
        (
          rdf( GeoidUndulationNode,gml:indeterminatePosition, GeoidUndulationIndeterminate, Graph ),
          GeoidUndulation=indeterminate(GeoidUndulationIndeterminate)
        )
      )
  )
  ,(
    ( VerticalDatum='$null$',
      \+ rdf( ElevatedSurface,aixm:'verticalDatum',_VerticalDatum,Graph )
    );
  ( rdf( ElevatedSurface,aixm:'verticalDatum',VerticalDatumNode,Graph )),
      (
        (
          rdf(VerticalDatumNode,rdf:value,VerticalDatumValue,Graph),
         \+ ( rdf( VerticalDatumNode, aixm:uom, _VerticalDatumUOM, Graph ); rdf( VerticalDatumNode, fixm:uom, _VerticalDatumUOM, Graph ); rdf( VerticalDatumNode, plain:uom, _VerticalDatumUOM, Graph ) ),
          VerticalDatum=val(VerticalDatumValue)
        );
        (
          rdf( VerticalDatumNode,rdf:value,VerticalDatumValue,Graph ),
          ( rdf( VerticalDatumNode, aixm:uom, VerticalDatumUOM, Graph ); rdf( VerticalDatumNode, fixm:uom, VerticalDatumUOM, Graph ); rdf( VerticalDatumNode, plain:uom, VerticalDatumUOM, Graph ) ),
          VerticalDatum=xval(VerticalDatumValue,VerticalDatumUOM)
        );
        (
          rdf( VerticalDatumNode,aixm:nilReason, VerticalDatumNilReason, Graph ),
          VerticalDatum=nil(VerticalDatumNilReason)
        );
        (
          rdf( VerticalDatumNode,gml:indeterminatePosition, VerticalDatumIndeterminate, Graph ),
          VerticalDatum=indeterminate(VerticalDatumIndeterminate)
        )
      )
  )
  ,(
    ( VerticalAccuracy='$null$',
      \+ rdf( ElevatedSurface,aixm:'verticalAccuracy',_VerticalAccuracy,Graph )
    );
  ( rdf( ElevatedSurface,aixm:'verticalAccuracy',VerticalAccuracyNode,Graph )),
      (
        (
          rdf(VerticalAccuracyNode,rdf:value,VerticalAccuracyValue,Graph),
         \+ ( rdf( VerticalAccuracyNode, aixm:uom, _VerticalAccuracyUOM, Graph ); rdf( VerticalAccuracyNode, fixm:uom, _VerticalAccuracyUOM, Graph ); rdf( VerticalAccuracyNode, plain:uom, _VerticalAccuracyUOM, Graph ) ),
          VerticalAccuracy=val(VerticalAccuracyValue)
        );
        (
          rdf( VerticalAccuracyNode,rdf:value,VerticalAccuracyValue,Graph ),
          ( rdf( VerticalAccuracyNode, aixm:uom, VerticalAccuracyUOM, Graph ); rdf( VerticalAccuracyNode, fixm:uom, VerticalAccuracyUOM, Graph ); rdf( VerticalAccuracyNode, plain:uom, VerticalAccuracyUOM, Graph ) ),
          VerticalAccuracy=xval(VerticalAccuracyValue,VerticalAccuracyUOM)
        );
        (
          rdf( VerticalAccuracyNode,aixm:nilReason, VerticalAccuracyNilReason, Graph ),
          VerticalAccuracy=nil(VerticalAccuracyNilReason)
        );
        (
          rdf( VerticalAccuracyNode,gml:indeterminatePosition, VerticalAccuracyIndeterminate, Graph ),
          VerticalAccuracy=indeterminate(VerticalAccuracyIndeterminate)
        )
      )
  ) .

fixm_Dimensions(Graph, Dimensions, Height, Length, Width) :-
  rdf(Dimensions,rdf:type,fixm:'Dimensions',Graph)
  ,(
    ( Height='$null$',
      \+ rdf( Dimensions,fixm:'height',_Height,Graph )
    );
  ( rdf( Dimensions,fixm:'height',HeightNode,Graph )),
      (
        (
          rdf(HeightNode,rdf:value,HeightValue,Graph),
         \+ ( rdf( HeightNode, aixm:uom, _HeightUOM, Graph ); rdf( HeightNode, fixm:uom, _HeightUOM, Graph ); rdf( HeightNode, plain:uom, _HeightUOM, Graph ) ),
          Height=val(HeightValue)
        );
        (
          rdf( HeightNode,rdf:value,HeightValue,Graph ),
          ( rdf( HeightNode, aixm:uom, HeightUOM, Graph ); rdf( HeightNode, fixm:uom, HeightUOM, Graph ); rdf( HeightNode, plain:uom, HeightUOM, Graph ) ),
          Height=xval(HeightValue,HeightUOM)
        );
        (
          rdf( HeightNode,aixm:nilReason, HeightNilReason, Graph ),
          Height=nil(HeightNilReason)
        );
        (
          rdf( HeightNode,gml:indeterminatePosition, HeightIndeterminate, Graph ),
          Height=indeterminate(HeightIndeterminate)
        )
      )
  )
  ,(
    ( Length='$null$',
      \+ rdf( Dimensions,fixm:'length',_Length,Graph )
    );
  ( rdf( Dimensions,fixm:'length',LengthNode,Graph )),
      (
        (
          rdf(LengthNode,rdf:value,LengthValue,Graph),
         \+ ( rdf( LengthNode, aixm:uom, _LengthUOM, Graph ); rdf( LengthNode, fixm:uom, _LengthUOM, Graph ); rdf( LengthNode, plain:uom, _LengthUOM, Graph ) ),
          Length=val(LengthValue)
        );
        (
          rdf( LengthNode,rdf:value,LengthValue,Graph ),
          ( rdf( LengthNode, aixm:uom, LengthUOM, Graph ); rdf( LengthNode, fixm:uom, LengthUOM, Graph ); rdf( LengthNode, plain:uom, LengthUOM, Graph ) ),
          Length=xval(LengthValue,LengthUOM)
        );
        (
          rdf( LengthNode,aixm:nilReason, LengthNilReason, Graph ),
          Length=nil(LengthNilReason)
        );
        (
          rdf( LengthNode,gml:indeterminatePosition, LengthIndeterminate, Graph ),
          Length=indeterminate(LengthIndeterminate)
        )
      )
  )
  ,(
    ( Width='$null$',
      \+ rdf( Dimensions,fixm:'width',_Width,Graph )
    );
  ( rdf( Dimensions,fixm:'width',WidthNode,Graph )),
      (
        (
          rdf(WidthNode,rdf:value,WidthValue,Graph),
         \+ ( rdf( WidthNode, aixm:uom, _WidthUOM, Graph ); rdf( WidthNode, fixm:uom, _WidthUOM, Graph ); rdf( WidthNode, plain:uom, _WidthUOM, Graph ) ),
          Width=val(WidthValue)
        );
        (
          rdf( WidthNode,rdf:value,WidthValue,Graph ),
          ( rdf( WidthNode, aixm:uom, WidthUOM, Graph ); rdf( WidthNode, fixm:uom, WidthUOM, Graph ); rdf( WidthNode, plain:uom, WidthUOM, Graph ) ),
          Width=xval(WidthValue,WidthUOM)
        );
        (
          rdf( WidthNode,aixm:nilReason, WidthNilReason, Graph ),
          Width=nil(WidthNilReason)
        );
        (
          rdf( WidthNode,gml:indeterminatePosition, WidthIndeterminate, Graph ),
          Width=indeterminate(WidthIndeterminate)
        )
      )
  ) .

fixm_StandPositionAndTime(Graph, StandPositionAndTime, StandName, StandTime, TerminalName) :-
  rdf(StandPositionAndTime,rdf:type,fixm:'StandPositionAndTime',Graph)
  ,(
    ( StandName='$null$',
      \+ rdf( StandPositionAndTime,fixm:'standName',_StandName,Graph )
    );
  ( rdf( StandPositionAndTime,fixm:'standName',StandNameNode,Graph )),
      (
        (
          rdf(StandNameNode,rdf:value,StandNameValue,Graph),
         \+ ( rdf( StandNameNode, aixm:uom, _StandNameUOM, Graph ); rdf( StandNameNode, fixm:uom, _StandNameUOM, Graph ); rdf( StandNameNode, plain:uom, _StandNameUOM, Graph ) ),
          StandName=val(StandNameValue)
        );
        (
          rdf( StandNameNode,rdf:value,StandNameValue,Graph ),
          ( rdf( StandNameNode, aixm:uom, StandNameUOM, Graph ); rdf( StandNameNode, fixm:uom, StandNameUOM, Graph ); rdf( StandNameNode, plain:uom, StandNameUOM, Graph ) ),
          StandName=xval(StandNameValue,StandNameUOM)
        );
        (
          rdf( StandNameNode,aixm:nilReason, StandNameNilReason, Graph ),
          StandName=nil(StandNameNilReason)
        );
        (
          rdf( StandNameNode,gml:indeterminatePosition, StandNameIndeterminate, Graph ),
          StandName=indeterminate(StandNameIndeterminate)
        )
      )
  )
  ,( ( StandTime='$null$',
    \+ rdf( StandPositionAndTime,fixm:'standTime', _StandTime, Graph  )
   ; rdf(StandPositionAndTime,fixm:'standTime', StandTime, Graph ) )
  )
  ,(
    ( TerminalName='$null$',
      \+ rdf( StandPositionAndTime,fixm:'terminalName',_TerminalName,Graph )
    );
  ( rdf( StandPositionAndTime,fixm:'terminalName',TerminalNameNode,Graph )),
      (
        (
          rdf(TerminalNameNode,rdf:value,TerminalNameValue,Graph),
         \+ ( rdf( TerminalNameNode, aixm:uom, _TerminalNameUOM, Graph ); rdf( TerminalNameNode, fixm:uom, _TerminalNameUOM, Graph ); rdf( TerminalNameNode, plain:uom, _TerminalNameUOM, Graph ) ),
          TerminalName=val(TerminalNameValue)
        );
        (
          rdf( TerminalNameNode,rdf:value,TerminalNameValue,Graph ),
          ( rdf( TerminalNameNode, aixm:uom, TerminalNameUOM, Graph ); rdf( TerminalNameNode, fixm:uom, TerminalNameUOM, Graph ); rdf( TerminalNameNode, plain:uom, TerminalNameUOM, Graph ) ),
          TerminalName=xval(TerminalNameValue,TerminalNameUOM)
        );
        (
          rdf( TerminalNameNode,aixm:nilReason, TerminalNameNilReason, Graph ),
          TerminalName=nil(TerminalNameNilReason)
        );
        (
          rdf( TerminalNameNode,gml:indeterminatePosition, TerminalNameIndeterminate, Graph ),
          TerminalName=indeterminate(TerminalNameIndeterminate)
        )
      )
  ) .

fixm_RouteSegment(Graph, RouteSegment, Airway, RoutePoint) :-
  rdf(RouteSegment,rdf:type,fixm:'RouteSegment',Graph)
  ,(
    ( Airway='$null$',
      \+ rdf( RouteSegment,fixm:'airway',_Airway,Graph )
    );
  ( rdf( RouteSegment,fixm:'airway',AirwayNode,Graph )),
      (
        (
          rdf(AirwayNode,rdf:value,AirwayValue,Graph),
         \+ ( rdf( AirwayNode, aixm:uom, _AirwayUOM, Graph ); rdf( AirwayNode, fixm:uom, _AirwayUOM, Graph ); rdf( AirwayNode, plain:uom, _AirwayUOM, Graph ) ),
          Airway=val(AirwayValue)
        );
        (
          rdf( AirwayNode,rdf:value,AirwayValue,Graph ),
          ( rdf( AirwayNode, aixm:uom, AirwayUOM, Graph ); rdf( AirwayNode, fixm:uom, AirwayUOM, Graph ); rdf( AirwayNode, plain:uom, AirwayUOM, Graph ) ),
          Airway=xval(AirwayValue,AirwayUOM)
        );
        (
          rdf( AirwayNode,aixm:nilReason, AirwayNilReason, Graph ),
          Airway=nil(AirwayNilReason)
        );
        (
          rdf( AirwayNode,gml:indeterminatePosition, AirwayIndeterminate, Graph ),
          Airway=indeterminate(AirwayIndeterminate)
        )
      )
  )
  ,( ( RoutePoint='$null$',
    \+ rdf( RouteSegment,fixm:'routePoint', _RoutePoint, Graph  )
   ; rdf(RouteSegment,fixm:'routePoint', RoutePoint, Graph ) )
  ) .

aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator, Flight, Aircraft, Weather, SubCondition) :-
  rdf(ConditionCombination,rdf:type,aixm:'ConditionCombination',Graph)
  ,(
    ( LogicalOperator='$null$',
      \+ rdf( ConditionCombination,aixm:'logicalOperator',_LogicalOperator,Graph )
    );
  ( rdf( ConditionCombination,aixm:'logicalOperator',LogicalOperatorNode,Graph )),
      (
        (
          rdf(LogicalOperatorNode,rdf:value,LogicalOperatorValue,Graph),
         \+ ( rdf( LogicalOperatorNode, aixm:uom, _LogicalOperatorUOM, Graph ); rdf( LogicalOperatorNode, fixm:uom, _LogicalOperatorUOM, Graph ); rdf( LogicalOperatorNode, plain:uom, _LogicalOperatorUOM, Graph ) ),
          LogicalOperator=val(LogicalOperatorValue)
        );
        (
          rdf( LogicalOperatorNode,rdf:value,LogicalOperatorValue,Graph ),
          ( rdf( LogicalOperatorNode, aixm:uom, LogicalOperatorUOM, Graph ); rdf( LogicalOperatorNode, fixm:uom, LogicalOperatorUOM, Graph ); rdf( LogicalOperatorNode, plain:uom, LogicalOperatorUOM, Graph ) ),
          LogicalOperator=xval(LogicalOperatorValue,LogicalOperatorUOM)
        );
        (
          rdf( LogicalOperatorNode,aixm:nilReason, LogicalOperatorNilReason, Graph ),
          LogicalOperator=nil(LogicalOperatorNilReason)
        );
        (
          rdf( LogicalOperatorNode,gml:indeterminatePosition, LogicalOperatorIndeterminate, Graph ),
          LogicalOperator=indeterminate(LogicalOperatorIndeterminate)
        )
      )
  )
  ,findall(A, rdf(ConditionCombination,aixm:'flight',A,Graph), Flight)
  ,findall(A, rdf(ConditionCombination,aixm:'aircraft',A,Graph), Aircraft)
  ,findall(A, rdf(ConditionCombination,aixm:'weather',A,Graph), Weather)
  ,findall(A, rdf(ConditionCombination,aixm:'subCondition',A,Graph), SubCondition) .

aixm_SurfaceContaminationLayer(Graph, SurfaceContaminationLayer, LayerOrder, Type, Extent, Annotation) :-
  rdf(SurfaceContaminationLayer,rdf:type,aixm:'SurfaceContaminationLayer',Graph)
  ,(
    ( LayerOrder='$null$',
      \+ rdf( SurfaceContaminationLayer,aixm:'layerOrder',_LayerOrder,Graph )
    );
  ( rdf( SurfaceContaminationLayer,aixm:'layerOrder',LayerOrderNode,Graph )),
      (
        (
          rdf(LayerOrderNode,rdf:value,LayerOrderValue,Graph),
         \+ ( rdf( LayerOrderNode, aixm:uom, _LayerOrderUOM, Graph ); rdf( LayerOrderNode, fixm:uom, _LayerOrderUOM, Graph ); rdf( LayerOrderNode, plain:uom, _LayerOrderUOM, Graph ) ),
          LayerOrder=val(LayerOrderValue)
        );
        (
          rdf( LayerOrderNode,rdf:value,LayerOrderValue,Graph ),
          ( rdf( LayerOrderNode, aixm:uom, LayerOrderUOM, Graph ); rdf( LayerOrderNode, fixm:uom, LayerOrderUOM, Graph ); rdf( LayerOrderNode, plain:uom, LayerOrderUOM, Graph ) ),
          LayerOrder=xval(LayerOrderValue,LayerOrderUOM)
        );
        (
          rdf( LayerOrderNode,aixm:nilReason, LayerOrderNilReason, Graph ),
          LayerOrder=nil(LayerOrderNilReason)
        );
        (
          rdf( LayerOrderNode,gml:indeterminatePosition, LayerOrderIndeterminate, Graph ),
          LayerOrder=indeterminate(LayerOrderIndeterminate)
        )
      )
  )
  ,(
    ( Type='$null$',
      \+ rdf( SurfaceContaminationLayer,aixm:'type',_Type,Graph )
    );
  ( rdf( SurfaceContaminationLayer,aixm:'type',TypeNode,Graph )),
      (
        (
          rdf(TypeNode,rdf:value,TypeValue,Graph),
         \+ ( rdf( TypeNode, aixm:uom, _TypeUOM, Graph ); rdf( TypeNode, fixm:uom, _TypeUOM, Graph ); rdf( TypeNode, plain:uom, _TypeUOM, Graph ) ),
          Type=val(TypeValue)
        );
        (
          rdf( TypeNode,rdf:value,TypeValue,Graph ),
          ( rdf( TypeNode, aixm:uom, TypeUOM, Graph ); rdf( TypeNode, fixm:uom, TypeUOM, Graph ); rdf( TypeNode, plain:uom, TypeUOM, Graph ) ),
          Type=xval(TypeValue,TypeUOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, TypeNilReason, Graph ),
          Type=nil(TypeNilReason)
        );
        (
          rdf( TypeNode,gml:indeterminatePosition, TypeIndeterminate, Graph ),
          Type=indeterminate(TypeIndeterminate)
        )
      )
  )
  ,findall(A, rdf(SurfaceContaminationLayer,aixm:'extent',A,Graph), Extent)
  ,findall(A, rdf(SurfaceContaminationLayer,aixm:'annotation',A,Graph), Annotation) .

fixm_Organization(Graph, Organization, Name, OtherOrganization, Contact) :-
  rdf(Organization,rdf:type,fixm:'Organization',Graph)
  ,(
    ( Name='$null$',
      \+ rdf( Organization,fixm:'name',_Name,Graph )
    );
  ( rdf( Organization,fixm:'name',NameNode,Graph )),
      (
        (
          rdf(NameNode,rdf:value,NameValue,Graph),
         \+ ( rdf( NameNode, aixm:uom, _NameUOM, Graph ); rdf( NameNode, fixm:uom, _NameUOM, Graph ); rdf( NameNode, plain:uom, _NameUOM, Graph ) ),
          Name=val(NameValue)
        );
        (
          rdf( NameNode,rdf:value,NameValue,Graph ),
          ( rdf( NameNode, aixm:uom, NameUOM, Graph ); rdf( NameNode, fixm:uom, NameUOM, Graph ); rdf( NameNode, plain:uom, NameUOM, Graph ) ),
          Name=xval(NameValue,NameUOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NameNilReason, Graph ),
          Name=nil(NameNilReason)
        );
        (
          rdf( NameNode,gml:indeterminatePosition, NameIndeterminate, Graph ),
          Name=indeterminate(NameIndeterminate)
        )
      )
  )
  ,(
    ( OtherOrganization='$null$',
      \+ rdf( Organization,fixm:'otherOrganization',_OtherOrganization,Graph )
    );
  ( rdf( Organization,fixm:'otherOrganization',OtherOrganizationNode,Graph )),
      (
        (
          rdf(OtherOrganizationNode,rdf:value,OtherOrganizationValue,Graph),
         \+ ( rdf( OtherOrganizationNode, aixm:uom, _OtherOrganizationUOM, Graph ); rdf( OtherOrganizationNode, fixm:uom, _OtherOrganizationUOM, Graph ); rdf( OtherOrganizationNode, plain:uom, _OtherOrganizationUOM, Graph ) ),
          OtherOrganization=val(OtherOrganizationValue)
        );
        (
          rdf( OtherOrganizationNode,rdf:value,OtherOrganizationValue,Graph ),
          ( rdf( OtherOrganizationNode, aixm:uom, OtherOrganizationUOM, Graph ); rdf( OtherOrganizationNode, fixm:uom, OtherOrganizationUOM, Graph ); rdf( OtherOrganizationNode, plain:uom, OtherOrganizationUOM, Graph ) ),
          OtherOrganization=xval(OtherOrganizationValue,OtherOrganizationUOM)
        );
        (
          rdf( OtherOrganizationNode,aixm:nilReason, OtherOrganizationNilReason, Graph ),
          OtherOrganization=nil(OtherOrganizationNilReason)
        );
        (
          rdf( OtherOrganizationNode,gml:indeterminatePosition, OtherOrganizationIndeterminate, Graph ),
          OtherOrganization=indeterminate(OtherOrganizationIndeterminate)
        )
      )
  )
  ,( ( Contact='$null$',
    \+ rdf( Organization,fixm:'contact', _Contact, Graph  )
   ; rdf(Organization,fixm:'contact', Contact, Graph ) )
  ) .

aixm_OrganisationAuthorityAssociation(Graph, OrganisationAuthorityAssociation, Type, Annotation, TheOrganisationAuthority) :-
  rdf(OrganisationAuthorityAssociation,rdf:type,aixm:'OrganisationAuthorityAssociation',Graph)
  ,(
    ( Type='$null$',
      \+ rdf( OrganisationAuthorityAssociation,aixm:'type',_Type,Graph )
    );
  ( rdf( OrganisationAuthorityAssociation,aixm:'type',TypeNode,Graph )),
      (
        (
          rdf(TypeNode,rdf:value,TypeValue,Graph),
         \+ ( rdf( TypeNode, aixm:uom, _TypeUOM, Graph ); rdf( TypeNode, fixm:uom, _TypeUOM, Graph ); rdf( TypeNode, plain:uom, _TypeUOM, Graph ) ),
          Type=val(TypeValue)
        );
        (
          rdf( TypeNode,rdf:value,TypeValue,Graph ),
          ( rdf( TypeNode, aixm:uom, TypeUOM, Graph ); rdf( TypeNode, fixm:uom, TypeUOM, Graph ); rdf( TypeNode, plain:uom, TypeUOM, Graph ) ),
          Type=xval(TypeValue,TypeUOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, TypeNilReason, Graph ),
          Type=nil(TypeNilReason)
        );
        (
          rdf( TypeNode,gml:indeterminatePosition, TypeIndeterminate, Graph ),
          Type=indeterminate(TypeIndeterminate)
        )
      )
  )
  ,findall(A, rdf(OrganisationAuthorityAssociation,aixm:'annotation',A,Graph), Annotation)
  ,rdf(OrganisationAuthorityAssociation,aixm:'theOrganisationAuthority',TheOrganisationAuthority,Graph) .

aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  rdf(ElevatedPoint,rdf:type,aixm:'ElevatedPoint',Graph)
  ,(
    ( Elevation='$null$',
      \+ rdf( ElevatedPoint,aixm:'elevation',_Elevation,Graph )
    );
  ( rdf( ElevatedPoint,aixm:'elevation',ElevationNode,Graph )),
      (
        (
          rdf(ElevationNode,rdf:value,ElevationValue,Graph),
         \+ ( rdf( ElevationNode, aixm:uom, _ElevationUOM, Graph ); rdf( ElevationNode, fixm:uom, _ElevationUOM, Graph ); rdf( ElevationNode, plain:uom, _ElevationUOM, Graph ) ),
          Elevation=val(ElevationValue)
        );
        (
          rdf( ElevationNode,rdf:value,ElevationValue,Graph ),
          ( rdf( ElevationNode, aixm:uom, ElevationUOM, Graph ); rdf( ElevationNode, fixm:uom, ElevationUOM, Graph ); rdf( ElevationNode, plain:uom, ElevationUOM, Graph ) ),
          Elevation=xval(ElevationValue,ElevationUOM)
        );
        (
          rdf( ElevationNode,aixm:nilReason, ElevationNilReason, Graph ),
          Elevation=nil(ElevationNilReason)
        );
        (
          rdf( ElevationNode,gml:indeterminatePosition, ElevationIndeterminate, Graph ),
          Elevation=indeterminate(ElevationIndeterminate)
        )
      )
  )
  ,(
    ( GeoidUndulation='$null$',
      \+ rdf( ElevatedPoint,aixm:'geoidUndulation',_GeoidUndulation,Graph )
    );
  ( rdf( ElevatedPoint,aixm:'geoidUndulation',GeoidUndulationNode,Graph )),
      (
        (
          rdf(GeoidUndulationNode,rdf:value,GeoidUndulationValue,Graph),
         \+ ( rdf( GeoidUndulationNode, aixm:uom, _GeoidUndulationUOM, Graph ); rdf( GeoidUndulationNode, fixm:uom, _GeoidUndulationUOM, Graph ); rdf( GeoidUndulationNode, plain:uom, _GeoidUndulationUOM, Graph ) ),
          GeoidUndulation=val(GeoidUndulationValue)
        );
        (
          rdf( GeoidUndulationNode,rdf:value,GeoidUndulationValue,Graph ),
          ( rdf( GeoidUndulationNode, aixm:uom, GeoidUndulationUOM, Graph ); rdf( GeoidUndulationNode, fixm:uom, GeoidUndulationUOM, Graph ); rdf( GeoidUndulationNode, plain:uom, GeoidUndulationUOM, Graph ) ),
          GeoidUndulation=xval(GeoidUndulationValue,GeoidUndulationUOM)
        );
        (
          rdf( GeoidUndulationNode,aixm:nilReason, GeoidUndulationNilReason, Graph ),
          GeoidUndulation=nil(GeoidUndulationNilReason)
        );
        (
          rdf( GeoidUndulationNode,gml:indeterminatePosition, GeoidUndulationIndeterminate, Graph ),
          GeoidUndulation=indeterminate(GeoidUndulationIndeterminate)
        )
      )
  )
  ,(
    ( VerticalDatum='$null$',
      \+ rdf( ElevatedPoint,aixm:'verticalDatum',_VerticalDatum,Graph )
    );
  ( rdf( ElevatedPoint,aixm:'verticalDatum',VerticalDatumNode,Graph )),
      (
        (
          rdf(VerticalDatumNode,rdf:value,VerticalDatumValue,Graph),
         \+ ( rdf( VerticalDatumNode, aixm:uom, _VerticalDatumUOM, Graph ); rdf( VerticalDatumNode, fixm:uom, _VerticalDatumUOM, Graph ); rdf( VerticalDatumNode, plain:uom, _VerticalDatumUOM, Graph ) ),
          VerticalDatum=val(VerticalDatumValue)
        );
        (
          rdf( VerticalDatumNode,rdf:value,VerticalDatumValue,Graph ),
          ( rdf( VerticalDatumNode, aixm:uom, VerticalDatumUOM, Graph ); rdf( VerticalDatumNode, fixm:uom, VerticalDatumUOM, Graph ); rdf( VerticalDatumNode, plain:uom, VerticalDatumUOM, Graph ) ),
          VerticalDatum=xval(VerticalDatumValue,VerticalDatumUOM)
        );
        (
          rdf( VerticalDatumNode,aixm:nilReason, VerticalDatumNilReason, Graph ),
          VerticalDatum=nil(VerticalDatumNilReason)
        );
        (
          rdf( VerticalDatumNode,gml:indeterminatePosition, VerticalDatumIndeterminate, Graph ),
          VerticalDatum=indeterminate(VerticalDatumIndeterminate)
        )
      )
  )
  ,(
    ( VerticalAccuracy='$null$',
      \+ rdf( ElevatedPoint,aixm:'verticalAccuracy',_VerticalAccuracy,Graph )
    );
  ( rdf( ElevatedPoint,aixm:'verticalAccuracy',VerticalAccuracyNode,Graph )),
      (
        (
          rdf(VerticalAccuracyNode,rdf:value,VerticalAccuracyValue,Graph),
         \+ ( rdf( VerticalAccuracyNode, aixm:uom, _VerticalAccuracyUOM, Graph ); rdf( VerticalAccuracyNode, fixm:uom, _VerticalAccuracyUOM, Graph ); rdf( VerticalAccuracyNode, plain:uom, _VerticalAccuracyUOM, Graph ) ),
          VerticalAccuracy=val(VerticalAccuracyValue)
        );
        (
          rdf( VerticalAccuracyNode,rdf:value,VerticalAccuracyValue,Graph ),
          ( rdf( VerticalAccuracyNode, aixm:uom, VerticalAccuracyUOM, Graph ); rdf( VerticalAccuracyNode, fixm:uom, VerticalAccuracyUOM, Graph ); rdf( VerticalAccuracyNode, plain:uom, VerticalAccuracyUOM, Graph ) ),
          VerticalAccuracy=xval(VerticalAccuracyValue,VerticalAccuracyUOM)
        );
        (
          rdf( VerticalAccuracyNode,aixm:nilReason, VerticalAccuracyNilReason, Graph ),
          VerticalAccuracy=nil(VerticalAccuracyNilReason)
        );
        (
          rdf( VerticalAccuracyNode,gml:indeterminatePosition, VerticalAccuracyIndeterminate, Graph ),
          VerticalAccuracy=indeterminate(VerticalAccuracyIndeterminate)
        )
      )
  ) .

fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel) :-
  rdf(EfplPoint4D,rdf:type,fixm:'EfplPoint4D',Graph)
  ,( ( FlightLevel='$null$',
    \+ rdf( EfplPoint4D,fixm:'flightLevel', _FlightLevel, Graph  )
   ; rdf(EfplPoint4D,fixm:'flightLevel', FlightLevel, Graph ) )
  ) .

fixm_AircraftOperator(Graph, AircraftOperator, OperatingOrganization, OperatorCategory) :-
  rdf(AircraftOperator,rdf:type,fixm:'AircraftOperator',Graph)
  ,(
    ( OperatingOrganization='$null$',
      \+ rdf( AircraftOperator,fixm:'operatingOrganization',_OperatingOrganization,Graph )
    );
  ( rdf( AircraftOperator,fixm:'operatingOrganization',OperatingOrganizationNode,Graph )),
      (
        (
          rdf(OperatingOrganizationNode,rdf:value,OperatingOrganizationValue,Graph),
         \+ ( rdf( OperatingOrganizationNode, aixm:uom, _OperatingOrganizationUOM, Graph ); rdf( OperatingOrganizationNode, fixm:uom, _OperatingOrganizationUOM, Graph ); rdf( OperatingOrganizationNode, plain:uom, _OperatingOrganizationUOM, Graph ) ),
          OperatingOrganization=val(OperatingOrganizationValue)
        );
        (
          rdf( OperatingOrganizationNode,rdf:value,OperatingOrganizationValue,Graph ),
          ( rdf( OperatingOrganizationNode, aixm:uom, OperatingOrganizationUOM, Graph ); rdf( OperatingOrganizationNode, fixm:uom, OperatingOrganizationUOM, Graph ); rdf( OperatingOrganizationNode, plain:uom, OperatingOrganizationUOM, Graph ) ),
          OperatingOrganization=xval(OperatingOrganizationValue,OperatingOrganizationUOM)
        );
        (
          rdf( OperatingOrganizationNode,aixm:nilReason, OperatingOrganizationNilReason, Graph ),
          OperatingOrganization=nil(OperatingOrganizationNilReason)
        );
        (
          rdf( OperatingOrganizationNode,gml:indeterminatePosition, OperatingOrganizationIndeterminate, Graph ),
          OperatingOrganization=indeterminate(OperatingOrganizationIndeterminate)
        )
      )
  )
  ,(
    ( OperatorCategory='$null$',
      \+ rdf( AircraftOperator,fixm:'operatorCategory',_OperatorCategory,Graph )
    );
  ( rdf( AircraftOperator,fixm:'operatorCategory',OperatorCategoryNode,Graph )),
      (
        (
          rdf(OperatorCategoryNode,rdf:value,OperatorCategoryValue,Graph),
         \+ ( rdf( OperatorCategoryNode, aixm:uom, _OperatorCategoryUOM, Graph ); rdf( OperatorCategoryNode, fixm:uom, _OperatorCategoryUOM, Graph ); rdf( OperatorCategoryNode, plain:uom, _OperatorCategoryUOM, Graph ) ),
          OperatorCategory=val(OperatorCategoryValue)
        );
        (
          rdf( OperatorCategoryNode,rdf:value,OperatorCategoryValue,Graph ),
          ( rdf( OperatorCategoryNode, aixm:uom, OperatorCategoryUOM, Graph ); rdf( OperatorCategoryNode, fixm:uom, OperatorCategoryUOM, Graph ); rdf( OperatorCategoryNode, plain:uom, OperatorCategoryUOM, Graph ) ),
          OperatorCategory=xval(OperatorCategoryValue,OperatorCategoryUOM)
        );
        (
          rdf( OperatorCategoryNode,aixm:nilReason, OperatorCategoryNilReason, Graph ),
          OperatorCategory=nil(OperatorCategoryNilReason)
        );
        (
          rdf( OperatorCategoryNode,gml:indeterminatePosition, OperatorCategoryIndeterminate, Graph ),
          OperatorCategory=indeterminate(OperatorCategoryIndeterminate)
        )
      )
  ) .

gml_Point(Graph, Point) :-
  subClassOf(T,gml:'Point')
  ,rdf(Point,rdf:type,T,Graph) .

fixm_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair) :-
  rdf(EfplTrajectoryRoutePair,rdf:type,fixm:'EfplTrajectoryRoutePair',Graph) .

fixm_RoutePoint(Graph, RoutePoint, Constraint) :-
  rdf(RoutePoint,rdf:type,fixm:'RoutePoint',Graph)
  ,findall(A, rdf(RoutePoint,fixm:'constraint',A,Graph), Constraint) .

fixm_BeaconCodeAssignment(Graph, BeaconCodeAssignment, CurrentBeaconCode, PreviousBeaconCode, ReassignedBeaconCode, ReassigningUnit) :-
  rdf(BeaconCodeAssignment,rdf:type,fixm:'BeaconCodeAssignment',Graph)
  ,(
    ( CurrentBeaconCode='$null$',
      \+ rdf( BeaconCodeAssignment,fixm:'currentBeaconCode',_CurrentBeaconCode,Graph )
    );
  ( rdf( BeaconCodeAssignment,fixm:'currentBeaconCode',CurrentBeaconCodeNode,Graph )),
      (
        (
          rdf(CurrentBeaconCodeNode,rdf:value,CurrentBeaconCodeValue,Graph),
         \+ ( rdf( CurrentBeaconCodeNode, aixm:uom, _CurrentBeaconCodeUOM, Graph ); rdf( CurrentBeaconCodeNode, fixm:uom, _CurrentBeaconCodeUOM, Graph ); rdf( CurrentBeaconCodeNode, plain:uom, _CurrentBeaconCodeUOM, Graph ) ),
          CurrentBeaconCode=val(CurrentBeaconCodeValue)
        );
        (
          rdf( CurrentBeaconCodeNode,rdf:value,CurrentBeaconCodeValue,Graph ),
          ( rdf( CurrentBeaconCodeNode, aixm:uom, CurrentBeaconCodeUOM, Graph ); rdf( CurrentBeaconCodeNode, fixm:uom, CurrentBeaconCodeUOM, Graph ); rdf( CurrentBeaconCodeNode, plain:uom, CurrentBeaconCodeUOM, Graph ) ),
          CurrentBeaconCode=xval(CurrentBeaconCodeValue,CurrentBeaconCodeUOM)
        );
        (
          rdf( CurrentBeaconCodeNode,aixm:nilReason, CurrentBeaconCodeNilReason, Graph ),
          CurrentBeaconCode=nil(CurrentBeaconCodeNilReason)
        );
        (
          rdf( CurrentBeaconCodeNode,gml:indeterminatePosition, CurrentBeaconCodeIndeterminate, Graph ),
          CurrentBeaconCode=indeterminate(CurrentBeaconCodeIndeterminate)
        )
      )
  )
  ,(
    ( PreviousBeaconCode='$null$',
      \+ rdf( BeaconCodeAssignment,fixm:'previousBeaconCode',_PreviousBeaconCode,Graph )
    );
  ( rdf( BeaconCodeAssignment,fixm:'previousBeaconCode',PreviousBeaconCodeNode,Graph )),
      (
        (
          rdf(PreviousBeaconCodeNode,rdf:value,PreviousBeaconCodeValue,Graph),
         \+ ( rdf( PreviousBeaconCodeNode, aixm:uom, _PreviousBeaconCodeUOM, Graph ); rdf( PreviousBeaconCodeNode, fixm:uom, _PreviousBeaconCodeUOM, Graph ); rdf( PreviousBeaconCodeNode, plain:uom, _PreviousBeaconCodeUOM, Graph ) ),
          PreviousBeaconCode=val(PreviousBeaconCodeValue)
        );
        (
          rdf( PreviousBeaconCodeNode,rdf:value,PreviousBeaconCodeValue,Graph ),
          ( rdf( PreviousBeaconCodeNode, aixm:uom, PreviousBeaconCodeUOM, Graph ); rdf( PreviousBeaconCodeNode, fixm:uom, PreviousBeaconCodeUOM, Graph ); rdf( PreviousBeaconCodeNode, plain:uom, PreviousBeaconCodeUOM, Graph ) ),
          PreviousBeaconCode=xval(PreviousBeaconCodeValue,PreviousBeaconCodeUOM)
        );
        (
          rdf( PreviousBeaconCodeNode,aixm:nilReason, PreviousBeaconCodeNilReason, Graph ),
          PreviousBeaconCode=nil(PreviousBeaconCodeNilReason)
        );
        (
          rdf( PreviousBeaconCodeNode,gml:indeterminatePosition, PreviousBeaconCodeIndeterminate, Graph ),
          PreviousBeaconCode=indeterminate(PreviousBeaconCodeIndeterminate)
        )
      )
  )
  ,(
    ( ReassignedBeaconCode='$null$',
      \+ rdf( BeaconCodeAssignment,fixm:'reassignedBeaconCode',_ReassignedBeaconCode,Graph )
    );
  ( rdf( BeaconCodeAssignment,fixm:'reassignedBeaconCode',ReassignedBeaconCodeNode,Graph )),
      (
        (
          rdf(ReassignedBeaconCodeNode,rdf:value,ReassignedBeaconCodeValue,Graph),
         \+ ( rdf( ReassignedBeaconCodeNode, aixm:uom, _ReassignedBeaconCodeUOM, Graph ); rdf( ReassignedBeaconCodeNode, fixm:uom, _ReassignedBeaconCodeUOM, Graph ); rdf( ReassignedBeaconCodeNode, plain:uom, _ReassignedBeaconCodeUOM, Graph ) ),
          ReassignedBeaconCode=val(ReassignedBeaconCodeValue)
        );
        (
          rdf( ReassignedBeaconCodeNode,rdf:value,ReassignedBeaconCodeValue,Graph ),
          ( rdf( ReassignedBeaconCodeNode, aixm:uom, ReassignedBeaconCodeUOM, Graph ); rdf( ReassignedBeaconCodeNode, fixm:uom, ReassignedBeaconCodeUOM, Graph ); rdf( ReassignedBeaconCodeNode, plain:uom, ReassignedBeaconCodeUOM, Graph ) ),
          ReassignedBeaconCode=xval(ReassignedBeaconCodeValue,ReassignedBeaconCodeUOM)
        );
        (
          rdf( ReassignedBeaconCodeNode,aixm:nilReason, ReassignedBeaconCodeNilReason, Graph ),
          ReassignedBeaconCode=nil(ReassignedBeaconCodeNilReason)
        );
        (
          rdf( ReassignedBeaconCodeNode,gml:indeterminatePosition, ReassignedBeaconCodeIndeterminate, Graph ),
          ReassignedBeaconCode=indeterminate(ReassignedBeaconCodeIndeterminate)
        )
      )
  )
  ,( ( ReassigningUnit='$null$',
    \+ rdf( BeaconCodeAssignment,fixm:'reassigningUnit', _ReassigningUnit, Graph  )
   ; rdf(BeaconCodeAssignment,fixm:'reassigningUnit', ReassigningUnit, Graph ) )
  ) .

fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile) :-
  rdf(FlightPerformanceData,rdf:type,fixm:'FlightPerformanceData',Graph)
  ,findall(A, rdf(FlightPerformanceData,fixm:'climbProfile',A,Graph), ClimbProfile)
  ,findall(A, rdf(FlightPerformanceData,fixm:'descentProfile',A,Graph), DescentProfile) .

fixm_ExpandedRoute(Graph, ExpandedRoute, RoutePoint) :-
  rdf(ExpandedRoute,rdf:type,fixm:'ExpandedRoute',Graph)
  ,findall(A, rdf(ExpandedRoute,fixm:'routePoint',A,Graph), RoutePoint) .

fixm_RouteConstraintOrPreference(Graph, RouteConstraintOrPreference, ConstraintType) :-
  rdf(RouteConstraintOrPreference,rdf:type,fixm:'RouteConstraintOrPreference',Graph)
  ,(
    ( ConstraintType='$null$',
      \+ rdf( RouteConstraintOrPreference,fixm:'constraintType',_ConstraintType,Graph )
    );
  ( rdf( RouteConstraintOrPreference,fixm:'constraintType',ConstraintTypeNode,Graph )),
      (
        (
          rdf(ConstraintTypeNode,rdf:value,ConstraintTypeValue,Graph),
         \+ ( rdf( ConstraintTypeNode, aixm:uom, _ConstraintTypeUOM, Graph ); rdf( ConstraintTypeNode, fixm:uom, _ConstraintTypeUOM, Graph ); rdf( ConstraintTypeNode, plain:uom, _ConstraintTypeUOM, Graph ) ),
          ConstraintType=val(ConstraintTypeValue)
        );
        (
          rdf( ConstraintTypeNode,rdf:value,ConstraintTypeValue,Graph ),
          ( rdf( ConstraintTypeNode, aixm:uom, ConstraintTypeUOM, Graph ); rdf( ConstraintTypeNode, fixm:uom, ConstraintTypeUOM, Graph ); rdf( ConstraintTypeNode, plain:uom, ConstraintTypeUOM, Graph ) ),
          ConstraintType=xval(ConstraintTypeValue,ConstraintTypeUOM)
        );
        (
          rdf( ConstraintTypeNode,aixm:nilReason, ConstraintTypeNilReason, Graph ),
          ConstraintType=nil(ConstraintTypeNilReason)
        );
        (
          rdf( ConstraintTypeNode,gml:indeterminatePosition, ConstraintTypeIndeterminate, Graph ),
          ConstraintType=indeterminate(ConstraintTypeIndeterminate)
        )
      )
  ) .

fixm_DeclarationText(Graph, DeclarationText, Compliance, Consignor, Shipper) :-
  rdf(DeclarationText,rdf:type,fixm:'DeclarationText',Graph)
  ,(
    ( Compliance='$null$',
      \+ rdf( DeclarationText,fixm:'compliance',_Compliance,Graph )
    );
  ( rdf( DeclarationText,fixm:'compliance',ComplianceNode,Graph )),
      (
        (
          rdf(ComplianceNode,rdf:value,ComplianceValue,Graph),
         \+ ( rdf( ComplianceNode, aixm:uom, _ComplianceUOM, Graph ); rdf( ComplianceNode, fixm:uom, _ComplianceUOM, Graph ); rdf( ComplianceNode, plain:uom, _ComplianceUOM, Graph ) ),
          Compliance=val(ComplianceValue)
        );
        (
          rdf( ComplianceNode,rdf:value,ComplianceValue,Graph ),
          ( rdf( ComplianceNode, aixm:uom, ComplianceUOM, Graph ); rdf( ComplianceNode, fixm:uom, ComplianceUOM, Graph ); rdf( ComplianceNode, plain:uom, ComplianceUOM, Graph ) ),
          Compliance=xval(ComplianceValue,ComplianceUOM)
        );
        (
          rdf( ComplianceNode,aixm:nilReason, ComplianceNilReason, Graph ),
          Compliance=nil(ComplianceNilReason)
        );
        (
          rdf( ComplianceNode,gml:indeterminatePosition, ComplianceIndeterminate, Graph ),
          Compliance=indeterminate(ComplianceIndeterminate)
        )
      )
  )
  ,(
    ( Consignor='$null$',
      \+ rdf( DeclarationText,fixm:'consignor',_Consignor,Graph )
    );
  ( rdf( DeclarationText,fixm:'consignor',ConsignorNode,Graph )),
      (
        (
          rdf(ConsignorNode,rdf:value,ConsignorValue,Graph),
         \+ ( rdf( ConsignorNode, aixm:uom, _ConsignorUOM, Graph ); rdf( ConsignorNode, fixm:uom, _ConsignorUOM, Graph ); rdf( ConsignorNode, plain:uom, _ConsignorUOM, Graph ) ),
          Consignor=val(ConsignorValue)
        );
        (
          rdf( ConsignorNode,rdf:value,ConsignorValue,Graph ),
          ( rdf( ConsignorNode, aixm:uom, ConsignorUOM, Graph ); rdf( ConsignorNode, fixm:uom, ConsignorUOM, Graph ); rdf( ConsignorNode, plain:uom, ConsignorUOM, Graph ) ),
          Consignor=xval(ConsignorValue,ConsignorUOM)
        );
        (
          rdf( ConsignorNode,aixm:nilReason, ConsignorNilReason, Graph ),
          Consignor=nil(ConsignorNilReason)
        );
        (
          rdf( ConsignorNode,gml:indeterminatePosition, ConsignorIndeterminate, Graph ),
          Consignor=indeterminate(ConsignorIndeterminate)
        )
      )
  )
  ,(
    ( Shipper='$null$',
      \+ rdf( DeclarationText,fixm:'shipper',_Shipper,Graph )
    );
  ( rdf( DeclarationText,fixm:'shipper',ShipperNode,Graph )),
      (
        (
          rdf(ShipperNode,rdf:value,ShipperValue,Graph),
         \+ ( rdf( ShipperNode, aixm:uom, _ShipperUOM, Graph ); rdf( ShipperNode, fixm:uom, _ShipperUOM, Graph ); rdf( ShipperNode, plain:uom, _ShipperUOM, Graph ) ),
          Shipper=val(ShipperValue)
        );
        (
          rdf( ShipperNode,rdf:value,ShipperValue,Graph ),
          ( rdf( ShipperNode, aixm:uom, ShipperUOM, Graph ); rdf( ShipperNode, fixm:uom, ShipperUOM, Graph ); rdf( ShipperNode, plain:uom, ShipperUOM, Graph ) ),
          Shipper=xval(ShipperValue,ShipperUOM)
        );
        (
          rdf( ShipperNode,aixm:nilReason, ShipperNilReason, Graph ),
          Shipper=nil(ShipperNilReason)
        );
        (
          rdf( ShipperNode,gml:indeterminatePosition, ShipperIndeterminate, Graph ),
          Shipper=indeterminate(ShipperIndeterminate)
        )
      )
  ) .

fixm_EstimatedElapsedTime(Graph, EstimatedElapsedTime, ElapsedTime, Location) :-
  rdf(EstimatedElapsedTime,rdf:type,fixm:'EstimatedElapsedTime',Graph)
  ,(
    ( ElapsedTime='$null$',
      \+ rdf( EstimatedElapsedTime,fixm:'elapsedTime',_ElapsedTime,Graph )
    );
  ( rdf( EstimatedElapsedTime,fixm:'elapsedTime',ElapsedTimeNode,Graph )),
      (
        (
          rdf(ElapsedTimeNode,rdf:value,ElapsedTimeValue,Graph),
         \+ ( rdf( ElapsedTimeNode, aixm:uom, _ElapsedTimeUOM, Graph ); rdf( ElapsedTimeNode, fixm:uom, _ElapsedTimeUOM, Graph ); rdf( ElapsedTimeNode, plain:uom, _ElapsedTimeUOM, Graph ) ),
          ElapsedTime=val(ElapsedTimeValue)
        );
        (
          rdf( ElapsedTimeNode,rdf:value,ElapsedTimeValue,Graph ),
          ( rdf( ElapsedTimeNode, aixm:uom, ElapsedTimeUOM, Graph ); rdf( ElapsedTimeNode, fixm:uom, ElapsedTimeUOM, Graph ); rdf( ElapsedTimeNode, plain:uom, ElapsedTimeUOM, Graph ) ),
          ElapsedTime=xval(ElapsedTimeValue,ElapsedTimeUOM)
        );
        (
          rdf( ElapsedTimeNode,aixm:nilReason, ElapsedTimeNilReason, Graph ),
          ElapsedTime=nil(ElapsedTimeNilReason)
        );
        (
          rdf( ElapsedTimeNode,gml:indeterminatePosition, ElapsedTimeIndeterminate, Graph ),
          ElapsedTime=indeterminate(ElapsedTimeIndeterminate)
        )
      )
  )
  ,(
    ( Location='$null$',
      \+ rdf( EstimatedElapsedTime,fixm:'location',_Location,Graph )
    );
  ( rdf( EstimatedElapsedTime,fixm:'location',LocationNode,Graph )),
      (
        (
          rdf(LocationNode,rdf:value,LocationValue,Graph),
         \+ ( rdf( LocationNode, aixm:uom, _LocationUOM, Graph ); rdf( LocationNode, fixm:uom, _LocationUOM, Graph ); rdf( LocationNode, plain:uom, _LocationUOM, Graph ) ),
          Location=val(LocationValue)
        );
        (
          rdf( LocationNode,rdf:value,LocationValue,Graph ),
          ( rdf( LocationNode, aixm:uom, LocationUOM, Graph ); rdf( LocationNode, fixm:uom, LocationUOM, Graph ); rdf( LocationNode, plain:uom, LocationUOM, Graph ) ),
          Location=xval(LocationValue,LocationUOM)
        );
        (
          rdf( LocationNode,aixm:nilReason, LocationNilReason, Graph ),
          Location=nil(LocationNilReason)
        );
        (
          rdf( LocationNode,gml:indeterminatePosition, LocationIndeterminate, Graph ),
          Location=indeterminate(LocationIndeterminate)
        )
      )
  ) .

fixm_ReportedTime(Graph, ReportedTime, Provenance, Time) :-
  rdf(ReportedTime,rdf:type,fixm:'ReportedTime',Graph)
  ,( ( Provenance='$null$',
    \+ rdf( ReportedTime,fixm:'provenance', _Provenance, Graph  )
   ; rdf(ReportedTime,fixm:'provenance', Provenance, Graph ) )
  )
  ,(
    ( Time='$null$',
      \+ rdf( ReportedTime,fixm:'time',_Time,Graph )
    );
  ( rdf( ReportedTime,fixm:'time',TimeNode,Graph )),
      (
        (
          rdf(TimeNode,rdf:value,TimeValue,Graph),
         \+ ( rdf( TimeNode, aixm:uom, _TimeUOM, Graph ); rdf( TimeNode, fixm:uom, _TimeUOM, Graph ); rdf( TimeNode, plain:uom, _TimeUOM, Graph ) ),
          Time=val(TimeValue)
        );
        (
          rdf( TimeNode,rdf:value,TimeValue,Graph ),
          ( rdf( TimeNode, aixm:uom, TimeUOM, Graph ); rdf( TimeNode, fixm:uom, TimeUOM, Graph ); rdf( TimeNode, plain:uom, TimeUOM, Graph ) ),
          Time=xval(TimeValue,TimeUOM)
        );
        (
          rdf( TimeNode,aixm:nilReason, TimeNilReason, Graph ),
          Time=nil(TimeNilReason)
        );
        (
          rdf( TimeNode,gml:indeterminatePosition, TimeIndeterminate, Graph ),
          Time=indeterminate(TimeIndeterminate)
        )
      )
  ) .

fixm_GeographicLocation(Graph, GeographicLocation, Pos, SrsName) :-
  subClassOf(T,fixm:'GeographicLocation')
  ,rdf(GeographicLocation,rdf:type,T,Graph)
  ,findall(A, rdf(GeographicLocation,fixm:'pos',A,Graph), Pos)
  ,(
    ( SrsName='$null$',
      \+ rdf( GeographicLocation,fixm:'srsName',_SrsName,Graph )
    );
  ( rdf( GeographicLocation,fixm:'srsName',SrsNameNode,Graph )),
      (
        (
          rdf(SrsNameNode,rdf:value,SrsNameValue,Graph),
         \+ ( rdf( SrsNameNode, aixm:uom, _SrsNameUOM, Graph ); rdf( SrsNameNode, fixm:uom, _SrsNameUOM, Graph ); rdf( SrsNameNode, plain:uom, _SrsNameUOM, Graph ) ),
          SrsName=val(SrsNameValue)
        );
        (
          rdf( SrsNameNode,rdf:value,SrsNameValue,Graph ),
          ( rdf( SrsNameNode, aixm:uom, SrsNameUOM, Graph ); rdf( SrsNameNode, fixm:uom, SrsNameUOM, Graph ); rdf( SrsNameNode, plain:uom, SrsNameUOM, Graph ) ),
          SrsName=xval(SrsNameValue,SrsNameUOM)
        );
        (
          rdf( SrsNameNode,aixm:nilReason, SrsNameNilReason, Graph ),
          SrsName=nil(SrsNameNilReason)
        );
        (
          rdf( SrsNameNode,gml:indeterminatePosition, SrsNameIndeterminate, Graph ),
          SrsName=indeterminate(SrsNameIndeterminate)
        )
      )
  ) .

aixm_LinguisticNote(Graph, LinguisticNote, Note) :-
  rdf(LinguisticNote,rdf:type,aixm:'LinguisticNote',Graph)
  ,(
    ( Note='$null$',
      \+ rdf( LinguisticNote,aixm:'note',_Note,Graph )
    );
  ( rdf( LinguisticNote,aixm:'note',NoteNode,Graph )),
      (
        (
          rdf(NoteNode,rdf:value,NoteValue,Graph),
         \+ ( rdf( NoteNode, aixm:uom, _NoteUOM, Graph ); rdf( NoteNode, fixm:uom, _NoteUOM, Graph ); rdf( NoteNode, plain:uom, _NoteUOM, Graph ) ),
          Note=val(NoteValue)
        );
        (
          rdf( NoteNode,rdf:value,NoteValue,Graph ),
          ( rdf( NoteNode, aixm:uom, NoteUOM, Graph ); rdf( NoteNode, fixm:uom, NoteUOM, Graph ); rdf( NoteNode, plain:uom, NoteUOM, Graph ) ),
          Note=xval(NoteValue,NoteUOM)
        );
        (
          rdf( NoteNode,aixm:nilReason, NoteNilReason, Graph ),
          Note=nil(NoteNilReason)
        );
        (
          rdf( NoteNode,gml:indeterminatePosition, NoteIndeterminate, Graph ),
          Note=indeterminate(NoteIndeterminate)
        )
      )
  ) .

aixm_Meteorology(Graph, Meteorology, FlightConditions, Visibility, VisibilityInterpretation, RunwayVisualRange, RunwayVisualRangeInterpretation, Annotation) :-
  rdf(Meteorology,rdf:type,aixm:'Meteorology',Graph)
  ,(
    ( FlightConditions='$null$',
      \+ rdf( Meteorology,aixm:'flightConditions',_FlightConditions,Graph )
    );
  ( rdf( Meteorology,aixm:'flightConditions',FlightConditionsNode,Graph )),
      (
        (
          rdf(FlightConditionsNode,rdf:value,FlightConditionsValue,Graph),
         \+ ( rdf( FlightConditionsNode, aixm:uom, _FlightConditionsUOM, Graph ); rdf( FlightConditionsNode, fixm:uom, _FlightConditionsUOM, Graph ); rdf( FlightConditionsNode, plain:uom, _FlightConditionsUOM, Graph ) ),
          FlightConditions=val(FlightConditionsValue)
        );
        (
          rdf( FlightConditionsNode,rdf:value,FlightConditionsValue,Graph ),
          ( rdf( FlightConditionsNode, aixm:uom, FlightConditionsUOM, Graph ); rdf( FlightConditionsNode, fixm:uom, FlightConditionsUOM, Graph ); rdf( FlightConditionsNode, plain:uom, FlightConditionsUOM, Graph ) ),
          FlightConditions=xval(FlightConditionsValue,FlightConditionsUOM)
        );
        (
          rdf( FlightConditionsNode,aixm:nilReason, FlightConditionsNilReason, Graph ),
          FlightConditions=nil(FlightConditionsNilReason)
        );
        (
          rdf( FlightConditionsNode,gml:indeterminatePosition, FlightConditionsIndeterminate, Graph ),
          FlightConditions=indeterminate(FlightConditionsIndeterminate)
        )
      )
  )
  ,(
    ( Visibility='$null$',
      \+ rdf( Meteorology,aixm:'visibility',_Visibility,Graph )
    );
  ( rdf( Meteorology,aixm:'visibility',VisibilityNode,Graph )),
      (
        (
          rdf(VisibilityNode,rdf:value,VisibilityValue,Graph),
         \+ ( rdf( VisibilityNode, aixm:uom, _VisibilityUOM, Graph ); rdf( VisibilityNode, fixm:uom, _VisibilityUOM, Graph ); rdf( VisibilityNode, plain:uom, _VisibilityUOM, Graph ) ),
          Visibility=val(VisibilityValue)
        );
        (
          rdf( VisibilityNode,rdf:value,VisibilityValue,Graph ),
          ( rdf( VisibilityNode, aixm:uom, VisibilityUOM, Graph ); rdf( VisibilityNode, fixm:uom, VisibilityUOM, Graph ); rdf( VisibilityNode, plain:uom, VisibilityUOM, Graph ) ),
          Visibility=xval(VisibilityValue,VisibilityUOM)
        );
        (
          rdf( VisibilityNode,aixm:nilReason, VisibilityNilReason, Graph ),
          Visibility=nil(VisibilityNilReason)
        );
        (
          rdf( VisibilityNode,gml:indeterminatePosition, VisibilityIndeterminate, Graph ),
          Visibility=indeterminate(VisibilityIndeterminate)
        )
      )
  )
  ,(
    ( VisibilityInterpretation='$null$',
      \+ rdf( Meteorology,aixm:'visibilityInterpretation',_VisibilityInterpretation,Graph )
    );
  ( rdf( Meteorology,aixm:'visibilityInterpretation',VisibilityInterpretationNode,Graph )),
      (
        (
          rdf(VisibilityInterpretationNode,rdf:value,VisibilityInterpretationValue,Graph),
         \+ ( rdf( VisibilityInterpretationNode, aixm:uom, _VisibilityInterpretationUOM, Graph ); rdf( VisibilityInterpretationNode, fixm:uom, _VisibilityInterpretationUOM, Graph ); rdf( VisibilityInterpretationNode, plain:uom, _VisibilityInterpretationUOM, Graph ) ),
          VisibilityInterpretation=val(VisibilityInterpretationValue)
        );
        (
          rdf( VisibilityInterpretationNode,rdf:value,VisibilityInterpretationValue,Graph ),
          ( rdf( VisibilityInterpretationNode, aixm:uom, VisibilityInterpretationUOM, Graph ); rdf( VisibilityInterpretationNode, fixm:uom, VisibilityInterpretationUOM, Graph ); rdf( VisibilityInterpretationNode, plain:uom, VisibilityInterpretationUOM, Graph ) ),
          VisibilityInterpretation=xval(VisibilityInterpretationValue,VisibilityInterpretationUOM)
        );
        (
          rdf( VisibilityInterpretationNode,aixm:nilReason, VisibilityInterpretationNilReason, Graph ),
          VisibilityInterpretation=nil(VisibilityInterpretationNilReason)
        );
        (
          rdf( VisibilityInterpretationNode,gml:indeterminatePosition, VisibilityInterpretationIndeterminate, Graph ),
          VisibilityInterpretation=indeterminate(VisibilityInterpretationIndeterminate)
        )
      )
  )
  ,(
    ( RunwayVisualRange='$null$',
      \+ rdf( Meteorology,aixm:'runwayVisualRange',_RunwayVisualRange,Graph )
    );
  ( rdf( Meteorology,aixm:'runwayVisualRange',RunwayVisualRangeNode,Graph )),
      (
        (
          rdf(RunwayVisualRangeNode,rdf:value,RunwayVisualRangeValue,Graph),
         \+ ( rdf( RunwayVisualRangeNode, aixm:uom, _RunwayVisualRangeUOM, Graph ); rdf( RunwayVisualRangeNode, fixm:uom, _RunwayVisualRangeUOM, Graph ); rdf( RunwayVisualRangeNode, plain:uom, _RunwayVisualRangeUOM, Graph ) ),
          RunwayVisualRange=val(RunwayVisualRangeValue)
        );
        (
          rdf( RunwayVisualRangeNode,rdf:value,RunwayVisualRangeValue,Graph ),
          ( rdf( RunwayVisualRangeNode, aixm:uom, RunwayVisualRangeUOM, Graph ); rdf( RunwayVisualRangeNode, fixm:uom, RunwayVisualRangeUOM, Graph ); rdf( RunwayVisualRangeNode, plain:uom, RunwayVisualRangeUOM, Graph ) ),
          RunwayVisualRange=xval(RunwayVisualRangeValue,RunwayVisualRangeUOM)
        );
        (
          rdf( RunwayVisualRangeNode,aixm:nilReason, RunwayVisualRangeNilReason, Graph ),
          RunwayVisualRange=nil(RunwayVisualRangeNilReason)
        );
        (
          rdf( RunwayVisualRangeNode,gml:indeterminatePosition, RunwayVisualRangeIndeterminate, Graph ),
          RunwayVisualRange=indeterminate(RunwayVisualRangeIndeterminate)
        )
      )
  )
  ,(
    ( RunwayVisualRangeInterpretation='$null$',
      \+ rdf( Meteorology,aixm:'runwayVisualRangeInterpretation',_RunwayVisualRangeInterpretation,Graph )
    );
  ( rdf( Meteorology,aixm:'runwayVisualRangeInterpretation',RunwayVisualRangeInterpretationNode,Graph )),
      (
        (
          rdf(RunwayVisualRangeInterpretationNode,rdf:value,RunwayVisualRangeInterpretationValue,Graph),
         \+ ( rdf( RunwayVisualRangeInterpretationNode, aixm:uom, _RunwayVisualRangeInterpretationUOM, Graph ); rdf( RunwayVisualRangeInterpretationNode, fixm:uom, _RunwayVisualRangeInterpretationUOM, Graph ); rdf( RunwayVisualRangeInterpretationNode, plain:uom, _RunwayVisualRangeInterpretationUOM, Graph ) ),
          RunwayVisualRangeInterpretation=val(RunwayVisualRangeInterpretationValue)
        );
        (
          rdf( RunwayVisualRangeInterpretationNode,rdf:value,RunwayVisualRangeInterpretationValue,Graph ),
          ( rdf( RunwayVisualRangeInterpretationNode, aixm:uom, RunwayVisualRangeInterpretationUOM, Graph ); rdf( RunwayVisualRangeInterpretationNode, fixm:uom, RunwayVisualRangeInterpretationUOM, Graph ); rdf( RunwayVisualRangeInterpretationNode, plain:uom, RunwayVisualRangeInterpretationUOM, Graph ) ),
          RunwayVisualRangeInterpretation=xval(RunwayVisualRangeInterpretationValue,RunwayVisualRangeInterpretationUOM)
        );
        (
          rdf( RunwayVisualRangeInterpretationNode,aixm:nilReason, RunwayVisualRangeInterpretationNilReason, Graph ),
          RunwayVisualRangeInterpretation=nil(RunwayVisualRangeInterpretationNilReason)
        );
        (
          rdf( RunwayVisualRangeInterpretationNode,gml:indeterminatePosition, RunwayVisualRangeInterpretationIndeterminate, Graph ),
          RunwayVisualRangeInterpretation=indeterminate(RunwayVisualRangeInterpretationIndeterminate)
        )
      )
  )
  ,findall(A, rdf(Meteorology,aixm:'annotation',A,Graph), Annotation) .

fixm_PointRange(Graph, PointRange, LateralRange, VerticalRange, TemporalRange) :-
  rdf(PointRange,rdf:type,fixm:'PointRange',Graph)
  ,( ( LateralRange='$null$',
    \+ rdf( PointRange,fixm:'lateralRange', _LateralRange, Graph  )
   ; rdf(PointRange,fixm:'lateralRange', LateralRange, Graph ) )
  )
  ,( ( VerticalRange='$null$',
    \+ rdf( PointRange,fixm:'verticalRange', _VerticalRange, Graph  )
   ; rdf(PointRange,fixm:'verticalRange', VerticalRange, Graph ) )
  )
  ,( ( TemporalRange='$null$',
    \+ rdf( PointRange,fixm:'temporalRange', _TemporalRange, Graph  )
   ; rdf(PointRange,fixm:'temporalRange', TemporalRange, Graph ) )
  ) .

aixm_City(Graph, City, Name, Annotation) :-
  rdf(City,rdf:type,aixm:'City',Graph)
  ,(
    ( Name='$null$',
      \+ rdf( City,aixm:'name',_Name,Graph )
    );
  ( rdf( City,aixm:'name',NameNode,Graph )),
      (
        (
          rdf(NameNode,rdf:value,NameValue,Graph),
         \+ ( rdf( NameNode, aixm:uom, _NameUOM, Graph ); rdf( NameNode, fixm:uom, _NameUOM, Graph ); rdf( NameNode, plain:uom, _NameUOM, Graph ) ),
          Name=val(NameValue)
        );
        (
          rdf( NameNode,rdf:value,NameValue,Graph ),
          ( rdf( NameNode, aixm:uom, NameUOM, Graph ); rdf( NameNode, fixm:uom, NameUOM, Graph ); rdf( NameNode, plain:uom, NameUOM, Graph ) ),
          Name=xval(NameValue,NameUOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NameNilReason, Graph ),
          Name=nil(NameNilReason)
        );
        (
          rdf( NameNode,gml:indeterminatePosition, NameIndeterminate, Graph ),
          Name=indeterminate(NameIndeterminate)
        )
      )
  )
  ,findall(A, rdf(City,aixm:'annotation',A,Graph), Annotation) .

aixm_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role, TheOrganisationAuthority) :-
  rdf(AirportHeliportResponsibilityOrganisation,rdf:type,aixm:'AirportHeliportResponsibilityOrganisation',Graph)
  ,(
    ( Role='$null$',
      \+ rdf( AirportHeliportResponsibilityOrganisation,aixm:'role',_Role,Graph )
    );
  ( rdf( AirportHeliportResponsibilityOrganisation,aixm:'role',RoleNode,Graph )),
      (
        (
          rdf(RoleNode,rdf:value,RoleValue,Graph),
         \+ ( rdf( RoleNode, aixm:uom, _RoleUOM, Graph ); rdf( RoleNode, fixm:uom, _RoleUOM, Graph ); rdf( RoleNode, plain:uom, _RoleUOM, Graph ) ),
          Role=val(RoleValue)
        );
        (
          rdf( RoleNode,rdf:value,RoleValue,Graph ),
          ( rdf( RoleNode, aixm:uom, RoleUOM, Graph ); rdf( RoleNode, fixm:uom, RoleUOM, Graph ); rdf( RoleNode, plain:uom, RoleUOM, Graph ) ),
          Role=xval(RoleValue,RoleUOM)
        );
        (
          rdf( RoleNode,aixm:nilReason, RoleNilReason, Graph ),
          Role=nil(RoleNilReason)
        );
        (
          rdf( RoleNode,gml:indeterminatePosition, RoleIndeterminate, Graph ),
          Role=indeterminate(RoleIndeterminate)
        )
      )
  )
  ,rdf(AirportHeliportResponsibilityOrganisation,aixm:'theOrganisationAuthority',TheOrganisationAuthority,Graph) .

fixm_AirspeedRange(Graph, AirspeedRange, LowerSpeed, UpperSpeed) :-
  rdf(AirspeedRange,rdf:type,fixm:'AirspeedRange',Graph)
  ,(
    ( LowerSpeed='$null$',
      \+ rdf( AirspeedRange,fixm:'lowerSpeed',_LowerSpeed,Graph )
    );
  ( rdf( AirspeedRange,fixm:'lowerSpeed',LowerSpeedNode,Graph )),
      (
        (
          rdf(LowerSpeedNode,rdf:value,LowerSpeedValue,Graph),
         \+ ( rdf( LowerSpeedNode, aixm:uom, _LowerSpeedUOM, Graph ); rdf( LowerSpeedNode, fixm:uom, _LowerSpeedUOM, Graph ); rdf( LowerSpeedNode, plain:uom, _LowerSpeedUOM, Graph ) ),
          LowerSpeed=val(LowerSpeedValue)
        );
        (
          rdf( LowerSpeedNode,rdf:value,LowerSpeedValue,Graph ),
          ( rdf( LowerSpeedNode, aixm:uom, LowerSpeedUOM, Graph ); rdf( LowerSpeedNode, fixm:uom, LowerSpeedUOM, Graph ); rdf( LowerSpeedNode, plain:uom, LowerSpeedUOM, Graph ) ),
          LowerSpeed=xval(LowerSpeedValue,LowerSpeedUOM)
        );
        (
          rdf( LowerSpeedNode,aixm:nilReason, LowerSpeedNilReason, Graph ),
          LowerSpeed=nil(LowerSpeedNilReason)
        );
        (
          rdf( LowerSpeedNode,gml:indeterminatePosition, LowerSpeedIndeterminate, Graph ),
          LowerSpeed=indeterminate(LowerSpeedIndeterminate)
        )
      )
  )
  ,(
    ( UpperSpeed='$null$',
      \+ rdf( AirspeedRange,fixm:'upperSpeed',_UpperSpeed,Graph )
    );
  ( rdf( AirspeedRange,fixm:'upperSpeed',UpperSpeedNode,Graph )),
      (
        (
          rdf(UpperSpeedNode,rdf:value,UpperSpeedValue,Graph),
         \+ ( rdf( UpperSpeedNode, aixm:uom, _UpperSpeedUOM, Graph ); rdf( UpperSpeedNode, fixm:uom, _UpperSpeedUOM, Graph ); rdf( UpperSpeedNode, plain:uom, _UpperSpeedUOM, Graph ) ),
          UpperSpeed=val(UpperSpeedValue)
        );
        (
          rdf( UpperSpeedNode,rdf:value,UpperSpeedValue,Graph ),
          ( rdf( UpperSpeedNode, aixm:uom, UpperSpeedUOM, Graph ); rdf( UpperSpeedNode, fixm:uom, UpperSpeedUOM, Graph ); rdf( UpperSpeedNode, plain:uom, UpperSpeedUOM, Graph ) ),
          UpperSpeed=xval(UpperSpeedValue,UpperSpeedUOM)
        );
        (
          rdf( UpperSpeedNode,aixm:nilReason, UpperSpeedNilReason, Graph ),
          UpperSpeed=nil(UpperSpeedNilReason)
        );
        (
          rdf( UpperSpeedNode,gml:indeterminatePosition, UpperSpeedIndeterminate, Graph ),
          UpperSpeed=indeterminate(UpperSpeedIndeterminate)
        )
      )
  ) .

fixm_RankedTrajectory(Graph, RankedTrajectory, Identifier, MaximumAcceptableDelay, AssignedIndicator, RouteTrajectoryPair) :-
  rdf(RankedTrajectory,rdf:type,fixm:'RankedTrajectory',Graph)
  ,(
    ( Identifier='$null$',
      \+ rdf( RankedTrajectory,fixm:'identifier',_Identifier,Graph )
    );
  ( rdf( RankedTrajectory,fixm:'identifier',IdentifierNode,Graph )),
      (
        (
          rdf(IdentifierNode,rdf:value,IdentifierValue,Graph),
         \+ ( rdf( IdentifierNode, aixm:uom, _IdentifierUOM, Graph ); rdf( IdentifierNode, fixm:uom, _IdentifierUOM, Graph ); rdf( IdentifierNode, plain:uom, _IdentifierUOM, Graph ) ),
          Identifier=val(IdentifierValue)
        );
        (
          rdf( IdentifierNode,rdf:value,IdentifierValue,Graph ),
          ( rdf( IdentifierNode, aixm:uom, IdentifierUOM, Graph ); rdf( IdentifierNode, fixm:uom, IdentifierUOM, Graph ); rdf( IdentifierNode, plain:uom, IdentifierUOM, Graph ) ),
          Identifier=xval(IdentifierValue,IdentifierUOM)
        );
        (
          rdf( IdentifierNode,aixm:nilReason, IdentifierNilReason, Graph ),
          Identifier=nil(IdentifierNilReason)
        );
        (
          rdf( IdentifierNode,gml:indeterminatePosition, IdentifierIndeterminate, Graph ),
          Identifier=indeterminate(IdentifierIndeterminate)
        )
      )
  )
  ,(
    ( MaximumAcceptableDelay='$null$',
      \+ rdf( RankedTrajectory,fixm:'maximumAcceptableDelay',_MaximumAcceptableDelay,Graph )
    );
  ( rdf( RankedTrajectory,fixm:'maximumAcceptableDelay',MaximumAcceptableDelayNode,Graph )),
      (
        (
          rdf(MaximumAcceptableDelayNode,rdf:value,MaximumAcceptableDelayValue,Graph),
         \+ ( rdf( MaximumAcceptableDelayNode, aixm:uom, _MaximumAcceptableDelayUOM, Graph ); rdf( MaximumAcceptableDelayNode, fixm:uom, _MaximumAcceptableDelayUOM, Graph ); rdf( MaximumAcceptableDelayNode, plain:uom, _MaximumAcceptableDelayUOM, Graph ) ),
          MaximumAcceptableDelay=val(MaximumAcceptableDelayValue)
        );
        (
          rdf( MaximumAcceptableDelayNode,rdf:value,MaximumAcceptableDelayValue,Graph ),
          ( rdf( MaximumAcceptableDelayNode, aixm:uom, MaximumAcceptableDelayUOM, Graph ); rdf( MaximumAcceptableDelayNode, fixm:uom, MaximumAcceptableDelayUOM, Graph ); rdf( MaximumAcceptableDelayNode, plain:uom, MaximumAcceptableDelayUOM, Graph ) ),
          MaximumAcceptableDelay=xval(MaximumAcceptableDelayValue,MaximumAcceptableDelayUOM)
        );
        (
          rdf( MaximumAcceptableDelayNode,aixm:nilReason, MaximumAcceptableDelayNilReason, Graph ),
          MaximumAcceptableDelay=nil(MaximumAcceptableDelayNilReason)
        );
        (
          rdf( MaximumAcceptableDelayNode,gml:indeterminatePosition, MaximumAcceptableDelayIndeterminate, Graph ),
          MaximumAcceptableDelay=indeterminate(MaximumAcceptableDelayIndeterminate)
        )
      )
  )
  ,(
    ( AssignedIndicator='$null$',
      \+ rdf( RankedTrajectory,fixm:'assignedIndicator',_AssignedIndicator,Graph )
    );
  ( rdf( RankedTrajectory,fixm:'assignedIndicator',AssignedIndicatorNode,Graph )),
      (
        (
          rdf(AssignedIndicatorNode,rdf:value,AssignedIndicatorValue,Graph),
         \+ ( rdf( AssignedIndicatorNode, aixm:uom, _AssignedIndicatorUOM, Graph ); rdf( AssignedIndicatorNode, fixm:uom, _AssignedIndicatorUOM, Graph ); rdf( AssignedIndicatorNode, plain:uom, _AssignedIndicatorUOM, Graph ) ),
          AssignedIndicator=val(AssignedIndicatorValue)
        );
        (
          rdf( AssignedIndicatorNode,rdf:value,AssignedIndicatorValue,Graph ),
          ( rdf( AssignedIndicatorNode, aixm:uom, AssignedIndicatorUOM, Graph ); rdf( AssignedIndicatorNode, fixm:uom, AssignedIndicatorUOM, Graph ); rdf( AssignedIndicatorNode, plain:uom, AssignedIndicatorUOM, Graph ) ),
          AssignedIndicator=xval(AssignedIndicatorValue,AssignedIndicatorUOM)
        );
        (
          rdf( AssignedIndicatorNode,aixm:nilReason, AssignedIndicatorNilReason, Graph ),
          AssignedIndicator=nil(AssignedIndicatorNilReason)
        );
        (
          rdf( AssignedIndicatorNode,gml:indeterminatePosition, AssignedIndicatorIndeterminate, Graph ),
          AssignedIndicator=indeterminate(AssignedIndicatorIndeterminate)
        )
      )
  )
  ,( ( RouteTrajectoryPair='$null$',
    \+ rdf( RankedTrajectory,fixm:'routeTrajectoryPair', _RouteTrajectoryPair, Graph  )
   ; rdf(RankedTrajectory,fixm:'routeTrajectoryPair', RouteTrajectoryPair, Graph ) )
  ) .

fixm_TrajectoryPointRole(Graph, TrajectoryPointRole, BottomOfClimb, BottomOfDescent, BoundaryPoint, FromGATToOAT, FromIFRToVFR, FromOATToGat, FromVFRToIFR, TopOfClimb, TopOfDescent) :-
  rdf(TrajectoryPointRole,rdf:type,fixm:'TrajectoryPointRole',Graph)
  ,(
    ( BottomOfClimb='$null$',
      \+ rdf( TrajectoryPointRole,fixm:'bottomOfClimb',_BottomOfClimb,Graph )
    );
  ( rdf( TrajectoryPointRole,fixm:'bottomOfClimb',BottomOfClimbNode,Graph )),
      (
        (
          rdf(BottomOfClimbNode,rdf:value,BottomOfClimbValue,Graph),
         \+ ( rdf( BottomOfClimbNode, aixm:uom, _BottomOfClimbUOM, Graph ); rdf( BottomOfClimbNode, fixm:uom, _BottomOfClimbUOM, Graph ); rdf( BottomOfClimbNode, plain:uom, _BottomOfClimbUOM, Graph ) ),
          BottomOfClimb=val(BottomOfClimbValue)
        );
        (
          rdf( BottomOfClimbNode,rdf:value,BottomOfClimbValue,Graph ),
          ( rdf( BottomOfClimbNode, aixm:uom, BottomOfClimbUOM, Graph ); rdf( BottomOfClimbNode, fixm:uom, BottomOfClimbUOM, Graph ); rdf( BottomOfClimbNode, plain:uom, BottomOfClimbUOM, Graph ) ),
          BottomOfClimb=xval(BottomOfClimbValue,BottomOfClimbUOM)
        );
        (
          rdf( BottomOfClimbNode,aixm:nilReason, BottomOfClimbNilReason, Graph ),
          BottomOfClimb=nil(BottomOfClimbNilReason)
        );
        (
          rdf( BottomOfClimbNode,gml:indeterminatePosition, BottomOfClimbIndeterminate, Graph ),
          BottomOfClimb=indeterminate(BottomOfClimbIndeterminate)
        )
      )
  )
  ,(
    ( BottomOfDescent='$null$',
      \+ rdf( TrajectoryPointRole,fixm:'bottomOfDescent',_BottomOfDescent,Graph )
    );
  ( rdf( TrajectoryPointRole,fixm:'bottomOfDescent',BottomOfDescentNode,Graph )),
      (
        (
          rdf(BottomOfDescentNode,rdf:value,BottomOfDescentValue,Graph),
         \+ ( rdf( BottomOfDescentNode, aixm:uom, _BottomOfDescentUOM, Graph ); rdf( BottomOfDescentNode, fixm:uom, _BottomOfDescentUOM, Graph ); rdf( BottomOfDescentNode, plain:uom, _BottomOfDescentUOM, Graph ) ),
          BottomOfDescent=val(BottomOfDescentValue)
        );
        (
          rdf( BottomOfDescentNode,rdf:value,BottomOfDescentValue,Graph ),
          ( rdf( BottomOfDescentNode, aixm:uom, BottomOfDescentUOM, Graph ); rdf( BottomOfDescentNode, fixm:uom, BottomOfDescentUOM, Graph ); rdf( BottomOfDescentNode, plain:uom, BottomOfDescentUOM, Graph ) ),
          BottomOfDescent=xval(BottomOfDescentValue,BottomOfDescentUOM)
        );
        (
          rdf( BottomOfDescentNode,aixm:nilReason, BottomOfDescentNilReason, Graph ),
          BottomOfDescent=nil(BottomOfDescentNilReason)
        );
        (
          rdf( BottomOfDescentNode,gml:indeterminatePosition, BottomOfDescentIndeterminate, Graph ),
          BottomOfDescent=indeterminate(BottomOfDescentIndeterminate)
        )
      )
  )
  ,(
    ( BoundaryPoint='$null$',
      \+ rdf( TrajectoryPointRole,fixm:'boundaryPoint',_BoundaryPoint,Graph )
    );
  ( rdf( TrajectoryPointRole,fixm:'boundaryPoint',BoundaryPointNode,Graph )),
      (
        (
          rdf(BoundaryPointNode,rdf:value,BoundaryPointValue,Graph),
         \+ ( rdf( BoundaryPointNode, aixm:uom, _BoundaryPointUOM, Graph ); rdf( BoundaryPointNode, fixm:uom, _BoundaryPointUOM, Graph ); rdf( BoundaryPointNode, plain:uom, _BoundaryPointUOM, Graph ) ),
          BoundaryPoint=val(BoundaryPointValue)
        );
        (
          rdf( BoundaryPointNode,rdf:value,BoundaryPointValue,Graph ),
          ( rdf( BoundaryPointNode, aixm:uom, BoundaryPointUOM, Graph ); rdf( BoundaryPointNode, fixm:uom, BoundaryPointUOM, Graph ); rdf( BoundaryPointNode, plain:uom, BoundaryPointUOM, Graph ) ),
          BoundaryPoint=xval(BoundaryPointValue,BoundaryPointUOM)
        );
        (
          rdf( BoundaryPointNode,aixm:nilReason, BoundaryPointNilReason, Graph ),
          BoundaryPoint=nil(BoundaryPointNilReason)
        );
        (
          rdf( BoundaryPointNode,gml:indeterminatePosition, BoundaryPointIndeterminate, Graph ),
          BoundaryPoint=indeterminate(BoundaryPointIndeterminate)
        )
      )
  )
  ,(
    ( FromGATToOAT='$null$',
      \+ rdf( TrajectoryPointRole,fixm:'fromGATToOAT',_FromGATToOAT,Graph )
    );
  ( rdf( TrajectoryPointRole,fixm:'fromGATToOAT',FromGATToOATNode,Graph )),
      (
        (
          rdf(FromGATToOATNode,rdf:value,FromGATToOATValue,Graph),
         \+ ( rdf( FromGATToOATNode, aixm:uom, _FromGATToOATUOM, Graph ); rdf( FromGATToOATNode, fixm:uom, _FromGATToOATUOM, Graph ); rdf( FromGATToOATNode, plain:uom, _FromGATToOATUOM, Graph ) ),
          FromGATToOAT=val(FromGATToOATValue)
        );
        (
          rdf( FromGATToOATNode,rdf:value,FromGATToOATValue,Graph ),
          ( rdf( FromGATToOATNode, aixm:uom, FromGATToOATUOM, Graph ); rdf( FromGATToOATNode, fixm:uom, FromGATToOATUOM, Graph ); rdf( FromGATToOATNode, plain:uom, FromGATToOATUOM, Graph ) ),
          FromGATToOAT=xval(FromGATToOATValue,FromGATToOATUOM)
        );
        (
          rdf( FromGATToOATNode,aixm:nilReason, FromGATToOATNilReason, Graph ),
          FromGATToOAT=nil(FromGATToOATNilReason)
        );
        (
          rdf( FromGATToOATNode,gml:indeterminatePosition, FromGATToOATIndeterminate, Graph ),
          FromGATToOAT=indeterminate(FromGATToOATIndeterminate)
        )
      )
  )
  ,(
    ( FromIFRToVFR='$null$',
      \+ rdf( TrajectoryPointRole,fixm:'fromIFRToVFR',_FromIFRToVFR,Graph )
    );
  ( rdf( TrajectoryPointRole,fixm:'fromIFRToVFR',FromIFRToVFRNode,Graph )),
      (
        (
          rdf(FromIFRToVFRNode,rdf:value,FromIFRToVFRValue,Graph),
         \+ ( rdf( FromIFRToVFRNode, aixm:uom, _FromIFRToVFRUOM, Graph ); rdf( FromIFRToVFRNode, fixm:uom, _FromIFRToVFRUOM, Graph ); rdf( FromIFRToVFRNode, plain:uom, _FromIFRToVFRUOM, Graph ) ),
          FromIFRToVFR=val(FromIFRToVFRValue)
        );
        (
          rdf( FromIFRToVFRNode,rdf:value,FromIFRToVFRValue,Graph ),
          ( rdf( FromIFRToVFRNode, aixm:uom, FromIFRToVFRUOM, Graph ); rdf( FromIFRToVFRNode, fixm:uom, FromIFRToVFRUOM, Graph ); rdf( FromIFRToVFRNode, plain:uom, FromIFRToVFRUOM, Graph ) ),
          FromIFRToVFR=xval(FromIFRToVFRValue,FromIFRToVFRUOM)
        );
        (
          rdf( FromIFRToVFRNode,aixm:nilReason, FromIFRToVFRNilReason, Graph ),
          FromIFRToVFR=nil(FromIFRToVFRNilReason)
        );
        (
          rdf( FromIFRToVFRNode,gml:indeterminatePosition, FromIFRToVFRIndeterminate, Graph ),
          FromIFRToVFR=indeterminate(FromIFRToVFRIndeterminate)
        )
      )
  )
  ,(
    ( FromOATToGat='$null$',
      \+ rdf( TrajectoryPointRole,fixm:'fromOATToGat',_FromOATToGat,Graph )
    );
  ( rdf( TrajectoryPointRole,fixm:'fromOATToGat',FromOATToGatNode,Graph )),
      (
        (
          rdf(FromOATToGatNode,rdf:value,FromOATToGatValue,Graph),
         \+ ( rdf( FromOATToGatNode, aixm:uom, _FromOATToGatUOM, Graph ); rdf( FromOATToGatNode, fixm:uom, _FromOATToGatUOM, Graph ); rdf( FromOATToGatNode, plain:uom, _FromOATToGatUOM, Graph ) ),
          FromOATToGat=val(FromOATToGatValue)
        );
        (
          rdf( FromOATToGatNode,rdf:value,FromOATToGatValue,Graph ),
          ( rdf( FromOATToGatNode, aixm:uom, FromOATToGatUOM, Graph ); rdf( FromOATToGatNode, fixm:uom, FromOATToGatUOM, Graph ); rdf( FromOATToGatNode, plain:uom, FromOATToGatUOM, Graph ) ),
          FromOATToGat=xval(FromOATToGatValue,FromOATToGatUOM)
        );
        (
          rdf( FromOATToGatNode,aixm:nilReason, FromOATToGatNilReason, Graph ),
          FromOATToGat=nil(FromOATToGatNilReason)
        );
        (
          rdf( FromOATToGatNode,gml:indeterminatePosition, FromOATToGatIndeterminate, Graph ),
          FromOATToGat=indeterminate(FromOATToGatIndeterminate)
        )
      )
  )
  ,(
    ( FromVFRToIFR='$null$',
      \+ rdf( TrajectoryPointRole,fixm:'fromVFRToIFR',_FromVFRToIFR,Graph )
    );
  ( rdf( TrajectoryPointRole,fixm:'fromVFRToIFR',FromVFRToIFRNode,Graph )),
      (
        (
          rdf(FromVFRToIFRNode,rdf:value,FromVFRToIFRValue,Graph),
         \+ ( rdf( FromVFRToIFRNode, aixm:uom, _FromVFRToIFRUOM, Graph ); rdf( FromVFRToIFRNode, fixm:uom, _FromVFRToIFRUOM, Graph ); rdf( FromVFRToIFRNode, plain:uom, _FromVFRToIFRUOM, Graph ) ),
          FromVFRToIFR=val(FromVFRToIFRValue)
        );
        (
          rdf( FromVFRToIFRNode,rdf:value,FromVFRToIFRValue,Graph ),
          ( rdf( FromVFRToIFRNode, aixm:uom, FromVFRToIFRUOM, Graph ); rdf( FromVFRToIFRNode, fixm:uom, FromVFRToIFRUOM, Graph ); rdf( FromVFRToIFRNode, plain:uom, FromVFRToIFRUOM, Graph ) ),
          FromVFRToIFR=xval(FromVFRToIFRValue,FromVFRToIFRUOM)
        );
        (
          rdf( FromVFRToIFRNode,aixm:nilReason, FromVFRToIFRNilReason, Graph ),
          FromVFRToIFR=nil(FromVFRToIFRNilReason)
        );
        (
          rdf( FromVFRToIFRNode,gml:indeterminatePosition, FromVFRToIFRIndeterminate, Graph ),
          FromVFRToIFR=indeterminate(FromVFRToIFRIndeterminate)
        )
      )
  )
  ,(
    ( TopOfClimb='$null$',
      \+ rdf( TrajectoryPointRole,fixm:'topOfClimb',_TopOfClimb,Graph )
    );
  ( rdf( TrajectoryPointRole,fixm:'topOfClimb',TopOfClimbNode,Graph )),
      (
        (
          rdf(TopOfClimbNode,rdf:value,TopOfClimbValue,Graph),
         \+ ( rdf( TopOfClimbNode, aixm:uom, _TopOfClimbUOM, Graph ); rdf( TopOfClimbNode, fixm:uom, _TopOfClimbUOM, Graph ); rdf( TopOfClimbNode, plain:uom, _TopOfClimbUOM, Graph ) ),
          TopOfClimb=val(TopOfClimbValue)
        );
        (
          rdf( TopOfClimbNode,rdf:value,TopOfClimbValue,Graph ),
          ( rdf( TopOfClimbNode, aixm:uom, TopOfClimbUOM, Graph ); rdf( TopOfClimbNode, fixm:uom, TopOfClimbUOM, Graph ); rdf( TopOfClimbNode, plain:uom, TopOfClimbUOM, Graph ) ),
          TopOfClimb=xval(TopOfClimbValue,TopOfClimbUOM)
        );
        (
          rdf( TopOfClimbNode,aixm:nilReason, TopOfClimbNilReason, Graph ),
          TopOfClimb=nil(TopOfClimbNilReason)
        );
        (
          rdf( TopOfClimbNode,gml:indeterminatePosition, TopOfClimbIndeterminate, Graph ),
          TopOfClimb=indeterminate(TopOfClimbIndeterminate)
        )
      )
  )
  ,(
    ( TopOfDescent='$null$',
      \+ rdf( TrajectoryPointRole,fixm:'topOfDescent',_TopOfDescent,Graph )
    );
  ( rdf( TrajectoryPointRole,fixm:'topOfDescent',TopOfDescentNode,Graph )),
      (
        (
          rdf(TopOfDescentNode,rdf:value,TopOfDescentValue,Graph),
         \+ ( rdf( TopOfDescentNode, aixm:uom, _TopOfDescentUOM, Graph ); rdf( TopOfDescentNode, fixm:uom, _TopOfDescentUOM, Graph ); rdf( TopOfDescentNode, plain:uom, _TopOfDescentUOM, Graph ) ),
          TopOfDescent=val(TopOfDescentValue)
        );
        (
          rdf( TopOfDescentNode,rdf:value,TopOfDescentValue,Graph ),
          ( rdf( TopOfDescentNode, aixm:uom, TopOfDescentUOM, Graph ); rdf( TopOfDescentNode, fixm:uom, TopOfDescentUOM, Graph ); rdf( TopOfDescentNode, plain:uom, TopOfDescentUOM, Graph ) ),
          TopOfDescent=xval(TopOfDescentValue,TopOfDescentUOM)
        );
        (
          rdf( TopOfDescentNode,aixm:nilReason, TopOfDescentNilReason, Graph ),
          TopOfDescent=nil(TopOfDescentNilReason)
        );
        (
          rdf( TopOfDescentNode,gml:indeterminatePosition, TopOfDescentIndeterminate, Graph ),
          TopOfDescent=indeterminate(TopOfDescentIndeterminate)
        )
      )
  ) .

fixm_CommunicationCapabilities(Graph, CommunicationCapabilities, OtherCommunicationCapabilities, OtherDataLinkCapabilities, DataLinkCode, SelectiveCallingCode, CommunicationCode) :-
  rdf(CommunicationCapabilities,rdf:type,fixm:'CommunicationCapabilities',Graph)
  ,(
    ( OtherCommunicationCapabilities='$null$',
      \+ rdf( CommunicationCapabilities,fixm:'otherCommunicationCapabilities',_OtherCommunicationCapabilities,Graph )
    );
  ( rdf( CommunicationCapabilities,fixm:'otherCommunicationCapabilities',OtherCommunicationCapabilitiesNode,Graph )),
      (
        (
          rdf(OtherCommunicationCapabilitiesNode,rdf:value,OtherCommunicationCapabilitiesValue,Graph),
         \+ ( rdf( OtherCommunicationCapabilitiesNode, aixm:uom, _OtherCommunicationCapabilitiesUOM, Graph ); rdf( OtherCommunicationCapabilitiesNode, fixm:uom, _OtherCommunicationCapabilitiesUOM, Graph ); rdf( OtherCommunicationCapabilitiesNode, plain:uom, _OtherCommunicationCapabilitiesUOM, Graph ) ),
          OtherCommunicationCapabilities=val(OtherCommunicationCapabilitiesValue)
        );
        (
          rdf( OtherCommunicationCapabilitiesNode,rdf:value,OtherCommunicationCapabilitiesValue,Graph ),
          ( rdf( OtherCommunicationCapabilitiesNode, aixm:uom, OtherCommunicationCapabilitiesUOM, Graph ); rdf( OtherCommunicationCapabilitiesNode, fixm:uom, OtherCommunicationCapabilitiesUOM, Graph ); rdf( OtherCommunicationCapabilitiesNode, plain:uom, OtherCommunicationCapabilitiesUOM, Graph ) ),
          OtherCommunicationCapabilities=xval(OtherCommunicationCapabilitiesValue,OtherCommunicationCapabilitiesUOM)
        );
        (
          rdf( OtherCommunicationCapabilitiesNode,aixm:nilReason, OtherCommunicationCapabilitiesNilReason, Graph ),
          OtherCommunicationCapabilities=nil(OtherCommunicationCapabilitiesNilReason)
        );
        (
          rdf( OtherCommunicationCapabilitiesNode,gml:indeterminatePosition, OtherCommunicationCapabilitiesIndeterminate, Graph ),
          OtherCommunicationCapabilities=indeterminate(OtherCommunicationCapabilitiesIndeterminate)
        )
      )
  )
  ,(
    ( OtherDataLinkCapabilities='$null$',
      \+ rdf( CommunicationCapabilities,fixm:'otherDataLinkCapabilities',_OtherDataLinkCapabilities,Graph )
    );
  ( rdf( CommunicationCapabilities,fixm:'otherDataLinkCapabilities',OtherDataLinkCapabilitiesNode,Graph )),
      (
        (
          rdf(OtherDataLinkCapabilitiesNode,rdf:value,OtherDataLinkCapabilitiesValue,Graph),
         \+ ( rdf( OtherDataLinkCapabilitiesNode, aixm:uom, _OtherDataLinkCapabilitiesUOM, Graph ); rdf( OtherDataLinkCapabilitiesNode, fixm:uom, _OtherDataLinkCapabilitiesUOM, Graph ); rdf( OtherDataLinkCapabilitiesNode, plain:uom, _OtherDataLinkCapabilitiesUOM, Graph ) ),
          OtherDataLinkCapabilities=val(OtherDataLinkCapabilitiesValue)
        );
        (
          rdf( OtherDataLinkCapabilitiesNode,rdf:value,OtherDataLinkCapabilitiesValue,Graph ),
          ( rdf( OtherDataLinkCapabilitiesNode, aixm:uom, OtherDataLinkCapabilitiesUOM, Graph ); rdf( OtherDataLinkCapabilitiesNode, fixm:uom, OtherDataLinkCapabilitiesUOM, Graph ); rdf( OtherDataLinkCapabilitiesNode, plain:uom, OtherDataLinkCapabilitiesUOM, Graph ) ),
          OtherDataLinkCapabilities=xval(OtherDataLinkCapabilitiesValue,OtherDataLinkCapabilitiesUOM)
        );
        (
          rdf( OtherDataLinkCapabilitiesNode,aixm:nilReason, OtherDataLinkCapabilitiesNilReason, Graph ),
          OtherDataLinkCapabilities=nil(OtherDataLinkCapabilitiesNilReason)
        );
        (
          rdf( OtherDataLinkCapabilitiesNode,gml:indeterminatePosition, OtherDataLinkCapabilitiesIndeterminate, Graph ),
          OtherDataLinkCapabilities=indeterminate(OtherDataLinkCapabilitiesIndeterminate)
        )
      )
  )
  ,findall(A, rdf(CommunicationCapabilities,fixm:'dataLinkCode',A,Graph), DataLinkCode)
  ,(
    ( SelectiveCallingCode='$null$',
      \+ rdf( CommunicationCapabilities,fixm:'selectiveCallingCode',_SelectiveCallingCode,Graph )
    );
  ( rdf( CommunicationCapabilities,fixm:'selectiveCallingCode',SelectiveCallingCodeNode,Graph )),
      (
        (
          rdf(SelectiveCallingCodeNode,rdf:value,SelectiveCallingCodeValue,Graph),
         \+ ( rdf( SelectiveCallingCodeNode, aixm:uom, _SelectiveCallingCodeUOM, Graph ); rdf( SelectiveCallingCodeNode, fixm:uom, _SelectiveCallingCodeUOM, Graph ); rdf( SelectiveCallingCodeNode, plain:uom, _SelectiveCallingCodeUOM, Graph ) ),
          SelectiveCallingCode=val(SelectiveCallingCodeValue)
        );
        (
          rdf( SelectiveCallingCodeNode,rdf:value,SelectiveCallingCodeValue,Graph ),
          ( rdf( SelectiveCallingCodeNode, aixm:uom, SelectiveCallingCodeUOM, Graph ); rdf( SelectiveCallingCodeNode, fixm:uom, SelectiveCallingCodeUOM, Graph ); rdf( SelectiveCallingCodeNode, plain:uom, SelectiveCallingCodeUOM, Graph ) ),
          SelectiveCallingCode=xval(SelectiveCallingCodeValue,SelectiveCallingCodeUOM)
        );
        (
          rdf( SelectiveCallingCodeNode,aixm:nilReason, SelectiveCallingCodeNilReason, Graph ),
          SelectiveCallingCode=nil(SelectiveCallingCodeNilReason)
        );
        (
          rdf( SelectiveCallingCodeNode,gml:indeterminatePosition, SelectiveCallingCodeIndeterminate, Graph ),
          SelectiveCallingCode=indeterminate(SelectiveCallingCodeIndeterminate)
        )
      )
  )
  ,findall(A, rdf(CommunicationCapabilities,fixm:'communicationCode',A,Graph), CommunicationCode) .

fixm_Dinghy(Graph, Dinghy, Quantity, TotalCapacity, Covered, Colour) :-
  rdf(Dinghy,rdf:type,fixm:'Dinghy',Graph)
  ,(
    ( Quantity='$null$',
      \+ rdf( Dinghy,fixm:'quantity',_Quantity,Graph )
    );
  ( rdf( Dinghy,fixm:'quantity',QuantityNode,Graph )),
      (
        (
          rdf(QuantityNode,rdf:value,QuantityValue,Graph),
         \+ ( rdf( QuantityNode, aixm:uom, _QuantityUOM, Graph ); rdf( QuantityNode, fixm:uom, _QuantityUOM, Graph ); rdf( QuantityNode, plain:uom, _QuantityUOM, Graph ) ),
          Quantity=val(QuantityValue)
        );
        (
          rdf( QuantityNode,rdf:value,QuantityValue,Graph ),
          ( rdf( QuantityNode, aixm:uom, QuantityUOM, Graph ); rdf( QuantityNode, fixm:uom, QuantityUOM, Graph ); rdf( QuantityNode, plain:uom, QuantityUOM, Graph ) ),
          Quantity=xval(QuantityValue,QuantityUOM)
        );
        (
          rdf( QuantityNode,aixm:nilReason, QuantityNilReason, Graph ),
          Quantity=nil(QuantityNilReason)
        );
        (
          rdf( QuantityNode,gml:indeterminatePosition, QuantityIndeterminate, Graph ),
          Quantity=indeterminate(QuantityIndeterminate)
        )
      )
  )
  ,(
    ( TotalCapacity='$null$',
      \+ rdf( Dinghy,fixm:'totalCapacity',_TotalCapacity,Graph )
    );
  ( rdf( Dinghy,fixm:'totalCapacity',TotalCapacityNode,Graph )),
      (
        (
          rdf(TotalCapacityNode,rdf:value,TotalCapacityValue,Graph),
         \+ ( rdf( TotalCapacityNode, aixm:uom, _TotalCapacityUOM, Graph ); rdf( TotalCapacityNode, fixm:uom, _TotalCapacityUOM, Graph ); rdf( TotalCapacityNode, plain:uom, _TotalCapacityUOM, Graph ) ),
          TotalCapacity=val(TotalCapacityValue)
        );
        (
          rdf( TotalCapacityNode,rdf:value,TotalCapacityValue,Graph ),
          ( rdf( TotalCapacityNode, aixm:uom, TotalCapacityUOM, Graph ); rdf( TotalCapacityNode, fixm:uom, TotalCapacityUOM, Graph ); rdf( TotalCapacityNode, plain:uom, TotalCapacityUOM, Graph ) ),
          TotalCapacity=xval(TotalCapacityValue,TotalCapacityUOM)
        );
        (
          rdf( TotalCapacityNode,aixm:nilReason, TotalCapacityNilReason, Graph ),
          TotalCapacity=nil(TotalCapacityNilReason)
        );
        (
          rdf( TotalCapacityNode,gml:indeterminatePosition, TotalCapacityIndeterminate, Graph ),
          TotalCapacity=indeterminate(TotalCapacityIndeterminate)
        )
      )
  )
  ,(
    ( Covered='$null$',
      \+ rdf( Dinghy,fixm:'covered',_Covered,Graph )
    );
  ( rdf( Dinghy,fixm:'covered',CoveredNode,Graph )),
      (
        (
          rdf(CoveredNode,rdf:value,CoveredValue,Graph),
         \+ ( rdf( CoveredNode, aixm:uom, _CoveredUOM, Graph ); rdf( CoveredNode, fixm:uom, _CoveredUOM, Graph ); rdf( CoveredNode, plain:uom, _CoveredUOM, Graph ) ),
          Covered=val(CoveredValue)
        );
        (
          rdf( CoveredNode,rdf:value,CoveredValue,Graph ),
          ( rdf( CoveredNode, aixm:uom, CoveredUOM, Graph ); rdf( CoveredNode, fixm:uom, CoveredUOM, Graph ); rdf( CoveredNode, plain:uom, CoveredUOM, Graph ) ),
          Covered=xval(CoveredValue,CoveredUOM)
        );
        (
          rdf( CoveredNode,aixm:nilReason, CoveredNilReason, Graph ),
          Covered=nil(CoveredNilReason)
        );
        (
          rdf( CoveredNode,gml:indeterminatePosition, CoveredIndeterminate, Graph ),
          Covered=indeterminate(CoveredIndeterminate)
        )
      )
  )
  ,(
    ( Colour='$null$',
      \+ rdf( Dinghy,fixm:'colour',_Colour,Graph )
    );
  ( rdf( Dinghy,fixm:'colour',ColourNode,Graph )),
      (
        (
          rdf(ColourNode,rdf:value,ColourValue,Graph),
         \+ ( rdf( ColourNode, aixm:uom, _ColourUOM, Graph ); rdf( ColourNode, fixm:uom, _ColourUOM, Graph ); rdf( ColourNode, plain:uom, _ColourUOM, Graph ) ),
          Colour=val(ColourValue)
        );
        (
          rdf( ColourNode,rdf:value,ColourValue,Graph ),
          ( rdf( ColourNode, aixm:uom, ColourUOM, Graph ); rdf( ColourNode, fixm:uom, ColourUOM, Graph ); rdf( ColourNode, plain:uom, ColourUOM, Graph ) ),
          Colour=xval(ColourValue,ColourUOM)
        );
        (
          rdf( ColourNode,aixm:nilReason, ColourNilReason, Graph ),
          Colour=nil(ColourNilReason)
        );
        (
          rdf( ColourNode,gml:indeterminatePosition, ColourIndeterminate, Graph ),
          Colour=indeterminate(ColourIndeterminate)
        )
      )
  ) .

aixm_ContactInformation(Graph, ContactInformation, Name, Title, Annotation, NetworkNode, Address, PhoneFax) :-
  rdf(ContactInformation,rdf:type,aixm:'ContactInformation',Graph)
  ,(
    ( Name='$null$',
      \+ rdf( ContactInformation,aixm:'name',_Name,Graph )
    );
  ( rdf( ContactInformation,aixm:'name',NameNode,Graph )),
      (
        (
          rdf(NameNode,rdf:value,NameValue,Graph),
         \+ ( rdf( NameNode, aixm:uom, _NameUOM, Graph ); rdf( NameNode, fixm:uom, _NameUOM, Graph ); rdf( NameNode, plain:uom, _NameUOM, Graph ) ),
          Name=val(NameValue)
        );
        (
          rdf( NameNode,rdf:value,NameValue,Graph ),
          ( rdf( NameNode, aixm:uom, NameUOM, Graph ); rdf( NameNode, fixm:uom, NameUOM, Graph ); rdf( NameNode, plain:uom, NameUOM, Graph ) ),
          Name=xval(NameValue,NameUOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NameNilReason, Graph ),
          Name=nil(NameNilReason)
        );
        (
          rdf( NameNode,gml:indeterminatePosition, NameIndeterminate, Graph ),
          Name=indeterminate(NameIndeterminate)
        )
      )
  )
  ,(
    ( Title='$null$',
      \+ rdf( ContactInformation,aixm:'title',_Title,Graph )
    );
  ( rdf( ContactInformation,aixm:'title',TitleNode,Graph )),
      (
        (
          rdf(TitleNode,rdf:value,TitleValue,Graph),
         \+ ( rdf( TitleNode, aixm:uom, _TitleUOM, Graph ); rdf( TitleNode, fixm:uom, _TitleUOM, Graph ); rdf( TitleNode, plain:uom, _TitleUOM, Graph ) ),
          Title=val(TitleValue)
        );
        (
          rdf( TitleNode,rdf:value,TitleValue,Graph ),
          ( rdf( TitleNode, aixm:uom, TitleUOM, Graph ); rdf( TitleNode, fixm:uom, TitleUOM, Graph ); rdf( TitleNode, plain:uom, TitleUOM, Graph ) ),
          Title=xval(TitleValue,TitleUOM)
        );
        (
          rdf( TitleNode,aixm:nilReason, TitleNilReason, Graph ),
          Title=nil(TitleNilReason)
        );
        (
          rdf( TitleNode,gml:indeterminatePosition, TitleIndeterminate, Graph ),
          Title=indeterminate(TitleIndeterminate)
        )
      )
  )
  ,findall(A, rdf(ContactInformation,aixm:'annotation',A,Graph), Annotation)
  ,findall(A, rdf(ContactInformation,aixm:'networkNode',A,Graph), NetworkNode)
  ,findall(A, rdf(ContactInformation,aixm:'address',A,Graph), Address)
  ,findall(A, rdf(ContactInformation,aixm:'phoneFax',A,Graph), PhoneFax) .

fixm_PlannedReportingPosition(Graph, PlannedReportingPosition, Position, PositionAltitude, PositionEstimatedTime) :-
  rdf(PlannedReportingPosition,rdf:type,fixm:'PlannedReportingPosition',Graph)
  ,( ( Position='$null$',
    \+ rdf( PlannedReportingPosition,fixm:'position', _Position, Graph  )
   ; rdf(PlannedReportingPosition,fixm:'position', Position, Graph ) )
  )
  ,(
    ( PositionAltitude='$null$',
      \+ rdf( PlannedReportingPosition,fixm:'positionAltitude',_PositionAltitude,Graph )
    );
  ( rdf( PlannedReportingPosition,fixm:'positionAltitude',PositionAltitudeNode,Graph )),
      (
        (
          rdf(PositionAltitudeNode,rdf:value,PositionAltitudeValue,Graph),
         \+ ( rdf( PositionAltitudeNode, aixm:uom, _PositionAltitudeUOM, Graph ); rdf( PositionAltitudeNode, fixm:uom, _PositionAltitudeUOM, Graph ); rdf( PositionAltitudeNode, plain:uom, _PositionAltitudeUOM, Graph ) ),
          PositionAltitude=val(PositionAltitudeValue)
        );
        (
          rdf( PositionAltitudeNode,rdf:value,PositionAltitudeValue,Graph ),
          ( rdf( PositionAltitudeNode, aixm:uom, PositionAltitudeUOM, Graph ); rdf( PositionAltitudeNode, fixm:uom, PositionAltitudeUOM, Graph ); rdf( PositionAltitudeNode, plain:uom, PositionAltitudeUOM, Graph ) ),
          PositionAltitude=xval(PositionAltitudeValue,PositionAltitudeUOM)
        );
        (
          rdf( PositionAltitudeNode,aixm:nilReason, PositionAltitudeNilReason, Graph ),
          PositionAltitude=nil(PositionAltitudeNilReason)
        );
        (
          rdf( PositionAltitudeNode,gml:indeterminatePosition, PositionAltitudeIndeterminate, Graph ),
          PositionAltitude=indeterminate(PositionAltitudeIndeterminate)
        )
      )
  )
  ,(
    ( PositionEstimatedTime='$null$',
      \+ rdf( PlannedReportingPosition,fixm:'positionEstimatedTime',_PositionEstimatedTime,Graph )
    );
  ( rdf( PlannedReportingPosition,fixm:'positionEstimatedTime',PositionEstimatedTimeNode,Graph )),
      (
        (
          rdf(PositionEstimatedTimeNode,rdf:value,PositionEstimatedTimeValue,Graph),
         \+ ( rdf( PositionEstimatedTimeNode, aixm:uom, _PositionEstimatedTimeUOM, Graph ); rdf( PositionEstimatedTimeNode, fixm:uom, _PositionEstimatedTimeUOM, Graph ); rdf( PositionEstimatedTimeNode, plain:uom, _PositionEstimatedTimeUOM, Graph ) ),
          PositionEstimatedTime=val(PositionEstimatedTimeValue)
        );
        (
          rdf( PositionEstimatedTimeNode,rdf:value,PositionEstimatedTimeValue,Graph ),
          ( rdf( PositionEstimatedTimeNode, aixm:uom, PositionEstimatedTimeUOM, Graph ); rdf( PositionEstimatedTimeNode, fixm:uom, PositionEstimatedTimeUOM, Graph ); rdf( PositionEstimatedTimeNode, plain:uom, PositionEstimatedTimeUOM, Graph ) ),
          PositionEstimatedTime=xval(PositionEstimatedTimeValue,PositionEstimatedTimeUOM)
        );
        (
          rdf( PositionEstimatedTimeNode,aixm:nilReason, PositionEstimatedTimeNilReason, Graph ),
          PositionEstimatedTime=nil(PositionEstimatedTimeNilReason)
        );
        (
          rdf( PositionEstimatedTimeNode,gml:indeterminatePosition, PositionEstimatedTimeIndeterminate, Graph ),
          PositionEstimatedTime=indeterminate(PositionEstimatedTimeIndeterminate)
        )
      )
  ) .

fixm_SignificantPoint(Graph, SignificantPoint) :-
  rdf(SignificantPoint,rdf:type,fixm:'SignificantPoint',Graph) .

fixm_SupplementalData(Graph, SupplementalData, FuelEndurance, PersonsOnBoard, PilotInCommand) :-
  rdf(SupplementalData,rdf:type,fixm:'SupplementalData',Graph)
  ,(
    ( FuelEndurance='$null$',
      \+ rdf( SupplementalData,fixm:'fuelEndurance',_FuelEndurance,Graph )
    );
  ( rdf( SupplementalData,fixm:'fuelEndurance',FuelEnduranceNode,Graph )),
      (
        (
          rdf(FuelEnduranceNode,rdf:value,FuelEnduranceValue,Graph),
         \+ ( rdf( FuelEnduranceNode, aixm:uom, _FuelEnduranceUOM, Graph ); rdf( FuelEnduranceNode, fixm:uom, _FuelEnduranceUOM, Graph ); rdf( FuelEnduranceNode, plain:uom, _FuelEnduranceUOM, Graph ) ),
          FuelEndurance=val(FuelEnduranceValue)
        );
        (
          rdf( FuelEnduranceNode,rdf:value,FuelEnduranceValue,Graph ),
          ( rdf( FuelEnduranceNode, aixm:uom, FuelEnduranceUOM, Graph ); rdf( FuelEnduranceNode, fixm:uom, FuelEnduranceUOM, Graph ); rdf( FuelEnduranceNode, plain:uom, FuelEnduranceUOM, Graph ) ),
          FuelEndurance=xval(FuelEnduranceValue,FuelEnduranceUOM)
        );
        (
          rdf( FuelEnduranceNode,aixm:nilReason, FuelEnduranceNilReason, Graph ),
          FuelEndurance=nil(FuelEnduranceNilReason)
        );
        (
          rdf( FuelEnduranceNode,gml:indeterminatePosition, FuelEnduranceIndeterminate, Graph ),
          FuelEndurance=indeterminate(FuelEnduranceIndeterminate)
        )
      )
  )
  ,(
    ( PersonsOnBoard='$null$',
      \+ rdf( SupplementalData,fixm:'personsOnBoard',_PersonsOnBoard,Graph )
    );
  ( rdf( SupplementalData,fixm:'personsOnBoard',PersonsOnBoardNode,Graph )),
      (
        (
          rdf(PersonsOnBoardNode,rdf:value,PersonsOnBoardValue,Graph),
         \+ ( rdf( PersonsOnBoardNode, aixm:uom, _PersonsOnBoardUOM, Graph ); rdf( PersonsOnBoardNode, fixm:uom, _PersonsOnBoardUOM, Graph ); rdf( PersonsOnBoardNode, plain:uom, _PersonsOnBoardUOM, Graph ) ),
          PersonsOnBoard=val(PersonsOnBoardValue)
        );
        (
          rdf( PersonsOnBoardNode,rdf:value,PersonsOnBoardValue,Graph ),
          ( rdf( PersonsOnBoardNode, aixm:uom, PersonsOnBoardUOM, Graph ); rdf( PersonsOnBoardNode, fixm:uom, PersonsOnBoardUOM, Graph ); rdf( PersonsOnBoardNode, plain:uom, PersonsOnBoardUOM, Graph ) ),
          PersonsOnBoard=xval(PersonsOnBoardValue,PersonsOnBoardUOM)
        );
        (
          rdf( PersonsOnBoardNode,aixm:nilReason, PersonsOnBoardNilReason, Graph ),
          PersonsOnBoard=nil(PersonsOnBoardNilReason)
        );
        (
          rdf( PersonsOnBoardNode,gml:indeterminatePosition, PersonsOnBoardIndeterminate, Graph ),
          PersonsOnBoard=indeterminate(PersonsOnBoardIndeterminate)
        )
      )
  )
  ,( ( PilotInCommand='$null$',
    \+ rdf( SupplementalData,fixm:'pilotInCommand', _PilotInCommand, Graph  )
   ; rdf(SupplementalData,fixm:'pilotInCommand', PilotInCommand, Graph ) )
  ) .

fixm_DangerousGoods(Graph, DangerousGoods, GuidebookNumber, OnboardLocation, HandlingInformation, AircraftLimitation, AirWayBill, Shipment, PackageGroup, ShippingInformation) :-
  rdf(DangerousGoods,rdf:type,fixm:'DangerousGoods',Graph)
  ,(
    ( GuidebookNumber='$null$',
      \+ rdf( DangerousGoods,fixm:'guidebookNumber',_GuidebookNumber,Graph )
    );
  ( rdf( DangerousGoods,fixm:'guidebookNumber',GuidebookNumberNode,Graph )),
      (
        (
          rdf(GuidebookNumberNode,rdf:value,GuidebookNumberValue,Graph),
         \+ ( rdf( GuidebookNumberNode, aixm:uom, _GuidebookNumberUOM, Graph ); rdf( GuidebookNumberNode, fixm:uom, _GuidebookNumberUOM, Graph ); rdf( GuidebookNumberNode, plain:uom, _GuidebookNumberUOM, Graph ) ),
          GuidebookNumber=val(GuidebookNumberValue)
        );
        (
          rdf( GuidebookNumberNode,rdf:value,GuidebookNumberValue,Graph ),
          ( rdf( GuidebookNumberNode, aixm:uom, GuidebookNumberUOM, Graph ); rdf( GuidebookNumberNode, fixm:uom, GuidebookNumberUOM, Graph ); rdf( GuidebookNumberNode, plain:uom, GuidebookNumberUOM, Graph ) ),
          GuidebookNumber=xval(GuidebookNumberValue,GuidebookNumberUOM)
        );
        (
          rdf( GuidebookNumberNode,aixm:nilReason, GuidebookNumberNilReason, Graph ),
          GuidebookNumber=nil(GuidebookNumberNilReason)
        );
        (
          rdf( GuidebookNumberNode,gml:indeterminatePosition, GuidebookNumberIndeterminate, Graph ),
          GuidebookNumber=indeterminate(GuidebookNumberIndeterminate)
        )
      )
  )
  ,(
    ( OnboardLocation='$null$',
      \+ rdf( DangerousGoods,fixm:'onboardLocation',_OnboardLocation,Graph )
    );
  ( rdf( DangerousGoods,fixm:'onboardLocation',OnboardLocationNode,Graph )),
      (
        (
          rdf(OnboardLocationNode,rdf:value,OnboardLocationValue,Graph),
         \+ ( rdf( OnboardLocationNode, aixm:uom, _OnboardLocationUOM, Graph ); rdf( OnboardLocationNode, fixm:uom, _OnboardLocationUOM, Graph ); rdf( OnboardLocationNode, plain:uom, _OnboardLocationUOM, Graph ) ),
          OnboardLocation=val(OnboardLocationValue)
        );
        (
          rdf( OnboardLocationNode,rdf:value,OnboardLocationValue,Graph ),
          ( rdf( OnboardLocationNode, aixm:uom, OnboardLocationUOM, Graph ); rdf( OnboardLocationNode, fixm:uom, OnboardLocationUOM, Graph ); rdf( OnboardLocationNode, plain:uom, OnboardLocationUOM, Graph ) ),
          OnboardLocation=xval(OnboardLocationValue,OnboardLocationUOM)
        );
        (
          rdf( OnboardLocationNode,aixm:nilReason, OnboardLocationNilReason, Graph ),
          OnboardLocation=nil(OnboardLocationNilReason)
        );
        (
          rdf( OnboardLocationNode,gml:indeterminatePosition, OnboardLocationIndeterminate, Graph ),
          OnboardLocation=indeterminate(OnboardLocationIndeterminate)
        )
      )
  )
  ,( ( HandlingInformation='$null$',
    \+ rdf( DangerousGoods,fixm:'handlingInformation', _HandlingInformation, Graph  )
   ; rdf(DangerousGoods,fixm:'handlingInformation', HandlingInformation, Graph ) )
  )
  ,(
    ( AircraftLimitation='$null$',
      \+ rdf( DangerousGoods,fixm:'aircraftLimitation',_AircraftLimitation,Graph )
    );
  ( rdf( DangerousGoods,fixm:'aircraftLimitation',AircraftLimitationNode,Graph )),
      (
        (
          rdf(AircraftLimitationNode,rdf:value,AircraftLimitationValue,Graph),
         \+ ( rdf( AircraftLimitationNode, aixm:uom, _AircraftLimitationUOM, Graph ); rdf( AircraftLimitationNode, fixm:uom, _AircraftLimitationUOM, Graph ); rdf( AircraftLimitationNode, plain:uom, _AircraftLimitationUOM, Graph ) ),
          AircraftLimitation=val(AircraftLimitationValue)
        );
        (
          rdf( AircraftLimitationNode,rdf:value,AircraftLimitationValue,Graph ),
          ( rdf( AircraftLimitationNode, aixm:uom, AircraftLimitationUOM, Graph ); rdf( AircraftLimitationNode, fixm:uom, AircraftLimitationUOM, Graph ); rdf( AircraftLimitationNode, plain:uom, AircraftLimitationUOM, Graph ) ),
          AircraftLimitation=xval(AircraftLimitationValue,AircraftLimitationUOM)
        );
        (
          rdf( AircraftLimitationNode,aixm:nilReason, AircraftLimitationNilReason, Graph ),
          AircraftLimitation=nil(AircraftLimitationNilReason)
        );
        (
          rdf( AircraftLimitationNode,gml:indeterminatePosition, AircraftLimitationIndeterminate, Graph ),
          AircraftLimitation=indeterminate(AircraftLimitationIndeterminate)
        )
      )
  )
  ,(
    ( AirWayBill='$null$',
      \+ rdf( DangerousGoods,fixm:'airWayBill',_AirWayBill,Graph )
    );
  ( rdf( DangerousGoods,fixm:'airWayBill',AirWayBillNode,Graph )),
      (
        (
          rdf(AirWayBillNode,rdf:value,AirWayBillValue,Graph),
         \+ ( rdf( AirWayBillNode, aixm:uom, _AirWayBillUOM, Graph ); rdf( AirWayBillNode, fixm:uom, _AirWayBillUOM, Graph ); rdf( AirWayBillNode, plain:uom, _AirWayBillUOM, Graph ) ),
          AirWayBill=val(AirWayBillValue)
        );
        (
          rdf( AirWayBillNode,rdf:value,AirWayBillValue,Graph ),
          ( rdf( AirWayBillNode, aixm:uom, AirWayBillUOM, Graph ); rdf( AirWayBillNode, fixm:uom, AirWayBillUOM, Graph ); rdf( AirWayBillNode, plain:uom, AirWayBillUOM, Graph ) ),
          AirWayBill=xval(AirWayBillValue,AirWayBillUOM)
        );
        (
          rdf( AirWayBillNode,aixm:nilReason, AirWayBillNilReason, Graph ),
          AirWayBill=nil(AirWayBillNilReason)
        );
        (
          rdf( AirWayBillNode,gml:indeterminatePosition, AirWayBillIndeterminate, Graph ),
          AirWayBill=indeterminate(AirWayBillIndeterminate)
        )
      )
  )
  ,(
    ( Shipment='$null$',
      \+ rdf( DangerousGoods,fixm:'shipment',_Shipment,Graph )
    );
  ( rdf( DangerousGoods,fixm:'shipment',ShipmentNode,Graph )),
      (
        (
          rdf(ShipmentNode,rdf:value,ShipmentValue,Graph),
         \+ ( rdf( ShipmentNode, aixm:uom, _ShipmentUOM, Graph ); rdf( ShipmentNode, fixm:uom, _ShipmentUOM, Graph ); rdf( ShipmentNode, plain:uom, _ShipmentUOM, Graph ) ),
          Shipment=val(ShipmentValue)
        );
        (
          rdf( ShipmentNode,rdf:value,ShipmentValue,Graph ),
          ( rdf( ShipmentNode, aixm:uom, ShipmentUOM, Graph ); rdf( ShipmentNode, fixm:uom, ShipmentUOM, Graph ); rdf( ShipmentNode, plain:uom, ShipmentUOM, Graph ) ),
          Shipment=xval(ShipmentValue,ShipmentUOM)
        );
        (
          rdf( ShipmentNode,aixm:nilReason, ShipmentNilReason, Graph ),
          Shipment=nil(ShipmentNilReason)
        );
        (
          rdf( ShipmentNode,gml:indeterminatePosition, ShipmentIndeterminate, Graph ),
          Shipment=indeterminate(ShipmentIndeterminate)
        )
      )
  )
  ,findall(A, rdf(DangerousGoods,fixm:'packageGroup',A,Graph), PackageGroup)
  ,( ( ShippingInformation='$null$',
    \+ rdf( DangerousGoods,fixm:'shippingInformation', _ShippingInformation, Graph  )
   ; rdf(DangerousGoods,fixm:'shippingInformation', ShippingInformation, Graph ) )
  ) .

fixm_DangerousGoodsPackageGroup(Graph, DangerousGoodsPackageGroup, ShipmentDimensions, DangerousGoodsPackage, ShipmentUseIndicator) :-
  rdf(DangerousGoodsPackageGroup,rdf:type,fixm:'DangerousGoodsPackageGroup',Graph)
  ,( ( ShipmentDimensions='$null$',
    \+ rdf( DangerousGoodsPackageGroup,fixm:'shipmentDimensions', _ShipmentDimensions, Graph  )
   ; rdf(DangerousGoodsPackageGroup,fixm:'shipmentDimensions', ShipmentDimensions, Graph ) )
  )
  ,findall(A, rdf(DangerousGoodsPackageGroup,fixm:'dangerousGoodsPackage',A,Graph), DangerousGoodsPackage)
  ,(
    ( ShipmentUseIndicator='$null$',
      \+ rdf( DangerousGoodsPackageGroup,fixm:'shipmentUseIndicator',_ShipmentUseIndicator,Graph )
    );
  ( rdf( DangerousGoodsPackageGroup,fixm:'shipmentUseIndicator',ShipmentUseIndicatorNode,Graph )),
      (
        (
          rdf(ShipmentUseIndicatorNode,rdf:value,ShipmentUseIndicatorValue,Graph),
         \+ ( rdf( ShipmentUseIndicatorNode, aixm:uom, _ShipmentUseIndicatorUOM, Graph ); rdf( ShipmentUseIndicatorNode, fixm:uom, _ShipmentUseIndicatorUOM, Graph ); rdf( ShipmentUseIndicatorNode, plain:uom, _ShipmentUseIndicatorUOM, Graph ) ),
          ShipmentUseIndicator=val(ShipmentUseIndicatorValue)
        );
        (
          rdf( ShipmentUseIndicatorNode,rdf:value,ShipmentUseIndicatorValue,Graph ),
          ( rdf( ShipmentUseIndicatorNode, aixm:uom, ShipmentUseIndicatorUOM, Graph ); rdf( ShipmentUseIndicatorNode, fixm:uom, ShipmentUseIndicatorUOM, Graph ); rdf( ShipmentUseIndicatorNode, plain:uom, ShipmentUseIndicatorUOM, Graph ) ),
          ShipmentUseIndicator=xval(ShipmentUseIndicatorValue,ShipmentUseIndicatorUOM)
        );
        (
          rdf( ShipmentUseIndicatorNode,aixm:nilReason, ShipmentUseIndicatorNilReason, Graph ),
          ShipmentUseIndicator=nil(ShipmentUseIndicatorNilReason)
        );
        (
          rdf( ShipmentUseIndicatorNode,gml:indeterminatePosition, ShipmentUseIndicatorIndeterminate, Graph ),
          ShipmentUseIndicator=indeterminate(ShipmentUseIndicatorIndeterminate)
        )
      )
  ) .

fixm_OfftrackDistance(Graph, OfftrackDistance, Distance, Direction) :-
  rdf(OfftrackDistance,rdf:type,fixm:'OfftrackDistance',Graph)
  ,(
    ( Distance='$null$',
      \+ rdf( OfftrackDistance,fixm:'distance',_Distance,Graph )
    );
  ( rdf( OfftrackDistance,fixm:'distance',DistanceNode,Graph )),
      (
        (
          rdf(DistanceNode,rdf:value,DistanceValue,Graph),
         \+ ( rdf( DistanceNode, aixm:uom, _DistanceUOM, Graph ); rdf( DistanceNode, fixm:uom, _DistanceUOM, Graph ); rdf( DistanceNode, plain:uom, _DistanceUOM, Graph ) ),
          Distance=val(DistanceValue)
        );
        (
          rdf( DistanceNode,rdf:value,DistanceValue,Graph ),
          ( rdf( DistanceNode, aixm:uom, DistanceUOM, Graph ); rdf( DistanceNode, fixm:uom, DistanceUOM, Graph ); rdf( DistanceNode, plain:uom, DistanceUOM, Graph ) ),
          Distance=xval(DistanceValue,DistanceUOM)
        );
        (
          rdf( DistanceNode,aixm:nilReason, DistanceNilReason, Graph ),
          Distance=nil(DistanceNilReason)
        );
        (
          rdf( DistanceNode,gml:indeterminatePosition, DistanceIndeterminate, Graph ),
          Distance=indeterminate(DistanceIndeterminate)
        )
      )
  )
  ,(
    ( Direction='$null$',
      \+ rdf( OfftrackDistance,fixm:'direction',_Direction,Graph )
    );
  ( rdf( OfftrackDistance,fixm:'direction',DirectionNode,Graph )),
      (
        (
          rdf(DirectionNode,rdf:value,DirectionValue,Graph),
         \+ ( rdf( DirectionNode, aixm:uom, _DirectionUOM, Graph ); rdf( DirectionNode, fixm:uom, _DirectionUOM, Graph ); rdf( DirectionNode, plain:uom, _DirectionUOM, Graph ) ),
          Direction=val(DirectionValue)
        );
        (
          rdf( DirectionNode,rdf:value,DirectionValue,Graph ),
          ( rdf( DirectionNode, aixm:uom, DirectionUOM, Graph ); rdf( DirectionNode, fixm:uom, DirectionUOM, Graph ); rdf( DirectionNode, plain:uom, DirectionUOM, Graph ) ),
          Direction=xval(DirectionValue,DirectionUOM)
        );
        (
          rdf( DirectionNode,aixm:nilReason, DirectionNilReason, Graph ),
          Direction=nil(DirectionNilReason)
        );
        (
          rdf( DirectionNode,gml:indeterminatePosition, DirectionIndeterminate, Graph ),
          Direction=indeterminate(DirectionIndeterminate)
        )
      )
  ) .

fixm_Handoff(Graph, Handoff, ReceivingUnit, TransferringUnit, CoordinationStatus) :-
  rdf(Handoff,rdf:type,fixm:'Handoff',Graph)
  ,( ( ReceivingUnit='$null$',
    \+ rdf( Handoff,fixm:'receivingUnit', _ReceivingUnit, Graph  )
   ; rdf(Handoff,fixm:'receivingUnit', ReceivingUnit, Graph ) )
  )
  ,( ( TransferringUnit='$null$',
    \+ rdf( Handoff,fixm:'transferringUnit', _TransferringUnit, Graph  )
   ; rdf(Handoff,fixm:'transferringUnit', TransferringUnit, Graph ) )
  )
  ,( ( CoordinationStatus='$null$',
    \+ rdf( Handoff,fixm:'coordinationStatus', _CoordinationStatus, Graph  )
   ; rdf(Handoff,fixm:'coordinationStatus', CoordinationStatus, Graph ) )
  ) .

fixm_TrajectoryChange(Graph, TrajectoryChange, ConstrainedAirspace, SpecialActivityAirspace) :-
  rdf(TrajectoryChange,rdf:type,fixm:'TrajectoryChange',Graph)
  ,(
    ( ConstrainedAirspace='$null$',
      \+ rdf( TrajectoryChange,fixm:'constrainedAirspace',_ConstrainedAirspace,Graph )
    );
  ( rdf( TrajectoryChange,fixm:'constrainedAirspace',ConstrainedAirspaceNode,Graph )),
      (
        (
          rdf(ConstrainedAirspaceNode,rdf:value,ConstrainedAirspaceValue,Graph),
         \+ ( rdf( ConstrainedAirspaceNode, aixm:uom, _ConstrainedAirspaceUOM, Graph ); rdf( ConstrainedAirspaceNode, fixm:uom, _ConstrainedAirspaceUOM, Graph ); rdf( ConstrainedAirspaceNode, plain:uom, _ConstrainedAirspaceUOM, Graph ) ),
          ConstrainedAirspace=val(ConstrainedAirspaceValue)
        );
        (
          rdf( ConstrainedAirspaceNode,rdf:value,ConstrainedAirspaceValue,Graph ),
          ( rdf( ConstrainedAirspaceNode, aixm:uom, ConstrainedAirspaceUOM, Graph ); rdf( ConstrainedAirspaceNode, fixm:uom, ConstrainedAirspaceUOM, Graph ); rdf( ConstrainedAirspaceNode, plain:uom, ConstrainedAirspaceUOM, Graph ) ),
          ConstrainedAirspace=xval(ConstrainedAirspaceValue,ConstrainedAirspaceUOM)
        );
        (
          rdf( ConstrainedAirspaceNode,aixm:nilReason, ConstrainedAirspaceNilReason, Graph ),
          ConstrainedAirspace=nil(ConstrainedAirspaceNilReason)
        );
        (
          rdf( ConstrainedAirspaceNode,gml:indeterminatePosition, ConstrainedAirspaceIndeterminate, Graph ),
          ConstrainedAirspace=indeterminate(ConstrainedAirspaceIndeterminate)
        )
      )
  )
  ,(
    ( SpecialActivityAirspace='$null$',
      \+ rdf( TrajectoryChange,fixm:'specialActivityAirspace',_SpecialActivityAirspace,Graph )
    );
  ( rdf( TrajectoryChange,fixm:'specialActivityAirspace',SpecialActivityAirspaceNode,Graph )),
      (
        (
          rdf(SpecialActivityAirspaceNode,rdf:value,SpecialActivityAirspaceValue,Graph),
         \+ ( rdf( SpecialActivityAirspaceNode, aixm:uom, _SpecialActivityAirspaceUOM, Graph ); rdf( SpecialActivityAirspaceNode, fixm:uom, _SpecialActivityAirspaceUOM, Graph ); rdf( SpecialActivityAirspaceNode, plain:uom, _SpecialActivityAirspaceUOM, Graph ) ),
          SpecialActivityAirspace=val(SpecialActivityAirspaceValue)
        );
        (
          rdf( SpecialActivityAirspaceNode,rdf:value,SpecialActivityAirspaceValue,Graph ),
          ( rdf( SpecialActivityAirspaceNode, aixm:uom, SpecialActivityAirspaceUOM, Graph ); rdf( SpecialActivityAirspaceNode, fixm:uom, SpecialActivityAirspaceUOM, Graph ); rdf( SpecialActivityAirspaceNode, plain:uom, SpecialActivityAirspaceUOM, Graph ) ),
          SpecialActivityAirspace=xval(SpecialActivityAirspaceValue,SpecialActivityAirspaceUOM)
        );
        (
          rdf( SpecialActivityAirspaceNode,aixm:nilReason, SpecialActivityAirspaceNilReason, Graph ),
          SpecialActivityAirspace=nil(SpecialActivityAirspaceNilReason)
        );
        (
          rdf( SpecialActivityAirspaceNode,gml:indeterminatePosition, SpecialActivityAirspaceIndeterminate, Graph ),
          SpecialActivityAirspace=indeterminate(SpecialActivityAirspaceIndeterminate)
        )
      )
  ) .

fixm_ContactInformation(Graph, ContactInformation, Name, Title, OnlineContact, PhoneFax, Address) :-
  subClassOf(T,fixm:'ContactInformation')
  ,rdf(ContactInformation,rdf:type,T,Graph)
  ,(
    ( Name='$null$',
      \+ rdf( ContactInformation,fixm:'name',_Name,Graph )
    );
  ( rdf( ContactInformation,fixm:'name',NameNode,Graph )),
      (
        (
          rdf(NameNode,rdf:value,NameValue,Graph),
         \+ ( rdf( NameNode, aixm:uom, _NameUOM, Graph ); rdf( NameNode, fixm:uom, _NameUOM, Graph ); rdf( NameNode, plain:uom, _NameUOM, Graph ) ),
          Name=val(NameValue)
        );
        (
          rdf( NameNode,rdf:value,NameValue,Graph ),
          ( rdf( NameNode, aixm:uom, NameUOM, Graph ); rdf( NameNode, fixm:uom, NameUOM, Graph ); rdf( NameNode, plain:uom, NameUOM, Graph ) ),
          Name=xval(NameValue,NameUOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NameNilReason, Graph ),
          Name=nil(NameNilReason)
        );
        (
          rdf( NameNode,gml:indeterminatePosition, NameIndeterminate, Graph ),
          Name=indeterminate(NameIndeterminate)
        )
      )
  )
  ,(
    ( Title='$null$',
      \+ rdf( ContactInformation,fixm:'title',_Title,Graph )
    );
  ( rdf( ContactInformation,fixm:'title',TitleNode,Graph )),
      (
        (
          rdf(TitleNode,rdf:value,TitleValue,Graph),
         \+ ( rdf( TitleNode, aixm:uom, _TitleUOM, Graph ); rdf( TitleNode, fixm:uom, _TitleUOM, Graph ); rdf( TitleNode, plain:uom, _TitleUOM, Graph ) ),
          Title=val(TitleValue)
        );
        (
          rdf( TitleNode,rdf:value,TitleValue,Graph ),
          ( rdf( TitleNode, aixm:uom, TitleUOM, Graph ); rdf( TitleNode, fixm:uom, TitleUOM, Graph ); rdf( TitleNode, plain:uom, TitleUOM, Graph ) ),
          Title=xval(TitleValue,TitleUOM)
        );
        (
          rdf( TitleNode,aixm:nilReason, TitleNilReason, Graph ),
          Title=nil(TitleNilReason)
        );
        (
          rdf( TitleNode,gml:indeterminatePosition, TitleIndeterminate, Graph ),
          Title=indeterminate(TitleIndeterminate)
        )
      )
  )
  ,( ( OnlineContact='$null$',
    \+ rdf( ContactInformation,fixm:'onlineContact', _OnlineContact, Graph  )
   ; rdf(ContactInformation,fixm:'onlineContact', OnlineContact, Graph ) )
  )
  ,( ( PhoneFax='$null$',
    \+ rdf( ContactInformation,fixm:'phoneFax', _PhoneFax, Graph  )
   ; rdf(ContactInformation,fixm:'phoneFax', PhoneFax, Graph ) )
  )
  ,( ( Address='$null$',
    \+ rdf( ContactInformation,fixm:'address', _Address, Graph  )
   ; rdf(ContactInformation,fixm:'address', Address, Graph ) )
  ) .

aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability) :-
  rdf(AirportHeliportTimeSlice,rdf:type,aixm:'AirportHeliportTimeSlice',Graph)
  ,(
    ( Designator='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'designator',_Designator,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'designator',DesignatorNode,Graph )),
      (
        (
          rdf(DesignatorNode,rdf:value,DesignatorValue,Graph),
         \+ ( rdf( DesignatorNode, aixm:uom, _DesignatorUOM, Graph ); rdf( DesignatorNode, fixm:uom, _DesignatorUOM, Graph ); rdf( DesignatorNode, plain:uom, _DesignatorUOM, Graph ) ),
          Designator=val(DesignatorValue)
        );
        (
          rdf( DesignatorNode,rdf:value,DesignatorValue,Graph ),
          ( rdf( DesignatorNode, aixm:uom, DesignatorUOM, Graph ); rdf( DesignatorNode, fixm:uom, DesignatorUOM, Graph ); rdf( DesignatorNode, plain:uom, DesignatorUOM, Graph ) ),
          Designator=xval(DesignatorValue,DesignatorUOM)
        );
        (
          rdf( DesignatorNode,aixm:nilReason, DesignatorNilReason, Graph ),
          Designator=nil(DesignatorNilReason)
        );
        (
          rdf( DesignatorNode,gml:indeterminatePosition, DesignatorIndeterminate, Graph ),
          Designator=indeterminate(DesignatorIndeterminate)
        )
      )
  )
  ,(
    ( Name='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'name',_Name,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'name',NameNode,Graph )),
      (
        (
          rdf(NameNode,rdf:value,NameValue,Graph),
         \+ ( rdf( NameNode, aixm:uom, _NameUOM, Graph ); rdf( NameNode, fixm:uom, _NameUOM, Graph ); rdf( NameNode, plain:uom, _NameUOM, Graph ) ),
          Name=val(NameValue)
        );
        (
          rdf( NameNode,rdf:value,NameValue,Graph ),
          ( rdf( NameNode, aixm:uom, NameUOM, Graph ); rdf( NameNode, fixm:uom, NameUOM, Graph ); rdf( NameNode, plain:uom, NameUOM, Graph ) ),
          Name=xval(NameValue,NameUOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NameNilReason, Graph ),
          Name=nil(NameNilReason)
        );
        (
          rdf( NameNode,gml:indeterminatePosition, NameIndeterminate, Graph ),
          Name=indeterminate(NameIndeterminate)
        )
      )
  )
  ,(
    ( LocationIndicatorICAO='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'locationIndicatorICAO',_LocationIndicatorICAO,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'locationIndicatorICAO',LocationIndicatorICAONode,Graph )),
      (
        (
          rdf(LocationIndicatorICAONode,rdf:value,LocationIndicatorICAOValue,Graph),
         \+ ( rdf( LocationIndicatorICAONode, aixm:uom, _LocationIndicatorICAOUOM, Graph ); rdf( LocationIndicatorICAONode, fixm:uom, _LocationIndicatorICAOUOM, Graph ); rdf( LocationIndicatorICAONode, plain:uom, _LocationIndicatorICAOUOM, Graph ) ),
          LocationIndicatorICAO=val(LocationIndicatorICAOValue)
        );
        (
          rdf( LocationIndicatorICAONode,rdf:value,LocationIndicatorICAOValue,Graph ),
          ( rdf( LocationIndicatorICAONode, aixm:uom, LocationIndicatorICAOUOM, Graph ); rdf( LocationIndicatorICAONode, fixm:uom, LocationIndicatorICAOUOM, Graph ); rdf( LocationIndicatorICAONode, plain:uom, LocationIndicatorICAOUOM, Graph ) ),
          LocationIndicatorICAO=xval(LocationIndicatorICAOValue,LocationIndicatorICAOUOM)
        );
        (
          rdf( LocationIndicatorICAONode,aixm:nilReason, LocationIndicatorICAONilReason, Graph ),
          LocationIndicatorICAO=nil(LocationIndicatorICAONilReason)
        );
        (
          rdf( LocationIndicatorICAONode,gml:indeterminatePosition, LocationIndicatorICAOIndeterminate, Graph ),
          LocationIndicatorICAO=indeterminate(LocationIndicatorICAOIndeterminate)
        )
      )
  )
  ,(
    ( DesignatorIATA='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'designatorIATA',_DesignatorIATA,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'designatorIATA',DesignatorIATANode,Graph )),
      (
        (
          rdf(DesignatorIATANode,rdf:value,DesignatorIATAValue,Graph),
         \+ ( rdf( DesignatorIATANode, aixm:uom, _DesignatorIATAUOM, Graph ); rdf( DesignatorIATANode, fixm:uom, _DesignatorIATAUOM, Graph ); rdf( DesignatorIATANode, plain:uom, _DesignatorIATAUOM, Graph ) ),
          DesignatorIATA=val(DesignatorIATAValue)
        );
        (
          rdf( DesignatorIATANode,rdf:value,DesignatorIATAValue,Graph ),
          ( rdf( DesignatorIATANode, aixm:uom, DesignatorIATAUOM, Graph ); rdf( DesignatorIATANode, fixm:uom, DesignatorIATAUOM, Graph ); rdf( DesignatorIATANode, plain:uom, DesignatorIATAUOM, Graph ) ),
          DesignatorIATA=xval(DesignatorIATAValue,DesignatorIATAUOM)
        );
        (
          rdf( DesignatorIATANode,aixm:nilReason, DesignatorIATANilReason, Graph ),
          DesignatorIATA=nil(DesignatorIATANilReason)
        );
        (
          rdf( DesignatorIATANode,gml:indeterminatePosition, DesignatorIATAIndeterminate, Graph ),
          DesignatorIATA=indeterminate(DesignatorIATAIndeterminate)
        )
      )
  )
  ,(
    ( Type='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'type',_Type,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'type',TypeNode,Graph )),
      (
        (
          rdf(TypeNode,rdf:value,TypeValue,Graph),
         \+ ( rdf( TypeNode, aixm:uom, _TypeUOM, Graph ); rdf( TypeNode, fixm:uom, _TypeUOM, Graph ); rdf( TypeNode, plain:uom, _TypeUOM, Graph ) ),
          Type=val(TypeValue)
        );
        (
          rdf( TypeNode,rdf:value,TypeValue,Graph ),
          ( rdf( TypeNode, aixm:uom, TypeUOM, Graph ); rdf( TypeNode, fixm:uom, TypeUOM, Graph ); rdf( TypeNode, plain:uom, TypeUOM, Graph ) ),
          Type=xval(TypeValue,TypeUOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, TypeNilReason, Graph ),
          Type=nil(TypeNilReason)
        );
        (
          rdf( TypeNode,gml:indeterminatePosition, TypeIndeterminate, Graph ),
          Type=indeterminate(TypeIndeterminate)
        )
      )
  )
  ,(
    ( CertifiedICAO='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'certifiedICAO',_CertifiedICAO,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'certifiedICAO',CertifiedICAONode,Graph )),
      (
        (
          rdf(CertifiedICAONode,rdf:value,CertifiedICAOValue,Graph),
         \+ ( rdf( CertifiedICAONode, aixm:uom, _CertifiedICAOUOM, Graph ); rdf( CertifiedICAONode, fixm:uom, _CertifiedICAOUOM, Graph ); rdf( CertifiedICAONode, plain:uom, _CertifiedICAOUOM, Graph ) ),
          CertifiedICAO=val(CertifiedICAOValue)
        );
        (
          rdf( CertifiedICAONode,rdf:value,CertifiedICAOValue,Graph ),
          ( rdf( CertifiedICAONode, aixm:uom, CertifiedICAOUOM, Graph ); rdf( CertifiedICAONode, fixm:uom, CertifiedICAOUOM, Graph ); rdf( CertifiedICAONode, plain:uom, CertifiedICAOUOM, Graph ) ),
          CertifiedICAO=xval(CertifiedICAOValue,CertifiedICAOUOM)
        );
        (
          rdf( CertifiedICAONode,aixm:nilReason, CertifiedICAONilReason, Graph ),
          CertifiedICAO=nil(CertifiedICAONilReason)
        );
        (
          rdf( CertifiedICAONode,gml:indeterminatePosition, CertifiedICAOIndeterminate, Graph ),
          CertifiedICAO=indeterminate(CertifiedICAOIndeterminate)
        )
      )
  )
  ,(
    ( PrivateUse='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'privateUse',_PrivateUse,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'privateUse',PrivateUseNode,Graph )),
      (
        (
          rdf(PrivateUseNode,rdf:value,PrivateUseValue,Graph),
         \+ ( rdf( PrivateUseNode, aixm:uom, _PrivateUseUOM, Graph ); rdf( PrivateUseNode, fixm:uom, _PrivateUseUOM, Graph ); rdf( PrivateUseNode, plain:uom, _PrivateUseUOM, Graph ) ),
          PrivateUse=val(PrivateUseValue)
        );
        (
          rdf( PrivateUseNode,rdf:value,PrivateUseValue,Graph ),
          ( rdf( PrivateUseNode, aixm:uom, PrivateUseUOM, Graph ); rdf( PrivateUseNode, fixm:uom, PrivateUseUOM, Graph ); rdf( PrivateUseNode, plain:uom, PrivateUseUOM, Graph ) ),
          PrivateUse=xval(PrivateUseValue,PrivateUseUOM)
        );
        (
          rdf( PrivateUseNode,aixm:nilReason, PrivateUseNilReason, Graph ),
          PrivateUse=nil(PrivateUseNilReason)
        );
        (
          rdf( PrivateUseNode,gml:indeterminatePosition, PrivateUseIndeterminate, Graph ),
          PrivateUse=indeterminate(PrivateUseIndeterminate)
        )
      )
  )
  ,(
    ( ControlType='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'controlType',_ControlType,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'controlType',ControlTypeNode,Graph )),
      (
        (
          rdf(ControlTypeNode,rdf:value,ControlTypeValue,Graph),
         \+ ( rdf( ControlTypeNode, aixm:uom, _ControlTypeUOM, Graph ); rdf( ControlTypeNode, fixm:uom, _ControlTypeUOM, Graph ); rdf( ControlTypeNode, plain:uom, _ControlTypeUOM, Graph ) ),
          ControlType=val(ControlTypeValue)
        );
        (
          rdf( ControlTypeNode,rdf:value,ControlTypeValue,Graph ),
          ( rdf( ControlTypeNode, aixm:uom, ControlTypeUOM, Graph ); rdf( ControlTypeNode, fixm:uom, ControlTypeUOM, Graph ); rdf( ControlTypeNode, plain:uom, ControlTypeUOM, Graph ) ),
          ControlType=xval(ControlTypeValue,ControlTypeUOM)
        );
        (
          rdf( ControlTypeNode,aixm:nilReason, ControlTypeNilReason, Graph ),
          ControlType=nil(ControlTypeNilReason)
        );
        (
          rdf( ControlTypeNode,gml:indeterminatePosition, ControlTypeIndeterminate, Graph ),
          ControlType=indeterminate(ControlTypeIndeterminate)
        )
      )
  )
  ,(
    ( FieldElevation='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'fieldElevation',_FieldElevation,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'fieldElevation',FieldElevationNode,Graph )),
      (
        (
          rdf(FieldElevationNode,rdf:value,FieldElevationValue,Graph),
         \+ ( rdf( FieldElevationNode, aixm:uom, _FieldElevationUOM, Graph ); rdf( FieldElevationNode, fixm:uom, _FieldElevationUOM, Graph ); rdf( FieldElevationNode, plain:uom, _FieldElevationUOM, Graph ) ),
          FieldElevation=val(FieldElevationValue)
        );
        (
          rdf( FieldElevationNode,rdf:value,FieldElevationValue,Graph ),
          ( rdf( FieldElevationNode, aixm:uom, FieldElevationUOM, Graph ); rdf( FieldElevationNode, fixm:uom, FieldElevationUOM, Graph ); rdf( FieldElevationNode, plain:uom, FieldElevationUOM, Graph ) ),
          FieldElevation=xval(FieldElevationValue,FieldElevationUOM)
        );
        (
          rdf( FieldElevationNode,aixm:nilReason, FieldElevationNilReason, Graph ),
          FieldElevation=nil(FieldElevationNilReason)
        );
        (
          rdf( FieldElevationNode,gml:indeterminatePosition, FieldElevationIndeterminate, Graph ),
          FieldElevation=indeterminate(FieldElevationIndeterminate)
        )
      )
  )
  ,(
    ( FieldElevationAccuracy='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'fieldElevationAccuracy',_FieldElevationAccuracy,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'fieldElevationAccuracy',FieldElevationAccuracyNode,Graph )),
      (
        (
          rdf(FieldElevationAccuracyNode,rdf:value,FieldElevationAccuracyValue,Graph),
         \+ ( rdf( FieldElevationAccuracyNode, aixm:uom, _FieldElevationAccuracyUOM, Graph ); rdf( FieldElevationAccuracyNode, fixm:uom, _FieldElevationAccuracyUOM, Graph ); rdf( FieldElevationAccuracyNode, plain:uom, _FieldElevationAccuracyUOM, Graph ) ),
          FieldElevationAccuracy=val(FieldElevationAccuracyValue)
        );
        (
          rdf( FieldElevationAccuracyNode,rdf:value,FieldElevationAccuracyValue,Graph ),
          ( rdf( FieldElevationAccuracyNode, aixm:uom, FieldElevationAccuracyUOM, Graph ); rdf( FieldElevationAccuracyNode, fixm:uom, FieldElevationAccuracyUOM, Graph ); rdf( FieldElevationAccuracyNode, plain:uom, FieldElevationAccuracyUOM, Graph ) ),
          FieldElevationAccuracy=xval(FieldElevationAccuracyValue,FieldElevationAccuracyUOM)
        );
        (
          rdf( FieldElevationAccuracyNode,aixm:nilReason, FieldElevationAccuracyNilReason, Graph ),
          FieldElevationAccuracy=nil(FieldElevationAccuracyNilReason)
        );
        (
          rdf( FieldElevationAccuracyNode,gml:indeterminatePosition, FieldElevationAccuracyIndeterminate, Graph ),
          FieldElevationAccuracy=indeterminate(FieldElevationAccuracyIndeterminate)
        )
      )
  )
  ,(
    ( VerticalDatum='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'verticalDatum',_VerticalDatum,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'verticalDatum',VerticalDatumNode,Graph )),
      (
        (
          rdf(VerticalDatumNode,rdf:value,VerticalDatumValue,Graph),
         \+ ( rdf( VerticalDatumNode, aixm:uom, _VerticalDatumUOM, Graph ); rdf( VerticalDatumNode, fixm:uom, _VerticalDatumUOM, Graph ); rdf( VerticalDatumNode, plain:uom, _VerticalDatumUOM, Graph ) ),
          VerticalDatum=val(VerticalDatumValue)
        );
        (
          rdf( VerticalDatumNode,rdf:value,VerticalDatumValue,Graph ),
          ( rdf( VerticalDatumNode, aixm:uom, VerticalDatumUOM, Graph ); rdf( VerticalDatumNode, fixm:uom, VerticalDatumUOM, Graph ); rdf( VerticalDatumNode, plain:uom, VerticalDatumUOM, Graph ) ),
          VerticalDatum=xval(VerticalDatumValue,VerticalDatumUOM)
        );
        (
          rdf( VerticalDatumNode,aixm:nilReason, VerticalDatumNilReason, Graph ),
          VerticalDatum=nil(VerticalDatumNilReason)
        );
        (
          rdf( VerticalDatumNode,gml:indeterminatePosition, VerticalDatumIndeterminate, Graph ),
          VerticalDatum=indeterminate(VerticalDatumIndeterminate)
        )
      )
  )
  ,(
    ( MagneticVariation='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'magneticVariation',_MagneticVariation,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'magneticVariation',MagneticVariationNode,Graph )),
      (
        (
          rdf(MagneticVariationNode,rdf:value,MagneticVariationValue,Graph),
         \+ ( rdf( MagneticVariationNode, aixm:uom, _MagneticVariationUOM, Graph ); rdf( MagneticVariationNode, fixm:uom, _MagneticVariationUOM, Graph ); rdf( MagneticVariationNode, plain:uom, _MagneticVariationUOM, Graph ) ),
          MagneticVariation=val(MagneticVariationValue)
        );
        (
          rdf( MagneticVariationNode,rdf:value,MagneticVariationValue,Graph ),
          ( rdf( MagneticVariationNode, aixm:uom, MagneticVariationUOM, Graph ); rdf( MagneticVariationNode, fixm:uom, MagneticVariationUOM, Graph ); rdf( MagneticVariationNode, plain:uom, MagneticVariationUOM, Graph ) ),
          MagneticVariation=xval(MagneticVariationValue,MagneticVariationUOM)
        );
        (
          rdf( MagneticVariationNode,aixm:nilReason, MagneticVariationNilReason, Graph ),
          MagneticVariation=nil(MagneticVariationNilReason)
        );
        (
          rdf( MagneticVariationNode,gml:indeterminatePosition, MagneticVariationIndeterminate, Graph ),
          MagneticVariation=indeterminate(MagneticVariationIndeterminate)
        )
      )
  )
  ,(
    ( MagneticVariationAccuracy='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'magneticVariationAccuracy',_MagneticVariationAccuracy,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'magneticVariationAccuracy',MagneticVariationAccuracyNode,Graph )),
      (
        (
          rdf(MagneticVariationAccuracyNode,rdf:value,MagneticVariationAccuracyValue,Graph),
         \+ ( rdf( MagneticVariationAccuracyNode, aixm:uom, _MagneticVariationAccuracyUOM, Graph ); rdf( MagneticVariationAccuracyNode, fixm:uom, _MagneticVariationAccuracyUOM, Graph ); rdf( MagneticVariationAccuracyNode, plain:uom, _MagneticVariationAccuracyUOM, Graph ) ),
          MagneticVariationAccuracy=val(MagneticVariationAccuracyValue)
        );
        (
          rdf( MagneticVariationAccuracyNode,rdf:value,MagneticVariationAccuracyValue,Graph ),
          ( rdf( MagneticVariationAccuracyNode, aixm:uom, MagneticVariationAccuracyUOM, Graph ); rdf( MagneticVariationAccuracyNode, fixm:uom, MagneticVariationAccuracyUOM, Graph ); rdf( MagneticVariationAccuracyNode, plain:uom, MagneticVariationAccuracyUOM, Graph ) ),
          MagneticVariationAccuracy=xval(MagneticVariationAccuracyValue,MagneticVariationAccuracyUOM)
        );
        (
          rdf( MagneticVariationAccuracyNode,aixm:nilReason, MagneticVariationAccuracyNilReason, Graph ),
          MagneticVariationAccuracy=nil(MagneticVariationAccuracyNilReason)
        );
        (
          rdf( MagneticVariationAccuracyNode,gml:indeterminatePosition, MagneticVariationAccuracyIndeterminate, Graph ),
          MagneticVariationAccuracy=indeterminate(MagneticVariationAccuracyIndeterminate)
        )
      )
  )
  ,(
    ( DateMagneticVariation='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'dateMagneticVariation',_DateMagneticVariation,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'dateMagneticVariation',DateMagneticVariationNode,Graph )),
      (
        (
          rdf(DateMagneticVariationNode,rdf:value,DateMagneticVariationValue,Graph),
         \+ ( rdf( DateMagneticVariationNode, aixm:uom, _DateMagneticVariationUOM, Graph ); rdf( DateMagneticVariationNode, fixm:uom, _DateMagneticVariationUOM, Graph ); rdf( DateMagneticVariationNode, plain:uom, _DateMagneticVariationUOM, Graph ) ),
          DateMagneticVariation=val(DateMagneticVariationValue)
        );
        (
          rdf( DateMagneticVariationNode,rdf:value,DateMagneticVariationValue,Graph ),
          ( rdf( DateMagneticVariationNode, aixm:uom, DateMagneticVariationUOM, Graph ); rdf( DateMagneticVariationNode, fixm:uom, DateMagneticVariationUOM, Graph ); rdf( DateMagneticVariationNode, plain:uom, DateMagneticVariationUOM, Graph ) ),
          DateMagneticVariation=xval(DateMagneticVariationValue,DateMagneticVariationUOM)
        );
        (
          rdf( DateMagneticVariationNode,aixm:nilReason, DateMagneticVariationNilReason, Graph ),
          DateMagneticVariation=nil(DateMagneticVariationNilReason)
        );
        (
          rdf( DateMagneticVariationNode,gml:indeterminatePosition, DateMagneticVariationIndeterminate, Graph ),
          DateMagneticVariation=indeterminate(DateMagneticVariationIndeterminate)
        )
      )
  )
  ,(
    ( MagneticVariationChange='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'magneticVariationChange',_MagneticVariationChange,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'magneticVariationChange',MagneticVariationChangeNode,Graph )),
      (
        (
          rdf(MagneticVariationChangeNode,rdf:value,MagneticVariationChangeValue,Graph),
         \+ ( rdf( MagneticVariationChangeNode, aixm:uom, _MagneticVariationChangeUOM, Graph ); rdf( MagneticVariationChangeNode, fixm:uom, _MagneticVariationChangeUOM, Graph ); rdf( MagneticVariationChangeNode, plain:uom, _MagneticVariationChangeUOM, Graph ) ),
          MagneticVariationChange=val(MagneticVariationChangeValue)
        );
        (
          rdf( MagneticVariationChangeNode,rdf:value,MagneticVariationChangeValue,Graph ),
          ( rdf( MagneticVariationChangeNode, aixm:uom, MagneticVariationChangeUOM, Graph ); rdf( MagneticVariationChangeNode, fixm:uom, MagneticVariationChangeUOM, Graph ); rdf( MagneticVariationChangeNode, plain:uom, MagneticVariationChangeUOM, Graph ) ),
          MagneticVariationChange=xval(MagneticVariationChangeValue,MagneticVariationChangeUOM)
        );
        (
          rdf( MagneticVariationChangeNode,aixm:nilReason, MagneticVariationChangeNilReason, Graph ),
          MagneticVariationChange=nil(MagneticVariationChangeNilReason)
        );
        (
          rdf( MagneticVariationChangeNode,gml:indeterminatePosition, MagneticVariationChangeIndeterminate, Graph ),
          MagneticVariationChange=indeterminate(MagneticVariationChangeIndeterminate)
        )
      )
  )
  ,(
    ( ReferenceTemperature='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'referenceTemperature',_ReferenceTemperature,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'referenceTemperature',ReferenceTemperatureNode,Graph )),
      (
        (
          rdf(ReferenceTemperatureNode,rdf:value,ReferenceTemperatureValue,Graph),
         \+ ( rdf( ReferenceTemperatureNode, aixm:uom, _ReferenceTemperatureUOM, Graph ); rdf( ReferenceTemperatureNode, fixm:uom, _ReferenceTemperatureUOM, Graph ); rdf( ReferenceTemperatureNode, plain:uom, _ReferenceTemperatureUOM, Graph ) ),
          ReferenceTemperature=val(ReferenceTemperatureValue)
        );
        (
          rdf( ReferenceTemperatureNode,rdf:value,ReferenceTemperatureValue,Graph ),
          ( rdf( ReferenceTemperatureNode, aixm:uom, ReferenceTemperatureUOM, Graph ); rdf( ReferenceTemperatureNode, fixm:uom, ReferenceTemperatureUOM, Graph ); rdf( ReferenceTemperatureNode, plain:uom, ReferenceTemperatureUOM, Graph ) ),
          ReferenceTemperature=xval(ReferenceTemperatureValue,ReferenceTemperatureUOM)
        );
        (
          rdf( ReferenceTemperatureNode,aixm:nilReason, ReferenceTemperatureNilReason, Graph ),
          ReferenceTemperature=nil(ReferenceTemperatureNilReason)
        );
        (
          rdf( ReferenceTemperatureNode,gml:indeterminatePosition, ReferenceTemperatureIndeterminate, Graph ),
          ReferenceTemperature=indeterminate(ReferenceTemperatureIndeterminate)
        )
      )
  )
  ,(
    ( AltimeterCheckLocation='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'altimeterCheckLocation',_AltimeterCheckLocation,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'altimeterCheckLocation',AltimeterCheckLocationNode,Graph )),
      (
        (
          rdf(AltimeterCheckLocationNode,rdf:value,AltimeterCheckLocationValue,Graph),
         \+ ( rdf( AltimeterCheckLocationNode, aixm:uom, _AltimeterCheckLocationUOM, Graph ); rdf( AltimeterCheckLocationNode, fixm:uom, _AltimeterCheckLocationUOM, Graph ); rdf( AltimeterCheckLocationNode, plain:uom, _AltimeterCheckLocationUOM, Graph ) ),
          AltimeterCheckLocation=val(AltimeterCheckLocationValue)
        );
        (
          rdf( AltimeterCheckLocationNode,rdf:value,AltimeterCheckLocationValue,Graph ),
          ( rdf( AltimeterCheckLocationNode, aixm:uom, AltimeterCheckLocationUOM, Graph ); rdf( AltimeterCheckLocationNode, fixm:uom, AltimeterCheckLocationUOM, Graph ); rdf( AltimeterCheckLocationNode, plain:uom, AltimeterCheckLocationUOM, Graph ) ),
          AltimeterCheckLocation=xval(AltimeterCheckLocationValue,AltimeterCheckLocationUOM)
        );
        (
          rdf( AltimeterCheckLocationNode,aixm:nilReason, AltimeterCheckLocationNilReason, Graph ),
          AltimeterCheckLocation=nil(AltimeterCheckLocationNilReason)
        );
        (
          rdf( AltimeterCheckLocationNode,gml:indeterminatePosition, AltimeterCheckLocationIndeterminate, Graph ),
          AltimeterCheckLocation=indeterminate(AltimeterCheckLocationIndeterminate)
        )
      )
  )
  ,(
    ( SecondaryPowerSupply='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'secondaryPowerSupply',_SecondaryPowerSupply,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'secondaryPowerSupply',SecondaryPowerSupplyNode,Graph )),
      (
        (
          rdf(SecondaryPowerSupplyNode,rdf:value,SecondaryPowerSupplyValue,Graph),
         \+ ( rdf( SecondaryPowerSupplyNode, aixm:uom, _SecondaryPowerSupplyUOM, Graph ); rdf( SecondaryPowerSupplyNode, fixm:uom, _SecondaryPowerSupplyUOM, Graph ); rdf( SecondaryPowerSupplyNode, plain:uom, _SecondaryPowerSupplyUOM, Graph ) ),
          SecondaryPowerSupply=val(SecondaryPowerSupplyValue)
        );
        (
          rdf( SecondaryPowerSupplyNode,rdf:value,SecondaryPowerSupplyValue,Graph ),
          ( rdf( SecondaryPowerSupplyNode, aixm:uom, SecondaryPowerSupplyUOM, Graph ); rdf( SecondaryPowerSupplyNode, fixm:uom, SecondaryPowerSupplyUOM, Graph ); rdf( SecondaryPowerSupplyNode, plain:uom, SecondaryPowerSupplyUOM, Graph ) ),
          SecondaryPowerSupply=xval(SecondaryPowerSupplyValue,SecondaryPowerSupplyUOM)
        );
        (
          rdf( SecondaryPowerSupplyNode,aixm:nilReason, SecondaryPowerSupplyNilReason, Graph ),
          SecondaryPowerSupply=nil(SecondaryPowerSupplyNilReason)
        );
        (
          rdf( SecondaryPowerSupplyNode,gml:indeterminatePosition, SecondaryPowerSupplyIndeterminate, Graph ),
          SecondaryPowerSupply=indeterminate(SecondaryPowerSupplyIndeterminate)
        )
      )
  )
  ,(
    ( WindDirectionIndicator='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'windDirectionIndicator',_WindDirectionIndicator,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'windDirectionIndicator',WindDirectionIndicatorNode,Graph )),
      (
        (
          rdf(WindDirectionIndicatorNode,rdf:value,WindDirectionIndicatorValue,Graph),
         \+ ( rdf( WindDirectionIndicatorNode, aixm:uom, _WindDirectionIndicatorUOM, Graph ); rdf( WindDirectionIndicatorNode, fixm:uom, _WindDirectionIndicatorUOM, Graph ); rdf( WindDirectionIndicatorNode, plain:uom, _WindDirectionIndicatorUOM, Graph ) ),
          WindDirectionIndicator=val(WindDirectionIndicatorValue)
        );
        (
          rdf( WindDirectionIndicatorNode,rdf:value,WindDirectionIndicatorValue,Graph ),
          ( rdf( WindDirectionIndicatorNode, aixm:uom, WindDirectionIndicatorUOM, Graph ); rdf( WindDirectionIndicatorNode, fixm:uom, WindDirectionIndicatorUOM, Graph ); rdf( WindDirectionIndicatorNode, plain:uom, WindDirectionIndicatorUOM, Graph ) ),
          WindDirectionIndicator=xval(WindDirectionIndicatorValue,WindDirectionIndicatorUOM)
        );
        (
          rdf( WindDirectionIndicatorNode,aixm:nilReason, WindDirectionIndicatorNilReason, Graph ),
          WindDirectionIndicator=nil(WindDirectionIndicatorNilReason)
        );
        (
          rdf( WindDirectionIndicatorNode,gml:indeterminatePosition, WindDirectionIndicatorIndeterminate, Graph ),
          WindDirectionIndicator=indeterminate(WindDirectionIndicatorIndeterminate)
        )
      )
  )
  ,(
    ( LandingDirectionIndicator='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'landingDirectionIndicator',_LandingDirectionIndicator,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'landingDirectionIndicator',LandingDirectionIndicatorNode,Graph )),
      (
        (
          rdf(LandingDirectionIndicatorNode,rdf:value,LandingDirectionIndicatorValue,Graph),
         \+ ( rdf( LandingDirectionIndicatorNode, aixm:uom, _LandingDirectionIndicatorUOM, Graph ); rdf( LandingDirectionIndicatorNode, fixm:uom, _LandingDirectionIndicatorUOM, Graph ); rdf( LandingDirectionIndicatorNode, plain:uom, _LandingDirectionIndicatorUOM, Graph ) ),
          LandingDirectionIndicator=val(LandingDirectionIndicatorValue)
        );
        (
          rdf( LandingDirectionIndicatorNode,rdf:value,LandingDirectionIndicatorValue,Graph ),
          ( rdf( LandingDirectionIndicatorNode, aixm:uom, LandingDirectionIndicatorUOM, Graph ); rdf( LandingDirectionIndicatorNode, fixm:uom, LandingDirectionIndicatorUOM, Graph ); rdf( LandingDirectionIndicatorNode, plain:uom, LandingDirectionIndicatorUOM, Graph ) ),
          LandingDirectionIndicator=xval(LandingDirectionIndicatorValue,LandingDirectionIndicatorUOM)
        );
        (
          rdf( LandingDirectionIndicatorNode,aixm:nilReason, LandingDirectionIndicatorNilReason, Graph ),
          LandingDirectionIndicator=nil(LandingDirectionIndicatorNilReason)
        );
        (
          rdf( LandingDirectionIndicatorNode,gml:indeterminatePosition, LandingDirectionIndicatorIndeterminate, Graph ),
          LandingDirectionIndicator=indeterminate(LandingDirectionIndicatorIndeterminate)
        )
      )
  )
  ,(
    ( TransitionAltitude='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'transitionAltitude',_TransitionAltitude,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'transitionAltitude',TransitionAltitudeNode,Graph )),
      (
        (
          rdf(TransitionAltitudeNode,rdf:value,TransitionAltitudeValue,Graph),
         \+ ( rdf( TransitionAltitudeNode, aixm:uom, _TransitionAltitudeUOM, Graph ); rdf( TransitionAltitudeNode, fixm:uom, _TransitionAltitudeUOM, Graph ); rdf( TransitionAltitudeNode, plain:uom, _TransitionAltitudeUOM, Graph ) ),
          TransitionAltitude=val(TransitionAltitudeValue)
        );
        (
          rdf( TransitionAltitudeNode,rdf:value,TransitionAltitudeValue,Graph ),
          ( rdf( TransitionAltitudeNode, aixm:uom, TransitionAltitudeUOM, Graph ); rdf( TransitionAltitudeNode, fixm:uom, TransitionAltitudeUOM, Graph ); rdf( TransitionAltitudeNode, plain:uom, TransitionAltitudeUOM, Graph ) ),
          TransitionAltitude=xval(TransitionAltitudeValue,TransitionAltitudeUOM)
        );
        (
          rdf( TransitionAltitudeNode,aixm:nilReason, TransitionAltitudeNilReason, Graph ),
          TransitionAltitude=nil(TransitionAltitudeNilReason)
        );
        (
          rdf( TransitionAltitudeNode,gml:indeterminatePosition, TransitionAltitudeIndeterminate, Graph ),
          TransitionAltitude=indeterminate(TransitionAltitudeIndeterminate)
        )
      )
  )
  ,(
    ( TransitionLevel='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'transitionLevel',_TransitionLevel,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'transitionLevel',TransitionLevelNode,Graph )),
      (
        (
          rdf(TransitionLevelNode,rdf:value,TransitionLevelValue,Graph),
         \+ ( rdf( TransitionLevelNode, aixm:uom, _TransitionLevelUOM, Graph ); rdf( TransitionLevelNode, fixm:uom, _TransitionLevelUOM, Graph ); rdf( TransitionLevelNode, plain:uom, _TransitionLevelUOM, Graph ) ),
          TransitionLevel=val(TransitionLevelValue)
        );
        (
          rdf( TransitionLevelNode,rdf:value,TransitionLevelValue,Graph ),
          ( rdf( TransitionLevelNode, aixm:uom, TransitionLevelUOM, Graph ); rdf( TransitionLevelNode, fixm:uom, TransitionLevelUOM, Graph ); rdf( TransitionLevelNode, plain:uom, TransitionLevelUOM, Graph ) ),
          TransitionLevel=xval(TransitionLevelValue,TransitionLevelUOM)
        );
        (
          rdf( TransitionLevelNode,aixm:nilReason, TransitionLevelNilReason, Graph ),
          TransitionLevel=nil(TransitionLevelNilReason)
        );
        (
          rdf( TransitionLevelNode,gml:indeterminatePosition, TransitionLevelIndeterminate, Graph ),
          TransitionLevel=indeterminate(TransitionLevelIndeterminate)
        )
      )
  )
  ,(
    ( LowestTemperature='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'lowestTemperature',_LowestTemperature,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'lowestTemperature',LowestTemperatureNode,Graph )),
      (
        (
          rdf(LowestTemperatureNode,rdf:value,LowestTemperatureValue,Graph),
         \+ ( rdf( LowestTemperatureNode, aixm:uom, _LowestTemperatureUOM, Graph ); rdf( LowestTemperatureNode, fixm:uom, _LowestTemperatureUOM, Graph ); rdf( LowestTemperatureNode, plain:uom, _LowestTemperatureUOM, Graph ) ),
          LowestTemperature=val(LowestTemperatureValue)
        );
        (
          rdf( LowestTemperatureNode,rdf:value,LowestTemperatureValue,Graph ),
          ( rdf( LowestTemperatureNode, aixm:uom, LowestTemperatureUOM, Graph ); rdf( LowestTemperatureNode, fixm:uom, LowestTemperatureUOM, Graph ); rdf( LowestTemperatureNode, plain:uom, LowestTemperatureUOM, Graph ) ),
          LowestTemperature=xval(LowestTemperatureValue,LowestTemperatureUOM)
        );
        (
          rdf( LowestTemperatureNode,aixm:nilReason, LowestTemperatureNilReason, Graph ),
          LowestTemperature=nil(LowestTemperatureNilReason)
        );
        (
          rdf( LowestTemperatureNode,gml:indeterminatePosition, LowestTemperatureIndeterminate, Graph ),
          LowestTemperature=indeterminate(LowestTemperatureIndeterminate)
        )
      )
  )
  ,(
    ( Abandoned='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'abandoned',_Abandoned,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'abandoned',AbandonedNode,Graph )),
      (
        (
          rdf(AbandonedNode,rdf:value,AbandonedValue,Graph),
         \+ ( rdf( AbandonedNode, aixm:uom, _AbandonedUOM, Graph ); rdf( AbandonedNode, fixm:uom, _AbandonedUOM, Graph ); rdf( AbandonedNode, plain:uom, _AbandonedUOM, Graph ) ),
          Abandoned=val(AbandonedValue)
        );
        (
          rdf( AbandonedNode,rdf:value,AbandonedValue,Graph ),
          ( rdf( AbandonedNode, aixm:uom, AbandonedUOM, Graph ); rdf( AbandonedNode, fixm:uom, AbandonedUOM, Graph ); rdf( AbandonedNode, plain:uom, AbandonedUOM, Graph ) ),
          Abandoned=xval(AbandonedValue,AbandonedUOM)
        );
        (
          rdf( AbandonedNode,aixm:nilReason, AbandonedNilReason, Graph ),
          Abandoned=nil(AbandonedNilReason)
        );
        (
          rdf( AbandonedNode,gml:indeterminatePosition, AbandonedIndeterminate, Graph ),
          Abandoned=indeterminate(AbandonedIndeterminate)
        )
      )
  )
  ,(
    ( CertificationDate='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'certificationDate',_CertificationDate,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'certificationDate',CertificationDateNode,Graph )),
      (
        (
          rdf(CertificationDateNode,rdf:value,CertificationDateValue,Graph),
         \+ ( rdf( CertificationDateNode, aixm:uom, _CertificationDateUOM, Graph ); rdf( CertificationDateNode, fixm:uom, _CertificationDateUOM, Graph ); rdf( CertificationDateNode, plain:uom, _CertificationDateUOM, Graph ) ),
          CertificationDate=val(CertificationDateValue)
        );
        (
          rdf( CertificationDateNode,rdf:value,CertificationDateValue,Graph ),
          ( rdf( CertificationDateNode, aixm:uom, CertificationDateUOM, Graph ); rdf( CertificationDateNode, fixm:uom, CertificationDateUOM, Graph ); rdf( CertificationDateNode, plain:uom, CertificationDateUOM, Graph ) ),
          CertificationDate=xval(CertificationDateValue,CertificationDateUOM)
        );
        (
          rdf( CertificationDateNode,aixm:nilReason, CertificationDateNilReason, Graph ),
          CertificationDate=nil(CertificationDateNilReason)
        );
        (
          rdf( CertificationDateNode,gml:indeterminatePosition, CertificationDateIndeterminate, Graph ),
          CertificationDate=indeterminate(CertificationDateIndeterminate)
        )
      )
  )
  ,(
    ( CertificationExpirationDate='$null$',
      \+ rdf( AirportHeliportTimeSlice,aixm:'certificationExpirationDate',_CertificationExpirationDate,Graph )
    );
  ( rdf( AirportHeliportTimeSlice,aixm:'certificationExpirationDate',CertificationExpirationDateNode,Graph )),
      (
        (
          rdf(CertificationExpirationDateNode,rdf:value,CertificationExpirationDateValue,Graph),
         \+ ( rdf( CertificationExpirationDateNode, aixm:uom, _CertificationExpirationDateUOM, Graph ); rdf( CertificationExpirationDateNode, fixm:uom, _CertificationExpirationDateUOM, Graph ); rdf( CertificationExpirationDateNode, plain:uom, _CertificationExpirationDateUOM, Graph ) ),
          CertificationExpirationDate=val(CertificationExpirationDateValue)
        );
        (
          rdf( CertificationExpirationDateNode,rdf:value,CertificationExpirationDateValue,Graph ),
          ( rdf( CertificationExpirationDateNode, aixm:uom, CertificationExpirationDateUOM, Graph ); rdf( CertificationExpirationDateNode, fixm:uom, CertificationExpirationDateUOM, Graph ); rdf( CertificationExpirationDateNode, plain:uom, CertificationExpirationDateUOM, Graph ) ),
          CertificationExpirationDate=xval(CertificationExpirationDateValue,CertificationExpirationDateUOM)
        );
        (
          rdf( CertificationExpirationDateNode,aixm:nilReason, CertificationExpirationDateNilReason, Graph ),
          CertificationExpirationDate=nil(CertificationExpirationDateNilReason)
        );
        (
          rdf( CertificationExpirationDateNode,gml:indeterminatePosition, CertificationExpirationDateIndeterminate, Graph ),
          CertificationExpirationDate=indeterminate(CertificationExpirationDateIndeterminate)
        )
      )
  )
  ,findall(A, rdf(AirportHeliportTimeSlice,aixm:'contact',A,Graph), Contact)
  ,findall(A, rdf(AirportHeliportTimeSlice,aixm:'annotation',A,Graph), Annotation)
  ,( ( ARP='$null$',
    \+ rdf( AirportHeliportTimeSlice,aixm:'ARP', _ARP, Graph  )
   ; rdf(AirportHeliportTimeSlice,aixm:'ARP', ARP, Graph ) )
  )
  ,findall(A, rdf(AirportHeliportTimeSlice,aixm:'altimeterSource',A,Graph), AltimeterSource)
  ,findall(A, rdf(AirportHeliportTimeSlice,aixm:'contaminant',A,Graph), Contaminant)
  ,findall(A, rdf(AirportHeliportTimeSlice,aixm:'servedCity',A,Graph), ServedCity)
  ,( ( ResponsibleOrganisation='$null$',
    \+ rdf( AirportHeliportTimeSlice,aixm:'responsibleOrganisation', _ResponsibleOrganisation, Graph  )
   ; rdf(AirportHeliportTimeSlice,aixm:'responsibleOrganisation', ResponsibleOrganisation, Graph ) )
  )
  ,( ( AviationBoundary='$null$',
    \+ rdf( AirportHeliportTimeSlice,aixm:'aviationBoundary', _AviationBoundary, Graph  )
   ; rdf(AirportHeliportTimeSlice,aixm:'aviationBoundary', AviationBoundary, Graph ) )
  )
  ,findall(A, rdf(AirportHeliportTimeSlice,aixm:'availability',A,Graph), Availability) .

fixm_Point4D(Graph, Point4D, Altitude, Time, PointRange) :-
  subClassOf(T,fixm:'Point4D')
  ,rdf(Point4D,rdf:type,T,Graph)
  ,(
    ( Altitude='$null$',
      \+ rdf( Point4D,fixm:'altitude',_Altitude,Graph )
    );
  ( rdf( Point4D,fixm:'altitude',AltitudeNode,Graph )),
      (
        (
          rdf(AltitudeNode,rdf:value,AltitudeValue,Graph),
         \+ ( rdf( AltitudeNode, aixm:uom, _AltitudeUOM, Graph ); rdf( AltitudeNode, fixm:uom, _AltitudeUOM, Graph ); rdf( AltitudeNode, plain:uom, _AltitudeUOM, Graph ) ),
          Altitude=val(AltitudeValue)
        );
        (
          rdf( AltitudeNode,rdf:value,AltitudeValue,Graph ),
          ( rdf( AltitudeNode, aixm:uom, AltitudeUOM, Graph ); rdf( AltitudeNode, fixm:uom, AltitudeUOM, Graph ); rdf( AltitudeNode, plain:uom, AltitudeUOM, Graph ) ),
          Altitude=xval(AltitudeValue,AltitudeUOM)
        );
        (
          rdf( AltitudeNode,aixm:nilReason, AltitudeNilReason, Graph ),
          Altitude=nil(AltitudeNilReason)
        );
        (
          rdf( AltitudeNode,gml:indeterminatePosition, AltitudeIndeterminate, Graph ),
          Altitude=indeterminate(AltitudeIndeterminate)
        )
      )
  )
  ,(
    ( Time='$null$',
      \+ rdf( Point4D,fixm:'time',_Time,Graph )
    );
  ( rdf( Point4D,fixm:'time',TimeNode,Graph )),
      (
        (
          rdf(TimeNode,rdf:value,TimeValue,Graph),
         \+ ( rdf( TimeNode, aixm:uom, _TimeUOM, Graph ); rdf( TimeNode, fixm:uom, _TimeUOM, Graph ); rdf( TimeNode, plain:uom, _TimeUOM, Graph ) ),
          Time=val(TimeValue)
        );
        (
          rdf( TimeNode,rdf:value,TimeValue,Graph ),
          ( rdf( TimeNode, aixm:uom, TimeUOM, Graph ); rdf( TimeNode, fixm:uom, TimeUOM, Graph ); rdf( TimeNode, plain:uom, TimeUOM, Graph ) ),
          Time=xval(TimeValue,TimeUOM)
        );
        (
          rdf( TimeNode,aixm:nilReason, TimeNilReason, Graph ),
          Time=nil(TimeNilReason)
        );
        (
          rdf( TimeNode,gml:indeterminatePosition, TimeIndeterminate, Graph ),
          Time=indeterminate(TimeIndeterminate)
        )
      )
  )
  ,( ( PointRange='$null$',
    \+ rdf( Point4D,fixm:'pointRange', _PointRange, Graph  )
   ; rdf(Point4D,fixm:'pointRange', PointRange, Graph ) )
  ) .

fixm_AbstractRoutePoint(Graph, AbstractRoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit) :-
  subClassOf(T,fixm:'AbstractRoutePoint')
  ,rdf(AbstractRoutePoint,rdf:type,T,Graph)
  ,(
    ( AirTrafficType='$null$',
      \+ rdf( AbstractRoutePoint,fixm:'airTrafficType',_AirTrafficType,Graph )
    );
  ( rdf( AbstractRoutePoint,fixm:'airTrafficType',AirTrafficTypeNode,Graph )),
      (
        (
          rdf(AirTrafficTypeNode,rdf:value,AirTrafficTypeValue,Graph),
         \+ ( rdf( AirTrafficTypeNode, aixm:uom, _AirTrafficTypeUOM, Graph ); rdf( AirTrafficTypeNode, fixm:uom, _AirTrafficTypeUOM, Graph ); rdf( AirTrafficTypeNode, plain:uom, _AirTrafficTypeUOM, Graph ) ),
          AirTrafficType=val(AirTrafficTypeValue)
        );
        (
          rdf( AirTrafficTypeNode,rdf:value,AirTrafficTypeValue,Graph ),
          ( rdf( AirTrafficTypeNode, aixm:uom, AirTrafficTypeUOM, Graph ); rdf( AirTrafficTypeNode, fixm:uom, AirTrafficTypeUOM, Graph ); rdf( AirTrafficTypeNode, plain:uom, AirTrafficTypeUOM, Graph ) ),
          AirTrafficType=xval(AirTrafficTypeValue,AirTrafficTypeUOM)
        );
        (
          rdf( AirTrafficTypeNode,aixm:nilReason, AirTrafficTypeNilReason, Graph ),
          AirTrafficType=nil(AirTrafficTypeNilReason)
        );
        (
          rdf( AirTrafficTypeNode,gml:indeterminatePosition, AirTrafficTypeIndeterminate, Graph ),
          AirTrafficType=indeterminate(AirTrafficTypeIndeterminate)
        )
      )
  )
  ,(
    ( DelayAtPoint='$null$',
      \+ rdf( AbstractRoutePoint,fixm:'delayAtPoint',_DelayAtPoint,Graph )
    );
  ( rdf( AbstractRoutePoint,fixm:'delayAtPoint',DelayAtPointNode,Graph )),
      (
        (
          rdf(DelayAtPointNode,rdf:value,DelayAtPointValue,Graph),
         \+ ( rdf( DelayAtPointNode, aixm:uom, _DelayAtPointUOM, Graph ); rdf( DelayAtPointNode, fixm:uom, _DelayAtPointUOM, Graph ); rdf( DelayAtPointNode, plain:uom, _DelayAtPointUOM, Graph ) ),
          DelayAtPoint=val(DelayAtPointValue)
        );
        (
          rdf( DelayAtPointNode,rdf:value,DelayAtPointValue,Graph ),
          ( rdf( DelayAtPointNode, aixm:uom, DelayAtPointUOM, Graph ); rdf( DelayAtPointNode, fixm:uom, DelayAtPointUOM, Graph ); rdf( DelayAtPointNode, plain:uom, DelayAtPointUOM, Graph ) ),
          DelayAtPoint=xval(DelayAtPointValue,DelayAtPointUOM)
        );
        (
          rdf( DelayAtPointNode,aixm:nilReason, DelayAtPointNilReason, Graph ),
          DelayAtPoint=nil(DelayAtPointNilReason)
        );
        (
          rdf( DelayAtPointNode,gml:indeterminatePosition, DelayAtPointIndeterminate, Graph ),
          DelayAtPoint=indeterminate(DelayAtPointIndeterminate)
        )
      )
  )
  ,(
    ( FlightRules='$null$',
      \+ rdf( AbstractRoutePoint,fixm:'flightRules',_FlightRules,Graph )
    );
  ( rdf( AbstractRoutePoint,fixm:'flightRules',FlightRulesNode,Graph )),
      (
        (
          rdf(FlightRulesNode,rdf:value,FlightRulesValue,Graph),
         \+ ( rdf( FlightRulesNode, aixm:uom, _FlightRulesUOM, Graph ); rdf( FlightRulesNode, fixm:uom, _FlightRulesUOM, Graph ); rdf( FlightRulesNode, plain:uom, _FlightRulesUOM, Graph ) ),
          FlightRules=val(FlightRulesValue)
        );
        (
          rdf( FlightRulesNode,rdf:value,FlightRulesValue,Graph ),
          ( rdf( FlightRulesNode, aixm:uom, FlightRulesUOM, Graph ); rdf( FlightRulesNode, fixm:uom, FlightRulesUOM, Graph ); rdf( FlightRulesNode, plain:uom, FlightRulesUOM, Graph ) ),
          FlightRules=xval(FlightRulesValue,FlightRulesUOM)
        );
        (
          rdf( FlightRulesNode,aixm:nilReason, FlightRulesNilReason, Graph ),
          FlightRules=nil(FlightRulesNilReason)
        );
        (
          rdf( FlightRulesNode,gml:indeterminatePosition, FlightRulesIndeterminate, Graph ),
          FlightRules=indeterminate(FlightRulesIndeterminate)
        )
      )
  )
  ,( ( Point='$null$',
    \+ rdf( AbstractRoutePoint,fixm:'point', _Point, Graph  )
   ; rdf(AbstractRoutePoint,fixm:'point', Point, Graph ) )
  )
  ,(
    ( ClearanceLimit='$null$',
      \+ rdf( AbstractRoutePoint,fixm:'clearanceLimit',_ClearanceLimit,Graph )
    );
  ( rdf( AbstractRoutePoint,fixm:'clearanceLimit',ClearanceLimitNode,Graph )),
      (
        (
          rdf(ClearanceLimitNode,rdf:value,ClearanceLimitValue,Graph),
         \+ ( rdf( ClearanceLimitNode, aixm:uom, _ClearanceLimitUOM, Graph ); rdf( ClearanceLimitNode, fixm:uom, _ClearanceLimitUOM, Graph ); rdf( ClearanceLimitNode, plain:uom, _ClearanceLimitUOM, Graph ) ),
          ClearanceLimit=val(ClearanceLimitValue)
        );
        (
          rdf( ClearanceLimitNode,rdf:value,ClearanceLimitValue,Graph ),
          ( rdf( ClearanceLimitNode, aixm:uom, ClearanceLimitUOM, Graph ); rdf( ClearanceLimitNode, fixm:uom, ClearanceLimitUOM, Graph ); rdf( ClearanceLimitNode, plain:uom, ClearanceLimitUOM, Graph ) ),
          ClearanceLimit=xval(ClearanceLimitValue,ClearanceLimitUOM)
        );
        (
          rdf( ClearanceLimitNode,aixm:nilReason, ClearanceLimitNilReason, Graph ),
          ClearanceLimit=nil(ClearanceLimitNilReason)
        );
        (
          rdf( ClearanceLimitNode,gml:indeterminatePosition, ClearanceLimitIndeterminate, Graph ),
          ClearanceLimit=indeterminate(ClearanceLimitIndeterminate)
        )
      )
  ) .

aixm_Ridge(Graph, Ridge, Side, Distance, Depth, Annotation) :-
  rdf(Ridge,rdf:type,aixm:'Ridge',Graph)
  ,(
    ( Side='$null$',
      \+ rdf( Ridge,aixm:'side',_Side,Graph )
    );
  ( rdf( Ridge,aixm:'side',SideNode,Graph )),
      (
        (
          rdf(SideNode,rdf:value,SideValue,Graph),
         \+ ( rdf( SideNode, aixm:uom, _SideUOM, Graph ); rdf( SideNode, fixm:uom, _SideUOM, Graph ); rdf( SideNode, plain:uom, _SideUOM, Graph ) ),
          Side=val(SideValue)
        );
        (
          rdf( SideNode,rdf:value,SideValue,Graph ),
          ( rdf( SideNode, aixm:uom, SideUOM, Graph ); rdf( SideNode, fixm:uom, SideUOM, Graph ); rdf( SideNode, plain:uom, SideUOM, Graph ) ),
          Side=xval(SideValue,SideUOM)
        );
        (
          rdf( SideNode,aixm:nilReason, SideNilReason, Graph ),
          Side=nil(SideNilReason)
        );
        (
          rdf( SideNode,gml:indeterminatePosition, SideIndeterminate, Graph ),
          Side=indeterminate(SideIndeterminate)
        )
      )
  )
  ,(
    ( Distance='$null$',
      \+ rdf( Ridge,aixm:'distance',_Distance,Graph )
    );
  ( rdf( Ridge,aixm:'distance',DistanceNode,Graph )),
      (
        (
          rdf(DistanceNode,rdf:value,DistanceValue,Graph),
         \+ ( rdf( DistanceNode, aixm:uom, _DistanceUOM, Graph ); rdf( DistanceNode, fixm:uom, _DistanceUOM, Graph ); rdf( DistanceNode, plain:uom, _DistanceUOM, Graph ) ),
          Distance=val(DistanceValue)
        );
        (
          rdf( DistanceNode,rdf:value,DistanceValue,Graph ),
          ( rdf( DistanceNode, aixm:uom, DistanceUOM, Graph ); rdf( DistanceNode, fixm:uom, DistanceUOM, Graph ); rdf( DistanceNode, plain:uom, DistanceUOM, Graph ) ),
          Distance=xval(DistanceValue,DistanceUOM)
        );
        (
          rdf( DistanceNode,aixm:nilReason, DistanceNilReason, Graph ),
          Distance=nil(DistanceNilReason)
        );
        (
          rdf( DistanceNode,gml:indeterminatePosition, DistanceIndeterminate, Graph ),
          Distance=indeterminate(DistanceIndeterminate)
        )
      )
  )
  ,(
    ( Depth='$null$',
      \+ rdf( Ridge,aixm:'depth',_Depth,Graph )
    );
  ( rdf( Ridge,aixm:'depth',DepthNode,Graph )),
      (
        (
          rdf(DepthNode,rdf:value,DepthValue,Graph),
         \+ ( rdf( DepthNode, aixm:uom, _DepthUOM, Graph ); rdf( DepthNode, fixm:uom, _DepthUOM, Graph ); rdf( DepthNode, plain:uom, _DepthUOM, Graph ) ),
          Depth=val(DepthValue)
        );
        (
          rdf( DepthNode,rdf:value,DepthValue,Graph ),
          ( rdf( DepthNode, aixm:uom, DepthUOM, Graph ); rdf( DepthNode, fixm:uom, DepthUOM, Graph ); rdf( DepthNode, plain:uom, DepthUOM, Graph ) ),
          Depth=xval(DepthValue,DepthUOM)
        );
        (
          rdf( DepthNode,aixm:nilReason, DepthNilReason, Graph ),
          Depth=nil(DepthNilReason)
        );
        (
          rdf( DepthNode,gml:indeterminatePosition, DepthIndeterminate, Graph ),
          Depth=indeterminate(DepthIndeterminate)
        )
      )
  )
  ,findall(A, rdf(Ridge,aixm:'annotation',A,Graph), Annotation) .

fixm_DepartureActivityTimes(Graph, DepartureActivityTimes, BoardingTime, DeIcingTime, GroundHandlingTime, StartupTime) :-
  rdf(DepartureActivityTimes,rdf:type,fixm:'DepartureActivityTimes',Graph)
  ,( ( BoardingTime='$null$',
    \+ rdf( DepartureActivityTimes,fixm:'boardingTime', _BoardingTime, Graph  )
   ; rdf(DepartureActivityTimes,fixm:'boardingTime', BoardingTime, Graph ) )
  )
  ,( ( DeIcingTime='$null$',
    \+ rdf( DepartureActivityTimes,fixm:'deIcingTime', _DeIcingTime, Graph  )
   ; rdf(DepartureActivityTimes,fixm:'deIcingTime', DeIcingTime, Graph ) )
  )
  ,( ( GroundHandlingTime='$null$',
    \+ rdf( DepartureActivityTimes,fixm:'groundHandlingTime', _GroundHandlingTime, Graph  )
   ; rdf(DepartureActivityTimes,fixm:'groundHandlingTime', GroundHandlingTime, Graph ) )
  )
  ,( ( StartupTime='$null$',
    \+ rdf( DepartureActivityTimes,fixm:'startupTime', _StartupTime, Graph  )
   ; rdf(DepartureActivityTimes,fixm:'startupTime', StartupTime, Graph ) )
  ) .

fixm_EnRouteDiversion(Graph, EnRouteDiversion, DiversionRecoveryInformation) :-
  rdf(EnRouteDiversion,rdf:type,fixm:'EnRouteDiversion',Graph)
  ,(
    ( DiversionRecoveryInformation='$null$',
      \+ rdf( EnRouteDiversion,fixm:'diversionRecoveryInformation',_DiversionRecoveryInformation,Graph )
    );
  ( rdf( EnRouteDiversion,fixm:'diversionRecoveryInformation',DiversionRecoveryInformationNode,Graph )),
      (
        (
          rdf(DiversionRecoveryInformationNode,rdf:value,DiversionRecoveryInformationValue,Graph),
         \+ ( rdf( DiversionRecoveryInformationNode, aixm:uom, _DiversionRecoveryInformationUOM, Graph ); rdf( DiversionRecoveryInformationNode, fixm:uom, _DiversionRecoveryInformationUOM, Graph ); rdf( DiversionRecoveryInformationNode, plain:uom, _DiversionRecoveryInformationUOM, Graph ) ),
          DiversionRecoveryInformation=val(DiversionRecoveryInformationValue)
        );
        (
          rdf( DiversionRecoveryInformationNode,rdf:value,DiversionRecoveryInformationValue,Graph ),
          ( rdf( DiversionRecoveryInformationNode, aixm:uom, DiversionRecoveryInformationUOM, Graph ); rdf( DiversionRecoveryInformationNode, fixm:uom, DiversionRecoveryInformationUOM, Graph ); rdf( DiversionRecoveryInformationNode, plain:uom, DiversionRecoveryInformationUOM, Graph ) ),
          DiversionRecoveryInformation=xval(DiversionRecoveryInformationValue,DiversionRecoveryInformationUOM)
        );
        (
          rdf( DiversionRecoveryInformationNode,aixm:nilReason, DiversionRecoveryInformationNilReason, Graph ),
          DiversionRecoveryInformation=nil(DiversionRecoveryInformationNilReason)
        );
        (
          rdf( DiversionRecoveryInformationNode,gml:indeterminatePosition, DiversionRecoveryInformationIndeterminate, Graph ),
          DiversionRecoveryInformation=indeterminate(DiversionRecoveryInformationIndeterminate)
        )
      )
  ) .

fixm_ActualSpeed(Graph, ActualSpeed, Calculated, PilotReported, Surveillance) :-
  rdf(ActualSpeed,rdf:type,fixm:'ActualSpeed',Graph)
  ,(
    ( Calculated='$null$',
      \+ rdf( ActualSpeed,fixm:'calculated',_Calculated,Graph )
    );
  ( rdf( ActualSpeed,fixm:'calculated',CalculatedNode,Graph )),
      (
        (
          rdf(CalculatedNode,rdf:value,CalculatedValue,Graph),
         \+ ( rdf( CalculatedNode, aixm:uom, _CalculatedUOM, Graph ); rdf( CalculatedNode, fixm:uom, _CalculatedUOM, Graph ); rdf( CalculatedNode, plain:uom, _CalculatedUOM, Graph ) ),
          Calculated=val(CalculatedValue)
        );
        (
          rdf( CalculatedNode,rdf:value,CalculatedValue,Graph ),
          ( rdf( CalculatedNode, aixm:uom, CalculatedUOM, Graph ); rdf( CalculatedNode, fixm:uom, CalculatedUOM, Graph ); rdf( CalculatedNode, plain:uom, CalculatedUOM, Graph ) ),
          Calculated=xval(CalculatedValue,CalculatedUOM)
        );
        (
          rdf( CalculatedNode,aixm:nilReason, CalculatedNilReason, Graph ),
          Calculated=nil(CalculatedNilReason)
        );
        (
          rdf( CalculatedNode,gml:indeterminatePosition, CalculatedIndeterminate, Graph ),
          Calculated=indeterminate(CalculatedIndeterminate)
        )
      )
  )
  ,(
    ( PilotReported='$null$',
      \+ rdf( ActualSpeed,fixm:'pilotReported',_PilotReported,Graph )
    );
  ( rdf( ActualSpeed,fixm:'pilotReported',PilotReportedNode,Graph )),
      (
        (
          rdf(PilotReportedNode,rdf:value,PilotReportedValue,Graph),
         \+ ( rdf( PilotReportedNode, aixm:uom, _PilotReportedUOM, Graph ); rdf( PilotReportedNode, fixm:uom, _PilotReportedUOM, Graph ); rdf( PilotReportedNode, plain:uom, _PilotReportedUOM, Graph ) ),
          PilotReported=val(PilotReportedValue)
        );
        (
          rdf( PilotReportedNode,rdf:value,PilotReportedValue,Graph ),
          ( rdf( PilotReportedNode, aixm:uom, PilotReportedUOM, Graph ); rdf( PilotReportedNode, fixm:uom, PilotReportedUOM, Graph ); rdf( PilotReportedNode, plain:uom, PilotReportedUOM, Graph ) ),
          PilotReported=xval(PilotReportedValue,PilotReportedUOM)
        );
        (
          rdf( PilotReportedNode,aixm:nilReason, PilotReportedNilReason, Graph ),
          PilotReported=nil(PilotReportedNilReason)
        );
        (
          rdf( PilotReportedNode,gml:indeterminatePosition, PilotReportedIndeterminate, Graph ),
          PilotReported=indeterminate(PilotReportedIndeterminate)
        )
      )
  )
  ,(
    ( Surveillance='$null$',
      \+ rdf( ActualSpeed,fixm:'surveillance',_Surveillance,Graph )
    );
  ( rdf( ActualSpeed,fixm:'surveillance',SurveillanceNode,Graph )),
      (
        (
          rdf(SurveillanceNode,rdf:value,SurveillanceValue,Graph),
         \+ ( rdf( SurveillanceNode, aixm:uom, _SurveillanceUOM, Graph ); rdf( SurveillanceNode, fixm:uom, _SurveillanceUOM, Graph ); rdf( SurveillanceNode, plain:uom, _SurveillanceUOM, Graph ) ),
          Surveillance=val(SurveillanceValue)
        );
        (
          rdf( SurveillanceNode,rdf:value,SurveillanceValue,Graph ),
          ( rdf( SurveillanceNode, aixm:uom, SurveillanceUOM, Graph ); rdf( SurveillanceNode, fixm:uom, SurveillanceUOM, Graph ); rdf( SurveillanceNode, plain:uom, SurveillanceUOM, Graph ) ),
          Surveillance=xval(SurveillanceValue,SurveillanceUOM)
        );
        (
          rdf( SurveillanceNode,aixm:nilReason, SurveillanceNilReason, Graph ),
          Surveillance=nil(SurveillanceNilReason)
        );
        (
          rdf( SurveillanceNode,gml:indeterminatePosition, SurveillanceIndeterminate, Graph ),
          Surveillance=indeterminate(SurveillanceIndeterminate)
        )
      )
  ) .

fixm_FlightEmergency(Graph, FlightEmergency, ActionTaken, EmergencyDescription, Originator, OtherInformation, Phase, Contact) :-
  rdf(FlightEmergency,rdf:type,fixm:'FlightEmergency',Graph)
  ,(
    ( ActionTaken='$null$',
      \+ rdf( FlightEmergency,fixm:'actionTaken',_ActionTaken,Graph )
    );
  ( rdf( FlightEmergency,fixm:'actionTaken',ActionTakenNode,Graph )),
      (
        (
          rdf(ActionTakenNode,rdf:value,ActionTakenValue,Graph),
         \+ ( rdf( ActionTakenNode, aixm:uom, _ActionTakenUOM, Graph ); rdf( ActionTakenNode, fixm:uom, _ActionTakenUOM, Graph ); rdf( ActionTakenNode, plain:uom, _ActionTakenUOM, Graph ) ),
          ActionTaken=val(ActionTakenValue)
        );
        (
          rdf( ActionTakenNode,rdf:value,ActionTakenValue,Graph ),
          ( rdf( ActionTakenNode, aixm:uom, ActionTakenUOM, Graph ); rdf( ActionTakenNode, fixm:uom, ActionTakenUOM, Graph ); rdf( ActionTakenNode, plain:uom, ActionTakenUOM, Graph ) ),
          ActionTaken=xval(ActionTakenValue,ActionTakenUOM)
        );
        (
          rdf( ActionTakenNode,aixm:nilReason, ActionTakenNilReason, Graph ),
          ActionTaken=nil(ActionTakenNilReason)
        );
        (
          rdf( ActionTakenNode,gml:indeterminatePosition, ActionTakenIndeterminate, Graph ),
          ActionTaken=indeterminate(ActionTakenIndeterminate)
        )
      )
  )
  ,(
    ( EmergencyDescription='$null$',
      \+ rdf( FlightEmergency,fixm:'emergencyDescription',_EmergencyDescription,Graph )
    );
  ( rdf( FlightEmergency,fixm:'emergencyDescription',EmergencyDescriptionNode,Graph )),
      (
        (
          rdf(EmergencyDescriptionNode,rdf:value,EmergencyDescriptionValue,Graph),
         \+ ( rdf( EmergencyDescriptionNode, aixm:uom, _EmergencyDescriptionUOM, Graph ); rdf( EmergencyDescriptionNode, fixm:uom, _EmergencyDescriptionUOM, Graph ); rdf( EmergencyDescriptionNode, plain:uom, _EmergencyDescriptionUOM, Graph ) ),
          EmergencyDescription=val(EmergencyDescriptionValue)
        );
        (
          rdf( EmergencyDescriptionNode,rdf:value,EmergencyDescriptionValue,Graph ),
          ( rdf( EmergencyDescriptionNode, aixm:uom, EmergencyDescriptionUOM, Graph ); rdf( EmergencyDescriptionNode, fixm:uom, EmergencyDescriptionUOM, Graph ); rdf( EmergencyDescriptionNode, plain:uom, EmergencyDescriptionUOM, Graph ) ),
          EmergencyDescription=xval(EmergencyDescriptionValue,EmergencyDescriptionUOM)
        );
        (
          rdf( EmergencyDescriptionNode,aixm:nilReason, EmergencyDescriptionNilReason, Graph ),
          EmergencyDescription=nil(EmergencyDescriptionNilReason)
        );
        (
          rdf( EmergencyDescriptionNode,gml:indeterminatePosition, EmergencyDescriptionIndeterminate, Graph ),
          EmergencyDescription=indeterminate(EmergencyDescriptionIndeterminate)
        )
      )
  )
  ,( ( Originator='$null$',
    \+ rdf( FlightEmergency,fixm:'originator', _Originator, Graph  )
   ; rdf(FlightEmergency,fixm:'originator', Originator, Graph ) )
  )
  ,(
    ( OtherInformation='$null$',
      \+ rdf( FlightEmergency,fixm:'otherInformation',_OtherInformation,Graph )
    );
  ( rdf( FlightEmergency,fixm:'otherInformation',OtherInformationNode,Graph )),
      (
        (
          rdf(OtherInformationNode,rdf:value,OtherInformationValue,Graph),
         \+ ( rdf( OtherInformationNode, aixm:uom, _OtherInformationUOM, Graph ); rdf( OtherInformationNode, fixm:uom, _OtherInformationUOM, Graph ); rdf( OtherInformationNode, plain:uom, _OtherInformationUOM, Graph ) ),
          OtherInformation=val(OtherInformationValue)
        );
        (
          rdf( OtherInformationNode,rdf:value,OtherInformationValue,Graph ),
          ( rdf( OtherInformationNode, aixm:uom, OtherInformationUOM, Graph ); rdf( OtherInformationNode, fixm:uom, OtherInformationUOM, Graph ); rdf( OtherInformationNode, plain:uom, OtherInformationUOM, Graph ) ),
          OtherInformation=xval(OtherInformationValue,OtherInformationUOM)
        );
        (
          rdf( OtherInformationNode,aixm:nilReason, OtherInformationNilReason, Graph ),
          OtherInformation=nil(OtherInformationNilReason)
        );
        (
          rdf( OtherInformationNode,gml:indeterminatePosition, OtherInformationIndeterminate, Graph ),
          OtherInformation=indeterminate(OtherInformationIndeterminate)
        )
      )
  )
  ,(
    ( Phase='$null$',
      \+ rdf( FlightEmergency,fixm:'phase',_Phase,Graph )
    );
  ( rdf( FlightEmergency,fixm:'phase',PhaseNode,Graph )),
      (
        (
          rdf(PhaseNode,rdf:value,PhaseValue,Graph),
         \+ ( rdf( PhaseNode, aixm:uom, _PhaseUOM, Graph ); rdf( PhaseNode, fixm:uom, _PhaseUOM, Graph ); rdf( PhaseNode, plain:uom, _PhaseUOM, Graph ) ),
          Phase=val(PhaseValue)
        );
        (
          rdf( PhaseNode,rdf:value,PhaseValue,Graph ),
          ( rdf( PhaseNode, aixm:uom, PhaseUOM, Graph ); rdf( PhaseNode, fixm:uom, PhaseUOM, Graph ); rdf( PhaseNode, plain:uom, PhaseUOM, Graph ) ),
          Phase=xval(PhaseValue,PhaseUOM)
        );
        (
          rdf( PhaseNode,aixm:nilReason, PhaseNilReason, Graph ),
          Phase=nil(PhaseNilReason)
        );
        (
          rdf( PhaseNode,gml:indeterminatePosition, PhaseIndeterminate, Graph ),
          Phase=indeterminate(PhaseIndeterminate)
        )
      )
  )
  ,( ( Contact='$null$',
    \+ rdf( FlightEmergency,fixm:'contact', _Contact, Graph  )
   ; rdf(FlightEmergency,fixm:'contact', Contact, Graph ) )
  ) .

fixm_Flight(Graph, Flight, ControllingUnit, Extensions, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoods, RankedTrajectories, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandling) :-
  subClassOf(T,fixm:'Flight')
  ,rdf(Flight,rdf:type,T,Graph)
  ,( ( ControllingUnit='$null$',
    \+ rdf( Flight,fixm:'controllingUnit', _ControllingUnit, Graph  )
   ; rdf(Flight,fixm:'controllingUnit', ControllingUnit, Graph ) )
  )
  ,findall(A, rdf(Flight,fixm:'extensions',A,Graph), Extensions)
  ,(
    ( FlightFiler='$null$',
      \+ rdf( Flight,fixm:'flightFiler',_FlightFiler,Graph )
    );
  ( rdf( Flight,fixm:'flightFiler',FlightFilerNode,Graph )),
      (
        (
          rdf(FlightFilerNode,rdf:value,FlightFilerValue,Graph),
         \+ ( rdf( FlightFilerNode, aixm:uom, _FlightFilerUOM, Graph ); rdf( FlightFilerNode, fixm:uom, _FlightFilerUOM, Graph ); rdf( FlightFilerNode, plain:uom, _FlightFilerUOM, Graph ) ),
          FlightFiler=val(FlightFilerValue)
        );
        (
          rdf( FlightFilerNode,rdf:value,FlightFilerValue,Graph ),
          ( rdf( FlightFilerNode, aixm:uom, FlightFilerUOM, Graph ); rdf( FlightFilerNode, fixm:uom, FlightFilerUOM, Graph ); rdf( FlightFilerNode, plain:uom, FlightFilerUOM, Graph ) ),
          FlightFiler=xval(FlightFilerValue,FlightFilerUOM)
        );
        (
          rdf( FlightFilerNode,aixm:nilReason, FlightFilerNilReason, Graph ),
          FlightFiler=nil(FlightFilerNilReason)
        );
        (
          rdf( FlightFilerNode,gml:indeterminatePosition, FlightFilerIndeterminate, Graph ),
          FlightFiler=indeterminate(FlightFilerIndeterminate)
        )
      )
  )
  ,(
    ( Gufi='$null$',
      \+ rdf( Flight,fixm:'gufi',_Gufi,Graph )
    );
  ( rdf( Flight,fixm:'gufi',GufiNode,Graph )),
      (
        (
          rdf(GufiNode,rdf:value,GufiValue,Graph),
         \+ ( rdf( GufiNode, aixm:uom, _GufiUOM, Graph ); rdf( GufiNode, fixm:uom, _GufiUOM, Graph ); rdf( GufiNode, plain:uom, _GufiUOM, Graph ) ),
          Gufi=val(GufiValue)
        );
        (
          rdf( GufiNode,rdf:value,GufiValue,Graph ),
          ( rdf( GufiNode, aixm:uom, GufiUOM, Graph ); rdf( GufiNode, fixm:uom, GufiUOM, Graph ); rdf( GufiNode, plain:uom, GufiUOM, Graph ) ),
          Gufi=xval(GufiValue,GufiUOM)
        );
        (
          rdf( GufiNode,aixm:nilReason, GufiNilReason, Graph ),
          Gufi=nil(GufiNilReason)
        );
        (
          rdf( GufiNode,gml:indeterminatePosition, GufiIndeterminate, Graph ),
          Gufi=indeterminate(GufiIndeterminate)
        )
      )
  )
  ,(
    ( Remarks='$null$',
      \+ rdf( Flight,fixm:'remarks',_Remarks,Graph )
    );
  ( rdf( Flight,fixm:'remarks',RemarksNode,Graph )),
      (
        (
          rdf(RemarksNode,rdf:value,RemarksValue,Graph),
         \+ ( rdf( RemarksNode, aixm:uom, _RemarksUOM, Graph ); rdf( RemarksNode, fixm:uom, _RemarksUOM, Graph ); rdf( RemarksNode, plain:uom, _RemarksUOM, Graph ) ),
          Remarks=val(RemarksValue)
        );
        (
          rdf( RemarksNode,rdf:value,RemarksValue,Graph ),
          ( rdf( RemarksNode, aixm:uom, RemarksUOM, Graph ); rdf( RemarksNode, fixm:uom, RemarksUOM, Graph ); rdf( RemarksNode, plain:uom, RemarksUOM, Graph ) ),
          Remarks=xval(RemarksValue,RemarksUOM)
        );
        (
          rdf( RemarksNode,aixm:nilReason, RemarksNilReason, Graph ),
          Remarks=nil(RemarksNilReason)
        );
        (
          rdf( RemarksNode,gml:indeterminatePosition, RemarksIndeterminate, Graph ),
          Remarks=indeterminate(RemarksIndeterminate)
        )
      )
  )
  ,( ( AircraftDescription='$null$',
    \+ rdf( Flight,fixm:'aircraftDescription', _AircraftDescription, Graph  )
   ; rdf(Flight,fixm:'aircraftDescription', AircraftDescription, Graph ) )
  )
  ,findall(A, rdf(Flight,fixm:'dangerousGoods',A,Graph), DangerousGoods)
  ,findall(A, rdf(Flight,fixm:'rankedTrajectories',A,Graph), RankedTrajectories)
  ,( ( RouteToRevisedDestination='$null$',
    \+ rdf( Flight,fixm:'routeToRevisedDestination', _RouteToRevisedDestination, Graph  )
   ; rdf(Flight,fixm:'routeToRevisedDestination', RouteToRevisedDestination, Graph ) )
  )
  ,( ( Negotiating='$null$',
    \+ rdf( Flight,fixm:'negotiating', _Negotiating, Graph  )
   ; rdf(Flight,fixm:'negotiating', Negotiating, Graph ) )
  )
  ,( ( Agreed='$null$',
    \+ rdf( Flight,fixm:'agreed', _Agreed, Graph  )
   ; rdf(Flight,fixm:'agreed', Agreed, Graph ) )
  )
  ,( ( Arrival='$null$',
    \+ rdf( Flight,fixm:'arrival', _Arrival, Graph  )
   ; rdf(Flight,fixm:'arrival', Arrival, Graph ) )
  )
  ,( ( Departure='$null$',
    \+ rdf( Flight,fixm:'departure', _Departure, Graph  )
   ; rdf(Flight,fixm:'departure', Departure, Graph ) )
  )
  ,( ( Emergency='$null$',
    \+ rdf( Flight,fixm:'emergency', _Emergency, Graph  )
   ; rdf(Flight,fixm:'emergency', Emergency, Graph ) )
  )
  ,( ( RadioCommunicationFailure='$null$',
    \+ rdf( Flight,fixm:'radioCommunicationFailure', _RadioCommunicationFailure, Graph  )
   ; rdf(Flight,fixm:'radioCommunicationFailure', RadioCommunicationFailure, Graph ) )
  )
  ,( ( EnRoute='$null$',
    \+ rdf( Flight,fixm:'enRoute', _EnRoute, Graph  )
   ; rdf(Flight,fixm:'enRoute', EnRoute, Graph ) )
  )
  ,( ( Operator='$null$',
    \+ rdf( Flight,fixm:'operator', _Operator, Graph  )
   ; rdf(Flight,fixm:'operator', Operator, Graph ) )
  )
  ,( ( EnRouteDiversion='$null$',
    \+ rdf( Flight,fixm:'enRouteDiversion', _EnRouteDiversion, Graph  )
   ; rdf(Flight,fixm:'enRouteDiversion', EnRouteDiversion, Graph ) )
  )
  ,(
    ( FlightType='$null$',
      \+ rdf( Flight,fixm:'flightType',_FlightType,Graph )
    );
  ( rdf( Flight,fixm:'flightType',FlightTypeNode,Graph )),
      (
        (
          rdf(FlightTypeNode,rdf:value,FlightTypeValue,Graph),
         \+ ( rdf( FlightTypeNode, aixm:uom, _FlightTypeUOM, Graph ); rdf( FlightTypeNode, fixm:uom, _FlightTypeUOM, Graph ); rdf( FlightTypeNode, plain:uom, _FlightTypeUOM, Graph ) ),
          FlightType=val(FlightTypeValue)
        );
        (
          rdf( FlightTypeNode,rdf:value,FlightTypeValue,Graph ),
          ( rdf( FlightTypeNode, aixm:uom, FlightTypeUOM, Graph ); rdf( FlightTypeNode, fixm:uom, FlightTypeUOM, Graph ); rdf( FlightTypeNode, plain:uom, FlightTypeUOM, Graph ) ),
          FlightType=xval(FlightTypeValue,FlightTypeUOM)
        );
        (
          rdf( FlightTypeNode,aixm:nilReason, FlightTypeNilReason, Graph ),
          FlightType=nil(FlightTypeNilReason)
        );
        (
          rdf( FlightTypeNode,gml:indeterminatePosition, FlightTypeIndeterminate, Graph ),
          FlightType=indeterminate(FlightTypeIndeterminate)
        )
      )
  )
  ,( ( FlightStatus='$null$',
    \+ rdf( Flight,fixm:'flightStatus', _FlightStatus, Graph  )
   ; rdf(Flight,fixm:'flightStatus', FlightStatus, Graph ) )
  )
  ,(
    ( Originator='$null$',
      \+ rdf( Flight,fixm:'originator',_Originator,Graph )
    );
  ( rdf( Flight,fixm:'originator',OriginatorNode,Graph )),
      (
        (
          rdf(OriginatorNode,rdf:value,OriginatorValue,Graph),
         \+ ( rdf( OriginatorNode, aixm:uom, _OriginatorUOM, Graph ); rdf( OriginatorNode, fixm:uom, _OriginatorUOM, Graph ); rdf( OriginatorNode, plain:uom, _OriginatorUOM, Graph ) ),
          Originator=val(OriginatorValue)
        );
        (
          rdf( OriginatorNode,rdf:value,OriginatorValue,Graph ),
          ( rdf( OriginatorNode, aixm:uom, OriginatorUOM, Graph ); rdf( OriginatorNode, fixm:uom, OriginatorUOM, Graph ); rdf( OriginatorNode, plain:uom, OriginatorUOM, Graph ) ),
          Originator=xval(OriginatorValue,OriginatorUOM)
        );
        (
          rdf( OriginatorNode,aixm:nilReason, OriginatorNilReason, Graph ),
          Originator=nil(OriginatorNilReason)
        );
        (
          rdf( OriginatorNode,gml:indeterminatePosition, OriginatorIndeterminate, Graph ),
          Originator=indeterminate(OriginatorIndeterminate)
        )
      )
  )
  ,( ( SupplementalData='$null$',
    \+ rdf( Flight,fixm:'supplementalData', _SupplementalData, Graph  )
   ; rdf(Flight,fixm:'supplementalData', SupplementalData, Graph ) )
  )
  ,( ( FlightIdentification='$null$',
    \+ rdf( Flight,fixm:'flightIdentification', _FlightIdentification, Graph  )
   ; rdf(Flight,fixm:'flightIdentification', FlightIdentification, Graph ) )
  )
  ,findall(A, rdf(Flight,fixm:'specialHandling',A,Graph), SpecialHandling) .

aixm_PropertiesWithSchedule(Graph, PropertiesWithSchedule, Annotation, SpecialDateAuthority, TimeInterval) :-
  subClassOf(T,aixm:'PropertiesWithSchedule')
  ,rdf(PropertiesWithSchedule,rdf:type,T,Graph)
  ,findall(A, rdf(PropertiesWithSchedule,aixm:'annotation',A,Graph), Annotation)
  ,findall(A, rdf(PropertiesWithSchedule,aixm:'specialDateAuthority',A,Graph), SpecialDateAuthority)
  ,findall(A, rdf(PropertiesWithSchedule,aixm:'timeInterval',A,Graph), TimeInterval) .

gml_Surface(Graph, Surface, Patch) :-
  subClassOf(T,gml:'Surface')
  ,rdf(Surface,rdf:type,T,Graph)
  ,findall(A, rdf(Surface,aixm:'patch',A,Graph), Patch) .

fixm_ClearedFlightInformation(Graph, ClearedFlightInformation, ClearedFlightLevel, ClearedSpeed, Heading, OfftrackClearance, RateOfClimbDescend, DirectRouting) :-
  rdf(ClearedFlightInformation,rdf:type,fixm:'ClearedFlightInformation',Graph)
  ,(
    ( ClearedFlightLevel='$null$',
      \+ rdf( ClearedFlightInformation,fixm:'clearedFlightLevel',_ClearedFlightLevel,Graph )
    );
  ( rdf( ClearedFlightInformation,fixm:'clearedFlightLevel',ClearedFlightLevelNode,Graph )),
      (
        (
          rdf(ClearedFlightLevelNode,rdf:value,ClearedFlightLevelValue,Graph),
         \+ ( rdf( ClearedFlightLevelNode, aixm:uom, _ClearedFlightLevelUOM, Graph ); rdf( ClearedFlightLevelNode, fixm:uom, _ClearedFlightLevelUOM, Graph ); rdf( ClearedFlightLevelNode, plain:uom, _ClearedFlightLevelUOM, Graph ) ),
          ClearedFlightLevel=val(ClearedFlightLevelValue)
        );
        (
          rdf( ClearedFlightLevelNode,rdf:value,ClearedFlightLevelValue,Graph ),
          ( rdf( ClearedFlightLevelNode, aixm:uom, ClearedFlightLevelUOM, Graph ); rdf( ClearedFlightLevelNode, fixm:uom, ClearedFlightLevelUOM, Graph ); rdf( ClearedFlightLevelNode, plain:uom, ClearedFlightLevelUOM, Graph ) ),
          ClearedFlightLevel=xval(ClearedFlightLevelValue,ClearedFlightLevelUOM)
        );
        (
          rdf( ClearedFlightLevelNode,aixm:nilReason, ClearedFlightLevelNilReason, Graph ),
          ClearedFlightLevel=nil(ClearedFlightLevelNilReason)
        );
        (
          rdf( ClearedFlightLevelNode,gml:indeterminatePosition, ClearedFlightLevelIndeterminate, Graph ),
          ClearedFlightLevel=indeterminate(ClearedFlightLevelIndeterminate)
        )
      )
  )
  ,(
    ( ClearedSpeed='$null$',
      \+ rdf( ClearedFlightInformation,fixm:'clearedSpeed',_ClearedSpeed,Graph )
    );
  ( rdf( ClearedFlightInformation,fixm:'clearedSpeed',ClearedSpeedNode,Graph )),
      (
        (
          rdf(ClearedSpeedNode,rdf:value,ClearedSpeedValue,Graph),
         \+ ( rdf( ClearedSpeedNode, aixm:uom, _ClearedSpeedUOM, Graph ); rdf( ClearedSpeedNode, fixm:uom, _ClearedSpeedUOM, Graph ); rdf( ClearedSpeedNode, plain:uom, _ClearedSpeedUOM, Graph ) ),
          ClearedSpeed=val(ClearedSpeedValue)
        );
        (
          rdf( ClearedSpeedNode,rdf:value,ClearedSpeedValue,Graph ),
          ( rdf( ClearedSpeedNode, aixm:uom, ClearedSpeedUOM, Graph ); rdf( ClearedSpeedNode, fixm:uom, ClearedSpeedUOM, Graph ); rdf( ClearedSpeedNode, plain:uom, ClearedSpeedUOM, Graph ) ),
          ClearedSpeed=xval(ClearedSpeedValue,ClearedSpeedUOM)
        );
        (
          rdf( ClearedSpeedNode,aixm:nilReason, ClearedSpeedNilReason, Graph ),
          ClearedSpeed=nil(ClearedSpeedNilReason)
        );
        (
          rdf( ClearedSpeedNode,gml:indeterminatePosition, ClearedSpeedIndeterminate, Graph ),
          ClearedSpeed=indeterminate(ClearedSpeedIndeterminate)
        )
      )
  )
  ,(
    ( Heading='$null$',
      \+ rdf( ClearedFlightInformation,fixm:'heading',_Heading,Graph )
    );
  ( rdf( ClearedFlightInformation,fixm:'heading',HeadingNode,Graph )),
      (
        (
          rdf(HeadingNode,rdf:value,HeadingValue,Graph),
         \+ ( rdf( HeadingNode, aixm:uom, _HeadingUOM, Graph ); rdf( HeadingNode, fixm:uom, _HeadingUOM, Graph ); rdf( HeadingNode, plain:uom, _HeadingUOM, Graph ) ),
          Heading=val(HeadingValue)
        );
        (
          rdf( HeadingNode,rdf:value,HeadingValue,Graph ),
          ( rdf( HeadingNode, aixm:uom, HeadingUOM, Graph ); rdf( HeadingNode, fixm:uom, HeadingUOM, Graph ); rdf( HeadingNode, plain:uom, HeadingUOM, Graph ) ),
          Heading=xval(HeadingValue,HeadingUOM)
        );
        (
          rdf( HeadingNode,aixm:nilReason, HeadingNilReason, Graph ),
          Heading=nil(HeadingNilReason)
        );
        (
          rdf( HeadingNode,gml:indeterminatePosition, HeadingIndeterminate, Graph ),
          Heading=indeterminate(HeadingIndeterminate)
        )
      )
  )
  ,( ( OfftrackClearance='$null$',
    \+ rdf( ClearedFlightInformation,fixm:'offtrackClearance', _OfftrackClearance, Graph  )
   ; rdf(ClearedFlightInformation,fixm:'offtrackClearance', OfftrackClearance, Graph ) )
  )
  ,(
    ( RateOfClimbDescend='$null$',
      \+ rdf( ClearedFlightInformation,fixm:'rateOfClimbDescend',_RateOfClimbDescend,Graph )
    );
  ( rdf( ClearedFlightInformation,fixm:'rateOfClimbDescend',RateOfClimbDescendNode,Graph )),
      (
        (
          rdf(RateOfClimbDescendNode,rdf:value,RateOfClimbDescendValue,Graph),
         \+ ( rdf( RateOfClimbDescendNode, aixm:uom, _RateOfClimbDescendUOM, Graph ); rdf( RateOfClimbDescendNode, fixm:uom, _RateOfClimbDescendUOM, Graph ); rdf( RateOfClimbDescendNode, plain:uom, _RateOfClimbDescendUOM, Graph ) ),
          RateOfClimbDescend=val(RateOfClimbDescendValue)
        );
        (
          rdf( RateOfClimbDescendNode,rdf:value,RateOfClimbDescendValue,Graph ),
          ( rdf( RateOfClimbDescendNode, aixm:uom, RateOfClimbDescendUOM, Graph ); rdf( RateOfClimbDescendNode, fixm:uom, RateOfClimbDescendUOM, Graph ); rdf( RateOfClimbDescendNode, plain:uom, RateOfClimbDescendUOM, Graph ) ),
          RateOfClimbDescend=xval(RateOfClimbDescendValue,RateOfClimbDescendUOM)
        );
        (
          rdf( RateOfClimbDescendNode,aixm:nilReason, RateOfClimbDescendNilReason, Graph ),
          RateOfClimbDescend=nil(RateOfClimbDescendNilReason)
        );
        (
          rdf( RateOfClimbDescendNode,gml:indeterminatePosition, RateOfClimbDescendIndeterminate, Graph ),
          RateOfClimbDescend=indeterminate(RateOfClimbDescendIndeterminate)
        )
      )
  )
  ,( ( DirectRouting='$null$',
    \+ rdf( ClearedFlightInformation,fixm:'directRouting', _DirectRouting, Graph  )
   ; rdf(ClearedFlightInformation,fixm:'directRouting', DirectRouting, Graph ) )
  ) .

fixm_TrajectoryRoutePair(Graph, TrajectoryRoutePair, Trajectory, Route) :-
  subClassOf(T,fixm:'TrajectoryRoutePair')
  ,rdf(TrajectoryRoutePair,rdf:type,T,Graph)
  ,( ( Trajectory='$null$',
    \+ rdf( TrajectoryRoutePair,fixm:'trajectory', _Trajectory, Graph  )
   ; rdf(TrajectoryRoutePair,fixm:'trajectory', Trajectory, Graph ) )
  )
  ,( ( Route='$null$',
    \+ rdf( TrajectoryRoutePair,fixm:'route', _Route, Graph  )
   ; rdf(TrajectoryRoutePair,fixm:'route', Route, Graph ) )
  ) .

fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit, UpstreamUnit, BoundaryCrossingProposed, BoundaryCrossingCoordinated, Handoff, UnitBoundaryIndicator) :-
  rdf(UnitBoundary,rdf:type,fixm:'UnitBoundary',Graph)
  ,( ( DownstreamUnit='$null$',
    \+ rdf( UnitBoundary,fixm:'downstreamUnit', _DownstreamUnit, Graph  )
   ; rdf(UnitBoundary,fixm:'downstreamUnit', DownstreamUnit, Graph ) )
  )
  ,( ( UpstreamUnit='$null$',
    \+ rdf( UnitBoundary,fixm:'upstreamUnit', _UpstreamUnit, Graph  )
   ; rdf(UnitBoundary,fixm:'upstreamUnit', UpstreamUnit, Graph ) )
  )
  ,( ( BoundaryCrossingProposed='$null$',
    \+ rdf( UnitBoundary,fixm:'boundaryCrossingProposed', _BoundaryCrossingProposed, Graph  )
   ; rdf(UnitBoundary,fixm:'boundaryCrossingProposed', BoundaryCrossingProposed, Graph ) )
  )
  ,( ( BoundaryCrossingCoordinated='$null$',
    \+ rdf( UnitBoundary,fixm:'boundaryCrossingCoordinated', _BoundaryCrossingCoordinated, Graph  )
   ; rdf(UnitBoundary,fixm:'boundaryCrossingCoordinated', BoundaryCrossingCoordinated, Graph ) )
  )
  ,( ( Handoff='$null$',
    \+ rdf( UnitBoundary,fixm:'handoff', _Handoff, Graph  )
   ; rdf(UnitBoundary,fixm:'handoff', Handoff, Graph ) )
  )
  ,(
    ( UnitBoundaryIndicator='$null$',
      \+ rdf( UnitBoundary,fixm:'unitBoundaryIndicator',_UnitBoundaryIndicator,Graph )
    );
  ( rdf( UnitBoundary,fixm:'unitBoundaryIndicator',UnitBoundaryIndicatorNode,Graph )),
      (
        (
          rdf(UnitBoundaryIndicatorNode,rdf:value,UnitBoundaryIndicatorValue,Graph),
         \+ ( rdf( UnitBoundaryIndicatorNode, aixm:uom, _UnitBoundaryIndicatorUOM, Graph ); rdf( UnitBoundaryIndicatorNode, fixm:uom, _UnitBoundaryIndicatorUOM, Graph ); rdf( UnitBoundaryIndicatorNode, plain:uom, _UnitBoundaryIndicatorUOM, Graph ) ),
          UnitBoundaryIndicator=val(UnitBoundaryIndicatorValue)
        );
        (
          rdf( UnitBoundaryIndicatorNode,rdf:value,UnitBoundaryIndicatorValue,Graph ),
          ( rdf( UnitBoundaryIndicatorNode, aixm:uom, UnitBoundaryIndicatorUOM, Graph ); rdf( UnitBoundaryIndicatorNode, fixm:uom, UnitBoundaryIndicatorUOM, Graph ); rdf( UnitBoundaryIndicatorNode, plain:uom, UnitBoundaryIndicatorUOM, Graph ) ),
          UnitBoundaryIndicator=xval(UnitBoundaryIndicatorValue,UnitBoundaryIndicatorUOM)
        );
        (
          rdf( UnitBoundaryIndicatorNode,aixm:nilReason, UnitBoundaryIndicatorNilReason, Graph ),
          UnitBoundaryIndicator=nil(UnitBoundaryIndicatorNilReason)
        );
        (
          rdf( UnitBoundaryIndicatorNode,gml:indeterminatePosition, UnitBoundaryIndicatorIndeterminate, Graph ),
          UnitBoundaryIndicator=indeterminate(UnitBoundaryIndicatorIndeterminate)
        )
      )
  ) .

aixm_SurfaceContamination(Graph, SurfaceContamination, ObservationTime, Depth, FrictionCoefficient, FrictionEstimation, FrictionDevice, ObscuredLights, FurtherClearanceTime, FurtherTotalClearance, NextObservationTime, Proportion, CriticalRidge, Annotation, Layer) :-
  subClassOf(T,aixm:'SurfaceContamination')
  ,rdf(SurfaceContamination,rdf:type,T,Graph)
  ,(
    ( ObservationTime='$null$',
      \+ rdf( SurfaceContamination,aixm:'observationTime',_ObservationTime,Graph )
    );
  ( rdf( SurfaceContamination,aixm:'observationTime',ObservationTimeNode,Graph )),
      (
        (
          rdf(ObservationTimeNode,rdf:value,ObservationTimeValue,Graph),
         \+ ( rdf( ObservationTimeNode, aixm:uom, _ObservationTimeUOM, Graph ); rdf( ObservationTimeNode, fixm:uom, _ObservationTimeUOM, Graph ); rdf( ObservationTimeNode, plain:uom, _ObservationTimeUOM, Graph ) ),
          ObservationTime=val(ObservationTimeValue)
        );
        (
          rdf( ObservationTimeNode,rdf:value,ObservationTimeValue,Graph ),
          ( rdf( ObservationTimeNode, aixm:uom, ObservationTimeUOM, Graph ); rdf( ObservationTimeNode, fixm:uom, ObservationTimeUOM, Graph ); rdf( ObservationTimeNode, plain:uom, ObservationTimeUOM, Graph ) ),
          ObservationTime=xval(ObservationTimeValue,ObservationTimeUOM)
        );
        (
          rdf( ObservationTimeNode,aixm:nilReason, ObservationTimeNilReason, Graph ),
          ObservationTime=nil(ObservationTimeNilReason)
        );
        (
          rdf( ObservationTimeNode,gml:indeterminatePosition, ObservationTimeIndeterminate, Graph ),
          ObservationTime=indeterminate(ObservationTimeIndeterminate)
        )
      )
  )
  ,(
    ( Depth='$null$',
      \+ rdf( SurfaceContamination,aixm:'depth',_Depth,Graph )
    );
  ( rdf( SurfaceContamination,aixm:'depth',DepthNode,Graph )),
      (
        (
          rdf(DepthNode,rdf:value,DepthValue,Graph),
         \+ ( rdf( DepthNode, aixm:uom, _DepthUOM, Graph ); rdf( DepthNode, fixm:uom, _DepthUOM, Graph ); rdf( DepthNode, plain:uom, _DepthUOM, Graph ) ),
          Depth=val(DepthValue)
        );
        (
          rdf( DepthNode,rdf:value,DepthValue,Graph ),
          ( rdf( DepthNode, aixm:uom, DepthUOM, Graph ); rdf( DepthNode, fixm:uom, DepthUOM, Graph ); rdf( DepthNode, plain:uom, DepthUOM, Graph ) ),
          Depth=xval(DepthValue,DepthUOM)
        );
        (
          rdf( DepthNode,aixm:nilReason, DepthNilReason, Graph ),
          Depth=nil(DepthNilReason)
        );
        (
          rdf( DepthNode,gml:indeterminatePosition, DepthIndeterminate, Graph ),
          Depth=indeterminate(DepthIndeterminate)
        )
      )
  )
  ,(
    ( FrictionCoefficient='$null$',
      \+ rdf( SurfaceContamination,aixm:'frictionCoefficient',_FrictionCoefficient,Graph )
    );
  ( rdf( SurfaceContamination,aixm:'frictionCoefficient',FrictionCoefficientNode,Graph )),
      (
        (
          rdf(FrictionCoefficientNode,rdf:value,FrictionCoefficientValue,Graph),
         \+ ( rdf( FrictionCoefficientNode, aixm:uom, _FrictionCoefficientUOM, Graph ); rdf( FrictionCoefficientNode, fixm:uom, _FrictionCoefficientUOM, Graph ); rdf( FrictionCoefficientNode, plain:uom, _FrictionCoefficientUOM, Graph ) ),
          FrictionCoefficient=val(FrictionCoefficientValue)
        );
        (
          rdf( FrictionCoefficientNode,rdf:value,FrictionCoefficientValue,Graph ),
          ( rdf( FrictionCoefficientNode, aixm:uom, FrictionCoefficientUOM, Graph ); rdf( FrictionCoefficientNode, fixm:uom, FrictionCoefficientUOM, Graph ); rdf( FrictionCoefficientNode, plain:uom, FrictionCoefficientUOM, Graph ) ),
          FrictionCoefficient=xval(FrictionCoefficientValue,FrictionCoefficientUOM)
        );
        (
          rdf( FrictionCoefficientNode,aixm:nilReason, FrictionCoefficientNilReason, Graph ),
          FrictionCoefficient=nil(FrictionCoefficientNilReason)
        );
        (
          rdf( FrictionCoefficientNode,gml:indeterminatePosition, FrictionCoefficientIndeterminate, Graph ),
          FrictionCoefficient=indeterminate(FrictionCoefficientIndeterminate)
        )
      )
  )
  ,(
    ( FrictionEstimation='$null$',
      \+ rdf( SurfaceContamination,aixm:'frictionEstimation',_FrictionEstimation,Graph )
    );
  ( rdf( SurfaceContamination,aixm:'frictionEstimation',FrictionEstimationNode,Graph )),
      (
        (
          rdf(FrictionEstimationNode,rdf:value,FrictionEstimationValue,Graph),
         \+ ( rdf( FrictionEstimationNode, aixm:uom, _FrictionEstimationUOM, Graph ); rdf( FrictionEstimationNode, fixm:uom, _FrictionEstimationUOM, Graph ); rdf( FrictionEstimationNode, plain:uom, _FrictionEstimationUOM, Graph ) ),
          FrictionEstimation=val(FrictionEstimationValue)
        );
        (
          rdf( FrictionEstimationNode,rdf:value,FrictionEstimationValue,Graph ),
          ( rdf( FrictionEstimationNode, aixm:uom, FrictionEstimationUOM, Graph ); rdf( FrictionEstimationNode, fixm:uom, FrictionEstimationUOM, Graph ); rdf( FrictionEstimationNode, plain:uom, FrictionEstimationUOM, Graph ) ),
          FrictionEstimation=xval(FrictionEstimationValue,FrictionEstimationUOM)
        );
        (
          rdf( FrictionEstimationNode,aixm:nilReason, FrictionEstimationNilReason, Graph ),
          FrictionEstimation=nil(FrictionEstimationNilReason)
        );
        (
          rdf( FrictionEstimationNode,gml:indeterminatePosition, FrictionEstimationIndeterminate, Graph ),
          FrictionEstimation=indeterminate(FrictionEstimationIndeterminate)
        )
      )
  )
  ,(
    ( FrictionDevice='$null$',
      \+ rdf( SurfaceContamination,aixm:'frictionDevice',_FrictionDevice,Graph )
    );
  ( rdf( SurfaceContamination,aixm:'frictionDevice',FrictionDeviceNode,Graph )),
      (
        (
          rdf(FrictionDeviceNode,rdf:value,FrictionDeviceValue,Graph),
         \+ ( rdf( FrictionDeviceNode, aixm:uom, _FrictionDeviceUOM, Graph ); rdf( FrictionDeviceNode, fixm:uom, _FrictionDeviceUOM, Graph ); rdf( FrictionDeviceNode, plain:uom, _FrictionDeviceUOM, Graph ) ),
          FrictionDevice=val(FrictionDeviceValue)
        );
        (
          rdf( FrictionDeviceNode,rdf:value,FrictionDeviceValue,Graph ),
          ( rdf( FrictionDeviceNode, aixm:uom, FrictionDeviceUOM, Graph ); rdf( FrictionDeviceNode, fixm:uom, FrictionDeviceUOM, Graph ); rdf( FrictionDeviceNode, plain:uom, FrictionDeviceUOM, Graph ) ),
          FrictionDevice=xval(FrictionDeviceValue,FrictionDeviceUOM)
        );
        (
          rdf( FrictionDeviceNode,aixm:nilReason, FrictionDeviceNilReason, Graph ),
          FrictionDevice=nil(FrictionDeviceNilReason)
        );
        (
          rdf( FrictionDeviceNode,gml:indeterminatePosition, FrictionDeviceIndeterminate, Graph ),
          FrictionDevice=indeterminate(FrictionDeviceIndeterminate)
        )
      )
  )
  ,(
    ( ObscuredLights='$null$',
      \+ rdf( SurfaceContamination,aixm:'obscuredLights',_ObscuredLights,Graph )
    );
  ( rdf( SurfaceContamination,aixm:'obscuredLights',ObscuredLightsNode,Graph )),
      (
        (
          rdf(ObscuredLightsNode,rdf:value,ObscuredLightsValue,Graph),
         \+ ( rdf( ObscuredLightsNode, aixm:uom, _ObscuredLightsUOM, Graph ); rdf( ObscuredLightsNode, fixm:uom, _ObscuredLightsUOM, Graph ); rdf( ObscuredLightsNode, plain:uom, _ObscuredLightsUOM, Graph ) ),
          ObscuredLights=val(ObscuredLightsValue)
        );
        (
          rdf( ObscuredLightsNode,rdf:value,ObscuredLightsValue,Graph ),
          ( rdf( ObscuredLightsNode, aixm:uom, ObscuredLightsUOM, Graph ); rdf( ObscuredLightsNode, fixm:uom, ObscuredLightsUOM, Graph ); rdf( ObscuredLightsNode, plain:uom, ObscuredLightsUOM, Graph ) ),
          ObscuredLights=xval(ObscuredLightsValue,ObscuredLightsUOM)
        );
        (
          rdf( ObscuredLightsNode,aixm:nilReason, ObscuredLightsNilReason, Graph ),
          ObscuredLights=nil(ObscuredLightsNilReason)
        );
        (
          rdf( ObscuredLightsNode,gml:indeterminatePosition, ObscuredLightsIndeterminate, Graph ),
          ObscuredLights=indeterminate(ObscuredLightsIndeterminate)
        )
      )
  )
  ,(
    ( FurtherClearanceTime='$null$',
      \+ rdf( SurfaceContamination,aixm:'furtherClearanceTime',_FurtherClearanceTime,Graph )
    );
  ( rdf( SurfaceContamination,aixm:'furtherClearanceTime',FurtherClearanceTimeNode,Graph )),
      (
        (
          rdf(FurtherClearanceTimeNode,rdf:value,FurtherClearanceTimeValue,Graph),
         \+ ( rdf( FurtherClearanceTimeNode, aixm:uom, _FurtherClearanceTimeUOM, Graph ); rdf( FurtherClearanceTimeNode, fixm:uom, _FurtherClearanceTimeUOM, Graph ); rdf( FurtherClearanceTimeNode, plain:uom, _FurtherClearanceTimeUOM, Graph ) ),
          FurtherClearanceTime=val(FurtherClearanceTimeValue)
        );
        (
          rdf( FurtherClearanceTimeNode,rdf:value,FurtherClearanceTimeValue,Graph ),
          ( rdf( FurtherClearanceTimeNode, aixm:uom, FurtherClearanceTimeUOM, Graph ); rdf( FurtherClearanceTimeNode, fixm:uom, FurtherClearanceTimeUOM, Graph ); rdf( FurtherClearanceTimeNode, plain:uom, FurtherClearanceTimeUOM, Graph ) ),
          FurtherClearanceTime=xval(FurtherClearanceTimeValue,FurtherClearanceTimeUOM)
        );
        (
          rdf( FurtherClearanceTimeNode,aixm:nilReason, FurtherClearanceTimeNilReason, Graph ),
          FurtherClearanceTime=nil(FurtherClearanceTimeNilReason)
        );
        (
          rdf( FurtherClearanceTimeNode,gml:indeterminatePosition, FurtherClearanceTimeIndeterminate, Graph ),
          FurtherClearanceTime=indeterminate(FurtherClearanceTimeIndeterminate)
        )
      )
  )
  ,(
    ( FurtherTotalClearance='$null$',
      \+ rdf( SurfaceContamination,aixm:'furtherTotalClearance',_FurtherTotalClearance,Graph )
    );
  ( rdf( SurfaceContamination,aixm:'furtherTotalClearance',FurtherTotalClearanceNode,Graph )),
      (
        (
          rdf(FurtherTotalClearanceNode,rdf:value,FurtherTotalClearanceValue,Graph),
         \+ ( rdf( FurtherTotalClearanceNode, aixm:uom, _FurtherTotalClearanceUOM, Graph ); rdf( FurtherTotalClearanceNode, fixm:uom, _FurtherTotalClearanceUOM, Graph ); rdf( FurtherTotalClearanceNode, plain:uom, _FurtherTotalClearanceUOM, Graph ) ),
          FurtherTotalClearance=val(FurtherTotalClearanceValue)
        );
        (
          rdf( FurtherTotalClearanceNode,rdf:value,FurtherTotalClearanceValue,Graph ),
          ( rdf( FurtherTotalClearanceNode, aixm:uom, FurtherTotalClearanceUOM, Graph ); rdf( FurtherTotalClearanceNode, fixm:uom, FurtherTotalClearanceUOM, Graph ); rdf( FurtherTotalClearanceNode, plain:uom, FurtherTotalClearanceUOM, Graph ) ),
          FurtherTotalClearance=xval(FurtherTotalClearanceValue,FurtherTotalClearanceUOM)
        );
        (
          rdf( FurtherTotalClearanceNode,aixm:nilReason, FurtherTotalClearanceNilReason, Graph ),
          FurtherTotalClearance=nil(FurtherTotalClearanceNilReason)
        );
        (
          rdf( FurtherTotalClearanceNode,gml:indeterminatePosition, FurtherTotalClearanceIndeterminate, Graph ),
          FurtherTotalClearance=indeterminate(FurtherTotalClearanceIndeterminate)
        )
      )
  )
  ,(
    ( NextObservationTime='$null$',
      \+ rdf( SurfaceContamination,aixm:'nextObservationTime',_NextObservationTime,Graph )
    );
  ( rdf( SurfaceContamination,aixm:'nextObservationTime',NextObservationTimeNode,Graph )),
      (
        (
          rdf(NextObservationTimeNode,rdf:value,NextObservationTimeValue,Graph),
         \+ ( rdf( NextObservationTimeNode, aixm:uom, _NextObservationTimeUOM, Graph ); rdf( NextObservationTimeNode, fixm:uom, _NextObservationTimeUOM, Graph ); rdf( NextObservationTimeNode, plain:uom, _NextObservationTimeUOM, Graph ) ),
          NextObservationTime=val(NextObservationTimeValue)
        );
        (
          rdf( NextObservationTimeNode,rdf:value,NextObservationTimeValue,Graph ),
          ( rdf( NextObservationTimeNode, aixm:uom, NextObservationTimeUOM, Graph ); rdf( NextObservationTimeNode, fixm:uom, NextObservationTimeUOM, Graph ); rdf( NextObservationTimeNode, plain:uom, NextObservationTimeUOM, Graph ) ),
          NextObservationTime=xval(NextObservationTimeValue,NextObservationTimeUOM)
        );
        (
          rdf( NextObservationTimeNode,aixm:nilReason, NextObservationTimeNilReason, Graph ),
          NextObservationTime=nil(NextObservationTimeNilReason)
        );
        (
          rdf( NextObservationTimeNode,gml:indeterminatePosition, NextObservationTimeIndeterminate, Graph ),
          NextObservationTime=indeterminate(NextObservationTimeIndeterminate)
        )
      )
  )
  ,(
    ( Proportion='$null$',
      \+ rdf( SurfaceContamination,aixm:'proportion',_Proportion,Graph )
    );
  ( rdf( SurfaceContamination,aixm:'proportion',ProportionNode,Graph )),
      (
        (
          rdf(ProportionNode,rdf:value,ProportionValue,Graph),
         \+ ( rdf( ProportionNode, aixm:uom, _ProportionUOM, Graph ); rdf( ProportionNode, fixm:uom, _ProportionUOM, Graph ); rdf( ProportionNode, plain:uom, _ProportionUOM, Graph ) ),
          Proportion=val(ProportionValue)
        );
        (
          rdf( ProportionNode,rdf:value,ProportionValue,Graph ),
          ( rdf( ProportionNode, aixm:uom, ProportionUOM, Graph ); rdf( ProportionNode, fixm:uom, ProportionUOM, Graph ); rdf( ProportionNode, plain:uom, ProportionUOM, Graph ) ),
          Proportion=xval(ProportionValue,ProportionUOM)
        );
        (
          rdf( ProportionNode,aixm:nilReason, ProportionNilReason, Graph ),
          Proportion=nil(ProportionNilReason)
        );
        (
          rdf( ProportionNode,gml:indeterminatePosition, ProportionIndeterminate, Graph ),
          Proportion=indeterminate(ProportionIndeterminate)
        )
      )
  )
  ,findall(A, rdf(SurfaceContamination,aixm:'criticalRidge',A,Graph), CriticalRidge)
  ,findall(A, rdf(SurfaceContamination,aixm:'annotation',A,Graph), Annotation)
  ,findall(A, rdf(SurfaceContamination,aixm:'layer',A,Graph), Layer) .

fixm_MeteorologicalData(Graph, MeteorologicalData, Temperature, WindDirection, WindSpeed) :-
  rdf(MeteorologicalData,rdf:type,fixm:'MeteorologicalData',Graph)
  ,(
    ( Temperature='$null$',
      \+ rdf( MeteorologicalData,fixm:'temperature',_Temperature,Graph )
    );
  ( rdf( MeteorologicalData,fixm:'temperature',TemperatureNode,Graph )),
      (
        (
          rdf(TemperatureNode,rdf:value,TemperatureValue,Graph),
         \+ ( rdf( TemperatureNode, aixm:uom, _TemperatureUOM, Graph ); rdf( TemperatureNode, fixm:uom, _TemperatureUOM, Graph ); rdf( TemperatureNode, plain:uom, _TemperatureUOM, Graph ) ),
          Temperature=val(TemperatureValue)
        );
        (
          rdf( TemperatureNode,rdf:value,TemperatureValue,Graph ),
          ( rdf( TemperatureNode, aixm:uom, TemperatureUOM, Graph ); rdf( TemperatureNode, fixm:uom, TemperatureUOM, Graph ); rdf( TemperatureNode, plain:uom, TemperatureUOM, Graph ) ),
          Temperature=xval(TemperatureValue,TemperatureUOM)
        );
        (
          rdf( TemperatureNode,aixm:nilReason, TemperatureNilReason, Graph ),
          Temperature=nil(TemperatureNilReason)
        );
        (
          rdf( TemperatureNode,gml:indeterminatePosition, TemperatureIndeterminate, Graph ),
          Temperature=indeterminate(TemperatureIndeterminate)
        )
      )
  )
  ,(
    ( WindDirection='$null$',
      \+ rdf( MeteorologicalData,fixm:'windDirection',_WindDirection,Graph )
    );
  ( rdf( MeteorologicalData,fixm:'windDirection',WindDirectionNode,Graph )),
      (
        (
          rdf(WindDirectionNode,rdf:value,WindDirectionValue,Graph),
         \+ ( rdf( WindDirectionNode, aixm:uom, _WindDirectionUOM, Graph ); rdf( WindDirectionNode, fixm:uom, _WindDirectionUOM, Graph ); rdf( WindDirectionNode, plain:uom, _WindDirectionUOM, Graph ) ),
          WindDirection=val(WindDirectionValue)
        );
        (
          rdf( WindDirectionNode,rdf:value,WindDirectionValue,Graph ),
          ( rdf( WindDirectionNode, aixm:uom, WindDirectionUOM, Graph ); rdf( WindDirectionNode, fixm:uom, WindDirectionUOM, Graph ); rdf( WindDirectionNode, plain:uom, WindDirectionUOM, Graph ) ),
          WindDirection=xval(WindDirectionValue,WindDirectionUOM)
        );
        (
          rdf( WindDirectionNode,aixm:nilReason, WindDirectionNilReason, Graph ),
          WindDirection=nil(WindDirectionNilReason)
        );
        (
          rdf( WindDirectionNode,gml:indeterminatePosition, WindDirectionIndeterminate, Graph ),
          WindDirection=indeterminate(WindDirectionIndeterminate)
        )
      )
  )
  ,(
    ( WindSpeed='$null$',
      \+ rdf( MeteorologicalData,fixm:'windSpeed',_WindSpeed,Graph )
    );
  ( rdf( MeteorologicalData,fixm:'windSpeed',WindSpeedNode,Graph )),
      (
        (
          rdf(WindSpeedNode,rdf:value,WindSpeedValue,Graph),
         \+ ( rdf( WindSpeedNode, aixm:uom, _WindSpeedUOM, Graph ); rdf( WindSpeedNode, fixm:uom, _WindSpeedUOM, Graph ); rdf( WindSpeedNode, plain:uom, _WindSpeedUOM, Graph ) ),
          WindSpeed=val(WindSpeedValue)
        );
        (
          rdf( WindSpeedNode,rdf:value,WindSpeedValue,Graph ),
          ( rdf( WindSpeedNode, aixm:uom, WindSpeedUOM, Graph ); rdf( WindSpeedNode, fixm:uom, WindSpeedUOM, Graph ); rdf( WindSpeedNode, plain:uom, WindSpeedUOM, Graph ) ),
          WindSpeed=xval(WindSpeedValue,WindSpeedUOM)
        );
        (
          rdf( WindSpeedNode,aixm:nilReason, WindSpeedNilReason, Graph ),
          WindSpeed=nil(WindSpeedNilReason)
        );
        (
          rdf( WindSpeedNode,gml:indeterminatePosition, WindSpeedIndeterminate, Graph ),
          WindSpeed=indeterminate(WindSpeedIndeterminate)
        )
      )
  ) .

aixm_OrganisationAuthority(Graph, OrganisationAuthority, TimeSlice) :-
  rdf(OrganisationAuthority,rdf:type,aixm:'OrganisationAuthority',Graph)
  ,findall(A, rdf(OrganisationAuthority,aixm:'timeSlice',A,Graph), TimeSlice) .

fixm_TelephoneContact(Graph, TelephoneContact, Voice, Facimile) :-
  rdf(TelephoneContact,rdf:type,fixm:'TelephoneContact',Graph)
  ,(
    ( Voice='$null$',
      \+ rdf( TelephoneContact,fixm:'voice',_Voice,Graph )
    );
  ( rdf( TelephoneContact,fixm:'voice',VoiceNode,Graph )),
      (
        (
          rdf(VoiceNode,rdf:value,VoiceValue,Graph),
         \+ ( rdf( VoiceNode, aixm:uom, _VoiceUOM, Graph ); rdf( VoiceNode, fixm:uom, _VoiceUOM, Graph ); rdf( VoiceNode, plain:uom, _VoiceUOM, Graph ) ),
          Voice=val(VoiceValue)
        );
        (
          rdf( VoiceNode,rdf:value,VoiceValue,Graph ),
          ( rdf( VoiceNode, aixm:uom, VoiceUOM, Graph ); rdf( VoiceNode, fixm:uom, VoiceUOM, Graph ); rdf( VoiceNode, plain:uom, VoiceUOM, Graph ) ),
          Voice=xval(VoiceValue,VoiceUOM)
        );
        (
          rdf( VoiceNode,aixm:nilReason, VoiceNilReason, Graph ),
          Voice=nil(VoiceNilReason)
        );
        (
          rdf( VoiceNode,gml:indeterminatePosition, VoiceIndeterminate, Graph ),
          Voice=indeterminate(VoiceIndeterminate)
        )
      )
  )
  ,(
    ( Facimile='$null$',
      \+ rdf( TelephoneContact,fixm:'facimile',_Facimile,Graph )
    );
  ( rdf( TelephoneContact,fixm:'facimile',FacimileNode,Graph )),
      (
        (
          rdf(FacimileNode,rdf:value,FacimileValue,Graph),
         \+ ( rdf( FacimileNode, aixm:uom, _FacimileUOM, Graph ); rdf( FacimileNode, fixm:uom, _FacimileUOM, Graph ); rdf( FacimileNode, plain:uom, _FacimileUOM, Graph ) ),
          Facimile=val(FacimileValue)
        );
        (
          rdf( FacimileNode,rdf:value,FacimileValue,Graph ),
          ( rdf( FacimileNode, aixm:uom, FacimileUOM, Graph ); rdf( FacimileNode, fixm:uom, FacimileUOM, Graph ); rdf( FacimileNode, plain:uom, FacimileUOM, Graph ) ),
          Facimile=xval(FacimileValue,FacimileUOM)
        );
        (
          rdf( FacimileNode,aixm:nilReason, FacimileNilReason, Graph ),
          Facimile=nil(FacimileNilReason)
        );
        (
          rdf( FacimileNode,gml:indeterminatePosition, FacimileIndeterminate, Graph ),
          Facimile=indeterminate(FacimileIndeterminate)
        )
      )
  ) .

fixm_ShippingInformation(Graph, ShippingInformation, AerodromeOfLoading, AerodromeOfUnloading, DangerousGoodsScreeningLocation, DepartureCountry, DestinationCountry, OriginCountry, ShipmentAuthorizations, SubsidiaryHazardClassAndDivision, SupplementaryInformation, TransferAerodromes, DeclarationText, Consignee, Shipper) :-
  rdf(ShippingInformation,rdf:type,fixm:'ShippingInformation',Graph)
  ,( ( AerodromeOfLoading='$null$',
    \+ rdf( ShippingInformation,fixm:'aerodromeOfLoading', _AerodromeOfLoading, Graph  )
   ; rdf(ShippingInformation,fixm:'aerodromeOfLoading', AerodromeOfLoading, Graph ) )
  )
  ,( ( AerodromeOfUnloading='$null$',
    \+ rdf( ShippingInformation,fixm:'aerodromeOfUnloading', _AerodromeOfUnloading, Graph  )
   ; rdf(ShippingInformation,fixm:'aerodromeOfUnloading', AerodromeOfUnloading, Graph ) )
  )
  ,(
    ( DangerousGoodsScreeningLocation='$null$',
      \+ rdf( ShippingInformation,fixm:'dangerousGoodsScreeningLocation',_DangerousGoodsScreeningLocation,Graph )
    );
  ( rdf( ShippingInformation,fixm:'dangerousGoodsScreeningLocation',DangerousGoodsScreeningLocationNode,Graph )),
      (
        (
          rdf(DangerousGoodsScreeningLocationNode,rdf:value,DangerousGoodsScreeningLocationValue,Graph),
         \+ ( rdf( DangerousGoodsScreeningLocationNode, aixm:uom, _DangerousGoodsScreeningLocationUOM, Graph ); rdf( DangerousGoodsScreeningLocationNode, fixm:uom, _DangerousGoodsScreeningLocationUOM, Graph ); rdf( DangerousGoodsScreeningLocationNode, plain:uom, _DangerousGoodsScreeningLocationUOM, Graph ) ),
          DangerousGoodsScreeningLocation=val(DangerousGoodsScreeningLocationValue)
        );
        (
          rdf( DangerousGoodsScreeningLocationNode,rdf:value,DangerousGoodsScreeningLocationValue,Graph ),
          ( rdf( DangerousGoodsScreeningLocationNode, aixm:uom, DangerousGoodsScreeningLocationUOM, Graph ); rdf( DangerousGoodsScreeningLocationNode, fixm:uom, DangerousGoodsScreeningLocationUOM, Graph ); rdf( DangerousGoodsScreeningLocationNode, plain:uom, DangerousGoodsScreeningLocationUOM, Graph ) ),
          DangerousGoodsScreeningLocation=xval(DangerousGoodsScreeningLocationValue,DangerousGoodsScreeningLocationUOM)
        );
        (
          rdf( DangerousGoodsScreeningLocationNode,aixm:nilReason, DangerousGoodsScreeningLocationNilReason, Graph ),
          DangerousGoodsScreeningLocation=nil(DangerousGoodsScreeningLocationNilReason)
        );
        (
          rdf( DangerousGoodsScreeningLocationNode,gml:indeterminatePosition, DangerousGoodsScreeningLocationIndeterminate, Graph ),
          DangerousGoodsScreeningLocation=indeterminate(DangerousGoodsScreeningLocationIndeterminate)
        )
      )
  )
  ,(
    ( DepartureCountry='$null$',
      \+ rdf( ShippingInformation,fixm:'departureCountry',_DepartureCountry,Graph )
    );
  ( rdf( ShippingInformation,fixm:'departureCountry',DepartureCountryNode,Graph )),
      (
        (
          rdf(DepartureCountryNode,rdf:value,DepartureCountryValue,Graph),
         \+ ( rdf( DepartureCountryNode, aixm:uom, _DepartureCountryUOM, Graph ); rdf( DepartureCountryNode, fixm:uom, _DepartureCountryUOM, Graph ); rdf( DepartureCountryNode, plain:uom, _DepartureCountryUOM, Graph ) ),
          DepartureCountry=val(DepartureCountryValue)
        );
        (
          rdf( DepartureCountryNode,rdf:value,DepartureCountryValue,Graph ),
          ( rdf( DepartureCountryNode, aixm:uom, DepartureCountryUOM, Graph ); rdf( DepartureCountryNode, fixm:uom, DepartureCountryUOM, Graph ); rdf( DepartureCountryNode, plain:uom, DepartureCountryUOM, Graph ) ),
          DepartureCountry=xval(DepartureCountryValue,DepartureCountryUOM)
        );
        (
          rdf( DepartureCountryNode,aixm:nilReason, DepartureCountryNilReason, Graph ),
          DepartureCountry=nil(DepartureCountryNilReason)
        );
        (
          rdf( DepartureCountryNode,gml:indeterminatePosition, DepartureCountryIndeterminate, Graph ),
          DepartureCountry=indeterminate(DepartureCountryIndeterminate)
        )
      )
  )
  ,(
    ( DestinationCountry='$null$',
      \+ rdf( ShippingInformation,fixm:'destinationCountry',_DestinationCountry,Graph )
    );
  ( rdf( ShippingInformation,fixm:'destinationCountry',DestinationCountryNode,Graph )),
      (
        (
          rdf(DestinationCountryNode,rdf:value,DestinationCountryValue,Graph),
         \+ ( rdf( DestinationCountryNode, aixm:uom, _DestinationCountryUOM, Graph ); rdf( DestinationCountryNode, fixm:uom, _DestinationCountryUOM, Graph ); rdf( DestinationCountryNode, plain:uom, _DestinationCountryUOM, Graph ) ),
          DestinationCountry=val(DestinationCountryValue)
        );
        (
          rdf( DestinationCountryNode,rdf:value,DestinationCountryValue,Graph ),
          ( rdf( DestinationCountryNode, aixm:uom, DestinationCountryUOM, Graph ); rdf( DestinationCountryNode, fixm:uom, DestinationCountryUOM, Graph ); rdf( DestinationCountryNode, plain:uom, DestinationCountryUOM, Graph ) ),
          DestinationCountry=xval(DestinationCountryValue,DestinationCountryUOM)
        );
        (
          rdf( DestinationCountryNode,aixm:nilReason, DestinationCountryNilReason, Graph ),
          DestinationCountry=nil(DestinationCountryNilReason)
        );
        (
          rdf( DestinationCountryNode,gml:indeterminatePosition, DestinationCountryIndeterminate, Graph ),
          DestinationCountry=indeterminate(DestinationCountryIndeterminate)
        )
      )
  )
  ,(
    ( OriginCountry='$null$',
      \+ rdf( ShippingInformation,fixm:'originCountry',_OriginCountry,Graph )
    );
  ( rdf( ShippingInformation,fixm:'originCountry',OriginCountryNode,Graph )),
      (
        (
          rdf(OriginCountryNode,rdf:value,OriginCountryValue,Graph),
         \+ ( rdf( OriginCountryNode, aixm:uom, _OriginCountryUOM, Graph ); rdf( OriginCountryNode, fixm:uom, _OriginCountryUOM, Graph ); rdf( OriginCountryNode, plain:uom, _OriginCountryUOM, Graph ) ),
          OriginCountry=val(OriginCountryValue)
        );
        (
          rdf( OriginCountryNode,rdf:value,OriginCountryValue,Graph ),
          ( rdf( OriginCountryNode, aixm:uom, OriginCountryUOM, Graph ); rdf( OriginCountryNode, fixm:uom, OriginCountryUOM, Graph ); rdf( OriginCountryNode, plain:uom, OriginCountryUOM, Graph ) ),
          OriginCountry=xval(OriginCountryValue,OriginCountryUOM)
        );
        (
          rdf( OriginCountryNode,aixm:nilReason, OriginCountryNilReason, Graph ),
          OriginCountry=nil(OriginCountryNilReason)
        );
        (
          rdf( OriginCountryNode,gml:indeterminatePosition, OriginCountryIndeterminate, Graph ),
          OriginCountry=indeterminate(OriginCountryIndeterminate)
        )
      )
  )
  ,(
    ( ShipmentAuthorizations='$null$',
      \+ rdf( ShippingInformation,fixm:'shipmentAuthorizations',_ShipmentAuthorizations,Graph )
    );
  ( rdf( ShippingInformation,fixm:'shipmentAuthorizations',ShipmentAuthorizationsNode,Graph )),
      (
        (
          rdf(ShipmentAuthorizationsNode,rdf:value,ShipmentAuthorizationsValue,Graph),
         \+ ( rdf( ShipmentAuthorizationsNode, aixm:uom, _ShipmentAuthorizationsUOM, Graph ); rdf( ShipmentAuthorizationsNode, fixm:uom, _ShipmentAuthorizationsUOM, Graph ); rdf( ShipmentAuthorizationsNode, plain:uom, _ShipmentAuthorizationsUOM, Graph ) ),
          ShipmentAuthorizations=val(ShipmentAuthorizationsValue)
        );
        (
          rdf( ShipmentAuthorizationsNode,rdf:value,ShipmentAuthorizationsValue,Graph ),
          ( rdf( ShipmentAuthorizationsNode, aixm:uom, ShipmentAuthorizationsUOM, Graph ); rdf( ShipmentAuthorizationsNode, fixm:uom, ShipmentAuthorizationsUOM, Graph ); rdf( ShipmentAuthorizationsNode, plain:uom, ShipmentAuthorizationsUOM, Graph ) ),
          ShipmentAuthorizations=xval(ShipmentAuthorizationsValue,ShipmentAuthorizationsUOM)
        );
        (
          rdf( ShipmentAuthorizationsNode,aixm:nilReason, ShipmentAuthorizationsNilReason, Graph ),
          ShipmentAuthorizations=nil(ShipmentAuthorizationsNilReason)
        );
        (
          rdf( ShipmentAuthorizationsNode,gml:indeterminatePosition, ShipmentAuthorizationsIndeterminate, Graph ),
          ShipmentAuthorizations=indeterminate(ShipmentAuthorizationsIndeterminate)
        )
      )
  )
  ,(
    ( SubsidiaryHazardClassAndDivision='$null$',
      \+ rdf( ShippingInformation,fixm:'subsidiaryHazardClassAndDivision',_SubsidiaryHazardClassAndDivision,Graph )
    );
  ( rdf( ShippingInformation,fixm:'subsidiaryHazardClassAndDivision',SubsidiaryHazardClassAndDivisionNode,Graph )),
      (
        (
          rdf(SubsidiaryHazardClassAndDivisionNode,rdf:value,SubsidiaryHazardClassAndDivisionValue,Graph),
         \+ ( rdf( SubsidiaryHazardClassAndDivisionNode, aixm:uom, _SubsidiaryHazardClassAndDivisionUOM, Graph ); rdf( SubsidiaryHazardClassAndDivisionNode, fixm:uom, _SubsidiaryHazardClassAndDivisionUOM, Graph ); rdf( SubsidiaryHazardClassAndDivisionNode, plain:uom, _SubsidiaryHazardClassAndDivisionUOM, Graph ) ),
          SubsidiaryHazardClassAndDivision=val(SubsidiaryHazardClassAndDivisionValue)
        );
        (
          rdf( SubsidiaryHazardClassAndDivisionNode,rdf:value,SubsidiaryHazardClassAndDivisionValue,Graph ),
          ( rdf( SubsidiaryHazardClassAndDivisionNode, aixm:uom, SubsidiaryHazardClassAndDivisionUOM, Graph ); rdf( SubsidiaryHazardClassAndDivisionNode, fixm:uom, SubsidiaryHazardClassAndDivisionUOM, Graph ); rdf( SubsidiaryHazardClassAndDivisionNode, plain:uom, SubsidiaryHazardClassAndDivisionUOM, Graph ) ),
          SubsidiaryHazardClassAndDivision=xval(SubsidiaryHazardClassAndDivisionValue,SubsidiaryHazardClassAndDivisionUOM)
        );
        (
          rdf( SubsidiaryHazardClassAndDivisionNode,aixm:nilReason, SubsidiaryHazardClassAndDivisionNilReason, Graph ),
          SubsidiaryHazardClassAndDivision=nil(SubsidiaryHazardClassAndDivisionNilReason)
        );
        (
          rdf( SubsidiaryHazardClassAndDivisionNode,gml:indeterminatePosition, SubsidiaryHazardClassAndDivisionIndeterminate, Graph ),
          SubsidiaryHazardClassAndDivision=indeterminate(SubsidiaryHazardClassAndDivisionIndeterminate)
        )
      )
  )
  ,(
    ( SupplementaryInformation='$null$',
      \+ rdf( ShippingInformation,fixm:'supplementaryInformation',_SupplementaryInformation,Graph )
    );
  ( rdf( ShippingInformation,fixm:'supplementaryInformation',SupplementaryInformationNode,Graph )),
      (
        (
          rdf(SupplementaryInformationNode,rdf:value,SupplementaryInformationValue,Graph),
         \+ ( rdf( SupplementaryInformationNode, aixm:uom, _SupplementaryInformationUOM, Graph ); rdf( SupplementaryInformationNode, fixm:uom, _SupplementaryInformationUOM, Graph ); rdf( SupplementaryInformationNode, plain:uom, _SupplementaryInformationUOM, Graph ) ),
          SupplementaryInformation=val(SupplementaryInformationValue)
        );
        (
          rdf( SupplementaryInformationNode,rdf:value,SupplementaryInformationValue,Graph ),
          ( rdf( SupplementaryInformationNode, aixm:uom, SupplementaryInformationUOM, Graph ); rdf( SupplementaryInformationNode, fixm:uom, SupplementaryInformationUOM, Graph ); rdf( SupplementaryInformationNode, plain:uom, SupplementaryInformationUOM, Graph ) ),
          SupplementaryInformation=xval(SupplementaryInformationValue,SupplementaryInformationUOM)
        );
        (
          rdf( SupplementaryInformationNode,aixm:nilReason, SupplementaryInformationNilReason, Graph ),
          SupplementaryInformation=nil(SupplementaryInformationNilReason)
        );
        (
          rdf( SupplementaryInformationNode,gml:indeterminatePosition, SupplementaryInformationIndeterminate, Graph ),
          SupplementaryInformation=indeterminate(SupplementaryInformationIndeterminate)
        )
      )
  )
  ,findall(A, rdf(ShippingInformation,fixm:'transferAerodromes',A,Graph), TransferAerodromes)
  ,( ( DeclarationText='$null$',
    \+ rdf( ShippingInformation,fixm:'declarationText', _DeclarationText, Graph  )
   ; rdf(ShippingInformation,fixm:'declarationText', DeclarationText, Graph ) )
  )
  ,( ( Consignee='$null$',
    \+ rdf( ShippingInformation,fixm:'consignee', _Consignee, Graph  )
   ; rdf(ShippingInformation,fixm:'consignee', Consignee, Graph ) )
  )
  ,( ( Shipper='$null$',
    \+ rdf( ShippingInformation,fixm:'shipper', _Shipper, Graph  )
   ; rdf(ShippingInformation,fixm:'shipper', Shipper, Graph ) )
  ) .

aixm_AirportHeliportContamination(Graph, AirportHeliportContamination) :-
  rdf(AirportHeliportContamination,rdf:type,aixm:'AirportHeliportContamination',Graph) .

fixm_OtherInformation(Graph, OtherInformation, ReplacementFlightPlanIndicator, RunwayVisualRange) :-
  rdf(OtherInformation,rdf:type,fixm:'OtherInformation',Graph)
  ,(
    ( ReplacementFlightPlanIndicator='$null$',
      \+ rdf( OtherInformation,fixm:'replacementFlightPlanIndicator',_ReplacementFlightPlanIndicator,Graph )
    );
  ( rdf( OtherInformation,fixm:'replacementFlightPlanIndicator',ReplacementFlightPlanIndicatorNode,Graph )),
      (
        (
          rdf(ReplacementFlightPlanIndicatorNode,rdf:value,ReplacementFlightPlanIndicatorValue,Graph),
         \+ ( rdf( ReplacementFlightPlanIndicatorNode, aixm:uom, _ReplacementFlightPlanIndicatorUOM, Graph ); rdf( ReplacementFlightPlanIndicatorNode, fixm:uom, _ReplacementFlightPlanIndicatorUOM, Graph ); rdf( ReplacementFlightPlanIndicatorNode, plain:uom, _ReplacementFlightPlanIndicatorUOM, Graph ) ),
          ReplacementFlightPlanIndicator=val(ReplacementFlightPlanIndicatorValue)
        );
        (
          rdf( ReplacementFlightPlanIndicatorNode,rdf:value,ReplacementFlightPlanIndicatorValue,Graph ),
          ( rdf( ReplacementFlightPlanIndicatorNode, aixm:uom, ReplacementFlightPlanIndicatorUOM, Graph ); rdf( ReplacementFlightPlanIndicatorNode, fixm:uom, ReplacementFlightPlanIndicatorUOM, Graph ); rdf( ReplacementFlightPlanIndicatorNode, plain:uom, ReplacementFlightPlanIndicatorUOM, Graph ) ),
          ReplacementFlightPlanIndicator=xval(ReplacementFlightPlanIndicatorValue,ReplacementFlightPlanIndicatorUOM)
        );
        (
          rdf( ReplacementFlightPlanIndicatorNode,aixm:nilReason, ReplacementFlightPlanIndicatorNilReason, Graph ),
          ReplacementFlightPlanIndicator=nil(ReplacementFlightPlanIndicatorNilReason)
        );
        (
          rdf( ReplacementFlightPlanIndicatorNode,gml:indeterminatePosition, ReplacementFlightPlanIndicatorIndeterminate, Graph ),
          ReplacementFlightPlanIndicator=indeterminate(ReplacementFlightPlanIndicatorIndeterminate)
        )
      )
  )
  ,(
    ( RunwayVisualRange='$null$',
      \+ rdf( OtherInformation,fixm:'runwayVisualRange',_RunwayVisualRange,Graph )
    );
  ( rdf( OtherInformation,fixm:'runwayVisualRange',RunwayVisualRangeNode,Graph )),
      (
        (
          rdf(RunwayVisualRangeNode,rdf:value,RunwayVisualRangeValue,Graph),
         \+ ( rdf( RunwayVisualRangeNode, aixm:uom, _RunwayVisualRangeUOM, Graph ); rdf( RunwayVisualRangeNode, fixm:uom, _RunwayVisualRangeUOM, Graph ); rdf( RunwayVisualRangeNode, plain:uom, _RunwayVisualRangeUOM, Graph ) ),
          RunwayVisualRange=val(RunwayVisualRangeValue)
        );
        (
          rdf( RunwayVisualRangeNode,rdf:value,RunwayVisualRangeValue,Graph ),
          ( rdf( RunwayVisualRangeNode, aixm:uom, RunwayVisualRangeUOM, Graph ); rdf( RunwayVisualRangeNode, fixm:uom, RunwayVisualRangeUOM, Graph ); rdf( RunwayVisualRangeNode, plain:uom, RunwayVisualRangeUOM, Graph ) ),
          RunwayVisualRange=xval(RunwayVisualRangeValue,RunwayVisualRangeUOM)
        );
        (
          rdf( RunwayVisualRangeNode,aixm:nilReason, RunwayVisualRangeNilReason, Graph ),
          RunwayVisualRange=nil(RunwayVisualRangeNilReason)
        );
        (
          rdf( RunwayVisualRangeNode,gml:indeterminatePosition, RunwayVisualRangeIndeterminate, Graph ),
          RunwayVisualRange=indeterminate(RunwayVisualRangeIndeterminate)
        )
      )
  ) .

fixm_DinghyColour(Graph, DinghyColour) :-
  rdf(DinghyColour,rdf:type,fixm:'DinghyColour',Graph) .

fixm_CpdlcConnection(Graph, CpdlcConnection, ReceivingUnitFrequency, AtnLogonParameters, SendCpldcIndicator, ConnectionStatus, FrequencyUsage, Fans1ALogonParameters) :-
  rdf(CpdlcConnection,rdf:type,fixm:'CpdlcConnection',Graph)
  ,(
    ( ReceivingUnitFrequency='$null$',
      \+ rdf( CpdlcConnection,fixm:'receivingUnitFrequency',_ReceivingUnitFrequency,Graph )
    );
  ( rdf( CpdlcConnection,fixm:'receivingUnitFrequency',ReceivingUnitFrequencyNode,Graph )),
      (
        (
          rdf(ReceivingUnitFrequencyNode,rdf:value,ReceivingUnitFrequencyValue,Graph),
         \+ ( rdf( ReceivingUnitFrequencyNode, aixm:uom, _ReceivingUnitFrequencyUOM, Graph ); rdf( ReceivingUnitFrequencyNode, fixm:uom, _ReceivingUnitFrequencyUOM, Graph ); rdf( ReceivingUnitFrequencyNode, plain:uom, _ReceivingUnitFrequencyUOM, Graph ) ),
          ReceivingUnitFrequency=val(ReceivingUnitFrequencyValue)
        );
        (
          rdf( ReceivingUnitFrequencyNode,rdf:value,ReceivingUnitFrequencyValue,Graph ),
          ( rdf( ReceivingUnitFrequencyNode, aixm:uom, ReceivingUnitFrequencyUOM, Graph ); rdf( ReceivingUnitFrequencyNode, fixm:uom, ReceivingUnitFrequencyUOM, Graph ); rdf( ReceivingUnitFrequencyNode, plain:uom, ReceivingUnitFrequencyUOM, Graph ) ),
          ReceivingUnitFrequency=xval(ReceivingUnitFrequencyValue,ReceivingUnitFrequencyUOM)
        );
        (
          rdf( ReceivingUnitFrequencyNode,aixm:nilReason, ReceivingUnitFrequencyNilReason, Graph ),
          ReceivingUnitFrequency=nil(ReceivingUnitFrequencyNilReason)
        );
        (
          rdf( ReceivingUnitFrequencyNode,gml:indeterminatePosition, ReceivingUnitFrequencyIndeterminate, Graph ),
          ReceivingUnitFrequency=indeterminate(ReceivingUnitFrequencyIndeterminate)
        )
      )
  )
  ,(
    ( AtnLogonParameters='$null$',
      \+ rdf( CpdlcConnection,fixm:'atnLogonParameters',_AtnLogonParameters,Graph )
    );
  ( rdf( CpdlcConnection,fixm:'atnLogonParameters',AtnLogonParametersNode,Graph )),
      (
        (
          rdf(AtnLogonParametersNode,rdf:value,AtnLogonParametersValue,Graph),
         \+ ( rdf( AtnLogonParametersNode, aixm:uom, _AtnLogonParametersUOM, Graph ); rdf( AtnLogonParametersNode, fixm:uom, _AtnLogonParametersUOM, Graph ); rdf( AtnLogonParametersNode, plain:uom, _AtnLogonParametersUOM, Graph ) ),
          AtnLogonParameters=val(AtnLogonParametersValue)
        );
        (
          rdf( AtnLogonParametersNode,rdf:value,AtnLogonParametersValue,Graph ),
          ( rdf( AtnLogonParametersNode, aixm:uom, AtnLogonParametersUOM, Graph ); rdf( AtnLogonParametersNode, fixm:uom, AtnLogonParametersUOM, Graph ); rdf( AtnLogonParametersNode, plain:uom, AtnLogonParametersUOM, Graph ) ),
          AtnLogonParameters=xval(AtnLogonParametersValue,AtnLogonParametersUOM)
        );
        (
          rdf( AtnLogonParametersNode,aixm:nilReason, AtnLogonParametersNilReason, Graph ),
          AtnLogonParameters=nil(AtnLogonParametersNilReason)
        );
        (
          rdf( AtnLogonParametersNode,gml:indeterminatePosition, AtnLogonParametersIndeterminate, Graph ),
          AtnLogonParameters=indeterminate(AtnLogonParametersIndeterminate)
        )
      )
  )
  ,(
    ( SendCpldcIndicator='$null$',
      \+ rdf( CpdlcConnection,fixm:'sendCpldcIndicator',_SendCpldcIndicator,Graph )
    );
  ( rdf( CpdlcConnection,fixm:'sendCpldcIndicator',SendCpldcIndicatorNode,Graph )),
      (
        (
          rdf(SendCpldcIndicatorNode,rdf:value,SendCpldcIndicatorValue,Graph),
         \+ ( rdf( SendCpldcIndicatorNode, aixm:uom, _SendCpldcIndicatorUOM, Graph ); rdf( SendCpldcIndicatorNode, fixm:uom, _SendCpldcIndicatorUOM, Graph ); rdf( SendCpldcIndicatorNode, plain:uom, _SendCpldcIndicatorUOM, Graph ) ),
          SendCpldcIndicator=val(SendCpldcIndicatorValue)
        );
        (
          rdf( SendCpldcIndicatorNode,rdf:value,SendCpldcIndicatorValue,Graph ),
          ( rdf( SendCpldcIndicatorNode, aixm:uom, SendCpldcIndicatorUOM, Graph ); rdf( SendCpldcIndicatorNode, fixm:uom, SendCpldcIndicatorUOM, Graph ); rdf( SendCpldcIndicatorNode, plain:uom, SendCpldcIndicatorUOM, Graph ) ),
          SendCpldcIndicator=xval(SendCpldcIndicatorValue,SendCpldcIndicatorUOM)
        );
        (
          rdf( SendCpldcIndicatorNode,aixm:nilReason, SendCpldcIndicatorNilReason, Graph ),
          SendCpldcIndicator=nil(SendCpldcIndicatorNilReason)
        );
        (
          rdf( SendCpldcIndicatorNode,gml:indeterminatePosition, SendCpldcIndicatorIndeterminate, Graph ),
          SendCpldcIndicator=indeterminate(SendCpldcIndicatorIndeterminate)
        )
      )
  )
  ,(
    ( ConnectionStatus='$null$',
      \+ rdf( CpdlcConnection,fixm:'connectionStatus',_ConnectionStatus,Graph )
    );
  ( rdf( CpdlcConnection,fixm:'connectionStatus',ConnectionStatusNode,Graph )),
      (
        (
          rdf(ConnectionStatusNode,rdf:value,ConnectionStatusValue,Graph),
         \+ ( rdf( ConnectionStatusNode, aixm:uom, _ConnectionStatusUOM, Graph ); rdf( ConnectionStatusNode, fixm:uom, _ConnectionStatusUOM, Graph ); rdf( ConnectionStatusNode, plain:uom, _ConnectionStatusUOM, Graph ) ),
          ConnectionStatus=val(ConnectionStatusValue)
        );
        (
          rdf( ConnectionStatusNode,rdf:value,ConnectionStatusValue,Graph ),
          ( rdf( ConnectionStatusNode, aixm:uom, ConnectionStatusUOM, Graph ); rdf( ConnectionStatusNode, fixm:uom, ConnectionStatusUOM, Graph ); rdf( ConnectionStatusNode, plain:uom, ConnectionStatusUOM, Graph ) ),
          ConnectionStatus=xval(ConnectionStatusValue,ConnectionStatusUOM)
        );
        (
          rdf( ConnectionStatusNode,aixm:nilReason, ConnectionStatusNilReason, Graph ),
          ConnectionStatus=nil(ConnectionStatusNilReason)
        );
        (
          rdf( ConnectionStatusNode,gml:indeterminatePosition, ConnectionStatusIndeterminate, Graph ),
          ConnectionStatus=indeterminate(ConnectionStatusIndeterminate)
        )
      )
  )
  ,(
    ( FrequencyUsage='$null$',
      \+ rdf( CpdlcConnection,fixm:'frequencyUsage',_FrequencyUsage,Graph )
    );
  ( rdf( CpdlcConnection,fixm:'frequencyUsage',FrequencyUsageNode,Graph )),
      (
        (
          rdf(FrequencyUsageNode,rdf:value,FrequencyUsageValue,Graph),
         \+ ( rdf( FrequencyUsageNode, aixm:uom, _FrequencyUsageUOM, Graph ); rdf( FrequencyUsageNode, fixm:uom, _FrequencyUsageUOM, Graph ); rdf( FrequencyUsageNode, plain:uom, _FrequencyUsageUOM, Graph ) ),
          FrequencyUsage=val(FrequencyUsageValue)
        );
        (
          rdf( FrequencyUsageNode,rdf:value,FrequencyUsageValue,Graph ),
          ( rdf( FrequencyUsageNode, aixm:uom, FrequencyUsageUOM, Graph ); rdf( FrequencyUsageNode, fixm:uom, FrequencyUsageUOM, Graph ); rdf( FrequencyUsageNode, plain:uom, FrequencyUsageUOM, Graph ) ),
          FrequencyUsage=xval(FrequencyUsageValue,FrequencyUsageUOM)
        );
        (
          rdf( FrequencyUsageNode,aixm:nilReason, FrequencyUsageNilReason, Graph ),
          FrequencyUsage=nil(FrequencyUsageNilReason)
        );
        (
          rdf( FrequencyUsageNode,gml:indeterminatePosition, FrequencyUsageIndeterminate, Graph ),
          FrequencyUsage=indeterminate(FrequencyUsageIndeterminate)
        )
      )
  )
  ,(
    ( Fans1ALogonParameters='$null$',
      \+ rdf( CpdlcConnection,fixm:'fans1ALogonParameters',_Fans1ALogonParameters,Graph )
    );
  ( rdf( CpdlcConnection,fixm:'fans1ALogonParameters',Fans1ALogonParametersNode,Graph )),
      (
        (
          rdf(Fans1ALogonParametersNode,rdf:value,Fans1ALogonParametersValue,Graph),
         \+ ( rdf( Fans1ALogonParametersNode, aixm:uom, _Fans1ALogonParametersUOM, Graph ); rdf( Fans1ALogonParametersNode, fixm:uom, _Fans1ALogonParametersUOM, Graph ); rdf( Fans1ALogonParametersNode, plain:uom, _Fans1ALogonParametersUOM, Graph ) ),
          Fans1ALogonParameters=val(Fans1ALogonParametersValue)
        );
        (
          rdf( Fans1ALogonParametersNode,rdf:value,Fans1ALogonParametersValue,Graph ),
          ( rdf( Fans1ALogonParametersNode, aixm:uom, Fans1ALogonParametersUOM, Graph ); rdf( Fans1ALogonParametersNode, fixm:uom, Fans1ALogonParametersUOM, Graph ); rdf( Fans1ALogonParametersNode, plain:uom, Fans1ALogonParametersUOM, Graph ) ),
          Fans1ALogonParameters=xval(Fans1ALogonParametersValue,Fans1ALogonParametersUOM)
        );
        (
          rdf( Fans1ALogonParametersNode,aixm:nilReason, Fans1ALogonParametersNilReason, Graph ),
          Fans1ALogonParameters=nil(Fans1ALogonParametersNilReason)
        );
        (
          rdf( Fans1ALogonParametersNode,gml:indeterminatePosition, Fans1ALogonParametersIndeterminate, Graph ),
          Fans1ALogonParameters=indeterminate(Fans1ALogonParametersIndeterminate)
        )
      )
  ) .

aixm_TelephoneContact(Graph, TelephoneContact, Voice, Facsimile) :-
  rdf(TelephoneContact,rdf:type,aixm:'TelephoneContact',Graph)
  ,(
    ( Voice='$null$',
      \+ rdf( TelephoneContact,aixm:'voice',_Voice,Graph )
    );
  ( rdf( TelephoneContact,aixm:'voice',VoiceNode,Graph )),
      (
        (
          rdf(VoiceNode,rdf:value,VoiceValue,Graph),
         \+ ( rdf( VoiceNode, aixm:uom, _VoiceUOM, Graph ); rdf( VoiceNode, fixm:uom, _VoiceUOM, Graph ); rdf( VoiceNode, plain:uom, _VoiceUOM, Graph ) ),
          Voice=val(VoiceValue)
        );
        (
          rdf( VoiceNode,rdf:value,VoiceValue,Graph ),
          ( rdf( VoiceNode, aixm:uom, VoiceUOM, Graph ); rdf( VoiceNode, fixm:uom, VoiceUOM, Graph ); rdf( VoiceNode, plain:uom, VoiceUOM, Graph ) ),
          Voice=xval(VoiceValue,VoiceUOM)
        );
        (
          rdf( VoiceNode,aixm:nilReason, VoiceNilReason, Graph ),
          Voice=nil(VoiceNilReason)
        );
        (
          rdf( VoiceNode,gml:indeterminatePosition, VoiceIndeterminate, Graph ),
          Voice=indeterminate(VoiceIndeterminate)
        )
      )
  )
  ,(
    ( Facsimile='$null$',
      \+ rdf( TelephoneContact,aixm:'facsimile',_Facsimile,Graph )
    );
  ( rdf( TelephoneContact,aixm:'facsimile',FacsimileNode,Graph )),
      (
        (
          rdf(FacsimileNode,rdf:value,FacsimileValue,Graph),
         \+ ( rdf( FacsimileNode, aixm:uom, _FacsimileUOM, Graph ); rdf( FacsimileNode, fixm:uom, _FacsimileUOM, Graph ); rdf( FacsimileNode, plain:uom, _FacsimileUOM, Graph ) ),
          Facsimile=val(FacsimileValue)
        );
        (
          rdf( FacsimileNode,rdf:value,FacsimileValue,Graph ),
          ( rdf( FacsimileNode, aixm:uom, FacsimileUOM, Graph ); rdf( FacsimileNode, fixm:uom, FacsimileUOM, Graph ); rdf( FacsimileNode, plain:uom, FacsimileUOM, Graph ) ),
          Facsimile=xval(FacsimileValue,FacsimileUOM)
        );
        (
          rdf( FacsimileNode,aixm:nilReason, FacsimileNilReason, Graph ),
          Facsimile=nil(FacsimileNilReason)
        );
        (
          rdf( FacsimileNode,gml:indeterminatePosition, FacsimileIndeterminate, Graph ),
          Facsimile=indeterminate(FacsimileIndeterminate)
        )
      )
  ) .

fixm_Route(Graph, Route, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTime, ExpandedRoute, ClimbSchedule, DescentSchedule, Segment) :-
  subClassOf(T,fixm:'Route')
  ,rdf(Route,rdf:type,T,Graph)
  ,(
    ( AirfileRouteStartTime='$null$',
      \+ rdf( Route,fixm:'airfileRouteStartTime',_AirfileRouteStartTime,Graph )
    );
  ( rdf( Route,fixm:'airfileRouteStartTime',AirfileRouteStartTimeNode,Graph )),
      (
        (
          rdf(AirfileRouteStartTimeNode,rdf:value,AirfileRouteStartTimeValue,Graph),
         \+ ( rdf( AirfileRouteStartTimeNode, aixm:uom, _AirfileRouteStartTimeUOM, Graph ); rdf( AirfileRouteStartTimeNode, fixm:uom, _AirfileRouteStartTimeUOM, Graph ); rdf( AirfileRouteStartTimeNode, plain:uom, _AirfileRouteStartTimeUOM, Graph ) ),
          AirfileRouteStartTime=val(AirfileRouteStartTimeValue)
        );
        (
          rdf( AirfileRouteStartTimeNode,rdf:value,AirfileRouteStartTimeValue,Graph ),
          ( rdf( AirfileRouteStartTimeNode, aixm:uom, AirfileRouteStartTimeUOM, Graph ); rdf( AirfileRouteStartTimeNode, fixm:uom, AirfileRouteStartTimeUOM, Graph ); rdf( AirfileRouteStartTimeNode, plain:uom, AirfileRouteStartTimeUOM, Graph ) ),
          AirfileRouteStartTime=xval(AirfileRouteStartTimeValue,AirfileRouteStartTimeUOM)
        );
        (
          rdf( AirfileRouteStartTimeNode,aixm:nilReason, AirfileRouteStartTimeNilReason, Graph ),
          AirfileRouteStartTime=nil(AirfileRouteStartTimeNilReason)
        );
        (
          rdf( AirfileRouteStartTimeNode,gml:indeterminatePosition, AirfileRouteStartTimeIndeterminate, Graph ),
          AirfileRouteStartTime=indeterminate(AirfileRouteStartTimeIndeterminate)
        )
      )
  )
  ,(
    ( FlightDuration='$null$',
      \+ rdf( Route,fixm:'flightDuration',_FlightDuration,Graph )
    );
  ( rdf( Route,fixm:'flightDuration',FlightDurationNode,Graph )),
      (
        (
          rdf(FlightDurationNode,rdf:value,FlightDurationValue,Graph),
         \+ ( rdf( FlightDurationNode, aixm:uom, _FlightDurationUOM, Graph ); rdf( FlightDurationNode, fixm:uom, _FlightDurationUOM, Graph ); rdf( FlightDurationNode, plain:uom, _FlightDurationUOM, Graph ) ),
          FlightDuration=val(FlightDurationValue)
        );
        (
          rdf( FlightDurationNode,rdf:value,FlightDurationValue,Graph ),
          ( rdf( FlightDurationNode, aixm:uom, FlightDurationUOM, Graph ); rdf( FlightDurationNode, fixm:uom, FlightDurationUOM, Graph ); rdf( FlightDurationNode, plain:uom, FlightDurationUOM, Graph ) ),
          FlightDuration=xval(FlightDurationValue,FlightDurationUOM)
        );
        (
          rdf( FlightDurationNode,aixm:nilReason, FlightDurationNilReason, Graph ),
          FlightDuration=nil(FlightDurationNilReason)
        );
        (
          rdf( FlightDurationNode,gml:indeterminatePosition, FlightDurationIndeterminate, Graph ),
          FlightDuration=indeterminate(FlightDurationIndeterminate)
        )
      )
  )
  ,(
    ( InitialCruisingSpeed='$null$',
      \+ rdf( Route,fixm:'initialCruisingSpeed',_InitialCruisingSpeed,Graph )
    );
  ( rdf( Route,fixm:'initialCruisingSpeed',InitialCruisingSpeedNode,Graph )),
      (
        (
          rdf(InitialCruisingSpeedNode,rdf:value,InitialCruisingSpeedValue,Graph),
         \+ ( rdf( InitialCruisingSpeedNode, aixm:uom, _InitialCruisingSpeedUOM, Graph ); rdf( InitialCruisingSpeedNode, fixm:uom, _InitialCruisingSpeedUOM, Graph ); rdf( InitialCruisingSpeedNode, plain:uom, _InitialCruisingSpeedUOM, Graph ) ),
          InitialCruisingSpeed=val(InitialCruisingSpeedValue)
        );
        (
          rdf( InitialCruisingSpeedNode,rdf:value,InitialCruisingSpeedValue,Graph ),
          ( rdf( InitialCruisingSpeedNode, aixm:uom, InitialCruisingSpeedUOM, Graph ); rdf( InitialCruisingSpeedNode, fixm:uom, InitialCruisingSpeedUOM, Graph ); rdf( InitialCruisingSpeedNode, plain:uom, InitialCruisingSpeedUOM, Graph ) ),
          InitialCruisingSpeed=xval(InitialCruisingSpeedValue,InitialCruisingSpeedUOM)
        );
        (
          rdf( InitialCruisingSpeedNode,aixm:nilReason, InitialCruisingSpeedNilReason, Graph ),
          InitialCruisingSpeed=nil(InitialCruisingSpeedNilReason)
        );
        (
          rdf( InitialCruisingSpeedNode,gml:indeterminatePosition, InitialCruisingSpeedIndeterminate, Graph ),
          InitialCruisingSpeed=indeterminate(InitialCruisingSpeedIndeterminate)
        )
      )
  )
  ,(
    ( InitialFlightRules='$null$',
      \+ rdf( Route,fixm:'initialFlightRules',_InitialFlightRules,Graph )
    );
  ( rdf( Route,fixm:'initialFlightRules',InitialFlightRulesNode,Graph )),
      (
        (
          rdf(InitialFlightRulesNode,rdf:value,InitialFlightRulesValue,Graph),
         \+ ( rdf( InitialFlightRulesNode, aixm:uom, _InitialFlightRulesUOM, Graph ); rdf( InitialFlightRulesNode, fixm:uom, _InitialFlightRulesUOM, Graph ); rdf( InitialFlightRulesNode, plain:uom, _InitialFlightRulesUOM, Graph ) ),
          InitialFlightRules=val(InitialFlightRulesValue)
        );
        (
          rdf( InitialFlightRulesNode,rdf:value,InitialFlightRulesValue,Graph ),
          ( rdf( InitialFlightRulesNode, aixm:uom, InitialFlightRulesUOM, Graph ); rdf( InitialFlightRulesNode, fixm:uom, InitialFlightRulesUOM, Graph ); rdf( InitialFlightRulesNode, plain:uom, InitialFlightRulesUOM, Graph ) ),
          InitialFlightRules=xval(InitialFlightRulesValue,InitialFlightRulesUOM)
        );
        (
          rdf( InitialFlightRulesNode,aixm:nilReason, InitialFlightRulesNilReason, Graph ),
          InitialFlightRules=nil(InitialFlightRulesNilReason)
        );
        (
          rdf( InitialFlightRulesNode,gml:indeterminatePosition, InitialFlightRulesIndeterminate, Graph ),
          InitialFlightRules=indeterminate(InitialFlightRulesIndeterminate)
        )
      )
  )
  ,(
    ( RequestedAltitude='$null$',
      \+ rdf( Route,fixm:'requestedAltitude',_RequestedAltitude,Graph )
    );
  ( rdf( Route,fixm:'requestedAltitude',RequestedAltitudeNode,Graph )),
      (
        (
          rdf(RequestedAltitudeNode,rdf:value,RequestedAltitudeValue,Graph),
         \+ ( rdf( RequestedAltitudeNode, aixm:uom, _RequestedAltitudeUOM, Graph ); rdf( RequestedAltitudeNode, fixm:uom, _RequestedAltitudeUOM, Graph ); rdf( RequestedAltitudeNode, plain:uom, _RequestedAltitudeUOM, Graph ) ),
          RequestedAltitude=val(RequestedAltitudeValue)
        );
        (
          rdf( RequestedAltitudeNode,rdf:value,RequestedAltitudeValue,Graph ),
          ( rdf( RequestedAltitudeNode, aixm:uom, RequestedAltitudeUOM, Graph ); rdf( RequestedAltitudeNode, fixm:uom, RequestedAltitudeUOM, Graph ); rdf( RequestedAltitudeNode, plain:uom, RequestedAltitudeUOM, Graph ) ),
          RequestedAltitude=xval(RequestedAltitudeValue,RequestedAltitudeUOM)
        );
        (
          rdf( RequestedAltitudeNode,aixm:nilReason, RequestedAltitudeNilReason, Graph ),
          RequestedAltitude=nil(RequestedAltitudeNilReason)
        );
        (
          rdf( RequestedAltitudeNode,gml:indeterminatePosition, RequestedAltitudeIndeterminate, Graph ),
          RequestedAltitude=indeterminate(RequestedAltitudeIndeterminate)
        )
      )
  )
  ,(
    ( RouteText='$null$',
      \+ rdf( Route,fixm:'routeText',_RouteText,Graph )
    );
  ( rdf( Route,fixm:'routeText',RouteTextNode,Graph )),
      (
        (
          rdf(RouteTextNode,rdf:value,RouteTextValue,Graph),
         \+ ( rdf( RouteTextNode, aixm:uom, _RouteTextUOM, Graph ); rdf( RouteTextNode, fixm:uom, _RouteTextUOM, Graph ); rdf( RouteTextNode, plain:uom, _RouteTextUOM, Graph ) ),
          RouteText=val(RouteTextValue)
        );
        (
          rdf( RouteTextNode,rdf:value,RouteTextValue,Graph ),
          ( rdf( RouteTextNode, aixm:uom, RouteTextUOM, Graph ); rdf( RouteTextNode, fixm:uom, RouteTextUOM, Graph ); rdf( RouteTextNode, plain:uom, RouteTextUOM, Graph ) ),
          RouteText=xval(RouteTextValue,RouteTextUOM)
        );
        (
          rdf( RouteTextNode,aixm:nilReason, RouteTextNilReason, Graph ),
          RouteText=nil(RouteTextNilReason)
        );
        (
          rdf( RouteTextNode,gml:indeterminatePosition, RouteTextIndeterminate, Graph ),
          RouteText=indeterminate(RouteTextIndeterminate)
        )
      )
  )
  ,findall(A, rdf(Route,fixm:'estimatedElapsedTime',A,Graph), EstimatedElapsedTime)
  ,( ( ExpandedRoute='$null$',
    \+ rdf( Route,fixm:'expandedRoute', _ExpandedRoute, Graph  )
   ; rdf(Route,fixm:'expandedRoute', ExpandedRoute, Graph ) )
  )
  ,( ( ClimbSchedule='$null$',
    \+ rdf( Route,fixm:'climbSchedule', _ClimbSchedule, Graph  )
   ; rdf(Route,fixm:'climbSchedule', ClimbSchedule, Graph ) )
  )
  ,( ( DescentSchedule='$null$',
    \+ rdf( Route,fixm:'descentSchedule', _DescentSchedule, Graph  )
   ; rdf(Route,fixm:'descentSchedule', DescentSchedule, Graph ) )
  )
  ,findall(A, rdf(Route,fixm:'segment',A,Graph), Segment) .

fixm_Person(Graph, Person, Name, Contact) :-
  rdf(Person,rdf:type,fixm:'Person',Graph)
  ,(
    ( Name='$null$',
      \+ rdf( Person,fixm:'name',_Name,Graph )
    );
  ( rdf( Person,fixm:'name',NameNode,Graph )),
      (
        (
          rdf(NameNode,rdf:value,NameValue,Graph),
         \+ ( rdf( NameNode, aixm:uom, _NameUOM, Graph ); rdf( NameNode, fixm:uom, _NameUOM, Graph ); rdf( NameNode, plain:uom, _NameUOM, Graph ) ),
          Name=val(NameValue)
        );
        (
          rdf( NameNode,rdf:value,NameValue,Graph ),
          ( rdf( NameNode, aixm:uom, NameUOM, Graph ); rdf( NameNode, fixm:uom, NameUOM, Graph ); rdf( NameNode, plain:uom, NameUOM, Graph ) ),
          Name=xval(NameValue,NameUOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NameNilReason, Graph ),
          Name=nil(NameNilReason)
        );
        (
          rdf( NameNode,gml:indeterminatePosition, NameIndeterminate, Graph ),
          Name=indeterminate(NameIndeterminate)
        )
      )
  )
  ,( ( Contact='$null$',
    \+ rdf( Person,fixm:'contact', _Contact, Graph  )
   ; rdf(Person,fixm:'contact', Contact, Graph ) )
  ) .

fixm_EfplFlight(Graph, EfplFlight, IfplId, TotalEstimatedElapsedTime, AerodromesOfDestination, EfplSpecialHandling, EfplFiledTrajectory, EfplAcceptedTrajectory, OtherInformation, FlightPerformanceData) :-
  rdf(EfplFlight,rdf:type,fixm:'EfplFlight',Graph)
  ,(
    ( IfplId='$null$',
      \+ rdf( EfplFlight,fixm:'ifplId',_IfplId,Graph )
    );
  ( rdf( EfplFlight,fixm:'ifplId',IfplIdNode,Graph )),
      (
        (
          rdf(IfplIdNode,rdf:value,IfplIdValue,Graph),
         \+ ( rdf( IfplIdNode, aixm:uom, _IfplIdUOM, Graph ); rdf( IfplIdNode, fixm:uom, _IfplIdUOM, Graph ); rdf( IfplIdNode, plain:uom, _IfplIdUOM, Graph ) ),
          IfplId=val(IfplIdValue)
        );
        (
          rdf( IfplIdNode,rdf:value,IfplIdValue,Graph ),
          ( rdf( IfplIdNode, aixm:uom, IfplIdUOM, Graph ); rdf( IfplIdNode, fixm:uom, IfplIdUOM, Graph ); rdf( IfplIdNode, plain:uom, IfplIdUOM, Graph ) ),
          IfplId=xval(IfplIdValue,IfplIdUOM)
        );
        (
          rdf( IfplIdNode,aixm:nilReason, IfplIdNilReason, Graph ),
          IfplId=nil(IfplIdNilReason)
        );
        (
          rdf( IfplIdNode,gml:indeterminatePosition, IfplIdIndeterminate, Graph ),
          IfplId=indeterminate(IfplIdIndeterminate)
        )
      )
  )
  ,(
    ( TotalEstimatedElapsedTime='$null$',
      \+ rdf( EfplFlight,fixm:'totalEstimatedElapsedTime',_TotalEstimatedElapsedTime,Graph )
    );
  ( rdf( EfplFlight,fixm:'totalEstimatedElapsedTime',TotalEstimatedElapsedTimeNode,Graph )),
      (
        (
          rdf(TotalEstimatedElapsedTimeNode,rdf:value,TotalEstimatedElapsedTimeValue,Graph),
         \+ ( rdf( TotalEstimatedElapsedTimeNode, aixm:uom, _TotalEstimatedElapsedTimeUOM, Graph ); rdf( TotalEstimatedElapsedTimeNode, fixm:uom, _TotalEstimatedElapsedTimeUOM, Graph ); rdf( TotalEstimatedElapsedTimeNode, plain:uom, _TotalEstimatedElapsedTimeUOM, Graph ) ),
          TotalEstimatedElapsedTime=val(TotalEstimatedElapsedTimeValue)
        );
        (
          rdf( TotalEstimatedElapsedTimeNode,rdf:value,TotalEstimatedElapsedTimeValue,Graph ),
          ( rdf( TotalEstimatedElapsedTimeNode, aixm:uom, TotalEstimatedElapsedTimeUOM, Graph ); rdf( TotalEstimatedElapsedTimeNode, fixm:uom, TotalEstimatedElapsedTimeUOM, Graph ); rdf( TotalEstimatedElapsedTimeNode, plain:uom, TotalEstimatedElapsedTimeUOM, Graph ) ),
          TotalEstimatedElapsedTime=xval(TotalEstimatedElapsedTimeValue,TotalEstimatedElapsedTimeUOM)
        );
        (
          rdf( TotalEstimatedElapsedTimeNode,aixm:nilReason, TotalEstimatedElapsedTimeNilReason, Graph ),
          TotalEstimatedElapsedTime=nil(TotalEstimatedElapsedTimeNilReason)
        );
        (
          rdf( TotalEstimatedElapsedTimeNode,gml:indeterminatePosition, TotalEstimatedElapsedTimeIndeterminate, Graph ),
          TotalEstimatedElapsedTime=indeterminate(TotalEstimatedElapsedTimeIndeterminate)
        )
      )
  )
  ,( ( AerodromesOfDestination='$null$',
    \+ rdf( EfplFlight,fixm:'aerodromesOfDestination', _AerodromesOfDestination, Graph  )
   ; rdf(EfplFlight,fixm:'aerodromesOfDestination', AerodromesOfDestination, Graph ) )
  )
  ,(
    ( EfplSpecialHandling='$null$',
      \+ rdf( EfplFlight,fixm:'efplSpecialHandling',_EfplSpecialHandling,Graph )
    );
  ( rdf( EfplFlight,fixm:'efplSpecialHandling',EfplSpecialHandlingNode,Graph )),
      (
        (
          rdf(EfplSpecialHandlingNode,rdf:value,EfplSpecialHandlingValue,Graph),
         \+ ( rdf( EfplSpecialHandlingNode, aixm:uom, _EfplSpecialHandlingUOM, Graph ); rdf( EfplSpecialHandlingNode, fixm:uom, _EfplSpecialHandlingUOM, Graph ); rdf( EfplSpecialHandlingNode, plain:uom, _EfplSpecialHandlingUOM, Graph ) ),
          EfplSpecialHandling=val(EfplSpecialHandlingValue)
        );
        (
          rdf( EfplSpecialHandlingNode,rdf:value,EfplSpecialHandlingValue,Graph ),
          ( rdf( EfplSpecialHandlingNode, aixm:uom, EfplSpecialHandlingUOM, Graph ); rdf( EfplSpecialHandlingNode, fixm:uom, EfplSpecialHandlingUOM, Graph ); rdf( EfplSpecialHandlingNode, plain:uom, EfplSpecialHandlingUOM, Graph ) ),
          EfplSpecialHandling=xval(EfplSpecialHandlingValue,EfplSpecialHandlingUOM)
        );
        (
          rdf( EfplSpecialHandlingNode,aixm:nilReason, EfplSpecialHandlingNilReason, Graph ),
          EfplSpecialHandling=nil(EfplSpecialHandlingNilReason)
        );
        (
          rdf( EfplSpecialHandlingNode,gml:indeterminatePosition, EfplSpecialHandlingIndeterminate, Graph ),
          EfplSpecialHandling=indeterminate(EfplSpecialHandlingIndeterminate)
        )
      )
  )
  ,( ( EfplFiledTrajectory='$null$',
    \+ rdf( EfplFlight,fixm:'efplFiledTrajectory', _EfplFiledTrajectory, Graph  )
   ; rdf(EfplFlight,fixm:'efplFiledTrajectory', EfplFiledTrajectory, Graph ) )
  )
  ,( ( EfplAcceptedTrajectory='$null$',
    \+ rdf( EfplFlight,fixm:'efplAcceptedTrajectory', _EfplAcceptedTrajectory, Graph  )
   ; rdf(EfplFlight,fixm:'efplAcceptedTrajectory', EfplAcceptedTrajectory, Graph ) )
  )
  ,( ( OtherInformation='$null$',
    \+ rdf( EfplFlight,fixm:'otherInformation', _OtherInformation, Graph  )
   ; rdf(EfplFlight,fixm:'otherInformation', OtherInformation, Graph ) )
  )
  ,( ( FlightPerformanceData='$null$',
    \+ rdf( EfplFlight,fixm:'flightPerformanceData', _FlightPerformanceData, Graph  )
   ; rdf(EfplFlight,fixm:'flightPerformanceData', FlightPerformanceData, Graph ) )
  ) .

fixm_Originator(Graph, Originator) :-
  rdf(Originator,rdf:type,fixm:'Originator',Graph) .

fixm_FlightStatus(Graph, FlightStatus, AirborneHold, Airfile, Accepted, FlightCycle, MissedApproach, Suspended) :-
  rdf(FlightStatus,rdf:type,fixm:'FlightStatus',Graph)
  ,(
    ( AirborneHold='$null$',
      \+ rdf( FlightStatus,fixm:'airborneHold',_AirborneHold,Graph )
    );
  ( rdf( FlightStatus,fixm:'airborneHold',AirborneHoldNode,Graph )),
      (
        (
          rdf(AirborneHoldNode,rdf:value,AirborneHoldValue,Graph),
         \+ ( rdf( AirborneHoldNode, aixm:uom, _AirborneHoldUOM, Graph ); rdf( AirborneHoldNode, fixm:uom, _AirborneHoldUOM, Graph ); rdf( AirborneHoldNode, plain:uom, _AirborneHoldUOM, Graph ) ),
          AirborneHold=val(AirborneHoldValue)
        );
        (
          rdf( AirborneHoldNode,rdf:value,AirborneHoldValue,Graph ),
          ( rdf( AirborneHoldNode, aixm:uom, AirborneHoldUOM, Graph ); rdf( AirborneHoldNode, fixm:uom, AirborneHoldUOM, Graph ); rdf( AirborneHoldNode, plain:uom, AirborneHoldUOM, Graph ) ),
          AirborneHold=xval(AirborneHoldValue,AirborneHoldUOM)
        );
        (
          rdf( AirborneHoldNode,aixm:nilReason, AirborneHoldNilReason, Graph ),
          AirborneHold=nil(AirborneHoldNilReason)
        );
        (
          rdf( AirborneHoldNode,gml:indeterminatePosition, AirborneHoldIndeterminate, Graph ),
          AirborneHold=indeterminate(AirborneHoldIndeterminate)
        )
      )
  )
  ,(
    ( Airfile='$null$',
      \+ rdf( FlightStatus,fixm:'airfile',_Airfile,Graph )
    );
  ( rdf( FlightStatus,fixm:'airfile',AirfileNode,Graph )),
      (
        (
          rdf(AirfileNode,rdf:value,AirfileValue,Graph),
         \+ ( rdf( AirfileNode, aixm:uom, _AirfileUOM, Graph ); rdf( AirfileNode, fixm:uom, _AirfileUOM, Graph ); rdf( AirfileNode, plain:uom, _AirfileUOM, Graph ) ),
          Airfile=val(AirfileValue)
        );
        (
          rdf( AirfileNode,rdf:value,AirfileValue,Graph ),
          ( rdf( AirfileNode, aixm:uom, AirfileUOM, Graph ); rdf( AirfileNode, fixm:uom, AirfileUOM, Graph ); rdf( AirfileNode, plain:uom, AirfileUOM, Graph ) ),
          Airfile=xval(AirfileValue,AirfileUOM)
        );
        (
          rdf( AirfileNode,aixm:nilReason, AirfileNilReason, Graph ),
          Airfile=nil(AirfileNilReason)
        );
        (
          rdf( AirfileNode,gml:indeterminatePosition, AirfileIndeterminate, Graph ),
          Airfile=indeterminate(AirfileIndeterminate)
        )
      )
  )
  ,(
    ( Accepted='$null$',
      \+ rdf( FlightStatus,fixm:'accepted',_Accepted,Graph )
    );
  ( rdf( FlightStatus,fixm:'accepted',AcceptedNode,Graph )),
      (
        (
          rdf(AcceptedNode,rdf:value,AcceptedValue,Graph),
         \+ ( rdf( AcceptedNode, aixm:uom, _AcceptedUOM, Graph ); rdf( AcceptedNode, fixm:uom, _AcceptedUOM, Graph ); rdf( AcceptedNode, plain:uom, _AcceptedUOM, Graph ) ),
          Accepted=val(AcceptedValue)
        );
        (
          rdf( AcceptedNode,rdf:value,AcceptedValue,Graph ),
          ( rdf( AcceptedNode, aixm:uom, AcceptedUOM, Graph ); rdf( AcceptedNode, fixm:uom, AcceptedUOM, Graph ); rdf( AcceptedNode, plain:uom, AcceptedUOM, Graph ) ),
          Accepted=xval(AcceptedValue,AcceptedUOM)
        );
        (
          rdf( AcceptedNode,aixm:nilReason, AcceptedNilReason, Graph ),
          Accepted=nil(AcceptedNilReason)
        );
        (
          rdf( AcceptedNode,gml:indeterminatePosition, AcceptedIndeterminate, Graph ),
          Accepted=indeterminate(AcceptedIndeterminate)
        )
      )
  )
  ,(
    ( FlightCycle='$null$',
      \+ rdf( FlightStatus,fixm:'flightCycle',_FlightCycle,Graph )
    );
  ( rdf( FlightStatus,fixm:'flightCycle',FlightCycleNode,Graph )),
      (
        (
          rdf(FlightCycleNode,rdf:value,FlightCycleValue,Graph),
         \+ ( rdf( FlightCycleNode, aixm:uom, _FlightCycleUOM, Graph ); rdf( FlightCycleNode, fixm:uom, _FlightCycleUOM, Graph ); rdf( FlightCycleNode, plain:uom, _FlightCycleUOM, Graph ) ),
          FlightCycle=val(FlightCycleValue)
        );
        (
          rdf( FlightCycleNode,rdf:value,FlightCycleValue,Graph ),
          ( rdf( FlightCycleNode, aixm:uom, FlightCycleUOM, Graph ); rdf( FlightCycleNode, fixm:uom, FlightCycleUOM, Graph ); rdf( FlightCycleNode, plain:uom, FlightCycleUOM, Graph ) ),
          FlightCycle=xval(FlightCycleValue,FlightCycleUOM)
        );
        (
          rdf( FlightCycleNode,aixm:nilReason, FlightCycleNilReason, Graph ),
          FlightCycle=nil(FlightCycleNilReason)
        );
        (
          rdf( FlightCycleNode,gml:indeterminatePosition, FlightCycleIndeterminate, Graph ),
          FlightCycle=indeterminate(FlightCycleIndeterminate)
        )
      )
  )
  ,(
    ( MissedApproach='$null$',
      \+ rdf( FlightStatus,fixm:'missedApproach',_MissedApproach,Graph )
    );
  ( rdf( FlightStatus,fixm:'missedApproach',MissedApproachNode,Graph )),
      (
        (
          rdf(MissedApproachNode,rdf:value,MissedApproachValue,Graph),
         \+ ( rdf( MissedApproachNode, aixm:uom, _MissedApproachUOM, Graph ); rdf( MissedApproachNode, fixm:uom, _MissedApproachUOM, Graph ); rdf( MissedApproachNode, plain:uom, _MissedApproachUOM, Graph ) ),
          MissedApproach=val(MissedApproachValue)
        );
        (
          rdf( MissedApproachNode,rdf:value,MissedApproachValue,Graph ),
          ( rdf( MissedApproachNode, aixm:uom, MissedApproachUOM, Graph ); rdf( MissedApproachNode, fixm:uom, MissedApproachUOM, Graph ); rdf( MissedApproachNode, plain:uom, MissedApproachUOM, Graph ) ),
          MissedApproach=xval(MissedApproachValue,MissedApproachUOM)
        );
        (
          rdf( MissedApproachNode,aixm:nilReason, MissedApproachNilReason, Graph ),
          MissedApproach=nil(MissedApproachNilReason)
        );
        (
          rdf( MissedApproachNode,gml:indeterminatePosition, MissedApproachIndeterminate, Graph ),
          MissedApproach=indeterminate(MissedApproachIndeterminate)
        )
      )
  )
  ,(
    ( Suspended='$null$',
      \+ rdf( FlightStatus,fixm:'suspended',_Suspended,Graph )
    );
  ( rdf( FlightStatus,fixm:'suspended',SuspendedNode,Graph )),
      (
        (
          rdf(SuspendedNode,rdf:value,SuspendedValue,Graph),
         \+ ( rdf( SuspendedNode, aixm:uom, _SuspendedUOM, Graph ); rdf( SuspendedNode, fixm:uom, _SuspendedUOM, Graph ); rdf( SuspendedNode, plain:uom, _SuspendedUOM, Graph ) ),
          Suspended=val(SuspendedValue)
        );
        (
          rdf( SuspendedNode,rdf:value,SuspendedValue,Graph ),
          ( rdf( SuspendedNode, aixm:uom, SuspendedUOM, Graph ); rdf( SuspendedNode, fixm:uom, SuspendedUOM, Graph ); rdf( SuspendedNode, plain:uom, SuspendedUOM, Graph ) ),
          Suspended=xval(SuspendedValue,SuspendedUOM)
        );
        (
          rdf( SuspendedNode,aixm:nilReason, SuspendedNilReason, Graph ),
          Suspended=nil(SuspendedNilReason)
        );
        (
          rdf( SuspendedNode,gml:indeterminatePosition, SuspendedIndeterminate, Graph ),
          Suspended=indeterminate(SuspendedIndeterminate)
        )
      )
  ) .

fixm_IdentifiedUnitReference(Graph, IdentifiedUnitReference, UnitIdentifier) :-
  rdf(IdentifiedUnitReference,rdf:type,fixm:'IdentifiedUnitReference',Graph)
  ,(
    ( UnitIdentifier='$null$',
      \+ rdf( IdentifiedUnitReference,fixm:'unitIdentifier',_UnitIdentifier,Graph )
    );
  ( rdf( IdentifiedUnitReference,fixm:'unitIdentifier',UnitIdentifierNode,Graph )),
      (
        (
          rdf(UnitIdentifierNode,rdf:value,UnitIdentifierValue,Graph),
         \+ ( rdf( UnitIdentifierNode, aixm:uom, _UnitIdentifierUOM, Graph ); rdf( UnitIdentifierNode, fixm:uom, _UnitIdentifierUOM, Graph ); rdf( UnitIdentifierNode, plain:uom, _UnitIdentifierUOM, Graph ) ),
          UnitIdentifier=val(UnitIdentifierValue)
        );
        (
          rdf( UnitIdentifierNode,rdf:value,UnitIdentifierValue,Graph ),
          ( rdf( UnitIdentifierNode, aixm:uom, UnitIdentifierUOM, Graph ); rdf( UnitIdentifierNode, fixm:uom, UnitIdentifierUOM, Graph ); rdf( UnitIdentifierNode, plain:uom, UnitIdentifierUOM, Graph ) ),
          UnitIdentifier=xval(UnitIdentifierValue,UnitIdentifierUOM)
        );
        (
          rdf( UnitIdentifierNode,aixm:nilReason, UnitIdentifierNilReason, Graph ),
          UnitIdentifier=nil(UnitIdentifierNilReason)
        );
        (
          rdf( UnitIdentifierNode,gml:indeterminatePosition, UnitIdentifierIndeterminate, Graph ),
          UnitIdentifier=indeterminate(UnitIdentifierIndeterminate)
        )
      )
  ) .

fixm_Radionuclide(Graph, Radionuclide, PhysicalChemicalForm, RadionuclideId, RadionuclideName, LowDispersibleMaterialIndicator, Activity, SpecialFormIndicator) :-
  rdf(Radionuclide,rdf:type,fixm:'Radionuclide',Graph)
  ,(
    ( PhysicalChemicalForm='$null$',
      \+ rdf( Radionuclide,fixm:'physicalChemicalForm',_PhysicalChemicalForm,Graph )
    );
  ( rdf( Radionuclide,fixm:'physicalChemicalForm',PhysicalChemicalFormNode,Graph )),
      (
        (
          rdf(PhysicalChemicalFormNode,rdf:value,PhysicalChemicalFormValue,Graph),
         \+ ( rdf( PhysicalChemicalFormNode, aixm:uom, _PhysicalChemicalFormUOM, Graph ); rdf( PhysicalChemicalFormNode, fixm:uom, _PhysicalChemicalFormUOM, Graph ); rdf( PhysicalChemicalFormNode, plain:uom, _PhysicalChemicalFormUOM, Graph ) ),
          PhysicalChemicalForm=val(PhysicalChemicalFormValue)
        );
        (
          rdf( PhysicalChemicalFormNode,rdf:value,PhysicalChemicalFormValue,Graph ),
          ( rdf( PhysicalChemicalFormNode, aixm:uom, PhysicalChemicalFormUOM, Graph ); rdf( PhysicalChemicalFormNode, fixm:uom, PhysicalChemicalFormUOM, Graph ); rdf( PhysicalChemicalFormNode, plain:uom, PhysicalChemicalFormUOM, Graph ) ),
          PhysicalChemicalForm=xval(PhysicalChemicalFormValue,PhysicalChemicalFormUOM)
        );
        (
          rdf( PhysicalChemicalFormNode,aixm:nilReason, PhysicalChemicalFormNilReason, Graph ),
          PhysicalChemicalForm=nil(PhysicalChemicalFormNilReason)
        );
        (
          rdf( PhysicalChemicalFormNode,gml:indeterminatePosition, PhysicalChemicalFormIndeterminate, Graph ),
          PhysicalChemicalForm=indeterminate(PhysicalChemicalFormIndeterminate)
        )
      )
  )
  ,(
    ( RadionuclideId='$null$',
      \+ rdf( Radionuclide,fixm:'radionuclideId',_RadionuclideId,Graph )
    );
  ( rdf( Radionuclide,fixm:'radionuclideId',RadionuclideIdNode,Graph )),
      (
        (
          rdf(RadionuclideIdNode,rdf:value,RadionuclideIdValue,Graph),
         \+ ( rdf( RadionuclideIdNode, aixm:uom, _RadionuclideIdUOM, Graph ); rdf( RadionuclideIdNode, fixm:uom, _RadionuclideIdUOM, Graph ); rdf( RadionuclideIdNode, plain:uom, _RadionuclideIdUOM, Graph ) ),
          RadionuclideId=val(RadionuclideIdValue)
        );
        (
          rdf( RadionuclideIdNode,rdf:value,RadionuclideIdValue,Graph ),
          ( rdf( RadionuclideIdNode, aixm:uom, RadionuclideIdUOM, Graph ); rdf( RadionuclideIdNode, fixm:uom, RadionuclideIdUOM, Graph ); rdf( RadionuclideIdNode, plain:uom, RadionuclideIdUOM, Graph ) ),
          RadionuclideId=xval(RadionuclideIdValue,RadionuclideIdUOM)
        );
        (
          rdf( RadionuclideIdNode,aixm:nilReason, RadionuclideIdNilReason, Graph ),
          RadionuclideId=nil(RadionuclideIdNilReason)
        );
        (
          rdf( RadionuclideIdNode,gml:indeterminatePosition, RadionuclideIdIndeterminate, Graph ),
          RadionuclideId=indeterminate(RadionuclideIdIndeterminate)
        )
      )
  )
  ,(
    ( RadionuclideName='$null$',
      \+ rdf( Radionuclide,fixm:'radionuclideName',_RadionuclideName,Graph )
    );
  ( rdf( Radionuclide,fixm:'radionuclideName',RadionuclideNameNode,Graph )),
      (
        (
          rdf(RadionuclideNameNode,rdf:value,RadionuclideNameValue,Graph),
         \+ ( rdf( RadionuclideNameNode, aixm:uom, _RadionuclideNameUOM, Graph ); rdf( RadionuclideNameNode, fixm:uom, _RadionuclideNameUOM, Graph ); rdf( RadionuclideNameNode, plain:uom, _RadionuclideNameUOM, Graph ) ),
          RadionuclideName=val(RadionuclideNameValue)
        );
        (
          rdf( RadionuclideNameNode,rdf:value,RadionuclideNameValue,Graph ),
          ( rdf( RadionuclideNameNode, aixm:uom, RadionuclideNameUOM, Graph ); rdf( RadionuclideNameNode, fixm:uom, RadionuclideNameUOM, Graph ); rdf( RadionuclideNameNode, plain:uom, RadionuclideNameUOM, Graph ) ),
          RadionuclideName=xval(RadionuclideNameValue,RadionuclideNameUOM)
        );
        (
          rdf( RadionuclideNameNode,aixm:nilReason, RadionuclideNameNilReason, Graph ),
          RadionuclideName=nil(RadionuclideNameNilReason)
        );
        (
          rdf( RadionuclideNameNode,gml:indeterminatePosition, RadionuclideNameIndeterminate, Graph ),
          RadionuclideName=indeterminate(RadionuclideNameIndeterminate)
        )
      )
  )
  ,(
    ( LowDispersibleMaterialIndicator='$null$',
      \+ rdf( Radionuclide,fixm:'lowDispersibleMaterialIndicator',_LowDispersibleMaterialIndicator,Graph )
    );
  ( rdf( Radionuclide,fixm:'lowDispersibleMaterialIndicator',LowDispersibleMaterialIndicatorNode,Graph )),
      (
        (
          rdf(LowDispersibleMaterialIndicatorNode,rdf:value,LowDispersibleMaterialIndicatorValue,Graph),
         \+ ( rdf( LowDispersibleMaterialIndicatorNode, aixm:uom, _LowDispersibleMaterialIndicatorUOM, Graph ); rdf( LowDispersibleMaterialIndicatorNode, fixm:uom, _LowDispersibleMaterialIndicatorUOM, Graph ); rdf( LowDispersibleMaterialIndicatorNode, plain:uom, _LowDispersibleMaterialIndicatorUOM, Graph ) ),
          LowDispersibleMaterialIndicator=val(LowDispersibleMaterialIndicatorValue)
        );
        (
          rdf( LowDispersibleMaterialIndicatorNode,rdf:value,LowDispersibleMaterialIndicatorValue,Graph ),
          ( rdf( LowDispersibleMaterialIndicatorNode, aixm:uom, LowDispersibleMaterialIndicatorUOM, Graph ); rdf( LowDispersibleMaterialIndicatorNode, fixm:uom, LowDispersibleMaterialIndicatorUOM, Graph ); rdf( LowDispersibleMaterialIndicatorNode, plain:uom, LowDispersibleMaterialIndicatorUOM, Graph ) ),
          LowDispersibleMaterialIndicator=xval(LowDispersibleMaterialIndicatorValue,LowDispersibleMaterialIndicatorUOM)
        );
        (
          rdf( LowDispersibleMaterialIndicatorNode,aixm:nilReason, LowDispersibleMaterialIndicatorNilReason, Graph ),
          LowDispersibleMaterialIndicator=nil(LowDispersibleMaterialIndicatorNilReason)
        );
        (
          rdf( LowDispersibleMaterialIndicatorNode,gml:indeterminatePosition, LowDispersibleMaterialIndicatorIndeterminate, Graph ),
          LowDispersibleMaterialIndicator=indeterminate(LowDispersibleMaterialIndicatorIndeterminate)
        )
      )
  )
  ,(
    ( Activity='$null$',
      \+ rdf( Radionuclide,fixm:'activity',_Activity,Graph )
    );
  ( rdf( Radionuclide,fixm:'activity',ActivityNode,Graph )),
      (
        (
          rdf(ActivityNode,rdf:value,ActivityValue,Graph),
         \+ ( rdf( ActivityNode, aixm:uom, _ActivityUOM, Graph ); rdf( ActivityNode, fixm:uom, _ActivityUOM, Graph ); rdf( ActivityNode, plain:uom, _ActivityUOM, Graph ) ),
          Activity=val(ActivityValue)
        );
        (
          rdf( ActivityNode,rdf:value,ActivityValue,Graph ),
          ( rdf( ActivityNode, aixm:uom, ActivityUOM, Graph ); rdf( ActivityNode, fixm:uom, ActivityUOM, Graph ); rdf( ActivityNode, plain:uom, ActivityUOM, Graph ) ),
          Activity=xval(ActivityValue,ActivityUOM)
        );
        (
          rdf( ActivityNode,aixm:nilReason, ActivityNilReason, Graph ),
          Activity=nil(ActivityNilReason)
        );
        (
          rdf( ActivityNode,gml:indeterminatePosition, ActivityIndeterminate, Graph ),
          Activity=indeterminate(ActivityIndeterminate)
        )
      )
  )
  ,(
    ( SpecialFormIndicator='$null$',
      \+ rdf( Radionuclide,fixm:'specialFormIndicator',_SpecialFormIndicator,Graph )
    );
  ( rdf( Radionuclide,fixm:'specialFormIndicator',SpecialFormIndicatorNode,Graph )),
      (
        (
          rdf(SpecialFormIndicatorNode,rdf:value,SpecialFormIndicatorValue,Graph),
         \+ ( rdf( SpecialFormIndicatorNode, aixm:uom, _SpecialFormIndicatorUOM, Graph ); rdf( SpecialFormIndicatorNode, fixm:uom, _SpecialFormIndicatorUOM, Graph ); rdf( SpecialFormIndicatorNode, plain:uom, _SpecialFormIndicatorUOM, Graph ) ),
          SpecialFormIndicator=val(SpecialFormIndicatorValue)
        );
        (
          rdf( SpecialFormIndicatorNode,rdf:value,SpecialFormIndicatorValue,Graph ),
          ( rdf( SpecialFormIndicatorNode, aixm:uom, SpecialFormIndicatorUOM, Graph ); rdf( SpecialFormIndicatorNode, fixm:uom, SpecialFormIndicatorUOM, Graph ); rdf( SpecialFormIndicatorNode, plain:uom, SpecialFormIndicatorUOM, Graph ) ),
          SpecialFormIndicator=xval(SpecialFormIndicatorValue,SpecialFormIndicatorUOM)
        );
        (
          rdf( SpecialFormIndicatorNode,aixm:nilReason, SpecialFormIndicatorNilReason, Graph ),
          SpecialFormIndicator=nil(SpecialFormIndicatorNilReason)
        );
        (
          rdf( SpecialFormIndicatorNode,gml:indeterminatePosition, SpecialFormIndicatorIndeterminate, Graph ),
          SpecialFormIndicator=indeterminate(SpecialFormIndicatorIndeterminate)
        )
      )
  ) .

aixm_OnlineContact(Graph, OnlineContact, Network, Linkage, Protocol, EMail) :-
  rdf(OnlineContact,rdf:type,aixm:'OnlineContact',Graph)
  ,(
    ( Network='$null$',
      \+ rdf( OnlineContact,aixm:'network',_Network,Graph )
    );
  ( rdf( OnlineContact,aixm:'network',NetworkNode,Graph )),
      (
        (
          rdf(NetworkNode,rdf:value,NetworkValue,Graph),
         \+ ( rdf( NetworkNode, aixm:uom, _NetworkUOM, Graph ); rdf( NetworkNode, fixm:uom, _NetworkUOM, Graph ); rdf( NetworkNode, plain:uom, _NetworkUOM, Graph ) ),
          Network=val(NetworkValue)
        );
        (
          rdf( NetworkNode,rdf:value,NetworkValue,Graph ),
          ( rdf( NetworkNode, aixm:uom, NetworkUOM, Graph ); rdf( NetworkNode, fixm:uom, NetworkUOM, Graph ); rdf( NetworkNode, plain:uom, NetworkUOM, Graph ) ),
          Network=xval(NetworkValue,NetworkUOM)
        );
        (
          rdf( NetworkNode,aixm:nilReason, NetworkNilReason, Graph ),
          Network=nil(NetworkNilReason)
        );
        (
          rdf( NetworkNode,gml:indeterminatePosition, NetworkIndeterminate, Graph ),
          Network=indeterminate(NetworkIndeterminate)
        )
      )
  )
  ,(
    ( Linkage='$null$',
      \+ rdf( OnlineContact,aixm:'linkage',_Linkage,Graph )
    );
  ( rdf( OnlineContact,aixm:'linkage',LinkageNode,Graph )),
      (
        (
          rdf(LinkageNode,rdf:value,LinkageValue,Graph),
         \+ ( rdf( LinkageNode, aixm:uom, _LinkageUOM, Graph ); rdf( LinkageNode, fixm:uom, _LinkageUOM, Graph ); rdf( LinkageNode, plain:uom, _LinkageUOM, Graph ) ),
          Linkage=val(LinkageValue)
        );
        (
          rdf( LinkageNode,rdf:value,LinkageValue,Graph ),
          ( rdf( LinkageNode, aixm:uom, LinkageUOM, Graph ); rdf( LinkageNode, fixm:uom, LinkageUOM, Graph ); rdf( LinkageNode, plain:uom, LinkageUOM, Graph ) ),
          Linkage=xval(LinkageValue,LinkageUOM)
        );
        (
          rdf( LinkageNode,aixm:nilReason, LinkageNilReason, Graph ),
          Linkage=nil(LinkageNilReason)
        );
        (
          rdf( LinkageNode,gml:indeterminatePosition, LinkageIndeterminate, Graph ),
          Linkage=indeterminate(LinkageIndeterminate)
        )
      )
  )
  ,(
    ( Protocol='$null$',
      \+ rdf( OnlineContact,aixm:'protocol',_Protocol,Graph )
    );
  ( rdf( OnlineContact,aixm:'protocol',ProtocolNode,Graph )),
      (
        (
          rdf(ProtocolNode,rdf:value,ProtocolValue,Graph),
         \+ ( rdf( ProtocolNode, aixm:uom, _ProtocolUOM, Graph ); rdf( ProtocolNode, fixm:uom, _ProtocolUOM, Graph ); rdf( ProtocolNode, plain:uom, _ProtocolUOM, Graph ) ),
          Protocol=val(ProtocolValue)
        );
        (
          rdf( ProtocolNode,rdf:value,ProtocolValue,Graph ),
          ( rdf( ProtocolNode, aixm:uom, ProtocolUOM, Graph ); rdf( ProtocolNode, fixm:uom, ProtocolUOM, Graph ); rdf( ProtocolNode, plain:uom, ProtocolUOM, Graph ) ),
          Protocol=xval(ProtocolValue,ProtocolUOM)
        );
        (
          rdf( ProtocolNode,aixm:nilReason, ProtocolNilReason, Graph ),
          Protocol=nil(ProtocolNilReason)
        );
        (
          rdf( ProtocolNode,gml:indeterminatePosition, ProtocolIndeterminate, Graph ),
          Protocol=indeterminate(ProtocolIndeterminate)
        )
      )
  )
  ,(
    ( EMail='$null$',
      \+ rdf( OnlineContact,aixm:'eMail',_EMail,Graph )
    );
  ( rdf( OnlineContact,aixm:'eMail',EMailNode,Graph )),
      (
        (
          rdf(EMailNode,rdf:value,EMailValue,Graph),
         \+ ( rdf( EMailNode, aixm:uom, _EMailUOM, Graph ); rdf( EMailNode, fixm:uom, _EMailUOM, Graph ); rdf( EMailNode, plain:uom, _EMailUOM, Graph ) ),
          EMail=val(EMailValue)
        );
        (
          rdf( EMailNode,rdf:value,EMailValue,Graph ),
          ( rdf( EMailNode, aixm:uom, EMailUOM, Graph ); rdf( EMailNode, fixm:uom, EMailUOM, Graph ); rdf( EMailNode, plain:uom, EMailUOM, Graph ) ),
          EMail=xval(EMailValue,EMailUOM)
        );
        (
          rdf( EMailNode,aixm:nilReason, EMailNilReason, Graph ),
          EMail=nil(EMailNilReason)
        );
        (
          rdf( EMailNode,gml:indeterminatePosition, EMailIndeterminate, Graph ),
          EMail=indeterminate(EMailIndeterminate)
        )
      )
  ) .

fixm_StructuredPostalAddress(Graph, StructuredPostalAddress) :-
  rdf(StructuredPostalAddress,rdf:type,fixm:'StructuredPostalAddress',Graph) .

fixm_AircraftPosition(Graph, AircraftPosition, Altitude, Position, PositionTime, Track, ActualSpeed, NextPosition, ReportSource, FollowingPosition) :-
  rdf(AircraftPosition,rdf:type,fixm:'AircraftPosition',Graph)
  ,(
    ( Altitude='$null$',
      \+ rdf( AircraftPosition,fixm:'altitude',_Altitude,Graph )
    );
  ( rdf( AircraftPosition,fixm:'altitude',AltitudeNode,Graph )),
      (
        (
          rdf(AltitudeNode,rdf:value,AltitudeValue,Graph),
         \+ ( rdf( AltitudeNode, aixm:uom, _AltitudeUOM, Graph ); rdf( AltitudeNode, fixm:uom, _AltitudeUOM, Graph ); rdf( AltitudeNode, plain:uom, _AltitudeUOM, Graph ) ),
          Altitude=val(AltitudeValue)
        );
        (
          rdf( AltitudeNode,rdf:value,AltitudeValue,Graph ),
          ( rdf( AltitudeNode, aixm:uom, AltitudeUOM, Graph ); rdf( AltitudeNode, fixm:uom, AltitudeUOM, Graph ); rdf( AltitudeNode, plain:uom, AltitudeUOM, Graph ) ),
          Altitude=xval(AltitudeValue,AltitudeUOM)
        );
        (
          rdf( AltitudeNode,aixm:nilReason, AltitudeNilReason, Graph ),
          Altitude=nil(AltitudeNilReason)
        );
        (
          rdf( AltitudeNode,gml:indeterminatePosition, AltitudeIndeterminate, Graph ),
          Altitude=indeterminate(AltitudeIndeterminate)
        )
      )
  )
  ,( ( Position='$null$',
    \+ rdf( AircraftPosition,fixm:'position', _Position, Graph  )
   ; rdf(AircraftPosition,fixm:'position', Position, Graph ) )
  )
  ,(
    ( PositionTime='$null$',
      \+ rdf( AircraftPosition,fixm:'positionTime',_PositionTime,Graph )
    );
  ( rdf( AircraftPosition,fixm:'positionTime',PositionTimeNode,Graph )),
      (
        (
          rdf(PositionTimeNode,rdf:value,PositionTimeValue,Graph),
         \+ ( rdf( PositionTimeNode, aixm:uom, _PositionTimeUOM, Graph ); rdf( PositionTimeNode, fixm:uom, _PositionTimeUOM, Graph ); rdf( PositionTimeNode, plain:uom, _PositionTimeUOM, Graph ) ),
          PositionTime=val(PositionTimeValue)
        );
        (
          rdf( PositionTimeNode,rdf:value,PositionTimeValue,Graph ),
          ( rdf( PositionTimeNode, aixm:uom, PositionTimeUOM, Graph ); rdf( PositionTimeNode, fixm:uom, PositionTimeUOM, Graph ); rdf( PositionTimeNode, plain:uom, PositionTimeUOM, Graph ) ),
          PositionTime=xval(PositionTimeValue,PositionTimeUOM)
        );
        (
          rdf( PositionTimeNode,aixm:nilReason, PositionTimeNilReason, Graph ),
          PositionTime=nil(PositionTimeNilReason)
        );
        (
          rdf( PositionTimeNode,gml:indeterminatePosition, PositionTimeIndeterminate, Graph ),
          PositionTime=indeterminate(PositionTimeIndeterminate)
        )
      )
  )
  ,(
    ( Track='$null$',
      \+ rdf( AircraftPosition,fixm:'track',_Track,Graph )
    );
  ( rdf( AircraftPosition,fixm:'track',TrackNode,Graph )),
      (
        (
          rdf(TrackNode,rdf:value,TrackValue,Graph),
         \+ ( rdf( TrackNode, aixm:uom, _TrackUOM, Graph ); rdf( TrackNode, fixm:uom, _TrackUOM, Graph ); rdf( TrackNode, plain:uom, _TrackUOM, Graph ) ),
          Track=val(TrackValue)
        );
        (
          rdf( TrackNode,rdf:value,TrackValue,Graph ),
          ( rdf( TrackNode, aixm:uom, TrackUOM, Graph ); rdf( TrackNode, fixm:uom, TrackUOM, Graph ); rdf( TrackNode, plain:uom, TrackUOM, Graph ) ),
          Track=xval(TrackValue,TrackUOM)
        );
        (
          rdf( TrackNode,aixm:nilReason, TrackNilReason, Graph ),
          Track=nil(TrackNilReason)
        );
        (
          rdf( TrackNode,gml:indeterminatePosition, TrackIndeterminate, Graph ),
          Track=indeterminate(TrackIndeterminate)
        )
      )
  )
  ,( ( ActualSpeed='$null$',
    \+ rdf( AircraftPosition,fixm:'actualSpeed', _ActualSpeed, Graph  )
   ; rdf(AircraftPosition,fixm:'actualSpeed', ActualSpeed, Graph ) )
  )
  ,( ( NextPosition='$null$',
    \+ rdf( AircraftPosition,fixm:'nextPosition', _NextPosition, Graph  )
   ; rdf(AircraftPosition,fixm:'nextPosition', NextPosition, Graph ) )
  )
  ,(
    ( ReportSource='$null$',
      \+ rdf( AircraftPosition,fixm:'reportSource',_ReportSource,Graph )
    );
  ( rdf( AircraftPosition,fixm:'reportSource',ReportSourceNode,Graph )),
      (
        (
          rdf(ReportSourceNode,rdf:value,ReportSourceValue,Graph),
         \+ ( rdf( ReportSourceNode, aixm:uom, _ReportSourceUOM, Graph ); rdf( ReportSourceNode, fixm:uom, _ReportSourceUOM, Graph ); rdf( ReportSourceNode, plain:uom, _ReportSourceUOM, Graph ) ),
          ReportSource=val(ReportSourceValue)
        );
        (
          rdf( ReportSourceNode,rdf:value,ReportSourceValue,Graph ),
          ( rdf( ReportSourceNode, aixm:uom, ReportSourceUOM, Graph ); rdf( ReportSourceNode, fixm:uom, ReportSourceUOM, Graph ); rdf( ReportSourceNode, plain:uom, ReportSourceUOM, Graph ) ),
          ReportSource=xval(ReportSourceValue,ReportSourceUOM)
        );
        (
          rdf( ReportSourceNode,aixm:nilReason, ReportSourceNilReason, Graph ),
          ReportSource=nil(ReportSourceNilReason)
        );
        (
          rdf( ReportSourceNode,gml:indeterminatePosition, ReportSourceIndeterminate, Graph ),
          ReportSource=indeterminate(ReportSourceIndeterminate)
        )
      )
  )
  ,( ( FollowingPosition='$null$',
    \+ rdf( AircraftPosition,fixm:'followingPosition', _FollowingPosition, Graph  )
   ; rdf(AircraftPosition,fixm:'followingPosition', FollowingPosition, Graph ) )
  ) .

aixm_AirportHeliportUsage(Graph, AirportHeliportUsage, Operation) :-
  rdf(AirportHeliportUsage,rdf:type,aixm:'AirportHeliportUsage',Graph)
  ,(
    ( Operation='$null$',
      \+ rdf( AirportHeliportUsage,aixm:'operation',_Operation,Graph )
    );
  ( rdf( AirportHeliportUsage,aixm:'operation',OperationNode,Graph )),
      (
        (
          rdf(OperationNode,rdf:value,OperationValue,Graph),
         \+ ( rdf( OperationNode, aixm:uom, _OperationUOM, Graph ); rdf( OperationNode, fixm:uom, _OperationUOM, Graph ); rdf( OperationNode, plain:uom, _OperationUOM, Graph ) ),
          Operation=val(OperationValue)
        );
        (
          rdf( OperationNode,rdf:value,OperationValue,Graph ),
          ( rdf( OperationNode, aixm:uom, OperationUOM, Graph ); rdf( OperationNode, fixm:uom, OperationUOM, Graph ); rdf( OperationNode, plain:uom, OperationUOM, Graph ) ),
          Operation=xval(OperationValue,OperationUOM)
        );
        (
          rdf( OperationNode,aixm:nilReason, OperationNilReason, Graph ),
          Operation=nil(OperationNilReason)
        );
        (
          rdf( OperationNode,gml:indeterminatePosition, OperationIndeterminate, Graph ),
          Operation=indeterminate(OperationIndeterminate)
        )
      )
  ) .

aixm_Timesheet(Graph, Timesheet, TimeReference, StartDate, EndDate, Day, DayTil, StartTime, StartEvent, StartTimeRelativeEvent, StartEventInterpretation, EndTime, EndEvent, EndTimeRelativeEvent, EndEventInterpretation, DaylightSavingAdjust, Excluded, Annotation) :-
  rdf(Timesheet,rdf:type,aixm:'Timesheet',Graph)
  ,(
    ( TimeReference='$null$',
      \+ rdf( Timesheet,aixm:'timeReference',_TimeReference,Graph )
    );
  ( rdf( Timesheet,aixm:'timeReference',TimeReferenceNode,Graph )),
      (
        (
          rdf(TimeReferenceNode,rdf:value,TimeReferenceValue,Graph),
         \+ ( rdf( TimeReferenceNode, aixm:uom, _TimeReferenceUOM, Graph ); rdf( TimeReferenceNode, fixm:uom, _TimeReferenceUOM, Graph ); rdf( TimeReferenceNode, plain:uom, _TimeReferenceUOM, Graph ) ),
          TimeReference=val(TimeReferenceValue)
        );
        (
          rdf( TimeReferenceNode,rdf:value,TimeReferenceValue,Graph ),
          ( rdf( TimeReferenceNode, aixm:uom, TimeReferenceUOM, Graph ); rdf( TimeReferenceNode, fixm:uom, TimeReferenceUOM, Graph ); rdf( TimeReferenceNode, plain:uom, TimeReferenceUOM, Graph ) ),
          TimeReference=xval(TimeReferenceValue,TimeReferenceUOM)
        );
        (
          rdf( TimeReferenceNode,aixm:nilReason, TimeReferenceNilReason, Graph ),
          TimeReference=nil(TimeReferenceNilReason)
        );
        (
          rdf( TimeReferenceNode,gml:indeterminatePosition, TimeReferenceIndeterminate, Graph ),
          TimeReference=indeterminate(TimeReferenceIndeterminate)
        )
      )
  )
  ,(
    ( StartDate='$null$',
      \+ rdf( Timesheet,aixm:'startDate',_StartDate,Graph )
    );
  ( rdf( Timesheet,aixm:'startDate',StartDateNode,Graph )),
      (
        (
          rdf(StartDateNode,rdf:value,StartDateValue,Graph),
         \+ ( rdf( StartDateNode, aixm:uom, _StartDateUOM, Graph ); rdf( StartDateNode, fixm:uom, _StartDateUOM, Graph ); rdf( StartDateNode, plain:uom, _StartDateUOM, Graph ) ),
          StartDate=val(StartDateValue)
        );
        (
          rdf( StartDateNode,rdf:value,StartDateValue,Graph ),
          ( rdf( StartDateNode, aixm:uom, StartDateUOM, Graph ); rdf( StartDateNode, fixm:uom, StartDateUOM, Graph ); rdf( StartDateNode, plain:uom, StartDateUOM, Graph ) ),
          StartDate=xval(StartDateValue,StartDateUOM)
        );
        (
          rdf( StartDateNode,aixm:nilReason, StartDateNilReason, Graph ),
          StartDate=nil(StartDateNilReason)
        );
        (
          rdf( StartDateNode,gml:indeterminatePosition, StartDateIndeterminate, Graph ),
          StartDate=indeterminate(StartDateIndeterminate)
        )
      )
  )
  ,(
    ( EndDate='$null$',
      \+ rdf( Timesheet,aixm:'endDate',_EndDate,Graph )
    );
  ( rdf( Timesheet,aixm:'endDate',EndDateNode,Graph )),
      (
        (
          rdf(EndDateNode,rdf:value,EndDateValue,Graph),
         \+ ( rdf( EndDateNode, aixm:uom, _EndDateUOM, Graph ); rdf( EndDateNode, fixm:uom, _EndDateUOM, Graph ); rdf( EndDateNode, plain:uom, _EndDateUOM, Graph ) ),
          EndDate=val(EndDateValue)
        );
        (
          rdf( EndDateNode,rdf:value,EndDateValue,Graph ),
          ( rdf( EndDateNode, aixm:uom, EndDateUOM, Graph ); rdf( EndDateNode, fixm:uom, EndDateUOM, Graph ); rdf( EndDateNode, plain:uom, EndDateUOM, Graph ) ),
          EndDate=xval(EndDateValue,EndDateUOM)
        );
        (
          rdf( EndDateNode,aixm:nilReason, EndDateNilReason, Graph ),
          EndDate=nil(EndDateNilReason)
        );
        (
          rdf( EndDateNode,gml:indeterminatePosition, EndDateIndeterminate, Graph ),
          EndDate=indeterminate(EndDateIndeterminate)
        )
      )
  )
  ,(
    ( Day='$null$',
      \+ rdf( Timesheet,aixm:'day',_Day,Graph )
    );
  ( rdf( Timesheet,aixm:'day',DayNode,Graph )),
      (
        (
          rdf(DayNode,rdf:value,DayValue,Graph),
         \+ ( rdf( DayNode, aixm:uom, _DayUOM, Graph ); rdf( DayNode, fixm:uom, _DayUOM, Graph ); rdf( DayNode, plain:uom, _DayUOM, Graph ) ),
          Day=val(DayValue)
        );
        (
          rdf( DayNode,rdf:value,DayValue,Graph ),
          ( rdf( DayNode, aixm:uom, DayUOM, Graph ); rdf( DayNode, fixm:uom, DayUOM, Graph ); rdf( DayNode, plain:uom, DayUOM, Graph ) ),
          Day=xval(DayValue,DayUOM)
        );
        (
          rdf( DayNode,aixm:nilReason, DayNilReason, Graph ),
          Day=nil(DayNilReason)
        );
        (
          rdf( DayNode,gml:indeterminatePosition, DayIndeterminate, Graph ),
          Day=indeterminate(DayIndeterminate)
        )
      )
  )
  ,(
    ( DayTil='$null$',
      \+ rdf( Timesheet,aixm:'dayTil',_DayTil,Graph )
    );
  ( rdf( Timesheet,aixm:'dayTil',DayTilNode,Graph )),
      (
        (
          rdf(DayTilNode,rdf:value,DayTilValue,Graph),
         \+ ( rdf( DayTilNode, aixm:uom, _DayTilUOM, Graph ); rdf( DayTilNode, fixm:uom, _DayTilUOM, Graph ); rdf( DayTilNode, plain:uom, _DayTilUOM, Graph ) ),
          DayTil=val(DayTilValue)
        );
        (
          rdf( DayTilNode,rdf:value,DayTilValue,Graph ),
          ( rdf( DayTilNode, aixm:uom, DayTilUOM, Graph ); rdf( DayTilNode, fixm:uom, DayTilUOM, Graph ); rdf( DayTilNode, plain:uom, DayTilUOM, Graph ) ),
          DayTil=xval(DayTilValue,DayTilUOM)
        );
        (
          rdf( DayTilNode,aixm:nilReason, DayTilNilReason, Graph ),
          DayTil=nil(DayTilNilReason)
        );
        (
          rdf( DayTilNode,gml:indeterminatePosition, DayTilIndeterminate, Graph ),
          DayTil=indeterminate(DayTilIndeterminate)
        )
      )
  )
  ,(
    ( StartTime='$null$',
      \+ rdf( Timesheet,aixm:'startTime',_StartTime,Graph )
    );
  ( rdf( Timesheet,aixm:'startTime',StartTimeNode,Graph )),
      (
        (
          rdf(StartTimeNode,rdf:value,StartTimeValue,Graph),
         \+ ( rdf( StartTimeNode, aixm:uom, _StartTimeUOM, Graph ); rdf( StartTimeNode, fixm:uom, _StartTimeUOM, Graph ); rdf( StartTimeNode, plain:uom, _StartTimeUOM, Graph ) ),
          StartTime=val(StartTimeValue)
        );
        (
          rdf( StartTimeNode,rdf:value,StartTimeValue,Graph ),
          ( rdf( StartTimeNode, aixm:uom, StartTimeUOM, Graph ); rdf( StartTimeNode, fixm:uom, StartTimeUOM, Graph ); rdf( StartTimeNode, plain:uom, StartTimeUOM, Graph ) ),
          StartTime=xval(StartTimeValue,StartTimeUOM)
        );
        (
          rdf( StartTimeNode,aixm:nilReason, StartTimeNilReason, Graph ),
          StartTime=nil(StartTimeNilReason)
        );
        (
          rdf( StartTimeNode,gml:indeterminatePosition, StartTimeIndeterminate, Graph ),
          StartTime=indeterminate(StartTimeIndeterminate)
        )
      )
  )
  ,(
    ( StartEvent='$null$',
      \+ rdf( Timesheet,aixm:'startEvent',_StartEvent,Graph )
    );
  ( rdf( Timesheet,aixm:'startEvent',StartEventNode,Graph )),
      (
        (
          rdf(StartEventNode,rdf:value,StartEventValue,Graph),
         \+ ( rdf( StartEventNode, aixm:uom, _StartEventUOM, Graph ); rdf( StartEventNode, fixm:uom, _StartEventUOM, Graph ); rdf( StartEventNode, plain:uom, _StartEventUOM, Graph ) ),
          StartEvent=val(StartEventValue)
        );
        (
          rdf( StartEventNode,rdf:value,StartEventValue,Graph ),
          ( rdf( StartEventNode, aixm:uom, StartEventUOM, Graph ); rdf( StartEventNode, fixm:uom, StartEventUOM, Graph ); rdf( StartEventNode, plain:uom, StartEventUOM, Graph ) ),
          StartEvent=xval(StartEventValue,StartEventUOM)
        );
        (
          rdf( StartEventNode,aixm:nilReason, StartEventNilReason, Graph ),
          StartEvent=nil(StartEventNilReason)
        );
        (
          rdf( StartEventNode,gml:indeterminatePosition, StartEventIndeterminate, Graph ),
          StartEvent=indeterminate(StartEventIndeterminate)
        )
      )
  )
  ,(
    ( StartTimeRelativeEvent='$null$',
      \+ rdf( Timesheet,aixm:'startTimeRelativeEvent',_StartTimeRelativeEvent,Graph )
    );
  ( rdf( Timesheet,aixm:'startTimeRelativeEvent',StartTimeRelativeEventNode,Graph )),
      (
        (
          rdf(StartTimeRelativeEventNode,rdf:value,StartTimeRelativeEventValue,Graph),
         \+ ( rdf( StartTimeRelativeEventNode, aixm:uom, _StartTimeRelativeEventUOM, Graph ); rdf( StartTimeRelativeEventNode, fixm:uom, _StartTimeRelativeEventUOM, Graph ); rdf( StartTimeRelativeEventNode, plain:uom, _StartTimeRelativeEventUOM, Graph ) ),
          StartTimeRelativeEvent=val(StartTimeRelativeEventValue)
        );
        (
          rdf( StartTimeRelativeEventNode,rdf:value,StartTimeRelativeEventValue,Graph ),
          ( rdf( StartTimeRelativeEventNode, aixm:uom, StartTimeRelativeEventUOM, Graph ); rdf( StartTimeRelativeEventNode, fixm:uom, StartTimeRelativeEventUOM, Graph ); rdf( StartTimeRelativeEventNode, plain:uom, StartTimeRelativeEventUOM, Graph ) ),
          StartTimeRelativeEvent=xval(StartTimeRelativeEventValue,StartTimeRelativeEventUOM)
        );
        (
          rdf( StartTimeRelativeEventNode,aixm:nilReason, StartTimeRelativeEventNilReason, Graph ),
          StartTimeRelativeEvent=nil(StartTimeRelativeEventNilReason)
        );
        (
          rdf( StartTimeRelativeEventNode,gml:indeterminatePosition, StartTimeRelativeEventIndeterminate, Graph ),
          StartTimeRelativeEvent=indeterminate(StartTimeRelativeEventIndeterminate)
        )
      )
  )
  ,(
    ( StartEventInterpretation='$null$',
      \+ rdf( Timesheet,aixm:'startEventInterpretation',_StartEventInterpretation,Graph )
    );
  ( rdf( Timesheet,aixm:'startEventInterpretation',StartEventInterpretationNode,Graph )),
      (
        (
          rdf(StartEventInterpretationNode,rdf:value,StartEventInterpretationValue,Graph),
         \+ ( rdf( StartEventInterpretationNode, aixm:uom, _StartEventInterpretationUOM, Graph ); rdf( StartEventInterpretationNode, fixm:uom, _StartEventInterpretationUOM, Graph ); rdf( StartEventInterpretationNode, plain:uom, _StartEventInterpretationUOM, Graph ) ),
          StartEventInterpretation=val(StartEventInterpretationValue)
        );
        (
          rdf( StartEventInterpretationNode,rdf:value,StartEventInterpretationValue,Graph ),
          ( rdf( StartEventInterpretationNode, aixm:uom, StartEventInterpretationUOM, Graph ); rdf( StartEventInterpretationNode, fixm:uom, StartEventInterpretationUOM, Graph ); rdf( StartEventInterpretationNode, plain:uom, StartEventInterpretationUOM, Graph ) ),
          StartEventInterpretation=xval(StartEventInterpretationValue,StartEventInterpretationUOM)
        );
        (
          rdf( StartEventInterpretationNode,aixm:nilReason, StartEventInterpretationNilReason, Graph ),
          StartEventInterpretation=nil(StartEventInterpretationNilReason)
        );
        (
          rdf( StartEventInterpretationNode,gml:indeterminatePosition, StartEventInterpretationIndeterminate, Graph ),
          StartEventInterpretation=indeterminate(StartEventInterpretationIndeterminate)
        )
      )
  )
  ,(
    ( EndTime='$null$',
      \+ rdf( Timesheet,aixm:'endTime',_EndTime,Graph )
    );
  ( rdf( Timesheet,aixm:'endTime',EndTimeNode,Graph )),
      (
        (
          rdf(EndTimeNode,rdf:value,EndTimeValue,Graph),
         \+ ( rdf( EndTimeNode, aixm:uom, _EndTimeUOM, Graph ); rdf( EndTimeNode, fixm:uom, _EndTimeUOM, Graph ); rdf( EndTimeNode, plain:uom, _EndTimeUOM, Graph ) ),
          EndTime=val(EndTimeValue)
        );
        (
          rdf( EndTimeNode,rdf:value,EndTimeValue,Graph ),
          ( rdf( EndTimeNode, aixm:uom, EndTimeUOM, Graph ); rdf( EndTimeNode, fixm:uom, EndTimeUOM, Graph ); rdf( EndTimeNode, plain:uom, EndTimeUOM, Graph ) ),
          EndTime=xval(EndTimeValue,EndTimeUOM)
        );
        (
          rdf( EndTimeNode,aixm:nilReason, EndTimeNilReason, Graph ),
          EndTime=nil(EndTimeNilReason)
        );
        (
          rdf( EndTimeNode,gml:indeterminatePosition, EndTimeIndeterminate, Graph ),
          EndTime=indeterminate(EndTimeIndeterminate)
        )
      )
  )
  ,(
    ( EndEvent='$null$',
      \+ rdf( Timesheet,aixm:'endEvent',_EndEvent,Graph )
    );
  ( rdf( Timesheet,aixm:'endEvent',EndEventNode,Graph )),
      (
        (
          rdf(EndEventNode,rdf:value,EndEventValue,Graph),
         \+ ( rdf( EndEventNode, aixm:uom, _EndEventUOM, Graph ); rdf( EndEventNode, fixm:uom, _EndEventUOM, Graph ); rdf( EndEventNode, plain:uom, _EndEventUOM, Graph ) ),
          EndEvent=val(EndEventValue)
        );
        (
          rdf( EndEventNode,rdf:value,EndEventValue,Graph ),
          ( rdf( EndEventNode, aixm:uom, EndEventUOM, Graph ); rdf( EndEventNode, fixm:uom, EndEventUOM, Graph ); rdf( EndEventNode, plain:uom, EndEventUOM, Graph ) ),
          EndEvent=xval(EndEventValue,EndEventUOM)
        );
        (
          rdf( EndEventNode,aixm:nilReason, EndEventNilReason, Graph ),
          EndEvent=nil(EndEventNilReason)
        );
        (
          rdf( EndEventNode,gml:indeterminatePosition, EndEventIndeterminate, Graph ),
          EndEvent=indeterminate(EndEventIndeterminate)
        )
      )
  )
  ,(
    ( EndTimeRelativeEvent='$null$',
      \+ rdf( Timesheet,aixm:'endTimeRelativeEvent',_EndTimeRelativeEvent,Graph )
    );
  ( rdf( Timesheet,aixm:'endTimeRelativeEvent',EndTimeRelativeEventNode,Graph )),
      (
        (
          rdf(EndTimeRelativeEventNode,rdf:value,EndTimeRelativeEventValue,Graph),
         \+ ( rdf( EndTimeRelativeEventNode, aixm:uom, _EndTimeRelativeEventUOM, Graph ); rdf( EndTimeRelativeEventNode, fixm:uom, _EndTimeRelativeEventUOM, Graph ); rdf( EndTimeRelativeEventNode, plain:uom, _EndTimeRelativeEventUOM, Graph ) ),
          EndTimeRelativeEvent=val(EndTimeRelativeEventValue)
        );
        (
          rdf( EndTimeRelativeEventNode,rdf:value,EndTimeRelativeEventValue,Graph ),
          ( rdf( EndTimeRelativeEventNode, aixm:uom, EndTimeRelativeEventUOM, Graph ); rdf( EndTimeRelativeEventNode, fixm:uom, EndTimeRelativeEventUOM, Graph ); rdf( EndTimeRelativeEventNode, plain:uom, EndTimeRelativeEventUOM, Graph ) ),
          EndTimeRelativeEvent=xval(EndTimeRelativeEventValue,EndTimeRelativeEventUOM)
        );
        (
          rdf( EndTimeRelativeEventNode,aixm:nilReason, EndTimeRelativeEventNilReason, Graph ),
          EndTimeRelativeEvent=nil(EndTimeRelativeEventNilReason)
        );
        (
          rdf( EndTimeRelativeEventNode,gml:indeterminatePosition, EndTimeRelativeEventIndeterminate, Graph ),
          EndTimeRelativeEvent=indeterminate(EndTimeRelativeEventIndeterminate)
        )
      )
  )
  ,(
    ( EndEventInterpretation='$null$',
      \+ rdf( Timesheet,aixm:'endEventInterpretation',_EndEventInterpretation,Graph )
    );
  ( rdf( Timesheet,aixm:'endEventInterpretation',EndEventInterpretationNode,Graph )),
      (
        (
          rdf(EndEventInterpretationNode,rdf:value,EndEventInterpretationValue,Graph),
         \+ ( rdf( EndEventInterpretationNode, aixm:uom, _EndEventInterpretationUOM, Graph ); rdf( EndEventInterpretationNode, fixm:uom, _EndEventInterpretationUOM, Graph ); rdf( EndEventInterpretationNode, plain:uom, _EndEventInterpretationUOM, Graph ) ),
          EndEventInterpretation=val(EndEventInterpretationValue)
        );
        (
          rdf( EndEventInterpretationNode,rdf:value,EndEventInterpretationValue,Graph ),
          ( rdf( EndEventInterpretationNode, aixm:uom, EndEventInterpretationUOM, Graph ); rdf( EndEventInterpretationNode, fixm:uom, EndEventInterpretationUOM, Graph ); rdf( EndEventInterpretationNode, plain:uom, EndEventInterpretationUOM, Graph ) ),
          EndEventInterpretation=xval(EndEventInterpretationValue,EndEventInterpretationUOM)
        );
        (
          rdf( EndEventInterpretationNode,aixm:nilReason, EndEventInterpretationNilReason, Graph ),
          EndEventInterpretation=nil(EndEventInterpretationNilReason)
        );
        (
          rdf( EndEventInterpretationNode,gml:indeterminatePosition, EndEventInterpretationIndeterminate, Graph ),
          EndEventInterpretation=indeterminate(EndEventInterpretationIndeterminate)
        )
      )
  )
  ,(
    ( DaylightSavingAdjust='$null$',
      \+ rdf( Timesheet,aixm:'daylightSavingAdjust',_DaylightSavingAdjust,Graph )
    );
  ( rdf( Timesheet,aixm:'daylightSavingAdjust',DaylightSavingAdjustNode,Graph )),
      (
        (
          rdf(DaylightSavingAdjustNode,rdf:value,DaylightSavingAdjustValue,Graph),
         \+ ( rdf( DaylightSavingAdjustNode, aixm:uom, _DaylightSavingAdjustUOM, Graph ); rdf( DaylightSavingAdjustNode, fixm:uom, _DaylightSavingAdjustUOM, Graph ); rdf( DaylightSavingAdjustNode, plain:uom, _DaylightSavingAdjustUOM, Graph ) ),
          DaylightSavingAdjust=val(DaylightSavingAdjustValue)
        );
        (
          rdf( DaylightSavingAdjustNode,rdf:value,DaylightSavingAdjustValue,Graph ),
          ( rdf( DaylightSavingAdjustNode, aixm:uom, DaylightSavingAdjustUOM, Graph ); rdf( DaylightSavingAdjustNode, fixm:uom, DaylightSavingAdjustUOM, Graph ); rdf( DaylightSavingAdjustNode, plain:uom, DaylightSavingAdjustUOM, Graph ) ),
          DaylightSavingAdjust=xval(DaylightSavingAdjustValue,DaylightSavingAdjustUOM)
        );
        (
          rdf( DaylightSavingAdjustNode,aixm:nilReason, DaylightSavingAdjustNilReason, Graph ),
          DaylightSavingAdjust=nil(DaylightSavingAdjustNilReason)
        );
        (
          rdf( DaylightSavingAdjustNode,gml:indeterminatePosition, DaylightSavingAdjustIndeterminate, Graph ),
          DaylightSavingAdjust=indeterminate(DaylightSavingAdjustIndeterminate)
        )
      )
  )
  ,(
    ( Excluded='$null$',
      \+ rdf( Timesheet,aixm:'excluded',_Excluded,Graph )
    );
  ( rdf( Timesheet,aixm:'excluded',ExcludedNode,Graph )),
      (
        (
          rdf(ExcludedNode,rdf:value,ExcludedValue,Graph),
         \+ ( rdf( ExcludedNode, aixm:uom, _ExcludedUOM, Graph ); rdf( ExcludedNode, fixm:uom, _ExcludedUOM, Graph ); rdf( ExcludedNode, plain:uom, _ExcludedUOM, Graph ) ),
          Excluded=val(ExcludedValue)
        );
        (
          rdf( ExcludedNode,rdf:value,ExcludedValue,Graph ),
          ( rdf( ExcludedNode, aixm:uom, ExcludedUOM, Graph ); rdf( ExcludedNode, fixm:uom, ExcludedUOM, Graph ); rdf( ExcludedNode, plain:uom, ExcludedUOM, Graph ) ),
          Excluded=xval(ExcludedValue,ExcludedUOM)
        );
        (
          rdf( ExcludedNode,aixm:nilReason, ExcludedNilReason, Graph ),
          Excluded=nil(ExcludedNilReason)
        );
        (
          rdf( ExcludedNode,gml:indeterminatePosition, ExcludedIndeterminate, Graph ),
          Excluded=indeterminate(ExcludedIndeterminate)
        )
      )
  )
  ,findall(A, rdf(Timesheet,aixm:'annotation',A,Graph), Annotation) .

gml_SurfacePatch(Graph, SurfacePatch) :-
  subClassOf(T,gml:'SurfacePatch')
  ,rdf(SurfacePatch,rdf:type,T,Graph) .

fixm_MultiTime(Graph, MultiTime, Actual, Estimated) :-
  subClassOf(T,fixm:'MultiTime')
  ,rdf(MultiTime,rdf:type,T,Graph)
  ,( ( Actual='$null$',
    \+ rdf( MultiTime,fixm:'actual', _Actual, Graph  )
   ; rdf(MultiTime,fixm:'actual', Actual, Graph ) )
  )
  ,( ( Estimated='$null$',
    \+ rdf( MultiTime,fixm:'estimated', _Estimated, Graph  )
   ; rdf(MultiTime,fixm:'estimated', Estimated, Graph ) )
  ) .

aixm_FlightCharacteristic(Graph, FlightCharacteristic, Type, Rule, Status, Military, Origin, Purpose, Annotation) :-
  rdf(FlightCharacteristic,rdf:type,aixm:'FlightCharacteristic',Graph)
  ,(
    ( Type='$null$',
      \+ rdf( FlightCharacteristic,aixm:'type',_Type,Graph )
    );
  ( rdf( FlightCharacteristic,aixm:'type',TypeNode,Graph )),
      (
        (
          rdf(TypeNode,rdf:value,TypeValue,Graph),
         \+ ( rdf( TypeNode, aixm:uom, _TypeUOM, Graph ); rdf( TypeNode, fixm:uom, _TypeUOM, Graph ); rdf( TypeNode, plain:uom, _TypeUOM, Graph ) ),
          Type=val(TypeValue)
        );
        (
          rdf( TypeNode,rdf:value,TypeValue,Graph ),
          ( rdf( TypeNode, aixm:uom, TypeUOM, Graph ); rdf( TypeNode, fixm:uom, TypeUOM, Graph ); rdf( TypeNode, plain:uom, TypeUOM, Graph ) ),
          Type=xval(TypeValue,TypeUOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, TypeNilReason, Graph ),
          Type=nil(TypeNilReason)
        );
        (
          rdf( TypeNode,gml:indeterminatePosition, TypeIndeterminate, Graph ),
          Type=indeterminate(TypeIndeterminate)
        )
      )
  )
  ,(
    ( Rule='$null$',
      \+ rdf( FlightCharacteristic,aixm:'rule',_Rule,Graph )
    );
  ( rdf( FlightCharacteristic,aixm:'rule',RuleNode,Graph )),
      (
        (
          rdf(RuleNode,rdf:value,RuleValue,Graph),
         \+ ( rdf( RuleNode, aixm:uom, _RuleUOM, Graph ); rdf( RuleNode, fixm:uom, _RuleUOM, Graph ); rdf( RuleNode, plain:uom, _RuleUOM, Graph ) ),
          Rule=val(RuleValue)
        );
        (
          rdf( RuleNode,rdf:value,RuleValue,Graph ),
          ( rdf( RuleNode, aixm:uom, RuleUOM, Graph ); rdf( RuleNode, fixm:uom, RuleUOM, Graph ); rdf( RuleNode, plain:uom, RuleUOM, Graph ) ),
          Rule=xval(RuleValue,RuleUOM)
        );
        (
          rdf( RuleNode,aixm:nilReason, RuleNilReason, Graph ),
          Rule=nil(RuleNilReason)
        );
        (
          rdf( RuleNode,gml:indeterminatePosition, RuleIndeterminate, Graph ),
          Rule=indeterminate(RuleIndeterminate)
        )
      )
  )
  ,(
    ( Status='$null$',
      \+ rdf( FlightCharacteristic,aixm:'status',_Status,Graph )
    );
  ( rdf( FlightCharacteristic,aixm:'status',StatusNode,Graph )),
      (
        (
          rdf(StatusNode,rdf:value,StatusValue,Graph),
         \+ ( rdf( StatusNode, aixm:uom, _StatusUOM, Graph ); rdf( StatusNode, fixm:uom, _StatusUOM, Graph ); rdf( StatusNode, plain:uom, _StatusUOM, Graph ) ),
          Status=val(StatusValue)
        );
        (
          rdf( StatusNode,rdf:value,StatusValue,Graph ),
          ( rdf( StatusNode, aixm:uom, StatusUOM, Graph ); rdf( StatusNode, fixm:uom, StatusUOM, Graph ); rdf( StatusNode, plain:uom, StatusUOM, Graph ) ),
          Status=xval(StatusValue,StatusUOM)
        );
        (
          rdf( StatusNode,aixm:nilReason, StatusNilReason, Graph ),
          Status=nil(StatusNilReason)
        );
        (
          rdf( StatusNode,gml:indeterminatePosition, StatusIndeterminate, Graph ),
          Status=indeterminate(StatusIndeterminate)
        )
      )
  )
  ,(
    ( Military='$null$',
      \+ rdf( FlightCharacteristic,aixm:'military',_Military,Graph )
    );
  ( rdf( FlightCharacteristic,aixm:'military',MilitaryNode,Graph )),
      (
        (
          rdf(MilitaryNode,rdf:value,MilitaryValue,Graph),
         \+ ( rdf( MilitaryNode, aixm:uom, _MilitaryUOM, Graph ); rdf( MilitaryNode, fixm:uom, _MilitaryUOM, Graph ); rdf( MilitaryNode, plain:uom, _MilitaryUOM, Graph ) ),
          Military=val(MilitaryValue)
        );
        (
          rdf( MilitaryNode,rdf:value,MilitaryValue,Graph ),
          ( rdf( MilitaryNode, aixm:uom, MilitaryUOM, Graph ); rdf( MilitaryNode, fixm:uom, MilitaryUOM, Graph ); rdf( MilitaryNode, plain:uom, MilitaryUOM, Graph ) ),
          Military=xval(MilitaryValue,MilitaryUOM)
        );
        (
          rdf( MilitaryNode,aixm:nilReason, MilitaryNilReason, Graph ),
          Military=nil(MilitaryNilReason)
        );
        (
          rdf( MilitaryNode,gml:indeterminatePosition, MilitaryIndeterminate, Graph ),
          Military=indeterminate(MilitaryIndeterminate)
        )
      )
  )
  ,(
    ( Origin='$null$',
      \+ rdf( FlightCharacteristic,aixm:'origin',_Origin,Graph )
    );
  ( rdf( FlightCharacteristic,aixm:'origin',OriginNode,Graph )),
      (
        (
          rdf(OriginNode,rdf:value,OriginValue,Graph),
         \+ ( rdf( OriginNode, aixm:uom, _OriginUOM, Graph ); rdf( OriginNode, fixm:uom, _OriginUOM, Graph ); rdf( OriginNode, plain:uom, _OriginUOM, Graph ) ),
          Origin=val(OriginValue)
        );
        (
          rdf( OriginNode,rdf:value,OriginValue,Graph ),
          ( rdf( OriginNode, aixm:uom, OriginUOM, Graph ); rdf( OriginNode, fixm:uom, OriginUOM, Graph ); rdf( OriginNode, plain:uom, OriginUOM, Graph ) ),
          Origin=xval(OriginValue,OriginUOM)
        );
        (
          rdf( OriginNode,aixm:nilReason, OriginNilReason, Graph ),
          Origin=nil(OriginNilReason)
        );
        (
          rdf( OriginNode,gml:indeterminatePosition, OriginIndeterminate, Graph ),
          Origin=indeterminate(OriginIndeterminate)
        )
      )
  )
  ,(
    ( Purpose='$null$',
      \+ rdf( FlightCharacteristic,aixm:'purpose',_Purpose,Graph )
    );
  ( rdf( FlightCharacteristic,aixm:'purpose',PurposeNode,Graph )),
      (
        (
          rdf(PurposeNode,rdf:value,PurposeValue,Graph),
         \+ ( rdf( PurposeNode, aixm:uom, _PurposeUOM, Graph ); rdf( PurposeNode, fixm:uom, _PurposeUOM, Graph ); rdf( PurposeNode, plain:uom, _PurposeUOM, Graph ) ),
          Purpose=val(PurposeValue)
        );
        (
          rdf( PurposeNode,rdf:value,PurposeValue,Graph ),
          ( rdf( PurposeNode, aixm:uom, PurposeUOM, Graph ); rdf( PurposeNode, fixm:uom, PurposeUOM, Graph ); rdf( PurposeNode, plain:uom, PurposeUOM, Graph ) ),
          Purpose=xval(PurposeValue,PurposeUOM)
        );
        (
          rdf( PurposeNode,aixm:nilReason, PurposeNilReason, Graph ),
          Purpose=nil(PurposeNilReason)
        );
        (
          rdf( PurposeNode,gml:indeterminatePosition, PurposeIndeterminate, Graph ),
          Purpose=indeterminate(PurposeIndeterminate)
        )
      )
  )
  ,findall(A, rdf(FlightCharacteristic,aixm:'annotation',A,Graph), Annotation) .

fixm_Provenance(Graph, Provenance, Timestamp, Centre, Source, System) :-
  rdf(Provenance,rdf:type,fixm:'Provenance',Graph)
  ,(
    ( Timestamp='$null$',
      \+ rdf( Provenance,fixm:'timestamp',_Timestamp,Graph )
    );
  ( rdf( Provenance,fixm:'timestamp',TimestampNode,Graph )),
      (
        (
          rdf(TimestampNode,rdf:value,TimestampValue,Graph),
         \+ ( rdf( TimestampNode, aixm:uom, _TimestampUOM, Graph ); rdf( TimestampNode, fixm:uom, _TimestampUOM, Graph ); rdf( TimestampNode, plain:uom, _TimestampUOM, Graph ) ),
          Timestamp=val(TimestampValue)
        );
        (
          rdf( TimestampNode,rdf:value,TimestampValue,Graph ),
          ( rdf( TimestampNode, aixm:uom, TimestampUOM, Graph ); rdf( TimestampNode, fixm:uom, TimestampUOM, Graph ); rdf( TimestampNode, plain:uom, TimestampUOM, Graph ) ),
          Timestamp=xval(TimestampValue,TimestampUOM)
        );
        (
          rdf( TimestampNode,aixm:nilReason, TimestampNilReason, Graph ),
          Timestamp=nil(TimestampNilReason)
        );
        (
          rdf( TimestampNode,gml:indeterminatePosition, TimestampIndeterminate, Graph ),
          Timestamp=indeterminate(TimestampIndeterminate)
        )
      )
  )
  ,(
    ( Centre='$null$',
      \+ rdf( Provenance,fixm:'centre',_Centre,Graph )
    );
  ( rdf( Provenance,fixm:'centre',CentreNode,Graph )),
      (
        (
          rdf(CentreNode,rdf:value,CentreValue,Graph),
         \+ ( rdf( CentreNode, aixm:uom, _CentreUOM, Graph ); rdf( CentreNode, fixm:uom, _CentreUOM, Graph ); rdf( CentreNode, plain:uom, _CentreUOM, Graph ) ),
          Centre=val(CentreValue)
        );
        (
          rdf( CentreNode,rdf:value,CentreValue,Graph ),
          ( rdf( CentreNode, aixm:uom, CentreUOM, Graph ); rdf( CentreNode, fixm:uom, CentreUOM, Graph ); rdf( CentreNode, plain:uom, CentreUOM, Graph ) ),
          Centre=xval(CentreValue,CentreUOM)
        );
        (
          rdf( CentreNode,aixm:nilReason, CentreNilReason, Graph ),
          Centre=nil(CentreNilReason)
        );
        (
          rdf( CentreNode,gml:indeterminatePosition, CentreIndeterminate, Graph ),
          Centre=indeterminate(CentreIndeterminate)
        )
      )
  )
  ,(
    ( Source='$null$',
      \+ rdf( Provenance,fixm:'source',_Source,Graph )
    );
  ( rdf( Provenance,fixm:'source',SourceNode,Graph )),
      (
        (
          rdf(SourceNode,rdf:value,SourceValue,Graph),
         \+ ( rdf( SourceNode, aixm:uom, _SourceUOM, Graph ); rdf( SourceNode, fixm:uom, _SourceUOM, Graph ); rdf( SourceNode, plain:uom, _SourceUOM, Graph ) ),
          Source=val(SourceValue)
        );
        (
          rdf( SourceNode,rdf:value,SourceValue,Graph ),
          ( rdf( SourceNode, aixm:uom, SourceUOM, Graph ); rdf( SourceNode, fixm:uom, SourceUOM, Graph ); rdf( SourceNode, plain:uom, SourceUOM, Graph ) ),
          Source=xval(SourceValue,SourceUOM)
        );
        (
          rdf( SourceNode,aixm:nilReason, SourceNilReason, Graph ),
          Source=nil(SourceNilReason)
        );
        (
          rdf( SourceNode,gml:indeterminatePosition, SourceIndeterminate, Graph ),
          Source=indeterminate(SourceIndeterminate)
        )
      )
  )
  ,(
    ( System='$null$',
      \+ rdf( Provenance,fixm:'system',_System,Graph )
    );
  ( rdf( Provenance,fixm:'system',SystemNode,Graph )),
      (
        (
          rdf(SystemNode,rdf:value,SystemValue,Graph),
         \+ ( rdf( SystemNode, aixm:uom, _SystemUOM, Graph ); rdf( SystemNode, fixm:uom, _SystemUOM, Graph ); rdf( SystemNode, plain:uom, _SystemUOM, Graph ) ),
          System=val(SystemValue)
        );
        (
          rdf( SystemNode,rdf:value,SystemValue,Graph ),
          ( rdf( SystemNode, aixm:uom, SystemUOM, Graph ); rdf( SystemNode, fixm:uom, SystemUOM, Graph ); rdf( SystemNode, plain:uom, SystemUOM, Graph ) ),
          System=xval(SystemValue,SystemUOM)
        );
        (
          rdf( SystemNode,aixm:nilReason, SystemNilReason, Graph ),
          System=nil(SystemNilReason)
        );
        (
          rdf( SystemNode,gml:indeterminatePosition, SystemIndeterminate, Graph ),
          System=indeterminate(SystemIndeterminate)
        )
      )
  ) .

aixm_AirportHeliport(Graph, AirportHeliport, TimeSlice) :-
  rdf(AirportHeliport,rdf:type,aixm:'AirportHeliport',Graph)
  ,findall(A, rdf(AirportHeliport,aixm:'timeSlice',A,Graph), TimeSlice) .

fixm_TrajectoryPoint(Graph, TrajectoryPoint, AltimeterSetting, PredictedAirspeed, PredictedGroundspeed, MetData, Point, TrajectoryChange, TrajectoryChangeType, ReferencePoint) :-
  subClassOf(T,fixm:'TrajectoryPoint')
  ,rdf(TrajectoryPoint,rdf:type,T,Graph)
  ,(
    ( AltimeterSetting='$null$',
      \+ rdf( TrajectoryPoint,fixm:'altimeterSetting',_AltimeterSetting,Graph )
    );
  ( rdf( TrajectoryPoint,fixm:'altimeterSetting',AltimeterSettingNode,Graph )),
      (
        (
          rdf(AltimeterSettingNode,rdf:value,AltimeterSettingValue,Graph),
         \+ ( rdf( AltimeterSettingNode, aixm:uom, _AltimeterSettingUOM, Graph ); rdf( AltimeterSettingNode, fixm:uom, _AltimeterSettingUOM, Graph ); rdf( AltimeterSettingNode, plain:uom, _AltimeterSettingUOM, Graph ) ),
          AltimeterSetting=val(AltimeterSettingValue)
        );
        (
          rdf( AltimeterSettingNode,rdf:value,AltimeterSettingValue,Graph ),
          ( rdf( AltimeterSettingNode, aixm:uom, AltimeterSettingUOM, Graph ); rdf( AltimeterSettingNode, fixm:uom, AltimeterSettingUOM, Graph ); rdf( AltimeterSettingNode, plain:uom, AltimeterSettingUOM, Graph ) ),
          AltimeterSetting=xval(AltimeterSettingValue,AltimeterSettingUOM)
        );
        (
          rdf( AltimeterSettingNode,aixm:nilReason, AltimeterSettingNilReason, Graph ),
          AltimeterSetting=nil(AltimeterSettingNilReason)
        );
        (
          rdf( AltimeterSettingNode,gml:indeterminatePosition, AltimeterSettingIndeterminate, Graph ),
          AltimeterSetting=indeterminate(AltimeterSettingIndeterminate)
        )
      )
  )
  ,(
    ( PredictedAirspeed='$null$',
      \+ rdf( TrajectoryPoint,fixm:'predictedAirspeed',_PredictedAirspeed,Graph )
    );
  ( rdf( TrajectoryPoint,fixm:'predictedAirspeed',PredictedAirspeedNode,Graph )),
      (
        (
          rdf(PredictedAirspeedNode,rdf:value,PredictedAirspeedValue,Graph),
         \+ ( rdf( PredictedAirspeedNode, aixm:uom, _PredictedAirspeedUOM, Graph ); rdf( PredictedAirspeedNode, fixm:uom, _PredictedAirspeedUOM, Graph ); rdf( PredictedAirspeedNode, plain:uom, _PredictedAirspeedUOM, Graph ) ),
          PredictedAirspeed=val(PredictedAirspeedValue)
        );
        (
          rdf( PredictedAirspeedNode,rdf:value,PredictedAirspeedValue,Graph ),
          ( rdf( PredictedAirspeedNode, aixm:uom, PredictedAirspeedUOM, Graph ); rdf( PredictedAirspeedNode, fixm:uom, PredictedAirspeedUOM, Graph ); rdf( PredictedAirspeedNode, plain:uom, PredictedAirspeedUOM, Graph ) ),
          PredictedAirspeed=xval(PredictedAirspeedValue,PredictedAirspeedUOM)
        );
        (
          rdf( PredictedAirspeedNode,aixm:nilReason, PredictedAirspeedNilReason, Graph ),
          PredictedAirspeed=nil(PredictedAirspeedNilReason)
        );
        (
          rdf( PredictedAirspeedNode,gml:indeterminatePosition, PredictedAirspeedIndeterminate, Graph ),
          PredictedAirspeed=indeterminate(PredictedAirspeedIndeterminate)
        )
      )
  )
  ,(
    ( PredictedGroundspeed='$null$',
      \+ rdf( TrajectoryPoint,fixm:'predictedGroundspeed',_PredictedGroundspeed,Graph )
    );
  ( rdf( TrajectoryPoint,fixm:'predictedGroundspeed',PredictedGroundspeedNode,Graph )),
      (
        (
          rdf(PredictedGroundspeedNode,rdf:value,PredictedGroundspeedValue,Graph),
         \+ ( rdf( PredictedGroundspeedNode, aixm:uom, _PredictedGroundspeedUOM, Graph ); rdf( PredictedGroundspeedNode, fixm:uom, _PredictedGroundspeedUOM, Graph ); rdf( PredictedGroundspeedNode, plain:uom, _PredictedGroundspeedUOM, Graph ) ),
          PredictedGroundspeed=val(PredictedGroundspeedValue)
        );
        (
          rdf( PredictedGroundspeedNode,rdf:value,PredictedGroundspeedValue,Graph ),
          ( rdf( PredictedGroundspeedNode, aixm:uom, PredictedGroundspeedUOM, Graph ); rdf( PredictedGroundspeedNode, fixm:uom, PredictedGroundspeedUOM, Graph ); rdf( PredictedGroundspeedNode, plain:uom, PredictedGroundspeedUOM, Graph ) ),
          PredictedGroundspeed=xval(PredictedGroundspeedValue,PredictedGroundspeedUOM)
        );
        (
          rdf( PredictedGroundspeedNode,aixm:nilReason, PredictedGroundspeedNilReason, Graph ),
          PredictedGroundspeed=nil(PredictedGroundspeedNilReason)
        );
        (
          rdf( PredictedGroundspeedNode,gml:indeterminatePosition, PredictedGroundspeedIndeterminate, Graph ),
          PredictedGroundspeed=indeterminate(PredictedGroundspeedIndeterminate)
        )
      )
  )
  ,( ( MetData='$null$',
    \+ rdf( TrajectoryPoint,fixm:'metData', _MetData, Graph  )
   ; rdf(TrajectoryPoint,fixm:'metData', MetData, Graph ) )
  )
  ,( ( Point='$null$',
    \+ rdf( TrajectoryPoint,fixm:'point', _Point, Graph  )
   ; rdf(TrajectoryPoint,fixm:'point', Point, Graph ) )
  )
  ,findall(A, rdf(TrajectoryPoint,fixm:'trajectoryChange',A,Graph), TrajectoryChange)
  ,findall(A, rdf(TrajectoryPoint,fixm:'trajectoryChangeType',A,Graph), TrajectoryChangeType)
  ,( ( ReferencePoint='$null$',
    \+ rdf( TrajectoryPoint,fixm:'referencePoint', _ReferencePoint, Graph  )
   ; rdf(TrajectoryPoint,fixm:'referencePoint', ReferencePoint, Graph ) )
  ) .

fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier, DistanceFromTakeOff, EfplEstimatedSpeed, ElapsedTime, GrossWeight, TrajectoryPointType, TrajectoryPointRole, InboundSegment) :-
  rdf(EfplTrajectoryPoint,rdf:type,fixm:'EfplTrajectoryPoint',Graph)
  ,( ( AerodromeIdentifier='$null$',
    \+ rdf( EfplTrajectoryPoint,fixm:'aerodromeIdentifier', _AerodromeIdentifier, Graph  )
   ; rdf(EfplTrajectoryPoint,fixm:'aerodromeIdentifier', AerodromeIdentifier, Graph ) )
  )
  ,(
    ( DistanceFromTakeOff='$null$',
      \+ rdf( EfplTrajectoryPoint,fixm:'distanceFromTakeOff',_DistanceFromTakeOff,Graph )
    );
  ( rdf( EfplTrajectoryPoint,fixm:'distanceFromTakeOff',DistanceFromTakeOffNode,Graph )),
      (
        (
          rdf(DistanceFromTakeOffNode,rdf:value,DistanceFromTakeOffValue,Graph),
         \+ ( rdf( DistanceFromTakeOffNode, aixm:uom, _DistanceFromTakeOffUOM, Graph ); rdf( DistanceFromTakeOffNode, fixm:uom, _DistanceFromTakeOffUOM, Graph ); rdf( DistanceFromTakeOffNode, plain:uom, _DistanceFromTakeOffUOM, Graph ) ),
          DistanceFromTakeOff=val(DistanceFromTakeOffValue)
        );
        (
          rdf( DistanceFromTakeOffNode,rdf:value,DistanceFromTakeOffValue,Graph ),
          ( rdf( DistanceFromTakeOffNode, aixm:uom, DistanceFromTakeOffUOM, Graph ); rdf( DistanceFromTakeOffNode, fixm:uom, DistanceFromTakeOffUOM, Graph ); rdf( DistanceFromTakeOffNode, plain:uom, DistanceFromTakeOffUOM, Graph ) ),
          DistanceFromTakeOff=xval(DistanceFromTakeOffValue,DistanceFromTakeOffUOM)
        );
        (
          rdf( DistanceFromTakeOffNode,aixm:nilReason, DistanceFromTakeOffNilReason, Graph ),
          DistanceFromTakeOff=nil(DistanceFromTakeOffNilReason)
        );
        (
          rdf( DistanceFromTakeOffNode,gml:indeterminatePosition, DistanceFromTakeOffIndeterminate, Graph ),
          DistanceFromTakeOff=indeterminate(DistanceFromTakeOffIndeterminate)
        )
      )
  )
  ,(
    ( EfplEstimatedSpeed='$null$',
      \+ rdf( EfplTrajectoryPoint,fixm:'efplEstimatedSpeed',_EfplEstimatedSpeed,Graph )
    );
  ( rdf( EfplTrajectoryPoint,fixm:'efplEstimatedSpeed',EfplEstimatedSpeedNode,Graph )),
      (
        (
          rdf(EfplEstimatedSpeedNode,rdf:value,EfplEstimatedSpeedValue,Graph),
         \+ ( rdf( EfplEstimatedSpeedNode, aixm:uom, _EfplEstimatedSpeedUOM, Graph ); rdf( EfplEstimatedSpeedNode, fixm:uom, _EfplEstimatedSpeedUOM, Graph ); rdf( EfplEstimatedSpeedNode, plain:uom, _EfplEstimatedSpeedUOM, Graph ) ),
          EfplEstimatedSpeed=val(EfplEstimatedSpeedValue)
        );
        (
          rdf( EfplEstimatedSpeedNode,rdf:value,EfplEstimatedSpeedValue,Graph ),
          ( rdf( EfplEstimatedSpeedNode, aixm:uom, EfplEstimatedSpeedUOM, Graph ); rdf( EfplEstimatedSpeedNode, fixm:uom, EfplEstimatedSpeedUOM, Graph ); rdf( EfplEstimatedSpeedNode, plain:uom, EfplEstimatedSpeedUOM, Graph ) ),
          EfplEstimatedSpeed=xval(EfplEstimatedSpeedValue,EfplEstimatedSpeedUOM)
        );
        (
          rdf( EfplEstimatedSpeedNode,aixm:nilReason, EfplEstimatedSpeedNilReason, Graph ),
          EfplEstimatedSpeed=nil(EfplEstimatedSpeedNilReason)
        );
        (
          rdf( EfplEstimatedSpeedNode,gml:indeterminatePosition, EfplEstimatedSpeedIndeterminate, Graph ),
          EfplEstimatedSpeed=indeterminate(EfplEstimatedSpeedIndeterminate)
        )
      )
  )
  ,(
    ( ElapsedTime='$null$',
      \+ rdf( EfplTrajectoryPoint,fixm:'elapsedTime',_ElapsedTime,Graph )
    );
  ( rdf( EfplTrajectoryPoint,fixm:'elapsedTime',ElapsedTimeNode,Graph )),
      (
        (
          rdf(ElapsedTimeNode,rdf:value,ElapsedTimeValue,Graph),
         \+ ( rdf( ElapsedTimeNode, aixm:uom, _ElapsedTimeUOM, Graph ); rdf( ElapsedTimeNode, fixm:uom, _ElapsedTimeUOM, Graph ); rdf( ElapsedTimeNode, plain:uom, _ElapsedTimeUOM, Graph ) ),
          ElapsedTime=val(ElapsedTimeValue)
        );
        (
          rdf( ElapsedTimeNode,rdf:value,ElapsedTimeValue,Graph ),
          ( rdf( ElapsedTimeNode, aixm:uom, ElapsedTimeUOM, Graph ); rdf( ElapsedTimeNode, fixm:uom, ElapsedTimeUOM, Graph ); rdf( ElapsedTimeNode, plain:uom, ElapsedTimeUOM, Graph ) ),
          ElapsedTime=xval(ElapsedTimeValue,ElapsedTimeUOM)
        );
        (
          rdf( ElapsedTimeNode,aixm:nilReason, ElapsedTimeNilReason, Graph ),
          ElapsedTime=nil(ElapsedTimeNilReason)
        );
        (
          rdf( ElapsedTimeNode,gml:indeterminatePosition, ElapsedTimeIndeterminate, Graph ),
          ElapsedTime=indeterminate(ElapsedTimeIndeterminate)
        )
      )
  )
  ,(
    ( GrossWeight='$null$',
      \+ rdf( EfplTrajectoryPoint,fixm:'grossWeight',_GrossWeight,Graph )
    );
  ( rdf( EfplTrajectoryPoint,fixm:'grossWeight',GrossWeightNode,Graph )),
      (
        (
          rdf(GrossWeightNode,rdf:value,GrossWeightValue,Graph),
         \+ ( rdf( GrossWeightNode, aixm:uom, _GrossWeightUOM, Graph ); rdf( GrossWeightNode, fixm:uom, _GrossWeightUOM, Graph ); rdf( GrossWeightNode, plain:uom, _GrossWeightUOM, Graph ) ),
          GrossWeight=val(GrossWeightValue)
        );
        (
          rdf( GrossWeightNode,rdf:value,GrossWeightValue,Graph ),
          ( rdf( GrossWeightNode, aixm:uom, GrossWeightUOM, Graph ); rdf( GrossWeightNode, fixm:uom, GrossWeightUOM, Graph ); rdf( GrossWeightNode, plain:uom, GrossWeightUOM, Graph ) ),
          GrossWeight=xval(GrossWeightValue,GrossWeightUOM)
        );
        (
          rdf( GrossWeightNode,aixm:nilReason, GrossWeightNilReason, Graph ),
          GrossWeight=nil(GrossWeightNilReason)
        );
        (
          rdf( GrossWeightNode,gml:indeterminatePosition, GrossWeightIndeterminate, Graph ),
          GrossWeight=indeterminate(GrossWeightIndeterminate)
        )
      )
  )
  ,(
    ( TrajectoryPointType='$null$',
      \+ rdf( EfplTrajectoryPoint,fixm:'trajectoryPointType',_TrajectoryPointType,Graph )
    );
  ( rdf( EfplTrajectoryPoint,fixm:'trajectoryPointType',TrajectoryPointTypeNode,Graph )),
      (
        (
          rdf(TrajectoryPointTypeNode,rdf:value,TrajectoryPointTypeValue,Graph),
         \+ ( rdf( TrajectoryPointTypeNode, aixm:uom, _TrajectoryPointTypeUOM, Graph ); rdf( TrajectoryPointTypeNode, fixm:uom, _TrajectoryPointTypeUOM, Graph ); rdf( TrajectoryPointTypeNode, plain:uom, _TrajectoryPointTypeUOM, Graph ) ),
          TrajectoryPointType=val(TrajectoryPointTypeValue)
        );
        (
          rdf( TrajectoryPointTypeNode,rdf:value,TrajectoryPointTypeValue,Graph ),
          ( rdf( TrajectoryPointTypeNode, aixm:uom, TrajectoryPointTypeUOM, Graph ); rdf( TrajectoryPointTypeNode, fixm:uom, TrajectoryPointTypeUOM, Graph ); rdf( TrajectoryPointTypeNode, plain:uom, TrajectoryPointTypeUOM, Graph ) ),
          TrajectoryPointType=xval(TrajectoryPointTypeValue,TrajectoryPointTypeUOM)
        );
        (
          rdf( TrajectoryPointTypeNode,aixm:nilReason, TrajectoryPointTypeNilReason, Graph ),
          TrajectoryPointType=nil(TrajectoryPointTypeNilReason)
        );
        (
          rdf( TrajectoryPointTypeNode,gml:indeterminatePosition, TrajectoryPointTypeIndeterminate, Graph ),
          TrajectoryPointType=indeterminate(TrajectoryPointTypeIndeterminate)
        )
      )
  )
  ,( ( TrajectoryPointRole='$null$',
    \+ rdf( EfplTrajectoryPoint,fixm:'trajectoryPointRole', _TrajectoryPointRole, Graph  )
   ; rdf(EfplTrajectoryPoint,fixm:'trajectoryPointRole', TrajectoryPointRole, Graph ) )
  )
  ,( ( InboundSegment='$null$',
    \+ rdf( EfplTrajectoryPoint,fixm:'inboundSegment', _InboundSegment, Graph  )
   ; rdf(EfplTrajectoryPoint,fixm:'inboundSegment', InboundSegment, Graph ) )
  ) .

fixm_Temperatures(Graph, Temperatures, ControlTemperature, EmergencyTemperature, FlashpointTemperature) :-
  rdf(Temperatures,rdf:type,fixm:'Temperatures',Graph)
  ,(
    ( ControlTemperature='$null$',
      \+ rdf( Temperatures,fixm:'controlTemperature',_ControlTemperature,Graph )
    );
  ( rdf( Temperatures,fixm:'controlTemperature',ControlTemperatureNode,Graph )),
      (
        (
          rdf(ControlTemperatureNode,rdf:value,ControlTemperatureValue,Graph),
         \+ ( rdf( ControlTemperatureNode, aixm:uom, _ControlTemperatureUOM, Graph ); rdf( ControlTemperatureNode, fixm:uom, _ControlTemperatureUOM, Graph ); rdf( ControlTemperatureNode, plain:uom, _ControlTemperatureUOM, Graph ) ),
          ControlTemperature=val(ControlTemperatureValue)
        );
        (
          rdf( ControlTemperatureNode,rdf:value,ControlTemperatureValue,Graph ),
          ( rdf( ControlTemperatureNode, aixm:uom, ControlTemperatureUOM, Graph ); rdf( ControlTemperatureNode, fixm:uom, ControlTemperatureUOM, Graph ); rdf( ControlTemperatureNode, plain:uom, ControlTemperatureUOM, Graph ) ),
          ControlTemperature=xval(ControlTemperatureValue,ControlTemperatureUOM)
        );
        (
          rdf( ControlTemperatureNode,aixm:nilReason, ControlTemperatureNilReason, Graph ),
          ControlTemperature=nil(ControlTemperatureNilReason)
        );
        (
          rdf( ControlTemperatureNode,gml:indeterminatePosition, ControlTemperatureIndeterminate, Graph ),
          ControlTemperature=indeterminate(ControlTemperatureIndeterminate)
        )
      )
  )
  ,(
    ( EmergencyTemperature='$null$',
      \+ rdf( Temperatures,fixm:'emergencyTemperature',_EmergencyTemperature,Graph )
    );
  ( rdf( Temperatures,fixm:'emergencyTemperature',EmergencyTemperatureNode,Graph )),
      (
        (
          rdf(EmergencyTemperatureNode,rdf:value,EmergencyTemperatureValue,Graph),
         \+ ( rdf( EmergencyTemperatureNode, aixm:uom, _EmergencyTemperatureUOM, Graph ); rdf( EmergencyTemperatureNode, fixm:uom, _EmergencyTemperatureUOM, Graph ); rdf( EmergencyTemperatureNode, plain:uom, _EmergencyTemperatureUOM, Graph ) ),
          EmergencyTemperature=val(EmergencyTemperatureValue)
        );
        (
          rdf( EmergencyTemperatureNode,rdf:value,EmergencyTemperatureValue,Graph ),
          ( rdf( EmergencyTemperatureNode, aixm:uom, EmergencyTemperatureUOM, Graph ); rdf( EmergencyTemperatureNode, fixm:uom, EmergencyTemperatureUOM, Graph ); rdf( EmergencyTemperatureNode, plain:uom, EmergencyTemperatureUOM, Graph ) ),
          EmergencyTemperature=xval(EmergencyTemperatureValue,EmergencyTemperatureUOM)
        );
        (
          rdf( EmergencyTemperatureNode,aixm:nilReason, EmergencyTemperatureNilReason, Graph ),
          EmergencyTemperature=nil(EmergencyTemperatureNilReason)
        );
        (
          rdf( EmergencyTemperatureNode,gml:indeterminatePosition, EmergencyTemperatureIndeterminate, Graph ),
          EmergencyTemperature=indeterminate(EmergencyTemperatureIndeterminate)
        )
      )
  )
  ,(
    ( FlashpointTemperature='$null$',
      \+ rdf( Temperatures,fixm:'flashpointTemperature',_FlashpointTemperature,Graph )
    );
  ( rdf( Temperatures,fixm:'flashpointTemperature',FlashpointTemperatureNode,Graph )),
      (
        (
          rdf(FlashpointTemperatureNode,rdf:value,FlashpointTemperatureValue,Graph),
         \+ ( rdf( FlashpointTemperatureNode, aixm:uom, _FlashpointTemperatureUOM, Graph ); rdf( FlashpointTemperatureNode, fixm:uom, _FlashpointTemperatureUOM, Graph ); rdf( FlashpointTemperatureNode, plain:uom, _FlashpointTemperatureUOM, Graph ) ),
          FlashpointTemperature=val(FlashpointTemperatureValue)
        );
        (
          rdf( FlashpointTemperatureNode,rdf:value,FlashpointTemperatureValue,Graph ),
          ( rdf( FlashpointTemperatureNode, aixm:uom, FlashpointTemperatureUOM, Graph ); rdf( FlashpointTemperatureNode, fixm:uom, FlashpointTemperatureUOM, Graph ); rdf( FlashpointTemperatureNode, plain:uom, FlashpointTemperatureUOM, Graph ) ),
          FlashpointTemperature=xval(FlashpointTemperatureValue,FlashpointTemperatureUOM)
        );
        (
          rdf( FlashpointTemperatureNode,aixm:nilReason, FlashpointTemperatureNilReason, Graph ),
          FlashpointTemperature=nil(FlashpointTemperatureNilReason)
        );
        (
          rdf( FlashpointTemperatureNode,gml:indeterminatePosition, FlashpointTemperatureIndeterminate, Graph ),
          FlashpointTemperature=indeterminate(FlashpointTemperatureIndeterminate)
        )
      )
  ) .

fixm_TrajectorySegment(Graph, TrajectorySegment, SegmentIdentifier, SegmentType) :-
  rdf(TrajectorySegment,rdf:type,fixm:'TrajectorySegment',Graph)
  ,(
    ( SegmentIdentifier='$null$',
      \+ rdf( TrajectorySegment,fixm:'segmentIdentifier',_SegmentIdentifier,Graph )
    );
  ( rdf( TrajectorySegment,fixm:'segmentIdentifier',SegmentIdentifierNode,Graph )),
      (
        (
          rdf(SegmentIdentifierNode,rdf:value,SegmentIdentifierValue,Graph),
         \+ ( rdf( SegmentIdentifierNode, aixm:uom, _SegmentIdentifierUOM, Graph ); rdf( SegmentIdentifierNode, fixm:uom, _SegmentIdentifierUOM, Graph ); rdf( SegmentIdentifierNode, plain:uom, _SegmentIdentifierUOM, Graph ) ),
          SegmentIdentifier=val(SegmentIdentifierValue)
        );
        (
          rdf( SegmentIdentifierNode,rdf:value,SegmentIdentifierValue,Graph ),
          ( rdf( SegmentIdentifierNode, aixm:uom, SegmentIdentifierUOM, Graph ); rdf( SegmentIdentifierNode, fixm:uom, SegmentIdentifierUOM, Graph ); rdf( SegmentIdentifierNode, plain:uom, SegmentIdentifierUOM, Graph ) ),
          SegmentIdentifier=xval(SegmentIdentifierValue,SegmentIdentifierUOM)
        );
        (
          rdf( SegmentIdentifierNode,aixm:nilReason, SegmentIdentifierNilReason, Graph ),
          SegmentIdentifier=nil(SegmentIdentifierNilReason)
        );
        (
          rdf( SegmentIdentifierNode,gml:indeterminatePosition, SegmentIdentifierIndeterminate, Graph ),
          SegmentIdentifier=indeterminate(SegmentIdentifierIndeterminate)
        )
      )
  )
  ,(
    ( SegmentType='$null$',
      \+ rdf( TrajectorySegment,fixm:'segmentType',_SegmentType,Graph )
    );
  ( rdf( TrajectorySegment,fixm:'segmentType',SegmentTypeNode,Graph )),
      (
        (
          rdf(SegmentTypeNode,rdf:value,SegmentTypeValue,Graph),
         \+ ( rdf( SegmentTypeNode, aixm:uom, _SegmentTypeUOM, Graph ); rdf( SegmentTypeNode, fixm:uom, _SegmentTypeUOM, Graph ); rdf( SegmentTypeNode, plain:uom, _SegmentTypeUOM, Graph ) ),
          SegmentType=val(SegmentTypeValue)
        );
        (
          rdf( SegmentTypeNode,rdf:value,SegmentTypeValue,Graph ),
          ( rdf( SegmentTypeNode, aixm:uom, SegmentTypeUOM, Graph ); rdf( SegmentTypeNode, fixm:uom, SegmentTypeUOM, Graph ); rdf( SegmentTypeNode, plain:uom, SegmentTypeUOM, Graph ) ),
          SegmentType=xval(SegmentTypeValue,SegmentTypeUOM)
        );
        (
          rdf( SegmentTypeNode,aixm:nilReason, SegmentTypeNilReason, Graph ),
          SegmentType=nil(SegmentTypeNilReason)
        );
        (
          rdf( SegmentTypeNode,gml:indeterminatePosition, SegmentTypeIndeterminate, Graph ),
          SegmentType=indeterminate(SegmentTypeIndeterminate)
        )
      )
  ) .

fixm_RunwayPositionAndTime(Graph, RunwayPositionAndTime, RunwayName, RunwayTime) :-
  rdf(RunwayPositionAndTime,rdf:type,fixm:'RunwayPositionAndTime',Graph)
  ,(
    ( RunwayName='$null$',
      \+ rdf( RunwayPositionAndTime,fixm:'runwayName',_RunwayName,Graph )
    );
  ( rdf( RunwayPositionAndTime,fixm:'runwayName',RunwayNameNode,Graph )),
      (
        (
          rdf(RunwayNameNode,rdf:value,RunwayNameValue,Graph),
         \+ ( rdf( RunwayNameNode, aixm:uom, _RunwayNameUOM, Graph ); rdf( RunwayNameNode, fixm:uom, _RunwayNameUOM, Graph ); rdf( RunwayNameNode, plain:uom, _RunwayNameUOM, Graph ) ),
          RunwayName=val(RunwayNameValue)
        );
        (
          rdf( RunwayNameNode,rdf:value,RunwayNameValue,Graph ),
          ( rdf( RunwayNameNode, aixm:uom, RunwayNameUOM, Graph ); rdf( RunwayNameNode, fixm:uom, RunwayNameUOM, Graph ); rdf( RunwayNameNode, plain:uom, RunwayNameUOM, Graph ) ),
          RunwayName=xval(RunwayNameValue,RunwayNameUOM)
        );
        (
          rdf( RunwayNameNode,aixm:nilReason, RunwayNameNilReason, Graph ),
          RunwayName=nil(RunwayNameNilReason)
        );
        (
          rdf( RunwayNameNode,gml:indeterminatePosition, RunwayNameIndeterminate, Graph ),
          RunwayName=indeterminate(RunwayNameIndeterminate)
        )
      )
  )
  ,( ( RunwayTime='$null$',
    \+ rdf( RunwayPositionAndTime,fixm:'runwayTime', _RunwayTime, Graph  )
   ; rdf(RunwayPositionAndTime,fixm:'runwayTime', RunwayTime, Graph ) )
  ) .

fixm_Feature(Graph, Feature, Provenance) :-
  subClassOf(T,fixm:'Feature')
  ,rdf(Feature,rdf:type,T,Graph)
  ,( ( Provenance='$null$',
    \+ rdf( Feature,fixm:'provenance', _Provenance, Graph  )
   ; rdf(Feature,fixm:'provenance', Provenance, Graph ) )
  ) .

fixm_FlightIdentification(Graph, FlightIdentification, AircraftIdentification, MajorCarrierIdentifier, MarketingCarrierFlightIdentifier) :-
  rdf(FlightIdentification,rdf:type,fixm:'FlightIdentification',Graph)
  ,(
    ( AircraftIdentification='$null$',
      \+ rdf( FlightIdentification,fixm:'aircraftIdentification',_AircraftIdentification,Graph )
    );
  ( rdf( FlightIdentification,fixm:'aircraftIdentification',AircraftIdentificationNode,Graph )),
      (
        (
          rdf(AircraftIdentificationNode,rdf:value,AircraftIdentificationValue,Graph),
         \+ ( rdf( AircraftIdentificationNode, aixm:uom, _AircraftIdentificationUOM, Graph ); rdf( AircraftIdentificationNode, fixm:uom, _AircraftIdentificationUOM, Graph ); rdf( AircraftIdentificationNode, plain:uom, _AircraftIdentificationUOM, Graph ) ),
          AircraftIdentification=val(AircraftIdentificationValue)
        );
        (
          rdf( AircraftIdentificationNode,rdf:value,AircraftIdentificationValue,Graph ),
          ( rdf( AircraftIdentificationNode, aixm:uom, AircraftIdentificationUOM, Graph ); rdf( AircraftIdentificationNode, fixm:uom, AircraftIdentificationUOM, Graph ); rdf( AircraftIdentificationNode, plain:uom, AircraftIdentificationUOM, Graph ) ),
          AircraftIdentification=xval(AircraftIdentificationValue,AircraftIdentificationUOM)
        );
        (
          rdf( AircraftIdentificationNode,aixm:nilReason, AircraftIdentificationNilReason, Graph ),
          AircraftIdentification=nil(AircraftIdentificationNilReason)
        );
        (
          rdf( AircraftIdentificationNode,gml:indeterminatePosition, AircraftIdentificationIndeterminate, Graph ),
          AircraftIdentification=indeterminate(AircraftIdentificationIndeterminate)
        )
      )
  )
  ,(
    ( MajorCarrierIdentifier='$null$',
      \+ rdf( FlightIdentification,fixm:'majorCarrierIdentifier',_MajorCarrierIdentifier,Graph )
    );
  ( rdf( FlightIdentification,fixm:'majorCarrierIdentifier',MajorCarrierIdentifierNode,Graph )),
      (
        (
          rdf(MajorCarrierIdentifierNode,rdf:value,MajorCarrierIdentifierValue,Graph),
         \+ ( rdf( MajorCarrierIdentifierNode, aixm:uom, _MajorCarrierIdentifierUOM, Graph ); rdf( MajorCarrierIdentifierNode, fixm:uom, _MajorCarrierIdentifierUOM, Graph ); rdf( MajorCarrierIdentifierNode, plain:uom, _MajorCarrierIdentifierUOM, Graph ) ),
          MajorCarrierIdentifier=val(MajorCarrierIdentifierValue)
        );
        (
          rdf( MajorCarrierIdentifierNode,rdf:value,MajorCarrierIdentifierValue,Graph ),
          ( rdf( MajorCarrierIdentifierNode, aixm:uom, MajorCarrierIdentifierUOM, Graph ); rdf( MajorCarrierIdentifierNode, fixm:uom, MajorCarrierIdentifierUOM, Graph ); rdf( MajorCarrierIdentifierNode, plain:uom, MajorCarrierIdentifierUOM, Graph ) ),
          MajorCarrierIdentifier=xval(MajorCarrierIdentifierValue,MajorCarrierIdentifierUOM)
        );
        (
          rdf( MajorCarrierIdentifierNode,aixm:nilReason, MajorCarrierIdentifierNilReason, Graph ),
          MajorCarrierIdentifier=nil(MajorCarrierIdentifierNilReason)
        );
        (
          rdf( MajorCarrierIdentifierNode,gml:indeterminatePosition, MajorCarrierIdentifierIndeterminate, Graph ),
          MajorCarrierIdentifier=indeterminate(MajorCarrierIdentifierIndeterminate)
        )
      )
  )
  ,findall(A, rdf(FlightIdentification,fixm:'marketingCarrierFlightIdentifier',A,Graph), MarketingCarrierFlightIdentifier) .

fixm_LastContact(Graph, LastContact, ContactFrequency, LastContactTime, LastContactUnit, Position) :-
  rdf(LastContact,rdf:type,fixm:'LastContact',Graph)
  ,(
    ( ContactFrequency='$null$',
      \+ rdf( LastContact,fixm:'contactFrequency',_ContactFrequency,Graph )
    );
  ( rdf( LastContact,fixm:'contactFrequency',ContactFrequencyNode,Graph )),
      (
        (
          rdf(ContactFrequencyNode,rdf:value,ContactFrequencyValue,Graph),
         \+ ( rdf( ContactFrequencyNode, aixm:uom, _ContactFrequencyUOM, Graph ); rdf( ContactFrequencyNode, fixm:uom, _ContactFrequencyUOM, Graph ); rdf( ContactFrequencyNode, plain:uom, _ContactFrequencyUOM, Graph ) ),
          ContactFrequency=val(ContactFrequencyValue)
        );
        (
          rdf( ContactFrequencyNode,rdf:value,ContactFrequencyValue,Graph ),
          ( rdf( ContactFrequencyNode, aixm:uom, ContactFrequencyUOM, Graph ); rdf( ContactFrequencyNode, fixm:uom, ContactFrequencyUOM, Graph ); rdf( ContactFrequencyNode, plain:uom, ContactFrequencyUOM, Graph ) ),
          ContactFrequency=xval(ContactFrequencyValue,ContactFrequencyUOM)
        );
        (
          rdf( ContactFrequencyNode,aixm:nilReason, ContactFrequencyNilReason, Graph ),
          ContactFrequency=nil(ContactFrequencyNilReason)
        );
        (
          rdf( ContactFrequencyNode,gml:indeterminatePosition, ContactFrequencyIndeterminate, Graph ),
          ContactFrequency=indeterminate(ContactFrequencyIndeterminate)
        )
      )
  )
  ,(
    ( LastContactTime='$null$',
      \+ rdf( LastContact,fixm:'lastContactTime',_LastContactTime,Graph )
    );
  ( rdf( LastContact,fixm:'lastContactTime',LastContactTimeNode,Graph )),
      (
        (
          rdf(LastContactTimeNode,rdf:value,LastContactTimeValue,Graph),
         \+ ( rdf( LastContactTimeNode, aixm:uom, _LastContactTimeUOM, Graph ); rdf( LastContactTimeNode, fixm:uom, _LastContactTimeUOM, Graph ); rdf( LastContactTimeNode, plain:uom, _LastContactTimeUOM, Graph ) ),
          LastContactTime=val(LastContactTimeValue)
        );
        (
          rdf( LastContactTimeNode,rdf:value,LastContactTimeValue,Graph ),
          ( rdf( LastContactTimeNode, aixm:uom, LastContactTimeUOM, Graph ); rdf( LastContactTimeNode, fixm:uom, LastContactTimeUOM, Graph ); rdf( LastContactTimeNode, plain:uom, LastContactTimeUOM, Graph ) ),
          LastContactTime=xval(LastContactTimeValue,LastContactTimeUOM)
        );
        (
          rdf( LastContactTimeNode,aixm:nilReason, LastContactTimeNilReason, Graph ),
          LastContactTime=nil(LastContactTimeNilReason)
        );
        (
          rdf( LastContactTimeNode,gml:indeterminatePosition, LastContactTimeIndeterminate, Graph ),
          LastContactTime=indeterminate(LastContactTimeIndeterminate)
        )
      )
  )
  ,(
    ( LastContactUnit='$null$',
      \+ rdf( LastContact,fixm:'lastContactUnit',_LastContactUnit,Graph )
    );
  ( rdf( LastContact,fixm:'lastContactUnit',LastContactUnitNode,Graph )),
      (
        (
          rdf(LastContactUnitNode,rdf:value,LastContactUnitValue,Graph),
         \+ ( rdf( LastContactUnitNode, aixm:uom, _LastContactUnitUOM, Graph ); rdf( LastContactUnitNode, fixm:uom, _LastContactUnitUOM, Graph ); rdf( LastContactUnitNode, plain:uom, _LastContactUnitUOM, Graph ) ),
          LastContactUnit=val(LastContactUnitValue)
        );
        (
          rdf( LastContactUnitNode,rdf:value,LastContactUnitValue,Graph ),
          ( rdf( LastContactUnitNode, aixm:uom, LastContactUnitUOM, Graph ); rdf( LastContactUnitNode, fixm:uom, LastContactUnitUOM, Graph ); rdf( LastContactUnitNode, plain:uom, LastContactUnitUOM, Graph ) ),
          LastContactUnit=xval(LastContactUnitValue,LastContactUnitUOM)
        );
        (
          rdf( LastContactUnitNode,aixm:nilReason, LastContactUnitNilReason, Graph ),
          LastContactUnit=nil(LastContactUnitNilReason)
        );
        (
          rdf( LastContactUnitNode,gml:indeterminatePosition, LastContactUnitIndeterminate, Graph ),
          LastContactUnit=indeterminate(LastContactUnitIndeterminate)
        )
      )
  )
  ,( ( Position='$null$',
    \+ rdf( LastContact,fixm:'position', _Position, Graph  )
   ; rdf(LastContact,fixm:'position', Position, Graph ) )
  ) .

fixm_ElapsedTimeLocation(Graph, ElapsedTimeLocation) :-
  rdf(ElapsedTimeLocation,rdf:type,fixm:'ElapsedTimeLocation',Graph) .

aixm_Surface(Graph, Surface, HorizontalAccuracy, Annotation) :-
  subClassOf(T,aixm:'Surface')
  ,rdf(Surface,rdf:type,T,Graph)
  ,(
    ( HorizontalAccuracy='$null$',
      \+ rdf( Surface,aixm:'horizontalAccuracy',_HorizontalAccuracy,Graph )
    );
  ( rdf( Surface,aixm:'horizontalAccuracy',HorizontalAccuracyNode,Graph )),
      (
        (
          rdf(HorizontalAccuracyNode,rdf:value,HorizontalAccuracyValue,Graph),
         \+ ( rdf( HorizontalAccuracyNode, aixm:uom, _HorizontalAccuracyUOM, Graph ); rdf( HorizontalAccuracyNode, fixm:uom, _HorizontalAccuracyUOM, Graph ); rdf( HorizontalAccuracyNode, plain:uom, _HorizontalAccuracyUOM, Graph ) ),
          HorizontalAccuracy=val(HorizontalAccuracyValue)
        );
        (
          rdf( HorizontalAccuracyNode,rdf:value,HorizontalAccuracyValue,Graph ),
          ( rdf( HorizontalAccuracyNode, aixm:uom, HorizontalAccuracyUOM, Graph ); rdf( HorizontalAccuracyNode, fixm:uom, HorizontalAccuracyUOM, Graph ); rdf( HorizontalAccuracyNode, plain:uom, HorizontalAccuracyUOM, Graph ) ),
          HorizontalAccuracy=xval(HorizontalAccuracyValue,HorizontalAccuracyUOM)
        );
        (
          rdf( HorizontalAccuracyNode,aixm:nilReason, HorizontalAccuracyNilReason, Graph ),
          HorizontalAccuracy=nil(HorizontalAccuracyNilReason)
        );
        (
          rdf( HorizontalAccuracyNode,gml:indeterminatePosition, HorizontalAccuracyIndeterminate, Graph ),
          HorizontalAccuracy=indeterminate(HorizontalAccuracyIndeterminate)
        )
      )
  )
  ,findall(A, rdf(Surface,aixm:'annotation',A,Graph), Annotation) .

gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition) :-
  rdf(TimePeriod,rdf:type,gml:'TimePeriod',Graph)
  ,(
  ( rdf( TimePeriod,gml:'beginPosition',BeginPositionNode,Graph )),
      (
        (
          rdf(BeginPositionNode,rdf:value,BeginPositionValue,Graph),
         \+ ( rdf( BeginPositionNode, aixm:uom, _BeginPositionUOM, Graph ); rdf( BeginPositionNode, fixm:uom, _BeginPositionUOM, Graph ); rdf( BeginPositionNode, plain:uom, _BeginPositionUOM, Graph ) ),
          BeginPosition=val(BeginPositionValue)
        );
        (
          rdf( BeginPositionNode,rdf:value,BeginPositionValue,Graph ),
          ( rdf( BeginPositionNode, aixm:uom, BeginPositionUOM, Graph ); rdf( BeginPositionNode, fixm:uom, BeginPositionUOM, Graph ); rdf( BeginPositionNode, plain:uom, BeginPositionUOM, Graph ) ),
          BeginPosition=xval(BeginPositionValue,BeginPositionUOM)
        );
        (
          rdf( BeginPositionNode,aixm:nilReason, BeginPositionNilReason, Graph ),
          BeginPosition=nil(BeginPositionNilReason)
        );
        (
          rdf( BeginPositionNode,gml:indeterminatePosition, BeginPositionIndeterminate, Graph ),
          BeginPosition=indeterminate(BeginPositionIndeterminate)
        )
      )
  )
  ,(
  ( rdf( TimePeriod,gml:'endPosition',EndPositionNode,Graph )),
      (
        (
          rdf(EndPositionNode,rdf:value,EndPositionValue,Graph),
         \+ ( rdf( EndPositionNode, aixm:uom, _EndPositionUOM, Graph ); rdf( EndPositionNode, fixm:uom, _EndPositionUOM, Graph ); rdf( EndPositionNode, plain:uom, _EndPositionUOM, Graph ) ),
          EndPosition=val(EndPositionValue)
        );
        (
          rdf( EndPositionNode,rdf:value,EndPositionValue,Graph ),
          ( rdf( EndPositionNode, aixm:uom, EndPositionUOM, Graph ); rdf( EndPositionNode, fixm:uom, EndPositionUOM, Graph ); rdf( EndPositionNode, plain:uom, EndPositionUOM, Graph ) ),
          EndPosition=xval(EndPositionValue,EndPositionUOM)
        );
        (
          rdf( EndPositionNode,aixm:nilReason, EndPositionNilReason, Graph ),
          EndPosition=nil(EndPositionNilReason)
        );
        (
          rdf( EndPositionNode,gml:indeterminatePosition, EndPositionIndeterminate, Graph ),
          EndPosition=indeterminate(EndPositionIndeterminate)
        )
      )
  ) .

fixm_AircraftCapabilities(Graph, AircraftCapabilities, Survival, Communication, Navigation, Surveillance, StandardCapabilities) :-
  rdf(AircraftCapabilities,rdf:type,fixm:'AircraftCapabilities',Graph)
  ,( ( Survival='$null$',
    \+ rdf( AircraftCapabilities,fixm:'survival', _Survival, Graph  )
   ; rdf(AircraftCapabilities,fixm:'survival', Survival, Graph ) )
  )
  ,( ( Communication='$null$',
    \+ rdf( AircraftCapabilities,fixm:'communication', _Communication, Graph  )
   ; rdf(AircraftCapabilities,fixm:'communication', Communication, Graph ) )
  )
  ,( ( Navigation='$null$',
    \+ rdf( AircraftCapabilities,fixm:'navigation', _Navigation, Graph  )
   ; rdf(AircraftCapabilities,fixm:'navigation', Navigation, Graph ) )
  )
  ,( ( Surveillance='$null$',
    \+ rdf( AircraftCapabilities,fixm:'surveillance', _Surveillance, Graph  )
   ; rdf(AircraftCapabilities,fixm:'surveillance', Surveillance, Graph ) )
  )
  ,(
    ( StandardCapabilities='$null$',
      \+ rdf( AircraftCapabilities,fixm:'standardCapabilities',_StandardCapabilities,Graph )
    );
  ( rdf( AircraftCapabilities,fixm:'standardCapabilities',StandardCapabilitiesNode,Graph )),
      (
        (
          rdf(StandardCapabilitiesNode,rdf:value,StandardCapabilitiesValue,Graph),
         \+ ( rdf( StandardCapabilitiesNode, aixm:uom, _StandardCapabilitiesUOM, Graph ); rdf( StandardCapabilitiesNode, fixm:uom, _StandardCapabilitiesUOM, Graph ); rdf( StandardCapabilitiesNode, plain:uom, _StandardCapabilitiesUOM, Graph ) ),
          StandardCapabilities=val(StandardCapabilitiesValue)
        );
        (
          rdf( StandardCapabilitiesNode,rdf:value,StandardCapabilitiesValue,Graph ),
          ( rdf( StandardCapabilitiesNode, aixm:uom, StandardCapabilitiesUOM, Graph ); rdf( StandardCapabilitiesNode, fixm:uom, StandardCapabilitiesUOM, Graph ); rdf( StandardCapabilitiesNode, plain:uom, StandardCapabilitiesUOM, Graph ) ),
          StandardCapabilities=xval(StandardCapabilitiesValue,StandardCapabilitiesUOM)
        );
        (
          rdf( StandardCapabilitiesNode,aixm:nilReason, StandardCapabilitiesNilReason, Graph ),
          StandardCapabilities=nil(StandardCapabilitiesNilReason)
        );
        (
          rdf( StandardCapabilitiesNode,gml:indeterminatePosition, StandardCapabilitiesIndeterminate, Graph ),
          StandardCapabilities=indeterminate(StandardCapabilitiesIndeterminate)
        )
      )
  ) .

fixm_SpeedSchedule(Graph, SpeedSchedule, InitialSpeed, SubsequentSpeed) :-
  rdf(SpeedSchedule,rdf:type,fixm:'SpeedSchedule',Graph)
  ,(
    ( InitialSpeed='$null$',
      \+ rdf( SpeedSchedule,fixm:'initialSpeed',_InitialSpeed,Graph )
    );
  ( rdf( SpeedSchedule,fixm:'initialSpeed',InitialSpeedNode,Graph )),
      (
        (
          rdf(InitialSpeedNode,rdf:value,InitialSpeedValue,Graph),
         \+ ( rdf( InitialSpeedNode, aixm:uom, _InitialSpeedUOM, Graph ); rdf( InitialSpeedNode, fixm:uom, _InitialSpeedUOM, Graph ); rdf( InitialSpeedNode, plain:uom, _InitialSpeedUOM, Graph ) ),
          InitialSpeed=val(InitialSpeedValue)
        );
        (
          rdf( InitialSpeedNode,rdf:value,InitialSpeedValue,Graph ),
          ( rdf( InitialSpeedNode, aixm:uom, InitialSpeedUOM, Graph ); rdf( InitialSpeedNode, fixm:uom, InitialSpeedUOM, Graph ); rdf( InitialSpeedNode, plain:uom, InitialSpeedUOM, Graph ) ),
          InitialSpeed=xval(InitialSpeedValue,InitialSpeedUOM)
        );
        (
          rdf( InitialSpeedNode,aixm:nilReason, InitialSpeedNilReason, Graph ),
          InitialSpeed=nil(InitialSpeedNilReason)
        );
        (
          rdf( InitialSpeedNode,gml:indeterminatePosition, InitialSpeedIndeterminate, Graph ),
          InitialSpeed=indeterminate(InitialSpeedIndeterminate)
        )
      )
  )
  ,(
    ( SubsequentSpeed='$null$',
      \+ rdf( SpeedSchedule,fixm:'subsequentSpeed',_SubsequentSpeed,Graph )
    );
  ( rdf( SpeedSchedule,fixm:'subsequentSpeed',SubsequentSpeedNode,Graph )),
      (
        (
          rdf(SubsequentSpeedNode,rdf:value,SubsequentSpeedValue,Graph),
         \+ ( rdf( SubsequentSpeedNode, aixm:uom, _SubsequentSpeedUOM, Graph ); rdf( SubsequentSpeedNode, fixm:uom, _SubsequentSpeedUOM, Graph ); rdf( SubsequentSpeedNode, plain:uom, _SubsequentSpeedUOM, Graph ) ),
          SubsequentSpeed=val(SubsequentSpeedValue)
        );
        (
          rdf( SubsequentSpeedNode,rdf:value,SubsequentSpeedValue,Graph ),
          ( rdf( SubsequentSpeedNode, aixm:uom, SubsequentSpeedUOM, Graph ); rdf( SubsequentSpeedNode, fixm:uom, SubsequentSpeedUOM, Graph ); rdf( SubsequentSpeedNode, plain:uom, SubsequentSpeedUOM, Graph ) ),
          SubsequentSpeed=xval(SubsequentSpeedValue,SubsequentSpeedUOM)
        );
        (
          rdf( SubsequentSpeedNode,aixm:nilReason, SubsequentSpeedNilReason, Graph ),
          SubsequentSpeed=nil(SubsequentSpeedNilReason)
        );
        (
          rdf( SubsequentSpeedNode,gml:indeterminatePosition, SubsequentSpeedIndeterminate, Graph ),
          SubsequentSpeed=indeterminate(SubsequentSpeedIndeterminate)
        )
      )
  ) .

aixm_OrganisationAuthorityTimeSlice(Graph, OrganisationAuthorityTimeSlice, Name, Designator, Type, Military, Annotation, Contact, RelatedOrganisationAuthority) :-
  rdf(OrganisationAuthorityTimeSlice,rdf:type,aixm:'OrganisationAuthorityTimeSlice',Graph)
  ,(
    ( Name='$null$',
      \+ rdf( OrganisationAuthorityTimeSlice,aixm:'name',_Name,Graph )
    );
  ( rdf( OrganisationAuthorityTimeSlice,aixm:'name',NameNode,Graph )),
      (
        (
          rdf(NameNode,rdf:value,NameValue,Graph),
         \+ ( rdf( NameNode, aixm:uom, _NameUOM, Graph ); rdf( NameNode, fixm:uom, _NameUOM, Graph ); rdf( NameNode, plain:uom, _NameUOM, Graph ) ),
          Name=val(NameValue)
        );
        (
          rdf( NameNode,rdf:value,NameValue,Graph ),
          ( rdf( NameNode, aixm:uom, NameUOM, Graph ); rdf( NameNode, fixm:uom, NameUOM, Graph ); rdf( NameNode, plain:uom, NameUOM, Graph ) ),
          Name=xval(NameValue,NameUOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NameNilReason, Graph ),
          Name=nil(NameNilReason)
        );
        (
          rdf( NameNode,gml:indeterminatePosition, NameIndeterminate, Graph ),
          Name=indeterminate(NameIndeterminate)
        )
      )
  )
  ,(
    ( Designator='$null$',
      \+ rdf( OrganisationAuthorityTimeSlice,aixm:'designator',_Designator,Graph )
    );
  ( rdf( OrganisationAuthorityTimeSlice,aixm:'designator',DesignatorNode,Graph )),
      (
        (
          rdf(DesignatorNode,rdf:value,DesignatorValue,Graph),
         \+ ( rdf( DesignatorNode, aixm:uom, _DesignatorUOM, Graph ); rdf( DesignatorNode, fixm:uom, _DesignatorUOM, Graph ); rdf( DesignatorNode, plain:uom, _DesignatorUOM, Graph ) ),
          Designator=val(DesignatorValue)
        );
        (
          rdf( DesignatorNode,rdf:value,DesignatorValue,Graph ),
          ( rdf( DesignatorNode, aixm:uom, DesignatorUOM, Graph ); rdf( DesignatorNode, fixm:uom, DesignatorUOM, Graph ); rdf( DesignatorNode, plain:uom, DesignatorUOM, Graph ) ),
          Designator=xval(DesignatorValue,DesignatorUOM)
        );
        (
          rdf( DesignatorNode,aixm:nilReason, DesignatorNilReason, Graph ),
          Designator=nil(DesignatorNilReason)
        );
        (
          rdf( DesignatorNode,gml:indeterminatePosition, DesignatorIndeterminate, Graph ),
          Designator=indeterminate(DesignatorIndeterminate)
        )
      )
  )
  ,(
    ( Type='$null$',
      \+ rdf( OrganisationAuthorityTimeSlice,aixm:'type',_Type,Graph )
    );
  ( rdf( OrganisationAuthorityTimeSlice,aixm:'type',TypeNode,Graph )),
      (
        (
          rdf(TypeNode,rdf:value,TypeValue,Graph),
         \+ ( rdf( TypeNode, aixm:uom, _TypeUOM, Graph ); rdf( TypeNode, fixm:uom, _TypeUOM, Graph ); rdf( TypeNode, plain:uom, _TypeUOM, Graph ) ),
          Type=val(TypeValue)
        );
        (
          rdf( TypeNode,rdf:value,TypeValue,Graph ),
          ( rdf( TypeNode, aixm:uom, TypeUOM, Graph ); rdf( TypeNode, fixm:uom, TypeUOM, Graph ); rdf( TypeNode, plain:uom, TypeUOM, Graph ) ),
          Type=xval(TypeValue,TypeUOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, TypeNilReason, Graph ),
          Type=nil(TypeNilReason)
        );
        (
          rdf( TypeNode,gml:indeterminatePosition, TypeIndeterminate, Graph ),
          Type=indeterminate(TypeIndeterminate)
        )
      )
  )
  ,(
    ( Military='$null$',
      \+ rdf( OrganisationAuthorityTimeSlice,aixm:'military',_Military,Graph )
    );
  ( rdf( OrganisationAuthorityTimeSlice,aixm:'military',MilitaryNode,Graph )),
      (
        (
          rdf(MilitaryNode,rdf:value,MilitaryValue,Graph),
         \+ ( rdf( MilitaryNode, aixm:uom, _MilitaryUOM, Graph ); rdf( MilitaryNode, fixm:uom, _MilitaryUOM, Graph ); rdf( MilitaryNode, plain:uom, _MilitaryUOM, Graph ) ),
          Military=val(MilitaryValue)
        );
        (
          rdf( MilitaryNode,rdf:value,MilitaryValue,Graph ),
          ( rdf( MilitaryNode, aixm:uom, MilitaryUOM, Graph ); rdf( MilitaryNode, fixm:uom, MilitaryUOM, Graph ); rdf( MilitaryNode, plain:uom, MilitaryUOM, Graph ) ),
          Military=xval(MilitaryValue,MilitaryUOM)
        );
        (
          rdf( MilitaryNode,aixm:nilReason, MilitaryNilReason, Graph ),
          Military=nil(MilitaryNilReason)
        );
        (
          rdf( MilitaryNode,gml:indeterminatePosition, MilitaryIndeterminate, Graph ),
          Military=indeterminate(MilitaryIndeterminate)
        )
      )
  )
  ,findall(A, rdf(OrganisationAuthorityTimeSlice,aixm:'annotation',A,Graph), Annotation)
  ,findall(A, rdf(OrganisationAuthorityTimeSlice,aixm:'contact',A,Graph), Contact)
  ,findall(A, rdf(OrganisationAuthorityTimeSlice,aixm:'relatedOrganisationAuthority',A,Graph), RelatedOrganisationAuthority) .

fixm_EnRoute(Graph, EnRoute, AlternateAerodrome, FleetPrioritization, BoundaryCrossings, CpdlcConnection, BeaconCodeAssignment, Cleared, ControlElement, Pointout, Position) :-
  rdf(EnRoute,rdf:type,fixm:'EnRoute',Graph)
  ,findall(A, rdf(EnRoute,fixm:'alternateAerodrome',A,Graph), AlternateAerodrome)
  ,(
    ( FleetPrioritization='$null$',
      \+ rdf( EnRoute,fixm:'fleetPrioritization',_FleetPrioritization,Graph )
    );
  ( rdf( EnRoute,fixm:'fleetPrioritization',FleetPrioritizationNode,Graph )),
      (
        (
          rdf(FleetPrioritizationNode,rdf:value,FleetPrioritizationValue,Graph),
         \+ ( rdf( FleetPrioritizationNode, aixm:uom, _FleetPrioritizationUOM, Graph ); rdf( FleetPrioritizationNode, fixm:uom, _FleetPrioritizationUOM, Graph ); rdf( FleetPrioritizationNode, plain:uom, _FleetPrioritizationUOM, Graph ) ),
          FleetPrioritization=val(FleetPrioritizationValue)
        );
        (
          rdf( FleetPrioritizationNode,rdf:value,FleetPrioritizationValue,Graph ),
          ( rdf( FleetPrioritizationNode, aixm:uom, FleetPrioritizationUOM, Graph ); rdf( FleetPrioritizationNode, fixm:uom, FleetPrioritizationUOM, Graph ); rdf( FleetPrioritizationNode, plain:uom, FleetPrioritizationUOM, Graph ) ),
          FleetPrioritization=xval(FleetPrioritizationValue,FleetPrioritizationUOM)
        );
        (
          rdf( FleetPrioritizationNode,aixm:nilReason, FleetPrioritizationNilReason, Graph ),
          FleetPrioritization=nil(FleetPrioritizationNilReason)
        );
        (
          rdf( FleetPrioritizationNode,gml:indeterminatePosition, FleetPrioritizationIndeterminate, Graph ),
          FleetPrioritization=indeterminate(FleetPrioritizationIndeterminate)
        )
      )
  )
  ,findall(A, rdf(EnRoute,fixm:'boundaryCrossings',A,Graph), BoundaryCrossings)
  ,( ( CpdlcConnection='$null$',
    \+ rdf( EnRoute,fixm:'cpdlcConnection', _CpdlcConnection, Graph  )
   ; rdf(EnRoute,fixm:'cpdlcConnection', CpdlcConnection, Graph ) )
  )
  ,( ( BeaconCodeAssignment='$null$',
    \+ rdf( EnRoute,fixm:'beaconCodeAssignment', _BeaconCodeAssignment, Graph  )
   ; rdf(EnRoute,fixm:'beaconCodeAssignment', BeaconCodeAssignment, Graph ) )
  )
  ,( ( Cleared='$null$',
    \+ rdf( EnRoute,fixm:'cleared', _Cleared, Graph  )
   ; rdf(EnRoute,fixm:'cleared', Cleared, Graph ) )
  )
  ,findall(A, rdf(EnRoute,fixm:'controlElement',A,Graph), ControlElement)
  ,( ( Pointout='$null$',
    \+ rdf( EnRoute,fixm:'pointout', _Pointout, Graph  )
   ; rdf(EnRoute,fixm:'pointout', Pointout, Graph ) )
  )
  ,( ( Position='$null$',
    \+ rdf( EnRoute,fixm:'position', _Position, Graph  )
   ; rdf(EnRoute,fixm:'position', Position, Graph ) )
  ) .

fixm_FlightLevel(Graph, FlightLevel, Level, Unit) :-
  rdf(FlightLevel,rdf:type,fixm:'FlightLevel',Graph)
  ,(
    ( Level='$null$',
      \+ rdf( FlightLevel,fixm:'level',_Level,Graph )
    );
  ( rdf( FlightLevel,fixm:'level',LevelNode,Graph )),
      (
        (
          rdf(LevelNode,rdf:value,LevelValue,Graph),
         \+ ( rdf( LevelNode, aixm:uom, _LevelUOM, Graph ); rdf( LevelNode, fixm:uom, _LevelUOM, Graph ); rdf( LevelNode, plain:uom, _LevelUOM, Graph ) ),
          Level=val(LevelValue)
        );
        (
          rdf( LevelNode,rdf:value,LevelValue,Graph ),
          ( rdf( LevelNode, aixm:uom, LevelUOM, Graph ); rdf( LevelNode, fixm:uom, LevelUOM, Graph ); rdf( LevelNode, plain:uom, LevelUOM, Graph ) ),
          Level=xval(LevelValue,LevelUOM)
        );
        (
          rdf( LevelNode,aixm:nilReason, LevelNilReason, Graph ),
          Level=nil(LevelNilReason)
        );
        (
          rdf( LevelNode,gml:indeterminatePosition, LevelIndeterminate, Graph ),
          Level=indeterminate(LevelIndeterminate)
        )
      )
  )
  ,(
    ( Unit='$null$',
      \+ rdf( FlightLevel,fixm:'unit',_Unit,Graph )
    );
  ( rdf( FlightLevel,fixm:'unit',UnitNode,Graph )),
      (
        (
          rdf(UnitNode,rdf:value,UnitValue,Graph),
         \+ ( rdf( UnitNode, aixm:uom, _UnitUOM, Graph ); rdf( UnitNode, fixm:uom, _UnitUOM, Graph ); rdf( UnitNode, plain:uom, _UnitUOM, Graph ) ),
          Unit=val(UnitValue)
        );
        (
          rdf( UnitNode,rdf:value,UnitValue,Graph ),
          ( rdf( UnitNode, aixm:uom, UnitUOM, Graph ); rdf( UnitNode, fixm:uom, UnitUOM, Graph ); rdf( UnitNode, plain:uom, UnitUOM, Graph ) ),
          Unit=xval(UnitValue,UnitUOM)
        );
        (
          rdf( UnitNode,aixm:nilReason, UnitNilReason, Graph ),
          Unit=nil(UnitNilReason)
        );
        (
          rdf( UnitNode,gml:indeterminatePosition, UnitIndeterminate, Graph ),
          Unit=indeterminate(UnitIndeterminate)
        )
      )
  ) .

fixm_LateralOfftrack(Graph, LateralOfftrack, OfftrackDistance, OfftrackReason) :-
  rdf(LateralOfftrack,rdf:type,fixm:'LateralOfftrack',Graph)
  ,findall(A, rdf(LateralOfftrack,fixm:'offtrackDistance',A,Graph), OfftrackDistance)
  ,(
    ( OfftrackReason='$null$',
      \+ rdf( LateralOfftrack,fixm:'offtrackReason',_OfftrackReason,Graph )
    );
  ( rdf( LateralOfftrack,fixm:'offtrackReason',OfftrackReasonNode,Graph )),
      (
        (
          rdf(OfftrackReasonNode,rdf:value,OfftrackReasonValue,Graph),
         \+ ( rdf( OfftrackReasonNode, aixm:uom, _OfftrackReasonUOM, Graph ); rdf( OfftrackReasonNode, fixm:uom, _OfftrackReasonUOM, Graph ); rdf( OfftrackReasonNode, plain:uom, _OfftrackReasonUOM, Graph ) ),
          OfftrackReason=val(OfftrackReasonValue)
        );
        (
          rdf( OfftrackReasonNode,rdf:value,OfftrackReasonValue,Graph ),
          ( rdf( OfftrackReasonNode, aixm:uom, OfftrackReasonUOM, Graph ); rdf( OfftrackReasonNode, fixm:uom, OfftrackReasonUOM, Graph ); rdf( OfftrackReasonNode, plain:uom, OfftrackReasonUOM, Graph ) ),
          OfftrackReason=xval(OfftrackReasonValue,OfftrackReasonUOM)
        );
        (
          rdf( OfftrackReasonNode,aixm:nilReason, OfftrackReasonNilReason, Graph ),
          OfftrackReason=nil(OfftrackReasonNilReason)
        );
        (
          rdf( OfftrackReasonNode,gml:indeterminatePosition, OfftrackReasonIndeterminate, Graph ),
          OfftrackReason=indeterminate(OfftrackReasonIndeterminate)
        )
      )
  ) .

fixm_TemporalRange(Graph, TemporalRange, Earliest, Latest) :-
  rdf(TemporalRange,rdf:type,fixm:'TemporalRange',Graph)
  ,(
    ( Earliest='$null$',
      \+ rdf( TemporalRange,fixm:'earliest',_Earliest,Graph )
    );
  ( rdf( TemporalRange,fixm:'earliest',EarliestNode,Graph )),
      (
        (
          rdf(EarliestNode,rdf:value,EarliestValue,Graph),
         \+ ( rdf( EarliestNode, aixm:uom, _EarliestUOM, Graph ); rdf( EarliestNode, fixm:uom, _EarliestUOM, Graph ); rdf( EarliestNode, plain:uom, _EarliestUOM, Graph ) ),
          Earliest=val(EarliestValue)
        );
        (
          rdf( EarliestNode,rdf:value,EarliestValue,Graph ),
          ( rdf( EarliestNode, aixm:uom, EarliestUOM, Graph ); rdf( EarliestNode, fixm:uom, EarliestUOM, Graph ); rdf( EarliestNode, plain:uom, EarliestUOM, Graph ) ),
          Earliest=xval(EarliestValue,EarliestUOM)
        );
        (
          rdf( EarliestNode,aixm:nilReason, EarliestNilReason, Graph ),
          Earliest=nil(EarliestNilReason)
        );
        (
          rdf( EarliestNode,gml:indeterminatePosition, EarliestIndeterminate, Graph ),
          Earliest=indeterminate(EarliestIndeterminate)
        )
      )
  )
  ,(
    ( Latest='$null$',
      \+ rdf( TemporalRange,fixm:'latest',_Latest,Graph )
    );
  ( rdf( TemporalRange,fixm:'latest',LatestNode,Graph )),
      (
        (
          rdf(LatestNode,rdf:value,LatestValue,Graph),
         \+ ( rdf( LatestNode, aixm:uom, _LatestUOM, Graph ); rdf( LatestNode, fixm:uom, _LatestUOM, Graph ); rdf( LatestNode, plain:uom, _LatestUOM, Graph ) ),
          Latest=val(LatestValue)
        );
        (
          rdf( LatestNode,rdf:value,LatestValue,Graph ),
          ( rdf( LatestNode, aixm:uom, LatestUOM, Graph ); rdf( LatestNode, fixm:uom, LatestUOM, Graph ); rdf( LatestNode, plain:uom, LatestUOM, Graph ) ),
          Latest=xval(LatestValue,LatestUOM)
        );
        (
          rdf( LatestNode,aixm:nilReason, LatestNilReason, Graph ),
          Latest=nil(LatestNilReason)
        );
        (
          rdf( LatestNode,gml:indeterminatePosition, LatestIndeterminate, Graph ),
          Latest=indeterminate(LatestIndeterminate)
        )
      )
  ) .

fixm_Aircraft(Graph, Aircraft, AircraftColours, AircraftQuantity, EngineType, AircraftAddress, Capabilities, Registration, AircraftType, WakeTurbulence, AircraftPerformance) :-
  rdf(Aircraft,rdf:type,fixm:'Aircraft',Graph)
  ,(
    ( AircraftColours='$null$',
      \+ rdf( Aircraft,fixm:'aircraftColours',_AircraftColours,Graph )
    );
  ( rdf( Aircraft,fixm:'aircraftColours',AircraftColoursNode,Graph )),
      (
        (
          rdf(AircraftColoursNode,rdf:value,AircraftColoursValue,Graph),
         \+ ( rdf( AircraftColoursNode, aixm:uom, _AircraftColoursUOM, Graph ); rdf( AircraftColoursNode, fixm:uom, _AircraftColoursUOM, Graph ); rdf( AircraftColoursNode, plain:uom, _AircraftColoursUOM, Graph ) ),
          AircraftColours=val(AircraftColoursValue)
        );
        (
          rdf( AircraftColoursNode,rdf:value,AircraftColoursValue,Graph ),
          ( rdf( AircraftColoursNode, aixm:uom, AircraftColoursUOM, Graph ); rdf( AircraftColoursNode, fixm:uom, AircraftColoursUOM, Graph ); rdf( AircraftColoursNode, plain:uom, AircraftColoursUOM, Graph ) ),
          AircraftColours=xval(AircraftColoursValue,AircraftColoursUOM)
        );
        (
          rdf( AircraftColoursNode,aixm:nilReason, AircraftColoursNilReason, Graph ),
          AircraftColours=nil(AircraftColoursNilReason)
        );
        (
          rdf( AircraftColoursNode,gml:indeterminatePosition, AircraftColoursIndeterminate, Graph ),
          AircraftColours=indeterminate(AircraftColoursIndeterminate)
        )
      )
  )
  ,(
    ( AircraftQuantity='$null$',
      \+ rdf( Aircraft,fixm:'aircraftQuantity',_AircraftQuantity,Graph )
    );
  ( rdf( Aircraft,fixm:'aircraftQuantity',AircraftQuantityNode,Graph )),
      (
        (
          rdf(AircraftQuantityNode,rdf:value,AircraftQuantityValue,Graph),
         \+ ( rdf( AircraftQuantityNode, aixm:uom, _AircraftQuantityUOM, Graph ); rdf( AircraftQuantityNode, fixm:uom, _AircraftQuantityUOM, Graph ); rdf( AircraftQuantityNode, plain:uom, _AircraftQuantityUOM, Graph ) ),
          AircraftQuantity=val(AircraftQuantityValue)
        );
        (
          rdf( AircraftQuantityNode,rdf:value,AircraftQuantityValue,Graph ),
          ( rdf( AircraftQuantityNode, aixm:uom, AircraftQuantityUOM, Graph ); rdf( AircraftQuantityNode, fixm:uom, AircraftQuantityUOM, Graph ); rdf( AircraftQuantityNode, plain:uom, AircraftQuantityUOM, Graph ) ),
          AircraftQuantity=xval(AircraftQuantityValue,AircraftQuantityUOM)
        );
        (
          rdf( AircraftQuantityNode,aixm:nilReason, AircraftQuantityNilReason, Graph ),
          AircraftQuantity=nil(AircraftQuantityNilReason)
        );
        (
          rdf( AircraftQuantityNode,gml:indeterminatePosition, AircraftQuantityIndeterminate, Graph ),
          AircraftQuantity=indeterminate(AircraftQuantityIndeterminate)
        )
      )
  )
  ,(
    ( EngineType='$null$',
      \+ rdf( Aircraft,fixm:'engineType',_EngineType,Graph )
    );
  ( rdf( Aircraft,fixm:'engineType',EngineTypeNode,Graph )),
      (
        (
          rdf(EngineTypeNode,rdf:value,EngineTypeValue,Graph),
         \+ ( rdf( EngineTypeNode, aixm:uom, _EngineTypeUOM, Graph ); rdf( EngineTypeNode, fixm:uom, _EngineTypeUOM, Graph ); rdf( EngineTypeNode, plain:uom, _EngineTypeUOM, Graph ) ),
          EngineType=val(EngineTypeValue)
        );
        (
          rdf( EngineTypeNode,rdf:value,EngineTypeValue,Graph ),
          ( rdf( EngineTypeNode, aixm:uom, EngineTypeUOM, Graph ); rdf( EngineTypeNode, fixm:uom, EngineTypeUOM, Graph ); rdf( EngineTypeNode, plain:uom, EngineTypeUOM, Graph ) ),
          EngineType=xval(EngineTypeValue,EngineTypeUOM)
        );
        (
          rdf( EngineTypeNode,aixm:nilReason, EngineTypeNilReason, Graph ),
          EngineType=nil(EngineTypeNilReason)
        );
        (
          rdf( EngineTypeNode,gml:indeterminatePosition, EngineTypeIndeterminate, Graph ),
          EngineType=indeterminate(EngineTypeIndeterminate)
        )
      )
  )
  ,(
    ( AircraftAddress='$null$',
      \+ rdf( Aircraft,fixm:'aircraftAddress',_AircraftAddress,Graph )
    );
  ( rdf( Aircraft,fixm:'aircraftAddress',AircraftAddressNode,Graph )),
      (
        (
          rdf(AircraftAddressNode,rdf:value,AircraftAddressValue,Graph),
         \+ ( rdf( AircraftAddressNode, aixm:uom, _AircraftAddressUOM, Graph ); rdf( AircraftAddressNode, fixm:uom, _AircraftAddressUOM, Graph ); rdf( AircraftAddressNode, plain:uom, _AircraftAddressUOM, Graph ) ),
          AircraftAddress=val(AircraftAddressValue)
        );
        (
          rdf( AircraftAddressNode,rdf:value,AircraftAddressValue,Graph ),
          ( rdf( AircraftAddressNode, aixm:uom, AircraftAddressUOM, Graph ); rdf( AircraftAddressNode, fixm:uom, AircraftAddressUOM, Graph ); rdf( AircraftAddressNode, plain:uom, AircraftAddressUOM, Graph ) ),
          AircraftAddress=xval(AircraftAddressValue,AircraftAddressUOM)
        );
        (
          rdf( AircraftAddressNode,aixm:nilReason, AircraftAddressNilReason, Graph ),
          AircraftAddress=nil(AircraftAddressNilReason)
        );
        (
          rdf( AircraftAddressNode,gml:indeterminatePosition, AircraftAddressIndeterminate, Graph ),
          AircraftAddress=indeterminate(AircraftAddressIndeterminate)
        )
      )
  )
  ,( ( Capabilities='$null$',
    \+ rdf( Aircraft,fixm:'capabilities', _Capabilities, Graph  )
   ; rdf(Aircraft,fixm:'capabilities', Capabilities, Graph ) )
  )
  ,(
    ( Registration='$null$',
      \+ rdf( Aircraft,fixm:'registration',_Registration,Graph )
    );
  ( rdf( Aircraft,fixm:'registration',RegistrationNode,Graph )),
      (
        (
          rdf(RegistrationNode,rdf:value,RegistrationValue,Graph),
         \+ ( rdf( RegistrationNode, aixm:uom, _RegistrationUOM, Graph ); rdf( RegistrationNode, fixm:uom, _RegistrationUOM, Graph ); rdf( RegistrationNode, plain:uom, _RegistrationUOM, Graph ) ),
          Registration=val(RegistrationValue)
        );
        (
          rdf( RegistrationNode,rdf:value,RegistrationValue,Graph ),
          ( rdf( RegistrationNode, aixm:uom, RegistrationUOM, Graph ); rdf( RegistrationNode, fixm:uom, RegistrationUOM, Graph ); rdf( RegistrationNode, plain:uom, RegistrationUOM, Graph ) ),
          Registration=xval(RegistrationValue,RegistrationUOM)
        );
        (
          rdf( RegistrationNode,aixm:nilReason, RegistrationNilReason, Graph ),
          Registration=nil(RegistrationNilReason)
        );
        (
          rdf( RegistrationNode,gml:indeterminatePosition, RegistrationIndeterminate, Graph ),
          Registration=indeterminate(RegistrationIndeterminate)
        )
      )
  )
  ,(
    ( AircraftType='$null$',
      \+ rdf( Aircraft,fixm:'aircraftType',_AircraftType,Graph )
    );
  ( rdf( Aircraft,fixm:'aircraftType',AircraftTypeNode,Graph )),
      (
        (
          rdf(AircraftTypeNode,rdf:value,AircraftTypeValue,Graph),
         \+ ( rdf( AircraftTypeNode, aixm:uom, _AircraftTypeUOM, Graph ); rdf( AircraftTypeNode, fixm:uom, _AircraftTypeUOM, Graph ); rdf( AircraftTypeNode, plain:uom, _AircraftTypeUOM, Graph ) ),
          AircraftType=val(AircraftTypeValue)
        );
        (
          rdf( AircraftTypeNode,rdf:value,AircraftTypeValue,Graph ),
          ( rdf( AircraftTypeNode, aixm:uom, AircraftTypeUOM, Graph ); rdf( AircraftTypeNode, fixm:uom, AircraftTypeUOM, Graph ); rdf( AircraftTypeNode, plain:uom, AircraftTypeUOM, Graph ) ),
          AircraftType=xval(AircraftTypeValue,AircraftTypeUOM)
        );
        (
          rdf( AircraftTypeNode,aixm:nilReason, AircraftTypeNilReason, Graph ),
          AircraftType=nil(AircraftTypeNilReason)
        );
        (
          rdf( AircraftTypeNode,gml:indeterminatePosition, AircraftTypeIndeterminate, Graph ),
          AircraftType=indeterminate(AircraftTypeIndeterminate)
        )
      )
  )
  ,(
    ( WakeTurbulence='$null$',
      \+ rdf( Aircraft,fixm:'wakeTurbulence',_WakeTurbulence,Graph )
    );
  ( rdf( Aircraft,fixm:'wakeTurbulence',WakeTurbulenceNode,Graph )),
      (
        (
          rdf(WakeTurbulenceNode,rdf:value,WakeTurbulenceValue,Graph),
         \+ ( rdf( WakeTurbulenceNode, aixm:uom, _WakeTurbulenceUOM, Graph ); rdf( WakeTurbulenceNode, fixm:uom, _WakeTurbulenceUOM, Graph ); rdf( WakeTurbulenceNode, plain:uom, _WakeTurbulenceUOM, Graph ) ),
          WakeTurbulence=val(WakeTurbulenceValue)
        );
        (
          rdf( WakeTurbulenceNode,rdf:value,WakeTurbulenceValue,Graph ),
          ( rdf( WakeTurbulenceNode, aixm:uom, WakeTurbulenceUOM, Graph ); rdf( WakeTurbulenceNode, fixm:uom, WakeTurbulenceUOM, Graph ); rdf( WakeTurbulenceNode, plain:uom, WakeTurbulenceUOM, Graph ) ),
          WakeTurbulence=xval(WakeTurbulenceValue,WakeTurbulenceUOM)
        );
        (
          rdf( WakeTurbulenceNode,aixm:nilReason, WakeTurbulenceNilReason, Graph ),
          WakeTurbulence=nil(WakeTurbulenceNilReason)
        );
        (
          rdf( WakeTurbulenceNode,gml:indeterminatePosition, WakeTurbulenceIndeterminate, Graph ),
          WakeTurbulence=indeterminate(WakeTurbulenceIndeterminate)
        )
      )
  )
  ,(
    ( AircraftPerformance='$null$',
      \+ rdf( Aircraft,fixm:'aircraftPerformance',_AircraftPerformance,Graph )
    );
  ( rdf( Aircraft,fixm:'aircraftPerformance',AircraftPerformanceNode,Graph )),
      (
        (
          rdf(AircraftPerformanceNode,rdf:value,AircraftPerformanceValue,Graph),
         \+ ( rdf( AircraftPerformanceNode, aixm:uom, _AircraftPerformanceUOM, Graph ); rdf( AircraftPerformanceNode, fixm:uom, _AircraftPerformanceUOM, Graph ); rdf( AircraftPerformanceNode, plain:uom, _AircraftPerformanceUOM, Graph ) ),
          AircraftPerformance=val(AircraftPerformanceValue)
        );
        (
          rdf( AircraftPerformanceNode,rdf:value,AircraftPerformanceValue,Graph ),
          ( rdf( AircraftPerformanceNode, aixm:uom, AircraftPerformanceUOM, Graph ); rdf( AircraftPerformanceNode, fixm:uom, AircraftPerformanceUOM, Graph ); rdf( AircraftPerformanceNode, plain:uom, AircraftPerformanceUOM, Graph ) ),
          AircraftPerformance=xval(AircraftPerformanceValue,AircraftPerformanceUOM)
        );
        (
          rdf( AircraftPerformanceNode,aixm:nilReason, AircraftPerformanceNilReason, Graph ),
          AircraftPerformance=nil(AircraftPerformanceNilReason)
        );
        (
          rdf( AircraftPerformanceNode,gml:indeterminatePosition, AircraftPerformanceIndeterminate, Graph ),
          AircraftPerformance=indeterminate(AircraftPerformanceIndeterminate)
        )
      )
  ) .

fixm_OnlineContact(Graph, OnlineContact, Email) :-
  rdf(OnlineContact,rdf:type,fixm:'OnlineContact',Graph)
  ,(
    ( Email='$null$',
      \+ rdf( OnlineContact,fixm:'email',_Email,Graph )
    );
  ( rdf( OnlineContact,fixm:'email',EmailNode,Graph )),
      (
        (
          rdf(EmailNode,rdf:value,EmailValue,Graph),
         \+ ( rdf( EmailNode, aixm:uom, _EmailUOM, Graph ); rdf( EmailNode, fixm:uom, _EmailUOM, Graph ); rdf( EmailNode, plain:uom, _EmailUOM, Graph ) ),
          Email=val(EmailValue)
        );
        (
          rdf( EmailNode,rdf:value,EmailValue,Graph ),
          ( rdf( EmailNode, aixm:uom, EmailUOM, Graph ); rdf( EmailNode, fixm:uom, EmailUOM, Graph ); rdf( EmailNode, plain:uom, EmailUOM, Graph ) ),
          Email=xval(EmailValue,EmailUOM)
        );
        (
          rdf( EmailNode,aixm:nilReason, EmailNilReason, Graph ),
          Email=nil(EmailNilReason)
        );
        (
          rdf( EmailNode,gml:indeterminatePosition, EmailIndeterminate, Graph ),
          Email=indeterminate(EmailIndeterminate)
        )
      )
  ) .

fixm_AirspaceConstraint(Graph, AirspaceConstraint, AirspaceControlledEntryTime, ConstrainedAirspace) :-
  rdf(AirspaceConstraint,rdf:type,fixm:'AirspaceConstraint',Graph)
  ,(
    ( AirspaceControlledEntryTime='$null$',
      \+ rdf( AirspaceConstraint,fixm:'airspaceControlledEntryTime',_AirspaceControlledEntryTime,Graph )
    );
  ( rdf( AirspaceConstraint,fixm:'airspaceControlledEntryTime',AirspaceControlledEntryTimeNode,Graph )),
      (
        (
          rdf(AirspaceControlledEntryTimeNode,rdf:value,AirspaceControlledEntryTimeValue,Graph),
         \+ ( rdf( AirspaceControlledEntryTimeNode, aixm:uom, _AirspaceControlledEntryTimeUOM, Graph ); rdf( AirspaceControlledEntryTimeNode, fixm:uom, _AirspaceControlledEntryTimeUOM, Graph ); rdf( AirspaceControlledEntryTimeNode, plain:uom, _AirspaceControlledEntryTimeUOM, Graph ) ),
          AirspaceControlledEntryTime=val(AirspaceControlledEntryTimeValue)
        );
        (
          rdf( AirspaceControlledEntryTimeNode,rdf:value,AirspaceControlledEntryTimeValue,Graph ),
          ( rdf( AirspaceControlledEntryTimeNode, aixm:uom, AirspaceControlledEntryTimeUOM, Graph ); rdf( AirspaceControlledEntryTimeNode, fixm:uom, AirspaceControlledEntryTimeUOM, Graph ); rdf( AirspaceControlledEntryTimeNode, plain:uom, AirspaceControlledEntryTimeUOM, Graph ) ),
          AirspaceControlledEntryTime=xval(AirspaceControlledEntryTimeValue,AirspaceControlledEntryTimeUOM)
        );
        (
          rdf( AirspaceControlledEntryTimeNode,aixm:nilReason, AirspaceControlledEntryTimeNilReason, Graph ),
          AirspaceControlledEntryTime=nil(AirspaceControlledEntryTimeNilReason)
        );
        (
          rdf( AirspaceControlledEntryTimeNode,gml:indeterminatePosition, AirspaceControlledEntryTimeIndeterminate, Graph ),
          AirspaceControlledEntryTime=indeterminate(AirspaceControlledEntryTimeIndeterminate)
        )
      )
  )
  ,(
    ( ConstrainedAirspace='$null$',
      \+ rdf( AirspaceConstraint,fixm:'constrainedAirspace',_ConstrainedAirspace,Graph )
    );
  ( rdf( AirspaceConstraint,fixm:'constrainedAirspace',ConstrainedAirspaceNode,Graph )),
      (
        (
          rdf(ConstrainedAirspaceNode,rdf:value,ConstrainedAirspaceValue,Graph),
         \+ ( rdf( ConstrainedAirspaceNode, aixm:uom, _ConstrainedAirspaceUOM, Graph ); rdf( ConstrainedAirspaceNode, fixm:uom, _ConstrainedAirspaceUOM, Graph ); rdf( ConstrainedAirspaceNode, plain:uom, _ConstrainedAirspaceUOM, Graph ) ),
          ConstrainedAirspace=val(ConstrainedAirspaceValue)
        );
        (
          rdf( ConstrainedAirspaceNode,rdf:value,ConstrainedAirspaceValue,Graph ),
          ( rdf( ConstrainedAirspaceNode, aixm:uom, ConstrainedAirspaceUOM, Graph ); rdf( ConstrainedAirspaceNode, fixm:uom, ConstrainedAirspaceUOM, Graph ); rdf( ConstrainedAirspaceNode, plain:uom, ConstrainedAirspaceUOM, Graph ) ),
          ConstrainedAirspace=xval(ConstrainedAirspaceValue,ConstrainedAirspaceUOM)
        );
        (
          rdf( ConstrainedAirspaceNode,aixm:nilReason, ConstrainedAirspaceNilReason, Graph ),
          ConstrainedAirspace=nil(ConstrainedAirspaceNilReason)
        );
        (
          rdf( ConstrainedAirspaceNode,gml:indeterminatePosition, ConstrainedAirspaceIndeterminate, Graph ),
          ConstrainedAirspace=indeterminate(ConstrainedAirspaceIndeterminate)
        )
      )
  ) .

fixm_TimeSequence(Graph, TimeSequence, Approval, Begin, End, Ready, Request) :-
  rdf(TimeSequence,rdf:type,fixm:'TimeSequence',Graph)
  ,( ( Approval='$null$',
    \+ rdf( TimeSequence,fixm:'approval', _Approval, Graph  )
   ; rdf(TimeSequence,fixm:'approval', Approval, Graph ) )
  )
  ,( ( Begin='$null$',
    \+ rdf( TimeSequence,fixm:'begin', _Begin, Graph  )
   ; rdf(TimeSequence,fixm:'begin', Begin, Graph ) )
  )
  ,( ( End='$null$',
    \+ rdf( TimeSequence,fixm:'end', _End, Graph  )
   ; rdf(TimeSequence,fixm:'end', End, Graph ) )
  )
  ,( ( Ready='$null$',
    \+ rdf( TimeSequence,fixm:'ready', _Ready, Graph  )
   ; rdf(TimeSequence,fixm:'ready', Ready, Graph ) )
  )
  ,( ( Request='$null$',
    \+ rdf( TimeSequence,fixm:'request', _Request, Graph  )
   ; rdf(TimeSequence,fixm:'request', Request, Graph ) )
  ) .

fixm_AdditionalHandlingInformation(Graph, AdditionalHandlingInformation, ResponsibleAgent) :-
  rdf(AdditionalHandlingInformation,rdf:type,fixm:'AdditionalHandlingInformation',Graph)
  ,(
    ( ResponsibleAgent='$null$',
      \+ rdf( AdditionalHandlingInformation,fixm:'responsibleAgent',_ResponsibleAgent,Graph )
    );
  ( rdf( AdditionalHandlingInformation,fixm:'responsibleAgent',ResponsibleAgentNode,Graph )),
      (
        (
          rdf(ResponsibleAgentNode,rdf:value,ResponsibleAgentValue,Graph),
         \+ ( rdf( ResponsibleAgentNode, aixm:uom, _ResponsibleAgentUOM, Graph ); rdf( ResponsibleAgentNode, fixm:uom, _ResponsibleAgentUOM, Graph ); rdf( ResponsibleAgentNode, plain:uom, _ResponsibleAgentUOM, Graph ) ),
          ResponsibleAgent=val(ResponsibleAgentValue)
        );
        (
          rdf( ResponsibleAgentNode,rdf:value,ResponsibleAgentValue,Graph ),
          ( rdf( ResponsibleAgentNode, aixm:uom, ResponsibleAgentUOM, Graph ); rdf( ResponsibleAgentNode, fixm:uom, ResponsibleAgentUOM, Graph ); rdf( ResponsibleAgentNode, plain:uom, ResponsibleAgentUOM, Graph ) ),
          ResponsibleAgent=xval(ResponsibleAgentValue,ResponsibleAgentUOM)
        );
        (
          rdf( ResponsibleAgentNode,aixm:nilReason, ResponsibleAgentNilReason, Graph ),
          ResponsibleAgent=nil(ResponsibleAgentNilReason)
        );
        (
          rdf( ResponsibleAgentNode,gml:indeterminatePosition, ResponsibleAgentIndeterminate, Graph ),
          ResponsibleAgent=indeterminate(ResponsibleAgentIndeterminate)
        )
      )
  ) .

fixm_AtcUnitReference(Graph, AtcUnitReference, SectorIdentifier, Delegated) :-
  subClassOf(T,fixm:'AtcUnitReference')
  ,rdf(AtcUnitReference,rdf:type,T,Graph)
  ,(
    ( SectorIdentifier='$null$',
      \+ rdf( AtcUnitReference,fixm:'sectorIdentifier',_SectorIdentifier,Graph )
    );
  ( rdf( AtcUnitReference,fixm:'sectorIdentifier',SectorIdentifierNode,Graph )),
      (
        (
          rdf(SectorIdentifierNode,rdf:value,SectorIdentifierValue,Graph),
         \+ ( rdf( SectorIdentifierNode, aixm:uom, _SectorIdentifierUOM, Graph ); rdf( SectorIdentifierNode, fixm:uom, _SectorIdentifierUOM, Graph ); rdf( SectorIdentifierNode, plain:uom, _SectorIdentifierUOM, Graph ) ),
          SectorIdentifier=val(SectorIdentifierValue)
        );
        (
          rdf( SectorIdentifierNode,rdf:value,SectorIdentifierValue,Graph ),
          ( rdf( SectorIdentifierNode, aixm:uom, SectorIdentifierUOM, Graph ); rdf( SectorIdentifierNode, fixm:uom, SectorIdentifierUOM, Graph ); rdf( SectorIdentifierNode, plain:uom, SectorIdentifierUOM, Graph ) ),
          SectorIdentifier=xval(SectorIdentifierValue,SectorIdentifierUOM)
        );
        (
          rdf( SectorIdentifierNode,aixm:nilReason, SectorIdentifierNilReason, Graph ),
          SectorIdentifier=nil(SectorIdentifierNilReason)
        );
        (
          rdf( SectorIdentifierNode,gml:indeterminatePosition, SectorIdentifierIndeterminate, Graph ),
          SectorIdentifier=indeterminate(SectorIdentifierIndeterminate)
        )
      )
  )
  ,(
    ( Delegated='$null$',
      \+ rdf( AtcUnitReference,fixm:'delegated',_Delegated,Graph )
    );
  ( rdf( AtcUnitReference,fixm:'delegated',DelegatedNode,Graph )),
      (
        (
          rdf(DelegatedNode,rdf:value,DelegatedValue,Graph),
         \+ ( rdf( DelegatedNode, aixm:uom, _DelegatedUOM, Graph ); rdf( DelegatedNode, fixm:uom, _DelegatedUOM, Graph ); rdf( DelegatedNode, plain:uom, _DelegatedUOM, Graph ) ),
          Delegated=val(DelegatedValue)
        );
        (
          rdf( DelegatedNode,rdf:value,DelegatedValue,Graph ),
          ( rdf( DelegatedNode, aixm:uom, DelegatedUOM, Graph ); rdf( DelegatedNode, fixm:uom, DelegatedUOM, Graph ); rdf( DelegatedNode, plain:uom, DelegatedUOM, Graph ) ),
          Delegated=xval(DelegatedValue,DelegatedUOM)
        );
        (
          rdf( DelegatedNode,aixm:nilReason, DelegatedNilReason, Graph ),
          Delegated=nil(DelegatedNilReason)
        );
        (
          rdf( DelegatedNode,gml:indeterminatePosition, DelegatedIndeterminate, Graph ),
          Delegated=indeterminate(DelegatedIndeterminate)
        )
      )
  ) .

fixm_Extension(Graph, Extension) :-
  rdf(Extension,rdf:type,fixm:'Extension',Graph) .

fixm_SurveillanceCapabilities(Graph, SurveillanceCapabilities, OtherSurveillanceCapabilities, SurveillanceCode) :-
  rdf(SurveillanceCapabilities,rdf:type,fixm:'SurveillanceCapabilities',Graph)
  ,(
    ( OtherSurveillanceCapabilities='$null$',
      \+ rdf( SurveillanceCapabilities,fixm:'otherSurveillanceCapabilities',_OtherSurveillanceCapabilities,Graph )
    );
  ( rdf( SurveillanceCapabilities,fixm:'otherSurveillanceCapabilities',OtherSurveillanceCapabilitiesNode,Graph )),
      (
        (
          rdf(OtherSurveillanceCapabilitiesNode,rdf:value,OtherSurveillanceCapabilitiesValue,Graph),
         \+ ( rdf( OtherSurveillanceCapabilitiesNode, aixm:uom, _OtherSurveillanceCapabilitiesUOM, Graph ); rdf( OtherSurveillanceCapabilitiesNode, fixm:uom, _OtherSurveillanceCapabilitiesUOM, Graph ); rdf( OtherSurveillanceCapabilitiesNode, plain:uom, _OtherSurveillanceCapabilitiesUOM, Graph ) ),
          OtherSurveillanceCapabilities=val(OtherSurveillanceCapabilitiesValue)
        );
        (
          rdf( OtherSurveillanceCapabilitiesNode,rdf:value,OtherSurveillanceCapabilitiesValue,Graph ),
          ( rdf( OtherSurveillanceCapabilitiesNode, aixm:uom, OtherSurveillanceCapabilitiesUOM, Graph ); rdf( OtherSurveillanceCapabilitiesNode, fixm:uom, OtherSurveillanceCapabilitiesUOM, Graph ); rdf( OtherSurveillanceCapabilitiesNode, plain:uom, OtherSurveillanceCapabilitiesUOM, Graph ) ),
          OtherSurveillanceCapabilities=xval(OtherSurveillanceCapabilitiesValue,OtherSurveillanceCapabilitiesUOM)
        );
        (
          rdf( OtherSurveillanceCapabilitiesNode,aixm:nilReason, OtherSurveillanceCapabilitiesNilReason, Graph ),
          OtherSurveillanceCapabilities=nil(OtherSurveillanceCapabilitiesNilReason)
        );
        (
          rdf( OtherSurveillanceCapabilitiesNode,gml:indeterminatePosition, OtherSurveillanceCapabilitiesIndeterminate, Graph ),
          OtherSurveillanceCapabilities=indeterminate(OtherSurveillanceCapabilitiesIndeterminate)
        )
      )
  )
  ,findall(A, rdf(SurveillanceCapabilities,fixm:'surveillanceCode',A,Graph), SurveillanceCode) .

fixm_Trajectory(Graph, Trajectory, TrajectoryPoint) :-
  rdf(Trajectory,rdf:type,fixm:'Trajectory',Graph)
  ,findall(A, rdf(Trajectory,fixm:'trajectoryPoint',A,Graph), TrajectoryPoint) .

aixm_AltimeterSourceTimeSlice(Graph, AltimeterSourceTimeSlice, IsRemote, IsPrimary, Availability, Annotation) :-
  rdf(AltimeterSourceTimeSlice,rdf:type,aixm:'AltimeterSourceTimeSlice',Graph)
  ,(
    ( IsRemote='$null$',
      \+ rdf( AltimeterSourceTimeSlice,aixm:'isRemote',_IsRemote,Graph )
    );
  ( rdf( AltimeterSourceTimeSlice,aixm:'isRemote',IsRemoteNode,Graph )),
      (
        (
          rdf(IsRemoteNode,rdf:value,IsRemoteValue,Graph),
         \+ ( rdf( IsRemoteNode, aixm:uom, _IsRemoteUOM, Graph ); rdf( IsRemoteNode, fixm:uom, _IsRemoteUOM, Graph ); rdf( IsRemoteNode, plain:uom, _IsRemoteUOM, Graph ) ),
          IsRemote=val(IsRemoteValue)
        );
        (
          rdf( IsRemoteNode,rdf:value,IsRemoteValue,Graph ),
          ( rdf( IsRemoteNode, aixm:uom, IsRemoteUOM, Graph ); rdf( IsRemoteNode, fixm:uom, IsRemoteUOM, Graph ); rdf( IsRemoteNode, plain:uom, IsRemoteUOM, Graph ) ),
          IsRemote=xval(IsRemoteValue,IsRemoteUOM)
        );
        (
          rdf( IsRemoteNode,aixm:nilReason, IsRemoteNilReason, Graph ),
          IsRemote=nil(IsRemoteNilReason)
        );
        (
          rdf( IsRemoteNode,gml:indeterminatePosition, IsRemoteIndeterminate, Graph ),
          IsRemote=indeterminate(IsRemoteIndeterminate)
        )
      )
  )
  ,(
    ( IsPrimary='$null$',
      \+ rdf( AltimeterSourceTimeSlice,aixm:'isPrimary',_IsPrimary,Graph )
    );
  ( rdf( AltimeterSourceTimeSlice,aixm:'isPrimary',IsPrimaryNode,Graph )),
      (
        (
          rdf(IsPrimaryNode,rdf:value,IsPrimaryValue,Graph),
         \+ ( rdf( IsPrimaryNode, aixm:uom, _IsPrimaryUOM, Graph ); rdf( IsPrimaryNode, fixm:uom, _IsPrimaryUOM, Graph ); rdf( IsPrimaryNode, plain:uom, _IsPrimaryUOM, Graph ) ),
          IsPrimary=val(IsPrimaryValue)
        );
        (
          rdf( IsPrimaryNode,rdf:value,IsPrimaryValue,Graph ),
          ( rdf( IsPrimaryNode, aixm:uom, IsPrimaryUOM, Graph ); rdf( IsPrimaryNode, fixm:uom, IsPrimaryUOM, Graph ); rdf( IsPrimaryNode, plain:uom, IsPrimaryUOM, Graph ) ),
          IsPrimary=xval(IsPrimaryValue,IsPrimaryUOM)
        );
        (
          rdf( IsPrimaryNode,aixm:nilReason, IsPrimaryNilReason, Graph ),
          IsPrimary=nil(IsPrimaryNilReason)
        );
        (
          rdf( IsPrimaryNode,gml:indeterminatePosition, IsPrimaryIndeterminate, Graph ),
          IsPrimary=indeterminate(IsPrimaryIndeterminate)
        )
      )
  )
  ,findall(A, rdf(AltimeterSourceTimeSlice,aixm:'availability',A,Graph), Availability)
  ,findall(A, rdf(AltimeterSourceTimeSlice,aixm:'annotation',A,Graph), Annotation) .

aixm_Point(Graph, Point, HorizontalAccuracy, Annotation) :-
  subClassOf(T,aixm:'Point')
  ,rdf(Point,rdf:type,T,Graph)
  ,(
    ( HorizontalAccuracy='$null$',
      \+ rdf( Point,aixm:'horizontalAccuracy',_HorizontalAccuracy,Graph )
    );
  ( rdf( Point,aixm:'horizontalAccuracy',HorizontalAccuracyNode,Graph )),
      (
        (
          rdf(HorizontalAccuracyNode,rdf:value,HorizontalAccuracyValue,Graph),
         \+ ( rdf( HorizontalAccuracyNode, aixm:uom, _HorizontalAccuracyUOM, Graph ); rdf( HorizontalAccuracyNode, fixm:uom, _HorizontalAccuracyUOM, Graph ); rdf( HorizontalAccuracyNode, plain:uom, _HorizontalAccuracyUOM, Graph ) ),
          HorizontalAccuracy=val(HorizontalAccuracyValue)
        );
        (
          rdf( HorizontalAccuracyNode,rdf:value,HorizontalAccuracyValue,Graph ),
          ( rdf( HorizontalAccuracyNode, aixm:uom, HorizontalAccuracyUOM, Graph ); rdf( HorizontalAccuracyNode, fixm:uom, HorizontalAccuracyUOM, Graph ); rdf( HorizontalAccuracyNode, plain:uom, HorizontalAccuracyUOM, Graph ) ),
          HorizontalAccuracy=xval(HorizontalAccuracyValue,HorizontalAccuracyUOM)
        );
        (
          rdf( HorizontalAccuracyNode,aixm:nilReason, HorizontalAccuracyNilReason, Graph ),
          HorizontalAccuracy=nil(HorizontalAccuracyNilReason)
        );
        (
          rdf( HorizontalAccuracyNode,gml:indeterminatePosition, HorizontalAccuracyIndeterminate, Graph ),
          HorizontalAccuracy=indeterminate(HorizontalAccuracyIndeterminate)
        )
      )
  )
  ,findall(A, rdf(Point,aixm:'annotation',A,Graph), Annotation) .

aixm_AircraftCharacteristic(Graph, AircraftCharacteristic, Type, Engine, NumberEngine, TypeAircraftICAO, AircraftLandingCategory, WingSpan, WingSpanInterpretation, ClassWingSpan, Weight, WeightInterpretation, Passengers, PassengersInterpretation, Speed, SpeedInterpretation, WakeTurbulence, NavigationEquipment, NavigationSpecification, VerticalSeparationCapability, AntiCollisionAndSeparationEquipment, CommunicationEquipment, SurveillanceEquipment, Annotation) :-
  rdf(AircraftCharacteristic,rdf:type,aixm:'AircraftCharacteristic',Graph)
  ,(
    ( Type='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'type',_Type,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'type',TypeNode,Graph )),
      (
        (
          rdf(TypeNode,rdf:value,TypeValue,Graph),
         \+ ( rdf( TypeNode, aixm:uom, _TypeUOM, Graph ); rdf( TypeNode, fixm:uom, _TypeUOM, Graph ); rdf( TypeNode, plain:uom, _TypeUOM, Graph ) ),
          Type=val(TypeValue)
        );
        (
          rdf( TypeNode,rdf:value,TypeValue,Graph ),
          ( rdf( TypeNode, aixm:uom, TypeUOM, Graph ); rdf( TypeNode, fixm:uom, TypeUOM, Graph ); rdf( TypeNode, plain:uom, TypeUOM, Graph ) ),
          Type=xval(TypeValue,TypeUOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, TypeNilReason, Graph ),
          Type=nil(TypeNilReason)
        );
        (
          rdf( TypeNode,gml:indeterminatePosition, TypeIndeterminate, Graph ),
          Type=indeterminate(TypeIndeterminate)
        )
      )
  )
  ,(
    ( Engine='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'engine',_Engine,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'engine',EngineNode,Graph )),
      (
        (
          rdf(EngineNode,rdf:value,EngineValue,Graph),
         \+ ( rdf( EngineNode, aixm:uom, _EngineUOM, Graph ); rdf( EngineNode, fixm:uom, _EngineUOM, Graph ); rdf( EngineNode, plain:uom, _EngineUOM, Graph ) ),
          Engine=val(EngineValue)
        );
        (
          rdf( EngineNode,rdf:value,EngineValue,Graph ),
          ( rdf( EngineNode, aixm:uom, EngineUOM, Graph ); rdf( EngineNode, fixm:uom, EngineUOM, Graph ); rdf( EngineNode, plain:uom, EngineUOM, Graph ) ),
          Engine=xval(EngineValue,EngineUOM)
        );
        (
          rdf( EngineNode,aixm:nilReason, EngineNilReason, Graph ),
          Engine=nil(EngineNilReason)
        );
        (
          rdf( EngineNode,gml:indeterminatePosition, EngineIndeterminate, Graph ),
          Engine=indeterminate(EngineIndeterminate)
        )
      )
  )
  ,(
    ( NumberEngine='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'numberEngine',_NumberEngine,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'numberEngine',NumberEngineNode,Graph )),
      (
        (
          rdf(NumberEngineNode,rdf:value,NumberEngineValue,Graph),
         \+ ( rdf( NumberEngineNode, aixm:uom, _NumberEngineUOM, Graph ); rdf( NumberEngineNode, fixm:uom, _NumberEngineUOM, Graph ); rdf( NumberEngineNode, plain:uom, _NumberEngineUOM, Graph ) ),
          NumberEngine=val(NumberEngineValue)
        );
        (
          rdf( NumberEngineNode,rdf:value,NumberEngineValue,Graph ),
          ( rdf( NumberEngineNode, aixm:uom, NumberEngineUOM, Graph ); rdf( NumberEngineNode, fixm:uom, NumberEngineUOM, Graph ); rdf( NumberEngineNode, plain:uom, NumberEngineUOM, Graph ) ),
          NumberEngine=xval(NumberEngineValue,NumberEngineUOM)
        );
        (
          rdf( NumberEngineNode,aixm:nilReason, NumberEngineNilReason, Graph ),
          NumberEngine=nil(NumberEngineNilReason)
        );
        (
          rdf( NumberEngineNode,gml:indeterminatePosition, NumberEngineIndeterminate, Graph ),
          NumberEngine=indeterminate(NumberEngineIndeterminate)
        )
      )
  )
  ,(
    ( TypeAircraftICAO='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'typeAircraftICAO',_TypeAircraftICAO,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'typeAircraftICAO',TypeAircraftICAONode,Graph )),
      (
        (
          rdf(TypeAircraftICAONode,rdf:value,TypeAircraftICAOValue,Graph),
         \+ ( rdf( TypeAircraftICAONode, aixm:uom, _TypeAircraftICAOUOM, Graph ); rdf( TypeAircraftICAONode, fixm:uom, _TypeAircraftICAOUOM, Graph ); rdf( TypeAircraftICAONode, plain:uom, _TypeAircraftICAOUOM, Graph ) ),
          TypeAircraftICAO=val(TypeAircraftICAOValue)
        );
        (
          rdf( TypeAircraftICAONode,rdf:value,TypeAircraftICAOValue,Graph ),
          ( rdf( TypeAircraftICAONode, aixm:uom, TypeAircraftICAOUOM, Graph ); rdf( TypeAircraftICAONode, fixm:uom, TypeAircraftICAOUOM, Graph ); rdf( TypeAircraftICAONode, plain:uom, TypeAircraftICAOUOM, Graph ) ),
          TypeAircraftICAO=xval(TypeAircraftICAOValue,TypeAircraftICAOUOM)
        );
        (
          rdf( TypeAircraftICAONode,aixm:nilReason, TypeAircraftICAONilReason, Graph ),
          TypeAircraftICAO=nil(TypeAircraftICAONilReason)
        );
        (
          rdf( TypeAircraftICAONode,gml:indeterminatePosition, TypeAircraftICAOIndeterminate, Graph ),
          TypeAircraftICAO=indeterminate(TypeAircraftICAOIndeterminate)
        )
      )
  )
  ,(
    ( AircraftLandingCategory='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'aircraftLandingCategory',_AircraftLandingCategory,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'aircraftLandingCategory',AircraftLandingCategoryNode,Graph )),
      (
        (
          rdf(AircraftLandingCategoryNode,rdf:value,AircraftLandingCategoryValue,Graph),
         \+ ( rdf( AircraftLandingCategoryNode, aixm:uom, _AircraftLandingCategoryUOM, Graph ); rdf( AircraftLandingCategoryNode, fixm:uom, _AircraftLandingCategoryUOM, Graph ); rdf( AircraftLandingCategoryNode, plain:uom, _AircraftLandingCategoryUOM, Graph ) ),
          AircraftLandingCategory=val(AircraftLandingCategoryValue)
        );
        (
          rdf( AircraftLandingCategoryNode,rdf:value,AircraftLandingCategoryValue,Graph ),
          ( rdf( AircraftLandingCategoryNode, aixm:uom, AircraftLandingCategoryUOM, Graph ); rdf( AircraftLandingCategoryNode, fixm:uom, AircraftLandingCategoryUOM, Graph ); rdf( AircraftLandingCategoryNode, plain:uom, AircraftLandingCategoryUOM, Graph ) ),
          AircraftLandingCategory=xval(AircraftLandingCategoryValue,AircraftLandingCategoryUOM)
        );
        (
          rdf( AircraftLandingCategoryNode,aixm:nilReason, AircraftLandingCategoryNilReason, Graph ),
          AircraftLandingCategory=nil(AircraftLandingCategoryNilReason)
        );
        (
          rdf( AircraftLandingCategoryNode,gml:indeterminatePosition, AircraftLandingCategoryIndeterminate, Graph ),
          AircraftLandingCategory=indeterminate(AircraftLandingCategoryIndeterminate)
        )
      )
  )
  ,(
    ( WingSpan='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'wingSpan',_WingSpan,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'wingSpan',WingSpanNode,Graph )),
      (
        (
          rdf(WingSpanNode,rdf:value,WingSpanValue,Graph),
         \+ ( rdf( WingSpanNode, aixm:uom, _WingSpanUOM, Graph ); rdf( WingSpanNode, fixm:uom, _WingSpanUOM, Graph ); rdf( WingSpanNode, plain:uom, _WingSpanUOM, Graph ) ),
          WingSpan=val(WingSpanValue)
        );
        (
          rdf( WingSpanNode,rdf:value,WingSpanValue,Graph ),
          ( rdf( WingSpanNode, aixm:uom, WingSpanUOM, Graph ); rdf( WingSpanNode, fixm:uom, WingSpanUOM, Graph ); rdf( WingSpanNode, plain:uom, WingSpanUOM, Graph ) ),
          WingSpan=xval(WingSpanValue,WingSpanUOM)
        );
        (
          rdf( WingSpanNode,aixm:nilReason, WingSpanNilReason, Graph ),
          WingSpan=nil(WingSpanNilReason)
        );
        (
          rdf( WingSpanNode,gml:indeterminatePosition, WingSpanIndeterminate, Graph ),
          WingSpan=indeterminate(WingSpanIndeterminate)
        )
      )
  )
  ,(
    ( WingSpanInterpretation='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'wingSpanInterpretation',_WingSpanInterpretation,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'wingSpanInterpretation',WingSpanInterpretationNode,Graph )),
      (
        (
          rdf(WingSpanInterpretationNode,rdf:value,WingSpanInterpretationValue,Graph),
         \+ ( rdf( WingSpanInterpretationNode, aixm:uom, _WingSpanInterpretationUOM, Graph ); rdf( WingSpanInterpretationNode, fixm:uom, _WingSpanInterpretationUOM, Graph ); rdf( WingSpanInterpretationNode, plain:uom, _WingSpanInterpretationUOM, Graph ) ),
          WingSpanInterpretation=val(WingSpanInterpretationValue)
        );
        (
          rdf( WingSpanInterpretationNode,rdf:value,WingSpanInterpretationValue,Graph ),
          ( rdf( WingSpanInterpretationNode, aixm:uom, WingSpanInterpretationUOM, Graph ); rdf( WingSpanInterpretationNode, fixm:uom, WingSpanInterpretationUOM, Graph ); rdf( WingSpanInterpretationNode, plain:uom, WingSpanInterpretationUOM, Graph ) ),
          WingSpanInterpretation=xval(WingSpanInterpretationValue,WingSpanInterpretationUOM)
        );
        (
          rdf( WingSpanInterpretationNode,aixm:nilReason, WingSpanInterpretationNilReason, Graph ),
          WingSpanInterpretation=nil(WingSpanInterpretationNilReason)
        );
        (
          rdf( WingSpanInterpretationNode,gml:indeterminatePosition, WingSpanInterpretationIndeterminate, Graph ),
          WingSpanInterpretation=indeterminate(WingSpanInterpretationIndeterminate)
        )
      )
  )
  ,(
    ( ClassWingSpan='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'classWingSpan',_ClassWingSpan,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'classWingSpan',ClassWingSpanNode,Graph )),
      (
        (
          rdf(ClassWingSpanNode,rdf:value,ClassWingSpanValue,Graph),
         \+ ( rdf( ClassWingSpanNode, aixm:uom, _ClassWingSpanUOM, Graph ); rdf( ClassWingSpanNode, fixm:uom, _ClassWingSpanUOM, Graph ); rdf( ClassWingSpanNode, plain:uom, _ClassWingSpanUOM, Graph ) ),
          ClassWingSpan=val(ClassWingSpanValue)
        );
        (
          rdf( ClassWingSpanNode,rdf:value,ClassWingSpanValue,Graph ),
          ( rdf( ClassWingSpanNode, aixm:uom, ClassWingSpanUOM, Graph ); rdf( ClassWingSpanNode, fixm:uom, ClassWingSpanUOM, Graph ); rdf( ClassWingSpanNode, plain:uom, ClassWingSpanUOM, Graph ) ),
          ClassWingSpan=xval(ClassWingSpanValue,ClassWingSpanUOM)
        );
        (
          rdf( ClassWingSpanNode,aixm:nilReason, ClassWingSpanNilReason, Graph ),
          ClassWingSpan=nil(ClassWingSpanNilReason)
        );
        (
          rdf( ClassWingSpanNode,gml:indeterminatePosition, ClassWingSpanIndeterminate, Graph ),
          ClassWingSpan=indeterminate(ClassWingSpanIndeterminate)
        )
      )
  )
  ,(
    ( Weight='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'weight',_Weight,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'weight',WeightNode,Graph )),
      (
        (
          rdf(WeightNode,rdf:value,WeightValue,Graph),
         \+ ( rdf( WeightNode, aixm:uom, _WeightUOM, Graph ); rdf( WeightNode, fixm:uom, _WeightUOM, Graph ); rdf( WeightNode, plain:uom, _WeightUOM, Graph ) ),
          Weight=val(WeightValue)
        );
        (
          rdf( WeightNode,rdf:value,WeightValue,Graph ),
          ( rdf( WeightNode, aixm:uom, WeightUOM, Graph ); rdf( WeightNode, fixm:uom, WeightUOM, Graph ); rdf( WeightNode, plain:uom, WeightUOM, Graph ) ),
          Weight=xval(WeightValue,WeightUOM)
        );
        (
          rdf( WeightNode,aixm:nilReason, WeightNilReason, Graph ),
          Weight=nil(WeightNilReason)
        );
        (
          rdf( WeightNode,gml:indeterminatePosition, WeightIndeterminate, Graph ),
          Weight=indeterminate(WeightIndeterminate)
        )
      )
  )
  ,(
    ( WeightInterpretation='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'weightInterpretation',_WeightInterpretation,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'weightInterpretation',WeightInterpretationNode,Graph )),
      (
        (
          rdf(WeightInterpretationNode,rdf:value,WeightInterpretationValue,Graph),
         \+ ( rdf( WeightInterpretationNode, aixm:uom, _WeightInterpretationUOM, Graph ); rdf( WeightInterpretationNode, fixm:uom, _WeightInterpretationUOM, Graph ); rdf( WeightInterpretationNode, plain:uom, _WeightInterpretationUOM, Graph ) ),
          WeightInterpretation=val(WeightInterpretationValue)
        );
        (
          rdf( WeightInterpretationNode,rdf:value,WeightInterpretationValue,Graph ),
          ( rdf( WeightInterpretationNode, aixm:uom, WeightInterpretationUOM, Graph ); rdf( WeightInterpretationNode, fixm:uom, WeightInterpretationUOM, Graph ); rdf( WeightInterpretationNode, plain:uom, WeightInterpretationUOM, Graph ) ),
          WeightInterpretation=xval(WeightInterpretationValue,WeightInterpretationUOM)
        );
        (
          rdf( WeightInterpretationNode,aixm:nilReason, WeightInterpretationNilReason, Graph ),
          WeightInterpretation=nil(WeightInterpretationNilReason)
        );
        (
          rdf( WeightInterpretationNode,gml:indeterminatePosition, WeightInterpretationIndeterminate, Graph ),
          WeightInterpretation=indeterminate(WeightInterpretationIndeterminate)
        )
      )
  )
  ,(
    ( Passengers='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'passengers',_Passengers,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'passengers',PassengersNode,Graph )),
      (
        (
          rdf(PassengersNode,rdf:value,PassengersValue,Graph),
         \+ ( rdf( PassengersNode, aixm:uom, _PassengersUOM, Graph ); rdf( PassengersNode, fixm:uom, _PassengersUOM, Graph ); rdf( PassengersNode, plain:uom, _PassengersUOM, Graph ) ),
          Passengers=val(PassengersValue)
        );
        (
          rdf( PassengersNode,rdf:value,PassengersValue,Graph ),
          ( rdf( PassengersNode, aixm:uom, PassengersUOM, Graph ); rdf( PassengersNode, fixm:uom, PassengersUOM, Graph ); rdf( PassengersNode, plain:uom, PassengersUOM, Graph ) ),
          Passengers=xval(PassengersValue,PassengersUOM)
        );
        (
          rdf( PassengersNode,aixm:nilReason, PassengersNilReason, Graph ),
          Passengers=nil(PassengersNilReason)
        );
        (
          rdf( PassengersNode,gml:indeterminatePosition, PassengersIndeterminate, Graph ),
          Passengers=indeterminate(PassengersIndeterminate)
        )
      )
  )
  ,(
    ( PassengersInterpretation='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'passengersInterpretation',_PassengersInterpretation,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'passengersInterpretation',PassengersInterpretationNode,Graph )),
      (
        (
          rdf(PassengersInterpretationNode,rdf:value,PassengersInterpretationValue,Graph),
         \+ ( rdf( PassengersInterpretationNode, aixm:uom, _PassengersInterpretationUOM, Graph ); rdf( PassengersInterpretationNode, fixm:uom, _PassengersInterpretationUOM, Graph ); rdf( PassengersInterpretationNode, plain:uom, _PassengersInterpretationUOM, Graph ) ),
          PassengersInterpretation=val(PassengersInterpretationValue)
        );
        (
          rdf( PassengersInterpretationNode,rdf:value,PassengersInterpretationValue,Graph ),
          ( rdf( PassengersInterpretationNode, aixm:uom, PassengersInterpretationUOM, Graph ); rdf( PassengersInterpretationNode, fixm:uom, PassengersInterpretationUOM, Graph ); rdf( PassengersInterpretationNode, plain:uom, PassengersInterpretationUOM, Graph ) ),
          PassengersInterpretation=xval(PassengersInterpretationValue,PassengersInterpretationUOM)
        );
        (
          rdf( PassengersInterpretationNode,aixm:nilReason, PassengersInterpretationNilReason, Graph ),
          PassengersInterpretation=nil(PassengersInterpretationNilReason)
        );
        (
          rdf( PassengersInterpretationNode,gml:indeterminatePosition, PassengersInterpretationIndeterminate, Graph ),
          PassengersInterpretation=indeterminate(PassengersInterpretationIndeterminate)
        )
      )
  )
  ,(
    ( Speed='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'speed',_Speed,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'speed',SpeedNode,Graph )),
      (
        (
          rdf(SpeedNode,rdf:value,SpeedValue,Graph),
         \+ ( rdf( SpeedNode, aixm:uom, _SpeedUOM, Graph ); rdf( SpeedNode, fixm:uom, _SpeedUOM, Graph ); rdf( SpeedNode, plain:uom, _SpeedUOM, Graph ) ),
          Speed=val(SpeedValue)
        );
        (
          rdf( SpeedNode,rdf:value,SpeedValue,Graph ),
          ( rdf( SpeedNode, aixm:uom, SpeedUOM, Graph ); rdf( SpeedNode, fixm:uom, SpeedUOM, Graph ); rdf( SpeedNode, plain:uom, SpeedUOM, Graph ) ),
          Speed=xval(SpeedValue,SpeedUOM)
        );
        (
          rdf( SpeedNode,aixm:nilReason, SpeedNilReason, Graph ),
          Speed=nil(SpeedNilReason)
        );
        (
          rdf( SpeedNode,gml:indeterminatePosition, SpeedIndeterminate, Graph ),
          Speed=indeterminate(SpeedIndeterminate)
        )
      )
  )
  ,(
    ( SpeedInterpretation='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'speedInterpretation',_SpeedInterpretation,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'speedInterpretation',SpeedInterpretationNode,Graph )),
      (
        (
          rdf(SpeedInterpretationNode,rdf:value,SpeedInterpretationValue,Graph),
         \+ ( rdf( SpeedInterpretationNode, aixm:uom, _SpeedInterpretationUOM, Graph ); rdf( SpeedInterpretationNode, fixm:uom, _SpeedInterpretationUOM, Graph ); rdf( SpeedInterpretationNode, plain:uom, _SpeedInterpretationUOM, Graph ) ),
          SpeedInterpretation=val(SpeedInterpretationValue)
        );
        (
          rdf( SpeedInterpretationNode,rdf:value,SpeedInterpretationValue,Graph ),
          ( rdf( SpeedInterpretationNode, aixm:uom, SpeedInterpretationUOM, Graph ); rdf( SpeedInterpretationNode, fixm:uom, SpeedInterpretationUOM, Graph ); rdf( SpeedInterpretationNode, plain:uom, SpeedInterpretationUOM, Graph ) ),
          SpeedInterpretation=xval(SpeedInterpretationValue,SpeedInterpretationUOM)
        );
        (
          rdf( SpeedInterpretationNode,aixm:nilReason, SpeedInterpretationNilReason, Graph ),
          SpeedInterpretation=nil(SpeedInterpretationNilReason)
        );
        (
          rdf( SpeedInterpretationNode,gml:indeterminatePosition, SpeedInterpretationIndeterminate, Graph ),
          SpeedInterpretation=indeterminate(SpeedInterpretationIndeterminate)
        )
      )
  )
  ,(
    ( WakeTurbulence='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'wakeTurbulence',_WakeTurbulence,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'wakeTurbulence',WakeTurbulenceNode,Graph )),
      (
        (
          rdf(WakeTurbulenceNode,rdf:value,WakeTurbulenceValue,Graph),
         \+ ( rdf( WakeTurbulenceNode, aixm:uom, _WakeTurbulenceUOM, Graph ); rdf( WakeTurbulenceNode, fixm:uom, _WakeTurbulenceUOM, Graph ); rdf( WakeTurbulenceNode, plain:uom, _WakeTurbulenceUOM, Graph ) ),
          WakeTurbulence=val(WakeTurbulenceValue)
        );
        (
          rdf( WakeTurbulenceNode,rdf:value,WakeTurbulenceValue,Graph ),
          ( rdf( WakeTurbulenceNode, aixm:uom, WakeTurbulenceUOM, Graph ); rdf( WakeTurbulenceNode, fixm:uom, WakeTurbulenceUOM, Graph ); rdf( WakeTurbulenceNode, plain:uom, WakeTurbulenceUOM, Graph ) ),
          WakeTurbulence=xval(WakeTurbulenceValue,WakeTurbulenceUOM)
        );
        (
          rdf( WakeTurbulenceNode,aixm:nilReason, WakeTurbulenceNilReason, Graph ),
          WakeTurbulence=nil(WakeTurbulenceNilReason)
        );
        (
          rdf( WakeTurbulenceNode,gml:indeterminatePosition, WakeTurbulenceIndeterminate, Graph ),
          WakeTurbulence=indeterminate(WakeTurbulenceIndeterminate)
        )
      )
  )
  ,(
    ( NavigationEquipment='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'navigationEquipment',_NavigationEquipment,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'navigationEquipment',NavigationEquipmentNode,Graph )),
      (
        (
          rdf(NavigationEquipmentNode,rdf:value,NavigationEquipmentValue,Graph),
         \+ ( rdf( NavigationEquipmentNode, aixm:uom, _NavigationEquipmentUOM, Graph ); rdf( NavigationEquipmentNode, fixm:uom, _NavigationEquipmentUOM, Graph ); rdf( NavigationEquipmentNode, plain:uom, _NavigationEquipmentUOM, Graph ) ),
          NavigationEquipment=val(NavigationEquipmentValue)
        );
        (
          rdf( NavigationEquipmentNode,rdf:value,NavigationEquipmentValue,Graph ),
          ( rdf( NavigationEquipmentNode, aixm:uom, NavigationEquipmentUOM, Graph ); rdf( NavigationEquipmentNode, fixm:uom, NavigationEquipmentUOM, Graph ); rdf( NavigationEquipmentNode, plain:uom, NavigationEquipmentUOM, Graph ) ),
          NavigationEquipment=xval(NavigationEquipmentValue,NavigationEquipmentUOM)
        );
        (
          rdf( NavigationEquipmentNode,aixm:nilReason, NavigationEquipmentNilReason, Graph ),
          NavigationEquipment=nil(NavigationEquipmentNilReason)
        );
        (
          rdf( NavigationEquipmentNode,gml:indeterminatePosition, NavigationEquipmentIndeterminate, Graph ),
          NavigationEquipment=indeterminate(NavigationEquipmentIndeterminate)
        )
      )
  )
  ,(
    ( NavigationSpecification='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'navigationSpecification',_NavigationSpecification,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'navigationSpecification',NavigationSpecificationNode,Graph )),
      (
        (
          rdf(NavigationSpecificationNode,rdf:value,NavigationSpecificationValue,Graph),
         \+ ( rdf( NavigationSpecificationNode, aixm:uom, _NavigationSpecificationUOM, Graph ); rdf( NavigationSpecificationNode, fixm:uom, _NavigationSpecificationUOM, Graph ); rdf( NavigationSpecificationNode, plain:uom, _NavigationSpecificationUOM, Graph ) ),
          NavigationSpecification=val(NavigationSpecificationValue)
        );
        (
          rdf( NavigationSpecificationNode,rdf:value,NavigationSpecificationValue,Graph ),
          ( rdf( NavigationSpecificationNode, aixm:uom, NavigationSpecificationUOM, Graph ); rdf( NavigationSpecificationNode, fixm:uom, NavigationSpecificationUOM, Graph ); rdf( NavigationSpecificationNode, plain:uom, NavigationSpecificationUOM, Graph ) ),
          NavigationSpecification=xval(NavigationSpecificationValue,NavigationSpecificationUOM)
        );
        (
          rdf( NavigationSpecificationNode,aixm:nilReason, NavigationSpecificationNilReason, Graph ),
          NavigationSpecification=nil(NavigationSpecificationNilReason)
        );
        (
          rdf( NavigationSpecificationNode,gml:indeterminatePosition, NavigationSpecificationIndeterminate, Graph ),
          NavigationSpecification=indeterminate(NavigationSpecificationIndeterminate)
        )
      )
  )
  ,(
    ( VerticalSeparationCapability='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'verticalSeparationCapability',_VerticalSeparationCapability,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'verticalSeparationCapability',VerticalSeparationCapabilityNode,Graph )),
      (
        (
          rdf(VerticalSeparationCapabilityNode,rdf:value,VerticalSeparationCapabilityValue,Graph),
         \+ ( rdf( VerticalSeparationCapabilityNode, aixm:uom, _VerticalSeparationCapabilityUOM, Graph ); rdf( VerticalSeparationCapabilityNode, fixm:uom, _VerticalSeparationCapabilityUOM, Graph ); rdf( VerticalSeparationCapabilityNode, plain:uom, _VerticalSeparationCapabilityUOM, Graph ) ),
          VerticalSeparationCapability=val(VerticalSeparationCapabilityValue)
        );
        (
          rdf( VerticalSeparationCapabilityNode,rdf:value,VerticalSeparationCapabilityValue,Graph ),
          ( rdf( VerticalSeparationCapabilityNode, aixm:uom, VerticalSeparationCapabilityUOM, Graph ); rdf( VerticalSeparationCapabilityNode, fixm:uom, VerticalSeparationCapabilityUOM, Graph ); rdf( VerticalSeparationCapabilityNode, plain:uom, VerticalSeparationCapabilityUOM, Graph ) ),
          VerticalSeparationCapability=xval(VerticalSeparationCapabilityValue,VerticalSeparationCapabilityUOM)
        );
        (
          rdf( VerticalSeparationCapabilityNode,aixm:nilReason, VerticalSeparationCapabilityNilReason, Graph ),
          VerticalSeparationCapability=nil(VerticalSeparationCapabilityNilReason)
        );
        (
          rdf( VerticalSeparationCapabilityNode,gml:indeterminatePosition, VerticalSeparationCapabilityIndeterminate, Graph ),
          VerticalSeparationCapability=indeterminate(VerticalSeparationCapabilityIndeterminate)
        )
      )
  )
  ,(
    ( AntiCollisionAndSeparationEquipment='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'antiCollisionAndSeparationEquipment',_AntiCollisionAndSeparationEquipment,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'antiCollisionAndSeparationEquipment',AntiCollisionAndSeparationEquipmentNode,Graph )),
      (
        (
          rdf(AntiCollisionAndSeparationEquipmentNode,rdf:value,AntiCollisionAndSeparationEquipmentValue,Graph),
         \+ ( rdf( AntiCollisionAndSeparationEquipmentNode, aixm:uom, _AntiCollisionAndSeparationEquipmentUOM, Graph ); rdf( AntiCollisionAndSeparationEquipmentNode, fixm:uom, _AntiCollisionAndSeparationEquipmentUOM, Graph ); rdf( AntiCollisionAndSeparationEquipmentNode, plain:uom, _AntiCollisionAndSeparationEquipmentUOM, Graph ) ),
          AntiCollisionAndSeparationEquipment=val(AntiCollisionAndSeparationEquipmentValue)
        );
        (
          rdf( AntiCollisionAndSeparationEquipmentNode,rdf:value,AntiCollisionAndSeparationEquipmentValue,Graph ),
          ( rdf( AntiCollisionAndSeparationEquipmentNode, aixm:uom, AntiCollisionAndSeparationEquipmentUOM, Graph ); rdf( AntiCollisionAndSeparationEquipmentNode, fixm:uom, AntiCollisionAndSeparationEquipmentUOM, Graph ); rdf( AntiCollisionAndSeparationEquipmentNode, plain:uom, AntiCollisionAndSeparationEquipmentUOM, Graph ) ),
          AntiCollisionAndSeparationEquipment=xval(AntiCollisionAndSeparationEquipmentValue,AntiCollisionAndSeparationEquipmentUOM)
        );
        (
          rdf( AntiCollisionAndSeparationEquipmentNode,aixm:nilReason, AntiCollisionAndSeparationEquipmentNilReason, Graph ),
          AntiCollisionAndSeparationEquipment=nil(AntiCollisionAndSeparationEquipmentNilReason)
        );
        (
          rdf( AntiCollisionAndSeparationEquipmentNode,gml:indeterminatePosition, AntiCollisionAndSeparationEquipmentIndeterminate, Graph ),
          AntiCollisionAndSeparationEquipment=indeterminate(AntiCollisionAndSeparationEquipmentIndeterminate)
        )
      )
  )
  ,(
    ( CommunicationEquipment='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'communicationEquipment',_CommunicationEquipment,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'communicationEquipment',CommunicationEquipmentNode,Graph )),
      (
        (
          rdf(CommunicationEquipmentNode,rdf:value,CommunicationEquipmentValue,Graph),
         \+ ( rdf( CommunicationEquipmentNode, aixm:uom, _CommunicationEquipmentUOM, Graph ); rdf( CommunicationEquipmentNode, fixm:uom, _CommunicationEquipmentUOM, Graph ); rdf( CommunicationEquipmentNode, plain:uom, _CommunicationEquipmentUOM, Graph ) ),
          CommunicationEquipment=val(CommunicationEquipmentValue)
        );
        (
          rdf( CommunicationEquipmentNode,rdf:value,CommunicationEquipmentValue,Graph ),
          ( rdf( CommunicationEquipmentNode, aixm:uom, CommunicationEquipmentUOM, Graph ); rdf( CommunicationEquipmentNode, fixm:uom, CommunicationEquipmentUOM, Graph ); rdf( CommunicationEquipmentNode, plain:uom, CommunicationEquipmentUOM, Graph ) ),
          CommunicationEquipment=xval(CommunicationEquipmentValue,CommunicationEquipmentUOM)
        );
        (
          rdf( CommunicationEquipmentNode,aixm:nilReason, CommunicationEquipmentNilReason, Graph ),
          CommunicationEquipment=nil(CommunicationEquipmentNilReason)
        );
        (
          rdf( CommunicationEquipmentNode,gml:indeterminatePosition, CommunicationEquipmentIndeterminate, Graph ),
          CommunicationEquipment=indeterminate(CommunicationEquipmentIndeterminate)
        )
      )
  )
  ,(
    ( SurveillanceEquipment='$null$',
      \+ rdf( AircraftCharacteristic,aixm:'surveillanceEquipment',_SurveillanceEquipment,Graph )
    );
  ( rdf( AircraftCharacteristic,aixm:'surveillanceEquipment',SurveillanceEquipmentNode,Graph )),
      (
        (
          rdf(SurveillanceEquipmentNode,rdf:value,SurveillanceEquipmentValue,Graph),
         \+ ( rdf( SurveillanceEquipmentNode, aixm:uom, _SurveillanceEquipmentUOM, Graph ); rdf( SurveillanceEquipmentNode, fixm:uom, _SurveillanceEquipmentUOM, Graph ); rdf( SurveillanceEquipmentNode, plain:uom, _SurveillanceEquipmentUOM, Graph ) ),
          SurveillanceEquipment=val(SurveillanceEquipmentValue)
        );
        (
          rdf( SurveillanceEquipmentNode,rdf:value,SurveillanceEquipmentValue,Graph ),
          ( rdf( SurveillanceEquipmentNode, aixm:uom, SurveillanceEquipmentUOM, Graph ); rdf( SurveillanceEquipmentNode, fixm:uom, SurveillanceEquipmentUOM, Graph ); rdf( SurveillanceEquipmentNode, plain:uom, SurveillanceEquipmentUOM, Graph ) ),
          SurveillanceEquipment=xval(SurveillanceEquipmentValue,SurveillanceEquipmentUOM)
        );
        (
          rdf( SurveillanceEquipmentNode,aixm:nilReason, SurveillanceEquipmentNilReason, Graph ),
          SurveillanceEquipment=nil(SurveillanceEquipmentNilReason)
        );
        (
          rdf( SurveillanceEquipmentNode,gml:indeterminatePosition, SurveillanceEquipmentIndeterminate, Graph ),
          SurveillanceEquipment=indeterminate(SurveillanceEquipmentIndeterminate)
        )
      )
  )
  ,findall(A, rdf(AircraftCharacteristic,aixm:'annotation',A,Graph), Annotation) .

aixm_PostalAddress(Graph, PostalAddress, DeliveryPoint, City, AdministrativeArea, PostalCode, Country) :-
  rdf(PostalAddress,rdf:type,aixm:'PostalAddress',Graph)
  ,(
    ( DeliveryPoint='$null$',
      \+ rdf( PostalAddress,aixm:'deliveryPoint',_DeliveryPoint,Graph )
    );
  ( rdf( PostalAddress,aixm:'deliveryPoint',DeliveryPointNode,Graph )),
      (
        (
          rdf(DeliveryPointNode,rdf:value,DeliveryPointValue,Graph),
         \+ ( rdf( DeliveryPointNode, aixm:uom, _DeliveryPointUOM, Graph ); rdf( DeliveryPointNode, fixm:uom, _DeliveryPointUOM, Graph ); rdf( DeliveryPointNode, plain:uom, _DeliveryPointUOM, Graph ) ),
          DeliveryPoint=val(DeliveryPointValue)
        );
        (
          rdf( DeliveryPointNode,rdf:value,DeliveryPointValue,Graph ),
          ( rdf( DeliveryPointNode, aixm:uom, DeliveryPointUOM, Graph ); rdf( DeliveryPointNode, fixm:uom, DeliveryPointUOM, Graph ); rdf( DeliveryPointNode, plain:uom, DeliveryPointUOM, Graph ) ),
          DeliveryPoint=xval(DeliveryPointValue,DeliveryPointUOM)
        );
        (
          rdf( DeliveryPointNode,aixm:nilReason, DeliveryPointNilReason, Graph ),
          DeliveryPoint=nil(DeliveryPointNilReason)
        );
        (
          rdf( DeliveryPointNode,gml:indeterminatePosition, DeliveryPointIndeterminate, Graph ),
          DeliveryPoint=indeterminate(DeliveryPointIndeterminate)
        )
      )
  )
  ,(
    ( City='$null$',
      \+ rdf( PostalAddress,aixm:'city',_City,Graph )
    );
  ( rdf( PostalAddress,aixm:'city',CityNode,Graph )),
      (
        (
          rdf(CityNode,rdf:value,CityValue,Graph),
         \+ ( rdf( CityNode, aixm:uom, _CityUOM, Graph ); rdf( CityNode, fixm:uom, _CityUOM, Graph ); rdf( CityNode, plain:uom, _CityUOM, Graph ) ),
          City=val(CityValue)
        );
        (
          rdf( CityNode,rdf:value,CityValue,Graph ),
          ( rdf( CityNode, aixm:uom, CityUOM, Graph ); rdf( CityNode, fixm:uom, CityUOM, Graph ); rdf( CityNode, plain:uom, CityUOM, Graph ) ),
          City=xval(CityValue,CityUOM)
        );
        (
          rdf( CityNode,aixm:nilReason, CityNilReason, Graph ),
          City=nil(CityNilReason)
        );
        (
          rdf( CityNode,gml:indeterminatePosition, CityIndeterminate, Graph ),
          City=indeterminate(CityIndeterminate)
        )
      )
  )
  ,(
    ( AdministrativeArea='$null$',
      \+ rdf( PostalAddress,aixm:'administrativeArea',_AdministrativeArea,Graph )
    );
  ( rdf( PostalAddress,aixm:'administrativeArea',AdministrativeAreaNode,Graph )),
      (
        (
          rdf(AdministrativeAreaNode,rdf:value,AdministrativeAreaValue,Graph),
         \+ ( rdf( AdministrativeAreaNode, aixm:uom, _AdministrativeAreaUOM, Graph ); rdf( AdministrativeAreaNode, fixm:uom, _AdministrativeAreaUOM, Graph ); rdf( AdministrativeAreaNode, plain:uom, _AdministrativeAreaUOM, Graph ) ),
          AdministrativeArea=val(AdministrativeAreaValue)
        );
        (
          rdf( AdministrativeAreaNode,rdf:value,AdministrativeAreaValue,Graph ),
          ( rdf( AdministrativeAreaNode, aixm:uom, AdministrativeAreaUOM, Graph ); rdf( AdministrativeAreaNode, fixm:uom, AdministrativeAreaUOM, Graph ); rdf( AdministrativeAreaNode, plain:uom, AdministrativeAreaUOM, Graph ) ),
          AdministrativeArea=xval(AdministrativeAreaValue,AdministrativeAreaUOM)
        );
        (
          rdf( AdministrativeAreaNode,aixm:nilReason, AdministrativeAreaNilReason, Graph ),
          AdministrativeArea=nil(AdministrativeAreaNilReason)
        );
        (
          rdf( AdministrativeAreaNode,gml:indeterminatePosition, AdministrativeAreaIndeterminate, Graph ),
          AdministrativeArea=indeterminate(AdministrativeAreaIndeterminate)
        )
      )
  )
  ,(
    ( PostalCode='$null$',
      \+ rdf( PostalAddress,aixm:'postalCode',_PostalCode,Graph )
    );
  ( rdf( PostalAddress,aixm:'postalCode',PostalCodeNode,Graph )),
      (
        (
          rdf(PostalCodeNode,rdf:value,PostalCodeValue,Graph),
         \+ ( rdf( PostalCodeNode, aixm:uom, _PostalCodeUOM, Graph ); rdf( PostalCodeNode, fixm:uom, _PostalCodeUOM, Graph ); rdf( PostalCodeNode, plain:uom, _PostalCodeUOM, Graph ) ),
          PostalCode=val(PostalCodeValue)
        );
        (
          rdf( PostalCodeNode,rdf:value,PostalCodeValue,Graph ),
          ( rdf( PostalCodeNode, aixm:uom, PostalCodeUOM, Graph ); rdf( PostalCodeNode, fixm:uom, PostalCodeUOM, Graph ); rdf( PostalCodeNode, plain:uom, PostalCodeUOM, Graph ) ),
          PostalCode=xval(PostalCodeValue,PostalCodeUOM)
        );
        (
          rdf( PostalCodeNode,aixm:nilReason, PostalCodeNilReason, Graph ),
          PostalCode=nil(PostalCodeNilReason)
        );
        (
          rdf( PostalCodeNode,gml:indeterminatePosition, PostalCodeIndeterminate, Graph ),
          PostalCode=indeterminate(PostalCodeIndeterminate)
        )
      )
  )
  ,(
    ( Country='$null$',
      \+ rdf( PostalAddress,aixm:'country',_Country,Graph )
    );
  ( rdf( PostalAddress,aixm:'country',CountryNode,Graph )),
      (
        (
          rdf(CountryNode,rdf:value,CountryValue,Graph),
         \+ ( rdf( CountryNode, aixm:uom, _CountryUOM, Graph ); rdf( CountryNode, fixm:uom, _CountryUOM, Graph ); rdf( CountryNode, plain:uom, _CountryUOM, Graph ) ),
          Country=val(CountryValue)
        );
        (
          rdf( CountryNode,rdf:value,CountryValue,Graph ),
          ( rdf( CountryNode, aixm:uom, CountryUOM, Graph ); rdf( CountryNode, fixm:uom, CountryUOM, Graph ); rdf( CountryNode, plain:uom, CountryUOM, Graph ) ),
          Country=xval(CountryValue,CountryUOM)
        );
        (
          rdf( CountryNode,aixm:nilReason, CountryNilReason, Graph ),
          Country=nil(CountryNilReason)
        );
        (
          rdf( CountryNode,gml:indeterminatePosition, CountryIndeterminate, Graph ),
          Country=indeterminate(CountryIndeterminate)
        )
      )
  ) .

fixm_DangerousGoodsPackage(Graph, DangerousGoodsPackage, DangerousGoodsQuantity, PackageDimensions, PackingInstructionNumber, ProductName, ProperShippingName, ReportableQuantity, SupplementaryInformation, TechnicalName, TypeOfPackaging, UnNumber, DangerousGoodsLimitation, ShipmentType, AllPackedInOne, CompatibilityGroup, ShipmentDimensions, MarinePollutantIndicator, RadioactiveMaterials, HazardClass, PackingGroup, Temperatures, OverpackIndicator, SubsidiaryHazardClass) :-
  rdf(DangerousGoodsPackage,rdf:type,fixm:'DangerousGoodsPackage',Graph)
  ,(
    ( DangerousGoodsQuantity='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'dangerousGoodsQuantity',_DangerousGoodsQuantity,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'dangerousGoodsQuantity',DangerousGoodsQuantityNode,Graph )),
      (
        (
          rdf(DangerousGoodsQuantityNode,rdf:value,DangerousGoodsQuantityValue,Graph),
         \+ ( rdf( DangerousGoodsQuantityNode, aixm:uom, _DangerousGoodsQuantityUOM, Graph ); rdf( DangerousGoodsQuantityNode, fixm:uom, _DangerousGoodsQuantityUOM, Graph ); rdf( DangerousGoodsQuantityNode, plain:uom, _DangerousGoodsQuantityUOM, Graph ) ),
          DangerousGoodsQuantity=val(DangerousGoodsQuantityValue)
        );
        (
          rdf( DangerousGoodsQuantityNode,rdf:value,DangerousGoodsQuantityValue,Graph ),
          ( rdf( DangerousGoodsQuantityNode, aixm:uom, DangerousGoodsQuantityUOM, Graph ); rdf( DangerousGoodsQuantityNode, fixm:uom, DangerousGoodsQuantityUOM, Graph ); rdf( DangerousGoodsQuantityNode, plain:uom, DangerousGoodsQuantityUOM, Graph ) ),
          DangerousGoodsQuantity=xval(DangerousGoodsQuantityValue,DangerousGoodsQuantityUOM)
        );
        (
          rdf( DangerousGoodsQuantityNode,aixm:nilReason, DangerousGoodsQuantityNilReason, Graph ),
          DangerousGoodsQuantity=nil(DangerousGoodsQuantityNilReason)
        );
        (
          rdf( DangerousGoodsQuantityNode,gml:indeterminatePosition, DangerousGoodsQuantityIndeterminate, Graph ),
          DangerousGoodsQuantity=indeterminate(DangerousGoodsQuantityIndeterminate)
        )
      )
  )
  ,( ( PackageDimensions='$null$',
    \+ rdf( DangerousGoodsPackage,fixm:'packageDimensions', _PackageDimensions, Graph  )
   ; rdf(DangerousGoodsPackage,fixm:'packageDimensions', PackageDimensions, Graph ) )
  )
  ,(
    ( PackingInstructionNumber='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'packingInstructionNumber',_PackingInstructionNumber,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'packingInstructionNumber',PackingInstructionNumberNode,Graph )),
      (
        (
          rdf(PackingInstructionNumberNode,rdf:value,PackingInstructionNumberValue,Graph),
         \+ ( rdf( PackingInstructionNumberNode, aixm:uom, _PackingInstructionNumberUOM, Graph ); rdf( PackingInstructionNumberNode, fixm:uom, _PackingInstructionNumberUOM, Graph ); rdf( PackingInstructionNumberNode, plain:uom, _PackingInstructionNumberUOM, Graph ) ),
          PackingInstructionNumber=val(PackingInstructionNumberValue)
        );
        (
          rdf( PackingInstructionNumberNode,rdf:value,PackingInstructionNumberValue,Graph ),
          ( rdf( PackingInstructionNumberNode, aixm:uom, PackingInstructionNumberUOM, Graph ); rdf( PackingInstructionNumberNode, fixm:uom, PackingInstructionNumberUOM, Graph ); rdf( PackingInstructionNumberNode, plain:uom, PackingInstructionNumberUOM, Graph ) ),
          PackingInstructionNumber=xval(PackingInstructionNumberValue,PackingInstructionNumberUOM)
        );
        (
          rdf( PackingInstructionNumberNode,aixm:nilReason, PackingInstructionNumberNilReason, Graph ),
          PackingInstructionNumber=nil(PackingInstructionNumberNilReason)
        );
        (
          rdf( PackingInstructionNumberNode,gml:indeterminatePosition, PackingInstructionNumberIndeterminate, Graph ),
          PackingInstructionNumber=indeterminate(PackingInstructionNumberIndeterminate)
        )
      )
  )
  ,(
    ( ProductName='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'productName',_ProductName,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'productName',ProductNameNode,Graph )),
      (
        (
          rdf(ProductNameNode,rdf:value,ProductNameValue,Graph),
         \+ ( rdf( ProductNameNode, aixm:uom, _ProductNameUOM, Graph ); rdf( ProductNameNode, fixm:uom, _ProductNameUOM, Graph ); rdf( ProductNameNode, plain:uom, _ProductNameUOM, Graph ) ),
          ProductName=val(ProductNameValue)
        );
        (
          rdf( ProductNameNode,rdf:value,ProductNameValue,Graph ),
          ( rdf( ProductNameNode, aixm:uom, ProductNameUOM, Graph ); rdf( ProductNameNode, fixm:uom, ProductNameUOM, Graph ); rdf( ProductNameNode, plain:uom, ProductNameUOM, Graph ) ),
          ProductName=xval(ProductNameValue,ProductNameUOM)
        );
        (
          rdf( ProductNameNode,aixm:nilReason, ProductNameNilReason, Graph ),
          ProductName=nil(ProductNameNilReason)
        );
        (
          rdf( ProductNameNode,gml:indeterminatePosition, ProductNameIndeterminate, Graph ),
          ProductName=indeterminate(ProductNameIndeterminate)
        )
      )
  )
  ,(
    ( ProperShippingName='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'properShippingName',_ProperShippingName,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'properShippingName',ProperShippingNameNode,Graph )),
      (
        (
          rdf(ProperShippingNameNode,rdf:value,ProperShippingNameValue,Graph),
         \+ ( rdf( ProperShippingNameNode, aixm:uom, _ProperShippingNameUOM, Graph ); rdf( ProperShippingNameNode, fixm:uom, _ProperShippingNameUOM, Graph ); rdf( ProperShippingNameNode, plain:uom, _ProperShippingNameUOM, Graph ) ),
          ProperShippingName=val(ProperShippingNameValue)
        );
        (
          rdf( ProperShippingNameNode,rdf:value,ProperShippingNameValue,Graph ),
          ( rdf( ProperShippingNameNode, aixm:uom, ProperShippingNameUOM, Graph ); rdf( ProperShippingNameNode, fixm:uom, ProperShippingNameUOM, Graph ); rdf( ProperShippingNameNode, plain:uom, ProperShippingNameUOM, Graph ) ),
          ProperShippingName=xval(ProperShippingNameValue,ProperShippingNameUOM)
        );
        (
          rdf( ProperShippingNameNode,aixm:nilReason, ProperShippingNameNilReason, Graph ),
          ProperShippingName=nil(ProperShippingNameNilReason)
        );
        (
          rdf( ProperShippingNameNode,gml:indeterminatePosition, ProperShippingNameIndeterminate, Graph ),
          ProperShippingName=indeterminate(ProperShippingNameIndeterminate)
        )
      )
  )
  ,(
    ( ReportableQuantity='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'reportableQuantity',_ReportableQuantity,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'reportableQuantity',ReportableQuantityNode,Graph )),
      (
        (
          rdf(ReportableQuantityNode,rdf:value,ReportableQuantityValue,Graph),
         \+ ( rdf( ReportableQuantityNode, aixm:uom, _ReportableQuantityUOM, Graph ); rdf( ReportableQuantityNode, fixm:uom, _ReportableQuantityUOM, Graph ); rdf( ReportableQuantityNode, plain:uom, _ReportableQuantityUOM, Graph ) ),
          ReportableQuantity=val(ReportableQuantityValue)
        );
        (
          rdf( ReportableQuantityNode,rdf:value,ReportableQuantityValue,Graph ),
          ( rdf( ReportableQuantityNode, aixm:uom, ReportableQuantityUOM, Graph ); rdf( ReportableQuantityNode, fixm:uom, ReportableQuantityUOM, Graph ); rdf( ReportableQuantityNode, plain:uom, ReportableQuantityUOM, Graph ) ),
          ReportableQuantity=xval(ReportableQuantityValue,ReportableQuantityUOM)
        );
        (
          rdf( ReportableQuantityNode,aixm:nilReason, ReportableQuantityNilReason, Graph ),
          ReportableQuantity=nil(ReportableQuantityNilReason)
        );
        (
          rdf( ReportableQuantityNode,gml:indeterminatePosition, ReportableQuantityIndeterminate, Graph ),
          ReportableQuantity=indeterminate(ReportableQuantityIndeterminate)
        )
      )
  )
  ,(
    ( SupplementaryInformation='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'supplementaryInformation',_SupplementaryInformation,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'supplementaryInformation',SupplementaryInformationNode,Graph )),
      (
        (
          rdf(SupplementaryInformationNode,rdf:value,SupplementaryInformationValue,Graph),
         \+ ( rdf( SupplementaryInformationNode, aixm:uom, _SupplementaryInformationUOM, Graph ); rdf( SupplementaryInformationNode, fixm:uom, _SupplementaryInformationUOM, Graph ); rdf( SupplementaryInformationNode, plain:uom, _SupplementaryInformationUOM, Graph ) ),
          SupplementaryInformation=val(SupplementaryInformationValue)
        );
        (
          rdf( SupplementaryInformationNode,rdf:value,SupplementaryInformationValue,Graph ),
          ( rdf( SupplementaryInformationNode, aixm:uom, SupplementaryInformationUOM, Graph ); rdf( SupplementaryInformationNode, fixm:uom, SupplementaryInformationUOM, Graph ); rdf( SupplementaryInformationNode, plain:uom, SupplementaryInformationUOM, Graph ) ),
          SupplementaryInformation=xval(SupplementaryInformationValue,SupplementaryInformationUOM)
        );
        (
          rdf( SupplementaryInformationNode,aixm:nilReason, SupplementaryInformationNilReason, Graph ),
          SupplementaryInformation=nil(SupplementaryInformationNilReason)
        );
        (
          rdf( SupplementaryInformationNode,gml:indeterminatePosition, SupplementaryInformationIndeterminate, Graph ),
          SupplementaryInformation=indeterminate(SupplementaryInformationIndeterminate)
        )
      )
  )
  ,(
    ( TechnicalName='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'technicalName',_TechnicalName,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'technicalName',TechnicalNameNode,Graph )),
      (
        (
          rdf(TechnicalNameNode,rdf:value,TechnicalNameValue,Graph),
         \+ ( rdf( TechnicalNameNode, aixm:uom, _TechnicalNameUOM, Graph ); rdf( TechnicalNameNode, fixm:uom, _TechnicalNameUOM, Graph ); rdf( TechnicalNameNode, plain:uom, _TechnicalNameUOM, Graph ) ),
          TechnicalName=val(TechnicalNameValue)
        );
        (
          rdf( TechnicalNameNode,rdf:value,TechnicalNameValue,Graph ),
          ( rdf( TechnicalNameNode, aixm:uom, TechnicalNameUOM, Graph ); rdf( TechnicalNameNode, fixm:uom, TechnicalNameUOM, Graph ); rdf( TechnicalNameNode, plain:uom, TechnicalNameUOM, Graph ) ),
          TechnicalName=xval(TechnicalNameValue,TechnicalNameUOM)
        );
        (
          rdf( TechnicalNameNode,aixm:nilReason, TechnicalNameNilReason, Graph ),
          TechnicalName=nil(TechnicalNameNilReason)
        );
        (
          rdf( TechnicalNameNode,gml:indeterminatePosition, TechnicalNameIndeterminate, Graph ),
          TechnicalName=indeterminate(TechnicalNameIndeterminate)
        )
      )
  )
  ,(
    ( TypeOfPackaging='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'typeOfPackaging',_TypeOfPackaging,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'typeOfPackaging',TypeOfPackagingNode,Graph )),
      (
        (
          rdf(TypeOfPackagingNode,rdf:value,TypeOfPackagingValue,Graph),
         \+ ( rdf( TypeOfPackagingNode, aixm:uom, _TypeOfPackagingUOM, Graph ); rdf( TypeOfPackagingNode, fixm:uom, _TypeOfPackagingUOM, Graph ); rdf( TypeOfPackagingNode, plain:uom, _TypeOfPackagingUOM, Graph ) ),
          TypeOfPackaging=val(TypeOfPackagingValue)
        );
        (
          rdf( TypeOfPackagingNode,rdf:value,TypeOfPackagingValue,Graph ),
          ( rdf( TypeOfPackagingNode, aixm:uom, TypeOfPackagingUOM, Graph ); rdf( TypeOfPackagingNode, fixm:uom, TypeOfPackagingUOM, Graph ); rdf( TypeOfPackagingNode, plain:uom, TypeOfPackagingUOM, Graph ) ),
          TypeOfPackaging=xval(TypeOfPackagingValue,TypeOfPackagingUOM)
        );
        (
          rdf( TypeOfPackagingNode,aixm:nilReason, TypeOfPackagingNilReason, Graph ),
          TypeOfPackaging=nil(TypeOfPackagingNilReason)
        );
        (
          rdf( TypeOfPackagingNode,gml:indeterminatePosition, TypeOfPackagingIndeterminate, Graph ),
          TypeOfPackaging=indeterminate(TypeOfPackagingIndeterminate)
        )
      )
  )
  ,(
    ( UnNumber='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'unNumber',_UnNumber,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'unNumber',UnNumberNode,Graph )),
      (
        (
          rdf(UnNumberNode,rdf:value,UnNumberValue,Graph),
         \+ ( rdf( UnNumberNode, aixm:uom, _UnNumberUOM, Graph ); rdf( UnNumberNode, fixm:uom, _UnNumberUOM, Graph ); rdf( UnNumberNode, plain:uom, _UnNumberUOM, Graph ) ),
          UnNumber=val(UnNumberValue)
        );
        (
          rdf( UnNumberNode,rdf:value,UnNumberValue,Graph ),
          ( rdf( UnNumberNode, aixm:uom, UnNumberUOM, Graph ); rdf( UnNumberNode, fixm:uom, UnNumberUOM, Graph ); rdf( UnNumberNode, plain:uom, UnNumberUOM, Graph ) ),
          UnNumber=xval(UnNumberValue,UnNumberUOM)
        );
        (
          rdf( UnNumberNode,aixm:nilReason, UnNumberNilReason, Graph ),
          UnNumber=nil(UnNumberNilReason)
        );
        (
          rdf( UnNumberNode,gml:indeterminatePosition, UnNumberIndeterminate, Graph ),
          UnNumber=indeterminate(UnNumberIndeterminate)
        )
      )
  )
  ,(
    ( DangerousGoodsLimitation='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'dangerousGoodsLimitation',_DangerousGoodsLimitation,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'dangerousGoodsLimitation',DangerousGoodsLimitationNode,Graph )),
      (
        (
          rdf(DangerousGoodsLimitationNode,rdf:value,DangerousGoodsLimitationValue,Graph),
         \+ ( rdf( DangerousGoodsLimitationNode, aixm:uom, _DangerousGoodsLimitationUOM, Graph ); rdf( DangerousGoodsLimitationNode, fixm:uom, _DangerousGoodsLimitationUOM, Graph ); rdf( DangerousGoodsLimitationNode, plain:uom, _DangerousGoodsLimitationUOM, Graph ) ),
          DangerousGoodsLimitation=val(DangerousGoodsLimitationValue)
        );
        (
          rdf( DangerousGoodsLimitationNode,rdf:value,DangerousGoodsLimitationValue,Graph ),
          ( rdf( DangerousGoodsLimitationNode, aixm:uom, DangerousGoodsLimitationUOM, Graph ); rdf( DangerousGoodsLimitationNode, fixm:uom, DangerousGoodsLimitationUOM, Graph ); rdf( DangerousGoodsLimitationNode, plain:uom, DangerousGoodsLimitationUOM, Graph ) ),
          DangerousGoodsLimitation=xval(DangerousGoodsLimitationValue,DangerousGoodsLimitationUOM)
        );
        (
          rdf( DangerousGoodsLimitationNode,aixm:nilReason, DangerousGoodsLimitationNilReason, Graph ),
          DangerousGoodsLimitation=nil(DangerousGoodsLimitationNilReason)
        );
        (
          rdf( DangerousGoodsLimitationNode,gml:indeterminatePosition, DangerousGoodsLimitationIndeterminate, Graph ),
          DangerousGoodsLimitation=indeterminate(DangerousGoodsLimitationIndeterminate)
        )
      )
  )
  ,(
    ( ShipmentType='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'shipmentType',_ShipmentType,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'shipmentType',ShipmentTypeNode,Graph )),
      (
        (
          rdf(ShipmentTypeNode,rdf:value,ShipmentTypeValue,Graph),
         \+ ( rdf( ShipmentTypeNode, aixm:uom, _ShipmentTypeUOM, Graph ); rdf( ShipmentTypeNode, fixm:uom, _ShipmentTypeUOM, Graph ); rdf( ShipmentTypeNode, plain:uom, _ShipmentTypeUOM, Graph ) ),
          ShipmentType=val(ShipmentTypeValue)
        );
        (
          rdf( ShipmentTypeNode,rdf:value,ShipmentTypeValue,Graph ),
          ( rdf( ShipmentTypeNode, aixm:uom, ShipmentTypeUOM, Graph ); rdf( ShipmentTypeNode, fixm:uom, ShipmentTypeUOM, Graph ); rdf( ShipmentTypeNode, plain:uom, ShipmentTypeUOM, Graph ) ),
          ShipmentType=xval(ShipmentTypeValue,ShipmentTypeUOM)
        );
        (
          rdf( ShipmentTypeNode,aixm:nilReason, ShipmentTypeNilReason, Graph ),
          ShipmentType=nil(ShipmentTypeNilReason)
        );
        (
          rdf( ShipmentTypeNode,gml:indeterminatePosition, ShipmentTypeIndeterminate, Graph ),
          ShipmentType=indeterminate(ShipmentTypeIndeterminate)
        )
      )
  )
  ,( ( AllPackedInOne='$null$',
    \+ rdf( DangerousGoodsPackage,fixm:'allPackedInOne', _AllPackedInOne, Graph  )
   ; rdf(DangerousGoodsPackage,fixm:'allPackedInOne', AllPackedInOne, Graph ) )
  )
  ,(
    ( CompatibilityGroup='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'compatibilityGroup',_CompatibilityGroup,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'compatibilityGroup',CompatibilityGroupNode,Graph )),
      (
        (
          rdf(CompatibilityGroupNode,rdf:value,CompatibilityGroupValue,Graph),
         \+ ( rdf( CompatibilityGroupNode, aixm:uom, _CompatibilityGroupUOM, Graph ); rdf( CompatibilityGroupNode, fixm:uom, _CompatibilityGroupUOM, Graph ); rdf( CompatibilityGroupNode, plain:uom, _CompatibilityGroupUOM, Graph ) ),
          CompatibilityGroup=val(CompatibilityGroupValue)
        );
        (
          rdf( CompatibilityGroupNode,rdf:value,CompatibilityGroupValue,Graph ),
          ( rdf( CompatibilityGroupNode, aixm:uom, CompatibilityGroupUOM, Graph ); rdf( CompatibilityGroupNode, fixm:uom, CompatibilityGroupUOM, Graph ); rdf( CompatibilityGroupNode, plain:uom, CompatibilityGroupUOM, Graph ) ),
          CompatibilityGroup=xval(CompatibilityGroupValue,CompatibilityGroupUOM)
        );
        (
          rdf( CompatibilityGroupNode,aixm:nilReason, CompatibilityGroupNilReason, Graph ),
          CompatibilityGroup=nil(CompatibilityGroupNilReason)
        );
        (
          rdf( CompatibilityGroupNode,gml:indeterminatePosition, CompatibilityGroupIndeterminate, Graph ),
          CompatibilityGroup=indeterminate(CompatibilityGroupIndeterminate)
        )
      )
  )
  ,( ( ShipmentDimensions='$null$',
    \+ rdf( DangerousGoodsPackage,fixm:'shipmentDimensions', _ShipmentDimensions, Graph  )
   ; rdf(DangerousGoodsPackage,fixm:'shipmentDimensions', ShipmentDimensions, Graph ) )
  )
  ,(
    ( MarinePollutantIndicator='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'marinePollutantIndicator',_MarinePollutantIndicator,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'marinePollutantIndicator',MarinePollutantIndicatorNode,Graph )),
      (
        (
          rdf(MarinePollutantIndicatorNode,rdf:value,MarinePollutantIndicatorValue,Graph),
         \+ ( rdf( MarinePollutantIndicatorNode, aixm:uom, _MarinePollutantIndicatorUOM, Graph ); rdf( MarinePollutantIndicatorNode, fixm:uom, _MarinePollutantIndicatorUOM, Graph ); rdf( MarinePollutantIndicatorNode, plain:uom, _MarinePollutantIndicatorUOM, Graph ) ),
          MarinePollutantIndicator=val(MarinePollutantIndicatorValue)
        );
        (
          rdf( MarinePollutantIndicatorNode,rdf:value,MarinePollutantIndicatorValue,Graph ),
          ( rdf( MarinePollutantIndicatorNode, aixm:uom, MarinePollutantIndicatorUOM, Graph ); rdf( MarinePollutantIndicatorNode, fixm:uom, MarinePollutantIndicatorUOM, Graph ); rdf( MarinePollutantIndicatorNode, plain:uom, MarinePollutantIndicatorUOM, Graph ) ),
          MarinePollutantIndicator=xval(MarinePollutantIndicatorValue,MarinePollutantIndicatorUOM)
        );
        (
          rdf( MarinePollutantIndicatorNode,aixm:nilReason, MarinePollutantIndicatorNilReason, Graph ),
          MarinePollutantIndicator=nil(MarinePollutantIndicatorNilReason)
        );
        (
          rdf( MarinePollutantIndicatorNode,gml:indeterminatePosition, MarinePollutantIndicatorIndeterminate, Graph ),
          MarinePollutantIndicator=indeterminate(MarinePollutantIndicatorIndeterminate)
        )
      )
  )
  ,( ( RadioactiveMaterials='$null$',
    \+ rdf( DangerousGoodsPackage,fixm:'radioactiveMaterials', _RadioactiveMaterials, Graph  )
   ; rdf(DangerousGoodsPackage,fixm:'radioactiveMaterials', RadioactiveMaterials, Graph ) )
  )
  ,(
    ( HazardClass='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'hazardClass',_HazardClass,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'hazardClass',HazardClassNode,Graph )),
      (
        (
          rdf(HazardClassNode,rdf:value,HazardClassValue,Graph),
         \+ ( rdf( HazardClassNode, aixm:uom, _HazardClassUOM, Graph ); rdf( HazardClassNode, fixm:uom, _HazardClassUOM, Graph ); rdf( HazardClassNode, plain:uom, _HazardClassUOM, Graph ) ),
          HazardClass=val(HazardClassValue)
        );
        (
          rdf( HazardClassNode,rdf:value,HazardClassValue,Graph ),
          ( rdf( HazardClassNode, aixm:uom, HazardClassUOM, Graph ); rdf( HazardClassNode, fixm:uom, HazardClassUOM, Graph ); rdf( HazardClassNode, plain:uom, HazardClassUOM, Graph ) ),
          HazardClass=xval(HazardClassValue,HazardClassUOM)
        );
        (
          rdf( HazardClassNode,aixm:nilReason, HazardClassNilReason, Graph ),
          HazardClass=nil(HazardClassNilReason)
        );
        (
          rdf( HazardClassNode,gml:indeterminatePosition, HazardClassIndeterminate, Graph ),
          HazardClass=indeterminate(HazardClassIndeterminate)
        )
      )
  )
  ,(
    ( PackingGroup='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'packingGroup',_PackingGroup,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'packingGroup',PackingGroupNode,Graph )),
      (
        (
          rdf(PackingGroupNode,rdf:value,PackingGroupValue,Graph),
         \+ ( rdf( PackingGroupNode, aixm:uom, _PackingGroupUOM, Graph ); rdf( PackingGroupNode, fixm:uom, _PackingGroupUOM, Graph ); rdf( PackingGroupNode, plain:uom, _PackingGroupUOM, Graph ) ),
          PackingGroup=val(PackingGroupValue)
        );
        (
          rdf( PackingGroupNode,rdf:value,PackingGroupValue,Graph ),
          ( rdf( PackingGroupNode, aixm:uom, PackingGroupUOM, Graph ); rdf( PackingGroupNode, fixm:uom, PackingGroupUOM, Graph ); rdf( PackingGroupNode, plain:uom, PackingGroupUOM, Graph ) ),
          PackingGroup=xval(PackingGroupValue,PackingGroupUOM)
        );
        (
          rdf( PackingGroupNode,aixm:nilReason, PackingGroupNilReason, Graph ),
          PackingGroup=nil(PackingGroupNilReason)
        );
        (
          rdf( PackingGroupNode,gml:indeterminatePosition, PackingGroupIndeterminate, Graph ),
          PackingGroup=indeterminate(PackingGroupIndeterminate)
        )
      )
  )
  ,( ( Temperatures='$null$',
    \+ rdf( DangerousGoodsPackage,fixm:'temperatures', _Temperatures, Graph  )
   ; rdf(DangerousGoodsPackage,fixm:'temperatures', Temperatures, Graph ) )
  )
  ,(
    ( OverpackIndicator='$null$',
      \+ rdf( DangerousGoodsPackage,fixm:'overpackIndicator',_OverpackIndicator,Graph )
    );
  ( rdf( DangerousGoodsPackage,fixm:'overpackIndicator',OverpackIndicatorNode,Graph )),
      (
        (
          rdf(OverpackIndicatorNode,rdf:value,OverpackIndicatorValue,Graph),
         \+ ( rdf( OverpackIndicatorNode, aixm:uom, _OverpackIndicatorUOM, Graph ); rdf( OverpackIndicatorNode, fixm:uom, _OverpackIndicatorUOM, Graph ); rdf( OverpackIndicatorNode, plain:uom, _OverpackIndicatorUOM, Graph ) ),
          OverpackIndicator=val(OverpackIndicatorValue)
        );
        (
          rdf( OverpackIndicatorNode,rdf:value,OverpackIndicatorValue,Graph ),
          ( rdf( OverpackIndicatorNode, aixm:uom, OverpackIndicatorUOM, Graph ); rdf( OverpackIndicatorNode, fixm:uom, OverpackIndicatorUOM, Graph ); rdf( OverpackIndicatorNode, plain:uom, OverpackIndicatorUOM, Graph ) ),
          OverpackIndicator=xval(OverpackIndicatorValue,OverpackIndicatorUOM)
        );
        (
          rdf( OverpackIndicatorNode,aixm:nilReason, OverpackIndicatorNilReason, Graph ),
          OverpackIndicator=nil(OverpackIndicatorNilReason)
        );
        (
          rdf( OverpackIndicatorNode,gml:indeterminatePosition, OverpackIndicatorIndeterminate, Graph ),
          OverpackIndicator=indeterminate(OverpackIndicatorIndeterminate)
        )
      )
  )
  ,findall(A, rdf(DangerousGoodsPackage,fixm:'subsidiaryHazardClass',A,Graph), SubsidiaryHazardClass) .

fixm_LastPositionReport(Graph, LastPositionReport, DeterminationMethod, Position, TimeAtPosition) :-
  rdf(LastPositionReport,rdf:type,fixm:'LastPositionReport',Graph)
  ,(
    ( DeterminationMethod='$null$',
      \+ rdf( LastPositionReport,fixm:'determinationMethod',_DeterminationMethod,Graph )
    );
  ( rdf( LastPositionReport,fixm:'determinationMethod',DeterminationMethodNode,Graph )),
      (
        (
          rdf(DeterminationMethodNode,rdf:value,DeterminationMethodValue,Graph),
         \+ ( rdf( DeterminationMethodNode, aixm:uom, _DeterminationMethodUOM, Graph ); rdf( DeterminationMethodNode, fixm:uom, _DeterminationMethodUOM, Graph ); rdf( DeterminationMethodNode, plain:uom, _DeterminationMethodUOM, Graph ) ),
          DeterminationMethod=val(DeterminationMethodValue)
        );
        (
          rdf( DeterminationMethodNode,rdf:value,DeterminationMethodValue,Graph ),
          ( rdf( DeterminationMethodNode, aixm:uom, DeterminationMethodUOM, Graph ); rdf( DeterminationMethodNode, fixm:uom, DeterminationMethodUOM, Graph ); rdf( DeterminationMethodNode, plain:uom, DeterminationMethodUOM, Graph ) ),
          DeterminationMethod=xval(DeterminationMethodValue,DeterminationMethodUOM)
        );
        (
          rdf( DeterminationMethodNode,aixm:nilReason, DeterminationMethodNilReason, Graph ),
          DeterminationMethod=nil(DeterminationMethodNilReason)
        );
        (
          rdf( DeterminationMethodNode,gml:indeterminatePosition, DeterminationMethodIndeterminate, Graph ),
          DeterminationMethod=indeterminate(DeterminationMethodIndeterminate)
        )
      )
  )
  ,( ( Position='$null$',
    \+ rdf( LastPositionReport,fixm:'position', _Position, Graph  )
   ; rdf(LastPositionReport,fixm:'position', Position, Graph ) )
  )
  ,(
    ( TimeAtPosition='$null$',
      \+ rdf( LastPositionReport,fixm:'timeAtPosition',_TimeAtPosition,Graph )
    );
  ( rdf( LastPositionReport,fixm:'timeAtPosition',TimeAtPositionNode,Graph )),
      (
        (
          rdf(TimeAtPositionNode,rdf:value,TimeAtPositionValue,Graph),
         \+ ( rdf( TimeAtPositionNode, aixm:uom, _TimeAtPositionUOM, Graph ); rdf( TimeAtPositionNode, fixm:uom, _TimeAtPositionUOM, Graph ); rdf( TimeAtPositionNode, plain:uom, _TimeAtPositionUOM, Graph ) ),
          TimeAtPosition=val(TimeAtPositionValue)
        );
        (
          rdf( TimeAtPositionNode,rdf:value,TimeAtPositionValue,Graph ),
          ( rdf( TimeAtPositionNode, aixm:uom, TimeAtPositionUOM, Graph ); rdf( TimeAtPositionNode, fixm:uom, TimeAtPositionUOM, Graph ); rdf( TimeAtPositionNode, plain:uom, TimeAtPositionUOM, Graph ) ),
          TimeAtPosition=xval(TimeAtPositionValue,TimeAtPositionUOM)
        );
        (
          rdf( TimeAtPositionNode,aixm:nilReason, TimeAtPositionNilReason, Graph ),
          TimeAtPosition=nil(TimeAtPositionNilReason)
        );
        (
          rdf( TimeAtPositionNode,gml:indeterminatePosition, TimeAtPositionIndeterminate, Graph ),
          TimeAtPosition=indeterminate(TimeAtPositionIndeterminate)
        )
      )
  ) .

aixm_AltimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus) :-
  rdf(AltimeterSourceStatus,rdf:type,aixm:'AltimeterSourceStatus',Graph)
  ,(
    ( OperationalStatus='$null$',
      \+ rdf( AltimeterSourceStatus,aixm:'operationalStatus',_OperationalStatus,Graph )
    );
  ( rdf( AltimeterSourceStatus,aixm:'operationalStatus',OperationalStatusNode,Graph )),
      (
        (
          rdf(OperationalStatusNode,rdf:value,OperationalStatusValue,Graph),
         \+ ( rdf( OperationalStatusNode, aixm:uom, _OperationalStatusUOM, Graph ); rdf( OperationalStatusNode, fixm:uom, _OperationalStatusUOM, Graph ); rdf( OperationalStatusNode, plain:uom, _OperationalStatusUOM, Graph ) ),
          OperationalStatus=val(OperationalStatusValue)
        );
        (
          rdf( OperationalStatusNode,rdf:value,OperationalStatusValue,Graph ),
          ( rdf( OperationalStatusNode, aixm:uom, OperationalStatusUOM, Graph ); rdf( OperationalStatusNode, fixm:uom, OperationalStatusUOM, Graph ); rdf( OperationalStatusNode, plain:uom, OperationalStatusUOM, Graph ) ),
          OperationalStatus=xval(OperationalStatusValue,OperationalStatusUOM)
        );
        (
          rdf( OperationalStatusNode,aixm:nilReason, OperationalStatusNilReason, Graph ),
          OperationalStatus=nil(OperationalStatusNilReason)
        );
        (
          rdf( OperationalStatusNode,gml:indeterminatePosition, OperationalStatusIndeterminate, Graph ),
          OperationalStatus=indeterminate(OperationalStatusIndeterminate)
        )
      )
  ) .

fixm_DangerousGoodsDimensions(Graph, DangerousGoodsDimensions, GrossWeight, NetWeight, Volume) :-
  rdf(DangerousGoodsDimensions,rdf:type,fixm:'DangerousGoodsDimensions',Graph)
  ,(
    ( GrossWeight='$null$',
      \+ rdf( DangerousGoodsDimensions,fixm:'grossWeight',_GrossWeight,Graph )
    );
  ( rdf( DangerousGoodsDimensions,fixm:'grossWeight',GrossWeightNode,Graph )),
      (
        (
          rdf(GrossWeightNode,rdf:value,GrossWeightValue,Graph),
         \+ ( rdf( GrossWeightNode, aixm:uom, _GrossWeightUOM, Graph ); rdf( GrossWeightNode, fixm:uom, _GrossWeightUOM, Graph ); rdf( GrossWeightNode, plain:uom, _GrossWeightUOM, Graph ) ),
          GrossWeight=val(GrossWeightValue)
        );
        (
          rdf( GrossWeightNode,rdf:value,GrossWeightValue,Graph ),
          ( rdf( GrossWeightNode, aixm:uom, GrossWeightUOM, Graph ); rdf( GrossWeightNode, fixm:uom, GrossWeightUOM, Graph ); rdf( GrossWeightNode, plain:uom, GrossWeightUOM, Graph ) ),
          GrossWeight=xval(GrossWeightValue,GrossWeightUOM)
        );
        (
          rdf( GrossWeightNode,aixm:nilReason, GrossWeightNilReason, Graph ),
          GrossWeight=nil(GrossWeightNilReason)
        );
        (
          rdf( GrossWeightNode,gml:indeterminatePosition, GrossWeightIndeterminate, Graph ),
          GrossWeight=indeterminate(GrossWeightIndeterminate)
        )
      )
  )
  ,(
    ( NetWeight='$null$',
      \+ rdf( DangerousGoodsDimensions,fixm:'netWeight',_NetWeight,Graph )
    );
  ( rdf( DangerousGoodsDimensions,fixm:'netWeight',NetWeightNode,Graph )),
      (
        (
          rdf(NetWeightNode,rdf:value,NetWeightValue,Graph),
         \+ ( rdf( NetWeightNode, aixm:uom, _NetWeightUOM, Graph ); rdf( NetWeightNode, fixm:uom, _NetWeightUOM, Graph ); rdf( NetWeightNode, plain:uom, _NetWeightUOM, Graph ) ),
          NetWeight=val(NetWeightValue)
        );
        (
          rdf( NetWeightNode,rdf:value,NetWeightValue,Graph ),
          ( rdf( NetWeightNode, aixm:uom, NetWeightUOM, Graph ); rdf( NetWeightNode, fixm:uom, NetWeightUOM, Graph ); rdf( NetWeightNode, plain:uom, NetWeightUOM, Graph ) ),
          NetWeight=xval(NetWeightValue,NetWeightUOM)
        );
        (
          rdf( NetWeightNode,aixm:nilReason, NetWeightNilReason, Graph ),
          NetWeight=nil(NetWeightNilReason)
        );
        (
          rdf( NetWeightNode,gml:indeterminatePosition, NetWeightIndeterminate, Graph ),
          NetWeight=indeterminate(NetWeightIndeterminate)
        )
      )
  )
  ,(
    ( Volume='$null$',
      \+ rdf( DangerousGoodsDimensions,fixm:'volume',_Volume,Graph )
    );
  ( rdf( DangerousGoodsDimensions,fixm:'volume',VolumeNode,Graph )),
      (
        (
          rdf(VolumeNode,rdf:value,VolumeValue,Graph),
         \+ ( rdf( VolumeNode, aixm:uom, _VolumeUOM, Graph ); rdf( VolumeNode, fixm:uom, _VolumeUOM, Graph ); rdf( VolumeNode, plain:uom, _VolumeUOM, Graph ) ),
          Volume=val(VolumeValue)
        );
        (
          rdf( VolumeNode,rdf:value,VolumeValue,Graph ),
          ( rdf( VolumeNode, aixm:uom, VolumeUOM, Graph ); rdf( VolumeNode, fixm:uom, VolumeUOM, Graph ); rdf( VolumeNode, plain:uom, VolumeUOM, Graph ) ),
          Volume=xval(VolumeValue,VolumeUOM)
        );
        (
          rdf( VolumeNode,aixm:nilReason, VolumeNilReason, Graph ),
          Volume=nil(VolumeNilReason)
        );
        (
          rdf( VolumeNode,gml:indeterminatePosition, VolumeIndeterminate, Graph ),
          Volume=indeterminate(VolumeIndeterminate)
        )
      )
  ) .

fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules) :-
  rdf(EfplRoute,rdf:type,fixm:'EfplRoute',Graph)
  ,(
    ( EfplFlightRules='$null$',
      \+ rdf( EfplRoute,fixm:'efplFlightRules',_EfplFlightRules,Graph )
    );
  ( rdf( EfplRoute,fixm:'efplFlightRules',EfplFlightRulesNode,Graph )),
      (
        (
          rdf(EfplFlightRulesNode,rdf:value,EfplFlightRulesValue,Graph),
         \+ ( rdf( EfplFlightRulesNode, aixm:uom, _EfplFlightRulesUOM, Graph ); rdf( EfplFlightRulesNode, fixm:uom, _EfplFlightRulesUOM, Graph ); rdf( EfplFlightRulesNode, plain:uom, _EfplFlightRulesUOM, Graph ) ),
          EfplFlightRules=val(EfplFlightRulesValue)
        );
        (
          rdf( EfplFlightRulesNode,rdf:value,EfplFlightRulesValue,Graph ),
          ( rdf( EfplFlightRulesNode, aixm:uom, EfplFlightRulesUOM, Graph ); rdf( EfplFlightRulesNode, fixm:uom, EfplFlightRulesUOM, Graph ); rdf( EfplFlightRulesNode, plain:uom, EfplFlightRulesUOM, Graph ) ),
          EfplFlightRules=xval(EfplFlightRulesValue,EfplFlightRulesUOM)
        );
        (
          rdf( EfplFlightRulesNode,aixm:nilReason, EfplFlightRulesNilReason, Graph ),
          EfplFlightRules=nil(EfplFlightRulesNilReason)
        );
        (
          rdf( EfplFlightRulesNode,gml:indeterminatePosition, EfplFlightRulesIndeterminate, Graph ),
          EfplFlightRules=indeterminate(EfplFlightRulesIndeterminate)
        )
      )
  ) .

fixm_CoordinationStatus(Graph, CoordinationStatus, AbrogationReason, CoordinationStatus, NonStandardCommunicationReason, ReleaseConditions) :-
  rdf(CoordinationStatus,rdf:type,fixm:'CoordinationStatus',Graph)
  ,(
    ( AbrogationReason='$null$',
      \+ rdf( CoordinationStatus,fixm:'abrogationReason',_AbrogationReason,Graph )
    );
  ( rdf( CoordinationStatus,fixm:'abrogationReason',AbrogationReasonNode,Graph )),
      (
        (
          rdf(AbrogationReasonNode,rdf:value,AbrogationReasonValue,Graph),
         \+ ( rdf( AbrogationReasonNode, aixm:uom, _AbrogationReasonUOM, Graph ); rdf( AbrogationReasonNode, fixm:uom, _AbrogationReasonUOM, Graph ); rdf( AbrogationReasonNode, plain:uom, _AbrogationReasonUOM, Graph ) ),
          AbrogationReason=val(AbrogationReasonValue)
        );
        (
          rdf( AbrogationReasonNode,rdf:value,AbrogationReasonValue,Graph ),
          ( rdf( AbrogationReasonNode, aixm:uom, AbrogationReasonUOM, Graph ); rdf( AbrogationReasonNode, fixm:uom, AbrogationReasonUOM, Graph ); rdf( AbrogationReasonNode, plain:uom, AbrogationReasonUOM, Graph ) ),
          AbrogationReason=xval(AbrogationReasonValue,AbrogationReasonUOM)
        );
        (
          rdf( AbrogationReasonNode,aixm:nilReason, AbrogationReasonNilReason, Graph ),
          AbrogationReason=nil(AbrogationReasonNilReason)
        );
        (
          rdf( AbrogationReasonNode,gml:indeterminatePosition, AbrogationReasonIndeterminate, Graph ),
          AbrogationReason=indeterminate(AbrogationReasonIndeterminate)
        )
      )
  )
  ,(
    ( CoordinationStatus='$null$',
      \+ rdf( CoordinationStatus,fixm:'coordinationStatus',_CoordinationStatus,Graph )
    );
  ( rdf( CoordinationStatus,fixm:'coordinationStatus',CoordinationStatusNode,Graph )),
      (
        (
          rdf(CoordinationStatusNode,rdf:value,CoordinationStatusValue,Graph),
         \+ ( rdf( CoordinationStatusNode, aixm:uom, _CoordinationStatusUOM, Graph ); rdf( CoordinationStatusNode, fixm:uom, _CoordinationStatusUOM, Graph ); rdf( CoordinationStatusNode, plain:uom, _CoordinationStatusUOM, Graph ) ),
          CoordinationStatus=val(CoordinationStatusValue)
        );
        (
          rdf( CoordinationStatusNode,rdf:value,CoordinationStatusValue,Graph ),
          ( rdf( CoordinationStatusNode, aixm:uom, CoordinationStatusUOM, Graph ); rdf( CoordinationStatusNode, fixm:uom, CoordinationStatusUOM, Graph ); rdf( CoordinationStatusNode, plain:uom, CoordinationStatusUOM, Graph ) ),
          CoordinationStatus=xval(CoordinationStatusValue,CoordinationStatusUOM)
        );
        (
          rdf( CoordinationStatusNode,aixm:nilReason, CoordinationStatusNilReason, Graph ),
          CoordinationStatus=nil(CoordinationStatusNilReason)
        );
        (
          rdf( CoordinationStatusNode,gml:indeterminatePosition, CoordinationStatusIndeterminate, Graph ),
          CoordinationStatus=indeterminate(CoordinationStatusIndeterminate)
        )
      )
  )
  ,(
    ( NonStandardCommunicationReason='$null$',
      \+ rdf( CoordinationStatus,fixm:'nonStandardCommunicationReason',_NonStandardCommunicationReason,Graph )
    );
  ( rdf( CoordinationStatus,fixm:'nonStandardCommunicationReason',NonStandardCommunicationReasonNode,Graph )),
      (
        (
          rdf(NonStandardCommunicationReasonNode,rdf:value,NonStandardCommunicationReasonValue,Graph),
         \+ ( rdf( NonStandardCommunicationReasonNode, aixm:uom, _NonStandardCommunicationReasonUOM, Graph ); rdf( NonStandardCommunicationReasonNode, fixm:uom, _NonStandardCommunicationReasonUOM, Graph ); rdf( NonStandardCommunicationReasonNode, plain:uom, _NonStandardCommunicationReasonUOM, Graph ) ),
          NonStandardCommunicationReason=val(NonStandardCommunicationReasonValue)
        );
        (
          rdf( NonStandardCommunicationReasonNode,rdf:value,NonStandardCommunicationReasonValue,Graph ),
          ( rdf( NonStandardCommunicationReasonNode, aixm:uom, NonStandardCommunicationReasonUOM, Graph ); rdf( NonStandardCommunicationReasonNode, fixm:uom, NonStandardCommunicationReasonUOM, Graph ); rdf( NonStandardCommunicationReasonNode, plain:uom, NonStandardCommunicationReasonUOM, Graph ) ),
          NonStandardCommunicationReason=xval(NonStandardCommunicationReasonValue,NonStandardCommunicationReasonUOM)
        );
        (
          rdf( NonStandardCommunicationReasonNode,aixm:nilReason, NonStandardCommunicationReasonNilReason, Graph ),
          NonStandardCommunicationReason=nil(NonStandardCommunicationReasonNilReason)
        );
        (
          rdf( NonStandardCommunicationReasonNode,gml:indeterminatePosition, NonStandardCommunicationReasonIndeterminate, Graph ),
          NonStandardCommunicationReason=indeterminate(NonStandardCommunicationReasonIndeterminate)
        )
      )
  )
  ,(
    ( ReleaseConditions='$null$',
      \+ rdf( CoordinationStatus,fixm:'releaseConditions',_ReleaseConditions,Graph )
    );
  ( rdf( CoordinationStatus,fixm:'releaseConditions',ReleaseConditionsNode,Graph )),
      (
        (
          rdf(ReleaseConditionsNode,rdf:value,ReleaseConditionsValue,Graph),
         \+ ( rdf( ReleaseConditionsNode, aixm:uom, _ReleaseConditionsUOM, Graph ); rdf( ReleaseConditionsNode, fixm:uom, _ReleaseConditionsUOM, Graph ); rdf( ReleaseConditionsNode, plain:uom, _ReleaseConditionsUOM, Graph ) ),
          ReleaseConditions=val(ReleaseConditionsValue)
        );
        (
          rdf( ReleaseConditionsNode,rdf:value,ReleaseConditionsValue,Graph ),
          ( rdf( ReleaseConditionsNode, aixm:uom, ReleaseConditionsUOM, Graph ); rdf( ReleaseConditionsNode, fixm:uom, ReleaseConditionsUOM, Graph ); rdf( ReleaseConditionsNode, plain:uom, ReleaseConditionsUOM, Graph ) ),
          ReleaseConditions=xval(ReleaseConditionsValue,ReleaseConditionsUOM)
        );
        (
          rdf( ReleaseConditionsNode,aixm:nilReason, ReleaseConditionsNilReason, Graph ),
          ReleaseConditions=nil(ReleaseConditionsNilReason)
        );
        (
          rdf( ReleaseConditionsNode,gml:indeterminatePosition, ReleaseConditionsIndeterminate, Graph ),
          ReleaseConditions=indeterminate(ReleaseConditionsIndeterminate)
        )
      )
  ) .

fixm_BoundaryCrossing(Graph, BoundaryCrossing, Altitude, CrossingPoint, CrossingSpeed, CrossingTime, Offtrack, AltitudeInTransition) :-
  rdf(BoundaryCrossing,rdf:type,fixm:'BoundaryCrossing',Graph)
  ,(
    ( Altitude='$null$',
      \+ rdf( BoundaryCrossing,fixm:'altitude',_Altitude,Graph )
    );
  ( rdf( BoundaryCrossing,fixm:'altitude',AltitudeNode,Graph )),
      (
        (
          rdf(AltitudeNode,rdf:value,AltitudeValue,Graph),
         \+ ( rdf( AltitudeNode, aixm:uom, _AltitudeUOM, Graph ); rdf( AltitudeNode, fixm:uom, _AltitudeUOM, Graph ); rdf( AltitudeNode, plain:uom, _AltitudeUOM, Graph ) ),
          Altitude=val(AltitudeValue)
        );
        (
          rdf( AltitudeNode,rdf:value,AltitudeValue,Graph ),
          ( rdf( AltitudeNode, aixm:uom, AltitudeUOM, Graph ); rdf( AltitudeNode, fixm:uom, AltitudeUOM, Graph ); rdf( AltitudeNode, plain:uom, AltitudeUOM, Graph ) ),
          Altitude=xval(AltitudeValue,AltitudeUOM)
        );
        (
          rdf( AltitudeNode,aixm:nilReason, AltitudeNilReason, Graph ),
          Altitude=nil(AltitudeNilReason)
        );
        (
          rdf( AltitudeNode,gml:indeterminatePosition, AltitudeIndeterminate, Graph ),
          Altitude=indeterminate(AltitudeIndeterminate)
        )
      )
  )
  ,( ( CrossingPoint='$null$',
    \+ rdf( BoundaryCrossing,fixm:'crossingPoint', _CrossingPoint, Graph  )
   ; rdf(BoundaryCrossing,fixm:'crossingPoint', CrossingPoint, Graph ) )
  )
  ,(
    ( CrossingSpeed='$null$',
      \+ rdf( BoundaryCrossing,fixm:'crossingSpeed',_CrossingSpeed,Graph )
    );
  ( rdf( BoundaryCrossing,fixm:'crossingSpeed',CrossingSpeedNode,Graph )),
      (
        (
          rdf(CrossingSpeedNode,rdf:value,CrossingSpeedValue,Graph),
         \+ ( rdf( CrossingSpeedNode, aixm:uom, _CrossingSpeedUOM, Graph ); rdf( CrossingSpeedNode, fixm:uom, _CrossingSpeedUOM, Graph ); rdf( CrossingSpeedNode, plain:uom, _CrossingSpeedUOM, Graph ) ),
          CrossingSpeed=val(CrossingSpeedValue)
        );
        (
          rdf( CrossingSpeedNode,rdf:value,CrossingSpeedValue,Graph ),
          ( rdf( CrossingSpeedNode, aixm:uom, CrossingSpeedUOM, Graph ); rdf( CrossingSpeedNode, fixm:uom, CrossingSpeedUOM, Graph ); rdf( CrossingSpeedNode, plain:uom, CrossingSpeedUOM, Graph ) ),
          CrossingSpeed=xval(CrossingSpeedValue,CrossingSpeedUOM)
        );
        (
          rdf( CrossingSpeedNode,aixm:nilReason, CrossingSpeedNilReason, Graph ),
          CrossingSpeed=nil(CrossingSpeedNilReason)
        );
        (
          rdf( CrossingSpeedNode,gml:indeterminatePosition, CrossingSpeedIndeterminate, Graph ),
          CrossingSpeed=indeterminate(CrossingSpeedIndeterminate)
        )
      )
  )
  ,(
    ( CrossingTime='$null$',
      \+ rdf( BoundaryCrossing,fixm:'crossingTime',_CrossingTime,Graph )
    );
  ( rdf( BoundaryCrossing,fixm:'crossingTime',CrossingTimeNode,Graph )),
      (
        (
          rdf(CrossingTimeNode,rdf:value,CrossingTimeValue,Graph),
         \+ ( rdf( CrossingTimeNode, aixm:uom, _CrossingTimeUOM, Graph ); rdf( CrossingTimeNode, fixm:uom, _CrossingTimeUOM, Graph ); rdf( CrossingTimeNode, plain:uom, _CrossingTimeUOM, Graph ) ),
          CrossingTime=val(CrossingTimeValue)
        );
        (
          rdf( CrossingTimeNode,rdf:value,CrossingTimeValue,Graph ),
          ( rdf( CrossingTimeNode, aixm:uom, CrossingTimeUOM, Graph ); rdf( CrossingTimeNode, fixm:uom, CrossingTimeUOM, Graph ); rdf( CrossingTimeNode, plain:uom, CrossingTimeUOM, Graph ) ),
          CrossingTime=xval(CrossingTimeValue,CrossingTimeUOM)
        );
        (
          rdf( CrossingTimeNode,aixm:nilReason, CrossingTimeNilReason, Graph ),
          CrossingTime=nil(CrossingTimeNilReason)
        );
        (
          rdf( CrossingTimeNode,gml:indeterminatePosition, CrossingTimeIndeterminate, Graph ),
          CrossingTime=indeterminate(CrossingTimeIndeterminate)
        )
      )
  )
  ,( ( Offtrack='$null$',
    \+ rdf( BoundaryCrossing,fixm:'offtrack', _Offtrack, Graph  )
   ; rdf(BoundaryCrossing,fixm:'offtrack', Offtrack, Graph ) )
  )
  ,(
    ( AltitudeInTransition='$null$',
      \+ rdf( BoundaryCrossing,fixm:'altitudeInTransition',_AltitudeInTransition,Graph )
    );
  ( rdf( BoundaryCrossing,fixm:'altitudeInTransition',AltitudeInTransitionNode,Graph )),
      (
        (
          rdf(AltitudeInTransitionNode,rdf:value,AltitudeInTransitionValue,Graph),
         \+ ( rdf( AltitudeInTransitionNode, aixm:uom, _AltitudeInTransitionUOM, Graph ); rdf( AltitudeInTransitionNode, fixm:uom, _AltitudeInTransitionUOM, Graph ); rdf( AltitudeInTransitionNode, plain:uom, _AltitudeInTransitionUOM, Graph ) ),
          AltitudeInTransition=val(AltitudeInTransitionValue)
        );
        (
          rdf( AltitudeInTransitionNode,rdf:value,AltitudeInTransitionValue,Graph ),
          ( rdf( AltitudeInTransitionNode, aixm:uom, AltitudeInTransitionUOM, Graph ); rdf( AltitudeInTransitionNode, fixm:uom, AltitudeInTransitionUOM, Graph ); rdf( AltitudeInTransitionNode, plain:uom, AltitudeInTransitionUOM, Graph ) ),
          AltitudeInTransition=xval(AltitudeInTransitionValue,AltitudeInTransitionUOM)
        );
        (
          rdf( AltitudeInTransitionNode,aixm:nilReason, AltitudeInTransitionNilReason, Graph ),
          AltitudeInTransition=nil(AltitudeInTransitionNilReason)
        );
        (
          rdf( AltitudeInTransitionNode,gml:indeterminatePosition, AltitudeInTransitionIndeterminate, Graph ),
          AltitudeInTransition=indeterminate(AltitudeInTransitionIndeterminate)
        )
      )
  ) .

fixm_IcaoAerodromeReference(Graph, IcaoAerodromeReference, Code) :-
  rdf(IcaoAerodromeReference,rdf:type,fixm:'IcaoAerodromeReference',Graph)
  ,(
    ( Code='$null$',
      \+ rdf( IcaoAerodromeReference,fixm:'code',_Code,Graph )
    );
  ( rdf( IcaoAerodromeReference,fixm:'code',CodeNode,Graph )),
      (
        (
          rdf(CodeNode,rdf:value,CodeValue,Graph),
         \+ ( rdf( CodeNode, aixm:uom, _CodeUOM, Graph ); rdf( CodeNode, fixm:uom, _CodeUOM, Graph ); rdf( CodeNode, plain:uom, _CodeUOM, Graph ) ),
          Code=val(CodeValue)
        );
        (
          rdf( CodeNode,rdf:value,CodeValue,Graph ),
          ( rdf( CodeNode, aixm:uom, CodeUOM, Graph ); rdf( CodeNode, fixm:uom, CodeUOM, Graph ); rdf( CodeNode, plain:uom, CodeUOM, Graph ) ),
          Code=xval(CodeValue,CodeUOM)
        );
        (
          rdf( CodeNode,aixm:nilReason, CodeNilReason, Graph ),
          Code=nil(CodeNilReason)
        );
        (
          rdf( CodeNode,gml:indeterminatePosition, CodeIndeterminate, Graph ),
          Code=indeterminate(CodeIndeterminate)
        )
      )
  ) .

fixm_RadioCommunicationFailure(Graph, RadioCommunicationFailure, RadioFailureRemarks, RemainingComCapability, Contact) :-
  rdf(RadioCommunicationFailure,rdf:type,fixm:'RadioCommunicationFailure',Graph)
  ,(
    ( RadioFailureRemarks='$null$',
      \+ rdf( RadioCommunicationFailure,fixm:'radioFailureRemarks',_RadioFailureRemarks,Graph )
    );
  ( rdf( RadioCommunicationFailure,fixm:'radioFailureRemarks',RadioFailureRemarksNode,Graph )),
      (
        (
          rdf(RadioFailureRemarksNode,rdf:value,RadioFailureRemarksValue,Graph),
         \+ ( rdf( RadioFailureRemarksNode, aixm:uom, _RadioFailureRemarksUOM, Graph ); rdf( RadioFailureRemarksNode, fixm:uom, _RadioFailureRemarksUOM, Graph ); rdf( RadioFailureRemarksNode, plain:uom, _RadioFailureRemarksUOM, Graph ) ),
          RadioFailureRemarks=val(RadioFailureRemarksValue)
        );
        (
          rdf( RadioFailureRemarksNode,rdf:value,RadioFailureRemarksValue,Graph ),
          ( rdf( RadioFailureRemarksNode, aixm:uom, RadioFailureRemarksUOM, Graph ); rdf( RadioFailureRemarksNode, fixm:uom, RadioFailureRemarksUOM, Graph ); rdf( RadioFailureRemarksNode, plain:uom, RadioFailureRemarksUOM, Graph ) ),
          RadioFailureRemarks=xval(RadioFailureRemarksValue,RadioFailureRemarksUOM)
        );
        (
          rdf( RadioFailureRemarksNode,aixm:nilReason, RadioFailureRemarksNilReason, Graph ),
          RadioFailureRemarks=nil(RadioFailureRemarksNilReason)
        );
        (
          rdf( RadioFailureRemarksNode,gml:indeterminatePosition, RadioFailureRemarksIndeterminate, Graph ),
          RadioFailureRemarks=indeterminate(RadioFailureRemarksIndeterminate)
        )
      )
  )
  ,(
    ( RemainingComCapability='$null$',
      \+ rdf( RadioCommunicationFailure,fixm:'remainingComCapability',_RemainingComCapability,Graph )
    );
  ( rdf( RadioCommunicationFailure,fixm:'remainingComCapability',RemainingComCapabilityNode,Graph )),
      (
        (
          rdf(RemainingComCapabilityNode,rdf:value,RemainingComCapabilityValue,Graph),
         \+ ( rdf( RemainingComCapabilityNode, aixm:uom, _RemainingComCapabilityUOM, Graph ); rdf( RemainingComCapabilityNode, fixm:uom, _RemainingComCapabilityUOM, Graph ); rdf( RemainingComCapabilityNode, plain:uom, _RemainingComCapabilityUOM, Graph ) ),
          RemainingComCapability=val(RemainingComCapabilityValue)
        );
        (
          rdf( RemainingComCapabilityNode,rdf:value,RemainingComCapabilityValue,Graph ),
          ( rdf( RemainingComCapabilityNode, aixm:uom, RemainingComCapabilityUOM, Graph ); rdf( RemainingComCapabilityNode, fixm:uom, RemainingComCapabilityUOM, Graph ); rdf( RemainingComCapabilityNode, plain:uom, RemainingComCapabilityUOM, Graph ) ),
          RemainingComCapability=xval(RemainingComCapabilityValue,RemainingComCapabilityUOM)
        );
        (
          rdf( RemainingComCapabilityNode,aixm:nilReason, RemainingComCapabilityNilReason, Graph ),
          RemainingComCapability=nil(RemainingComCapabilityNilReason)
        );
        (
          rdf( RemainingComCapabilityNode,gml:indeterminatePosition, RemainingComCapabilityIndeterminate, Graph ),
          RemainingComCapability=indeterminate(RemainingComCapabilityIndeterminate)
        )
      )
  )
  ,( ( Contact='$null$',
    \+ rdf( RadioCommunicationFailure,fixm:'contact', _Contact, Graph  )
   ; rdf(RadioCommunicationFailure,fixm:'contact', Contact, Graph ) )
  ) .

aixm_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus, Warning, Usage) :-
  rdf(AirportHeliportAvailability,rdf:type,aixm:'AirportHeliportAvailability',Graph)
  ,(
    ( OperationalStatus='$null$',
      \+ rdf( AirportHeliportAvailability,aixm:'operationalStatus',_OperationalStatus,Graph )
    );
  ( rdf( AirportHeliportAvailability,aixm:'operationalStatus',OperationalStatusNode,Graph )),
      (
        (
          rdf(OperationalStatusNode,rdf:value,OperationalStatusValue,Graph),
         \+ ( rdf( OperationalStatusNode, aixm:uom, _OperationalStatusUOM, Graph ); rdf( OperationalStatusNode, fixm:uom, _OperationalStatusUOM, Graph ); rdf( OperationalStatusNode, plain:uom, _OperationalStatusUOM, Graph ) ),
          OperationalStatus=val(OperationalStatusValue)
        );
        (
          rdf( OperationalStatusNode,rdf:value,OperationalStatusValue,Graph ),
          ( rdf( OperationalStatusNode, aixm:uom, OperationalStatusUOM, Graph ); rdf( OperationalStatusNode, fixm:uom, OperationalStatusUOM, Graph ); rdf( OperationalStatusNode, plain:uom, OperationalStatusUOM, Graph ) ),
          OperationalStatus=xval(OperationalStatusValue,OperationalStatusUOM)
        );
        (
          rdf( OperationalStatusNode,aixm:nilReason, OperationalStatusNilReason, Graph ),
          OperationalStatus=nil(OperationalStatusNilReason)
        );
        (
          rdf( OperationalStatusNode,gml:indeterminatePosition, OperationalStatusIndeterminate, Graph ),
          OperationalStatus=indeterminate(OperationalStatusIndeterminate)
        )
      )
  )
  ,(
    ( Warning='$null$',
      \+ rdf( AirportHeliportAvailability,aixm:'warning',_Warning,Graph )
    );
  ( rdf( AirportHeliportAvailability,aixm:'warning',WarningNode,Graph )),
      (
        (
          rdf(WarningNode,rdf:value,WarningValue,Graph),
         \+ ( rdf( WarningNode, aixm:uom, _WarningUOM, Graph ); rdf( WarningNode, fixm:uom, _WarningUOM, Graph ); rdf( WarningNode, plain:uom, _WarningUOM, Graph ) ),
          Warning=val(WarningValue)
        );
        (
          rdf( WarningNode,rdf:value,WarningValue,Graph ),
          ( rdf( WarningNode, aixm:uom, WarningUOM, Graph ); rdf( WarningNode, fixm:uom, WarningUOM, Graph ); rdf( WarningNode, plain:uom, WarningUOM, Graph ) ),
          Warning=xval(WarningValue,WarningUOM)
        );
        (
          rdf( WarningNode,aixm:nilReason, WarningNilReason, Graph ),
          Warning=nil(WarningNilReason)
        );
        (
          rdf( WarningNode,gml:indeterminatePosition, WarningIndeterminate, Graph ),
          Warning=indeterminate(WarningIndeterminate)
        )
      )
  )
  ,findall(A, rdf(AirportHeliportAvailability,aixm:'usage',A,Graph), Usage) .

fixm_FlightArrival(Graph, FlightArrival, ApproachFix, ApproachTime, ArrivalAerodrome, ArrivalAerodromeAlternate, ArrivalAerodromeOriginal, ArrivalFix, ArrivalFixTime, ArrivalFleetPrioritization, ArrivalSequenceNumber, EarliestInBlockTime, FiledRevisedDestinationAerodrome, FiledRevisedDestinationStar, RunwayPositionAndTime, StandardInstrumentArrival, StandPositionAndTime, LandingLimits) :-
  rdf(FlightArrival,rdf:type,fixm:'FlightArrival',Graph)
  ,( ( ApproachFix='$null$',
    \+ rdf( FlightArrival,fixm:'approachFix', _ApproachFix, Graph  )
   ; rdf(FlightArrival,fixm:'approachFix', ApproachFix, Graph ) )
  )
  ,( ( ApproachTime='$null$',
    \+ rdf( FlightArrival,fixm:'approachTime', _ApproachTime, Graph  )
   ; rdf(FlightArrival,fixm:'approachTime', ApproachTime, Graph ) )
  )
  ,( ( ArrivalAerodrome='$null$',
    \+ rdf( FlightArrival,fixm:'arrivalAerodrome', _ArrivalAerodrome, Graph  )
   ; rdf(FlightArrival,fixm:'arrivalAerodrome', ArrivalAerodrome, Graph ) )
  )
  ,findall(A, rdf(FlightArrival,fixm:'arrivalAerodromeAlternate',A,Graph), ArrivalAerodromeAlternate)
  ,( ( ArrivalAerodromeOriginal='$null$',
    \+ rdf( FlightArrival,fixm:'arrivalAerodromeOriginal', _ArrivalAerodromeOriginal, Graph  )
   ; rdf(FlightArrival,fixm:'arrivalAerodromeOriginal', ArrivalAerodromeOriginal, Graph ) )
  )
  ,( ( ArrivalFix='$null$',
    \+ rdf( FlightArrival,fixm:'arrivalFix', _ArrivalFix, Graph  )
   ; rdf(FlightArrival,fixm:'arrivalFix', ArrivalFix, Graph ) )
  )
  ,( ( ArrivalFixTime='$null$',
    \+ rdf( FlightArrival,fixm:'arrivalFixTime', _ArrivalFixTime, Graph  )
   ; rdf(FlightArrival,fixm:'arrivalFixTime', ArrivalFixTime, Graph ) )
  )
  ,(
    ( ArrivalFleetPrioritization='$null$',
      \+ rdf( FlightArrival,fixm:'arrivalFleetPrioritization',_ArrivalFleetPrioritization,Graph )
    );
  ( rdf( FlightArrival,fixm:'arrivalFleetPrioritization',ArrivalFleetPrioritizationNode,Graph )),
      (
        (
          rdf(ArrivalFleetPrioritizationNode,rdf:value,ArrivalFleetPrioritizationValue,Graph),
         \+ ( rdf( ArrivalFleetPrioritizationNode, aixm:uom, _ArrivalFleetPrioritizationUOM, Graph ); rdf( ArrivalFleetPrioritizationNode, fixm:uom, _ArrivalFleetPrioritizationUOM, Graph ); rdf( ArrivalFleetPrioritizationNode, plain:uom, _ArrivalFleetPrioritizationUOM, Graph ) ),
          ArrivalFleetPrioritization=val(ArrivalFleetPrioritizationValue)
        );
        (
          rdf( ArrivalFleetPrioritizationNode,rdf:value,ArrivalFleetPrioritizationValue,Graph ),
          ( rdf( ArrivalFleetPrioritizationNode, aixm:uom, ArrivalFleetPrioritizationUOM, Graph ); rdf( ArrivalFleetPrioritizationNode, fixm:uom, ArrivalFleetPrioritizationUOM, Graph ); rdf( ArrivalFleetPrioritizationNode, plain:uom, ArrivalFleetPrioritizationUOM, Graph ) ),
          ArrivalFleetPrioritization=xval(ArrivalFleetPrioritizationValue,ArrivalFleetPrioritizationUOM)
        );
        (
          rdf( ArrivalFleetPrioritizationNode,aixm:nilReason, ArrivalFleetPrioritizationNilReason, Graph ),
          ArrivalFleetPrioritization=nil(ArrivalFleetPrioritizationNilReason)
        );
        (
          rdf( ArrivalFleetPrioritizationNode,gml:indeterminatePosition, ArrivalFleetPrioritizationIndeterminate, Graph ),
          ArrivalFleetPrioritization=indeterminate(ArrivalFleetPrioritizationIndeterminate)
        )
      )
  )
  ,(
    ( ArrivalSequenceNumber='$null$',
      \+ rdf( FlightArrival,fixm:'arrivalSequenceNumber',_ArrivalSequenceNumber,Graph )
    );
  ( rdf( FlightArrival,fixm:'arrivalSequenceNumber',ArrivalSequenceNumberNode,Graph )),
      (
        (
          rdf(ArrivalSequenceNumberNode,rdf:value,ArrivalSequenceNumberValue,Graph),
         \+ ( rdf( ArrivalSequenceNumberNode, aixm:uom, _ArrivalSequenceNumberUOM, Graph ); rdf( ArrivalSequenceNumberNode, fixm:uom, _ArrivalSequenceNumberUOM, Graph ); rdf( ArrivalSequenceNumberNode, plain:uom, _ArrivalSequenceNumberUOM, Graph ) ),
          ArrivalSequenceNumber=val(ArrivalSequenceNumberValue)
        );
        (
          rdf( ArrivalSequenceNumberNode,rdf:value,ArrivalSequenceNumberValue,Graph ),
          ( rdf( ArrivalSequenceNumberNode, aixm:uom, ArrivalSequenceNumberUOM, Graph ); rdf( ArrivalSequenceNumberNode, fixm:uom, ArrivalSequenceNumberUOM, Graph ); rdf( ArrivalSequenceNumberNode, plain:uom, ArrivalSequenceNumberUOM, Graph ) ),
          ArrivalSequenceNumber=xval(ArrivalSequenceNumberValue,ArrivalSequenceNumberUOM)
        );
        (
          rdf( ArrivalSequenceNumberNode,aixm:nilReason, ArrivalSequenceNumberNilReason, Graph ),
          ArrivalSequenceNumber=nil(ArrivalSequenceNumberNilReason)
        );
        (
          rdf( ArrivalSequenceNumberNode,gml:indeterminatePosition, ArrivalSequenceNumberIndeterminate, Graph ),
          ArrivalSequenceNumber=indeterminate(ArrivalSequenceNumberIndeterminate)
        )
      )
  )
  ,(
    ( EarliestInBlockTime='$null$',
      \+ rdf( FlightArrival,fixm:'earliestInBlockTime',_EarliestInBlockTime,Graph )
    );
  ( rdf( FlightArrival,fixm:'earliestInBlockTime',EarliestInBlockTimeNode,Graph )),
      (
        (
          rdf(EarliestInBlockTimeNode,rdf:value,EarliestInBlockTimeValue,Graph),
         \+ ( rdf( EarliestInBlockTimeNode, aixm:uom, _EarliestInBlockTimeUOM, Graph ); rdf( EarliestInBlockTimeNode, fixm:uom, _EarliestInBlockTimeUOM, Graph ); rdf( EarliestInBlockTimeNode, plain:uom, _EarliestInBlockTimeUOM, Graph ) ),
          EarliestInBlockTime=val(EarliestInBlockTimeValue)
        );
        (
          rdf( EarliestInBlockTimeNode,rdf:value,EarliestInBlockTimeValue,Graph ),
          ( rdf( EarliestInBlockTimeNode, aixm:uom, EarliestInBlockTimeUOM, Graph ); rdf( EarliestInBlockTimeNode, fixm:uom, EarliestInBlockTimeUOM, Graph ); rdf( EarliestInBlockTimeNode, plain:uom, EarliestInBlockTimeUOM, Graph ) ),
          EarliestInBlockTime=xval(EarliestInBlockTimeValue,EarliestInBlockTimeUOM)
        );
        (
          rdf( EarliestInBlockTimeNode,aixm:nilReason, EarliestInBlockTimeNilReason, Graph ),
          EarliestInBlockTime=nil(EarliestInBlockTimeNilReason)
        );
        (
          rdf( EarliestInBlockTimeNode,gml:indeterminatePosition, EarliestInBlockTimeIndeterminate, Graph ),
          EarliestInBlockTime=indeterminate(EarliestInBlockTimeIndeterminate)
        )
      )
  )
  ,( ( FiledRevisedDestinationAerodrome='$null$',
    \+ rdf( FlightArrival,fixm:'filedRevisedDestinationAerodrome', _FiledRevisedDestinationAerodrome, Graph  )
   ; rdf(FlightArrival,fixm:'filedRevisedDestinationAerodrome', FiledRevisedDestinationAerodrome, Graph ) )
  )
  ,(
    ( FiledRevisedDestinationStar='$null$',
      \+ rdf( FlightArrival,fixm:'filedRevisedDestinationStar',_FiledRevisedDestinationStar,Graph )
    );
  ( rdf( FlightArrival,fixm:'filedRevisedDestinationStar',FiledRevisedDestinationStarNode,Graph )),
      (
        (
          rdf(FiledRevisedDestinationStarNode,rdf:value,FiledRevisedDestinationStarValue,Graph),
         \+ ( rdf( FiledRevisedDestinationStarNode, aixm:uom, _FiledRevisedDestinationStarUOM, Graph ); rdf( FiledRevisedDestinationStarNode, fixm:uom, _FiledRevisedDestinationStarUOM, Graph ); rdf( FiledRevisedDestinationStarNode, plain:uom, _FiledRevisedDestinationStarUOM, Graph ) ),
          FiledRevisedDestinationStar=val(FiledRevisedDestinationStarValue)
        );
        (
          rdf( FiledRevisedDestinationStarNode,rdf:value,FiledRevisedDestinationStarValue,Graph ),
          ( rdf( FiledRevisedDestinationStarNode, aixm:uom, FiledRevisedDestinationStarUOM, Graph ); rdf( FiledRevisedDestinationStarNode, fixm:uom, FiledRevisedDestinationStarUOM, Graph ); rdf( FiledRevisedDestinationStarNode, plain:uom, FiledRevisedDestinationStarUOM, Graph ) ),
          FiledRevisedDestinationStar=xval(FiledRevisedDestinationStarValue,FiledRevisedDestinationStarUOM)
        );
        (
          rdf( FiledRevisedDestinationStarNode,aixm:nilReason, FiledRevisedDestinationStarNilReason, Graph ),
          FiledRevisedDestinationStar=nil(FiledRevisedDestinationStarNilReason)
        );
        (
          rdf( FiledRevisedDestinationStarNode,gml:indeterminatePosition, FiledRevisedDestinationStarIndeterminate, Graph ),
          FiledRevisedDestinationStar=indeterminate(FiledRevisedDestinationStarIndeterminate)
        )
      )
  )
  ,( ( RunwayPositionAndTime='$null$',
    \+ rdf( FlightArrival,fixm:'runwayPositionAndTime', _RunwayPositionAndTime, Graph  )
   ; rdf(FlightArrival,fixm:'runwayPositionAndTime', RunwayPositionAndTime, Graph ) )
  )
  ,(
    ( StandardInstrumentArrival='$null$',
      \+ rdf( FlightArrival,fixm:'standardInstrumentArrival',_StandardInstrumentArrival,Graph )
    );
  ( rdf( FlightArrival,fixm:'standardInstrumentArrival',StandardInstrumentArrivalNode,Graph )),
      (
        (
          rdf(StandardInstrumentArrivalNode,rdf:value,StandardInstrumentArrivalValue,Graph),
         \+ ( rdf( StandardInstrumentArrivalNode, aixm:uom, _StandardInstrumentArrivalUOM, Graph ); rdf( StandardInstrumentArrivalNode, fixm:uom, _StandardInstrumentArrivalUOM, Graph ); rdf( StandardInstrumentArrivalNode, plain:uom, _StandardInstrumentArrivalUOM, Graph ) ),
          StandardInstrumentArrival=val(StandardInstrumentArrivalValue)
        );
        (
          rdf( StandardInstrumentArrivalNode,rdf:value,StandardInstrumentArrivalValue,Graph ),
          ( rdf( StandardInstrumentArrivalNode, aixm:uom, StandardInstrumentArrivalUOM, Graph ); rdf( StandardInstrumentArrivalNode, fixm:uom, StandardInstrumentArrivalUOM, Graph ); rdf( StandardInstrumentArrivalNode, plain:uom, StandardInstrumentArrivalUOM, Graph ) ),
          StandardInstrumentArrival=xval(StandardInstrumentArrivalValue,StandardInstrumentArrivalUOM)
        );
        (
          rdf( StandardInstrumentArrivalNode,aixm:nilReason, StandardInstrumentArrivalNilReason, Graph ),
          StandardInstrumentArrival=nil(StandardInstrumentArrivalNilReason)
        );
        (
          rdf( StandardInstrumentArrivalNode,gml:indeterminatePosition, StandardInstrumentArrivalIndeterminate, Graph ),
          StandardInstrumentArrival=indeterminate(StandardInstrumentArrivalIndeterminate)
        )
      )
  )
  ,( ( StandPositionAndTime='$null$',
    \+ rdf( FlightArrival,fixm:'standPositionAndTime', _StandPositionAndTime, Graph  )
   ; rdf(FlightArrival,fixm:'standPositionAndTime', StandPositionAndTime, Graph ) )
  )
  ,(
    ( LandingLimits='$null$',
      \+ rdf( FlightArrival,fixm:'landingLimits',_LandingLimits,Graph )
    );
  ( rdf( FlightArrival,fixm:'landingLimits',LandingLimitsNode,Graph )),
      (
        (
          rdf(LandingLimitsNode,rdf:value,LandingLimitsValue,Graph),
         \+ ( rdf( LandingLimitsNode, aixm:uom, _LandingLimitsUOM, Graph ); rdf( LandingLimitsNode, fixm:uom, _LandingLimitsUOM, Graph ); rdf( LandingLimitsNode, plain:uom, _LandingLimitsUOM, Graph ) ),
          LandingLimits=val(LandingLimitsValue)
        );
        (
          rdf( LandingLimitsNode,rdf:value,LandingLimitsValue,Graph ),
          ( rdf( LandingLimitsNode, aixm:uom, LandingLimitsUOM, Graph ); rdf( LandingLimitsNode, fixm:uom, LandingLimitsUOM, Graph ); rdf( LandingLimitsNode, plain:uom, LandingLimitsUOM, Graph ) ),
          LandingLimits=xval(LandingLimitsValue,LandingLimitsUOM)
        );
        (
          rdf( LandingLimitsNode,aixm:nilReason, LandingLimitsNilReason, Graph ),
          LandingLimits=nil(LandingLimitsNilReason)
        );
        (
          rdf( LandingLimitsNode,gml:indeterminatePosition, LandingLimitsIndeterminate, Graph ),
          LandingLimits=indeterminate(LandingLimitsIndeterminate)
        )
      )
  ) .

fixm_RadioactiveMaterial(Graph, RadioactiveMaterial, CriticalitySafetyIndex, TransportIndex, FissileExceptedIndicator, Category, Radionuclide) :-
  rdf(RadioactiveMaterial,rdf:type,fixm:'RadioactiveMaterial',Graph)
  ,(
    ( CriticalitySafetyIndex='$null$',
      \+ rdf( RadioactiveMaterial,fixm:'criticalitySafetyIndex',_CriticalitySafetyIndex,Graph )
    );
  ( rdf( RadioactiveMaterial,fixm:'criticalitySafetyIndex',CriticalitySafetyIndexNode,Graph )),
      (
        (
          rdf(CriticalitySafetyIndexNode,rdf:value,CriticalitySafetyIndexValue,Graph),
         \+ ( rdf( CriticalitySafetyIndexNode, aixm:uom, _CriticalitySafetyIndexUOM, Graph ); rdf( CriticalitySafetyIndexNode, fixm:uom, _CriticalitySafetyIndexUOM, Graph ); rdf( CriticalitySafetyIndexNode, plain:uom, _CriticalitySafetyIndexUOM, Graph ) ),
          CriticalitySafetyIndex=val(CriticalitySafetyIndexValue)
        );
        (
          rdf( CriticalitySafetyIndexNode,rdf:value,CriticalitySafetyIndexValue,Graph ),
          ( rdf( CriticalitySafetyIndexNode, aixm:uom, CriticalitySafetyIndexUOM, Graph ); rdf( CriticalitySafetyIndexNode, fixm:uom, CriticalitySafetyIndexUOM, Graph ); rdf( CriticalitySafetyIndexNode, plain:uom, CriticalitySafetyIndexUOM, Graph ) ),
          CriticalitySafetyIndex=xval(CriticalitySafetyIndexValue,CriticalitySafetyIndexUOM)
        );
        (
          rdf( CriticalitySafetyIndexNode,aixm:nilReason, CriticalitySafetyIndexNilReason, Graph ),
          CriticalitySafetyIndex=nil(CriticalitySafetyIndexNilReason)
        );
        (
          rdf( CriticalitySafetyIndexNode,gml:indeterminatePosition, CriticalitySafetyIndexIndeterminate, Graph ),
          CriticalitySafetyIndex=indeterminate(CriticalitySafetyIndexIndeterminate)
        )
      )
  )
  ,(
    ( TransportIndex='$null$',
      \+ rdf( RadioactiveMaterial,fixm:'transportIndex',_TransportIndex,Graph )
    );
  ( rdf( RadioactiveMaterial,fixm:'transportIndex',TransportIndexNode,Graph )),
      (
        (
          rdf(TransportIndexNode,rdf:value,TransportIndexValue,Graph),
         \+ ( rdf( TransportIndexNode, aixm:uom, _TransportIndexUOM, Graph ); rdf( TransportIndexNode, fixm:uom, _TransportIndexUOM, Graph ); rdf( TransportIndexNode, plain:uom, _TransportIndexUOM, Graph ) ),
          TransportIndex=val(TransportIndexValue)
        );
        (
          rdf( TransportIndexNode,rdf:value,TransportIndexValue,Graph ),
          ( rdf( TransportIndexNode, aixm:uom, TransportIndexUOM, Graph ); rdf( TransportIndexNode, fixm:uom, TransportIndexUOM, Graph ); rdf( TransportIndexNode, plain:uom, TransportIndexUOM, Graph ) ),
          TransportIndex=xval(TransportIndexValue,TransportIndexUOM)
        );
        (
          rdf( TransportIndexNode,aixm:nilReason, TransportIndexNilReason, Graph ),
          TransportIndex=nil(TransportIndexNilReason)
        );
        (
          rdf( TransportIndexNode,gml:indeterminatePosition, TransportIndexIndeterminate, Graph ),
          TransportIndex=indeterminate(TransportIndexIndeterminate)
        )
      )
  )
  ,(
    ( FissileExceptedIndicator='$null$',
      \+ rdf( RadioactiveMaterial,fixm:'fissileExceptedIndicator',_FissileExceptedIndicator,Graph )
    );
  ( rdf( RadioactiveMaterial,fixm:'fissileExceptedIndicator',FissileExceptedIndicatorNode,Graph )),
      (
        (
          rdf(FissileExceptedIndicatorNode,rdf:value,FissileExceptedIndicatorValue,Graph),
         \+ ( rdf( FissileExceptedIndicatorNode, aixm:uom, _FissileExceptedIndicatorUOM, Graph ); rdf( FissileExceptedIndicatorNode, fixm:uom, _FissileExceptedIndicatorUOM, Graph ); rdf( FissileExceptedIndicatorNode, plain:uom, _FissileExceptedIndicatorUOM, Graph ) ),
          FissileExceptedIndicator=val(FissileExceptedIndicatorValue)
        );
        (
          rdf( FissileExceptedIndicatorNode,rdf:value,FissileExceptedIndicatorValue,Graph ),
          ( rdf( FissileExceptedIndicatorNode, aixm:uom, FissileExceptedIndicatorUOM, Graph ); rdf( FissileExceptedIndicatorNode, fixm:uom, FissileExceptedIndicatorUOM, Graph ); rdf( FissileExceptedIndicatorNode, plain:uom, FissileExceptedIndicatorUOM, Graph ) ),
          FissileExceptedIndicator=xval(FissileExceptedIndicatorValue,FissileExceptedIndicatorUOM)
        );
        (
          rdf( FissileExceptedIndicatorNode,aixm:nilReason, FissileExceptedIndicatorNilReason, Graph ),
          FissileExceptedIndicator=nil(FissileExceptedIndicatorNilReason)
        );
        (
          rdf( FissileExceptedIndicatorNode,gml:indeterminatePosition, FissileExceptedIndicatorIndeterminate, Graph ),
          FissileExceptedIndicator=indeterminate(FissileExceptedIndicatorIndeterminate)
        )
      )
  )
  ,(
    ( Category='$null$',
      \+ rdf( RadioactiveMaterial,fixm:'category',_Category,Graph )
    );
  ( rdf( RadioactiveMaterial,fixm:'category',CategoryNode,Graph )),
      (
        (
          rdf(CategoryNode,rdf:value,CategoryValue,Graph),
         \+ ( rdf( CategoryNode, aixm:uom, _CategoryUOM, Graph ); rdf( CategoryNode, fixm:uom, _CategoryUOM, Graph ); rdf( CategoryNode, plain:uom, _CategoryUOM, Graph ) ),
          Category=val(CategoryValue)
        );
        (
          rdf( CategoryNode,rdf:value,CategoryValue,Graph ),
          ( rdf( CategoryNode, aixm:uom, CategoryUOM, Graph ); rdf( CategoryNode, fixm:uom, CategoryUOM, Graph ); rdf( CategoryNode, plain:uom, CategoryUOM, Graph ) ),
          Category=xval(CategoryValue,CategoryUOM)
        );
        (
          rdf( CategoryNode,aixm:nilReason, CategoryNilReason, Graph ),
          Category=nil(CategoryNilReason)
        );
        (
          rdf( CategoryNode,gml:indeterminatePosition, CategoryIndeterminate, Graph ),
          Category=indeterminate(CategoryIndeterminate)
        )
      )
  )
  ,( ( Radionuclide='$null$',
    \+ rdf( RadioactiveMaterial,fixm:'radionuclide', _Radionuclide, Graph  )
   ; rdf(RadioactiveMaterial,fixm:'radionuclide', Radionuclide, Graph ) )
  ) .

fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled, Initial) :-
  rdf(ExtendedMultiTime,rdf:type,fixm:'ExtendedMultiTime',Graph)
  ,( ( Controlled='$null$',
    \+ rdf( ExtendedMultiTime,fixm:'controlled', _Controlled, Graph  )
   ; rdf(ExtendedMultiTime,fixm:'controlled', Controlled, Graph ) )
  )
  ,( ( Initial='$null$',
    \+ rdf( ExtendedMultiTime,fixm:'initial', _Initial, Graph  )
   ; rdf(ExtendedMultiTime,fixm:'initial', Initial, Graph ) )
  ) .

fixm_ControlElement(Graph, ControlElement) :-
  rdf(ControlElement,rdf:type,fixm:'ControlElement',Graph) .

fixm_AerodromesOfDestination(Graph, AerodromesOfDestination, AerodromeOfDestination, Alternate1, Alternate2, FiledRevisedDestinationAerodrome) :-
  rdf(AerodromesOfDestination,rdf:type,fixm:'AerodromesOfDestination',Graph)
  ,( ( AerodromeOfDestination='$null$',
    \+ rdf( AerodromesOfDestination,fixm:'aerodromeOfDestination', _AerodromeOfDestination, Graph  )
   ; rdf(AerodromesOfDestination,fixm:'aerodromeOfDestination', AerodromeOfDestination, Graph ) )
  )
  ,( ( Alternate1='$null$',
    \+ rdf( AerodromesOfDestination,fixm:'alternate1', _Alternate1, Graph  )
   ; rdf(AerodromesOfDestination,fixm:'alternate1', Alternate1, Graph ) )
  )
  ,( ( Alternate2='$null$',
    \+ rdf( AerodromesOfDestination,fixm:'alternate2', _Alternate2, Graph  )
   ; rdf(AerodromesOfDestination,fixm:'alternate2', Alternate2, Graph ) )
  )
  ,( ( FiledRevisedDestinationAerodrome='$null$',
    \+ rdf( AerodromesOfDestination,fixm:'filedRevisedDestinationAerodrome', _FiledRevisedDestinationAerodrome, Graph  )
   ; rdf(AerodromesOfDestination,fixm:'filedRevisedDestinationAerodrome', FiledRevisedDestinationAerodrome, Graph ) )
  ) .

fixm_AllPackedInOne(Graph, AllPackedInOne, NumberOfPackages, QValue) :-
  rdf(AllPackedInOne,rdf:type,fixm:'AllPackedInOne',Graph)
  ,(
    ( NumberOfPackages='$null$',
      \+ rdf( AllPackedInOne,fixm:'numberOfPackages',_NumberOfPackages,Graph )
    );
  ( rdf( AllPackedInOne,fixm:'numberOfPackages',NumberOfPackagesNode,Graph )),
      (
        (
          rdf(NumberOfPackagesNode,rdf:value,NumberOfPackagesValue,Graph),
         \+ ( rdf( NumberOfPackagesNode, aixm:uom, _NumberOfPackagesUOM, Graph ); rdf( NumberOfPackagesNode, fixm:uom, _NumberOfPackagesUOM, Graph ); rdf( NumberOfPackagesNode, plain:uom, _NumberOfPackagesUOM, Graph ) ),
          NumberOfPackages=val(NumberOfPackagesValue)
        );
        (
          rdf( NumberOfPackagesNode,rdf:value,NumberOfPackagesValue,Graph ),
          ( rdf( NumberOfPackagesNode, aixm:uom, NumberOfPackagesUOM, Graph ); rdf( NumberOfPackagesNode, fixm:uom, NumberOfPackagesUOM, Graph ); rdf( NumberOfPackagesNode, plain:uom, NumberOfPackagesUOM, Graph ) ),
          NumberOfPackages=xval(NumberOfPackagesValue,NumberOfPackagesUOM)
        );
        (
          rdf( NumberOfPackagesNode,aixm:nilReason, NumberOfPackagesNilReason, Graph ),
          NumberOfPackages=nil(NumberOfPackagesNilReason)
        );
        (
          rdf( NumberOfPackagesNode,gml:indeterminatePosition, NumberOfPackagesIndeterminate, Graph ),
          NumberOfPackages=indeterminate(NumberOfPackagesIndeterminate)
        )
      )
  )
  ,(
    ( QValue='$null$',
      \+ rdf( AllPackedInOne,fixm:'qValue',_QValue,Graph )
    );
  ( rdf( AllPackedInOne,fixm:'qValue',QValueNode,Graph )),
      (
        (
          rdf(QValueNode,rdf:value,QValueValue,Graph),
         \+ ( rdf( QValueNode, aixm:uom, _QValueUOM, Graph ); rdf( QValueNode, fixm:uom, _QValueUOM, Graph ); rdf( QValueNode, plain:uom, _QValueUOM, Graph ) ),
          QValue=val(QValueValue)
        );
        (
          rdf( QValueNode,rdf:value,QValueValue,Graph ),
          ( rdf( QValueNode, aixm:uom, QValueUOM, Graph ); rdf( QValueNode, fixm:uom, QValueUOM, Graph ); rdf( QValueNode, plain:uom, QValueUOM, Graph ) ),
          QValue=xval(QValueValue,QValueUOM)
        );
        (
          rdf( QValueNode,aixm:nilReason, QValueNilReason, Graph ),
          QValue=nil(QValueNilReason)
        );
        (
          rdf( QValueNode,gml:indeterminatePosition, QValueIndeterminate, Graph ),
          QValue=indeterminate(QValueIndeterminate)
        )
      )
  ) .

aixm_AltimeterSource(Graph, AltimeterSource, TimeSlice) :-
  rdf(AltimeterSource,rdf:type,aixm:'AltimeterSource',Graph)
  ,findall(A, rdf(AltimeterSource,aixm:'timeSlice',A,Graph), TimeSlice) .

fixm_SurvivalCapabilities(Graph, SurvivalCapabilities, SurvivalEquipmentRemarks, DinghyInformation, EmergencyRadioCode, LifeJacketCode, SurvivalEquipmentCode) :-
  rdf(SurvivalCapabilities,rdf:type,fixm:'SurvivalCapabilities',Graph)
  ,(
    ( SurvivalEquipmentRemarks='$null$',
      \+ rdf( SurvivalCapabilities,fixm:'survivalEquipmentRemarks',_SurvivalEquipmentRemarks,Graph )
    );
  ( rdf( SurvivalCapabilities,fixm:'survivalEquipmentRemarks',SurvivalEquipmentRemarksNode,Graph )),
      (
        (
          rdf(SurvivalEquipmentRemarksNode,rdf:value,SurvivalEquipmentRemarksValue,Graph),
         \+ ( rdf( SurvivalEquipmentRemarksNode, aixm:uom, _SurvivalEquipmentRemarksUOM, Graph ); rdf( SurvivalEquipmentRemarksNode, fixm:uom, _SurvivalEquipmentRemarksUOM, Graph ); rdf( SurvivalEquipmentRemarksNode, plain:uom, _SurvivalEquipmentRemarksUOM, Graph ) ),
          SurvivalEquipmentRemarks=val(SurvivalEquipmentRemarksValue)
        );
        (
          rdf( SurvivalEquipmentRemarksNode,rdf:value,SurvivalEquipmentRemarksValue,Graph ),
          ( rdf( SurvivalEquipmentRemarksNode, aixm:uom, SurvivalEquipmentRemarksUOM, Graph ); rdf( SurvivalEquipmentRemarksNode, fixm:uom, SurvivalEquipmentRemarksUOM, Graph ); rdf( SurvivalEquipmentRemarksNode, plain:uom, SurvivalEquipmentRemarksUOM, Graph ) ),
          SurvivalEquipmentRemarks=xval(SurvivalEquipmentRemarksValue,SurvivalEquipmentRemarksUOM)
        );
        (
          rdf( SurvivalEquipmentRemarksNode,aixm:nilReason, SurvivalEquipmentRemarksNilReason, Graph ),
          SurvivalEquipmentRemarks=nil(SurvivalEquipmentRemarksNilReason)
        );
        (
          rdf( SurvivalEquipmentRemarksNode,gml:indeterminatePosition, SurvivalEquipmentRemarksIndeterminate, Graph ),
          SurvivalEquipmentRemarks=indeterminate(SurvivalEquipmentRemarksIndeterminate)
        )
      )
  )
  ,( ( DinghyInformation='$null$',
    \+ rdf( SurvivalCapabilities,fixm:'dinghyInformation', _DinghyInformation, Graph  )
   ; rdf(SurvivalCapabilities,fixm:'dinghyInformation', DinghyInformation, Graph ) )
  )
  ,findall(A, rdf(SurvivalCapabilities,fixm:'emergencyRadioCode',A,Graph), EmergencyRadioCode)
  ,findall(A, rdf(SurvivalCapabilities,fixm:'lifeJacketCode',A,Graph), LifeJacketCode)
  ,findall(A, rdf(SurvivalCapabilities,fixm:'survivalEquipmentCode',A,Graph), SurvivalEquipmentCode) .

fixm_DirectRouting(Graph, DirectRouting, From, To) :-
  rdf(DirectRouting,rdf:type,fixm:'DirectRouting',Graph)
  ,( ( From='$null$',
    \+ rdf( DirectRouting,fixm:'from', _From, Graph  )
   ; rdf(DirectRouting,fixm:'from', From, Graph ) )
  )
  ,( ( To='$null$',
    \+ rdf( DirectRouting,fixm:'to', _To, Graph  )
   ; rdf(DirectRouting,fixm:'to', To, Graph ) )
  ) .

fixm_TargetMultiTime(Graph, TargetMultiTime, Target) :-
  subClassOf(T,fixm:'TargetMultiTime')
  ,rdf(TargetMultiTime,rdf:type,T,Graph)
  ,( ( Target='$null$',
    \+ rdf( TargetMultiTime,fixm:'target', _Target, Graph  )
   ; rdf(TargetMultiTime,fixm:'target', Target, Graph ) )
  ) .

fixm_AircraftType(Graph, AircraftType) :-
  rdf(AircraftType,rdf:type,fixm:'AircraftType',Graph) .

fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes) :-
  subClassOf(T,fixm:'FlightDeparture')
  ,rdf(FlightDeparture,rdf:type,T,Graph)
  ,( ( DepartureAerodrome='$null$',
    \+ rdf( FlightDeparture,fixm:'departureAerodrome', _DepartureAerodrome, Graph  )
   ; rdf(FlightDeparture,fixm:'departureAerodrome', DepartureAerodrome, Graph ) )
  )
  ,( ( DepartureFix='$null$',
    \+ rdf( FlightDeparture,fixm:'departureFix', _DepartureFix, Graph  )
   ; rdf(FlightDeparture,fixm:'departureFix', DepartureFix, Graph ) )
  )
  ,( ( DepartureFixTime='$null$',
    \+ rdf( FlightDeparture,fixm:'departureFixTime', _DepartureFixTime, Graph  )
   ; rdf(FlightDeparture,fixm:'departureFixTime', DepartureFixTime, Graph ) )
  )
  ,(
    ( DepartureFleetPrioritization='$null$',
      \+ rdf( FlightDeparture,fixm:'departureFleetPrioritization',_DepartureFleetPrioritization,Graph )
    );
  ( rdf( FlightDeparture,fixm:'departureFleetPrioritization',DepartureFleetPrioritizationNode,Graph )),
      (
        (
          rdf(DepartureFleetPrioritizationNode,rdf:value,DepartureFleetPrioritizationValue,Graph),
         \+ ( rdf( DepartureFleetPrioritizationNode, aixm:uom, _DepartureFleetPrioritizationUOM, Graph ); rdf( DepartureFleetPrioritizationNode, fixm:uom, _DepartureFleetPrioritizationUOM, Graph ); rdf( DepartureFleetPrioritizationNode, plain:uom, _DepartureFleetPrioritizationUOM, Graph ) ),
          DepartureFleetPrioritization=val(DepartureFleetPrioritizationValue)
        );
        (
          rdf( DepartureFleetPrioritizationNode,rdf:value,DepartureFleetPrioritizationValue,Graph ),
          ( rdf( DepartureFleetPrioritizationNode, aixm:uom, DepartureFleetPrioritizationUOM, Graph ); rdf( DepartureFleetPrioritizationNode, fixm:uom, DepartureFleetPrioritizationUOM, Graph ); rdf( DepartureFleetPrioritizationNode, plain:uom, DepartureFleetPrioritizationUOM, Graph ) ),
          DepartureFleetPrioritization=xval(DepartureFleetPrioritizationValue,DepartureFleetPrioritizationUOM)
        );
        (
          rdf( DepartureFleetPrioritizationNode,aixm:nilReason, DepartureFleetPrioritizationNilReason, Graph ),
          DepartureFleetPrioritization=nil(DepartureFleetPrioritizationNilReason)
        );
        (
          rdf( DepartureFleetPrioritizationNode,gml:indeterminatePosition, DepartureFleetPrioritizationIndeterminate, Graph ),
          DepartureFleetPrioritization=indeterminate(DepartureFleetPrioritizationIndeterminate)
        )
      )
  )
  ,(
    ( DepartureSlot='$null$',
      \+ rdf( FlightDeparture,fixm:'departureSlot',_DepartureSlot,Graph )
    );
  ( rdf( FlightDeparture,fixm:'departureSlot',DepartureSlotNode,Graph )),
      (
        (
          rdf(DepartureSlotNode,rdf:value,DepartureSlotValue,Graph),
         \+ ( rdf( DepartureSlotNode, aixm:uom, _DepartureSlotUOM, Graph ); rdf( DepartureSlotNode, fixm:uom, _DepartureSlotUOM, Graph ); rdf( DepartureSlotNode, plain:uom, _DepartureSlotUOM, Graph ) ),
          DepartureSlot=val(DepartureSlotValue)
        );
        (
          rdf( DepartureSlotNode,rdf:value,DepartureSlotValue,Graph ),
          ( rdf( DepartureSlotNode, aixm:uom, DepartureSlotUOM, Graph ); rdf( DepartureSlotNode, fixm:uom, DepartureSlotUOM, Graph ); rdf( DepartureSlotNode, plain:uom, DepartureSlotUOM, Graph ) ),
          DepartureSlot=xval(DepartureSlotValue,DepartureSlotUOM)
        );
        (
          rdf( DepartureSlotNode,aixm:nilReason, DepartureSlotNilReason, Graph ),
          DepartureSlot=nil(DepartureSlotNilReason)
        );
        (
          rdf( DepartureSlotNode,gml:indeterminatePosition, DepartureSlotIndeterminate, Graph ),
          DepartureSlot=indeterminate(DepartureSlotIndeterminate)
        )
      )
  )
  ,(
    ( EarliestOffBlockTime='$null$',
      \+ rdf( FlightDeparture,fixm:'earliestOffBlockTime',_EarliestOffBlockTime,Graph )
    );
  ( rdf( FlightDeparture,fixm:'earliestOffBlockTime',EarliestOffBlockTimeNode,Graph )),
      (
        (
          rdf(EarliestOffBlockTimeNode,rdf:value,EarliestOffBlockTimeValue,Graph),
         \+ ( rdf( EarliestOffBlockTimeNode, aixm:uom, _EarliestOffBlockTimeUOM, Graph ); rdf( EarliestOffBlockTimeNode, fixm:uom, _EarliestOffBlockTimeUOM, Graph ); rdf( EarliestOffBlockTimeNode, plain:uom, _EarliestOffBlockTimeUOM, Graph ) ),
          EarliestOffBlockTime=val(EarliestOffBlockTimeValue)
        );
        (
          rdf( EarliestOffBlockTimeNode,rdf:value,EarliestOffBlockTimeValue,Graph ),
          ( rdf( EarliestOffBlockTimeNode, aixm:uom, EarliestOffBlockTimeUOM, Graph ); rdf( EarliestOffBlockTimeNode, fixm:uom, EarliestOffBlockTimeUOM, Graph ); rdf( EarliestOffBlockTimeNode, plain:uom, EarliestOffBlockTimeUOM, Graph ) ),
          EarliestOffBlockTime=xval(EarliestOffBlockTimeValue,EarliestOffBlockTimeUOM)
        );
        (
          rdf( EarliestOffBlockTimeNode,aixm:nilReason, EarliestOffBlockTimeNilReason, Graph ),
          EarliestOffBlockTime=nil(EarliestOffBlockTimeNilReason)
        );
        (
          rdf( EarliestOffBlockTimeNode,gml:indeterminatePosition, EarliestOffBlockTimeIndeterminate, Graph ),
          EarliestOffBlockTime=indeterminate(EarliestOffBlockTimeIndeterminate)
        )
      )
  )
  ,( ( OffBlockReadyTime='$null$',
    \+ rdf( FlightDeparture,fixm:'offBlockReadyTime', _OffBlockReadyTime, Graph  )
   ; rdf(FlightDeparture,fixm:'offBlockReadyTime', OffBlockReadyTime, Graph ) )
  )
  ,( ( RunwayPositionAndTime='$null$',
    \+ rdf( FlightDeparture,fixm:'runwayPositionAndTime', _RunwayPositionAndTime, Graph  )
   ; rdf(FlightDeparture,fixm:'runwayPositionAndTime', RunwayPositionAndTime, Graph ) )
  )
  ,(
    ( StandardInstrumentDeparture='$null$',
      \+ rdf( FlightDeparture,fixm:'standardInstrumentDeparture',_StandardInstrumentDeparture,Graph )
    );
  ( rdf( FlightDeparture,fixm:'standardInstrumentDeparture',StandardInstrumentDepartureNode,Graph )),
      (
        (
          rdf(StandardInstrumentDepartureNode,rdf:value,StandardInstrumentDepartureValue,Graph),
         \+ ( rdf( StandardInstrumentDepartureNode, aixm:uom, _StandardInstrumentDepartureUOM, Graph ); rdf( StandardInstrumentDepartureNode, fixm:uom, _StandardInstrumentDepartureUOM, Graph ); rdf( StandardInstrumentDepartureNode, plain:uom, _StandardInstrumentDepartureUOM, Graph ) ),
          StandardInstrumentDeparture=val(StandardInstrumentDepartureValue)
        );
        (
          rdf( StandardInstrumentDepartureNode,rdf:value,StandardInstrumentDepartureValue,Graph ),
          ( rdf( StandardInstrumentDepartureNode, aixm:uom, StandardInstrumentDepartureUOM, Graph ); rdf( StandardInstrumentDepartureNode, fixm:uom, StandardInstrumentDepartureUOM, Graph ); rdf( StandardInstrumentDepartureNode, plain:uom, StandardInstrumentDepartureUOM, Graph ) ),
          StandardInstrumentDeparture=xval(StandardInstrumentDepartureValue,StandardInstrumentDepartureUOM)
        );
        (
          rdf( StandardInstrumentDepartureNode,aixm:nilReason, StandardInstrumentDepartureNilReason, Graph ),
          StandardInstrumentDeparture=nil(StandardInstrumentDepartureNilReason)
        );
        (
          rdf( StandardInstrumentDepartureNode,gml:indeterminatePosition, StandardInstrumentDepartureIndeterminate, Graph ),
          StandardInstrumentDeparture=indeterminate(StandardInstrumentDepartureIndeterminate)
        )
      )
  )
  ,( ( StandPositionAndTime='$null$',
    \+ rdf( FlightDeparture,fixm:'standPositionAndTime', _StandPositionAndTime, Graph  )
   ; rdf(FlightDeparture,fixm:'standPositionAndTime', StandPositionAndTime, Graph ) )
  )
  ,findall(A, rdf(FlightDeparture,fixm:'takeoffAlternateAerodrome',A,Graph), TakeoffAlternateAerodrome)
  ,(
    ( TakeoffWeight='$null$',
      \+ rdf( FlightDeparture,fixm:'takeoffWeight',_TakeoffWeight,Graph )
    );
  ( rdf( FlightDeparture,fixm:'takeoffWeight',TakeoffWeightNode,Graph )),
      (
        (
          rdf(TakeoffWeightNode,rdf:value,TakeoffWeightValue,Graph),
         \+ ( rdf( TakeoffWeightNode, aixm:uom, _TakeoffWeightUOM, Graph ); rdf( TakeoffWeightNode, fixm:uom, _TakeoffWeightUOM, Graph ); rdf( TakeoffWeightNode, plain:uom, _TakeoffWeightUOM, Graph ) ),
          TakeoffWeight=val(TakeoffWeightValue)
        );
        (
          rdf( TakeoffWeightNode,rdf:value,TakeoffWeightValue,Graph ),
          ( rdf( TakeoffWeightNode, aixm:uom, TakeoffWeightUOM, Graph ); rdf( TakeoffWeightNode, fixm:uom, TakeoffWeightUOM, Graph ); rdf( TakeoffWeightNode, plain:uom, TakeoffWeightUOM, Graph ) ),
          TakeoffWeight=xval(TakeoffWeightValue,TakeoffWeightUOM)
        );
        (
          rdf( TakeoffWeightNode,aixm:nilReason, TakeoffWeightNilReason, Graph ),
          TakeoffWeight=nil(TakeoffWeightNilReason)
        );
        (
          rdf( TakeoffWeightNode,gml:indeterminatePosition, TakeoffWeightIndeterminate, Graph ),
          TakeoffWeight=indeterminate(TakeoffWeightIndeterminate)
        )
      )
  )
  ,( ( DepartureTimes='$null$',
    \+ rdf( FlightDeparture,fixm:'departureTimes', _DepartureTimes, Graph  )
   ; rdf(FlightDeparture,fixm:'departureTimes', DepartureTimes, Graph ) )
  ) .

fixm_AerodromeReference(Graph, AerodromeReference) :-
  subClassOf(T,fixm:'AerodromeReference')
  ,rdf(AerodromeReference,rdf:type,T,Graph) .

fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime, TaxiTime) :-
  rdf(EfplFlightDeparture,rdf:type,fixm:'EfplFlightDeparture',Graph)
  ,(
    ( EstimatedOffBlockTime='$null$',
      \+ rdf( EfplFlightDeparture,fixm:'estimatedOffBlockTime',_EstimatedOffBlockTime,Graph )
    );
  ( rdf( EfplFlightDeparture,fixm:'estimatedOffBlockTime',EstimatedOffBlockTimeNode,Graph )),
      (
        (
          rdf(EstimatedOffBlockTimeNode,rdf:value,EstimatedOffBlockTimeValue,Graph),
         \+ ( rdf( EstimatedOffBlockTimeNode, aixm:uom, _EstimatedOffBlockTimeUOM, Graph ); rdf( EstimatedOffBlockTimeNode, fixm:uom, _EstimatedOffBlockTimeUOM, Graph ); rdf( EstimatedOffBlockTimeNode, plain:uom, _EstimatedOffBlockTimeUOM, Graph ) ),
          EstimatedOffBlockTime=val(EstimatedOffBlockTimeValue)
        );
        (
          rdf( EstimatedOffBlockTimeNode,rdf:value,EstimatedOffBlockTimeValue,Graph ),
          ( rdf( EstimatedOffBlockTimeNode, aixm:uom, EstimatedOffBlockTimeUOM, Graph ); rdf( EstimatedOffBlockTimeNode, fixm:uom, EstimatedOffBlockTimeUOM, Graph ); rdf( EstimatedOffBlockTimeNode, plain:uom, EstimatedOffBlockTimeUOM, Graph ) ),
          EstimatedOffBlockTime=xval(EstimatedOffBlockTimeValue,EstimatedOffBlockTimeUOM)
        );
        (
          rdf( EstimatedOffBlockTimeNode,aixm:nilReason, EstimatedOffBlockTimeNilReason, Graph ),
          EstimatedOffBlockTime=nil(EstimatedOffBlockTimeNilReason)
        );
        (
          rdf( EstimatedOffBlockTimeNode,gml:indeterminatePosition, EstimatedOffBlockTimeIndeterminate, Graph ),
          EstimatedOffBlockTime=indeterminate(EstimatedOffBlockTimeIndeterminate)
        )
      )
  )
  ,(
    ( TaxiTime='$null$',
      \+ rdf( EfplFlightDeparture,fixm:'taxiTime',_TaxiTime,Graph )
    );
  ( rdf( EfplFlightDeparture,fixm:'taxiTime',TaxiTimeNode,Graph )),
      (
        (
          rdf(TaxiTimeNode,rdf:value,TaxiTimeValue,Graph),
         \+ ( rdf( TaxiTimeNode, aixm:uom, _TaxiTimeUOM, Graph ); rdf( TaxiTimeNode, fixm:uom, _TaxiTimeUOM, Graph ); rdf( TaxiTimeNode, plain:uom, _TaxiTimeUOM, Graph ) ),
          TaxiTime=val(TaxiTimeValue)
        );
        (
          rdf( TaxiTimeNode,rdf:value,TaxiTimeValue,Graph ),
          ( rdf( TaxiTimeNode, aixm:uom, TaxiTimeUOM, Graph ); rdf( TaxiTimeNode, fixm:uom, TaxiTimeUOM, Graph ); rdf( TaxiTimeNode, plain:uom, TaxiTimeUOM, Graph ) ),
          TaxiTime=xval(TaxiTimeValue,TaxiTimeUOM)
        );
        (
          rdf( TaxiTimeNode,aixm:nilReason, TaxiTimeNilReason, Graph ),
          TaxiTime=nil(TaxiTimeNilReason)
        );
        (
          rdf( TaxiTimeNode,gml:indeterminatePosition, TaxiTimeIndeterminate, Graph ),
          TaxiTime=indeterminate(TaxiTimeIndeterminate)
        )
      )
  ) .

aixm_UsageCondition(Graph, UsageCondition, Type, PriorPermission, Selection, Annotation, Contact) :-
  subClassOf(T,aixm:'UsageCondition')
  ,rdf(UsageCondition,rdf:type,T,Graph)
  ,(
    ( Type='$null$',
      \+ rdf( UsageCondition,aixm:'type',_Type,Graph )
    );
  ( rdf( UsageCondition,aixm:'type',TypeNode,Graph )),
      (
        (
          rdf(TypeNode,rdf:value,TypeValue,Graph),
         \+ ( rdf( TypeNode, aixm:uom, _TypeUOM, Graph ); rdf( TypeNode, fixm:uom, _TypeUOM, Graph ); rdf( TypeNode, plain:uom, _TypeUOM, Graph ) ),
          Type=val(TypeValue)
        );
        (
          rdf( TypeNode,rdf:value,TypeValue,Graph ),
          ( rdf( TypeNode, aixm:uom, TypeUOM, Graph ); rdf( TypeNode, fixm:uom, TypeUOM, Graph ); rdf( TypeNode, plain:uom, TypeUOM, Graph ) ),
          Type=xval(TypeValue,TypeUOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, TypeNilReason, Graph ),
          Type=nil(TypeNilReason)
        );
        (
          rdf( TypeNode,gml:indeterminatePosition, TypeIndeterminate, Graph ),
          Type=indeterminate(TypeIndeterminate)
        )
      )
  )
  ,(
    ( PriorPermission='$null$',
      \+ rdf( UsageCondition,aixm:'priorPermission',_PriorPermission,Graph )
    );
  ( rdf( UsageCondition,aixm:'priorPermission',PriorPermissionNode,Graph )),
      (
        (
          rdf(PriorPermissionNode,rdf:value,PriorPermissionValue,Graph),
         \+ ( rdf( PriorPermissionNode, aixm:uom, _PriorPermissionUOM, Graph ); rdf( PriorPermissionNode, fixm:uom, _PriorPermissionUOM, Graph ); rdf( PriorPermissionNode, plain:uom, _PriorPermissionUOM, Graph ) ),
          PriorPermission=val(PriorPermissionValue)
        );
        (
          rdf( PriorPermissionNode,rdf:value,PriorPermissionValue,Graph ),
          ( rdf( PriorPermissionNode, aixm:uom, PriorPermissionUOM, Graph ); rdf( PriorPermissionNode, fixm:uom, PriorPermissionUOM, Graph ); rdf( PriorPermissionNode, plain:uom, PriorPermissionUOM, Graph ) ),
          PriorPermission=xval(PriorPermissionValue,PriorPermissionUOM)
        );
        (
          rdf( PriorPermissionNode,aixm:nilReason, PriorPermissionNilReason, Graph ),
          PriorPermission=nil(PriorPermissionNilReason)
        );
        (
          rdf( PriorPermissionNode,gml:indeterminatePosition, PriorPermissionIndeterminate, Graph ),
          PriorPermission=indeterminate(PriorPermissionIndeterminate)
        )
      )
  )
  ,( ( Selection='$null$',
    \+ rdf( UsageCondition,aixm:'selection', _Selection, Graph  )
   ; rdf(UsageCondition,aixm:'selection', Selection, Graph ) )
  )
  ,findall(A, rdf(UsageCondition,aixm:'annotation',A,Graph), Annotation)
  ,findall(A, rdf(UsageCondition,aixm:'contact',A,Graph), Contact) .

fixm_ExpandedRoutePoint_Combined(Graph, ExpandedRoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit, EstimatedLevel, EstimatedTime, Constraint) :-
  fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel, EstimatedTime, Constraint),
  fixm_AbstractRoutePoint(Graph, ExpandedRoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit) .

aixm_ElevatedSurface_Combined(Graph, ElevatedSurface, Patch, HorizontalAccuracy, Annotation, HorizontalAccuracy, Annotation, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy),
  aixm_Surface_Combined(Graph,ElevatedSurface, Patch, HorizontalAccuracy, Annotation) .

aixm_ConditionCombination_Combined(Graph, ConditionCombination, Annotation, SpecialDateAuthority, TimeInterval, LogicalOperator, Flight, Aircraft, Weather, SubCondition) :-
  aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator, Flight, Aircraft, Weather, SubCondition),
  aixm_PropertiesWithSchedule(Graph, ConditionCombination, Annotation, SpecialDateAuthority, TimeInterval) .

aixm_ElevatedPoint_Combined(Graph, ElevatedPoint, HorizontalAccuracy, Annotation, HorizontalAccuracy, Annotation, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy),
  aixm_Point_Combined(Graph,ElevatedPoint, HorizontalAccuracy, Annotation) .

fixm_EfplPoint4D_Combined(Graph, EfplPoint4D, PosList, SrsName, Altitude, Time, PointRange, Altitude, Time, PointRange, FlightLevel) :-
  fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel),
  fixm_Point4D_Combined(Graph,EfplPoint4D, PosList, SrsName, Altitude, Time, PointRange) .

fixm_EfplTrajectoryRoutePair_Combined(Graph, EfplTrajectoryRoutePair, Trajectory, Route) :-
  fixm_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair),
  fixm_TrajectoryRoutePair(Graph, EfplTrajectoryRoutePair, Trajectory, Route) .

fixm_RoutePoint_Combined(Graph, RoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit, Constraint) :-
  fixm_RoutePoint(Graph, RoutePoint, Constraint),
  fixm_AbstractRoutePoint(Graph, RoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit) .

aixm_AirportHeliportResponsibilityOrganisation_Combined(Graph, AirportHeliportResponsibilityOrganisation, Annotation, SpecialDateAuthority, TimeInterval, Role, TheOrganisationAuthority) :-
  aixm_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role, TheOrganisationAuthority),
  aixm_PropertiesWithSchedule(Graph, AirportHeliportResponsibilityOrganisation, Annotation, SpecialDateAuthority, TimeInterval) .

fixm_DangerousGoods_Combined(Graph, DangerousGoods, Provenance, GuidebookNumber, OnboardLocation, HandlingInformation, AircraftLimitation, AirWayBill, Shipment, PackageGroup, ShippingInformation) :-
  fixm_DangerousGoods(Graph, DangerousGoods, GuidebookNumber, OnboardLocation, HandlingInformation, AircraftLimitation, AirWayBill, Shipment, PackageGroup, ShippingInformation),
  fixm_Feature(Graph, DangerousGoods, Provenance) .

fixm_Point4D_Combined(Graph, Point4D, PosList, SrsName, Altitude, Time, PointRange) :-
  fixm_Point4D(Graph, Point4D, Altitude, Time, PointRange),
  fixm_GeographicLocation(Graph, Point4D, PosList, SrsName) .

fixm_FlightEmergency_Combined(Graph, FlightEmergency, Provenance, ActionTaken, EmergencyDescription, Originator, OtherInformation, Phase, Contact) :-
  fixm_FlightEmergency(Graph, FlightEmergency, ActionTaken, EmergencyDescription, Originator, OtherInformation, Phase, Contact),
  fixm_Feature(Graph, FlightEmergency, Provenance) .

fixm_Flight_Combined(Graph, Flight, Provenance, ControllingUnit, Extensions, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoods, RankedTrajectories, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandling) :-
  fixm_Flight(Graph, Flight, ControllingUnit, Extensions, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoods, RankedTrajectories, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandling),
  fixm_Feature(Graph, Flight, Provenance) .

gml_Surface_Combined(Graph, Surface, Patch) :-
  gml_Surface(Graph, Surface, Patch),
  gml_SurfacePatch(Graph, Surface) .

fixm_UnitBoundary_Combined(Graph, UnitBoundary, SectorIdentifier, Delegated, DownstreamUnit, UpstreamUnit, BoundaryCrossingProposed, BoundaryCrossingCoordinated, Handoff, UnitBoundaryIndicator) :-
  fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit, UpstreamUnit, BoundaryCrossingProposed, BoundaryCrossingCoordinated, Handoff, UnitBoundaryIndicator),
  fixm_AtcUnitReference(Graph, UnitBoundary, SectorIdentifier, Delegated) .

aixm_AirportHeliportContamination_Combined(Graph, AirportHeliportContamination, ObservationTime, Depth, FrictionCoefficient, FrictionEstimation, FrictionDevice, ObscuredLights, FurtherClearanceTime, FurtherTotalClearance, NextObservationTime, Proportion, CriticalRidge, Annotation, Layer) :-
  aixm_AirportHeliportContamination(Graph, AirportHeliportContamination),
  aixm_SurfaceContamination(Graph, AirportHeliportContamination, ObservationTime, Depth, FrictionCoefficient, FrictionEstimation, FrictionDevice, ObscuredLights, FurtherClearanceTime, FurtherTotalClearance, NextObservationTime, Proportion, CriticalRidge, Annotation, Layer) .

aixm_TelephoneContact_Combined(Graph, TelephoneContact, Annotation, SpecialDateAuthority, TimeInterval, Voice, Facsimile) :-
  aixm_TelephoneContact(Graph, TelephoneContact, Voice, Facsimile),
  aixm_PropertiesWithSchedule(Graph, TelephoneContact, Annotation, SpecialDateAuthority, TimeInterval) .

fixm_Route_Combined(Graph, Route, Provenance, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTime, ExpandedRoute, ClimbSchedule, DescentSchedule, Segment) :-
  fixm_Route(Graph, Route, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTime, ExpandedRoute, ClimbSchedule, DescentSchedule, Segment),
  fixm_Feature(Graph, Route, Provenance) .

fixm_EfplFlight_Combined(Graph, EfplFlight, Provenance, ControllingUnit, Extensions, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoods, RankedTrajectories, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandling, ControllingUnit, Extensions, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoods, RankedTrajectories, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandling, IfplId, TotalEstimatedElapsedTime, AerodromesOfDestination, EfplSpecialHandling, EfplFiledTrajectory, EfplAcceptedTrajectory, OtherInformation, FlightPerformanceData) :-
  fixm_EfplFlight(Graph, EfplFlight, IfplId, TotalEstimatedElapsedTime, AerodromesOfDestination, EfplSpecialHandling, EfplFiledTrajectory, EfplAcceptedTrajectory, OtherInformation, FlightPerformanceData),
  fixm_Flight_Combined(Graph,EfplFlight, Provenance, ControllingUnit, Extensions, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoods, RankedTrajectories, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandling) .

fixm_FlightStatus_Combined(Graph, FlightStatus, Provenance, AirborneHold, Airfile, Accepted, FlightCycle, MissedApproach, Suspended) :-
  fixm_FlightStatus(Graph, FlightStatus, AirborneHold, Airfile, Accepted, FlightCycle, MissedApproach, Suspended),
  fixm_Feature(Graph, FlightStatus, Provenance) .

fixm_IdentifiedUnitReference_Combined(Graph, IdentifiedUnitReference, SectorIdentifier, Delegated, UnitIdentifier) :-
  fixm_IdentifiedUnitReference(Graph, IdentifiedUnitReference, UnitIdentifier),
  fixm_AtcUnitReference(Graph, IdentifiedUnitReference, SectorIdentifier, Delegated) .

aixm_OnlineContact_Combined(Graph, OnlineContact, Annotation, SpecialDateAuthority, TimeInterval, Network, Linkage, Protocol, EMail) :-
  aixm_OnlineContact(Graph, OnlineContact, Network, Linkage, Protocol, EMail),
  aixm_PropertiesWithSchedule(Graph, OnlineContact, Annotation, SpecialDateAuthority, TimeInterval) .

fixm_StructuredPostalAddress_Combined(Graph, StructuredPostalAddress, Name, Title, OnlineContact, PhoneFax, Address) :-
  fixm_StructuredPostalAddress(Graph, StructuredPostalAddress),
  fixm_ContactInformation(Graph, StructuredPostalAddress, Name, Title, OnlineContact, PhoneFax, Address) .

fixm_AircraftPosition_Combined(Graph, AircraftPosition, Provenance, Altitude, Position, PositionTime, Track, ActualSpeed, NextPosition, ReportSource, FollowingPosition) :-
  fixm_AircraftPosition(Graph, AircraftPosition, Altitude, Position, PositionTime, Track, ActualSpeed, NextPosition, ReportSource, FollowingPosition),
  fixm_Feature(Graph, AircraftPosition, Provenance) .

aixm_AirportHeliportUsage_Combined(Graph, AirportHeliportUsage, Type, PriorPermission, Selection, Annotation, Contact, Operation) :-
  aixm_AirportHeliportUsage(Graph, AirportHeliportUsage, Operation),
  aixm_UsageCondition(Graph, AirportHeliportUsage, Type, PriorPermission, Selection, Annotation, Contact) .

fixm_EfplTrajectoryPoint_Combined(Graph, EfplTrajectoryPoint, AltimeterSetting, PredictedAirspeed, PredictedGroundspeed, MetData, Point, TrajectoryChange, TrajectoryChangeType, ReferencePoint, AerodromeIdentifier, DistanceFromTakeOff, EfplEstimatedSpeed, ElapsedTime, GrossWeight, TrajectoryPointType, TrajectoryPointRole, InboundSegment) :-
  fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier, DistanceFromTakeOff, EfplEstimatedSpeed, ElapsedTime, GrossWeight, TrajectoryPointType, TrajectoryPointRole, InboundSegment),
  fixm_TrajectoryPoint(Graph, EfplTrajectoryPoint, AltimeterSetting, PredictedAirspeed, PredictedGroundspeed, MetData, Point, TrajectoryChange, TrajectoryChangeType, ReferencePoint) .

fixm_LastContact_Combined(Graph, LastContact, Provenance, ContactFrequency, LastContactTime, LastContactUnit, Position) :-
  fixm_LastContact(Graph, LastContact, ContactFrequency, LastContactTime, LastContactUnit, Position),
  fixm_Feature(Graph, LastContact, Provenance) .

aixm_Surface_Combined(Graph, Surface, Patch, Patch, HorizontalAccuracy, Annotation) :-
  aixm_Surface(Graph, Surface, HorizontalAccuracy, Annotation),
  gml_Surface_Combined(Graph,Surface, Patch) .

fixm_EnRoute_Combined(Graph, EnRoute, Provenance, AlternateAerodrome, FleetPrioritization, BoundaryCrossings, CpdlcConnection, BeaconCodeAssignment, Cleared, ControlElement, Pointout, Position) :-
  fixm_EnRoute(Graph, EnRoute, AlternateAerodrome, FleetPrioritization, BoundaryCrossings, CpdlcConnection, BeaconCodeAssignment, Cleared, ControlElement, Pointout, Position),
  fixm_Feature(Graph, EnRoute, Provenance) .

fixm_Aircraft_Combined(Graph, Aircraft, Provenance, AircraftColours, AircraftQuantity, EngineType, AircraftAddress, Capabilities, Registration, AircraftType, WakeTurbulence, AircraftPerformance) :-
  fixm_Aircraft(Graph, Aircraft, AircraftColours, AircraftQuantity, EngineType, AircraftAddress, Capabilities, Registration, AircraftType, WakeTurbulence, AircraftPerformance),
  fixm_Feature(Graph, Aircraft, Provenance) .

fixm_Extension_Combined(Graph, Extension, Provenance) :-
  fixm_Extension(Graph, Extension),
  fixm_Feature(Graph, Extension, Provenance) .

aixm_Point_Combined(Graph, Point, HorizontalAccuracy, Annotation) :-
  aixm_Point(Graph, Point, HorizontalAccuracy, Annotation),
  gml_Point(Graph, Point) .

aixm_PostalAddress_Combined(Graph, PostalAddress, Annotation, SpecialDateAuthority, TimeInterval, DeliveryPoint, City, AdministrativeArea, PostalCode, Country) :-
  aixm_PostalAddress(Graph, PostalAddress, DeliveryPoint, City, AdministrativeArea, PostalCode, Country),
  aixm_PropertiesWithSchedule(Graph, PostalAddress, Annotation, SpecialDateAuthority, TimeInterval) .

aixm_AltimeterSourceStatus_Combined(Graph, AltimeterSourceStatus, Annotation, SpecialDateAuthority, TimeInterval, OperationalStatus) :-
  aixm_AltimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus),
  aixm_PropertiesWithSchedule(Graph, AltimeterSourceStatus, Annotation, SpecialDateAuthority, TimeInterval) .

fixm_EfplRoute_Combined(Graph, EfplRoute, Provenance, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTime, ExpandedRoute, ClimbSchedule, DescentSchedule, Segment, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTime, ExpandedRoute, ClimbSchedule, DescentSchedule, Segment, EfplFlightRules) :-
  fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules),
  fixm_Route_Combined(Graph,EfplRoute, Provenance, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTime, ExpandedRoute, ClimbSchedule, DescentSchedule, Segment) .

fixm_IcaoAerodromeReference_Combined(Graph, IcaoAerodromeReference, Code) :-
  fixm_IcaoAerodromeReference(Graph, IcaoAerodromeReference, Code),
  fixm_AerodromeReference(Graph, IcaoAerodromeReference) .

fixm_RadioCommunicationFailure_Combined(Graph, RadioCommunicationFailure, Provenance, RadioFailureRemarks, RemainingComCapability, Contact) :-
  fixm_RadioCommunicationFailure(Graph, RadioCommunicationFailure, RadioFailureRemarks, RemainingComCapability, Contact),
  fixm_Feature(Graph, RadioCommunicationFailure, Provenance) .

aixm_AirportHeliportAvailability_Combined(Graph, AirportHeliportAvailability, Annotation, SpecialDateAuthority, TimeInterval, OperationalStatus, Warning, Usage) :-
  aixm_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus, Warning, Usage),
  aixm_PropertiesWithSchedule(Graph, AirportHeliportAvailability, Annotation, SpecialDateAuthority, TimeInterval) .

fixm_FlightArrival_Combined(Graph, FlightArrival, Provenance, ApproachFix, ApproachTime, ArrivalAerodrome, ArrivalAerodromeAlternate, ArrivalAerodromeOriginal, ArrivalFix, ArrivalFixTime, ArrivalFleetPrioritization, ArrivalSequenceNumber, EarliestInBlockTime, FiledRevisedDestinationAerodrome, FiledRevisedDestinationStar, RunwayPositionAndTime, StandardInstrumentArrival, StandPositionAndTime, LandingLimits) :-
  fixm_FlightArrival(Graph, FlightArrival, ApproachFix, ApproachTime, ArrivalAerodrome, ArrivalAerodromeAlternate, ArrivalAerodromeOriginal, ArrivalFix, ArrivalFixTime, ArrivalFleetPrioritization, ArrivalSequenceNumber, EarliestInBlockTime, FiledRevisedDestinationAerodrome, FiledRevisedDestinationStar, RunwayPositionAndTime, StandardInstrumentArrival, StandPositionAndTime, LandingLimits),
  fixm_Feature(Graph, FlightArrival, Provenance) .

fixm_ExtendedMultiTime_Combined(Graph, ExtendedMultiTime, Actual, Estimated, Target, Target, Controlled, Initial) :-
  fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled, Initial),
  fixm_TargetMultiTime_Combined(Graph,ExtendedMultiTime, Actual, Estimated, Target) .

fixm_TargetMultiTime_Combined(Graph, TargetMultiTime, Actual, Estimated, Target) :-
  fixm_TargetMultiTime(Graph, TargetMultiTime, Target),
  fixm_MultiTime(Graph, TargetMultiTime, Actual, Estimated) .

fixm_FlightDeparture_Combined(Graph, FlightDeparture, Provenance, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes) :-
  fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes),
  fixm_Feature(Graph, FlightDeparture, Provenance) .

fixm_EfplFlightDeparture_Combined(Graph, EfplFlightDeparture, Provenance, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes, EstimatedOffBlockTime, TaxiTime) :-
  fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime, TaxiTime),
  fixm_FlightDeparture_Combined(Graph,EfplFlightDeparture, Provenance, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes) .

