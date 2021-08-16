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
          ( rdf( AdministrativeAreaNode, aixm:uom, UOM, Graph ); rdf( AdministrativeAreaNode, fixm:uom, UOM, Graph ); rdf( AdministrativeAreaNode, plain:uom, UOM, Graph ) ),
          AdministrativeArea=xval(AdministrativeAreaValue,UOM)
        );
        (
          rdf( AdministrativeAreaNode,aixm:nilReason, NilReason, Graph ),
          AdministrativeArea=nil(NilReason)
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
          ( rdf( PostalCodeNode, aixm:uom, UOM, Graph ); rdf( PostalCodeNode, fixm:uom, UOM, Graph ); rdf( PostalCodeNode, plain:uom, UOM, Graph ) ),
          PostalCode=xval(PostalCodeValue,UOM)
        );
        (
          rdf( PostalCodeNode,aixm:nilReason, NilReason, Graph ),
          PostalCode=nil(NilReason)
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
          ( rdf( DeliveryPointNode, aixm:uom, UOM, Graph ); rdf( DeliveryPointNode, fixm:uom, UOM, Graph ); rdf( DeliveryPointNode, plain:uom, UOM, Graph ) ),
          DeliveryPoint=xval(DeliveryPointValue,UOM)
        );
        (
          rdf( DeliveryPointNode,aixm:nilReason, NilReason, Graph ),
          DeliveryPoint=nil(NilReason)
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
          ( rdf( CountryCodeNode, aixm:uom, UOM, Graph ); rdf( CountryCodeNode, fixm:uom, UOM, Graph ); rdf( CountryCodeNode, plain:uom, UOM, Graph ) ),
          CountryCode=xval(CountryCodeValue,UOM)
        );
        (
          rdf( CountryCodeNode,aixm:nilReason, NilReason, Graph ),
          CountryCode=nil(NilReason)
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
          ( rdf( CountryNameNode, aixm:uom, UOM, Graph ); rdf( CountryNameNode, fixm:uom, UOM, Graph ); rdf( CountryNameNode, plain:uom, UOM, Graph ) ),
          CountryName=xval(CountryNameValue,UOM)
        );
        (
          rdf( CountryNameNode,aixm:nilReason, NilReason, Graph ),
          CountryName=nil(NilReason)
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
          ( rdf( CityNode, aixm:uom, UOM, Graph ); rdf( CityNode, fixm:uom, UOM, Graph ); rdf( CityNode, plain:uom, UOM, Graph ) ),
          City=xval(CityValue,UOM)
        );
        (
          rdf( CityNode,aixm:nilReason, NilReason, Graph ),
          City=nil(NilReason)
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
          ( rdf( OtherNavigationCapabilitiesNode, aixm:uom, UOM, Graph ); rdf( OtherNavigationCapabilitiesNode, fixm:uom, UOM, Graph ); rdf( OtherNavigationCapabilitiesNode, plain:uom, UOM, Graph ) ),
          OtherNavigationCapabilities=xval(OtherNavigationCapabilitiesValue,UOM)
        );
        (
          rdf( OtherNavigationCapabilitiesNode,aixm:nilReason, NilReason, Graph ),
          OtherNavigationCapabilities=nil(NilReason)
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
          ( rdf( LowerSpeedNode, aixm:uom, UOM, Graph ); rdf( LowerSpeedNode, fixm:uom, UOM, Graph ); rdf( LowerSpeedNode, plain:uom, UOM, Graph ) ),
          LowerSpeed=xval(LowerSpeedValue,UOM)
        );
        (
          rdf( LowerSpeedNode,aixm:nilReason, NilReason, Graph ),
          LowerSpeed=nil(NilReason)
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
          ( rdf( UpperSpeedNode, aixm:uom, UOM, Graph ); rdf( UpperSpeedNode, fixm:uom, UOM, Graph ); rdf( UpperSpeedNode, plain:uom, UOM, Graph ) ),
          UpperSpeed=xval(UpperSpeedValue,UOM)
        );
        (
          rdf( UpperSpeedNode,aixm:nilReason, NilReason, Graph ),
          UpperSpeed=nil(NilReason)
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
          ( rdf( PropertyNameNode, aixm:uom, UOM, Graph ); rdf( PropertyNameNode, fixm:uom, UOM, Graph ); rdf( PropertyNameNode, plain:uom, UOM, Graph ) ),
          PropertyName=xval(PropertyNameValue,UOM)
        );
        (
          rdf( PropertyNameNode,aixm:nilReason, NilReason, Graph ),
          PropertyName=nil(NilReason)
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
          ( rdf( PurposeNode, aixm:uom, UOM, Graph ); rdf( PurposeNode, fixm:uom, UOM, Graph ); rdf( PurposeNode, plain:uom, UOM, Graph ) ),
          Purpose=xval(PurposeValue,UOM)
        );
        (
          rdf( PurposeNode,aixm:nilReason, NilReason, Graph ),
          Purpose=nil(NilReason)
        )
      )
  )
  ,( TranslatedNote='$null$',
    \+ rdf( Note,aixm:'translatedNote', _TranslatedNote, Graph )
  ) .

fixm_Pointout(Graph, Pointout, OriginatingUnit, ReceivingUnit) :-
  rdf(Pointout,rdf:type,fixm:'Pointout',Graph)
  ,( OriginatingUnit='$null$',
    \+ rdf( Pointout,fixm:'originatingUnit', _OriginatingUnit, Graph )
  )
  ,( ReceivingUnit='$null$',
    \+ rdf( Pointout,fixm:'receivingUnit', _ReceivingUnit, Graph )
  ) .

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
          ( rdf( LowerBoundNode, aixm:uom, UOM, Graph ); rdf( LowerBoundNode, fixm:uom, UOM, Graph ); rdf( LowerBoundNode, plain:uom, UOM, Graph ) ),
          LowerBound=xval(LowerBoundValue,UOM)
        );
        (
          rdf( LowerBoundNode,aixm:nilReason, NilReason, Graph ),
          LowerBound=nil(NilReason)
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
          ( rdf( UpperBoundNode, aixm:uom, UOM, Graph ); rdf( UpperBoundNode, fixm:uom, UOM, Graph ); rdf( UpperBoundNode, plain:uom, UOM, Graph ) ),
          UpperBound=xval(UpperBoundValue,UOM)
        );
        (
          rdf( UpperBoundNode,aixm:nilReason, NilReason, Graph ),
          UpperBound=nil(NilReason)
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
          ( rdf( EstimatedLevelNode, aixm:uom, UOM, Graph ); rdf( EstimatedLevelNode, fixm:uom, UOM, Graph ); rdf( EstimatedLevelNode, plain:uom, UOM, Graph ) ),
          EstimatedLevel=xval(EstimatedLevelValue,UOM)
        );
        (
          rdf( EstimatedLevelNode,aixm:nilReason, NilReason, Graph ),
          EstimatedLevel=nil(NilReason)
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
          ( rdf( EstimatedTimeNode, aixm:uom, UOM, Graph ); rdf( EstimatedTimeNode, fixm:uom, UOM, Graph ); rdf( EstimatedTimeNode, plain:uom, UOM, Graph ) ),
          EstimatedTime=xval(EstimatedTimeValue,UOM)
        );
        (
          rdf( EstimatedTimeNode,aixm:nilReason, NilReason, Graph ),
          EstimatedTime=nil(NilReason)
        )
      )
  )
  ,( Constraint='$null$',
    \+ rdf( ExpandedRoutePoint,fixm:'constraint', _Constraint, Graph )
  ) .

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
          ( rdf( ElevationNode, aixm:uom, UOM, Graph ); rdf( ElevationNode, fixm:uom, UOM, Graph ); rdf( ElevationNode, plain:uom, UOM, Graph ) ),
          Elevation=xval(ElevationValue,UOM)
        );
        (
          rdf( ElevationNode,aixm:nilReason, NilReason, Graph ),
          Elevation=nil(NilReason)
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
          ( rdf( GeoidUndulationNode, aixm:uom, UOM, Graph ); rdf( GeoidUndulationNode, fixm:uom, UOM, Graph ); rdf( GeoidUndulationNode, plain:uom, UOM, Graph ) ),
          GeoidUndulation=xval(GeoidUndulationValue,UOM)
        );
        (
          rdf( GeoidUndulationNode,aixm:nilReason, NilReason, Graph ),
          GeoidUndulation=nil(NilReason)
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
          ( rdf( VerticalDatumNode, aixm:uom, UOM, Graph ); rdf( VerticalDatumNode, fixm:uom, UOM, Graph ); rdf( VerticalDatumNode, plain:uom, UOM, Graph ) ),
          VerticalDatum=xval(VerticalDatumValue,UOM)
        );
        (
          rdf( VerticalDatumNode,aixm:nilReason, NilReason, Graph ),
          VerticalDatum=nil(NilReason)
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
          ( rdf( VerticalAccuracyNode, aixm:uom, UOM, Graph ); rdf( VerticalAccuracyNode, fixm:uom, UOM, Graph ); rdf( VerticalAccuracyNode, plain:uom, UOM, Graph ) ),
          VerticalAccuracy=xval(VerticalAccuracyValue,UOM)
        );
        (
          rdf( VerticalAccuracyNode,aixm:nilReason, NilReason, Graph ),
          VerticalAccuracy=nil(NilReason)
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
          ( rdf( HeightNode, aixm:uom, UOM, Graph ); rdf( HeightNode, fixm:uom, UOM, Graph ); rdf( HeightNode, plain:uom, UOM, Graph ) ),
          Height=xval(HeightValue,UOM)
        );
        (
          rdf( HeightNode,aixm:nilReason, NilReason, Graph ),
          Height=nil(NilReason)
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
          ( rdf( LengthNode, aixm:uom, UOM, Graph ); rdf( LengthNode, fixm:uom, UOM, Graph ); rdf( LengthNode, plain:uom, UOM, Graph ) ),
          Length=xval(LengthValue,UOM)
        );
        (
          rdf( LengthNode,aixm:nilReason, NilReason, Graph ),
          Length=nil(NilReason)
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
          ( rdf( WidthNode, aixm:uom, UOM, Graph ); rdf( WidthNode, fixm:uom, UOM, Graph ); rdf( WidthNode, plain:uom, UOM, Graph ) ),
          Width=xval(WidthValue,UOM)
        );
        (
          rdf( WidthNode,aixm:nilReason, NilReason, Graph ),
          Width=nil(NilReason)
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
          ( rdf( StandNameNode, aixm:uom, UOM, Graph ); rdf( StandNameNode, fixm:uom, UOM, Graph ); rdf( StandNameNode, plain:uom, UOM, Graph ) ),
          StandName=xval(StandNameValue,UOM)
        );
        (
          rdf( StandNameNode,aixm:nilReason, NilReason, Graph ),
          StandName=nil(NilReason)
        )
      )
  )
  ,( StandTime='$null$',
    \+ rdf( StandPositionAndTime,fixm:'standTime', _StandTime, Graph )
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
          ( rdf( TerminalNameNode, aixm:uom, UOM, Graph ); rdf( TerminalNameNode, fixm:uom, UOM, Graph ); rdf( TerminalNameNode, plain:uom, UOM, Graph ) ),
          TerminalName=xval(TerminalNameValue,UOM)
        );
        (
          rdf( TerminalNameNode,aixm:nilReason, NilReason, Graph ),
          TerminalName=nil(NilReason)
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
          ( rdf( AirwayNode, aixm:uom, UOM, Graph ); rdf( AirwayNode, fixm:uom, UOM, Graph ); rdf( AirwayNode, plain:uom, UOM, Graph ) ),
          Airway=xval(AirwayValue,UOM)
        );
        (
          rdf( AirwayNode,aixm:nilReason, NilReason, Graph ),
          Airway=nil(NilReason)
        )
      )
  )
  ,( RoutePoint='$null$',
    \+ rdf( RouteSegment,fixm:'routePoint', _RoutePoint, Graph )
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
          ( rdf( LogicalOperatorNode, aixm:uom, UOM, Graph ); rdf( LogicalOperatorNode, fixm:uom, UOM, Graph ); rdf( LogicalOperatorNode, plain:uom, UOM, Graph ) ),
          LogicalOperator=xval(LogicalOperatorValue,UOM)
        );
        (
          rdf( LogicalOperatorNode,aixm:nilReason, NilReason, Graph ),
          LogicalOperator=nil(NilReason)
        )
      )
  )
  ,( Flight='$null$',
    \+ rdf( ConditionCombination,aixm:'flight', _Flight, Graph )
  )
  ,( Aircraft='$null$',
    \+ rdf( ConditionCombination,aixm:'aircraft', _Aircraft, Graph )
  )
  ,( Weather='$null$',
    \+ rdf( ConditionCombination,aixm:'weather', _Weather, Graph )
  )
  ,( SubCondition='$null$',
    \+ rdf( ConditionCombination,aixm:'subCondition', _SubCondition, Graph )
  ) .

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
          ( rdf( LayerOrderNode, aixm:uom, UOM, Graph ); rdf( LayerOrderNode, fixm:uom, UOM, Graph ); rdf( LayerOrderNode, plain:uom, UOM, Graph ) ),
          LayerOrder=xval(LayerOrderValue,UOM)
        );
        (
          rdf( LayerOrderNode,aixm:nilReason, NilReason, Graph ),
          LayerOrder=nil(NilReason)
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
          ( rdf( TypeNode, aixm:uom, UOM, Graph ); rdf( TypeNode, fixm:uom, UOM, Graph ); rdf( TypeNode, plain:uom, UOM, Graph ) ),
          Type=xval(TypeValue,UOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, NilReason, Graph ),
          Type=nil(NilReason)
        )
      )
  )
  ,( Extent='$null$',
    \+ rdf( SurfaceContaminationLayer,aixm:'extent', _Extent, Graph )
  )
  ,( Annotation='$null$',
    \+ rdf( SurfaceContaminationLayer,aixm:'annotation', _Annotation, Graph )
  ) .

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
          ( rdf( NameNode, aixm:uom, UOM, Graph ); rdf( NameNode, fixm:uom, UOM, Graph ); rdf( NameNode, plain:uom, UOM, Graph ) ),
          Name=xval(NameValue,UOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NilReason, Graph ),
          Name=nil(NilReason)
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
          ( rdf( OtherOrganizationNode, aixm:uom, UOM, Graph ); rdf( OtherOrganizationNode, fixm:uom, UOM, Graph ); rdf( OtherOrganizationNode, plain:uom, UOM, Graph ) ),
          OtherOrganization=xval(OtherOrganizationValue,UOM)
        );
        (
          rdf( OtherOrganizationNode,aixm:nilReason, NilReason, Graph ),
          OtherOrganization=nil(NilReason)
        )
      )
  )
  ,( Contact='$null$',
    \+ rdf( Organization,fixm:'contact', _Contact, Graph )
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
          ( rdf( TypeNode, aixm:uom, UOM, Graph ); rdf( TypeNode, fixm:uom, UOM, Graph ); rdf( TypeNode, plain:uom, UOM, Graph ) ),
          Type=xval(TypeValue,UOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, NilReason, Graph ),
          Type=nil(NilReason)
        )
      )
  )
  ,( Annotation='$null$',
    \+ rdf( OrganisationAuthorityAssociation,aixm:'annotation', _Annotation, Graph )
  )
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
          ( rdf( ElevationNode, aixm:uom, UOM, Graph ); rdf( ElevationNode, fixm:uom, UOM, Graph ); rdf( ElevationNode, plain:uom, UOM, Graph ) ),
          Elevation=xval(ElevationValue,UOM)
        );
        (
          rdf( ElevationNode,aixm:nilReason, NilReason, Graph ),
          Elevation=nil(NilReason)
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
          ( rdf( GeoidUndulationNode, aixm:uom, UOM, Graph ); rdf( GeoidUndulationNode, fixm:uom, UOM, Graph ); rdf( GeoidUndulationNode, plain:uom, UOM, Graph ) ),
          GeoidUndulation=xval(GeoidUndulationValue,UOM)
        );
        (
          rdf( GeoidUndulationNode,aixm:nilReason, NilReason, Graph ),
          GeoidUndulation=nil(NilReason)
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
          ( rdf( VerticalDatumNode, aixm:uom, UOM, Graph ); rdf( VerticalDatumNode, fixm:uom, UOM, Graph ); rdf( VerticalDatumNode, plain:uom, UOM, Graph ) ),
          VerticalDatum=xval(VerticalDatumValue,UOM)
        );
        (
          rdf( VerticalDatumNode,aixm:nilReason, NilReason, Graph ),
          VerticalDatum=nil(NilReason)
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
          ( rdf( VerticalAccuracyNode, aixm:uom, UOM, Graph ); rdf( VerticalAccuracyNode, fixm:uom, UOM, Graph ); rdf( VerticalAccuracyNode, plain:uom, UOM, Graph ) ),
          VerticalAccuracy=xval(VerticalAccuracyValue,UOM)
        );
        (
          rdf( VerticalAccuracyNode,aixm:nilReason, NilReason, Graph ),
          VerticalAccuracy=nil(NilReason)
        )
      )
  ) .

fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel) :-
  rdf(EfplPoint4D,rdf:type,fixm:'EfplPoint4D',Graph)
  ,( FlightLevel='$null$',
    \+ rdf( EfplPoint4D,fixm:'flightLevel', _FlightLevel, Graph )
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
          ( rdf( OperatingOrganizationNode, aixm:uom, UOM, Graph ); rdf( OperatingOrganizationNode, fixm:uom, UOM, Graph ); rdf( OperatingOrganizationNode, plain:uom, UOM, Graph ) ),
          OperatingOrganization=xval(OperatingOrganizationValue,UOM)
        );
        (
          rdf( OperatingOrganizationNode,aixm:nilReason, NilReason, Graph ),
          OperatingOrganization=nil(NilReason)
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
          ( rdf( OperatorCategoryNode, aixm:uom, UOM, Graph ); rdf( OperatorCategoryNode, fixm:uom, UOM, Graph ); rdf( OperatorCategoryNode, plain:uom, UOM, Graph ) ),
          OperatorCategory=xval(OperatorCategoryValue,UOM)
        );
        (
          rdf( OperatorCategoryNode,aixm:nilReason, NilReason, Graph ),
          OperatorCategory=nil(NilReason)
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
  ,( Constraint='$null$',
    \+ rdf( RoutePoint,fixm:'constraint', _Constraint, Graph )
  ) .

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
          ( rdf( CurrentBeaconCodeNode, aixm:uom, UOM, Graph ); rdf( CurrentBeaconCodeNode, fixm:uom, UOM, Graph ); rdf( CurrentBeaconCodeNode, plain:uom, UOM, Graph ) ),
          CurrentBeaconCode=xval(CurrentBeaconCodeValue,UOM)
        );
        (
          rdf( CurrentBeaconCodeNode,aixm:nilReason, NilReason, Graph ),
          CurrentBeaconCode=nil(NilReason)
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
          ( rdf( PreviousBeaconCodeNode, aixm:uom, UOM, Graph ); rdf( PreviousBeaconCodeNode, fixm:uom, UOM, Graph ); rdf( PreviousBeaconCodeNode, plain:uom, UOM, Graph ) ),
          PreviousBeaconCode=xval(PreviousBeaconCodeValue,UOM)
        );
        (
          rdf( PreviousBeaconCodeNode,aixm:nilReason, NilReason, Graph ),
          PreviousBeaconCode=nil(NilReason)
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
          ( rdf( ReassignedBeaconCodeNode, aixm:uom, UOM, Graph ); rdf( ReassignedBeaconCodeNode, fixm:uom, UOM, Graph ); rdf( ReassignedBeaconCodeNode, plain:uom, UOM, Graph ) ),
          ReassignedBeaconCode=xval(ReassignedBeaconCodeValue,UOM)
        );
        (
          rdf( ReassignedBeaconCodeNode,aixm:nilReason, NilReason, Graph ),
          ReassignedBeaconCode=nil(NilReason)
        )
      )
  )
  ,( ReassigningUnit='$null$',
    \+ rdf( BeaconCodeAssignment,fixm:'reassigningUnit', _ReassigningUnit, Graph )
  ) .

fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile) :-
  rdf(FlightPerformanceData,rdf:type,fixm:'FlightPerformanceData',Graph)
  ,( ClimbProfile='$null$',
    \+ rdf( FlightPerformanceData,fixm:'climbProfile', _ClimbProfile, Graph )
  )
  ,( DescentProfile='$null$',
    \+ rdf( FlightPerformanceData,fixm:'descentProfile', _DescentProfile, Graph )
  ) .

fixm_ExpandedRoute(Graph, ExpandedRoute, RoutePoint) :-
  rdf(ExpandedRoute,rdf:type,fixm:'ExpandedRoute',Graph)
  ,( RoutePoint='$null$',
    \+ rdf( ExpandedRoute,fixm:'routePoint', _RoutePoint, Graph )
  ) .

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
          ( rdf( ConstraintTypeNode, aixm:uom, UOM, Graph ); rdf( ConstraintTypeNode, fixm:uom, UOM, Graph ); rdf( ConstraintTypeNode, plain:uom, UOM, Graph ) ),
          ConstraintType=xval(ConstraintTypeValue,UOM)
        );
        (
          rdf( ConstraintTypeNode,aixm:nilReason, NilReason, Graph ),
          ConstraintType=nil(NilReason)
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
          ( rdf( ComplianceNode, aixm:uom, UOM, Graph ); rdf( ComplianceNode, fixm:uom, UOM, Graph ); rdf( ComplianceNode, plain:uom, UOM, Graph ) ),
          Compliance=xval(ComplianceValue,UOM)
        );
        (
          rdf( ComplianceNode,aixm:nilReason, NilReason, Graph ),
          Compliance=nil(NilReason)
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
          ( rdf( ConsignorNode, aixm:uom, UOM, Graph ); rdf( ConsignorNode, fixm:uom, UOM, Graph ); rdf( ConsignorNode, plain:uom, UOM, Graph ) ),
          Consignor=xval(ConsignorValue,UOM)
        );
        (
          rdf( ConsignorNode,aixm:nilReason, NilReason, Graph ),
          Consignor=nil(NilReason)
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
          ( rdf( ShipperNode, aixm:uom, UOM, Graph ); rdf( ShipperNode, fixm:uom, UOM, Graph ); rdf( ShipperNode, plain:uom, UOM, Graph ) ),
          Shipper=xval(ShipperValue,UOM)
        );
        (
          rdf( ShipperNode,aixm:nilReason, NilReason, Graph ),
          Shipper=nil(NilReason)
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
          ( rdf( ElapsedTimeNode, aixm:uom, UOM, Graph ); rdf( ElapsedTimeNode, fixm:uom, UOM, Graph ); rdf( ElapsedTimeNode, plain:uom, UOM, Graph ) ),
          ElapsedTime=xval(ElapsedTimeValue,UOM)
        );
        (
          rdf( ElapsedTimeNode,aixm:nilReason, NilReason, Graph ),
          ElapsedTime=nil(NilReason)
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
          ( rdf( LocationNode, aixm:uom, UOM, Graph ); rdf( LocationNode, fixm:uom, UOM, Graph ); rdf( LocationNode, plain:uom, UOM, Graph ) ),
          Location=xval(LocationValue,UOM)
        );
        (
          rdf( LocationNode,aixm:nilReason, NilReason, Graph ),
          Location=nil(NilReason)
        )
      )
  ) .

fixm_ReportedTime(Graph, ReportedTime, Provenance, Time) :-
  rdf(ReportedTime,rdf:type,fixm:'ReportedTime',Graph)
  ,( Provenance='$null$',
    \+ rdf( ReportedTime,fixm:'provenance', _Provenance, Graph )
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
          ( rdf( TimeNode, aixm:uom, UOM, Graph ); rdf( TimeNode, fixm:uom, UOM, Graph ); rdf( TimeNode, plain:uom, UOM, Graph ) ),
          Time=xval(TimeValue,UOM)
        );
        (
          rdf( TimeNode,aixm:nilReason, NilReason, Graph ),
          Time=nil(NilReason)
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
          ( rdf( SrsNameNode, aixm:uom, UOM, Graph ); rdf( SrsNameNode, fixm:uom, UOM, Graph ); rdf( SrsNameNode, plain:uom, UOM, Graph ) ),
          SrsName=xval(SrsNameValue,UOM)
        );
        (
          rdf( SrsNameNode,aixm:nilReason, NilReason, Graph ),
          SrsName=nil(NilReason)
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
          ( rdf( NoteNode, aixm:uom, UOM, Graph ); rdf( NoteNode, fixm:uom, UOM, Graph ); rdf( NoteNode, plain:uom, UOM, Graph ) ),
          Note=xval(NoteValue,UOM)
        );
        (
          rdf( NoteNode,aixm:nilReason, NilReason, Graph ),
          Note=nil(NilReason)
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
          ( rdf( FlightConditionsNode, aixm:uom, UOM, Graph ); rdf( FlightConditionsNode, fixm:uom, UOM, Graph ); rdf( FlightConditionsNode, plain:uom, UOM, Graph ) ),
          FlightConditions=xval(FlightConditionsValue,UOM)
        );
        (
          rdf( FlightConditionsNode,aixm:nilReason, NilReason, Graph ),
          FlightConditions=nil(NilReason)
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
          ( rdf( VisibilityNode, aixm:uom, UOM, Graph ); rdf( VisibilityNode, fixm:uom, UOM, Graph ); rdf( VisibilityNode, plain:uom, UOM, Graph ) ),
          Visibility=xval(VisibilityValue,UOM)
        );
        (
          rdf( VisibilityNode,aixm:nilReason, NilReason, Graph ),
          Visibility=nil(NilReason)
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
          ( rdf( VisibilityInterpretationNode, aixm:uom, UOM, Graph ); rdf( VisibilityInterpretationNode, fixm:uom, UOM, Graph ); rdf( VisibilityInterpretationNode, plain:uom, UOM, Graph ) ),
          VisibilityInterpretation=xval(VisibilityInterpretationValue,UOM)
        );
        (
          rdf( VisibilityInterpretationNode,aixm:nilReason, NilReason, Graph ),
          VisibilityInterpretation=nil(NilReason)
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
          ( rdf( RunwayVisualRangeNode, aixm:uom, UOM, Graph ); rdf( RunwayVisualRangeNode, fixm:uom, UOM, Graph ); rdf( RunwayVisualRangeNode, plain:uom, UOM, Graph ) ),
          RunwayVisualRange=xval(RunwayVisualRangeValue,UOM)
        );
        (
          rdf( RunwayVisualRangeNode,aixm:nilReason, NilReason, Graph ),
          RunwayVisualRange=nil(NilReason)
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
          ( rdf( RunwayVisualRangeInterpretationNode, aixm:uom, UOM, Graph ); rdf( RunwayVisualRangeInterpretationNode, fixm:uom, UOM, Graph ); rdf( RunwayVisualRangeInterpretationNode, plain:uom, UOM, Graph ) ),
          RunwayVisualRangeInterpretation=xval(RunwayVisualRangeInterpretationValue,UOM)
        );
        (
          rdf( RunwayVisualRangeInterpretationNode,aixm:nilReason, NilReason, Graph ),
          RunwayVisualRangeInterpretation=nil(NilReason)
        )
      )
  )
  ,( Annotation='$null$',
    \+ rdf( Meteorology,aixm:'annotation', _Annotation, Graph )
  ) .

fixm_PointRange(Graph, PointRange, LateralRange, VerticalRange, TemporalRange) :-
  rdf(PointRange,rdf:type,fixm:'PointRange',Graph)
  ,( LateralRange='$null$',
    \+ rdf( PointRange,fixm:'lateralRange', _LateralRange, Graph )
  )
  ,( VerticalRange='$null$',
    \+ rdf( PointRange,fixm:'verticalRange', _VerticalRange, Graph )
  )
  ,( TemporalRange='$null$',
    \+ rdf( PointRange,fixm:'temporalRange', _TemporalRange, Graph )
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
          ( rdf( NameNode, aixm:uom, UOM, Graph ); rdf( NameNode, fixm:uom, UOM, Graph ); rdf( NameNode, plain:uom, UOM, Graph ) ),
          Name=xval(NameValue,UOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NilReason, Graph ),
          Name=nil(NilReason)
        )
      )
  )
  ,( Annotation='$null$',
    \+ rdf( City,aixm:'annotation', _Annotation, Graph )
  ) .

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
          ( rdf( RoleNode, aixm:uom, UOM, Graph ); rdf( RoleNode, fixm:uom, UOM, Graph ); rdf( RoleNode, plain:uom, UOM, Graph ) ),
          Role=xval(RoleValue,UOM)
        );
        (
          rdf( RoleNode,aixm:nilReason, NilReason, Graph ),
          Role=nil(NilReason)
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
          ( rdf( LowerSpeedNode, aixm:uom, UOM, Graph ); rdf( LowerSpeedNode, fixm:uom, UOM, Graph ); rdf( LowerSpeedNode, plain:uom, UOM, Graph ) ),
          LowerSpeed=xval(LowerSpeedValue,UOM)
        );
        (
          rdf( LowerSpeedNode,aixm:nilReason, NilReason, Graph ),
          LowerSpeed=nil(NilReason)
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
          ( rdf( UpperSpeedNode, aixm:uom, UOM, Graph ); rdf( UpperSpeedNode, fixm:uom, UOM, Graph ); rdf( UpperSpeedNode, plain:uom, UOM, Graph ) ),
          UpperSpeed=xval(UpperSpeedValue,UOM)
        );
        (
          rdf( UpperSpeedNode,aixm:nilReason, NilReason, Graph ),
          UpperSpeed=nil(NilReason)
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
          ( rdf( IdentifierNode, aixm:uom, UOM, Graph ); rdf( IdentifierNode, fixm:uom, UOM, Graph ); rdf( IdentifierNode, plain:uom, UOM, Graph ) ),
          Identifier=xval(IdentifierValue,UOM)
        );
        (
          rdf( IdentifierNode,aixm:nilReason, NilReason, Graph ),
          Identifier=nil(NilReason)
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
          ( rdf( MaximumAcceptableDelayNode, aixm:uom, UOM, Graph ); rdf( MaximumAcceptableDelayNode, fixm:uom, UOM, Graph ); rdf( MaximumAcceptableDelayNode, plain:uom, UOM, Graph ) ),
          MaximumAcceptableDelay=xval(MaximumAcceptableDelayValue,UOM)
        );
        (
          rdf( MaximumAcceptableDelayNode,aixm:nilReason, NilReason, Graph ),
          MaximumAcceptableDelay=nil(NilReason)
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
          ( rdf( AssignedIndicatorNode, aixm:uom, UOM, Graph ); rdf( AssignedIndicatorNode, fixm:uom, UOM, Graph ); rdf( AssignedIndicatorNode, plain:uom, UOM, Graph ) ),
          AssignedIndicator=xval(AssignedIndicatorValue,UOM)
        );
        (
          rdf( AssignedIndicatorNode,aixm:nilReason, NilReason, Graph ),
          AssignedIndicator=nil(NilReason)
        )
      )
  )
  ,( RouteTrajectoryPair='$null$',
    \+ rdf( RankedTrajectory,fixm:'routeTrajectoryPair', _RouteTrajectoryPair, Graph )
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
          ( rdf( BottomOfClimbNode, aixm:uom, UOM, Graph ); rdf( BottomOfClimbNode, fixm:uom, UOM, Graph ); rdf( BottomOfClimbNode, plain:uom, UOM, Graph ) ),
          BottomOfClimb=xval(BottomOfClimbValue,UOM)
        );
        (
          rdf( BottomOfClimbNode,aixm:nilReason, NilReason, Graph ),
          BottomOfClimb=nil(NilReason)
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
          ( rdf( BottomOfDescentNode, aixm:uom, UOM, Graph ); rdf( BottomOfDescentNode, fixm:uom, UOM, Graph ); rdf( BottomOfDescentNode, plain:uom, UOM, Graph ) ),
          BottomOfDescent=xval(BottomOfDescentValue,UOM)
        );
        (
          rdf( BottomOfDescentNode,aixm:nilReason, NilReason, Graph ),
          BottomOfDescent=nil(NilReason)
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
          ( rdf( BoundaryPointNode, aixm:uom, UOM, Graph ); rdf( BoundaryPointNode, fixm:uom, UOM, Graph ); rdf( BoundaryPointNode, plain:uom, UOM, Graph ) ),
          BoundaryPoint=xval(BoundaryPointValue,UOM)
        );
        (
          rdf( BoundaryPointNode,aixm:nilReason, NilReason, Graph ),
          BoundaryPoint=nil(NilReason)
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
          ( rdf( FromGATToOATNode, aixm:uom, UOM, Graph ); rdf( FromGATToOATNode, fixm:uom, UOM, Graph ); rdf( FromGATToOATNode, plain:uom, UOM, Graph ) ),
          FromGATToOAT=xval(FromGATToOATValue,UOM)
        );
        (
          rdf( FromGATToOATNode,aixm:nilReason, NilReason, Graph ),
          FromGATToOAT=nil(NilReason)
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
          ( rdf( FromIFRToVFRNode, aixm:uom, UOM, Graph ); rdf( FromIFRToVFRNode, fixm:uom, UOM, Graph ); rdf( FromIFRToVFRNode, plain:uom, UOM, Graph ) ),
          FromIFRToVFR=xval(FromIFRToVFRValue,UOM)
        );
        (
          rdf( FromIFRToVFRNode,aixm:nilReason, NilReason, Graph ),
          FromIFRToVFR=nil(NilReason)
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
          ( rdf( FromOATToGatNode, aixm:uom, UOM, Graph ); rdf( FromOATToGatNode, fixm:uom, UOM, Graph ); rdf( FromOATToGatNode, plain:uom, UOM, Graph ) ),
          FromOATToGat=xval(FromOATToGatValue,UOM)
        );
        (
          rdf( FromOATToGatNode,aixm:nilReason, NilReason, Graph ),
          FromOATToGat=nil(NilReason)
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
          ( rdf( FromVFRToIFRNode, aixm:uom, UOM, Graph ); rdf( FromVFRToIFRNode, fixm:uom, UOM, Graph ); rdf( FromVFRToIFRNode, plain:uom, UOM, Graph ) ),
          FromVFRToIFR=xval(FromVFRToIFRValue,UOM)
        );
        (
          rdf( FromVFRToIFRNode,aixm:nilReason, NilReason, Graph ),
          FromVFRToIFR=nil(NilReason)
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
          ( rdf( TopOfClimbNode, aixm:uom, UOM, Graph ); rdf( TopOfClimbNode, fixm:uom, UOM, Graph ); rdf( TopOfClimbNode, plain:uom, UOM, Graph ) ),
          TopOfClimb=xval(TopOfClimbValue,UOM)
        );
        (
          rdf( TopOfClimbNode,aixm:nilReason, NilReason, Graph ),
          TopOfClimb=nil(NilReason)
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
          ( rdf( TopOfDescentNode, aixm:uom, UOM, Graph ); rdf( TopOfDescentNode, fixm:uom, UOM, Graph ); rdf( TopOfDescentNode, plain:uom, UOM, Graph ) ),
          TopOfDescent=xval(TopOfDescentValue,UOM)
        );
        (
          rdf( TopOfDescentNode,aixm:nilReason, NilReason, Graph ),
          TopOfDescent=nil(NilReason)
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
          ( rdf( OtherCommunicationCapabilitiesNode, aixm:uom, UOM, Graph ); rdf( OtherCommunicationCapabilitiesNode, fixm:uom, UOM, Graph ); rdf( OtherCommunicationCapabilitiesNode, plain:uom, UOM, Graph ) ),
          OtherCommunicationCapabilities=xval(OtherCommunicationCapabilitiesValue,UOM)
        );
        (
          rdf( OtherCommunicationCapabilitiesNode,aixm:nilReason, NilReason, Graph ),
          OtherCommunicationCapabilities=nil(NilReason)
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
          ( rdf( OtherDataLinkCapabilitiesNode, aixm:uom, UOM, Graph ); rdf( OtherDataLinkCapabilitiesNode, fixm:uom, UOM, Graph ); rdf( OtherDataLinkCapabilitiesNode, plain:uom, UOM, Graph ) ),
          OtherDataLinkCapabilities=xval(OtherDataLinkCapabilitiesValue,UOM)
        );
        (
          rdf( OtherDataLinkCapabilitiesNode,aixm:nilReason, NilReason, Graph ),
          OtherDataLinkCapabilities=nil(NilReason)
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
          ( rdf( SelectiveCallingCodeNode, aixm:uom, UOM, Graph ); rdf( SelectiveCallingCodeNode, fixm:uom, UOM, Graph ); rdf( SelectiveCallingCodeNode, plain:uom, UOM, Graph ) ),
          SelectiveCallingCode=xval(SelectiveCallingCodeValue,UOM)
        );
        (
          rdf( SelectiveCallingCodeNode,aixm:nilReason, NilReason, Graph ),
          SelectiveCallingCode=nil(NilReason)
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
          ( rdf( QuantityNode, aixm:uom, UOM, Graph ); rdf( QuantityNode, fixm:uom, UOM, Graph ); rdf( QuantityNode, plain:uom, UOM, Graph ) ),
          Quantity=xval(QuantityValue,UOM)
        );
        (
          rdf( QuantityNode,aixm:nilReason, NilReason, Graph ),
          Quantity=nil(NilReason)
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
          ( rdf( TotalCapacityNode, aixm:uom, UOM, Graph ); rdf( TotalCapacityNode, fixm:uom, UOM, Graph ); rdf( TotalCapacityNode, plain:uom, UOM, Graph ) ),
          TotalCapacity=xval(TotalCapacityValue,UOM)
        );
        (
          rdf( TotalCapacityNode,aixm:nilReason, NilReason, Graph ),
          TotalCapacity=nil(NilReason)
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
          ( rdf( CoveredNode, aixm:uom, UOM, Graph ); rdf( CoveredNode, fixm:uom, UOM, Graph ); rdf( CoveredNode, plain:uom, UOM, Graph ) ),
          Covered=xval(CoveredValue,UOM)
        );
        (
          rdf( CoveredNode,aixm:nilReason, NilReason, Graph ),
          Covered=nil(NilReason)
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
          ( rdf( ColourNode, aixm:uom, UOM, Graph ); rdf( ColourNode, fixm:uom, UOM, Graph ); rdf( ColourNode, plain:uom, UOM, Graph ) ),
          Colour=xval(ColourValue,UOM)
        );
        (
          rdf( ColourNode,aixm:nilReason, NilReason, Graph ),
          Colour=nil(NilReason)
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
          ( rdf( NameNode, aixm:uom, UOM, Graph ); rdf( NameNode, fixm:uom, UOM, Graph ); rdf( NameNode, plain:uom, UOM, Graph ) ),
          Name=xval(NameValue,UOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NilReason, Graph ),
          Name=nil(NilReason)
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
          ( rdf( TitleNode, aixm:uom, UOM, Graph ); rdf( TitleNode, fixm:uom, UOM, Graph ); rdf( TitleNode, plain:uom, UOM, Graph ) ),
          Title=xval(TitleValue,UOM)
        );
        (
          rdf( TitleNode,aixm:nilReason, NilReason, Graph ),
          Title=nil(NilReason)
        )
      )
  )
  ,( Annotation='$null$',
    \+ rdf( ContactInformation,aixm:'annotation', _Annotation, Graph )
  )
  ,( NetworkNode='$null$',
    \+ rdf( ContactInformation,aixm:'networkNode', _NetworkNode, Graph )
  )
  ,( Address='$null$',
    \+ rdf( ContactInformation,aixm:'address', _Address, Graph )
  )
  ,( PhoneFax='$null$',
    \+ rdf( ContactInformation,aixm:'phoneFax', _PhoneFax, Graph )
  ) .

fixm_PlannedReportingPosition(Graph, PlannedReportingPosition, Position, PositionAltitude, PositionEstimatedTime) :-
  rdf(PlannedReportingPosition,rdf:type,fixm:'PlannedReportingPosition',Graph)
  ,( Position='$null$',
    \+ rdf( PlannedReportingPosition,fixm:'position', _Position, Graph )
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
          ( rdf( PositionAltitudeNode, aixm:uom, UOM, Graph ); rdf( PositionAltitudeNode, fixm:uom, UOM, Graph ); rdf( PositionAltitudeNode, plain:uom, UOM, Graph ) ),
          PositionAltitude=xval(PositionAltitudeValue,UOM)
        );
        (
          rdf( PositionAltitudeNode,aixm:nilReason, NilReason, Graph ),
          PositionAltitude=nil(NilReason)
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
          ( rdf( PositionEstimatedTimeNode, aixm:uom, UOM, Graph ); rdf( PositionEstimatedTimeNode, fixm:uom, UOM, Graph ); rdf( PositionEstimatedTimeNode, plain:uom, UOM, Graph ) ),
          PositionEstimatedTime=xval(PositionEstimatedTimeValue,UOM)
        );
        (
          rdf( PositionEstimatedTimeNode,aixm:nilReason, NilReason, Graph ),
          PositionEstimatedTime=nil(NilReason)
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
          ( rdf( FuelEnduranceNode, aixm:uom, UOM, Graph ); rdf( FuelEnduranceNode, fixm:uom, UOM, Graph ); rdf( FuelEnduranceNode, plain:uom, UOM, Graph ) ),
          FuelEndurance=xval(FuelEnduranceValue,UOM)
        );
        (
          rdf( FuelEnduranceNode,aixm:nilReason, NilReason, Graph ),
          FuelEndurance=nil(NilReason)
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
          ( rdf( PersonsOnBoardNode, aixm:uom, UOM, Graph ); rdf( PersonsOnBoardNode, fixm:uom, UOM, Graph ); rdf( PersonsOnBoardNode, plain:uom, UOM, Graph ) ),
          PersonsOnBoard=xval(PersonsOnBoardValue,UOM)
        );
        (
          rdf( PersonsOnBoardNode,aixm:nilReason, NilReason, Graph ),
          PersonsOnBoard=nil(NilReason)
        )
      )
  )
  ,( PilotInCommand='$null$',
    \+ rdf( SupplementalData,fixm:'pilotInCommand', _PilotInCommand, Graph )
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
          ( rdf( GuidebookNumberNode, aixm:uom, UOM, Graph ); rdf( GuidebookNumberNode, fixm:uom, UOM, Graph ); rdf( GuidebookNumberNode, plain:uom, UOM, Graph ) ),
          GuidebookNumber=xval(GuidebookNumberValue,UOM)
        );
        (
          rdf( GuidebookNumberNode,aixm:nilReason, NilReason, Graph ),
          GuidebookNumber=nil(NilReason)
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
          ( rdf( OnboardLocationNode, aixm:uom, UOM, Graph ); rdf( OnboardLocationNode, fixm:uom, UOM, Graph ); rdf( OnboardLocationNode, plain:uom, UOM, Graph ) ),
          OnboardLocation=xval(OnboardLocationValue,UOM)
        );
        (
          rdf( OnboardLocationNode,aixm:nilReason, NilReason, Graph ),
          OnboardLocation=nil(NilReason)
        )
      )
  )
  ,( HandlingInformation='$null$',
    \+ rdf( DangerousGoods,fixm:'handlingInformation', _HandlingInformation, Graph )
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
          ( rdf( AircraftLimitationNode, aixm:uom, UOM, Graph ); rdf( AircraftLimitationNode, fixm:uom, UOM, Graph ); rdf( AircraftLimitationNode, plain:uom, UOM, Graph ) ),
          AircraftLimitation=xval(AircraftLimitationValue,UOM)
        );
        (
          rdf( AircraftLimitationNode,aixm:nilReason, NilReason, Graph ),
          AircraftLimitation=nil(NilReason)
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
          ( rdf( AirWayBillNode, aixm:uom, UOM, Graph ); rdf( AirWayBillNode, fixm:uom, UOM, Graph ); rdf( AirWayBillNode, plain:uom, UOM, Graph ) ),
          AirWayBill=xval(AirWayBillValue,UOM)
        );
        (
          rdf( AirWayBillNode,aixm:nilReason, NilReason, Graph ),
          AirWayBill=nil(NilReason)
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
          ( rdf( ShipmentNode, aixm:uom, UOM, Graph ); rdf( ShipmentNode, fixm:uom, UOM, Graph ); rdf( ShipmentNode, plain:uom, UOM, Graph ) ),
          Shipment=xval(ShipmentValue,UOM)
        );
        (
          rdf( ShipmentNode,aixm:nilReason, NilReason, Graph ),
          Shipment=nil(NilReason)
        )
      )
  )
  ,( PackageGroup='$null$',
    \+ rdf( DangerousGoods,fixm:'packageGroup', _PackageGroup, Graph )
  )
  ,( ShippingInformation='$null$',
    \+ rdf( DangerousGoods,fixm:'shippingInformation', _ShippingInformation, Graph )
  ) .

fixm_DangerousGoodsPackageGroup(Graph, DangerousGoodsPackageGroup, ShipmentDimensions, DangerousGoodsPackage, ShipmentUseIndicator) :-
  rdf(DangerousGoodsPackageGroup,rdf:type,fixm:'DangerousGoodsPackageGroup',Graph)
  ,( ShipmentDimensions='$null$',
    \+ rdf( DangerousGoodsPackageGroup,fixm:'shipmentDimensions', _ShipmentDimensions, Graph )
  )
  ,( DangerousGoodsPackage='$null$',
    \+ rdf( DangerousGoodsPackageGroup,fixm:'dangerousGoodsPackage', _DangerousGoodsPackage, Graph )
  )
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
          ( rdf( ShipmentUseIndicatorNode, aixm:uom, UOM, Graph ); rdf( ShipmentUseIndicatorNode, fixm:uom, UOM, Graph ); rdf( ShipmentUseIndicatorNode, plain:uom, UOM, Graph ) ),
          ShipmentUseIndicator=xval(ShipmentUseIndicatorValue,UOM)
        );
        (
          rdf( ShipmentUseIndicatorNode,aixm:nilReason, NilReason, Graph ),
          ShipmentUseIndicator=nil(NilReason)
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
          ( rdf( DistanceNode, aixm:uom, UOM, Graph ); rdf( DistanceNode, fixm:uom, UOM, Graph ); rdf( DistanceNode, plain:uom, UOM, Graph ) ),
          Distance=xval(DistanceValue,UOM)
        );
        (
          rdf( DistanceNode,aixm:nilReason, NilReason, Graph ),
          Distance=nil(NilReason)
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
          ( rdf( DirectionNode, aixm:uom, UOM, Graph ); rdf( DirectionNode, fixm:uom, UOM, Graph ); rdf( DirectionNode, plain:uom, UOM, Graph ) ),
          Direction=xval(DirectionValue,UOM)
        );
        (
          rdf( DirectionNode,aixm:nilReason, NilReason, Graph ),
          Direction=nil(NilReason)
        )
      )
  ) .

fixm_Handoff(Graph, Handoff, ReceivingUnit, TransferringUnit, CoordinationStatus) :-
  rdf(Handoff,rdf:type,fixm:'Handoff',Graph)
  ,( ReceivingUnit='$null$',
    \+ rdf( Handoff,fixm:'receivingUnit', _ReceivingUnit, Graph )
  )
  ,( TransferringUnit='$null$',
    \+ rdf( Handoff,fixm:'transferringUnit', _TransferringUnit, Graph )
  )
  ,( CoordinationStatus='$null$',
    \+ rdf( Handoff,fixm:'coordinationStatus', _CoordinationStatus, Graph )
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
          ( rdf( ConstrainedAirspaceNode, aixm:uom, UOM, Graph ); rdf( ConstrainedAirspaceNode, fixm:uom, UOM, Graph ); rdf( ConstrainedAirspaceNode, plain:uom, UOM, Graph ) ),
          ConstrainedAirspace=xval(ConstrainedAirspaceValue,UOM)
        );
        (
          rdf( ConstrainedAirspaceNode,aixm:nilReason, NilReason, Graph ),
          ConstrainedAirspace=nil(NilReason)
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
          ( rdf( SpecialActivityAirspaceNode, aixm:uom, UOM, Graph ); rdf( SpecialActivityAirspaceNode, fixm:uom, UOM, Graph ); rdf( SpecialActivityAirspaceNode, plain:uom, UOM, Graph ) ),
          SpecialActivityAirspace=xval(SpecialActivityAirspaceValue,UOM)
        );
        (
          rdf( SpecialActivityAirspaceNode,aixm:nilReason, NilReason, Graph ),
          SpecialActivityAirspace=nil(NilReason)
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
          ( rdf( NameNode, aixm:uom, UOM, Graph ); rdf( NameNode, fixm:uom, UOM, Graph ); rdf( NameNode, plain:uom, UOM, Graph ) ),
          Name=xval(NameValue,UOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NilReason, Graph ),
          Name=nil(NilReason)
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
          ( rdf( TitleNode, aixm:uom, UOM, Graph ); rdf( TitleNode, fixm:uom, UOM, Graph ); rdf( TitleNode, plain:uom, UOM, Graph ) ),
          Title=xval(TitleValue,UOM)
        );
        (
          rdf( TitleNode,aixm:nilReason, NilReason, Graph ),
          Title=nil(NilReason)
        )
      )
  )
  ,( OnlineContact='$null$',
    \+ rdf( ContactInformation,fixm:'onlineContact', _OnlineContact, Graph )
  )
  ,( PhoneFax='$null$',
    \+ rdf( ContactInformation,fixm:'phoneFax', _PhoneFax, Graph )
  )
  ,( Address='$null$',
    \+ rdf( ContactInformation,fixm:'address', _Address, Graph )
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
          ( rdf( DesignatorNode, aixm:uom, UOM, Graph ); rdf( DesignatorNode, fixm:uom, UOM, Graph ); rdf( DesignatorNode, plain:uom, UOM, Graph ) ),
          Designator=xval(DesignatorValue,UOM)
        );
        (
          rdf( DesignatorNode,aixm:nilReason, NilReason, Graph ),
          Designator=nil(NilReason)
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
          ( rdf( NameNode, aixm:uom, UOM, Graph ); rdf( NameNode, fixm:uom, UOM, Graph ); rdf( NameNode, plain:uom, UOM, Graph ) ),
          Name=xval(NameValue,UOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NilReason, Graph ),
          Name=nil(NilReason)
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
          ( rdf( LocationIndicatorICAONode, aixm:uom, UOM, Graph ); rdf( LocationIndicatorICAONode, fixm:uom, UOM, Graph ); rdf( LocationIndicatorICAONode, plain:uom, UOM, Graph ) ),
          LocationIndicatorICAO=xval(LocationIndicatorICAOValue,UOM)
        );
        (
          rdf( LocationIndicatorICAONode,aixm:nilReason, NilReason, Graph ),
          LocationIndicatorICAO=nil(NilReason)
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
          ( rdf( DesignatorIATANode, aixm:uom, UOM, Graph ); rdf( DesignatorIATANode, fixm:uom, UOM, Graph ); rdf( DesignatorIATANode, plain:uom, UOM, Graph ) ),
          DesignatorIATA=xval(DesignatorIATAValue,UOM)
        );
        (
          rdf( DesignatorIATANode,aixm:nilReason, NilReason, Graph ),
          DesignatorIATA=nil(NilReason)
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
          ( rdf( TypeNode, aixm:uom, UOM, Graph ); rdf( TypeNode, fixm:uom, UOM, Graph ); rdf( TypeNode, plain:uom, UOM, Graph ) ),
          Type=xval(TypeValue,UOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, NilReason, Graph ),
          Type=nil(NilReason)
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
          ( rdf( CertifiedICAONode, aixm:uom, UOM, Graph ); rdf( CertifiedICAONode, fixm:uom, UOM, Graph ); rdf( CertifiedICAONode, plain:uom, UOM, Graph ) ),
          CertifiedICAO=xval(CertifiedICAOValue,UOM)
        );
        (
          rdf( CertifiedICAONode,aixm:nilReason, NilReason, Graph ),
          CertifiedICAO=nil(NilReason)
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
          ( rdf( PrivateUseNode, aixm:uom, UOM, Graph ); rdf( PrivateUseNode, fixm:uom, UOM, Graph ); rdf( PrivateUseNode, plain:uom, UOM, Graph ) ),
          PrivateUse=xval(PrivateUseValue,UOM)
        );
        (
          rdf( PrivateUseNode,aixm:nilReason, NilReason, Graph ),
          PrivateUse=nil(NilReason)
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
          ( rdf( ControlTypeNode, aixm:uom, UOM, Graph ); rdf( ControlTypeNode, fixm:uom, UOM, Graph ); rdf( ControlTypeNode, plain:uom, UOM, Graph ) ),
          ControlType=xval(ControlTypeValue,UOM)
        );
        (
          rdf( ControlTypeNode,aixm:nilReason, NilReason, Graph ),
          ControlType=nil(NilReason)
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
          ( rdf( FieldElevationNode, aixm:uom, UOM, Graph ); rdf( FieldElevationNode, fixm:uom, UOM, Graph ); rdf( FieldElevationNode, plain:uom, UOM, Graph ) ),
          FieldElevation=xval(FieldElevationValue,UOM)
        );
        (
          rdf( FieldElevationNode,aixm:nilReason, NilReason, Graph ),
          FieldElevation=nil(NilReason)
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
          ( rdf( FieldElevationAccuracyNode, aixm:uom, UOM, Graph ); rdf( FieldElevationAccuracyNode, fixm:uom, UOM, Graph ); rdf( FieldElevationAccuracyNode, plain:uom, UOM, Graph ) ),
          FieldElevationAccuracy=xval(FieldElevationAccuracyValue,UOM)
        );
        (
          rdf( FieldElevationAccuracyNode,aixm:nilReason, NilReason, Graph ),
          FieldElevationAccuracy=nil(NilReason)
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
          ( rdf( VerticalDatumNode, aixm:uom, UOM, Graph ); rdf( VerticalDatumNode, fixm:uom, UOM, Graph ); rdf( VerticalDatumNode, plain:uom, UOM, Graph ) ),
          VerticalDatum=xval(VerticalDatumValue,UOM)
        );
        (
          rdf( VerticalDatumNode,aixm:nilReason, NilReason, Graph ),
          VerticalDatum=nil(NilReason)
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
          ( rdf( MagneticVariationNode, aixm:uom, UOM, Graph ); rdf( MagneticVariationNode, fixm:uom, UOM, Graph ); rdf( MagneticVariationNode, plain:uom, UOM, Graph ) ),
          MagneticVariation=xval(MagneticVariationValue,UOM)
        );
        (
          rdf( MagneticVariationNode,aixm:nilReason, NilReason, Graph ),
          MagneticVariation=nil(NilReason)
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
          ( rdf( MagneticVariationAccuracyNode, aixm:uom, UOM, Graph ); rdf( MagneticVariationAccuracyNode, fixm:uom, UOM, Graph ); rdf( MagneticVariationAccuracyNode, plain:uom, UOM, Graph ) ),
          MagneticVariationAccuracy=xval(MagneticVariationAccuracyValue,UOM)
        );
        (
          rdf( MagneticVariationAccuracyNode,aixm:nilReason, NilReason, Graph ),
          MagneticVariationAccuracy=nil(NilReason)
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
          ( rdf( DateMagneticVariationNode, aixm:uom, UOM, Graph ); rdf( DateMagneticVariationNode, fixm:uom, UOM, Graph ); rdf( DateMagneticVariationNode, plain:uom, UOM, Graph ) ),
          DateMagneticVariation=xval(DateMagneticVariationValue,UOM)
        );
        (
          rdf( DateMagneticVariationNode,aixm:nilReason, NilReason, Graph ),
          DateMagneticVariation=nil(NilReason)
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
          ( rdf( MagneticVariationChangeNode, aixm:uom, UOM, Graph ); rdf( MagneticVariationChangeNode, fixm:uom, UOM, Graph ); rdf( MagneticVariationChangeNode, plain:uom, UOM, Graph ) ),
          MagneticVariationChange=xval(MagneticVariationChangeValue,UOM)
        );
        (
          rdf( MagneticVariationChangeNode,aixm:nilReason, NilReason, Graph ),
          MagneticVariationChange=nil(NilReason)
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
          ( rdf( ReferenceTemperatureNode, aixm:uom, UOM, Graph ); rdf( ReferenceTemperatureNode, fixm:uom, UOM, Graph ); rdf( ReferenceTemperatureNode, plain:uom, UOM, Graph ) ),
          ReferenceTemperature=xval(ReferenceTemperatureValue,UOM)
        );
        (
          rdf( ReferenceTemperatureNode,aixm:nilReason, NilReason, Graph ),
          ReferenceTemperature=nil(NilReason)
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
          ( rdf( AltimeterCheckLocationNode, aixm:uom, UOM, Graph ); rdf( AltimeterCheckLocationNode, fixm:uom, UOM, Graph ); rdf( AltimeterCheckLocationNode, plain:uom, UOM, Graph ) ),
          AltimeterCheckLocation=xval(AltimeterCheckLocationValue,UOM)
        );
        (
          rdf( AltimeterCheckLocationNode,aixm:nilReason, NilReason, Graph ),
          AltimeterCheckLocation=nil(NilReason)
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
          ( rdf( SecondaryPowerSupplyNode, aixm:uom, UOM, Graph ); rdf( SecondaryPowerSupplyNode, fixm:uom, UOM, Graph ); rdf( SecondaryPowerSupplyNode, plain:uom, UOM, Graph ) ),
          SecondaryPowerSupply=xval(SecondaryPowerSupplyValue,UOM)
        );
        (
          rdf( SecondaryPowerSupplyNode,aixm:nilReason, NilReason, Graph ),
          SecondaryPowerSupply=nil(NilReason)
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
          ( rdf( WindDirectionIndicatorNode, aixm:uom, UOM, Graph ); rdf( WindDirectionIndicatorNode, fixm:uom, UOM, Graph ); rdf( WindDirectionIndicatorNode, plain:uom, UOM, Graph ) ),
          WindDirectionIndicator=xval(WindDirectionIndicatorValue,UOM)
        );
        (
          rdf( WindDirectionIndicatorNode,aixm:nilReason, NilReason, Graph ),
          WindDirectionIndicator=nil(NilReason)
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
          ( rdf( LandingDirectionIndicatorNode, aixm:uom, UOM, Graph ); rdf( LandingDirectionIndicatorNode, fixm:uom, UOM, Graph ); rdf( LandingDirectionIndicatorNode, plain:uom, UOM, Graph ) ),
          LandingDirectionIndicator=xval(LandingDirectionIndicatorValue,UOM)
        );
        (
          rdf( LandingDirectionIndicatorNode,aixm:nilReason, NilReason, Graph ),
          LandingDirectionIndicator=nil(NilReason)
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
          ( rdf( TransitionAltitudeNode, aixm:uom, UOM, Graph ); rdf( TransitionAltitudeNode, fixm:uom, UOM, Graph ); rdf( TransitionAltitudeNode, plain:uom, UOM, Graph ) ),
          TransitionAltitude=xval(TransitionAltitudeValue,UOM)
        );
        (
          rdf( TransitionAltitudeNode,aixm:nilReason, NilReason, Graph ),
          TransitionAltitude=nil(NilReason)
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
          ( rdf( TransitionLevelNode, aixm:uom, UOM, Graph ); rdf( TransitionLevelNode, fixm:uom, UOM, Graph ); rdf( TransitionLevelNode, plain:uom, UOM, Graph ) ),
          TransitionLevel=xval(TransitionLevelValue,UOM)
        );
        (
          rdf( TransitionLevelNode,aixm:nilReason, NilReason, Graph ),
          TransitionLevel=nil(NilReason)
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
          ( rdf( LowestTemperatureNode, aixm:uom, UOM, Graph ); rdf( LowestTemperatureNode, fixm:uom, UOM, Graph ); rdf( LowestTemperatureNode, plain:uom, UOM, Graph ) ),
          LowestTemperature=xval(LowestTemperatureValue,UOM)
        );
        (
          rdf( LowestTemperatureNode,aixm:nilReason, NilReason, Graph ),
          LowestTemperature=nil(NilReason)
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
          ( rdf( AbandonedNode, aixm:uom, UOM, Graph ); rdf( AbandonedNode, fixm:uom, UOM, Graph ); rdf( AbandonedNode, plain:uom, UOM, Graph ) ),
          Abandoned=xval(AbandonedValue,UOM)
        );
        (
          rdf( AbandonedNode,aixm:nilReason, NilReason, Graph ),
          Abandoned=nil(NilReason)
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
          ( rdf( CertificationDateNode, aixm:uom, UOM, Graph ); rdf( CertificationDateNode, fixm:uom, UOM, Graph ); rdf( CertificationDateNode, plain:uom, UOM, Graph ) ),
          CertificationDate=xval(CertificationDateValue,UOM)
        );
        (
          rdf( CertificationDateNode,aixm:nilReason, NilReason, Graph ),
          CertificationDate=nil(NilReason)
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
          ( rdf( CertificationExpirationDateNode, aixm:uom, UOM, Graph ); rdf( CertificationExpirationDateNode, fixm:uom, UOM, Graph ); rdf( CertificationExpirationDateNode, plain:uom, UOM, Graph ) ),
          CertificationExpirationDate=xval(CertificationExpirationDateValue,UOM)
        );
        (
          rdf( CertificationExpirationDateNode,aixm:nilReason, NilReason, Graph ),
          CertificationExpirationDate=nil(NilReason)
        )
      )
  )
  ,( Contact='$null$',
    \+ rdf( AirportHeliportTimeSlice,aixm:'contact', _Contact, Graph )
  )
  ,( Annotation='$null$',
    \+ rdf( AirportHeliportTimeSlice,aixm:'annotation', _Annotation, Graph )
  )
  ,( ARP='$null$',
    \+ rdf( AirportHeliportTimeSlice,aixm:'ARP', _ARP, Graph )
  )
  ,( AltimeterSource='$null$',
    \+ rdf( AirportHeliportTimeSlice,aixm:'altimeterSource', _AltimeterSource, Graph )
  )
  ,( Contaminant='$null$',
    \+ rdf( AirportHeliportTimeSlice,aixm:'contaminant', _Contaminant, Graph )
  )
  ,( ServedCity='$null$',
    \+ rdf( AirportHeliportTimeSlice,aixm:'servedCity', _ServedCity, Graph )
  )
  ,( ResponsibleOrganisation='$null$',
    \+ rdf( AirportHeliportTimeSlice,aixm:'responsibleOrganisation', _ResponsibleOrganisation, Graph )
  )
  ,( AviationBoundary='$null$',
    \+ rdf( AirportHeliportTimeSlice,aixm:'aviationBoundary', _AviationBoundary, Graph )
  )
  ,( Availability='$null$',
    \+ rdf( AirportHeliportTimeSlice,aixm:'availability', _Availability, Graph )
  ) .

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
          ( rdf( AltitudeNode, aixm:uom, UOM, Graph ); rdf( AltitudeNode, fixm:uom, UOM, Graph ); rdf( AltitudeNode, plain:uom, UOM, Graph ) ),
          Altitude=xval(AltitudeValue,UOM)
        );
        (
          rdf( AltitudeNode,aixm:nilReason, NilReason, Graph ),
          Altitude=nil(NilReason)
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
          ( rdf( TimeNode, aixm:uom, UOM, Graph ); rdf( TimeNode, fixm:uom, UOM, Graph ); rdf( TimeNode, plain:uom, UOM, Graph ) ),
          Time=xval(TimeValue,UOM)
        );
        (
          rdf( TimeNode,aixm:nilReason, NilReason, Graph ),
          Time=nil(NilReason)
        )
      )
  )
  ,( PointRange='$null$',
    \+ rdf( Point4D,fixm:'pointRange', _PointRange, Graph )
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
          ( rdf( AirTrafficTypeNode, aixm:uom, UOM, Graph ); rdf( AirTrafficTypeNode, fixm:uom, UOM, Graph ); rdf( AirTrafficTypeNode, plain:uom, UOM, Graph ) ),
          AirTrafficType=xval(AirTrafficTypeValue,UOM)
        );
        (
          rdf( AirTrafficTypeNode,aixm:nilReason, NilReason, Graph ),
          AirTrafficType=nil(NilReason)
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
          ( rdf( DelayAtPointNode, aixm:uom, UOM, Graph ); rdf( DelayAtPointNode, fixm:uom, UOM, Graph ); rdf( DelayAtPointNode, plain:uom, UOM, Graph ) ),
          DelayAtPoint=xval(DelayAtPointValue,UOM)
        );
        (
          rdf( DelayAtPointNode,aixm:nilReason, NilReason, Graph ),
          DelayAtPoint=nil(NilReason)
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
          ( rdf( FlightRulesNode, aixm:uom, UOM, Graph ); rdf( FlightRulesNode, fixm:uom, UOM, Graph ); rdf( FlightRulesNode, plain:uom, UOM, Graph ) ),
          FlightRules=xval(FlightRulesValue,UOM)
        );
        (
          rdf( FlightRulesNode,aixm:nilReason, NilReason, Graph ),
          FlightRules=nil(NilReason)
        )
      )
  )
  ,( Point='$null$',
    \+ rdf( AbstractRoutePoint,fixm:'point', _Point, Graph )
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
          ( rdf( ClearanceLimitNode, aixm:uom, UOM, Graph ); rdf( ClearanceLimitNode, fixm:uom, UOM, Graph ); rdf( ClearanceLimitNode, plain:uom, UOM, Graph ) ),
          ClearanceLimit=xval(ClearanceLimitValue,UOM)
        );
        (
          rdf( ClearanceLimitNode,aixm:nilReason, NilReason, Graph ),
          ClearanceLimit=nil(NilReason)
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
          ( rdf( SideNode, aixm:uom, UOM, Graph ); rdf( SideNode, fixm:uom, UOM, Graph ); rdf( SideNode, plain:uom, UOM, Graph ) ),
          Side=xval(SideValue,UOM)
        );
        (
          rdf( SideNode,aixm:nilReason, NilReason, Graph ),
          Side=nil(NilReason)
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
          ( rdf( DistanceNode, aixm:uom, UOM, Graph ); rdf( DistanceNode, fixm:uom, UOM, Graph ); rdf( DistanceNode, plain:uom, UOM, Graph ) ),
          Distance=xval(DistanceValue,UOM)
        );
        (
          rdf( DistanceNode,aixm:nilReason, NilReason, Graph ),
          Distance=nil(NilReason)
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
          ( rdf( DepthNode, aixm:uom, UOM, Graph ); rdf( DepthNode, fixm:uom, UOM, Graph ); rdf( DepthNode, plain:uom, UOM, Graph ) ),
          Depth=xval(DepthValue,UOM)
        );
        (
          rdf( DepthNode,aixm:nilReason, NilReason, Graph ),
          Depth=nil(NilReason)
        )
      )
  )
  ,( Annotation='$null$',
    \+ rdf( Ridge,aixm:'annotation', _Annotation, Graph )
  ) .

fixm_DepartureActivityTimes(Graph, DepartureActivityTimes, BoardingTime, DeIcingTime, GroundHandlingTime, StartupTime) :-
  rdf(DepartureActivityTimes,rdf:type,fixm:'DepartureActivityTimes',Graph)
  ,( BoardingTime='$null$',
    \+ rdf( DepartureActivityTimes,fixm:'boardingTime', _BoardingTime, Graph )
  )
  ,( DeIcingTime='$null$',
    \+ rdf( DepartureActivityTimes,fixm:'deIcingTime', _DeIcingTime, Graph )
  )
  ,( GroundHandlingTime='$null$',
    \+ rdf( DepartureActivityTimes,fixm:'groundHandlingTime', _GroundHandlingTime, Graph )
  )
  ,( StartupTime='$null$',
    \+ rdf( DepartureActivityTimes,fixm:'startupTime', _StartupTime, Graph )
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
          ( rdf( DiversionRecoveryInformationNode, aixm:uom, UOM, Graph ); rdf( DiversionRecoveryInformationNode, fixm:uom, UOM, Graph ); rdf( DiversionRecoveryInformationNode, plain:uom, UOM, Graph ) ),
          DiversionRecoveryInformation=xval(DiversionRecoveryInformationValue,UOM)
        );
        (
          rdf( DiversionRecoveryInformationNode,aixm:nilReason, NilReason, Graph ),
          DiversionRecoveryInformation=nil(NilReason)
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
          ( rdf( CalculatedNode, aixm:uom, UOM, Graph ); rdf( CalculatedNode, fixm:uom, UOM, Graph ); rdf( CalculatedNode, plain:uom, UOM, Graph ) ),
          Calculated=xval(CalculatedValue,UOM)
        );
        (
          rdf( CalculatedNode,aixm:nilReason, NilReason, Graph ),
          Calculated=nil(NilReason)
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
          ( rdf( PilotReportedNode, aixm:uom, UOM, Graph ); rdf( PilotReportedNode, fixm:uom, UOM, Graph ); rdf( PilotReportedNode, plain:uom, UOM, Graph ) ),
          PilotReported=xval(PilotReportedValue,UOM)
        );
        (
          rdf( PilotReportedNode,aixm:nilReason, NilReason, Graph ),
          PilotReported=nil(NilReason)
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
          ( rdf( SurveillanceNode, aixm:uom, UOM, Graph ); rdf( SurveillanceNode, fixm:uom, UOM, Graph ); rdf( SurveillanceNode, plain:uom, UOM, Graph ) ),
          Surveillance=xval(SurveillanceValue,UOM)
        );
        (
          rdf( SurveillanceNode,aixm:nilReason, NilReason, Graph ),
          Surveillance=nil(NilReason)
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
          ( rdf( ActionTakenNode, aixm:uom, UOM, Graph ); rdf( ActionTakenNode, fixm:uom, UOM, Graph ); rdf( ActionTakenNode, plain:uom, UOM, Graph ) ),
          ActionTaken=xval(ActionTakenValue,UOM)
        );
        (
          rdf( ActionTakenNode,aixm:nilReason, NilReason, Graph ),
          ActionTaken=nil(NilReason)
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
          ( rdf( EmergencyDescriptionNode, aixm:uom, UOM, Graph ); rdf( EmergencyDescriptionNode, fixm:uom, UOM, Graph ); rdf( EmergencyDescriptionNode, plain:uom, UOM, Graph ) ),
          EmergencyDescription=xval(EmergencyDescriptionValue,UOM)
        );
        (
          rdf( EmergencyDescriptionNode,aixm:nilReason, NilReason, Graph ),
          EmergencyDescription=nil(NilReason)
        )
      )
  )
  ,( Originator='$null$',
    \+ rdf( FlightEmergency,fixm:'originator', _Originator, Graph )
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
          ( rdf( OtherInformationNode, aixm:uom, UOM, Graph ); rdf( OtherInformationNode, fixm:uom, UOM, Graph ); rdf( OtherInformationNode, plain:uom, UOM, Graph ) ),
          OtherInformation=xval(OtherInformationValue,UOM)
        );
        (
          rdf( OtherInformationNode,aixm:nilReason, NilReason, Graph ),
          OtherInformation=nil(NilReason)
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
          ( rdf( PhaseNode, aixm:uom, UOM, Graph ); rdf( PhaseNode, fixm:uom, UOM, Graph ); rdf( PhaseNode, plain:uom, UOM, Graph ) ),
          Phase=xval(PhaseValue,UOM)
        );
        (
          rdf( PhaseNode,aixm:nilReason, NilReason, Graph ),
          Phase=nil(NilReason)
        )
      )
  )
  ,( Contact='$null$',
    \+ rdf( FlightEmergency,fixm:'contact', _Contact, Graph )
  ) .

fixm_Flight(Graph, Flight, ControllingUnit, Extensions, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoods, RankedTrajectories, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandling) :-
  subClassOf(T,fixm:'Flight')
  ,rdf(Flight,rdf:type,T,Graph)
  ,( ControllingUnit='$null$',
    \+ rdf( Flight,fixm:'controllingUnit', _ControllingUnit, Graph )
  )
  ,( Extensions='$null$',
    \+ rdf( Flight,fixm:'extensions', _Extensions, Graph )
  )
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
          ( rdf( FlightFilerNode, aixm:uom, UOM, Graph ); rdf( FlightFilerNode, fixm:uom, UOM, Graph ); rdf( FlightFilerNode, plain:uom, UOM, Graph ) ),
          FlightFiler=xval(FlightFilerValue,UOM)
        );
        (
          rdf( FlightFilerNode,aixm:nilReason, NilReason, Graph ),
          FlightFiler=nil(NilReason)
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
          ( rdf( GufiNode, aixm:uom, UOM, Graph ); rdf( GufiNode, fixm:uom, UOM, Graph ); rdf( GufiNode, plain:uom, UOM, Graph ) ),
          Gufi=xval(GufiValue,UOM)
        );
        (
          rdf( GufiNode,aixm:nilReason, NilReason, Graph ),
          Gufi=nil(NilReason)
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
          ( rdf( RemarksNode, aixm:uom, UOM, Graph ); rdf( RemarksNode, fixm:uom, UOM, Graph ); rdf( RemarksNode, plain:uom, UOM, Graph ) ),
          Remarks=xval(RemarksValue,UOM)
        );
        (
          rdf( RemarksNode,aixm:nilReason, NilReason, Graph ),
          Remarks=nil(NilReason)
        )
      )
  )
  ,( AircraftDescription='$null$',
    \+ rdf( Flight,fixm:'aircraftDescription', _AircraftDescription, Graph )
  )
  ,( DangerousGoods='$null$',
    \+ rdf( Flight,fixm:'dangerousGoods', _DangerousGoods, Graph )
  )
  ,( RankedTrajectories='$null$',
    \+ rdf( Flight,fixm:'rankedTrajectories', _RankedTrajectories, Graph )
  )
  ,( RouteToRevisedDestination='$null$',
    \+ rdf( Flight,fixm:'routeToRevisedDestination', _RouteToRevisedDestination, Graph )
  )
  ,( Negotiating='$null$',
    \+ rdf( Flight,fixm:'negotiating', _Negotiating, Graph )
  )
  ,( Agreed='$null$',
    \+ rdf( Flight,fixm:'agreed', _Agreed, Graph )
  )
  ,( Arrival='$null$',
    \+ rdf( Flight,fixm:'arrival', _Arrival, Graph )
  )
  ,( Departure='$null$',
    \+ rdf( Flight,fixm:'departure', _Departure, Graph )
  )
  ,( Emergency='$null$',
    \+ rdf( Flight,fixm:'emergency', _Emergency, Graph )
  )
  ,( RadioCommunicationFailure='$null$',
    \+ rdf( Flight,fixm:'radioCommunicationFailure', _RadioCommunicationFailure, Graph )
  )
  ,( EnRoute='$null$',
    \+ rdf( Flight,fixm:'enRoute', _EnRoute, Graph )
  )
  ,( Operator='$null$',
    \+ rdf( Flight,fixm:'operator', _Operator, Graph )
  )
  ,( EnRouteDiversion='$null$',
    \+ rdf( Flight,fixm:'enRouteDiversion', _EnRouteDiversion, Graph )
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
          ( rdf( FlightTypeNode, aixm:uom, UOM, Graph ); rdf( FlightTypeNode, fixm:uom, UOM, Graph ); rdf( FlightTypeNode, plain:uom, UOM, Graph ) ),
          FlightType=xval(FlightTypeValue,UOM)
        );
        (
          rdf( FlightTypeNode,aixm:nilReason, NilReason, Graph ),
          FlightType=nil(NilReason)
        )
      )
  )
  ,( FlightStatus='$null$',
    \+ rdf( Flight,fixm:'flightStatus', _FlightStatus, Graph )
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
          ( rdf( OriginatorNode, aixm:uom, UOM, Graph ); rdf( OriginatorNode, fixm:uom, UOM, Graph ); rdf( OriginatorNode, plain:uom, UOM, Graph ) ),
          Originator=xval(OriginatorValue,UOM)
        );
        (
          rdf( OriginatorNode,aixm:nilReason, NilReason, Graph ),
          Originator=nil(NilReason)
        )
      )
  )
  ,( SupplementalData='$null$',
    \+ rdf( Flight,fixm:'supplementalData', _SupplementalData, Graph )
  )
  ,( FlightIdentification='$null$',
    \+ rdf( Flight,fixm:'flightIdentification', _FlightIdentification, Graph )
  )
  ,findall(A, rdf(Flight,fixm:'specialHandling',A,Graph), SpecialHandling) .

aixm_PropertiesWithSchedule(Graph, PropertiesWithSchedule, Annotation, SpecialDateAuthority, TimeInterval) :-
  subClassOf(T,aixm:'PropertiesWithSchedule')
  ,rdf(PropertiesWithSchedule,rdf:type,T,Graph)
  ,( Annotation='$null$',
    \+ rdf( PropertiesWithSchedule,aixm:'annotation', _Annotation, Graph )
  )
  ,( SpecialDateAuthority='$null$',
    \+ rdf( PropertiesWithSchedule,aixm:'specialDateAuthority', _SpecialDateAuthority, Graph )
  )
  ,( TimeInterval='$null$',
    \+ rdf( PropertiesWithSchedule,aixm:'timeInterval', _TimeInterval, Graph )
  ) .

gml_Surface(Graph, Surface, Patch) :-
  subClassOf(T,gml:'Surface')
  ,rdf(Surface,rdf:type,T,Graph)
  ,rdf(Surface,aixm:'patch',Patch,Graph) .

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
          ( rdf( ClearedFlightLevelNode, aixm:uom, UOM, Graph ); rdf( ClearedFlightLevelNode, fixm:uom, UOM, Graph ); rdf( ClearedFlightLevelNode, plain:uom, UOM, Graph ) ),
          ClearedFlightLevel=xval(ClearedFlightLevelValue,UOM)
        );
        (
          rdf( ClearedFlightLevelNode,aixm:nilReason, NilReason, Graph ),
          ClearedFlightLevel=nil(NilReason)
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
          ( rdf( ClearedSpeedNode, aixm:uom, UOM, Graph ); rdf( ClearedSpeedNode, fixm:uom, UOM, Graph ); rdf( ClearedSpeedNode, plain:uom, UOM, Graph ) ),
          ClearedSpeed=xval(ClearedSpeedValue,UOM)
        );
        (
          rdf( ClearedSpeedNode,aixm:nilReason, NilReason, Graph ),
          ClearedSpeed=nil(NilReason)
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
          ( rdf( HeadingNode, aixm:uom, UOM, Graph ); rdf( HeadingNode, fixm:uom, UOM, Graph ); rdf( HeadingNode, plain:uom, UOM, Graph ) ),
          Heading=xval(HeadingValue,UOM)
        );
        (
          rdf( HeadingNode,aixm:nilReason, NilReason, Graph ),
          Heading=nil(NilReason)
        )
      )
  )
  ,( OfftrackClearance='$null$',
    \+ rdf( ClearedFlightInformation,fixm:'offtrackClearance', _OfftrackClearance, Graph )
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
          ( rdf( RateOfClimbDescendNode, aixm:uom, UOM, Graph ); rdf( RateOfClimbDescendNode, fixm:uom, UOM, Graph ); rdf( RateOfClimbDescendNode, plain:uom, UOM, Graph ) ),
          RateOfClimbDescend=xval(RateOfClimbDescendValue,UOM)
        );
        (
          rdf( RateOfClimbDescendNode,aixm:nilReason, NilReason, Graph ),
          RateOfClimbDescend=nil(NilReason)
        )
      )
  )
  ,( DirectRouting='$null$',
    \+ rdf( ClearedFlightInformation,fixm:'directRouting', _DirectRouting, Graph )
  ) .

fixm_TrajectoryRoutePair(Graph, TrajectoryRoutePair, Trajectory, Route) :-
  subClassOf(T,fixm:'TrajectoryRoutePair')
  ,rdf(TrajectoryRoutePair,rdf:type,T,Graph)
  ,( Trajectory='$null$',
    \+ rdf( TrajectoryRoutePair,fixm:'trajectory', _Trajectory, Graph )
  )
  ,( Route='$null$',
    \+ rdf( TrajectoryRoutePair,fixm:'route', _Route, Graph )
  ) .

fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit, UpstreamUnit, BoundaryCrossingProposed, BoundaryCrossingCoordinated, Handoff, UnitBoundaryIndicator) :-
  rdf(UnitBoundary,rdf:type,fixm:'UnitBoundary',Graph)
  ,( DownstreamUnit='$null$',
    \+ rdf( UnitBoundary,fixm:'downstreamUnit', _DownstreamUnit, Graph )
  )
  ,( UpstreamUnit='$null$',
    \+ rdf( UnitBoundary,fixm:'upstreamUnit', _UpstreamUnit, Graph )
  )
  ,( BoundaryCrossingProposed='$null$',
    \+ rdf( UnitBoundary,fixm:'boundaryCrossingProposed', _BoundaryCrossingProposed, Graph )
  )
  ,( BoundaryCrossingCoordinated='$null$',
    \+ rdf( UnitBoundary,fixm:'boundaryCrossingCoordinated', _BoundaryCrossingCoordinated, Graph )
  )
  ,( Handoff='$null$',
    \+ rdf( UnitBoundary,fixm:'handoff', _Handoff, Graph )
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
          ( rdf( UnitBoundaryIndicatorNode, aixm:uom, UOM, Graph ); rdf( UnitBoundaryIndicatorNode, fixm:uom, UOM, Graph ); rdf( UnitBoundaryIndicatorNode, plain:uom, UOM, Graph ) ),
          UnitBoundaryIndicator=xval(UnitBoundaryIndicatorValue,UOM)
        );
        (
          rdf( UnitBoundaryIndicatorNode,aixm:nilReason, NilReason, Graph ),
          UnitBoundaryIndicator=nil(NilReason)
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
          ( rdf( ObservationTimeNode, aixm:uom, UOM, Graph ); rdf( ObservationTimeNode, fixm:uom, UOM, Graph ); rdf( ObservationTimeNode, plain:uom, UOM, Graph ) ),
          ObservationTime=xval(ObservationTimeValue,UOM)
        );
        (
          rdf( ObservationTimeNode,aixm:nilReason, NilReason, Graph ),
          ObservationTime=nil(NilReason)
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
          ( rdf( DepthNode, aixm:uom, UOM, Graph ); rdf( DepthNode, fixm:uom, UOM, Graph ); rdf( DepthNode, plain:uom, UOM, Graph ) ),
          Depth=xval(DepthValue,UOM)
        );
        (
          rdf( DepthNode,aixm:nilReason, NilReason, Graph ),
          Depth=nil(NilReason)
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
          ( rdf( FrictionCoefficientNode, aixm:uom, UOM, Graph ); rdf( FrictionCoefficientNode, fixm:uom, UOM, Graph ); rdf( FrictionCoefficientNode, plain:uom, UOM, Graph ) ),
          FrictionCoefficient=xval(FrictionCoefficientValue,UOM)
        );
        (
          rdf( FrictionCoefficientNode,aixm:nilReason, NilReason, Graph ),
          FrictionCoefficient=nil(NilReason)
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
          ( rdf( FrictionEstimationNode, aixm:uom, UOM, Graph ); rdf( FrictionEstimationNode, fixm:uom, UOM, Graph ); rdf( FrictionEstimationNode, plain:uom, UOM, Graph ) ),
          FrictionEstimation=xval(FrictionEstimationValue,UOM)
        );
        (
          rdf( FrictionEstimationNode,aixm:nilReason, NilReason, Graph ),
          FrictionEstimation=nil(NilReason)
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
          ( rdf( FrictionDeviceNode, aixm:uom, UOM, Graph ); rdf( FrictionDeviceNode, fixm:uom, UOM, Graph ); rdf( FrictionDeviceNode, plain:uom, UOM, Graph ) ),
          FrictionDevice=xval(FrictionDeviceValue,UOM)
        );
        (
          rdf( FrictionDeviceNode,aixm:nilReason, NilReason, Graph ),
          FrictionDevice=nil(NilReason)
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
          ( rdf( ObscuredLightsNode, aixm:uom, UOM, Graph ); rdf( ObscuredLightsNode, fixm:uom, UOM, Graph ); rdf( ObscuredLightsNode, plain:uom, UOM, Graph ) ),
          ObscuredLights=xval(ObscuredLightsValue,UOM)
        );
        (
          rdf( ObscuredLightsNode,aixm:nilReason, NilReason, Graph ),
          ObscuredLights=nil(NilReason)
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
          ( rdf( FurtherClearanceTimeNode, aixm:uom, UOM, Graph ); rdf( FurtherClearanceTimeNode, fixm:uom, UOM, Graph ); rdf( FurtherClearanceTimeNode, plain:uom, UOM, Graph ) ),
          FurtherClearanceTime=xval(FurtherClearanceTimeValue,UOM)
        );
        (
          rdf( FurtherClearanceTimeNode,aixm:nilReason, NilReason, Graph ),
          FurtherClearanceTime=nil(NilReason)
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
          ( rdf( FurtherTotalClearanceNode, aixm:uom, UOM, Graph ); rdf( FurtherTotalClearanceNode, fixm:uom, UOM, Graph ); rdf( FurtherTotalClearanceNode, plain:uom, UOM, Graph ) ),
          FurtherTotalClearance=xval(FurtherTotalClearanceValue,UOM)
        );
        (
          rdf( FurtherTotalClearanceNode,aixm:nilReason, NilReason, Graph ),
          FurtherTotalClearance=nil(NilReason)
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
          ( rdf( NextObservationTimeNode, aixm:uom, UOM, Graph ); rdf( NextObservationTimeNode, fixm:uom, UOM, Graph ); rdf( NextObservationTimeNode, plain:uom, UOM, Graph ) ),
          NextObservationTime=xval(NextObservationTimeValue,UOM)
        );
        (
          rdf( NextObservationTimeNode,aixm:nilReason, NilReason, Graph ),
          NextObservationTime=nil(NilReason)
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
          ( rdf( ProportionNode, aixm:uom, UOM, Graph ); rdf( ProportionNode, fixm:uom, UOM, Graph ); rdf( ProportionNode, plain:uom, UOM, Graph ) ),
          Proportion=xval(ProportionValue,UOM)
        );
        (
          rdf( ProportionNode,aixm:nilReason, NilReason, Graph ),
          Proportion=nil(NilReason)
        )
      )
  )
  ,( CriticalRidge='$null$',
    \+ rdf( SurfaceContamination,aixm:'criticalRidge', _CriticalRidge, Graph )
  )
  ,( Annotation='$null$',
    \+ rdf( SurfaceContamination,aixm:'annotation', _Annotation, Graph )
  )
  ,( Layer='$null$',
    \+ rdf( SurfaceContamination,aixm:'layer', _Layer, Graph )
  ) .

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
          ( rdf( TemperatureNode, aixm:uom, UOM, Graph ); rdf( TemperatureNode, fixm:uom, UOM, Graph ); rdf( TemperatureNode, plain:uom, UOM, Graph ) ),
          Temperature=xval(TemperatureValue,UOM)
        );
        (
          rdf( TemperatureNode,aixm:nilReason, NilReason, Graph ),
          Temperature=nil(NilReason)
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
          ( rdf( WindDirectionNode, aixm:uom, UOM, Graph ); rdf( WindDirectionNode, fixm:uom, UOM, Graph ); rdf( WindDirectionNode, plain:uom, UOM, Graph ) ),
          WindDirection=xval(WindDirectionValue,UOM)
        );
        (
          rdf( WindDirectionNode,aixm:nilReason, NilReason, Graph ),
          WindDirection=nil(NilReason)
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
          ( rdf( WindSpeedNode, aixm:uom, UOM, Graph ); rdf( WindSpeedNode, fixm:uom, UOM, Graph ); rdf( WindSpeedNode, plain:uom, UOM, Graph ) ),
          WindSpeed=xval(WindSpeedValue,UOM)
        );
        (
          rdf( WindSpeedNode,aixm:nilReason, NilReason, Graph ),
          WindSpeed=nil(NilReason)
        )
      )
  ) .

aixm_OrganisationAuthority(Graph, OrganisationAuthority, TimeSlice) :-
  rdf(OrganisationAuthority,rdf:type,aixm:'OrganisationAuthority',Graph)
  ,( TimeSlice='$null$',
    \+ rdf( OrganisationAuthority,aixm:'timeSlice', _TimeSlice, Graph )
  ) .

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
          ( rdf( VoiceNode, aixm:uom, UOM, Graph ); rdf( VoiceNode, fixm:uom, UOM, Graph ); rdf( VoiceNode, plain:uom, UOM, Graph ) ),
          Voice=xval(VoiceValue,UOM)
        );
        (
          rdf( VoiceNode,aixm:nilReason, NilReason, Graph ),
          Voice=nil(NilReason)
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
          ( rdf( FacimileNode, aixm:uom, UOM, Graph ); rdf( FacimileNode, fixm:uom, UOM, Graph ); rdf( FacimileNode, plain:uom, UOM, Graph ) ),
          Facimile=xval(FacimileValue,UOM)
        );
        (
          rdf( FacimileNode,aixm:nilReason, NilReason, Graph ),
          Facimile=nil(NilReason)
        )
      )
  ) .

fixm_ShippingInformation(Graph, ShippingInformation, AerodromeOfLoading, AerodromeOfUnloading, DangerousGoodsScreeningLocation, DepartureCountry, DestinationCountry, OriginCountry, ShipmentAuthorizations, SubsidiaryHazardClassAndDivision, SupplementaryInformation, TransferAerodromes, DeclarationText, Consignee, Shipper) :-
  rdf(ShippingInformation,rdf:type,fixm:'ShippingInformation',Graph)
  ,( AerodromeOfLoading='$null$',
    \+ rdf( ShippingInformation,fixm:'aerodromeOfLoading', _AerodromeOfLoading, Graph )
  )
  ,( AerodromeOfUnloading='$null$',
    \+ rdf( ShippingInformation,fixm:'aerodromeOfUnloading', _AerodromeOfUnloading, Graph )
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
          ( rdf( DangerousGoodsScreeningLocationNode, aixm:uom, UOM, Graph ); rdf( DangerousGoodsScreeningLocationNode, fixm:uom, UOM, Graph ); rdf( DangerousGoodsScreeningLocationNode, plain:uom, UOM, Graph ) ),
          DangerousGoodsScreeningLocation=xval(DangerousGoodsScreeningLocationValue,UOM)
        );
        (
          rdf( DangerousGoodsScreeningLocationNode,aixm:nilReason, NilReason, Graph ),
          DangerousGoodsScreeningLocation=nil(NilReason)
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
          ( rdf( DepartureCountryNode, aixm:uom, UOM, Graph ); rdf( DepartureCountryNode, fixm:uom, UOM, Graph ); rdf( DepartureCountryNode, plain:uom, UOM, Graph ) ),
          DepartureCountry=xval(DepartureCountryValue,UOM)
        );
        (
          rdf( DepartureCountryNode,aixm:nilReason, NilReason, Graph ),
          DepartureCountry=nil(NilReason)
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
          ( rdf( DestinationCountryNode, aixm:uom, UOM, Graph ); rdf( DestinationCountryNode, fixm:uom, UOM, Graph ); rdf( DestinationCountryNode, plain:uom, UOM, Graph ) ),
          DestinationCountry=xval(DestinationCountryValue,UOM)
        );
        (
          rdf( DestinationCountryNode,aixm:nilReason, NilReason, Graph ),
          DestinationCountry=nil(NilReason)
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
          ( rdf( OriginCountryNode, aixm:uom, UOM, Graph ); rdf( OriginCountryNode, fixm:uom, UOM, Graph ); rdf( OriginCountryNode, plain:uom, UOM, Graph ) ),
          OriginCountry=xval(OriginCountryValue,UOM)
        );
        (
          rdf( OriginCountryNode,aixm:nilReason, NilReason, Graph ),
          OriginCountry=nil(NilReason)
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
          ( rdf( ShipmentAuthorizationsNode, aixm:uom, UOM, Graph ); rdf( ShipmentAuthorizationsNode, fixm:uom, UOM, Graph ); rdf( ShipmentAuthorizationsNode, plain:uom, UOM, Graph ) ),
          ShipmentAuthorizations=xval(ShipmentAuthorizationsValue,UOM)
        );
        (
          rdf( ShipmentAuthorizationsNode,aixm:nilReason, NilReason, Graph ),
          ShipmentAuthorizations=nil(NilReason)
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
          ( rdf( SubsidiaryHazardClassAndDivisionNode, aixm:uom, UOM, Graph ); rdf( SubsidiaryHazardClassAndDivisionNode, fixm:uom, UOM, Graph ); rdf( SubsidiaryHazardClassAndDivisionNode, plain:uom, UOM, Graph ) ),
          SubsidiaryHazardClassAndDivision=xval(SubsidiaryHazardClassAndDivisionValue,UOM)
        );
        (
          rdf( SubsidiaryHazardClassAndDivisionNode,aixm:nilReason, NilReason, Graph ),
          SubsidiaryHazardClassAndDivision=nil(NilReason)
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
          ( rdf( SupplementaryInformationNode, aixm:uom, UOM, Graph ); rdf( SupplementaryInformationNode, fixm:uom, UOM, Graph ); rdf( SupplementaryInformationNode, plain:uom, UOM, Graph ) ),
          SupplementaryInformation=xval(SupplementaryInformationValue,UOM)
        );
        (
          rdf( SupplementaryInformationNode,aixm:nilReason, NilReason, Graph ),
          SupplementaryInformation=nil(NilReason)
        )
      )
  )
  ,findall(A, rdf(ShippingInformation,fixm:'transferAerodromes',A,Graph), TransferAerodromes)
  ,( DeclarationText='$null$',
    \+ rdf( ShippingInformation,fixm:'declarationText', _DeclarationText, Graph )
  )
  ,( Consignee='$null$',
    \+ rdf( ShippingInformation,fixm:'consignee', _Consignee, Graph )
  )
  ,( Shipper='$null$',
    \+ rdf( ShippingInformation,fixm:'shipper', _Shipper, Graph )
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
          ( rdf( ReplacementFlightPlanIndicatorNode, aixm:uom, UOM, Graph ); rdf( ReplacementFlightPlanIndicatorNode, fixm:uom, UOM, Graph ); rdf( ReplacementFlightPlanIndicatorNode, plain:uom, UOM, Graph ) ),
          ReplacementFlightPlanIndicator=xval(ReplacementFlightPlanIndicatorValue,UOM)
        );
        (
          rdf( ReplacementFlightPlanIndicatorNode,aixm:nilReason, NilReason, Graph ),
          ReplacementFlightPlanIndicator=nil(NilReason)
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
          ( rdf( RunwayVisualRangeNode, aixm:uom, UOM, Graph ); rdf( RunwayVisualRangeNode, fixm:uom, UOM, Graph ); rdf( RunwayVisualRangeNode, plain:uom, UOM, Graph ) ),
          RunwayVisualRange=xval(RunwayVisualRangeValue,UOM)
        );
        (
          rdf( RunwayVisualRangeNode,aixm:nilReason, NilReason, Graph ),
          RunwayVisualRange=nil(NilReason)
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
          ( rdf( ReceivingUnitFrequencyNode, aixm:uom, UOM, Graph ); rdf( ReceivingUnitFrequencyNode, fixm:uom, UOM, Graph ); rdf( ReceivingUnitFrequencyNode, plain:uom, UOM, Graph ) ),
          ReceivingUnitFrequency=xval(ReceivingUnitFrequencyValue,UOM)
        );
        (
          rdf( ReceivingUnitFrequencyNode,aixm:nilReason, NilReason, Graph ),
          ReceivingUnitFrequency=nil(NilReason)
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
          ( rdf( AtnLogonParametersNode, aixm:uom, UOM, Graph ); rdf( AtnLogonParametersNode, fixm:uom, UOM, Graph ); rdf( AtnLogonParametersNode, plain:uom, UOM, Graph ) ),
          AtnLogonParameters=xval(AtnLogonParametersValue,UOM)
        );
        (
          rdf( AtnLogonParametersNode,aixm:nilReason, NilReason, Graph ),
          AtnLogonParameters=nil(NilReason)
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
          ( rdf( SendCpldcIndicatorNode, aixm:uom, UOM, Graph ); rdf( SendCpldcIndicatorNode, fixm:uom, UOM, Graph ); rdf( SendCpldcIndicatorNode, plain:uom, UOM, Graph ) ),
          SendCpldcIndicator=xval(SendCpldcIndicatorValue,UOM)
        );
        (
          rdf( SendCpldcIndicatorNode,aixm:nilReason, NilReason, Graph ),
          SendCpldcIndicator=nil(NilReason)
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
          ( rdf( ConnectionStatusNode, aixm:uom, UOM, Graph ); rdf( ConnectionStatusNode, fixm:uom, UOM, Graph ); rdf( ConnectionStatusNode, plain:uom, UOM, Graph ) ),
          ConnectionStatus=xval(ConnectionStatusValue,UOM)
        );
        (
          rdf( ConnectionStatusNode,aixm:nilReason, NilReason, Graph ),
          ConnectionStatus=nil(NilReason)
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
          ( rdf( FrequencyUsageNode, aixm:uom, UOM, Graph ); rdf( FrequencyUsageNode, fixm:uom, UOM, Graph ); rdf( FrequencyUsageNode, plain:uom, UOM, Graph ) ),
          FrequencyUsage=xval(FrequencyUsageValue,UOM)
        );
        (
          rdf( FrequencyUsageNode,aixm:nilReason, NilReason, Graph ),
          FrequencyUsage=nil(NilReason)
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
          ( rdf( Fans1ALogonParametersNode, aixm:uom, UOM, Graph ); rdf( Fans1ALogonParametersNode, fixm:uom, UOM, Graph ); rdf( Fans1ALogonParametersNode, plain:uom, UOM, Graph ) ),
          Fans1ALogonParameters=xval(Fans1ALogonParametersValue,UOM)
        );
        (
          rdf( Fans1ALogonParametersNode,aixm:nilReason, NilReason, Graph ),
          Fans1ALogonParameters=nil(NilReason)
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
          ( rdf( VoiceNode, aixm:uom, UOM, Graph ); rdf( VoiceNode, fixm:uom, UOM, Graph ); rdf( VoiceNode, plain:uom, UOM, Graph ) ),
          Voice=xval(VoiceValue,UOM)
        );
        (
          rdf( VoiceNode,aixm:nilReason, NilReason, Graph ),
          Voice=nil(NilReason)
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
          ( rdf( FacsimileNode, aixm:uom, UOM, Graph ); rdf( FacsimileNode, fixm:uom, UOM, Graph ); rdf( FacsimileNode, plain:uom, UOM, Graph ) ),
          Facsimile=xval(FacsimileValue,UOM)
        );
        (
          rdf( FacsimileNode,aixm:nilReason, NilReason, Graph ),
          Facsimile=nil(NilReason)
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
          ( rdf( AirfileRouteStartTimeNode, aixm:uom, UOM, Graph ); rdf( AirfileRouteStartTimeNode, fixm:uom, UOM, Graph ); rdf( AirfileRouteStartTimeNode, plain:uom, UOM, Graph ) ),
          AirfileRouteStartTime=xval(AirfileRouteStartTimeValue,UOM)
        );
        (
          rdf( AirfileRouteStartTimeNode,aixm:nilReason, NilReason, Graph ),
          AirfileRouteStartTime=nil(NilReason)
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
          ( rdf( FlightDurationNode, aixm:uom, UOM, Graph ); rdf( FlightDurationNode, fixm:uom, UOM, Graph ); rdf( FlightDurationNode, plain:uom, UOM, Graph ) ),
          FlightDuration=xval(FlightDurationValue,UOM)
        );
        (
          rdf( FlightDurationNode,aixm:nilReason, NilReason, Graph ),
          FlightDuration=nil(NilReason)
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
          ( rdf( InitialCruisingSpeedNode, aixm:uom, UOM, Graph ); rdf( InitialCruisingSpeedNode, fixm:uom, UOM, Graph ); rdf( InitialCruisingSpeedNode, plain:uom, UOM, Graph ) ),
          InitialCruisingSpeed=xval(InitialCruisingSpeedValue,UOM)
        );
        (
          rdf( InitialCruisingSpeedNode,aixm:nilReason, NilReason, Graph ),
          InitialCruisingSpeed=nil(NilReason)
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
          ( rdf( InitialFlightRulesNode, aixm:uom, UOM, Graph ); rdf( InitialFlightRulesNode, fixm:uom, UOM, Graph ); rdf( InitialFlightRulesNode, plain:uom, UOM, Graph ) ),
          InitialFlightRules=xval(InitialFlightRulesValue,UOM)
        );
        (
          rdf( InitialFlightRulesNode,aixm:nilReason, NilReason, Graph ),
          InitialFlightRules=nil(NilReason)
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
          ( rdf( RequestedAltitudeNode, aixm:uom, UOM, Graph ); rdf( RequestedAltitudeNode, fixm:uom, UOM, Graph ); rdf( RequestedAltitudeNode, plain:uom, UOM, Graph ) ),
          RequestedAltitude=xval(RequestedAltitudeValue,UOM)
        );
        (
          rdf( RequestedAltitudeNode,aixm:nilReason, NilReason, Graph ),
          RequestedAltitude=nil(NilReason)
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
          ( rdf( RouteTextNode, aixm:uom, UOM, Graph ); rdf( RouteTextNode, fixm:uom, UOM, Graph ); rdf( RouteTextNode, plain:uom, UOM, Graph ) ),
          RouteText=xval(RouteTextValue,UOM)
        );
        (
          rdf( RouteTextNode,aixm:nilReason, NilReason, Graph ),
          RouteText=nil(NilReason)
        )
      )
  )
  ,( EstimatedElapsedTime='$null$',
    \+ rdf( Route,fixm:'estimatedElapsedTime', _EstimatedElapsedTime, Graph )
  )
  ,( ExpandedRoute='$null$',
    \+ rdf( Route,fixm:'expandedRoute', _ExpandedRoute, Graph )
  )
  ,( ClimbSchedule='$null$',
    \+ rdf( Route,fixm:'climbSchedule', _ClimbSchedule, Graph )
  )
  ,( DescentSchedule='$null$',
    \+ rdf( Route,fixm:'descentSchedule', _DescentSchedule, Graph )
  )
  ,( Segment='$null$',
    \+ rdf( Route,fixm:'segment', _Segment, Graph )
  ) .

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
          ( rdf( NameNode, aixm:uom, UOM, Graph ); rdf( NameNode, fixm:uom, UOM, Graph ); rdf( NameNode, plain:uom, UOM, Graph ) ),
          Name=xval(NameValue,UOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NilReason, Graph ),
          Name=nil(NilReason)
        )
      )
  )
  ,( Contact='$null$',
    \+ rdf( Person,fixm:'contact', _Contact, Graph )
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
          ( rdf( IfplIdNode, aixm:uom, UOM, Graph ); rdf( IfplIdNode, fixm:uom, UOM, Graph ); rdf( IfplIdNode, plain:uom, UOM, Graph ) ),
          IfplId=xval(IfplIdValue,UOM)
        );
        (
          rdf( IfplIdNode,aixm:nilReason, NilReason, Graph ),
          IfplId=nil(NilReason)
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
          ( rdf( TotalEstimatedElapsedTimeNode, aixm:uom, UOM, Graph ); rdf( TotalEstimatedElapsedTimeNode, fixm:uom, UOM, Graph ); rdf( TotalEstimatedElapsedTimeNode, plain:uom, UOM, Graph ) ),
          TotalEstimatedElapsedTime=xval(TotalEstimatedElapsedTimeValue,UOM)
        );
        (
          rdf( TotalEstimatedElapsedTimeNode,aixm:nilReason, NilReason, Graph ),
          TotalEstimatedElapsedTime=nil(NilReason)
        )
      )
  )
  ,( AerodromesOfDestination='$null$',
    \+ rdf( EfplFlight,fixm:'aerodromesOfDestination', _AerodromesOfDestination, Graph )
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
          ( rdf( EfplSpecialHandlingNode, aixm:uom, UOM, Graph ); rdf( EfplSpecialHandlingNode, fixm:uom, UOM, Graph ); rdf( EfplSpecialHandlingNode, plain:uom, UOM, Graph ) ),
          EfplSpecialHandling=xval(EfplSpecialHandlingValue,UOM)
        );
        (
          rdf( EfplSpecialHandlingNode,aixm:nilReason, NilReason, Graph ),
          EfplSpecialHandling=nil(NilReason)
        )
      )
  )
  ,( EfplFiledTrajectory='$null$',
    \+ rdf( EfplFlight,fixm:'efplFiledTrajectory', _EfplFiledTrajectory, Graph )
  )
  ,( EfplAcceptedTrajectory='$null$',
    \+ rdf( EfplFlight,fixm:'efplAcceptedTrajectory', _EfplAcceptedTrajectory, Graph )
  )
  ,( OtherInformation='$null$',
    \+ rdf( EfplFlight,fixm:'otherInformation', _OtherInformation, Graph )
  )
  ,( FlightPerformanceData='$null$',
    \+ rdf( EfplFlight,fixm:'flightPerformanceData', _FlightPerformanceData, Graph )
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
          ( rdf( AirborneHoldNode, aixm:uom, UOM, Graph ); rdf( AirborneHoldNode, fixm:uom, UOM, Graph ); rdf( AirborneHoldNode, plain:uom, UOM, Graph ) ),
          AirborneHold=xval(AirborneHoldValue,UOM)
        );
        (
          rdf( AirborneHoldNode,aixm:nilReason, NilReason, Graph ),
          AirborneHold=nil(NilReason)
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
          ( rdf( AirfileNode, aixm:uom, UOM, Graph ); rdf( AirfileNode, fixm:uom, UOM, Graph ); rdf( AirfileNode, plain:uom, UOM, Graph ) ),
          Airfile=xval(AirfileValue,UOM)
        );
        (
          rdf( AirfileNode,aixm:nilReason, NilReason, Graph ),
          Airfile=nil(NilReason)
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
          ( rdf( AcceptedNode, aixm:uom, UOM, Graph ); rdf( AcceptedNode, fixm:uom, UOM, Graph ); rdf( AcceptedNode, plain:uom, UOM, Graph ) ),
          Accepted=xval(AcceptedValue,UOM)
        );
        (
          rdf( AcceptedNode,aixm:nilReason, NilReason, Graph ),
          Accepted=nil(NilReason)
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
          ( rdf( FlightCycleNode, aixm:uom, UOM, Graph ); rdf( FlightCycleNode, fixm:uom, UOM, Graph ); rdf( FlightCycleNode, plain:uom, UOM, Graph ) ),
          FlightCycle=xval(FlightCycleValue,UOM)
        );
        (
          rdf( FlightCycleNode,aixm:nilReason, NilReason, Graph ),
          FlightCycle=nil(NilReason)
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
          ( rdf( MissedApproachNode, aixm:uom, UOM, Graph ); rdf( MissedApproachNode, fixm:uom, UOM, Graph ); rdf( MissedApproachNode, plain:uom, UOM, Graph ) ),
          MissedApproach=xval(MissedApproachValue,UOM)
        );
        (
          rdf( MissedApproachNode,aixm:nilReason, NilReason, Graph ),
          MissedApproach=nil(NilReason)
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
          ( rdf( SuspendedNode, aixm:uom, UOM, Graph ); rdf( SuspendedNode, fixm:uom, UOM, Graph ); rdf( SuspendedNode, plain:uom, UOM, Graph ) ),
          Suspended=xval(SuspendedValue,UOM)
        );
        (
          rdf( SuspendedNode,aixm:nilReason, NilReason, Graph ),
          Suspended=nil(NilReason)
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
          ( rdf( UnitIdentifierNode, aixm:uom, UOM, Graph ); rdf( UnitIdentifierNode, fixm:uom, UOM, Graph ); rdf( UnitIdentifierNode, plain:uom, UOM, Graph ) ),
          UnitIdentifier=xval(UnitIdentifierValue,UOM)
        );
        (
          rdf( UnitIdentifierNode,aixm:nilReason, NilReason, Graph ),
          UnitIdentifier=nil(NilReason)
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
          ( rdf( PhysicalChemicalFormNode, aixm:uom, UOM, Graph ); rdf( PhysicalChemicalFormNode, fixm:uom, UOM, Graph ); rdf( PhysicalChemicalFormNode, plain:uom, UOM, Graph ) ),
          PhysicalChemicalForm=xval(PhysicalChemicalFormValue,UOM)
        );
        (
          rdf( PhysicalChemicalFormNode,aixm:nilReason, NilReason, Graph ),
          PhysicalChemicalForm=nil(NilReason)
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
          ( rdf( RadionuclideIdNode, aixm:uom, UOM, Graph ); rdf( RadionuclideIdNode, fixm:uom, UOM, Graph ); rdf( RadionuclideIdNode, plain:uom, UOM, Graph ) ),
          RadionuclideId=xval(RadionuclideIdValue,UOM)
        );
        (
          rdf( RadionuclideIdNode,aixm:nilReason, NilReason, Graph ),
          RadionuclideId=nil(NilReason)
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
          ( rdf( RadionuclideNameNode, aixm:uom, UOM, Graph ); rdf( RadionuclideNameNode, fixm:uom, UOM, Graph ); rdf( RadionuclideNameNode, plain:uom, UOM, Graph ) ),
          RadionuclideName=xval(RadionuclideNameValue,UOM)
        );
        (
          rdf( RadionuclideNameNode,aixm:nilReason, NilReason, Graph ),
          RadionuclideName=nil(NilReason)
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
          ( rdf( LowDispersibleMaterialIndicatorNode, aixm:uom, UOM, Graph ); rdf( LowDispersibleMaterialIndicatorNode, fixm:uom, UOM, Graph ); rdf( LowDispersibleMaterialIndicatorNode, plain:uom, UOM, Graph ) ),
          LowDispersibleMaterialIndicator=xval(LowDispersibleMaterialIndicatorValue,UOM)
        );
        (
          rdf( LowDispersibleMaterialIndicatorNode,aixm:nilReason, NilReason, Graph ),
          LowDispersibleMaterialIndicator=nil(NilReason)
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
          ( rdf( ActivityNode, aixm:uom, UOM, Graph ); rdf( ActivityNode, fixm:uom, UOM, Graph ); rdf( ActivityNode, plain:uom, UOM, Graph ) ),
          Activity=xval(ActivityValue,UOM)
        );
        (
          rdf( ActivityNode,aixm:nilReason, NilReason, Graph ),
          Activity=nil(NilReason)
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
          ( rdf( SpecialFormIndicatorNode, aixm:uom, UOM, Graph ); rdf( SpecialFormIndicatorNode, fixm:uom, UOM, Graph ); rdf( SpecialFormIndicatorNode, plain:uom, UOM, Graph ) ),
          SpecialFormIndicator=xval(SpecialFormIndicatorValue,UOM)
        );
        (
          rdf( SpecialFormIndicatorNode,aixm:nilReason, NilReason, Graph ),
          SpecialFormIndicator=nil(NilReason)
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
          ( rdf( NetworkNode, aixm:uom, UOM, Graph ); rdf( NetworkNode, fixm:uom, UOM, Graph ); rdf( NetworkNode, plain:uom, UOM, Graph ) ),
          Network=xval(NetworkValue,UOM)
        );
        (
          rdf( NetworkNode,aixm:nilReason, NilReason, Graph ),
          Network=nil(NilReason)
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
          ( rdf( LinkageNode, aixm:uom, UOM, Graph ); rdf( LinkageNode, fixm:uom, UOM, Graph ); rdf( LinkageNode, plain:uom, UOM, Graph ) ),
          Linkage=xval(LinkageValue,UOM)
        );
        (
          rdf( LinkageNode,aixm:nilReason, NilReason, Graph ),
          Linkage=nil(NilReason)
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
          ( rdf( ProtocolNode, aixm:uom, UOM, Graph ); rdf( ProtocolNode, fixm:uom, UOM, Graph ); rdf( ProtocolNode, plain:uom, UOM, Graph ) ),
          Protocol=xval(ProtocolValue,UOM)
        );
        (
          rdf( ProtocolNode,aixm:nilReason, NilReason, Graph ),
          Protocol=nil(NilReason)
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
          ( rdf( EMailNode, aixm:uom, UOM, Graph ); rdf( EMailNode, fixm:uom, UOM, Graph ); rdf( EMailNode, plain:uom, UOM, Graph ) ),
          EMail=xval(EMailValue,UOM)
        );
        (
          rdf( EMailNode,aixm:nilReason, NilReason, Graph ),
          EMail=nil(NilReason)
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
          ( rdf( AltitudeNode, aixm:uom, UOM, Graph ); rdf( AltitudeNode, fixm:uom, UOM, Graph ); rdf( AltitudeNode, plain:uom, UOM, Graph ) ),
          Altitude=xval(AltitudeValue,UOM)
        );
        (
          rdf( AltitudeNode,aixm:nilReason, NilReason, Graph ),
          Altitude=nil(NilReason)
        )
      )
  )
  ,( Position='$null$',
    \+ rdf( AircraftPosition,fixm:'position', _Position, Graph )
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
          ( rdf( PositionTimeNode, aixm:uom, UOM, Graph ); rdf( PositionTimeNode, fixm:uom, UOM, Graph ); rdf( PositionTimeNode, plain:uom, UOM, Graph ) ),
          PositionTime=xval(PositionTimeValue,UOM)
        );
        (
          rdf( PositionTimeNode,aixm:nilReason, NilReason, Graph ),
          PositionTime=nil(NilReason)
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
          ( rdf( TrackNode, aixm:uom, UOM, Graph ); rdf( TrackNode, fixm:uom, UOM, Graph ); rdf( TrackNode, plain:uom, UOM, Graph ) ),
          Track=xval(TrackValue,UOM)
        );
        (
          rdf( TrackNode,aixm:nilReason, NilReason, Graph ),
          Track=nil(NilReason)
        )
      )
  )
  ,( ActualSpeed='$null$',
    \+ rdf( AircraftPosition,fixm:'actualSpeed', _ActualSpeed, Graph )
  )
  ,( NextPosition='$null$',
    \+ rdf( AircraftPosition,fixm:'nextPosition', _NextPosition, Graph )
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
          ( rdf( ReportSourceNode, aixm:uom, UOM, Graph ); rdf( ReportSourceNode, fixm:uom, UOM, Graph ); rdf( ReportSourceNode, plain:uom, UOM, Graph ) ),
          ReportSource=xval(ReportSourceValue,UOM)
        );
        (
          rdf( ReportSourceNode,aixm:nilReason, NilReason, Graph ),
          ReportSource=nil(NilReason)
        )
      )
  )
  ,( FollowingPosition='$null$',
    \+ rdf( AircraftPosition,fixm:'followingPosition', _FollowingPosition, Graph )
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
          ( rdf( OperationNode, aixm:uom, UOM, Graph ); rdf( OperationNode, fixm:uom, UOM, Graph ); rdf( OperationNode, plain:uom, UOM, Graph ) ),
          Operation=xval(OperationValue,UOM)
        );
        (
          rdf( OperationNode,aixm:nilReason, NilReason, Graph ),
          Operation=nil(NilReason)
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
          ( rdf( TimeReferenceNode, aixm:uom, UOM, Graph ); rdf( TimeReferenceNode, fixm:uom, UOM, Graph ); rdf( TimeReferenceNode, plain:uom, UOM, Graph ) ),
          TimeReference=xval(TimeReferenceValue,UOM)
        );
        (
          rdf( TimeReferenceNode,aixm:nilReason, NilReason, Graph ),
          TimeReference=nil(NilReason)
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
          ( rdf( StartDateNode, aixm:uom, UOM, Graph ); rdf( StartDateNode, fixm:uom, UOM, Graph ); rdf( StartDateNode, plain:uom, UOM, Graph ) ),
          StartDate=xval(StartDateValue,UOM)
        );
        (
          rdf( StartDateNode,aixm:nilReason, NilReason, Graph ),
          StartDate=nil(NilReason)
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
          ( rdf( EndDateNode, aixm:uom, UOM, Graph ); rdf( EndDateNode, fixm:uom, UOM, Graph ); rdf( EndDateNode, plain:uom, UOM, Graph ) ),
          EndDate=xval(EndDateValue,UOM)
        );
        (
          rdf( EndDateNode,aixm:nilReason, NilReason, Graph ),
          EndDate=nil(NilReason)
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
          ( rdf( DayNode, aixm:uom, UOM, Graph ); rdf( DayNode, fixm:uom, UOM, Graph ); rdf( DayNode, plain:uom, UOM, Graph ) ),
          Day=xval(DayValue,UOM)
        );
        (
          rdf( DayNode,aixm:nilReason, NilReason, Graph ),
          Day=nil(NilReason)
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
          ( rdf( DayTilNode, aixm:uom, UOM, Graph ); rdf( DayTilNode, fixm:uom, UOM, Graph ); rdf( DayTilNode, plain:uom, UOM, Graph ) ),
          DayTil=xval(DayTilValue,UOM)
        );
        (
          rdf( DayTilNode,aixm:nilReason, NilReason, Graph ),
          DayTil=nil(NilReason)
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
          ( rdf( StartTimeNode, aixm:uom, UOM, Graph ); rdf( StartTimeNode, fixm:uom, UOM, Graph ); rdf( StartTimeNode, plain:uom, UOM, Graph ) ),
          StartTime=xval(StartTimeValue,UOM)
        );
        (
          rdf( StartTimeNode,aixm:nilReason, NilReason, Graph ),
          StartTime=nil(NilReason)
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
          ( rdf( StartEventNode, aixm:uom, UOM, Graph ); rdf( StartEventNode, fixm:uom, UOM, Graph ); rdf( StartEventNode, plain:uom, UOM, Graph ) ),
          StartEvent=xval(StartEventValue,UOM)
        );
        (
          rdf( StartEventNode,aixm:nilReason, NilReason, Graph ),
          StartEvent=nil(NilReason)
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
          ( rdf( StartTimeRelativeEventNode, aixm:uom, UOM, Graph ); rdf( StartTimeRelativeEventNode, fixm:uom, UOM, Graph ); rdf( StartTimeRelativeEventNode, plain:uom, UOM, Graph ) ),
          StartTimeRelativeEvent=xval(StartTimeRelativeEventValue,UOM)
        );
        (
          rdf( StartTimeRelativeEventNode,aixm:nilReason, NilReason, Graph ),
          StartTimeRelativeEvent=nil(NilReason)
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
          ( rdf( StartEventInterpretationNode, aixm:uom, UOM, Graph ); rdf( StartEventInterpretationNode, fixm:uom, UOM, Graph ); rdf( StartEventInterpretationNode, plain:uom, UOM, Graph ) ),
          StartEventInterpretation=xval(StartEventInterpretationValue,UOM)
        );
        (
          rdf( StartEventInterpretationNode,aixm:nilReason, NilReason, Graph ),
          StartEventInterpretation=nil(NilReason)
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
          ( rdf( EndTimeNode, aixm:uom, UOM, Graph ); rdf( EndTimeNode, fixm:uom, UOM, Graph ); rdf( EndTimeNode, plain:uom, UOM, Graph ) ),
          EndTime=xval(EndTimeValue,UOM)
        );
        (
          rdf( EndTimeNode,aixm:nilReason, NilReason, Graph ),
          EndTime=nil(NilReason)
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
          ( rdf( EndEventNode, aixm:uom, UOM, Graph ); rdf( EndEventNode, fixm:uom, UOM, Graph ); rdf( EndEventNode, plain:uom, UOM, Graph ) ),
          EndEvent=xval(EndEventValue,UOM)
        );
        (
          rdf( EndEventNode,aixm:nilReason, NilReason, Graph ),
          EndEvent=nil(NilReason)
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
          ( rdf( EndTimeRelativeEventNode, aixm:uom, UOM, Graph ); rdf( EndTimeRelativeEventNode, fixm:uom, UOM, Graph ); rdf( EndTimeRelativeEventNode, plain:uom, UOM, Graph ) ),
          EndTimeRelativeEvent=xval(EndTimeRelativeEventValue,UOM)
        );
        (
          rdf( EndTimeRelativeEventNode,aixm:nilReason, NilReason, Graph ),
          EndTimeRelativeEvent=nil(NilReason)
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
          ( rdf( EndEventInterpretationNode, aixm:uom, UOM, Graph ); rdf( EndEventInterpretationNode, fixm:uom, UOM, Graph ); rdf( EndEventInterpretationNode, plain:uom, UOM, Graph ) ),
          EndEventInterpretation=xval(EndEventInterpretationValue,UOM)
        );
        (
          rdf( EndEventInterpretationNode,aixm:nilReason, NilReason, Graph ),
          EndEventInterpretation=nil(NilReason)
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
          ( rdf( DaylightSavingAdjustNode, aixm:uom, UOM, Graph ); rdf( DaylightSavingAdjustNode, fixm:uom, UOM, Graph ); rdf( DaylightSavingAdjustNode, plain:uom, UOM, Graph ) ),
          DaylightSavingAdjust=xval(DaylightSavingAdjustValue,UOM)
        );
        (
          rdf( DaylightSavingAdjustNode,aixm:nilReason, NilReason, Graph ),
          DaylightSavingAdjust=nil(NilReason)
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
          ( rdf( ExcludedNode, aixm:uom, UOM, Graph ); rdf( ExcludedNode, fixm:uom, UOM, Graph ); rdf( ExcludedNode, plain:uom, UOM, Graph ) ),
          Excluded=xval(ExcludedValue,UOM)
        );
        (
          rdf( ExcludedNode,aixm:nilReason, NilReason, Graph ),
          Excluded=nil(NilReason)
        )
      )
  )
  ,( Annotation='$null$',
    \+ rdf( Timesheet,aixm:'annotation', _Annotation, Graph )
  ) .

gml_SurfacePatch(Graph, SurfacePatch) :-
  subClassOf(T,gml:'SurfacePatch')
  ,rdf(SurfacePatch,rdf:type,T,Graph) .

fixm_MultiTime(Graph, MultiTime, Actual, Estimated) :-
  subClassOf(T,fixm:'MultiTime')
  ,rdf(MultiTime,rdf:type,T,Graph)
  ,( Actual='$null$',
    \+ rdf( MultiTime,fixm:'actual', _Actual, Graph )
  )
  ,( Estimated='$null$',
    \+ rdf( MultiTime,fixm:'estimated', _Estimated, Graph )
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
          ( rdf( TypeNode, aixm:uom, UOM, Graph ); rdf( TypeNode, fixm:uom, UOM, Graph ); rdf( TypeNode, plain:uom, UOM, Graph ) ),
          Type=xval(TypeValue,UOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, NilReason, Graph ),
          Type=nil(NilReason)
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
          ( rdf( RuleNode, aixm:uom, UOM, Graph ); rdf( RuleNode, fixm:uom, UOM, Graph ); rdf( RuleNode, plain:uom, UOM, Graph ) ),
          Rule=xval(RuleValue,UOM)
        );
        (
          rdf( RuleNode,aixm:nilReason, NilReason, Graph ),
          Rule=nil(NilReason)
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
          ( rdf( StatusNode, aixm:uom, UOM, Graph ); rdf( StatusNode, fixm:uom, UOM, Graph ); rdf( StatusNode, plain:uom, UOM, Graph ) ),
          Status=xval(StatusValue,UOM)
        );
        (
          rdf( StatusNode,aixm:nilReason, NilReason, Graph ),
          Status=nil(NilReason)
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
          ( rdf( MilitaryNode, aixm:uom, UOM, Graph ); rdf( MilitaryNode, fixm:uom, UOM, Graph ); rdf( MilitaryNode, plain:uom, UOM, Graph ) ),
          Military=xval(MilitaryValue,UOM)
        );
        (
          rdf( MilitaryNode,aixm:nilReason, NilReason, Graph ),
          Military=nil(NilReason)
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
          ( rdf( OriginNode, aixm:uom, UOM, Graph ); rdf( OriginNode, fixm:uom, UOM, Graph ); rdf( OriginNode, plain:uom, UOM, Graph ) ),
          Origin=xval(OriginValue,UOM)
        );
        (
          rdf( OriginNode,aixm:nilReason, NilReason, Graph ),
          Origin=nil(NilReason)
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
          ( rdf( PurposeNode, aixm:uom, UOM, Graph ); rdf( PurposeNode, fixm:uom, UOM, Graph ); rdf( PurposeNode, plain:uom, UOM, Graph ) ),
          Purpose=xval(PurposeValue,UOM)
        );
        (
          rdf( PurposeNode,aixm:nilReason, NilReason, Graph ),
          Purpose=nil(NilReason)
        )
      )
  )
  ,( Annotation='$null$',
    \+ rdf( FlightCharacteristic,aixm:'annotation', _Annotation, Graph )
  ) .

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
          ( rdf( TimestampNode, aixm:uom, UOM, Graph ); rdf( TimestampNode, fixm:uom, UOM, Graph ); rdf( TimestampNode, plain:uom, UOM, Graph ) ),
          Timestamp=xval(TimestampValue,UOM)
        );
        (
          rdf( TimestampNode,aixm:nilReason, NilReason, Graph ),
          Timestamp=nil(NilReason)
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
          ( rdf( CentreNode, aixm:uom, UOM, Graph ); rdf( CentreNode, fixm:uom, UOM, Graph ); rdf( CentreNode, plain:uom, UOM, Graph ) ),
          Centre=xval(CentreValue,UOM)
        );
        (
          rdf( CentreNode,aixm:nilReason, NilReason, Graph ),
          Centre=nil(NilReason)
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
          ( rdf( SourceNode, aixm:uom, UOM, Graph ); rdf( SourceNode, fixm:uom, UOM, Graph ); rdf( SourceNode, plain:uom, UOM, Graph ) ),
          Source=xval(SourceValue,UOM)
        );
        (
          rdf( SourceNode,aixm:nilReason, NilReason, Graph ),
          Source=nil(NilReason)
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
          ( rdf( SystemNode, aixm:uom, UOM, Graph ); rdf( SystemNode, fixm:uom, UOM, Graph ); rdf( SystemNode, plain:uom, UOM, Graph ) ),
          System=xval(SystemValue,UOM)
        );
        (
          rdf( SystemNode,aixm:nilReason, NilReason, Graph ),
          System=nil(NilReason)
        )
      )
  ) .

aixm_AirportHeliport(Graph, AirportHeliport, TimeSlice) :-
  rdf(AirportHeliport,rdf:type,aixm:'AirportHeliport',Graph)
  ,( TimeSlice='$null$',
    \+ rdf( AirportHeliport,aixm:'timeSlice', _TimeSlice, Graph )
  ) .

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
          ( rdf( AltimeterSettingNode, aixm:uom, UOM, Graph ); rdf( AltimeterSettingNode, fixm:uom, UOM, Graph ); rdf( AltimeterSettingNode, plain:uom, UOM, Graph ) ),
          AltimeterSetting=xval(AltimeterSettingValue,UOM)
        );
        (
          rdf( AltimeterSettingNode,aixm:nilReason, NilReason, Graph ),
          AltimeterSetting=nil(NilReason)
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
          ( rdf( PredictedAirspeedNode, aixm:uom, UOM, Graph ); rdf( PredictedAirspeedNode, fixm:uom, UOM, Graph ); rdf( PredictedAirspeedNode, plain:uom, UOM, Graph ) ),
          PredictedAirspeed=xval(PredictedAirspeedValue,UOM)
        );
        (
          rdf( PredictedAirspeedNode,aixm:nilReason, NilReason, Graph ),
          PredictedAirspeed=nil(NilReason)
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
          ( rdf( PredictedGroundspeedNode, aixm:uom, UOM, Graph ); rdf( PredictedGroundspeedNode, fixm:uom, UOM, Graph ); rdf( PredictedGroundspeedNode, plain:uom, UOM, Graph ) ),
          PredictedGroundspeed=xval(PredictedGroundspeedValue,UOM)
        );
        (
          rdf( PredictedGroundspeedNode,aixm:nilReason, NilReason, Graph ),
          PredictedGroundspeed=nil(NilReason)
        )
      )
  )
  ,( MetData='$null$',
    \+ rdf( TrajectoryPoint,fixm:'metData', _MetData, Graph )
  )
  ,( Point='$null$',
    \+ rdf( TrajectoryPoint,fixm:'point', _Point, Graph )
  )
  ,( TrajectoryChange='$null$',
    \+ rdf( TrajectoryPoint,fixm:'trajectoryChange', _TrajectoryChange, Graph )
  )
  ,findall(A, rdf(TrajectoryPoint,fixm:'trajectoryChangeType',A,Graph), TrajectoryChangeType)
  ,( ReferencePoint='$null$',
    \+ rdf( TrajectoryPoint,fixm:'referencePoint', _ReferencePoint, Graph )
  ) .

fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier, DistanceFromTakeOff, EfplEstimatedSpeed, ElapsedTime, GrossWeight, TrajectoryPointType, TrajectoryPointRole, InboundSegment) :-
  rdf(EfplTrajectoryPoint,rdf:type,fixm:'EfplTrajectoryPoint',Graph)
  ,( AerodromeIdentifier='$null$',
    \+ rdf( EfplTrajectoryPoint,fixm:'aerodromeIdentifier', _AerodromeIdentifier, Graph )
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
          ( rdf( DistanceFromTakeOffNode, aixm:uom, UOM, Graph ); rdf( DistanceFromTakeOffNode, fixm:uom, UOM, Graph ); rdf( DistanceFromTakeOffNode, plain:uom, UOM, Graph ) ),
          DistanceFromTakeOff=xval(DistanceFromTakeOffValue,UOM)
        );
        (
          rdf( DistanceFromTakeOffNode,aixm:nilReason, NilReason, Graph ),
          DistanceFromTakeOff=nil(NilReason)
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
          ( rdf( EfplEstimatedSpeedNode, aixm:uom, UOM, Graph ); rdf( EfplEstimatedSpeedNode, fixm:uom, UOM, Graph ); rdf( EfplEstimatedSpeedNode, plain:uom, UOM, Graph ) ),
          EfplEstimatedSpeed=xval(EfplEstimatedSpeedValue,UOM)
        );
        (
          rdf( EfplEstimatedSpeedNode,aixm:nilReason, NilReason, Graph ),
          EfplEstimatedSpeed=nil(NilReason)
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
          ( rdf( ElapsedTimeNode, aixm:uom, UOM, Graph ); rdf( ElapsedTimeNode, fixm:uom, UOM, Graph ); rdf( ElapsedTimeNode, plain:uom, UOM, Graph ) ),
          ElapsedTime=xval(ElapsedTimeValue,UOM)
        );
        (
          rdf( ElapsedTimeNode,aixm:nilReason, NilReason, Graph ),
          ElapsedTime=nil(NilReason)
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
          ( rdf( GrossWeightNode, aixm:uom, UOM, Graph ); rdf( GrossWeightNode, fixm:uom, UOM, Graph ); rdf( GrossWeightNode, plain:uom, UOM, Graph ) ),
          GrossWeight=xval(GrossWeightValue,UOM)
        );
        (
          rdf( GrossWeightNode,aixm:nilReason, NilReason, Graph ),
          GrossWeight=nil(NilReason)
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
          ( rdf( TrajectoryPointTypeNode, aixm:uom, UOM, Graph ); rdf( TrajectoryPointTypeNode, fixm:uom, UOM, Graph ); rdf( TrajectoryPointTypeNode, plain:uom, UOM, Graph ) ),
          TrajectoryPointType=xval(TrajectoryPointTypeValue,UOM)
        );
        (
          rdf( TrajectoryPointTypeNode,aixm:nilReason, NilReason, Graph ),
          TrajectoryPointType=nil(NilReason)
        )
      )
  )
  ,( TrajectoryPointRole='$null$',
    \+ rdf( EfplTrajectoryPoint,fixm:'trajectoryPointRole', _TrajectoryPointRole, Graph )
  )
  ,( InboundSegment='$null$',
    \+ rdf( EfplTrajectoryPoint,fixm:'inboundSegment', _InboundSegment, Graph )
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
          ( rdf( ControlTemperatureNode, aixm:uom, UOM, Graph ); rdf( ControlTemperatureNode, fixm:uom, UOM, Graph ); rdf( ControlTemperatureNode, plain:uom, UOM, Graph ) ),
          ControlTemperature=xval(ControlTemperatureValue,UOM)
        );
        (
          rdf( ControlTemperatureNode,aixm:nilReason, NilReason, Graph ),
          ControlTemperature=nil(NilReason)
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
          ( rdf( EmergencyTemperatureNode, aixm:uom, UOM, Graph ); rdf( EmergencyTemperatureNode, fixm:uom, UOM, Graph ); rdf( EmergencyTemperatureNode, plain:uom, UOM, Graph ) ),
          EmergencyTemperature=xval(EmergencyTemperatureValue,UOM)
        );
        (
          rdf( EmergencyTemperatureNode,aixm:nilReason, NilReason, Graph ),
          EmergencyTemperature=nil(NilReason)
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
          ( rdf( FlashpointTemperatureNode, aixm:uom, UOM, Graph ); rdf( FlashpointTemperatureNode, fixm:uom, UOM, Graph ); rdf( FlashpointTemperatureNode, plain:uom, UOM, Graph ) ),
          FlashpointTemperature=xval(FlashpointTemperatureValue,UOM)
        );
        (
          rdf( FlashpointTemperatureNode,aixm:nilReason, NilReason, Graph ),
          FlashpointTemperature=nil(NilReason)
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
          ( rdf( SegmentIdentifierNode, aixm:uom, UOM, Graph ); rdf( SegmentIdentifierNode, fixm:uom, UOM, Graph ); rdf( SegmentIdentifierNode, plain:uom, UOM, Graph ) ),
          SegmentIdentifier=xval(SegmentIdentifierValue,UOM)
        );
        (
          rdf( SegmentIdentifierNode,aixm:nilReason, NilReason, Graph ),
          SegmentIdentifier=nil(NilReason)
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
          ( rdf( SegmentTypeNode, aixm:uom, UOM, Graph ); rdf( SegmentTypeNode, fixm:uom, UOM, Graph ); rdf( SegmentTypeNode, plain:uom, UOM, Graph ) ),
          SegmentType=xval(SegmentTypeValue,UOM)
        );
        (
          rdf( SegmentTypeNode,aixm:nilReason, NilReason, Graph ),
          SegmentType=nil(NilReason)
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
          ( rdf( RunwayNameNode, aixm:uom, UOM, Graph ); rdf( RunwayNameNode, fixm:uom, UOM, Graph ); rdf( RunwayNameNode, plain:uom, UOM, Graph ) ),
          RunwayName=xval(RunwayNameValue,UOM)
        );
        (
          rdf( RunwayNameNode,aixm:nilReason, NilReason, Graph ),
          RunwayName=nil(NilReason)
        )
      )
  )
  ,( RunwayTime='$null$',
    \+ rdf( RunwayPositionAndTime,fixm:'runwayTime', _RunwayTime, Graph )
  ) .

fixm_Feature(Graph, Feature, Provenance) :-
  subClassOf(T,fixm:'Feature')
  ,rdf(Feature,rdf:type,T,Graph)
  ,( Provenance='$null$',
    \+ rdf( Feature,fixm:'provenance', _Provenance, Graph )
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
          ( rdf( AircraftIdentificationNode, aixm:uom, UOM, Graph ); rdf( AircraftIdentificationNode, fixm:uom, UOM, Graph ); rdf( AircraftIdentificationNode, plain:uom, UOM, Graph ) ),
          AircraftIdentification=xval(AircraftIdentificationValue,UOM)
        );
        (
          rdf( AircraftIdentificationNode,aixm:nilReason, NilReason, Graph ),
          AircraftIdentification=nil(NilReason)
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
          ( rdf( MajorCarrierIdentifierNode, aixm:uom, UOM, Graph ); rdf( MajorCarrierIdentifierNode, fixm:uom, UOM, Graph ); rdf( MajorCarrierIdentifierNode, plain:uom, UOM, Graph ) ),
          MajorCarrierIdentifier=xval(MajorCarrierIdentifierValue,UOM)
        );
        (
          rdf( MajorCarrierIdentifierNode,aixm:nilReason, NilReason, Graph ),
          MajorCarrierIdentifier=nil(NilReason)
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
          ( rdf( ContactFrequencyNode, aixm:uom, UOM, Graph ); rdf( ContactFrequencyNode, fixm:uom, UOM, Graph ); rdf( ContactFrequencyNode, plain:uom, UOM, Graph ) ),
          ContactFrequency=xval(ContactFrequencyValue,UOM)
        );
        (
          rdf( ContactFrequencyNode,aixm:nilReason, NilReason, Graph ),
          ContactFrequency=nil(NilReason)
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
          ( rdf( LastContactTimeNode, aixm:uom, UOM, Graph ); rdf( LastContactTimeNode, fixm:uom, UOM, Graph ); rdf( LastContactTimeNode, plain:uom, UOM, Graph ) ),
          LastContactTime=xval(LastContactTimeValue,UOM)
        );
        (
          rdf( LastContactTimeNode,aixm:nilReason, NilReason, Graph ),
          LastContactTime=nil(NilReason)
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
          ( rdf( LastContactUnitNode, aixm:uom, UOM, Graph ); rdf( LastContactUnitNode, fixm:uom, UOM, Graph ); rdf( LastContactUnitNode, plain:uom, UOM, Graph ) ),
          LastContactUnit=xval(LastContactUnitValue,UOM)
        );
        (
          rdf( LastContactUnitNode,aixm:nilReason, NilReason, Graph ),
          LastContactUnit=nil(NilReason)
        )
      )
  )
  ,( Position='$null$',
    \+ rdf( LastContact,fixm:'position', _Position, Graph )
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
          ( rdf( HorizontalAccuracyNode, aixm:uom, UOM, Graph ); rdf( HorizontalAccuracyNode, fixm:uom, UOM, Graph ); rdf( HorizontalAccuracyNode, plain:uom, UOM, Graph ) ),
          HorizontalAccuracy=xval(HorizontalAccuracyValue,UOM)
        );
        (
          rdf( HorizontalAccuracyNode,aixm:nilReason, NilReason, Graph ),
          HorizontalAccuracy=nil(NilReason)
        )
      )
  )
  ,( Annotation='$null$',
    \+ rdf( Surface,aixm:'annotation', _Annotation, Graph )
  ) .

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
          ( rdf( BeginPositionNode, aixm:uom, UOM, Graph ); rdf( BeginPositionNode, fixm:uom, UOM, Graph ); rdf( BeginPositionNode, plain:uom, UOM, Graph ) ),
          BeginPosition=xval(BeginPositionValue,UOM)
        );
        (
          rdf( BeginPositionNode,aixm:nilReason, NilReason, Graph ),
          BeginPosition=nil(NilReason)
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
          ( rdf( EndPositionNode, aixm:uom, UOM, Graph ); rdf( EndPositionNode, fixm:uom, UOM, Graph ); rdf( EndPositionNode, plain:uom, UOM, Graph ) ),
          EndPosition=xval(EndPositionValue,UOM)
        );
        (
          rdf( EndPositionNode,aixm:nilReason, NilReason, Graph ),
          EndPosition=nil(NilReason)
        )
      )
  ) .

fixm_AircraftCapabilities(Graph, AircraftCapabilities, Survival, Communication, Navigation, Surveillance, StandardCapabilities) :-
  rdf(AircraftCapabilities,rdf:type,fixm:'AircraftCapabilities',Graph)
  ,( Survival='$null$',
    \+ rdf( AircraftCapabilities,fixm:'survival', _Survival, Graph )
  )
  ,( Communication='$null$',
    \+ rdf( AircraftCapabilities,fixm:'communication', _Communication, Graph )
  )
  ,( Navigation='$null$',
    \+ rdf( AircraftCapabilities,fixm:'navigation', _Navigation, Graph )
  )
  ,( Surveillance='$null$',
    \+ rdf( AircraftCapabilities,fixm:'surveillance', _Surveillance, Graph )
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
          ( rdf( StandardCapabilitiesNode, aixm:uom, UOM, Graph ); rdf( StandardCapabilitiesNode, fixm:uom, UOM, Graph ); rdf( StandardCapabilitiesNode, plain:uom, UOM, Graph ) ),
          StandardCapabilities=xval(StandardCapabilitiesValue,UOM)
        );
        (
          rdf( StandardCapabilitiesNode,aixm:nilReason, NilReason, Graph ),
          StandardCapabilities=nil(NilReason)
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
          ( rdf( InitialSpeedNode, aixm:uom, UOM, Graph ); rdf( InitialSpeedNode, fixm:uom, UOM, Graph ); rdf( InitialSpeedNode, plain:uom, UOM, Graph ) ),
          InitialSpeed=xval(InitialSpeedValue,UOM)
        );
        (
          rdf( InitialSpeedNode,aixm:nilReason, NilReason, Graph ),
          InitialSpeed=nil(NilReason)
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
          ( rdf( SubsequentSpeedNode, aixm:uom, UOM, Graph ); rdf( SubsequentSpeedNode, fixm:uom, UOM, Graph ); rdf( SubsequentSpeedNode, plain:uom, UOM, Graph ) ),
          SubsequentSpeed=xval(SubsequentSpeedValue,UOM)
        );
        (
          rdf( SubsequentSpeedNode,aixm:nilReason, NilReason, Graph ),
          SubsequentSpeed=nil(NilReason)
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
          ( rdf( NameNode, aixm:uom, UOM, Graph ); rdf( NameNode, fixm:uom, UOM, Graph ); rdf( NameNode, plain:uom, UOM, Graph ) ),
          Name=xval(NameValue,UOM)
        );
        (
          rdf( NameNode,aixm:nilReason, NilReason, Graph ),
          Name=nil(NilReason)
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
          ( rdf( DesignatorNode, aixm:uom, UOM, Graph ); rdf( DesignatorNode, fixm:uom, UOM, Graph ); rdf( DesignatorNode, plain:uom, UOM, Graph ) ),
          Designator=xval(DesignatorValue,UOM)
        );
        (
          rdf( DesignatorNode,aixm:nilReason, NilReason, Graph ),
          Designator=nil(NilReason)
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
          ( rdf( TypeNode, aixm:uom, UOM, Graph ); rdf( TypeNode, fixm:uom, UOM, Graph ); rdf( TypeNode, plain:uom, UOM, Graph ) ),
          Type=xval(TypeValue,UOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, NilReason, Graph ),
          Type=nil(NilReason)
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
          ( rdf( MilitaryNode, aixm:uom, UOM, Graph ); rdf( MilitaryNode, fixm:uom, UOM, Graph ); rdf( MilitaryNode, plain:uom, UOM, Graph ) ),
          Military=xval(MilitaryValue,UOM)
        );
        (
          rdf( MilitaryNode,aixm:nilReason, NilReason, Graph ),
          Military=nil(NilReason)
        )
      )
  )
  ,( Annotation='$null$',
    \+ rdf( OrganisationAuthorityTimeSlice,aixm:'annotation', _Annotation, Graph )
  )
  ,( Contact='$null$',
    \+ rdf( OrganisationAuthorityTimeSlice,aixm:'contact', _Contact, Graph )
  )
  ,( RelatedOrganisationAuthority='$null$',
    \+ rdf( OrganisationAuthorityTimeSlice,aixm:'relatedOrganisationAuthority', _RelatedOrganisationAuthority, Graph )
  ) .

fixm_EnRoute(Graph, EnRoute, AlternateAerodrome, FleetPrioritization, BoundaryCrossings, CpdlcConnection, BeaconCodeAssignment, Cleared, ControlElement, Pointout, Position) :-
  rdf(EnRoute,rdf:type,fixm:'EnRoute',Graph)
  ,( AlternateAerodrome='$null$',
    \+ rdf( EnRoute,fixm:'alternateAerodrome', _AlternateAerodrome, Graph )
  )
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
          ( rdf( FleetPrioritizationNode, aixm:uom, UOM, Graph ); rdf( FleetPrioritizationNode, fixm:uom, UOM, Graph ); rdf( FleetPrioritizationNode, plain:uom, UOM, Graph ) ),
          FleetPrioritization=xval(FleetPrioritizationValue,UOM)
        );
        (
          rdf( FleetPrioritizationNode,aixm:nilReason, NilReason, Graph ),
          FleetPrioritization=nil(NilReason)
        )
      )
  )
  ,( BoundaryCrossings='$null$',
    \+ rdf( EnRoute,fixm:'boundaryCrossings', _BoundaryCrossings, Graph )
  )
  ,( CpdlcConnection='$null$',
    \+ rdf( EnRoute,fixm:'cpdlcConnection', _CpdlcConnection, Graph )
  )
  ,( BeaconCodeAssignment='$null$',
    \+ rdf( EnRoute,fixm:'beaconCodeAssignment', _BeaconCodeAssignment, Graph )
  )
  ,( Cleared='$null$',
    \+ rdf( EnRoute,fixm:'cleared', _Cleared, Graph )
  )
  ,findall(A, rdf(EnRoute,fixm:'controlElement',A,Graph), ControlElement)
  ,( Pointout='$null$',
    \+ rdf( EnRoute,fixm:'pointout', _Pointout, Graph )
  )
  ,( Position='$null$',
    \+ rdf( EnRoute,fixm:'position', _Position, Graph )
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
          ( rdf( LevelNode, aixm:uom, UOM, Graph ); rdf( LevelNode, fixm:uom, UOM, Graph ); rdf( LevelNode, plain:uom, UOM, Graph ) ),
          Level=xval(LevelValue,UOM)
        );
        (
          rdf( LevelNode,aixm:nilReason, NilReason, Graph ),
          Level=nil(NilReason)
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
          ( rdf( UnitNode, aixm:uom, UOM, Graph ); rdf( UnitNode, fixm:uom, UOM, Graph ); rdf( UnitNode, plain:uom, UOM, Graph ) ),
          Unit=xval(UnitValue,UOM)
        );
        (
          rdf( UnitNode,aixm:nilReason, NilReason, Graph ),
          Unit=nil(NilReason)
        )
      )
  ) .

fixm_LateralOfftrack(Graph, LateralOfftrack, OfftrackDistance, OfftrackReason) :-
  rdf(LateralOfftrack,rdf:type,fixm:'LateralOfftrack',Graph)
  ,( OfftrackDistance='$null$',
    \+ rdf( LateralOfftrack,fixm:'offtrackDistance', _OfftrackDistance, Graph )
  )
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
          ( rdf( OfftrackReasonNode, aixm:uom, UOM, Graph ); rdf( OfftrackReasonNode, fixm:uom, UOM, Graph ); rdf( OfftrackReasonNode, plain:uom, UOM, Graph ) ),
          OfftrackReason=xval(OfftrackReasonValue,UOM)
        );
        (
          rdf( OfftrackReasonNode,aixm:nilReason, NilReason, Graph ),
          OfftrackReason=nil(NilReason)
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
          ( rdf( EarliestNode, aixm:uom, UOM, Graph ); rdf( EarliestNode, fixm:uom, UOM, Graph ); rdf( EarliestNode, plain:uom, UOM, Graph ) ),
          Earliest=xval(EarliestValue,UOM)
        );
        (
          rdf( EarliestNode,aixm:nilReason, NilReason, Graph ),
          Earliest=nil(NilReason)
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
          ( rdf( LatestNode, aixm:uom, UOM, Graph ); rdf( LatestNode, fixm:uom, UOM, Graph ); rdf( LatestNode, plain:uom, UOM, Graph ) ),
          Latest=xval(LatestValue,UOM)
        );
        (
          rdf( LatestNode,aixm:nilReason, NilReason, Graph ),
          Latest=nil(NilReason)
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
          ( rdf( AircraftColoursNode, aixm:uom, UOM, Graph ); rdf( AircraftColoursNode, fixm:uom, UOM, Graph ); rdf( AircraftColoursNode, plain:uom, UOM, Graph ) ),
          AircraftColours=xval(AircraftColoursValue,UOM)
        );
        (
          rdf( AircraftColoursNode,aixm:nilReason, NilReason, Graph ),
          AircraftColours=nil(NilReason)
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
          ( rdf( AircraftQuantityNode, aixm:uom, UOM, Graph ); rdf( AircraftQuantityNode, fixm:uom, UOM, Graph ); rdf( AircraftQuantityNode, plain:uom, UOM, Graph ) ),
          AircraftQuantity=xval(AircraftQuantityValue,UOM)
        );
        (
          rdf( AircraftQuantityNode,aixm:nilReason, NilReason, Graph ),
          AircraftQuantity=nil(NilReason)
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
          ( rdf( EngineTypeNode, aixm:uom, UOM, Graph ); rdf( EngineTypeNode, fixm:uom, UOM, Graph ); rdf( EngineTypeNode, plain:uom, UOM, Graph ) ),
          EngineType=xval(EngineTypeValue,UOM)
        );
        (
          rdf( EngineTypeNode,aixm:nilReason, NilReason, Graph ),
          EngineType=nil(NilReason)
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
          ( rdf( AircraftAddressNode, aixm:uom, UOM, Graph ); rdf( AircraftAddressNode, fixm:uom, UOM, Graph ); rdf( AircraftAddressNode, plain:uom, UOM, Graph ) ),
          AircraftAddress=xval(AircraftAddressValue,UOM)
        );
        (
          rdf( AircraftAddressNode,aixm:nilReason, NilReason, Graph ),
          AircraftAddress=nil(NilReason)
        )
      )
  )
  ,( Capabilities='$null$',
    \+ rdf( Aircraft,fixm:'capabilities', _Capabilities, Graph )
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
          ( rdf( RegistrationNode, aixm:uom, UOM, Graph ); rdf( RegistrationNode, fixm:uom, UOM, Graph ); rdf( RegistrationNode, plain:uom, UOM, Graph ) ),
          Registration=xval(RegistrationValue,UOM)
        );
        (
          rdf( RegistrationNode,aixm:nilReason, NilReason, Graph ),
          Registration=nil(NilReason)
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
          ( rdf( AircraftTypeNode, aixm:uom, UOM, Graph ); rdf( AircraftTypeNode, fixm:uom, UOM, Graph ); rdf( AircraftTypeNode, plain:uom, UOM, Graph ) ),
          AircraftType=xval(AircraftTypeValue,UOM)
        );
        (
          rdf( AircraftTypeNode,aixm:nilReason, NilReason, Graph ),
          AircraftType=nil(NilReason)
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
          ( rdf( WakeTurbulenceNode, aixm:uom, UOM, Graph ); rdf( WakeTurbulenceNode, fixm:uom, UOM, Graph ); rdf( WakeTurbulenceNode, plain:uom, UOM, Graph ) ),
          WakeTurbulence=xval(WakeTurbulenceValue,UOM)
        );
        (
          rdf( WakeTurbulenceNode,aixm:nilReason, NilReason, Graph ),
          WakeTurbulence=nil(NilReason)
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
          ( rdf( AircraftPerformanceNode, aixm:uom, UOM, Graph ); rdf( AircraftPerformanceNode, fixm:uom, UOM, Graph ); rdf( AircraftPerformanceNode, plain:uom, UOM, Graph ) ),
          AircraftPerformance=xval(AircraftPerformanceValue,UOM)
        );
        (
          rdf( AircraftPerformanceNode,aixm:nilReason, NilReason, Graph ),
          AircraftPerformance=nil(NilReason)
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
          ( rdf( EmailNode, aixm:uom, UOM, Graph ); rdf( EmailNode, fixm:uom, UOM, Graph ); rdf( EmailNode, plain:uom, UOM, Graph ) ),
          Email=xval(EmailValue,UOM)
        );
        (
          rdf( EmailNode,aixm:nilReason, NilReason, Graph ),
          Email=nil(NilReason)
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
          ( rdf( AirspaceControlledEntryTimeNode, aixm:uom, UOM, Graph ); rdf( AirspaceControlledEntryTimeNode, fixm:uom, UOM, Graph ); rdf( AirspaceControlledEntryTimeNode, plain:uom, UOM, Graph ) ),
          AirspaceControlledEntryTime=xval(AirspaceControlledEntryTimeValue,UOM)
        );
        (
          rdf( AirspaceControlledEntryTimeNode,aixm:nilReason, NilReason, Graph ),
          AirspaceControlledEntryTime=nil(NilReason)
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
          ( rdf( ConstrainedAirspaceNode, aixm:uom, UOM, Graph ); rdf( ConstrainedAirspaceNode, fixm:uom, UOM, Graph ); rdf( ConstrainedAirspaceNode, plain:uom, UOM, Graph ) ),
          ConstrainedAirspace=xval(ConstrainedAirspaceValue,UOM)
        );
        (
          rdf( ConstrainedAirspaceNode,aixm:nilReason, NilReason, Graph ),
          ConstrainedAirspace=nil(NilReason)
        )
      )
  ) .

fixm_TimeSequence(Graph, TimeSequence, Approval, Begin, End, Ready, Request) :-
  rdf(TimeSequence,rdf:type,fixm:'TimeSequence',Graph)
  ,( Approval='$null$',
    \+ rdf( TimeSequence,fixm:'approval', _Approval, Graph )
  )
  ,( Begin='$null$',
    \+ rdf( TimeSequence,fixm:'begin', _Begin, Graph )
  )
  ,( End='$null$',
    \+ rdf( TimeSequence,fixm:'end', _End, Graph )
  )
  ,( Ready='$null$',
    \+ rdf( TimeSequence,fixm:'ready', _Ready, Graph )
  )
  ,( Request='$null$',
    \+ rdf( TimeSequence,fixm:'request', _Request, Graph )
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
          ( rdf( ResponsibleAgentNode, aixm:uom, UOM, Graph ); rdf( ResponsibleAgentNode, fixm:uom, UOM, Graph ); rdf( ResponsibleAgentNode, plain:uom, UOM, Graph ) ),
          ResponsibleAgent=xval(ResponsibleAgentValue,UOM)
        );
        (
          rdf( ResponsibleAgentNode,aixm:nilReason, NilReason, Graph ),
          ResponsibleAgent=nil(NilReason)
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
          ( rdf( SectorIdentifierNode, aixm:uom, UOM, Graph ); rdf( SectorIdentifierNode, fixm:uom, UOM, Graph ); rdf( SectorIdentifierNode, plain:uom, UOM, Graph ) ),
          SectorIdentifier=xval(SectorIdentifierValue,UOM)
        );
        (
          rdf( SectorIdentifierNode,aixm:nilReason, NilReason, Graph ),
          SectorIdentifier=nil(NilReason)
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
          ( rdf( DelegatedNode, aixm:uom, UOM, Graph ); rdf( DelegatedNode, fixm:uom, UOM, Graph ); rdf( DelegatedNode, plain:uom, UOM, Graph ) ),
          Delegated=xval(DelegatedValue,UOM)
        );
        (
          rdf( DelegatedNode,aixm:nilReason, NilReason, Graph ),
          Delegated=nil(NilReason)
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
          ( rdf( OtherSurveillanceCapabilitiesNode, aixm:uom, UOM, Graph ); rdf( OtherSurveillanceCapabilitiesNode, fixm:uom, UOM, Graph ); rdf( OtherSurveillanceCapabilitiesNode, plain:uom, UOM, Graph ) ),
          OtherSurveillanceCapabilities=xval(OtherSurveillanceCapabilitiesValue,UOM)
        );
        (
          rdf( OtherSurveillanceCapabilitiesNode,aixm:nilReason, NilReason, Graph ),
          OtherSurveillanceCapabilities=nil(NilReason)
        )
      )
  )
  ,findall(A, rdf(SurveillanceCapabilities,fixm:'surveillanceCode',A,Graph), SurveillanceCode) .

fixm_Trajectory(Graph, Trajectory, TrajectoryPoint) :-
  rdf(Trajectory,rdf:type,fixm:'Trajectory',Graph)
  ,( TrajectoryPoint='$null$',
    \+ rdf( Trajectory,fixm:'trajectoryPoint', _TrajectoryPoint, Graph )
  ) .

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
          ( rdf( IsRemoteNode, aixm:uom, UOM, Graph ); rdf( IsRemoteNode, fixm:uom, UOM, Graph ); rdf( IsRemoteNode, plain:uom, UOM, Graph ) ),
          IsRemote=xval(IsRemoteValue,UOM)
        );
        (
          rdf( IsRemoteNode,aixm:nilReason, NilReason, Graph ),
          IsRemote=nil(NilReason)
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
          ( rdf( IsPrimaryNode, aixm:uom, UOM, Graph ); rdf( IsPrimaryNode, fixm:uom, UOM, Graph ); rdf( IsPrimaryNode, plain:uom, UOM, Graph ) ),
          IsPrimary=xval(IsPrimaryValue,UOM)
        );
        (
          rdf( IsPrimaryNode,aixm:nilReason, NilReason, Graph ),
          IsPrimary=nil(NilReason)
        )
      )
  )
  ,( Availability='$null$',
    \+ rdf( AltimeterSourceTimeSlice,aixm:'availability', _Availability, Graph )
  )
  ,( Annotation='$null$',
    \+ rdf( AltimeterSourceTimeSlice,aixm:'annotation', _Annotation, Graph )
  ) .

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
          ( rdf( HorizontalAccuracyNode, aixm:uom, UOM, Graph ); rdf( HorizontalAccuracyNode, fixm:uom, UOM, Graph ); rdf( HorizontalAccuracyNode, plain:uom, UOM, Graph ) ),
          HorizontalAccuracy=xval(HorizontalAccuracyValue,UOM)
        );
        (
          rdf( HorizontalAccuracyNode,aixm:nilReason, NilReason, Graph ),
          HorizontalAccuracy=nil(NilReason)
        )
      )
  )
  ,( Annotation='$null$',
    \+ rdf( Point,aixm:'annotation', _Annotation, Graph )
  ) .

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
          ( rdf( TypeNode, aixm:uom, UOM, Graph ); rdf( TypeNode, fixm:uom, UOM, Graph ); rdf( TypeNode, plain:uom, UOM, Graph ) ),
          Type=xval(TypeValue,UOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, NilReason, Graph ),
          Type=nil(NilReason)
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
          ( rdf( EngineNode, aixm:uom, UOM, Graph ); rdf( EngineNode, fixm:uom, UOM, Graph ); rdf( EngineNode, plain:uom, UOM, Graph ) ),
          Engine=xval(EngineValue,UOM)
        );
        (
          rdf( EngineNode,aixm:nilReason, NilReason, Graph ),
          Engine=nil(NilReason)
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
          ( rdf( NumberEngineNode, aixm:uom, UOM, Graph ); rdf( NumberEngineNode, fixm:uom, UOM, Graph ); rdf( NumberEngineNode, plain:uom, UOM, Graph ) ),
          NumberEngine=xval(NumberEngineValue,UOM)
        );
        (
          rdf( NumberEngineNode,aixm:nilReason, NilReason, Graph ),
          NumberEngine=nil(NilReason)
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
          ( rdf( TypeAircraftICAONode, aixm:uom, UOM, Graph ); rdf( TypeAircraftICAONode, fixm:uom, UOM, Graph ); rdf( TypeAircraftICAONode, plain:uom, UOM, Graph ) ),
          TypeAircraftICAO=xval(TypeAircraftICAOValue,UOM)
        );
        (
          rdf( TypeAircraftICAONode,aixm:nilReason, NilReason, Graph ),
          TypeAircraftICAO=nil(NilReason)
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
          ( rdf( AircraftLandingCategoryNode, aixm:uom, UOM, Graph ); rdf( AircraftLandingCategoryNode, fixm:uom, UOM, Graph ); rdf( AircraftLandingCategoryNode, plain:uom, UOM, Graph ) ),
          AircraftLandingCategory=xval(AircraftLandingCategoryValue,UOM)
        );
        (
          rdf( AircraftLandingCategoryNode,aixm:nilReason, NilReason, Graph ),
          AircraftLandingCategory=nil(NilReason)
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
          ( rdf( WingSpanNode, aixm:uom, UOM, Graph ); rdf( WingSpanNode, fixm:uom, UOM, Graph ); rdf( WingSpanNode, plain:uom, UOM, Graph ) ),
          WingSpan=xval(WingSpanValue,UOM)
        );
        (
          rdf( WingSpanNode,aixm:nilReason, NilReason, Graph ),
          WingSpan=nil(NilReason)
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
          ( rdf( WingSpanInterpretationNode, aixm:uom, UOM, Graph ); rdf( WingSpanInterpretationNode, fixm:uom, UOM, Graph ); rdf( WingSpanInterpretationNode, plain:uom, UOM, Graph ) ),
          WingSpanInterpretation=xval(WingSpanInterpretationValue,UOM)
        );
        (
          rdf( WingSpanInterpretationNode,aixm:nilReason, NilReason, Graph ),
          WingSpanInterpretation=nil(NilReason)
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
          ( rdf( ClassWingSpanNode, aixm:uom, UOM, Graph ); rdf( ClassWingSpanNode, fixm:uom, UOM, Graph ); rdf( ClassWingSpanNode, plain:uom, UOM, Graph ) ),
          ClassWingSpan=xval(ClassWingSpanValue,UOM)
        );
        (
          rdf( ClassWingSpanNode,aixm:nilReason, NilReason, Graph ),
          ClassWingSpan=nil(NilReason)
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
          ( rdf( WeightNode, aixm:uom, UOM, Graph ); rdf( WeightNode, fixm:uom, UOM, Graph ); rdf( WeightNode, plain:uom, UOM, Graph ) ),
          Weight=xval(WeightValue,UOM)
        );
        (
          rdf( WeightNode,aixm:nilReason, NilReason, Graph ),
          Weight=nil(NilReason)
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
          ( rdf( WeightInterpretationNode, aixm:uom, UOM, Graph ); rdf( WeightInterpretationNode, fixm:uom, UOM, Graph ); rdf( WeightInterpretationNode, plain:uom, UOM, Graph ) ),
          WeightInterpretation=xval(WeightInterpretationValue,UOM)
        );
        (
          rdf( WeightInterpretationNode,aixm:nilReason, NilReason, Graph ),
          WeightInterpretation=nil(NilReason)
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
          ( rdf( PassengersNode, aixm:uom, UOM, Graph ); rdf( PassengersNode, fixm:uom, UOM, Graph ); rdf( PassengersNode, plain:uom, UOM, Graph ) ),
          Passengers=xval(PassengersValue,UOM)
        );
        (
          rdf( PassengersNode,aixm:nilReason, NilReason, Graph ),
          Passengers=nil(NilReason)
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
          ( rdf( PassengersInterpretationNode, aixm:uom, UOM, Graph ); rdf( PassengersInterpretationNode, fixm:uom, UOM, Graph ); rdf( PassengersInterpretationNode, plain:uom, UOM, Graph ) ),
          PassengersInterpretation=xval(PassengersInterpretationValue,UOM)
        );
        (
          rdf( PassengersInterpretationNode,aixm:nilReason, NilReason, Graph ),
          PassengersInterpretation=nil(NilReason)
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
          ( rdf( SpeedNode, aixm:uom, UOM, Graph ); rdf( SpeedNode, fixm:uom, UOM, Graph ); rdf( SpeedNode, plain:uom, UOM, Graph ) ),
          Speed=xval(SpeedValue,UOM)
        );
        (
          rdf( SpeedNode,aixm:nilReason, NilReason, Graph ),
          Speed=nil(NilReason)
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
          ( rdf( SpeedInterpretationNode, aixm:uom, UOM, Graph ); rdf( SpeedInterpretationNode, fixm:uom, UOM, Graph ); rdf( SpeedInterpretationNode, plain:uom, UOM, Graph ) ),
          SpeedInterpretation=xval(SpeedInterpretationValue,UOM)
        );
        (
          rdf( SpeedInterpretationNode,aixm:nilReason, NilReason, Graph ),
          SpeedInterpretation=nil(NilReason)
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
          ( rdf( WakeTurbulenceNode, aixm:uom, UOM, Graph ); rdf( WakeTurbulenceNode, fixm:uom, UOM, Graph ); rdf( WakeTurbulenceNode, plain:uom, UOM, Graph ) ),
          WakeTurbulence=xval(WakeTurbulenceValue,UOM)
        );
        (
          rdf( WakeTurbulenceNode,aixm:nilReason, NilReason, Graph ),
          WakeTurbulence=nil(NilReason)
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
          ( rdf( NavigationEquipmentNode, aixm:uom, UOM, Graph ); rdf( NavigationEquipmentNode, fixm:uom, UOM, Graph ); rdf( NavigationEquipmentNode, plain:uom, UOM, Graph ) ),
          NavigationEquipment=xval(NavigationEquipmentValue,UOM)
        );
        (
          rdf( NavigationEquipmentNode,aixm:nilReason, NilReason, Graph ),
          NavigationEquipment=nil(NilReason)
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
          ( rdf( NavigationSpecificationNode, aixm:uom, UOM, Graph ); rdf( NavigationSpecificationNode, fixm:uom, UOM, Graph ); rdf( NavigationSpecificationNode, plain:uom, UOM, Graph ) ),
          NavigationSpecification=xval(NavigationSpecificationValue,UOM)
        );
        (
          rdf( NavigationSpecificationNode,aixm:nilReason, NilReason, Graph ),
          NavigationSpecification=nil(NilReason)
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
          ( rdf( VerticalSeparationCapabilityNode, aixm:uom, UOM, Graph ); rdf( VerticalSeparationCapabilityNode, fixm:uom, UOM, Graph ); rdf( VerticalSeparationCapabilityNode, plain:uom, UOM, Graph ) ),
          VerticalSeparationCapability=xval(VerticalSeparationCapabilityValue,UOM)
        );
        (
          rdf( VerticalSeparationCapabilityNode,aixm:nilReason, NilReason, Graph ),
          VerticalSeparationCapability=nil(NilReason)
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
          ( rdf( AntiCollisionAndSeparationEquipmentNode, aixm:uom, UOM, Graph ); rdf( AntiCollisionAndSeparationEquipmentNode, fixm:uom, UOM, Graph ); rdf( AntiCollisionAndSeparationEquipmentNode, plain:uom, UOM, Graph ) ),
          AntiCollisionAndSeparationEquipment=xval(AntiCollisionAndSeparationEquipmentValue,UOM)
        );
        (
          rdf( AntiCollisionAndSeparationEquipmentNode,aixm:nilReason, NilReason, Graph ),
          AntiCollisionAndSeparationEquipment=nil(NilReason)
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
          ( rdf( CommunicationEquipmentNode, aixm:uom, UOM, Graph ); rdf( CommunicationEquipmentNode, fixm:uom, UOM, Graph ); rdf( CommunicationEquipmentNode, plain:uom, UOM, Graph ) ),
          CommunicationEquipment=xval(CommunicationEquipmentValue,UOM)
        );
        (
          rdf( CommunicationEquipmentNode,aixm:nilReason, NilReason, Graph ),
          CommunicationEquipment=nil(NilReason)
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
          ( rdf( SurveillanceEquipmentNode, aixm:uom, UOM, Graph ); rdf( SurveillanceEquipmentNode, fixm:uom, UOM, Graph ); rdf( SurveillanceEquipmentNode, plain:uom, UOM, Graph ) ),
          SurveillanceEquipment=xval(SurveillanceEquipmentValue,UOM)
        );
        (
          rdf( SurveillanceEquipmentNode,aixm:nilReason, NilReason, Graph ),
          SurveillanceEquipment=nil(NilReason)
        )
      )
  )
  ,( Annotation='$null$',
    \+ rdf( AircraftCharacteristic,aixm:'annotation', _Annotation, Graph )
  ) .

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
          ( rdf( DeliveryPointNode, aixm:uom, UOM, Graph ); rdf( DeliveryPointNode, fixm:uom, UOM, Graph ); rdf( DeliveryPointNode, plain:uom, UOM, Graph ) ),
          DeliveryPoint=xval(DeliveryPointValue,UOM)
        );
        (
          rdf( DeliveryPointNode,aixm:nilReason, NilReason, Graph ),
          DeliveryPoint=nil(NilReason)
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
          ( rdf( CityNode, aixm:uom, UOM, Graph ); rdf( CityNode, fixm:uom, UOM, Graph ); rdf( CityNode, plain:uom, UOM, Graph ) ),
          City=xval(CityValue,UOM)
        );
        (
          rdf( CityNode,aixm:nilReason, NilReason, Graph ),
          City=nil(NilReason)
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
          ( rdf( AdministrativeAreaNode, aixm:uom, UOM, Graph ); rdf( AdministrativeAreaNode, fixm:uom, UOM, Graph ); rdf( AdministrativeAreaNode, plain:uom, UOM, Graph ) ),
          AdministrativeArea=xval(AdministrativeAreaValue,UOM)
        );
        (
          rdf( AdministrativeAreaNode,aixm:nilReason, NilReason, Graph ),
          AdministrativeArea=nil(NilReason)
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
          ( rdf( PostalCodeNode, aixm:uom, UOM, Graph ); rdf( PostalCodeNode, fixm:uom, UOM, Graph ); rdf( PostalCodeNode, plain:uom, UOM, Graph ) ),
          PostalCode=xval(PostalCodeValue,UOM)
        );
        (
          rdf( PostalCodeNode,aixm:nilReason, NilReason, Graph ),
          PostalCode=nil(NilReason)
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
          ( rdf( CountryNode, aixm:uom, UOM, Graph ); rdf( CountryNode, fixm:uom, UOM, Graph ); rdf( CountryNode, plain:uom, UOM, Graph ) ),
          Country=xval(CountryValue,UOM)
        );
        (
          rdf( CountryNode,aixm:nilReason, NilReason, Graph ),
          Country=nil(NilReason)
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
          ( rdf( DangerousGoodsQuantityNode, aixm:uom, UOM, Graph ); rdf( DangerousGoodsQuantityNode, fixm:uom, UOM, Graph ); rdf( DangerousGoodsQuantityNode, plain:uom, UOM, Graph ) ),
          DangerousGoodsQuantity=xval(DangerousGoodsQuantityValue,UOM)
        );
        (
          rdf( DangerousGoodsQuantityNode,aixm:nilReason, NilReason, Graph ),
          DangerousGoodsQuantity=nil(NilReason)
        )
      )
  )
  ,( PackageDimensions='$null$',
    \+ rdf( DangerousGoodsPackage,fixm:'packageDimensions', _PackageDimensions, Graph )
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
          ( rdf( PackingInstructionNumberNode, aixm:uom, UOM, Graph ); rdf( PackingInstructionNumberNode, fixm:uom, UOM, Graph ); rdf( PackingInstructionNumberNode, plain:uom, UOM, Graph ) ),
          PackingInstructionNumber=xval(PackingInstructionNumberValue,UOM)
        );
        (
          rdf( PackingInstructionNumberNode,aixm:nilReason, NilReason, Graph ),
          PackingInstructionNumber=nil(NilReason)
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
          ( rdf( ProductNameNode, aixm:uom, UOM, Graph ); rdf( ProductNameNode, fixm:uom, UOM, Graph ); rdf( ProductNameNode, plain:uom, UOM, Graph ) ),
          ProductName=xval(ProductNameValue,UOM)
        );
        (
          rdf( ProductNameNode,aixm:nilReason, NilReason, Graph ),
          ProductName=nil(NilReason)
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
          ( rdf( ProperShippingNameNode, aixm:uom, UOM, Graph ); rdf( ProperShippingNameNode, fixm:uom, UOM, Graph ); rdf( ProperShippingNameNode, plain:uom, UOM, Graph ) ),
          ProperShippingName=xval(ProperShippingNameValue,UOM)
        );
        (
          rdf( ProperShippingNameNode,aixm:nilReason, NilReason, Graph ),
          ProperShippingName=nil(NilReason)
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
          ( rdf( ReportableQuantityNode, aixm:uom, UOM, Graph ); rdf( ReportableQuantityNode, fixm:uom, UOM, Graph ); rdf( ReportableQuantityNode, plain:uom, UOM, Graph ) ),
          ReportableQuantity=xval(ReportableQuantityValue,UOM)
        );
        (
          rdf( ReportableQuantityNode,aixm:nilReason, NilReason, Graph ),
          ReportableQuantity=nil(NilReason)
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
          ( rdf( SupplementaryInformationNode, aixm:uom, UOM, Graph ); rdf( SupplementaryInformationNode, fixm:uom, UOM, Graph ); rdf( SupplementaryInformationNode, plain:uom, UOM, Graph ) ),
          SupplementaryInformation=xval(SupplementaryInformationValue,UOM)
        );
        (
          rdf( SupplementaryInformationNode,aixm:nilReason, NilReason, Graph ),
          SupplementaryInformation=nil(NilReason)
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
          ( rdf( TechnicalNameNode, aixm:uom, UOM, Graph ); rdf( TechnicalNameNode, fixm:uom, UOM, Graph ); rdf( TechnicalNameNode, plain:uom, UOM, Graph ) ),
          TechnicalName=xval(TechnicalNameValue,UOM)
        );
        (
          rdf( TechnicalNameNode,aixm:nilReason, NilReason, Graph ),
          TechnicalName=nil(NilReason)
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
          ( rdf( TypeOfPackagingNode, aixm:uom, UOM, Graph ); rdf( TypeOfPackagingNode, fixm:uom, UOM, Graph ); rdf( TypeOfPackagingNode, plain:uom, UOM, Graph ) ),
          TypeOfPackaging=xval(TypeOfPackagingValue,UOM)
        );
        (
          rdf( TypeOfPackagingNode,aixm:nilReason, NilReason, Graph ),
          TypeOfPackaging=nil(NilReason)
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
          ( rdf( UnNumberNode, aixm:uom, UOM, Graph ); rdf( UnNumberNode, fixm:uom, UOM, Graph ); rdf( UnNumberNode, plain:uom, UOM, Graph ) ),
          UnNumber=xval(UnNumberValue,UOM)
        );
        (
          rdf( UnNumberNode,aixm:nilReason, NilReason, Graph ),
          UnNumber=nil(NilReason)
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
          ( rdf( DangerousGoodsLimitationNode, aixm:uom, UOM, Graph ); rdf( DangerousGoodsLimitationNode, fixm:uom, UOM, Graph ); rdf( DangerousGoodsLimitationNode, plain:uom, UOM, Graph ) ),
          DangerousGoodsLimitation=xval(DangerousGoodsLimitationValue,UOM)
        );
        (
          rdf( DangerousGoodsLimitationNode,aixm:nilReason, NilReason, Graph ),
          DangerousGoodsLimitation=nil(NilReason)
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
          ( rdf( ShipmentTypeNode, aixm:uom, UOM, Graph ); rdf( ShipmentTypeNode, fixm:uom, UOM, Graph ); rdf( ShipmentTypeNode, plain:uom, UOM, Graph ) ),
          ShipmentType=xval(ShipmentTypeValue,UOM)
        );
        (
          rdf( ShipmentTypeNode,aixm:nilReason, NilReason, Graph ),
          ShipmentType=nil(NilReason)
        )
      )
  )
  ,( AllPackedInOne='$null$',
    \+ rdf( DangerousGoodsPackage,fixm:'allPackedInOne', _AllPackedInOne, Graph )
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
          ( rdf( CompatibilityGroupNode, aixm:uom, UOM, Graph ); rdf( CompatibilityGroupNode, fixm:uom, UOM, Graph ); rdf( CompatibilityGroupNode, plain:uom, UOM, Graph ) ),
          CompatibilityGroup=xval(CompatibilityGroupValue,UOM)
        );
        (
          rdf( CompatibilityGroupNode,aixm:nilReason, NilReason, Graph ),
          CompatibilityGroup=nil(NilReason)
        )
      )
  )
  ,( ShipmentDimensions='$null$',
    \+ rdf( DangerousGoodsPackage,fixm:'shipmentDimensions', _ShipmentDimensions, Graph )
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
          ( rdf( MarinePollutantIndicatorNode, aixm:uom, UOM, Graph ); rdf( MarinePollutantIndicatorNode, fixm:uom, UOM, Graph ); rdf( MarinePollutantIndicatorNode, plain:uom, UOM, Graph ) ),
          MarinePollutantIndicator=xval(MarinePollutantIndicatorValue,UOM)
        );
        (
          rdf( MarinePollutantIndicatorNode,aixm:nilReason, NilReason, Graph ),
          MarinePollutantIndicator=nil(NilReason)
        )
      )
  )
  ,( RadioactiveMaterials='$null$',
    \+ rdf( DangerousGoodsPackage,fixm:'radioactiveMaterials', _RadioactiveMaterials, Graph )
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
          ( rdf( HazardClassNode, aixm:uom, UOM, Graph ); rdf( HazardClassNode, fixm:uom, UOM, Graph ); rdf( HazardClassNode, plain:uom, UOM, Graph ) ),
          HazardClass=xval(HazardClassValue,UOM)
        );
        (
          rdf( HazardClassNode,aixm:nilReason, NilReason, Graph ),
          HazardClass=nil(NilReason)
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
          ( rdf( PackingGroupNode, aixm:uom, UOM, Graph ); rdf( PackingGroupNode, fixm:uom, UOM, Graph ); rdf( PackingGroupNode, plain:uom, UOM, Graph ) ),
          PackingGroup=xval(PackingGroupValue,UOM)
        );
        (
          rdf( PackingGroupNode,aixm:nilReason, NilReason, Graph ),
          PackingGroup=nil(NilReason)
        )
      )
  )
  ,( Temperatures='$null$',
    \+ rdf( DangerousGoodsPackage,fixm:'temperatures', _Temperatures, Graph )
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
          ( rdf( OverpackIndicatorNode, aixm:uom, UOM, Graph ); rdf( OverpackIndicatorNode, fixm:uom, UOM, Graph ); rdf( OverpackIndicatorNode, plain:uom, UOM, Graph ) ),
          OverpackIndicator=xval(OverpackIndicatorValue,UOM)
        );
        (
          rdf( OverpackIndicatorNode,aixm:nilReason, NilReason, Graph ),
          OverpackIndicator=nil(NilReason)
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
          ( rdf( DeterminationMethodNode, aixm:uom, UOM, Graph ); rdf( DeterminationMethodNode, fixm:uom, UOM, Graph ); rdf( DeterminationMethodNode, plain:uom, UOM, Graph ) ),
          DeterminationMethod=xval(DeterminationMethodValue,UOM)
        );
        (
          rdf( DeterminationMethodNode,aixm:nilReason, NilReason, Graph ),
          DeterminationMethod=nil(NilReason)
        )
      )
  )
  ,( Position='$null$',
    \+ rdf( LastPositionReport,fixm:'position', _Position, Graph )
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
          ( rdf( TimeAtPositionNode, aixm:uom, UOM, Graph ); rdf( TimeAtPositionNode, fixm:uom, UOM, Graph ); rdf( TimeAtPositionNode, plain:uom, UOM, Graph ) ),
          TimeAtPosition=xval(TimeAtPositionValue,UOM)
        );
        (
          rdf( TimeAtPositionNode,aixm:nilReason, NilReason, Graph ),
          TimeAtPosition=nil(NilReason)
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
          ( rdf( OperationalStatusNode, aixm:uom, UOM, Graph ); rdf( OperationalStatusNode, fixm:uom, UOM, Graph ); rdf( OperationalStatusNode, plain:uom, UOM, Graph ) ),
          OperationalStatus=xval(OperationalStatusValue,UOM)
        );
        (
          rdf( OperationalStatusNode,aixm:nilReason, NilReason, Graph ),
          OperationalStatus=nil(NilReason)
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
          ( rdf( GrossWeightNode, aixm:uom, UOM, Graph ); rdf( GrossWeightNode, fixm:uom, UOM, Graph ); rdf( GrossWeightNode, plain:uom, UOM, Graph ) ),
          GrossWeight=xval(GrossWeightValue,UOM)
        );
        (
          rdf( GrossWeightNode,aixm:nilReason, NilReason, Graph ),
          GrossWeight=nil(NilReason)
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
          ( rdf( NetWeightNode, aixm:uom, UOM, Graph ); rdf( NetWeightNode, fixm:uom, UOM, Graph ); rdf( NetWeightNode, plain:uom, UOM, Graph ) ),
          NetWeight=xval(NetWeightValue,UOM)
        );
        (
          rdf( NetWeightNode,aixm:nilReason, NilReason, Graph ),
          NetWeight=nil(NilReason)
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
          ( rdf( VolumeNode, aixm:uom, UOM, Graph ); rdf( VolumeNode, fixm:uom, UOM, Graph ); rdf( VolumeNode, plain:uom, UOM, Graph ) ),
          Volume=xval(VolumeValue,UOM)
        );
        (
          rdf( VolumeNode,aixm:nilReason, NilReason, Graph ),
          Volume=nil(NilReason)
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
          ( rdf( EfplFlightRulesNode, aixm:uom, UOM, Graph ); rdf( EfplFlightRulesNode, fixm:uom, UOM, Graph ); rdf( EfplFlightRulesNode, plain:uom, UOM, Graph ) ),
          EfplFlightRules=xval(EfplFlightRulesValue,UOM)
        );
        (
          rdf( EfplFlightRulesNode,aixm:nilReason, NilReason, Graph ),
          EfplFlightRules=nil(NilReason)
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
          ( rdf( AbrogationReasonNode, aixm:uom, UOM, Graph ); rdf( AbrogationReasonNode, fixm:uom, UOM, Graph ); rdf( AbrogationReasonNode, plain:uom, UOM, Graph ) ),
          AbrogationReason=xval(AbrogationReasonValue,UOM)
        );
        (
          rdf( AbrogationReasonNode,aixm:nilReason, NilReason, Graph ),
          AbrogationReason=nil(NilReason)
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
          ( rdf( CoordinationStatusNode, aixm:uom, UOM, Graph ); rdf( CoordinationStatusNode, fixm:uom, UOM, Graph ); rdf( CoordinationStatusNode, plain:uom, UOM, Graph ) ),
          CoordinationStatus=xval(CoordinationStatusValue,UOM)
        );
        (
          rdf( CoordinationStatusNode,aixm:nilReason, NilReason, Graph ),
          CoordinationStatus=nil(NilReason)
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
          ( rdf( NonStandardCommunicationReasonNode, aixm:uom, UOM, Graph ); rdf( NonStandardCommunicationReasonNode, fixm:uom, UOM, Graph ); rdf( NonStandardCommunicationReasonNode, plain:uom, UOM, Graph ) ),
          NonStandardCommunicationReason=xval(NonStandardCommunicationReasonValue,UOM)
        );
        (
          rdf( NonStandardCommunicationReasonNode,aixm:nilReason, NilReason, Graph ),
          NonStandardCommunicationReason=nil(NilReason)
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
          ( rdf( ReleaseConditionsNode, aixm:uom, UOM, Graph ); rdf( ReleaseConditionsNode, fixm:uom, UOM, Graph ); rdf( ReleaseConditionsNode, plain:uom, UOM, Graph ) ),
          ReleaseConditions=xval(ReleaseConditionsValue,UOM)
        );
        (
          rdf( ReleaseConditionsNode,aixm:nilReason, NilReason, Graph ),
          ReleaseConditions=nil(NilReason)
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
          ( rdf( AltitudeNode, aixm:uom, UOM, Graph ); rdf( AltitudeNode, fixm:uom, UOM, Graph ); rdf( AltitudeNode, plain:uom, UOM, Graph ) ),
          Altitude=xval(AltitudeValue,UOM)
        );
        (
          rdf( AltitudeNode,aixm:nilReason, NilReason, Graph ),
          Altitude=nil(NilReason)
        )
      )
  )
  ,( CrossingPoint='$null$',
    \+ rdf( BoundaryCrossing,fixm:'crossingPoint', _CrossingPoint, Graph )
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
          ( rdf( CrossingSpeedNode, aixm:uom, UOM, Graph ); rdf( CrossingSpeedNode, fixm:uom, UOM, Graph ); rdf( CrossingSpeedNode, plain:uom, UOM, Graph ) ),
          CrossingSpeed=xval(CrossingSpeedValue,UOM)
        );
        (
          rdf( CrossingSpeedNode,aixm:nilReason, NilReason, Graph ),
          CrossingSpeed=nil(NilReason)
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
          ( rdf( CrossingTimeNode, aixm:uom, UOM, Graph ); rdf( CrossingTimeNode, fixm:uom, UOM, Graph ); rdf( CrossingTimeNode, plain:uom, UOM, Graph ) ),
          CrossingTime=xval(CrossingTimeValue,UOM)
        );
        (
          rdf( CrossingTimeNode,aixm:nilReason, NilReason, Graph ),
          CrossingTime=nil(NilReason)
        )
      )
  )
  ,( Offtrack='$null$',
    \+ rdf( BoundaryCrossing,fixm:'offtrack', _Offtrack, Graph )
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
          ( rdf( AltitudeInTransitionNode, aixm:uom, UOM, Graph ); rdf( AltitudeInTransitionNode, fixm:uom, UOM, Graph ); rdf( AltitudeInTransitionNode, plain:uom, UOM, Graph ) ),
          AltitudeInTransition=xval(AltitudeInTransitionValue,UOM)
        );
        (
          rdf( AltitudeInTransitionNode,aixm:nilReason, NilReason, Graph ),
          AltitudeInTransition=nil(NilReason)
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
          ( rdf( CodeNode, aixm:uom, UOM, Graph ); rdf( CodeNode, fixm:uom, UOM, Graph ); rdf( CodeNode, plain:uom, UOM, Graph ) ),
          Code=xval(CodeValue,UOM)
        );
        (
          rdf( CodeNode,aixm:nilReason, NilReason, Graph ),
          Code=nil(NilReason)
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
          ( rdf( RadioFailureRemarksNode, aixm:uom, UOM, Graph ); rdf( RadioFailureRemarksNode, fixm:uom, UOM, Graph ); rdf( RadioFailureRemarksNode, plain:uom, UOM, Graph ) ),
          RadioFailureRemarks=xval(RadioFailureRemarksValue,UOM)
        );
        (
          rdf( RadioFailureRemarksNode,aixm:nilReason, NilReason, Graph ),
          RadioFailureRemarks=nil(NilReason)
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
          ( rdf( RemainingComCapabilityNode, aixm:uom, UOM, Graph ); rdf( RemainingComCapabilityNode, fixm:uom, UOM, Graph ); rdf( RemainingComCapabilityNode, plain:uom, UOM, Graph ) ),
          RemainingComCapability=xval(RemainingComCapabilityValue,UOM)
        );
        (
          rdf( RemainingComCapabilityNode,aixm:nilReason, NilReason, Graph ),
          RemainingComCapability=nil(NilReason)
        )
      )
  )
  ,( Contact='$null$',
    \+ rdf( RadioCommunicationFailure,fixm:'contact', _Contact, Graph )
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
          ( rdf( OperationalStatusNode, aixm:uom, UOM, Graph ); rdf( OperationalStatusNode, fixm:uom, UOM, Graph ); rdf( OperationalStatusNode, plain:uom, UOM, Graph ) ),
          OperationalStatus=xval(OperationalStatusValue,UOM)
        );
        (
          rdf( OperationalStatusNode,aixm:nilReason, NilReason, Graph ),
          OperationalStatus=nil(NilReason)
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
          ( rdf( WarningNode, aixm:uom, UOM, Graph ); rdf( WarningNode, fixm:uom, UOM, Graph ); rdf( WarningNode, plain:uom, UOM, Graph ) ),
          Warning=xval(WarningValue,UOM)
        );
        (
          rdf( WarningNode,aixm:nilReason, NilReason, Graph ),
          Warning=nil(NilReason)
        )
      )
  )
  ,( Usage='$null$',
    \+ rdf( AirportHeliportAvailability,aixm:'usage', _Usage, Graph )
  ) .

fixm_FlightArrival(Graph, FlightArrival, ApproachFix, ApproachTime, ArrivalAerodrome, ArrivalAerodromeAlternate, ArrivalAerodromeOriginal, ArrivalFix, ArrivalFixTime, ArrivalFleetPrioritization, ArrivalSequenceNumber, EarliestInBlockTime, FiledRevisedDestinationAerodrome, FiledRevisedDestinationStar, RunwayPositionAndTime, StandardInstrumentArrival, StandPositionAndTime, LandingLimits) :-
  rdf(FlightArrival,rdf:type,fixm:'FlightArrival',Graph)
  ,( ApproachFix='$null$',
    \+ rdf( FlightArrival,fixm:'approachFix', _ApproachFix, Graph )
  )
  ,( ApproachTime='$null$',
    \+ rdf( FlightArrival,fixm:'approachTime', _ApproachTime, Graph )
  )
  ,( ArrivalAerodrome='$null$',
    \+ rdf( FlightArrival,fixm:'arrivalAerodrome', _ArrivalAerodrome, Graph )
  )
  ,( ArrivalAerodromeAlternate='$null$',
    \+ rdf( FlightArrival,fixm:'arrivalAerodromeAlternate', _ArrivalAerodromeAlternate, Graph )
  )
  ,( ArrivalAerodromeOriginal='$null$',
    \+ rdf( FlightArrival,fixm:'arrivalAerodromeOriginal', _ArrivalAerodromeOriginal, Graph )
  )
  ,( ArrivalFix='$null$',
    \+ rdf( FlightArrival,fixm:'arrivalFix', _ArrivalFix, Graph )
  )
  ,( ArrivalFixTime='$null$',
    \+ rdf( FlightArrival,fixm:'arrivalFixTime', _ArrivalFixTime, Graph )
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
          ( rdf( ArrivalFleetPrioritizationNode, aixm:uom, UOM, Graph ); rdf( ArrivalFleetPrioritizationNode, fixm:uom, UOM, Graph ); rdf( ArrivalFleetPrioritizationNode, plain:uom, UOM, Graph ) ),
          ArrivalFleetPrioritization=xval(ArrivalFleetPrioritizationValue,UOM)
        );
        (
          rdf( ArrivalFleetPrioritizationNode,aixm:nilReason, NilReason, Graph ),
          ArrivalFleetPrioritization=nil(NilReason)
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
          ( rdf( ArrivalSequenceNumberNode, aixm:uom, UOM, Graph ); rdf( ArrivalSequenceNumberNode, fixm:uom, UOM, Graph ); rdf( ArrivalSequenceNumberNode, plain:uom, UOM, Graph ) ),
          ArrivalSequenceNumber=xval(ArrivalSequenceNumberValue,UOM)
        );
        (
          rdf( ArrivalSequenceNumberNode,aixm:nilReason, NilReason, Graph ),
          ArrivalSequenceNumber=nil(NilReason)
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
          ( rdf( EarliestInBlockTimeNode, aixm:uom, UOM, Graph ); rdf( EarliestInBlockTimeNode, fixm:uom, UOM, Graph ); rdf( EarliestInBlockTimeNode, plain:uom, UOM, Graph ) ),
          EarliestInBlockTime=xval(EarliestInBlockTimeValue,UOM)
        );
        (
          rdf( EarliestInBlockTimeNode,aixm:nilReason, NilReason, Graph ),
          EarliestInBlockTime=nil(NilReason)
        )
      )
  )
  ,( FiledRevisedDestinationAerodrome='$null$',
    \+ rdf( FlightArrival,fixm:'filedRevisedDestinationAerodrome', _FiledRevisedDestinationAerodrome, Graph )
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
          ( rdf( FiledRevisedDestinationStarNode, aixm:uom, UOM, Graph ); rdf( FiledRevisedDestinationStarNode, fixm:uom, UOM, Graph ); rdf( FiledRevisedDestinationStarNode, plain:uom, UOM, Graph ) ),
          FiledRevisedDestinationStar=xval(FiledRevisedDestinationStarValue,UOM)
        );
        (
          rdf( FiledRevisedDestinationStarNode,aixm:nilReason, NilReason, Graph ),
          FiledRevisedDestinationStar=nil(NilReason)
        )
      )
  )
  ,( RunwayPositionAndTime='$null$',
    \+ rdf( FlightArrival,fixm:'runwayPositionAndTime', _RunwayPositionAndTime, Graph )
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
          ( rdf( StandardInstrumentArrivalNode, aixm:uom, UOM, Graph ); rdf( StandardInstrumentArrivalNode, fixm:uom, UOM, Graph ); rdf( StandardInstrumentArrivalNode, plain:uom, UOM, Graph ) ),
          StandardInstrumentArrival=xval(StandardInstrumentArrivalValue,UOM)
        );
        (
          rdf( StandardInstrumentArrivalNode,aixm:nilReason, NilReason, Graph ),
          StandardInstrumentArrival=nil(NilReason)
        )
      )
  )
  ,( StandPositionAndTime='$null$',
    \+ rdf( FlightArrival,fixm:'standPositionAndTime', _StandPositionAndTime, Graph )
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
          ( rdf( LandingLimitsNode, aixm:uom, UOM, Graph ); rdf( LandingLimitsNode, fixm:uom, UOM, Graph ); rdf( LandingLimitsNode, plain:uom, UOM, Graph ) ),
          LandingLimits=xval(LandingLimitsValue,UOM)
        );
        (
          rdf( LandingLimitsNode,aixm:nilReason, NilReason, Graph ),
          LandingLimits=nil(NilReason)
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
          ( rdf( CriticalitySafetyIndexNode, aixm:uom, UOM, Graph ); rdf( CriticalitySafetyIndexNode, fixm:uom, UOM, Graph ); rdf( CriticalitySafetyIndexNode, plain:uom, UOM, Graph ) ),
          CriticalitySafetyIndex=xval(CriticalitySafetyIndexValue,UOM)
        );
        (
          rdf( CriticalitySafetyIndexNode,aixm:nilReason, NilReason, Graph ),
          CriticalitySafetyIndex=nil(NilReason)
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
          ( rdf( TransportIndexNode, aixm:uom, UOM, Graph ); rdf( TransportIndexNode, fixm:uom, UOM, Graph ); rdf( TransportIndexNode, plain:uom, UOM, Graph ) ),
          TransportIndex=xval(TransportIndexValue,UOM)
        );
        (
          rdf( TransportIndexNode,aixm:nilReason, NilReason, Graph ),
          TransportIndex=nil(NilReason)
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
          ( rdf( FissileExceptedIndicatorNode, aixm:uom, UOM, Graph ); rdf( FissileExceptedIndicatorNode, fixm:uom, UOM, Graph ); rdf( FissileExceptedIndicatorNode, plain:uom, UOM, Graph ) ),
          FissileExceptedIndicator=xval(FissileExceptedIndicatorValue,UOM)
        );
        (
          rdf( FissileExceptedIndicatorNode,aixm:nilReason, NilReason, Graph ),
          FissileExceptedIndicator=nil(NilReason)
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
          ( rdf( CategoryNode, aixm:uom, UOM, Graph ); rdf( CategoryNode, fixm:uom, UOM, Graph ); rdf( CategoryNode, plain:uom, UOM, Graph ) ),
          Category=xval(CategoryValue,UOM)
        );
        (
          rdf( CategoryNode,aixm:nilReason, NilReason, Graph ),
          Category=nil(NilReason)
        )
      )
  )
  ,( Radionuclide='$null$',
    \+ rdf( RadioactiveMaterial,fixm:'radionuclide', _Radionuclide, Graph )
  ) .

fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled, Initial) :-
  rdf(ExtendedMultiTime,rdf:type,fixm:'ExtendedMultiTime',Graph)
  ,( Controlled='$null$',
    \+ rdf( ExtendedMultiTime,fixm:'controlled', _Controlled, Graph )
  )
  ,( Initial='$null$',
    \+ rdf( ExtendedMultiTime,fixm:'initial', _Initial, Graph )
  ) .

fixm_ControlElement(Graph, ControlElement) :-
  rdf(ControlElement,rdf:type,fixm:'ControlElement',Graph) .

fixm_AerodromesOfDestination(Graph, AerodromesOfDestination, AerodromeOfDestination, Alternate1, Alternate2, FiledRevisedDestinationAerodrome) :-
  rdf(AerodromesOfDestination,rdf:type,fixm:'AerodromesOfDestination',Graph)
  ,( AerodromeOfDestination='$null$',
    \+ rdf( AerodromesOfDestination,fixm:'aerodromeOfDestination', _AerodromeOfDestination, Graph )
  )
  ,( Alternate1='$null$',
    \+ rdf( AerodromesOfDestination,fixm:'alternate1', _Alternate1, Graph )
  )
  ,( Alternate2='$null$',
    \+ rdf( AerodromesOfDestination,fixm:'alternate2', _Alternate2, Graph )
  )
  ,( FiledRevisedDestinationAerodrome='$null$',
    \+ rdf( AerodromesOfDestination,fixm:'filedRevisedDestinationAerodrome', _FiledRevisedDestinationAerodrome, Graph )
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
          ( rdf( NumberOfPackagesNode, aixm:uom, UOM, Graph ); rdf( NumberOfPackagesNode, fixm:uom, UOM, Graph ); rdf( NumberOfPackagesNode, plain:uom, UOM, Graph ) ),
          NumberOfPackages=xval(NumberOfPackagesValue,UOM)
        );
        (
          rdf( NumberOfPackagesNode,aixm:nilReason, NilReason, Graph ),
          NumberOfPackages=nil(NilReason)
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
          ( rdf( QValueNode, aixm:uom, UOM, Graph ); rdf( QValueNode, fixm:uom, UOM, Graph ); rdf( QValueNode, plain:uom, UOM, Graph ) ),
          QValue=xval(QValueValue,UOM)
        );
        (
          rdf( QValueNode,aixm:nilReason, NilReason, Graph ),
          QValue=nil(NilReason)
        )
      )
  ) .

aixm_AltimeterSource(Graph, AltimeterSource, TimeSlice) :-
  rdf(AltimeterSource,rdf:type,aixm:'AltimeterSource',Graph)
  ,( TimeSlice='$null$',
    \+ rdf( AltimeterSource,aixm:'timeSlice', _TimeSlice, Graph )
  ) .

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
          ( rdf( SurvivalEquipmentRemarksNode, aixm:uom, UOM, Graph ); rdf( SurvivalEquipmentRemarksNode, fixm:uom, UOM, Graph ); rdf( SurvivalEquipmentRemarksNode, plain:uom, UOM, Graph ) ),
          SurvivalEquipmentRemarks=xval(SurvivalEquipmentRemarksValue,UOM)
        );
        (
          rdf( SurvivalEquipmentRemarksNode,aixm:nilReason, NilReason, Graph ),
          SurvivalEquipmentRemarks=nil(NilReason)
        )
      )
  )
  ,( DinghyInformation='$null$',
    \+ rdf( SurvivalCapabilities,fixm:'dinghyInformation', _DinghyInformation, Graph )
  )
  ,findall(A, rdf(SurvivalCapabilities,fixm:'emergencyRadioCode',A,Graph), EmergencyRadioCode)
  ,findall(A, rdf(SurvivalCapabilities,fixm:'lifeJacketCode',A,Graph), LifeJacketCode)
  ,findall(A, rdf(SurvivalCapabilities,fixm:'survivalEquipmentCode',A,Graph), SurvivalEquipmentCode) .

fixm_DirectRouting(Graph, DirectRouting, From, To) :-
  rdf(DirectRouting,rdf:type,fixm:'DirectRouting',Graph)
  ,( From='$null$',
    \+ rdf( DirectRouting,fixm:'from', _From, Graph )
  )
  ,( To='$null$',
    \+ rdf( DirectRouting,fixm:'to', _To, Graph )
  ) .

fixm_TargetMultiTime(Graph, TargetMultiTime, Target) :-
  subClassOf(T,fixm:'TargetMultiTime')
  ,rdf(TargetMultiTime,rdf:type,T,Graph)
  ,( Target='$null$',
    \+ rdf( TargetMultiTime,fixm:'target', _Target, Graph )
  ) .

fixm_AircraftType(Graph, AircraftType) :-
  rdf(AircraftType,rdf:type,fixm:'AircraftType',Graph) .

fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes) :-
  subClassOf(T,fixm:'FlightDeparture')
  ,rdf(FlightDeparture,rdf:type,T,Graph)
  ,( DepartureAerodrome='$null$',
    \+ rdf( FlightDeparture,fixm:'departureAerodrome', _DepartureAerodrome, Graph )
  )
  ,( DepartureFix='$null$',
    \+ rdf( FlightDeparture,fixm:'departureFix', _DepartureFix, Graph )
  )
  ,( DepartureFixTime='$null$',
    \+ rdf( FlightDeparture,fixm:'departureFixTime', _DepartureFixTime, Graph )
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
          ( rdf( DepartureFleetPrioritizationNode, aixm:uom, UOM, Graph ); rdf( DepartureFleetPrioritizationNode, fixm:uom, UOM, Graph ); rdf( DepartureFleetPrioritizationNode, plain:uom, UOM, Graph ) ),
          DepartureFleetPrioritization=xval(DepartureFleetPrioritizationValue,UOM)
        );
        (
          rdf( DepartureFleetPrioritizationNode,aixm:nilReason, NilReason, Graph ),
          DepartureFleetPrioritization=nil(NilReason)
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
          ( rdf( DepartureSlotNode, aixm:uom, UOM, Graph ); rdf( DepartureSlotNode, fixm:uom, UOM, Graph ); rdf( DepartureSlotNode, plain:uom, UOM, Graph ) ),
          DepartureSlot=xval(DepartureSlotValue,UOM)
        );
        (
          rdf( DepartureSlotNode,aixm:nilReason, NilReason, Graph ),
          DepartureSlot=nil(NilReason)
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
          ( rdf( EarliestOffBlockTimeNode, aixm:uom, UOM, Graph ); rdf( EarliestOffBlockTimeNode, fixm:uom, UOM, Graph ); rdf( EarliestOffBlockTimeNode, plain:uom, UOM, Graph ) ),
          EarliestOffBlockTime=xval(EarliestOffBlockTimeValue,UOM)
        );
        (
          rdf( EarliestOffBlockTimeNode,aixm:nilReason, NilReason, Graph ),
          EarliestOffBlockTime=nil(NilReason)
        )
      )
  )
  ,( OffBlockReadyTime='$null$',
    \+ rdf( FlightDeparture,fixm:'offBlockReadyTime', _OffBlockReadyTime, Graph )
  )
  ,( RunwayPositionAndTime='$null$',
    \+ rdf( FlightDeparture,fixm:'runwayPositionAndTime', _RunwayPositionAndTime, Graph )
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
          ( rdf( StandardInstrumentDepartureNode, aixm:uom, UOM, Graph ); rdf( StandardInstrumentDepartureNode, fixm:uom, UOM, Graph ); rdf( StandardInstrumentDepartureNode, plain:uom, UOM, Graph ) ),
          StandardInstrumentDeparture=xval(StandardInstrumentDepartureValue,UOM)
        );
        (
          rdf( StandardInstrumentDepartureNode,aixm:nilReason, NilReason, Graph ),
          StandardInstrumentDeparture=nil(NilReason)
        )
      )
  )
  ,( StandPositionAndTime='$null$',
    \+ rdf( FlightDeparture,fixm:'standPositionAndTime', _StandPositionAndTime, Graph )
  )
  ,( TakeoffAlternateAerodrome='$null$',
    \+ rdf( FlightDeparture,fixm:'takeoffAlternateAerodrome', _TakeoffAlternateAerodrome, Graph )
  )
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
          ( rdf( TakeoffWeightNode, aixm:uom, UOM, Graph ); rdf( TakeoffWeightNode, fixm:uom, UOM, Graph ); rdf( TakeoffWeightNode, plain:uom, UOM, Graph ) ),
          TakeoffWeight=xval(TakeoffWeightValue,UOM)
        );
        (
          rdf( TakeoffWeightNode,aixm:nilReason, NilReason, Graph ),
          TakeoffWeight=nil(NilReason)
        )
      )
  )
  ,( DepartureTimes='$null$',
    \+ rdf( FlightDeparture,fixm:'departureTimes', _DepartureTimes, Graph )
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
          ( rdf( EstimatedOffBlockTimeNode, aixm:uom, UOM, Graph ); rdf( EstimatedOffBlockTimeNode, fixm:uom, UOM, Graph ); rdf( EstimatedOffBlockTimeNode, plain:uom, UOM, Graph ) ),
          EstimatedOffBlockTime=xval(EstimatedOffBlockTimeValue,UOM)
        );
        (
          rdf( EstimatedOffBlockTimeNode,aixm:nilReason, NilReason, Graph ),
          EstimatedOffBlockTime=nil(NilReason)
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
          ( rdf( TaxiTimeNode, aixm:uom, UOM, Graph ); rdf( TaxiTimeNode, fixm:uom, UOM, Graph ); rdf( TaxiTimeNode, plain:uom, UOM, Graph ) ),
          TaxiTime=xval(TaxiTimeValue,UOM)
        );
        (
          rdf( TaxiTimeNode,aixm:nilReason, NilReason, Graph ),
          TaxiTime=nil(NilReason)
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
          ( rdf( TypeNode, aixm:uom, UOM, Graph ); rdf( TypeNode, fixm:uom, UOM, Graph ); rdf( TypeNode, plain:uom, UOM, Graph ) ),
          Type=xval(TypeValue,UOM)
        );
        (
          rdf( TypeNode,aixm:nilReason, NilReason, Graph ),
          Type=nil(NilReason)
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
          ( rdf( PriorPermissionNode, aixm:uom, UOM, Graph ); rdf( PriorPermissionNode, fixm:uom, UOM, Graph ); rdf( PriorPermissionNode, plain:uom, UOM, Graph ) ),
          PriorPermission=xval(PriorPermissionValue,UOM)
        );
        (
          rdf( PriorPermissionNode,aixm:nilReason, NilReason, Graph ),
          PriorPermission=nil(NilReason)
        )
      )
  )
  ,( Selection='$null$',
    \+ rdf( UsageCondition,aixm:'selection', _Selection, Graph )
  )
  ,( Annotation='$null$',
    \+ rdf( UsageCondition,aixm:'annotation', _Annotation, Graph )
  )
  ,( Contact='$null$',
    \+ rdf( UsageCondition,aixm:'contact', _Contact, Graph )
  ) .

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

