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
  ,((AdministrativeArea='$null$',\+ rdf(PostalAddress,fixm:'administrativeArea',_AdministrativeArea,Graph))
  ,(rdf(AdministrativeArea,fixm:'administrativeArea',AdministrativeAreaNode,Graph))
  ,rdf(AdministrativeAreaNode,rdf:value,AdministrativeAreaValue,Graph)
  ,AdministrativeArea=val(AdministrativeAreaValue))
  ,((PostalCode='$null$',\+ rdf(PostalAddress,fixm:'postalCode',_PostalCode,Graph))
  ,(rdf(PostalCode,fixm:'postalCode',PostalCodeNode,Graph))
  ,rdf(PostalCodeNode,rdf:value,PostalCodeValue,Graph)
  ,PostalCode=val(PostalCodeValue))
  ,((DeliveryPoint='$null$',\+ rdf(PostalAddress,fixm:'deliveryPoint',_DeliveryPoint,Graph))
  ,(rdf(DeliveryPoint,fixm:'deliveryPoint',DeliveryPointNode,Graph))
  ,rdf(DeliveryPointNode,rdf:value,DeliveryPointValue,Graph)
  ,DeliveryPoint=val(DeliveryPointValue))
  ,((CountryCode='$null$',\+ rdf(PostalAddress,fixm:'countryCode',_CountryCode,Graph))
  ,(rdf(CountryCode,fixm:'countryCode',CountryCodeNode,Graph))
  ,rdf(CountryCodeNode,rdf:value,CountryCodeValue,Graph)
  ,CountryCode=val(CountryCodeValue))
  ,((CountryName='$null$',\+ rdf(PostalAddress,fixm:'countryName',_CountryName,Graph))
  ,(rdf(CountryName,fixm:'countryName',CountryNameNode,Graph))
  ,rdf(CountryNameNode,rdf:value,CountryNameValue,Graph)
  ,CountryName=val(CountryNameValue))
  ,((City='$null$',\+ rdf(PostalAddress,fixm:'city',_City,Graph))
  ,(rdf(City,fixm:'city',CityNode,Graph))
  ,rdf(CityNode,rdf:value,CityValue,Graph)
  ,City=val(CityValue)) .

fixm_NavigationCapabilities(Graph, NavigationCapabilities, OtherNavigationCapabilities, PerformanceBasedCode, NavigationCode) :-
  rdf(NavigationCapabilities,rdf:type,fixm:'NavigationCapabilities',Graph)
  ,((OtherNavigationCapabilities='$null$',\+ rdf(NavigationCapabilities,fixm:'otherNavigationCapabilities',_OtherNavigationCapabilities,Graph))
  ,(rdf(OtherNavigationCapabilities,fixm:'otherNavigationCapabilities',OtherNavigationCapabilitiesNode,Graph))
  ,rdf(OtherNavigationCapabilitiesNode,rdf:value,OtherNavigationCapabilitiesValue,Graph)
  ,OtherNavigationCapabilities=val(OtherNavigationCapabilitiesValue))

 .

fixm_GroundspeedRange(Graph, GroundspeedRange, LowerSpeed, UpperSpeed) :-
  rdf(GroundspeedRange,rdf:type,fixm:'GroundspeedRange',Graph)
  ,((LowerSpeed='$null$',\+ rdf(GroundspeedRange,fixm:'lowerSpeed',_LowerSpeed,Graph))
  ,(rdf(LowerSpeed,fixm:'lowerSpeed',LowerSpeedNode,Graph))
  ,rdf(LowerSpeedNode,rdf:value,LowerSpeedValue,Graph)
  ,LowerSpeed=val(LowerSpeedValue))
  ,((UpperSpeed='$null$',\+ rdf(GroundspeedRange,fixm:'upperSpeed',_UpperSpeed,Graph))
  ,(rdf(UpperSpeed,fixm:'upperSpeed',UpperSpeedNode,Graph))
  ,rdf(UpperSpeedNode,rdf:value,UpperSpeedValue,Graph)
  ,UpperSpeed=val(UpperSpeedValue)) .

aixm_Note(Graph, Note, PropertyName, Purpose, TranslatedNote) :-
  rdf(Note,rdf:type,aixm:'Note',Graph)
  ,((PropertyName='$null$',\+ rdf(Note,aixm:'propertyName',_PropertyName,Graph))
  ,(rdf(PropertyName,aixm:'propertyName',PropertyNameNode,Graph))
  ,rdf(PropertyNameNode,rdf:value,PropertyNameValue,Graph)
  ,PropertyName=val(PropertyNameValue))
  ,((Purpose='$null$',\+ rdf(Note,aixm:'purpose',_Purpose,Graph))
  ,(rdf(Purpose,aixm:'purpose',PurposeNode,Graph))
  ,rdf(PurposeNode,rdf:value,PurposeValue,Graph)
  ,Purpose=val(PurposeValue))
 .

fixm_Pointout(Graph, Pointout, OriginatingUnit, ReceivingUnit) :-
  rdf(Pointout,rdf:type,fixm:'Pointout',Graph)
  ,((OriginatingUnit='$null$',\+ rdf(Pointout,fixm:'originatingUnit',_OriginatingUnit,Graph))
  ,(rdf(OriginatingUnit,fixm:'originatingUnit',OriginatingUnitNode,Graph))
  ,rdf(OriginatingUnitNode,rdf:value,OriginatingUnitValue,Graph)
  ,OriginatingUnit=val(OriginatingUnitValue))
 .

fixm_VerticalRange(Graph, VerticalRange, LowerBound, UpperBound) :-
  rdf(VerticalRange,rdf:type,fixm:'VerticalRange',Graph)
  ,((LowerBound='$null$',\+ rdf(VerticalRange,fixm:'lowerBound',_LowerBound,Graph))
  ,(rdf(LowerBound,fixm:'lowerBound',LowerBoundNode,Graph))
  ,rdf(LowerBoundNode,rdf:value,LowerBoundValue,Graph)
  ,LowerBound=val(LowerBoundValue))
  ,((UpperBound='$null$',\+ rdf(VerticalRange,fixm:'upperBound',_UpperBound,Graph))
  ,(rdf(UpperBound,fixm:'upperBound',UpperBoundNode,Graph))
  ,rdf(UpperBoundNode,rdf:value,UpperBoundValue,Graph)
  ,UpperBound=val(UpperBoundValue)) .

fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel, EstimatedTime, Constraint) :-
  rdf(ExpandedRoutePoint,rdf:type,fixm:'ExpandedRoutePoint',Graph)
  ,((EstimatedLevel='$null$',\+ rdf(ExpandedRoutePoint,fixm:'estimatedLevel',_EstimatedLevel,Graph))
  ,(rdf(EstimatedLevel,fixm:'estimatedLevel',EstimatedLevelNode,Graph))
  ,rdf(EstimatedLevelNode,rdf:value,EstimatedLevelValue,Graph)
  ,EstimatedLevel=val(EstimatedLevelValue))
  ,((EstimatedTime='$null$',\+ rdf(ExpandedRoutePoint,fixm:'estimatedTime',_EstimatedTime,Graph))
  ,(rdf(EstimatedTime,fixm:'estimatedTime',EstimatedTimeNode,Graph))
  ,rdf(EstimatedTimeNode,rdf:value,EstimatedTimeValue,Graph)
  ,EstimatedTime=val(EstimatedTimeValue))
 .

aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  rdf(ElevatedSurface,rdf:type,aixm:'ElevatedSurface',Graph)
  ,((Elevation='$null$',\+ rdf(ElevatedSurface,aixm:'elevation',_Elevation,Graph))
  ,(rdf(Elevation,aixm:'elevation',ElevationNode,Graph))
  ,rdf(ElevationNode,rdf:value,ElevationValue,Graph)
  ,Elevation=val(ElevationValue))
  ,((GeoidUndulation='$null$',\+ rdf(ElevatedSurface,aixm:'geoidUndulation',_GeoidUndulation,Graph))
  ,(rdf(GeoidUndulation,aixm:'geoidUndulation',GeoidUndulationNode,Graph))
  ,rdf(GeoidUndulationNode,rdf:value,GeoidUndulationValue,Graph)
  ,GeoidUndulation=val(GeoidUndulationValue))
  ,((VerticalDatum='$null$',\+ rdf(ElevatedSurface,aixm:'verticalDatum',_VerticalDatum,Graph))
  ,(rdf(VerticalDatum,aixm:'verticalDatum',VerticalDatumNode,Graph))
  ,rdf(VerticalDatumNode,rdf:value,VerticalDatumValue,Graph)
  ,VerticalDatum=val(VerticalDatumValue))
  ,((VerticalAccuracy='$null$',\+ rdf(ElevatedSurface,aixm:'verticalAccuracy',_VerticalAccuracy,Graph))
  ,(rdf(VerticalAccuracy,aixm:'verticalAccuracy',VerticalAccuracyNode,Graph))
  ,rdf(VerticalAccuracyNode,rdf:value,VerticalAccuracyValue,Graph)
  ,VerticalAccuracy=val(VerticalAccuracyValue)) .

fixm_Dimensions(Graph, Dimensions, Height, Length, Width) :-
  rdf(Dimensions,rdf:type,fixm:'Dimensions',Graph)
  ,((Height='$null$',\+ rdf(Dimensions,fixm:'height',_Height,Graph))
  ,(rdf(Height,fixm:'height',HeightNode,Graph))
  ,rdf(HeightNode,rdf:value,HeightValue,Graph)
  ,Height=val(HeightValue))
  ,((Length='$null$',\+ rdf(Dimensions,fixm:'length',_Length,Graph))
  ,(rdf(Length,fixm:'length',LengthNode,Graph))
  ,rdf(LengthNode,rdf:value,LengthValue,Graph)
  ,Length=val(LengthValue))
  ,((Width='$null$',\+ rdf(Dimensions,fixm:'width',_Width,Graph))
  ,(rdf(Width,fixm:'width',WidthNode,Graph))
  ,rdf(WidthNode,rdf:value,WidthValue,Graph)
  ,Width=val(WidthValue)) .

fixm_StandPositionAndTime(Graph, StandPositionAndTime, StandName, StandTime, TerminalName) :-
  rdf(StandPositionAndTime,rdf:type,fixm:'StandPositionAndTime',Graph)
  ,((StandName='$null$',\+ rdf(StandPositionAndTime,fixm:'standName',_StandName,Graph))
  ,(rdf(StandName,fixm:'standName',StandNameNode,Graph))
  ,rdf(StandNameNode,rdf:value,StandNameValue,Graph)
  ,StandName=val(StandNameValue))
  ,((StandTime='$null$',\+ rdf(StandPositionAndTime,fixm:'standTime',_StandTime,Graph))
  ,(rdf(StandTime,fixm:'standTime',StandTimeNode,Graph))
  ,rdf(StandTimeNode,rdf:value,StandTimeValue,Graph)
  ,StandTime=val(StandTimeValue))
  ,((TerminalName='$null$',\+ rdf(StandPositionAndTime,fixm:'terminalName',_TerminalName,Graph))
  ,(rdf(TerminalName,fixm:'terminalName',TerminalNameNode,Graph))
  ,rdf(TerminalNameNode,rdf:value,TerminalNameValue,Graph)
  ,TerminalName=val(TerminalNameValue)) .

fixm_RouteSegment(Graph, RouteSegment, Airway, RoutePoint) :-
  rdf(RouteSegment,rdf:type,fixm:'RouteSegment',Graph)
  ,((Airway='$null$',\+ rdf(RouteSegment,fixm:'airway',_Airway,Graph))
  ,(rdf(Airway,fixm:'airway',AirwayNode,Graph))
  ,rdf(AirwayNode,rdf:value,AirwayValue,Graph)
  ,Airway=val(AirwayValue))
  ,((RoutePoint='$null$',\+ rdf(RouteSegment,fixm:'routePoint',_RoutePoint,Graph))
  ,(rdf(RoutePoint,fixm:'routePoint',RoutePointNode,Graph))
  ,rdf(RoutePointNode,rdf:value,RoutePointValue,Graph)
  ,RoutePoint=val(RoutePointValue)) .

aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator, Flight, Aircraft, Weather, SubCondition) :-
  rdf(ConditionCombination,rdf:type,aixm:'ConditionCombination',Graph)
  ,((LogicalOperator='$null$',\+ rdf(ConditionCombination,aixm:'logicalOperator',_LogicalOperator,Graph))
  ,(rdf(LogicalOperator,aixm:'logicalOperator',LogicalOperatorNode,Graph))
  ,rdf(LogicalOperatorNode,rdf:value,LogicalOperatorValue,Graph)
  ,LogicalOperator=val(LogicalOperatorValue))



 .

aixm_SurfaceContaminationLayer(Graph, SurfaceContaminationLayer, LayerOrder, Type, Extent, Annotation) :-
  rdf(SurfaceContaminationLayer,rdf:type,aixm:'SurfaceContaminationLayer',Graph)
  ,((LayerOrder='$null$',\+ rdf(SurfaceContaminationLayer,aixm:'layerOrder',_LayerOrder,Graph))
  ,(rdf(LayerOrder,aixm:'layerOrder',LayerOrderNode,Graph))
  ,rdf(LayerOrderNode,rdf:value,LayerOrderValue,Graph)
  ,LayerOrder=val(LayerOrderValue))
  ,((Type='$null$',\+ rdf(SurfaceContaminationLayer,aixm:'type',_Type,Graph))
  ,(rdf(Type,aixm:'type',TypeNode,Graph))
  ,rdf(TypeNode,rdf:value,TypeValue,Graph)
  ,Type=val(TypeValue))

 .

fixm_Organization(Graph, Organization, Name, OtherOrganization, Contact) :-
  rdf(Organization,rdf:type,fixm:'Organization',Graph)
  ,((Name='$null$',\+ rdf(Organization,fixm:'name',_Name,Graph))
  ,(rdf(Name,fixm:'name',NameNode,Graph))
  ,rdf(NameNode,rdf:value,NameValue,Graph)
  ,Name=val(NameValue))
  ,((OtherOrganization='$null$',\+ rdf(Organization,fixm:'otherOrganization',_OtherOrganization,Graph))
  ,(rdf(OtherOrganization,fixm:'otherOrganization',OtherOrganizationNode,Graph))
  ,rdf(OtherOrganizationNode,rdf:value,OtherOrganizationValue,Graph)
  ,OtherOrganization=val(OtherOrganizationValue))
  ,((Contact='$null$',\+ rdf(Organization,fixm:'contact',_Contact,Graph))
  ,(rdf(Contact,fixm:'contact',ContactNode,Graph))
  ,rdf(ContactNode,rdf:value,ContactValue,Graph)
  ,Contact=val(ContactValue)) .

aixm_OrganisationAuthorityAssociation(Graph, OrganisationAuthorityAssociation, Type, Annotation, TheOrganisationAuthority) :-
  rdf(OrganisationAuthorityAssociation,rdf:type,aixm:'OrganisationAuthorityAssociation',Graph)
  ,((Type='$null$',\+ rdf(OrganisationAuthorityAssociation,aixm:'type',_Type,Graph))
  ,(rdf(Type,aixm:'type',TypeNode,Graph))
  ,rdf(TypeNode,rdf:value,TypeValue,Graph)
  ,Type=val(TypeValue))

  ,rdf(OrganisationAuthorityAssociation,aixm:'theOrganisationAuthority',TheOrganisationAuthority,Graph) .

aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  rdf(ElevatedPoint,rdf:type,aixm:'ElevatedPoint',Graph)
  ,((Elevation='$null$',\+ rdf(ElevatedPoint,aixm:'elevation',_Elevation,Graph))
  ,(rdf(Elevation,aixm:'elevation',ElevationNode,Graph))
  ,rdf(ElevationNode,rdf:value,ElevationValue,Graph)
  ,Elevation=val(ElevationValue))
  ,((GeoidUndulation='$null$',\+ rdf(ElevatedPoint,aixm:'geoidUndulation',_GeoidUndulation,Graph))
  ,(rdf(GeoidUndulation,aixm:'geoidUndulation',GeoidUndulationNode,Graph))
  ,rdf(GeoidUndulationNode,rdf:value,GeoidUndulationValue,Graph)
  ,GeoidUndulation=val(GeoidUndulationValue))
  ,((VerticalDatum='$null$',\+ rdf(ElevatedPoint,aixm:'verticalDatum',_VerticalDatum,Graph))
  ,(rdf(VerticalDatum,aixm:'verticalDatum',VerticalDatumNode,Graph))
  ,rdf(VerticalDatumNode,rdf:value,VerticalDatumValue,Graph)
  ,VerticalDatum=val(VerticalDatumValue))
  ,((VerticalAccuracy='$null$',\+ rdf(ElevatedPoint,aixm:'verticalAccuracy',_VerticalAccuracy,Graph))
  ,(rdf(VerticalAccuracy,aixm:'verticalAccuracy',VerticalAccuracyNode,Graph))
  ,rdf(VerticalAccuracyNode,rdf:value,VerticalAccuracyValue,Graph)
  ,VerticalAccuracy=val(VerticalAccuracyValue)) .

fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel) :-
  rdf(EfplPoint4D,rdf:type,fixm:'EfplPoint4D',Graph)
  ,((FlightLevel='$null$',\+ rdf(EfplPoint4D,fixm:'flightLevel',_FlightLevel,Graph))
  ,(rdf(FlightLevel,fixm:'flightLevel',FlightLevelNode,Graph))
  ,rdf(FlightLevelNode,rdf:value,FlightLevelValue,Graph)
  ,FlightLevel=val(FlightLevelValue)) .

fixm_AircraftOperator(Graph, AircraftOperator, OperatingOrganization, OperatorCategory) :-
  rdf(AircraftOperator,rdf:type,fixm:'AircraftOperator',Graph)
  ,((OperatingOrganization='$null$',\+ rdf(AircraftOperator,fixm:'operatingOrganization',_OperatingOrganization,Graph))
  ,(rdf(OperatingOrganization,fixm:'operatingOrganization',OperatingOrganizationNode,Graph))
  ,rdf(OperatingOrganizationNode,rdf:value,OperatingOrganizationValue,Graph)
  ,OperatingOrganization=val(OperatingOrganizationValue))
  ,((OperatorCategory='$null$',\+ rdf(AircraftOperator,fixm:'operatorCategory',_OperatorCategory,Graph))
  ,(rdf(OperatorCategory,fixm:'operatorCategory',OperatorCategoryNode,Graph))
  ,rdf(OperatorCategoryNode,rdf:value,OperatorCategoryValue,Graph)
  ,OperatorCategory=val(OperatorCategoryValue)) .

gml_Point(Graph, Point) :-
  subClassOf(T,gml:'Point')
  ,rdf(Point,rdf:type,T,Graph) .

fixm_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair) :-
  rdf(EfplTrajectoryRoutePair,rdf:type,fixm:'EfplTrajectoryRoutePair',Graph) .

fixm_RoutePoint(Graph, RoutePoint, Constraint) :-
  rdf(RoutePoint,rdf:type,fixm:'RoutePoint',Graph)
 .

fixm_BeaconCodeAssignment(Graph, BeaconCodeAssignment, CurrentBeaconCode, PreviousBeaconCode, ReassignedBeaconCode, ReassigningUnit) :-
  rdf(BeaconCodeAssignment,rdf:type,fixm:'BeaconCodeAssignment',Graph)
  ,((CurrentBeaconCode='$null$',\+ rdf(BeaconCodeAssignment,fixm:'currentBeaconCode',_CurrentBeaconCode,Graph))
  ,(rdf(CurrentBeaconCode,fixm:'currentBeaconCode',CurrentBeaconCodeNode,Graph))
  ,rdf(CurrentBeaconCodeNode,rdf:value,CurrentBeaconCodeValue,Graph)
  ,CurrentBeaconCode=val(CurrentBeaconCodeValue))
  ,((PreviousBeaconCode='$null$',\+ rdf(BeaconCodeAssignment,fixm:'previousBeaconCode',_PreviousBeaconCode,Graph))
  ,(rdf(PreviousBeaconCode,fixm:'previousBeaconCode',PreviousBeaconCodeNode,Graph))
  ,rdf(PreviousBeaconCodeNode,rdf:value,PreviousBeaconCodeValue,Graph)
  ,PreviousBeaconCode=val(PreviousBeaconCodeValue))
  ,((ReassignedBeaconCode='$null$',\+ rdf(BeaconCodeAssignment,fixm:'reassignedBeaconCode',_ReassignedBeaconCode,Graph))
  ,(rdf(ReassignedBeaconCode,fixm:'reassignedBeaconCode',ReassignedBeaconCodeNode,Graph))
  ,rdf(ReassignedBeaconCodeNode,rdf:value,ReassignedBeaconCodeValue,Graph)
  ,ReassignedBeaconCode=val(ReassignedBeaconCodeValue))
  ,((ReassigningUnit='$null$',\+ rdf(BeaconCodeAssignment,fixm:'reassigningUnit',_ReassigningUnit,Graph))
  ,(rdf(ReassigningUnit,fixm:'reassigningUnit',ReassigningUnitNode,Graph))
  ,rdf(ReassigningUnitNode,rdf:value,ReassigningUnitValue,Graph)
  ,ReassigningUnit=val(ReassigningUnitValue)) .

fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile) :-
  rdf(FlightPerformanceData,rdf:type,fixm:'FlightPerformanceData',Graph)

 .

fixm_ExpandedRoute(Graph, ExpandedRoute, RoutePoint) :-
  rdf(ExpandedRoute,rdf:type,fixm:'ExpandedRoute',Graph)
 .

fixm_RouteConstraintOrPreference(Graph, RouteConstraintOrPreference, ConstraintType) :-
  rdf(RouteConstraintOrPreference,rdf:type,fixm:'RouteConstraintOrPreference',Graph)
  ,((ConstraintType='$null$',\+ rdf(RouteConstraintOrPreference,fixm:'constraintType',_ConstraintType,Graph))
  ,(rdf(ConstraintType,fixm:'constraintType',ConstraintTypeNode,Graph))
  ,rdf(ConstraintTypeNode,rdf:value,ConstraintTypeValue,Graph)
  ,ConstraintType=val(ConstraintTypeValue)) .

fixm_DeclarationText(Graph, DeclarationText, Compliance, Consignor, Shipper) :-
  rdf(DeclarationText,rdf:type,fixm:'DeclarationText',Graph)
  ,((Compliance='$null$',\+ rdf(DeclarationText,fixm:'compliance',_Compliance,Graph))
  ,(rdf(Compliance,fixm:'compliance',ComplianceNode,Graph))
  ,rdf(ComplianceNode,rdf:value,ComplianceValue,Graph)
  ,Compliance=val(ComplianceValue))
  ,((Consignor='$null$',\+ rdf(DeclarationText,fixm:'consignor',_Consignor,Graph))
  ,(rdf(Consignor,fixm:'consignor',ConsignorNode,Graph))
  ,rdf(ConsignorNode,rdf:value,ConsignorValue,Graph)
  ,Consignor=val(ConsignorValue))
  ,((Shipper='$null$',\+ rdf(DeclarationText,fixm:'shipper',_Shipper,Graph))
  ,(rdf(Shipper,fixm:'shipper',ShipperNode,Graph))
  ,rdf(ShipperNode,rdf:value,ShipperValue,Graph)
  ,Shipper=val(ShipperValue)) .

fixm_EstimatedElapsedTime(Graph, EstimatedElapsedTime, ElapsedTime, Location) :-
  rdf(EstimatedElapsedTime,rdf:type,fixm:'EstimatedElapsedTime',Graph)
  ,((ElapsedTime='$null$',\+ rdf(EstimatedElapsedTime,fixm:'elapsedTime',_ElapsedTime,Graph))
  ,(rdf(ElapsedTime,fixm:'elapsedTime',ElapsedTimeNode,Graph))
  ,rdf(ElapsedTimeNode,rdf:value,ElapsedTimeValue,Graph)
  ,ElapsedTime=val(ElapsedTimeValue))
  ,((Location='$null$',\+ rdf(EstimatedElapsedTime,fixm:'location',_Location,Graph))
  ,(rdf(Location,fixm:'location',LocationNode,Graph))
  ,rdf(LocationNode,rdf:value,LocationValue,Graph)
  ,Location=val(LocationValue)) .

fixm_ReportedTime(Graph, ReportedTime, Provenance, Time) :-
  rdf(ReportedTime,rdf:type,fixm:'ReportedTime',Graph)
  ,((Provenance='$null$',\+ rdf(ReportedTime,fixm:'provenance',_Provenance,Graph))
  ,(rdf(Provenance,fixm:'provenance',ProvenanceNode,Graph))
  ,rdf(ProvenanceNode,rdf:value,ProvenanceValue,Graph)
  ,Provenance=val(ProvenanceValue))
  ,((Time='$null$',\+ rdf(ReportedTime,fixm:'time',_Time,Graph))
  ,(rdf(Time,fixm:'time',TimeNode,Graph))
  ,rdf(TimeNode,rdf:value,TimeValue,Graph)
  ,Time=val(TimeValue)) .

fixm_GeographicLocation(Graph, GeographicLocation, PosList, SrsName) :-
  subClassOf(T,fixm:'GeographicLocation')
  ,rdf(GeographicLocation,rdf:type,T,Graph)

  ,((SrsName='$null$',\+ rdf(GeographicLocation,fixm:'srsName',_SrsName,Graph))
  ,(rdf(SrsName,fixm:'srsName',SrsNameNode,Graph))
  ,rdf(SrsNameNode,rdf:value,SrsNameValue,Graph)
  ,SrsName=val(SrsNameValue)) .

aixm_LinguisticNote(Graph, LinguisticNote, Note) :-
  rdf(LinguisticNote,rdf:type,aixm:'LinguisticNote',Graph)
  ,((Note='$null$',\+ rdf(LinguisticNote,aixm:'note',_Note,Graph))
  ,(rdf(Note,aixm:'note',NoteNode,Graph))
  ,rdf(NoteNode,rdf:value,NoteValue,Graph)
  ,Note=val(NoteValue)) .

aixm_Meteorology(Graph, Meteorology, FlightConditions, Visibility, VisibilityInterpretation, RunwayVisualRange, RunwayVisualRangeInterpretation, Annotation) :-
  rdf(Meteorology,rdf:type,aixm:'Meteorology',Graph)
  ,((FlightConditions='$null$',\+ rdf(Meteorology,aixm:'flightConditions',_FlightConditions,Graph))
  ,(rdf(FlightConditions,aixm:'flightConditions',FlightConditionsNode,Graph))
  ,rdf(FlightConditionsNode,rdf:value,FlightConditionsValue,Graph)
  ,FlightConditions=val(FlightConditionsValue))
  ,((Visibility='$null$',\+ rdf(Meteorology,aixm:'visibility',_Visibility,Graph))
  ,(rdf(Visibility,aixm:'visibility',VisibilityNode,Graph))
  ,rdf(VisibilityNode,rdf:value,VisibilityValue,Graph)
  ,Visibility=val(VisibilityValue))
  ,((VisibilityInterpretation='$null$',\+ rdf(Meteorology,aixm:'visibilityInterpretation',_VisibilityInterpretation,Graph))
  ,(rdf(VisibilityInterpretation,aixm:'visibilityInterpretation',VisibilityInterpretationNode,Graph))
  ,rdf(VisibilityInterpretationNode,rdf:value,VisibilityInterpretationValue,Graph)
  ,VisibilityInterpretation=val(VisibilityInterpretationValue))
  ,((RunwayVisualRange='$null$',\+ rdf(Meteorology,aixm:'runwayVisualRange',_RunwayVisualRange,Graph))
  ,(rdf(RunwayVisualRange,aixm:'runwayVisualRange',RunwayVisualRangeNode,Graph))
  ,rdf(RunwayVisualRangeNode,rdf:value,RunwayVisualRangeValue,Graph)
  ,RunwayVisualRange=val(RunwayVisualRangeValue))
  ,((RunwayVisualRangeInterpretation='$null$',\+ rdf(Meteorology,aixm:'runwayVisualRangeInterpretation',_RunwayVisualRangeInterpretation,Graph))
  ,(rdf(RunwayVisualRangeInterpretation,aixm:'runwayVisualRangeInterpretation',RunwayVisualRangeInterpretationNode,Graph))
  ,rdf(RunwayVisualRangeInterpretationNode,rdf:value,RunwayVisualRangeInterpretationValue,Graph)
  ,RunwayVisualRangeInterpretation=val(RunwayVisualRangeInterpretationValue))
 .

fixm_PointRange(Graph, PointRange, LateralRange, VerticalRange, TemporalRange) :-
  rdf(PointRange,rdf:type,fixm:'PointRange',Graph)
  ,((LateralRange='$null$',\+ rdf(PointRange,fixm:'lateralRange',_LateralRange,Graph))
  ,(rdf(LateralRange,fixm:'lateralRange',LateralRangeNode,Graph))
  ,rdf(LateralRangeNode,rdf:value,LateralRangeValue,Graph)
  ,LateralRange=val(LateralRangeValue))
  ,((VerticalRange='$null$',\+ rdf(PointRange,fixm:'verticalRange',_VerticalRange,Graph))
  ,(rdf(VerticalRange,fixm:'verticalRange',VerticalRangeNode,Graph))
  ,rdf(VerticalRangeNode,rdf:value,VerticalRangeValue,Graph)
  ,VerticalRange=val(VerticalRangeValue))
  ,((TemporalRange='$null$',\+ rdf(PointRange,fixm:'temporalRange',_TemporalRange,Graph))
  ,(rdf(TemporalRange,fixm:'temporalRange',TemporalRangeNode,Graph))
  ,rdf(TemporalRangeNode,rdf:value,TemporalRangeValue,Graph)
  ,TemporalRange=val(TemporalRangeValue)) .

aixm_City(Graph, City, Name, Annotation) :-
  rdf(City,rdf:type,aixm:'City',Graph)
  ,((Name='$null$',\+ rdf(City,aixm:'name',_Name,Graph))
  ,(rdf(Name,aixm:'name',NameNode,Graph))
  ,rdf(NameNode,rdf:value,NameValue,Graph)
  ,Name=val(NameValue))
 .

aixm_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role, TheOrganisationAuthority) :-
  rdf(AirportHeliportResponsibilityOrganisation,rdf:type,aixm:'AirportHeliportResponsibilityOrganisation',Graph)
  ,((Role='$null$',\+ rdf(AirportHeliportResponsibilityOrganisation,aixm:'role',_Role,Graph))
  ,(rdf(Role,aixm:'role',RoleNode,Graph))
  ,rdf(RoleNode,rdf:value,RoleValue,Graph)
  ,Role=val(RoleValue))
  ,rdf(AirportHeliportResponsibilityOrganisation,aixm:'theOrganisationAuthority',TheOrganisationAuthority,Graph) .

fixm_AirspeedRange(Graph, AirspeedRange, LowerSpeed, UpperSpeed) :-
  rdf(AirspeedRange,rdf:type,fixm:'AirspeedRange',Graph)
  ,((LowerSpeed='$null$',\+ rdf(AirspeedRange,fixm:'lowerSpeed',_LowerSpeed,Graph))
  ,(rdf(LowerSpeed,fixm:'lowerSpeed',LowerSpeedNode,Graph))
  ,rdf(LowerSpeedNode,rdf:value,LowerSpeedValue,Graph)
  ,LowerSpeed=val(LowerSpeedValue))
  ,((UpperSpeed='$null$',\+ rdf(AirspeedRange,fixm:'upperSpeed',_UpperSpeed,Graph))
  ,(rdf(UpperSpeed,fixm:'upperSpeed',UpperSpeedNode,Graph))
  ,rdf(UpperSpeedNode,rdf:value,UpperSpeedValue,Graph)
  ,UpperSpeed=val(UpperSpeedValue)) .

fixm_RankedTrajectory(Graph, RankedTrajectory, Identifier, MaximumAcceptableDelay, AssignedIndicator, RouteTrajectoryPair) :-
  rdf(RankedTrajectory,rdf:type,fixm:'RankedTrajectory',Graph)
  ,((Identifier='$null$',\+ rdf(RankedTrajectory,fixm:'identifier',_Identifier,Graph))
  ,(rdf(Identifier,fixm:'identifier',IdentifierNode,Graph))
  ,rdf(IdentifierNode,rdf:value,IdentifierValue,Graph)
  ,Identifier=val(IdentifierValue))
  ,((MaximumAcceptableDelay='$null$',\+ rdf(RankedTrajectory,fixm:'maximumAcceptableDelay',_MaximumAcceptableDelay,Graph))
  ,(rdf(MaximumAcceptableDelay,fixm:'maximumAcceptableDelay',MaximumAcceptableDelayNode,Graph))
  ,rdf(MaximumAcceptableDelayNode,rdf:value,MaximumAcceptableDelayValue,Graph)
  ,MaximumAcceptableDelay=val(MaximumAcceptableDelayValue))
  ,((AssignedIndicator='$null$',\+ rdf(RankedTrajectory,fixm:'assignedIndicator',_AssignedIndicator,Graph))
  ,(rdf(AssignedIndicator,fixm:'assignedIndicator',AssignedIndicatorNode,Graph))
  ,rdf(AssignedIndicatorNode,rdf:value,AssignedIndicatorValue,Graph)
  ,AssignedIndicator=val(AssignedIndicatorValue))
  ,((RouteTrajectoryPair='$null$',\+ rdf(RankedTrajectory,fixm:'routeTrajectoryPair',_RouteTrajectoryPair,Graph))
  ,(rdf(RouteTrajectoryPair,fixm:'routeTrajectoryPair',RouteTrajectoryPairNode,Graph))
  ,rdf(RouteTrajectoryPairNode,rdf:value,RouteTrajectoryPairValue,Graph)
  ,RouteTrajectoryPair=val(RouteTrajectoryPairValue)) .

fixm_TrajectoryPointRole(Graph, TrajectoryPointRole, BottomOfClimb, BottomOfDescent, BoundaryPoint, FromGATToOAT, FromIFRToVFR, FromOATToGat, FromVFRToIFR, TopOfClimb, TopOfDescent) :-
  rdf(TrajectoryPointRole,rdf:type,fixm:'TrajectoryPointRole',Graph)
  ,((BottomOfClimb='$null$',\+ rdf(TrajectoryPointRole,fixm:'bottomOfClimb',_BottomOfClimb,Graph))
  ,(rdf(BottomOfClimb,fixm:'bottomOfClimb',BottomOfClimbNode,Graph))
  ,rdf(BottomOfClimbNode,rdf:value,BottomOfClimbValue,Graph)
  ,BottomOfClimb=val(BottomOfClimbValue))
  ,((BottomOfDescent='$null$',\+ rdf(TrajectoryPointRole,fixm:'bottomOfDescent',_BottomOfDescent,Graph))
  ,(rdf(BottomOfDescent,fixm:'bottomOfDescent',BottomOfDescentNode,Graph))
  ,rdf(BottomOfDescentNode,rdf:value,BottomOfDescentValue,Graph)
  ,BottomOfDescent=val(BottomOfDescentValue))
  ,((BoundaryPoint='$null$',\+ rdf(TrajectoryPointRole,fixm:'boundaryPoint',_BoundaryPoint,Graph))
  ,(rdf(BoundaryPoint,fixm:'boundaryPoint',BoundaryPointNode,Graph))
  ,rdf(BoundaryPointNode,rdf:value,BoundaryPointValue,Graph)
  ,BoundaryPoint=val(BoundaryPointValue))
  ,((FromGATToOAT='$null$',\+ rdf(TrajectoryPointRole,fixm:'fromGATToOAT',_FromGATToOAT,Graph))
  ,(rdf(FromGATToOAT,fixm:'fromGATToOAT',FromGATToOATNode,Graph))
  ,rdf(FromGATToOATNode,rdf:value,FromGATToOATValue,Graph)
  ,FromGATToOAT=val(FromGATToOATValue))
  ,((FromIFRToVFR='$null$',\+ rdf(TrajectoryPointRole,fixm:'fromIFRToVFR',_FromIFRToVFR,Graph))
  ,(rdf(FromIFRToVFR,fixm:'fromIFRToVFR',FromIFRToVFRNode,Graph))
  ,rdf(FromIFRToVFRNode,rdf:value,FromIFRToVFRValue,Graph)
  ,FromIFRToVFR=val(FromIFRToVFRValue))
  ,((FromOATToGat='$null$',\+ rdf(TrajectoryPointRole,fixm:'fromOATToGat',_FromOATToGat,Graph))
  ,(rdf(FromOATToGat,fixm:'fromOATToGat',FromOATToGatNode,Graph))
  ,rdf(FromOATToGatNode,rdf:value,FromOATToGatValue,Graph)
  ,FromOATToGat=val(FromOATToGatValue))
  ,((FromVFRToIFR='$null$',\+ rdf(TrajectoryPointRole,fixm:'fromVFRToIFR',_FromVFRToIFR,Graph))
  ,(rdf(FromVFRToIFR,fixm:'fromVFRToIFR',FromVFRToIFRNode,Graph))
  ,rdf(FromVFRToIFRNode,rdf:value,FromVFRToIFRValue,Graph)
  ,FromVFRToIFR=val(FromVFRToIFRValue))
  ,((TopOfClimb='$null$',\+ rdf(TrajectoryPointRole,fixm:'topOfClimb',_TopOfClimb,Graph))
  ,(rdf(TopOfClimb,fixm:'topOfClimb',TopOfClimbNode,Graph))
  ,rdf(TopOfClimbNode,rdf:value,TopOfClimbValue,Graph)
  ,TopOfClimb=val(TopOfClimbValue))
  ,((TopOfDescent='$null$',\+ rdf(TrajectoryPointRole,fixm:'topOfDescent',_TopOfDescent,Graph))
  ,(rdf(TopOfDescent,fixm:'topOfDescent',TopOfDescentNode,Graph))
  ,rdf(TopOfDescentNode,rdf:value,TopOfDescentValue,Graph)
  ,TopOfDescent=val(TopOfDescentValue)) .

fixm_CommunicationCapabilities(Graph, CommunicationCapabilities, OtherCommunicationCapabilities, OtherDataLinkCapabilities, DataLinkCode, SelectiveCallingCode, CommunicationCode) :-
  rdf(CommunicationCapabilities,rdf:type,fixm:'CommunicationCapabilities',Graph)
  ,((OtherCommunicationCapabilities='$null$',\+ rdf(CommunicationCapabilities,fixm:'otherCommunicationCapabilities',_OtherCommunicationCapabilities,Graph))
  ,(rdf(OtherCommunicationCapabilities,fixm:'otherCommunicationCapabilities',OtherCommunicationCapabilitiesNode,Graph))
  ,rdf(OtherCommunicationCapabilitiesNode,rdf:value,OtherCommunicationCapabilitiesValue,Graph)
  ,OtherCommunicationCapabilities=val(OtherCommunicationCapabilitiesValue))
  ,((OtherDataLinkCapabilities='$null$',\+ rdf(CommunicationCapabilities,fixm:'otherDataLinkCapabilities',_OtherDataLinkCapabilities,Graph))
  ,(rdf(OtherDataLinkCapabilities,fixm:'otherDataLinkCapabilities',OtherDataLinkCapabilitiesNode,Graph))
  ,rdf(OtherDataLinkCapabilitiesNode,rdf:value,OtherDataLinkCapabilitiesValue,Graph)
  ,OtherDataLinkCapabilities=val(OtherDataLinkCapabilitiesValue))

  ,((SelectiveCallingCode='$null$',\+ rdf(CommunicationCapabilities,fixm:'selectiveCallingCode',_SelectiveCallingCode,Graph))
  ,(rdf(SelectiveCallingCode,fixm:'selectiveCallingCode',SelectiveCallingCodeNode,Graph))
  ,rdf(SelectiveCallingCodeNode,rdf:value,SelectiveCallingCodeValue,Graph)
  ,SelectiveCallingCode=val(SelectiveCallingCodeValue))
 .

fixm_Dinghy(Graph, Dinghy, Quantity, TotalCapacity, Covered, Colour) :-
  rdf(Dinghy,rdf:type,fixm:'Dinghy',Graph)
  ,((Quantity='$null$',\+ rdf(Dinghy,fixm:'quantity',_Quantity,Graph))
  ,(rdf(Quantity,fixm:'quantity',QuantityNode,Graph))
  ,rdf(QuantityNode,rdf:value,QuantityValue,Graph)
  ,Quantity=val(QuantityValue))
  ,((TotalCapacity='$null$',\+ rdf(Dinghy,fixm:'totalCapacity',_TotalCapacity,Graph))
  ,(rdf(TotalCapacity,fixm:'totalCapacity',TotalCapacityNode,Graph))
  ,rdf(TotalCapacityNode,rdf:value,TotalCapacityValue,Graph)
  ,TotalCapacity=val(TotalCapacityValue))
  ,((Covered='$null$',\+ rdf(Dinghy,fixm:'covered',_Covered,Graph))
  ,(rdf(Covered,fixm:'covered',CoveredNode,Graph))
  ,rdf(CoveredNode,rdf:value,CoveredValue,Graph)
  ,Covered=val(CoveredValue))
  ,((Colour='$null$',\+ rdf(Dinghy,fixm:'colour',_Colour,Graph))
  ,(rdf(Colour,fixm:'colour',ColourNode,Graph))
  ,rdf(ColourNode,rdf:value,ColourValue,Graph)
  ,Colour=val(ColourValue)) .

aixm_ContactInformation(Graph, ContactInformation, Name, Title, Annotation, NetworkNode, Address, PhoneFax) :-
  rdf(ContactInformation,rdf:type,aixm:'ContactInformation',Graph)
  ,((Name='$null$',\+ rdf(ContactInformation,aixm:'name',_Name,Graph))
  ,(rdf(Name,aixm:'name',NameNode,Graph))
  ,rdf(NameNode,rdf:value,NameValue,Graph)
  ,Name=val(NameValue))
  ,((Title='$null$',\+ rdf(ContactInformation,aixm:'title',_Title,Graph))
  ,(rdf(Title,aixm:'title',TitleNode,Graph))
  ,rdf(TitleNode,rdf:value,TitleValue,Graph)
  ,Title=val(TitleValue))



 .

fixm_PlannedReportingPosition(Graph, PlannedReportingPosition, Position, PositionAltitude, PositionEstimatedTime) :-
  rdf(PlannedReportingPosition,rdf:type,fixm:'PlannedReportingPosition',Graph)
  ,((Position='$null$',\+ rdf(PlannedReportingPosition,fixm:'position',_Position,Graph))
  ,(rdf(Position,fixm:'position',PositionNode,Graph))
  ,rdf(PositionNode,rdf:value,PositionValue,Graph)
  ,Position=val(PositionValue))
  ,((PositionAltitude='$null$',\+ rdf(PlannedReportingPosition,fixm:'positionAltitude',_PositionAltitude,Graph))
  ,(rdf(PositionAltitude,fixm:'positionAltitude',PositionAltitudeNode,Graph))
  ,rdf(PositionAltitudeNode,rdf:value,PositionAltitudeValue,Graph)
  ,PositionAltitude=val(PositionAltitudeValue))
  ,((PositionEstimatedTime='$null$',\+ rdf(PlannedReportingPosition,fixm:'positionEstimatedTime',_PositionEstimatedTime,Graph))
  ,(rdf(PositionEstimatedTime,fixm:'positionEstimatedTime',PositionEstimatedTimeNode,Graph))
  ,rdf(PositionEstimatedTimeNode,rdf:value,PositionEstimatedTimeValue,Graph)
  ,PositionEstimatedTime=val(PositionEstimatedTimeValue)) .

fixm_SignificantPoint(Graph, SignificantPoint) :-
  rdf(SignificantPoint,rdf:type,fixm:'SignificantPoint',Graph) .

fixm_SupplementalData(Graph, SupplementalData, FuelEndurance, PersonsOnBoard, PilotInCommand) :-
  rdf(SupplementalData,rdf:type,fixm:'SupplementalData',Graph)
  ,((FuelEndurance='$null$',\+ rdf(SupplementalData,fixm:'fuelEndurance',_FuelEndurance,Graph))
  ,(rdf(FuelEndurance,fixm:'fuelEndurance',FuelEnduranceNode,Graph))
  ,rdf(FuelEnduranceNode,rdf:value,FuelEnduranceValue,Graph)
  ,FuelEndurance=val(FuelEnduranceValue))
  ,((PersonsOnBoard='$null$',\+ rdf(SupplementalData,fixm:'personsOnBoard',_PersonsOnBoard,Graph))
  ,(rdf(PersonsOnBoard,fixm:'personsOnBoard',PersonsOnBoardNode,Graph))
  ,rdf(PersonsOnBoardNode,rdf:value,PersonsOnBoardValue,Graph)
  ,PersonsOnBoard=val(PersonsOnBoardValue))
  ,((PilotInCommand='$null$',\+ rdf(SupplementalData,fixm:'pilotInCommand',_PilotInCommand,Graph))
  ,(rdf(PilotInCommand,fixm:'pilotInCommand',PilotInCommandNode,Graph))
  ,rdf(PilotInCommandNode,rdf:value,PilotInCommandValue,Graph)
  ,PilotInCommand=val(PilotInCommandValue)) .

fixm_DangerousGoods(Graph, DangerousGoods, GuidebookNumber, OnboardLocation, HandlingInformation, AircraftLimitation, AirWayBill, Shipment, PackageGroup, ShippingInformation) :-
  rdf(DangerousGoods,rdf:type,fixm:'DangerousGoods',Graph)
  ,((GuidebookNumber='$null$',\+ rdf(DangerousGoods,fixm:'guidebookNumber',_GuidebookNumber,Graph))
  ,(rdf(GuidebookNumber,fixm:'guidebookNumber',GuidebookNumberNode,Graph))
  ,rdf(GuidebookNumberNode,rdf:value,GuidebookNumberValue,Graph)
  ,GuidebookNumber=val(GuidebookNumberValue))
  ,((OnboardLocation='$null$',\+ rdf(DangerousGoods,fixm:'onboardLocation',_OnboardLocation,Graph))
  ,(rdf(OnboardLocation,fixm:'onboardLocation',OnboardLocationNode,Graph))
  ,rdf(OnboardLocationNode,rdf:value,OnboardLocationValue,Graph)
  ,OnboardLocation=val(OnboardLocationValue))
  ,((HandlingInformation='$null$',\+ rdf(DangerousGoods,fixm:'handlingInformation',_HandlingInformation,Graph))
  ,(rdf(HandlingInformation,fixm:'handlingInformation',HandlingInformationNode,Graph))
  ,rdf(HandlingInformationNode,rdf:value,HandlingInformationValue,Graph)
  ,HandlingInformation=val(HandlingInformationValue))
  ,((AircraftLimitation='$null$',\+ rdf(DangerousGoods,fixm:'aircraftLimitation',_AircraftLimitation,Graph))
  ,(rdf(AircraftLimitation,fixm:'aircraftLimitation',AircraftLimitationNode,Graph))
  ,rdf(AircraftLimitationNode,rdf:value,AircraftLimitationValue,Graph)
  ,AircraftLimitation=val(AircraftLimitationValue))
  ,((AirWayBill='$null$',\+ rdf(DangerousGoods,fixm:'airWayBill',_AirWayBill,Graph))
  ,(rdf(AirWayBill,fixm:'airWayBill',AirWayBillNode,Graph))
  ,rdf(AirWayBillNode,rdf:value,AirWayBillValue,Graph)
  ,AirWayBill=val(AirWayBillValue))
  ,((Shipment='$null$',\+ rdf(DangerousGoods,fixm:'shipment',_Shipment,Graph))
  ,(rdf(Shipment,fixm:'shipment',ShipmentNode,Graph))
  ,rdf(ShipmentNode,rdf:value,ShipmentValue,Graph)
  ,Shipment=val(ShipmentValue))

  ,((ShippingInformation='$null$',\+ rdf(DangerousGoods,fixm:'shippingInformation',_ShippingInformation,Graph))
  ,(rdf(ShippingInformation,fixm:'shippingInformation',ShippingInformationNode,Graph))
  ,rdf(ShippingInformationNode,rdf:value,ShippingInformationValue,Graph)
  ,ShippingInformation=val(ShippingInformationValue)) .

fixm_DangerousGoodsPackageGroup(Graph, DangerousGoodsPackageGroup, ShipmentDimensions, DangerousGoodsPackage, ShipmentUseIndicator) :-
  rdf(DangerousGoodsPackageGroup,rdf:type,fixm:'DangerousGoodsPackageGroup',Graph)
  ,((ShipmentDimensions='$null$',\+ rdf(DangerousGoodsPackageGroup,fixm:'shipmentDimensions',_ShipmentDimensions,Graph))
  ,(rdf(ShipmentDimensions,fixm:'shipmentDimensions',ShipmentDimensionsNode,Graph))
  ,rdf(ShipmentDimensionsNode,rdf:value,ShipmentDimensionsValue,Graph)
  ,ShipmentDimensions=val(ShipmentDimensionsValue))

  ,((ShipmentUseIndicator='$null$',\+ rdf(DangerousGoodsPackageGroup,fixm:'shipmentUseIndicator',_ShipmentUseIndicator,Graph))
  ,(rdf(ShipmentUseIndicator,fixm:'shipmentUseIndicator',ShipmentUseIndicatorNode,Graph))
  ,rdf(ShipmentUseIndicatorNode,rdf:value,ShipmentUseIndicatorValue,Graph)
  ,ShipmentUseIndicator=val(ShipmentUseIndicatorValue)) .

fixm_OfftrackDistance(Graph, OfftrackDistance, Distance, Direction) :-
  rdf(OfftrackDistance,rdf:type,fixm:'OfftrackDistance',Graph)
  ,((Distance='$null$',\+ rdf(OfftrackDistance,fixm:'distance',_Distance,Graph))
  ,(rdf(Distance,fixm:'distance',DistanceNode,Graph))
  ,rdf(DistanceNode,rdf:value,DistanceValue,Graph)
  ,Distance=val(DistanceValue))
  ,((Direction='$null$',\+ rdf(OfftrackDistance,fixm:'direction',_Direction,Graph))
  ,(rdf(Direction,fixm:'direction',DirectionNode,Graph))
  ,rdf(DirectionNode,rdf:value,DirectionValue,Graph)
  ,Direction=val(DirectionValue)) .

fixm_Handoff(Graph, Handoff, ReceivingUnit, TransferringUnit, CoordinationStatus) :-
  rdf(Handoff,rdf:type,fixm:'Handoff',Graph)
  ,((ReceivingUnit='$null$',\+ rdf(Handoff,fixm:'receivingUnit',_ReceivingUnit,Graph))
  ,(rdf(ReceivingUnit,fixm:'receivingUnit',ReceivingUnitNode,Graph))
  ,rdf(ReceivingUnitNode,rdf:value,ReceivingUnitValue,Graph)
  ,ReceivingUnit=val(ReceivingUnitValue))
  ,((TransferringUnit='$null$',\+ rdf(Handoff,fixm:'transferringUnit',_TransferringUnit,Graph))
  ,(rdf(TransferringUnit,fixm:'transferringUnit',TransferringUnitNode,Graph))
  ,rdf(TransferringUnitNode,rdf:value,TransferringUnitValue,Graph)
  ,TransferringUnit=val(TransferringUnitValue))
  ,((CoordinationStatus='$null$',\+ rdf(Handoff,fixm:'coordinationStatus',_CoordinationStatus,Graph))
  ,(rdf(CoordinationStatus,fixm:'coordinationStatus',CoordinationStatusNode,Graph))
  ,rdf(CoordinationStatusNode,rdf:value,CoordinationStatusValue,Graph)
  ,CoordinationStatus=val(CoordinationStatusValue)) .

fixm_TrajectoryChange(Graph, TrajectoryChange, ConstrainedAirspace, SpecialActivityAirspace) :-
  rdf(TrajectoryChange,rdf:type,fixm:'TrajectoryChange',Graph)
  ,((ConstrainedAirspace='$null$',\+ rdf(TrajectoryChange,fixm:'constrainedAirspace',_ConstrainedAirspace,Graph))
  ,(rdf(ConstrainedAirspace,fixm:'constrainedAirspace',ConstrainedAirspaceNode,Graph))
  ,rdf(ConstrainedAirspaceNode,rdf:value,ConstrainedAirspaceValue,Graph)
  ,ConstrainedAirspace=val(ConstrainedAirspaceValue))
  ,((SpecialActivityAirspace='$null$',\+ rdf(TrajectoryChange,fixm:'specialActivityAirspace',_SpecialActivityAirspace,Graph))
  ,(rdf(SpecialActivityAirspace,fixm:'specialActivityAirspace',SpecialActivityAirspaceNode,Graph))
  ,rdf(SpecialActivityAirspaceNode,rdf:value,SpecialActivityAirspaceValue,Graph)
  ,SpecialActivityAirspace=val(SpecialActivityAirspaceValue)) .

fixm_ContactInformation(Graph, ContactInformation, Name, Title, OnlineContact, PhoneFax, Address) :-
  subClassOf(T,fixm:'ContactInformation')
  ,rdf(ContactInformation,rdf:type,T,Graph)
  ,((Name='$null$',\+ rdf(ContactInformation,fixm:'name',_Name,Graph))
  ,(rdf(Name,fixm:'name',NameNode,Graph))
  ,rdf(NameNode,rdf:value,NameValue,Graph)
  ,Name=val(NameValue))
  ,((Title='$null$',\+ rdf(ContactInformation,fixm:'title',_Title,Graph))
  ,(rdf(Title,fixm:'title',TitleNode,Graph))
  ,rdf(TitleNode,rdf:value,TitleValue,Graph)
  ,Title=val(TitleValue))
  ,((OnlineContact='$null$',\+ rdf(ContactInformation,fixm:'onlineContact',_OnlineContact,Graph))
  ,(rdf(OnlineContact,fixm:'onlineContact',OnlineContactNode,Graph))
  ,rdf(OnlineContactNode,rdf:value,OnlineContactValue,Graph)
  ,OnlineContact=val(OnlineContactValue))
  ,((PhoneFax='$null$',\+ rdf(ContactInformation,fixm:'phoneFax',_PhoneFax,Graph))
  ,(rdf(PhoneFax,fixm:'phoneFax',PhoneFaxNode,Graph))
  ,rdf(PhoneFaxNode,rdf:value,PhoneFaxValue,Graph)
  ,PhoneFax=val(PhoneFaxValue))
  ,((Address='$null$',\+ rdf(ContactInformation,fixm:'address',_Address,Graph))
  ,(rdf(Address,fixm:'address',AddressNode,Graph))
  ,rdf(AddressNode,rdf:value,AddressValue,Graph)
  ,Address=val(AddressValue)) .

aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability) :-
  rdf(AirportHeliportTimeSlice,rdf:type,aixm:'AirportHeliportTimeSlice',Graph)
  ,((Designator='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'designator',_Designator,Graph))
  ,(rdf(Designator,aixm:'designator',DesignatorNode,Graph))
  ,rdf(DesignatorNode,rdf:value,DesignatorValue,Graph)
  ,Designator=val(DesignatorValue))
  ,((Name='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'name',_Name,Graph))
  ,(rdf(Name,aixm:'name',NameNode,Graph))
  ,rdf(NameNode,rdf:value,NameValue,Graph)
  ,Name=val(NameValue))
  ,((LocationIndicatorICAO='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'locationIndicatorICAO',_LocationIndicatorICAO,Graph))
  ,(rdf(LocationIndicatorICAO,aixm:'locationIndicatorICAO',LocationIndicatorICAONode,Graph))
  ,rdf(LocationIndicatorICAONode,rdf:value,LocationIndicatorICAOValue,Graph)
  ,LocationIndicatorICAO=val(LocationIndicatorICAOValue))
  ,((DesignatorIATA='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'designatorIATA',_DesignatorIATA,Graph))
  ,(rdf(DesignatorIATA,aixm:'designatorIATA',DesignatorIATANode,Graph))
  ,rdf(DesignatorIATANode,rdf:value,DesignatorIATAValue,Graph)
  ,DesignatorIATA=val(DesignatorIATAValue))
  ,((Type='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'type',_Type,Graph))
  ,(rdf(Type,aixm:'type',TypeNode,Graph))
  ,rdf(TypeNode,rdf:value,TypeValue,Graph)
  ,Type=val(TypeValue))
  ,((CertifiedICAO='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'certifiedICAO',_CertifiedICAO,Graph))
  ,(rdf(CertifiedICAO,aixm:'certifiedICAO',CertifiedICAONode,Graph))
  ,rdf(CertifiedICAONode,rdf:value,CertifiedICAOValue,Graph)
  ,CertifiedICAO=val(CertifiedICAOValue))
  ,((PrivateUse='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'privateUse',_PrivateUse,Graph))
  ,(rdf(PrivateUse,aixm:'privateUse',PrivateUseNode,Graph))
  ,rdf(PrivateUseNode,rdf:value,PrivateUseValue,Graph)
  ,PrivateUse=val(PrivateUseValue))
  ,((ControlType='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'controlType',_ControlType,Graph))
  ,(rdf(ControlType,aixm:'controlType',ControlTypeNode,Graph))
  ,rdf(ControlTypeNode,rdf:value,ControlTypeValue,Graph)
  ,ControlType=val(ControlTypeValue))
  ,((FieldElevation='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'fieldElevation',_FieldElevation,Graph))
  ,(rdf(FieldElevation,aixm:'fieldElevation',FieldElevationNode,Graph))
  ,rdf(FieldElevationNode,rdf:value,FieldElevationValue,Graph)
  ,FieldElevation=val(FieldElevationValue))
  ,((FieldElevationAccuracy='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'fieldElevationAccuracy',_FieldElevationAccuracy,Graph))
  ,(rdf(FieldElevationAccuracy,aixm:'fieldElevationAccuracy',FieldElevationAccuracyNode,Graph))
  ,rdf(FieldElevationAccuracyNode,rdf:value,FieldElevationAccuracyValue,Graph)
  ,FieldElevationAccuracy=val(FieldElevationAccuracyValue))
  ,((VerticalDatum='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'verticalDatum',_VerticalDatum,Graph))
  ,(rdf(VerticalDatum,aixm:'verticalDatum',VerticalDatumNode,Graph))
  ,rdf(VerticalDatumNode,rdf:value,VerticalDatumValue,Graph)
  ,VerticalDatum=val(VerticalDatumValue))
  ,((MagneticVariation='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'magneticVariation',_MagneticVariation,Graph))
  ,(rdf(MagneticVariation,aixm:'magneticVariation',MagneticVariationNode,Graph))
  ,rdf(MagneticVariationNode,rdf:value,MagneticVariationValue,Graph)
  ,MagneticVariation=val(MagneticVariationValue))
  ,((MagneticVariationAccuracy='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'magneticVariationAccuracy',_MagneticVariationAccuracy,Graph))
  ,(rdf(MagneticVariationAccuracy,aixm:'magneticVariationAccuracy',MagneticVariationAccuracyNode,Graph))
  ,rdf(MagneticVariationAccuracyNode,rdf:value,MagneticVariationAccuracyValue,Graph)
  ,MagneticVariationAccuracy=val(MagneticVariationAccuracyValue))
  ,((DateMagneticVariation='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'dateMagneticVariation',_DateMagneticVariation,Graph))
  ,(rdf(DateMagneticVariation,aixm:'dateMagneticVariation',DateMagneticVariationNode,Graph))
  ,rdf(DateMagneticVariationNode,rdf:value,DateMagneticVariationValue,Graph)
  ,DateMagneticVariation=val(DateMagneticVariationValue))
  ,((MagneticVariationChange='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'magneticVariationChange',_MagneticVariationChange,Graph))
  ,(rdf(MagneticVariationChange,aixm:'magneticVariationChange',MagneticVariationChangeNode,Graph))
  ,rdf(MagneticVariationChangeNode,rdf:value,MagneticVariationChangeValue,Graph)
  ,MagneticVariationChange=val(MagneticVariationChangeValue))
  ,((ReferenceTemperature='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'referenceTemperature',_ReferenceTemperature,Graph))
  ,(rdf(ReferenceTemperature,aixm:'referenceTemperature',ReferenceTemperatureNode,Graph))
  ,rdf(ReferenceTemperatureNode,rdf:value,ReferenceTemperatureValue,Graph)
  ,ReferenceTemperature=val(ReferenceTemperatureValue))
  ,((AltimeterCheckLocation='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'altimeterCheckLocation',_AltimeterCheckLocation,Graph))
  ,(rdf(AltimeterCheckLocation,aixm:'altimeterCheckLocation',AltimeterCheckLocationNode,Graph))
  ,rdf(AltimeterCheckLocationNode,rdf:value,AltimeterCheckLocationValue,Graph)
  ,AltimeterCheckLocation=val(AltimeterCheckLocationValue))
  ,((SecondaryPowerSupply='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'secondaryPowerSupply',_SecondaryPowerSupply,Graph))
  ,(rdf(SecondaryPowerSupply,aixm:'secondaryPowerSupply',SecondaryPowerSupplyNode,Graph))
  ,rdf(SecondaryPowerSupplyNode,rdf:value,SecondaryPowerSupplyValue,Graph)
  ,SecondaryPowerSupply=val(SecondaryPowerSupplyValue))
  ,((WindDirectionIndicator='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'windDirectionIndicator',_WindDirectionIndicator,Graph))
  ,(rdf(WindDirectionIndicator,aixm:'windDirectionIndicator',WindDirectionIndicatorNode,Graph))
  ,rdf(WindDirectionIndicatorNode,rdf:value,WindDirectionIndicatorValue,Graph)
  ,WindDirectionIndicator=val(WindDirectionIndicatorValue))
  ,((LandingDirectionIndicator='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'landingDirectionIndicator',_LandingDirectionIndicator,Graph))
  ,(rdf(LandingDirectionIndicator,aixm:'landingDirectionIndicator',LandingDirectionIndicatorNode,Graph))
  ,rdf(LandingDirectionIndicatorNode,rdf:value,LandingDirectionIndicatorValue,Graph)
  ,LandingDirectionIndicator=val(LandingDirectionIndicatorValue))
  ,((TransitionAltitude='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'transitionAltitude',_TransitionAltitude,Graph))
  ,(rdf(TransitionAltitude,aixm:'transitionAltitude',TransitionAltitudeNode,Graph))
  ,rdf(TransitionAltitudeNode,rdf:value,TransitionAltitudeValue,Graph)
  ,TransitionAltitude=val(TransitionAltitudeValue))
  ,((TransitionLevel='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'transitionLevel',_TransitionLevel,Graph))
  ,(rdf(TransitionLevel,aixm:'transitionLevel',TransitionLevelNode,Graph))
  ,rdf(TransitionLevelNode,rdf:value,TransitionLevelValue,Graph)
  ,TransitionLevel=val(TransitionLevelValue))
  ,((LowestTemperature='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'lowestTemperature',_LowestTemperature,Graph))
  ,(rdf(LowestTemperature,aixm:'lowestTemperature',LowestTemperatureNode,Graph))
  ,rdf(LowestTemperatureNode,rdf:value,LowestTemperatureValue,Graph)
  ,LowestTemperature=val(LowestTemperatureValue))
  ,((Abandoned='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'abandoned',_Abandoned,Graph))
  ,(rdf(Abandoned,aixm:'abandoned',AbandonedNode,Graph))
  ,rdf(AbandonedNode,rdf:value,AbandonedValue,Graph)
  ,Abandoned=val(AbandonedValue))
  ,((CertificationDate='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'certificationDate',_CertificationDate,Graph))
  ,(rdf(CertificationDate,aixm:'certificationDate',CertificationDateNode,Graph))
  ,rdf(CertificationDateNode,rdf:value,CertificationDateValue,Graph)
  ,CertificationDate=val(CertificationDateValue))
  ,((CertificationExpirationDate='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'certificationExpirationDate',_CertificationExpirationDate,Graph))
  ,(rdf(CertificationExpirationDate,aixm:'certificationExpirationDate',CertificationExpirationDateNode,Graph))
  ,rdf(CertificationExpirationDateNode,rdf:value,CertificationExpirationDateValue,Graph)
  ,CertificationExpirationDate=val(CertificationExpirationDateValue))


  ,((ARP='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'ARP',_ARP,Graph))
  ,(rdf(ARP,aixm:'ARP',ARPNode,Graph))
  ,rdf(ARPNode,rdf:value,ARPValue,Graph)
  ,ARP=val(ARPValue))



  ,((ResponsibleOrganisation='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'responsibleOrganisation',_ResponsibleOrganisation,Graph))
  ,(rdf(ResponsibleOrganisation,aixm:'responsibleOrganisation',ResponsibleOrganisationNode,Graph))
  ,rdf(ResponsibleOrganisationNode,rdf:value,ResponsibleOrganisationValue,Graph)
  ,ResponsibleOrganisation=val(ResponsibleOrganisationValue))
  ,((AviationBoundary='$null$',\+ rdf(AirportHeliportTimeSlice,aixm:'aviationBoundary',_AviationBoundary,Graph))
  ,(rdf(AviationBoundary,aixm:'aviationBoundary',AviationBoundaryNode,Graph))
  ,rdf(AviationBoundaryNode,rdf:value,AviationBoundaryValue,Graph)
  ,AviationBoundary=val(AviationBoundaryValue))
 .

fixm_Point4D(Graph, Point4D, Altitude, Time, PointRange) :-
  subClassOf(T,fixm:'Point4D')
  ,rdf(Point4D,rdf:type,T,Graph)
  ,((Altitude='$null$',\+ rdf(Point4D,fixm:'altitude',_Altitude,Graph))
  ,(rdf(Altitude,fixm:'altitude',AltitudeNode,Graph))
  ,rdf(AltitudeNode,rdf:value,AltitudeValue,Graph)
  ,Altitude=val(AltitudeValue))
  ,((Time='$null$',\+ rdf(Point4D,fixm:'time',_Time,Graph))
  ,(rdf(Time,fixm:'time',TimeNode,Graph))
  ,rdf(TimeNode,rdf:value,TimeValue,Graph)
  ,Time=val(TimeValue))
  ,((PointRange='$null$',\+ rdf(Point4D,fixm:'pointRange',_PointRange,Graph))
  ,(rdf(PointRange,fixm:'pointRange',PointRangeNode,Graph))
  ,rdf(PointRangeNode,rdf:value,PointRangeValue,Graph)
  ,PointRange=val(PointRangeValue)) .

fixm_AbstractRoutePoint(Graph, AbstractRoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit) :-
  subClassOf(T,fixm:'AbstractRoutePoint')
  ,rdf(AbstractRoutePoint,rdf:type,T,Graph)
  ,((AirTrafficType='$null$',\+ rdf(AbstractRoutePoint,fixm:'airTrafficType',_AirTrafficType,Graph))
  ,(rdf(AirTrafficType,fixm:'airTrafficType',AirTrafficTypeNode,Graph))
  ,rdf(AirTrafficTypeNode,rdf:value,AirTrafficTypeValue,Graph)
  ,AirTrafficType=val(AirTrafficTypeValue))
  ,((DelayAtPoint='$null$',\+ rdf(AbstractRoutePoint,fixm:'delayAtPoint',_DelayAtPoint,Graph))
  ,(rdf(DelayAtPoint,fixm:'delayAtPoint',DelayAtPointNode,Graph))
  ,rdf(DelayAtPointNode,rdf:value,DelayAtPointValue,Graph)
  ,DelayAtPoint=val(DelayAtPointValue))
  ,((FlightRules='$null$',\+ rdf(AbstractRoutePoint,fixm:'flightRules',_FlightRules,Graph))
  ,(rdf(FlightRules,fixm:'flightRules',FlightRulesNode,Graph))
  ,rdf(FlightRulesNode,rdf:value,FlightRulesValue,Graph)
  ,FlightRules=val(FlightRulesValue))
  ,((Point='$null$',\+ rdf(AbstractRoutePoint,fixm:'point',_Point,Graph))
  ,(rdf(Point,fixm:'point',PointNode,Graph))
  ,rdf(PointNode,rdf:value,PointValue,Graph)
  ,Point=val(PointValue))
  ,((ClearanceLimit='$null$',\+ rdf(AbstractRoutePoint,fixm:'clearanceLimit',_ClearanceLimit,Graph))
  ,(rdf(ClearanceLimit,fixm:'clearanceLimit',ClearanceLimitNode,Graph))
  ,rdf(ClearanceLimitNode,rdf:value,ClearanceLimitValue,Graph)
  ,ClearanceLimit=val(ClearanceLimitValue)) .

aixm_Ridge(Graph, Ridge, Side, Distance, Depth, Annotation) :-
  rdf(Ridge,rdf:type,aixm:'Ridge',Graph)
  ,((Side='$null$',\+ rdf(Ridge,aixm:'side',_Side,Graph))
  ,(rdf(Side,aixm:'side',SideNode,Graph))
  ,rdf(SideNode,rdf:value,SideValue,Graph)
  ,Side=val(SideValue))
  ,((Distance='$null$',\+ rdf(Ridge,aixm:'distance',_Distance,Graph))
  ,(rdf(Distance,aixm:'distance',DistanceNode,Graph))
  ,rdf(DistanceNode,rdf:value,DistanceValue,Graph)
  ,Distance=val(DistanceValue))
  ,((Depth='$null$',\+ rdf(Ridge,aixm:'depth',_Depth,Graph))
  ,(rdf(Depth,aixm:'depth',DepthNode,Graph))
  ,rdf(DepthNode,rdf:value,DepthValue,Graph)
  ,Depth=val(DepthValue))
 .

fixm_DepartureActivityTimes(Graph, DepartureActivityTimes, BoardingTime, DeIcingTime, GroundHandlingTime, StartupTime) :-
  rdf(DepartureActivityTimes,rdf:type,fixm:'DepartureActivityTimes',Graph)
  ,((BoardingTime='$null$',\+ rdf(DepartureActivityTimes,fixm:'boardingTime',_BoardingTime,Graph))
  ,(rdf(BoardingTime,fixm:'boardingTime',BoardingTimeNode,Graph))
  ,rdf(BoardingTimeNode,rdf:value,BoardingTimeValue,Graph)
  ,BoardingTime=val(BoardingTimeValue))
  ,((DeIcingTime='$null$',\+ rdf(DepartureActivityTimes,fixm:'deIcingTime',_DeIcingTime,Graph))
  ,(rdf(DeIcingTime,fixm:'deIcingTime',DeIcingTimeNode,Graph))
  ,rdf(DeIcingTimeNode,rdf:value,DeIcingTimeValue,Graph)
  ,DeIcingTime=val(DeIcingTimeValue))
  ,((GroundHandlingTime='$null$',\+ rdf(DepartureActivityTimes,fixm:'groundHandlingTime',_GroundHandlingTime,Graph))
  ,(rdf(GroundHandlingTime,fixm:'groundHandlingTime',GroundHandlingTimeNode,Graph))
  ,rdf(GroundHandlingTimeNode,rdf:value,GroundHandlingTimeValue,Graph)
  ,GroundHandlingTime=val(GroundHandlingTimeValue))
  ,((StartupTime='$null$',\+ rdf(DepartureActivityTimes,fixm:'startupTime',_StartupTime,Graph))
  ,(rdf(StartupTime,fixm:'startupTime',StartupTimeNode,Graph))
  ,rdf(StartupTimeNode,rdf:value,StartupTimeValue,Graph)
  ,StartupTime=val(StartupTimeValue)) .

fixm_EnRouteDiversion(Graph, EnRouteDiversion, DiversionRecoveryInformation) :-
  rdf(EnRouteDiversion,rdf:type,fixm:'EnRouteDiversion',Graph)
  ,((DiversionRecoveryInformation='$null$',\+ rdf(EnRouteDiversion,fixm:'diversionRecoveryInformation',_DiversionRecoveryInformation,Graph))
  ,(rdf(DiversionRecoveryInformation,fixm:'diversionRecoveryInformation',DiversionRecoveryInformationNode,Graph))
  ,rdf(DiversionRecoveryInformationNode,rdf:value,DiversionRecoveryInformationValue,Graph)
  ,DiversionRecoveryInformation=val(DiversionRecoveryInformationValue)) .

fixm_ActualSpeed(Graph, ActualSpeed, Calculated, PilotReported, Surveillance) :-
  rdf(ActualSpeed,rdf:type,fixm:'ActualSpeed',Graph)
  ,((Calculated='$null$',\+ rdf(ActualSpeed,fixm:'calculated',_Calculated,Graph))
  ,(rdf(Calculated,fixm:'calculated',CalculatedNode,Graph))
  ,rdf(CalculatedNode,rdf:value,CalculatedValue,Graph)
  ,Calculated=val(CalculatedValue))
  ,((PilotReported='$null$',\+ rdf(ActualSpeed,fixm:'pilotReported',_PilotReported,Graph))
  ,(rdf(PilotReported,fixm:'pilotReported',PilotReportedNode,Graph))
  ,rdf(PilotReportedNode,rdf:value,PilotReportedValue,Graph)
  ,PilotReported=val(PilotReportedValue))
  ,((Surveillance='$null$',\+ rdf(ActualSpeed,fixm:'surveillance',_Surveillance,Graph))
  ,(rdf(Surveillance,fixm:'surveillance',SurveillanceNode,Graph))
  ,rdf(SurveillanceNode,rdf:value,SurveillanceValue,Graph)
  ,Surveillance=val(SurveillanceValue)) .

fixm_FlightEmergency(Graph, FlightEmergency, ActionTaken, EmergencyDescription, Originator, OtherInformation, Phase, Contact) :-
  rdf(FlightEmergency,rdf:type,fixm:'FlightEmergency',Graph)
  ,((ActionTaken='$null$',\+ rdf(FlightEmergency,fixm:'actionTaken',_ActionTaken,Graph))
  ,(rdf(ActionTaken,fixm:'actionTaken',ActionTakenNode,Graph))
  ,rdf(ActionTakenNode,rdf:value,ActionTakenValue,Graph)
  ,ActionTaken=val(ActionTakenValue))
  ,((EmergencyDescription='$null$',\+ rdf(FlightEmergency,fixm:'emergencyDescription',_EmergencyDescription,Graph))
  ,(rdf(EmergencyDescription,fixm:'emergencyDescription',EmergencyDescriptionNode,Graph))
  ,rdf(EmergencyDescriptionNode,rdf:value,EmergencyDescriptionValue,Graph)
  ,EmergencyDescription=val(EmergencyDescriptionValue))
  ,((Originator='$null$',\+ rdf(FlightEmergency,fixm:'originator',_Originator,Graph))
  ,(rdf(Originator,fixm:'originator',OriginatorNode,Graph))
  ,rdf(OriginatorNode,rdf:value,OriginatorValue,Graph)
  ,Originator=val(OriginatorValue))
  ,((OtherInformation='$null$',\+ rdf(FlightEmergency,fixm:'otherInformation',_OtherInformation,Graph))
  ,(rdf(OtherInformation,fixm:'otherInformation',OtherInformationNode,Graph))
  ,rdf(OtherInformationNode,rdf:value,OtherInformationValue,Graph)
  ,OtherInformation=val(OtherInformationValue))
  ,((Phase='$null$',\+ rdf(FlightEmergency,fixm:'phase',_Phase,Graph))
  ,(rdf(Phase,fixm:'phase',PhaseNode,Graph))
  ,rdf(PhaseNode,rdf:value,PhaseValue,Graph)
  ,Phase=val(PhaseValue))
  ,((Contact='$null$',\+ rdf(FlightEmergency,fixm:'contact',_Contact,Graph))
  ,(rdf(Contact,fixm:'contact',ContactNode,Graph))
  ,rdf(ContactNode,rdf:value,ContactValue,Graph)
  ,Contact=val(ContactValue)) .

fixm_Flight(Graph, Flight, ControllingUnit, Extensions, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoods, RankedTrajectories, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandling) :-
  subClassOf(T,fixm:'Flight')
  ,rdf(Flight,rdf:type,T,Graph)
  ,((ControllingUnit='$null$',\+ rdf(Flight,fixm:'controllingUnit',_ControllingUnit,Graph))
  ,(rdf(ControllingUnit,fixm:'controllingUnit',ControllingUnitNode,Graph))
  ,rdf(ControllingUnitNode,rdf:value,ControllingUnitValue,Graph)
  ,ControllingUnit=val(ControllingUnitValue))

  ,((FlightFiler='$null$',\+ rdf(Flight,fixm:'flightFiler',_FlightFiler,Graph))
  ,(rdf(FlightFiler,fixm:'flightFiler',FlightFilerNode,Graph))
  ,rdf(FlightFilerNode,rdf:value,FlightFilerValue,Graph)
  ,FlightFiler=val(FlightFilerValue))
  ,((Gufi='$null$',\+ rdf(Flight,fixm:'gufi',_Gufi,Graph))
  ,(rdf(Gufi,fixm:'gufi',GufiNode,Graph))
  ,rdf(GufiNode,rdf:value,GufiValue,Graph)
  ,Gufi=val(GufiValue))
  ,((Remarks='$null$',\+ rdf(Flight,fixm:'remarks',_Remarks,Graph))
  ,(rdf(Remarks,fixm:'remarks',RemarksNode,Graph))
  ,rdf(RemarksNode,rdf:value,RemarksValue,Graph)
  ,Remarks=val(RemarksValue))
  ,((AircraftDescription='$null$',\+ rdf(Flight,fixm:'aircraftDescription',_AircraftDescription,Graph))
  ,(rdf(AircraftDescription,fixm:'aircraftDescription',AircraftDescriptionNode,Graph))
  ,rdf(AircraftDescriptionNode,rdf:value,AircraftDescriptionValue,Graph)
  ,AircraftDescription=val(AircraftDescriptionValue))


  ,((RouteToRevisedDestination='$null$',\+ rdf(Flight,fixm:'routeToRevisedDestination',_RouteToRevisedDestination,Graph))
  ,(rdf(RouteToRevisedDestination,fixm:'routeToRevisedDestination',RouteToRevisedDestinationNode,Graph))
  ,rdf(RouteToRevisedDestinationNode,rdf:value,RouteToRevisedDestinationValue,Graph)
  ,RouteToRevisedDestination=val(RouteToRevisedDestinationValue))
  ,((Negotiating='$null$',\+ rdf(Flight,fixm:'negotiating',_Negotiating,Graph))
  ,(rdf(Negotiating,fixm:'negotiating',NegotiatingNode,Graph))
  ,rdf(NegotiatingNode,rdf:value,NegotiatingValue,Graph)
  ,Negotiating=val(NegotiatingValue))
  ,((Agreed='$null$',\+ rdf(Flight,fixm:'agreed',_Agreed,Graph))
  ,(rdf(Agreed,fixm:'agreed',AgreedNode,Graph))
  ,rdf(AgreedNode,rdf:value,AgreedValue,Graph)
  ,Agreed=val(AgreedValue))
  ,((Arrival='$null$',\+ rdf(Flight,fixm:'arrival',_Arrival,Graph))
  ,(rdf(Arrival,fixm:'arrival',ArrivalNode,Graph))
  ,rdf(ArrivalNode,rdf:value,ArrivalValue,Graph)
  ,Arrival=val(ArrivalValue))
  ,((Departure='$null$',\+ rdf(Flight,fixm:'departure',_Departure,Graph))
  ,(rdf(Departure,fixm:'departure',DepartureNode,Graph))
  ,rdf(DepartureNode,rdf:value,DepartureValue,Graph)
  ,Departure=val(DepartureValue))
  ,((Emergency='$null$',\+ rdf(Flight,fixm:'emergency',_Emergency,Graph))
  ,(rdf(Emergency,fixm:'emergency',EmergencyNode,Graph))
  ,rdf(EmergencyNode,rdf:value,EmergencyValue,Graph)
  ,Emergency=val(EmergencyValue))
  ,((RadioCommunicationFailure='$null$',\+ rdf(Flight,fixm:'radioCommunicationFailure',_RadioCommunicationFailure,Graph))
  ,(rdf(RadioCommunicationFailure,fixm:'radioCommunicationFailure',RadioCommunicationFailureNode,Graph))
  ,rdf(RadioCommunicationFailureNode,rdf:value,RadioCommunicationFailureValue,Graph)
  ,RadioCommunicationFailure=val(RadioCommunicationFailureValue))
  ,((EnRoute='$null$',\+ rdf(Flight,fixm:'enRoute',_EnRoute,Graph))
  ,(rdf(EnRoute,fixm:'enRoute',EnRouteNode,Graph))
  ,rdf(EnRouteNode,rdf:value,EnRouteValue,Graph)
  ,EnRoute=val(EnRouteValue))
  ,((Operator='$null$',\+ rdf(Flight,fixm:'operator',_Operator,Graph))
  ,(rdf(Operator,fixm:'operator',OperatorNode,Graph))
  ,rdf(OperatorNode,rdf:value,OperatorValue,Graph)
  ,Operator=val(OperatorValue))
  ,((EnRouteDiversion='$null$',\+ rdf(Flight,fixm:'enRouteDiversion',_EnRouteDiversion,Graph))
  ,(rdf(EnRouteDiversion,fixm:'enRouteDiversion',EnRouteDiversionNode,Graph))
  ,rdf(EnRouteDiversionNode,rdf:value,EnRouteDiversionValue,Graph)
  ,EnRouteDiversion=val(EnRouteDiversionValue))
  ,((FlightType='$null$',\+ rdf(Flight,fixm:'flightType',_FlightType,Graph))
  ,(rdf(FlightType,fixm:'flightType',FlightTypeNode,Graph))
  ,rdf(FlightTypeNode,rdf:value,FlightTypeValue,Graph)
  ,FlightType=val(FlightTypeValue))
  ,((FlightStatus='$null$',\+ rdf(Flight,fixm:'flightStatus',_FlightStatus,Graph))
  ,(rdf(FlightStatus,fixm:'flightStatus',FlightStatusNode,Graph))
  ,rdf(FlightStatusNode,rdf:value,FlightStatusValue,Graph)
  ,FlightStatus=val(FlightStatusValue))
  ,((Originator='$null$',\+ rdf(Flight,fixm:'originator',_Originator,Graph))
  ,(rdf(Originator,fixm:'originator',OriginatorNode,Graph))
  ,rdf(OriginatorNode,rdf:value,OriginatorValue,Graph)
  ,Originator=val(OriginatorValue))
  ,((SupplementalData='$null$',\+ rdf(Flight,fixm:'supplementalData',_SupplementalData,Graph))
  ,(rdf(SupplementalData,fixm:'supplementalData',SupplementalDataNode,Graph))
  ,rdf(SupplementalDataNode,rdf:value,SupplementalDataValue,Graph)
  ,SupplementalData=val(SupplementalDataValue))
  ,((FlightIdentification='$null$',\+ rdf(Flight,fixm:'flightIdentification',_FlightIdentification,Graph))
  ,(rdf(FlightIdentification,fixm:'flightIdentification',FlightIdentificationNode,Graph))
  ,rdf(FlightIdentificationNode,rdf:value,FlightIdentificationValue,Graph)
  ,FlightIdentification=val(FlightIdentificationValue))
 .

aixm_PropertiesWithSchedule(Graph, PropertiesWithSchedule, Annotation, SpecialDateAuthority, TimeInterval) :-
  subClassOf(T,aixm:'PropertiesWithSchedule')
  ,rdf(PropertiesWithSchedule,rdf:type,T,Graph)


 .

gml_Surface(Graph, Surface, Patch) :-
  subClassOf(T,gml:'Surface')
  ,rdf(Surface,rdf:type,T,Graph)
  ,findall(A, rdf(Surface,aixm:'patch',A,Graph), Patch) .

fixm_ClearedFlightInformation(Graph, ClearedFlightInformation, ClearedFlightLevel, ClearedSpeed, Heading, OfftrackClearance, RateOfClimbDescend, DirectRouting) :-
  rdf(ClearedFlightInformation,rdf:type,fixm:'ClearedFlightInformation',Graph)
  ,((ClearedFlightLevel='$null$',\+ rdf(ClearedFlightInformation,fixm:'clearedFlightLevel',_ClearedFlightLevel,Graph))
  ,(rdf(ClearedFlightLevel,fixm:'clearedFlightLevel',ClearedFlightLevelNode,Graph))
  ,rdf(ClearedFlightLevelNode,rdf:value,ClearedFlightLevelValue,Graph)
  ,ClearedFlightLevel=val(ClearedFlightLevelValue))
  ,((ClearedSpeed='$null$',\+ rdf(ClearedFlightInformation,fixm:'clearedSpeed',_ClearedSpeed,Graph))
  ,(rdf(ClearedSpeed,fixm:'clearedSpeed',ClearedSpeedNode,Graph))
  ,rdf(ClearedSpeedNode,rdf:value,ClearedSpeedValue,Graph)
  ,ClearedSpeed=val(ClearedSpeedValue))
  ,((Heading='$null$',\+ rdf(ClearedFlightInformation,fixm:'heading',_Heading,Graph))
  ,(rdf(Heading,fixm:'heading',HeadingNode,Graph))
  ,rdf(HeadingNode,rdf:value,HeadingValue,Graph)
  ,Heading=val(HeadingValue))
  ,((OfftrackClearance='$null$',\+ rdf(ClearedFlightInformation,fixm:'offtrackClearance',_OfftrackClearance,Graph))
  ,(rdf(OfftrackClearance,fixm:'offtrackClearance',OfftrackClearanceNode,Graph))
  ,rdf(OfftrackClearanceNode,rdf:value,OfftrackClearanceValue,Graph)
  ,OfftrackClearance=val(OfftrackClearanceValue))
  ,((RateOfClimbDescend='$null$',\+ rdf(ClearedFlightInformation,fixm:'rateOfClimbDescend',_RateOfClimbDescend,Graph))
  ,(rdf(RateOfClimbDescend,fixm:'rateOfClimbDescend',RateOfClimbDescendNode,Graph))
  ,rdf(RateOfClimbDescendNode,rdf:value,RateOfClimbDescendValue,Graph)
  ,RateOfClimbDescend=val(RateOfClimbDescendValue))
  ,((DirectRouting='$null$',\+ rdf(ClearedFlightInformation,fixm:'directRouting',_DirectRouting,Graph))
  ,(rdf(DirectRouting,fixm:'directRouting',DirectRoutingNode,Graph))
  ,rdf(DirectRoutingNode,rdf:value,DirectRoutingValue,Graph)
  ,DirectRouting=val(DirectRoutingValue)) .

fixm_TrajectoryRoutePair(Graph, TrajectoryRoutePair, Trajectory, Route) :-
  subClassOf(T,fixm:'TrajectoryRoutePair')
  ,rdf(TrajectoryRoutePair,rdf:type,T,Graph)
  ,((Trajectory='$null$',\+ rdf(TrajectoryRoutePair,fixm:'trajectory',_Trajectory,Graph))
  ,(rdf(Trajectory,fixm:'trajectory',TrajectoryNode,Graph))
  ,rdf(TrajectoryNode,rdf:value,TrajectoryValue,Graph)
  ,Trajectory=val(TrajectoryValue))
  ,((Route='$null$',\+ rdf(TrajectoryRoutePair,fixm:'route',_Route,Graph))
  ,(rdf(Route,fixm:'route',RouteNode,Graph))
  ,rdf(RouteNode,rdf:value,RouteValue,Graph)
  ,Route=val(RouteValue)) .

fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit, UpstreamUnit, BoundaryCrossingProposed, BoundaryCrossingCoordinated, Handoff, UnitBoundaryIndicator) :-
  rdf(UnitBoundary,rdf:type,fixm:'UnitBoundary',Graph)
  ,((DownstreamUnit='$null$',\+ rdf(UnitBoundary,fixm:'downstreamUnit',_DownstreamUnit,Graph))
  ,(rdf(DownstreamUnit,fixm:'downstreamUnit',DownstreamUnitNode,Graph))
  ,rdf(DownstreamUnitNode,rdf:value,DownstreamUnitValue,Graph)
  ,DownstreamUnit=val(DownstreamUnitValue))
  ,((UpstreamUnit='$null$',\+ rdf(UnitBoundary,fixm:'upstreamUnit',_UpstreamUnit,Graph))
  ,(rdf(UpstreamUnit,fixm:'upstreamUnit',UpstreamUnitNode,Graph))
  ,rdf(UpstreamUnitNode,rdf:value,UpstreamUnitValue,Graph)
  ,UpstreamUnit=val(UpstreamUnitValue))
  ,((BoundaryCrossingProposed='$null$',\+ rdf(UnitBoundary,fixm:'boundaryCrossingProposed',_BoundaryCrossingProposed,Graph))
  ,(rdf(BoundaryCrossingProposed,fixm:'boundaryCrossingProposed',BoundaryCrossingProposedNode,Graph))
  ,rdf(BoundaryCrossingProposedNode,rdf:value,BoundaryCrossingProposedValue,Graph)
  ,BoundaryCrossingProposed=val(BoundaryCrossingProposedValue))
  ,((BoundaryCrossingCoordinated='$null$',\+ rdf(UnitBoundary,fixm:'boundaryCrossingCoordinated',_BoundaryCrossingCoordinated,Graph))
  ,(rdf(BoundaryCrossingCoordinated,fixm:'boundaryCrossingCoordinated',BoundaryCrossingCoordinatedNode,Graph))
  ,rdf(BoundaryCrossingCoordinatedNode,rdf:value,BoundaryCrossingCoordinatedValue,Graph)
  ,BoundaryCrossingCoordinated=val(BoundaryCrossingCoordinatedValue))
  ,((Handoff='$null$',\+ rdf(UnitBoundary,fixm:'handoff',_Handoff,Graph))
  ,(rdf(Handoff,fixm:'handoff',HandoffNode,Graph))
  ,rdf(HandoffNode,rdf:value,HandoffValue,Graph)
  ,Handoff=val(HandoffValue))
  ,((UnitBoundaryIndicator='$null$',\+ rdf(UnitBoundary,fixm:'unitBoundaryIndicator',_UnitBoundaryIndicator,Graph))
  ,(rdf(UnitBoundaryIndicator,fixm:'unitBoundaryIndicator',UnitBoundaryIndicatorNode,Graph))
  ,rdf(UnitBoundaryIndicatorNode,rdf:value,UnitBoundaryIndicatorValue,Graph)
  ,UnitBoundaryIndicator=val(UnitBoundaryIndicatorValue)) .

aixm_SurfaceContamination(Graph, SurfaceContamination, ObservationTime, Depth, FrictionCoefficient, FrictionEstimation, FrictionDevice, ObscuredLights, FurtherClearanceTime, FurtherTotalClearance, NextObservationTime, Proportion, CriticalRidge, Annotation, Layer) :-
  subClassOf(T,aixm:'SurfaceContamination')
  ,rdf(SurfaceContamination,rdf:type,T,Graph)
  ,((ObservationTime='$null$',\+ rdf(SurfaceContamination,aixm:'observationTime',_ObservationTime,Graph))
  ,(rdf(ObservationTime,aixm:'observationTime',ObservationTimeNode,Graph))
  ,rdf(ObservationTimeNode,rdf:value,ObservationTimeValue,Graph)
  ,ObservationTime=val(ObservationTimeValue))
  ,((Depth='$null$',\+ rdf(SurfaceContamination,aixm:'depth',_Depth,Graph))
  ,(rdf(Depth,aixm:'depth',DepthNode,Graph))
  ,rdf(DepthNode,rdf:value,DepthValue,Graph)
  ,Depth=val(DepthValue))
  ,((FrictionCoefficient='$null$',\+ rdf(SurfaceContamination,aixm:'frictionCoefficient',_FrictionCoefficient,Graph))
  ,(rdf(FrictionCoefficient,aixm:'frictionCoefficient',FrictionCoefficientNode,Graph))
  ,rdf(FrictionCoefficientNode,rdf:value,FrictionCoefficientValue,Graph)
  ,FrictionCoefficient=val(FrictionCoefficientValue))
  ,((FrictionEstimation='$null$',\+ rdf(SurfaceContamination,aixm:'frictionEstimation',_FrictionEstimation,Graph))
  ,(rdf(FrictionEstimation,aixm:'frictionEstimation',FrictionEstimationNode,Graph))
  ,rdf(FrictionEstimationNode,rdf:value,FrictionEstimationValue,Graph)
  ,FrictionEstimation=val(FrictionEstimationValue))
  ,((FrictionDevice='$null$',\+ rdf(SurfaceContamination,aixm:'frictionDevice',_FrictionDevice,Graph))
  ,(rdf(FrictionDevice,aixm:'frictionDevice',FrictionDeviceNode,Graph))
  ,rdf(FrictionDeviceNode,rdf:value,FrictionDeviceValue,Graph)
  ,FrictionDevice=val(FrictionDeviceValue))
  ,((ObscuredLights='$null$',\+ rdf(SurfaceContamination,aixm:'obscuredLights',_ObscuredLights,Graph))
  ,(rdf(ObscuredLights,aixm:'obscuredLights',ObscuredLightsNode,Graph))
  ,rdf(ObscuredLightsNode,rdf:value,ObscuredLightsValue,Graph)
  ,ObscuredLights=val(ObscuredLightsValue))
  ,((FurtherClearanceTime='$null$',\+ rdf(SurfaceContamination,aixm:'furtherClearanceTime',_FurtherClearanceTime,Graph))
  ,(rdf(FurtherClearanceTime,aixm:'furtherClearanceTime',FurtherClearanceTimeNode,Graph))
  ,rdf(FurtherClearanceTimeNode,rdf:value,FurtherClearanceTimeValue,Graph)
  ,FurtherClearanceTime=val(FurtherClearanceTimeValue))
  ,((FurtherTotalClearance='$null$',\+ rdf(SurfaceContamination,aixm:'furtherTotalClearance',_FurtherTotalClearance,Graph))
  ,(rdf(FurtherTotalClearance,aixm:'furtherTotalClearance',FurtherTotalClearanceNode,Graph))
  ,rdf(FurtherTotalClearanceNode,rdf:value,FurtherTotalClearanceValue,Graph)
  ,FurtherTotalClearance=val(FurtherTotalClearanceValue))
  ,((NextObservationTime='$null$',\+ rdf(SurfaceContamination,aixm:'nextObservationTime',_NextObservationTime,Graph))
  ,(rdf(NextObservationTime,aixm:'nextObservationTime',NextObservationTimeNode,Graph))
  ,rdf(NextObservationTimeNode,rdf:value,NextObservationTimeValue,Graph)
  ,NextObservationTime=val(NextObservationTimeValue))
  ,((Proportion='$null$',\+ rdf(SurfaceContamination,aixm:'proportion',_Proportion,Graph))
  ,(rdf(Proportion,aixm:'proportion',ProportionNode,Graph))
  ,rdf(ProportionNode,rdf:value,ProportionValue,Graph)
  ,Proportion=val(ProportionValue))


 .

fixm_MeteorologicalData(Graph, MeteorologicalData, Temperature, WindDirection, WindSpeed) :-
  rdf(MeteorologicalData,rdf:type,fixm:'MeteorologicalData',Graph)
  ,((Temperature='$null$',\+ rdf(MeteorologicalData,fixm:'temperature',_Temperature,Graph))
  ,(rdf(Temperature,fixm:'temperature',TemperatureNode,Graph))
  ,rdf(TemperatureNode,rdf:value,TemperatureValue,Graph)
  ,Temperature=val(TemperatureValue))
  ,((WindDirection='$null$',\+ rdf(MeteorologicalData,fixm:'windDirection',_WindDirection,Graph))
  ,(rdf(WindDirection,fixm:'windDirection',WindDirectionNode,Graph))
  ,rdf(WindDirectionNode,rdf:value,WindDirectionValue,Graph)
  ,WindDirection=val(WindDirectionValue))
  ,((WindSpeed='$null$',\+ rdf(MeteorologicalData,fixm:'windSpeed',_WindSpeed,Graph))
  ,(rdf(WindSpeed,fixm:'windSpeed',WindSpeedNode,Graph))
  ,rdf(WindSpeedNode,rdf:value,WindSpeedValue,Graph)
  ,WindSpeed=val(WindSpeedValue)) .

aixm_OrganisationAuthority(Graph, OrganisationAuthority, TimeSlice) :-
  rdf(OrganisationAuthority,rdf:type,aixm:'OrganisationAuthority',Graph)
 .

fixm_TelephoneContact(Graph, TelephoneContact, Voice, Facimile) :-
  rdf(TelephoneContact,rdf:type,fixm:'TelephoneContact',Graph)
  ,((Voice='$null$',\+ rdf(TelephoneContact,fixm:'voice',_Voice,Graph))
  ,(rdf(Voice,fixm:'voice',VoiceNode,Graph))
  ,rdf(VoiceNode,rdf:value,VoiceValue,Graph)
  ,Voice=val(VoiceValue))
  ,((Facimile='$null$',\+ rdf(TelephoneContact,fixm:'facimile',_Facimile,Graph))
  ,(rdf(Facimile,fixm:'facimile',FacimileNode,Graph))
  ,rdf(FacimileNode,rdf:value,FacimileValue,Graph)
  ,Facimile=val(FacimileValue)) .

fixm_ShippingInformation(Graph, ShippingInformation, AerodromeOfLoading, AerodromeOfUnloading, DangerousGoodsScreeningLocation, DepartureCountry, DestinationCountry, OriginCountry, ShipmentAuthorizations, SubsidiaryHazardClassAndDivision, SupplementaryInformation, TransferAerodromes, DeclarationText, Consignee, Shipper) :-
  rdf(ShippingInformation,rdf:type,fixm:'ShippingInformation',Graph)
  ,((AerodromeOfLoading='$null$',\+ rdf(ShippingInformation,fixm:'aerodromeOfLoading',_AerodromeOfLoading,Graph))
  ,(rdf(AerodromeOfLoading,fixm:'aerodromeOfLoading',AerodromeOfLoadingNode,Graph))
  ,rdf(AerodromeOfLoadingNode,rdf:value,AerodromeOfLoadingValue,Graph)
  ,AerodromeOfLoading=val(AerodromeOfLoadingValue))
  ,((AerodromeOfUnloading='$null$',\+ rdf(ShippingInformation,fixm:'aerodromeOfUnloading',_AerodromeOfUnloading,Graph))
  ,(rdf(AerodromeOfUnloading,fixm:'aerodromeOfUnloading',AerodromeOfUnloadingNode,Graph))
  ,rdf(AerodromeOfUnloadingNode,rdf:value,AerodromeOfUnloadingValue,Graph)
  ,AerodromeOfUnloading=val(AerodromeOfUnloadingValue))
  ,((DangerousGoodsScreeningLocation='$null$',\+ rdf(ShippingInformation,fixm:'dangerousGoodsScreeningLocation',_DangerousGoodsScreeningLocation,Graph))
  ,(rdf(DangerousGoodsScreeningLocation,fixm:'dangerousGoodsScreeningLocation',DangerousGoodsScreeningLocationNode,Graph))
  ,rdf(DangerousGoodsScreeningLocationNode,rdf:value,DangerousGoodsScreeningLocationValue,Graph)
  ,DangerousGoodsScreeningLocation=val(DangerousGoodsScreeningLocationValue))
  ,((DepartureCountry='$null$',\+ rdf(ShippingInformation,fixm:'departureCountry',_DepartureCountry,Graph))
  ,(rdf(DepartureCountry,fixm:'departureCountry',DepartureCountryNode,Graph))
  ,rdf(DepartureCountryNode,rdf:value,DepartureCountryValue,Graph)
  ,DepartureCountry=val(DepartureCountryValue))
  ,((DestinationCountry='$null$',\+ rdf(ShippingInformation,fixm:'destinationCountry',_DestinationCountry,Graph))
  ,(rdf(DestinationCountry,fixm:'destinationCountry',DestinationCountryNode,Graph))
  ,rdf(DestinationCountryNode,rdf:value,DestinationCountryValue,Graph)
  ,DestinationCountry=val(DestinationCountryValue))
  ,((OriginCountry='$null$',\+ rdf(ShippingInformation,fixm:'originCountry',_OriginCountry,Graph))
  ,(rdf(OriginCountry,fixm:'originCountry',OriginCountryNode,Graph))
  ,rdf(OriginCountryNode,rdf:value,OriginCountryValue,Graph)
  ,OriginCountry=val(OriginCountryValue))
  ,((ShipmentAuthorizations='$null$',\+ rdf(ShippingInformation,fixm:'shipmentAuthorizations',_ShipmentAuthorizations,Graph))
  ,(rdf(ShipmentAuthorizations,fixm:'shipmentAuthorizations',ShipmentAuthorizationsNode,Graph))
  ,rdf(ShipmentAuthorizationsNode,rdf:value,ShipmentAuthorizationsValue,Graph)
  ,ShipmentAuthorizations=val(ShipmentAuthorizationsValue))
  ,((SubsidiaryHazardClassAndDivision='$null$',\+ rdf(ShippingInformation,fixm:'subsidiaryHazardClassAndDivision',_SubsidiaryHazardClassAndDivision,Graph))
  ,(rdf(SubsidiaryHazardClassAndDivision,fixm:'subsidiaryHazardClassAndDivision',SubsidiaryHazardClassAndDivisionNode,Graph))
  ,rdf(SubsidiaryHazardClassAndDivisionNode,rdf:value,SubsidiaryHazardClassAndDivisionValue,Graph)
  ,SubsidiaryHazardClassAndDivision=val(SubsidiaryHazardClassAndDivisionValue))
  ,((SupplementaryInformation='$null$',\+ rdf(ShippingInformation,fixm:'supplementaryInformation',_SupplementaryInformation,Graph))
  ,(rdf(SupplementaryInformation,fixm:'supplementaryInformation',SupplementaryInformationNode,Graph))
  ,rdf(SupplementaryInformationNode,rdf:value,SupplementaryInformationValue,Graph)
  ,SupplementaryInformation=val(SupplementaryInformationValue))

  ,((DeclarationText='$null$',\+ rdf(ShippingInformation,fixm:'declarationText',_DeclarationText,Graph))
  ,(rdf(DeclarationText,fixm:'declarationText',DeclarationTextNode,Graph))
  ,rdf(DeclarationTextNode,rdf:value,DeclarationTextValue,Graph)
  ,DeclarationText=val(DeclarationTextValue))
  ,((Consignee='$null$',\+ rdf(ShippingInformation,fixm:'consignee',_Consignee,Graph))
  ,(rdf(Consignee,fixm:'consignee',ConsigneeNode,Graph))
  ,rdf(ConsigneeNode,rdf:value,ConsigneeValue,Graph)
  ,Consignee=val(ConsigneeValue))
  ,((Shipper='$null$',\+ rdf(ShippingInformation,fixm:'shipper',_Shipper,Graph))
  ,(rdf(Shipper,fixm:'shipper',ShipperNode,Graph))
  ,rdf(ShipperNode,rdf:value,ShipperValue,Graph)
  ,Shipper=val(ShipperValue)) .

aixm_AirportHeliportContamination(Graph, AirportHeliportContamination) :-
  rdf(AirportHeliportContamination,rdf:type,aixm:'AirportHeliportContamination',Graph) .

fixm_OtherInformation(Graph, OtherInformation, ReplacementFlightPlanIndicator, RunwayVisualRange) :-
  rdf(OtherInformation,rdf:type,fixm:'OtherInformation',Graph)
  ,((ReplacementFlightPlanIndicator='$null$',\+ rdf(OtherInformation,fixm:'replacementFlightPlanIndicator',_ReplacementFlightPlanIndicator,Graph))
  ,(rdf(ReplacementFlightPlanIndicator,fixm:'replacementFlightPlanIndicator',ReplacementFlightPlanIndicatorNode,Graph))
  ,rdf(ReplacementFlightPlanIndicatorNode,rdf:value,ReplacementFlightPlanIndicatorValue,Graph)
  ,ReplacementFlightPlanIndicator=val(ReplacementFlightPlanIndicatorValue))
  ,((RunwayVisualRange='$null$',\+ rdf(OtherInformation,fixm:'runwayVisualRange',_RunwayVisualRange,Graph))
  ,(rdf(RunwayVisualRange,fixm:'runwayVisualRange',RunwayVisualRangeNode,Graph))
  ,rdf(RunwayVisualRangeNode,rdf:value,RunwayVisualRangeValue,Graph)
  ,RunwayVisualRange=val(RunwayVisualRangeValue)) .

fixm_DinghyColour(Graph, DinghyColour) :-
  rdf(DinghyColour,rdf:type,fixm:'DinghyColour',Graph) .

fixm_CpdlcConnection(Graph, CpdlcConnection, ReceivingUnitFrequency, AtnLogonParameters, SendCpldcIndicator, ConnectionStatus, FrequencyUsage, Fans1ALogonParameters) :-
  rdf(CpdlcConnection,rdf:type,fixm:'CpdlcConnection',Graph)
  ,((ReceivingUnitFrequency='$null$',\+ rdf(CpdlcConnection,fixm:'receivingUnitFrequency',_ReceivingUnitFrequency,Graph))
  ,(rdf(ReceivingUnitFrequency,fixm:'receivingUnitFrequency',ReceivingUnitFrequencyNode,Graph))
  ,rdf(ReceivingUnitFrequencyNode,rdf:value,ReceivingUnitFrequencyValue,Graph)
  ,ReceivingUnitFrequency=val(ReceivingUnitFrequencyValue))
  ,((AtnLogonParameters='$null$',\+ rdf(CpdlcConnection,fixm:'atnLogonParameters',_AtnLogonParameters,Graph))
  ,(rdf(AtnLogonParameters,fixm:'atnLogonParameters',AtnLogonParametersNode,Graph))
  ,rdf(AtnLogonParametersNode,rdf:value,AtnLogonParametersValue,Graph)
  ,AtnLogonParameters=val(AtnLogonParametersValue))
  ,((SendCpldcIndicator='$null$',\+ rdf(CpdlcConnection,fixm:'sendCpldcIndicator',_SendCpldcIndicator,Graph))
  ,(rdf(SendCpldcIndicator,fixm:'sendCpldcIndicator',SendCpldcIndicatorNode,Graph))
  ,rdf(SendCpldcIndicatorNode,rdf:value,SendCpldcIndicatorValue,Graph)
  ,SendCpldcIndicator=val(SendCpldcIndicatorValue))
  ,((ConnectionStatus='$null$',\+ rdf(CpdlcConnection,fixm:'connectionStatus',_ConnectionStatus,Graph))
  ,(rdf(ConnectionStatus,fixm:'connectionStatus',ConnectionStatusNode,Graph))
  ,rdf(ConnectionStatusNode,rdf:value,ConnectionStatusValue,Graph)
  ,ConnectionStatus=val(ConnectionStatusValue))
  ,((FrequencyUsage='$null$',\+ rdf(CpdlcConnection,fixm:'frequencyUsage',_FrequencyUsage,Graph))
  ,(rdf(FrequencyUsage,fixm:'frequencyUsage',FrequencyUsageNode,Graph))
  ,rdf(FrequencyUsageNode,rdf:value,FrequencyUsageValue,Graph)
  ,FrequencyUsage=val(FrequencyUsageValue))
  ,((Fans1ALogonParameters='$null$',\+ rdf(CpdlcConnection,fixm:'fans1ALogonParameters',_Fans1ALogonParameters,Graph))
  ,(rdf(Fans1ALogonParameters,fixm:'fans1ALogonParameters',Fans1ALogonParametersNode,Graph))
  ,rdf(Fans1ALogonParametersNode,rdf:value,Fans1ALogonParametersValue,Graph)
  ,Fans1ALogonParameters=val(Fans1ALogonParametersValue)) .

aixm_TelephoneContact(Graph, TelephoneContact, Voice, Facsimile) :-
  rdf(TelephoneContact,rdf:type,aixm:'TelephoneContact',Graph)
  ,((Voice='$null$',\+ rdf(TelephoneContact,aixm:'voice',_Voice,Graph))
  ,(rdf(Voice,aixm:'voice',VoiceNode,Graph))
  ,rdf(VoiceNode,rdf:value,VoiceValue,Graph)
  ,Voice=val(VoiceValue))
  ,((Facsimile='$null$',\+ rdf(TelephoneContact,aixm:'facsimile',_Facsimile,Graph))
  ,(rdf(Facsimile,aixm:'facsimile',FacsimileNode,Graph))
  ,rdf(FacsimileNode,rdf:value,FacsimileValue,Graph)
  ,Facsimile=val(FacsimileValue)) .

fixm_Route(Graph, Route, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTime, ExpandedRoute, ClimbSchedule, DescentSchedule, Segment) :-
  subClassOf(T,fixm:'Route')
  ,rdf(Route,rdf:type,T,Graph)
  ,((AirfileRouteStartTime='$null$',\+ rdf(Route,fixm:'airfileRouteStartTime',_AirfileRouteStartTime,Graph))
  ,(rdf(AirfileRouteStartTime,fixm:'airfileRouteStartTime',AirfileRouteStartTimeNode,Graph))
  ,rdf(AirfileRouteStartTimeNode,rdf:value,AirfileRouteStartTimeValue,Graph)
  ,AirfileRouteStartTime=val(AirfileRouteStartTimeValue))
  ,((FlightDuration='$null$',\+ rdf(Route,fixm:'flightDuration',_FlightDuration,Graph))
  ,(rdf(FlightDuration,fixm:'flightDuration',FlightDurationNode,Graph))
  ,rdf(FlightDurationNode,rdf:value,FlightDurationValue,Graph)
  ,FlightDuration=val(FlightDurationValue))
  ,((InitialCruisingSpeed='$null$',\+ rdf(Route,fixm:'initialCruisingSpeed',_InitialCruisingSpeed,Graph))
  ,(rdf(InitialCruisingSpeed,fixm:'initialCruisingSpeed',InitialCruisingSpeedNode,Graph))
  ,rdf(InitialCruisingSpeedNode,rdf:value,InitialCruisingSpeedValue,Graph)
  ,InitialCruisingSpeed=val(InitialCruisingSpeedValue))
  ,((InitialFlightRules='$null$',\+ rdf(Route,fixm:'initialFlightRules',_InitialFlightRules,Graph))
  ,(rdf(InitialFlightRules,fixm:'initialFlightRules',InitialFlightRulesNode,Graph))
  ,rdf(InitialFlightRulesNode,rdf:value,InitialFlightRulesValue,Graph)
  ,InitialFlightRules=val(InitialFlightRulesValue))
  ,((RequestedAltitude='$null$',\+ rdf(Route,fixm:'requestedAltitude',_RequestedAltitude,Graph))
  ,(rdf(RequestedAltitude,fixm:'requestedAltitude',RequestedAltitudeNode,Graph))
  ,rdf(RequestedAltitudeNode,rdf:value,RequestedAltitudeValue,Graph)
  ,RequestedAltitude=val(RequestedAltitudeValue))
  ,((RouteText='$null$',\+ rdf(Route,fixm:'routeText',_RouteText,Graph))
  ,(rdf(RouteText,fixm:'routeText',RouteTextNode,Graph))
  ,rdf(RouteTextNode,rdf:value,RouteTextValue,Graph)
  ,RouteText=val(RouteTextValue))

  ,((ExpandedRoute='$null$',\+ rdf(Route,fixm:'expandedRoute',_ExpandedRoute,Graph))
  ,(rdf(ExpandedRoute,fixm:'expandedRoute',ExpandedRouteNode,Graph))
  ,rdf(ExpandedRouteNode,rdf:value,ExpandedRouteValue,Graph)
  ,ExpandedRoute=val(ExpandedRouteValue))
  ,((ClimbSchedule='$null$',\+ rdf(Route,fixm:'climbSchedule',_ClimbSchedule,Graph))
  ,(rdf(ClimbSchedule,fixm:'climbSchedule',ClimbScheduleNode,Graph))
  ,rdf(ClimbScheduleNode,rdf:value,ClimbScheduleValue,Graph)
  ,ClimbSchedule=val(ClimbScheduleValue))
  ,((DescentSchedule='$null$',\+ rdf(Route,fixm:'descentSchedule',_DescentSchedule,Graph))
  ,(rdf(DescentSchedule,fixm:'descentSchedule',DescentScheduleNode,Graph))
  ,rdf(DescentScheduleNode,rdf:value,DescentScheduleValue,Graph)
  ,DescentSchedule=val(DescentScheduleValue))
 .

fixm_Person(Graph, Person, Name, Contact) :-
  rdf(Person,rdf:type,fixm:'Person',Graph)
  ,((Name='$null$',\+ rdf(Person,fixm:'name',_Name,Graph))
  ,(rdf(Name,fixm:'name',NameNode,Graph))
  ,rdf(NameNode,rdf:value,NameValue,Graph)
  ,Name=val(NameValue))
  ,((Contact='$null$',\+ rdf(Person,fixm:'contact',_Contact,Graph))
  ,(rdf(Contact,fixm:'contact',ContactNode,Graph))
  ,rdf(ContactNode,rdf:value,ContactValue,Graph)
  ,Contact=val(ContactValue)) .

fixm_EfplFlight(Graph, EfplFlight, IfplId, TotalEstimatedElapsedTime, AerodromesOfDestination, EfplSpecialHandling, EfplFiledTrajectory, EfplAcceptedTrajectory, OtherInformation, FlightPerformanceData) :-
  rdf(EfplFlight,rdf:type,fixm:'EfplFlight',Graph)
  ,((IfplId='$null$',\+ rdf(EfplFlight,fixm:'ifplId',_IfplId,Graph))
  ,(rdf(IfplId,fixm:'ifplId',IfplIdNode,Graph))
  ,rdf(IfplIdNode,rdf:value,IfplIdValue,Graph)
  ,IfplId=val(IfplIdValue))
  ,((TotalEstimatedElapsedTime='$null$',\+ rdf(EfplFlight,fixm:'totalEstimatedElapsedTime',_TotalEstimatedElapsedTime,Graph))
  ,(rdf(TotalEstimatedElapsedTime,fixm:'totalEstimatedElapsedTime',TotalEstimatedElapsedTimeNode,Graph))
  ,rdf(TotalEstimatedElapsedTimeNode,rdf:value,TotalEstimatedElapsedTimeValue,Graph)
  ,TotalEstimatedElapsedTime=val(TotalEstimatedElapsedTimeValue))
  ,((AerodromesOfDestination='$null$',\+ rdf(EfplFlight,fixm:'aerodromesOfDestination',_AerodromesOfDestination,Graph))
  ,(rdf(AerodromesOfDestination,fixm:'aerodromesOfDestination',AerodromesOfDestinationNode,Graph))
  ,rdf(AerodromesOfDestinationNode,rdf:value,AerodromesOfDestinationValue,Graph)
  ,AerodromesOfDestination=val(AerodromesOfDestinationValue))
  ,((EfplSpecialHandling='$null$',\+ rdf(EfplFlight,fixm:'efplSpecialHandling',_EfplSpecialHandling,Graph))
  ,(rdf(EfplSpecialHandling,fixm:'efplSpecialHandling',EfplSpecialHandlingNode,Graph))
  ,rdf(EfplSpecialHandlingNode,rdf:value,EfplSpecialHandlingValue,Graph)
  ,EfplSpecialHandling=val(EfplSpecialHandlingValue))
  ,((EfplFiledTrajectory='$null$',\+ rdf(EfplFlight,fixm:'efplFiledTrajectory',_EfplFiledTrajectory,Graph))
  ,(rdf(EfplFiledTrajectory,fixm:'efplFiledTrajectory',EfplFiledTrajectoryNode,Graph))
  ,rdf(EfplFiledTrajectoryNode,rdf:value,EfplFiledTrajectoryValue,Graph)
  ,EfplFiledTrajectory=val(EfplFiledTrajectoryValue))
  ,((EfplAcceptedTrajectory='$null$',\+ rdf(EfplFlight,fixm:'efplAcceptedTrajectory',_EfplAcceptedTrajectory,Graph))
  ,(rdf(EfplAcceptedTrajectory,fixm:'efplAcceptedTrajectory',EfplAcceptedTrajectoryNode,Graph))
  ,rdf(EfplAcceptedTrajectoryNode,rdf:value,EfplAcceptedTrajectoryValue,Graph)
  ,EfplAcceptedTrajectory=val(EfplAcceptedTrajectoryValue))
  ,((OtherInformation='$null$',\+ rdf(EfplFlight,fixm:'otherInformation',_OtherInformation,Graph))
  ,(rdf(OtherInformation,fixm:'otherInformation',OtherInformationNode,Graph))
  ,rdf(OtherInformationNode,rdf:value,OtherInformationValue,Graph)
  ,OtherInformation=val(OtherInformationValue))
  ,((FlightPerformanceData='$null$',\+ rdf(EfplFlight,fixm:'flightPerformanceData',_FlightPerformanceData,Graph))
  ,(rdf(FlightPerformanceData,fixm:'flightPerformanceData',FlightPerformanceDataNode,Graph))
  ,rdf(FlightPerformanceDataNode,rdf:value,FlightPerformanceDataValue,Graph)
  ,FlightPerformanceData=val(FlightPerformanceDataValue)) .

fixm_Originator(Graph, Originator) :-
  rdf(Originator,rdf:type,fixm:'Originator',Graph) .

fixm_FlightStatus(Graph, FlightStatus, AirborneHold, Airfile, Accepted, FlightCycle, MissedApproach, Suspended) :-
  rdf(FlightStatus,rdf:type,fixm:'FlightStatus',Graph)
  ,((AirborneHold='$null$',\+ rdf(FlightStatus,fixm:'airborneHold',_AirborneHold,Graph))
  ,(rdf(AirborneHold,fixm:'airborneHold',AirborneHoldNode,Graph))
  ,rdf(AirborneHoldNode,rdf:value,AirborneHoldValue,Graph)
  ,AirborneHold=val(AirborneHoldValue))
  ,((Airfile='$null$',\+ rdf(FlightStatus,fixm:'airfile',_Airfile,Graph))
  ,(rdf(Airfile,fixm:'airfile',AirfileNode,Graph))
  ,rdf(AirfileNode,rdf:value,AirfileValue,Graph)
  ,Airfile=val(AirfileValue))
  ,((Accepted='$null$',\+ rdf(FlightStatus,fixm:'accepted',_Accepted,Graph))
  ,(rdf(Accepted,fixm:'accepted',AcceptedNode,Graph))
  ,rdf(AcceptedNode,rdf:value,AcceptedValue,Graph)
  ,Accepted=val(AcceptedValue))
  ,((FlightCycle='$null$',\+ rdf(FlightStatus,fixm:'flightCycle',_FlightCycle,Graph))
  ,(rdf(FlightCycle,fixm:'flightCycle',FlightCycleNode,Graph))
  ,rdf(FlightCycleNode,rdf:value,FlightCycleValue,Graph)
  ,FlightCycle=val(FlightCycleValue))
  ,((MissedApproach='$null$',\+ rdf(FlightStatus,fixm:'missedApproach',_MissedApproach,Graph))
  ,(rdf(MissedApproach,fixm:'missedApproach',MissedApproachNode,Graph))
  ,rdf(MissedApproachNode,rdf:value,MissedApproachValue,Graph)
  ,MissedApproach=val(MissedApproachValue))
  ,((Suspended='$null$',\+ rdf(FlightStatus,fixm:'suspended',_Suspended,Graph))
  ,(rdf(Suspended,fixm:'suspended',SuspendedNode,Graph))
  ,rdf(SuspendedNode,rdf:value,SuspendedValue,Graph)
  ,Suspended=val(SuspendedValue)) .

fixm_IdentifiedUnitReference(Graph, IdentifiedUnitReference, UnitIdentifier) :-
  rdf(IdentifiedUnitReference,rdf:type,fixm:'IdentifiedUnitReference',Graph)
  ,((UnitIdentifier='$null$',\+ rdf(IdentifiedUnitReference,fixm:'unitIdentifier',_UnitIdentifier,Graph))
  ,(rdf(UnitIdentifier,fixm:'unitIdentifier',UnitIdentifierNode,Graph))
  ,rdf(UnitIdentifierNode,rdf:value,UnitIdentifierValue,Graph)
  ,UnitIdentifier=val(UnitIdentifierValue)) .

fixm_Radionuclide(Graph, Radionuclide, PhysicalChemicalForm, RadionuclideId, RadionuclideName, LowDispersibleMaterialIndicator, Activity, SpecialFormIndicator) :-
  rdf(Radionuclide,rdf:type,fixm:'Radionuclide',Graph)
  ,((PhysicalChemicalForm='$null$',\+ rdf(Radionuclide,fixm:'physicalChemicalForm',_PhysicalChemicalForm,Graph))
  ,(rdf(PhysicalChemicalForm,fixm:'physicalChemicalForm',PhysicalChemicalFormNode,Graph))
  ,rdf(PhysicalChemicalFormNode,rdf:value,PhysicalChemicalFormValue,Graph)
  ,PhysicalChemicalForm=val(PhysicalChemicalFormValue))
  ,((RadionuclideId='$null$',\+ rdf(Radionuclide,fixm:'radionuclideId',_RadionuclideId,Graph))
  ,(rdf(RadionuclideId,fixm:'radionuclideId',RadionuclideIdNode,Graph))
  ,rdf(RadionuclideIdNode,rdf:value,RadionuclideIdValue,Graph)
  ,RadionuclideId=val(RadionuclideIdValue))
  ,((RadionuclideName='$null$',\+ rdf(Radionuclide,fixm:'radionuclideName',_RadionuclideName,Graph))
  ,(rdf(RadionuclideName,fixm:'radionuclideName',RadionuclideNameNode,Graph))
  ,rdf(RadionuclideNameNode,rdf:value,RadionuclideNameValue,Graph)
  ,RadionuclideName=val(RadionuclideNameValue))
  ,((LowDispersibleMaterialIndicator='$null$',\+ rdf(Radionuclide,fixm:'lowDispersibleMaterialIndicator',_LowDispersibleMaterialIndicator,Graph))
  ,(rdf(LowDispersibleMaterialIndicator,fixm:'lowDispersibleMaterialIndicator',LowDispersibleMaterialIndicatorNode,Graph))
  ,rdf(LowDispersibleMaterialIndicatorNode,rdf:value,LowDispersibleMaterialIndicatorValue,Graph)
  ,LowDispersibleMaterialIndicator=val(LowDispersibleMaterialIndicatorValue))
  ,((Activity='$null$',\+ rdf(Radionuclide,fixm:'activity',_Activity,Graph))
  ,(rdf(Activity,fixm:'activity',ActivityNode,Graph))
  ,rdf(ActivityNode,rdf:value,ActivityValue,Graph)
  ,Activity=val(ActivityValue))
  ,((SpecialFormIndicator='$null$',\+ rdf(Radionuclide,fixm:'specialFormIndicator',_SpecialFormIndicator,Graph))
  ,(rdf(SpecialFormIndicator,fixm:'specialFormIndicator',SpecialFormIndicatorNode,Graph))
  ,rdf(SpecialFormIndicatorNode,rdf:value,SpecialFormIndicatorValue,Graph)
  ,SpecialFormIndicator=val(SpecialFormIndicatorValue)) .

aixm_OnlineContact(Graph, OnlineContact, Network, Linkage, Protocol, EMail) :-
  rdf(OnlineContact,rdf:type,aixm:'OnlineContact',Graph)
  ,((Network='$null$',\+ rdf(OnlineContact,aixm:'network',_Network,Graph))
  ,(rdf(Network,aixm:'network',NetworkNode,Graph))
  ,rdf(NetworkNode,rdf:value,NetworkValue,Graph)
  ,Network=val(NetworkValue))
  ,((Linkage='$null$',\+ rdf(OnlineContact,aixm:'linkage',_Linkage,Graph))
  ,(rdf(Linkage,aixm:'linkage',LinkageNode,Graph))
  ,rdf(LinkageNode,rdf:value,LinkageValue,Graph)
  ,Linkage=val(LinkageValue))
  ,((Protocol='$null$',\+ rdf(OnlineContact,aixm:'protocol',_Protocol,Graph))
  ,(rdf(Protocol,aixm:'protocol',ProtocolNode,Graph))
  ,rdf(ProtocolNode,rdf:value,ProtocolValue,Graph)
  ,Protocol=val(ProtocolValue))
  ,((EMail='$null$',\+ rdf(OnlineContact,aixm:'eMail',_EMail,Graph))
  ,(rdf(EMail,aixm:'eMail',EMailNode,Graph))
  ,rdf(EMailNode,rdf:value,EMailValue,Graph)
  ,EMail=val(EMailValue)) .

fixm_StructuredPostalAddress(Graph, StructuredPostalAddress) :-
  rdf(StructuredPostalAddress,rdf:type,fixm:'StructuredPostalAddress',Graph) .

fixm_AircraftPosition(Graph, AircraftPosition, Altitude, Position, PositionTime, Track, ActualSpeed, NextPosition, ReportSource, FollowingPosition) :-
  rdf(AircraftPosition,rdf:type,fixm:'AircraftPosition',Graph)
  ,((Altitude='$null$',\+ rdf(AircraftPosition,fixm:'altitude',_Altitude,Graph))
  ,(rdf(Altitude,fixm:'altitude',AltitudeNode,Graph))
  ,rdf(AltitudeNode,rdf:value,AltitudeValue,Graph)
  ,Altitude=val(AltitudeValue))
  ,((Position='$null$',\+ rdf(AircraftPosition,fixm:'position',_Position,Graph))
  ,(rdf(Position,fixm:'position',PositionNode,Graph))
  ,rdf(PositionNode,rdf:value,PositionValue,Graph)
  ,Position=val(PositionValue))
  ,((PositionTime='$null$',\+ rdf(AircraftPosition,fixm:'positionTime',_PositionTime,Graph))
  ,(rdf(PositionTime,fixm:'positionTime',PositionTimeNode,Graph))
  ,rdf(PositionTimeNode,rdf:value,PositionTimeValue,Graph)
  ,PositionTime=val(PositionTimeValue))
  ,((Track='$null$',\+ rdf(AircraftPosition,fixm:'track',_Track,Graph))
  ,(rdf(Track,fixm:'track',TrackNode,Graph))
  ,rdf(TrackNode,rdf:value,TrackValue,Graph)
  ,Track=val(TrackValue))
  ,((ActualSpeed='$null$',\+ rdf(AircraftPosition,fixm:'actualSpeed',_ActualSpeed,Graph))
  ,(rdf(ActualSpeed,fixm:'actualSpeed',ActualSpeedNode,Graph))
  ,rdf(ActualSpeedNode,rdf:value,ActualSpeedValue,Graph)
  ,ActualSpeed=val(ActualSpeedValue))
  ,((NextPosition='$null$',\+ rdf(AircraftPosition,fixm:'nextPosition',_NextPosition,Graph))
  ,(rdf(NextPosition,fixm:'nextPosition',NextPositionNode,Graph))
  ,rdf(NextPositionNode,rdf:value,NextPositionValue,Graph)
  ,NextPosition=val(NextPositionValue))
  ,((ReportSource='$null$',\+ rdf(AircraftPosition,fixm:'reportSource',_ReportSource,Graph))
  ,(rdf(ReportSource,fixm:'reportSource',ReportSourceNode,Graph))
  ,rdf(ReportSourceNode,rdf:value,ReportSourceValue,Graph)
  ,ReportSource=val(ReportSourceValue))
  ,((FollowingPosition='$null$',\+ rdf(AircraftPosition,fixm:'followingPosition',_FollowingPosition,Graph))
  ,(rdf(FollowingPosition,fixm:'followingPosition',FollowingPositionNode,Graph))
  ,rdf(FollowingPositionNode,rdf:value,FollowingPositionValue,Graph)
  ,FollowingPosition=val(FollowingPositionValue)) .

aixm_AirportHeliportUsage(Graph, AirportHeliportUsage, Operation) :-
  rdf(AirportHeliportUsage,rdf:type,aixm:'AirportHeliportUsage',Graph)
  ,((Operation='$null$',\+ rdf(AirportHeliportUsage,aixm:'operation',_Operation,Graph))
  ,(rdf(Operation,aixm:'operation',OperationNode,Graph))
  ,rdf(OperationNode,rdf:value,OperationValue,Graph)
  ,Operation=val(OperationValue)) .

aixm_Timesheet(Graph, Timesheet, TimeReference, StartDate, EndDate, Day, DayTil, StartTime, StartEvent, StartTimeRelativeEvent, StartEventInterpretation, EndTime, EndEvent, EndTimeRelativeEvent, EndEventInterpretation, DaylightSavingAdjust, Excluded, Annotation) :-
  rdf(Timesheet,rdf:type,aixm:'Timesheet',Graph)
  ,((TimeReference='$null$',\+ rdf(Timesheet,aixm:'timeReference',_TimeReference,Graph))
  ,(rdf(TimeReference,aixm:'timeReference',TimeReferenceNode,Graph))
  ,rdf(TimeReferenceNode,rdf:value,TimeReferenceValue,Graph)
  ,TimeReference=val(TimeReferenceValue))
  ,((StartDate='$null$',\+ rdf(Timesheet,aixm:'startDate',_StartDate,Graph))
  ,(rdf(StartDate,aixm:'startDate',StartDateNode,Graph))
  ,rdf(StartDateNode,rdf:value,StartDateValue,Graph)
  ,StartDate=val(StartDateValue))
  ,((EndDate='$null$',\+ rdf(Timesheet,aixm:'endDate',_EndDate,Graph))
  ,(rdf(EndDate,aixm:'endDate',EndDateNode,Graph))
  ,rdf(EndDateNode,rdf:value,EndDateValue,Graph)
  ,EndDate=val(EndDateValue))
  ,((Day='$null$',\+ rdf(Timesheet,aixm:'day',_Day,Graph))
  ,(rdf(Day,aixm:'day',DayNode,Graph))
  ,rdf(DayNode,rdf:value,DayValue,Graph)
  ,Day=val(DayValue))
  ,((DayTil='$null$',\+ rdf(Timesheet,aixm:'dayTil',_DayTil,Graph))
  ,(rdf(DayTil,aixm:'dayTil',DayTilNode,Graph))
  ,rdf(DayTilNode,rdf:value,DayTilValue,Graph)
  ,DayTil=val(DayTilValue))
  ,((StartTime='$null$',\+ rdf(Timesheet,aixm:'startTime',_StartTime,Graph))
  ,(rdf(StartTime,aixm:'startTime',StartTimeNode,Graph))
  ,rdf(StartTimeNode,rdf:value,StartTimeValue,Graph)
  ,StartTime=val(StartTimeValue))
  ,((StartEvent='$null$',\+ rdf(Timesheet,aixm:'startEvent',_StartEvent,Graph))
  ,(rdf(StartEvent,aixm:'startEvent',StartEventNode,Graph))
  ,rdf(StartEventNode,rdf:value,StartEventValue,Graph)
  ,StartEvent=val(StartEventValue))
  ,((StartTimeRelativeEvent='$null$',\+ rdf(Timesheet,aixm:'startTimeRelativeEvent',_StartTimeRelativeEvent,Graph))
  ,(rdf(StartTimeRelativeEvent,aixm:'startTimeRelativeEvent',StartTimeRelativeEventNode,Graph))
  ,rdf(StartTimeRelativeEventNode,rdf:value,StartTimeRelativeEventValue,Graph)
  ,StartTimeRelativeEvent=val(StartTimeRelativeEventValue))
  ,((StartEventInterpretation='$null$',\+ rdf(Timesheet,aixm:'startEventInterpretation',_StartEventInterpretation,Graph))
  ,(rdf(StartEventInterpretation,aixm:'startEventInterpretation',StartEventInterpretationNode,Graph))
  ,rdf(StartEventInterpretationNode,rdf:value,StartEventInterpretationValue,Graph)
  ,StartEventInterpretation=val(StartEventInterpretationValue))
  ,((EndTime='$null$',\+ rdf(Timesheet,aixm:'endTime',_EndTime,Graph))
  ,(rdf(EndTime,aixm:'endTime',EndTimeNode,Graph))
  ,rdf(EndTimeNode,rdf:value,EndTimeValue,Graph)
  ,EndTime=val(EndTimeValue))
  ,((EndEvent='$null$',\+ rdf(Timesheet,aixm:'endEvent',_EndEvent,Graph))
  ,(rdf(EndEvent,aixm:'endEvent',EndEventNode,Graph))
  ,rdf(EndEventNode,rdf:value,EndEventValue,Graph)
  ,EndEvent=val(EndEventValue))
  ,((EndTimeRelativeEvent='$null$',\+ rdf(Timesheet,aixm:'endTimeRelativeEvent',_EndTimeRelativeEvent,Graph))
  ,(rdf(EndTimeRelativeEvent,aixm:'endTimeRelativeEvent',EndTimeRelativeEventNode,Graph))
  ,rdf(EndTimeRelativeEventNode,rdf:value,EndTimeRelativeEventValue,Graph)
  ,EndTimeRelativeEvent=val(EndTimeRelativeEventValue))
  ,((EndEventInterpretation='$null$',\+ rdf(Timesheet,aixm:'endEventInterpretation',_EndEventInterpretation,Graph))
  ,(rdf(EndEventInterpretation,aixm:'endEventInterpretation',EndEventInterpretationNode,Graph))
  ,rdf(EndEventInterpretationNode,rdf:value,EndEventInterpretationValue,Graph)
  ,EndEventInterpretation=val(EndEventInterpretationValue))
  ,((DaylightSavingAdjust='$null$',\+ rdf(Timesheet,aixm:'daylightSavingAdjust',_DaylightSavingAdjust,Graph))
  ,(rdf(DaylightSavingAdjust,aixm:'daylightSavingAdjust',DaylightSavingAdjustNode,Graph))
  ,rdf(DaylightSavingAdjustNode,rdf:value,DaylightSavingAdjustValue,Graph)
  ,DaylightSavingAdjust=val(DaylightSavingAdjustValue))
  ,((Excluded='$null$',\+ rdf(Timesheet,aixm:'excluded',_Excluded,Graph))
  ,(rdf(Excluded,aixm:'excluded',ExcludedNode,Graph))
  ,rdf(ExcludedNode,rdf:value,ExcludedValue,Graph)
  ,Excluded=val(ExcludedValue))
 .

gml_SurfacePatch(Graph, SurfacePatch) :-
  subClassOf(T,gml:'SurfacePatch')
  ,rdf(SurfacePatch,rdf:type,T,Graph) .

fixm_MultiTime(Graph, MultiTime, Actual, Estimated) :-
  subClassOf(T,fixm:'MultiTime')
  ,rdf(MultiTime,rdf:type,T,Graph)
  ,((Actual='$null$',\+ rdf(MultiTime,fixm:'actual',_Actual,Graph))
  ,(rdf(Actual,fixm:'actual',ActualNode,Graph))
  ,rdf(ActualNode,rdf:value,ActualValue,Graph)
  ,Actual=val(ActualValue))
  ,((Estimated='$null$',\+ rdf(MultiTime,fixm:'estimated',_Estimated,Graph))
  ,(rdf(Estimated,fixm:'estimated',EstimatedNode,Graph))
  ,rdf(EstimatedNode,rdf:value,EstimatedValue,Graph)
  ,Estimated=val(EstimatedValue)) .

aixm_FlightCharacteristic(Graph, FlightCharacteristic, Type, Rule, Status, Military, Origin, Purpose, Annotation) :-
  rdf(FlightCharacteristic,rdf:type,aixm:'FlightCharacteristic',Graph)
  ,((Type='$null$',\+ rdf(FlightCharacteristic,aixm:'type',_Type,Graph))
  ,(rdf(Type,aixm:'type',TypeNode,Graph))
  ,rdf(TypeNode,rdf:value,TypeValue,Graph)
  ,Type=val(TypeValue))
  ,((Rule='$null$',\+ rdf(FlightCharacteristic,aixm:'rule',_Rule,Graph))
  ,(rdf(Rule,aixm:'rule',RuleNode,Graph))
  ,rdf(RuleNode,rdf:value,RuleValue,Graph)
  ,Rule=val(RuleValue))
  ,((Status='$null$',\+ rdf(FlightCharacteristic,aixm:'status',_Status,Graph))
  ,(rdf(Status,aixm:'status',StatusNode,Graph))
  ,rdf(StatusNode,rdf:value,StatusValue,Graph)
  ,Status=val(StatusValue))
  ,((Military='$null$',\+ rdf(FlightCharacteristic,aixm:'military',_Military,Graph))
  ,(rdf(Military,aixm:'military',MilitaryNode,Graph))
  ,rdf(MilitaryNode,rdf:value,MilitaryValue,Graph)
  ,Military=val(MilitaryValue))
  ,((Origin='$null$',\+ rdf(FlightCharacteristic,aixm:'origin',_Origin,Graph))
  ,(rdf(Origin,aixm:'origin',OriginNode,Graph))
  ,rdf(OriginNode,rdf:value,OriginValue,Graph)
  ,Origin=val(OriginValue))
  ,((Purpose='$null$',\+ rdf(FlightCharacteristic,aixm:'purpose',_Purpose,Graph))
  ,(rdf(Purpose,aixm:'purpose',PurposeNode,Graph))
  ,rdf(PurposeNode,rdf:value,PurposeValue,Graph)
  ,Purpose=val(PurposeValue))
 .

fixm_Provenance(Graph, Provenance, Timestamp, Centre, Source, System) :-
  rdf(Provenance,rdf:type,fixm:'Provenance',Graph)
  ,((Timestamp='$null$',\+ rdf(Provenance,fixm:'timestamp',_Timestamp,Graph))
  ,(rdf(Timestamp,fixm:'timestamp',TimestampNode,Graph))
  ,rdf(TimestampNode,rdf:value,TimestampValue,Graph)
  ,Timestamp=val(TimestampValue))
  ,((Centre='$null$',\+ rdf(Provenance,fixm:'centre',_Centre,Graph))
  ,(rdf(Centre,fixm:'centre',CentreNode,Graph))
  ,rdf(CentreNode,rdf:value,CentreValue,Graph)
  ,Centre=val(CentreValue))
  ,((Source='$null$',\+ rdf(Provenance,fixm:'source',_Source,Graph))
  ,(rdf(Source,fixm:'source',SourceNode,Graph))
  ,rdf(SourceNode,rdf:value,SourceValue,Graph)
  ,Source=val(SourceValue))
  ,((System='$null$',\+ rdf(Provenance,fixm:'system',_System,Graph))
  ,(rdf(System,fixm:'system',SystemNode,Graph))
  ,rdf(SystemNode,rdf:value,SystemValue,Graph)
  ,System=val(SystemValue)) .

aixm_AirportHeliport(Graph, AirportHeliport, TimeSlice) :-
  rdf(AirportHeliport,rdf:type,aixm:'AirportHeliport',Graph)
 .

fixm_TrajectoryPoint(Graph, TrajectoryPoint, AltimeterSetting, PredictedAirspeed, PredictedGroundspeed, MetData, Point, TrajectoryChange, TrajectoryChangeType, ReferencePoint) :-
  subClassOf(T,fixm:'TrajectoryPoint')
  ,rdf(TrajectoryPoint,rdf:type,T,Graph)
  ,((AltimeterSetting='$null$',\+ rdf(TrajectoryPoint,fixm:'altimeterSetting',_AltimeterSetting,Graph))
  ,(rdf(AltimeterSetting,fixm:'altimeterSetting',AltimeterSettingNode,Graph))
  ,rdf(AltimeterSettingNode,rdf:value,AltimeterSettingValue,Graph)
  ,AltimeterSetting=val(AltimeterSettingValue))
  ,((PredictedAirspeed='$null$',\+ rdf(TrajectoryPoint,fixm:'predictedAirspeed',_PredictedAirspeed,Graph))
  ,(rdf(PredictedAirspeed,fixm:'predictedAirspeed',PredictedAirspeedNode,Graph))
  ,rdf(PredictedAirspeedNode,rdf:value,PredictedAirspeedValue,Graph)
  ,PredictedAirspeed=val(PredictedAirspeedValue))
  ,((PredictedGroundspeed='$null$',\+ rdf(TrajectoryPoint,fixm:'predictedGroundspeed',_PredictedGroundspeed,Graph))
  ,(rdf(PredictedGroundspeed,fixm:'predictedGroundspeed',PredictedGroundspeedNode,Graph))
  ,rdf(PredictedGroundspeedNode,rdf:value,PredictedGroundspeedValue,Graph)
  ,PredictedGroundspeed=val(PredictedGroundspeedValue))
  ,((MetData='$null$',\+ rdf(TrajectoryPoint,fixm:'metData',_MetData,Graph))
  ,(rdf(MetData,fixm:'metData',MetDataNode,Graph))
  ,rdf(MetDataNode,rdf:value,MetDataValue,Graph)
  ,MetData=val(MetDataValue))
  ,((Point='$null$',\+ rdf(TrajectoryPoint,fixm:'point',_Point,Graph))
  ,(rdf(Point,fixm:'point',PointNode,Graph))
  ,rdf(PointNode,rdf:value,PointValue,Graph)
  ,Point=val(PointValue))


  ,((ReferencePoint='$null$',\+ rdf(TrajectoryPoint,fixm:'referencePoint',_ReferencePoint,Graph))
  ,(rdf(ReferencePoint,fixm:'referencePoint',ReferencePointNode,Graph))
  ,rdf(ReferencePointNode,rdf:value,ReferencePointValue,Graph)
  ,ReferencePoint=val(ReferencePointValue)) .

fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier, DistanceFromTakeOff, EfplEstimatedSpeed, ElapsedTime, GrossWeight, TrajectoryPointType, TrajectoryPointRole, InboundSegment) :-
  rdf(EfplTrajectoryPoint,rdf:type,fixm:'EfplTrajectoryPoint',Graph)
  ,((AerodromeIdentifier='$null$',\+ rdf(EfplTrajectoryPoint,fixm:'aerodromeIdentifier',_AerodromeIdentifier,Graph))
  ,(rdf(AerodromeIdentifier,fixm:'aerodromeIdentifier',AerodromeIdentifierNode,Graph))
  ,rdf(AerodromeIdentifierNode,rdf:value,AerodromeIdentifierValue,Graph)
  ,AerodromeIdentifier=val(AerodromeIdentifierValue))
  ,((DistanceFromTakeOff='$null$',\+ rdf(EfplTrajectoryPoint,fixm:'distanceFromTakeOff',_DistanceFromTakeOff,Graph))
  ,(rdf(DistanceFromTakeOff,fixm:'distanceFromTakeOff',DistanceFromTakeOffNode,Graph))
  ,rdf(DistanceFromTakeOffNode,rdf:value,DistanceFromTakeOffValue,Graph)
  ,DistanceFromTakeOff=val(DistanceFromTakeOffValue))
  ,((EfplEstimatedSpeed='$null$',\+ rdf(EfplTrajectoryPoint,fixm:'efplEstimatedSpeed',_EfplEstimatedSpeed,Graph))
  ,(rdf(EfplEstimatedSpeed,fixm:'efplEstimatedSpeed',EfplEstimatedSpeedNode,Graph))
  ,rdf(EfplEstimatedSpeedNode,rdf:value,EfplEstimatedSpeedValue,Graph)
  ,EfplEstimatedSpeed=val(EfplEstimatedSpeedValue))
  ,((ElapsedTime='$null$',\+ rdf(EfplTrajectoryPoint,fixm:'elapsedTime',_ElapsedTime,Graph))
  ,(rdf(ElapsedTime,fixm:'elapsedTime',ElapsedTimeNode,Graph))
  ,rdf(ElapsedTimeNode,rdf:value,ElapsedTimeValue,Graph)
  ,ElapsedTime=val(ElapsedTimeValue))
  ,((GrossWeight='$null$',\+ rdf(EfplTrajectoryPoint,fixm:'grossWeight',_GrossWeight,Graph))
  ,(rdf(GrossWeight,fixm:'grossWeight',GrossWeightNode,Graph))
  ,rdf(GrossWeightNode,rdf:value,GrossWeightValue,Graph)
  ,GrossWeight=val(GrossWeightValue))
  ,((TrajectoryPointType='$null$',\+ rdf(EfplTrajectoryPoint,fixm:'trajectoryPointType',_TrajectoryPointType,Graph))
  ,(rdf(TrajectoryPointType,fixm:'trajectoryPointType',TrajectoryPointTypeNode,Graph))
  ,rdf(TrajectoryPointTypeNode,rdf:value,TrajectoryPointTypeValue,Graph)
  ,TrajectoryPointType=val(TrajectoryPointTypeValue))
  ,((TrajectoryPointRole='$null$',\+ rdf(EfplTrajectoryPoint,fixm:'trajectoryPointRole',_TrajectoryPointRole,Graph))
  ,(rdf(TrajectoryPointRole,fixm:'trajectoryPointRole',TrajectoryPointRoleNode,Graph))
  ,rdf(TrajectoryPointRoleNode,rdf:value,TrajectoryPointRoleValue,Graph)
  ,TrajectoryPointRole=val(TrajectoryPointRoleValue))
  ,((InboundSegment='$null$',\+ rdf(EfplTrajectoryPoint,fixm:'inboundSegment',_InboundSegment,Graph))
  ,(rdf(InboundSegment,fixm:'inboundSegment',InboundSegmentNode,Graph))
  ,rdf(InboundSegmentNode,rdf:value,InboundSegmentValue,Graph)
  ,InboundSegment=val(InboundSegmentValue)) .

fixm_Temperatures(Graph, Temperatures, ControlTemperature, EmergencyTemperature, FlashpointTemperature) :-
  rdf(Temperatures,rdf:type,fixm:'Temperatures',Graph)
  ,((ControlTemperature='$null$',\+ rdf(Temperatures,fixm:'controlTemperature',_ControlTemperature,Graph))
  ,(rdf(ControlTemperature,fixm:'controlTemperature',ControlTemperatureNode,Graph))
  ,rdf(ControlTemperatureNode,rdf:value,ControlTemperatureValue,Graph)
  ,ControlTemperature=val(ControlTemperatureValue))
  ,((EmergencyTemperature='$null$',\+ rdf(Temperatures,fixm:'emergencyTemperature',_EmergencyTemperature,Graph))
  ,(rdf(EmergencyTemperature,fixm:'emergencyTemperature',EmergencyTemperatureNode,Graph))
  ,rdf(EmergencyTemperatureNode,rdf:value,EmergencyTemperatureValue,Graph)
  ,EmergencyTemperature=val(EmergencyTemperatureValue))
  ,((FlashpointTemperature='$null$',\+ rdf(Temperatures,fixm:'flashpointTemperature',_FlashpointTemperature,Graph))
  ,(rdf(FlashpointTemperature,fixm:'flashpointTemperature',FlashpointTemperatureNode,Graph))
  ,rdf(FlashpointTemperatureNode,rdf:value,FlashpointTemperatureValue,Graph)
  ,FlashpointTemperature=val(FlashpointTemperatureValue)) .

fixm_TrajectorySegment(Graph, TrajectorySegment, SegmentIdentifier, SegmentType) :-
  rdf(TrajectorySegment,rdf:type,fixm:'TrajectorySegment',Graph)
  ,((SegmentIdentifier='$null$',\+ rdf(TrajectorySegment,fixm:'segmentIdentifier',_SegmentIdentifier,Graph))
  ,(rdf(SegmentIdentifier,fixm:'segmentIdentifier',SegmentIdentifierNode,Graph))
  ,rdf(SegmentIdentifierNode,rdf:value,SegmentIdentifierValue,Graph)
  ,SegmentIdentifier=val(SegmentIdentifierValue))
  ,((SegmentType='$null$',\+ rdf(TrajectorySegment,fixm:'segmentType',_SegmentType,Graph))
  ,(rdf(SegmentType,fixm:'segmentType',SegmentTypeNode,Graph))
  ,rdf(SegmentTypeNode,rdf:value,SegmentTypeValue,Graph)
  ,SegmentType=val(SegmentTypeValue)) .

fixm_RunwayPositionAndTime(Graph, RunwayPositionAndTime, RunwayName, RunwayTime) :-
  rdf(RunwayPositionAndTime,rdf:type,fixm:'RunwayPositionAndTime',Graph)
  ,((RunwayName='$null$',\+ rdf(RunwayPositionAndTime,fixm:'runwayName',_RunwayName,Graph))
  ,(rdf(RunwayName,fixm:'runwayName',RunwayNameNode,Graph))
  ,rdf(RunwayNameNode,rdf:value,RunwayNameValue,Graph)
  ,RunwayName=val(RunwayNameValue))
  ,((RunwayTime='$null$',\+ rdf(RunwayPositionAndTime,fixm:'runwayTime',_RunwayTime,Graph))
  ,(rdf(RunwayTime,fixm:'runwayTime',RunwayTimeNode,Graph))
  ,rdf(RunwayTimeNode,rdf:value,RunwayTimeValue,Graph)
  ,RunwayTime=val(RunwayTimeValue)) .

fixm_Feature(Graph, Feature, Provenance) :-
  subClassOf(T,fixm:'Feature')
  ,rdf(Feature,rdf:type,T,Graph)
  ,((Provenance='$null$',\+ rdf(Feature,fixm:'provenance',_Provenance,Graph))
  ,(rdf(Provenance,fixm:'provenance',ProvenanceNode,Graph))
  ,rdf(ProvenanceNode,rdf:value,ProvenanceValue,Graph)
  ,Provenance=val(ProvenanceValue)) .

fixm_FlightIdentification(Graph, FlightIdentification, AircraftIdentification, MajorCarrierIdentifier, MarketingCarrierFlightIdentifier) :-
  rdf(FlightIdentification,rdf:type,fixm:'FlightIdentification',Graph)
  ,((AircraftIdentification='$null$',\+ rdf(FlightIdentification,fixm:'aircraftIdentification',_AircraftIdentification,Graph))
  ,(rdf(AircraftIdentification,fixm:'aircraftIdentification',AircraftIdentificationNode,Graph))
  ,rdf(AircraftIdentificationNode,rdf:value,AircraftIdentificationValue,Graph)
  ,AircraftIdentification=val(AircraftIdentificationValue))
  ,((MajorCarrierIdentifier='$null$',\+ rdf(FlightIdentification,fixm:'majorCarrierIdentifier',_MajorCarrierIdentifier,Graph))
  ,(rdf(MajorCarrierIdentifier,fixm:'majorCarrierIdentifier',MajorCarrierIdentifierNode,Graph))
  ,rdf(MajorCarrierIdentifierNode,rdf:value,MajorCarrierIdentifierValue,Graph)
  ,MajorCarrierIdentifier=val(MajorCarrierIdentifierValue))
 .

fixm_LastContact(Graph, LastContact, ContactFrequency, LastContactTime, LastContactUnit, Position) :-
  rdf(LastContact,rdf:type,fixm:'LastContact',Graph)
  ,((ContactFrequency='$null$',\+ rdf(LastContact,fixm:'contactFrequency',_ContactFrequency,Graph))
  ,(rdf(ContactFrequency,fixm:'contactFrequency',ContactFrequencyNode,Graph))
  ,rdf(ContactFrequencyNode,rdf:value,ContactFrequencyValue,Graph)
  ,ContactFrequency=val(ContactFrequencyValue))
  ,((LastContactTime='$null$',\+ rdf(LastContact,fixm:'lastContactTime',_LastContactTime,Graph))
  ,(rdf(LastContactTime,fixm:'lastContactTime',LastContactTimeNode,Graph))
  ,rdf(LastContactTimeNode,rdf:value,LastContactTimeValue,Graph)
  ,LastContactTime=val(LastContactTimeValue))
  ,((LastContactUnit='$null$',\+ rdf(LastContact,fixm:'lastContactUnit',_LastContactUnit,Graph))
  ,(rdf(LastContactUnit,fixm:'lastContactUnit',LastContactUnitNode,Graph))
  ,rdf(LastContactUnitNode,rdf:value,LastContactUnitValue,Graph)
  ,LastContactUnit=val(LastContactUnitValue))
  ,((Position='$null$',\+ rdf(LastContact,fixm:'position',_Position,Graph))
  ,(rdf(Position,fixm:'position',PositionNode,Graph))
  ,rdf(PositionNode,rdf:value,PositionValue,Graph)
  ,Position=val(PositionValue)) .

fixm_ElapsedTimeLocation(Graph, ElapsedTimeLocation) :-
  rdf(ElapsedTimeLocation,rdf:type,fixm:'ElapsedTimeLocation',Graph) .

aixm_Surface(Graph, Surface, HorizontalAccuracy, Annotation) :-
  subClassOf(T,aixm:'Surface')
  ,rdf(Surface,rdf:type,T,Graph)
  ,((HorizontalAccuracy='$null$',\+ rdf(Surface,aixm:'horizontalAccuracy',_HorizontalAccuracy,Graph))
  ,(rdf(HorizontalAccuracy,aixm:'horizontalAccuracy',HorizontalAccuracyNode,Graph))
  ,rdf(HorizontalAccuracyNode,rdf:value,HorizontalAccuracyValue,Graph)
  ,HorizontalAccuracy=val(HorizontalAccuracyValue))
 .

gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition) :-
  rdf(TimePeriod,rdf:type,gml:'TimePeriod',Graph)
  ,rdf(TimePeriod,gml:'beginPosition',BeginPosition,Graph)
  ,rdf(TimePeriod,gml:'endPosition',EndPosition,Graph) .

fixm_AircraftCapabilities(Graph, AircraftCapabilities, Survival, Communication, Navigation, Surveillance, StandardCapabilities) :-
  rdf(AircraftCapabilities,rdf:type,fixm:'AircraftCapabilities',Graph)
  ,((Survival='$null$',\+ rdf(AircraftCapabilities,fixm:'survival',_Survival,Graph))
  ,(rdf(Survival,fixm:'survival',SurvivalNode,Graph))
  ,rdf(SurvivalNode,rdf:value,SurvivalValue,Graph)
  ,Survival=val(SurvivalValue))
  ,((Communication='$null$',\+ rdf(AircraftCapabilities,fixm:'communication',_Communication,Graph))
  ,(rdf(Communication,fixm:'communication',CommunicationNode,Graph))
  ,rdf(CommunicationNode,rdf:value,CommunicationValue,Graph)
  ,Communication=val(CommunicationValue))
  ,((Navigation='$null$',\+ rdf(AircraftCapabilities,fixm:'navigation',_Navigation,Graph))
  ,(rdf(Navigation,fixm:'navigation',NavigationNode,Graph))
  ,rdf(NavigationNode,rdf:value,NavigationValue,Graph)
  ,Navigation=val(NavigationValue))
  ,((Surveillance='$null$',\+ rdf(AircraftCapabilities,fixm:'surveillance',_Surveillance,Graph))
  ,(rdf(Surveillance,fixm:'surveillance',SurveillanceNode,Graph))
  ,rdf(SurveillanceNode,rdf:value,SurveillanceValue,Graph)
  ,Surveillance=val(SurveillanceValue))
  ,((StandardCapabilities='$null$',\+ rdf(AircraftCapabilities,fixm:'standardCapabilities',_StandardCapabilities,Graph))
  ,(rdf(StandardCapabilities,fixm:'standardCapabilities',StandardCapabilitiesNode,Graph))
  ,rdf(StandardCapabilitiesNode,rdf:value,StandardCapabilitiesValue,Graph)
  ,StandardCapabilities=val(StandardCapabilitiesValue)) .

fixm_SpeedSchedule(Graph, SpeedSchedule, InitialSpeed, SubsequentSpeed) :-
  rdf(SpeedSchedule,rdf:type,fixm:'SpeedSchedule',Graph)
  ,((InitialSpeed='$null$',\+ rdf(SpeedSchedule,fixm:'initialSpeed',_InitialSpeed,Graph))
  ,(rdf(InitialSpeed,fixm:'initialSpeed',InitialSpeedNode,Graph))
  ,rdf(InitialSpeedNode,rdf:value,InitialSpeedValue,Graph)
  ,InitialSpeed=val(InitialSpeedValue))
  ,((SubsequentSpeed='$null$',\+ rdf(SpeedSchedule,fixm:'subsequentSpeed',_SubsequentSpeed,Graph))
  ,(rdf(SubsequentSpeed,fixm:'subsequentSpeed',SubsequentSpeedNode,Graph))
  ,rdf(SubsequentSpeedNode,rdf:value,SubsequentSpeedValue,Graph)
  ,SubsequentSpeed=val(SubsequentSpeedValue)) .

aixm_OrganisationAuthorityTimeSlice(Graph, OrganisationAuthorityTimeSlice, Name, Designator, Type, Military, Annotation, Contact, RelatedOrganisationAuthority) :-
  rdf(OrganisationAuthorityTimeSlice,rdf:type,aixm:'OrganisationAuthorityTimeSlice',Graph)
  ,((Name='$null$',\+ rdf(OrganisationAuthorityTimeSlice,aixm:'name',_Name,Graph))
  ,(rdf(Name,aixm:'name',NameNode,Graph))
  ,rdf(NameNode,rdf:value,NameValue,Graph)
  ,Name=val(NameValue))
  ,((Designator='$null$',\+ rdf(OrganisationAuthorityTimeSlice,aixm:'designator',_Designator,Graph))
  ,(rdf(Designator,aixm:'designator',DesignatorNode,Graph))
  ,rdf(DesignatorNode,rdf:value,DesignatorValue,Graph)
  ,Designator=val(DesignatorValue))
  ,((Type='$null$',\+ rdf(OrganisationAuthorityTimeSlice,aixm:'type',_Type,Graph))
  ,(rdf(Type,aixm:'type',TypeNode,Graph))
  ,rdf(TypeNode,rdf:value,TypeValue,Graph)
  ,Type=val(TypeValue))
  ,((Military='$null$',\+ rdf(OrganisationAuthorityTimeSlice,aixm:'military',_Military,Graph))
  ,(rdf(Military,aixm:'military',MilitaryNode,Graph))
  ,rdf(MilitaryNode,rdf:value,MilitaryValue,Graph)
  ,Military=val(MilitaryValue))


 .

fixm_EnRoute(Graph, EnRoute, AlternateAerodrome, FleetPrioritization, BoundaryCrossings, CpdlcConnection, BeaconCodeAssignment, Cleared, ControlElement, Pointout, Position) :-
  rdf(EnRoute,rdf:type,fixm:'EnRoute',Graph)

  ,((FleetPrioritization='$null$',\+ rdf(EnRoute,fixm:'fleetPrioritization',_FleetPrioritization,Graph))
  ,(rdf(FleetPrioritization,fixm:'fleetPrioritization',FleetPrioritizationNode,Graph))
  ,rdf(FleetPrioritizationNode,rdf:value,FleetPrioritizationValue,Graph)
  ,FleetPrioritization=val(FleetPrioritizationValue))

  ,((CpdlcConnection='$null$',\+ rdf(EnRoute,fixm:'cpdlcConnection',_CpdlcConnection,Graph))
  ,(rdf(CpdlcConnection,fixm:'cpdlcConnection',CpdlcConnectionNode,Graph))
  ,rdf(CpdlcConnectionNode,rdf:value,CpdlcConnectionValue,Graph)
  ,CpdlcConnection=val(CpdlcConnectionValue))
  ,((BeaconCodeAssignment='$null$',\+ rdf(EnRoute,fixm:'beaconCodeAssignment',_BeaconCodeAssignment,Graph))
  ,(rdf(BeaconCodeAssignment,fixm:'beaconCodeAssignment',BeaconCodeAssignmentNode,Graph))
  ,rdf(BeaconCodeAssignmentNode,rdf:value,BeaconCodeAssignmentValue,Graph)
  ,BeaconCodeAssignment=val(BeaconCodeAssignmentValue))
  ,((Cleared='$null$',\+ rdf(EnRoute,fixm:'cleared',_Cleared,Graph))
  ,(rdf(Cleared,fixm:'cleared',ClearedNode,Graph))
  ,rdf(ClearedNode,rdf:value,ClearedValue,Graph)
  ,Cleared=val(ClearedValue))

  ,((Pointout='$null$',\+ rdf(EnRoute,fixm:'pointout',_Pointout,Graph))
  ,(rdf(Pointout,fixm:'pointout',PointoutNode,Graph))
  ,rdf(PointoutNode,rdf:value,PointoutValue,Graph)
  ,Pointout=val(PointoutValue))
  ,((Position='$null$',\+ rdf(EnRoute,fixm:'position',_Position,Graph))
  ,(rdf(Position,fixm:'position',PositionNode,Graph))
  ,rdf(PositionNode,rdf:value,PositionValue,Graph)
  ,Position=val(PositionValue)) .

fixm_FlightLevel(Graph, FlightLevel, Level, Unit) :-
  rdf(FlightLevel,rdf:type,fixm:'FlightLevel',Graph)
  ,((Level='$null$',\+ rdf(FlightLevel,fixm:'level',_Level,Graph))
  ,(rdf(Level,fixm:'level',LevelNode,Graph))
  ,rdf(LevelNode,rdf:value,LevelValue,Graph)
  ,Level=val(LevelValue))
  ,((Unit='$null$',\+ rdf(FlightLevel,fixm:'unit',_Unit,Graph))
  ,(rdf(Unit,fixm:'unit',UnitNode,Graph))
  ,rdf(UnitNode,rdf:value,UnitValue,Graph)
  ,Unit=val(UnitValue)) .

fixm_LateralOfftrack(Graph, LateralOfftrack, OfftrackDistanceList, OfftrackReason) :-
  rdf(LateralOfftrack,rdf:type,fixm:'LateralOfftrack',Graph)

  ,((OfftrackReason='$null$',\+ rdf(LateralOfftrack,fixm:'offtrackReason',_OfftrackReason,Graph))
  ,(rdf(OfftrackReason,fixm:'offtrackReason',OfftrackReasonNode,Graph))
  ,rdf(OfftrackReasonNode,rdf:value,OfftrackReasonValue,Graph)
  ,OfftrackReason=val(OfftrackReasonValue)) .

fixm_TemporalRange(Graph, TemporalRange, Earliest, Latest) :-
  rdf(TemporalRange,rdf:type,fixm:'TemporalRange',Graph)
  ,((Earliest='$null$',\+ rdf(TemporalRange,fixm:'earliest',_Earliest,Graph))
  ,(rdf(Earliest,fixm:'earliest',EarliestNode,Graph))
  ,rdf(EarliestNode,rdf:value,EarliestValue,Graph)
  ,Earliest=val(EarliestValue))
  ,((Latest='$null$',\+ rdf(TemporalRange,fixm:'latest',_Latest,Graph))
  ,(rdf(Latest,fixm:'latest',LatestNode,Graph))
  ,rdf(LatestNode,rdf:value,LatestValue,Graph)
  ,Latest=val(LatestValue)) .

fixm_Aircraft(Graph, Aircraft, AircraftColours, AircraftQuantity, EngineType, AircraftAddress, Capabilities, Registration, AircraftType, WakeTurbulence, AircraftPerformance) :-
  rdf(Aircraft,rdf:type,fixm:'Aircraft',Graph)
  ,((AircraftColours='$null$',\+ rdf(Aircraft,fixm:'aircraftColours',_AircraftColours,Graph))
  ,(rdf(AircraftColours,fixm:'aircraftColours',AircraftColoursNode,Graph))
  ,rdf(AircraftColoursNode,rdf:value,AircraftColoursValue,Graph)
  ,AircraftColours=val(AircraftColoursValue))
  ,((AircraftQuantity='$null$',\+ rdf(Aircraft,fixm:'aircraftQuantity',_AircraftQuantity,Graph))
  ,(rdf(AircraftQuantity,fixm:'aircraftQuantity',AircraftQuantityNode,Graph))
  ,rdf(AircraftQuantityNode,rdf:value,AircraftQuantityValue,Graph)
  ,AircraftQuantity=val(AircraftQuantityValue))
  ,((EngineType='$null$',\+ rdf(Aircraft,fixm:'engineType',_EngineType,Graph))
  ,(rdf(EngineType,fixm:'engineType',EngineTypeNode,Graph))
  ,rdf(EngineTypeNode,rdf:value,EngineTypeValue,Graph)
  ,EngineType=val(EngineTypeValue))
  ,((AircraftAddress='$null$',\+ rdf(Aircraft,fixm:'aircraftAddress',_AircraftAddress,Graph))
  ,(rdf(AircraftAddress,fixm:'aircraftAddress',AircraftAddressNode,Graph))
  ,rdf(AircraftAddressNode,rdf:value,AircraftAddressValue,Graph)
  ,AircraftAddress=val(AircraftAddressValue))
  ,((Capabilities='$null$',\+ rdf(Aircraft,fixm:'capabilities',_Capabilities,Graph))
  ,(rdf(Capabilities,fixm:'capabilities',CapabilitiesNode,Graph))
  ,rdf(CapabilitiesNode,rdf:value,CapabilitiesValue,Graph)
  ,Capabilities=val(CapabilitiesValue))
  ,((Registration='$null$',\+ rdf(Aircraft,fixm:'registration',_Registration,Graph))
  ,(rdf(Registration,fixm:'registration',RegistrationNode,Graph))
  ,rdf(RegistrationNode,rdf:value,RegistrationValue,Graph)
  ,Registration=val(RegistrationValue))
  ,((AircraftType='$null$',\+ rdf(Aircraft,fixm:'aircraftType',_AircraftType,Graph))
  ,(rdf(AircraftType,fixm:'aircraftType',AircraftTypeNode,Graph))
  ,rdf(AircraftTypeNode,rdf:value,AircraftTypeValue,Graph)
  ,AircraftType=val(AircraftTypeValue))
  ,((WakeTurbulence='$null$',\+ rdf(Aircraft,fixm:'wakeTurbulence',_WakeTurbulence,Graph))
  ,(rdf(WakeTurbulence,fixm:'wakeTurbulence',WakeTurbulenceNode,Graph))
  ,rdf(WakeTurbulenceNode,rdf:value,WakeTurbulenceValue,Graph)
  ,WakeTurbulence=val(WakeTurbulenceValue))
  ,((AircraftPerformance='$null$',\+ rdf(Aircraft,fixm:'aircraftPerformance',_AircraftPerformance,Graph))
  ,(rdf(AircraftPerformance,fixm:'aircraftPerformance',AircraftPerformanceNode,Graph))
  ,rdf(AircraftPerformanceNode,rdf:value,AircraftPerformanceValue,Graph)
  ,AircraftPerformance=val(AircraftPerformanceValue)) .

fixm_OnlineContact(Graph, OnlineContact, Email) :-
  rdf(OnlineContact,rdf:type,fixm:'OnlineContact',Graph)
  ,((Email='$null$',\+ rdf(OnlineContact,fixm:'email',_Email,Graph))
  ,(rdf(Email,fixm:'email',EmailNode,Graph))
  ,rdf(EmailNode,rdf:value,EmailValue,Graph)
  ,Email=val(EmailValue)) .

fixm_AirspaceConstraint(Graph, AirspaceConstraint, AirspaceControlledEntryTime, ConstrainedAirspace) :-
  rdf(AirspaceConstraint,rdf:type,fixm:'AirspaceConstraint',Graph)
  ,((AirspaceControlledEntryTime='$null$',\+ rdf(AirspaceConstraint,fixm:'airspaceControlledEntryTime',_AirspaceControlledEntryTime,Graph))
  ,(rdf(AirspaceControlledEntryTime,fixm:'airspaceControlledEntryTime',AirspaceControlledEntryTimeNode,Graph))
  ,rdf(AirspaceControlledEntryTimeNode,rdf:value,AirspaceControlledEntryTimeValue,Graph)
  ,AirspaceControlledEntryTime=val(AirspaceControlledEntryTimeValue))
  ,((ConstrainedAirspace='$null$',\+ rdf(AirspaceConstraint,fixm:'constrainedAirspace',_ConstrainedAirspace,Graph))
  ,(rdf(ConstrainedAirspace,fixm:'constrainedAirspace',ConstrainedAirspaceNode,Graph))
  ,rdf(ConstrainedAirspaceNode,rdf:value,ConstrainedAirspaceValue,Graph)
  ,ConstrainedAirspace=val(ConstrainedAirspaceValue)) .

fixm_TimeSequence(Graph, TimeSequence, Approval, Begin, End, Ready, Request) :-
  rdf(TimeSequence,rdf:type,fixm:'TimeSequence',Graph)
  ,((Approval='$null$',\+ rdf(TimeSequence,fixm:'approval',_Approval,Graph))
  ,(rdf(Approval,fixm:'approval',ApprovalNode,Graph))
  ,rdf(ApprovalNode,rdf:value,ApprovalValue,Graph)
  ,Approval=val(ApprovalValue))
  ,((Begin='$null$',\+ rdf(TimeSequence,fixm:'begin',_Begin,Graph))
  ,(rdf(Begin,fixm:'begin',BeginNode,Graph))
  ,rdf(BeginNode,rdf:value,BeginValue,Graph)
  ,Begin=val(BeginValue))
  ,((End='$null$',\+ rdf(TimeSequence,fixm:'end',_End,Graph))
  ,(rdf(End,fixm:'end',EndNode,Graph))
  ,rdf(EndNode,rdf:value,EndValue,Graph)
  ,End=val(EndValue))
  ,((Ready='$null$',\+ rdf(TimeSequence,fixm:'ready',_Ready,Graph))
  ,(rdf(Ready,fixm:'ready',ReadyNode,Graph))
  ,rdf(ReadyNode,rdf:value,ReadyValue,Graph)
  ,Ready=val(ReadyValue))
  ,((Request='$null$',\+ rdf(TimeSequence,fixm:'request',_Request,Graph))
  ,(rdf(Request,fixm:'request',RequestNode,Graph))
  ,rdf(RequestNode,rdf:value,RequestValue,Graph)
  ,Request=val(RequestValue)) .

fixm_AdditionalHandlingInformation(Graph, AdditionalHandlingInformation, ResponsibleAgent) :-
  rdf(AdditionalHandlingInformation,rdf:type,fixm:'AdditionalHandlingInformation',Graph)
  ,((ResponsibleAgent='$null$',\+ rdf(AdditionalHandlingInformation,fixm:'responsibleAgent',_ResponsibleAgent,Graph))
  ,(rdf(ResponsibleAgent,fixm:'responsibleAgent',ResponsibleAgentNode,Graph))
  ,rdf(ResponsibleAgentNode,rdf:value,ResponsibleAgentValue,Graph)
  ,ResponsibleAgent=val(ResponsibleAgentValue)) .

fixm_AtcUnitReference(Graph, AtcUnitReference, SectorIdentifier, Delegated) :-
  subClassOf(T,fixm:'AtcUnitReference')
  ,rdf(AtcUnitReference,rdf:type,T,Graph)
  ,((SectorIdentifier='$null$',\+ rdf(AtcUnitReference,fixm:'sectorIdentifier',_SectorIdentifier,Graph))
  ,(rdf(SectorIdentifier,fixm:'sectorIdentifier',SectorIdentifierNode,Graph))
  ,rdf(SectorIdentifierNode,rdf:value,SectorIdentifierValue,Graph)
  ,SectorIdentifier=val(SectorIdentifierValue))
  ,((Delegated='$null$',\+ rdf(AtcUnitReference,fixm:'delegated',_Delegated,Graph))
  ,(rdf(Delegated,fixm:'delegated',DelegatedNode,Graph))
  ,rdf(DelegatedNode,rdf:value,DelegatedValue,Graph)
  ,Delegated=val(DelegatedValue)) .

fixm_Extension(Graph, Extension) :-
  rdf(Extension,rdf:type,fixm:'Extension',Graph) .

fixm_SurveillanceCapabilities(Graph, SurveillanceCapabilities, OtherSurveillanceCapabilities, SurveillanceCode) :-
  rdf(SurveillanceCapabilities,rdf:type,fixm:'SurveillanceCapabilities',Graph)
  ,((OtherSurveillanceCapabilities='$null$',\+ rdf(SurveillanceCapabilities,fixm:'otherSurveillanceCapabilities',_OtherSurveillanceCapabilities,Graph))
  ,(rdf(OtherSurveillanceCapabilities,fixm:'otherSurveillanceCapabilities',OtherSurveillanceCapabilitiesNode,Graph))
  ,rdf(OtherSurveillanceCapabilitiesNode,rdf:value,OtherSurveillanceCapabilitiesValue,Graph)
  ,OtherSurveillanceCapabilities=val(OtherSurveillanceCapabilitiesValue))
 .

fixm_Trajectory(Graph, Trajectory, TrajectoryPoint) :-
  rdf(Trajectory,rdf:type,fixm:'Trajectory',Graph)
 .

aixm_AltimeterSourceTimeSlice(Graph, AltimeterSourceTimeSlice, IsRemote, IsPrimary, Availability, Annotation) :-
  rdf(AltimeterSourceTimeSlice,rdf:type,aixm:'AltimeterSourceTimeSlice',Graph)
  ,((IsRemote='$null$',\+ rdf(AltimeterSourceTimeSlice,aixm:'isRemote',_IsRemote,Graph))
  ,(rdf(IsRemote,aixm:'isRemote',IsRemoteNode,Graph))
  ,rdf(IsRemoteNode,rdf:value,IsRemoteValue,Graph)
  ,IsRemote=val(IsRemoteValue))
  ,((IsPrimary='$null$',\+ rdf(AltimeterSourceTimeSlice,aixm:'isPrimary',_IsPrimary,Graph))
  ,(rdf(IsPrimary,aixm:'isPrimary',IsPrimaryNode,Graph))
  ,rdf(IsPrimaryNode,rdf:value,IsPrimaryValue,Graph)
  ,IsPrimary=val(IsPrimaryValue))

 .

aixm_Point(Graph, Point, HorizontalAccuracy, Annotation) :-
  subClassOf(T,aixm:'Point')
  ,rdf(Point,rdf:type,T,Graph)
  ,((HorizontalAccuracy='$null$',\+ rdf(Point,aixm:'horizontalAccuracy',_HorizontalAccuracy,Graph))
  ,(rdf(HorizontalAccuracy,aixm:'horizontalAccuracy',HorizontalAccuracyNode,Graph))
  ,rdf(HorizontalAccuracyNode,rdf:value,HorizontalAccuracyValue,Graph)
  ,HorizontalAccuracy=val(HorizontalAccuracyValue))
 .

aixm_AircraftCharacteristic(Graph, AircraftCharacteristic, Type, Engine, NumberEngine, TypeAircraftICAO, AircraftLandingCategory, WingSpan, WingSpanInterpretation, ClassWingSpan, Weight, WeightInterpretation, Passengers, PassengersInterpretation, Speed, SpeedInterpretation, WakeTurbulence, NavigationEquipment, NavigationSpecification, VerticalSeparationCapability, AntiCollisionAndSeparationEquipment, CommunicationEquipment, SurveillanceEquipment, Annotation) :-
  rdf(AircraftCharacteristic,rdf:type,aixm:'AircraftCharacteristic',Graph)
  ,((Type='$null$',\+ rdf(AircraftCharacteristic,aixm:'type',_Type,Graph))
  ,(rdf(Type,aixm:'type',TypeNode,Graph))
  ,rdf(TypeNode,rdf:value,TypeValue,Graph)
  ,Type=val(TypeValue))
  ,((Engine='$null$',\+ rdf(AircraftCharacteristic,aixm:'engine',_Engine,Graph))
  ,(rdf(Engine,aixm:'engine',EngineNode,Graph))
  ,rdf(EngineNode,rdf:value,EngineValue,Graph)
  ,Engine=val(EngineValue))
  ,((NumberEngine='$null$',\+ rdf(AircraftCharacteristic,aixm:'numberEngine',_NumberEngine,Graph))
  ,(rdf(NumberEngine,aixm:'numberEngine',NumberEngineNode,Graph))
  ,rdf(NumberEngineNode,rdf:value,NumberEngineValue,Graph)
  ,NumberEngine=val(NumberEngineValue))
  ,((TypeAircraftICAO='$null$',\+ rdf(AircraftCharacteristic,aixm:'typeAircraftICAO',_TypeAircraftICAO,Graph))
  ,(rdf(TypeAircraftICAO,aixm:'typeAircraftICAO',TypeAircraftICAONode,Graph))
  ,rdf(TypeAircraftICAONode,rdf:value,TypeAircraftICAOValue,Graph)
  ,TypeAircraftICAO=val(TypeAircraftICAOValue))
  ,((AircraftLandingCategory='$null$',\+ rdf(AircraftCharacteristic,aixm:'aircraftLandingCategory',_AircraftLandingCategory,Graph))
  ,(rdf(AircraftLandingCategory,aixm:'aircraftLandingCategory',AircraftLandingCategoryNode,Graph))
  ,rdf(AircraftLandingCategoryNode,rdf:value,AircraftLandingCategoryValue,Graph)
  ,AircraftLandingCategory=val(AircraftLandingCategoryValue))
  ,((WingSpan='$null$',\+ rdf(AircraftCharacteristic,aixm:'wingSpan',_WingSpan,Graph))
  ,(rdf(WingSpan,aixm:'wingSpan',WingSpanNode,Graph))
  ,rdf(WingSpanNode,rdf:value,WingSpanValue,Graph)
  ,WingSpan=val(WingSpanValue))
  ,((WingSpanInterpretation='$null$',\+ rdf(AircraftCharacteristic,aixm:'wingSpanInterpretation',_WingSpanInterpretation,Graph))
  ,(rdf(WingSpanInterpretation,aixm:'wingSpanInterpretation',WingSpanInterpretationNode,Graph))
  ,rdf(WingSpanInterpretationNode,rdf:value,WingSpanInterpretationValue,Graph)
  ,WingSpanInterpretation=val(WingSpanInterpretationValue))
  ,((ClassWingSpan='$null$',\+ rdf(AircraftCharacteristic,aixm:'classWingSpan',_ClassWingSpan,Graph))
  ,(rdf(ClassWingSpan,aixm:'classWingSpan',ClassWingSpanNode,Graph))
  ,rdf(ClassWingSpanNode,rdf:value,ClassWingSpanValue,Graph)
  ,ClassWingSpan=val(ClassWingSpanValue))
  ,((Weight='$null$',\+ rdf(AircraftCharacteristic,aixm:'weight',_Weight,Graph))
  ,(rdf(Weight,aixm:'weight',WeightNode,Graph))
  ,rdf(WeightNode,rdf:value,WeightValue,Graph)
  ,Weight=val(WeightValue))
  ,((WeightInterpretation='$null$',\+ rdf(AircraftCharacteristic,aixm:'weightInterpretation',_WeightInterpretation,Graph))
  ,(rdf(WeightInterpretation,aixm:'weightInterpretation',WeightInterpretationNode,Graph))
  ,rdf(WeightInterpretationNode,rdf:value,WeightInterpretationValue,Graph)
  ,WeightInterpretation=val(WeightInterpretationValue))
  ,((Passengers='$null$',\+ rdf(AircraftCharacteristic,aixm:'passengers',_Passengers,Graph))
  ,(rdf(Passengers,aixm:'passengers',PassengersNode,Graph))
  ,rdf(PassengersNode,rdf:value,PassengersValue,Graph)
  ,Passengers=val(PassengersValue))
  ,((PassengersInterpretation='$null$',\+ rdf(AircraftCharacteristic,aixm:'passengersInterpretation',_PassengersInterpretation,Graph))
  ,(rdf(PassengersInterpretation,aixm:'passengersInterpretation',PassengersInterpretationNode,Graph))
  ,rdf(PassengersInterpretationNode,rdf:value,PassengersInterpretationValue,Graph)
  ,PassengersInterpretation=val(PassengersInterpretationValue))
  ,((Speed='$null$',\+ rdf(AircraftCharacteristic,aixm:'speed',_Speed,Graph))
  ,(rdf(Speed,aixm:'speed',SpeedNode,Graph))
  ,rdf(SpeedNode,rdf:value,SpeedValue,Graph)
  ,Speed=val(SpeedValue))
  ,((SpeedInterpretation='$null$',\+ rdf(AircraftCharacteristic,aixm:'speedInterpretation',_SpeedInterpretation,Graph))
  ,(rdf(SpeedInterpretation,aixm:'speedInterpretation',SpeedInterpretationNode,Graph))
  ,rdf(SpeedInterpretationNode,rdf:value,SpeedInterpretationValue,Graph)
  ,SpeedInterpretation=val(SpeedInterpretationValue))
  ,((WakeTurbulence='$null$',\+ rdf(AircraftCharacteristic,aixm:'wakeTurbulence',_WakeTurbulence,Graph))
  ,(rdf(WakeTurbulence,aixm:'wakeTurbulence',WakeTurbulenceNode,Graph))
  ,rdf(WakeTurbulenceNode,rdf:value,WakeTurbulenceValue,Graph)
  ,WakeTurbulence=val(WakeTurbulenceValue))
  ,((NavigationEquipment='$null$',\+ rdf(AircraftCharacteristic,aixm:'navigationEquipment',_NavigationEquipment,Graph))
  ,(rdf(NavigationEquipment,aixm:'navigationEquipment',NavigationEquipmentNode,Graph))
  ,rdf(NavigationEquipmentNode,rdf:value,NavigationEquipmentValue,Graph)
  ,NavigationEquipment=val(NavigationEquipmentValue))
  ,((NavigationSpecification='$null$',\+ rdf(AircraftCharacteristic,aixm:'navigationSpecification',_NavigationSpecification,Graph))
  ,(rdf(NavigationSpecification,aixm:'navigationSpecification',NavigationSpecificationNode,Graph))
  ,rdf(NavigationSpecificationNode,rdf:value,NavigationSpecificationValue,Graph)
  ,NavigationSpecification=val(NavigationSpecificationValue))
  ,((VerticalSeparationCapability='$null$',\+ rdf(AircraftCharacteristic,aixm:'verticalSeparationCapability',_VerticalSeparationCapability,Graph))
  ,(rdf(VerticalSeparationCapability,aixm:'verticalSeparationCapability',VerticalSeparationCapabilityNode,Graph))
  ,rdf(VerticalSeparationCapabilityNode,rdf:value,VerticalSeparationCapabilityValue,Graph)
  ,VerticalSeparationCapability=val(VerticalSeparationCapabilityValue))
  ,((AntiCollisionAndSeparationEquipment='$null$',\+ rdf(AircraftCharacteristic,aixm:'antiCollisionAndSeparationEquipment',_AntiCollisionAndSeparationEquipment,Graph))
  ,(rdf(AntiCollisionAndSeparationEquipment,aixm:'antiCollisionAndSeparationEquipment',AntiCollisionAndSeparationEquipmentNode,Graph))
  ,rdf(AntiCollisionAndSeparationEquipmentNode,rdf:value,AntiCollisionAndSeparationEquipmentValue,Graph)
  ,AntiCollisionAndSeparationEquipment=val(AntiCollisionAndSeparationEquipmentValue))
  ,((CommunicationEquipment='$null$',\+ rdf(AircraftCharacteristic,aixm:'communicationEquipment',_CommunicationEquipment,Graph))
  ,(rdf(CommunicationEquipment,aixm:'communicationEquipment',CommunicationEquipmentNode,Graph))
  ,rdf(CommunicationEquipmentNode,rdf:value,CommunicationEquipmentValue,Graph)
  ,CommunicationEquipment=val(CommunicationEquipmentValue))
  ,((SurveillanceEquipment='$null$',\+ rdf(AircraftCharacteristic,aixm:'surveillanceEquipment',_SurveillanceEquipment,Graph))
  ,(rdf(SurveillanceEquipment,aixm:'surveillanceEquipment',SurveillanceEquipmentNode,Graph))
  ,rdf(SurveillanceEquipmentNode,rdf:value,SurveillanceEquipmentValue,Graph)
  ,SurveillanceEquipment=val(SurveillanceEquipmentValue))
 .

aixm_PostalAddress(Graph, PostalAddress, DeliveryPoint, City, AdministrativeArea, PostalCode, Country) :-
  rdf(PostalAddress,rdf:type,aixm:'PostalAddress',Graph)
  ,((DeliveryPoint='$null$',\+ rdf(PostalAddress,aixm:'deliveryPoint',_DeliveryPoint,Graph))
  ,(rdf(DeliveryPoint,aixm:'deliveryPoint',DeliveryPointNode,Graph))
  ,rdf(DeliveryPointNode,rdf:value,DeliveryPointValue,Graph)
  ,DeliveryPoint=val(DeliveryPointValue))
  ,((City='$null$',\+ rdf(PostalAddress,aixm:'city',_City,Graph))
  ,(rdf(City,aixm:'city',CityNode,Graph))
  ,rdf(CityNode,rdf:value,CityValue,Graph)
  ,City=val(CityValue))
  ,((AdministrativeArea='$null$',\+ rdf(PostalAddress,aixm:'administrativeArea',_AdministrativeArea,Graph))
  ,(rdf(AdministrativeArea,aixm:'administrativeArea',AdministrativeAreaNode,Graph))
  ,rdf(AdministrativeAreaNode,rdf:value,AdministrativeAreaValue,Graph)
  ,AdministrativeArea=val(AdministrativeAreaValue))
  ,((PostalCode='$null$',\+ rdf(PostalAddress,aixm:'postalCode',_PostalCode,Graph))
  ,(rdf(PostalCode,aixm:'postalCode',PostalCodeNode,Graph))
  ,rdf(PostalCodeNode,rdf:value,PostalCodeValue,Graph)
  ,PostalCode=val(PostalCodeValue))
  ,((Country='$null$',\+ rdf(PostalAddress,aixm:'country',_Country,Graph))
  ,(rdf(Country,aixm:'country',CountryNode,Graph))
  ,rdf(CountryNode,rdf:value,CountryValue,Graph)
  ,Country=val(CountryValue)) .

fixm_DangerousGoodsPackage(Graph, DangerousGoodsPackage, DangerousGoodsQuantity, PackageDimensions, PackingInstructionNumber, ProductName, ProperShippingName, ReportableQuantity, SupplementaryInformation, TechnicalName, TypeOfPackaging, UnNumber, DangerousGoodsLimitation, ShipmentType, AllPackedInOne, CompatibilityGroup, ShipmentDimensions, MarinePollutantIndicator, RadioactiveMaterials, HazardClass, PackingGroup, Temperatures, OverpackIndicator, SubsidiaryHazardClassList) :-
  rdf(DangerousGoodsPackage,rdf:type,fixm:'DangerousGoodsPackage',Graph)
  ,((DangerousGoodsQuantity='$null$',\+ rdf(DangerousGoodsPackage,fixm:'dangerousGoodsQuantity',_DangerousGoodsQuantity,Graph))
  ,(rdf(DangerousGoodsQuantity,fixm:'dangerousGoodsQuantity',DangerousGoodsQuantityNode,Graph))
  ,rdf(DangerousGoodsQuantityNode,rdf:value,DangerousGoodsQuantityValue,Graph)
  ,DangerousGoodsQuantity=val(DangerousGoodsQuantityValue))
  ,((PackageDimensions='$null$',\+ rdf(DangerousGoodsPackage,fixm:'packageDimensions',_PackageDimensions,Graph))
  ,(rdf(PackageDimensions,fixm:'packageDimensions',PackageDimensionsNode,Graph))
  ,rdf(PackageDimensionsNode,rdf:value,PackageDimensionsValue,Graph)
  ,PackageDimensions=val(PackageDimensionsValue))
  ,((PackingInstructionNumber='$null$',\+ rdf(DangerousGoodsPackage,fixm:'packingInstructionNumber',_PackingInstructionNumber,Graph))
  ,(rdf(PackingInstructionNumber,fixm:'packingInstructionNumber',PackingInstructionNumberNode,Graph))
  ,rdf(PackingInstructionNumberNode,rdf:value,PackingInstructionNumberValue,Graph)
  ,PackingInstructionNumber=val(PackingInstructionNumberValue))
  ,((ProductName='$null$',\+ rdf(DangerousGoodsPackage,fixm:'productName',_ProductName,Graph))
  ,(rdf(ProductName,fixm:'productName',ProductNameNode,Graph))
  ,rdf(ProductNameNode,rdf:value,ProductNameValue,Graph)
  ,ProductName=val(ProductNameValue))
  ,((ProperShippingName='$null$',\+ rdf(DangerousGoodsPackage,fixm:'properShippingName',_ProperShippingName,Graph))
  ,(rdf(ProperShippingName,fixm:'properShippingName',ProperShippingNameNode,Graph))
  ,rdf(ProperShippingNameNode,rdf:value,ProperShippingNameValue,Graph)
  ,ProperShippingName=val(ProperShippingNameValue))
  ,((ReportableQuantity='$null$',\+ rdf(DangerousGoodsPackage,fixm:'reportableQuantity',_ReportableQuantity,Graph))
  ,(rdf(ReportableQuantity,fixm:'reportableQuantity',ReportableQuantityNode,Graph))
  ,rdf(ReportableQuantityNode,rdf:value,ReportableQuantityValue,Graph)
  ,ReportableQuantity=val(ReportableQuantityValue))
  ,((SupplementaryInformation='$null$',\+ rdf(DangerousGoodsPackage,fixm:'supplementaryInformation',_SupplementaryInformation,Graph))
  ,(rdf(SupplementaryInformation,fixm:'supplementaryInformation',SupplementaryInformationNode,Graph))
  ,rdf(SupplementaryInformationNode,rdf:value,SupplementaryInformationValue,Graph)
  ,SupplementaryInformation=val(SupplementaryInformationValue))
  ,((TechnicalName='$null$',\+ rdf(DangerousGoodsPackage,fixm:'technicalName',_TechnicalName,Graph))
  ,(rdf(TechnicalName,fixm:'technicalName',TechnicalNameNode,Graph))
  ,rdf(TechnicalNameNode,rdf:value,TechnicalNameValue,Graph)
  ,TechnicalName=val(TechnicalNameValue))
  ,((TypeOfPackaging='$null$',\+ rdf(DangerousGoodsPackage,fixm:'typeOfPackaging',_TypeOfPackaging,Graph))
  ,(rdf(TypeOfPackaging,fixm:'typeOfPackaging',TypeOfPackagingNode,Graph))
  ,rdf(TypeOfPackagingNode,rdf:value,TypeOfPackagingValue,Graph)
  ,TypeOfPackaging=val(TypeOfPackagingValue))
  ,((UnNumber='$null$',\+ rdf(DangerousGoodsPackage,fixm:'unNumber',_UnNumber,Graph))
  ,(rdf(UnNumber,fixm:'unNumber',UnNumberNode,Graph))
  ,rdf(UnNumberNode,rdf:value,UnNumberValue,Graph)
  ,UnNumber=val(UnNumberValue))
  ,((DangerousGoodsLimitation='$null$',\+ rdf(DangerousGoodsPackage,fixm:'dangerousGoodsLimitation',_DangerousGoodsLimitation,Graph))
  ,(rdf(DangerousGoodsLimitation,fixm:'dangerousGoodsLimitation',DangerousGoodsLimitationNode,Graph))
  ,rdf(DangerousGoodsLimitationNode,rdf:value,DangerousGoodsLimitationValue,Graph)
  ,DangerousGoodsLimitation=val(DangerousGoodsLimitationValue))
  ,((ShipmentType='$null$',\+ rdf(DangerousGoodsPackage,fixm:'shipmentType',_ShipmentType,Graph))
  ,(rdf(ShipmentType,fixm:'shipmentType',ShipmentTypeNode,Graph))
  ,rdf(ShipmentTypeNode,rdf:value,ShipmentTypeValue,Graph)
  ,ShipmentType=val(ShipmentTypeValue))
  ,((AllPackedInOne='$null$',\+ rdf(DangerousGoodsPackage,fixm:'allPackedInOne',_AllPackedInOne,Graph))
  ,(rdf(AllPackedInOne,fixm:'allPackedInOne',AllPackedInOneNode,Graph))
  ,rdf(AllPackedInOneNode,rdf:value,AllPackedInOneValue,Graph)
  ,AllPackedInOne=val(AllPackedInOneValue))
  ,((CompatibilityGroup='$null$',\+ rdf(DangerousGoodsPackage,fixm:'compatibilityGroup',_CompatibilityGroup,Graph))
  ,(rdf(CompatibilityGroup,fixm:'compatibilityGroup',CompatibilityGroupNode,Graph))
  ,rdf(CompatibilityGroupNode,rdf:value,CompatibilityGroupValue,Graph)
  ,CompatibilityGroup=val(CompatibilityGroupValue))
  ,((ShipmentDimensions='$null$',\+ rdf(DangerousGoodsPackage,fixm:'shipmentDimensions',_ShipmentDimensions,Graph))
  ,(rdf(ShipmentDimensions,fixm:'shipmentDimensions',ShipmentDimensionsNode,Graph))
  ,rdf(ShipmentDimensionsNode,rdf:value,ShipmentDimensionsValue,Graph)
  ,ShipmentDimensions=val(ShipmentDimensionsValue))
  ,((MarinePollutantIndicator='$null$',\+ rdf(DangerousGoodsPackage,fixm:'marinePollutantIndicator',_MarinePollutantIndicator,Graph))
  ,(rdf(MarinePollutantIndicator,fixm:'marinePollutantIndicator',MarinePollutantIndicatorNode,Graph))
  ,rdf(MarinePollutantIndicatorNode,rdf:value,MarinePollutantIndicatorValue,Graph)
  ,MarinePollutantIndicator=val(MarinePollutantIndicatorValue))
  ,((RadioactiveMaterials='$null$',\+ rdf(DangerousGoodsPackage,fixm:'radioactiveMaterials',_RadioactiveMaterials,Graph))
  ,(rdf(RadioactiveMaterials,fixm:'radioactiveMaterials',RadioactiveMaterialsNode,Graph))
  ,rdf(RadioactiveMaterialsNode,rdf:value,RadioactiveMaterialsValue,Graph)
  ,RadioactiveMaterials=val(RadioactiveMaterialsValue))
  ,((HazardClass='$null$',\+ rdf(DangerousGoodsPackage,fixm:'hazardClass',_HazardClass,Graph))
  ,(rdf(HazardClass,fixm:'hazardClass',HazardClassNode,Graph))
  ,rdf(HazardClassNode,rdf:value,HazardClassValue,Graph)
  ,HazardClass=val(HazardClassValue))
  ,((PackingGroup='$null$',\+ rdf(DangerousGoodsPackage,fixm:'packingGroup',_PackingGroup,Graph))
  ,(rdf(PackingGroup,fixm:'packingGroup',PackingGroupNode,Graph))
  ,rdf(PackingGroupNode,rdf:value,PackingGroupValue,Graph)
  ,PackingGroup=val(PackingGroupValue))
  ,((Temperatures='$null$',\+ rdf(DangerousGoodsPackage,fixm:'temperatures',_Temperatures,Graph))
  ,(rdf(Temperatures,fixm:'temperatures',TemperaturesNode,Graph))
  ,rdf(TemperaturesNode,rdf:value,TemperaturesValue,Graph)
  ,Temperatures=val(TemperaturesValue))
  ,((OverpackIndicator='$null$',\+ rdf(DangerousGoodsPackage,fixm:'overpackIndicator',_OverpackIndicator,Graph))
  ,(rdf(OverpackIndicator,fixm:'overpackIndicator',OverpackIndicatorNode,Graph))
  ,rdf(OverpackIndicatorNode,rdf:value,OverpackIndicatorValue,Graph)
  ,OverpackIndicator=val(OverpackIndicatorValue))
 .

fixm_LastPositionReport(Graph, LastPositionReport, DeterminationMethod, Position, TimeAtPosition) :-
  rdf(LastPositionReport,rdf:type,fixm:'LastPositionReport',Graph)
  ,((DeterminationMethod='$null$',\+ rdf(LastPositionReport,fixm:'determinationMethod',_DeterminationMethod,Graph))
  ,(rdf(DeterminationMethod,fixm:'determinationMethod',DeterminationMethodNode,Graph))
  ,rdf(DeterminationMethodNode,rdf:value,DeterminationMethodValue,Graph)
  ,DeterminationMethod=val(DeterminationMethodValue))
  ,((Position='$null$',\+ rdf(LastPositionReport,fixm:'position',_Position,Graph))
  ,(rdf(Position,fixm:'position',PositionNode,Graph))
  ,rdf(PositionNode,rdf:value,PositionValue,Graph)
  ,Position=val(PositionValue))
  ,((TimeAtPosition='$null$',\+ rdf(LastPositionReport,fixm:'timeAtPosition',_TimeAtPosition,Graph))
  ,(rdf(TimeAtPosition,fixm:'timeAtPosition',TimeAtPositionNode,Graph))
  ,rdf(TimeAtPositionNode,rdf:value,TimeAtPositionValue,Graph)
  ,TimeAtPosition=val(TimeAtPositionValue)) .

aixm_AltimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus) :-
  rdf(AltimeterSourceStatus,rdf:type,aixm:'AltimeterSourceStatus',Graph)
  ,((OperationalStatus='$null$',\+ rdf(AltimeterSourceStatus,aixm:'operationalStatus',_OperationalStatus,Graph))
  ,(rdf(OperationalStatus,aixm:'operationalStatus',OperationalStatusNode,Graph))
  ,rdf(OperationalStatusNode,rdf:value,OperationalStatusValue,Graph)
  ,OperationalStatus=val(OperationalStatusValue)) .

fixm_DangerousGoodsDimensions(Graph, DangerousGoodsDimensions, GrossWeight, NetWeight, Volume) :-
  rdf(DangerousGoodsDimensions,rdf:type,fixm:'DangerousGoodsDimensions',Graph)
  ,((GrossWeight='$null$',\+ rdf(DangerousGoodsDimensions,fixm:'grossWeight',_GrossWeight,Graph))
  ,(rdf(GrossWeight,fixm:'grossWeight',GrossWeightNode,Graph))
  ,rdf(GrossWeightNode,rdf:value,GrossWeightValue,Graph)
  ,GrossWeight=val(GrossWeightValue))
  ,((NetWeight='$null$',\+ rdf(DangerousGoodsDimensions,fixm:'netWeight',_NetWeight,Graph))
  ,(rdf(NetWeight,fixm:'netWeight',NetWeightNode,Graph))
  ,rdf(NetWeightNode,rdf:value,NetWeightValue,Graph)
  ,NetWeight=val(NetWeightValue))
  ,((Volume='$null$',\+ rdf(DangerousGoodsDimensions,fixm:'volume',_Volume,Graph))
  ,(rdf(Volume,fixm:'volume',VolumeNode,Graph))
  ,rdf(VolumeNode,rdf:value,VolumeValue,Graph)
  ,Volume=val(VolumeValue)) .

fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules) :-
  rdf(EfplRoute,rdf:type,fixm:'EfplRoute',Graph)
  ,((EfplFlightRules='$null$',\+ rdf(EfplRoute,fixm:'efplFlightRules',_EfplFlightRules,Graph))
  ,(rdf(EfplFlightRules,fixm:'efplFlightRules',EfplFlightRulesNode,Graph))
  ,rdf(EfplFlightRulesNode,rdf:value,EfplFlightRulesValue,Graph)
  ,EfplFlightRules=val(EfplFlightRulesValue)) .

fixm_CoordinationStatus(Graph, CoordinationStatus, AbrogationReason, CoordinationStatus, NonStandardCommunicationReason, ReleaseConditions) :-
  rdf(CoordinationStatus,rdf:type,fixm:'CoordinationStatus',Graph)
  ,((AbrogationReason='$null$',\+ rdf(CoordinationStatus,fixm:'abrogationReason',_AbrogationReason,Graph))
  ,(rdf(AbrogationReason,fixm:'abrogationReason',AbrogationReasonNode,Graph))
  ,rdf(AbrogationReasonNode,rdf:value,AbrogationReasonValue,Graph)
  ,AbrogationReason=val(AbrogationReasonValue))
  ,((CoordinationStatus='$null$',\+ rdf(CoordinationStatus,fixm:'coordinationStatus',_CoordinationStatus,Graph))
  ,(rdf(CoordinationStatus,fixm:'coordinationStatus',CoordinationStatusNode,Graph))
  ,rdf(CoordinationStatusNode,rdf:value,CoordinationStatusValue,Graph)
  ,CoordinationStatus=val(CoordinationStatusValue))
  ,((NonStandardCommunicationReason='$null$',\+ rdf(CoordinationStatus,fixm:'nonStandardCommunicationReason',_NonStandardCommunicationReason,Graph))
  ,(rdf(NonStandardCommunicationReason,fixm:'nonStandardCommunicationReason',NonStandardCommunicationReasonNode,Graph))
  ,rdf(NonStandardCommunicationReasonNode,rdf:value,NonStandardCommunicationReasonValue,Graph)
  ,NonStandardCommunicationReason=val(NonStandardCommunicationReasonValue))
  ,((ReleaseConditions='$null$',\+ rdf(CoordinationStatus,fixm:'releaseConditions',_ReleaseConditions,Graph))
  ,(rdf(ReleaseConditions,fixm:'releaseConditions',ReleaseConditionsNode,Graph))
  ,rdf(ReleaseConditionsNode,rdf:value,ReleaseConditionsValue,Graph)
  ,ReleaseConditions=val(ReleaseConditionsValue)) .

fixm_BoundaryCrossing(Graph, BoundaryCrossing, Altitude, CrossingPoint, CrossingSpeed, CrossingTime, Offtrack, AltitudeInTransition) :-
  rdf(BoundaryCrossing,rdf:type,fixm:'BoundaryCrossing',Graph)
  ,((Altitude='$null$',\+ rdf(BoundaryCrossing,fixm:'altitude',_Altitude,Graph))
  ,(rdf(Altitude,fixm:'altitude',AltitudeNode,Graph))
  ,rdf(AltitudeNode,rdf:value,AltitudeValue,Graph)
  ,Altitude=val(AltitudeValue))
  ,((CrossingPoint='$null$',\+ rdf(BoundaryCrossing,fixm:'crossingPoint',_CrossingPoint,Graph))
  ,(rdf(CrossingPoint,fixm:'crossingPoint',CrossingPointNode,Graph))
  ,rdf(CrossingPointNode,rdf:value,CrossingPointValue,Graph)
  ,CrossingPoint=val(CrossingPointValue))
  ,((CrossingSpeed='$null$',\+ rdf(BoundaryCrossing,fixm:'crossingSpeed',_CrossingSpeed,Graph))
  ,(rdf(CrossingSpeed,fixm:'crossingSpeed',CrossingSpeedNode,Graph))
  ,rdf(CrossingSpeedNode,rdf:value,CrossingSpeedValue,Graph)
  ,CrossingSpeed=val(CrossingSpeedValue))
  ,((CrossingTime='$null$',\+ rdf(BoundaryCrossing,fixm:'crossingTime',_CrossingTime,Graph))
  ,(rdf(CrossingTime,fixm:'crossingTime',CrossingTimeNode,Graph))
  ,rdf(CrossingTimeNode,rdf:value,CrossingTimeValue,Graph)
  ,CrossingTime=val(CrossingTimeValue))
  ,((Offtrack='$null$',\+ rdf(BoundaryCrossing,fixm:'offtrack',_Offtrack,Graph))
  ,(rdf(Offtrack,fixm:'offtrack',OfftrackNode,Graph))
  ,rdf(OfftrackNode,rdf:value,OfftrackValue,Graph)
  ,Offtrack=val(OfftrackValue))
  ,((AltitudeInTransition='$null$',\+ rdf(BoundaryCrossing,fixm:'altitudeInTransition',_AltitudeInTransition,Graph))
  ,(rdf(AltitudeInTransition,fixm:'altitudeInTransition',AltitudeInTransitionNode,Graph))
  ,rdf(AltitudeInTransitionNode,rdf:value,AltitudeInTransitionValue,Graph)
  ,AltitudeInTransition=val(AltitudeInTransitionValue)) .

fixm_IcaoAerodromeReference(Graph, IcaoAerodromeReference, Code) :-
  rdf(IcaoAerodromeReference,rdf:type,fixm:'IcaoAerodromeReference',Graph)
  ,((Code='$null$',\+ rdf(IcaoAerodromeReference,fixm:'code',_Code,Graph))
  ,(rdf(Code,fixm:'code',CodeNode,Graph))
  ,rdf(CodeNode,rdf:value,CodeValue,Graph)
  ,Code=val(CodeValue)) .

fixm_RadioCommunicationFailure(Graph, RadioCommunicationFailure, RadioFailureRemarks, RemainingComCapability, Contact) :-
  rdf(RadioCommunicationFailure,rdf:type,fixm:'RadioCommunicationFailure',Graph)
  ,((RadioFailureRemarks='$null$',\+ rdf(RadioCommunicationFailure,fixm:'radioFailureRemarks',_RadioFailureRemarks,Graph))
  ,(rdf(RadioFailureRemarks,fixm:'radioFailureRemarks',RadioFailureRemarksNode,Graph))
  ,rdf(RadioFailureRemarksNode,rdf:value,RadioFailureRemarksValue,Graph)
  ,RadioFailureRemarks=val(RadioFailureRemarksValue))
  ,((RemainingComCapability='$null$',\+ rdf(RadioCommunicationFailure,fixm:'remainingComCapability',_RemainingComCapability,Graph))
  ,(rdf(RemainingComCapability,fixm:'remainingComCapability',RemainingComCapabilityNode,Graph))
  ,rdf(RemainingComCapabilityNode,rdf:value,RemainingComCapabilityValue,Graph)
  ,RemainingComCapability=val(RemainingComCapabilityValue))
  ,((Contact='$null$',\+ rdf(RadioCommunicationFailure,fixm:'contact',_Contact,Graph))
  ,(rdf(Contact,fixm:'contact',ContactNode,Graph))
  ,rdf(ContactNode,rdf:value,ContactValue,Graph)
  ,Contact=val(ContactValue)) .

aixm_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus, Warning, Usage) :-
  rdf(AirportHeliportAvailability,rdf:type,aixm:'AirportHeliportAvailability',Graph)
  ,((OperationalStatus='$null$',\+ rdf(AirportHeliportAvailability,aixm:'operationalStatus',_OperationalStatus,Graph))
  ,(rdf(OperationalStatus,aixm:'operationalStatus',OperationalStatusNode,Graph))
  ,rdf(OperationalStatusNode,rdf:value,OperationalStatusValue,Graph)
  ,OperationalStatus=val(OperationalStatusValue))
  ,((Warning='$null$',\+ rdf(AirportHeliportAvailability,aixm:'warning',_Warning,Graph))
  ,(rdf(Warning,aixm:'warning',WarningNode,Graph))
  ,rdf(WarningNode,rdf:value,WarningValue,Graph)
  ,Warning=val(WarningValue))
 .

fixm_FlightArrival(Graph, FlightArrival, ApproachFix, ApproachTime, ArrivalAerodrome, ArrivalAerodromeAlternate, ArrivalAerodromeOriginal, ArrivalFix, ArrivalFixTime, ArrivalFleetPrioritization, ArrivalSequenceNumber, EarliestInBlockTime, FiledRevisedDestinationAerodrome, FiledRevisedDestinationStar, RunwayPositionAndTime, StandardInstrumentArrival, StandPositionAndTime, LandingLimits) :-
  rdf(FlightArrival,rdf:type,fixm:'FlightArrival',Graph)
  ,((ApproachFix='$null$',\+ rdf(FlightArrival,fixm:'approachFix',_ApproachFix,Graph))
  ,(rdf(ApproachFix,fixm:'approachFix',ApproachFixNode,Graph))
  ,rdf(ApproachFixNode,rdf:value,ApproachFixValue,Graph)
  ,ApproachFix=val(ApproachFixValue))
  ,((ApproachTime='$null$',\+ rdf(FlightArrival,fixm:'approachTime',_ApproachTime,Graph))
  ,(rdf(ApproachTime,fixm:'approachTime',ApproachTimeNode,Graph))
  ,rdf(ApproachTimeNode,rdf:value,ApproachTimeValue,Graph)
  ,ApproachTime=val(ApproachTimeValue))
  ,((ArrivalAerodrome='$null$',\+ rdf(FlightArrival,fixm:'arrivalAerodrome',_ArrivalAerodrome,Graph))
  ,(rdf(ArrivalAerodrome,fixm:'arrivalAerodrome',ArrivalAerodromeNode,Graph))
  ,rdf(ArrivalAerodromeNode,rdf:value,ArrivalAerodromeValue,Graph)
  ,ArrivalAerodrome=val(ArrivalAerodromeValue))

  ,((ArrivalAerodromeOriginal='$null$',\+ rdf(FlightArrival,fixm:'arrivalAerodromeOriginal',_ArrivalAerodromeOriginal,Graph))
  ,(rdf(ArrivalAerodromeOriginal,fixm:'arrivalAerodromeOriginal',ArrivalAerodromeOriginalNode,Graph))
  ,rdf(ArrivalAerodromeOriginalNode,rdf:value,ArrivalAerodromeOriginalValue,Graph)
  ,ArrivalAerodromeOriginal=val(ArrivalAerodromeOriginalValue))
  ,((ArrivalFix='$null$',\+ rdf(FlightArrival,fixm:'arrivalFix',_ArrivalFix,Graph))
  ,(rdf(ArrivalFix,fixm:'arrivalFix',ArrivalFixNode,Graph))
  ,rdf(ArrivalFixNode,rdf:value,ArrivalFixValue,Graph)
  ,ArrivalFix=val(ArrivalFixValue))
  ,((ArrivalFixTime='$null$',\+ rdf(FlightArrival,fixm:'arrivalFixTime',_ArrivalFixTime,Graph))
  ,(rdf(ArrivalFixTime,fixm:'arrivalFixTime',ArrivalFixTimeNode,Graph))
  ,rdf(ArrivalFixTimeNode,rdf:value,ArrivalFixTimeValue,Graph)
  ,ArrivalFixTime=val(ArrivalFixTimeValue))
  ,((ArrivalFleetPrioritization='$null$',\+ rdf(FlightArrival,fixm:'arrivalFleetPrioritization',_ArrivalFleetPrioritization,Graph))
  ,(rdf(ArrivalFleetPrioritization,fixm:'arrivalFleetPrioritization',ArrivalFleetPrioritizationNode,Graph))
  ,rdf(ArrivalFleetPrioritizationNode,rdf:value,ArrivalFleetPrioritizationValue,Graph)
  ,ArrivalFleetPrioritization=val(ArrivalFleetPrioritizationValue))
  ,((ArrivalSequenceNumber='$null$',\+ rdf(FlightArrival,fixm:'arrivalSequenceNumber',_ArrivalSequenceNumber,Graph))
  ,(rdf(ArrivalSequenceNumber,fixm:'arrivalSequenceNumber',ArrivalSequenceNumberNode,Graph))
  ,rdf(ArrivalSequenceNumberNode,rdf:value,ArrivalSequenceNumberValue,Graph)
  ,ArrivalSequenceNumber=val(ArrivalSequenceNumberValue))
  ,((EarliestInBlockTime='$null$',\+ rdf(FlightArrival,fixm:'earliestInBlockTime',_EarliestInBlockTime,Graph))
  ,(rdf(EarliestInBlockTime,fixm:'earliestInBlockTime',EarliestInBlockTimeNode,Graph))
  ,rdf(EarliestInBlockTimeNode,rdf:value,EarliestInBlockTimeValue,Graph)
  ,EarliestInBlockTime=val(EarliestInBlockTimeValue))
  ,((FiledRevisedDestinationAerodrome='$null$',\+ rdf(FlightArrival,fixm:'filedRevisedDestinationAerodrome',_FiledRevisedDestinationAerodrome,Graph))
  ,(rdf(FiledRevisedDestinationAerodrome,fixm:'filedRevisedDestinationAerodrome',FiledRevisedDestinationAerodromeNode,Graph))
  ,rdf(FiledRevisedDestinationAerodromeNode,rdf:value,FiledRevisedDestinationAerodromeValue,Graph)
  ,FiledRevisedDestinationAerodrome=val(FiledRevisedDestinationAerodromeValue))
  ,((FiledRevisedDestinationStar='$null$',\+ rdf(FlightArrival,fixm:'filedRevisedDestinationStar',_FiledRevisedDestinationStar,Graph))
  ,(rdf(FiledRevisedDestinationStar,fixm:'filedRevisedDestinationStar',FiledRevisedDestinationStarNode,Graph))
  ,rdf(FiledRevisedDestinationStarNode,rdf:value,FiledRevisedDestinationStarValue,Graph)
  ,FiledRevisedDestinationStar=val(FiledRevisedDestinationStarValue))
  ,((RunwayPositionAndTime='$null$',\+ rdf(FlightArrival,fixm:'runwayPositionAndTime',_RunwayPositionAndTime,Graph))
  ,(rdf(RunwayPositionAndTime,fixm:'runwayPositionAndTime',RunwayPositionAndTimeNode,Graph))
  ,rdf(RunwayPositionAndTimeNode,rdf:value,RunwayPositionAndTimeValue,Graph)
  ,RunwayPositionAndTime=val(RunwayPositionAndTimeValue))
  ,((StandardInstrumentArrival='$null$',\+ rdf(FlightArrival,fixm:'standardInstrumentArrival',_StandardInstrumentArrival,Graph))
  ,(rdf(StandardInstrumentArrival,fixm:'standardInstrumentArrival',StandardInstrumentArrivalNode,Graph))
  ,rdf(StandardInstrumentArrivalNode,rdf:value,StandardInstrumentArrivalValue,Graph)
  ,StandardInstrumentArrival=val(StandardInstrumentArrivalValue))
  ,((StandPositionAndTime='$null$',\+ rdf(FlightArrival,fixm:'standPositionAndTime',_StandPositionAndTime,Graph))
  ,(rdf(StandPositionAndTime,fixm:'standPositionAndTime',StandPositionAndTimeNode,Graph))
  ,rdf(StandPositionAndTimeNode,rdf:value,StandPositionAndTimeValue,Graph)
  ,StandPositionAndTime=val(StandPositionAndTimeValue))
  ,((LandingLimits='$null$',\+ rdf(FlightArrival,fixm:'landingLimits',_LandingLimits,Graph))
  ,(rdf(LandingLimits,fixm:'landingLimits',LandingLimitsNode,Graph))
  ,rdf(LandingLimitsNode,rdf:value,LandingLimitsValue,Graph)
  ,LandingLimits=val(LandingLimitsValue)) .

fixm_RadioactiveMaterial(Graph, RadioactiveMaterial, CriticalitySafetyIndex, TransportIndex, FissileExceptedIndicator, Category, Radionuclide) :-
  rdf(RadioactiveMaterial,rdf:type,fixm:'RadioactiveMaterial',Graph)
  ,((CriticalitySafetyIndex='$null$',\+ rdf(RadioactiveMaterial,fixm:'criticalitySafetyIndex',_CriticalitySafetyIndex,Graph))
  ,(rdf(CriticalitySafetyIndex,fixm:'criticalitySafetyIndex',CriticalitySafetyIndexNode,Graph))
  ,rdf(CriticalitySafetyIndexNode,rdf:value,CriticalitySafetyIndexValue,Graph)
  ,CriticalitySafetyIndex=val(CriticalitySafetyIndexValue))
  ,((TransportIndex='$null$',\+ rdf(RadioactiveMaterial,fixm:'transportIndex',_TransportIndex,Graph))
  ,(rdf(TransportIndex,fixm:'transportIndex',TransportIndexNode,Graph))
  ,rdf(TransportIndexNode,rdf:value,TransportIndexValue,Graph)
  ,TransportIndex=val(TransportIndexValue))
  ,((FissileExceptedIndicator='$null$',\+ rdf(RadioactiveMaterial,fixm:'fissileExceptedIndicator',_FissileExceptedIndicator,Graph))
  ,(rdf(FissileExceptedIndicator,fixm:'fissileExceptedIndicator',FissileExceptedIndicatorNode,Graph))
  ,rdf(FissileExceptedIndicatorNode,rdf:value,FissileExceptedIndicatorValue,Graph)
  ,FissileExceptedIndicator=val(FissileExceptedIndicatorValue))
  ,((Category='$null$',\+ rdf(RadioactiveMaterial,fixm:'category',_Category,Graph))
  ,(rdf(Category,fixm:'category',CategoryNode,Graph))
  ,rdf(CategoryNode,rdf:value,CategoryValue,Graph)
  ,Category=val(CategoryValue))
  ,((Radionuclide='$null$',\+ rdf(RadioactiveMaterial,fixm:'radionuclide',_Radionuclide,Graph))
  ,(rdf(Radionuclide,fixm:'radionuclide',RadionuclideNode,Graph))
  ,rdf(RadionuclideNode,rdf:value,RadionuclideValue,Graph)
  ,Radionuclide=val(RadionuclideValue)) .

fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled, Initial) :-
  rdf(ExtendedMultiTime,rdf:type,fixm:'ExtendedMultiTime',Graph)
  ,((Controlled='$null$',\+ rdf(ExtendedMultiTime,fixm:'controlled',_Controlled,Graph))
  ,(rdf(Controlled,fixm:'controlled',ControlledNode,Graph))
  ,rdf(ControlledNode,rdf:value,ControlledValue,Graph)
  ,Controlled=val(ControlledValue))
  ,((Initial='$null$',\+ rdf(ExtendedMultiTime,fixm:'initial',_Initial,Graph))
  ,(rdf(Initial,fixm:'initial',InitialNode,Graph))
  ,rdf(InitialNode,rdf:value,InitialValue,Graph)
  ,Initial=val(InitialValue)) .

fixm_ControlElement(Graph, ControlElement) :-
  rdf(ControlElement,rdf:type,fixm:'ControlElement',Graph) .

fixm_AerodromesOfDestination(Graph, AerodromesOfDestination, AerodromeOfDestination, Alternate1, Alternate2, FiledRevisedDestinationAerodrome) :-
  rdf(AerodromesOfDestination,rdf:type,fixm:'AerodromesOfDestination',Graph)
  ,((AerodromeOfDestination='$null$',\+ rdf(AerodromesOfDestination,fixm:'aerodromeOfDestination',_AerodromeOfDestination,Graph))
  ,(rdf(AerodromeOfDestination,fixm:'aerodromeOfDestination',AerodromeOfDestinationNode,Graph))
  ,rdf(AerodromeOfDestinationNode,rdf:value,AerodromeOfDestinationValue,Graph)
  ,AerodromeOfDestination=val(AerodromeOfDestinationValue))
  ,((Alternate1='$null$',\+ rdf(AerodromesOfDestination,fixm:'alternate1',_Alternate1,Graph))
  ,(rdf(Alternate1,fixm:'alternate1',Alternate1Node,Graph))
  ,rdf(Alternate1Node,rdf:value,Alternate1Value,Graph)
  ,Alternate1=val(Alternate1Value))
  ,((Alternate2='$null$',\+ rdf(AerodromesOfDestination,fixm:'alternate2',_Alternate2,Graph))
  ,(rdf(Alternate2,fixm:'alternate2',Alternate2Node,Graph))
  ,rdf(Alternate2Node,rdf:value,Alternate2Value,Graph)
  ,Alternate2=val(Alternate2Value))
  ,((FiledRevisedDestinationAerodrome='$null$',\+ rdf(AerodromesOfDestination,fixm:'filedRevisedDestinationAerodrome',_FiledRevisedDestinationAerodrome,Graph))
  ,(rdf(FiledRevisedDestinationAerodrome,fixm:'filedRevisedDestinationAerodrome',FiledRevisedDestinationAerodromeNode,Graph))
  ,rdf(FiledRevisedDestinationAerodromeNode,rdf:value,FiledRevisedDestinationAerodromeValue,Graph)
  ,FiledRevisedDestinationAerodrome=val(FiledRevisedDestinationAerodromeValue)) .

fixm_AllPackedInOne(Graph, AllPackedInOne, NumberOfPackages, QValue) :-
  rdf(AllPackedInOne,rdf:type,fixm:'AllPackedInOne',Graph)
  ,((NumberOfPackages='$null$',\+ rdf(AllPackedInOne,fixm:'numberOfPackages',_NumberOfPackages,Graph))
  ,(rdf(NumberOfPackages,fixm:'numberOfPackages',NumberOfPackagesNode,Graph))
  ,rdf(NumberOfPackagesNode,rdf:value,NumberOfPackagesValue,Graph)
  ,NumberOfPackages=val(NumberOfPackagesValue))
  ,((QValue='$null$',\+ rdf(AllPackedInOne,fixm:'qValue',_QValue,Graph))
  ,(rdf(QValue,fixm:'qValue',QValueNode,Graph))
  ,rdf(QValueNode,rdf:value,QValueValue,Graph)
  ,QValue=val(QValueValue)) .

aixm_AltimeterSource(Graph, AltimeterSource, TimeSlice) :-
  rdf(AltimeterSource,rdf:type,aixm:'AltimeterSource',Graph)
 .

fixm_SurvivalCapabilities(Graph, SurvivalCapabilities, SurvivalEquipmentRemarks, DinghyInformation, EmergencyRadioCode, LifeJacketCode, SurvivalEquipmentCode) :-
  rdf(SurvivalCapabilities,rdf:type,fixm:'SurvivalCapabilities',Graph)
  ,((SurvivalEquipmentRemarks='$null$',\+ rdf(SurvivalCapabilities,fixm:'survivalEquipmentRemarks',_SurvivalEquipmentRemarks,Graph))
  ,(rdf(SurvivalEquipmentRemarks,fixm:'survivalEquipmentRemarks',SurvivalEquipmentRemarksNode,Graph))
  ,rdf(SurvivalEquipmentRemarksNode,rdf:value,SurvivalEquipmentRemarksValue,Graph)
  ,SurvivalEquipmentRemarks=val(SurvivalEquipmentRemarksValue))
  ,((DinghyInformation='$null$',\+ rdf(SurvivalCapabilities,fixm:'dinghyInformation',_DinghyInformation,Graph))
  ,(rdf(DinghyInformation,fixm:'dinghyInformation',DinghyInformationNode,Graph))
  ,rdf(DinghyInformationNode,rdf:value,DinghyInformationValue,Graph)
  ,DinghyInformation=val(DinghyInformationValue))


 .

fixm_DirectRouting(Graph, DirectRouting, From, To) :-
  rdf(DirectRouting,rdf:type,fixm:'DirectRouting',Graph)
  ,((From='$null$',\+ rdf(DirectRouting,fixm:'from',_From,Graph))
  ,(rdf(From,fixm:'from',FromNode,Graph))
  ,rdf(FromNode,rdf:value,FromValue,Graph)
  ,From=val(FromValue))
  ,((To='$null$',\+ rdf(DirectRouting,fixm:'to',_To,Graph))
  ,(rdf(To,fixm:'to',ToNode,Graph))
  ,rdf(ToNode,rdf:value,ToValue,Graph)
  ,To=val(ToValue)) .

fixm_TargetMultiTime(Graph, TargetMultiTime, Target) :-
  subClassOf(T,fixm:'TargetMultiTime')
  ,rdf(TargetMultiTime,rdf:type,T,Graph)
  ,((Target='$null$',\+ rdf(TargetMultiTime,fixm:'target',_Target,Graph))
  ,(rdf(Target,fixm:'target',TargetNode,Graph))
  ,rdf(TargetNode,rdf:value,TargetValue,Graph)
  ,Target=val(TargetValue)) .

fixm_AircraftType(Graph, AircraftType) :-
  rdf(AircraftType,rdf:type,fixm:'AircraftType',Graph) .

fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes) :-
  subClassOf(T,fixm:'FlightDeparture')
  ,rdf(FlightDeparture,rdf:type,T,Graph)
  ,((DepartureAerodrome='$null$',\+ rdf(FlightDeparture,fixm:'departureAerodrome',_DepartureAerodrome,Graph))
  ,(rdf(DepartureAerodrome,fixm:'departureAerodrome',DepartureAerodromeNode,Graph))
  ,rdf(DepartureAerodromeNode,rdf:value,DepartureAerodromeValue,Graph)
  ,DepartureAerodrome=val(DepartureAerodromeValue))
  ,((DepartureFix='$null$',\+ rdf(FlightDeparture,fixm:'departureFix',_DepartureFix,Graph))
  ,(rdf(DepartureFix,fixm:'departureFix',DepartureFixNode,Graph))
  ,rdf(DepartureFixNode,rdf:value,DepartureFixValue,Graph)
  ,DepartureFix=val(DepartureFixValue))
  ,((DepartureFixTime='$null$',\+ rdf(FlightDeparture,fixm:'departureFixTime',_DepartureFixTime,Graph))
  ,(rdf(DepartureFixTime,fixm:'departureFixTime',DepartureFixTimeNode,Graph))
  ,rdf(DepartureFixTimeNode,rdf:value,DepartureFixTimeValue,Graph)
  ,DepartureFixTime=val(DepartureFixTimeValue))
  ,((DepartureFleetPrioritization='$null$',\+ rdf(FlightDeparture,fixm:'departureFleetPrioritization',_DepartureFleetPrioritization,Graph))
  ,(rdf(DepartureFleetPrioritization,fixm:'departureFleetPrioritization',DepartureFleetPrioritizationNode,Graph))
  ,rdf(DepartureFleetPrioritizationNode,rdf:value,DepartureFleetPrioritizationValue,Graph)
  ,DepartureFleetPrioritization=val(DepartureFleetPrioritizationValue))
  ,((DepartureSlot='$null$',\+ rdf(FlightDeparture,fixm:'departureSlot',_DepartureSlot,Graph))
  ,(rdf(DepartureSlot,fixm:'departureSlot',DepartureSlotNode,Graph))
  ,rdf(DepartureSlotNode,rdf:value,DepartureSlotValue,Graph)
  ,DepartureSlot=val(DepartureSlotValue))
  ,((EarliestOffBlockTime='$null$',\+ rdf(FlightDeparture,fixm:'earliestOffBlockTime',_EarliestOffBlockTime,Graph))
  ,(rdf(EarliestOffBlockTime,fixm:'earliestOffBlockTime',EarliestOffBlockTimeNode,Graph))
  ,rdf(EarliestOffBlockTimeNode,rdf:value,EarliestOffBlockTimeValue,Graph)
  ,EarliestOffBlockTime=val(EarliestOffBlockTimeValue))
  ,((OffBlockReadyTime='$null$',\+ rdf(FlightDeparture,fixm:'offBlockReadyTime',_OffBlockReadyTime,Graph))
  ,(rdf(OffBlockReadyTime,fixm:'offBlockReadyTime',OffBlockReadyTimeNode,Graph))
  ,rdf(OffBlockReadyTimeNode,rdf:value,OffBlockReadyTimeValue,Graph)
  ,OffBlockReadyTime=val(OffBlockReadyTimeValue))
  ,((RunwayPositionAndTime='$null$',\+ rdf(FlightDeparture,fixm:'runwayPositionAndTime',_RunwayPositionAndTime,Graph))
  ,(rdf(RunwayPositionAndTime,fixm:'runwayPositionAndTime',RunwayPositionAndTimeNode,Graph))
  ,rdf(RunwayPositionAndTimeNode,rdf:value,RunwayPositionAndTimeValue,Graph)
  ,RunwayPositionAndTime=val(RunwayPositionAndTimeValue))
  ,((StandardInstrumentDeparture='$null$',\+ rdf(FlightDeparture,fixm:'standardInstrumentDeparture',_StandardInstrumentDeparture,Graph))
  ,(rdf(StandardInstrumentDeparture,fixm:'standardInstrumentDeparture',StandardInstrumentDepartureNode,Graph))
  ,rdf(StandardInstrumentDepartureNode,rdf:value,StandardInstrumentDepartureValue,Graph)
  ,StandardInstrumentDeparture=val(StandardInstrumentDepartureValue))
  ,((StandPositionAndTime='$null$',\+ rdf(FlightDeparture,fixm:'standPositionAndTime',_StandPositionAndTime,Graph))
  ,(rdf(StandPositionAndTime,fixm:'standPositionAndTime',StandPositionAndTimeNode,Graph))
  ,rdf(StandPositionAndTimeNode,rdf:value,StandPositionAndTimeValue,Graph)
  ,StandPositionAndTime=val(StandPositionAndTimeValue))

  ,((TakeoffWeight='$null$',\+ rdf(FlightDeparture,fixm:'takeoffWeight',_TakeoffWeight,Graph))
  ,(rdf(TakeoffWeight,fixm:'takeoffWeight',TakeoffWeightNode,Graph))
  ,rdf(TakeoffWeightNode,rdf:value,TakeoffWeightValue,Graph)
  ,TakeoffWeight=val(TakeoffWeightValue))
  ,((DepartureTimes='$null$',\+ rdf(FlightDeparture,fixm:'departureTimes',_DepartureTimes,Graph))
  ,(rdf(DepartureTimes,fixm:'departureTimes',DepartureTimesNode,Graph))
  ,rdf(DepartureTimesNode,rdf:value,DepartureTimesValue,Graph)
  ,DepartureTimes=val(DepartureTimesValue)) .

fixm_AerodromeReference(Graph, AerodromeReference) :-
  subClassOf(T,fixm:'AerodromeReference')
  ,rdf(AerodromeReference,rdf:type,T,Graph) .

fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime, TaxiTime) :-
  rdf(EfplFlightDeparture,rdf:type,fixm:'EfplFlightDeparture',Graph)
  ,((EstimatedOffBlockTime='$null$',\+ rdf(EfplFlightDeparture,fixm:'estimatedOffBlockTime',_EstimatedOffBlockTime,Graph))
  ,(rdf(EstimatedOffBlockTime,fixm:'estimatedOffBlockTime',EstimatedOffBlockTimeNode,Graph))
  ,rdf(EstimatedOffBlockTimeNode,rdf:value,EstimatedOffBlockTimeValue,Graph)
  ,EstimatedOffBlockTime=val(EstimatedOffBlockTimeValue))
  ,((TaxiTime='$null$',\+ rdf(EfplFlightDeparture,fixm:'taxiTime',_TaxiTime,Graph))
  ,(rdf(TaxiTime,fixm:'taxiTime',TaxiTimeNode,Graph))
  ,rdf(TaxiTimeNode,rdf:value,TaxiTimeValue,Graph)
  ,TaxiTime=val(TaxiTimeValue)) .

aixm_UsageCondition(Graph, UsageCondition, Type, PriorPermission, Selection, Annotation, Contact) :-
  subClassOf(T,aixm:'UsageCondition')
  ,rdf(UsageCondition,rdf:type,T,Graph)
  ,((Type='$null$',\+ rdf(UsageCondition,aixm:'type',_Type,Graph))
  ,(rdf(Type,aixm:'type',TypeNode,Graph))
  ,rdf(TypeNode,rdf:value,TypeValue,Graph)
  ,Type=val(TypeValue))
  ,((PriorPermission='$null$',\+ rdf(UsageCondition,aixm:'priorPermission',_PriorPermission,Graph))
  ,(rdf(PriorPermission,aixm:'priorPermission',PriorPermissionNode,Graph))
  ,rdf(PriorPermissionNode,rdf:value,PriorPermissionValue,Graph)
  ,PriorPermission=val(PriorPermissionValue))
  ,((Selection='$null$',\+ rdf(UsageCondition,aixm:'selection',_Selection,Graph))
  ,(rdf(Selection,aixm:'selection',SelectionNode,Graph))
  ,rdf(SelectionNode,rdf:value,SelectionValue,Graph)
  ,Selection=val(SelectionValue))

 .

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

