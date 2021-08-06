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
fixm_NavigationCapabilities(Graph, NavigationCapabilities, OtherNavigationCapabilities, PerformanceBasedCode, NavigationCode) :-
fixm_GroundspeedRange(Graph, GroundspeedRange, LowerSpeed, UpperSpeed) :-
aixm_Note(Graph, Note, PropertyName, Purpose, TranslatedNote) :-
fixm_Pointout(Graph, Pointout, OriginatingUnit, ReceivingUnit) :-
fixm_VerticalRange(Graph, VerticalRange, LowerBound, UpperBound) :-
fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel, EstimatedTime, Constraint) :-
aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
fixm_Dimensions(Graph, Dimensions, Height, Length, Width) :-
fixm_StandPositionAndTime(Graph, StandPositionAndTime, StandName, StandTime, TerminalName) :-
fixm_RouteSegment(Graph, RouteSegment, Airway, RoutePoint) :-
aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator, Flight, Aircraft, Weather, SubCondition) :-
aixm_SurfaceContaminationLayer(Graph, SurfaceContaminationLayer, LayerOrder, Type, Extent, Annotation) :-
fixm_Organization(Graph, Organization, Name, OtherOrganization, Contact) :-
aixm_OrganisationAuthorityAssociation(Graph, OrganisationAuthorityAssociation, Type, Annotation, TheOrganisationAuthority) :-
aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel) :-
fixm_AircraftOperator(Graph, AircraftOperator, OperatingOrganization, OperatorCategory) :-
gml_Point(Graph, Point) :-

fixm_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair) :-
fixm_RoutePoint(Graph, RoutePoint, Constraint) :-
fixm_BeaconCodeAssignment(Graph, BeaconCodeAssignment, CurrentBeaconCode, PreviousBeaconCode, ReassignedBeaconCode, ReassigningUnit) :-
fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile) :-
fixm_ExpandedRoute(Graph, ExpandedRoute, RoutePoint) :-
fixm_RouteConstraintOrPreference(Graph, RouteConstraintOrPreference, ConstraintType) :-
fixm_DeclarationText(Graph, DeclarationText, Compliance, Consignor, Shipper) :-
fixm_EstimatedElapsedTime(Graph, EstimatedElapsedTime, ElapsedTime, Location) :-
fixm_ReportedTime(Graph, ReportedTime, Provenance, Time) :-
fixm_GeographicLocation(Graph, GeographicLocation, PosList, SrsName) :-

aixm_LinguisticNote(Graph, LinguisticNote, Note) :-
aixm_Meteorology(Graph, Meteorology, FlightConditions, Visibility, VisibilityInterpretation, RunwayVisualRange, RunwayVisualRangeInterpretation, Annotation) :-
fixm_PointRange(Graph, PointRange, LateralRange, VerticalRange, TemporalRange) :-
aixm_City(Graph, City, Name, Annotation) :-
aixm_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role, TheOrganisationAuthority) :-
fixm_AirspeedRange(Graph, AirspeedRange, LowerSpeed, UpperSpeed) :-
fixm_RankedTrajectory(Graph, RankedTrajectory, Identifier, MaximumAcceptableDelay, AssignedIndicator, RouteTrajectoryPair) :-
fixm_TrajectoryPointRole(Graph, TrajectoryPointRole, BottomOfClimb, BottomOfDescent, BoundaryPoint, FromGATToOAT, FromIFRToVFR, FromOATToGat, FromVFRToIFR, TopOfClimb, TopOfDescent) :-
fixm_CommunicationCapabilities(Graph, CommunicationCapabilities, OtherCommunicationCapabilities, OtherDataLinkCapabilities, DataLinkCode, SelectiveCallingCode, CommunicationCode) :-
fixm_Dinghy(Graph, Dinghy, Quantity, TotalCapacity, Covered, Colour) :-
aixm_ContactInformation(Graph, ContactInformation, Name, Title, Annotation, NetworkNode, Address, PhoneFax) :-
fixm_PlannedReportingPosition(Graph, PlannedReportingPosition, Position, PositionAltitude, PositionEstimatedTime) :-
fixm_SignificantPoint(Graph, SignificantPoint) :-
fixm_SupplementalData(Graph, SupplementalData, FuelEndurance, PersonsOnBoard, PilotInCommand) :-
fixm_DangerousGoods(Graph, DangerousGoods, GuidebookNumber, OnboardLocation, HandlingInformation, AircraftLimitation, AirWayBill, Shipment, PackageGroup, ShippingInformation) :-
fixm_DangerousGoodsPackageGroup(Graph, DangerousGoodsPackageGroup, ShipmentDimensions, DangerousGoodsPackage, ShipmentUseIndicator) :-
fixm_OfftrackDistance(Graph, OfftrackDistance, Distance, Direction) :-
fixm_Handoff(Graph, Handoff, ReceivingUnit, TransferringUnit, CoordinationStatus) :-
fixm_TrajectoryChange(Graph, TrajectoryChange, ConstrainedAirspace, SpecialActivityAirspace) :-
fixm_ContactInformation(Graph, ContactInformation, Name, Title, OnlineContact, PhoneFax, Address) :-

aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability) :-
fixm_Point4D(Graph, Point4D, Altitude, Time, PointRange) :-

fixm_AbstractRoutePoint(Graph, AbstractRoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit) :-

aixm_Ridge(Graph, Ridge, Side, Distance, Depth, Annotation) :-
fixm_DepartureActivityTimes(Graph, DepartureActivityTimes, BoardingTime, DeIcingTime, GroundHandlingTime, StartupTime) :-
fixm_EnRouteDiversion(Graph, EnRouteDiversion, DiversionRecoveryInformation) :-
fixm_ActualSpeed(Graph, ActualSpeed, Calculated, PilotReported, Surveillance) :-
fixm_FlightEmergency(Graph, FlightEmergency, ActionTaken, EmergencyDescription, Originator, OtherInformation, Phase, Contact) :-
fixm_Flight(Graph, Flight, ControllingUnit, Extensions, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoods, RankedTrajectories, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandling) :-

aixm_PropertiesWithSchedule(Graph, PropertiesWithSchedule, Annotation, SpecialDateAuthority, TimeInterval) :-

gml_Surface(Graph, Surface, Patch) :-

fixm_ClearedFlightInformation(Graph, ClearedFlightInformation, ClearedFlightLevel, ClearedSpeed, Heading, OfftrackClearance, RateOfClimbDescend, DirectRouting) :-
fixm_TrajectoryRoutePair(Graph, TrajectoryRoutePair, Trajectory, Route) :-

fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit, UpstreamUnit, BoundaryCrossingProposed, BoundaryCrossingCoordinated, Handoff, UnitBoundaryIndicator) :-
aixm_SurfaceContamination(Graph, SurfaceContamination, ObservationTime, Depth, FrictionCoefficient, FrictionEstimation, FrictionDevice, ObscuredLights, FurtherClearanceTime, FurtherTotalClearance, NextObservationTime, Proportion, CriticalRidge, Annotation, Layer) :-

fixm_MeteorologicalData(Graph, MeteorologicalData, Temperature, WindDirection, WindSpeed) :-
aixm_OrganisationAuthority(Graph, OrganisationAuthority, TimeSlice) :-
fixm_TelephoneContact(Graph, TelephoneContact, Voice, Facimile) :-
fixm_ShippingInformation(Graph, ShippingInformation, AerodromeOfLoading, AerodromeOfUnloading, DangerousGoodsScreeningLocation, DepartureCountry, DestinationCountry, OriginCountry, ShipmentAuthorizations, SubsidiaryHazardClassAndDivision, SupplementaryInformation, TransferAerodromes, DeclarationText, Consignee, Shipper) :-
aixm_AirportHeliportContamination(Graph, AirportHeliportContamination) :-
fixm_OtherInformation(Graph, OtherInformation, ReplacementFlightPlanIndicator, RunwayVisualRange) :-
fixm_DinghyColour(Graph, DinghyColour) :-
fixm_CpdlcConnection(Graph, CpdlcConnection, ReceivingUnitFrequency, AtnLogonParameters, SendCpldcIndicator, ConnectionStatus, FrequencyUsage, Fans1ALogonParameters) :-
aixm_TelephoneContact(Graph, TelephoneContact, Voice, Facsimile) :-
fixm_Route(Graph, Route, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTime, ExpandedRoute, ClimbSchedule, DescentSchedule, Segment) :-

fixm_Person(Graph, Person, Name, Contact) :-
fixm_EfplFlight(Graph, EfplFlight, IfplId, TotalEstimatedElapsedTime, AerodromesOfDestination, EfplSpecialHandling, EfplFiledTrajectory, EfplAcceptedTrajectory, OtherInformation, FlightPerformanceData) :-
fixm_Originator(Graph, Originator) :-
fixm_FlightStatus(Graph, FlightStatus, AirborneHold, Airfile, Accepted, FlightCycle, MissedApproach, Suspended) :-
fixm_IdentifiedUnitReference(Graph, IdentifiedUnitReference, UnitIdentifier) :-
fixm_Radionuclide(Graph, Radionuclide, PhysicalChemicalForm, RadionuclideId, RadionuclideName, LowDispersibleMaterialIndicator, Activity, SpecialFormIndicator) :-
aixm_OnlineContact(Graph, OnlineContact, Network, Linkage, Protocol, EMail) :-
fixm_StructuredPostalAddress(Graph, StructuredPostalAddress) :-
fixm_AircraftPosition(Graph, AircraftPosition, Altitude, Position, PositionTime, Track, ActualSpeed, NextPosition, ReportSource, FollowingPosition) :-
aixm_AirportHeliportUsage(Graph, AirportHeliportUsage, Operation) :-
aixm_Timesheet(Graph, Timesheet, TimeReference, StartDate, EndDate, Day, DayTil, StartTime, StartEvent, StartTimeRelativeEvent, StartEventInterpretation, EndTime, EndEvent, EndTimeRelativeEvent, EndEventInterpretation, DaylightSavingAdjust, Excluded, Annotation) :-
gml_SurfacePatch(Graph, SurfacePatch) :-

fixm_MultiTime(Graph, MultiTime, Actual, Estimated) :-

aixm_FlightCharacteristic(Graph, FlightCharacteristic, Type, Rule, Status, Military, Origin, Purpose, Annotation) :-
fixm_Provenance(Graph, Provenance, Timestamp, Centre, Source, System) :-
aixm_AirportHeliport(Graph, AirportHeliport, TimeSlice) :-
fixm_TrajectoryPoint(Graph, TrajectoryPoint, AltimeterSetting, PredictedAirspeed, PredictedGroundspeed, MetData, Point, TrajectoryChange, TrajectoryChangeType, ReferencePoint) :-

fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier, DistanceFromTakeOff, EfplEstimatedSpeed, ElapsedTime, GrossWeight, TrajectoryPointType, TrajectoryPointRole, InboundSegment) :-
fixm_Temperatures(Graph, Temperatures, ControlTemperature, EmergencyTemperature, FlashpointTemperature) :-
fixm_TrajectorySegment(Graph, TrajectorySegment, SegmentIdentifier, SegmentType) :-
fixm_RunwayPositionAndTime(Graph, RunwayPositionAndTime, RunwayName, RunwayTime) :-
fixm_Feature(Graph, Feature, Provenance) :-

fixm_FlightIdentification(Graph, FlightIdentification, AircraftIdentification, MajorCarrierIdentifier, MarketingCarrierFlightIdentifier) :-
fixm_LastContact(Graph, LastContact, ContactFrequency, LastContactTime, LastContactUnit, Position) :-
fixm_ElapsedTimeLocation(Graph, ElapsedTimeLocation) :-
aixm_Surface(Graph, Surface, HorizontalAccuracy, Annotation) :-

gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition) :-
fixm_AircraftCapabilities(Graph, AircraftCapabilities, Survival, Communication, Navigation, Surveillance, StandardCapabilities) :-
fixm_SpeedSchedule(Graph, SpeedSchedule, InitialSpeed, SubsequentSpeed) :-
aixm_OrganisationAuthorityTimeSlice(Graph, OrganisationAuthorityTimeSlice, Name, Designator, Type, Military, Annotation, Contact, RelatedOrganisationAuthority) :-
fixm_EnRoute(Graph, EnRoute, AlternateAerodrome, FleetPrioritization, BoundaryCrossings, CpdlcConnection, BeaconCodeAssignment, Cleared, ControlElement, Pointout, Position) :-
fixm_FlightLevel(Graph, FlightLevel, Level, Unit) :-
fixm_LateralOfftrack(Graph, LateralOfftrack, OfftrackDistanceList, OfftrackReason) :-
fixm_TemporalRange(Graph, TemporalRange, Earliest, Latest) :-
fixm_Aircraft(Graph, Aircraft, AircraftColours, AircraftQuantity, EngineType, AircraftAddress, Capabilities, Registration, AircraftType, WakeTurbulence, AircraftPerformance) :-
fixm_OnlineContact(Graph, OnlineContact, Email) :-
fixm_AirspaceConstraint(Graph, AirspaceConstraint, AirspaceControlledEntryTime, ConstrainedAirspace) :-
fixm_TimeSequence(Graph, TimeSequence, Approval, Begin, End, Ready, Request) :-
fixm_AdditionalHandlingInformation(Graph, AdditionalHandlingInformation, ResponsibleAgent) :-
fixm_AtcUnitReference(Graph, AtcUnitReference, SectorIdentifier, Delegated) :-

fixm_Extension(Graph, Extension) :-
fixm_SurveillanceCapabilities(Graph, SurveillanceCapabilities, OtherSurveillanceCapabilities, SurveillanceCode) :-
fixm_Trajectory(Graph, Trajectory, TrajectoryPoint) :-
aixm_AltimeterSourceTimeSlice(Graph, AltimeterSourceTimeSlice, IsRemote, IsPrimary, Availability, Annotation) :-
aixm_Point(Graph, Point, HorizontalAccuracy, Annotation) :-

aixm_AircraftCharacteristic(Graph, AircraftCharacteristic, Type, Engine, NumberEngine, TypeAircraftICAO, AircraftLandingCategory, WingSpan, WingSpanInterpretation, ClassWingSpan, Weight, WeightInterpretation, Passengers, PassengersInterpretation, Speed, SpeedInterpretation, WakeTurbulence, NavigationEquipment, NavigationSpecification, VerticalSeparationCapability, AntiCollisionAndSeparationEquipment, CommunicationEquipment, SurveillanceEquipment, Annotation) :-
aixm_PostalAddress(Graph, PostalAddress, DeliveryPoint, City, AdministrativeArea, PostalCode, Country) :-
fixm_DangerousGoodsPackage(Graph, DangerousGoodsPackage, DangerousGoodsQuantity, PackageDimensions, PackingInstructionNumber, ProductName, ProperShippingName, ReportableQuantity, SupplementaryInformation, TechnicalName, TypeOfPackaging, UnNumber, DangerousGoodsLimitation, ShipmentType, AllPackedInOne, CompatibilityGroup, ShipmentDimensions, MarinePollutantIndicator, RadioactiveMaterials, HazardClass, PackingGroup, Temperatures, OverpackIndicator, SubsidiaryHazardClassList) :-
fixm_LastPositionReport(Graph, LastPositionReport, DeterminationMethod, Position, TimeAtPosition) :-
aixm_AltimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus) :-
fixm_DangerousGoodsDimensions(Graph, DangerousGoodsDimensions, GrossWeight, NetWeight, Volume) :-
fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules) :-
fixm_CoordinationStatus(Graph, CoordinationStatus, AbrogationReason, CoordinationStatus, NonStandardCommunicationReason, ReleaseConditions) :-
fixm_BoundaryCrossing(Graph, BoundaryCrossing, Altitude, CrossingPoint, CrossingSpeed, CrossingTime, Offtrack, AltitudeInTransition) :-
fixm_IcaoAerodromeReference(Graph, IcaoAerodromeReference, Code) :-
fixm_RadioCommunicationFailure(Graph, RadioCommunicationFailure, RadioFailureRemarks, RemainingComCapability, Contact) :-
aixm_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus, Warning, Usage) :-
fixm_FlightArrival(Graph, FlightArrival, ApproachFix, ApproachTime, ArrivalAerodrome, ArrivalAerodromeAlternate, ArrivalAerodromeOriginal, ArrivalFix, ArrivalFixTime, ArrivalFleetPrioritization, ArrivalSequenceNumber, EarliestInBlockTime, FiledRevisedDestinationAerodrome, FiledRevisedDestinationStar, RunwayPositionAndTime, StandardInstrumentArrival, StandPositionAndTime, LandingLimits) :-
fixm_RadioactiveMaterial(Graph, RadioactiveMaterial, CriticalitySafetyIndex, TransportIndex, FissileExceptedIndicator, Category, Radionuclide) :-
fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled, Initial) :-
fixm_ControlElement(Graph, ControlElement) :-
fixm_AerodromesOfDestination(Graph, AerodromesOfDestination, AerodromeOfDestination, Alternate1, Alternate2, FiledRevisedDestinationAerodrome) :-
fixm_AllPackedInOne(Graph, AllPackedInOne, NumberOfPackages, QValue) :-
aixm_AltimeterSource(Graph, AltimeterSource, TimeSlice) :-
fixm_SurvivalCapabilities(Graph, SurvivalCapabilities, SurvivalEquipmentRemarks, DinghyInformation, EmergencyRadioCode, LifeJacketCode, SurvivalEquipmentCode) :-
fixm_DirectRouting(Graph, DirectRouting, From, To) :-
fixm_TargetMultiTime(Graph, TargetMultiTime, Target) :-

fixm_AircraftType(Graph, AircraftType) :-
fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes) :-

fixm_AerodromeReference(Graph, AerodromeReference) :-

fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime, TaxiTime) :-
aixm_UsageCondition(Graph, UsageCondition, Type, PriorPermission, Selection, Annotation, Contact) :-

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

