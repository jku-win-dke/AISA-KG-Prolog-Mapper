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
  % fixm_PostalAddress(Graph, PostalAddress, AdministrativeArea?, PostalCode?, DeliveryPoint?, CountryCode?, CountryName?, City?)
  fixm_PostalAddress(r,r,t,t,t,t,t,t)

  % fixm_NavigationCapabilities(Graph, NavigationCapabilities, OtherNavigationCapabilities?, PerformanceBasedCode*, NavigationCode*)
  ,fixm_NavigationCapabilities(r,r,t,t,t)

  % fixm_GroundspeedRange(Graph, GroundspeedRange, LowerSpeed?, UpperSpeed?)
  ,fixm_GroundspeedRange(r,r,t,t)

  % aixm_Note(Graph, Note, PropertyName?, Purpose?, TranslatedNote*)
  ,aixm_Note(r,r,t,t,t)

  % fixm_Pointout(Graph, Pointout, OriginatingUnit?, ReceivingUnit*)
  ,fixm_Pointout(r,r,t,t)

  % fixm_VerticalRange(Graph, VerticalRange, LowerBound?, UpperBound?)
  ,fixm_VerticalRange(r,r,t,t)

  % fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel?, EstimatedTime?, Constraint*)
  ,fixm_ExpandedRoutePoint(r,r,t,t,t)

  % aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)
  ,aixm_ElevatedSurface(r,r,t,t,t,t)

  % fixm_Dimensions(Graph, Dimensions, Height?, Length?, Width?)
  ,fixm_Dimensions(r,r,t,t,t)

  % fixm_StandPositionAndTime(Graph, StandPositionAndTime, StandName?, StandTime?, TerminalName?)
  ,fixm_StandPositionAndTime(r,r,t,t,t)

  % fixm_RouteSegment(Graph, RouteSegment, Airway?, RoutePoint?)
  ,fixm_RouteSegment(r,r,t,t)

  % aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator?, Flight*, Aircraft*, Weather*, SubCondition*)
  ,aixm_ConditionCombination(r,r,t,t,t,t,t)

  % aixm_SurfaceContaminationLayer(Graph, SurfaceContaminationLayer, LayerOrder?, Type?, Extent*, Annotation*)
  ,aixm_SurfaceContaminationLayer(r,r,t,t,t,t)

  % fixm_Organization(Graph, Organization, Name?, OtherOrganization?, Contact?)
  ,fixm_Organization(r,r,t,t,t)

  % aixm_OrganisationAuthorityAssociation(Graph, OrganisationAuthorityAssociation, Type?, Annotation*, TheOrganisationAuthority)
  ,aixm_OrganisationAuthorityAssociation(r,r,t,t,t)

  % aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)
  ,aixm_ElevatedPoint(r,r,t,t,t,t)

  % fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel?)
  ,fixm_EfplPoint4D(r,r,t)

  % fixm_AircraftOperator(Graph, AircraftOperator, OperatingOrganization?, OperatorCategory?)
  ,fixm_AircraftOperator(r,r,t,t)

  % gml_Point(Graph, Point)
  ,gml_Point(r,r)

  % fixm_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair)
  ,fixm_EfplTrajectoryRoutePair(r,r)

  % fixm_RoutePoint(Graph, RoutePoint, Constraint*)
  ,fixm_RoutePoint(r,r,t)

  % fixm_BeaconCodeAssignment(Graph, BeaconCodeAssignment, CurrentBeaconCode?, PreviousBeaconCode?, ReassignedBeaconCode?, ReassigningUnit?)
  ,fixm_BeaconCodeAssignment(r,r,t,t,t,t)

  % fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile*, DescentProfile*)
  ,fixm_FlightPerformanceData(r,r,t,t)

  % fixm_ExpandedRoute(Graph, ExpandedRoute, RoutePoint*)
  ,fixm_ExpandedRoute(r,r,t)

  % fixm_RouteConstraintOrPreference(Graph, RouteConstraintOrPreference, ConstraintType?)
  ,fixm_RouteConstraintOrPreference(r,r,t)

  % fixm_DeclarationText(Graph, DeclarationText, Compliance?, Consignor?, Shipper?)
  ,fixm_DeclarationText(r,r,t,t,t)

  % fixm_EstimatedElapsedTime(Graph, EstimatedElapsedTime, ElapsedTime?, Location?)
  ,fixm_EstimatedElapsedTime(r,r,t,t)

  % fixm_ReportedTime(Graph, ReportedTime, Provenance?, Time?)
  ,fixm_ReportedTime(r,r,t,t)

  % fixm_GeographicLocation(Graph, GeographicLocation, Pos, SrsName?)
  ,fixm_GeographicLocation(r,r,t,t)

  % aixm_LinguisticNote(Graph, LinguisticNote, Note?)
  ,aixm_LinguisticNote(r,r,t)

  % aixm_Meteorology(Graph, Meteorology, FlightConditions?, Visibility?, VisibilityInterpretation?, RunwayVisualRange?, RunwayVisualRangeInterpretation?, Annotation*)
  ,aixm_Meteorology(r,r,t,t,t,t,t,t)

  % fixm_PointRange(Graph, PointRange, LateralRange?, VerticalRange?, TemporalRange?)
  ,fixm_PointRange(r,r,t,t,t)

  % aixm_City(Graph, City, Name?, Annotation*)
  ,aixm_City(r,r,t,t)

  % aixm_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role?, TheOrganisationAuthority)
  ,aixm_AirportHeliportResponsibilityOrganisation(r,r,t,t)

  % fixm_AirspeedRange(Graph, AirspeedRange, LowerSpeed?, UpperSpeed?)
  ,fixm_AirspeedRange(r,r,t,t)

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

  % fixm_PlannedReportingPosition(Graph, PlannedReportingPosition, Position?, PositionAltitude?, PositionEstimatedTime?)
  ,fixm_PlannedReportingPosition(r,r,t,t,t)

  % fixm_SignificantPoint(Graph, SignificantPoint)
  ,fixm_SignificantPoint(r,r)

  % fixm_SupplementalData(Graph, SupplementalData, FuelEndurance?, PersonsOnBoard?, PilotInCommand?)
  ,fixm_SupplementalData(r,r,t,t,t)

  % fixm_DangerousGoods(Graph, DangerousGoods, GuidebookNumber?, OnboardLocation?, HandlingInformation?, AircraftLimitation?, AirWayBill?, Shipment?, PackageGroup*, ShippingInformation?)
  ,fixm_DangerousGoods(r,r,t,t,t,t,t,t,t,t)

  % fixm_DangerousGoodsPackageGroup(Graph, DangerousGoodsPackageGroup, ShipmentDimensions?, DangerousGoodsPackage*, ShipmentUseIndicator?)
  ,fixm_DangerousGoodsPackageGroup(r,r,t,t,t)

  % fixm_OfftrackDistance(Graph, OfftrackDistance, Distance?, Direction?)
  ,fixm_OfftrackDistance(r,r,t,t)

  % fixm_Handoff(Graph, Handoff, ReceivingUnit?, TransferringUnit?, CoordinationStatus?)
  ,fixm_Handoff(r,r,t,t,t)

  % fixm_TrajectoryChange(Graph, TrajectoryChange, ConstrainedAirspace?, SpecialActivityAirspace?)
  ,fixm_TrajectoryChange(r,r,t,t)

  % fixm_ContactInformation(Graph, ContactInformation, Name?, Title?, OnlineContact?, PhoneFax?, Address?)
  ,fixm_ContactInformation(r,r,t,t,t,t,t)

  % aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator?, Name?, LocationIndicatorICAO?, DesignatorIATA?, Type?, CertifiedICAO?, PrivateUse?, ControlType?, FieldElevation?, FieldElevationAccuracy?, VerticalDatum?, MagneticVariation?, MagneticVariationAccuracy?, DateMagneticVariation?, MagneticVariationChange?, ReferenceTemperature?, AltimeterCheckLocation?, SecondaryPowerSupply?, WindDirectionIndicator?, LandingDirectionIndicator?, TransitionAltitude?, TransitionLevel?, LowestTemperature?, Abandoned?, CertificationDate?, CertificationExpirationDate?, Contact*, Annotation*, ARP?, AltimeterSource*, Contaminant*, ServedCity*, ResponsibleOrganisation?, AviationBoundary?, Availability*)
  ,aixm_AirportHeliportTimeSlice(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_Point4D(Graph, Point4D, Altitude?, Time?, PointRange?)
  ,fixm_Point4D(r,r,t,t,t)

  % fixm_AbstractRoutePoint(Graph, AbstractRoutePoint, AirTrafficType?, DelayAtPoint?, FlightRules?, Point?, ClearanceLimit?)
  ,fixm_AbstractRoutePoint(r,r,t,t,t,t,t)

  % aixm_Ridge(Graph, Ridge, Side?, Distance?, Depth?, Annotation*)
  ,aixm_Ridge(r,r,t,t,t,t)

  % fixm_DepartureActivityTimes(Graph, DepartureActivityTimes, BoardingTime?, DeIcingTime?, GroundHandlingTime?, StartupTime?)
  ,fixm_DepartureActivityTimes(r,r,t,t,t,t)

  % fixm_EnRouteDiversion(Graph, EnRouteDiversion, DiversionRecoveryInformation?)
  ,fixm_EnRouteDiversion(r,r,t)

  % fixm_ActualSpeed(Graph, ActualSpeed, Calculated?, PilotReported?, Surveillance?)
  ,fixm_ActualSpeed(r,r,t,t,t)

  % fixm_FlightEmergency(Graph, FlightEmergency, ActionTaken?, EmergencyDescription?, Originator?, OtherInformation?, Phase?, Contact?)
  ,fixm_FlightEmergency(r,r,t,t,t,t,t,t)

  % fixm_Flight(Graph, Flight, ControllingUnit?, Extensions*, FlightFiler?, Gufi?, Remarks?, AircraftDescription?, DangerousGoods*, RankedTrajectories*, RouteToRevisedDestination?, Negotiating?, Agreed?, Arrival?, Departure?, Emergency?, RadioCommunicationFailure?, EnRoute?, Operator?, EnRouteDiversion?, FlightType?, FlightStatus?, Originator?, SupplementalData?, FlightIdentification?, SpecialHandling*)
  ,fixm_Flight(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % aixm_PropertiesWithSchedule(Graph, PropertiesWithSchedule, Annotation*, SpecialDateAuthority*, TimeInterval*)
  ,aixm_PropertiesWithSchedule(r,r,t,t,t)

  % gml_Surface(Graph, Surface, Patch+)
  ,gml_Surface(r,r,t)

  % fixm_ClearedFlightInformation(Graph, ClearedFlightInformation, ClearedFlightLevel?, ClearedSpeed?, Heading?, OfftrackClearance?, RateOfClimbDescend?, DirectRouting?)
  ,fixm_ClearedFlightInformation(r,r,t,t,t,t,t,t)

  % fixm_TrajectoryRoutePair(Graph, TrajectoryRoutePair, Trajectory?, Route?)
  ,fixm_TrajectoryRoutePair(r,r,t,t)

  % fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit?, UpstreamUnit?, BoundaryCrossingProposed?, BoundaryCrossingCoordinated?, Handoff?, UnitBoundaryIndicator?)
  ,fixm_UnitBoundary(r,r,t,t,t,t,t,t)

  % aixm_SurfaceContamination(Graph, SurfaceContamination, ObservationTime?, Depth?, FrictionCoefficient?, FrictionEstimation?, FrictionDevice?, ObscuredLights?, FurtherClearanceTime?, FurtherTotalClearance?, NextObservationTime?, Proportion?, CriticalRidge*, Annotation*, Layer*)
  ,aixm_SurfaceContamination(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_MeteorologicalData(Graph, MeteorologicalData, Temperature?, WindDirection?, WindSpeed?)
  ,fixm_MeteorologicalData(r,r,t,t,t)

  % aixm_OrganisationAuthority(Graph, OrganisationAuthority, TimeSlice*)
  ,aixm_OrganisationAuthority(r,r,t)

  % fixm_TelephoneContact(Graph, TelephoneContact, Voice?, Facimile?)
  ,fixm_TelephoneContact(r,r,t,t)

  % fixm_ShippingInformation(Graph, ShippingInformation, AerodromeOfLoading?, AerodromeOfUnloading?, DangerousGoodsScreeningLocation?, DepartureCountry?, DestinationCountry?, OriginCountry?, ShipmentAuthorizations?, SubsidiaryHazardClassAndDivision?, SupplementaryInformation?, TransferAerodromes*, DeclarationText?, Consignee?, Shipper?)
  ,fixm_ShippingInformation(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % aixm_AirportHeliportContamination(Graph, AirportHeliportContamination)
  ,aixm_AirportHeliportContamination(r,r)

  % fixm_OtherInformation(Graph, OtherInformation, ReplacementFlightPlanIndicator?, RunwayVisualRange?)
  ,fixm_OtherInformation(r,r,t,t)

  % fixm_DinghyColour(Graph, DinghyColour)
  ,fixm_DinghyColour(r,r)

  % fixm_CpdlcConnection(Graph, CpdlcConnection, ReceivingUnitFrequency?, AtnLogonParameters?, SendCpldcIndicator?, ConnectionStatus?, FrequencyUsage?, Fans1ALogonParameters?)
  ,fixm_CpdlcConnection(r,r,t,t,t,t,t,t)

  % aixm_TelephoneContact(Graph, TelephoneContact, Voice?, Facsimile?)
  ,aixm_TelephoneContact(r,r,t,t)

  % fixm_Route(Graph, Route, AirfileRouteStartTime?, FlightDuration?, InitialCruisingSpeed?, InitialFlightRules?, RequestedAltitude?, RouteText?, EstimatedElapsedTime*, ExpandedRoute?, ClimbSchedule?, DescentSchedule?, Segment*)
  ,fixm_Route(r,r,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_Person(Graph, Person, Name?, Contact?)
  ,fixm_Person(r,r,t,t)

  % fixm_EfplFlight(Graph, EfplFlight, IfplId?, TotalEstimatedElapsedTime?, AerodromesOfDestination?, EfplSpecialHandling?, EfplFiledTrajectory?, EfplAcceptedTrajectory?, OtherInformation?, FlightPerformanceData?)
  ,fixm_EfplFlight(r,r,t,t,t,t,t,t,t,t)

  % fixm_Originator(Graph, Originator)
  ,fixm_Originator(r,r)

  % fixm_FlightStatus(Graph, FlightStatus, AirborneHold?, Airfile?, Accepted?, FlightCycle?, MissedApproach?, Suspended?)
  ,fixm_FlightStatus(r,r,t,t,t,t,t,t)

  % fixm_IdentifiedUnitReference(Graph, IdentifiedUnitReference, UnitIdentifier?)
  ,fixm_IdentifiedUnitReference(r,r,t)

  % fixm_Radionuclide(Graph, Radionuclide, PhysicalChemicalForm?, RadionuclideId?, RadionuclideName?, LowDispersibleMaterialIndicator?, Activity?, SpecialFormIndicator?)
  ,fixm_Radionuclide(r,r,t,t,t,t,t,t)

  % aixm_OnlineContact(Graph, OnlineContact, Network?, Linkage?, Protocol?, EMail?)
  ,aixm_OnlineContact(r,r,t,t,t,t)

  % fixm_StructuredPostalAddress(Graph, StructuredPostalAddress)
  ,fixm_StructuredPostalAddress(r,r)

  % fixm_AircraftPosition(Graph, AircraftPosition, Altitude?, Position?, PositionTime?, Track?, ActualSpeed?, NextPosition?, ReportSource?, FollowingPosition?)
  ,fixm_AircraftPosition(r,r,t,t,t,t,t,t,t,t)

  % aixm_AirportHeliportUsage(Graph, AirportHeliportUsage, Operation?)
  ,aixm_AirportHeliportUsage(r,r,t)

  % aixm_Timesheet(Graph, Timesheet, TimeReference?, StartDate?, EndDate?, Day?, DayTil?, StartTime?, StartEvent?, StartTimeRelativeEvent?, StartEventInterpretation?, EndTime?, EndEvent?, EndTimeRelativeEvent?, EndEventInterpretation?, DaylightSavingAdjust?, Excluded?, Annotation*)
  ,aixm_Timesheet(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % gml_SurfacePatch(Graph, SurfacePatch)
  ,gml_SurfacePatch(r,r)

  % fixm_MultiTime(Graph, MultiTime, Actual?, Estimated?)
  ,fixm_MultiTime(r,r,t,t)

  % aixm_FlightCharacteristic(Graph, FlightCharacteristic, Type?, Rule?, Status?, Military?, Origin?, Purpose?, Annotation*)
  ,aixm_FlightCharacteristic(r,r,t,t,t,t,t,t,t)

  % fixm_Provenance(Graph, Provenance, Timestamp?, Centre?, Source?, System?)
  ,fixm_Provenance(r,r,t,t,t,t)

  % aixm_AirportHeliport(Graph, AirportHeliport, TimeSlice*)
  ,aixm_AirportHeliport(r,r,t)

  % fixm_TrajectoryPoint(Graph, TrajectoryPoint, AltimeterSetting?, PredictedAirspeed?, PredictedGroundspeed?, MetData?, Point?, TrajectoryChange*, TrajectoryChangeType*, ReferencePoint?)
  ,fixm_TrajectoryPoint(r,r,t,t,t,t,t,t,t,t)

  % fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier?, DistanceFromTakeOff?, EfplEstimatedSpeed?, ElapsedTime?, GrossWeight?, TrajectoryPointType?, TrajectoryPointRole?, InboundSegment?)
  ,fixm_EfplTrajectoryPoint(r,r,t,t,t,t,t,t,t,t)

  % fixm_Temperatures(Graph, Temperatures, ControlTemperature?, EmergencyTemperature?, FlashpointTemperature?)
  ,fixm_Temperatures(r,r,t,t,t)

  % fixm_TrajectorySegment(Graph, TrajectorySegment, SegmentIdentifier?, SegmentType?)
  ,fixm_TrajectorySegment(r,r,t,t)

  % fixm_RunwayPositionAndTime(Graph, RunwayPositionAndTime, RunwayName?, RunwayTime?)
  ,fixm_RunwayPositionAndTime(r,r,t,t)

  % fixm_Feature(Graph, Feature, Provenance?)
  ,fixm_Feature(r,r,t)

  % fixm_FlightIdentification(Graph, FlightIdentification, AircraftIdentification?, MajorCarrierIdentifier?, MarketingCarrierFlightIdentifier*)
  ,fixm_FlightIdentification(r,r,t,t,t)

  % fixm_LastContact(Graph, LastContact, ContactFrequency?, LastContactTime?, LastContactUnit?, Position?)
  ,fixm_LastContact(r,r,t,t,t,t)

  % fixm_ElapsedTimeLocation(Graph, ElapsedTimeLocation)
  ,fixm_ElapsedTimeLocation(r,r)

  % aixm_Surface(Graph, Surface, HorizontalAccuracy?, Annotation*)
  ,aixm_Surface(r,r,t,t)

  % gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition)
  ,gml_TimePeriod(r,r,t,t)

  % fixm_AircraftCapabilities(Graph, AircraftCapabilities, Survival?, Communication?, Navigation?, Surveillance?, StandardCapabilities?)
  ,fixm_AircraftCapabilities(r,r,t,t,t,t,t)

  % fixm_SpeedSchedule(Graph, SpeedSchedule, InitialSpeed?, SubsequentSpeed?)
  ,fixm_SpeedSchedule(r,r,t,t)

  % aixm_OrganisationAuthorityTimeSlice(Graph, OrganisationAuthorityTimeSlice, Name?, Designator?, Type?, Military?, Annotation*, Contact*, RelatedOrganisationAuthority*)
  ,aixm_OrganisationAuthorityTimeSlice(r,r,t,t,t,t,t,t,t)

  % fixm_EnRoute(Graph, EnRoute, AlternateAerodrome*, FleetPrioritization?, BoundaryCrossings*, CpdlcConnection?, BeaconCodeAssignment?, Cleared?, ControlElement*, Pointout?, Position?)
  ,fixm_EnRoute(r,r,t,t,t,t,t,t,t,t,t)

  % fixm_FlightLevel(Graph, FlightLevel, Level?, Unit?)
  ,fixm_FlightLevel(r,r,t,t)

  % fixm_LateralOfftrack(Graph, LateralOfftrack, OfftrackDistance, OfftrackReason?)
  ,fixm_LateralOfftrack(r,r,t,t)

  % fixm_TemporalRange(Graph, TemporalRange, Earliest?, Latest?)
  ,fixm_TemporalRange(r,r,t,t)

  % fixm_Aircraft(Graph, Aircraft, AircraftColours?, AircraftQuantity?, EngineType?, AircraftAddress?, Capabilities?, Registration?, AircraftType?, WakeTurbulence?, AircraftPerformance?)
  ,fixm_Aircraft(r,r,t,t,t,t,t,t,t,t,t)

  % fixm_OnlineContact(Graph, OnlineContact, Email?)
  ,fixm_OnlineContact(r,r,t)

  % fixm_AirspaceConstraint(Graph, AirspaceConstraint, AirspaceControlledEntryTime?, ConstrainedAirspace?)
  ,fixm_AirspaceConstraint(r,r,t,t)

  % fixm_TimeSequence(Graph, TimeSequence, Approval?, Begin?, End?, Ready?, Request?)
  ,fixm_TimeSequence(r,r,t,t,t,t,t)

  % fixm_AdditionalHandlingInformation(Graph, AdditionalHandlingInformation, ResponsibleAgent?)
  ,fixm_AdditionalHandlingInformation(r,r,t)

  % fixm_AtcUnitReference(Graph, AtcUnitReference, SectorIdentifier?, Delegated?)
  ,fixm_AtcUnitReference(r,r,t,t)

  % fixm_Extension(Graph, Extension)
  ,fixm_Extension(r,r)

  % fixm_SurveillanceCapabilities(Graph, SurveillanceCapabilities, OtherSurveillanceCapabilities?, SurveillanceCode*)
  ,fixm_SurveillanceCapabilities(r,r,t,t)

  % fixm_Trajectory(Graph, Trajectory, TrajectoryPoint*)
  ,fixm_Trajectory(r,r,t)

  % aixm_AltimeterSourceTimeSlice(Graph, AltimeterSourceTimeSlice, IsRemote?, IsPrimary?, Availability*, Annotation*)
  ,aixm_AltimeterSourceTimeSlice(r,r,t,t,t,t)

  % aixm_Point(Graph, Point, HorizontalAccuracy?, Annotation*)
  ,aixm_Point(r,r,t,t)

  % aixm_AircraftCharacteristic(Graph, AircraftCharacteristic, Type?, Engine?, NumberEngine?, TypeAircraftICAO?, AircraftLandingCategory?, WingSpan?, WingSpanInterpretation?, ClassWingSpan?, Weight?, WeightInterpretation?, Passengers?, PassengersInterpretation?, Speed?, SpeedInterpretation?, WakeTurbulence?, NavigationEquipment?, NavigationSpecification?, VerticalSeparationCapability?, AntiCollisionAndSeparationEquipment?, CommunicationEquipment?, SurveillanceEquipment?, Annotation*)
  ,aixm_AircraftCharacteristic(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % aixm_PostalAddress(Graph, PostalAddress, DeliveryPoint?, City?, AdministrativeArea?, PostalCode?, Country?)
  ,aixm_PostalAddress(r,r,t,t,t,t,t)

  % fixm_DangerousGoodsPackage(Graph, DangerousGoodsPackage, DangerousGoodsQuantity?, PackageDimensions?, PackingInstructionNumber?, ProductName?, ProperShippingName?, ReportableQuantity?, SupplementaryInformation?, TechnicalName?, TypeOfPackaging?, UnNumber?, DangerousGoodsLimitation?, ShipmentType?, AllPackedInOne?, CompatibilityGroup?, ShipmentDimensions?, MarinePollutantIndicator?, RadioactiveMaterials?, HazardClass?, PackingGroup?, Temperatures?, OverpackIndicator?, SubsidiaryHazardClass)
  ,fixm_DangerousGoodsPackage(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_LastPositionReport(Graph, LastPositionReport, DeterminationMethod?, Position?, TimeAtPosition?)
  ,fixm_LastPositionReport(r,r,t,t,t)

  % aixm_AltimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus?)
  ,aixm_AltimeterSourceStatus(r,r,t)

  % fixm_DangerousGoodsDimensions(Graph, DangerousGoodsDimensions, GrossWeight?, NetWeight?, Volume?)
  ,fixm_DangerousGoodsDimensions(r,r,t,t,t)

  % fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules?)
  ,fixm_EfplRoute(r,r,t)

  % fixm_CoordinationStatus(Graph, CoordinationStatus, AbrogationReason?, CoordinationStatus?, NonStandardCommunicationReason?, ReleaseConditions?)
  ,fixm_CoordinationStatus(r,r,t,t,t,t)

  % fixm_BoundaryCrossing(Graph, BoundaryCrossing, Altitude?, CrossingPoint?, CrossingSpeed?, CrossingTime?, Offtrack?, AltitudeInTransition?)
  ,fixm_BoundaryCrossing(r,r,t,t,t,t,t,t)

  % fixm_IcaoAerodromeReference(Graph, IcaoAerodromeReference, Code?)
  ,fixm_IcaoAerodromeReference(r,r,t)

  % fixm_RadioCommunicationFailure(Graph, RadioCommunicationFailure, RadioFailureRemarks?, RemainingComCapability?, Contact?)
  ,fixm_RadioCommunicationFailure(r,r,t,t,t)

  % aixm_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus?, Warning?, Usage*)
  ,aixm_AirportHeliportAvailability(r,r,t,t,t)

  % fixm_FlightArrival(Graph, FlightArrival, ApproachFix?, ApproachTime?, ArrivalAerodrome?, ArrivalAerodromeAlternate*, ArrivalAerodromeOriginal?, ArrivalFix?, ArrivalFixTime?, ArrivalFleetPrioritization?, ArrivalSequenceNumber?, EarliestInBlockTime?, FiledRevisedDestinationAerodrome?, FiledRevisedDestinationStar?, RunwayPositionAndTime?, StandardInstrumentArrival?, StandPositionAndTime?, LandingLimits?)
  ,fixm_FlightArrival(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_RadioactiveMaterial(Graph, RadioactiveMaterial, CriticalitySafetyIndex?, TransportIndex?, FissileExceptedIndicator?, Category?, Radionuclide?)
  ,fixm_RadioactiveMaterial(r,r,t,t,t,t,t)

  % fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled?, Initial?)
  ,fixm_ExtendedMultiTime(r,r,t,t)

  % fixm_ControlElement(Graph, ControlElement)
  ,fixm_ControlElement(r,r)

  % fixm_AerodromesOfDestination(Graph, AerodromesOfDestination, AerodromeOfDestination?, Alternate1?, Alternate2?, FiledRevisedDestinationAerodrome?)
  ,fixm_AerodromesOfDestination(r,r,t,t,t,t)

  % fixm_AllPackedInOne(Graph, AllPackedInOne, NumberOfPackages?, QValue?)
  ,fixm_AllPackedInOne(r,r,t,t)

  % aixm_AltimeterSource(Graph, AltimeterSource, TimeSlice*)
  ,aixm_AltimeterSource(r,r,t)

  % fixm_SurvivalCapabilities(Graph, SurvivalCapabilities, SurvivalEquipmentRemarks?, DinghyInformation?, EmergencyRadioCode*, LifeJacketCode*, SurvivalEquipmentCode*)
  ,fixm_SurvivalCapabilities(r,r,t,t,t,t,t)

  % fixm_DirectRouting(Graph, DirectRouting, From?, To?)
  ,fixm_DirectRouting(r,r,t,t)

  % fixm_TargetMultiTime(Graph, TargetMultiTime, Target?)
  ,fixm_TargetMultiTime(r,r,t)

  % fixm_AircraftType(Graph, AircraftType)
  ,fixm_AircraftType(r,r)

  % fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome?, DepartureFix?, DepartureFixTime?, DepartureFleetPrioritization?, DepartureSlot?, EarliestOffBlockTime?, OffBlockReadyTime?, RunwayPositionAndTime?, StandardInstrumentDeparture?, StandPositionAndTime?, TakeoffAlternateAerodrome*, TakeoffWeight?, DepartureTimes?)
  ,fixm_FlightDeparture(r,r,t,t,t,t,t,t,t,t,t,t,t,t,t)

  % fixm_AerodromeReference(Graph, AerodromeReference)
  ,fixm_AerodromeReference(r,r)

  % fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime?, TaxiTime?)
  ,fixm_EfplFlightDeparture(r,r,t,t)

  % aixm_UsageCondition(Graph, UsageCondition, Type?, PriorPermission?, Selection?, Annotation*, Contact*)
  ,aixm_UsageCondition(r,r,t,t,t,t,t)
.

% fixm_PostalAddress(Graph, PostalAddress, AdministrativeArea?, PostalCode?, DeliveryPoint?, CountryCode?, CountryName?, City?)

% fixm_NavigationCapabilities(Graph, NavigationCapabilities, OtherNavigationCapabilities?, PerformanceBasedCode*, NavigationCode*)
fixm_NavigationCapabilities(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', val('RNVD1E2A1'), ['val:B1', 'val:A1', 'val:D1', 'val:L1', 'val:O1', 'val:S1', 'val:C1', 'val:S2'], ['val:I', 'val:A', 'val:D', 'val:W', 'val:X', 'val:G']).
fixm_NavigationCapabilities(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', val('RNVD1E2A1'), ['val:B1', 'val:A1', 'val:D1', 'val:L1', 'val:O1', 'val:S1', 'val:C1', 'val:S2'], ['val:I', 'val:A', 'val:D', 'val:W', 'val:X', 'val:G']).
fixm_NavigationCapabilities(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', val('RNVD1E2A1'), ['val:B1', 'val:A1', 'val:D1', 'val:L1', 'val:O1', 'val:S1', 'val:C1', 'val:S2'], ['val:I', 'val:A', 'val:D', 'val:W', 'val:X', 'val:G']).
fixm_NavigationCapabilities(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', val('RNVD1E2A1'), ['val:B1', 'val:A1', 'val:D1', 'val:L1', 'val:O1', 'val:S1', 'val:C1', 'val:S2'], ['val:I', 'val:A', 'val:D', 'val:W', 'val:X', 'val:G']).
fixm_NavigationCapabilities(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', val('RNVD1E2A1'), ['val:B1', 'val:A1', 'val:D1', 'val:L1', 'val:O1', 'val:S1', 'val:C1', 'val:S2'], ['val:I', 'val:A', 'val:D', 'val:W', 'val:X', 'val:G']).
fixm_NavigationCapabilities(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', val('RNVD1E2A1'), ['val:B1', 'val:A1', 'val:D1', 'val:L1', 'val:O1', 'val:S1', 'val:C1', 'val:S2'], ['val:I', 'val:A', 'val:D', 'val:W', 'val:X', 'val:G']).
fixm_NavigationCapabilities(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', val('RNVD1E2A1'), ['val:B1', 'val:A1', 'val:D1', 'val:L1', 'val:O1', 'val:S1', 'val:C1', 'val:S2'], ['val:I', 'val:A', 'val:D', 'val:W', 'val:X', 'val:G']).
fixm_NavigationCapabilities(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', val('RNVD1E2A1'), ['val:B1', 'val:A1', 'val:D1', 'val:L1', 'val:O1', 'val:S1', 'val:C1', 'val:S2'], ['val:I', 'val:A', 'val:D', 'val:W', 'val:X', 'val:G']).
fixm_NavigationCapabilities(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', val('RNVD1E2A1'), ['val:B1', 'val:A1', 'val:D1', 'val:L1', 'val:O1', 'val:S1', 'val:C1', 'val:S2'], ['val:I', 'val:A', 'val:D', 'val:W', 'val:X', 'val:G']).
fixm_NavigationCapabilities(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', val('RNVD1E2A1'), ['val:B1', 'val:A1', 'val:D1', 'val:L1', 'val:O1', 'val:S1', 'val:C1', 'val:S2'], ['val:I', 'val:A', 'val:D', 'val:W', 'val:X', 'val:G']).

% fixm_GroundspeedRange(Graph, GroundspeedRange, LowerSpeed?, UpperSpeed?)

% aixm_Note(Graph, Note, PropertyName?, Purpose?, TranslatedNote*)
aixm_Note(graph:'6_donlon-data.ttl', s1:'n003', val('aRP'), '$null$', [s1:'ln003']).
aixm_Note(graph:'0_donlon-data.ttl', s1:'n003', val('aRP'), '$null$', [s1:'ln003']).
aixm_Note(graph:'5_donlon-data.ttl', s1:'n002', val('fieldElevation'), '$null$', [s1:'ln002']).
aixm_Note(graph:'7_donlon-data.ttl', s1:'n002', val('fieldElevation'), '$null$', [s1:'ln002']).
aixm_Note(graph:'4_donlon-data.ttl', s1:'n003', val('aRP'), '$null$', [s1:'ln003']).
aixm_Note(graph:'8_donlon-data.ttl', s1:'n003', val('aRP'), '$null$', [s1:'ln003']).
aixm_Note(graph:'9_donlon-data.ttl', s1:'n002', val('fieldElevation'), '$null$', [s1:'ln002']).
aixm_Note(graph:'2_donlon-data.ttl', s1:'n003', val('aRP'), '$null$', [s1:'ln003']).
aixm_Note(graph:'3_donlon-data.ttl', s1:'n002', val('fieldElevation'), '$null$', [s1:'ln002']).
aixm_Note(graph:'1_donlon-data.ttl', s1:'n002', val('fieldElevation'), '$null$', [s1:'ln002']).
aixm_Note(graph:'0_donlon-data.ttl', s1:'n002', val('fieldElevation'), '$null$', [s1:'ln002']).
aixm_Note(graph:'5_donlon-data.ttl', s1:'n003', val('aRP'), '$null$', [s1:'ln003']).
aixm_Note(graph:'6_donlon-data.ttl', s1:'n002', val('fieldElevation'), '$null$', [s1:'ln002']).
aixm_Note(graph:'4_donlon-data.ttl', s1:'n002', val('fieldElevation'), '$null$', [s1:'ln002']).
aixm_Note(graph:'7_donlon-data.ttl', s1:'n003', val('aRP'), '$null$', [s1:'ln003']).
aixm_Note(graph:'8_donlon-data.ttl', s1:'n002', val('fieldElevation'), '$null$', [s1:'ln002']).
aixm_Note(graph:'9_donlon-data.ttl', s1:'n003', val('aRP'), '$null$', [s1:'ln003']).
aixm_Note(graph:'2_donlon-data.ttl', s1:'n002', val('fieldElevation'), '$null$', [s1:'ln002']).
aixm_Note(graph:'3_donlon-data.ttl', s1:'n003', val('aRP'), '$null$', [s1:'ln003']).
aixm_Note(graph:'1_donlon-data.ttl', s1:'n003', val('aRP'), '$null$', [s1:'ln003']).

% fixm_Pointout(Graph, Pointout, OriginatingUnit?, ReceivingUnit*)

% fixm_VerticalRange(Graph, VerticalRange, LowerBound?, UpperBound?)

% fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel?, EstimatedTime?, Constraint*)

% aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)

% fixm_Dimensions(Graph, Dimensions, Height?, Length?, Width?)

% fixm_StandPositionAndTime(Graph, StandPositionAndTime, StandName?, StandTime?, TerminalName?)

% fixm_RouteSegment(Graph, RouteSegment, Airway?, RoutePoint?)

% aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator?, Flight*, Aircraft*, Weather*, SubCondition*)
aixm_ConditionCombination(graph:'3_donlon-data.ttl', s2:'ID_ACT_15', val('OR'), [], [], [], [s2:'ID_ACT_16', s2:'ID_ACT_20', s2:'ID_ACT_18']).
aixm_ConditionCombination(graph:'6_donlon-data.ttl', s2:'ID_ACT_16', val('NONE'), [s2:'ID_ACT_17'], [], [], []).
aixm_ConditionCombination(graph:'2_donlon-data.ttl', s2:'ID_ACT_18', val('NONE'), [s2:'ID_ACT_19'], [], [], []).
aixm_ConditionCombination(graph:'6_donlon-data.ttl', s2:'ID_ACT_20', val('NONE'), [s2:'ID_ACT_21'], [], [], []).
aixm_ConditionCombination(graph:'3_donlon-data.ttl', s1:'agtayyat', val('NONE'), [s1:'F_yastadyt'], [], [], []).
aixm_ConditionCombination(graph:'0_donlon-data.ttl', s1:'agtayyat', val('NONE'), [s1:'F_yastadyt'], [], [], []).
aixm_ConditionCombination(graph:'5_donlon-data.ttl', s2:'ID_ACT_16', val('NONE'), [s2:'ID_ACT_17'], [], [], []).
aixm_ConditionCombination(graph:'8_donlon-data.ttl', s1:'agtayyat', val('NONE'), [s1:'F_yastadyt'], [], [], []).
aixm_ConditionCombination(graph:'0_donlon-data.ttl', s2:'ID_ACT_15', val('OR'), [], [], [], [s2:'ID_ACT_16', s2:'ID_ACT_20', s2:'ID_ACT_18']).
aixm_ConditionCombination(graph:'5_donlon-data.ttl', s2:'ID_ACT_20', val('NONE'), [s2:'ID_ACT_21'], [], [], []).
aixm_ConditionCombination(graph:'8_donlon-data.ttl', s2:'ID_ACT_15', val('OR'), [], [], [], [s2:'ID_ACT_16', s2:'ID_ACT_20', s2:'ID_ACT_18']).
aixm_ConditionCombination(graph:'1_donlon-data.ttl', s2:'ID_ACT_18', val('NONE'), [s2:'ID_ACT_19'], [], [], []).
aixm_ConditionCombination(graph:'4_donlon-data.ttl', s2:'ID_ACT_16', val('NONE'), [s2:'ID_ACT_17'], [], [], []).
aixm_ConditionCombination(graph:'0_donlon-data.ttl', s2:'ID_ACT_18', val('NONE'), [s2:'ID_ACT_19'], [], [], []).
aixm_ConditionCombination(graph:'8_donlon-data.ttl', s2:'ID_ACT_18', val('NONE'), [s2:'ID_ACT_19'], [], [], []).
aixm_ConditionCombination(graph:'1_donlon-data.ttl', s2:'ID_ACT_15', val('OR'), [], [], [], [s2:'ID_ACT_16', s2:'ID_ACT_20', s2:'ID_ACT_18']).
aixm_ConditionCombination(graph:'1_donlon-data.ttl', s1:'agtayyat', val('NONE'), [s1:'F_yastadyt'], [], [], []).
aixm_ConditionCombination(graph:'4_donlon-data.ttl', s2:'ID_ACT_20', val('NONE'), [s2:'ID_ACT_21'], [], [], []).
aixm_ConditionCombination(graph:'2_donlon-data.ttl', s2:'ID_ACT_15', val('OR'), [], [], [], [s2:'ID_ACT_16', s2:'ID_ACT_20', s2:'ID_ACT_18']).
aixm_ConditionCombination(graph:'7_donlon-data.ttl', s2:'ID_ACT_16', val('NONE'), [s2:'ID_ACT_17'], [], [], []).
aixm_ConditionCombination(graph:'9_donlon-data.ttl', s2:'ID_ACT_16', val('NONE'), [s2:'ID_ACT_17'], [], [], []).
aixm_ConditionCombination(graph:'3_donlon-data.ttl', s2:'ID_ACT_18', val('NONE'), [s2:'ID_ACT_19'], [], [], []).
aixm_ConditionCombination(graph:'2_donlon-data.ttl', s1:'agtayyat', val('NONE'), [s1:'F_yastadyt'], [], [], []).
aixm_ConditionCombination(graph:'9_donlon-data.ttl', s2:'ID_ACT_20', val('NONE'), [s2:'ID_ACT_21'], [], [], []).
aixm_ConditionCombination(graph:'7_donlon-data.ttl', s2:'ID_ACT_20', val('NONE'), [s2:'ID_ACT_21'], [], [], []).
aixm_ConditionCombination(graph:'2_donlon-data.ttl', s2:'ID_ACT_20', val('NONE'), [s2:'ID_ACT_21'], [], [], []).
aixm_ConditionCombination(graph:'6_donlon-data.ttl', s2:'ID_ACT_18', val('NONE'), [s2:'ID_ACT_19'], [], [], []).
aixm_ConditionCombination(graph:'7_donlon-data.ttl', s1:'agtayyat', val('NONE'), [s1:'F_yastadyt'], [], [], []).
aixm_ConditionCombination(graph:'9_donlon-data.ttl', s1:'agtayyat', val('NONE'), [s1:'F_yastadyt'], [], [], []).
aixm_ConditionCombination(graph:'9_donlon-data.ttl', s2:'ID_ACT_15', val('OR'), [], [], [], [s2:'ID_ACT_16', s2:'ID_ACT_20', s2:'ID_ACT_18']).
aixm_ConditionCombination(graph:'7_donlon-data.ttl', s2:'ID_ACT_15', val('OR'), [], [], [], [s2:'ID_ACT_16', s2:'ID_ACT_20', s2:'ID_ACT_18']).
aixm_ConditionCombination(graph:'2_donlon-data.ttl', s2:'ID_ACT_16', val('NONE'), [s2:'ID_ACT_17'], [], [], []).
aixm_ConditionCombination(graph:'5_donlon-data.ttl', s2:'ID_ACT_18', val('NONE'), [s2:'ID_ACT_19'], [], [], []).
aixm_ConditionCombination(graph:'4_donlon-data.ttl', s1:'agtayyat', val('NONE'), [s1:'F_yastadyt'], [], [], []).
aixm_ConditionCombination(graph:'1_donlon-data.ttl', s2:'ID_ACT_20', val('NONE'), [s2:'ID_ACT_21'], [], [], []).
aixm_ConditionCombination(graph:'1_donlon-data.ttl', s2:'ID_ACT_16', val('NONE'), [s2:'ID_ACT_17'], [], [], []).
aixm_ConditionCombination(graph:'4_donlon-data.ttl', s2:'ID_ACT_15', val('OR'), [], [], [], [s2:'ID_ACT_16', s2:'ID_ACT_20', s2:'ID_ACT_18']).
aixm_ConditionCombination(graph:'5_donlon-data.ttl', s1:'agtayyat', val('NONE'), [s1:'F_yastadyt'], [], [], []).
aixm_ConditionCombination(graph:'8_donlon-data.ttl', s2:'ID_ACT_16', val('NONE'), [s2:'ID_ACT_17'], [], [], []).
aixm_ConditionCombination(graph:'0_donlon-data.ttl', s2:'ID_ACT_16', val('NONE'), [s2:'ID_ACT_17'], [], [], []).
aixm_ConditionCombination(graph:'4_donlon-data.ttl', s2:'ID_ACT_18', val('NONE'), [s2:'ID_ACT_19'], [], [], []).
aixm_ConditionCombination(graph:'0_donlon-data.ttl', s2:'ID_ACT_20', val('NONE'), [s2:'ID_ACT_21'], [], [], []).
aixm_ConditionCombination(graph:'5_donlon-data.ttl', s2:'ID_ACT_15', val('OR'), [], [], [], [s2:'ID_ACT_16', s2:'ID_ACT_20', s2:'ID_ACT_18']).
aixm_ConditionCombination(graph:'8_donlon-data.ttl', s2:'ID_ACT_20', val('NONE'), [s2:'ID_ACT_21'], [], [], []).
aixm_ConditionCombination(graph:'6_donlon-data.ttl', s1:'agtayyat', val('NONE'), [s1:'F_yastadyt'], [], [], []).
aixm_ConditionCombination(graph:'3_donlon-data.ttl', s2:'ID_ACT_20', val('NONE'), [s2:'ID_ACT_21'], [], [], []).
aixm_ConditionCombination(graph:'9_donlon-data.ttl', s2:'ID_ACT_18', val('NONE'), [s2:'ID_ACT_19'], [], [], []).
aixm_ConditionCombination(graph:'7_donlon-data.ttl', s2:'ID_ACT_18', val('NONE'), [s2:'ID_ACT_19'], [], [], []).
aixm_ConditionCombination(graph:'6_donlon-data.ttl', s2:'ID_ACT_15', val('OR'), [], [], [], [s2:'ID_ACT_16', s2:'ID_ACT_20', s2:'ID_ACT_18']).
aixm_ConditionCombination(graph:'3_donlon-data.ttl', s2:'ID_ACT_16', val('NONE'), [s2:'ID_ACT_17'], [], [], []).

% aixm_SurfaceContaminationLayer(Graph, SurfaceContaminationLayer, LayerOrder?, Type?, Extent*, Annotation*)

% fixm_Organization(Graph, Organization, Name?, OtherOrganization?, Contact?)
fixm_Organization(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_017', val('DLH'), '$null$', '$null$').
fixm_Organization(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_017', val('DLH'), '$null$', '$null$').
fixm_Organization(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_017', val('DLH'), '$null$', '$null$').
fixm_Organization(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_017', val('DLH'), '$null$', '$null$').
fixm_Organization(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_017', val('DLH'), '$null$', '$null$').
fixm_Organization(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_017', val('DLH'), '$null$', '$null$').
fixm_Organization(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_017', val('DLH'), '$null$', '$null$').
fixm_Organization(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_017', val('DLH'), '$null$', '$null$').
fixm_Organization(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_017', val('DLH'), '$null$', '$null$').
fixm_Organization(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_017', val('DLH'), '$null$', '$null$').

% aixm_OrganisationAuthorityAssociation(Graph, OrganisationAuthorityAssociation, Type?, Annotation*, TheOrganisationAuthority)

% aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation?, GeoidUndulation?, VerticalDatum?, VerticalAccuracy?)
aixm_ElevatedPoint(graph:'0_donlon-data.ttl', s1:'elpoint1EADH', '$null$', '$null$', '$null$', '$null$').
aixm_ElevatedPoint(graph:'6_donlon-data.ttl', s1:'elpoint1EADH', '$null$', '$null$', '$null$', '$null$').
aixm_ElevatedPoint(graph:'8_donlon-data.ttl', s1:'elpoint1EADH', '$null$', '$null$', '$null$', '$null$').
aixm_ElevatedPoint(graph:'2_donlon-data.ttl', s1:'elpoint1EADH', '$null$', '$null$', '$null$', '$null$').
aixm_ElevatedPoint(graph:'7_donlon-data.ttl', s1:'elpoint1EADH', '$null$', '$null$', '$null$', '$null$').
aixm_ElevatedPoint(graph:'5_donlon-data.ttl', s1:'elpoint1EADH', '$null$', '$null$', '$null$', '$null$').
aixm_ElevatedPoint(graph:'3_donlon-data.ttl', s1:'elpoint1EADH', '$null$', '$null$', '$null$', '$null$').
aixm_ElevatedPoint(graph:'1_donlon-data.ttl', s1:'elpoint1EADH', '$null$', '$null$', '$null$', '$null$').
aixm_ElevatedPoint(graph:'4_donlon-data.ttl', s1:'elpoint1EADH', '$null$', '$null$', '$null$', '$null$').
aixm_ElevatedPoint(graph:'9_donlon-data.ttl', s1:'elpoint1EADH', '$null$', '$null$', '$null$', '$null$').

% fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel?)
fixm_EfplPoint4D(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034').
fixm_EfplPoint4D(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040').
fixm_EfplPoint4D(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031').
fixm_EfplPoint4D(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025').
fixm_EfplPoint4D(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037').
fixm_EfplPoint4D(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034').
fixm_EfplPoint4D(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040').
fixm_EfplPoint4D(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031').
fixm_EfplPoint4D(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025').
fixm_EfplPoint4D(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037').
fixm_EfplPoint4D(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034').
fixm_EfplPoint4D(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040').
fixm_EfplPoint4D(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031').
fixm_EfplPoint4D(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025').
fixm_EfplPoint4D(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037').
fixm_EfplPoint4D(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034').
fixm_EfplPoint4D(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040').
fixm_EfplPoint4D(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031').
fixm_EfplPoint4D(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025').
fixm_EfplPoint4D(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037').
fixm_EfplPoint4D(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034').
fixm_EfplPoint4D(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040').
fixm_EfplPoint4D(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031').
fixm_EfplPoint4D(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025').
fixm_EfplPoint4D(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037').
fixm_EfplPoint4D(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034').
fixm_EfplPoint4D(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040').
fixm_EfplPoint4D(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031').
fixm_EfplPoint4D(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025').
fixm_EfplPoint4D(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037').
fixm_EfplPoint4D(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034').
fixm_EfplPoint4D(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040').
fixm_EfplPoint4D(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031').
fixm_EfplPoint4D(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025').
fixm_EfplPoint4D(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037').
fixm_EfplPoint4D(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034').
fixm_EfplPoint4D(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040').
fixm_EfplPoint4D(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031').
fixm_EfplPoint4D(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025').
fixm_EfplPoint4D(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037').
fixm_EfplPoint4D(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034').
fixm_EfplPoint4D(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040').
fixm_EfplPoint4D(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031').
fixm_EfplPoint4D(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025').
fixm_EfplPoint4D(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037').
fixm_EfplPoint4D(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034').
fixm_EfplPoint4D(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040').
fixm_EfplPoint4D(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031').
fixm_EfplPoint4D(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025').
fixm_EfplPoint4D(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037').

% fixm_AircraftOperator(Graph, AircraftOperator, OperatingOrganization?, OperatorCategory?)
fixm_AircraftOperator(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('AIR_CARRIER')).
fixm_AircraftOperator(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('AIR_CARRIER')).
fixm_AircraftOperator(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('AIR_CARRIER')).
fixm_AircraftOperator(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('AIR_CARRIER')).
fixm_AircraftOperator(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('AIR_CARRIER')).
fixm_AircraftOperator(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('AIR_CARRIER')).
fixm_AircraftOperator(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('AIR_CARRIER')).
fixm_AircraftOperator(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('AIR_CARRIER')).
fixm_AircraftOperator(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('AIR_CARRIER')).
fixm_AircraftOperator(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('AIR_CARRIER')).

% gml_Point(Graph, Point)
gml_Point(graph:'0_donlon-data.ttl', s1:'elpoint1EADH').
gml_Point(graph:'6_donlon-data.ttl', s1:'elpoint1EADH').
gml_Point(graph:'8_donlon-data.ttl', s1:'elpoint1EADH').
gml_Point(graph:'2_donlon-data.ttl', s1:'elpoint1EADH').
gml_Point(graph:'7_donlon-data.ttl', s1:'elpoint1EADH').
gml_Point(graph:'5_donlon-data.ttl', s1:'elpoint1EADH').
gml_Point(graph:'3_donlon-data.ttl', s1:'elpoint1EADH').
gml_Point(graph:'1_donlon-data.ttl', s1:'elpoint1EADH').
gml_Point(graph:'4_donlon-data.ttl', s1:'elpoint1EADH').
gml_Point(graph:'9_donlon-data.ttl', s1:'elpoint1EADH').

% fixm_EfplTrajectoryRoutePair(Graph, EfplTrajectoryRoutePair)
fixm_EfplTrajectoryRoutePair(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008').
fixm_EfplTrajectoryRoutePair(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008').
fixm_EfplTrajectoryRoutePair(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008').
fixm_EfplTrajectoryRoutePair(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008').
fixm_EfplTrajectoryRoutePair(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008').
fixm_EfplTrajectoryRoutePair(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008').
fixm_EfplTrajectoryRoutePair(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008').
fixm_EfplTrajectoryRoutePair(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008').
fixm_EfplTrajectoryRoutePair(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008').
fixm_EfplTrajectoryRoutePair(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008').

% fixm_RoutePoint(Graph, RoutePoint, Constraint*)

% fixm_BeaconCodeAssignment(Graph, BeaconCodeAssignment, CurrentBeaconCode?, PreviousBeaconCode?, ReassignedBeaconCode?, ReassigningUnit?)

% fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile*, DescentProfile*)
fixm_FlightPerformanceData(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029'], ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035']).
fixm_FlightPerformanceData(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029'], ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035']).
fixm_FlightPerformanceData(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029'], ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035']).
fixm_FlightPerformanceData(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029'], ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035']).
fixm_FlightPerformanceData(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029'], ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035']).
fixm_FlightPerformanceData(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029'], ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035']).
fixm_FlightPerformanceData(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029'], ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035']).
fixm_FlightPerformanceData(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029'], ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035']).
fixm_FlightPerformanceData(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029'], ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035']).
fixm_FlightPerformanceData(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029'], ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035']).

% fixm_ExpandedRoute(Graph, ExpandedRoute, RoutePoint*)

% fixm_RouteConstraintOrPreference(Graph, RouteConstraintOrPreference, ConstraintType?)

% fixm_DeclarationText(Graph, DeclarationText, Compliance?, Consignor?, Shipper?)

% fixm_EstimatedElapsedTime(Graph, EstimatedElapsedTime, ElapsedTime?, Location?)

% fixm_ReportedTime(Graph, ReportedTime, Provenance?, Time?)

% fixm_GeographicLocation(Graph, GeographicLocation, Pos, SrsName?)
fixm_GeographicLocation(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', val('urn:ogc:def:crs:EPSG::4326')).
fixm_GeographicLocation(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', val('urn:ogc:def:crs:EPSG::4326')).

% aixm_LinguisticNote(Graph, LinguisticNote, Note?)
aixm_LinguisticNote(graph:'0_donlon-data.ttl', s1:'ln002', val('Geoid undulation at ELEV PSN is 9 M')).
aixm_LinguisticNote(graph:'0_donlon-data.ttl', s1:'ln003', val('Heliport reference point site at heliport: geometric centre of TLOF, Direction and distance from (city): Donlon	downtown, east shore of Donlon river')).
aixm_LinguisticNote(graph:'6_donlon-data.ttl', s1:'ln002', val('Geoid undulation at ELEV PSN is 9 M')).
aixm_LinguisticNote(graph:'6_donlon-data.ttl', s1:'ln003', val('Heliport reference point site at heliport: geometric centre of TLOF, Direction and distance from (city): Donlon	downtown, east shore of Donlon river')).
aixm_LinguisticNote(graph:'8_donlon-data.ttl', s1:'ln002', val('Geoid undulation at ELEV PSN is 9 M')).
aixm_LinguisticNote(graph:'8_donlon-data.ttl', s1:'ln003', val('Heliport reference point site at heliport: geometric centre of TLOF, Direction and distance from (city): Donlon	downtown, east shore of Donlon river')).
aixm_LinguisticNote(graph:'2_donlon-data.ttl', s1:'ln002', val('Geoid undulation at ELEV PSN is 9 M')).
aixm_LinguisticNote(graph:'2_donlon-data.ttl', s1:'ln003', val('Heliport reference point site at heliport: geometric centre of TLOF, Direction and distance from (city): Donlon	downtown, east shore of Donlon river')).
aixm_LinguisticNote(graph:'7_donlon-data.ttl', s1:'ln002', val('Geoid undulation at ELEV PSN is 9 M')).
aixm_LinguisticNote(graph:'7_donlon-data.ttl', s1:'ln003', val('Heliport reference point site at heliport: geometric centre of TLOF, Direction and distance from (city): Donlon	downtown, east shore of Donlon river')).
aixm_LinguisticNote(graph:'5_donlon-data.ttl', s1:'ln002', val('Geoid undulation at ELEV PSN is 9 M')).
aixm_LinguisticNote(graph:'5_donlon-data.ttl', s1:'ln003', val('Heliport reference point site at heliport: geometric centre of TLOF, Direction and distance from (city): Donlon	downtown, east shore of Donlon river')).
aixm_LinguisticNote(graph:'3_donlon-data.ttl', s1:'ln002', val('Geoid undulation at ELEV PSN is 9 M')).
aixm_LinguisticNote(graph:'3_donlon-data.ttl', s1:'ln003', val('Heliport reference point site at heliport: geometric centre of TLOF, Direction and distance from (city): Donlon	downtown, east shore of Donlon river')).
aixm_LinguisticNote(graph:'1_donlon-data.ttl', s1:'ln002', val('Geoid undulation at ELEV PSN is 9 M')).
aixm_LinguisticNote(graph:'1_donlon-data.ttl', s1:'ln003', val('Heliport reference point site at heliport: geometric centre of TLOF, Direction and distance from (city): Donlon	downtown, east shore of Donlon river')).
aixm_LinguisticNote(graph:'4_donlon-data.ttl', s1:'ln002', val('Geoid undulation at ELEV PSN is 9 M')).
aixm_LinguisticNote(graph:'4_donlon-data.ttl', s1:'ln003', val('Heliport reference point site at heliport: geometric centre of TLOF, Direction and distance from (city): Donlon	downtown, east shore of Donlon river')).
aixm_LinguisticNote(graph:'9_donlon-data.ttl', s1:'ln002', val('Geoid undulation at ELEV PSN is 9 M')).
aixm_LinguisticNote(graph:'9_donlon-data.ttl', s1:'ln003', val('Heliport reference point site at heliport: geometric centre of TLOF, Direction and distance from (city): Donlon	downtown, east shore of Donlon river')).

% aixm_Meteorology(Graph, Meteorology, FlightConditions?, Visibility?, VisibilityInterpretation?, RunwayVisualRange?, RunwayVisualRangeInterpretation?, Annotation*)

% fixm_PointRange(Graph, PointRange, LateralRange?, VerticalRange?, TemporalRange?)

% aixm_City(Graph, City, Name?, Annotation*)
aixm_City(graph:'7_donlon-data.ttl', s1:'ID_110', val('DONLON'), []).
aixm_City(graph:'4_donlon-data.ttl', s1:'ID_110', val('DONLON'), []).
aixm_City(graph:'9_donlon-data.ttl', s1:'ID_110', val('DONLON'), []).
aixm_City(graph:'8_donlon-data.ttl', s1:'ID_110', val('DONLON'), []).
aixm_City(graph:'0_donlon-data.ttl', s1:'ID_110', val('DONLON'), []).
aixm_City(graph:'6_donlon-data.ttl', s1:'ID_110', val('DONLON'), []).
aixm_City(graph:'5_donlon-data.ttl', s1:'ID_110', val('DONLON'), []).
aixm_City(graph:'1_donlon-data.ttl', s1:'ID_110', val('DONLON'), []).
aixm_City(graph:'2_donlon-data.ttl', s1:'ID_110', val('DONLON'), []).
aixm_City(graph:'3_donlon-data.ttl', s1:'ID_110', val('DONLON'), []).

% aixm_AirportHeliportResponsibilityOrganisation(Graph, AirportHeliportResponsibilityOrganisation, Role?, TheOrganisationAuthority)
aixm_AirportHeliportResponsibilityOrganisation(graph:'0_donlon-data.ttl', s1:'A-a72cfd3a', val('OPERATE'), uuid:'74efb6ba-a52a-46c0-a16b-03860d356882').
aixm_AirportHeliportResponsibilityOrganisation(graph:'6_donlon-data.ttl', s1:'A-a72cfd3a', val('OPERATE'), uuid:'74efb6ba-a52a-46c0-a16b-03860d356882').
aixm_AirportHeliportResponsibilityOrganisation(graph:'8_donlon-data.ttl', s1:'A-a72cfd3a', val('OPERATE'), uuid:'74efb6ba-a52a-46c0-a16b-03860d356882').
aixm_AirportHeliportResponsibilityOrganisation(graph:'2_donlon-data.ttl', s1:'A-a72cfd3a', val('OPERATE'), uuid:'74efb6ba-a52a-46c0-a16b-03860d356882').
aixm_AirportHeliportResponsibilityOrganisation(graph:'7_donlon-data.ttl', s1:'A-a72cfd3a', val('OPERATE'), uuid:'74efb6ba-a52a-46c0-a16b-03860d356882').
aixm_AirportHeliportResponsibilityOrganisation(graph:'5_donlon-data.ttl', s1:'A-a72cfd3a', val('OPERATE'), uuid:'74efb6ba-a52a-46c0-a16b-03860d356882').
aixm_AirportHeliportResponsibilityOrganisation(graph:'3_donlon-data.ttl', s1:'A-a72cfd3a', val('OPERATE'), uuid:'74efb6ba-a52a-46c0-a16b-03860d356882').
aixm_AirportHeliportResponsibilityOrganisation(graph:'1_donlon-data.ttl', s1:'A-a72cfd3a', val('OPERATE'), uuid:'74efb6ba-a52a-46c0-a16b-03860d356882').
aixm_AirportHeliportResponsibilityOrganisation(graph:'4_donlon-data.ttl', s1:'A-a72cfd3a', val('OPERATE'), uuid:'74efb6ba-a52a-46c0-a16b-03860d356882').
aixm_AirportHeliportResponsibilityOrganisation(graph:'9_donlon-data.ttl', s1:'A-a72cfd3a', val('OPERATE'), uuid:'74efb6ba-a52a-46c0-a16b-03860d356882').

% fixm_AirspeedRange(Graph, AirspeedRange, LowerSpeed?, UpperSpeed?)

% fixm_RankedTrajectory(Graph, RankedTrajectory, Identifier?, MaximumAcceptableDelay?, AssignedIndicator?, RouteTrajectoryPair?)

% fixm_TrajectoryPointRole(Graph, TrajectoryPointRole, BottomOfClimb?, BottomOfDescent?, BoundaryPoint?, FromGATToOAT?, FromIFRToVFR?, FromOATToGat?, FromVFRToIFR?, TopOfClimb?, TopOfDescent?)
fixm_TrajectoryPointRole(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$').
fixm_TrajectoryPointRole(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$').
fixm_TrajectoryPointRole(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$').
fixm_TrajectoryPointRole(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$').
fixm_TrajectoryPointRole(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$').
fixm_TrajectoryPointRole(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$').
fixm_TrajectoryPointRole(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$').
fixm_TrajectoryPointRole(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$').
fixm_TrajectoryPointRole(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$').
fixm_TrajectoryPointRole(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$').

% fixm_CommunicationCapabilities(Graph, CommunicationCapabilities, OtherCommunicationCapabilities?, OtherDataLinkCapabilities?, DataLinkCode*, SelectiveCallingCode?, CommunicationCode*)
fixm_CommunicationCapabilities(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', '$null$', val('SVM'), ['val:J4', 'val:J5', 'val:J3'], val('EHJM'), ['val:E3', 'val:H', 'val:E2', 'val:M1', 'val:Y']).
fixm_CommunicationCapabilities(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', '$null$', val('SVM'), ['val:J4', 'val:J5', 'val:J3'], val('EHJM'), ['val:E3', 'val:H', 'val:E2', 'val:M1', 'val:Y']).
fixm_CommunicationCapabilities(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', '$null$', val('SVM'), ['val:J4', 'val:J5', 'val:J3'], val('EHJM'), ['val:E3', 'val:H', 'val:E2', 'val:M1', 'val:Y']).
fixm_CommunicationCapabilities(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', '$null$', val('SVM'), ['val:J4', 'val:J5', 'val:J3'], val('EHJM'), ['val:E3', 'val:H', 'val:E2', 'val:M1', 'val:Y']).
fixm_CommunicationCapabilities(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', '$null$', val('SVM'), ['val:J4', 'val:J5', 'val:J3'], val('EHJM'), ['val:E3', 'val:H', 'val:E2', 'val:M1', 'val:Y']).
fixm_CommunicationCapabilities(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', '$null$', val('SVM'), ['val:J4', 'val:J5', 'val:J3'], val('EHJM'), ['val:E3', 'val:H', 'val:E2', 'val:M1', 'val:Y']).
fixm_CommunicationCapabilities(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', '$null$', val('SVM'), ['val:J4', 'val:J5', 'val:J3'], val('EHJM'), ['val:E3', 'val:H', 'val:E2', 'val:M1', 'val:Y']).
fixm_CommunicationCapabilities(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', '$null$', val('SVM'), ['val:J4', 'val:J5', 'val:J3'], val('EHJM'), ['val:E3', 'val:H', 'val:E2', 'val:M1', 'val:Y']).
fixm_CommunicationCapabilities(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', '$null$', val('SVM'), ['val:J4', 'val:J5', 'val:J3'], val('EHJM'), ['val:E3', 'val:H', 'val:E2', 'val:M1', 'val:Y']).
fixm_CommunicationCapabilities(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', '$null$', val('SVM'), ['val:J4', 'val:J5', 'val:J3'], val('EHJM'), ['val:E3', 'val:H', 'val:E2', 'val:M1', 'val:Y']).

% fixm_Dinghy(Graph, Dinghy, Quantity?, TotalCapacity?, Covered?, Colour?)

% aixm_ContactInformation(Graph, ContactInformation, Name?, Title?, Annotation*, NetworkNode*, Address*, PhoneFax*)

% fixm_PlannedReportingPosition(Graph, PlannedReportingPosition, Position?, PositionAltitude?, PositionEstimatedTime?)

% fixm_SignificantPoint(Graph, SignificantPoint)

% fixm_SupplementalData(Graph, SupplementalData, FuelEndurance?, PersonsOnBoard?, PilotInCommand?)

% fixm_DangerousGoods(Graph, DangerousGoods, GuidebookNumber?, OnboardLocation?, HandlingInformation?, AircraftLimitation?, AirWayBill?, Shipment?, PackageGroup*, ShippingInformation?)

% fixm_DangerousGoodsPackageGroup(Graph, DangerousGoodsPackageGroup, ShipmentDimensions?, DangerousGoodsPackage*, ShipmentUseIndicator?)

% fixm_OfftrackDistance(Graph, OfftrackDistance, Distance?, Direction?)

% fixm_Handoff(Graph, Handoff, ReceivingUnit?, TransferringUnit?, CoordinationStatus?)

% fixm_TrajectoryChange(Graph, TrajectoryChange, ConstrainedAirspace?, SpecialActivityAirspace?)

% fixm_ContactInformation(Graph, ContactInformation, Name?, Title?, OnlineContact?, PhoneFax?, Address?)

% aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator?, Name?, LocationIndicatorICAO?, DesignatorIATA?, Type?, CertifiedICAO?, PrivateUse?, ControlType?, FieldElevation?, FieldElevationAccuracy?, VerticalDatum?, MagneticVariation?, MagneticVariationAccuracy?, DateMagneticVariation?, MagneticVariationChange?, ReferenceTemperature?, AltimeterCheckLocation?, SecondaryPowerSupply?, WindDirectionIndicator?, LandingDirectionIndicator?, TransitionAltitude?, TransitionLevel?, LowestTemperature?, Abandoned?, CertificationDate?, CertificationExpirationDate?, Contact*, Annotation*, ARP?, AltimeterSource*, Contaminant*, ServedCity*, ResponsibleOrganisation?, AviationBoundary?, Availability*)
aixm_AirportHeliportTimeSlice(graph:'8_donlon-data.ttl', s1:'AHP_EADH', val('EADH'), val('DONLON/DOWNTOWN HELIPORT'), val('EADH'), nil('unknown'), '$null$', nil('unknown'), '$null$', val('CIVIL'), xval('18','M'), xval('0.5','M'), '$null$', '$null$', '$null$', val('1990'), '$null$', xval('21.0','C'), '$null$', '$null$', '$null$', '$null$', xval('3500','FT'), '$null$', '$null$', '$null$', nil('unknown'), '$null$', [], [s1:'n003', s1:'n002'], s1:'elpoint1EADH', [], [], [s1:'ID_110'], s1:'A-a72cfd3a', '$null$', [s1:'AHY_EADH_PERMIT']).
aixm_AirportHeliportTimeSlice(graph:'9_donlon-data.ttl', s2:'ID_ACT_11', '$null$', '$null$', '$null$', val('ysdf'), '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], [], '$null$', [], [], [s2:'city1', s2:'city2'], '$null$', '$null$', [s2:'ID_ACT_13']).
aixm_AirportHeliportTimeSlice(graph:'4_donlon-data.ttl', s1:'AHP_EADH', val('EADH'), val('DONLON/DOWNTOWN HELIPORT'), val('EADH'), nil('unknown'), '$null$', nil('unknown'), '$null$', val('CIVIL'), xval('18','M'), xval('0.5','M'), '$null$', '$null$', '$null$', val('1990'), '$null$', xval('21.0','C'), '$null$', '$null$', '$null$', '$null$', xval('3500','FT'), '$null$', '$null$', '$null$', nil('unknown'), '$null$', [], [s1:'n003', s1:'n002'], s1:'elpoint1EADH', [], [], [s1:'ID_110'], s1:'A-a72cfd3a', '$null$', [s1:'AHY_EADH_PERMIT']).
aixm_AirportHeliportTimeSlice(graph:'7_donlon-data.ttl', s2:'ID_ACT_11', '$null$', '$null$', '$null$', val('ysdf'), '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], [], '$null$', [], [], [s2:'city1', s2:'city2'], '$null$', '$null$', [s2:'ID_ACT_13']).
aixm_AirportHeliportTimeSlice(graph:'0_donlon-data.ttl', s1:'AHP_EADH', val('EADH'), val('DONLON/DOWNTOWN HELIPORT'), val('EADH'), nil('unknown'), '$null$', nil('unknown'), '$null$', val('CIVIL'), xval('18','M'), xval('0.5','M'), '$null$', '$null$', '$null$', val('1990'), '$null$', xval('21.0','C'), '$null$', '$null$', '$null$', '$null$', xval('3500','FT'), '$null$', '$null$', '$null$', nil('unknown'), '$null$', [], [s1:'n003', s1:'n002'], s1:'elpoint1EADH', [], [], [s1:'ID_110'], s1:'A-a72cfd3a', '$null$', [s1:'AHY_EADH_PERMIT']).
aixm_AirportHeliportTimeSlice(graph:'5_donlon-data.ttl', s2:'ID_ACT_11', '$null$', '$null$', '$null$', val('ysdf'), '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], [], '$null$', [], [], [s2:'city1', s2:'city2'], '$null$', '$null$', [s2:'ID_ACT_13']).
aixm_AirportHeliportTimeSlice(graph:'6_donlon-data.ttl', s1:'AHP_EADH', val('EADH'), val('DONLON/DOWNTOWN HELIPORT'), val('EADH'), nil('unknown'), '$null$', nil('unknown'), '$null$', val('CIVIL'), xval('18','M'), xval('0.5','M'), '$null$', '$null$', '$null$', val('1990'), '$null$', xval('21.0','C'), '$null$', '$null$', '$null$', '$null$', xval('3500','FT'), '$null$', '$null$', '$null$', nil('unknown'), '$null$', [], [s1:'n003', s1:'n002'], s1:'elpoint1EADH', [], [], [s1:'ID_110'], s1:'A-a72cfd3a', '$null$', [s1:'AHY_EADH_PERMIT']).
aixm_AirportHeliportTimeSlice(graph:'1_donlon-data.ttl', s2:'ID_ACT_11', '$null$', '$null$', '$null$', val('ysdf'), '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], [], '$null$', [], [], [s2:'city1', s2:'city2'], '$null$', '$null$', [s2:'ID_ACT_13']).
aixm_AirportHeliportTimeSlice(graph:'2_donlon-data.ttl', s1:'AHP_EADH', val('EADH'), val('DONLON/DOWNTOWN HELIPORT'), val('EADH'), nil('unknown'), '$null$', nil('unknown'), '$null$', val('CIVIL'), xval('18','M'), xval('0.5','M'), '$null$', '$null$', '$null$', val('1990'), '$null$', xval('21.0','C'), '$null$', '$null$', '$null$', '$null$', xval('3500','FT'), '$null$', '$null$', '$null$', nil('unknown'), '$null$', [], [s1:'n003', s1:'n002'], s1:'elpoint1EADH', [], [], [s1:'ID_110'], s1:'A-a72cfd3a', '$null$', [s1:'AHY_EADH_PERMIT']).
aixm_AirportHeliportTimeSlice(graph:'3_donlon-data.ttl', s2:'ID_ACT_11', '$null$', '$null$', '$null$', val('ysdf'), '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], [], '$null$', [], [], [s2:'city1', s2:'city2'], '$null$', '$null$', [s2:'ID_ACT_13']).
aixm_AirportHeliportTimeSlice(graph:'8_donlon-data.ttl', s2:'ID_ACT_11', '$null$', '$null$', '$null$', val('ysdf'), '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], [], '$null$', [], [], [s2:'city1', s2:'city2'], '$null$', '$null$', [s2:'ID_ACT_13']).
aixm_AirportHeliportTimeSlice(graph:'9_donlon-data.ttl', s1:'AHP_EADH', val('EADH'), val('DONLON/DOWNTOWN HELIPORT'), val('EADH'), nil('unknown'), '$null$', nil('unknown'), '$null$', val('CIVIL'), xval('18','M'), xval('0.5','M'), '$null$', '$null$', '$null$', val('1990'), '$null$', xval('21.0','C'), '$null$', '$null$', '$null$', '$null$', xval('3500','FT'), '$null$', '$null$', '$null$', nil('unknown'), '$null$', [], [s1:'n003', s1:'n002'], s1:'elpoint1EADH', [], [], [s1:'ID_110'], s1:'A-a72cfd3a', '$null$', [s1:'AHY_EADH_PERMIT']).
aixm_AirportHeliportTimeSlice(graph:'7_donlon-data.ttl', s1:'AHP_EADH', val('EADH'), val('DONLON/DOWNTOWN HELIPORT'), val('EADH'), nil('unknown'), '$null$', nil('unknown'), '$null$', val('CIVIL'), xval('18','M'), xval('0.5','M'), '$null$', '$null$', '$null$', val('1990'), '$null$', xval('21.0','C'), '$null$', '$null$', '$null$', '$null$', xval('3500','FT'), '$null$', '$null$', '$null$', nil('unknown'), '$null$', [], [s1:'n003', s1:'n002'], s1:'elpoint1EADH', [], [], [s1:'ID_110'], s1:'A-a72cfd3a', '$null$', [s1:'AHY_EADH_PERMIT']).
aixm_AirportHeliportTimeSlice(graph:'4_donlon-data.ttl', s2:'ID_ACT_11', '$null$', '$null$', '$null$', val('ysdf'), '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], [], '$null$', [], [], [s2:'city1', s2:'city2'], '$null$', '$null$', [s2:'ID_ACT_13']).
aixm_AirportHeliportTimeSlice(graph:'6_donlon-data.ttl', s2:'ID_ACT_11', '$null$', '$null$', '$null$', val('ysdf'), '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], [], '$null$', [], [], [s2:'city1', s2:'city2'], '$null$', '$null$', [s2:'ID_ACT_13']).
aixm_AirportHeliportTimeSlice(graph:'0_donlon-data.ttl', s2:'ID_ACT_11', '$null$', '$null$', '$null$', val('ysdf'), '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], [], '$null$', [], [], [s2:'city1', s2:'city2'], '$null$', '$null$', [s2:'ID_ACT_13']).
aixm_AirportHeliportTimeSlice(graph:'5_donlon-data.ttl', s1:'AHP_EADH', val('EADH'), val('DONLON/DOWNTOWN HELIPORT'), val('EADH'), nil('unknown'), '$null$', nil('unknown'), '$null$', val('CIVIL'), xval('18','M'), xval('0.5','M'), '$null$', '$null$', '$null$', val('1990'), '$null$', xval('21.0','C'), '$null$', '$null$', '$null$', '$null$', xval('3500','FT'), '$null$', '$null$', '$null$', nil('unknown'), '$null$', [], [s1:'n003', s1:'n002'], s1:'elpoint1EADH', [], [], [s1:'ID_110'], s1:'A-a72cfd3a', '$null$', [s1:'AHY_EADH_PERMIT']).
aixm_AirportHeliportTimeSlice(graph:'1_donlon-data.ttl', s1:'AHP_EADH', val('EADH'), val('DONLON/DOWNTOWN HELIPORT'), val('EADH'), nil('unknown'), '$null$', nil('unknown'), '$null$', val('CIVIL'), xval('18','M'), xval('0.5','M'), '$null$', '$null$', '$null$', val('1990'), '$null$', xval('21.0','C'), '$null$', '$null$', '$null$', '$null$', xval('3500','FT'), '$null$', '$null$', '$null$', nil('unknown'), '$null$', [], [s1:'n003', s1:'n002'], s1:'elpoint1EADH', [], [], [s1:'ID_110'], s1:'A-a72cfd3a', '$null$', [s1:'AHY_EADH_PERMIT']).
aixm_AirportHeliportTimeSlice(graph:'2_donlon-data.ttl', s2:'ID_ACT_11', '$null$', '$null$', '$null$', val('ysdf'), '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], [], '$null$', [], [], [s2:'city1', s2:'city2'], '$null$', '$null$', [s2:'ID_ACT_13']).
aixm_AirportHeliportTimeSlice(graph:'3_donlon-data.ttl', s1:'AHP_EADH', val('EADH'), val('DONLON/DOWNTOWN HELIPORT'), val('EADH'), nil('unknown'), '$null$', nil('unknown'), '$null$', val('CIVIL'), xval('18','M'), xval('0.5','M'), '$null$', '$null$', '$null$', val('1990'), '$null$', xval('21.0','C'), '$null$', '$null$', '$null$', '$null$', xval('3500','FT'), '$null$', '$null$', '$null$', nil('unknown'), '$null$', [], [s1:'n003', s1:'n002'], s1:'elpoint1EADH', [], [], [s1:'ID_110'], s1:'A-a72cfd3a', '$null$', [s1:'AHY_EADH_PERMIT']).

% fixm_Point4D(Graph, Point4D, Altitude?, Time?, PointRange?)
fixm_Point4D(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', '$null$', '$null$', '$null$').
fixm_Point4D(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', '$null$', '$null$', '$null$').

% fixm_AbstractRoutePoint(Graph, AbstractRoutePoint, AirTrafficType?, DelayAtPoint?, FlightRules?, Point?, ClearanceLimit?)

% aixm_Ridge(Graph, Ridge, Side?, Distance?, Depth?, Annotation*)

% fixm_DepartureActivityTimes(Graph, DepartureActivityTimes, BoardingTime?, DeIcingTime?, GroundHandlingTime?, StartupTime?)

% fixm_EnRouteDiversion(Graph, EnRouteDiversion, DiversionRecoveryInformation?)

% fixm_ActualSpeed(Graph, ActualSpeed, Calculated?, PilotReported?, Surveillance?)

% fixm_FlightEmergency(Graph, FlightEmergency, ActionTaken?, EmergencyDescription?, Originator?, OtherInformation?, Phase?, Contact?)

% fixm_Flight(Graph, Flight, ControllingUnit?, Extensions*, FlightFiler?, Gufi?, Remarks?, AircraftDescription?, DangerousGoods*, RankedTrajectories*, RouteToRevisedDestination?, Negotiating?, Agreed?, Arrival?, Departure?, Emergency?, RadioCommunicationFailure?, EnRoute?, Operator?, EnRouteDiversion?, FlightType?, FlightStatus?, Originator?, SupplementalData?, FlightIdentification?, SpecialHandling*)
fixm_Flight(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', [], '$null$', '$null$', val('TCAS'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', [], [], '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('SCHEDULED'), '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', []).
fixm_Flight(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', [], '$null$', '$null$', val('TCAS'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', [], [], '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('SCHEDULED'), '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', []).
fixm_Flight(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', [], '$null$', '$null$', val('TCAS'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', [], [], '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('SCHEDULED'), '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', []).
fixm_Flight(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', [], '$null$', '$null$', val('TCAS'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', [], [], '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('SCHEDULED'), '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', []).
fixm_Flight(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', [], '$null$', '$null$', val('TCAS'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', [], [], '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('SCHEDULED'), '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', []).
fixm_Flight(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', [], '$null$', '$null$', val('TCAS'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', [], [], '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('SCHEDULED'), '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', []).
fixm_Flight(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', [], '$null$', '$null$', val('TCAS'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', [], [], '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('SCHEDULED'), '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', []).
fixm_Flight(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', [], '$null$', '$null$', val('TCAS'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', [], [], '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('SCHEDULED'), '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', []).
fixm_Flight(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', [], '$null$', '$null$', val('TCAS'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', [], [], '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('SCHEDULED'), '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', []).
fixm_Flight(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', [], '$null$', '$null$', val('TCAS'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', [], [], '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_006', '$null$', val('SCHEDULED'), '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', []).

% aixm_PropertiesWithSchedule(Graph, PropertiesWithSchedule, Annotation*, SpecialDateAuthority*, TimeInterval*)
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s2:'ID_ACT_20', [], [], []).
aixm_PropertiesWithSchedule(graph:'2_donlon-data.ttl', s2:'ID_ACT_15', [], [], []).
aixm_PropertiesWithSchedule(graph:'8_donlon-data.ttl', s2:'ID_ACT_20', [], [], []).
aixm_PropertiesWithSchedule(graph:'8_donlon-data.ttl', s2:'ID_ACT_16', [], [], []).
aixm_PropertiesWithSchedule(graph:'5_donlon-data.ttl', s1:'agtayyat', [], [], []).
aixm_PropertiesWithSchedule(graph:'6_donlon-data.ttl', s1:'AHY_EADH_PERMIT', [], [], []).
aixm_PropertiesWithSchedule(graph:'4_donlon-data.ttl', s2:'ID_ACT_18', [], [], []).
aixm_PropertiesWithSchedule(graph:'6_donlon-data.ttl', s2:'ID_ACT_13', [], [], []).
aixm_PropertiesWithSchedule(graph:'9_donlon-data.ttl', s1:'A-a72cfd3a', [s1:'n002'], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s2:'ID_ACT_16', [], [], []).
aixm_PropertiesWithSchedule(graph:'7_donlon-data.ttl', s1:'A-a72cfd3a', [s1:'n002'], [], []).
aixm_PropertiesWithSchedule(graph:'1_donlon-data.ttl', s2:'ID_ACT_15', [], [], []).
aixm_PropertiesWithSchedule(graph:'3_donlon-data.ttl', s2:'ID_ACT_16', [], [], []).
aixm_PropertiesWithSchedule(graph:'6_donlon-data.ttl', s1:'agtayyat', [], [], []).
aixm_PropertiesWithSchedule(graph:'3_donlon-data.ttl', s2:'ID_ACT_20', [], [], []).
aixm_PropertiesWithSchedule(graph:'5_donlon-data.ttl', s1:'AHY_EADH_PERMIT', [], [], []).
aixm_PropertiesWithSchedule(graph:'7_donlon-data.ttl', s2:'ID_ACT_18', [], [], []).
aixm_PropertiesWithSchedule(graph:'5_donlon-data.ttl', s2:'ID_ACT_13', [], [], []).
aixm_PropertiesWithSchedule(graph:'9_donlon-data.ttl', s2:'ID_ACT_18', [], [], []).
aixm_PropertiesWithSchedule(graph:'4_donlon-data.ttl', s1:'A-a72cfd3a', [s1:'n002'], [], []).
aixm_PropertiesWithSchedule(graph:'2_donlon-data.ttl', s2:'ID_ACT_16', [], [], []).
aixm_PropertiesWithSchedule(graph:'8_donlon-data.ttl', s2:'ID_ACT_15', [], [], []).
aixm_PropertiesWithSchedule(graph:'6_donlon-data.ttl', s2:'ID_ACT_18', [], [], []).
aixm_PropertiesWithSchedule(graph:'4_donlon-data.ttl', s2:'ID_ACT_13', [], [], []).
aixm_PropertiesWithSchedule(graph:'5_donlon-data.ttl', s1:'A-a72cfd3a', [s1:'n002'], [], []).
aixm_PropertiesWithSchedule(graph:'2_donlon-data.ttl', s2:'ID_ACT_20', [], [], []).
aixm_PropertiesWithSchedule(graph:'9_donlon-data.ttl', s1:'agtayyat', [], [], []).
aixm_PropertiesWithSchedule(graph:'4_donlon-data.ttl', s1:'AHY_EADH_PERMIT', [], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s2:'ID_ACT_15', [], [], []).
aixm_PropertiesWithSchedule(graph:'7_donlon-data.ttl', s1:'agtayyat', [], [], []).
aixm_PropertiesWithSchedule(graph:'1_donlon-data.ttl', s2:'ID_ACT_16', [], [], []).
aixm_PropertiesWithSchedule(graph:'3_donlon-data.ttl', s2:'ID_ACT_15', [], [], []).
aixm_PropertiesWithSchedule(graph:'5_donlon-data.ttl', s2:'ID_ACT_18', [], [], []).
aixm_PropertiesWithSchedule(graph:'7_donlon-data.ttl', s2:'ID_ACT_13', [], [], []).
aixm_PropertiesWithSchedule(graph:'6_donlon-data.ttl', s1:'A-a72cfd3a', [s1:'n002'], [], []).
aixm_PropertiesWithSchedule(graph:'9_donlon-data.ttl', s2:'ID_ACT_13', [], [], []).
aixm_PropertiesWithSchedule(graph:'1_donlon-data.ttl', s2:'ID_ACT_20', [], [], []).
aixm_PropertiesWithSchedule(graph:'9_donlon-data.ttl', s1:'AHY_EADH_PERMIT', [], [], []).
aixm_PropertiesWithSchedule(graph:'7_donlon-data.ttl', s1:'AHY_EADH_PERMIT', [], [], []).
aixm_PropertiesWithSchedule(graph:'4_donlon-data.ttl', s1:'agtayyat', [], [], []).
aixm_PropertiesWithSchedule(graph:'3_donlon-data.ttl', s1:'A-a72cfd3a', [s1:'n002'], [], []).
aixm_PropertiesWithSchedule(graph:'2_donlon-data.ttl', s1:'AHY_EADH_PERMIT', [], [], []).
aixm_PropertiesWithSchedule(graph:'4_donlon-data.ttl', s2:'ID_ACT_20', [], [], []).
aixm_PropertiesWithSchedule(graph:'1_donlon-data.ttl', s1:'agtayyat', [], [], []).
aixm_PropertiesWithSchedule(graph:'2_donlon-data.ttl', s2:'ID_ACT_13', [], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s2:'ID_ACT_18', [], [], []).
aixm_PropertiesWithSchedule(graph:'4_donlon-data.ttl', s2:'ID_ACT_16', [], [], []).
aixm_PropertiesWithSchedule(graph:'6_donlon-data.ttl', s2:'ID_ACT_15', [], [], []).
aixm_PropertiesWithSchedule(graph:'8_donlon-data.ttl', s2:'ID_ACT_18', [], [], []).
aixm_PropertiesWithSchedule(graph:'1_donlon-data.ttl', s2:'ID_ACT_13', [], [], []).
aixm_PropertiesWithSchedule(graph:'2_donlon-data.ttl', s1:'agtayyat', [], [], []).
aixm_PropertiesWithSchedule(graph:'3_donlon-data.ttl', s2:'ID_ACT_18', [], [], []).
aixm_PropertiesWithSchedule(graph:'7_donlon-data.ttl', s2:'ID_ACT_20', [], [], []).
aixm_PropertiesWithSchedule(graph:'9_donlon-data.ttl', s2:'ID_ACT_20', [], [], []).
aixm_PropertiesWithSchedule(graph:'1_donlon-data.ttl', s1:'AHY_EADH_PERMIT', [], [], []).
aixm_PropertiesWithSchedule(graph:'9_donlon-data.ttl', s2:'ID_ACT_16', [], [], []).
aixm_PropertiesWithSchedule(graph:'7_donlon-data.ttl', s2:'ID_ACT_16', [], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s1:'A-a72cfd3a', [s1:'n002'], [], []).
aixm_PropertiesWithSchedule(graph:'8_donlon-data.ttl', s1:'A-a72cfd3a', [s1:'n002'], [], []).
aixm_PropertiesWithSchedule(graph:'5_donlon-data.ttl', s2:'ID_ACT_15', [], [], []).
aixm_PropertiesWithSchedule(graph:'6_donlon-data.ttl', s2:'ID_ACT_20', [], [], []).
aixm_PropertiesWithSchedule(graph:'3_donlon-data.ttl', s1:'agtayyat', [], [], []).
aixm_PropertiesWithSchedule(graph:'2_donlon-data.ttl', s2:'ID_ACT_18', [], [], []).
aixm_PropertiesWithSchedule(graph:'1_donlon-data.ttl', s1:'A-a72cfd3a', [s1:'n002'], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s1:'AHY_EADH_PERMIT', [], [], []).
aixm_PropertiesWithSchedule(graph:'8_donlon-data.ttl', s2:'ID_ACT_13', [], [], []).
aixm_PropertiesWithSchedule(graph:'4_donlon-data.ttl', s2:'ID_ACT_15', [], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s2:'ID_ACT_13', [], [], []).
aixm_PropertiesWithSchedule(graph:'8_donlon-data.ttl', s1:'AHY_EADH_PERMIT', [], [], []).
aixm_PropertiesWithSchedule(graph:'6_donlon-data.ttl', s2:'ID_ACT_16', [], [], []).
aixm_PropertiesWithSchedule(graph:'5_donlon-data.ttl', s2:'ID_ACT_20', [], [], []).
aixm_PropertiesWithSchedule(graph:'2_donlon-data.ttl', s1:'A-a72cfd3a', [s1:'n002'], [], []).
aixm_PropertiesWithSchedule(graph:'3_donlon-data.ttl', s1:'AHY_EADH_PERMIT', [], [], []).
aixm_PropertiesWithSchedule(graph:'1_donlon-data.ttl', s2:'ID_ACT_18', [], [], []).
aixm_PropertiesWithSchedule(graph:'3_donlon-data.ttl', s2:'ID_ACT_13', [], [], []).
aixm_PropertiesWithSchedule(graph:'9_donlon-data.ttl', s2:'ID_ACT_15', [], [], []).
aixm_PropertiesWithSchedule(graph:'0_donlon-data.ttl', s1:'agtayyat', [], [], []).
aixm_PropertiesWithSchedule(graph:'7_donlon-data.ttl', s2:'ID_ACT_15', [], [], []).
aixm_PropertiesWithSchedule(graph:'8_donlon-data.ttl', s1:'agtayyat', [], [], []).
aixm_PropertiesWithSchedule(graph:'5_donlon-data.ttl', s2:'ID_ACT_16', [], [], []).

% gml_Surface(Graph, Surface, Patch+)

% fixm_ClearedFlightInformation(Graph, ClearedFlightInformation, ClearedFlightLevel?, ClearedSpeed?, Heading?, OfftrackClearance?, RateOfClimbDescend?, DirectRouting?)

% fixm_TrajectoryRoutePair(Graph, TrajectoryRoutePair, Trajectory?, Route?)
fixm_TrajectoryRoutePair(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020').
fixm_TrajectoryRoutePair(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020').
fixm_TrajectoryRoutePair(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020').
fixm_TrajectoryRoutePair(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020').
fixm_TrajectoryRoutePair(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020').
fixm_TrajectoryRoutePair(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020').
fixm_TrajectoryRoutePair(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020').
fixm_TrajectoryRoutePair(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020').
fixm_TrajectoryRoutePair(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020').
fixm_TrajectoryRoutePair(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020').

% fixm_UnitBoundary(Graph, UnitBoundary, DownstreamUnit?, UpstreamUnit?, BoundaryCrossingProposed?, BoundaryCrossingCoordinated?, Handoff?, UnitBoundaryIndicator?)

% aixm_SurfaceContamination(Graph, SurfaceContamination, ObservationTime?, Depth?, FrictionCoefficient?, FrictionEstimation?, FrictionDevice?, ObscuredLights?, FurtherClearanceTime?, FurtherTotalClearance?, NextObservationTime?, Proportion?, CriticalRidge*, Annotation*, Layer*)

% fixm_MeteorologicalData(Graph, MeteorologicalData, Temperature?, WindDirection?, WindSpeed?)
fixm_MeteorologicalData(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', xval('4.3','CELSIUS'), xval('248.0','DEGREES'), xval('10.41','METERS_PER_SECOND')).
fixm_MeteorologicalData(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', xval('4.3','CELSIUS'), xval('248.0','DEGREES'), xval('10.41','METERS_PER_SECOND')).
fixm_MeteorologicalData(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', xval('4.3','CELSIUS'), xval('248.0','DEGREES'), xval('10.41','METERS_PER_SECOND')).
fixm_MeteorologicalData(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', xval('4.3','CELSIUS'), xval('248.0','DEGREES'), xval('10.41','METERS_PER_SECOND')).
fixm_MeteorologicalData(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', xval('4.3','CELSIUS'), xval('248.0','DEGREES'), xval('10.41','METERS_PER_SECOND')).
fixm_MeteorologicalData(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', xval('4.3','CELSIUS'), xval('248.0','DEGREES'), xval('10.41','METERS_PER_SECOND')).
fixm_MeteorologicalData(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', xval('4.3','CELSIUS'), xval('248.0','DEGREES'), xval('10.41','METERS_PER_SECOND')).
fixm_MeteorologicalData(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', xval('4.3','CELSIUS'), xval('248.0','DEGREES'), xval('10.41','METERS_PER_SECOND')).
fixm_MeteorologicalData(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', xval('4.3','CELSIUS'), xval('248.0','DEGREES'), xval('10.41','METERS_PER_SECOND')).
fixm_MeteorologicalData(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', xval('4.3','CELSIUS'), xval('248.0','DEGREES'), xval('10.41','METERS_PER_SECOND')).

% aixm_OrganisationAuthority(Graph, OrganisationAuthority, TimeSlice*)
aixm_OrganisationAuthority(graph:'1_donlon-data.ttl', uuid:'74efb6ba-a52a-46c0-a16b-03860d356882', []).
aixm_OrganisationAuthority(graph:'3_donlon-data.ttl', uuid:'74efb6ba-a52a-46c0-a16b-03860d356882', []).
aixm_OrganisationAuthority(graph:'2_donlon-data.ttl', uuid:'74efb6ba-a52a-46c0-a16b-03860d356882', []).
aixm_OrganisationAuthority(graph:'7_donlon-data.ttl', uuid:'74efb6ba-a52a-46c0-a16b-03860d356882', []).
aixm_OrganisationAuthority(graph:'4_donlon-data.ttl', uuid:'74efb6ba-a52a-46c0-a16b-03860d356882', []).
aixm_OrganisationAuthority(graph:'8_donlon-data.ttl', uuid:'74efb6ba-a52a-46c0-a16b-03860d356882', []).
aixm_OrganisationAuthority(graph:'9_donlon-data.ttl', uuid:'74efb6ba-a52a-46c0-a16b-03860d356882', []).
aixm_OrganisationAuthority(graph:'6_donlon-data.ttl', uuid:'74efb6ba-a52a-46c0-a16b-03860d356882', []).
aixm_OrganisationAuthority(graph:'5_donlon-data.ttl', uuid:'74efb6ba-a52a-46c0-a16b-03860d356882', []).
aixm_OrganisationAuthority(graph:'0_donlon-data.ttl', uuid:'74efb6ba-a52a-46c0-a16b-03860d356882', []).

% fixm_TelephoneContact(Graph, TelephoneContact, Voice?, Facimile?)

% fixm_ShippingInformation(Graph, ShippingInformation, AerodromeOfLoading?, AerodromeOfUnloading?, DangerousGoodsScreeningLocation?, DepartureCountry?, DestinationCountry?, OriginCountry?, ShipmentAuthorizations?, SubsidiaryHazardClassAndDivision?, SupplementaryInformation?, TransferAerodromes*, DeclarationText?, Consignee?, Shipper?)

% aixm_AirportHeliportContamination(Graph, AirportHeliportContamination)

% fixm_OtherInformation(Graph, OtherInformation, ReplacementFlightPlanIndicator?, RunwayVisualRange?)
fixm_OtherInformation(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', '$null$', xval('0.075','KILOMETERS')).
fixm_OtherInformation(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', '$null$', xval('0.075','KILOMETERS')).
fixm_OtherInformation(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', '$null$', xval('0.075','KILOMETERS')).
fixm_OtherInformation(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', '$null$', xval('0.075','KILOMETERS')).
fixm_OtherInformation(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', '$null$', xval('0.075','KILOMETERS')).
fixm_OtherInformation(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', '$null$', xval('0.075','KILOMETERS')).
fixm_OtherInformation(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', '$null$', xval('0.075','KILOMETERS')).
fixm_OtherInformation(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', '$null$', xval('0.075','KILOMETERS')).
fixm_OtherInformation(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', '$null$', xval('0.075','KILOMETERS')).
fixm_OtherInformation(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', '$null$', xval('0.075','KILOMETERS')).

% fixm_DinghyColour(Graph, DinghyColour)

% fixm_CpdlcConnection(Graph, CpdlcConnection, ReceivingUnitFrequency?, AtnLogonParameters?, SendCpldcIndicator?, ConnectionStatus?, FrequencyUsage?, Fans1ALogonParameters?)

% aixm_TelephoneContact(Graph, TelephoneContact, Voice?, Facsimile?)

% fixm_Route(Graph, Route, AirfileRouteStartTime?, FlightDuration?, InitialCruisingSpeed?, InitialFlightRules?, RequestedAltitude?, RouteText?, EstimatedElapsedTime*, ExpandedRoute?, ClimbSchedule?, DescentSchedule?, Segment*)
fixm_Route(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$', '$null$', '$null$', '$null$', '$null$', val('N0491F350 SULUS8G SULUS UZ650 ERETO UZ715 KULOK UL984 OKG L984 DOPOV T46 DOKEL N871 POLON Z169 GERVI P851 ABERO/K0887F350 P851 RAVOK Z860 TOBLO/K0880F350 B365 BANIP/K0865F330 B365 OLUPI B923 BEKAS B142 SIVKO/K0898F390 G3 AKB A360 BLH A110 TDK A124 RULAD/K0878S1190 A460 KCA L888 LEBAK/K0882S1250 L888 PEXUN B213 WFX B330 POU R473 SIERA'), [], '$null$', '$null$', '$null$', []).
fixm_Route(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$', '$null$', '$null$', '$null$', '$null$', val('N0491F350 SULUS8G SULUS UZ650 ERETO UZ715 KULOK UL984 OKG L984 DOPOV T46 DOKEL N871 POLON Z169 GERVI P851 ABERO/K0887F350 P851 RAVOK Z860 TOBLO/K0880F350 B365 BANIP/K0865F330 B365 OLUPI B923 BEKAS B142 SIVKO/K0898F390 G3 AKB A360 BLH A110 TDK A124 RULAD/K0878S1190 A460 KCA L888 LEBAK/K0882S1250 L888 PEXUN B213 WFX B330 POU R473 SIERA'), [], '$null$', '$null$', '$null$', []).
fixm_Route(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$', '$null$', '$null$', '$null$', '$null$', val('N0491F350 SULUS8G SULUS UZ650 ERETO UZ715 KULOK UL984 OKG L984 DOPOV T46 DOKEL N871 POLON Z169 GERVI P851 ABERO/K0887F350 P851 RAVOK Z860 TOBLO/K0880F350 B365 BANIP/K0865F330 B365 OLUPI B923 BEKAS B142 SIVKO/K0898F390 G3 AKB A360 BLH A110 TDK A124 RULAD/K0878S1190 A460 KCA L888 LEBAK/K0882S1250 L888 PEXUN B213 WFX B330 POU R473 SIERA'), [], '$null$', '$null$', '$null$', []).
fixm_Route(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$', '$null$', '$null$', '$null$', '$null$', val('N0491F350 SULUS8G SULUS UZ650 ERETO UZ715 KULOK UL984 OKG L984 DOPOV T46 DOKEL N871 POLON Z169 GERVI P851 ABERO/K0887F350 P851 RAVOK Z860 TOBLO/K0880F350 B365 BANIP/K0865F330 B365 OLUPI B923 BEKAS B142 SIVKO/K0898F390 G3 AKB A360 BLH A110 TDK A124 RULAD/K0878S1190 A460 KCA L888 LEBAK/K0882S1250 L888 PEXUN B213 WFX B330 POU R473 SIERA'), [], '$null$', '$null$', '$null$', []).
fixm_Route(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$', '$null$', '$null$', '$null$', '$null$', val('N0491F350 SULUS8G SULUS UZ650 ERETO UZ715 KULOK UL984 OKG L984 DOPOV T46 DOKEL N871 POLON Z169 GERVI P851 ABERO/K0887F350 P851 RAVOK Z860 TOBLO/K0880F350 B365 BANIP/K0865F330 B365 OLUPI B923 BEKAS B142 SIVKO/K0898F390 G3 AKB A360 BLH A110 TDK A124 RULAD/K0878S1190 A460 KCA L888 LEBAK/K0882S1250 L888 PEXUN B213 WFX B330 POU R473 SIERA'), [], '$null$', '$null$', '$null$', []).
fixm_Route(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$', '$null$', '$null$', '$null$', '$null$', val('N0491F350 SULUS8G SULUS UZ650 ERETO UZ715 KULOK UL984 OKG L984 DOPOV T46 DOKEL N871 POLON Z169 GERVI P851 ABERO/K0887F350 P851 RAVOK Z860 TOBLO/K0880F350 B365 BANIP/K0865F330 B365 OLUPI B923 BEKAS B142 SIVKO/K0898F390 G3 AKB A360 BLH A110 TDK A124 RULAD/K0878S1190 A460 KCA L888 LEBAK/K0882S1250 L888 PEXUN B213 WFX B330 POU R473 SIERA'), [], '$null$', '$null$', '$null$', []).
fixm_Route(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$', '$null$', '$null$', '$null$', '$null$', val('N0491F350 SULUS8G SULUS UZ650 ERETO UZ715 KULOK UL984 OKG L984 DOPOV T46 DOKEL N871 POLON Z169 GERVI P851 ABERO/K0887F350 P851 RAVOK Z860 TOBLO/K0880F350 B365 BANIP/K0865F330 B365 OLUPI B923 BEKAS B142 SIVKO/K0898F390 G3 AKB A360 BLH A110 TDK A124 RULAD/K0878S1190 A460 KCA L888 LEBAK/K0882S1250 L888 PEXUN B213 WFX B330 POU R473 SIERA'), [], '$null$', '$null$', '$null$', []).
fixm_Route(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$', '$null$', '$null$', '$null$', '$null$', val('N0491F350 SULUS8G SULUS UZ650 ERETO UZ715 KULOK UL984 OKG L984 DOPOV T46 DOKEL N871 POLON Z169 GERVI P851 ABERO/K0887F350 P851 RAVOK Z860 TOBLO/K0880F350 B365 BANIP/K0865F330 B365 OLUPI B923 BEKAS B142 SIVKO/K0898F390 G3 AKB A360 BLH A110 TDK A124 RULAD/K0878S1190 A460 KCA L888 LEBAK/K0882S1250 L888 PEXUN B213 WFX B330 POU R473 SIERA'), [], '$null$', '$null$', '$null$', []).
fixm_Route(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$', '$null$', '$null$', '$null$', '$null$', val('N0491F350 SULUS8G SULUS UZ650 ERETO UZ715 KULOK UL984 OKG L984 DOPOV T46 DOKEL N871 POLON Z169 GERVI P851 ABERO/K0887F350 P851 RAVOK Z860 TOBLO/K0880F350 B365 BANIP/K0865F330 B365 OLUPI B923 BEKAS B142 SIVKO/K0898F390 G3 AKB A360 BLH A110 TDK A124 RULAD/K0878S1190 A460 KCA L888 LEBAK/K0882S1250 L888 PEXUN B213 WFX B330 POU R473 SIERA'), [], '$null$', '$null$', '$null$', []).
fixm_Route(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$', '$null$', '$null$', '$null$', '$null$', val('N0491F350 SULUS8G SULUS UZ650 ERETO UZ715 KULOK UL984 OKG L984 DOPOV T46 DOKEL N871 POLON Z169 GERVI P851 ABERO/K0887F350 P851 RAVOK Z860 TOBLO/K0880F350 B365 BANIP/K0865F330 B365 OLUPI B923 BEKAS B142 SIVKO/K0898F390 G3 AKB A360 BLH A110 TDK A124 RULAD/K0878S1190 A460 KCA L888 LEBAK/K0882S1250 L888 PEXUN B213 WFX B330 POU R473 SIERA'), [], '$null$', '$null$', '$null$', []).

% fixm_Person(Graph, Person, Name?, Contact?)

% fixm_EfplFlight(Graph, EfplFlight, IfplId?, TotalEstimatedElapsedTime?, AerodromesOfDestination?, EfplSpecialHandling?, EfplFiledTrajectory?, EfplAcceptedTrajectory?, OtherInformation?, FlightPerformanceData?)
fixm_EfplFlight(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', val('P0Y0M0DT10H15M0S'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009').
fixm_EfplFlight(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', val('P0Y0M0DT10H15M0S'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009').
fixm_EfplFlight(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', val('P0Y0M0DT10H15M0S'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009').
fixm_EfplFlight(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', val('P0Y0M0DT10H15M0S'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009').
fixm_EfplFlight(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', val('P0Y0M0DT10H15M0S'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009').
fixm_EfplFlight(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', val('P0Y0M0DT10H15M0S'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009').
fixm_EfplFlight(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', val('P0Y0M0DT10H15M0S'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009').
fixm_EfplFlight(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', val('P0Y0M0DT10H15M0S'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009').
fixm_EfplFlight(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', val('P0Y0M0DT10H15M0S'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009').
fixm_EfplFlight(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$', val('P0Y0M0DT10H15M0S'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_008', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_010', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_009').

% fixm_Originator(Graph, Originator)

% fixm_FlightStatus(Graph, FlightStatus, AirborneHold?, Airfile?, Accepted?, FlightCycle?, MissedApproach?, Suspended?)

% fixm_IdentifiedUnitReference(Graph, IdentifiedUnitReference, UnitIdentifier?)

% fixm_Radionuclide(Graph, Radionuclide, PhysicalChemicalForm?, RadionuclideId?, RadionuclideName?, LowDispersibleMaterialIndicator?, Activity?, SpecialFormIndicator?)

% aixm_OnlineContact(Graph, OnlineContact, Network?, Linkage?, Protocol?, EMail?)

% fixm_StructuredPostalAddress(Graph, StructuredPostalAddress)

% fixm_AircraftPosition(Graph, AircraftPosition, Altitude?, Position?, PositionTime?, Track?, ActualSpeed?, NextPosition?, ReportSource?, FollowingPosition?)

% aixm_AirportHeliportUsage(Graph, AirportHeliportUsage, Operation?)
aixm_AirportHeliportUsage(graph:'0_donlon-data.ttl', s2:'ID_ACT_14', '$null$').
aixm_AirportHeliportUsage(graph:'0_donlon-data.ttl', s1:'AHU_EADH_PERMIT', '$null$').
aixm_AirportHeliportUsage(graph:'6_donlon-data.ttl', s2:'ID_ACT_14', '$null$').
aixm_AirportHeliportUsage(graph:'6_donlon-data.ttl', s1:'AHU_EADH_PERMIT', '$null$').
aixm_AirportHeliportUsage(graph:'8_donlon-data.ttl', s2:'ID_ACT_14', '$null$').
aixm_AirportHeliportUsage(graph:'8_donlon-data.ttl', s1:'AHU_EADH_PERMIT', '$null$').
aixm_AirportHeliportUsage(graph:'2_donlon-data.ttl', s2:'ID_ACT_14', '$null$').
aixm_AirportHeliportUsage(graph:'2_donlon-data.ttl', s1:'AHU_EADH_PERMIT', '$null$').
aixm_AirportHeliportUsage(graph:'7_donlon-data.ttl', s2:'ID_ACT_14', '$null$').
aixm_AirportHeliportUsage(graph:'7_donlon-data.ttl', s1:'AHU_EADH_PERMIT', '$null$').
aixm_AirportHeliportUsage(graph:'5_donlon-data.ttl', s2:'ID_ACT_14', '$null$').
aixm_AirportHeliportUsage(graph:'5_donlon-data.ttl', s1:'AHU_EADH_PERMIT', '$null$').
aixm_AirportHeliportUsage(graph:'3_donlon-data.ttl', s2:'ID_ACT_14', '$null$').
aixm_AirportHeliportUsage(graph:'3_donlon-data.ttl', s1:'AHU_EADH_PERMIT', '$null$').
aixm_AirportHeliportUsage(graph:'1_donlon-data.ttl', s2:'ID_ACT_14', '$null$').
aixm_AirportHeliportUsage(graph:'1_donlon-data.ttl', s1:'AHU_EADH_PERMIT', '$null$').
aixm_AirportHeliportUsage(graph:'4_donlon-data.ttl', s2:'ID_ACT_14', '$null$').
aixm_AirportHeliportUsage(graph:'4_donlon-data.ttl', s1:'AHU_EADH_PERMIT', '$null$').
aixm_AirportHeliportUsage(graph:'9_donlon-data.ttl', s2:'ID_ACT_14', '$null$').
aixm_AirportHeliportUsage(graph:'9_donlon-data.ttl', s1:'AHU_EADH_PERMIT', '$null$').

% aixm_Timesheet(Graph, Timesheet, TimeReference?, StartDate?, EndDate?, Day?, DayTil?, StartTime?, StartEvent?, StartTimeRelativeEvent?, StartEventInterpretation?, EndTime?, EndEvent?, EndTimeRelativeEvent?, EndEventInterpretation?, DaylightSavingAdjust?, Excluded?, Annotation*)

% gml_SurfacePatch(Graph, SurfacePatch)

% fixm_MultiTime(Graph, MultiTime, Actual?, Estimated?)

% aixm_FlightCharacteristic(Graph, FlightCharacteristic, Type?, Rule?, Status?, Military?, Origin?, Purpose?, Annotation*)
aixm_FlightCharacteristic(graph:'0_donlon-data.ttl', s2:'ID_ACT_19', '$null$', '$null$', val('SAR'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'3_donlon-data.ttl', s2:'ID_ACT_17', '$null$', '$null$', '$null$', '$null$', val('HOME_BASED'), '$null$', []).
aixm_FlightCharacteristic(graph:'4_donlon-data.ttl', s2:'ID_ACT_19', '$null$', '$null$', val('SAR'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'3_donlon-data.ttl', s2:'ID_ACT_21', '$null$', '$null$', val('OTHER'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'8_donlon-data.ttl', s2:'ID_ACT_19', '$null$', '$null$', val('SAR'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'4_donlon-data.ttl', s1:'F_yastadyt', '$null$', val('VFR'), '$null$', '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'7_donlon-data.ttl', s2:'ID_ACT_21', '$null$', '$null$', val('OTHER'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'8_donlon-data.ttl', s1:'F_yastadyt', '$null$', val('VFR'), '$null$', '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'9_donlon-data.ttl', s2:'ID_ACT_21', '$null$', '$null$', val('OTHER'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'0_donlon-data.ttl', s1:'F_yastadyt', '$null$', val('VFR'), '$null$', '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'9_donlon-data.ttl', s2:'ID_ACT_17', '$null$', '$null$', '$null$', '$null$', val('HOME_BASED'), '$null$', []).
aixm_FlightCharacteristic(graph:'7_donlon-data.ttl', s2:'ID_ACT_17', '$null$', '$null$', '$null$', '$null$', val('HOME_BASED'), '$null$', []).
aixm_FlightCharacteristic(graph:'7_donlon-data.ttl', s2:'ID_ACT_19', '$null$', '$null$', val('SAR'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'9_donlon-data.ttl', s2:'ID_ACT_19', '$null$', '$null$', val('SAR'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'3_donlon-data.ttl', s1:'F_yastadyt', '$null$', val('VFR'), '$null$', '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'7_donlon-data.ttl', s1:'F_yastadyt', '$null$', val('VFR'), '$null$', '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'4_donlon-data.ttl', s2:'ID_ACT_21', '$null$', '$null$', val('OTHER'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'0_donlon-data.ttl', s2:'ID_ACT_17', '$null$', '$null$', '$null$', '$null$', val('HOME_BASED'), '$null$', []).
aixm_FlightCharacteristic(graph:'8_donlon-data.ttl', s2:'ID_ACT_21', '$null$', '$null$', val('OTHER'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'9_donlon-data.ttl', s1:'F_yastadyt', '$null$', val('VFR'), '$null$', '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'3_donlon-data.ttl', s2:'ID_ACT_19', '$null$', '$null$', val('SAR'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'0_donlon-data.ttl', s2:'ID_ACT_21', '$null$', '$null$', val('OTHER'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'8_donlon-data.ttl', s2:'ID_ACT_17', '$null$', '$null$', '$null$', '$null$', val('HOME_BASED'), '$null$', []).
aixm_FlightCharacteristic(graph:'4_donlon-data.ttl', s2:'ID_ACT_17', '$null$', '$null$', '$null$', '$null$', val('HOME_BASED'), '$null$', []).
aixm_FlightCharacteristic(graph:'1_donlon-data.ttl', s2:'ID_ACT_21', '$null$', '$null$', val('OTHER'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'6_donlon-data.ttl', s2:'ID_ACT_19', '$null$', '$null$', val('SAR'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'2_donlon-data.ttl', s1:'F_yastadyt', '$null$', val('VFR'), '$null$', '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'1_donlon-data.ttl', s2:'ID_ACT_17', '$null$', '$null$', '$null$', '$null$', val('HOME_BASED'), '$null$', []).
aixm_FlightCharacteristic(graph:'2_donlon-data.ttl', s2:'ID_ACT_19', '$null$', '$null$', val('SAR'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'5_donlon-data.ttl', s2:'ID_ACT_17', '$null$', '$null$', '$null$', '$null$', val('HOME_BASED'), '$null$', []).
aixm_FlightCharacteristic(graph:'5_donlon-data.ttl', s2:'ID_ACT_21', '$null$', '$null$', val('OTHER'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'6_donlon-data.ttl', s1:'F_yastadyt', '$null$', val('VFR'), '$null$', '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'2_donlon-data.ttl', s2:'ID_ACT_17', '$null$', '$null$', '$null$', '$null$', val('HOME_BASED'), '$null$', []).
aixm_FlightCharacteristic(graph:'5_donlon-data.ttl', s2:'ID_ACT_19', '$null$', '$null$', val('SAR'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'1_donlon-data.ttl', s1:'F_yastadyt', '$null$', val('VFR'), '$null$', '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'2_donlon-data.ttl', s2:'ID_ACT_21', '$null$', '$null$', val('OTHER'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'6_donlon-data.ttl', s2:'ID_ACT_17', '$null$', '$null$', '$null$', '$null$', val('HOME_BASED'), '$null$', []).
aixm_FlightCharacteristic(graph:'6_donlon-data.ttl', s2:'ID_ACT_21', '$null$', '$null$', val('OTHER'), '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'5_donlon-data.ttl', s1:'F_yastadyt', '$null$', val('VFR'), '$null$', '$null$', '$null$', '$null$', []).
aixm_FlightCharacteristic(graph:'1_donlon-data.ttl', s2:'ID_ACT_19', '$null$', '$null$', val('SAR'), '$null$', '$null$', '$null$', []).

% fixm_Provenance(Graph, Provenance, Timestamp?, Centre?, Source?, System?)

% aixm_AirportHeliport(Graph, AirportHeliport, TimeSlice*)
aixm_AirportHeliport(graph:'1_donlon-data.ttl', uuid:'dd062d88-3e64-4a5d-bebd-89476db9ebea', [s1:'AHP_EADH', s2:'ID_ACT_11']).
aixm_AirportHeliport(graph:'3_donlon-data.ttl', uuid:'dd062d88-3e64-4a5d-bebd-89476db9ebea', [s1:'AHP_EADH', s2:'ID_ACT_11']).
aixm_AirportHeliport(graph:'2_donlon-data.ttl', uuid:'dd062d88-3e64-4a5d-bebd-89476db9ebea', [s1:'AHP_EADH', s2:'ID_ACT_11']).
aixm_AirportHeliport(graph:'7_donlon-data.ttl', uuid:'dd062d88-3e64-4a5d-bebd-89476db9ebea', [s1:'AHP_EADH', s2:'ID_ACT_11']).
aixm_AirportHeliport(graph:'4_donlon-data.ttl', uuid:'dd062d88-3e64-4a5d-bebd-89476db9ebea', [s1:'AHP_EADH', s2:'ID_ACT_11']).
aixm_AirportHeliport(graph:'8_donlon-data.ttl', uuid:'dd062d88-3e64-4a5d-bebd-89476db9ebea', [s1:'AHP_EADH', s2:'ID_ACT_11']).
aixm_AirportHeliport(graph:'9_donlon-data.ttl', uuid:'dd062d88-3e64-4a5d-bebd-89476db9ebea', [s1:'AHP_EADH', s2:'ID_ACT_11']).
aixm_AirportHeliport(graph:'6_donlon-data.ttl', uuid:'dd062d88-3e64-4a5d-bebd-89476db9ebea', [s1:'AHP_EADH', s2:'ID_ACT_11']).
aixm_AirportHeliport(graph:'5_donlon-data.ttl', uuid:'dd062d88-3e64-4a5d-bebd-89476db9ebea', [s1:'AHP_EADH', s2:'ID_ACT_11']).
aixm_AirportHeliport(graph:'0_donlon-data.ttl', uuid:'dd062d88-3e64-4a5d-bebd-89476db9ebea', [s1:'AHP_EADH', s2:'ID_ACT_11']).

% fixm_TrajectoryPoint(Graph, TrajectoryPoint, AltimeterSetting?, PredictedAirspeed?, PredictedGroundspeed?, MetData?, Point?, TrajectoryChange*, TrajectoryChangeType*, ReferencePoint?)
fixm_TrajectoryPoint(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', [], [], '$null$').
fixm_TrajectoryPoint(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', [], [], '$null$').
fixm_TrajectoryPoint(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', [], [], '$null$').
fixm_TrajectoryPoint(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', [], [], '$null$').
fixm_TrajectoryPoint(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', '$null$', xval('0.0','KILOMETRES_PER_HOUR'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', [], [], '$null$').
fixm_TrajectoryPoint(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', [], [], '$null$').
fixm_TrajectoryPoint(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', [], [], '$null$').
fixm_TrajectoryPoint(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', [], [], '$null$').
fixm_TrajectoryPoint(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', [], [], '$null$').
fixm_TrajectoryPoint(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', '$null$', xval('0.0','KILOMETRES_PER_HOUR'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', [], [], '$null$').
fixm_TrajectoryPoint(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', [], [], '$null$').
fixm_TrajectoryPoint(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', [], [], '$null$').
fixm_TrajectoryPoint(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', [], [], '$null$').
fixm_TrajectoryPoint(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', [], [], '$null$').
fixm_TrajectoryPoint(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', [], [], '$null$').
fixm_TrajectoryPoint(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', [], [], '$null$').
fixm_TrajectoryPoint(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', [], [], '$null$').
fixm_TrajectoryPoint(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', [], [], '$null$').
fixm_TrajectoryPoint(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', [], [], '$null$').
fixm_TrajectoryPoint(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', [], [], '$null$').
fixm_TrajectoryPoint(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', [], [], '$null$').
fixm_TrajectoryPoint(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', [], [], '$null$').
fixm_TrajectoryPoint(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', '$null$', xval('0.0','KILOMETRES_PER_HOUR'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', [], [], '$null$').
fixm_TrajectoryPoint(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', '$null$', xval('0.0','KILOMETRES_PER_HOUR'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', [], [], '$null$').
fixm_TrajectoryPoint(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', [], [], '$null$').
fixm_TrajectoryPoint(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', [], [], '$null$').
fixm_TrajectoryPoint(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', [], [], '$null$').
fixm_TrajectoryPoint(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', [], [], '$null$').
fixm_TrajectoryPoint(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', [], [], '$null$').
fixm_TrajectoryPoint(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', [], [], '$null$').
fixm_TrajectoryPoint(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', [], [], '$null$').
fixm_TrajectoryPoint(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', '$null$', xval('0.0','KILOMETRES_PER_HOUR'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', [], [], '$null$').
fixm_TrajectoryPoint(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', '$null$', xval('0.0','KILOMETRES_PER_HOUR'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', [], [], '$null$').
fixm_TrajectoryPoint(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', [], [], '$null$').
fixm_TrajectoryPoint(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', [], [], '$null$').
fixm_TrajectoryPoint(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', [], [], '$null$').
fixm_TrajectoryPoint(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', '$null$', xval('0.0','KILOMETRES_PER_HOUR'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', [], [], '$null$').
fixm_TrajectoryPoint(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', '$null$', xval('0.0','KILOMETRES_PER_HOUR'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', [], [], '$null$').
fixm_TrajectoryPoint(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', [], [], '$null$').
fixm_TrajectoryPoint(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', [], [], '$null$').
fixm_TrajectoryPoint(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', [], [], '$null$').
fixm_TrajectoryPoint(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_030', [], [], '$null$').
fixm_TrajectoryPoint(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', '$null$', xval('0.0','KILOMETRES_PER_HOUR'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', [], [], '$null$').
fixm_TrajectoryPoint(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', '$null$', xval('0.0','KILOMETRES_PER_HOUR'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_023', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_024', [], [], '$null$').
fixm_TrajectoryPoint(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', [], [], '$null$').
fixm_TrajectoryPoint(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_036', [], [], '$null$').
fixm_TrajectoryPoint(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', [], [], '$null$').
fixm_TrajectoryPoint(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_039', [], [], '$null$').
fixm_TrajectoryPoint(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', [], [], '$null$').
fixm_TrajectoryPoint(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', '$null$', '$null$', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_033', [], [], '$null$').

% fixm_EfplTrajectoryPoint(Graph, EfplTrajectoryPoint, AerodromeIdentifier?, DistanceFromTakeOff?, EfplEstimatedSpeed?, ElapsedTime?, GrossWeight?, TrajectoryPointType?, TrajectoryPointRole?, InboundSegment?)
fixm_EfplTrajectoryPoint(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', xval('250.151','KILOMETERS'), '$null$', val('P0DT0H24M3S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', xval('3.727','KILOMETERS'), '$null$', val('P0DT0H1M4S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', xval('0.0','KILOMETERS'), '$null$', val('P0DT0H0M0S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', xval('0.0','KILOMETERS'), xval('0.0','MACH'), val('P0DT0H0M0S'), xval('458849.0','KILOGRAMS'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$').
fixm_EfplTrajectoryPoint(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', xval('261.039','KILOMETERS'), '$null$', val('P0DT0H24M48S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', xval('250.151','KILOMETERS'), '$null$', val('P0DT0H24M3S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', xval('3.727','KILOMETERS'), '$null$', val('P0DT0H1M4S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', xval('0.0','KILOMETERS'), '$null$', val('P0DT0H0M0S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', xval('0.0','KILOMETERS'), xval('0.0','MACH'), val('P0DT0H0M0S'), xval('458849.0','KILOGRAMS'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$').
fixm_EfplTrajectoryPoint(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', xval('261.039','KILOMETERS'), '$null$', val('P0DT0H24M48S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', xval('250.151','KILOMETERS'), '$null$', val('P0DT0H24M3S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', xval('3.727','KILOMETERS'), '$null$', val('P0DT0H1M4S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', xval('0.0','KILOMETERS'), '$null$', val('P0DT0H0M0S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', xval('0.0','KILOMETERS'), xval('0.0','MACH'), val('P0DT0H0M0S'), xval('458849.0','KILOGRAMS'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$').
fixm_EfplTrajectoryPoint(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', xval('261.039','KILOMETERS'), '$null$', val('P0DT0H24M48S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', xval('250.151','KILOMETERS'), '$null$', val('P0DT0H24M3S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', xval('3.727','KILOMETERS'), '$null$', val('P0DT0H1M4S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', xval('0.0','KILOMETERS'), '$null$', val('P0DT0H0M0S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', xval('0.0','KILOMETERS'), xval('0.0','MACH'), val('P0DT0H0M0S'), xval('458849.0','KILOGRAMS'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$').
fixm_EfplTrajectoryPoint(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', xval('261.039','KILOMETERS'), '$null$', val('P0DT0H24M48S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', xval('250.151','KILOMETERS'), '$null$', val('P0DT0H24M3S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', xval('3.727','KILOMETERS'), '$null$', val('P0DT0H1M4S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', xval('0.0','KILOMETERS'), '$null$', val('P0DT0H0M0S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', xval('0.0','KILOMETERS'), xval('0.0','MACH'), val('P0DT0H0M0S'), xval('458849.0','KILOGRAMS'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$').
fixm_EfplTrajectoryPoint(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', xval('261.039','KILOMETERS'), '$null$', val('P0DT0H24M48S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', xval('250.151','KILOMETERS'), '$null$', val('P0DT0H24M3S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', xval('3.727','KILOMETERS'), '$null$', val('P0DT0H1M4S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', xval('0.0','KILOMETERS'), '$null$', val('P0DT0H0M0S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', xval('0.0','KILOMETERS'), xval('0.0','MACH'), val('P0DT0H0M0S'), xval('458849.0','KILOGRAMS'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$').
fixm_EfplTrajectoryPoint(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', xval('261.039','KILOMETERS'), '$null$', val('P0DT0H24M48S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', xval('250.151','KILOMETERS'), '$null$', val('P0DT0H24M3S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', xval('3.727','KILOMETERS'), '$null$', val('P0DT0H1M4S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', xval('0.0','KILOMETERS'), '$null$', val('P0DT0H0M0S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', xval('0.0','KILOMETERS'), xval('0.0','MACH'), val('P0DT0H0M0S'), xval('458849.0','KILOGRAMS'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$').
fixm_EfplTrajectoryPoint(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', xval('261.039','KILOMETERS'), '$null$', val('P0DT0H24M48S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', xval('250.151','KILOMETERS'), '$null$', val('P0DT0H24M3S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', xval('3.727','KILOMETERS'), '$null$', val('P0DT0H1M4S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', xval('0.0','KILOMETERS'), '$null$', val('P0DT0H0M0S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', xval('0.0','KILOMETERS'), xval('0.0','MACH'), val('P0DT0H0M0S'), xval('458849.0','KILOGRAMS'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$').
fixm_EfplTrajectoryPoint(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', xval('261.039','KILOMETERS'), '$null$', val('P0DT0H24M48S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', xval('250.151','KILOMETERS'), '$null$', val('P0DT0H24M3S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', xval('3.727','KILOMETERS'), '$null$', val('P0DT0H1M4S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', xval('0.0','KILOMETERS'), '$null$', val('P0DT0H0M0S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', xval('0.0','KILOMETERS'), xval('0.0','MACH'), val('P0DT0H0M0S'), xval('458849.0','KILOGRAMS'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$').
fixm_EfplTrajectoryPoint(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', xval('261.039','KILOMETERS'), '$null$', val('P0DT0H24M48S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_038', '$null$', xval('250.151','KILOMETERS'), '$null$', val('P0DT0H24M3S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_032', '$null$', xval('3.727','KILOMETERS'), '$null$', val('P0DT0H1M4S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_029', '$null$', xval('0.0','KILOMETERS'), '$null$', val('P0DT0H0M0S'), '$null$', '$null$', '$null$', '$null$').
fixm_EfplTrajectoryPoint(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', xval('0.0','KILOMETERS'), xval('0.0','MACH'), val('P0DT0H0M0S'), xval('458849.0','KILOGRAMS'), '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_028', '$null$').
fixm_EfplTrajectoryPoint(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_035', '$null$', xval('261.039','KILOMETERS'), '$null$', val('P0DT0H24M48S'), '$null$', '$null$', '$null$', '$null$').

% fixm_Temperatures(Graph, Temperatures, ControlTemperature?, EmergencyTemperature?, FlashpointTemperature?)

% fixm_TrajectorySegment(Graph, TrajectorySegment, SegmentIdentifier?, SegmentType?)

% fixm_RunwayPositionAndTime(Graph, RunwayPositionAndTime, RunwayName?, RunwayTime?)

% fixm_Feature(Graph, Feature, Provenance?)
fixm_Feature(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$').
fixm_Feature(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$').
fixm_Feature(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$').
fixm_Feature(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$').
fixm_Feature(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$').
fixm_Feature(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$').
fixm_Feature(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$').
fixm_Feature(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$').
fixm_Feature(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$').
fixm_Feature(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', '$null$').
fixm_Feature(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$').
fixm_Feature(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$').
fixm_Feature(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$').
fixm_Feature(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$').
fixm_Feature(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$').
fixm_Feature(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$').
fixm_Feature(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$').
fixm_Feature(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$').
fixm_Feature(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$').
fixm_Feature(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_002', '$null$').
fixm_Feature(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$').
fixm_Feature(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$').
fixm_Feature(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$').
fixm_Feature(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$').
fixm_Feature(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$').
fixm_Feature(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$').
fixm_Feature(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$').
fixm_Feature(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$').
fixm_Feature(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$').
fixm_Feature(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$').
fixm_Feature(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$').
fixm_Feature(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$').
fixm_Feature(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$').
fixm_Feature(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$').
fixm_Feature(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$').
fixm_Feature(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$').
fixm_Feature(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$').
fixm_Feature(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$').
fixm_Feature(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$').
fixm_Feature(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$').

% fixm_FlightIdentification(Graph, FlightIdentification, AircraftIdentification?, MajorCarrierIdentifier?, MarketingCarrierFlightIdentifier*)
fixm_FlightIdentification(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', val('DLH796'), '$null$', []).
fixm_FlightIdentification(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', val('DLH796'), '$null$', []).
fixm_FlightIdentification(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', val('DLH796'), '$null$', []).
fixm_FlightIdentification(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', val('DLH796'), '$null$', []).
fixm_FlightIdentification(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', val('DLH796'), '$null$', []).
fixm_FlightIdentification(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', val('DLH796'), '$null$', []).
fixm_FlightIdentification(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', val('DLH796'), '$null$', []).
fixm_FlightIdentification(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', val('DLH796'), '$null$', []).
fixm_FlightIdentification(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', val('DLH796'), '$null$', []).
fixm_FlightIdentification(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_005', val('DLH796'), '$null$', []).

% fixm_LastContact(Graph, LastContact, ContactFrequency?, LastContactTime?, LastContactUnit?, Position?)

% fixm_ElapsedTimeLocation(Graph, ElapsedTimeLocation)

% aixm_Surface(Graph, Surface, HorizontalAccuracy?, Annotation*)

% gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition)
gml_TimePeriod(graph:'0_donlon-data.ttl', s2:'IDE_ACT_23', '$null$', '$null$').
gml_TimePeriod(graph:'0_donlon-data.ttl', s2:'ID_ACT_12', '$null$', '$null$').
gml_TimePeriod(graph:'0_donlon-data.ttl', s2:'IDE_ACT_24', '$null$', '$null$').
gml_TimePeriod(graph:'6_donlon-data.ttl', s2:'IDE_ACT_23', '$null$', '$null$').
gml_TimePeriod(graph:'6_donlon-data.ttl', s2:'ID_ACT_12', '$null$', '$null$').
gml_TimePeriod(graph:'6_donlon-data.ttl', s2:'IDE_ACT_24', '$null$', '$null$').
gml_TimePeriod(graph:'8_donlon-data.ttl', s2:'IDE_ACT_23', '$null$', '$null$').
gml_TimePeriod(graph:'8_donlon-data.ttl', s2:'ID_ACT_12', '$null$', '$null$').
gml_TimePeriod(graph:'8_donlon-data.ttl', s2:'IDE_ACT_24', '$null$', '$null$').
gml_TimePeriod(graph:'2_donlon-data.ttl', s2:'IDE_ACT_23', '$null$', '$null$').
gml_TimePeriod(graph:'2_donlon-data.ttl', s2:'ID_ACT_12', '$null$', '$null$').
gml_TimePeriod(graph:'2_donlon-data.ttl', s2:'IDE_ACT_24', '$null$', '$null$').
gml_TimePeriod(graph:'7_donlon-data.ttl', s2:'IDE_ACT_23', '$null$', '$null$').
gml_TimePeriod(graph:'7_donlon-data.ttl', s2:'ID_ACT_12', '$null$', '$null$').
gml_TimePeriod(graph:'7_donlon-data.ttl', s2:'IDE_ACT_24', '$null$', '$null$').
gml_TimePeriod(graph:'5_donlon-data.ttl', s2:'IDE_ACT_23', '$null$', '$null$').
gml_TimePeriod(graph:'5_donlon-data.ttl', s2:'ID_ACT_12', '$null$', '$null$').
gml_TimePeriod(graph:'5_donlon-data.ttl', s2:'IDE_ACT_24', '$null$', '$null$').
gml_TimePeriod(graph:'3_donlon-data.ttl', s2:'IDE_ACT_23', '$null$', '$null$').
gml_TimePeriod(graph:'3_donlon-data.ttl', s2:'ID_ACT_12', '$null$', '$null$').
gml_TimePeriod(graph:'3_donlon-data.ttl', s2:'IDE_ACT_24', '$null$', '$null$').
gml_TimePeriod(graph:'1_donlon-data.ttl', s2:'IDE_ACT_23', '$null$', '$null$').
gml_TimePeriod(graph:'1_donlon-data.ttl', s2:'ID_ACT_12', '$null$', '$null$').
gml_TimePeriod(graph:'1_donlon-data.ttl', s2:'IDE_ACT_24', '$null$', '$null$').
gml_TimePeriod(graph:'4_donlon-data.ttl', s2:'IDE_ACT_23', '$null$', '$null$').
gml_TimePeriod(graph:'4_donlon-data.ttl', s2:'ID_ACT_12', '$null$', '$null$').
gml_TimePeriod(graph:'4_donlon-data.ttl', s2:'IDE_ACT_24', '$null$', '$null$').
gml_TimePeriod(graph:'9_donlon-data.ttl', s2:'IDE_ACT_23', '$null$', '$null$').
gml_TimePeriod(graph:'9_donlon-data.ttl', s2:'ID_ACT_12', '$null$', '$null$').
gml_TimePeriod(graph:'9_donlon-data.ttl', s2:'IDE_ACT_24', '$null$', '$null$').

% fixm_AircraftCapabilities(Graph, AircraftCapabilities, Survival?, Communication?, Navigation?, Surveillance?, StandardCapabilities?)
fixm_AircraftCapabilities(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$').
fixm_AircraftCapabilities(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$').
fixm_AircraftCapabilities(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$').
fixm_AircraftCapabilities(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$').
fixm_AircraftCapabilities(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$').
fixm_AircraftCapabilities(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$').
fixm_AircraftCapabilities(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$').
fixm_AircraftCapabilities(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$').
fixm_AircraftCapabilities(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$').
fixm_AircraftCapabilities(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', '$null$', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_013', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_014', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$').

% fixm_SpeedSchedule(Graph, SpeedSchedule, InitialSpeed?, SubsequentSpeed?)

% aixm_OrganisationAuthorityTimeSlice(Graph, OrganisationAuthorityTimeSlice, Name?, Designator?, Type?, Military?, Annotation*, Contact*, RelatedOrganisationAuthority*)

% fixm_EnRoute(Graph, EnRoute, AlternateAerodrome*, FleetPrioritization?, BoundaryCrossings*, CpdlcConnection?, BeaconCodeAssignment?, Cleared?, ControlElement*, Pointout?, Position?)

% fixm_FlightLevel(Graph, FlightLevel, Level?, Unit?)
fixm_FlightLevel(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025', '$null$', val('S')).
fixm_FlightLevel(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040', '$null$', val('SM')).
fixm_FlightLevel(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031', '$null$', val('SM')).
fixm_FlightLevel(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037', '$null$', val('SM')).
fixm_FlightLevel(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034', '$null$', val('SM')).
fixm_FlightLevel(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025', '$null$', val('S')).
fixm_FlightLevel(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040', '$null$', val('SM')).
fixm_FlightLevel(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031', '$null$', val('SM')).
fixm_FlightLevel(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037', '$null$', val('SM')).
fixm_FlightLevel(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034', '$null$', val('SM')).
fixm_FlightLevel(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025', '$null$', val('S')).
fixm_FlightLevel(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040', '$null$', val('SM')).
fixm_FlightLevel(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031', '$null$', val('SM')).
fixm_FlightLevel(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037', '$null$', val('SM')).
fixm_FlightLevel(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034', '$null$', val('SM')).
fixm_FlightLevel(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025', '$null$', val('S')).
fixm_FlightLevel(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040', '$null$', val('SM')).
fixm_FlightLevel(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031', '$null$', val('SM')).
fixm_FlightLevel(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037', '$null$', val('SM')).
fixm_FlightLevel(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034', '$null$', val('SM')).
fixm_FlightLevel(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025', '$null$', val('S')).
fixm_FlightLevel(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040', '$null$', val('SM')).
fixm_FlightLevel(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031', '$null$', val('SM')).
fixm_FlightLevel(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037', '$null$', val('SM')).
fixm_FlightLevel(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034', '$null$', val('SM')).
fixm_FlightLevel(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025', '$null$', val('S')).
fixm_FlightLevel(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040', '$null$', val('SM')).
fixm_FlightLevel(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031', '$null$', val('SM')).
fixm_FlightLevel(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037', '$null$', val('SM')).
fixm_FlightLevel(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034', '$null$', val('SM')).
fixm_FlightLevel(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025', '$null$', val('S')).
fixm_FlightLevel(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040', '$null$', val('SM')).
fixm_FlightLevel(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031', '$null$', val('SM')).
fixm_FlightLevel(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037', '$null$', val('SM')).
fixm_FlightLevel(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034', '$null$', val('SM')).
fixm_FlightLevel(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025', '$null$', val('S')).
fixm_FlightLevel(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040', '$null$', val('SM')).
fixm_FlightLevel(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031', '$null$', val('SM')).
fixm_FlightLevel(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037', '$null$', val('SM')).
fixm_FlightLevel(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034', '$null$', val('SM')).
fixm_FlightLevel(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025', '$null$', val('S')).
fixm_FlightLevel(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040', '$null$', val('SM')).
fixm_FlightLevel(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031', '$null$', val('SM')).
fixm_FlightLevel(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037', '$null$', val('SM')).
fixm_FlightLevel(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034', '$null$', val('SM')).
fixm_FlightLevel(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_025', '$null$', val('S')).
fixm_FlightLevel(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_040', '$null$', val('SM')).
fixm_FlightLevel(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_031', '$null$', val('SM')).
fixm_FlightLevel(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_037', '$null$', val('SM')).
fixm_FlightLevel(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_034', '$null$', val('SM')).

% fixm_LateralOfftrack(Graph, LateralOfftrack, OfftrackDistance, OfftrackReason?)

% fixm_TemporalRange(Graph, TemporalRange, Earliest?, Latest?)

% fixm_Aircraft(Graph, Aircraft, AircraftColours?, AircraftQuantity?, EngineType?, AircraftAddress?, Capabilities?, Registration?, AircraftType?, WakeTurbulence?, AircraftPerformance?)
fixm_Aircraft(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$', '$null$', '$null$', val('3C65A1'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', val('DAIMA'), '$null$', val('J'), val('C')).
fixm_Aircraft(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$', '$null$', '$null$', val('3C65A1'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', val('DAIMA'), '$null$', val('J'), val('C')).
fixm_Aircraft(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$', '$null$', '$null$', val('3C65A1'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', val('DAIMA'), '$null$', val('J'), val('C')).
fixm_Aircraft(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$', '$null$', '$null$', val('3C65A1'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', val('DAIMA'), '$null$', val('J'), val('C')).
fixm_Aircraft(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$', '$null$', '$null$', val('3C65A1'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', val('DAIMA'), '$null$', val('J'), val('C')).
fixm_Aircraft(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$', '$null$', '$null$', val('3C65A1'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', val('DAIMA'), '$null$', val('J'), val('C')).
fixm_Aircraft(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$', '$null$', '$null$', val('3C65A1'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', val('DAIMA'), '$null$', val('J'), val('C')).
fixm_Aircraft(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$', '$null$', '$null$', val('3C65A1'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', val('DAIMA'), '$null$', val('J'), val('C')).
fixm_Aircraft(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$', '$null$', '$null$', val('3C65A1'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', val('DAIMA'), '$null$', val('J'), val('C')).
fixm_Aircraft(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_003', '$null$', '$null$', '$null$', val('3C65A1'), 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_012', val('DAIMA'), '$null$', val('J'), val('C')).

% fixm_OnlineContact(Graph, OnlineContact, Email?)

% fixm_AirspaceConstraint(Graph, AirspaceConstraint, AirspaceControlledEntryTime?, ConstrainedAirspace?)

% fixm_TimeSequence(Graph, TimeSequence, Approval?, Begin?, End?, Ready?, Request?)

% fixm_AdditionalHandlingInformation(Graph, AdditionalHandlingInformation, ResponsibleAgent?)

% fixm_AtcUnitReference(Graph, AtcUnitReference, SectorIdentifier?, Delegated?)

% fixm_Extension(Graph, Extension)

% fixm_SurveillanceCapabilities(Graph, SurveillanceCapabilities, OtherSurveillanceCapabilities?, SurveillanceCode*)
fixm_SurveillanceCapabilities(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$', ['val:L', 'val:B1', 'val:D1']).
fixm_SurveillanceCapabilities(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$', ['val:L', 'val:D1', 'val:B1']).
fixm_SurveillanceCapabilities(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$', ['val:D1', 'val:B1', 'val:L']).
fixm_SurveillanceCapabilities(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$', ['val:D1', 'val:B1', 'val:L']).
fixm_SurveillanceCapabilities(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$', ['val:D1', 'val:B1', 'val:L']).
fixm_SurveillanceCapabilities(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$', ['val:B1', 'val:D1', 'val:L']).
fixm_SurveillanceCapabilities(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$', ['val:L', 'val:B1', 'val:D1']).
fixm_SurveillanceCapabilities(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$', ['val:D1', 'val:B1', 'val:L']).
fixm_SurveillanceCapabilities(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$', ['val:D1', 'val:L', 'val:B1']).
fixm_SurveillanceCapabilities(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_015', '$null$', ['val:L', 'val:B1', 'val:D1']).

% fixm_Trajectory(Graph, Trajectory, TrajectoryPoint*)
fixm_Trajectory(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022']).
fixm_Trajectory(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022']).
fixm_Trajectory(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022']).
fixm_Trajectory(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022']).
fixm_Trajectory(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022']).
fixm_Trajectory(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022']).
fixm_Trajectory(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022']).
fixm_Trajectory(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022']).
fixm_Trajectory(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022']).
fixm_Trajectory(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_021', ['https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_022']).

% aixm_AltimeterSourceTimeSlice(Graph, AltimeterSourceTimeSlice, IsRemote?, IsPrimary?, Availability*, Annotation*)

% aixm_Point(Graph, Point, HorizontalAccuracy?, Annotation*)
aixm_Point(graph:'1_donlon-data.ttl', s1:'elpoint1EADH', '$null$', []).
aixm_Point(graph:'3_donlon-data.ttl', s1:'elpoint1EADH', '$null$', []).
aixm_Point(graph:'2_donlon-data.ttl', s1:'elpoint1EADH', '$null$', []).
aixm_Point(graph:'7_donlon-data.ttl', s1:'elpoint1EADH', '$null$', []).
aixm_Point(graph:'4_donlon-data.ttl', s1:'elpoint1EADH', '$null$', []).
aixm_Point(graph:'8_donlon-data.ttl', s1:'elpoint1EADH', '$null$', []).
aixm_Point(graph:'9_donlon-data.ttl', s1:'elpoint1EADH', '$null$', []).
aixm_Point(graph:'6_donlon-data.ttl', s1:'elpoint1EADH', '$null$', []).
aixm_Point(graph:'5_donlon-data.ttl', s1:'elpoint1EADH', '$null$', []).
aixm_Point(graph:'0_donlon-data.ttl', s1:'elpoint1EADH', '$null$', []).

% aixm_AircraftCharacteristic(Graph, AircraftCharacteristic, Type?, Engine?, NumberEngine?, TypeAircraftICAO?, AircraftLandingCategory?, WingSpan?, WingSpanInterpretation?, ClassWingSpan?, Weight?, WeightInterpretation?, Passengers?, PassengersInterpretation?, Speed?, SpeedInterpretation?, WakeTurbulence?, NavigationEquipment?, NavigationSpecification?, VerticalSeparationCapability?, AntiCollisionAndSeparationEquipment?, CommunicationEquipment?, SurveillanceEquipment?, Annotation*)

% aixm_PostalAddress(Graph, PostalAddress, DeliveryPoint?, City?, AdministrativeArea?, PostalCode?, Country?)

% fixm_DangerousGoodsPackage(Graph, DangerousGoodsPackage, DangerousGoodsQuantity?, PackageDimensions?, PackingInstructionNumber?, ProductName?, ProperShippingName?, ReportableQuantity?, SupplementaryInformation?, TechnicalName?, TypeOfPackaging?, UnNumber?, DangerousGoodsLimitation?, ShipmentType?, AllPackedInOne?, CompatibilityGroup?, ShipmentDimensions?, MarinePollutantIndicator?, RadioactiveMaterials?, HazardClass?, PackingGroup?, Temperatures?, OverpackIndicator?, SubsidiaryHazardClass)

% fixm_LastPositionReport(Graph, LastPositionReport, DeterminationMethod?, Position?, TimeAtPosition?)

% aixm_AltimeterSourceStatus(Graph, AltimeterSourceStatus, OperationalStatus?)

% fixm_DangerousGoodsDimensions(Graph, DangerousGoodsDimensions, GrossWeight?, NetWeight?, Volume?)

% fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules?)
fixm_EfplRoute(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', val('IFR')).
fixm_EfplRoute(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', val('IFR')).
fixm_EfplRoute(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', val('IFR')).
fixm_EfplRoute(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', val('IFR')).
fixm_EfplRoute(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', val('IFR')).
fixm_EfplRoute(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', val('IFR')).
fixm_EfplRoute(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', val('IFR')).
fixm_EfplRoute(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', val('IFR')).
fixm_EfplRoute(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', val('IFR')).
fixm_EfplRoute(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_020', val('IFR')).

% fixm_CoordinationStatus(Graph, CoordinationStatus, AbrogationReason?, CoordinationStatus?, NonStandardCommunicationReason?, ReleaseConditions?)

% fixm_BoundaryCrossing(Graph, BoundaryCrossing, Altitude?, CrossingPoint?, CrossingSpeed?, CrossingTime?, Offtrack?, AltitudeInTransition?)

% fixm_IcaoAerodromeReference(Graph, IcaoAerodromeReference, Code?)
fixm_IcaoAerodromeReference(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', val('VHHH')).
fixm_IcaoAerodromeReference(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', val('ZGGG')).
fixm_IcaoAerodromeReference(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', val('VHHH')).
fixm_IcaoAerodromeReference(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', val('ZGGG')).
fixm_IcaoAerodromeReference(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', val('VHHH')).
fixm_IcaoAerodromeReference(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', val('ZGGG')).
fixm_IcaoAerodromeReference(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', val('VHHH')).
fixm_IcaoAerodromeReference(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', val('ZGGG')).
fixm_IcaoAerodromeReference(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', val('VHHH')).
fixm_IcaoAerodromeReference(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', val('ZGGG')).
fixm_IcaoAerodromeReference(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', val('VHHH')).
fixm_IcaoAerodromeReference(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', val('ZGGG')).
fixm_IcaoAerodromeReference(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', val('VHHH')).
fixm_IcaoAerodromeReference(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', val('ZGGG')).
fixm_IcaoAerodromeReference(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', val('VHHH')).
fixm_IcaoAerodromeReference(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', val('ZGGG')).
fixm_IcaoAerodromeReference(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', val('VHHH')).
fixm_IcaoAerodromeReference(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', val('ZGGG')).
fixm_IcaoAerodromeReference(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', val('VHHH')).
fixm_IcaoAerodromeReference(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', val('EDDF')).
fixm_IcaoAerodromeReference(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', val('ZGGG')).

% fixm_RadioCommunicationFailure(Graph, RadioCommunicationFailure, RadioFailureRemarks?, RemainingComCapability?, Contact?)

% aixm_AirportHeliportAvailability(Graph, AirportHeliportAvailability, OperationalStatus?, Warning?, Usage*)
aixm_AirportHeliportAvailability(graph:'4_donlon-data.ttl', s1:'AHY_EADH_PERMIT', val('NORMAL'), '$null$', [s1:'AHU_EADH_PERMIT']).
aixm_AirportHeliportAvailability(graph:'9_donlon-data.ttl', s2:'ID_ACT_13', val('CLOSED'), '$null$', [s2:'ID_ACT_14']).
aixm_AirportHeliportAvailability(graph:'7_donlon-data.ttl', s2:'ID_ACT_13', val('CLOSED'), '$null$', [s2:'ID_ACT_14']).
aixm_AirportHeliportAvailability(graph:'8_donlon-data.ttl', s1:'AHY_EADH_PERMIT', val('NORMAL'), '$null$', [s1:'AHU_EADH_PERMIT']).
aixm_AirportHeliportAvailability(graph:'0_donlon-data.ttl', s1:'AHY_EADH_PERMIT', val('NORMAL'), '$null$', [s1:'AHU_EADH_PERMIT']).
aixm_AirportHeliportAvailability(graph:'6_donlon-data.ttl', s1:'AHY_EADH_PERMIT', val('NORMAL'), '$null$', [s1:'AHU_EADH_PERMIT']).
aixm_AirportHeliportAvailability(graph:'5_donlon-data.ttl', s2:'ID_ACT_13', val('CLOSED'), '$null$', [s2:'ID_ACT_14']).
aixm_AirportHeliportAvailability(graph:'1_donlon-data.ttl', s2:'ID_ACT_13', val('CLOSED'), '$null$', [s2:'ID_ACT_14']).
aixm_AirportHeliportAvailability(graph:'3_donlon-data.ttl', s2:'ID_ACT_13', val('CLOSED'), '$null$', [s2:'ID_ACT_14']).
aixm_AirportHeliportAvailability(graph:'2_donlon-data.ttl', s1:'AHY_EADH_PERMIT', val('NORMAL'), '$null$', [s1:'AHU_EADH_PERMIT']).
aixm_AirportHeliportAvailability(graph:'7_donlon-data.ttl', s1:'AHY_EADH_PERMIT', val('NORMAL'), '$null$', [s1:'AHU_EADH_PERMIT']).
aixm_AirportHeliportAvailability(graph:'8_donlon-data.ttl', s2:'ID_ACT_13', val('CLOSED'), '$null$', [s2:'ID_ACT_14']).
aixm_AirportHeliportAvailability(graph:'4_donlon-data.ttl', s2:'ID_ACT_13', val('CLOSED'), '$null$', [s2:'ID_ACT_14']).
aixm_AirportHeliportAvailability(graph:'9_donlon-data.ttl', s1:'AHY_EADH_PERMIT', val('NORMAL'), '$null$', [s1:'AHU_EADH_PERMIT']).
aixm_AirportHeliportAvailability(graph:'5_donlon-data.ttl', s1:'AHY_EADH_PERMIT', val('NORMAL'), '$null$', [s1:'AHU_EADH_PERMIT']).
aixm_AirportHeliportAvailability(graph:'0_donlon-data.ttl', s2:'ID_ACT_13', val('CLOSED'), '$null$', [s2:'ID_ACT_14']).
aixm_AirportHeliportAvailability(graph:'6_donlon-data.ttl', s2:'ID_ACT_13', val('CLOSED'), '$null$', [s2:'ID_ACT_14']).
aixm_AirportHeliportAvailability(graph:'1_donlon-data.ttl', s1:'AHY_EADH_PERMIT', val('NORMAL'), '$null$', [s1:'AHU_EADH_PERMIT']).
aixm_AirportHeliportAvailability(graph:'2_donlon-data.ttl', s2:'ID_ACT_13', val('CLOSED'), '$null$', [s2:'ID_ACT_14']).
aixm_AirportHeliportAvailability(graph:'3_donlon-data.ttl', s1:'AHY_EADH_PERMIT', val('NORMAL'), '$null$', [s1:'AHU_EADH_PERMIT']).

% fixm_FlightArrival(Graph, FlightArrival, ApproachFix?, ApproachTime?, ArrivalAerodrome?, ArrivalAerodromeAlternate*, ArrivalAerodromeOriginal?, ArrivalFix?, ArrivalFixTime?, ArrivalFleetPrioritization?, ArrivalSequenceNumber?, EarliestInBlockTime?, FiledRevisedDestinationAerodrome?, FiledRevisedDestinationStar?, RunwayPositionAndTime?, StandardInstrumentArrival?, StandPositionAndTime?, LandingLimits?)

% fixm_RadioactiveMaterial(Graph, RadioactiveMaterial, CriticalitySafetyIndex?, TransportIndex?, FissileExceptedIndicator?, Category?, Radionuclide?)

% fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled?, Initial?)

% fixm_ControlElement(Graph, ControlElement)

% fixm_AerodromesOfDestination(Graph, AerodromesOfDestination, AerodromeOfDestination?, Alternate1?, Alternate2?, FiledRevisedDestinationAerodrome?)
fixm_AerodromesOfDestination(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', '$null$', '$null$').
fixm_AerodromesOfDestination(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', '$null$', '$null$').
fixm_AerodromesOfDestination(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', '$null$', '$null$').
fixm_AerodromesOfDestination(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', '$null$', '$null$').
fixm_AerodromesOfDestination(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', '$null$', '$null$').
fixm_AerodromesOfDestination(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', '$null$', '$null$').
fixm_AerodromesOfDestination(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', '$null$', '$null$').
fixm_AerodromesOfDestination(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', '$null$', '$null$').
fixm_AerodromesOfDestination(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', '$null$', '$null$').
fixm_AerodromesOfDestination(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_007', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019', '$null$', '$null$').

% fixm_AllPackedInOne(Graph, AllPackedInOne, NumberOfPackages?, QValue?)

% aixm_AltimeterSource(Graph, AltimeterSource, TimeSlice*)

% fixm_SurvivalCapabilities(Graph, SurvivalCapabilities, SurvivalEquipmentRemarks?, DinghyInformation?, EmergencyRadioCode*, LifeJacketCode*, SurvivalEquipmentCode*)

% fixm_DirectRouting(Graph, DirectRouting, From?, To?)

% fixm_TargetMultiTime(Graph, TargetMultiTime, Target?)

% fixm_AircraftType(Graph, AircraftType)
fixm_AircraftType(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_011').
fixm_AircraftType(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_011').
fixm_AircraftType(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_011').
fixm_AircraftType(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_011').
fixm_AircraftType(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_011').
fixm_AircraftType(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_011').
fixm_AircraftType(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_011').
fixm_AircraftType(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_011').
fixm_AircraftType(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_011').
fixm_AircraftType(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_011').

% fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome?, DepartureFix?, DepartureFixTime?, DepartureFleetPrioritization?, DepartureSlot?, EarliestOffBlockTime?, OffBlockReadyTime?, RunwayPositionAndTime?, StandardInstrumentDeparture?, StandPositionAndTime?, TakeoffAlternateAerodrome*, TakeoffWeight?, DepartureTimes?)
fixm_FlightDeparture(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], '$null$', '$null$').
fixm_FlightDeparture(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], '$null$', '$null$').
fixm_FlightDeparture(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], '$null$', '$null$').
fixm_FlightDeparture(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], '$null$', '$null$').
fixm_FlightDeparture(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], '$null$', '$null$').
fixm_FlightDeparture(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], '$null$', '$null$').
fixm_FlightDeparture(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], '$null$', '$null$').
fixm_FlightDeparture(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], '$null$', '$null$').
fixm_FlightDeparture(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], '$null$', '$null$').
fixm_FlightDeparture(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', '$null$', [], '$null$', '$null$').

% fixm_AerodromeReference(Graph, AerodromeReference)
fixm_AerodromeReference(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018').
fixm_AerodromeReference(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027').
fixm_AerodromeReference(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016').
fixm_AerodromeReference(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019').
fixm_AerodromeReference(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018').
fixm_AerodromeReference(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027').
fixm_AerodromeReference(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016').
fixm_AerodromeReference(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019').
fixm_AerodromeReference(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018').
fixm_AerodromeReference(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027').
fixm_AerodromeReference(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016').
fixm_AerodromeReference(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019').
fixm_AerodromeReference(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018').
fixm_AerodromeReference(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027').
fixm_AerodromeReference(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016').
fixm_AerodromeReference(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019').
fixm_AerodromeReference(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018').
fixm_AerodromeReference(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027').
fixm_AerodromeReference(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016').
fixm_AerodromeReference(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019').
fixm_AerodromeReference(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018').
fixm_AerodromeReference(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027').
fixm_AerodromeReference(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016').
fixm_AerodromeReference(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019').
fixm_AerodromeReference(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018').
fixm_AerodromeReference(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027').
fixm_AerodromeReference(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016').
fixm_AerodromeReference(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019').
fixm_AerodromeReference(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018').
fixm_AerodromeReference(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027').
fixm_AerodromeReference(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016').
fixm_AerodromeReference(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019').
fixm_AerodromeReference(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018').
fixm_AerodromeReference(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027').
fixm_AerodromeReference(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016').
fixm_AerodromeReference(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019').
fixm_AerodromeReference(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_018').
fixm_AerodromeReference(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_027').
fixm_AerodromeReference(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_016').
fixm_AerodromeReference(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_019').

% fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime?, TaxiTime?)
fixm_EfplFlightDeparture(graph:'3_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', val('P0DT0H16M0S')).
fixm_EfplFlightDeparture(graph:'7_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', val('P0DT0H16M0S')).
fixm_EfplFlightDeparture(graph:'9_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', val('P0DT0H16M0S')).
fixm_EfplFlightDeparture(graph:'2_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', val('P0DT0H16M0S')).
fixm_EfplFlightDeparture(graph:'5_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', val('P0DT0H16M0S')).
fixm_EfplFlightDeparture(graph:'0_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', val('P0DT0H16M0S')).
fixm_EfplFlightDeparture(graph:'4_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', val('P0DT0H16M0S')).
fixm_EfplFlightDeparture(graph:'6_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', val('P0DT0H16M0S')).
fixm_EfplFlightDeparture(graph:'1_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', val('P0DT0H16M0S')).
fixm_EfplFlightDeparture(graph:'8_FIXM_EDDF-VHHH.ttl', 'https://www.fixm.aero/releases/SESAR_Ext-1.0/SESAR_E-FPL_Extension_v1.0_beta.zip/EDDF-VHHH_FIXMCreationRequest.xml#ID_004', '$null$', val('P0DT0H16M0S')).

% aixm_UsageCondition(Graph, UsageCondition, Type?, PriorPermission?, Selection?, Annotation*, Contact*)
aixm_UsageCondition(graph:'7_donlon-data.ttl', s1:'AHU_EADH_PERMIT', val('PERMIT'), '$null$', s1:'agtayyat', [s1:'annotation1'], []).
aixm_UsageCondition(graph:'9_donlon-data.ttl', s1:'AHU_EADH_PERMIT', val('PERMIT'), '$null$', s1:'agtayyat', [s1:'annotation1'], []).
aixm_UsageCondition(graph:'2_donlon-data.ttl', s2:'ID_ACT_14', val('PERMIT'), '$null$', s2:'ID_ACT_15', [], []).
aixm_UsageCondition(graph:'5_donlon-data.ttl', s1:'AHU_EADH_PERMIT', val('PERMIT'), '$null$', s1:'agtayyat', [s1:'annotation1'], []).
aixm_UsageCondition(graph:'6_donlon-data.ttl', s2:'ID_ACT_14', val('PERMIT'), '$null$', s2:'ID_ACT_15', [], []).
aixm_UsageCondition(graph:'0_donlon-data.ttl', s2:'ID_ACT_14', val('PERMIT'), '$null$', s2:'ID_ACT_15', [], []).
aixm_UsageCondition(graph:'1_donlon-data.ttl', s1:'AHU_EADH_PERMIT', val('PERMIT'), '$null$', s1:'agtayyat', [s1:'annotation1'], []).
aixm_UsageCondition(graph:'4_donlon-data.ttl', s2:'ID_ACT_14', val('PERMIT'), '$null$', s2:'ID_ACT_15', [], []).
aixm_UsageCondition(graph:'8_donlon-data.ttl', s2:'ID_ACT_14', val('PERMIT'), '$null$', s2:'ID_ACT_15', [], []).
aixm_UsageCondition(graph:'3_donlon-data.ttl', s1:'AHU_EADH_PERMIT', val('PERMIT'), '$null$', s1:'agtayyat', [s1:'annotation1'], []).
aixm_UsageCondition(graph:'4_donlon-data.ttl', s1:'AHU_EADH_PERMIT', val('PERMIT'), '$null$', s1:'agtayyat', [s1:'annotation1'], []).
aixm_UsageCondition(graph:'3_donlon-data.ttl', s2:'ID_ACT_14', val('PERMIT'), '$null$', s2:'ID_ACT_15', [], []).
aixm_UsageCondition(graph:'8_donlon-data.ttl', s1:'AHU_EADH_PERMIT', val('PERMIT'), '$null$', s1:'agtayyat', [s1:'annotation1'], []).
aixm_UsageCondition(graph:'0_donlon-data.ttl', s1:'AHU_EADH_PERMIT', val('PERMIT'), '$null$', s1:'agtayyat', [s1:'annotation1'], []).
aixm_UsageCondition(graph:'1_donlon-data.ttl', s2:'ID_ACT_14', val('PERMIT'), '$null$', s2:'ID_ACT_15', [], []).
aixm_UsageCondition(graph:'6_donlon-data.ttl', s1:'AHU_EADH_PERMIT', val('PERMIT'), '$null$', s1:'agtayyat', [s1:'annotation1'], []).
aixm_UsageCondition(graph:'5_donlon-data.ttl', s2:'ID_ACT_14', val('PERMIT'), '$null$', s2:'ID_ACT_15', [], []).
aixm_UsageCondition(graph:'7_donlon-data.ttl', s2:'ID_ACT_14', val('PERMIT'), '$null$', s2:'ID_ACT_15', [], []).
aixm_UsageCondition(graph:'2_donlon-data.ttl', s1:'AHU_EADH_PERMIT', val('PERMIT'), '$null$', s1:'agtayyat', [s1:'annotation1'], []).
aixm_UsageCondition(graph:'9_donlon-data.ttl', s2:'ID_ACT_14', val('PERMIT'), '$null$', s2:'ID_ACT_15', [], []).

fixm_ExpandedRoutePoint_Combined(Graph, ExpandedRoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit, EstimatedLevel, EstimatedTime, Constraint) :-
  fixm_ExpandedRoutePoint(Graph, ExpandedRoutePoint, EstimatedLevel, EstimatedTime, Constraint),
  fixm_AbstractRoutePoint(Graph, ExpandedRoutePoint, AirTrafficType, DelayAtPoint, FlightRules, Point, ClearanceLimit) .

aixm_ElevatedSurface_Combined(Graph, ElevatedSurface, HorizontalAccuracy, Annotation, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  aixm_ElevatedSurface(Graph, ElevatedSurface, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy),
  aixm_Surface(Graph, ElevatedSurface, HorizontalAccuracy, Annotation) .

aixm_ConditionCombination_Combined(Graph, ConditionCombination, Annotation, SpecialDateAuthority, TimeInterval, LogicalOperator, Flight, Aircraft, Weather, SubCondition) :-
  aixm_ConditionCombination(Graph, ConditionCombination, LogicalOperator, Flight, Aircraft, Weather, SubCondition),
  aixm_PropertiesWithSchedule(Graph, ConditionCombination, Annotation, SpecialDateAuthority, TimeInterval) .

aixm_ElevatedPoint_Combined(Graph, ElevatedPoint, HorizontalAccuracy, Annotation, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy) :-
  aixm_ElevatedPoint(Graph, ElevatedPoint, Elevation, GeoidUndulation, VerticalDatum, VerticalAccuracy),
  aixm_Point(Graph, ElevatedPoint, HorizontalAccuracy, Annotation) .

fixm_EfplPoint4D_Combined(Graph, EfplPoint4D, Altitude, Time, PointRange, FlightLevel) :-
  fixm_EfplPoint4D(Graph, EfplPoint4D, FlightLevel),
  fixm_Point4D(Graph, EfplPoint4D, Altitude, Time, PointRange) .

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

fixm_Point4D_Combined(Graph, Point4D, Pos, SrsName, Altitude, Time, PointRange) :-
  fixm_Point4D(Graph, Point4D, Altitude, Time, PointRange),
  fixm_GeographicLocation(Graph, Point4D, Pos, SrsName) .

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

fixm_EfplFlight_Combined(Graph, EfplFlight, ControllingUnit, Extensions, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoods, RankedTrajectories, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandling, IfplId, TotalEstimatedElapsedTime, AerodromesOfDestination, EfplSpecialHandling, EfplFiledTrajectory, EfplAcceptedTrajectory, OtherInformation, FlightPerformanceData) :-
  fixm_EfplFlight(Graph, EfplFlight, IfplId, TotalEstimatedElapsedTime, AerodromesOfDestination, EfplSpecialHandling, EfplFiledTrajectory, EfplAcceptedTrajectory, OtherInformation, FlightPerformanceData),
  fixm_Flight(Graph, EfplFlight, ControllingUnit, Extensions, FlightFiler, Gufi, Remarks, AircraftDescription, DangerousGoods, RankedTrajectories, RouteToRevisedDestination, Negotiating, Agreed, Arrival, Departure, Emergency, RadioCommunicationFailure, EnRoute, Operator, EnRouteDiversion, FlightType, FlightStatus, Originator, SupplementalData, FlightIdentification, SpecialHandling) .

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

aixm_Surface_Combined(Graph, Surface, Patch, HorizontalAccuracy, Annotation) :-
  aixm_Surface(Graph, Surface, HorizontalAccuracy, Annotation),
  gml_Surface(Graph, Surface, Patch) .

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

fixm_EfplRoute_Combined(Graph, EfplRoute, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTime, ExpandedRoute, ClimbSchedule, DescentSchedule, Segment, EfplFlightRules) :-
  fixm_EfplRoute(Graph, EfplRoute, EfplFlightRules),
  fixm_Route(Graph, EfplRoute, AirfileRouteStartTime, FlightDuration, InitialCruisingSpeed, InitialFlightRules, RequestedAltitude, RouteText, EstimatedElapsedTime, ExpandedRoute, ClimbSchedule, DescentSchedule, Segment) .

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

fixm_ExtendedMultiTime_Combined(Graph, ExtendedMultiTime, Target, Controlled, Initial) :-
  fixm_ExtendedMultiTime(Graph, ExtendedMultiTime, Controlled, Initial),
  fixm_TargetMultiTime(Graph, ExtendedMultiTime, Target) .

fixm_TargetMultiTime_Combined(Graph, TargetMultiTime, Actual, Estimated, Target) :-
  fixm_TargetMultiTime(Graph, TargetMultiTime, Target),
  fixm_MultiTime(Graph, TargetMultiTime, Actual, Estimated) .

fixm_FlightDeparture_Combined(Graph, FlightDeparture, Provenance, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes) :-
  fixm_FlightDeparture(Graph, FlightDeparture, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes),
  fixm_Feature(Graph, FlightDeparture, Provenance) .

fixm_EfplFlightDeparture_Combined(Graph, EfplFlightDeparture, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes, EstimatedOffBlockTime, TaxiTime) :-
  fixm_EfplFlightDeparture(Graph, EfplFlightDeparture, EstimatedOffBlockTime, TaxiTime),
  fixm_FlightDeparture(Graph, EfplFlightDeparture, DepartureAerodrome, DepartureFix, DepartureFixTime, DepartureFleetPrioritization, DepartureSlot, EarliestOffBlockTime, OffBlockReadyTime, RunwayPositionAndTime, StandardInstrumentDeparture, StandPositionAndTime, TakeoffAlternateAerodrome, TakeoffWeight, DepartureTimes) .

