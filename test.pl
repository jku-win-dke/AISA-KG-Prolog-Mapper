:- [factsA].
:- [sparqlB].
:- [factsC].


mismatch(inA_notinB,gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition)) :-
  a:gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition), 
  \+ b:gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition).

mismatch(inB_notinA,gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition)) :-  
  b:gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition), 
  \+ a:gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition).
  
mismatch(inA_notinC,gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition)) :-
  a:gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition), 
  \+ c:gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition).
  
mismatch(inC_notinA,gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition)) :-
  c:gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition), 
  \+ a:gml_TimePeriod(Graph, TimePeriod, BeginPosition, EndPosition).  

  
mismatch(inA_notinB,fixm_MeteorologicalData(Graph, MeteorologicalData, TemperatureVal, WindDirectionVal, WindSpeedVal)) :-
  a:fixm_MeteorologicalData(Graph, MeteorologicalData, TemperatureVal, WindDirectionVal, WindSpeedVal), 
  \+ b:fixm_MeteorologicalData(Graph, MeteorologicalData, TemperatureVal, WindDirectionVal, WindSpeedVal).

mismatch(inB_notinA,fixm_MeteorologicalData(Graph, MeteorologicalData, TemperatureVal, WindDirectionVal, WindSpeedVal)) :-  
  b:fixm_MeteorologicalData(Graph, MeteorologicalData, TemperatureVal, WindDirectionVal, WindSpeedVal), 
  \+ a:fixm_MeteorologicalData(Graph, MeteorologicalData, TemperatureVal, WindDirectionVal, WindSpeedVal).
  
mismatch(inA_notinC,fixm_MeteorologicalData(Graph, MeteorologicalData, TemperatureVal, WindDirectionVal, WindSpeedVal)) :-
  a:fixm_MeteorologicalData(Graph, MeteorologicalData, TemperatureVal, WindDirectionVal, WindSpeedVal), 
  \+ c:fixm_MeteorologicalData(Graph, MeteorologicalData, TemperatureVal, WindDirectionVal, WindSpeedVal).
  
mismatch(inC_notinA,fixm_MeteorologicalData(Graph, MeteorologicalData, TemperatureVal, WindDirectionVal, WindSpeedVal)) :-
  c:fixm_MeteorologicalData(Graph, MeteorologicalData, TemperatureVal, WindDirectionVal, WindSpeedVal), 
  \+ a:fixm_MeteorologicalData(Graph, MeteorologicalData, TemperatureVal, WindDirectionVal, WindSpeedVal).  
  
  
% fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile)  
mismatch(inA_notinB,fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile)) :-
  a:fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile), 
  \+ b:fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile).

mismatch(inB_notinA,fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile)) :-  
  b:fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile), 
  \+ a:fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile).
  
mismatch(inA_notinC,fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile)) :-
  a:fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile), 
  \+ c:fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile).
  
mismatch(inC_notinA,fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile)) :-
  c:fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile), 
  \+ a:fixm_FlightPerformanceData(Graph, FlightPerformanceData, ClimbProfile, DescentProfile).  

% aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability)
mismatch(inA_notinB,aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability)) :-
  a:aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability), 
  \+ b:aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability).

mismatch(inB_notinA,aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability)) :-  
  b:aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability), 
  \+ a:aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability).
  
mismatch(inA_notinC,aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability)) :-
  a:aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability), 
  \+ c:aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability).
  
mismatch(inC_notinA,aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability)) :-
  c:aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability), 
  \+ a:aixm_AirportHeliportTimeSlice(Graph, AirportHeliportTimeSlice, Designator, Name, LocationIndicatorICAO, DesignatorIATA, Type, CertifiedICAO, PrivateUse, ControlType, FieldElevation, FieldElevationAccuracy, VerticalDatum, MagneticVariation, MagneticVariationAccuracy, DateMagneticVariation, MagneticVariationChange, ReferenceTemperature, AltimeterCheckLocation, SecondaryPowerSupply, WindDirectionIndicator, LandingDirectionIndicator, TransitionAltitude, TransitionLevel, LowestTemperature, Abandoned, CertificationDate, CertificationExpirationDate, Contact, Annotation, ARP, AltimeterSource, Contaminant, ServedCity, ResponsibleOrganisation, AviationBoundary, Availability).  

