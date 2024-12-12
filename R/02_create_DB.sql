-- Remove tables
DROP SCHEMA public CASCADE;
CREATE SCHEMA public;

-- Activate extrnsion
CREATE EXTENSION Postgis;

CREATE TABLE Species(
  speciesID INT PRIMARY KEY NOT NULL,
  scientificName VARCHAR(200),
  genus VARCHAR(200),
  family VARCHAR(200)
);

CREATE TABLE MonitoringProgram(
  monitoringProgramID INT PRIMARY KEY NOT NULL,
  monitoringProgramName VARCHAR(200),
  contactName VARCHAR(200),
  contactEmail VARCHAR(200),
  samplingProtocol VARCHAR(200)
);

CREATE TABLE Location(
  locationID INT PRIMARY KEY NOT NULL,
  locality VARCHAR(200),
  monitoringProgram INT REFERENCES MonitoringProgram(MonitoringProgramID),
  -- decimalLongitude NUMERIC,
  -- decimalLatitude NUMERIC,
  decimalCoordinates geography(POINT, 4326),
  polyline geography(LINESTRING, 4326),
  polygon geography(POLYGON, 4326),
  coordinateUncertaintyInMeters NUMERIC CHECK (coordinateUncertaintyInMeters > 0),
  locationType VARCHAR(100),
  country VARCHAR(100)
);

CREATE TABLE Observer(
  observerID INT PRIMARY KEY NOT NULL,
  name VARCHAR(100),
  url varchar(200)
);

CREATE TABLE Event(
  eventID INT PRIMARY KEY NOT NULL,
  observer INT REFERENCES Observer(observerID),
  location INT REFERENCES Location(LocationID),
  year INT,
  month INT,
  day INT,
  eventTime TIME without time zone,
  eventDateUncertainty VARCHAR(100),
  samplingEffort NUMERIC,
  monitoringProgram INT REFERENCES MonitoringProgram(monitoringProgramID),
  wind VARCHAR(100),
  cloudCover VARCHAR(100),
  eventRemarks VARCHAR(500)
  );
  
  CREATE TABLE Occurrence(
    occurrenceID INT PRIMARY KEY NOT NULL,
    event INT REFERENCES Event(eventID),
    species INT REFERENCES Species(speciesID),
    location INT REFERENCES Location(LocationID),
    lifeStage VARCHAR(100),
    sex VARCHAR(50),
    behavior VARCHAR(50),
    count INT,
    identificationVerificationStatus INT,
    associatedMedia VARCHAR(300),
    taxonRank VARCHAR(100)
  );