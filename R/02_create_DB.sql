CREATE TABLE Species(
  speciesID INT PRIMARY KEY NOT NULL,
  scientificName VARCHAR(200),
  genus VARCHAR(200),
  family VARCHAR(200)
);

CREATE TABLE MonitoringProgram(
  MonitoringProgramID INT PRIMARY KEY NOT NULL,
  MonitoringProgramName VARCHAR(200),
  contactName VARCHAR(200),
  contactEmail VARCHAR(200),
  samplingProtocol VARCHAR(200)
);

CREATE TABLE Location(
  locationID INT PRIMARY KEY NOT NULL,
  locality VARCHAR(200),
  monitoringProgram INT REFERENCES MonitoringProgram(MonitoringProgramID),
  decimalLongitude NUMERIC,
  decimalLatitude NUMERIC
);