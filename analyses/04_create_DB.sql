-- Remove tables
DROP TABLE IF EXISTS "Taxon" CASCADE;
DROP TABLE IF EXISTS "Recorder" CASCADE;
DROP TABLE IF EXISTS "Date" CASCADE;
DROP TABLE IF EXISTS "ParentDataset" CASCADE;
DROP TABLE IF EXISTS "Dataset" CASCADE;
DROP TABLE IF EXISTS "Contact" CASCADE;
DROP TABLE IF EXISTS "DatasetContact" CASCADE;
DROP TABLE IF EXISTS "ParentDatasetContact" CASCADE;
DROP TABLE IF EXISTS "Location" CASCADE;
DROP TABLE IF EXISTS "Event" CASCADE;
DROP TABLE IF EXISTS "Occurrence" CASCADE;

-- Create tables
CREATE TABLE "Taxon"(
  "taxonID" INT PRIMARY KEY NOT NULL,
  "scientificName" VARCHAR(60),
  "genus" VARCHAR(30),
  "family" VARCHAR(40),
  "taxonRank" VARCHAR(20)
);

CREATE TABLE "Recorder"(
  "recorderID" SERIAL PRIMARY KEY NOT NULL,
  "name" VARCHAR(100)
);

CREATE TABLE "Date"(
  "dateID" SERIAL PRIMARY KEY NOT NULL,
  "year" INT,
  "month" INT,
  "day" INT,
  "time" TIME without time zone,
  "eventDateUncertainty" VARCHAR(50)
  );

CREATE TABLE "ParentDataset"(
  "parentDatasetID" SERIAL PRIMARY KEY NOT NULL,
  "parentDatasetName" VARCHAR(20)
);

CREATE TABLE "Dataset"(
  "datasetID" SERIAL PRIMARY KEY NOT NULL,
  "datasetName" VARCHAR(20),
  "samplingProtocol" VARCHAR(20),
  "description" VARCHAR(200),
  "parentDataset" INT REFERENCES "ParentDataset"("parentDatasetID")
);

CREATE TABLE "Contact"(
  "contactID" SERIAL PRIMARY KEY NOT NULL,
  "contactName" VARCHAR(200),
  "contactEmail" VARCHAR(200)
);

CREATE TABLE "DatasetContact"(
  "dataset" INT REFERENCES "Dataset"("datasetID"),
  "contact" INT REFERENCES "Contact"("contactID"),
  PRIMARY KEY ("dataset", "contact")
);

CREATE TABLE "ParentDatasetContact"(
  "parentDataset" INT REFERENCES "ParentDataset"("parentDatasetID"),
  "contact" INT REFERENCES "Contact"("contactID"),
  PRIMARY KEY ("parentDataset", "contact")
);

CREATE TABLE "Location"(
  "locationID" SERIAL PRIMARY KEY NOT NULL,
  "decimalCoordinates" geometry,
  "coordinateUncertaintyInMeters" NUMERIC CHECK ("coordinateUncertaintyInMeters" > 0),
  "locality" VARCHAR(100),
  "municipality" VARCHAR(100),
  "county" VARCHAR(100),
  "country" VARCHAR(50)
);

CREATE TABLE "Event"(
  "eventID" SERIAL PRIMARY KEY NOT NULL,
  "location" INT REFERENCES "Location"("locationID"),
  "date" INT REFERENCES "Date"("dateID"),
  "recorder" INT REFERENCES "Recorder"("recorderID"),
  "dataset" INT REFERENCES "Dataset"("datasetID"),
  "samplingEffort" NUMERIC,
  "wind" VARCHAR(50),
  "cloudCover" VARCHAR(50),
  "elevation" NUMERIC,
  "eventRemarks" VARCHAR(200)
);

CREATE TABLE "Occurrence"(
  "occurrenceID" SERIAL PRIMARY KEY NOT NULL,
  "event" INT REFERENCES "Event"("eventID"),
  "taxon" INT REFERENCES "Taxon"("taxonID"),
  "location" INT REFERENCES "Location"("locationID"),
  "date" INT REFERENCES "Date"("dateID"),
  "individualCount" INT,
  "lifeStage" VARCHAR(20),
  "sex" VARCHAR(10),
  "behavior" VARCHAR(100),
  "degreeOfEstablishment" INT,
  "identificationVerificationStatus" INT,
  "associatedMedia" VARCHAR(200),
  "embargoDate" DATE,
  "accessRights" VARCHAR(50)
);
