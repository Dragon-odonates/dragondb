-- Remove tables
DROP TABLE IF EXISTS "Species" CASCADE;
DROP TABLE IF EXISTS "ParentDataset" CASCADE;
DROP TABLE IF EXISTS "Contact" CASCADE;
DROP TABLE IF EXISTS "Dataset" CASCADE;
DROP TABLE IF EXISTS "Location" CASCADE;
DROP TABLE IF EXISTS "Observer" CASCADE;
DROP TABLE IF EXISTS "Event" CASCADE;
DROP TABLE IF EXISTS "Occurrence" CASCADE;

-- Create tables
CREATE TABLE "Species"(
  "speciesID" SERIAL PRIMARY KEY NOT NULL,
  "scientificName" VARCHAR(200),
  "genus" VARCHAR(200),
  "family" VARCHAR(200)
);

CREATE TABLE "ParentDataset"(
  "parentDatasetID" SERIAL PRIMARY KEY NOT NULL,
  "parentDatasetName" VARCHAR(200),
  "samplingProtocol" VARCHAR(200)
);

CREATE TABLE "Contact"(
  "contactID" SERIAL PRIMARY KEY NOT NULL,
  "contactName" VARCHAR(200),
  "contactEmail" VARCHAR(200),
  "parentDataset" INT REFERENCES "ParentDataset"("parentDatasetID")
);

CREATE TABLE "Dataset"(
  "datasetID" SERIAL PRIMARY KEY NOT NULL,
  "datasetName" VARCHAR(200),
  "parentDataset" INT REFERENCES "ParentDataset"("parentDatasetID")
);

CREATE TABLE "Location"(
  "locationID" SERIAL PRIMARY KEY NOT NULL,
  "parentLocationID" INT,
  "locality" VARCHAR(200),
  "dataset" INT REFERENCES "Dataset"("datasetID"),
  "decimalCoordinates" geometry,
  -- "polyline" geography(LINESTRING, 4326),
  -- "polygon" geography(POLYGON, 4326),
  "coordinateUncertaintyInMeters" NUMERIC CHECK ("coordinateUncertaintyInMeters" > 0),
  -- "locationType" VARCHAR(100),
  "country" VARCHAR(100)
);

CREATE TABLE "Observer"(
  "observerID" SERIAL PRIMARY KEY NOT NULL,
  "name" VARCHAR(100),
  "url" varchar(200)
);

CREATE TABLE "Event"(
  "eventID" SERIAL PRIMARY KEY NOT NULL,
  "observer" INT REFERENCES "Observer"("observerID"),
  "location" INT REFERENCES "Location"("locationID"),
  "year" INT,
  "month" INT,
  "day" INT,
  "eventTime" TIME without time zone,
  "eventDateUncertainty" VARCHAR(100),
  "samplingEffort" NUMERIC,
  "dataset" INT REFERENCES "Dataset"("datasetID"),
  "wind" VARCHAR(100),
  "cloudCover" VARCHAR(100),
  "eventRemarks" VARCHAR(500)
  );
  
  CREATE TABLE "Occurrence"(
    "occurrenceID" SERIAL PRIMARY KEY NOT NULL,
    "event" INT REFERENCES "Event"("eventID"),
    "species" INT REFERENCES "Species"("speciesID"),
    "location" INT REFERENCES "Location"("locationID"),
    "lifeStage" VARCHAR(100),
    "sex" VARCHAR(50),
    "behavior" VARCHAR(50),
    count INT,
    "identificationVerificationStatus" INT,
    "associatedMedia" VARCHAR(300),
    "taxonRank" VARCHAR(100)
  );