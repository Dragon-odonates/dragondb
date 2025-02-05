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
  "scientificName" VARCHAR(60) UNIQUE,
  "genus" VARCHAR(30),
  "family" VARCHAR(40) NOT NULL,
  "taxonRank" VARCHAR(20) NOT NULL
  CONSTRAINT spp_taxo CHECK ("scientificName" IS NULL OR ("genus" IS NOT NULL AND "family" IS NOT NULL))
  CONSTRAINT genus_taxo CHECK ("genus" IS NULL OR "family" IS NOT NULL)
);

CREATE TABLE "Recorder"(
  "recorderID" SERIAL PRIMARY KEY NOT NULL,
  "recorderID_orig" VARCHAR(100),
  "name" VARCHAR(300),
  UNIQUE NULLS NOT DISTINCT ("recorderID_orig", "name")
);

CREATE TABLE "Date"(
  "dateID" SERIAL PRIMARY KEY NOT NULL,
  "date" date,
  "time" TIME without time zone,
  "dateUncertainty" VARCHAR(50),
  UNIQUE NULLS NOT DISTINCT ("date", "time", "dateUncertainty")
  );

CREATE TABLE "ParentDataset"(
  "parentDatasetID" VARCHAR(4) PRIMARY KEY NOT NULL,
  "parentDatasetName" VARCHAR(20) UNIQUE
);

CREATE TABLE "Dataset"(
  "datasetID" VARCHAR(4) PRIMARY KEY NOT NULL,
  "datasetName" VARCHAR(200) UNIQUE,
  "samplingProtocol" VARCHAR(20) CHECK ("samplingProtocol" IN ('transect',
                                        'opportunistic', 'site counts')),
  "description" VARCHAR(200),
  "parentDataset" VARCHAR(4) REFERENCES "ParentDataset"("parentDatasetID")
);

CREATE TABLE "Contact"(
  "contactID" INT PRIMARY KEY NOT NULL,
  "contactName" VARCHAR(100),
  "organisation" VARCHAR(100),
  "contactEmail" VARCHAR(100),
  UNIQUE ("contactName", "contactEmail")
);

CREATE TABLE "DatasetContact"(
  "dataset" VARCHAR(4) REFERENCES "Dataset"("datasetID"),
  "contact" INT REFERENCES "Contact"("contactID"),
  PRIMARY KEY ("dataset", "contact")
);

CREATE TABLE "ParentDatasetContact"(
  "parentDataset" VARCHAR(4) REFERENCES "ParentDataset"("parentDatasetID"),
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
  CONSTRAINT complete_loc CHECK ("decimalCoordinates" IS NOT NULL OR
                                 "county" IS NOT NULL OR "county" IS NOT NULL)
  CONSTRAINT orphan_county CHECK ("county" IS NOT NULL OR "county" IS  NULL)
);

CREATE TABLE "Event"(
  "eventID" SERIAL PRIMARY KEY NOT NULL,
  "location" INT REFERENCES "Location"("locationID"),
  "date" INT REFERENCES "Date"("dateID"),
  "recorder" INT REFERENCES "Recorder"("recorderID"),
  "dataset" VARCHAR(4) REFERENCES "Dataset"("datasetID"),
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
