-- Remove tables
DROP TABLE IF EXISTS "Taxon" CASCADE;
DROP TABLE IF EXISTS "Recorder" CASCADE;
DROP TABLE IF EXISTS "EventDate" CASCADE;
DROP TABLE IF EXISTS "Dataset" CASCADE;
DROP TABLE IF EXISTS "Contact" CASCADE;
DROP TABLE IF EXISTS "DatasetContact" CASCADE;
DROP TABLE IF EXISTS "Location" CASCADE;
DROP TABLE IF EXISTS "Event" CASCADE;
DROP TABLE IF EXISTS "Occurrence" CASCADE;

-- Create tables
CREATE TABLE "Taxon"(
  "taxonID" INT PRIMARY KEY NOT NULL,
  "scientificName" VARCHAR(60) UNIQUE,
  "species" VARCHAR(60),
  "speciesID" INT,
  "genus" VARCHAR(30),
  "family" VARCHAR(40) NOT NULL,
  "taxonRank" VARCHAR(20) NOT NULL,
  CONSTRAINT spp_taxo CHECK ("species" IS NULL OR "genus" IS NOT NULL),
  CONSTRAINT genus_taxo CHECK ("genus" IS NULL OR "family" IS NOT NULL),
  CONSTRAINT subspp_taxo CHECK (("taxonRank" != 'SUBSPECIES' OR "taxonRank" != 'SPECIES') OR "species" IS NOT NULL)
);

CREATE TABLE "Recorder"(
  "recorderID" SERIAL PRIMARY KEY,
  "recorderID_orig" VARCHAR(100),
  "name" VARCHAR(300)
  -- UNIQUE NULLS NOT DISTINCT ("recorderID_orig", "name")
);

CREATE TABLE "EventDate"(
  "eventDateID" SERIAL PRIMARY KEY,
  "eventDate" date,
  "eventTime" TIME without time zone,
  "eventDateUncertainty" VARCHAR(50),
  UNIQUE NULLS NOT DISTINCT ("eventDate", "eventTime", "eventDateUncertainty")
  );

CREATE TABLE "Dataset"(
  "datasetID" VARCHAR(8) PRIMARY KEY NOT NULL,
  "datasetName" VARCHAR(200) UNIQUE,
  "description" VARCHAR(200),
  "parentDatasetID" VARCHAR(8) REFERENCES "Dataset"("datasetID"),
  "isParentDataset" BOOLEAN
);

CREATE TABLE "Contact"(
  "contactID" INT PRIMARY KEY NOT NULL,
  "contactName" VARCHAR(100),
  "organisation" VARCHAR(100),
  "contactEmail" VARCHAR(100),
  UNIQUE ("contactName", "contactEmail")
);

CREATE TABLE "DatasetContact"(
  "datasetID" VARCHAR(8) REFERENCES "Dataset"("datasetID"),
  "contactID" INT REFERENCES "Contact"("contactID"),
  PRIMARY KEY ("datasetID", "contactID")
);

CREATE TABLE "Location"(
  "locationID" SERIAL PRIMARY KEY,
  "decimalCoordinates" geometry,
  "coordinateUncertaintyInMeters" NUMERIC CHECK ("coordinateUncertaintyInMeters" >= 0),
  "locality" VARCHAR(300),
  "municipality" VARCHAR(100),
  "county" VARCHAR(100),
  "country" VARCHAR(50),
  /*
  UNIQUE NULLS NOT DISTINCT ("decimalCoordinates",
                             "coordinateUncertaintyInMeters",
                             "locality", "municipality", "county", "country"),
                             */
  CONSTRAINT complete_loc CHECK ("decimalCoordinates" IS NOT NULL OR
                                 "country" IS NOT NULL OR "county" IS NOT NULL),
  CONSTRAINT orphan_county CHECK ("county" IS NULL OR "country" IS NOT NULL)
);

CREATE TABLE "Event"(
  "eventID" SERIAL PRIMARY KEY,
  "locationID" INT REFERENCES "Location"("locationID"),
  "eventDateID" INT REFERENCES "EventDate"("eventDateID"),
  "recorderID" INT REFERENCES "Recorder"("recorderID"),
  "datasetID" VARCHAR(8) REFERENCES "Dataset"("datasetID"),
  "eventType" VARCHAR(20) CHECK ("eventType" IN ('Transect',
                                 'HumanObservation', 'SiteCounts', 'PreservedSpecimen',
                                 'Historical')),
  "parentEventID" INT REFERENCES "Event"("eventID"),
  "samplingEffort" NUMERIC,
  "wind" VARCHAR(50),
  "cloudCover" VARCHAR(50),
  "elevation" NUMERIC,
  "eventRemarks" VARCHAR(200),
  "isParentEvent" BOOLEAN
  /*
  UNIQUE NULLS NOT DISTINCT ("locationID", "eventDateID", "recorderID",
                             "datasetID", "eventType", "parentEventID")
  */
);

CREATE TABLE "Occurrence"(
  "occurrenceID" SERIAL PRIMARY KEY,
  "eventID" INT REFERENCES "Event"("eventID"),
  "taxonID" INT REFERENCES "Taxon"("taxonID"),
  "individualCount" INT,
  "lifeStage" VARCHAR(50),
  "sex" VARCHAR(30),
  "behavior" VARCHAR(100),
  "degreeOfEstablishment" INT,
  "identificationVerificationStatus" VARCHAR(100),
  "associatedMedia" VARCHAR(200),
  "embargoDate" DATE,
  "accessRights" VARCHAR(50),
  "occurrenceStatus" VARCHAR(20) CHECK ("occurrenceStatus" IN ('present', 'absent')),
  "occurrenceRemarks" VARCHAR(5000)
);
