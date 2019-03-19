---------------------------
-- Giapi status apply
---------------------------

-- Giapi types

CREATE TYPE giapi_type AS ENUM (
  'String',
  'Int',
  'Float',
  'Double'
);

ALTER TYPE giapi_type OWNER TO postgres;

--
-- Name: status_item; Type: DOMAIN; Schema: public; Owner: postgres
--

CREATE DOMAIN status_item AS character varying(255)
	CONSTRAINT status_item_check CHECK (((VALUE)::text ~ '^[A-Za-z]*:[A-Za-z0-9_:\.]*$'::text));

ALTER DOMAIN status_item OWNER TO postgres;

-- Giapi status items and apply items
-- Associates a sattus item with its apply status item for an instrument

CREATE TABLE e_giapi_status_apply (
  id               identifier  NOT NULL,
  instrument_id    identifier  NOT NULL REFERENCES e_instrument(id),
  status_item      status_item NOT NULL,
  type             giapi_type  NOT NULL,
  apply_item       status_item NOT NULL,
  PRIMARY KEY(id, instrument_id)
);


ALTER TABLE e_giapi_status_apply OWNER TO postgres;

INSERT INTO e_giapi_status_apply(id, instrument_id, type, status_item, apply_item)
VALUES
	('Adc', 'Gpi', 'Int', 'gpi:adcDeploy', 'gpi:selectAdc.deploy'),
	('UseAo', 'Gpi', 'Int', 'gpi:ao:useAo', 'gpi:configAo.useAo'),
	('AoOptimize', 'Gpi', 'Int', 'gpi:ao:optimization', 'gpi:configAo.optimize'),
	('UseCal', 'Gpi', 'Int', 'gpi:cal:useCal', 'gpi:configCal.useCal'),
	('FpmPinholeBias', 'Gpi', 'Int', 'gpi:cal:fpmPinholeBias', 'gpi:configCal.fpmPinholeBias'),
	('IntegrationTime', 'Gpi', 'Float', 'gpi:currentIntegrationTime', 'gpi:configIfs.integrationTime'),
	('NumCoadds', 'Gpi', 'Int', 'gpi:currentNumCoadds', 'gpi:configIfs.numCoadds'),
	('MagI', 'Gpi', 'Float', 'gpi:starIntensity', 'gpi:configAo.magnitudeI'),
	('MagH', 'Gpi', 'Float', 'gpi:cal:magH', 'gpi:configCal.magnitudeH'),
	('CalEntranceShutter', 'Gpi', 'Int', 'gpi:calEntranceShutter', 'gpi:selectShutter.calEntranceShutter'),
	('CalReferenceShutter', 'Gpi', 'Int', 'gpi:referenceShutter', 'gpi:selectShutter.calReferenceShutter'),
	('CalScienceShutter', 'Gpi', 'Int', 'gpi:scienceShutter', 'gpi:selectShutter.calScienceShutter'),
	('EntranceShutter', 'Gpi', 'Int', 'gpi:omssEntranceShutter', 'gpi:selectShutter.entranceShutter'),
	('CalExitShutter', 'Gpi', 'Int', 'gpi:calExitShutter', 'gpi:selectShutter.calExitShutter'),
	('PupilCamera', 'Gpi', 'Int', 'gpi:pupilViewingMirror', 'gpi:selectPupilCamera.deploy'),
	('SCPower', 'Gpi', 'Float', 'gpi:artificialSourceSCpower', 'gpi:selectSource.sourceSCpower'),
	('SCAttenuation', 'Gpi', 'Float', 'gpi:artificialSourceSCDb', 'gpi:selectSource.sourceSCatten'),
	('SrcVis', 'Gpi', 'Int', 'gpi:artificialSourceVIS', 'gpi:selectSource.sourceVis'),
	('SrcIR', 'Gpi', 'Int', 'gpi:artificialSourceIR', 'gpi:selectSource.sourceIr'),
	('PolarizerDeplay', 'Gpi', 'Int', 'gpi:polarModulatorDeploy', 'gpi:configPolarizer.deploy'),
	('PolarizerAngle', 'Gpi', 'Float', 'gpi:polarizerAngle', 'gpi:configPolarizer.angle'),
	('ObservationMode', 'Gpi', 'String', 'gpi:observationMode', 'gpi:observationMode.mode'),
	('IFSFilter', 'Gpi', 'String', 'gpi:ifsFilter', 'gpi:ifs:selectIfsFilter.maskStr'),
	('PPM', 'Gpi', 'String', 'gpi:ppmMask', 'gpi:selectPupilPlaneMask.maskStr'),
	('FPM', 'Gpi', 'String', 'gpi:fpmMask', 'gpi:selectFocalPlaneMask.maskStr'),
	('Lyot', 'Gpi', 'String', 'gpi:lyotMask', 'gpi:selectLyotMask.maskStr');
