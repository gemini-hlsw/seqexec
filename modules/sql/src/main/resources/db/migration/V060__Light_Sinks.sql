---------------------------
-- Science Fold Light Sinks
-- The science fold directs the light to one of this named positions
---------------------------

CREATE TABLE e_light_sink_names (
    id identifier  PRIMARY KEY NOT NULL ,
    name VARCHAR(15)    NOT NULL CHECK (name ~ '[a-z][a-z0-9]*')
);

ALTER TABLE e_light_sink_names OWNER TO postgres;

COPY e_light_sink_names (id, name) FROM stdin;
Gmos	gmos
Niri_f6	nirif6p
Niri_f14	nirif14p
Niri_f32	nirif32p
Ac	ac
Hr	hr
Nifs	nifs
Gmos_Ifu	gmosifu
Gnirs	gnirs
Visitor	visitor
F2	f2
Gsaoi	gsaoi
Phoenix	phoenix
Gpi	gpi
Ghost	ghost
\.