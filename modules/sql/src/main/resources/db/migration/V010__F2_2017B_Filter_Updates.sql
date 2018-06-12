
COPY e_f2_filter (id, wavelength, short_name, long_name, obsolete) FROM stdin;
KBlue	2.06	K-blue	K-blue (2.06 um)	f
KRed	2.31	K-red	K-red (2.31 um)	f
\.

UPDATE e_f2_filter SET obsolete = 't' WHERE id = 'Open' OR id = 'Dark';
