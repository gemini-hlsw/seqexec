

# Design Principles - Addressed

This is a rough accounting of the design problems addressed so far.

### Axis Mundi

The Postgres database is the center of the world and the application design flows outward.

- We want the database to maintain as much consistency as possible, such that any program interacting with the database (not just the new ODB) will see an accurate view of the world and will only be able to make modifications that are "legal" under some definition of well-formedness.
- We want the database to be queryable easily, such that a read-replica can be made available to science staff for their own use. To this end some *checked* denormalization (such as program ids being broken into site, year, etc.) is ok.
- Similarly astronomical units need to be in a science-friendly format, and we may need server-side functions for doing computations. We need to be cautious about pushing too much *business* logic into the database but support for domain-specific data types is probably reasonable.
- Enumerated types like filters, dispersers, etc., are important for reporting and consistency checking and need to be defined in the database. We use code-generation to read these in at compile time and make them available as proper enumerated types in our Scala code.
- Events should be logged to the database and echoed to traditional file logs in case database connectivity fails.

The schema now represents the following notions:

- Programs
  - Title
  - Structured ID
- Users
  - Name, Email, Password, Staff flag
  - Relationship to a given program (PI, Science Contact, etc.)
- Observations
  - Structured ID, Name
- Steps
  - Bias, Dark, Gcal, Science
  - Instrument-specific (F2 only so far)
- Enumerated Types
  - Site
  - Program Type
  - Program Role (for users)
  - Instrument
  - Gcal config (Filter, Lamp)
  - F2 config (Disper, Filter, FPU, Lyot Wheel)

The data model more or less reflects the above, and some read/write database access is provided but it's not totally built out. Basically just what's needed for import and simple queries.

### Data Model and Level of Detail

We need a straightforward way to deal with *portions* of the data model (for instance a list of programs without any observation information, for an "Open" dialog) without a proliferation of specialized model classes.

- Model classes are parameterized on their children; i.e., any type that is represented by a foreign key in the schema. A `Program[Observation[...]]` is a program with a list of observations, a bit like the current model; a`Program[Observation.Id]` is a program with a list of observation *ids*; a `Program[Nothing]` has no observation information at all.
- This keeps the model representation simple and flexible for dealing with the varying levels of detail needed for the UI, and means we don't have to load and ship any more information than necessary.

### Data Import

A separate project module (dependent on OCS) reads old XML files and then inserts programs into the new database. This works for the portion of the model described above, with the exception of users and roles which haven't been added yet.

### CRDT-based Clients (i.e., no checkin/checkout or sync)

The OCS program sync problem is hugely complicated and we are determined to avoid it. The new system will have no notion of "offline mode" and edits will be propagated immediately. We intend to use conflict-free replicated data types (CRDTs) for client applications to allow simultaneous editing without need for explicit sync (think Google Docs). Shane has implemented such a type that is in principle capable of handling sequence editing.



# Design Principles - Pending

### Continuous Deployment

I want to break the 6-month development cycle and try continuous deployment: commits to `master` are pushed to a test environment automatically, and can be promoted to production automatically or on demand. This means we need to figure out schema migration, client notification, and so on.

It also means that we need to integrate the notion of time into some portions of our model, so we can switch filters on the fly for example without affecting historical data, and can add a new instrument before it's available for use.

### Client Design

We anticipate using a web-based front end but have not spent much time thinking about it other than the CRDT requirement above.
