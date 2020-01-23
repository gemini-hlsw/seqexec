[![Build status](https://badge.buildkite.com/2d1ba8c871db19fd0a34289ae0b0f706ebbecc72e9ecf57dec.svg)](https://buildkite.com/gemini-hlsw-1/ocs3-ci-pipeline)

# Remains of Gem Prototype

This is what remains of the initial work on a Postgres-based back end.

The core model and database layer have been moved to their own repositories. See the repositories for more information on their associated artifacts.

- [`gsp-math`](https://github.com/gemini-hlsw/gsp-math) provides fundamental mathematical data types and abstractions.
- [`gsp-core`](https://github.com/gemini-hlsw/gsp-core) provides data types specific to Gemini, as well as a database schema and persistence layer.

Several remaining projects are likely candidates for removal to independent library repositories.

- `acm` provides low-level EPICS support.
- `giapi` providess low-level GIAPI support.
- `ocs2-api` provides translations between OCS and gsp-core data types, and `ocs2` provides a service that can import OCS science programs into the GSP database.

The `ephemeris` project provides a service that maintains ephemerides for observing targets. This will likely become part of the target service which will have its own database.

The `seqexec` projects are likely all that will remain here, at which point the project can be renamed.

___

> ## The remainder of this document will move to `gsp-ocs2`.

## Importing

There are several options for importing existing OCS2 program and Smart Gcal data.

### Importing Old Programs

You can import old programs, but unfortunately not in the standard Phase 2 XML format exported from the ODB or OT. The importer works with a modified XML format which you can obtain via the `exportOcs3` OSGi shell command running in an ODB.  It works just like `exportXML` but writes the program with expanded sequence steps.  This is a big part of what enables the importer to work without ocs2 dependencies.

To import these modified `.xml` files into Postgres you need to create a symlink to a directory containing the program files first:

```
ln -s /path/to/some/old/xml/files archive
```

You can then import from the `sbt` prompt:

```
sbt> ocs2/runMain gem.ocs2.FileImporter 123     Import the first 123 programs.
```

You can skip the program limit argument if you want to import all the program files.

Right now just a sketch is imported:

- referenced semesters;
- programs (structured id and title)
- observations (title, instrument)
- sequence steps, generically, with slices for
  - bias
  - dark
  - gcal
  - science (offset p/q)
    - F2

There are no other instrument-specific slices for science steps yet.


### Importing Smart Gcal Configuration

Smart Gcal configuration is stored in database tables instead of in `.csv` files downloaded from SVN as in OCS2.  The `.csv` files can be imported, though much like old program `.xml` files, we use a modified format.  To obtain compatible Smart Gcal `.csv` files from OCS2, start the ODB and use the `exportSmartGcal` shell command providing the name of a directory into which to write the files.

Next, make a symlink to the directory containing the `.csv` files that were exported:

```
ln -s /path/to/old/smart/gcal/csv smartgcal
```

Having created the symlink, you can then import from the `sbt` prompt:

```
sbt> ocs2/runMain gem.ocs2.SmartGcalImporter
```

### Import Server

You can run an import server which will import programs or observations on demand directly from a running OCS2 Observing Database.  You start it with

```
sbt> ocs2/runMain gem.ocs2.ImportServer [odb-hostname]
```

where the `odb-hostname` defaults to `localhost` if not specified.  Once running, it accepts http requests to import data.  For example, to import program `GS-2017A-Q-1` from the ODB:

```
http://localhost:8989/import/prog/GS-2017A-Q-1
```

or to just get observation `GS-2017A-Q-1-2`:

```
http://localhost:8989/import/obs/GS-2017A-Q-1-2
```

When you re-import a program or observation, any existing data associated with it is first purged.


### Import Menu

Another option for importing is to just type the following at the sbt prompt

```
sbt> ocs2/run

Multiple main classes detected, select one to run:

 [1] gem.ocs2.FileImporter
 [2] gem.ocs2.ImportServer
 [3] gem.ocs2.SmartGcalImporter
```

If you pick the program importer, it will import everything which is the same as explicitly passing in `Int.MaxValue`.

