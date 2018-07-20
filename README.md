[![Build status](https://badge.buildkite.com/2d1ba8c871db19fd0a34289ae0b0f706ebbecc72e9ecf57dec.svg)](https://buildkite.com/gemini-hlsw-1/ocs3-ci-pipeline)

# Gem Prototype

This is the initial work on a Postgres-based back end, with an API based around recent work on the flat sequence model. It doesn't do very much yet.

### Setting Up

You need Postgres 9.5 or better. I **highly** recommend using [Postgres.app](http://postgresapp.com/) which is much much easier than dealing with a "real" installation. You need to add its binaries to your path, something along the lines of

```
export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/latest/bin
```

Next you can run the following to create the database and user.

```
psql -c 'create user postgres createdb'
psql -c 'create database gem' -U postgres
```

Now initialize the database by running the migration script.

```
sbt sql/flywayMigrate
```

You can also just do `sql/flywayMigrate` from the sbt prompt if you already have sbt running. You will repeat this step each time the schema changes. It's not part of the build yet but probably will be soon.

If you ever want to wipe out the database and start over, you can do

```
psql -c 'drop database gem' -U postgres
```

And then re-run steps 2 and 3 above. At any time you can say

```
psql -U postgres -d gem
```

to poke around with the database on the commandline. For real work I recommend a more full-featured front end. I use [Toad](https://www.toadworld.com/products/toad-mac-edition) but there are a lot of options.

### Generating Enumerated Types

There are many enumerated types in the database, represented by tables named `e_whatever`. The Scala equivalents are generated *on demand* by queries, then checked into source control like normal source files.
This is only needed if you update the contents of an enum in the schema, or add/modify a the generation
code in the `sql` project. In any case, you can [re]-generate the enumerated types thus:

```
sbt genEnums
```

The source files appear in `modules/core/shared/src/main/scala/gem/enum`.

### Generating Schema Documentation

You can do `sbt schemaSpy` to generate a little website about the database using [SchemaSpy](http://schemaspy.org/). It will appear in `modules/sql/target/schemaspy`.

### Importing

There are several options for importing existing OCS2 program and Smart Gcal data.

#### Importing Old Programs

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


#### Importing Smart Gcal Configuration

Smart Gcal configuration is stored in database tables instead of in `.csv` files downloaded from SVN as in OCS2.  The `.csv` files can be imported, though much like old program `.xml` files, we use a modified format.  To obtain compatible Smart Gcal `.csv` files from OCS2, start the ODB and use the `exportSmartGcal` shell command providing the name of a directory into which to write the files.

Next, make a symlink to the directory containing the `.csv` files that were exported:

```
ln -s /path/to/old/smart/gcal/csv smartgcal
```

Having created the symlink, you can then import from the `sbt` prompt:

```
sbt> ocs2/runMain gem.ocs2.SmartGcalImporter
```

#### Import Server

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


#### Import Menu

Another option for importing is to just type the following at the sbt prompt

```
sbt> ocs2/run

Multiple main classes detected, select one to run:

 [1] gem.ocs2.FileImporter
 [2] gem.ocs2.ImportServer
 [3] gem.ocs2.SmartGcalImporter
```

If you pick the program importer, it will import everything which is the same as explicitly passing in `Int.MaxValue`.


### Schema Updates

If you need to update the schema you can just make changes locally and then truncate user data and do a dump.

```
psql -c "truncate log; truncate program cascade; delete from gem_user where id != 'root'" -d gem -U postgres
pg_dump -U postgres -d gem > create.sql
```

### Trouble shooting

* If you see this message when setting up the db for the first time:
```
ERROR: must be owner of extension plpgsql
```

Assign super user privileges to postgres
```
alter role postgres superuser;
```
