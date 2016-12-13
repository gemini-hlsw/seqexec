
#### Moving a Postgres Database

The ability of pg_dump and psql to write to or read from pipes makes it possible to dump a database directly from one server to another; for example:

    pg_dump -h host1 dbname | psql -h host2 dbname

#### Transferring a Docker image via SSH, bzipping the content on the fly:

    docker save <image> | bzip2 | ssh user@host 'bunzip2 | docker load'

It's also a good idea to put pv in the middle of the pipe to see how the transfer is going:

    docker save <image> | bzip2 | pv | ssh user@host 'bunzip2 | docker load'

To run PSQL shell

    docker run -i -t --net=gem-net postgres:9.6.0 psql -h db-1 -U postgres
