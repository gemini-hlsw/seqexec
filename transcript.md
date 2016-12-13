
## One-time setup.

    docker network create gem-net

## First Deployment

// start postgres
docker run -d --net=gem-net --name db-1 postgres:9.6.0

// create gem database
docker run --rm --net=gem-net postgres:9.6.0 psql -h db-1 -c 'create database gem' -U postgres


docker run -d -t -i --net=gem-net --name gem-1 -p 1234:6666 \
  -e GEM_DB_URL=jdbc:postgresql://db-1/gem \
  telnetd:0.1-SNAPSHOT


```

// watch logs
docker logs -f gem-1



interactive:

docker run --rm -t -i --net=gem-net -p 1234:6666 \
  -e GEM_DB_URL=jdbc:postgresql://db-1/gem \
  telnetd:0.1-SNAPSHOT
