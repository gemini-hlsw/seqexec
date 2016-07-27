


```
psql -c 'create user postgres createdb'
psql -c 'create database gem;' -U postgres
psql -c '\i create.sql' -d gem -U postgres
```