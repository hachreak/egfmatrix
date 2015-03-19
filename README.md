Erlang Galois Field Matrix library
===============

This library manipulate matrix and use finite field mathematics.

You have at disposition many method to:
 - find the transpose
 -  sum/subtract matrix
 - multiply matrix
 - solve square matrices
 - find determinant
 - other operations.

There is a dedicate class to fill a matrix with random values.

You can use this kind of Galois Field: GF(2^3), GF(2^8), GF(2^16).

How to build docker image?
--------------------------

`docker build -t myerlang .`

Or, you can use my image with:

`docker pull hachreak/erlang`

See details here: https://registry.hub.docker.com/u/hachreak/erlang/


How to run the docker image?
----------------------------

```bash
docker run -i -t -v `pwd`:/var/www:rw --rm myerlang /bin/bash
```
