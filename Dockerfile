# Run container for client
FROM hachreak/erlang

MAINTAINER Leonardo Rossi <leonardo.rossi@studenti.unipr.it>

# Define mountable directory for client.
VOLUME ["/var/www"]

USER erlang

# Execute python server
WORKDIR /var/www/
CMD ["erl"]
