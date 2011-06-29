#!/bin/bash
# author: Emiliano Carlos de Moraes Firmino 
# criado em 06/28/11 23:13:53 (HPC)
# descricao: http://pdincau.wordpress.com/2011/06/22/a-brief-introduction-to-ssl-with-erlang/
openssl genrsa -out key.pem 1024
openssl req -new -key key.pem -out request.pem
openssl x509 -req -days 7 -in request.pem -signkey key.pem -out certificate.pem