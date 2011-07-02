#!/bin/bash
# author: Emiliano Carlos de Moraes Firmino 
# criado em 06/28/11 23:13:53 (HPC)
# descricao: http://pdincau.wordpress.com/2011/06/22/a-brief-introduction-to-ssl-with-erlang/
Ext=pem
BitLen=1024
ExpireDays=7

KeyFile=key.${Ext}
ReqFile=request.${Ext}
CertFile=certificate.${Ext}

openssl genrsa \
	-out ${KeyFile} ${BitLen}
openssl req -new \
	-key ${KeyFile} \
	-out ${ReqFile}
openssl x509 -req \
	-days ${ExpireDays} \
	-in ${ReqFile} \
	-signkey ${KeyFile} \
	-out ${CertFile}
