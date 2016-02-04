#!/bin/bash

if [ $# -ne 1 ]; then
   echo "Please specify the hostname"
   exit 1
fi

openssl req -new -newkey rsa:4096 -nodes -out "${1}.csr" -keyout "${1}.key" -subj "/C=CA/ST=ON/L=Toronto/O=control-alt-del.org/OU=Security/CN=${1}"
