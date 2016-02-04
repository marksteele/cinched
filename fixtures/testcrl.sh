#!/bin/bash

rm crl.pem
wget -qO crl.der 'http://dev1.control-alt-del.org:8080/ca/ee/ca/getCRL?op=getCRL&crlIssuingPoint=MasterCRL'
openssl crl -inform DER -in crl.der -outform PEM -out crl.pem
cat cacert.pem >>crl.pem

echo "TESTING INVALID CERT: "
openssl verify -crl_check -CAfile crl.pem invalid.control-alt-del.org.crt
echo
echo "TESTING VALID CERT: "
openssl verify -crl_check -CAfile crl.pem star.control-alt-del.org.crt
echo
