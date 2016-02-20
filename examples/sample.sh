#!/bin/bash

echo "Testing invalid cert: "
curl -X POST --cert ../fixtures/invalid.control-alt-del.org.crt --key ../fixtures/invalid.control-alt-del.org.key \
--cacert ../fixtures/cacert.pem --tlsv1.2  \
https://dev1.control-alt-del.org:55443/key/data-key
echo

echo "Input is foo and bar"
PAYLOAD="{\"foo\":\"bar\",\"bar\":\"baz\"}"
echo "PAYLOAD: $PAYLOAD"
DK=`curl -s \
-X POST --cert ../fixtures/star.control-alt-del.org.crt --key ../fixtures/star.control-alt-del.org.key \
--cacert ../fixtures/cacert.pem --tlsv1.2  \
https://dev1.control-alt-del.org:55443/key/data-key | jq -r ".dataKey"`
echo "DK: $DK"

RAW=`curl -s \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json' \
  -H "x-cinched-data-key: $DK" \
  -H "x-cinched-metadata: foobar" \
  -X POST -d "$PAYLOAD" \
  --cert ../fixtures/star.control-alt-del.org.crt --key ../fixtures/star.control-alt-del.org.key \
  --cacert ../fixtures/cacert.pem \
  --tlsv1.2 \
  https://dev1.control-alt-del.org:55443/doc/encrypt?fields=\(bar\)`
echo "RAW: $RAW"

FOO=`echo $RAW | jq -r ".foo"`
BAR=`echo $RAW | jq -r ".bar"`

echo "FOO: $FOO (should be 'bar')"
echo "BAR: $BAR (should be encrypted)"

DEC=`curl -s \
  -H 'Accept: application/json' \
  -H "x-cinched-data-key: $DK" \
  -H "x-cinched-metadata: fobarr" \
  -H 'Content-Type: application/json' \
  -X POST -d "
 {\"foo\": \"bar\",
 \"bar\": \"$BAR\"}
" --cert ../fixtures/star.control-alt-del.org.crt --key ../fixtures/star.control-alt-del.org.key \
  --cacert ../fixtures/cacert.pem \
  --tlsv1.2 \
https://dev1.control-alt-del.org:55443/doc/decrypt?fields=\(bar\)`

echo "Decoding decryption: "
echo "FOO (should be bar)"
echo $DEC | jq -r ".foo"
echo "BAR (should be baz)"
echo $DEC | jq -r ".bar"
echo

echo "Encrypt then decrypt blob, should print foobar:"
echo `echo foobar | \
curl -s \
  -H 'Content-Type: application/octet-stream' \
  -H 'Accept: application/octet-stream' \
  -H "x-cinched-data-key: $DK" \
  -H "x-cinched-metadata: foobar" \
  -X POST --data-binary @- \
  --cert ../fixtures/star.control-alt-del.org.crt --key ../fixtures/star.control-alt-del.org.key \
  --cacert ../fixtures/cacert.pem \
  --tlsv1.2 \
  https://dev1.control-alt-del.org:55443/blob/encrypt | \
curl -s \
  -H 'Accept: application/octet-stream' \
  -H "x-cinched-data-key: $DK" \
  -H "x-cinched-metadata: fobarr" \
  -H 'Content-Type: application/octet-stream' \
  -X POST --data-binary @- --cert ../fixtures/star.control-alt-del.org.crt --key ../fixtures/star.control-alt-del.org.key \
  --cacert ../fixtures/cacert.pem \
  --tlsv1.2 \
https://dev1.control-alt-del.org:55443/blob/decrypt`

echo
echo
echo
echo

## Some more examples:
DOC='
{
  "glossary": {
    "title": "example glossary",
    "GlossDiv": {
      "title": "S",
      "GlossList": {
        "GlossEntry": {
          "ID": "SGML",
          "SortAs": "SGML",
          "GlossTerm": "Standard Generalized Markup Language",
          "Acronym": "SGML",
          "Abbrev": "ISO 8879:1986",
          "GlossDef": {
            "para": "A meta-markup language, used to create markup languages such as DocBook.",
            "GlossSeeAlso": [
              "GML",
              "XML"
            ]
          },
          "GlossSee": "markup"
        }
      }
    }
  }
}
'

echo "Submitting doc for encryption: "
echo $DOC
echo

## Note field specs are not zero-based index, whereas the 'jq' tool is.
ENCDOC=`cat <<EOF | curl \
    -H 'Content-Type: application/json' -H 'Accept: application/json' \
    -H "x-cinched-data-key: $DK" -H "x-cinched-metadata: foobar" -X POST \
    --data-binary @-   --cert ../fixtures/star.control-alt-del.org.crt \
    --key ../fixtures/star.control-alt-del.org.key --cacert ../fixtures/cacert.pem \
    --tlsv1.2 \
    https://dev1.control-alt-del.org:55443/doc/encrypt?fields=\(glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso.2\)
$DOC
EOF
`
echo
echo
ENCVAL=`echo $ENCDOC | jq -r .glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso[1]`
echo "Encrypted doc: "
echo $ENCDOC
echo
echo "Encrypted value: $ENCVAL"
echo "Reconstructed doc: "
echo
cat <<EOF | curl -H 'Content-Type: application/json' -H 'Accept: application/json' \
-H "x-cinched-data-key: $DK" \
-H "x-cinched-metadata: foobar" -X POST \
--data-binary @-  --cert ../fixtures/star.control-alt-del.org.crt \
--key ../fixtures/star.control-alt-del.org.key --cacert ../fixtures/cacert.pem --tlsv1.2  \
https://dev1.control-alt-del.org:55443/doc/decrypt?fields=\(glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso.2\) | jq
$ENCDOC
EOF
