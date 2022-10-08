#!/bin/bash

read -p "andrew: " ANDREW
read -p "ta: " TA
stty -echo
read -p "password: [REDACTED]" PASSWORD
echo ""
stty echo
python3 -c 'from sys import argv; import hashlib; print("{"); print("\"andrew\": \"{0}\",\"ta\": \"{1}\",\"password\": \"{2}\"".format(argv[2], argv[3], hashlib.sha256("-".join(argv[1:]).encode()).hexdigest()[:8])); print("}")' $PASSWORD $ANDREW $TA > checkin.json
