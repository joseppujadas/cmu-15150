#!/bin/bash

read -p "andrew: " ANDREW
read -p "ta: " TA
read -p "password: " PASSWORD
echo ""
python3 -c 'from sys import argv; print("{"); print("\"andrew\": \"{0}\",\"ta\": \"{1}\",\"password\": \"{2}\"".format(argv[2], argv[3], argv[1])); print("}")' $PASSWORD $ANDREW $TA > checkin.json
