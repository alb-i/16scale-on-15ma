#!/bin/bash

cd $(dirname $0)

./scales.py | ./format_printable.py > scales.txt

./pitchtable.py | ./format_printable.py pitchtable > pitchtable.txt