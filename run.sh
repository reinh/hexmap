#!/bin/bash
stack build && stack exec hexmap-demo -- -w 800 -h 800 -o out.svg
echo "Done!"
