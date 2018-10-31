#!/bin/bash

./barki-ng.ml && cat out.l8 | pacat --format=u8 --rate=48000 --channels=1
