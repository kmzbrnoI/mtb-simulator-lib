MTB Simulator Library
=====================

MTB Simulator Library is a tool simulating real MTB bus. It meets
[MTB library interface](https://github.com/kmzbrnoI/mtb-lib/wiki/api-specs)
specification.

## Description

This library is an alternative library to
[MTB library](https://github.com/kmzbrnoI/mtb-lib), which provides connection
to the [MTB bus](https://mtb.kmz-brno.cz/). This library does NOT connect to the
MTB, however, MTB is simulated. Its main purpose is to simplify debugging -- MTB
bus (and whole layout) is not necessarry, because it is simulated.

In the main form, each MTB-UNI or MTB-TTL board is represented as a single column
of shapes. This column contains 16 shapes = 16 inputs and outputs. You can view
the state of all the outputs and change state of the inputs by clicking into the
shape.

 * Black border → input=0
 * Red border   → input=1
 * Black inside → output=0
 * Green inside → output=1

## Authors

 - Jan Horacek (jan.horacek@kmz-brno.cz)
 - Michal Petrilak (engineercz@gmail.com)

This software is distributed as open source under Apache License v2.0.
