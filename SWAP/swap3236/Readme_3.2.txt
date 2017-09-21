=== README.TXT =============
============================
This file is part of:

program    :  SWAP
version    :  3.2.36
releasedate:  07-Nov-2011
platform   :  MS-windows7


This file READ.ME contains the following information:
1. Short product description
2. Installation procedure
3. How to get started
4. Disclaimer
5. References
============================
1. Short product description

SWAP (Soil-Water-Atmosphere-Plant) simulates transport of water, solutes 
and heat in variably saturated soils. The program is designed to simulate the
transport processes at field scale level and during whole growing seasons. 
The program is developed within Wageningen-UR by the Wageningen Agricultural
University and the Alterra-Institute.

============================
2. Installation procedure

This version of SWAP is offered to you in a zip-file (Swap3236.zip)
Unzip this file to a folder where you have write-access. 
Leave the folder-structure intact to enable an easy start.


============================
3. How to get started

A quick start of a simulation of the example 'Hupsel' can be carried out as follows:

  - Change to the sub-directory   \swap3236\example\hupsel\;
  - On this directory you can simulate the reference situation by starting the batch-file Hupsel.bat;
  - Verify results by using an ASCII-editor. 
    The water balance of the first year 1980, as given in the file Result.bal, should read:

           Water balance components (cm)

           In                           Out
           =========================    ============================
           Rain + snow    :    66.01    Interception      :     4.37
           Runon          :     0.00    Runoff            :     0.00
           Irrigation     :     0.05    Transpiration     :    29.81
           Bottom flux    :     0.00    Soil evaporation  :    13.81
                                        Crack flux        :     0.00
                                        Drainage level 1  :    19.88
           =========================    ============================
           Sum            :    66.06    Sum               :    67.87


A manual is supplied in the folder \documents\
The program has been tested on MS Windows7. 
A test-report is supplied in the folder \documents\


============================
4. Disclaimer

The author(s) and the Alterra Institute disclaim all liability for 
direct, incidental or consequential damage resulting from use of the program.


============================
5. References:

An explanation of theory and ins and outs of this version of the model is given in the 
manual (Kroes et al, 2009). 
A general reference to the Swap model is Van Dam et al (2000) who also supplies an overview of 
applications. References may also be found on the internet www.swap.alterra.nl.
See the addendum to the manual for recent changes.


Kroes, J.G., J.C. Van Dam, P. Groenendijk, R.F.A. Hendriks, C.M.J. Jacobs, 2009. 
    SWAP version 3.2. Theory description and user manual. Alterra-report 1649(02). 
    Wageningen-UR, Alterra, Wageningen.
Van Dam, J.C, 2000. Field scale water flow and solute transport. SWAP model concepts, 
    parameter estimation and case studies. PhD thesis, Wageningen Universiteit, 167 p.



For information please contact:
J.G. (Joop) Kroes or other persons from the Development group (www.swap.alterra.nl)
Wageningen UR, Centre for Water and climate
Adresses:
Post:     PO.Box 47, 6700 AA Wageningen / Netherlands
Email:    swap.alterra@wur.nl  or  joop.kroes@wur.nl 
Phone:    + 31 317 486433
Internet: www.swap.alterra.nl
----------------------------

Copyright ALTERRA 2011
============================

=== end of README.TXT  =====
