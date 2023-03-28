IDENTIFICATION DIVISION.
PROGRAM-ID. MAGNETIC-FLUX-DENSITY.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  PI          PIC 9(4)V99 VALUE 3.1416.
01  B           PIC 9(4)V99.
01  U           PIC 9(4)V99.
01  I           PIC 9(4)V99.
01  R           PIC 9(4)V99.

PROCEDURE DIVISION.
    DISPLAY "Enter the magnetic permeability (u): "
    ACCEPT U
    DISPLAY "Enter the current flowing through the coil (i): "
    ACCEPT I
    DISPLAY "Enter the radius of the coil (r): "
    ACCEPT R
    COMPUTE B = (U * I) / (2 * PI * R)
    DISPLAY "The magnetic flux density is: " B
    STOP RUN.

