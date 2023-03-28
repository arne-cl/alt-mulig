IDENTIFICATION DIVISION.
    PROGRAM-ID. FLUX-DENSITY.

DATA DIVISION.
    WORKING-STORAGE SECTION.
    01  PI              PIC 9(8)V99 VALUE 3.14159265.
    01  COIL-DIAMETER   PIC 9(8)V99.
    01  COIL-LENGTH     PIC 9(8)V99.
    01  CURRENT         PIC 9(8)V99.
    01  FLUX-DENSITY    PIC 9(8)V99.

    PROCEDURE DIVISION.
    DISPLAY "Enter the diameter of the coil in meters: "
    ACCEPT COIL-DIAMETER
    DISPLAY "Enter the length of the coil in meters: "
    ACCEPT COIL-LENGTH
    DISPLAY "Enter the current flowing through the coil in amperes: "
    ACCEPT CURRENT

    COMPUTE FLUX-DENSITY = (2 * PI * COIL-LENGTH * CURRENT) / (COIL-DIAMETER * COIL-DIAMETER)
    ROUNDED
    DISPLAY "The magnetic flux density is ", FLUX-DENSITY, " tesla."
    STOP RUN.

