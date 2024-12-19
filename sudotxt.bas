'Sudoku fr QBasic
'von SpionAtom Dezember 2o24
'
'Anleitung:
'   Pfeiltasten = Cursor bewegen
'           1-9 = Ziffer eintragen
'             0 = Ziffer l”schen
'           Esc = Beenden




DEFINT A-Z
TYPE Koord
    zeile AS INTEGER
    spalte AS INTEGER
END TYPE
DECLARE SUB PrintCursor (c AS Koord)
DECLARE SUB PrintSudoku ()
DECLARE SUB LadeSudoku (n)
DECLARE FUNCTION ermittleHaus (zeile, spalte)

S1:
DATA 0, 0, 0, 1, 2, 3, 0, 8, 0
DATA 1, 0, 0, 0, 0, 0, 0, 3, 7
DATA 0, 0, 8, 7, 4, 6, 9, 0, 0
DATA 3, 0, 5, 0, 0, 0, 6, 2, 9
DATA 7, 2, 6, 0, 0, 0, 0, 0, 0
DATA 8, 0, 1, 0, 0, 0, 3, 0, 4
DATA 0, 0, 7, 9, 0, 0, 2, 0, 3
DATA 9, 0, 3, 4, 7, 0, 0, 6, 5
DATA 0, 5, 0, 6, 0, 8, 0, 0, 1

DIM SHARED sudo(0 TO 8, 0 TO 8)
DIM SHARED sudoInit(0 TO 8, 0 TO 8)
DIM SHARED sudoNotiz$(0 TO 8, 0 TO 8)
DIM Cursor AS Koord

LadeSudoku 1
Cursor.zeile = 4
Cursor.spalte = 4

SCREEN 0, , 1, 0

    DO
        'Eingabe
        taste$ = INKEY$
        SELECT CASE taste$
            CASE CHR$(0) + "H": IF Cursor.zeile > 0 THEN Cursor.zeile = Cursor.zeile - 1
            CASE CHR$(0) + "P": IF Cursor.zeile < 8 THEN Cursor.zeile = Cursor.zeile + 1
            CASE CHR$(0) + "K": IF Cursor.spalte > 0 THEN Cursor.spalte = Cursor.spalte - 1
            CASE CHR$(0) + "M": IF Cursor.spalte < 8 THEN Cursor.spalte = Cursor.spalte + 1
            CASE "0" TO "9": IF sudoInit(Cursor.zeile, Cursor.spalte) = 0 THEN sudo(Cursor.zeile, Cursor.spalte) = VAL(taste$)
        END SELECT

        'Ausgabe
        CLS
        PrintSudoku
        PrintCursor Cursor
        PCOPY 1, 0
        COLOR 7, 0
    LOOP UNTIL taste$ = CHR$(27)
    END

'Ermittelt anhand der Koordinaten, in welchem Haus das Feld ist.
'Gibt einen Wert zwischen 0 und 8 zurück
FUNCTION ermittleHaus (zeile, spalte)
    ermittleHaus = 3 * INT(zeile / 3) + INT(spalte / 3)
END FUNCTION

'l„dt ein Sudoku aus dem Data-Bestand
'Erwartet wird eine Nummer, je nach Nummer wird ein anderes Sudoku geladen.
'
SUB LadeSudoku (n)

    SELECT CASE n
        CASE ELSE:
            RESTORE S1
    END SELECT


    FOR zeile = 0 TO 8
    FOR spalte = 0 TO 8
        READ sudoInit(zeile, spalte)
        sudo(zeile, spalte) = sudoInit(zeile, spalte)
        sudoNotiz$(zeile, spalte) = ""
    NEXT
    NEXT

END SUB

SUB PrintCursor (c AS Koord)

    IF SIN(TIMER * 10) > 0 THEN EXIT SUB 'Blinker an

    haus = ermittleHaus(c.zeile, c.spalte)
    hausfarbe = 6 + haus MOD 2

    COLOR 14, hausfarbe
    LOCATE c.zeile * 2 + 1, c.spalte * 4 + 1: PRINT ">";
    LOCATE c.zeile * 2 + 1, c.spalte * 4 + 3: PRINT "<";

END SUB

'Zeichne das Sudoku
SUB PrintSudoku

    FOR zeile = 0 TO 8
    FOR spalte = 0 TO 8
        haus = ermittleHaus(zeile, spalte)
        hausfarbe = 6 + haus MOD 2

        ziffer = sudo(zeile, spalte)
        zifferZeichen$ = LTRIM$(STR$(ziffer))
        IF sudoInit(zeile, spalte) = 0 THEN
            zifferfarbe = 15
        ELSE
            zifferfarbe = 0
        END IF

        IF ziffer > 0 THEN
            COLOR zifferfarbe, hausfarbe
            LOCATE zeile * 2 + 1, spalte * 4 + 1: PRINT " " + zifferZeichen$ + " ";
        ELSE
            IF ziffer = 0 THEN
                COLOR 0, hausfarbe 'bei einer null, w„hle schwarz und "zeichne" bloá ein Leerzeichen
                LOCATE zeile * 2 + 1, spalte * 4 + 1: PRINT "   ";
            END IF
        END IF

    NEXT
    NEXT

    'vertikale und horizontale Linien
    COLOR 7, 0
    FOR zeile = 1 TO 17
        LOCATE zeile, 12: PRINT "³";
        LOCATE zeile, 24: PRINT "³";
    NEXT
    LOCATE 6, 1: PRINT STRING$(35, "Ä");
    LOCATE 12, 1: PRINT STRING$(35, "Ä");
    LOCATE 6, 12: PRINT "Å";
    LOCATE 6, 24: PRINT "Å";
    LOCATE 12, 12: PRINT "Å";
    LOCATE 12, 24: PRINT "Å";
    COLOR 7, 0
END SUB

