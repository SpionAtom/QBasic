'Sudoku fÅr QBasic
'von SpionAtom Dezember 2o24
'
'Anleitung:
'   Pfeiltasten = Cursor bewegen
'           1-9 = Ziffer eintragen
'             0 = Ziffer lîschen
'           Esc = Beenden




DEFINT A-Z
TYPE Koord
    zeile AS INTEGER
    spalte AS INTEGER
END TYPE
DECLARE SUB ErmittleSichtbarkeit (zeile, spalte, textverarbeitung)
DECLARE FUNCTION SudokuGewonnen ()
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
DIM SHARED sichtbar(1 TO 9), sichtbarText$, nichtsichtbar(1 TO 9), nichtsichtbarText$
DIM cursor AS Koord

LadeSudoku 1
cursor.zeile = 4
cursor.spalte = 4
gewonnen = 0


SCREEN 0, , 1, 0


    DO
        'Eingabe
        bearbeitet = 0
        taste$ = INKEY$
        SELECT CASE taste$
            CASE CHR$(0) + "H": IF cursor.zeile > 0 THEN cursor.zeile = cursor.zeile - 1
            CASE CHR$(0) + "P": IF cursor.zeile < 8 THEN cursor.zeile = cursor.zeile + 1
            CASE CHR$(0) + "K": IF cursor.spalte > 0 THEN cursor.spalte = cursor.spalte - 1
            CASE CHR$(0) + "M": IF cursor.spalte < 8 THEN cursor.spalte = cursor.spalte + 1
            CASE "0" TO "9": IF sudoInit(cursor.zeile, cursor.spalte) = 0 THEN sudo(cursor.zeile, cursor.spalte) = VAL(taste$): bearbeitet = 1
        END SELECT

        'Verarbeitung
        IF bearbeitet = 1 THEN
            IF SudokuGewonnen THEN gewonnen = 1 ELSE gewonnen = 0
        END IF

        ErmittleSichtbarkeit cursor.zeile, cursor.spalte, 1

        'Ausgabe
        CLS
        PrintSudoku
        PrintCursor cursor
        COLOR 7, 0
            sb$ = sichtbar$(cursor.zeile, cursor.spalte)
        LOCATE 19, 1: PRINT sichtbarText$;
        LOCATE 20, 1: PRINT nichtsichtbarText$;
        IF gewonnen = 1 THEN COLOR 14: LOCATE 22, 1: PRINT "Gewonnen!"
        PCOPY 1, 0
        COLOR 7, 0
    LOOP UNTIL taste$ = CHR$(27)
    END

'Ermittelt anhand der Koordinaten, in welchem Haus das Feld ist.
'Gibt einen Wert zwischen 0 und 8 zur¸ck
FUNCTION ermittleHaus (zeile, spalte)
    ermittleHaus = 3 * INT(zeile / 3) + INT(spalte / 3)
END FUNCTION

'Ermittelt fÅr ein Feld, welche anderen Ziffern gesehen werden kînnen.
'Und welche nicht gesehen werden kînnen.
'Und bastelt Ausgabestrings, wenn textverarbeitung = 1.
SUB ErmittleSichtbarkeit (zeile, spalte, textverarbeitung)

    FOR n = 1 TO 9
        sichtbar(n) = 0
        nichtsichtbar(n) = 1
    NEXT

    'Sammle alle Ziffern aus der Zeile
    FOR s = 0 TO 8
        ziffer = sudo(zeile, s)
        zifferZeichen$ = LTRIM$(STR$(ziffer))
        IF ziffer > 0 THEN
            sichtbar(ziffer) = 1
            nichtsichtbar(ziffer) = 0
        END IF
    NEXT

    'Sammle alle Ziffern aus der Spalte
    FOR s = 0 TO 8
        ziffer = sudo(s, spalte)
        zifferZeichen$ = LTRIM$(STR$(ziffer))
        IF ziffer > 0 THEN
            sichtbar(ziffer) = 1
            nichtsichtbar(ziffer) = 0
        END IF
    NEXT

    'Sammle alle Ziffern aus dem Haus
    HausZeile = 3 * INT(zeile / 3)
    HausSpalte = 3 * INT(spalte / 3)
    FOR z = HausZeile TO HausZeile + 2
    FOR s = HausSpalte TO HausSpalte + 2
        ziffer = sudo(z, s)
        zifferZeichen$ = LTRIM$(STR$(ziffer))
        IF ziffer > 0 THEN
            sichtbar(ziffer) = 1
            nichtsichtbar(ziffer) = 0
        END IF
    NEXT
    NEXT

    'String fÅr Ausgabe vorbereiten
    IF textverarbeitung = 0 THEN EXIT SUB
    sichtbarText$ = ""
    nichtsichtbarText$ = ""
    FOR n = 1 TO 9
        IF sichtbar(n) = 1 THEN
            IF sichtbarText$ <> "" THEN sichtbarText$ = sichtbarText$ + ","
            sichtbarText$ = sichtbarText$ + STR$(n)
        END IF
        IF nichtsichtbar(n) = 1 THEN
            IF nichtsichtbarText$ <> "" THEN nichtsichtbarText$ = nichtsichtbarText$ + ","
            nichtsichtbarText$ = nichtsichtbarText$ + STR$(n)
        END IF
    NEXT
    sichtbarText$ = "Sichtbar: " + sichtbarText$
    nichtsichtbarText$ = "Nicht sichtbar: " + nichtsichtbarText$



END SUB

'lÑdt ein Sudoku aus dem Data-Bestand
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
                COLOR 0, hausfarbe 'bei einer null, wÑhle schwarz und "zeichne" blo· ein Leerzeichen
                LOCATE zeile * 2 + 1, spalte * 4 + 1: PRINT "   ";
            END IF
        END IF

    NEXT
    NEXT

    'vertikale und horizontale Linien
    COLOR 7, 0
    FOR zeile = 1 TO 17
        LOCATE zeile, 12: PRINT "≥";
        LOCATE zeile, 24: PRINT "≥";
    NEXT
    LOCATE 6, 1: PRINT STRING$(35, "ƒ");
    LOCATE 12, 1: PRINT STRING$(35, "ƒ");
    LOCATE 6, 12: PRINT "≈";
    LOCATE 6, 24: PRINT "≈";
    LOCATE 12, 12: PRINT "≈";
    LOCATE 12, 24: PRINT "≈";
    COLOR 7, 0
END SUB

'PrÅft, ob das Sudoku gelîst wurde.
'Alle Felder mÅssen volle Sichtbarkeit haben
FUNCTION SudokuGewonnen

    FOR zeile = 0 TO 8
    FOR spalte = 0 TO 8
        ErmittleSichtbarkeit zeile, spalte, 0
        FOR n = 1 TO 9
            IF nichtsichtbar(n) = 1 THEN SudokuGewonnen = 0: EXIT FUNCTION
        NEXT
    NEXT
    NEXT

    SudokuGewonnen = 1



END FUNCTION

