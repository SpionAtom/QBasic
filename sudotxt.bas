'Sudoku fÅr QBasic
'von SpionAtom Dezember 2o24
'
'Anleitung:
'   Pfeiltasten = Cursor bewegen
'           1-9 = Ziffer eintragen
'             0 = Ziffer lîschen
'            F1 = Finde Naked Singles und trage sie ein
'            F2 = Finde Hidden Singles und trage sie ein
'           Esc = Beenden


'QB64 Magic, um kein so kleines Fenster zu haben
'$Resize:Stretch


DEFINT A-Z
TYPE Koord
    zeile AS INTEGER
    spalte AS INTEGER
END TYPE
DECLARE SUB TrageZifferEin (zeile, spalte, ziffer)
DECLARE SUB LoesungsStrategie (n)
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

S2:
DATA 0, 9, 4, 0, 3, 0, 1, 0, 0
DATA 8, 1, 2, 7, 0, 0, 0, 9, 6
DATA 3, 0, 0, 1, 9, 0, 0, 0, 0
DATA 0, 3, 0, 9, 0, 4, 6, 0, 0
DATA 0, 0, 8, 6, 1, 3, 0, 4, 9
DATA 0, 0, 6, 2, 0, 0, 0, 0, 1
DATA 4, 0, 3, 5, 0, 0, 0, 0, 8
DATA 5, 0, 0, 0, 2, 0, 7, 0, 0
DATA 0, 6, 0, 0, 0, 8, 4, 1, 5

S3:
DATA 0, 0, 0, 0, 9, 0, 0, 8, 4
DATA 0, 0, 8, 0, 0, 0, 5, 0, 7
DATA 0, 0, 0, 6, 0, 8, 0, 0, 0
DATA 8, 0, 0, 9, 0, 0, 0, 0, 5
DATA 0, 9, 0, 3, 0, 1, 0, 2, 0
DATA 6, 0, 0, 0, 0, 4, 0, 0, 3
DATA 0, 0, 0, 4, 0, 5, 0, 0, 0
DATA 2, 0, 7, 0, 0, 0, 3, 0, 0
DATA 9, 1, 0, 0, 7, 0, 0, 0, 0

DIM SHARED sudo(0 TO 8, 0 TO 8) 'Das Spiel-Array. Speichert die Eingaben des Benutzers
DIM SHARED sudoGueltig(0 TO 8, 0 TO 8) 'Das PrÅf-Array. Speichert 1, falls im Eingabe-Array eine gÅltige Ziffer ist, sonst 0
DIM SHARED sudoInit(0 TO 8, 0 TO 8)
DIM SHARED sudoNotiz$(0 TO 8, 0 TO 8)
DIM SHARED sichtbar(1 TO 9), sichtbarAnzahl, sichtbarText$, nichtsichtbar(1 TO 9), nichtsichtbarText$, nichtsichtbarAnzahl
DIM cursor AS Koord

LadeSudoku 3
cursor.zeile = 4
cursor.spalte = 4
gewonnen = 0


SCREEN 0, , 1, 0


    DO
        'Eingabe
        bearbeitet = 0
        eingabeZiffer = -1
        taste$ = INKEY$
        SELECT CASE taste$
            CASE CHR$(0) + "H": IF cursor.zeile > 0 THEN cursor.zeile = cursor.zeile - 1
            CASE CHR$(0) + "P": IF cursor.zeile < 8 THEN cursor.zeile = cursor.zeile + 1
            CASE CHR$(0) + "K": IF cursor.spalte > 0 THEN cursor.spalte = cursor.spalte - 1
            CASE CHR$(0) + "M": IF cursor.spalte < 8 THEN cursor.spalte = cursor.spalte + 1
            CASE "0" TO "9"
                IF sudoInit(cursor.zeile, cursor.spalte) = 0 THEN
                    eingabeZiffer = VAL(taste$)
                    TrageZifferEin cursor.zeile, cursor.spalte, eingabeZiffer
                    bearbeitet = 1

                END IF
            'CASE "!": IF sudoInit(cursor.zeile, cursor.spalte) = 0 THEN sudoNotiz$(cursor.zeile, cursor.spalte) = RIGHT$(sudoNotiz$(cursor.zeile, cursor.spalte) + "1", 3): sudo(cursor.zeile, cursor.spalte) = 0
            CASE CHR$(0) + CHR$(59): LoesungsStrategie 1: bearbeitet = 1 'Naked Singles
            CASE CHR$(0) + CHR$(60): LoesungsStrategie 2: bearbeitet = 1 'Hidden Singles
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
        LOCATE 19, 40: PRINT "GÅltig: "; sudoGueltig(cursor.zeile, cursor.spalte);
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
        IF ziffer > 0 AND s <> spalte THEN
            sichtbar(ziffer) = 1
            nichtsichtbar(ziffer) = 0
        END IF
    NEXT

    'Sammle alle Ziffern aus der Spalte
    FOR s = 0 TO 8
        ziffer = sudo(s, spalte)
        zifferZeichen$ = LTRIM$(STR$(ziffer))
        IF ziffer > 0 AND s <> zeile THEN
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
        IF ziffer > 0 AND (z <> zeile AND s <> spalte) THEN
            sichtbar(ziffer) = 1
            nichtsichtbar(ziffer) = 0
        END IF
    NEXT
    NEXT

    'String fÅr Ausgabe vorbereiten
    IF textverarbeitung = 0 THEN EXIT SUB
    sichtbarText$ = ""
    nichtsichtbarText$ = ""
    sichtbarAnzahl = 0
    nichtsichtbarAnzahl = 0
    FOR n = 1 TO 9
        IF sichtbar(n) = 1 THEN
            IF sichtbarText$ <> "" THEN sichtbarText$ = sichtbarText$ + ","
            sichtbarText$ = sichtbarText$ + STR$(n)
            sichtbarAnzahl = sichtbarAnzahl + 1
        END IF
        IF nichtsichtbar(n) = 1 THEN
            IF nichtsichtbarText$ <> "" THEN nichtsichtbarText$ = nichtsichtbarText$ + ","
            nichtsichtbarText$ = nichtsichtbarText$ + STR$(n)
            nichtsichtbarAnzahl = nichtsichtbarAnzahl + 1
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
        CASE 2:
            RESTORE S2
        CASE 3:
            RESTORE S3
        CASE ELSE:
            RESTORE S1
    END SELECT


    FOR zeile = 0 TO 8
    FOR spalte = 0 TO 8
        READ sudoInit(zeile, spalte)
        sudo(zeile, spalte) = sudoInit(zeile, spalte)
        IF sudoInit(zeile, spalte) > 0 THEN
            sudoGueltig(zeile, spalte) = 1 'Initialwerte sind per Definition bereits gÅltige Werte
        ELSE
            sudoGueltig(zeile, spalte) = 0
        END IF

        sudoNotiz$(zeile, spalte) = ""
    NEXT
    NEXT

END SUB

SUB LoesungsStrategie (n)

    SELECT CASE n
        CASE 1: 'Naked Singles
        'Trage Åberall dort eine Ziffer ein, bei dem die Nichtsichtbaren
        'Kandidaten auf eine Option begrenzt sind.
            FOR zeile = 0 TO 8
            FOR spalte = 0 TO 8
                ErmittleSichtbarkeit zeile, spalte, 1
                IF nichtsichtbarAnzahl = 1 THEN
                    ziffer = -1
                    FOR z = 1 TO 9
                        IF nichtsichtbar(z) = 1 THEN ziffer = z
                    NEXT
                    TrageZifferEin zeile, spalte, ziffer
                END IF
            NEXT
            NEXT
       
        CASE 2: 'Hidden Single
        'PrÅfe fÅr jede Zeile/Spalte/Haus, ob ein Ziffernkandidat nur in
        'einem unbelegten Feld vorkommt. Falls ja, handelt es sich um einen
        'Hidden Single, den man fest eintragen kann.
       
        'PrÅfung der Zeilen
        DIM PruefZiffernZaehler(1 TO 9)
            FOR zeile = 0 TO 8
                FOR pzz = 1 TO 9: PruefZiffernZaehler(pzz) = 0: NEXT
                FOR spalte = 0 TO 8
                    IF sudo(zeile, spalte) = 0 THEN
                    ErmittleSichtbarkeit zeile, spalte, 1
                        FOR z = 1 TO 9
                            PruefZiffernZaehler(z) = PruefZiffernZaehler(z) + nichtsichtbar(z)
                        NEXT
                    END IF
                NEXT
                FOR z = 1 TO 9
                    IF PruefZiffernZaehler(z) = 1 THEN 'Hidden Single gefunden
                          'Ermittle nochmal die Position des Hidden Singles
                          FOR spalte = 0 TO 8
                              IF sudo(zeile, spalte) = 0 THEN
                                    ErmittleSichtbarkeit zeile, spalte, 1
                                    IF nichtsichtbar(z) = 1 THEN
                                        TrageZifferEin zeile, spalte, z
                                        EXIT FOR
                                    END IF
                              END IF
                          NEXT
                    END IF
                NEXT
            NEXT


        'PrÅfung der Spalten
            FOR spalte = 0 TO 8
                FOR pzz = 1 TO 9: PruefZiffernZaehler(pzz) = 0: NEXT
                FOR zeile = 0 TO 8
                    IF sudo(zeile, spalte) = 0 THEN
                    ErmittleSichtbarkeit zeile, spalte, 1
                        FOR z = 1 TO 9
                            PruefZiffernZaehler(z) = PruefZiffernZaehler(z) + nichtsichtbar(z)
                        NEXT
                    END IF
                NEXT
                FOR z = 1 TO 9
                    IF PruefZiffernZaehler(z) = 1 THEN 'Hidden Single gefunden
                          'Ermittle nochmal die Position des Hidden Singles
                          FOR zeile = 0 TO 8
                              IF sudo(zeile, spalte) = 0 THEN
                                    ErmittleSichtbarkeit zeile, spalte, 1
                                    IF nichtsichtbar(z) = 1 THEN
                                        TrageZifferEin zeile, spalte, z
                                        EXIT FOR
                                    END IF
                              END IF
                          NEXT
                    END IF
                NEXT
            NEXT

        'PrÅfung der HÑuser
            FOR haus = 0 TO 8
                FOR pzz = 1 TO 9: PruefZiffernZaehler(pzz) = 0: NEXT
                HausZeile = 3 * INT(haus / 3)
                HausSpalte = 3 * INT(haus MOD 3)
   
                    FOR hz = HausZeile TO HausZeile + 2
                    FOR hs = HausSpalte TO HausSpalte + 2
                   
                        IF sudo(hz, hs) = 0 THEN
                            ErmittleSichtbarkeit hz, hs, 1
                                FOR z = 1 TO 9
                                PruefZiffernZaehler(z) = PruefZiffernZaehler(z) + nichtsichtbar(z)
                                NEXT
                        END IF
                    NEXT
                    NEXT

                    FOR z = 1 TO 9
                        IF PruefZiffernZaehler(z) = 1 THEN 'Hidden Single gefunden
                              'Ermittle nochmal die Position des Hidden Singles
                   
                            FOR hz = HausZeile TO HausZeile + 2
                            FOR hs = HausSpalte TO HausSpalte + 2
                           
                                IF sudo(hz, hs) = 0 THEN
                                    ErmittleSichtbarkeit hz, hs, 1
                                    IF nichtsichtbar(z) = 1 THEN
                                        TrageZifferEin hz, hs, z
                                        EXIT FOR
                                    END IF
                                END IF
                            NEXT
                            NEXT
                        END IF
                    NEXT
            NEXT

                     





    END SELECT
    


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
            IF sudoGueltig(zeile, spalte) = 0 THEN zifferfarbe = 13
        ELSE
            zifferfarbe = 0
        END IF

        IF ziffer > 0 THEN
            COLOR zifferfarbe, hausfarbe
            LOCATE zeile * 2 + 1, spalte * 4 + 1: PRINT " " + zifferZeichen$ + " ";
        ELSE
            IF ziffer = 0 THEN
                IF sudoNotiz$(zeile, spalte) <> "" THEN
                    COLOR 8, hausfarbe
                    LOCATE zeile * 2 + 1, spalte * 4 + 1: PRINT sudoNotiz$(zeile, spalte)
                END IF
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
        IF sudoGueltig(zeile, spalte) = 0 THEN SudokuGewonnen = 0: EXIT FUNCTION
        
    NEXT
    NEXT

    SudokuGewonnen = 1



END FUNCTION

SUB TrageZifferEin (zeile, spalte, ziffer)

    IF sudoInit(zeile, spalte) = 0 THEN
        sudo(zeile, spalte) = ziffer
        ErmittleSichtbarkeit zeile, spalte, 0
        IF ziffer = 0 THEN
            sudoGueltig(zeile, spalte) = 0
        ELSEIF sichtbar(ziffer) = 1 THEN
            sudoGueltig(zeile, spalte) = 0
        ELSE
            sudoGueltig(zeile, spalte) = 1
        END IF
    END IF

END SUB

