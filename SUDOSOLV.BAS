DEFINT A-Z
DECLARE SUB initKandidaten ()
DECLARE SUB EntferneKandidat (zeile%, spalte%, ziffer%)
DECLARE SUB EntferneKandidaten (zeile%, spalte%)
DECLARE SUB InitEinheiten ()
DECLARE FUNCTION Geloest ()
DECLARE FUNCTION Strategie (strat)
DECLARE SUB TrageZifferEin (zeile, spalte, ziffer)
DECLARE SUB PrintSudoku ()
DECLARE FUNCTION erlaubt (zeile, spalte, ziffer)
DECLARE FUNCTION loeseSudokuIterativ ()
CONST strategien = 2
DIM SHARED stratLabel$(1 TO 2)
RESTORE StratLabels: FOR s = 1 TO strategien: READ stratLabel$(s): NEXT
StratLabels:
DATA "Naked Single", "Hidden Single"
DIM SHARED sudo(0 TO 8, 0 TO 8) 'im Sudoarray wird das Sudokufeld gehalten.
DIM SHARED kandidaten(0 TO 8, 0 TO 8, 0 TO 9) 'Im Kandidatenarray werden die mîglichen Kandidaten festgehalten

TYPE Koord
    zeile AS INTEGER
    spalte AS INTEGER
END TYPE

'Einheit ist ein 3-Dimensionales Array, welches die Positionen aller Zeilen,
'Spalten und HÑuser in sich trÑgt.
'Jede Zeile/Spalte/Haus ist dabei durchnummeriert (0-8), und hat 9
'Positionen (0-8).
'Einheit(0, 2, 8) hÑlt die letzte Position (8), der dritten (2) Zeile (0).
'Einheit(2, 5, 6) hÑlt die siebte Position (6), des sechsten (5) Hauses (2).
'Die Hauspositionen sind wie folgt: 012
'                                   345
'                                   678
'
'All dieser Aufwand wird betrieben, um am Ende einheitlicher Åber diese
'Einheiten iterieren zu kînnen.

    CONST eZEILE = 0, eSPALTE = 1, eHAUS = 2
    DIM SHARED Einheit(0 TO 2, 0 TO 8, 0 TO 8) AS Koord
    '                    |       |       |
    '                   Typ      |     Position
    '                          Nummer

    InitEinheiten

   
   
    ''Lade ein Sudoku
    'RESTORE S6 'S1Unloesbar
    'FOR zeile = 0 TO 8
    'FOR spalte = 0 TO 8
    '    READ sudo(zeile, spalte)
    'NEXT
    'NEXT

    'Lade von Datei
    CLS
    INPUT "Bitte geben Sie einen Dateinnamen ein: ", datei$
    IF datei$ = "" THEN datei$ = "m1.sud"
        OPEN datei$ FOR INPUT AS #1
        FOR zeile = 0 TO 8
            INPUT #1, dateiZeile$
            FOR spalte = 0 TO 8
                sudo(zeile, spalte) = VAL(MID$(dateiZeile$, spalte + 1, 1))
            NEXT
        NEXT
        CLOSE #1



    'Ausgabe for dem Lîsen
    CLS
    startZeit! = TIMER
    PRINT "Beginn: " + TIME$
    PrintSudoku
    LOCATE 12, 1: PRINT "in Bearbeitung:"
    initKandidaten
   
    'Bevor es zum Backtracking geht, werden erstmal alle Strategien
    'erschîpft. Es wird Strategie 1 solange ausgefÅhrt, wie es erfolgreich
    'ist. Wenn es nicht mehr erfolgreich ist, wird Strategie 2 ausgefÅhrt.
    'Bei Erfolg geht es wieder mit Strategie 1 los, bei Misserfolg kommt
    'Strategie 3.
    'Wenn alle Strategien kein Ergebnis mehr liefern, dann verlasse die
    'Schleife, und das Backtracking kann kommen.
    locateZeile = 1
    locateSpalte = 20
    loesungAbgeschlossen = 0
    
    DO
        FOR strat = 1 TO strategien
            strategieErfolgreich = Strategie(strat)
            LOCATE locateZeile, locateSpalte: PRINT stratLabel$(strat); strategieErfolgreich
            locateZeile = locateZeile + 1
            IF locateZeile > 20 THEN
                locateZeile = 1
                locateSpalte = locateSpalte + 20
            END IF
           
            IF strategieErfolgreich = 1 THEN
                loesungAbgeschlossen = Geloest
                LOCATE 13, 1
                PrintSudoku
                EXIT FOR
            END IF
        NEXT
     LOOP WHILE strategieErfolgreich = 1 AND loesungAbgeschlossen = 0
     
     IF loesungAbgeschlossen = 1 THEN
        LOCATE 13, 1
        PrintSudoku
        LOCATE 24, 1
        PRINT "Bearbeitungszeit: " + STR$(TIMER - startZeit!) + "s";
     ELSE
        LOCATE locateZeile, locateSpalte: PRINT "Backtracking"
        IF loeseSudokuIterativ = 1 THEN
            LOCATE 13, 1
            PrintSudoku
            LOCATE 24, 1
            PRINT "Bearbeitungszeit: " + STR$(TIMER - startZeit!) + "s";
        ELSE
            LOCATE 24, 1
            PRINT "Keine Lîsung gefunden! Bearbeitungszeit: " + STR$(TIMER - startZeit!);
        END IF
    END IF


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

S1Unloesbar:
DATA 0, 0, 0, 1, 2, 3, 0, 8, 0
DATA 1, 0, 0, 0, 0, 0, 0, 3, 7
DATA 0, 0, 8, 7, 4, 6, 9, 0, 0
DATA 3, 0, 5, 0, 0, 0, 6, 2, 9
DATA 7, 2, 6, 0, 0, 0, 0, 0, 0
DATA 8, 0, 1, 0, 0, 0, 3, 0, 4
DATA 0, 0, 7, 9, 0, 0, 2, 0, 3
DATA 9, 0, 3, 4, 7, 0, 0, 6, 5
DATA 0, 2, 0, 6, 0, 8, 0, 0, 1

S5: 'benîtigt hidden singles
DATA 9, 8, 4, 7, 3, 5, 0, 0, 0
DATA 0, 0, 7, 0, 0, 0, 0, 0, 0
DATA 0, 0, 3, 0, 0, 0, 5, 0, 7
DATA 0, 0, 0, 9, 4, 0, 0, 1, 8
DATA 0, 4, 9, 1, 0, 2, 0, 0, 0
DATA 1, 3, 0, 0, 0, 0, 0, 4, 0
DATA 0, 0, 0, 2, 0, 0, 0, 7, 0
DATA 0, 7, 0, 4, 0, 8, 0, 0, 3
DATA 4, 0, 1, 0, 6, 7, 0, 5, 0


S6:
DATA 0, 0, 7, 0, 0, 3, 0, 0, 1
DATA 0, 0, 5, 1, 2, 0, 0, 4, 0
DATA 2, 0, 0, 0, 0, 6, 0, 0, 0
DATA 0, 0, 0, 0, 0, 2, 0, 0, 0
DATA 0, 0, 4, 0, 0, 0, 9, 0, 0
DATA 5, 0, 0, 7, 8, 0, 0, 0, 6
DATA 8, 0, 0, 5, 1, 0, 0, 0, 7
DATA 0, 7, 0, 0, 0, 0, 0, 6, 0
DATA 0, 0, 0, 3, 0, 0, 0, 0, 0

SUB EntferneKandidat (zeile, spalte, ziffer)

    IF sudo(zeile, spalte) = 0 THEN
        'Reduziere die Anzahl der Kandidaten
        IF kandidaten(zeile, spalte, ziffer) = 1 THEN kandidaten(zeile, spalte, 0) = kandidaten(zeile, spalte, 0) - 1
        'Setze Kandidat auf 0
        kandidaten(zeile, spalte, ziffer) = 0
    END IF

END SUB

'Erwartet als Parameter eine Koordinate vom Sudokufeld.
'Findet sich an dieser Koordinate eine Ziffer <> 0, so werden in der
'gesamten Zeile, Spalte und Haus diese Ziffer als Kandidaten gelîscht.
SUB EntferneKandidaten (zeile, spalte)

    ziffer = sudo(zeile, spalte)
    IF ziffer = 0 THEN EXIT SUB

    DIM k AS Koord
    ermittleHaus = 3 * INT(zeile / 3) + INT(spalte / 3)
    'Entferne Ziffer als Kandidat aus Zeile, Spalte und Haus
    FOR e = 0 TO 8
        k = Einheit(eZEILE, zeile, e)
        EntferneKandidat k.zeile, k.spalte, ziffer
        k = Einheit(eSPALTE, spalte, e)
        EntferneKandidat k.zeile, k.spalte, ziffer
        k = Einheit(eHAUS, ermittleHaus, e)
        EntferneKandidat k.zeile, k.spalte, ziffer
    NEXT

END SUB

'PrÅft, ob eine Ziffer in der Zelle erlaubt ist
FUNCTION erlaubt (zeile, spalte, ziffer)
   
    'PrÅfe Zeile
    FOR s = 0 TO 8
        IF sudo(zeile, s) = ziffer THEN
            erlaubt = 0
            EXIT FUNCTION
        END IF
    NEXT

    'PrÅfe Spalte
    FOR z = 0 TO 8
        IF sudo(z, spalte) = ziffer THEN
            erlaubt = 0
            EXIT FUNCTION
        END IF
    NEXT

    'PrÅfe Haus
    hauszeile = zeile - zeile MOD 3
    hausSpalte = spalte - spalte MOD 3
    FOR z = 0 TO 2
    FOR s = 0 TO 2
        IF sudo(hauszeile + z, hausSpalte + s) = ziffer THEN
            erlaubt = 0
            EXIT FUNCTION
        END IF
    NEXT
    NEXT

    'Ziffer kommt nicht vor in Zeile, Spalte oder Haus, also erlaubt
    erlaubt = 1

END FUNCTION

FUNCTION Geloest

    FOR zeile = 0 TO 8
    FOR spalte = 0 TO 8
        'IF kandidaten(zeile, spalte, 0) <> 1 THEN Geloest = 0: EXIT FUNCTION
        IF sudo(zeile, spalte) = 0 THEN Geloest = 0: EXIT FUNCTION
    NEXT
    NEXT
                                           
    Geloest = 1

END FUNCTION

SUB InitEinheiten
   
    DIM tempPos AS Koord

    'Zeilen initialisieren
    FOR zeile = 0 TO 8
        FOR position = 0 TO 8
            tempPos.zeile = zeile
            tempPos.spalte = position
            Einheit(eZEILE, zeile, position) = tempPos
        NEXT
    NEXT

    'Spalten initialisieren
    FOR spalte = 0 TO 8
        FOR position = 0 TO 8
            tempPos.zeile = position
            tempPos.spalte = spalte
            Einheit(eSPALTE, spalte, position) = tempPos
        NEXT
    NEXT

    'HÑuser initialisieren
    FOR haus = 0 TO 8
        FOR position = 0 TO 8
            tempPos.zeile = 3 * INT(haus / 3) + INT(position / 3)
            tempPos.spalte = 3 * INT(haus MOD 3) + INT(position MOD 3)
            Einheit(eHAUS, haus, position) = tempPos
        NEXT
    NEXT

END SUB

SUB initKandidaten

    'Setze alle Kandidaten auf sichtbar
    FOR zeile = 0 TO 8
    FOR spalte = 0 TO 8
        FOR ziffer = 1 TO 9
            kandidaten(zeile, spalte, ziffer) = 1
        NEXT
        IF sudo(zeile, spalte) <> 0 THEN
            kandidaten(zeile, spalte, 0) = 1
        ELSE
            kandidaten(zeile, spalte, 0) = 9
        END IF
    NEXT
    NEXT

    'Entferne Kandidaten entsprechend der Sudoku-Regeln
    FOR zeile = 0 TO 8
    FOR spalte = 0 TO 8
        EntferneKandidaten zeile, spalte
    NEXT
    NEXT

END SUB

'Lîsen mit Backtracking
'nach youtube.com/watch?v=J0N1G33wNIQ
FUNCTION loeseSudokuIterativ

    'Kopiere zunÑchst das sudo-Array in sudoFix. Wenn in sudoFix ein Wert
    'ungleich 0 steht, so darf dieser wÑhrend des Lîsens nicht mehr geÑndert
    'werden.
    DIM sudoFix(0 TO 8, 0 TO 8)
    FOR zeile = 0 TO 8
    FOR spalte = 0 TO 8
        sudoFix(zeile, spalte) = sudo(zeile, spalte)
    NEXT
    NEXT

    'Zeiger fÅr aktuelle Position
    zeile = 0
    spalte = 0
    erstefreieZeile = -1
    erstefreieSpalte = -1

    'Die Schleife lÑuft, bis die letzte Zelle ausgefÅllt wurde und der
    'Zeiger entsprechend auf Position [8,9] gesprungen ist.
    WHILE (NOT (zeile = 8 AND spalte = 9)) AND INKEY$ <> CHR$(27)
        'LOCATE 2, 1
        'PrintSudoku
        'LOCATE 1, 40: PRINT "Zeile: "; zeile; "  Spalte: "; spalte
        'SLEEP
        'PrÅfe zunÑchst, ob ein Zeilensprung des Zeigers stattfinden muss.
        IF spalte = 9 THEN
            zeile = zeile + 1
            spalte = 0
        END IF

        'Schaue, ob die Zeigerposition auf keine initiale Ziffer zeigt.
        'Dann erhîhe die Ziffer darin bis ein valider Wert erreicht wurde.
        IF sudoFix(zeile, spalte) = 0 THEN
            'gucken, ob es die erste freie Zelle ist
            IF erstefreieZeile = -1 AND erstefreieSpalte = -1 THEN
                erstefreieZeile = zeile
                erstefreieSpalte = spalte
            END IF

            FOR ziffer = sudo(zeile, spalte) + 1 TO 10
                IF erlaubt(zeile, spalte, ziffer) THEN
                    sudo(zeile, spalte) = ziffer
                    EXIT FOR
                END IF
            NEXT

            'Wurden alle gÅltigen Ziffern geprÅft und keine passt, so wurde
            'eine 10 eingetragen. Das zeigt an, dass keine gÅltige Ziffer
            'gefunden wurde, es muss wieder eine 0 eingetragen werden, und
            'es muss die vorherige Zelle gefunden werden, die verÑndert
            'werden kann.
            'Wenn aber die erste freie Zelle auf den Wert 10 gelaufen ist,
            'so gilt das Sudoku als unlîsbar.
            'Wurde eine gÅltige Ziffer eingetragen, so erhîhe den Zeiger auf
            'die nÑchste Zelle.
            IF sudo(zeile, spalte) = 10 THEN
                IF zeile = erstefreieZeile AND spalte = erstefreieSpalte THEN
                    loeseSudokuIterativ = 0
                    EXIT FUNCTION
                END IF
                sudo(zeile, spalte) = 0
                DO
                    spalte = spalte - 1
                    IF spalte = -1 THEN
                        zeile = zeile - 1
                        spalte = 8
                    END IF
                LOOP WHILE sudoFix(zeile, spalte) <> 0
            ELSE
                spalte = spalte + 1
            END IF
        ELSE
            spalte = spalte + 1
        END IF
    WEND

    loeseSudokuIterativ = 1


END FUNCTION

'Ausgabe des Sudokus
SUB PrintSudoku

    FOR zeile = 0 TO 8
        zeilenString$ = ""
        FOR spalte = 0 TO 8
            zeilenString$ = zeilenString$ + STR$(sudo(zeile, spalte))
        NEXT
        PRINT zeilenString$
    NEXT

END SUB

'FÅhrt eine Strategie aus.
'Gibt eine 1 zurÅck, wenn die Strategie eine énderung getÑtigt hat, sonst 0.
FUNCTION Strategie (strat)

    Strategie = 0

    SELECT CASE strat
        CASE 1:     'Naked Single
                    'PrÅfe, ob es in einer Zelle nur einen Kandidaten gibt.
            FOR zeile = 0 TO 8
            FOR spalte = 0 TO 8
                'PRINT kandidaten(zeile, spalte, 0);
                IF kandidaten(zeile, spalte, 0) = 1 THEN
                    FOR ziffer = 1 TO 9
                        IF kandidaten(zeile, spalte, ziffer) = 1 THEN EXIT FOR
                    NEXT
                    IF sudo(zeile, spalte) = 0 THEN
                        TrageZifferEin zeile, spalte, ziffer
                        Strategie = 1
                    END IF
                END IF
            NEXT
            NEXT
            
        CASE 2:     'Hidden Single
                    'PrÅfe, ob in einer Einheit ein Kandidaten nur an einer
                    'Stelle vorkommt
       
        DIM kandidatenHS(1 TO 9), kandidatenKoords(1 TO 9) AS Koord, kkAnzahl
        kkAnzahl = 0
        DIM k AS Koord

          
            'PrÅfe Zeilen
            FOR e = 0 TO 8
                FOR n = 1 TO 9: kandidatenHS(n) = 0: NEXT 'reset Kandidatenarray
                FOR posi = 0 TO 8
                    k = Einheit(eZEILE, e, posi)
                    'ErmittleKandidaten k.zeile, k.spalte
                    FOR n = 1 TO 9
                        IF sudo(k.zeile, k.spalte) = 0 THEN
                        IF kandidaten(k.zeile, k.spalte, n) = 1 THEN
                            kandidatenHS(n) = kandidatenHS(n) + 1
                            IF kandidatenHS(n) = 1 THEN 'Merke dir die Koordinaten beim ersten Fund eines Kandidaten
                                kandidatenKoords(n).zeile = k.zeile
                                kandidatenKoords(n).spalte = k.spalte
                            END IF
                        END IF
                        END IF
                    NEXT
                NEXT

                FOR kandidat = 1 TO 9
                    IF kandidatenHS(kandidat) = 1 THEN
                        'Dieser Kandidat taucht nur an einer Stelle auf
                        k = kandidatenKoords(kandidat)
                        TrageZifferEin k.zeile, k.spalte, kandidat
                        Strategie = 1
                    END IF
                NEXT
            NEXT

            'PrÅfe Spalten
            FOR e = 0 TO 8
                FOR n = 1 TO 9: kandidatenHS(n) = 0: NEXT 'reset Kandidatenarray
                FOR posi = 0 TO 8
                    k = Einheit(eSPALTE, e, posi)

                    'ErmittleKandidaten k.zeile, k.spalte
                    FOR n = 1 TO 9
                        IF sudo(k.zeile, k.spalte) = 0 THEN
                        IF kandidaten(k.zeile, k.spalte, n) = 1 THEN
                            kandidatenHS(n) = kandidatenHS(n) + 1
                            IF kandidatenHS(n) = 1 THEN 'Merke dir die Koordinaten beim ersten Fund eines Kandidaten
                                kandidatenKoords(n).zeile = k.zeile
                                kandidatenKoords(n).spalte = k.spalte
                            END IF
                        END IF
                        END IF
                    NEXT
                NEXT

                FOR kandidat = 1 TO 9
                    IF kandidatenHS(kandidat) = 1 THEN
                        'Dieser Kandidat taucht nur an einer Stelle auf
                        k = kandidatenKoords(kandidat)
                        TrageZifferEin k.zeile, k.spalte, kandidat
                        Strategie = 1
                    END IF
                NEXT
            NEXT
          
            'PrÅfe HÑuser
            FOR e = 0 TO 8
                FOR n = 1 TO 9: kandidatenHS(n) = 0: NEXT 'reset Kandidatenarray
                FOR posi = 0 TO 8
                    k = Einheit(eHAUS, e, posi)

                    'ErmittleKandidaten k.zeile, k.spalte
                    FOR n = 1 TO 9
                        IF sudo(k.zeile, k.spalte) = 0 THEN
                        IF kandidaten(k.zeile, k.spalte, n) = 1 THEN
                            kandidatenHS(n) = kandidatenHS(n) + 1
                            IF kandidatenHS(n) = 1 THEN 'Merke dir die Koordinaten beim ersten Fund eines Kandidaten
                                kandidatenKoords(n).zeile = k.zeile
                                kandidatenKoords(n).spalte = k.spalte
                            END IF
                        END IF
                        END IF
                    NEXT
                NEXT

                FOR kandidat = 1 TO 9
                    IF kandidatenHS(kandidat) = 1 THEN
                        'Dieser Kandidat taucht nur an einer Stelle auf
                        k = kandidatenKoords(kandidat)
                        TrageZifferEin k.zeile, k.spalte, kandidat
                        Strategie = 1
                    END IF
                NEXT
            NEXT

       
    END SELECT


END FUNCTION

SUB TrageZifferEin (zeile, spalte, ziffer)

    sudo(zeile, spalte) = ziffer
    EntferneKandidaten zeile, spalte

END SUB

