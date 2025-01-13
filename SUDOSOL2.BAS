DEFINT A-Z
DECLARE SUB PrintSudoku ()
DECLARE FUNCTION erlaubt (zeile, spalte, ziffer)
DECLARE FUNCTION loeseSudokuIterativ ()
DIM SHARED sudo(0 TO 8, 0 TO 8), sudoInit(0 TO 8, 0 TO 8)


    'Lade ein Sudoku
    RESTORE S1
    FOR zeile = 0 TO 8
    FOR spalte = 0 TO 8
        READ sudoInit(zeile, spalte)
        sudo(zeile, spalte) = sudoInit(zeile, spalte)
    NEXT
    NEXT

    'Ausgabe for dem Lîsen
    CLS
    PRINT "Beginn: " + TIME$
    PrintSudoku

    
    IF loeseSudokuIterativ = 1 THEN
        LOCATE 12, 1
        PRINT "Lîsung: " + TIME$
        PrintSudoku
    ELSE
        LOCATE 12, 1
        PRINT "Keine Lîsung gefunden " + TIME$
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
    hausZeile = zeile - zeile MOD 3
    hausSpalte = spalte - spalte MOD 3
    FOR z = 0 TO 2
    FOR s = 0 TO 2
        IF sudo(hausZeile + z, hausSpalte + s) = ziffer THEN
            erlaubt = 0
            EXIT FUNCTION
        END IF
    NEXT
    NEXT

    'Ziffer kommt nicht vor in Zeile, Spalte oder Haus, also erlaubt
    erlaubt = 1
   
END FUNCTION

'Lîsen mit Backtracking
'nach youtube.com/watch?v=J0N1G33wNIQ
FUNCTION loeseSudokuIterativ

    'Zeiger fÅr aktuelle Position
    zeile = 0
    spalte = 0
    erstefreieZeile = -1
    erstefreieSpalte = -1

    'Die Schleife lÑuft, bis die letzte Zelle ausgefÅllt wurde und der
    'Zeiger entsprechend auf Position [8,9] gesprungen ist.
    WHILE (NOT (zeile = 8 AND spalte = 9)) AND INKEY$ <> CHR$(27)
        LOCATE 2, 1
        PrintSudoku
        LOCATE 1, 40: PRINT "Zeile: "; zeile; "  Spalte: "; spalte
        'SLEEP
        'PrÅfe zunÑchst, ob ein Zeilensprung des Zeigers stattfinden muss.
        IF spalte = 9 THEN
            zeile = zeile + 1
            spalte = 0
        END IF

        'Schaue, ob die Zeigerposition auf keine initiale Ziffer zeigt.
        'Dann erhîhe die Ziffer darin bis ein valider Wert erreicht wurde.
        IF sudoInit(zeile, spalte) = 0 THEN
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
                LOOP WHILE sudoInit(zeile, spalte) <> 0
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

