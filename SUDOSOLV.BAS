DEFINT A-Z
DECLARE SUB PrintSudoku ()
DECLARE FUNCTION erlaubt (zeile, spalte, ziffer)
DECLARE FUNCTION loeseSudoku (zeile, spalte)
DIM SHARED sudo(0 TO 8, 0 TO 8)


    'Lade ein Sudoku
    RESTORE S1
    FOR zeile = 0 TO 8
    FOR spalte = 0 TO 8
        READ sudo(zeile, spalte)
    NEXT
    NEXT

    'Ausgabe for dem L�sen
    CLS
    PRINT "Beginn: " + TIME$
    PrintSudoku

    IF loeseSudoku(0, 0) = 1 THEN
        PRINT "L�sung: " + TIME$
        PrintSudoku
    ELSE
        PRINT "Keine L�sung gefunden"
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

'Pr�ft, ob eine Ziffer in der Zelle erlaubt ist
FUNCTION erlaubt (zeile, spalte, ziffer)

    'Pr�fe Zeile
    FOR s = 0 TO 8
        IF sudo(zeile, s) = ziffer THEN
            erlaubt = 0
            EXIT FUNCTION
        END IF
    NEXT

    'Pr�fe Spalte
    FOR z = 0 TO 8
        IF sudo(z, spalte) = ziffer THEN
            erlaubt = 0
            EXIT FUNCTION
        END IF
    NEXT

    'Pr�fe Haus
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

'L�sen mit Backtracking
'nach geeksforgeeks.org/sudoku-backtracking-7/
FUNCTION loeseSudoku (zeile, spalte)

    'Letzte Zeile ausgef�llt
    IF zeile = 8 AND spalte = 9 THEN
        loeseSudoku = 1
        EXIT FUNCTION
    END IF

    'Wenn Spalte aus dem Raster l�uft, dann gehe in die n�chste Zeile.
    IF spalte = 9 THEN
        zeile = zeile + 1
        spalte = 0
    END IF

    'Wenn die Zelle schon gef�llt ist, fahre mit n�chster Zelle fort.
    IF sudo(zeile, spalte) <> 0 THEN
        loeseSudoku = loeseSudoku(zeile, spalte + 1)
        EXIT FUNCTION
    END IF

    FOR ziffer = 1 TO 9
        'Pr�fe, ob Ziffer gesetzt werden darf
        IF erlaubt(zeile, spalte, ziffer) THEN
              'Setze Ziffer
              sudo(zeile, spalte) = ziffer
              'Pr�fe n�chste Zelle
              IF loeseSudoku(zeile, spalte + 1) = 1 THEN
                loeseSudoku = 1
                EXIT FUNCTION
              END IF
        END IF
        'Ziffer war nicht zielf�hrend, Zelle zur�ck auf 0 setzen.
        sudo(zeile, spalte) = 0
    NEXT
    loeseSudoku = 0


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

