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

    'Ausgabe for dem Lîsen
    CLS
    PRINT "Beginn: " + TIME$
    PrintSudoku

    IF loeseSudoku(0, 0) = 1 THEN
        PRINT "Lîsung: " + TIME$
        PrintSudoku
    ELSE
        PRINT "Keine Lîsung gefunden"
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
'nach geeksforgeeks.org/sudoku-backtracking-7/
FUNCTION loeseSudoku (zeile, spalte)

    'Letzte Zeile ausgefÅllt
    IF zeile = 8 AND spalte = 9 THEN
        loeseSudoku = 1
        EXIT FUNCTION
    END IF

    'Wenn Spalte aus dem Raster lÑuft, dann gehe in die nÑchste Zeile.
    IF spalte = 9 THEN
        zeile = zeile + 1
        spalte = 0
    END IF

    'Wenn die Zelle schon gefÅllt ist, fahre mit nÑchster Zelle fort.
    IF sudo(zeile, spalte) <> 0 THEN
        loeseSudoku = loeseSudoku(zeile, spalte + 1)
        EXIT FUNCTION
    END IF

    FOR ziffer = 1 TO 9
        'PrÅfe, ob Ziffer gesetzt werden darf
        IF erlaubt(zeile, spalte, ziffer) THEN
              'Setze Ziffer
              sudo(zeile, spalte) = ziffer
              'PrÅfe nÑchste Zelle
              IF loeseSudoku(zeile, spalte + 1) = 1 THEN
                loeseSudoku = 1
                EXIT FUNCTION
              END IF
        END IF
        'Ziffer war nicht zielfÅhrend, Zelle zurÅck auf 0 setzen.
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

