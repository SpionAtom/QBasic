'Sudoku f�r QBasic
'von SpionAtom Dezember 2o24
'
'Anleitung:
'   Pfeiltasten = Cursor bewegen
'           1-9 = Ziffer eintragen
'             0 = Ziffer l�schen
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
TYPE SudokuFeld
	wert AS INTEGER
	init AS INTEGER
	kandidatenAnzahl AS INTEGER
	gueltig AS INTEGER
	notiz AS STRING * 9
END TYPE
DECLARE SUB ErmittleKandidaten (zeile%, spalte%)
DECLARE SUB Spiel ()
DECLARE FUNCTION setzeZiffer$ (quelle$, Ziffer, wert)
DECLARE SUB initEinheiten ()
DECLARE SUB AusgabeSichtbarkeit (zeile, spalte)
DECLARE SUB TrageZifferEin (zeile, spalte, Ziffer)
DECLARE SUB LoesungsStrategie (n)
DECLARE SUB ErmittleSichtbarkeit (zeile, spalte)
DECLARE FUNCTION SudokuGewonnen ()
DECLARE SUB PrintCursor (c AS Koord)
DECLARE SUB PrintSudoku ()
DECLARE SUB LadeSudoku (n)
DECLARE FUNCTION ermittleHaus (zeile, spalte)


DIM SHARED sudo(0 TO 8, 0 TO 8) AS SudokuFeld
DIM SHARED kandidaten(0 TO 8, 0 TO 8, 1 TO 9), kandidatenAnzahl

'Einheit ist ein 3-Dimensionales Array, welches die Positionen aller Zeilen,
'Spalten und H�user in sich tr�gt.
'Jede Zeile/Spalte/Haus ist dabei durchnummeriert (0-8), und hat 9
'Positionen (0-8).
'Einheit(0, 2, 8) h�lt die letzte Position (8), der dritten (2) Zeile (0).
'Einheit(2, 5, 6) h�lt die siebte Position (6), des sechsten (5) Hauses (2).
'Die Hauspositionen sind wie folgt: 012
'                                   345
'                                   678
'
'All dieser Aufwand wird betrieben, um am Ende einheitlicher �ber diese
'Einheiten iterieren zu k�nnen.

	CONST eZEILE = 0, eSPALTE = 1, eHAUS = 2
	DIM SHARED Einheit(0 TO 2, 0 TO 8, 0 TO 8) AS Koord
	'                    |       |       |
	'                   Typ      |     Position
	'                          Nummer

	initEinheiten


DIM SHARED sichtbarText$, nichtsichtbarText$
SCREEN 0, , 1, 0

	'Hauptmen�
	DO
		taste$ = INKEY$

		CLS
		PRINT "Sudoku f�r QBasic!"
		PRINT "(von SpionAtom - begonnen Dezember 2o24)"
		PRINT
		PRINT "Sudoku ist eine Gattung von Logikr�tseln, die aus den lateinischen Quadraten"
		PRINT "entstand. In der �blichen Version ist es das Ziel, ein 9x9-Gitter mit den"
		PRINT "Ziffern 1 bis 9 so zu f�llen, dass jede Ziffer in jeder Einheit (Spalte, Zeile,"
		PRINT "Block = 3x3-Unterquadrat) genau einmal vorkommt - und in jedem der 81 Felder"
		PRINT "exakt eine Ziffer vorkommt. Ausgangspunkt ist ein Gitter, in dem bereits"
		PRINT "mehrere Ziffern vorgegeben sind. [Wikipedia]"
		PRINT
		PRINT "Mit den Pfeiltasten kann man durch das Gitter navigieren."
		PRINT "Zahlen k�nnen �ber die entsprechenden Zahl-Tasten eingegeben werden."
		PRINT "Tippt man eine 0 ein, so l�scht man die Zahl aus dem Feld."
		PRINT "Zu beachten ist, dass die Starteintr�ge schwarz gekennzeichnet sind und nicht"
		PRINT "gel�scht werden k�nnen."
		PRINT "Mit Esc geht es zur�ck ins Hauptmen�"
		PRINT
		PRINT "Zum Starten w�hlen Sie ein Sudoku aus der Liste:"
		PRINT "1) Easy 1    4) Mittel 1  7) -         0) Datei (rudiment�r, no errorhandling)"
		PRINT "2) Easy 2    5) Mittel 2  8) - "
		PRINT "3) Easy 3    6) Mittel 3  9) - "

		SELECT CASE taste$
			CASE "1", "2", "3", "4", "5", "6":
				LadeSudoku VAL(taste$)
				Spiel
			CASE "0"
				LadeSudoku 0
				Spiel
		END SELECT

		PCOPY 1, 0



	LOOP UNTIL taste$ = CHR$(27)
	END


LadeSudoku 6



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

S4:
DATA 0, 9, 6, 0, 0, 0, 0, 0, 8
DATA 0, 0, 0, 3, 0, 0, 0, 4, 0
DATA 0, 0, 0, 2, 4, 0, 0, 0, 0
DATA 0, 0, 1, 0, 0, 0, 0, 8, 9
DATA 0, 0, 0, 8, 0, 3, 7, 0, 0
DATA 0, 0, 7, 5, 9, 0, 0, 0, 0
DATA 0, 0, 2, 4, 5, 1, 0, 0, 0
DATA 1, 0, 5, 0, 0, 6, 9, 0, 0
DATA 3, 7, 0, 9, 8, 0, 1, 6, 0

S5:
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

SUB AusgabeSichtbarkeit (zeile, spalte)

	sichtbarText$ = ""
	nichtsichtbarText$ = ""
	sichtbarAnzahl = 0
	nichtsichtbarAnzahl = 0
	FOR n = 1 TO 9
		IF kandidaten(zeile, spalte, n) = 0 THEN
			IF sichtbarText$ <> "" THEN sichtbarText$ = sichtbarText$ + ","
			sichtbarText$ = sichtbarText$ + STR$(n)
			sichtbarAnzahl = sichtbarAnzahl + 1
		END IF
		IF kandidaten(zeile, spalte, n) = 1 THEN
			IF nichtsichtbarText$ <> "" THEN nichtsichtbarText$ = nichtsichtbarText$ + ","
			nichtsichtbarText$ = nichtsichtbarText$ + STR$(n)
			nichtsichtbarAnzahl = nichtsichtbarAnzahl + 1
		END IF
	NEXT
	sichtbarText$ = "Sichtbar: " + sichtbarText$
	nichtsichtbarText$ = "Nicht sichtbar: " + nichtsichtbarText$


END SUB

'Ermittelt anhand der Koordinaten, in welchem Haus das Feld ist.
'Gibt einen Wert zwischen 0 und 8 zur�ck
FUNCTION ermittleHaus (zeile, spalte)
	ermittleHaus = 3 * INT(zeile / 3) + INT(spalte / 3)
END FUNCTION

SUB ErmittleKandidaten (zeile, spalte)

	'Reset der Kandidaten
	FOR k = 1 TO 9: kandidaten(zeile, spalte, k) = 0: NEXT
	sudo(zeile, spalte).kandidatenAnzahl = 0

	DIM tempPos AS Koord

	
	FOR p = 0 TO 8
		'Sammle alle Ziffern aus der Zeile
		tempPos = Einheit(eZEILE, zeile, p)
		Ziffer = sudo(tempPos.zeile, tempPos.spalte).wert
		IF Ziffer > 0 AND (NOT (tempPos.zeile = zeile AND tempPos.spalte = spalte)) THEN
			kandidaten(zeile, spalte, Ziffer) = 1
		END IF

		'Sammle alle Ziffern aus der Spalte
		tempPos = Einheit(eSPALTE, spalte, p)
		Ziffer = sudo(tempPos.zeile, tempPos.spalte).wert
		IF Ziffer > 0 AND (NOT (tempPos.zeile = zeile AND tempPos.spalte = spalte)) THEN
			kandidaten(zeile, spalte, Ziffer) = 1
		END IF

		'Sammle alle Ziffern aus dem Haus
		haus = ermittleHaus(zeile, spalte)
		tempPos = Einheit(eHAUS, haus, p)
		Ziffer = sudo(tempPos.zeile, tempPos.spalte).wert
		IF Ziffer > 0 AND (NOT (tempPos.zeile = zeile AND tempPos.spalte = spalte)) THEN
			kandidaten(zeile, spalte, Ziffer) = 1
		END IF
	NEXT

	'Alle sichtbaren Ziffern des Feldes wurden gesammelt.
	'Um die Kandidaten zu ermitteln, muss man das Array invertieren,
	'denn nur die nichtsichtbaren sind die Kandidaten.
	FOR n = 1 TO 9
		kandidaten(zeile, spalte, n) = 1 - kandidaten(zeile, spalte, n)
		sudo(zeile, spalte).kandidatenAnzahl = sudo(zeile, spalte).kandidatenAnzahl + kandidaten(zeile, spalte, n)
	NEXT

END SUB

SUB initEinheiten

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

	'H�user initialisieren
	FOR haus = 0 TO 8
		FOR position = 0 TO 8
			tempPos.zeile = 3 * INT(haus / 3) + INT(position / 3)
			tempPos.spalte = 3 * INT(haus MOD 3) + INT(position MOD 3)
			Einheit(eHAUS, haus, position) = tempPos
		NEXT
	NEXT

END SUB

'l�dt ein Sudoku aus dem Data-Bestand
'Erwartet wird eine Nummer, je nach Nummer wird ein anderes Sudoku geladen.
'
SUB LadeSudoku (n)

	SELECT CASE n
		CASE 2:
			RESTORE S2
		CASE 3:
			RESTORE S3
		CASE 4:
			RESTORE S4
		CASE 5:
			RESTORE S5
		CASE 6:
			RESTORE S6
		CASE 0:
			SCREEN 0, , 0, 0
			LOCATE 20, 40: INPUT "Datei: ", datei$
			IF datei$ = "" THEN
				LOCATE 21, 40: PRINT "Fehler beim Laden der Datei. Lade Alternative."
				SLEEP 3
				RESTORE S1
				n = 1
			ELSE
				OPEN datei$ FOR BINARY AS #1: L = LOF(1): CLOSE #1
				IF L = 0 THEN
					KILL datei$
					LOCATE 21, 40: PRINT "Fehler beim Laden der Datei. Lade Alternative."
					SLEEP 3
					RESTORE S1
					n = 1
				END IF
			END IF


		CASE ELSE:
			RESTORE S1
	END SELECT

	'Lade das Sudoku vom Data-Block
	IF n > 0 THEN
		FOR zeile = 0 TO 8
		FOR spalte = 0 TO 8
			READ sudo(zeile, spalte).wert
			IF sudo(zeile, spalte).wert > 0 THEN
				sudo(zeile, spalte).init = 1
				sudo(zeile, spalte).gueltig = 1
			ELSE
				sudo(zeile, spalte).init = 0
				sudo(zeile, spalte).gueltig = 0
			END IF
			sudo(zeile, spalte).notiz = ""
		NEXT
		NEXT
	ELSE
		'Lade von Datei
		OPEN datei$ FOR INPUT AS #1
		FOR zeile = 0 TO 8
			INPUT #1, dateiZeile$
			FOR spalte = 0 TO 8
				sudo(zeile, spalte).wert = VAL(MID$(dateiZeile$, spalte + 1, 1))
				IF sudo(zeile, spalte).wert > 0 THEN
					sudo(zeile, spalte).init = 1
					sudo(zeile, spalte).gueltig = 1
				ELSE
					sudo(zeile, spalte).init = 0
					sudo(zeile, spalte).gueltig = 0
				END IF
				sudo(zeile, spalte).notiz = ""
			NEXT
		NEXT
		CLOSE #1
	END IF

	SCREEN 0, , 1, 0
	CLS
	PrintSudoku
	'Ermittle die initialen Sichtbarkeiten
	FOR zeile = 0 TO 8
	FOR spalte = 0 TO 8
		'ErmittleSichtbarkeit zeile, spalte
		ErmittleKandidaten zeile, spalte
		LOCATE zeile * 2 + 2, spalte * 4 + 1 + 1: PRINT "s";
		PCOPY 1, 0

	NEXT
	NEXT

END SUB

SUB LoesungsStrategie (n)

	SELECT CASE n
		CASE 1: 'Naked Singles
		'Trage �berall dort eine Ziffer ein, bei dem die Nichtsichtbaren
		'Kandidaten auf eine einzige Option begrenzt sind.
			FOR zeile = 0 TO 8
			FOR spalte = 0 TO 8
				LOCATE zeile * 2 + 2, spalte * 4 + 1 + 1: PRINT "N";
				PCOPY 1, 0
				ErmittleKandidaten zeile, spalte
				IF sudo(zeile, spalte).kandidatenAnzahl = 1 THEN
					Ziffer = -1
					FOR z = 1 TO 9
						IF kandidaten(zeile, spalte, z) = 1 THEN Ziffer = z
					NEXT
					IF sudo(zeile, spalte).wert = 0 THEN
						TrageZifferEin zeile, spalte, Ziffer
						LOCATE zeile * 2 + 2, spalte * 4 + 1 + 1: PRINT "*";
						PCOPY 1, 0
					END IF

				END IF
			NEXT
			NEXT
	   
		CASE 2: 'Hidden Single
		'Ist in einer Zeile oder Spalte oder Haus ein Kandidat nur in einer
		'Zelle vorhanden, ist dieses ein Hidden Single und wird eingetragen.
		DIM kandidatenHS(1 TO 9), kandidatenKoords(1 TO 9) AS Koord, kkAnzahl
		kkAnzahl = 0
		DIM k AS Koord

		   
			'Pr�fe Zeilen
			FOR e = 0 TO 8
				FOR n = 1 TO 9: kandidatenHS(n) = 0: NEXT 'reset Kandidatenarray
				FOR posi = 0 TO 8
					k = Einheit(eZEILE, e, posi)
					LOCATE k.zeile * 2 + 2, k.spalte * 4 + 1 + 1: PRINT "Z";
					PCOPY 1, 0
				   
					ErmittleKandidaten k.zeile, k.spalte
					FOR n = 1 TO 9
						IF sudo(k.zeile, k.spalte).wert = 0 THEN
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
						LOCATE k.zeile * 2 + 2, k.spalte * 4 + 1 + 1: PRINT "*";
						PCOPY 1, 0

					END IF
				NEXT
			NEXT

			'Pr�fe Spalten
			FOR e = 0 TO 8
				FOR n = 1 TO 9: kandidatenHS(n) = 0: NEXT 'reset Kandidatenarray
				FOR posi = 0 TO 8
					k = Einheit(eSPALTE, e, posi)
					LOCATE k.zeile * 2 + 2, k.spalte * 4 + 1 + 1: PRINT "S";
					PCOPY 1, 0

					ErmittleKandidaten k.zeile, k.spalte
					FOR n = 1 TO 9
						IF sudo(k.zeile, k.spalte).wert = 0 THEN
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
						LOCATE k.zeile * 2 + 2, k.spalte * 4 + 1 + 1: PRINT "*";
						PCOPY 1, 0

					END IF
				NEXT
			NEXT
		   
			'Pr�fe H�user
			FOR e = 0 TO 8
				FOR n = 1 TO 9: kandidatenHS(n) = 0: NEXT 'reset Kandidatenarray
				FOR posi = 0 TO 8
					k = Einheit(eHAUS, e, posi)
					LOCATE k.zeile * 2 + 2, k.spalte * 4 + 1 + 1: PRINT "H";
					PCOPY 1, 0

					ErmittleKandidaten k.zeile, k.spalte
					FOR n = 1 TO 9
						IF sudo(k.zeile, k.spalte).wert = 0 THEN
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
						LOCATE k.zeile * 2 + 2, k.spalte * 4 + 1 + 1: PRINT "*";
						PCOPY 1, 0

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

		Ziffer = sudo(zeile, spalte).wert
		zifferZeichen$ = LTRIM$(STR$(Ziffer))
		IF sudo(zeile, spalte).init = 0 THEN
			zifferfarbe = 15
			IF sudo(zeile, spalte).gueltig = 0 THEN zifferfarbe = 13
		ELSE
			zifferfarbe = 0
		END IF

		IF Ziffer > 0 THEN
			COLOR zifferfarbe, hausfarbe
			LOCATE zeile * 2 + 1, spalte * 4 + 1: PRINT " " + zifferZeichen$ + " ";
		ELSE
			IF Ziffer = 0 THEN
				IF sudo(zeile, spalte).notiz <> "" THEN
					COLOR 8, hausfarbe
					LOCATE zeile * 2 + 1, spalte * 4 + 1: PRINT "  ";
				END IF
				COLOR 0, hausfarbe 'bei einer null, w�hle schwarz und "zeichne" blo� ein Leerzeichen
				LOCATE zeile * 2 + 1, spalte * 4 + 1: PRINT "   ";
			END IF
		END IF
	NEXT
	NEXT

	'vertikale und horizontale Linien
	COLOR 7, 0
	FOR zeile = 1 TO 17
		LOCATE zeile, 12: PRINT "�";
		LOCATE zeile, 24: PRINT "�";
	NEXT
	LOCATE 6, 1: PRINT STRING$(35, "�");
	LOCATE 12, 1: PRINT STRING$(35, "�");
	LOCATE 6, 12: PRINT "�";
	LOCATE 6, 24: PRINT "�";
	LOCATE 12, 12: PRINT "�";
	LOCATE 12, 24: PRINT "�";
	COLOR 7, 0
END SUB

SUB Spiel
SCREEN 0, , 1, 0
DIM cursor AS Koord
cursor.zeile = 4
cursor.spalte = 4
gewonnen = 0

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
				IF sudo(cursor.zeile, cursor.spalte).init = 0 THEN
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


		'Ausgabe
		CLS
		PrintSudoku
		PrintCursor cursor
		COLOR 7, 0
		AusgabeSichtbarkeit cursor.zeile, cursor.spalte
		LOCATE 19, 1: PRINT sichtbarText$;
		LOCATE 20, 1: PRINT nichtsichtbarText$;
		LOCATE 19, 40: PRINT "G�ltig: "; sudo(cursor.zeile, cursor.spalte).gueltig;
		LOCATE 20, 40: PRINT "Nicht sichtbare: "; sudo(cursor.zeile, cursor.spalte).kandidatenAnzahl;
		IF gewonnen = 1 THEN COLOR 14: LOCATE 22, 1: PRINT "Gewonnen!"
		PCOPY 1, 0
		COLOR 7, 0
	   
	LOOP UNTIL taste$ = CHR$(27)
	WHILE INKEY$ <> "": WEND

END SUB

'Pr�ft, ob das Sudoku gel�st wurde.
'Alle Felder m�ssen volle Sichtbarkeit haben
FUNCTION SudokuGewonnen

	FOR zeile = 0 TO 8
	FOR spalte = 0 TO 8
		IF sudo(zeile, spalte).gueltig = 0 THEN SudokuGewonnen = 0: EXIT FUNCTION
		
	NEXT
	NEXT

	SudokuGewonnen = 1



END FUNCTION

SUB TrageZifferEin (zeile, spalte, Ziffer)
	IF sudo(zeile, spalte).init = 0 THEN
		sudo(zeile, spalte).wert = Ziffer
		'ErmittleSichtbarkeit zeile, spalte
		ErmittleKandidaten zeile, spalte
		IF Ziffer = 0 THEN
			sudo(zeile, spalte).gueltig = 0
		ELSEIF kandidaten(zeile, spalte, Ziffer) = 0 THEN
			sudo(zeile, spalte).gueltig = 0
		ELSE
			sudo(zeile, spalte).gueltig = 1
		END IF
	END IF

END SUB

