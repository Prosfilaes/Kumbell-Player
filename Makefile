clean:
	-rm *.o *.ali b~*

build_from_start_bohnenspiel:
	gnatmake -O3 -gnat2020 -march=native -gnata -gnatp -Wall -Isrc/ -Isrc/bohnenspiel/ build_from_start.adb -o ./build_from_start_bohnenspiel

build_from_start_tictactoe:
	gnatmake -O3 -gnat2020 -march=native -gnata -gnatp -Wall -Isrc/ -Isrc/tictactoe/ build_from_start.adb -o ./build_from_start_tictactoe
