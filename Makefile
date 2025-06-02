clean:
	-rm *.o *.ali b~* build_from_start*

all: build_from_start_bohnenspiel build_from_start_tictactoe build_from_start_kalah

build_from_start_bohnenspiel:
	gnatmake -O3 -gnat2020 -march=native -mtune=native -gnata -gnatp -Wall -Isrc/ -Isrc/bohnenspiel/ build_from_start.adb -o ./build_from_start_bohnenspiel

build_from_start_tictactoe:
	gnatmake -O3 -gnat2020 -march=native -mtune=native -gnata -gnatp -Wall -Isrc/ -Isrc/tictactoe/ build_from_start.adb -o ./build_from_start_tictactoe

build_from_start_kalah:
	gnatmake -O3 -gnat2020 -march=native -mtune=native -gnata -gnatp -Wall -Isrc/ -Isrc/kalah/ build_from_start.adb -o ./build_from_start_kalah

build_from_start_kalah3:
	gnatmake -O3 -gnat2020 -march=native -mtune=native -gnata -gnatp -Wall -Isrc/ -Isrc/kalah3/ build_from_start.adb -o ./build_from_start_kalah3
