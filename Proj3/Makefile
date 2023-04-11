#file = miSudoku
#file = olympiad
#file = roads
file = tsp

$(file): $(file).pl
	swipl -q -O -g main --stand_alone=true -o $(file) -c $(file).pl
