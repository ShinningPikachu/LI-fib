file = basket

$(file): $(file).pl
	swipl -O -q -g main --stand_alone=true -o $(file) -c $(file).pl
