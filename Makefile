ssax_sxml_path = ~/projects/scheme/
mparser_path = Mparser-r3

all:
	echo "(define ssax-sxml-path \"$(ssax_sxml_path)\")" > ssax-sxml-path.scm
	echo "(define mparser-path \"$(mparser_path)\")" > mparser-path.scm
	echo "(include \"$(mparser_path)/streams#.scm\")" >> mparser-path.scm
	echo "(include \"$(mparser_path)/language#.scm\")" >> mparser-path.scm
	echo "(include \"$(mparser_path)/monad#.scm\")" >> mparser-path.scm
	echo "(include \"$(mparser_path)/kernel#.scm\")" >> mparser-path.scm
	echo "(include \"$(mparser_path)/extras#.scm\")" >> mparser-path.scm

clean:
	rm ssax-sxml-path.scm mparser-path.scm
