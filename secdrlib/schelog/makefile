all: schelog.so bagof.so

schelog.so: schelog.ss
	echo '(compile-file "schelog")' | chez

bagof.so: bagof.ss schelog.so
	echo '(load "schelog.so") (compile-file "bagof")' | chez
