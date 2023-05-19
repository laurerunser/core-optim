all:
	# -shell escape : nécessaire pour minted
	pdflatex -shell-escape slide.tex
	# on compile une deuxième fois pour les références
	pdflatex -shell-escape slide.tex

clean:
	rm -rf *bak *~ *out *log *aux _minted-slide *.aex
