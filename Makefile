all:
	@raco scribble --htmls +m --redirect-main http://docs.racket-lang.org/ docs.scrbl
	@cp -R ./research ./docs
