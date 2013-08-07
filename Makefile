all:
	cask exec ert-runner run -l test/espuds-test.el

docs:
	cask exec ./bin/docs
