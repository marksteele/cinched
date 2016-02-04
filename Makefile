default:
	rebar -C rebar.config.lock get-deps compile
	cd policy && make
dialyzer:
	 dialyzer --plt .dialyzer_plt -c ebin

compile:
	rebar compile
	cd policy && make
