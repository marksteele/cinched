default:
	rebar -C rebar.config.lock get-deps compile
	cd policy && make
dialyzer:
	 dialyzer --plt .dialyzer_plt -c ebin

compile:
	rebar compile
	cd policy && make

release:
	bin/release.es

install: default release
	setenforce 0
	chattr -R -i /usr/sbin/cinched /usr/lib64/cinched /etc/bash_completion.d/cinched_bash_completions || /bin/true
	killall -9 beam.smp beam epmd || true
	mkdir -p /var/lib/cinched /usr/lib64/cinched/policy /etc/cinched /var/log/cinched/audit
	install -p -D -m 0500 bin/cinched /usr/sbin/
	cp -v policy/*.pp /usr/lib64/cinched/policy/
	cp -v bin/cinched_bash_completions /etc/bash_completion.d/
	cp -vR rel/cinched/{lib,bin,erts-*,releases} /usr/lib64/cinched/
	chown -R cinched:cinched /usr/lib64/cinched /etc/cinched /var/{lib,log}/cinched
	chmod -R 0500 /usr/lib64/cinched/*
	chmod -R 0700 /etc/cinched
	chmod -R 0700 /var/{lib,log}/cinched
	semodule -i /usr/lib64/cinched/policy/*.pp
	restorecon -Rv  /usr/sbin/cinched /usr/lib64/cinched /etc/cinched /var/{lib,log}/cinched
	chattr -R +i /usr/sbin/cinched /usr/lib64/cinched /etc/bash_completion.d/cinched_bash_completions
	setenforce 1
