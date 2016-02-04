# TODO

* Ressurect the jmeter testing or schlep something together with basho_bench.
* Better documentation
* More testing around cluster backups, resizing
* Test peer cert validation for erlang clustering
* Tests (unit, functional, quickcheck)
* More thorough type checks
* Bind TLS to specific interfaces if that's possible
* Security administrator role SELinux role for logfile administration (to allow deleting/archiving logs) and backups.
* stats HTTP endpoint ? SNMP?
* Figure out how disk_log behaves when the disk is full. My guess is the sync() will pause, which will cause the logging calls to timeout. As these are synchronous, it will cascade back to the client. (all client calls will slow down until timeout hits). Log data loss will probably happen at this point.
