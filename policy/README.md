# Cinched SELinux Policy

The goal of this policy is to create a restrictive environment for running the Cinched service.

The goal of the policy is:

* Remove write access to the library files and binaries used by Cinched (/usr/lib64/cinched, /usr/sbin/cinched)
* Restrict read/write access to Cinched's library and data files (/var/{lib,log}/cinched, /usr/sbin/cinched, /usr/lib64/cinched) to to a single security context, with a single entry point (/usr/sbin/cinched).

I have purposefully tried to limit the number of reference policy interfaces used in this policy to limit the possibility of accidentally granting more access to the system than what is necessary.

## Interfaces

* cinched_operator($role, $domain): Grants ability to execute binaries marked with ts_exec_t
* cinched_sysadmin($role): Grants ability to relabel some files into the secure domain.
* cinched_admin($role): Grants ability to relabel files into or out from all domains.

## Role mapping

I'm consuming the interfaces in the localuser policy.

Currently, only setfiles_t consumes the cinched_sysadmin inteface, and sysadm_t is consumes the cinched_operator interface.

No current mappings to the cinched_admin interface.