Cinched SELinux
===============

Linux SELinux targetting policy for the Cinched encryption service.

* cinched: Service policy files.
* localuser: Policy to apply policy to the local system..

Security model
==============

Introduction
------------

The security model rests upon the application domain having exclusive access to data files. Indirect access is granted to the security context to specific applications which transition.

Labelling of paths is controlled by the policy file context.

Types
-----

There are types used:

* ts_t: This is the most restrictive type. It can only be accessed via the ts_cli_t domain (which has read/write access)
* ts_misc_t: This is a type that can be used on folders that will auto-label files within it to the ts_t type upon creation
* ts_tmp_t: temp files
* ts_cli_t: This is the domain which can gain full access to files in ts_misc_t, ts_misc_tmp_t, and ts_t
* ts_exec_t: This is the entrypoint to the CLI security domain. All code related to the application is in this domain.
* ts_log_t: Application log files. Unreadable by anyone for now.
* ts_audit_log_t: Audit logs, can be viewed with the admin command. (no list feature yet)
* ts_var_run_t: Run file
* ts_lock_t: Lock file
* ts_etc_t: Configuration files

Interfaces
----------

* ts_role($role, $domain): Grants ability to execute binaries marked with ts_exec_t
* ts_sysadmin($role): Grants ability to relabel some files into the secure domain.
* ts_admin($role): Grants ability to relabel files into or out from all domains.


TODO
====

* Fork node_package, and setup post-install to apply rules


REFERENCE
=========

http://www.nsa.gov/SeLinux/papers/policy2.pdf

http://selinuxproject.org/page/NB_RefPolicy

http://www.billauer.co.il/selinux-policy-module-howto.html

http://blog-mlessard.rhcloud.com/?p=148

http://blog.siphos.be/2014/01/private-key-handling-and-selinux-protection/

http://blog.siphos.be/2015/07/restricting-even-root-access-to-a-folder/

http://selinuxproject.org/page/RefpolicyWriteModule

http://equivocation.org/node/42

http://blog.siphos.be/2013/05/a-selinux-policy-for-incron-our-first-interface/
