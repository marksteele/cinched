# Introduction

Cinched is a cryptographic service designed to help facilitate the development of applications which need to implement encryption of data at rest.

Why use a microservice for encryption?

* It is simpler to audit the correctness of the encryption implementation if it is centralized.
* A developer friendly API
* Backend storage agnostic
* Centralized secret key management
* Cinched is a clustered service with no single point of failure
* Cinched uses a highly available storage backend with automatic data replication.

## Security Model Overview

### Confidentiality

* The client API uses TLS encryption with both client and server certificates.
* Intra-cluster traffic is encrypted using TLS
* Data is encrypted via randomly generated keys using strong encryption algorithms.
* Only the service can decrypt the data when provided with the data key and encrypted payload.
* All data encryption keys at rest are stored encrypted.
* Only designated user groups (based on SELinux roles) can manage the service.
* Service files (library code, logs, data files) can only be accessed through a single security context, which has a well defined entry point. No users have direct filesystem access to any of the service files or write access to any of the executables.
* The policy defends against attempts to read keys from RAM (denying the ptrace() syscall, preventing the dynamic loading of kernel modules to dump RAM, executable mem/stack/mod/heap).
* Once enabled, the SELinux policy can be made immutable.
* Audit logs are encrypted using rotating encryption keys.

### Integrity

* All encryption operations performed on data and keys use authenticated encryption. Any attempts to alter the encrypted payloads will result decryption failures.
* The storage subsystem used in Cinched can detect and repair silent on-disk corruption via read-repair.
* Audit logs are tamper-resistant and can be verified via a hash chain. Any attempts to alter the logs can be immediately detected.
* In order to ensure consistency in a clustered environment, writes to the storage subsystem employ a multi-paxos consensus algorithm to ensure agreement of cluster state.

### Availability

* CPU intensive processes are bounded to a configurable number to partially mitigate denial of service conditions.
* Encryption keys stored in the Cinched cluster are replicated to a configurable number of nodes.
* Replication groups are spread across all nodes, and can tolerate the failure of nodes based on a quorum of peer replicas remaining online per replication group.

### Known attack vectors

This list is a first stab at identifying attack vectors which are not in the current scope or implementation. If anyone sees something I've missed, please get in touch with me.

#### Erlang clustering

This implementation assumes TLS Erlang distribution using unencrypted x509 private keys. The private keys are protected via SELinux policy.

Should an attacker manage to gain a trusted x509 certificate/key and the configured Erlang cookie (also protected by SELinux policy), he/she would be able to join a rogue node to the Erlang cluster assuming the attacker can run the attack code from the same subnet.

Once joined to the cluster, subverting all the other controls is relatively easy.

I believe the proper long-term solution to this would be to implement an attestation protocol that would run prior to erlang distribution being setup between nodes. The only way I an see this working in a provably secure way is to leverage TPM hardware and kernel integrity checking hooks.

#### Denial of service

There is limited defense against DoS attacks in the form of limiting the number of CPU intensive processes. Exceeding configured capacity will lead to clients waiting on available slots. Eventually the service will experience resource exhaustion when all file descriptors used for socket handling are used up.

It is possible for a root user to kill the service process without being a member of the correct SELinux context.

As all operations are logged, it would be possible for an authenticated client to exhaust disk space by crafting bogus requests. This will result in a DoS condition.

#### Stealing keys from swap

I've encountered several issues when attempting to lock memory inside Cinched and have unfortunately had to disable memory locking.

This means that should there be memory pressure, it's possible for data held in RAM by Cinched to be paged to swap, and thus be vulnerable to a variety of attacks.

### Suggested additional security measures.

* Disable swap (swapoff).
* Run the Cinched cluster in it's own subnet.
* Use a firewall between the service consumers and the cluster nodes.
* Enable a password on grub as well as set a BIOS boot password if possible. This will prevent an unattended reboot (which would be required to change SELinux policy).

# Technical details

## Storage

* Cinched splits up the key namespace into partitions which are mapped to a [consistent hash ring](https://en.wikipedia.org/wiki/Consistent_hashing). Each partition in the space is then mapped to nodes based on the desired replication factor. These groups of nodes form the consensus/replication group for that partition.
* Data for each partition is stored in a [LevelDB](http://leveldb.org/) database.
* Data is replicated to each node in the consensus group using the [multi-paxos](https://en.wikipedia.org/wiki/Paxos_\(computer_science\)) consensus algorithm.
* Data is encrypted prior to being stored/replicated.

## Audit Logging

All operations in Cinched are logged to an audit log. The following items are logged:

* Connection metadata
    * Peer IP/Port
    * Peer certificate information (CN, Serial)
* Operation
    * API endpoint (`/doc`,`/blob`)
    * Type (`encrypt`,`decrypt`)
* Administrative commands
    * User performing the action
    * Check/View/Rotate logs
* Application provided request metadata

Log entries are made tamper resistant by creating a [hash chain](https://en.wikipedia.org/wiki/Hash_chain). Any attempt to alter log entries can be identified immediately.

Individual log entries are encrypted using a rotating encryption key, ensuring that should the other protections be compromised, log contents are still confidential, and their integrity can be ensured.

## Discretionary and Mandatory Access Controls (DAC/MAC)

Cinched uses both DAC (unix permissions) and MAC (type enforcement via SELinux) to protect the confidentiality, integrity, and availability of the service.

### DAC

Cinched must be started as the root user in order to be able to setup an appropriate runtime environment. During the service startup, Cinched:

* Sets the system resource limit to ensure sufficient file descriptors are available.
* Disables kernel core dumps
* Resets all filesystem file/folder permissions and ownership to the configured Cinched user
* Drops privileges to the `cinched` user

### MAC

#### SElinux booleans

The following SELinux booleans are set during initial node setup using the `cinched setup` command:

* ssh_sysadmin_login: on
* deny_ptrace: on
* deny_execmem: on
* secure_mode_insmod: on (optional)
* selinuxuser_execstack: off
* selinuxuser_execmod: off
* selinuxuser_execheap: off
* secure_mode_policyload: on (optional)

#### MAC/Type Enforcement/SELinux policy

Cinched policy is stored in `/usr/lib64/cinched/policy`.

The policy relies on SELinux reference policy and implements a restrictive security domain. This policy should restrict operators from being able to directly access all data and library files used by the service.

There are three roles:

* Operators (can interact with the service)
* Admins (can label files into the security context)
* Security admins (have full access to the security context).

The policy currently only performs an association for the operators (no admins/secadmins). It's on the TODO to create the additional mappings.

## Encryption and message authentication algorithms

This service uses authenticated symmetrical encryption implemented via libsodium. The alogorithms used are Salsa20 for encryption/decryption, and Poly1305 for message authentication.

## Keys

All encryption keys used in Cinched are generated using libsodium's `randombytes_buf` function, which in turn will leverage the Linux kernel's `getrandom()` syscall. To ensure adequate amounts of entropy are always available, it is recommended that the [Haveged](http://www.issihosts.com/haveged/) daemon be installed on all nodes participating in the cluster.

The following sections describes the various types of keys that are part of Cinched.

### Operator key (OK)

This key is generated during the initial cluster setup in response to an operator running the `cinched init-cluster` command. The key is then split into several shards using [Shamir secret sharing](https://en.wikipedia.org/wiki/Shamir%27s_Secret_Sharing) and the shards are output to the operator terminal.

By default, 10 shards are created, with a threshold of three shards required to reconstruct the original key (therefore no more than 2 shards should be distributed to each shard custodians to ensure that more than one operator is required for node start-up).

This key is held in memory only during the initial cluster setup while encrypting the service key and node startup phases, to decrypt the service key. It is *never* persisted to disk.

Rotating this key is possible using the `cinched rotate-operator-key` command.

### Service key (SK)

This key is generated during the initial cluster setup in response to an operator running the `cinched init-cluster` command.

It is encrypted with the `OK` and persisted to the storage subsystem.

During cluster start-up, the encrypted key is loaded and decrypted with the `OK`. Once loaded, it is held in memory and used to encrypt and decrypt Master Keys.

### Master key (MK)

A new Master Key is automatically generated once per day on-demand during Data Key generation. It is encrypted with the `SK` and persisted in the storage system.

Master Keys are used to encrypt the Data Keys generated during the course of the day they are created, and decrypt the Data Keys thereafter.

Master Keys are associated with an integer number representing the number of days since the begining of the Unix Epoch (Jan 1, 1970).

### Data key (DK)

Data keys are generated on demand either through the key API (`/key/data-key`) or when encryption API endpoints are called without the `x-cinched-data-key` HTTP header provided.

The keys are encrypted with the `MK` for the date at which they are generated and returned to calling clients who are responsible for storing them.

It is recommended that the Data Keys are stored separately from the data they are protecting, for example in a different database server with different access credentials.

To make key management easier for calling applications, a `crypto-period` identifier is also returned to calling applications. This identifier can be stored and used to quickly find keys which have aged beyond application requirements.

Data keys are *not* persisted in Cinched.

## Transport Security

The network transport uses TLSv1.2 and requires client and server x509 certificates.

The list of allowed ciphers, in order of preference:

* ECDHE-ECDSA-AES256-SHA384
* ECDHE-RSA-AES256-SHA384
* ECDH-ECDSA-AES256-SHA384
* ECDH-RSA-AES256-SHA384
* DHE-RSA-AES256-SHA256
* DHE-DSS-AES256-SHA256
* ECDHE-ECDSA-AES128-SHA256
* ECDHE-RSA-AES128-SHA256
* ECDH-ECDSA-AES128-SHA256
* ECDH-RSA-AES128-SHA256
* DHE-RSA-AES128-SHA256
* DHE-DSS-AES128-SHA256

Some important things to keep in mind:

* Server and client certificates must be issued from the same certificate authority.
* If using DNS for load balancing, make sure you setup Subject Alternative Names when creating your x509 certificates that include each node in your cluster, otherwise some client libraries might get confused and refuse to connect. An alternative is to use a wildcard for the cluster (eg: *.cinched.mydomain.com)
* Cinched will validate certificates for both client calls as well as when establishing intra-cluster communication.
    * Checks CRLs only for erlang distribution
    * Checks only OCSP for client calls, and therefore requires a valid OCSP URL in the Authority Information Access field of the x509 certificates. OCSP check results are cached for a configurable amount of time (defaults to 60s).

# Operations guide

## Planning

A single node cluster is fine for testing and prototyping, however Cinched is designed to work in a cluster of servers. I strongly recommend you setup a minimum of five nodes, with a replication factor of 3.

At this time Cinched does not support dynamic cluster resizing or dynamic node membership.

It is possible to resize the cluster, however this cannot be done online.

Cinched shouldn't be terribly memory hungry, however I would expect that it will be very CPU hungry. It will use disk space for logging. After I run some benchmarks I'll add guidance on how much disk is used per log entry.

I have not performed much in the way of performance testing or tuning at this point. It's on the TODO list.

## Installation

### Setup the repo

See instructions [here](http://www.control-alt-del.org/repo) for setting up the repository for libocsp and cinched.

### Install packages

Cinched requires libsodium, which can be installed from the EPEL repo:

```
yum -y install epel-release
```

Next we install the Cinched package, which will pull it's dependancies (libsodium, haveged, libocsp):

```
yum -y install cinched
```

Repeat this process for each node that will be part of the cluster before proceeding.

### Preparing the kernel

One of the built-in protections offered during the setup wizard is to lock down dynamic loading of kernel modules.

It is recommended that this be enabled, however note that most users are running kernels that load modules during system initialization or at runtime for things like NIC drivers and so on.

Enabling the kernel module lock-down from the wizard **will break your system** unless you've built a monolithic kernel that includes all the modules you need.

The down-side of not locking down kernel module loading is that it's trivial to write a kernel module that can dump the contents of memory (thus allowing an attacker to get a copy of the encryption keys from RAM).

So if you're comfortable with compiling a custom kernel, stop here. Recompile the kernel on your nodes with all the modules you need built-in, then come back here.

If you don't know how to do this, select 'N' when prompted to lock down kernel module loading.

I've include crash-course like instructions at the end of this document for those who want to experiment.

### Initial setup

Once the software is installed, it must be configured.

Given the sensitive nature of this service, it's configuration cannot be changed once it has been set without bringing down the cluster. Furthermore, some of the configuration parameters cannot be changed after initial configuration without re-initializing the state, which would result in losing all stored keys.

The list of possible configuration parameters is below. Please ensure you have documented all values you wish to use prior to continuing with the setup process.

| Item | Description | Default value |
|------|-------------|---------------|
| Certificate | This is the path to a PEM encoded x509 certificate that will be used for TLS encryption between cluster node peers as well as consumers of the API. You'll need a copy of this certificate on each server in the cluster. The setup wizard will prompt for filesystem location of the certificate and copy it. | none |
| Certificate private key | This is the path to the PEM encoded x509 certificate private key. You'll need a copy of this key on each server in the cluster. The setup wizard will prompt for the filesystem location of the certificate private key and copy it. Note: The private key should not be encrypted. After the private key is copied by the wizard it should be removed from the nodes. | none |
| CA certificate | This is the path to the PEM encoded CA certificate that issued the service certificate. | none |
| Erlang cookie | A unique string configured in an Erlang cluster. All nodes in the cluster must use the same value. The setup wizard will offer to generate a random cookie value, or will prompt to input a value | none |
| Key cache TTL | The number of seconds a master key is cached in memory | 86400 |
| Key cache size | The size in bytes of the master key cache | 1048576 |
| Crypto worker processes | The number of parallel processes each server will spawn to process cryptographic operations | 50 |
| Partitions | The number of data partitions, or ring size. (should be 16-1024) | 64 |
| Key shards | The number of shards to generate when creating the operator key | 10 |
| Key shard recovery | The number of key shards required to reconstruct the operator key | 3 |
| OCSP TTL | The OCSP cache time-to-live in seconds | 120 |
| OCSP cache size | The size of the OCSP cache in bytes | 1048576 |
| Replication factor | The number of replicas to store for each master key | 3 |
| IP address | The IP address that will be used to bind the service to | 192.168.101.1 |
| REST API port | The port to use for the REST API | 55443 |
| Backup port | The port to use for Cinched backup traffic | 55442 |
| Cluster node list | A comma separated list of hosts in this cluster | 192.168.101.1,192.168.101.2,192.168.101.5 |

Configuration values should be identical on all nodes (except for the IP address).

After you've noted the desired configuration settings as well as copied the x509 certificates to all the nodes, it's time to run the setup wizard.

The wizard will generate the configuration file as well as setup SELinux policy. Before running it, you must ensure that you are running in permissive SELinux mode.

```
[root@dev2 ~]# setenforce 0
```

Next, running the wizard:

```
[root@dev2 ~]# cinched setup

Welcome to the cinched setup wizard. To complete this wizard you will need:

 * The path for the x509 certificate to be used for the service
 * The path for the x509 certficate key (unencrypted) to be used for the service
 * The path for the x509 CA certificate
 * The IP addresses of all hosts in this cluster
 * The port numbers cinched can use

Step 1. Set the Erlang cookie


Are you ready to continue? (Y/N)y
Checking for Erlang cookie file
Cookie not set. Do you want to generate a random cookie? (Y/N) : n
Would you like to set the value of the cookie now? (Y/N) : y
Enter cookie value: KatSKcGso5XcbxqEiVAA6F0YrCxPXhcO
Press any key to continue
Step 2. Setup the x509 certificates


Checking x509 certificate
x509 server certificate not found, please enter the path to the certificate: /root/star.control-alt-del.org.crt

/var/lib/cinched/cert./root/star.control-alt-del./var/lib/cinched/cert.
Success
Checking x509 certificate key
x509 server certificate key not found, please enter the path to the certificate key: /root/star.control-alt-del.org.key

/var/lib/cinched/key./root/star.control-alt-del./var/lib/cinched/key.
Success

It is strongly recommended to remove the unencrypted private key from the
location where it has been copied from.

Make sure you have a backup if you chose to let the setup wizard delete it.


Would you like to remove the unencrypted key now? (Y/N) : n
Checking CA certificate
x509 certificate authoritity file not found, please enter the path to the CA file: /root/cacert.pem

Success
Press any key to continue
Step 3. SELinux configuration


SELINUX=enforcing
SELinux is enforcing, good!
Checking SELinux enforcement type
SELINUXTYPE=targeted
SELinux type is targeted, good!
Checking for operators group
Group already exists
Associating users in operators group with the sysadm_u SELinux user
Already associated
Allowing sysadmn role to login via SSH
Denying ptrace to all processes (will break things like strace)
Denying the ability to mark memory as both writeable and executable
Do you want to disable loading of kernel modules?

*****WARNING*****

If you are using a stock OS kernel, there is a good chance that the system is dynamically loading kernel modules during the boot sequence (eg: loading iptables, NIC drivers, etc...), enabling this will break your system if this is the case.

The downside of *NOT* locking down the dynamic loading of kernel modules is that should the system become compromised, an attacker and load a kernel module that can dump the contents of RAM, which would expose the keys used for encryption.

It is therefore recommended to compile a kernel that contains all modules that your system uses compiled into a monolithic kernel.

To do so, control-c to exit this wizard, go compile your kernel, then re-run the setup.



Lock down kernel module insmod? (Y/N): y
Kernel module loading locked down
Disabling selinuxuser_execstack,selinuxuser_execmod,selinuxuser_execheap
Loading policy
CINCHED policy loaded
LOCALUSER policy loaded
Press any key to continue
Step 4. Settings
Key cache time-to-live in seconds: 86400
Key cache size in bytes: 1048576
Crypto worker processes: 50
Number of partitions (should be 16-1024): 64
Number of operator key shards: 10
Number of key shards required to reconstruct operator key: 3
OCSP cache time-to-live in seconds: 120
OCSP cache size in bytes: 1048576
Replication factor: 3
IP address to bind to: 192.168.101.2
Port to use for cinched service: 55443
Port to use for cinched backup traffic: 55442
Comma separated list of hosts in this cluster: 192.168.101.1,192.168.101.2,192.168.101.7
Restoring SELinux file labels
Setting filesystem attributes
Setting SELinux to enforcing
Press any key to continue
Step 5. Policy lock-down.

***** WARNING *****

If the policy is locked down, it will require the root user to edit the SELinux configuration and restart the server to effect a policy change.

It is recommended that the policy be locked down, which will prevent
a malicious user from changing runtime policy. Coupled with a boot-time password (eg: in the BIOS) and a bootloader password (grub), the system will not boot unattended, thus ensuring that if an attacker can change the SELinux configuration and attempt to reboot, the system will halt during the boot cycle, which should alert operators to examine the system for compromise.

For testing purposes, you should probably choose N. For production, you should probably choose Y.

Lock down policy? (Y/N): y
Policy locked down
Setup complete
```

### User setup

Cinched requires that users who have access to manage the service be in the operators group, which maps to a specific SELinux user (sysadm).

The setup wizard will create the operators group if it doesn't exist, and do the SELinux mapping.

Let's create a user:

```
useradd mark
usermod -G operators mark
passwd mark
```

Next, login as the user you just created, and `su` to root. To verify that everything went according to plan, you should see the following when running the `id -Z` command:

```
[root@dev1 ~]# id -Z
sysadm_u:sysadm_r:sysadm_t:s0-s0:c0.c1023
[root@dev1 ~]#
```

At this point, the installation is finished. The next steps are cluster initialization.

## Node/Cluster initialization

### First-time startup

The first time the cluster is started, it requires the operator to explicitly initialize the cluster. This will generate a set of operator key shards that need to be shared amongst shard custodians and must be input in order for a node to be able to decrypt the service key `SK`.

First, on all nodes fire up the service:

```
cinched start
```

Next, we need to wait for the storage subsystem to stabilize and reach steady state. To check the status of the cluster, use the following command:

```
cinched status
```

Possible status values:

| Status | Description |
|--------|-------------|
| waiting_ensemble | The cluster storage engine is initializing |
| waiting_init | The cluster storage engine is ready, however Cinched has not been initialized |
| waiting_shards| The cluster is initialized, the node is ready to receive the key shards to decrypt the stored service key |
| started | The service is initialized and running |

Once the status transitions from `waiting_ensemble` to `waiting_init`, it's time to do the one-time initialization of the cluster.

This will:

* generate a random service key `SK`
* generate a random operator key `OK`
* encrypt the `SK` with the `OK`, and store the encrypted key in the storage subsystem
* split the `OK` into a set of shards, and output the shards to the terminal.

The command to run the initialization is `cinched init-cluster`:

```
[root@dev1 cinched]# cinched status
Status: waiting_init
[root@dev1 cinched]# cinched init-cluster
g2gEZAAFc2hhcmVhAWEBbQAAACDPy3VOCF6FxFEgovbeH+T6i4Khot0s2AIbwQT2eiKcrg==
g2gEZAAFc2hhcmVhAWECbQAAACDPy3VOCF6FxFEgovbeH+T6i4Khot0s2AIbwQT2eiKcrg==
g2gEZAAFc2hhcmVhAWEDbQAAACDPy3VOCF6FxFEgovbeH+T6i4Khot0s2AIbwQT2eiKcrg==
g2gEZAAFc2hhcmVhAWEEbQAAACDPy3VOCF6FxFEgovbeH+T6i4Khot0s2AIbwQT2eiKcrg==
g2gEZAAFc2hhcmVhAWEFbQAAACDPy3VOCF6FxFEgovbeH+T6i4Khot0s2AIbwQT2eiKcrg==
g2gEZAAFc2hhcmVhAWEGbQAAACDPy3VOCF6FxFEgovbeH+T6i4Khot0s2AIbwQT2eiKcrg==
g2gEZAAFc2hhcmVhAWEHbQAAACDPy3VOCF6FxFEgovbeH+T6i4Khot0s2AIbwQT2eiKcrg==
g2gEZAAFc2hhcmVhAWEIbQAAACDPy3VOCF6FxFEgovbeH+T6i4Khot0s2AIbwQT2eiKcrg==
g2gEZAAFc2hhcmVhAWEJbQAAACDPy3VOCF6FxFEgovbeH+T6i4Khot0s2AIbwQT2eiKcrg==
g2gEZAAFc2hhcmVhAWEKbQAAACDPy3VOCF6FxFEgovbeH+T6i4Khot0s2AIbwQT2eiKcrg==
[root@dev1 cinched]#
```

You'll want to distribute those key shards to a set of custodians, as a number of them (default 3, depending on what you configured) will be required to startup node instances.

For example, if you used the default settings (10 shards, with a threshold of 3 to reconstruct), you should distribute 2 shards to 5 custodians. This ensures that 2 people are involved whenever a node gets restarted. This is to prevent a single rogue operator from having sufficient information for decrypting the `SK`.

After the cluster initialization has completed, the initial node will be up and running. We can now follow the normal startup procedure on the remaining nodes.

### Normal startup

On a cluster that is initialized, nodes will come up in a state of `waiting_shards`.

To provide the shards, use the `cinched send-key-shard`. Remember that in order to interact with cinched, your custodians need to have logged into to the node as users in the operators group su'ed to root.

Make sure the service is in the correct state:

```
[root@dev2 ~]# cinched status
Status: waiting_shards
```

Have custodians enter key shards.

Custodian #1:

```
[root@dev2 ~]# cinched send-key-shard
Key Shard:
Need more shards.
[root@dev2 ~]# cinched send-key-shard
Key Shard:
Need more shards.
```

Custodian #2:

```
[root@dev2 ~]# cinched send-key-shard
Key Shard:
Key recovered from shards
```

Once the threshold is met, the key will be reconstructed and the service will come online.

## Operations

### Service stats

Cinched tracks a variety of statistics which can be viewed with the `cinched stats` command:
```
cinched stats
"cache.key.hit": 0
"cache.key.miss": 0
"cache.key.put": 0
"cache.ocsp.hit": 0
"cache.ocsp.miss": 0
"cache.ocsp.put": 0
"cowboy.active_connections": 0
"crypto_worker.decrypt.error": 0
"crypto_worker.decrypt.ok": 12
"crypto_worker.decrypt.time.n": 5
"crypto_worker.decrypt.time.mean": 204
"crypto_worker.decrypt.time.min": 150
"crypto_worker.decrypt.time.max": 241
"crypto_worker.decrypt.time.median": 223
"crypto_worker.decrypt.time.50": 223
"crypto_worker.decrypt.time.75": 241
"crypto_worker.decrypt.time.90": 241
"crypto_worker.decrypt.time.95": 241
"crypto_worker.decrypt.time.99": 241
"crypto_worker.decrypt.time.999": 241
"crypto_worker.eek.time.n": 0
"crypto_worker.eek.time.mean": 0
"crypto_worker.eek.time.min": 0
"crypto_worker.eek.time.max": 0
"crypto_worker.eek.time.median": 0
"crypto_worker.eek.time.50": 0
"crypto_worker.eek.time.75": 0
"crypto_worker.eek.time.90": 0
"crypto_worker.eek.time.95": 0
"crypto_worker.eek.time.99": 0
"crypto_worker.eek.time.999": 0
"crypto_worker.encrypt.error": 0
"crypto_worker.encrypt.ok": 4
"crypto_worker.encrypt.time.n": 5
"crypto_worker.encrypt.time.mean": 277
"crypto_worker.encrypt.time.min": 178
"crypto_worker.encrypt.time.max": 339
"crypto_worker.encrypt.time.median": 314
"crypto_worker.encrypt.time.50": 314
"crypto_worker.encrypt.time.75": 339
"crypto_worker.encrypt.time.90": 339
"crypto_worker.encrypt.time.95": 339
"crypto_worker.encrypt.time.99": 339
"crypto_worker.encrypt.time.999": 339
"crypto_worker.generate_eek.error": 0
"crypto_worker.generate_eek.ok": 0
"crypto_worker.load_key.error": 0
"crypto_worker.load_key.ok": 9
"erlang.system_info.port_count": 16
"erlang.system_info.process_count": 527
"erlang.system_info.thread_pool_size": 64
"keystore.get.error": 0
"keystore.get.not_found": 0
"keystore.get.ok": 1
"keystore.put.error": 0
"keystore.put.ok": 0
"ocsp.check.error": 0
"ocsp.check.good": 17
"ocsp.check.revoked": 0
"ocsp.check.timeout": 0
"ocsp.lookup.time.n": 3
"ocsp.lookup.time.mean": 1176882
"ocsp.lookup.time.min": 1176882
"ocsp.lookup.time.max": 1176882
"ocsp.lookup.time.median": 1176882
"ocsp.lookup.time.50": 1176882
"ocsp.lookup.time.75": 1176882
"ocsp.lookup.time.90": 1176882
"ocsp.lookup.time.95": 1176882
"ocsp.lookup.time.99": 1176882
"ocsp.lookup.time.999": 1176882
"vm.erlang.total": 38693512
"vm.erlang.processes": 14688544
"vm.erlang.processes_used": 14666160
"vm.erlang.system": 24003328
"vm.erlang.atom": 504409
"vm.erlang.atom_used": 494084
"vm.erlang.binary": 430728
"vm.erlang.ets": 5743384
```

Notes:

* Timing values use microsecond resolution.
* Histograms use one minute sliding windows.

### Audit logs

#### Listing Audit Logs

The `cinched list-audit-logs` command lists all audit logs and associated file size.

```
cinched list-audit-logs
{"/var/log/cinched/audit/audit.log",6724}
{"/var/log/cinched/audit/audit.log.1450317986",6773}
{"/var/log/cinched/audit/audit.log.1450314263",4767}
{"/var/log/cinched/audit/audit.log.1450304379",3135}
{"/var/log/cinched/audit/audit.log.1450302608",397}
{"/var/log/cinched/audit/audit.log.1450301816",1177}
{"/var/log/cinched/audit/audit.log.1450300848",785}
{"/var/log/cinched/audit/audit.log.1450300094",397}
{"/var/log/cinched/audit/audit.log.1450297044",788}
{"/var/log/cinched/audit/audit.log.1450296589",1180}
{"/var/log/cinched/audit/audit.log.1450235718",398}
{"/var/log/cinched/audit/audit.log.1450235473",398}
{"/var/log/cinched/audit/audit.log.1450235364",788}
{"/var/log/cinched/audit/audit.log.1450234187",788}
{"/var/log/cinched/audit/audit.log.1450233908",8}
{"/var/log/cinched/audit/audit.log.1450233536",790}
{"/var/log/cinched/audit/audit.log.1450232528",1568}
{"/var/log/cinched/audit/audit.log.1450232123",8}
{"/var/log/cinched/audit/audit.log.1450231964",8}
{"/var/log/cinched/audit/audit.log.1450231205",8}
{"/var/log/cinched/audit/audit.log.1450230062",8}
```
Log files are rotated on service start-up and have the unix timestamp of the rotation time appended to the file name.

Operators can rotate the audit log with the `cinched rotate-audit-log` command. This will close the current log, rename it, and open a new log. This is the only correct way of rotating the logs.

#### Verifying audit log integrity

The integrity of a log file can be verified by running the `cinched check-audit-log` file and specifying the path of the log you want to verify.

```
cinched check-audit-log /var/log/cinched/audit/audit.log.1450232528
Status: ok
```

#### Viewing the contents of a log file

The `cinched view-audit-log` command can be used to view the contents of an audit log.

```
[root@dev1 cinched]# cinched view-audit-log /var/log/cinched/audit/audit.log
{"timestamp":"2016-01-04 14:34:25 +00:00","op":"list_logs","user":"root"}
{"timestamp":"2016-01-04 14:34:32 +00:00","op":"view_log","user":"root","log":"/var/log/cinched/audit/audit.log"}
{"timestamp":"2016-01-04 14:34:41 +00:00","meta":"undefined","query_string":"","user_agent":"curl/7.29.0","peer_ip":"192.168.101.1","peer_port":"32986","peer_cert_cn":"dev1.control-alt-del.org","peer_cert_serial":"7","op":"data_key","status":"ok"}
{"timestamp":"2016-01-04 14:34:44 +00:00","meta":"undefined","query_string":"","user_agent":"curl/7.29.0","peer_ip":"192.168.101.1","peer_port":"32988","peer_cert_cn":"dev1.control-alt-del.org","peer_cert_serial":"7","op":"data_key","status":"ok"}
{"timestamp":"2016-01-04 14:34:46 +00:00","op":"field_encrypt","status":"ok","meta":"foobar","query_string":"fields=(bar)","user_agent":"curl/7.29.0","peer_ip":"192.168.101.1","peer_port":"32989","peer_cert_cn":"dev1.control-alt-del.org","peer_cert_serial":"7"}
{"timestamp":"2016-01-04 14:34:47 +00:00","op":"field_decrypt","status":"ok","meta":"fobarr","query_string":"fields=(bar)","user_agent":"curl/7.29.0","peer_ip":"192.168.101.1","peer_port":"32990","peer_cert_cn":"dev1.control-alt-del.org","peer_cert_serial":"7"}
{"timestamp":"2016-01-04 14:34:49 +00:00","op":"blob_encrypt","status":"ok","meta":"foobar","query_string":"","user_agent":"curl/7.29.0","peer_ip":"192.168.101.1","peer_port":"32991","peer_cert_cn":"dev1.control-alt-del.org","peer_cert_serial":"7"}
{"timestamp":"2016-01-04 14:34:50 +00:00","op":"blob_decrypt","status":"ok","meta":"fobarr","query_string":"","user_agent":"curl/7.29.0","peer_ip":"192.168.101.1","peer_port":"32992","peer_cert_cn":"dev1.control-alt-del.org","peer_cert_serial":"7"}
{"timestamp":"2016-01-04 14:34:55 +00:00","meta":"undefined","query_string":"","user_agent":"curl/7.29.0","peer_ip":"192.168.101.1","peer_port":"32993","peer_cert_cn":"dev1.control-alt-del.org","peer_cert_serial":"7","op":"data_key","status":"ok"}
Log dump complete
```

### Backups/Restore

Backups can be created by a normal operator, but the backup files require an elevated level of access to the filesystem. Current policy does not map any groups to this level of access. It's on my TODO list, and until it's done there really isn't any point in taking backups as no users have sufficient filesystem access to be able to copy them.

The functionality is still here however in case of disaster. Should the worst happen and you lose a node read on!

A backup consists of a point in time snapshot of all replication groups. This is not a global snapshot, but rather a per-replica group snapshot.

To kick off a backup, use the `cinched backup` command (from any of your live nodes):

```
cinched backup
```

A backup of the ensembles in the cluster will be created in `/var/lib/cinched/backups`.

** DANGER DANGER DANGER **: I have only lightly tested this. This might be broken, so before relying on it too heavily a healthy dose of testing should be done. It might eat your toes in the middle of the night.

The ensemble data should be backed up successfully via a snapshot of the leveldb database, however the root ensemble is stored as a file and I currently simply copy it over. That _should_ be safe as every modification to the root ensemble is fsynched and uses atomic filesystem operations.

What I am not sure of however is how the copying of the synctree might break and I haven't yet had a chance to grok what that does yet. If my understanding is correct (and it very well may not be), it should fix itself on peer tree exchange.

On the node you did the backup, you'll want to shutdown Cinched (eg: `cinched stop`), disable SELinux, grab a copy of the backup, re-enable SELinux, then restart Cinched (eg: `cinched start`) and provide the key shards (`cinched send-key-shard`) to get the node back into service.

To replace a node:

* Re-install Cinched on the replacement node
    * Make sure it has the same IP
    * Make sure it has the same configuration
    * Do *not* run the `cinched setup` setup wizard
* Create a backup from a working node in the cluster
* Copy backup files to the new node
* Run `cinched setup`, this will ensure proper SELinux filesystem properties/permissions
* Start Cinched as normal with the `cinched start` command.
* Enter the key shards

### Rotating the Operator Key

To create a new set of shards in the cases where you would like to change your key custodians, use the `cinched rotate-operator-key` command.

This will initiate a process that will wait to receive currently valid key shards (to make sure this isn't a rogue request), then generate a new Operator Key with new shards.

To send the shards, use the `cinched rotate-operator-key-send-shard` command. Once sufficient shards have been provided, it will output the new set of shards on the terminal.

### Cluster resizing

There is no way to do an online cluster resize at this point. To resize:

* Prep new nodes (install packages)
* Make a backup from one of the nodes (`cinched backup`)
* Stop all nodes in cluster (`cinched stop` on each node)
* Disable SELinux on all nodes
* Grab the backup files from the node you initiated the backup
* Restore backup onto ALL nodes into `/var/lib/cinched/`
* Run the setup wizard (`cinched setup`) providing new cluster configuration
* Start all nodes (`cinched start`)
* Initialize all nodes with operator key (`cinched send-key-shard`)

# Quick Start API Crash Course

## Generate a data key

```
curl -s \
-X POST --cert fixtures/cert.pem --key fixtures/key.pem \
--cacert fixtures/cacert.pem --tlsv1.2  \
https://dev1.control-alt-del.org:55443/key/data-key | jq
{
  "dataKey": "g2gEZA9rZXliAABBk<snip>XA4WglqFGSzDA2H3FI2uXYu0bqGthAQ==",
  "cryptoPeriod": 16786
}
```

Store this information somewhere, we'll need it to decrypt the data later on.

## Encrypt something

Let's imagine we have a JSON document that looks like this:
```
{
 "foo": "bar",
 "favoriteBar":"baz's emporium of fine spirits"
}
```

And let's further suppose we want to encrypt the field 'favoriteBar', as that's confidential.

```
curl -s \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json' \
  -H "x-cinched-data-key: g2gEZA9rZXliAABBk<snip>XA4WglqFGSzDA2H3FI2uXYu0bqGthAQ==" \
  -H "x-cinched-metadata: foobar" \
  -X POST -d "{\"foo\":\"bar\",\"favoriteBar\":\"baz's emporium of fine spirits\"}" \
  --cert fixtures/cert.pem --key fixtures/key.pem \
  --cacert fixtures/cacert.pem \
  --tlsv1.2 \
  https://dev1.control-alt-del.org:55443/doc/encrypt?fields=\(favoriteBar\)
{"foo": "bar","favoriteBar": "A9rZXliAABBk<snip>XA4WglqFGSzD"}
```

Success! Now we can safely store this document somewhere without fear that a hacker who manages to steal our database (or a code bug) will be able to leak information.

## Decrypting data

To read the data back:

```
curl -s \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json' \
  -H "x-cinched-data-key: g2gEZA9rZXliAABBk<snip>XA4WglqFGSzDA2H3FI2uXYu0bqGthAQ==" \
  -H "x-cinched-metadata: foobar" \
  -X POST -d "{\"foo\":\"bar\",\"bar\":\"A9rZXliAABBk<snip>XA4WglqFGSzD\"}" \
  --cert fixtures/cert.pem --key fixtures/key.pem \
  --cacert fixtures/cacert.pem \
  --tlsv1.2 \
  https://dev1.control-alt-del.org:55443/doc/decrypt?fields=\(favoriteBar\)
{"foo": "bar","favoriteBar": "baz's emporium of fine spirits"}
```

Well done! Now we know where to go grab a brew.

# API

## Data key generation

Data keys are generated by the calling application as appropriate depending on your use case. They need to be persisted by the application as they are required for decryption and are not stored in the Cinched service.

### HTTP API

#### Request
```
POST /key/data-key HTTP/1.1
Accept: application/json
```

|Request Header     |Value              |Required |
|-------------------|-------------------|---------|
|Accept             |application/json   |Yes      |
|x-cinched-metadata |Application metadata to be logged| No |
|user-agent         |Client user agent (logged) | No |

#### Response
```
HTTP/1.1 200 OK
Content-type: application/json
Content-length: 134
{"dataKey":"ae4radfslkj4ljrlkjdflzASJDF","cryptoPeriod":16843}
```

|Response header |Value|
|----------------|-----|
|Content-length |Length of returned document|
|Content-type|application/json|

|Document Field |Value|
|---------------|-----|
|dataKey|The generated data key|
|cryptoPeriod|Number of days since Jan 1, 1970|

### Sample use case: one data key per user

If your system has multiple users, one way of managing keys that can be accomplished fairly easily is to generate one data key per user (for example at account creation time), and use that key for all operations relating to that user.

It's a good idea to store the data key in a separate storage system (with different access credentials) than the encrypted data.

To defend against brute force/offline attacks against the keys, you can keep track of the crypto-period per key and periodically query for keys that have aged past a given threshold and generate a new data key/re-encrypt all the protected data.

### Sample use case: one data key per encrypted object

This is the most secure stance. When creating or updating an object, let Cinched generate a new data key (by not passing in the `x-cinched-data-key` HTTP header, and store the generated key for later use.

The downside of this approach is that you now need to do a one-to-one mapping of stored document to data key.

## Document API

### Field specifications

The Document API endpoints have a required URI parameter called `fields`.

It's format looks like this:
```
/doc/encrypt?fields=(field1,field2,fieldN)
```

Is is a comma-delimited list of fields to be extracted from the provided JSON object to either encrypt or decrypt.

It is possible to traverse the document using a notation similar to how one would access object properties from Javascript.

For example, with the document:
```
{
  "field1":"bar",
  "field2": {"sub1": "foo","sub2":"bar"},
  "field3": {"sub1": "fuz","sub4":["one","two","three"]}
}
```

We can encrypt `field2->sub1` with the field specification:

```
/doc/encrypt?fields(field2.sub1)
```

Which would produce:
```
{
  "field1":"bar",
  "field2": {"sub1": "<ENCRYPTED>","sub2":"bar"},
  "field3": {"sub1": "fuz","sub4":["one","two","three"]}
}
```

It is also possible to access array elements by index. One could only encrypt the string "three" (`field3->sub4->[3]`) with a field spec of:

```
/doc/encrypt?fields(field3.sub4.3)
```

Which would produce:

```
{
  "field1":"bar",
  "field2": {"sub1": "foo","sub2":"bar"},
  "field3": {"sub1": "fuz","sub4":["one","two","<ENCRYPTED>"]}
}
```

When a specification emcompasses document sub-nodes, the sub-nodes get collapsed inside a encrypted string. Using the same example document above, the following spec:

```
/doc/encrypt?fields(field3.sub4)
```

Would produce:
```
{
  "field1":"bar",
  "field2": {"sub1": "foo","sub2":"bar"},
  "field3": {"sub1": "fuz","sub4":"<ENCRYPTED>"}
}
```

During decryption, collapsed sub-nodes get re-created.

If your field specification specifies both a parent level and sub-nodes of that parent, expect weird results as the field specifications don't check for that. Don't do that.

### Encryption

This API is used to encrypt portions of a JSON document.

#### Request
```
POST /doc/encrypt?fields=(field1,field2.sub1,field3.sub4.3)
Content-type: applicatin/json
Accept: application/json
Content-length: 2321
x-cinched-data-key: ae4radfslkj4ljrlkjdflzASJDF
x-cinched-metadata: some metadata
{
  "field1":"bar",
  "field2": {"sub1": "foo","sub2":"bar"},
  "field3": {"sub1": "fuz","sub4":["one","two","three"]}
}
```

|Request Header     |Value              |Required |
|-------------------|-------------------|---------|
|Accept             |application/json   |Yes      |
|Content-type       |application/json   |Yes      |
|Content-length     |Integer            |Yes      |
|x-cinched-data-key |A data key. Will auto-generate one if not present. |No       |
|x-cinched-metadata |Meta data to log   |No       |


#### Response
```
HTTP/1.1 200 OK
x-cinched-data-key: ae4radfslkj4ljrlkjdflzASJDF
x-cinched-crypto-period: 16843
Content-type: application/json
Content-length: 1214
{
  "field1":"<ENCRYPTED",
  "field2": {"sub1": "<ENCRYPTED>","sub2":"bar"},
  "field3": {"sub1": "fuz","sub4":["one","two","<ENCRYPTED>"]}
}
```

|Response header |Value|
|----------------|-----|
|x-cinched-data-key| The data key used during encryption|
|x-cinched-crypto-period|The date at which the key was created as epoch day|
|Content-length |Length of returned document|
|Content-type|application/json|

### Decryption

This API is used to decrypt portions of a previously encrypted JSON document.

#### Request
```
POST /doc/decrypt?fields=(field1,field2.sub1,field3.sub4.3)
Content-type: applicatin/json
Content-length: 1234
Accept: application/json
x-cinched-data-key: ae4radfslkj4ljrlkjdflzASJDF
x-cinched-metadata: some metadata
{
  "field1":"<ENCRYPTED",
  "field2": {"sub1": "<ENCRYPTED>","sub2":"bar"},
  "field3": {"sub1": "fuz","sub4":["one","two","<ENCRYPTED>"]}
}
```

|Request Header     |Value              |Required |
|-------------------|-------------------|---------|
|Accept             |applicatoin/json   |Yes      |
|Content-type       |application/json   |Yes      |
|Content-length     |Length of document |Yes      |
|x-cinched-data-key |The data key to use for decryption |Yes |
|x-cinched-metadata |Meta data to log   |No       |

#### Response
```
HTTP/1.1 200 OK
Content-type: application/json
Content-length: 1234
{
  "field1":"bar",
  "field2": {"sub1": "foo","sub2":"bar"},
  "field3": {"sub1": "fuz","sub4":["one","two","three"]}
}
```

|Response Header    |Value              |
|-------------------|-------------------|
|Content-type       |application/json   |
|Content-length     |Length of document |


## Blob API

### Encryption

This API is used to encrypt arbitrary data blobs.

#### Request
```
POST /blob/encrypt
Content-type: application/octet-stream
x-cinched-data-key: ae4radfslkj4ljrlkjdflzASJDF
x-cinched-metadata: some metadata
Content-length: 34234
<BINARY FILE DATA HERE>
```

|Request Header     |Value              |Required |
|-------------------|-------------------|---------|
|Accept             |application/octet-stream   |Yes      |
|Content-type       |application/octet-stream   |Yes      |
|Content-length     |Integer            |Yes      |
|x-cinched-data-key |A data key. Will auto-generate one if not present. |No       |
|x-cinched-metadata |Meta data to log   |No       |

#### Response
```
HTTP/1.1 200 OK
x-cinched-data-key: ae4radfslkj4ljrlkjdflzASJDF
x-cinched-crypto-period: 16843
Content-length: 2342342
Content-type: application/octet-stream
<BINARY ENCRYPTED DATA>
```

|Response header |Value|
|----------------|-----|
|x-cinched-data-key| The data key used during encryption|
|x-cinched-crypto-period|The date at which the key was created as epoch day|
|Content-type       |application/octet-stream   |
|Content-length     |Length of document |

### Decryption

This API is used to decrypt blobs previously encrypted.

#### Request
```
POST /blob/decrypt
Content-type: applicatin/octet-stream
Accept: application/octet-stream
x-cinched-data-key: ae4radfslkj4ljrlkjdflzASJDF
x-cinched-metadata: some metadata
Content-length: 234234
<ENCRYPTED BINARY FILE DATA>
```

|Request Header     |Value              |Required |
|-------------------|-------------------|---------|
|Accept             |application/octet-stream   |Yes      |
|Content-type       |application/octet-stream   |Yes      |
|Content-length     |length of blob            |Yes      |
|x-cinched-data-key |A data key. Will auto-generate one if not present.  |No       |
|x-cinched-metadata |Meta data to log   |No       |

#### Response
```
HTTP/1.1 200 OK
Content-type: application/octet-stream
Content-length: 12344
<FILE DATA>
```

|Response Header    |Value              |
|-------------------|-------------------|
|Content-type       |application/octet-stream   |
|Content-length     |Length of document |


# Future work

## Dynamic cluster membership

I'll probably re-implement this as a riak_core app, as riak_core will take care of some of the thornier bits of handling membership changes.

## Alternate encryption engines

I want to look at some alternatives for the encryption to see if/how I can leverage hardware assisted encryption (AES-NI) and possibly hardware assisted entropy generation (RDRAND, TPM chips).

## Transparent encryption document storage API

The plan here is to create an API that will transparently store the data keys and encrypted documents to the storage medium of your choice.

My current thoughts are to implement:

* MySQL mapping to a table with a JSON column for the payload, and varchar for the data key
* Riak bucket, storing as blob with data key as object metadata (only makes sense to do this with ensemble storage though....).
* MongoDb
* CouchDb

Each of these will require some mechanism for reasoning about concurrency (eg: compare-and-swap)

## Better integrity assurance

One of the biggest beefs I have with this design is that there is no good way that I know in Erlang to secure the private key used for establishing TLS in the Erlang distribution protocol. This isn't Erlang's fault, it's just a hard problem to solve.

I've resigned myself to use unencrypted private keys for now (to reduce operator friction), which are protected via SELinux policy.

Erlang's inter-node authentication mechanism (the humble cookie) isn't really designed for security. It was designed simply to be able for trusted clusters to be able to differentiate from each other.

When I have some time to look into it, I'd like to leverage TPMs to enforce integrity of the nodes and would provide a mechanism for peer authentication that would be superior to the approach I've currently taken.

This would involve leveraging the Integrity Management Architecture (IMA) facilities of the Linux kernel along with the Linux Extented Verification Module (EVM) to track known good state in hardware.

Based on that known good state several approaches emerge that could be used for establishing secure authenticated channels between nodes.

Using a parse transform, an attestation protocol could be layered on top of the erlang communication (eg: replace send, \!, etc...) to ensure valid peers (authentication), and payload encryption. The downside of this approach would be the funnelling of all traffic through a single gen_server (probably some clever ways of distributing this). Easy-ish to do though.

Another alternative would be a new distribution protocol carrier could be created to perform attestation followed by a key exchange/negotiation with periodic re-attestation via TPM.

In both these scenarios, you could restrict keys to only be usable when the system state is good, and the private keys would never leave the hardware chip.

Further reading:

https://www.mitre.org/sites/default/files/pdf/10_0959.pdf

https://www.mitre.org/sites/default/files/pdf/09_5343.pdf

http://opensecuritytraining.info/IntroToTrustedComputing_files/Day2-1-auth-and-att.pdf

http://sourceforge.net/p/linux-ima/wiki/Home/#integrity-measurement-architecture-ima

https://lwn.net/Articles/144681/

https://www.cylab.cmu.edu/tiw/slides/challener-TPM.pdf

https://www.kernel.org/doc/Documentation/security/keys-trusted-encrypted.txt

https://www.openstack.org/summit/vancouver-2015/summit-videos/presentation/using-tpms-for-the-benefit-of-the-entire-cloud

https://github.com/nebula/python-tss

rng-tools -> better random number generation (can speak to tpm broker)

libvirt can use rng stuff.

PKCS11 -> don't use Trousers for this (apparently bad?), use Chaps (from google)

https://github.com/google/chaps-linux

https://github.com/OpenAttestation/OpenAttestation

http://selinuxproject.org/~jmorris/lss2011_slides/Integrity_overview_lss.pdf

https://github.com/PeterHuewe/tpm-emulator

http://enforcer.sourceforge.net/

http://d.hatena.ne.jp/munetoh/20110901 <-- how to get tpm emulator, kernel, openpts working together...

http://www.infineon.com/dgdl/Linux+and+Open+Source+activities+for+Trusted+Computing+and+TPM+applications.pdf?fileId=db3a304412b407950112b4165abb2043

http://d.hatena.ne.jp/munetoh/20120326

http://cryptodev-linux.org/comparison.html  <-- /dev/crypto for linux. Leverages crypto api and exposes hardware AES, can patch openssl/gnutls

# Somewhat similar projects

* Barbican (secret storage, can leverage TPMs)
* Vault (secret storage)

# Building a monolithic kernel

```
yum -y groupinstall "Development Tools"
yum -y install ncurses-devel \
hmaccalc zlib-devel binutils-devel \
elfutils-libelf-devel pciutils openssl-devel bc
cd /usr/src/kernels
wget https://cdn.kernel.org/pub/linux/kernel/v4.x/testing/linux-4.5-rc1.tar.xz
tar xf linux-4.5-rc1.tar.xz
cd linux-4.5-rc1
cp /boot/config-3.10.0-327.4.5.el7.x86_64 .config
make oldconfig
make menuconfig
make -j3 bzImage && make -j3 modules && make install && make modules_install
```

If you've never done this, please search around on the internet on specific instructions on what options you might need enabled.

For the curious, here is my kernel configuration for a VMware guest (e1000 NIC, vmware scsi disk):

```
https://gist.github.com/marksteele/f491a23520b9cbe53868
```

Drop that into your kernel source tree as `.config`, then you can proceed to the `make oldconfig; make menuconfig` and adjust settings to reflect your host.

What it does:

* No kernel modules allowed. Truth be told, if an attacker can load modules the probability of the other defenses helping start getting slim unless you've set a boot password (preventing the node from booting unattended) and locked down the selinux policy.
* Compiles in the functionality I needed to get my system to boot

Your mileage may vary.


# Setup DogTag CA on CenttOS 7

See this fine document written by yours truly: http://www.control-alt-del.org/blog/2016/01/30/setting-up-a-ca/
