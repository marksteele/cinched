-include_lib("passwderl/include/passwderl.hrl").

-record(nacl_envelope,{nonce,ciphertext}).

-define(DEFAULT_TIMEOUT, 10000).
-define(RINGTOP, trunc(math:pow(2,160)-1)).  % SHA-1 space
-define(PEM_ENCODED_LINE_LENGTH, 64).

-define(AUDIT_LOG_SYNC_INTERVAL,2000).
-define(OPEN_FILE_LIMIT,65536).
-define(CINCHED_USER,"cinched").

-define(PLATFORM_LIB_DIR,"/usr/lib64/cinched").
-define(PLATFORM_DATA_DIR,"/var/lib/cinched").
-define(PLATFORM_ETC_DIR,"/etc/cinched").
-define(PLATFORM_LOG_DIR,"/var/log/cinched").
-define(PLATFORM_BIN_DIR,"/usr/lib64/cinched").

-define(CERTFILE,"/var/lib/cinched/cert.pem").
-define(KEYFILE,"/var/lib/cinched/key.pem").
-define(CACERTFILE,"/var/lib/cinched/cacert.pem").

-record(obj,{epoch,seq,key,value}).

-record(cinched_key, {
          crypto_period :: non_neg_integer(),
          key :: #nacl_envelope{},
          version :: non_neg_integer()
         }
       ).

-record(cinched_log, {
          hash :: binary(),
          payload :: binary(),
          timestamp :: tuple()
         }).
