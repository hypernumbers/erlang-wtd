%% the billet_doux record is how Pid are packaged for transport
-record(billet_doux, {id,
                      nonce}).

-record(proxy, {name,
                domain,
                epmd_port,
                public_key,
                private_key,
                wtd_node}).
