%% the billet_doux record is how Pid are packaged for transport
-record(billet_doux, {id,
                      nonce}).

-record(proxy, {name,
                domain,
                epmd_port,
                private_key,
                wtd_node}).

-record(signed_request, {public_key,
                         ack        = wtd_utils:get_ack(),
                         type       = sync,                % sync | async
                         signature,
                         body}).

-record(request, {node,     % called node
                  module,
                  function,
                  arguments,
                  date       = dh_date:format("D, j M Y H:i:s")}).

-record(response, {node,    % original caller
                   response,
                   date      = dh_date:format("D, j M Y H:i:s")}).

