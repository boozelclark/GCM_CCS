-module(gcm_ccs).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    
    %gcm:start_link(),
    gcm_ccs_sup:start_link().

stop(_State) ->
    ok.
