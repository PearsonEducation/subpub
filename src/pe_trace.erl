% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_trace).
-export([log_trace/3]).

log_trace(Mod, Meth, LineNum) ->
	io:format("Module: ~s\tMethod: ~s\tLine Number: ~B~n", [Mod, Meth, LineNum]).
