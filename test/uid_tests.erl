%%
%%   Copyright 2012 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
-module(uid_tests).
-include_lib("eunit/include/eunit.hrl").

%%
%%
l_test() ->
   {uid, _} = uid:l().

%%
%%
g_test() ->
   L = uid:l(),
   L = uid:l(uid:g(L)),
   L = uid:gtol(uid:g(L)).

%%
%%         --> D --
vclock_test() ->
   A = uid:vclock(),
   B = uid:vclock(A),
   C = [{a, uid:l()} | B],
   D = [{b ,uid:l()} | B],
   E = uid:join(C, D),
   false = uid:descend(A, B),
   true  = uid:descend(B, A),
   true  = uid:descend(C, B),   
   true  = uid:descend(D, B),
   false = uid:descend(C, D),   
   false = uid:descend(D, C),
   true  = uid:descend(E, C),   
   true  = uid:descend(E, D).   


