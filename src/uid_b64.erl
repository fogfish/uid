%% @doc
%%   a copy-pastable, url friendly, ascii embeddable, lexicographically sortable binary encoding.
%%   derived from https://github.com/dominictarr/d64
-module(uid_b64).

-export([encode/1, decode/1]).

%%
%%
encode(Bin) when is_binary(Bin) ->
   Len = 3 * (byte_size(Bin) div 3),
   <<Head:Len/binary, Tail/binary>> = Bin,
   <<(encode_head(Head))/binary, (encode_tail(Tail))/binary>>.

encode_head(Bin) ->
   << <<(encode_b64(X)):8>> || <<X:6>> <= Bin >>.

encode_tail(<<A:6, B:6, C:4>>) ->
   <<(encode_b64(A)):8, (encode_b64(B)):8, (encode_b64(C bsl 2)):8 >>;

encode_tail(<<A:6, B:2>>) ->
   <<(encode_b64(A)):8, (encode_b64(B bsl 4)):8>>;

encode_tail(<<>>) ->
   <<>>.

encode_b64(X) ->
   element(X + 1,
      {$., $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E,
       $F, $G, $H, $I, $J, $K, $L, $M, $N, $O, $P, $Q, $R, $S, $T, $U,
       $V, $W, $X, $Y, $Z, $_, $a, $b, $c, $d, $e, $f, $g, $h, $i, $j,
       $k, $l, $m, $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z}).

%%
%%
decode(Bin) when is_binary(Bin) ->
   Val = << <<(decode_b64(X)):6>> || <<X:8>> <= Bin >>,
   Len = 8 * (erlang:bit_size(Val) div 8),
   <<Head:Len/bitstring, _/bitstring>> = Val,
   Head.

decode_b64($.) -> 0;
decode_b64(X) when X >= $0 andalso X =< $9 -> (X - $0) +  1;
decode_b64(X) when X >= $A andalso X =< $Z -> (X - $A) + 11;
decode_b64($_) -> 37;
decode_b64(X) when X >= $a andalso X =< $z -> (X - $a) + 38.
