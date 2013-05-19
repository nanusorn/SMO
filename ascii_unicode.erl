%% Copyright (c) 2008-2013, Nanusorn Photpipat <nanusorn@photpipat.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.-module (ability_effect).
-module (ascii_unicode).

-export ([ascii_to_unicode/1, unicode_to_ascii/1]).

%ascii_to_unicode (Text, Language) ->
%	case Language of
%		th -> ascii_to_unicode (Text);
%		en -> ascii_to_unicode (Text);
%		_ -> io:format("Other language ~p~n", [Language])
%	end.
%
%ascii_to_unicode ([]) -> [];
%ascii_to_unicode ([Chr | T]) ->
%	if
%		Chr > 16#a0 -> % แปลงตัวอักษรเป็นภาษาไทย ใน Utf-8
%			TH_Chr = Chr + 16#0d60,
%			[16#0e, TH_Chr - 3584] ++ ascii_to_unicode (T);
%		true -> [00, Chr] ++ ascii_to_unicode (T)
%	end.

ascii_to_unicode ([]) -> [];
ascii_to_unicode ([Chr | Data]) ->
	if Chr >= 224, Chr =< 255 ->
		[16#e0, 16#b9, Chr - 16#60] ++ ascii_to_unicode (Data);
	   Chr =< 128 ->
		[Chr] ++ ascii_to_unicode (Data);
	   true ->
		[16#e0, 16#b8, Chr - 16#20] ++ ascii_to_unicode (Data)
	end.

unicode_to_ascii ([]) -> [];
unicode_to_ascii ([16#e0, Mibyte, Lobyte | T]) ->
	case Mibyte of
		16#b8 ->
%			<<Data:16>> = <<Mibyte, Lobyte>>,
%			Ascii = Data - 16#b7e0,
			Ascii = Lobyte + 16#20,
			[Ascii] ++ unicode_to_ascii (T);
		16#b9 ->
			Ascii = Lobyte + 16#60,
			[Ascii] ++ unicode_to_ascii (T)
	end;
unicode_to_ascii ([Ascii | T]) ->
	[Ascii] ++ unicode_to_ascii (T).

%unicode_to_ascii ([]) -> [];
%unicode_to_ascii ([Hibyte, Lobyte | T]) ->
%	case Hibyte of
%		16#00 -> [Lobyte] ++ unicode_to_ascii (T);
%		16#0e ->
%			if
%				Lobyte >= 16#01, Lobyte =< 16#7f ->
%					Ascii = 3584 + Lobyte - 16#0d60,
%					[Ascii] ++ unicode_to_ascii (T);
%				true -> io:format("Other lo byte ~p~n", [Lobyte])
%			end;
%		_ -> io:format("Other hi byte ~p~n", [Hibyte])
%	end.