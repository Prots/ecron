#!/bin/sh
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -sname dev -setcookie ecron -config sys -s ecron
