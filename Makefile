PROJECT = ecron

CT_OPTS += -erl_args -pa ./ebin -pa ./deps/*/ebin -s ecron -config sys

# this must be first
include erlang.mk

