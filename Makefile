PROJECT	= erldog


DEPS = lager pooler lhttpc jsx
dep_pooler = git https://github.com/seth/pooler.git 1.3.3
dep_lhttpc = git git://github.com/talko/lhttpc
include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}' 
#+warn_missing_spec
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

RUN := erl -pa ebin -pa deps/*/ebin -smp enable -s sync -boot start_sasl ${ERL_ARGS}
NODE ?= ${PROJECT}

shell: app
		if [ -n "${NODE}" ]; then ${RUN} -name ${NODE}@`hostname` -s ${PROJECT} -s sync -config rel/sys.config; \
				else ${RUN} -s ${PROJECT} -config rel/sys.config; \
					fi

erldocs: all
		erldocs . -o docs
