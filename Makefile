PROJECT	= erldog


DEPS = lager pooler lhttpc jsx
dep_lager = git https://github.com/basho/lager.git master
dep_pooler = git https://github.com/seth/pooler.git master
dep_lhttpc = git git://github.com/talko/lhttpc master
dep_jsx = git https://github.com/talentdeficit/jsx.git master
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
